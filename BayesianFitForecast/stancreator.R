generate_stan_file <- function() {
  # Creating the stan file
  library(checkmate)
  source("diff.R")
  source("ode_rhs.R")
  
  ############################################################################
  # Define the is.wholenumber function
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
    if (is.numeric(x)) {
      abs(x - round(x)) < tol
    } else {
      FALSE
    }
  }
  # Initialize empty vectors for params_to_include, params_integer, and params_real
  params_to_include <- c()
  params_integer <- c()
  params_real <- c()
  
  for (i in seq_along(paramsfix)) {
    prior_name <- paste0("params", i, "_prior")
    prior_value <- get(prior_name)
    
    if (paramsfix[i] == 0) {
      params_to_include <- c(params_to_include, params[i])
    } else if (is.wholenumber(prior_value)) {
      params_integer <- c(params_integer, params[i])
    } else {
      params_real <- c(params_real, params[i])
    }
  }
  # Have it as a set
  params_integer_set <- unique(params_integer)
  params_integer_set_formatted <- paste0("{", paste(params_integer_set, collapse = ","), "}")
  params_real_set <- unique(params_real)
  params_real_set_formatted <- paste0("{", paste(params_real_set, collapse = ","), "}")
  
  ##############################################################################
  # Creating the model
  diff_var <- diff_var(vars)
  
  for (i in 1:length(diff_var)) {
    pattern <- paste0("\\bdiff_var", i, "\\b")
    replacement <- diff_var[i]
    ode_system <- gsub(pattern, replacement, ode_system)
  }
  
  for (i in 1:length(vars)) {
    pattern <- paste0("\\bvars", i, "\\b")
    replacement <- vars[i]
    ode_system <- gsub(pattern, replacement, ode_system)
  }
  
  for (j in 1:length(params)) {
    pattern <- paste0("\\bparams", j, "\\b")
    replacement <- params[j]
    ode_system <- gsub(pattern, replacement, ode_system)
  }
  
  lines <- strsplit(ode_system, "\n", fixed = TRUE)[[1]]
  lines <- lines[-1]
  ode_system <- paste0("    real ",trimws(lines), ";")
  ode_system <- paste(ode_system, collapse = "\n")
  
  returnodesystem <- paste(diff_var, collapse = ",")
  returnodesystem <- paste0("\n return{", returnodesystem, "};\n }")
  ode_system <- paste(ode_system, returnodesystem)
  
  ##############################################################################
  
  # Check if the file exists before removing
  if (file.exists(stan_file)) {
    file.remove(stan_file)
    cat(paste("Removed file:", stan_file, "\n"))
  }
  ##############################################################################
  
  prior_assignments <- ""
  for (i in seq_along(params)) {
    prior_name <- paste0("params", i, "_prior")
    prior_value <- get(prior_name)
    
    if (!is.numeric(prior_value)) {
      prior_assignments <- paste0(prior_assignments, "  ", params[i], " ~ ", 
                                  prior_value, ";\n")
    }
  }
  ##############################################################################
  # Initialize the variable we are interested in fitting the model
  if (fitting_diff == 1) {
    fitting <- ode_rhs(fitting_index, ode_system)
  } else {
    fitting <- vars[fitting_index]
  }
  
  for (i in seq_along(vars)) {
    fitting <- gsub(paste0("\\b", vars[i], "\\b"), paste0("y[t,", i, "]"), fitting)
  }
  ##############################################################################
  # Generate the ODE function for Stan
  ode_function <- "
  real[] ode(real t, real[] y, real[] theta, real[] x_r, int[] x_i) { \n"
  
  ode_function <- paste0(ode_function, "\n \n")
  # Loop through params_to_include to construct the ode_function
  for (i in seq_along(params_to_include)) {
    ode_function <- paste0(ode_function, "    real ", params_to_include[i], " = theta[", i, "];\n")
  }
  
  for (i in seq_along(params_integer)) {
    ode_function <- paste0(ode_function, "    real ", params_integer[i], " = x_i[", i, "];\n")
  }
  for (i in seq_along(params_real)) {
    ode_function <- paste0(ode_function, "    real ", params_real[i], " = x_r[", i, "];\n")
  }
  if(vars.init == 1 )
  {
  for (i in seq_along(vars)) {
    ode_function <- paste0(ode_function, "    real ", vars[i], " = y[", i, "];\n")
  }
  }
  if(vars.init == 0 )
  {
    ode_function <- paste0(ode_function, "    real init[", length(vars), "] = {", paste(Ic, collapse = ", "), "};\n\n")
    for (i in seq_along(vars)) {
      ode_function <- paste0(ode_function, "    real ", vars[i], " = y[", i, "]+init[", i, "];\n")
    }
  }
  
  ode_function <- paste0(ode_function,"\n", ode_system)
  
  # Generate the full Stan code
  stan_code <- paste0("functions {", ode_function, "
}
  
data {
    int<lower=1> n_days;
    int<lower=0> nfst_days;")
    if (vars.init == 1)
    {
    stan_code <- paste0(stan_code, "real y0[", length(vars), "];")
    }
    stan_code <- paste0(stan_code,"real t0;
    real ts[n_days + nfst_days];
    int cases[n_days];\n")
  for (i in seq_along(params_integer)) {
    stan_code <- paste0(stan_code,"    int ",params_integer[i],";\n")
  }
  for (i in seq_along(params_real)) {
    stan_code <- paste0(stan_code,"    real ",params_real[i],";\n")
  }
  stan_code <- paste0(stan_code,"}
  
transformed data {\n")
  if(length(params_real)!=0)
  {
  stan_code <- paste0(stan_code, "    real x_r[",length(params_real),"] = ",params_real_set_formatted,";\n")
  }
  else
  {
    stan_code <- paste0(stan_code, "    real x_r[",length(params_real),"];\n")
  }
  if(length(params_integer)!=0)
  {
  stan_code <- paste0(stan_code, "    int x_i[",length(params_integer),"] = ",params_integer_set_formatted,";\n")
  }
  else
  {
    stan_code <- paste0(stan_code, "    int x_i[",length(params_integer),"];\n")
  }
  stan_code <- paste0(stan_code,"
  }")
  
  if (errstrc == 1) {
    # Find the indices of non-fixed parameters
    non_fixed_indices <- which(paramsfix == 0)
    
    stan_code <- paste0(stan_code, "
parameters {\n", 
                        paste0("    real<", sapply(non_fixed_indices, function(i) {
                          # Construct lower and upper bound names
                          LB_name <- paste0("params", i, "_LB")
                          UB_name <- paste0("params", i, "_UB")
                          
                          # Retrieve their values, with default of NA if not set
                          LB_value <- ifelse(exists(LB_name), get(LB_name), NA)
                          UB_value <- ifelse(exists(UB_name), get(UB_name), NA)
                          
                          # Construct the lower and upper bounds conditionally
                          bounds <- c()
                          if (!is.na(LB_value)) bounds <- c(bounds, paste0("lower=", LB_value))
                          if (!is.na(UB_value)) bounds <- c(bounds, paste0("upper=", UB_value))
                          
                          # Join the bounds with commas if both are present
                          paste(bounds, collapse = ", ")
                        }), "> ", params[paramsfix == 0], ";\n", collapse = ""), 
                        "    real<lower=0> phi_inv;
}
    
transformed parameters {
    real y[n_days + nfst_days, ", length(vars), "];
    real phi = 1. / phi_inv;\n
    {
    real theta[", length(params[paramsfix == 0]), "];
    ", paste0("    theta[", 1:length(params[paramsfix == 0]), "] = ", params[paramsfix == 0], ";\n", collapse = "")) 
    if (vars.init == 1) {
      stan_code <- paste0(stan_code, "y = integrate_ode_rk45(ode, y0, t0, ts, theta, x_r, x_i);")
    }
    if (vars.init == 0) {
      stan_code <- paste0(stan_code, "y = integrate_ode_rk45(ode, rep_array(0.0, ", length(vars), "), t0, ts, theta, x_r, x_i);")
    }
    stan_code <- paste0(stan_code, "
 }
}
    
model {
    ", prior_assignments, "  phi_inv ~")  
    
    stan_code <- paste0(stan_code, negbinerror_prior, ";
  for (t in 1:n_days) {
    cases[t] ~ neg_binomial_2(fmax(1e-6,", fitting, "), phi);
  }
}
    
generated quantities {
    real pred_cases[n_days + nfst_days];
    for (t in 1:n_days + nfst_days) {
        pred_cases[t] = neg_binomial_2_rng(fmax(1e-6,", fitting, "), phi);
    }
")
if (length(composite_expressions) > 0) {
stan_code <- paste0(stan_code,"
    // Composite quantities
    ", paste0("    real ", names(composite_expressions), " = ", 
              sapply(composite_expressions, function(expr) expr), 
              ";\n", collapse = ""),)}
    stan_code<-paste0(stan_code,"}")
}


if (errstrc == 2) {
  # Find the indices of non-fixed parameters
  non_fixed_indices <- which(paramsfix == 0)
  
  stan_code <- paste0(stan_code, "
parameters {\n", 
                      paste0("    real<", sapply(non_fixed_indices, function(i) {
                        # Construct lower and upper bound names
                        LB_name <- paste0("params", i, "_LB")
                        UB_name <- paste0("params", i, "_UB")
                        
                        # Retrieve their values, with default of NA if not set
                        LB_value <- ifelse(exists(LB_name), get(LB_name), NA)
                        UB_value <- ifelse(exists(UB_name), get(UB_name), NA)
                        
                        # Construct the lower and upper bounds conditionally
                        bounds <- c()
                        if (!is.na(LB_value)) bounds <- c(bounds, paste0("lower=", LB_value))
                        if (!is.na(UB_value)) bounds <- c(bounds, paste0("upper=", UB_value))
                        
                        # Join the bounds with commas if both are present
                        paste(bounds, collapse = ", ")
                      }), "> ", params[paramsfix == 0], ";\n", collapse = ""), 
                      "    real<lower=0> sigma;
}
    
transformed parameters {
    real y[n_days + nfst_days, ", length(vars), "];
    {
    real theta[", length(params[paramsfix == 0]), "];
    ", paste0("    theta[", 1:length(params[paramsfix == 0]), "] = ", params[paramsfix == 0], ";\n", collapse = "")) 
  if (vars.init == 1)
  {
    stan_code <- paste0(stan_code,"y = integrate_ode_rk45(ode, y0, t0, ts, theta, x_r, x_i);")
  }
  if(vars.init == 0)
  {
    stan_code <- paste0(stan_code,"y = integrate_ode_rk45(ode, rep_array(0.0, ",length(vars),"), t0, ts, theta, x_r, x_i);")
  }
  stan_code <- paste0(stan_code,"
 }
}
    
model {
", prior_assignments, "
    sigma ~", normalerror_prior, ";
  for (t in 1:n_days) {
    cases[t] ~ normal(fmax(1e-6,", fitting, "), sigma);
 }
}
    
generated quantities {
    real pred_cases[n_days+nfst_days];
    for (t in 1:n_days+nfst_days) {
        pred_cases[t] = normal_rng(fmax(1e-6, ", fitting, "), sigma);
    }
")
  if (length(composite_expressions) > 0) {
    stan_code <- paste0(stan_code,"
    // Composite quantities
    ", paste0("    real ", names(composite_expressions), " = ", 
              sapply(composite_expressions, function(expr) expr), 
              ";\n", collapse = ""))}
stan_code<-paste0(stan_code,"}")
}
  
if (errstrc == 3) {
  # Find the indices of non-fixed parameters
  non_fixed_indices <- which(paramsfix == 0)
  
  stan_code <- paste0(stan_code, "
parameters {\n", 
                      paste0("    real<", sapply(non_fixed_indices, function(i) {
                        # Construct lower and upper bound names
                        LB_name <- paste0("params", i, "_LB")
                        UB_name <- ifelse(exists(paste0("params", i, "_UB")), paste0("params", i, "_UB"), NA)
                        
                        # Retrieve their values, with default of NA if not set
                        LB_value <- ifelse(exists(LB_name), get(LB_name), NA)
                        UB_value <- ifelse(exists(UB_name), get(UB_name), NA)
                        
                        # Construct the lower and upper bounds conditionally
                        bounds <- c()
                        if (!is.na(LB_value)) bounds <- c(bounds, paste0("lower=", LB_value))
                        if (!is.na(UB_value)) bounds <- c(bounds, paste0("upper=", UB_value))
                        
                        # Join the bounds with commas if both are present
                        paste(bounds, collapse = ", ")
                      }), "> ", params[paramsfix == 0], ";\n", collapse = ""), 
                      "\n }
    
transformed parameters {
    real y[n_days + nfst_days, ", length(vars), "];
    {
    real theta[", length(params[paramsfix == 0]), "];
    ", paste0("    theta[", 1:length(params[paramsfix == 0]), "] = ", params[paramsfix == 0], ";\n", collapse = ""))
  
  if (vars.init == 1) {
    stan_code <- paste0(stan_code, "y = integrate_ode_rk45(ode, y0, t0, ts, theta, x_r, x_i);")
  }
  
  if (vars.init == 0) {
    stan_code <- paste0(stan_code, "y = integrate_ode_rk45(ode, rep_array(0.0, ", length(vars), "), t0, ts, theta, x_r, x_i);")
  }
  
  stan_code <- paste0(stan_code, "
 }
}
    
model {
    ", prior_assignments, "
  for (t in 1:n_days) {
    cases[t] ~ poisson(fmax(1e-6,", fitting, "));
 }
}
    
generated quantities {
    real pred_cases[n_days + nfst_days];
    for (t in 1:n_days + nfst_days) {
        pred_cases[t] = poisson_rng(fmax(1e-6,", fitting, "));
    }
")
if (length(composite_expressions) > 0) {
stan_code <- paste0(stan_code,"
    // Composite quantities
    ", paste0("    real ", names(composite_expressions), " = ", 
              sapply(composite_expressions, function(expr) expr), 
              ";\n", collapse = ""))}
  stan_code<-paste0(stan_code,"}")
}

  
  # Write the Stan code to a file
  writeLines(stan_code, stan_file)
  
  # Return the file name
  return(stan_code)
}
