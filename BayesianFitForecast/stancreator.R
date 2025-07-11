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
  # Function: Find parameters in a time-dependent parameter
  extract_used_params <- function(template) {
    matches <- gregexpr("params[0-9]+", template)[[1]]
    if (matches[1] == -1) return(character(0))
    used <- regmatches(template, gregexpr("params[0-9]+", template))[[1]]
    unique(used)
  }
  
  # Function: Map to real parameter names
  get_used_param_names <- function(used_placeholders, all_param_names) {
    indices <- as.integer(sub("params", "", used_placeholders))
    all_param_names[indices]
  }
  
  # Function: Substitute parameters with their real name in the time-dep
  replace_placeholders <- function(template, used_placeholders, param_names) {
    for (i in seq_along(used_placeholders)) {
      template <- gsub(used_placeholders[i], param_names[i], template, fixed = TRUE)
    }
    template
  }
  
  expand_time_dependent_calls <- function(ode_code, fname, used_param_names) {
    expanded_call <- paste0(fname, "(t, ", paste(used_param_names, collapse = ", "), ")")
    gsub(fname, expanded_call, ode_code, fixed = TRUE)
  }
  
  ##############################################################################
  fitting <- vector("list", length(fitting_index))  # Initialize an empty list to store the fitting results
  
  # Iterate through the fitting_index array
  for (j in seq_along(fitting_index)) {
    
    # Check the corresponding value of fitting_diff
    if (fitting_diff[j] == 1) {
      # Use ode_rhs if fitting_diff is 1
      fitting[[j]] <- ode_rhs(fitting_index[j], ode_system)
    } else {
      # Otherwise, use the vars value for the fitting index
      fitting[[j]] <- vars[fitting_index[j]]
    }
    
    # Now, substitute variables in fitting
    for (i in seq_along(vars)) {
      fitting[[j]] <- gsub(paste0("\\b", vars[i], "\\b"), paste0("y[t,", i, "]"), fitting[[j]])
    }
    # Replace time_dependent_paramX with correct form
    for (td_name in names(time_dependent_templates)) {
      if (grepl(td_name, fitting[[j]], fixed = TRUE)) {
        template <- time_dependent_templates[[td_name]]
        used_placeholders <- extract_used_params(template)
        used_param_names <- get_used_param_names(used_placeholders, params)
        fitting[[j]] <- expand_time_dependent_calls(fitting[[j]], td_name, used_param_names)
      }
    }
  }
  
  ##############################################################################
  # Generate the ODE function for Stan
  ode_function <- "
    array[] real ode(real t, array[] real y, array[] real theta, array[] real x_r, array[] int x_i) { \n"
  
  
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
  
 
  generated_functions <- c()
  
  for (fname in names(time_dependent_templates)) {
    template <- time_dependent_templates[[fname]]
    
    used_placeholders <- extract_used_params(template)
    used_param_names <- get_used_param_names(used_placeholders, params)
    function_body <- replace_placeholders(template, used_placeholders, used_param_names)
    
    function_header <- paste0(
      "real ", fname, "(real t, ",
      paste(paste0("real ", used_param_names), collapse = ", "),
      ") {"
    )
    full_function <- paste(function_header, function_body, "}", sep = "\n")
    generated_functions <- c(generated_functions, full_function)
    ode_function <- expand_time_dependent_calls(ode_function, fname, used_param_names)
  }
  
  # Generate the full Stan code
  stan_code <- paste("functions {", paste(generated_functions, collapse = "\n\n"), ode_function, "}", sep = "\n\n")
  stan_code <- paste0(stan_code,"
  
data {
    int<lower=1> n_days;
    int<lower=0> nfst_days;")
    if (vars.init == 1)
    {
    stan_code <- paste0(stan_code, "array[", length(vars), "] real y0;\n")
    }
   stan_code <- paste0(stan_code, "real t0;\narray[n_days + nfst_days] real ts;\n")
  
  for (i in 1:length(fitting_index)) {
    stan_code <- paste0(stan_code, "array[n_days] int cases", i, ";\n")
  }
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
  stan_code <- paste0(stan_code, "    array[", length(params_real), "] real x_r = ", params_real_set_formatted, ";\n")
  }
  else
  {
    stan_code <- paste0(stan_code, "    array[", length(params_real), "] real x_r;\n")
  }
  if(length(params_integer)!=0)
  {
  stan_code <- paste0(stan_code, "    array[", length(params_integer), "] int x_i = ", params_integer_set_formatted, ";\n")
  }
  else
  {
    stan_code <- paste0(stan_code, "    array[", length(params_integer), "] int x_i;\n")
  }
  stan_code <- paste0(stan_code,"
  }")
  
  if (errstrc == 1) {
    non_fixed_indices <- which(paramsfix == 0)
    
    stan_code <- paste0(stan_code, "parameters {\n",
                        paste0(
                          "    real<",
                          sapply(non_fixed_indices, function(i) {
                            LB_name <- paste0("params", i, "_LB")
                            UB_name <- paste0("params", i, "_UB")
                            LB_value <- ifelse(exists(LB_name), get(LB_name), NA)
                            UB_value <- ifelse(exists(UB_name), get(UB_name), NA)
                            bounds <- c()
                            if (!is.na(LB_value)) bounds <- c(bounds, paste0("lower=", LB_value))
                            if (!is.na(UB_value)) bounds <- c(bounds, paste0("upper=", UB_value))
                            paste(bounds, collapse = ", ")
                          }),
                          "> ", params[paramsfix == 0], ";\n",
                          collapse = ""
                        ),
                        paste0("    real<lower=0> phi_inv", 1:length(fitting_index), ";\n", collapse = ""),
                        "}\ntransformed parameters {\n  array[n_days + nfst_days, ", length(vars), "] real y;\n",
                        paste0("  real phi", 1:length(fitting_index), " = 1.0 / phi_inv", 1:length(fitting_index), ";\n", collapse = ""),
                        "  array[", length(params[paramsfix == 0]), "] real theta;\n",
                        paste0(
                          "  theta[", 1:length(params[paramsfix == 0]), "] = ", params[paramsfix == 0], ";\n",
                          collapse = ""
                        ))
    
    if (vars.init == 1) {
      stan_code <- paste0(stan_code, "\n  y = integrate_ode_rk45(ode, y0, t0, ts, theta, x_r, x_i);\n")
    } else if (vars.init == 0) {
      stan_code <- paste0(stan_code, "\n  y = integrate_ode_rk45(ode, rep_array(0.0, ", length(vars), "), t0, ts, theta, x_r, x_i);\n")
    }
    
    stan_code <- paste0(stan_code, "}\nmodel {\n", prior_assignments, "\n")
    
    stan_code <- paste0(stan_code,
                        paste0("  phi_inv", 1:length(fitting_index), " ~ ",
                               sapply(1:length(fitting_index), function(i) {
                                 get(paste0("negbinerror", i, "_prior"))
                               }), ";\n", collapse = ""),
                        "\n  for (t in 1:n_days) {\n")
    
    stan_code <- paste0(stan_code,
                        paste0("    cases", 1:length(fitting_index), "[t] ~ neg_binomial_2(fmax(1e-6, ", fitting[1:length(fitting_index)], "), phi", 1:length(fitting_index), ");\n", collapse = ""),
                        "  }\n}\n\n")
    
    stan_code <- paste0(stan_code, "generated quantities {\n",
                        paste0("array[n_days + nfst_days] real pred_cases", 1:length(fitting_index), ";\n", collapse = ""),
                        "  for (t in 1:(n_days + nfst_days)) {\n",
                        paste0("    pred_cases", 1:length(fitting_index), "[t] = neg_binomial_2_rng(fmax(1e-6, ", fitting[1:length(fitting_index)], "), phi", 1:length(fitting_index), ");\n", collapse = ""),
                        "  }\n")
    
    if (length(composite_expressions) > 0) {
      stan_code <- paste0(stan_code, "\n",
                          paste0("  real ", names(composite_expressions), " = ",
                                 sapply(composite_expressions, function(expr) expr), ";\n", collapse = ""))
    }
    
    stan_code <- paste0(stan_code, "}\n")
  }
  


if (errstrc == 2) {
  non_fixed_indices <- which(paramsfix == 0)
  
  stan_code <- paste0(stan_code, "parameters {\n", 
  paste0(
    "    real<",
    sapply(non_fixed_indices, function(i) {
      LB_name <- paste0("params", i, "_LB")
      UB_name <- paste0("params", i, "_UB")
      LB_value <- ifelse(exists(LB_name), get(LB_name), NA)
      UB_value <- ifelse(exists(UB_name), get(UB_name), NA)
      bounds <- c()
      if (!is.na(LB_value)) bounds <- c(bounds, paste0("lower=", LB_value))
      if (!is.na(UB_value)) bounds <- c(bounds, paste0("upper=", UB_value))
      paste(bounds, collapse = ", ")
    }),
    "> ", params[paramsfix == 0], ";\n",
    collapse = ""
  ),
  paste0("    real<lower=0> sigma", 1:length(fitting_index), ";\n", collapse = ""),
  "}\ntransformed parameters {\n  array[n_days + nfst_days, ", length(vars), "] real y;\n  array[", length(params[paramsfix == 0]), "] real theta;\n",
  paste0(
    "  theta[", 1:length(params[paramsfix == 0]), "] = ", params[paramsfix == 0], ";\n",
    collapse = ""
  )
)

  
  if (vars.init == 1) {
    stan_code <- paste0(stan_code, "\n  y = integrate_ode_rk45(ode, y0, t0, ts, theta, x_r, x_i);\n")
  } else if (vars.init == 0) {
    stan_code <- paste0(stan_code, "\n  y = integrate_ode_rk45(ode, rep_array(0.0, ", length(vars), "), t0, ts, theta, x_r, x_i);\n")
  }
  
  stan_code <- paste0(stan_code, "}\nmodel {\n", prior_assignments, "\n")
  
  stan_code <- paste0(stan_code,
                      paste0("  sigma", 1:length(fitting_index), " ~ ",
                             sapply(1:length(fitting_index), function(i) {
                               get(paste0("normalerror", i, "_prior"))
                             }), ";\n", collapse = ""),
                      "\n  for (t in 1:n_days) {\n")
  
  stan_code <- paste0(stan_code,
                      paste0("    cases", 1:length(fitting_index), "[t] ~ normal(fmax(1e-6, ", fitting[1:length(fitting_index)], "), sigma", 1:length(fitting_index), ");\n", collapse = ""),
                      "  }\n}\n\n")
  
  stan_code <- paste0(stan_code, "generated quantities {\n",
                      paste0("array[n_days + nfst_days] real pred_cases", 1:length(fitting_index), ";\n", collapse = ""),
                      "  for (t in 1:(n_days + nfst_days)) {\n",
                      paste0("    pred_cases", 1:length(fitting_index), "[t] = normal_rng(fmax(1e-6, ", fitting[1:length(fitting_index)], "), sigma", 1:length(fitting_index), ");\n", collapse = ""),
                      "  }\n")
  
  if (length(composite_expressions) > 0) {
  stan_code <- paste0(stan_code, "\n",
                      paste0("  real ", names(composite_expressions), " = ",
                             sapply(composite_expressions, function(expr) expr), ";\n", collapse = ""))
}

  
  stan_code <- paste0(stan_code, "}\n")
}

  
  if (errstrc == 3) {
    non_fixed_indices <- which(paramsfix == 0)
    
    stan_code <- paste0(stan_code, "parameters {\n",
                        paste0(
                          "    real<",
                          sapply(non_fixed_indices, function(i) {
                            LB_name <- paste0("params", i, "_LB")
                            UB_var <- paste0("params", i, "_UB")
                            UB_name <- ifelse(exists(UB_var), UB_var, NA)
                            
                            LB_value <- ifelse(exists(LB_name), get(LB_name), NA)
                            UB_value <- ifelse(!is.na(UB_name) && exists(UB_name), get(UB_name), NA)
                            
                            bounds <- c()
                            if (!is.na(LB_value)) bounds <- c(bounds, paste0("lower=", LB_value))
                            if (!is.na(UB_value)) bounds <- c(bounds, paste0("upper=", UB_value))
                            paste(bounds, collapse = ", ")
                          }),
                          "> ", params[paramsfix == 0], ";\n",
                          collapse = ""
                        ),
                        "}\ntransformed parameters {\n  array[n_days + nfst_days, ", length(vars), "] real y;\n  {\n    array[", length(params[paramsfix == 0]), "] real theta;\n",
                        paste0(
                          "    theta[", 1:length(params[paramsfix == 0]), "] = ", params[paramsfix == 0], ";\n",
                          collapse = ""
                        ))
    
    if (vars.init == 1) {
      stan_code <- paste0(stan_code, "    y = integrate_ode_rk45(ode, y0, t0, ts, theta, x_r, x_i);\n")
    } else if (vars.init == 0) {
      stan_code <- paste0(stan_code, "    y = integrate_ode_rk45(ode, rep_array(0.0, ", length(vars), "), t0, ts, theta, x_r, x_i);\n")
    }
    
    stan_code <- paste0(stan_code, "  }\n}\nmodel {\n", prior_assignments, "\n  for (t in 1:n_days) {\n")
    
    stan_code <- paste0(stan_code,
                        paste0("    cases", 1:length(fitting_index), "[t] ~ poisson(fmax(1e-6, ", fitting[1:length(fitting_index)], "));\n", collapse = ""),
                        "  }\n}\n\n")
    
    stan_code <- paste0(stan_code, "generated quantities {\n",
                        paste0("  array[n_days + nfst_days] real pred_cases", 1:length(fitting_index), ";\n", collapse = ""),
                        "  for (t in 1:(n_days + nfst_days)) {\n",
                        paste0("    pred_cases", 1:length(fitting_index), "[t] = poisson_rng(fmax(1e-6, ", fitting[1:length(fitting_index)], "));\n", collapse = ""),
                        "  }\n")
    
    if (length(composite_expressions) > 0) {
      stan_code <- paste0(stan_code, "\n",
                          paste0("  real ", names(composite_expressions), " = ",
                                 sapply(composite_expressions, function(expr) expr), ";\n", collapse = ""))
    }
    
    stan_code <- paste0(stan_code, "}\n")
  }
  
  
  # Write the Stan code to a file
  writeLines(stan_code, stan_file)
  
  # Return the file name
  return(stan_code)
}
