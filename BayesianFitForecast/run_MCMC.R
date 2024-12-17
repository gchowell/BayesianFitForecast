rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readxl)
library(rstan)
source("options_SEIR_sanfrancisco_Ex1.R")
source("diff.R")
source("ode_rhs.R")
source("stancreator.R")
# Define the file name
stan_file <- "ode_model.stan"
# Write the Stan code to a file
stan_code <- generate_stan_file()
writeLines(stan_code, stan_file)
Mydata <- read_excel(paste0(cadfilename1, ".xlsx"))
for (i in 1:length(fitting_index)) {
  assign(paste0("actualcases", i), Mydata[[paste0("cases", i)]])
}
model <- stan_model("ode_model.stan")
stan_code <- "ode_model.stan"
for(calibrationperiod in calibrationperiods){
  for (i in 1:length(fitting_index)) {
    assign(paste0("cases", i), as.integer(get(paste0("actualcases", i))[1:calibrationperiod]))
  }
  n_days <- length(get(paste0("cases", 1)))
  nfst_days <- forecastinghorizon
  t <- seq(1, n_days+nfst_days, by = 1)
  t0 = 0 
  y0 = Ic
  if(length(Ic) == 1) {
    y0 <- array(Ic[1], dim = 1)
  }
  
  
  for (i in 1:length(fitting_index)) {
    assign(paste0("cases_all", i), c(get(paste0("cases", i)), rep(NA, nfst_days)))
  }
  if(vars.init == 1)
  {
  data_ode <- list(n_days = n_days, nfst_days=nfst_days, y0 = y0, t0 = t0, ts = t)
  for (i in 1:length(fitting_index)) {
    data_ode[[paste0("cases", i)]] <- get(paste0("cases", i))
  }
  }
  if(vars.init == 0)
  {
    data_ode <- list(n_days = n_days, nfst_days=nfst_days, t0 = t0, ts = t)
    for (i in 1:length(fitting_index)) {
      data_ode[[paste0("cases", i)]] <- get(paste0("cases", i))
    }
  }
  for (i in seq_along(params)) {
    prior_name <- paste0("params", i, "_prior")
    prior_value <- get(prior_name)
    
    if (is.numeric(prior_value)) {
      data_ode[[params[i]]] <- prior_value  # Add parameter with its prior to ode_data
    }
  }
  fit_ode_model <- sampling(model,
                              data = data_ode,
                              iter = niter,
                              chains = num_chain, 
                              seed = 0)
  pars <- params[paramsfix == 0]
  if (errstrc == 1) {
    for (i in 1:length(fitting_index)) {
      pars <- c(pars, paste0("phi", i))     
    }
  } else if (errstrc == 2) {
    for (i in 1:length(fitting_index)) {
      pars <- c(pars, paste0("sigma", i))     
    }
  }
  s <- rstan::extract(fit_ode_model)
  param_samples <- list()
  for (param in pars) {
    param_samples[[param]] <- s[[param]]
  }
  for (i in 1:length(fitting_index)) {
    assign(paste0("pred_cases", i), s[[paste0("pred_cases", i)]])
  }
  
  for (i in 1:length(fitting_index)) {
    assign(paste0("phi", i, "_samples"), NULL)
  }
  for (i in 1:length(fitting_index)) {
    assign(paste0("sigma", i, "_samples"), NULL)
  }
  if (errstrc == 1) {
    for (i in 1:length(fitting_index)) {
      assign(paste0("phi", i, "_samples"), s[[paste0("phi", i)]])
      param_samples[[paste0("phi", i, "_samples")]] <- s[[paste0("phi", i)]]
    }
  } else if (errstrc == 2) {
    assign(paste0("sigma", i, "_samples"), s[[paste0("sigma", i)]])
    param_samples[[paste0("sigma", i, "_samples")]] <- s[[paste0("sigma", i)]]
  }
  composite_samples <- list()
  for (name in names(composite_expressions)) {
    composite_samples[[name]] <- s[[name]]
    }
  errorstructure <- c("negativebinomial", "normal", "poisson")
  pred_cases_list <- sapply(1:length(fitting_index), function(i) get(paste0("pred_cases", i)), simplify = FALSE)
  pred_cases <- abind::abind(pred_cases_list, along = 3)
  phi_samples <- sapply(1:length(fitting_index), function(i) get(paste0("phi", i, "_samples")))
  sigma_samples <- sapply(1:length(fitting_index), function(i) get(paste0("sigma", i, "_samples")))
  
  save(param_samples, pred_cases, fit_ode_model, pars, phi_samples, sigma_samples, 
   composite_expressions, composite_samples, file = paste(model_name, "cal", 
     calibrationperiod, "fcst", forecastinghorizon, errorstructure[errstrc], caddisease, "fit.Rdata", sep = "-"))
}