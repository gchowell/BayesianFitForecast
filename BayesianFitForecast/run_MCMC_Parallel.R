rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(readxl)
library(rstan)
library(parallel)

source("options_seir_timedep_Ex7.R")
source("diff.R")
source("ode_rhs.R")
source("stancreator.R")

n_cores <- max(1, detectCores() - 1)
options(mc.cores = n_cores)
rstan_options(auto_write = TRUE)

stan_file <- "ode_model.stan"
stan_code <- generate_stan_file()
writeLines(stan_code, stan_file)

model <- stan_model(stan_file, 
                    verbose = TRUE,
                    auto_write = TRUE)

Mydata <- read_excel(paste0(cadfilename1, ".xlsx"))

actual_cases_list <- vector("list", length(fitting_index))
for (i in 1:length(fitting_index)) {
  actual_cases_list[[i]] <- Mydata[[paste0("cases", i)]]
}

setup_prior_data <- function(data_ode, params) {
  for (i in seq_along(params)) {
    prior_name <- paste0("params", i, "_prior")
    if (exists(prior_name)) {
      prior_value <- get(prior_name)
      if (is.numeric(prior_value)) {
        data_ode[[params[i]]] <- prior_value
      }
    }
  }
  return(data_ode)
}
for(calibrationperiod in calibrationperiods){
  
  cases_list <- lapply(actual_cases_list, function(x) as.integer(x[1:calibrationperiod]))
  
  n_days <- length(cases_list[[1]])
  nfst_days <- forecastinghorizon
  t <- seq(1, n_days + nfst_days, by = 1)
  t0 <- 0 
  y0 <- if(length(Ic) == 1) array(Ic[1], dim = 1) else Ic
  
  data_ode <- list(
    n_days = n_days, 
    nfst_days = nfst_days, 
    t0 = t0, 
    ts = t
  )
  
  if(vars.init == 1) {
    data_ode$y0 <- y0
  }
  
  for (i in 1:length(fitting_index)) {
    data_ode[[paste0("cases", i)]] <- cases_list[[i]]
  }
  
  data_ode <- setup_prior_data(data_ode, params)
  
  flush.console()
  
  fit_ode_model <- sampling(
    model,
    data = data_ode,
    iter = niter,
    chains = num_chain,
    cores = min(num_chain, n_cores),
    seed = 0,
    control = list(
      adapt_delta = 0.8,
      max_treedepth = 10
    )
  )
  
  pars <- params[paramsfix == 0]
  
  if (errstrc == 1) {
    pars <- c(pars, paste0("phi", 1:length(fitting_index)))
  } else if (errstrc == 2) {
    pars <- c(pars, paste0("sigma", 1:length(fitting_index)))
  }
  
  s <- rstan::extract(fit_ode_model)
  
  param_samples <- s[pars]
  
  pred_cases_list <- lapply(1:length(fitting_index), function(i) {
    s[[paste0("pred_cases", i)]]
  })
  pred_cases <- abind::abind(pred_cases_list, along = 3)
  
  phi_samples <- if(errstrc == 1) {
    sapply(1:length(fitting_index), function(i) s[[paste0("phi", i)]])
  } else NULL
  
  sigma_samples <- if(errstrc == 2) {
    sapply(1:length(fitting_index), function(i) s[[paste0("sigma", i)]])
  } else NULL
  
  composite_samples <- s[names(composite_expressions)]
  
  errorstructure <- c("negativebinomial", "normal", "poisson")
  filename <- paste(model_name, "cal", calibrationperiod, "fcst", 
                    forecastinghorizon, errorstructure[errstrc], 
                    caddisease, "fit.Rdata", sep = "-")
  
  save(param_samples, pred_cases, fit_ode_model, pars, 
       phi_samples, sigma_samples, composite_expressions, 
       composite_samples, file = filename, compress = TRUE)
  
  if(calibrationperiod != tail(calibrationperiods, 1)) {
    rm(fit_ode_model, s, param_samples, pred_cases_list, pred_cases)
    gc()
  }
}

gc()