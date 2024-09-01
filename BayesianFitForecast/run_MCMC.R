rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readxl)
library(rstan)
source("options_SEIR_sanfrancisco_Ex5.R")
source("diff.R")
source("ode_rhs.R")
source("stancreator.R")

# Define the file name
stan_file <- "ode_model.stan"
# Write the Stan code to a file
stan_code <- generate_stan_file()
writeLines(stan_code, stan_file)



Mydata <- read_excel(paste0(cadfilename1, ".xlsx"))
actualcases <- Mydata$cases
model <- stan_model("ode_model.stan")
stan_code <- "ode_model.stan"


for(calibrationperiod in calibrationperiods){
cases <- as.integer(actualcases[1:calibrationperiod])
# times
n_days <- length(cases)
nfst_days <- forecastinghorizon
t <- seq(1, n_days+nfst_days, by = 1)
t0 = 0 
#initial conditions
y0 = Ic
if(length(Ic) == 1) {
  y0 <- array(Ic[1], dim = 1)
}



cases_all=c(cases, rep(NA, nfst_days))

# Initial condition fixed or estimated
if(vars.init == 1)
{
data_ode <- list(n_days = n_days, nfst_days=nfst_days, y0 = y0, t0 = t0, ts = t, cases = cases)
}
if(vars.init == 0)
{
  data_ode <- list(n_days = n_days, nfst_days=nfst_days, t0 = t0, ts = t, cases = cases)
}
# Add constant parameters
for (i in seq_along(params)) {
  prior_name <- paste0("params", i, "_prior")
  prior_value <- get(prior_name)
  
  if (is.numeric(prior_value)) {
    data_ode[[params[i]]] <- prior_value  # Add parameter with its prior to ode_data
  }
}

#Fitting the model
fit_ode_model <- sampling(model,
                            data = data_ode,
                            iter = niter,
                            chains = num_chain, 
                            seed = 0)
# Define the parameters to be saved
pars <- params[paramsfix == 0]

# Append phi or sigma to pars based on the error structure
if (errstrc == 1) {
  pars <- c(pars, "phi")
} else if (errstrc == 2) {
  pars <- c(pars, "sigma")
}

# Extract samples from the fitted model
s <- rstan::extract(fit_ode_model)

# Initialize a list to store posterior samples for individual parameters
param_samples <- list()

# Extract each parameter's posterior samples
for (param in pars) {
  param_samples[[param]] <- s[[param]]
}
pred_cases <- s$pred_cases
phi_samples <- NULL
sigma_samples <- NULL

# Handle different error structures
if (errstrc == 1) {
  phi_samples <- s$phi
  param_samples$phi <- phi_samples
} else if (errstrc == 2) {
  sigma_samples <- s$sigma
  param_samples$sigma <- sigma_samples
}

# Initialize a list to store the computed composite samples
composite_samples <- list()

# Evaluate composite expressions
for (name in names(composite_expressions)) {
  composite_samples[[name]] <- s[[name]]
  }


# Error structure names
errorstructure <- c("negativebinomial", "normal", "poisson")

# Save everything, including composite expression samples
save(param_samples, pred_cases, fit_ode_model, pars, phi_samples, sigma_samples,
     composite_expressions, composite_samples, file = paste(model_name, "cal", calibrationperiod,  
                            "fcst",forecastinghorizon,errorstructure[errstrc], caddisease, "fit.Rdata", sep = "-"))
}
