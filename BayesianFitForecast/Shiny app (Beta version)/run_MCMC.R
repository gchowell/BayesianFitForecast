  ###################
  # Needed packages #
  ###################
  library(readxl)
  library(rstan)
  
  ############################
  # Calling needed functions #
  ############################
  source("diff.R")
  source("ode_rhs.R")
  source("stancreator.R")
  
  
run_MCMC <- function(outputFile, data.temp){
  
  ###################################################
  # Calling the options file created in the UI code #
  ###################################################
  source(outputFile)
  
  # Renaming the options file
  options_file <- outputFile
  
  #################################################
  # Creating the temp file path for the stan file #
  #################################################
  stan_file <- file.path(tempdir(), "ode_model.stan")
  
  # Generating the STAN code 
  stan_code <- generate_stan_file(optionsFile = options_file)
  
  # Saving the Stan file to the temp directory 
  writeLines(stan_code, stan_file)
  
  #############################
  # Reading in the excel file #
  #############################
  Mydata <- data.temp
  
  #################################
  # List to store list of outputs #
  #################################
  store.outputs <- list()
  
  for (i in 1:length(fitting_index)) {
    assign(paste0("actualcases", i), Mydata[[paste0("cases", i)]])
  }
  
  #####################
  # Fitting the model #
  #####################
  model <- tryCatch({
    
   stan_model(stan_file)
   
  }, error = function(e){
    
    e$message
    
  })
  
  ################################
  # Returning an error if needed #
  ################################
  if(is.character(model)) {
    return(model)
  }

#######################################
# Looping through calibration periods #
#######################################
for(h in seq_along(calibrationperiods)){
  
  # Indexed calibration period
  calibrationperiod <- calibrationperiods[h]
  
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
  
#------------------------------------------------------------------------------#
# Creating the output list -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the output list that goes to the primary page.   #
# Its progress and resulting errors is then tracked within a pop-up on the     #
# dashboard's main page.                                                       #
#------------------------------------------------------------------------------#
  
  store.outputs[[h]] <- list(
    model = model,
    data = data_ode,
    iter = niter,
    chains = num_chain
  )
  
} # End of loop going through calibration periods

  #########################################
  # Returning the `fit_ode_model` outputs #
  #########################################
  return(store.outputs)

}
  
