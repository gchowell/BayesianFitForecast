#------------------------------------------------------------------------------#
# Needed packages --------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section calls the needed R-packages and source files to be able  #
# to run the code included below.                                              #
#------------------------------------------------------------------------------#

  ###################
  # Needed packages #
  ###################
  library(bayesplot)
  library("readxl")
  library(xlsx)
  library(readxl)
  library(openxlsx)
  library(rstan)
  library(ggplot2)
  
  #######################
  # Needed Source Files #
  #######################
  source("Metric functions.R")


#------------------------------------------------------------------------------#
# Function: Obtaining Output for Bayesian Models -------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the function that outputs the needed analytic    #
# materials from the RStan results. This includes both figures and data sets.  #
# The results are saved in a list, and then exported to the primary dashboard. #
#------------------------------------------------------------------------------#
run_analyzeResults <- function(optionsFile, MCMCOutput, data.temp){
  

#------------------------------------------------------------------------------#
# Calling, and renaming (when needed) function inputs --------------------------
#------------------------------------------------------------------------------#
# About: This section calls, unpacks, and renames the function inputs to be    #
# used throughout the rest of the function. For example, it takes the list of  #
# outputs received from the `run_MCMC` function, and saves each element as its #
# own list.                                                                    #
#------------------------------------------------------------------------------#
  
  ##############################################
  # Calling the options file created in the UI #
  ##############################################
  source(optionsFile)

  ##################################
  # Unpacking the MCMC output list #
  ##################################
  
  # `Param_samples` list
  param_samples <- MCMCOutput[[1]]
  
  # `Pred_Cases` list
  pred_cases <- MCMCOutput[[2]]
  
  # ODE Model fit
  fit_ode_model <- MCMCOutput[[3]]
  
  # `Pars` list
  pars <- MCMCOutput[[4]]
  
  # `Phi_Samples` list
  phi_samples <- MCMCOutput[[5]]
  
  # `Sigma_Samples` list
  sigma_samples <- MCMCOutput[[6]]
  
  # `Composite_Expressions` list
  composite_expressions <- MCMCOutput[[7]]
  
  # `Composite_Samples` list
  composite_samples <- MCMCOutput[[8]]
  
  ########################################################
  # Creating the list for calibration period information #
  ########################################################
  calibration.period.list <- list()
  
  
#------------------------------------------------------------------------------#
# Reading in data, and preparing for later -------------------------------------
#------------------------------------------------------------------------------#
# About: This section reads in the data selected in the UI, and prepares the   #
# user-selected inputs to facilitate code later on in the function.            #
#------------------------------------------------------------------------------#
  
  ############################################
  # Reading in the excel file: Original Data #
  ############################################
  Mydata <- data.temp
  
  # Pulling the data needed for each fitting index
  for (i in 1:length(fitting_index)) {
    
    assign(paste0("actualcases", i), Mydata[[paste0("cases", i)]])
    
  }
  
  ####################################################
  # Creating a vector of the error structure options #
  ####################################################
  errorstructure <- c("negativebinomial","normal","poisson")
  
  #########################################
  # Empty data frames to later store data #
  #########################################
  result_data1 <- data.frame()
  
  result_data2 <- data.frame()
  
  ################################
  # Array of calibration periods #
  ################################
  calibrationperiodLIST <- calibrationperiods
  
#------------------------------------------------------------------------------#
# Obtaining Output: Unique for each calibration period -------------------------
#------------------------------------------------------------------------------#
# About: This section loops through each user selected calibration period to   #
# produce various forms of output, included histograms, trace plots, and       #
# various excel files.                                                         #
#------------------------------------------------------------------------------#
 
  ################################
  # Empty vector to store output #
  ################################
  calibration.period.list <- c()
  
  #######################################
  # Looping through calibration periods #
  #######################################
  for(calibrationperiod in calibrationperiods){ 
    
    # Indexed calibration period
    index <- which(calibrationperiodLIST == calibrationperiod)

    #################################################################
    # Looping through data to pull various pieces from RStan output #
    #################################################################
    for (i in 1:length(fitting_index)) {
      
      assign(paste0("my_solutionaggr", i), pred_cases[,,i])
      assign(paste0("medcurve", i), apply(get(paste0("my_solutionaggr", i)), 2, median))
      assign(paste0("mcmc_intervals_aggr", i), apply(get(paste0("my_solutionaggr", i)), 2, quantile, probs = c(0.025, 0.975)))
    }
    
#------------------------------------------------------------------------------#
# Plotting the Histograms ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section plots the parameter histograms. If there is an issue     #
# where no samples to be plotted for a given parameter, an error will show to  #
# the user.                                                                    # 
#------------------------------------------------------------------------------#
    
    #################################
    # Unpacking the parameters list #
    #################################
    pars <- unlist(pars)
    
    ###########################################
    # Initialize empty lists to store results #
    ###########################################
    mcmc_intervals <- list()
    medians <- numeric(length(pars))
    means <- numeric(length(pars))
    lower_bounds <- numeric(length(pars))
    upper_bounds <- numeric(length(pars))
    
    #######################
    # List to store plots #
    #######################
    # Create the list dynamically
    assign(paste0("plot.list.params.", calibrationperiod), list())
    
    # Reference the list name
    plot_list_name <- paste0("plot.list.params.", calibrationperiod)
    
    ###############################
    # Loop through each parameter #
    ###############################
    for (i in seq_along(pars)) {
      
      # Extract samples for the current parameter
      current_samples <- param_samples[[pars[i]]]
      
      ######################################
      # Check if there are samples to plot #
      ######################################
      if(length(current_samples) == 0) {
        
        # Retrieve the dynamically created list
        temp_list <- get(plot_list_name)
        
        # Add the error message to the list
        temp_list[[i]] <- paste0("No samples for parameter: ", pars[i])
        
        # Add a name to the corresponding element
        names(temp_list)[i] <- "ERROR"
        
        # Update the dynamically created list in the global environment
        assign(plot_list_name, temp_list)
        
        # Skip to the next loop iteration
        next
        
      }
      
      ##################################################
      # Calculate summary statistics for the histogram #
      ##################################################
      
      # Calculating the quantiles from the current sample 
      mcmc_intervals[[i]] <- quantile(current_samples, prob = c(0.025, 0.975))
      
      # Calculating the median of the sample 
      medians[i] <- round(median(current_samples), digits = 2)
      
      # Calculating the mean of the sample 
      means[i] <- round(mean(current_samples), digits = 2)
      
      # Calculating the lower bound of the sample 
      lower_bounds[i] <- round(mcmc_intervals[[i]][1], digits = 2)
      
      # Calculating the upper bound of the sample 
      upper_bounds[i] <- round(mcmc_intervals[[i]][2], digits = 2)
      
      ##########################
      # Plotting the histogram #
      ##########################
      hist(current_samples,
           main = paste("Median:", medians[i], "Mean:", means[i], "95% CI:",
                        lower_bounds[i], ", ", upper_bounds[i]),
           xlab = pars[i],
           ylab = "Frequency",
           col = "lightblue",
           border = "black")
                        
      
      ##################################
      # Saving the results to the list #
      ##################################
      
      # Retrieving the list 
      temp_list <- get(plot_list_name) 
      
      # Saving the plot 
      temp_list[[i]] <- recordPlot()
      
      # Adding the name
      names(temp_list)[i] <- pars[i]
      
      # Update the list in the global environment
      assign(plot_list_name, temp_list)
      
      # Explicitly close the plot object
      #dev.off() 
      
    }
    
#------------------------------------------------------------------------------#
# Plotting the composite sample histograms -------------------------------------
#------------------------------------------------------------------------------#
# About: This section plots the composite sample histograms. If there is an    #
# issue where no samples to be plotted for a given composite sample, an        #
# error will show to the user.                                                 #
#------------------------------------------------------------------------------#
    
    #################################################################
    # Initialize empty lists to store results for composite samples #
    #################################################################
    composite_mcmc_intervals <- list()
    composite_medians <- numeric(length(composite_samples))
    composite_means <- numeric(length(composite_samples))
    composite_lower_bounds <- numeric(length(composite_samples))
    composite_upper_bounds <- numeric(length(composite_samples))
    
    #######################
    # List to store plots #
    #######################
    
    # Create the list dynamically
    assign(paste0("plot.list.composite.", calibrationperiod), list())
    
    # Reference the list name
    plot_list_name <- paste0("plot.list.composite.", calibrationperiod)
    
    ##########################################
    # Loop through each composite expression #
    ##########################################
    for(i in 1:length(composite_expressions)) {
      
      # Indexing the name
      name <- names(composite_expressions)[i]
      
      # Extract samples for the current composite expression
      current_samples <- composite_samples[[name]]
      
      ######################################
      # Check if there are samples to plot #
      ######################################
      if(length(current_samples) == 0) {
        
        # Retrieve the dynamically created list
        temp_list <- get(plot_list_name)
        
        # Add the error message to the list
        temp_list[[i]] <- paste0("No samples for composite expression: ", name)
        
        # Add a name to the corresponding element
        names(temp_list)[i] <- "ERROR"
        
        # Update the dynamically created list in the global environment
        assign(plot_list_name, temp_list)
        
        # Skip to the next loop iteration
        next
        
      }
      
      ###################################################
      # Calculate summary statistics for the histograms #
      ###################################################
      
      # Calculating the quantiles from the current sample 
      mcmc_intervals <- quantile(current_samples, prob = c(0.025, 0.975))
      
      # Calculating the median of the sample
      composite_medians <- round(median(current_samples), digits = 2)
      
      # Calculating the mean of the sample
      composite_means <- round(mean(current_samples), digits = 2)
      
      # Calculating the lower bound of the sample
      composite_lower_bounds <- round(mcmc_intervals[1], digits = 2)
      
      # Calculating the upper bound of the sample
      composite_upper_bounds <- round(mcmc_intervals[2], digits = 2)
      
      ##########################
      # Plotting the histogram #
      ##########################
      hist(current_samples, 
           main = paste(name, "- Median:", composite_medians, "Mean:", composite_means, "95% CI:",
                        composite_lower_bounds, ", ", composite_upper_bounds), 
           xlab = name, 
           ylab = "Frequency", 
           col = "lightblue", 
           border = "black")
      
      ##################################
      # Saving the results to the list #
      ##################################
      
      # Retrieving the list 
      temp_list <- get(plot_list_name) 
      
      # Saving the plot 
      temp_list[[i]] <- recordPlot()
      
      # Adding the name
      names(temp_list)[i] <- name
      
      # Update the list in the global environment
      assign(plot_list_name, temp_list)
      
      # Explicitly close the plot object
      #dev.off() 
      
    }
    
#------------------------------------------------------------------------------#
# Plotting the Trace Plots -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section plots the trace plots obtained from the model fit, and   #
# saves the plot within the list based on its calibration period.              #
#------------------------------------------------------------------------------#
    
    #######################
    # List to store plots #
    #######################
    
    # Create the list dynamically
    assign(paste0("plot.list.trace.", calibrationperiod), NULL)
    
    # Reference the list name
    plot_list_name <- paste0("plot.list.trace.", calibrationperiod)
    
    ##########################
    # Creating the traceplot #
    ##########################
    trace.plot <- traceplot(fit_ode_model, pars = pars)
    
    ##########################################
    # Saving the results to the empty vector #
    ##########################################
    
    # Retrieving the temp plot 
    temp_plot <- get(plot_list_name) 
    
    # Saving the plot 
    temp_plot <- trace.plot
    
    # Update the temp plot in the global environment
    assign(plot_list_name, temp_plot)
    
#------------------------------------------------------------------------------#
# Posterior Distribution figure ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the posterior distribution figure for each       #
# calibration period length.                                                   #
#------------------------------------------------------------------------------#
    
    #######################
    # List to store plots #
    #######################
    
    # Create the list dynamically
    assign(paste0("plot.list.posterior.", calibrationperiod), NULL)
    
    # Reference the list name
    plot_list_name <- paste0("plot.list.posterior.", calibrationperiod)
    
    ###############################
    # Creating the posterior plot #
    ###############################
    post.plot <- stan_dens(fit_ode_model, pars = pars, separate_chains = TRUE)
    
    ##########################################
    # Saving the results to the empty vector #
    ##########################################
    
    # Retrieving the temp plot 
    temp_plot <- get(plot_list_name) 
    
    # Saving the plot 
    temp_plot <- post.plot
    
    # Update the temp plot in the global environment
    assign(plot_list_name, temp_plot)
    
#------------------------------------------------------------------------------#
# Performance Metrics ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section calculates the performance metrics for the median curve  #
# and forecasting period for each calibration period. In a later step, the     #
# results are then saved in a list to be returned to the main dashboard.       #
#------------------------------------------------------------------------------#
    
    ################################################################################################
    # Defining the actual cases and the median curve for calibration period and forecasting period #
    ################################################################################################
    for (i in 1:length(fitting_index)) {
      
      # Pulling the observed value for the data 
      assign(paste0("actual", i, "_calibration"), get(paste0("actualcases", i))[1:calibrationperiod])
      
      # Pulling the median curve 
      assign(paste0("medcurve", i, "_calibration"), get(paste0("medcurve", i))[1:calibrationperiod])
      
      # Pulling the quantiles 
      assign(paste0("mysolutionaggr", i, "_calibration"), get(paste0("my_solutionaggr", i))[, 1:calibrationperiod])
      
      ##############################################################################
      # Creating the data frames to score information when a forecast is requested #
      ##############################################################################
      if (forecastinghorizon>0){
        
        # Pulling the observed value for the data 
        assign(paste0("actual", i, "_forecast"), get(paste0("actualcases", i))[(calibrationperiod + 1):(calibrationperiod + forecastinghorizon)])
        
        # Pulling the median curve 
        assign(paste0("medcurve", i, "_forecast"), get(paste0("medcurve", i))[(calibrationperiod + 1):(calibrationperiod + forecastinghorizon)])
        
        # Pulling the quantiles  
        assign(paste0("mysolutionaggr", i, "_forecast"), get(paste0("my_solutionaggr", i))[, (calibrationperiod + 1):(calibrationperiod + forecastinghorizon)])
      }
      
      ###################################################
      # Calculating the MAE, MSE. WIS, and 95% coverage #
      ###################################################
      
      # Calculating the MAE 
      assign(paste0("mae", i, "_calibration"), calculate_mae(get(paste0("actual", i, "_calibration")), get(paste0("medcurve", i, "_calibration"))))
      
      # Calculating the MSE 
      assign(paste0("mse", i, "_calibration"), calculate_mse(get(paste0("actual", i, "_calibration")), get(paste0("medcurve", i, "_calibration"))))
      
      # Calculating the WIS 
      assign(paste0("wis", i, "_calibration"), calculate_WIS(get(paste0("mysolutionaggr", i, "_calibration")), get(paste0("actual", i, "_calibration"))))
      
      # Calculating the 95% PI 
      assign(paste0("coverage", i, "_calibration"), calculate_percent_within_interval_calibration(get(paste0("actual", i, "_calibration")), get(paste0("mcmc_intervals_aggr", i))))
      
      #####################################################################
      # Check if the forecast period is within the length of actual cases #
      #####################################################################
      if ((calibrationperiod + forecastinghorizon) <= length(actualcases1) && forecastinghorizon > 0) {
        
        # Calculating the MAE 
        assign(paste0("mae", i, "_forecast"), calculate_mae(get(paste0("actual", i, "_forecast")), get(paste0("medcurve", i, "_forecast"))))
        
        # Calculating the MSE 
        assign(paste0("mse", i, "_forecast"), calculate_mse(get(paste0("actual", i, "_forecast")), get(paste0("medcurve", i, "_forecast"))))
        
        ##################################
        # Creating a matrix of quantiles #
        ##################################
        if(forecastinghorizon==1){
          
          # Creating the matrix of quantiles 
          assign(paste0("mysolutionaggr", i, "_forecast"), matrix(get(paste0("mysolutionaggr", i, "_forecast")), ncol = 1))
          
        }
        
        # Calculating the WIS 
        assign(paste0("wis", i, "_forecast"), calculate_WIS(get(paste0("mysolutionaggr", i, "_forecast")), get(paste0("actual", i, "_forecast"))))
        
        # Calculating the 95% PI 
        assign(paste0("coverage", i, "_forecast"), calculate_percent_within_interval_forecast(get(paste0("actual", i, "_forecast")), get(paste0("mcmc_intervals_aggr", i)), calibrationperiod))
      
      ##########################################################################
      # Set metrics to NA if data is not available to evaluate forecast period #
      ##########################################################################
      }else{
        
        # Set MAE variable to NA
        assign(paste0("mae", i, "_forecast"),NA)
        
        # Set MSE variable to NA
        assign(paste0("mse", i, "_forecast"),NA)
        
        # Set WIS variable to NA
        assign(paste0("wis", i, "_forecast"),NA)
        
        # Set 95% PI variable to NA
        assign(paste0("coverage", i, "_forecast"),NA)
        
      }
      
    }
    
#------------------------------------------------------------------------------#
# Generating the excel files: Performance Metrics ------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the performance metrics data frame which shows   #
# to the main dashboard.                                                       #
#------------------------------------------------------------------------------#
    
    #############################
    # List to store excel files #
    #############################
    
    # Create the list dynamically
    assign(paste0("performance.metrics.", calibrationperiod), NULL)
    
    # Reference the list name
    performance.metrics.list <- paste0("performance.metrics.", calibrationperiod)
    
    ################################################
    # Assuming series_cases is an array of strings #
    ################################################
    for (i in 1:length(fitting_index)) {
      
      # Create the data frame for the current index
      data <- data.frame(
        
        Metrics = c(rep(c("MAE", "MSE", "WIS", "Coverage"))), 
        Calibration = c(get(paste0("mae", i, "_calibration")), 
                        get(paste0("mse", i, "_calibration")), 
                        get(paste0("wis", i, "_calibration")), 
                        get(paste0("coverage", i, "_calibration"))),
        Forecasting = c(get(paste0("mae", i, "_forecast")), 
                        get(paste0("mse", i, "_forecast")), 
                        get(paste0("wis", i, "_forecast")), 
                        get(paste0("coverage", i, "_forecast"))),
        CalibrationLength = rep(calibrationperiod, 4))
        
        ##########################################
        # Saving the results to the empty vector #
        ##########################################
        
        # Update the temp data in the global environment
        assign(performance.metrics.list, data)
        
    }
        
#------------------------------------------------------------------------------#
# Creating the data frame for forecast -----------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the data frame that stores the forecasts and     #
# model fits.                                                                  #
#------------------------------------------------------------------------------#
    
    for (i in 1:length(fitting_index)) {
      
      #############################
      # List to store excel files #
      #############################
      
      # Create the list dynamically
      assign(paste0("forecasts.fits.", calibrationperiod), NULL)
      
      # Reference the list name
      forecast.fits.list <- paste0("forecasts.fits.", calibrationperiod)
      
      ##############################
      # Retrieve and process cases #
      ##############################
      cases_i <- as.integer(get(paste0("actualcases", i))[1:(calibrationperiod+forecastinghorizon)])
      
      # Adding name to the cases data frame 
      assign(paste0("cases", i), cases_i)
      
      ###############################################
      # Create the data frame for the current index #
      ###############################################
      data <- data.frame(
        Date = 0:(calibrationperiod + forecastinghorizon - 1),
        Data = c(cases_i, rep(NA, (calibrationperiod + forecastinghorizon) - length(cases_i))),
        median = round(get(paste0("medcurve", i))[1:(calibrationperiod + forecastinghorizon)], 1),
        LB = pmax(0, round(get(paste0("mcmc_intervals_aggr", i))[1, 1:(calibrationperiod + forecastinghorizon)], 1)),
        UB = round(get(paste0("mcmc_intervals_aggr", i))[2, 1:(calibrationperiod + forecastinghorizon)], 1))
      
      ##########################################
      # Saving the results to the empty vector #
      ##########################################
      
      # Retrieving the temp data
      temp_data <- get(forecast.fits.list) 

      # Update the temp data in the global environment
      assign(forecast.fits.list, data)
      
      
#------------------------------------------------------------------------------#
# Plotting the model fits and forecast -----------------------------------------
#------------------------------------------------------------------------------#
# About: This section plots the model fits and forecasts and then saves the    #
# ggplot to the list.                                                          #
#------------------------------------------------------------------------------#
      
      #############################
      # List to store excel files #
      #############################
      
      # Create the list dynamically
      assign(paste0("forecasts.fits.plot", calibrationperiod), NULL)
      
      # Reference the list name
      forecast.fits.plot.list <- paste0("forecasts.fits.plot", calibrationperiod)
      
      #######################################
      # Plotting the model fit and forecast #
      #######################################
      plot <- ggplot(data, aes(x = Date)) +
        geom_point(aes(y = Data), shape = 21, color = "black", fill = NA, size = 1.5, stroke = 0.7, show.legend = FALSE) +
        geom_line(aes(y = median), color = "red", size = 1, show.legend = FALSE) +
        geom_line(aes(y = LB), color = "black", linetype = "dashed", size = 0.5, show.legend = FALSE) +
        geom_line(aes(y = UB), color = "black", linetype = "dashed", size = 0.5, show.legend = FALSE) +
        labs(title = '', x = datetype, y = series_cases[i]) + 
        theme_classic()
      
      # Add the vertical line conditionally
      if (forecastinghorizon > 0) {
        
        plot <- plot + geom_vline(xintercept = calibrationperiod - 1, linetype = "dotted", size = 1, color = "blue", show.legend = FALSE)
        
      }
      
      ##########################################
      # Saving the results to the empty vector #
      ##########################################
      
      # Retrieving the temp plot 
      temp_plot <- get(forecast.fits.plot.list) 
      
      # Saving the plot 
      temp_plot <- plot
      
      # Update the temp plot in the global environment
      assign(forecast.fits.plot.list, temp_plot)

    }
    
#------------------------------------------------------------------------------#
# Storing the parameter values -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section stores the parameter mean, median, and bounds in a       #
# data frame. It then exports the data frame to the dashboard via a list.      #
#------------------------------------------------------------------------------#
    
    ######################################
    # Loop through each parameter values #
    ######################################
    for (i in seq_along(pars)) {
      
      # Extract samples for the current parameter
      current_samples <- param_samples[[pars[i]]]
      
      # Calculate mcmc interval
      mcmc_interval <- quantile(current_samples, prob=c(.025,.975))
      
      # Calculate median
      median_value <- round(median(current_samples), digits = 2)
      
      # Calculate mean
      mean_value <- round(mean(current_samples), digits = 2)
      
      # Extract lower bounds of the CI
      lower_bound <- round(mcmc_interval[1], digits = 2)
      
      # Extract upper bounds of the CI
      upper_bound <- round(mcmc_interval[2], digits = 2)
      
      ##################################################
      # Prepare a data frame for the current parameter #
      ##################################################
      data <- data.frame(
        CalibrationLength = calibrationperiod,
        parameter = pars[i],  # Assuming pars contains parameter names
        median = median_value,
        mean = mean_value,
        lower_bound = lower_bound,
        upper_bound = upper_bound
      )
      
      
      # Append the current parameter data to result_data
      result_data1 <- rbind(result_data1, data)
  
    }

#------------------------------------------------------------------------------#
# Storing information regarding parameters -------------------------------------
#------------------------------------------------------------------------------#
# About: This section stores information about the parameters and other        #
# statistics into a data frame.                                                #
#------------------------------------------------------------------------------#
    
    ############################################################
    # Initialize an empty data frame to store parameter values #
    ############################################################
    result_data2 <- data.frame()
    
    #######################################
    # Loop through each parameter in pars #
    #######################################
    for (param in pars) {
      
      # Calculate median for the current parameter
      median_value <- round(median(param_samples[[param]]), digits = 2)
      
      # Calculate mcmc interval for the current parameter
      mcmc_interval <- quantile(param_samples[[param]], prob=c(.025,.975))
      
      # Extract necessary summary statistics from fit_seir_negbin for the current parameter
      mean_value <- round(summary(fit_ode_model)$summary[param, "mean"], 2)
      n_eff_value <- round(summary(fit_ode_model)$summary[param, "n_eff"], 2)
      Rhat_value <- round(summary(fit_ode_model)$summary[param, "Rhat"], 2)
      
      # Construct CI_95 string for the current parameter
      CI_95 <- paste("(", round(mcmc_interval[1], 2), ",", round(mcmc_interval[2], 2), ")")
      
      # Create a data frame for the current parameter
      parameter_data <- data.frame(
        CalibrationLength = calibrationperiod,
        Parameter = param,
        Mean = mean_value,
        Median = median_value,
        LB = round(mcmc_interval[1], 2),
        UB = round(mcmc_interval[2], 2), 
        N_eff = n_eff_value,
        Rhat = Rhat_value,
        row.names = NULL
      )
      
      # Append parameter_data to result_data2
      result_data2 <- rbind(result_data2, parameter_data)
      
    }
  
#------------------------------------------------------------------------------#
# Forming the master list ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the master list that is output to the main       #
# dashboard. The list contains each of the plots for each of the calibration   #
# periods.                                                                     #
#------------------------------------------------------------------------------#
    
    #################################################
    # Creating the calibration period specific list #
    #################################################
    calibration.list <- list(get(paste0("plot.list.params.", calibrationperiod)),
                             get(paste0("plot.list.composite.", calibrationperiod)), 
                             get(paste0("plot.list.trace.", calibrationperiod)),
                             get(paste0("plot.list.posterior.", calibrationperiod)),
                             get(paste0("performance.metrics.", calibrationperiod)),
                             get(paste0("forecasts.fits.", calibrationperiod)),
                             get(paste0("forecasts.fits.plot", calibrationperiod)),
                             result_data1,
                             result_data2)
    
    # Adding the names to the list
    names(calibration.list) <- c(paste0("plot.list.params"),
                                 paste0("plot.list.composite"), 
                                 paste0("plot.list.trace"),
                                 paste0("plot.list.posterior"),
                                 paste0("performance.metrics"),
                                 paste0("forecasts.fits"),
                                 paste0("forecasts.fits.plot"),
                                 paste0("parameter.values"),
                                 paste0("add.parameter.values"))
    
    # Adding to the calibration period list
    calibration.period.list[[index]] <- calibration.list
    
    # Adding the name to the list
    names(calibration.period.list)[[index]] <- paste0(calibrationperiod)
                   
  }
  
  return(calibration.period.list)
}


