rm(list = ls())
library(bayesplot)
library("readxl")
library(xlsx)
library(readxl)
library(openxlsx)
library(rstan)
library(ggplot2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("options_SEIR_sanfrancisco_Ex5.R")



source("Metric functions.R")
Mydata <- read_excel(paste0(cadfilename1, ".xlsx"))

dir.create("output", showWarnings = FALSE)


errorstructure <- c("negativebinomial","normal","poisson")


actualcases <- Mydata$cases
result_data1 <- data.frame()
result_data2 <- data.frame()

for(calibrationperiod in calibrationperiods){ 
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  load(paste(model_name,"cal", (calibrationperiod),  
             "fcst",forecastinghorizon,errorstructure[errstrc] ,caddisease,
             "fit.Rdata", sep = "-"))
  setwd("output")
  
    folder_name <- paste(model_name, caddisease, errorstructure[errstrc], "cal",
                 calibrationperiod, "fcst", forecastinghorizon, sep = "-") 
    dir.create(folder_name, showWarnings = FALSE)
  
  my_solutionaggr <- pred_cases
  medcurve <- matrix(NA, nrow = 1, ncol = calibrationperiod+forecastinghorizon)
  medcurve <- apply(my_solutionaggr, 2, median)
  mcmc_intervals_aggr <- apply(my_solutionaggr, 2, quantile, prob=c(.025, .975))
  
     
  
  ##############################################################################
  ##############################################################################
  #                           HISTOGRAM PLOTTING
  ##############################################################################
  ##############################################################################
  pars <- unlist(pars)
  
  # Initialize empty lists to store results
  mcmc_intervals <- list()
  medians <- numeric(length(pars))
  means <- numeric(length(pars))
  lower_bounds <- numeric(length(pars))
  upper_bounds <- numeric(length(pars))
  
  # Loop through each parameter
  for (i in seq_along(pars)) {
    # Extract samples for the current parameter
    current_samples <- param_samples[[pars[i]]]
    
    # Check if there are samples to plot
    if (length(current_samples) == 0) {
      message("No samples for parameter: ", pars[i])
      next
    }
    
    # Calculate summary statistics
    mcmc_intervals[[i]] <- quantile(current_samples, prob = c(0.025, 0.975))
    medians[i] <- round(median(current_samples), digits = 2)
    means[i] <- round(mean(current_samples), digits = 2)
    lower_bounds[i] <- round(mcmc_intervals[[i]][1], digits = 2)
    upper_bounds[i] <- round(mcmc_intervals[[i]][2], digits = 2)
    
    # Start a new plot device (PDF) for the histogram
    hist_filename <- paste0(folder_name, "/", pars[i], "-histogram-", model_name, "-", caddisease, "-", 
                            errorstructure[errstrc], "-cal-", calibrationperiod, ".pdf")
    
    pdf(file = hist_filename)
    on.exit(dev.off(), add = TRUE)  # Ensure the plot device is closed
    
    # Plot histogram
    hist(current_samples, 
         main = paste("Median:", medians[i], "Mean:", means[i], "95% CI:",
                      lower_bounds[i], ", ", upper_bounds[i]), 
         xlab = pars[i], 
         ylab = "Frequency", 
         col = "lightblue", 
         border = "black")
    
    # Explicitly close the PDF device
    dev.off()  # Close the PDF device to finalize the file
  }
  
  ##############################################################################
  ##############################################################################
  #                           Composite Samples
  ##############################################################################
  ##############################################################################
  
  # Initialize empty lists to store results for composite samples
  composite_mcmc_intervals <- list()
  composite_medians <- numeric(length(composite_samples))
  composite_means <- numeric(length(composite_samples))
  composite_lower_bounds <- numeric(length(composite_samples))
  composite_upper_bounds <- numeric(length(composite_samples))
  
  # Loop through each composite expression
  for (name in names(composite_expressions)) {
    # Extract samples for the current composite expression
    current_samples <- composite_samples[[name]]
    
    # Check if there are samples to plot
    if (length(current_samples) == 0) {
      message("No samples for composite expression: ", name)
      next
    }
    
    # Calculate summary statistics
    mcmc_intervals <- quantile(current_samples, prob = c(0.025, 0.975))
    composite_medians <- round(median(current_samples), digits = 2)
    composite_means <- round(mean(current_samples), digits = 2)
    composite_lower_bounds <- round(mcmc_intervals[1], digits = 2)
    composite_upper_bounds <- round(mcmc_intervals[2], digits = 2)
    
    # Start a new plot device (PDF) for the histogram
    hist_filename <- paste0(folder_name, "/", name, "-histogram-", model_name, "-", caddisease, "-", 
                            errorstructure[errstrc], "-cal-", calibrationperiod, ".pdf")
    
    pdf(file = hist_filename)
    on.exit(dev.off(), add = TRUE)  # Ensure the plot device is closed
    
    # Plot histogram
    hist(current_samples, 
         main = paste(name, "- Median:", composite_medians, "Mean:", composite_means, "95% CI:",
                      composite_lower_bounds, ", ", composite_upper_bounds), 
         xlab = name, 
         ylab = "Frequency", 
         col = "lightblue", 
         border = "black")
    
    # Explicitly close the PDF device
    dev.off()  # Close the PDF device to finalize the file
  }
  
  ##############################################################################
  ##############################################################################
  #                              TRACE PLOTS
  ##############################################################################
  ##############################################################################
  # Define the file path for the PDF
  trace_filename <- paste0(folder_name, "/", "traceplot-", model_name, "-", caddisease, "-", 
                           errorstructure[errstrc], "-cal-", calibrationperiod, ".pdf")
  
  # Start a new PDF device for trace plots
  pdf(file = trace_filename)
  
  # Plot trace plot
  traceplot(fit_ode_model, pars = pars)
  print(traceplot(fit_ode_model, pars = pars))
  # Close the PDF device
  dev.off()
  ##############################################################################
  ##############################################################################
  #                        POSTERIOR DISTRIBUTION
  ##############################################################################
  ##############################################################################
  # Define the file path for the PDF
  posterior_filename <- paste0(folder_name, "/", "posterior-", model_name, "-", caddisease, "-", 
                               errorstructure[errstrc], "-cal-", calibrationperiod, ".pdf")
  
  # Start a new PDF device for posterior plots
  pdf(file = posterior_filename)
  
  # Plot posterior plot
  stan_dens(fit_ode_model, pars = pars, separate_chains = TRUE)
  print(stan_dens(fit_ode_model, pars = pars, separate_chains = TRUE))
  # Close the PDF device
  dev.off()
  ##############################################################################
  ##############################################################################
  
  
  ##############################################################################
  ##############################################################################
  #                          PERFORMANCE METRICS
  ##############################################################################
  ##############################################################################
  #Defining the actual cases and the median curve for calibration period and forecasting period
  actual_calibration <- actualcases[1:calibrationperiod]
  medcurve_calibration <- medcurve[1:calibrationperiod]
  mysolutionaggr_calibration <- my_solutionaggr[, 1:calibrationperiod]
  if (forecastinghorizon>0)
  {
  actual_forecast <- actualcases[(calibrationperiod + 1):
                                   (calibrationperiod + forecastinghorizon)]
  medcurve_forecast <- medcurve[(calibrationperiod + 1):
                                  (calibrationperiod + forecastinghorizon)]
  
  mysolutionaggr_forecast <- my_solutionaggr[, (calibrationperiod + 1):(calibrationperiod + forecastinghorizon)]
  }
  
  mae_calibration <- calculate_mae(actual_calibration, medcurve_calibration)
  mse_calibration <- calculate_mse(actual_calibration, medcurve_calibration)
  wis_calibration <- calculate_WIS(mysolutionaggr_calibration,actual_calibration)
  coverage_calibration <- calculate_percent_within_interval_calibration(actual_calibration, mcmc_intervals_aggr)
  # Check if the forecast period is within the length of actual cases
  if ((calibrationperiod + forecastinghorizon) <= length(actualcases) && forecastinghorizon > 0) {
    # Calculate metrics
    mae_forecast <- calculate_mae(actual_forecast, medcurve_forecast)
    mse_forecast <- calculate_mse(actual_forecast, medcurve_forecast)
    if(forecastinghorizon==1)
    {
      mysolutionaggr_forecast<-matrix(mysolutionaggr_forecast, ncol = 1)
    }
    wis_forecast <- calculate_WIS(mysolutionaggr_forecast, actual_forecast)
    coverage_forecast <- calculate_percent_within_interval_forecast(actual_forecast, mcmc_intervals_aggr)
  } else {
    # Set metrics to NA if the forecast period exceeds the length of actual cases
    mae_forecast <- NA
    mse_forecast <- NA
    wis_forecast <- NA
    coverage_forecast <- NA
  }
  ##############################################################################
  ##############################################################################
  
  
  ##############################################################################
  ##############################################################################
  #                         Generate excel files
  ##############################################################################
  ##############################################################################
  
  
  data <- data.frame(
    Calibration = c(mae_calibration, mse_calibration, wis_calibration, coverage_calibration),
    Forecasting = c(mae_forecast, mse_forecast, wis_forecast, coverage_forecast),
    row.names = c("mae", "mse", "WIS", "Coverage")
  )
  
  file_path <- file.path(
    folder_name, 
    sprintf(
      "performance metrics-%s-%s-%s-cal-%s-fcst-%s.xlsx",
      model_name,
      caddisease,
      errorstructure[errstrc],
      calibrationperiod,
      forecastinghorizon
    )
  )
  # Write the data frame to an Excel file
  write.xlsx(data, file_path, row.names = TRUE)
  ##############################################################################
  cases <- head(round(actualcases, 1), calibrationperiod + forecastinghorizon)
  # Create a data frame with the arrays
  data <- data.frame(
    Date = 0:(calibrationperiod + forecastinghorizon-1),
    Data = c(cases, rep(NA, (calibrationperiod + forecastinghorizon) - length(cases))),
    median = round(medcurve[1:(calibrationperiod + forecastinghorizon)], 1),
    LB = pmax(0, round(mcmc_intervals_aggr[1, 1:(calibrationperiod + forecastinghorizon)], 1)),
    UB = round(mcmc_intervals_aggr[2, 1:(calibrationperiod + forecastinghorizon)], 1)
  )
  
  excel_file <- file.path(
    folder_name, 
    sprintf(
      "forecast-%s-%s-%s-cal-%s-fcst-%s.xlsx",
      model_name,
      caddisease,
      errorstructure[errstrc],
      calibrationperiod,
      forecastinghorizon
    )
  )
  write.xlsx(data, file = excel_file, row.names = FALSE)
  ##############################################################################
  ##############################################################################
  # Plotting
  plot <- ggplot(data, aes(x = Date)) +
    geom_point(aes(y = Data), shape = 21, color = "black", fill = NA, size = 1.5, stroke = 0.7, show.legend = FALSE) +
    geom_line(aes(y = median), color = "red", size = 1, show.legend = FALSE) +
    geom_line(aes(y = LB), color = "black", linetype = "dashed", size = 0.5, show.legend = FALSE) +
    geom_line(aes(y = UB), color = "black", linetype = "dashed", size = 0.5, show.legend = FALSE) +
    labs(title = '', x = datetype, y = datatype) +
    theme_minimal()
  
  # Add the vertical line conditionally
  if (forecastinghorizon > 0) {
    plot <- plot + geom_vline(xintercept = calibrationperiod - 1, linetype = "dotted", size = 1, color = "blue", show.legend = FALSE)
  }
  
  # Save the plot as a PDF file
  pdf_filename <- file.path(
    folder_name, 
    sprintf(
      "Forecast-%s-%s-%s-cal-%s-fcst-%s.pdf",
      model_name,
      caddisease,
      errorstructure[errstrc],
      calibrationperiod,
      forecastinghorizon
    )
  )
  ggsave(filename = pdf_filename, plot = plot, width = 8, height = 6)
  ##############################################################################
  
  # Loop through each parameter
  for (i in seq_along(pars)) {
    # Extract samples for the current parameter
    current_samples <- param_samples[[pars[i]]]
    # Calculate mcmc interval
    mcmc_interval <- quantile(current_samples, prob=c(.025,.975))
    # Calculate median
    median_value <- round(median(current_samples), digits = 2)
    # Calculate mean
    mean_value <- round(mean(current_samples), digits = 2)
    # Extract lower and upper bounds of the CI
    lower_bound <- round(mcmc_interval[1], digits = 2)
    upper_bound <- round(mcmc_interval[2], digits = 2)
    
    # Prepare a data frame for the current parameter
    data <- data.frame(
      calibration = calibrationperiod,
      parameter = pars[i],  # Assuming pars contains parameter names
      median = median_value,
      mean = mean_value,
      lower_bound = lower_bound,
      upper_bound = upper_bound
    )
    
    # Append the current parameter data to result_data
    result_data1 <- rbind(result_data1, data)
  }
  excel_file1 <- file.path(
    folder_name, 
    sprintf(
      "parameters-%s-%s-%s-cal-%s-fcst-%s.xlsx",
      model_name,
      caddisease,
      errorstructure[errstrc],
      calibrationperiod,
      forecastinghorizon
    )
  )
  write.xlsx(result_data1, file = excel_file1, row.names = FALSE)
  ##############################################################################
  
  
  
  # Initialize an empty data frame to store parameter values
  result_data2 <- data.frame()
  
  # Loop through each parameter in pars
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
      Calibration = calibrationperiod,
      Parameter = param,
      Mean = mean_value,
      Median = median_value,
      CI_95 = CI_95,
      N_eff = n_eff_value,
      Rhat = Rhat_value
    )
    
    # Append parameter_data to result_data2
    result_data2 <- rbind(result_data2, parameter_data)
  }
  
  excel_file2 <- file.path(
    folder_name, 
    sprintf(
      "convergence-%s-%s-%s-cal-%s-fcst-%s.xlsx",
      model_name,
      caddisease,
      errorstructure[errstrc],
      calibrationperiod,
      forecastinghorizon
    )
  )
  
  write.xlsx(result_data2, file = excel_file2, row.names = FALSE)
  

}


