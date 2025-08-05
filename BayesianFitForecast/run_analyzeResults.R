rm(list = ls())
library(bayesplot)
library("readxl")
library(xlsx)
library(loo)
library(readxl)
library(openxlsx)
library(rstan)
library(ggplot2) 
library(stringr)
library(gridExtra)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("options_SEIR_sanfrancisco_Ex5.R")
source("Metric functions.R")
Mydata <- read_excel(paste0(cadfilename1, ".xlsx"))

dir.create("output", showWarnings = FALSE)


errorstructure <- c("negativebinomial","normal","poisson")


for (i in 1:length(fitting_index)) {
  assign(paste0("actualcases", i), Mydata[[paste0("cases", i)]])
}
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
  
    for (i in 1:length(fitting_index)) {
      assign(paste0("my_solutionaggr", i), pred_cases[,,i])
      assign(paste0("medcurve", i), apply(get(paste0("my_solutionaggr", i)), 2, median))
      assign(paste0("mcmc_intervals_aggr", i), apply(get(paste0("my_solutionaggr", i)), 
                                                     2, quantile, probs = c(0.025, 0.975)))
    }
  
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
  for (i in 1:length(fitting_index)) {
    assign(paste0("actual", i, "_calibration"), get(paste0("actualcases", i))[1:calibrationperiod])
    assign(paste0("medcurve", i, "_calibration"), get(paste0("medcurve", i))[1:calibrationperiod])
    assign(paste0("mysolutionaggr", i, "_calibration"), get(paste0("my_solutionaggr", i))[, 1:calibrationperiod])
    if (forecastinghorizon>0)
    {
      assign(paste0("actual", i, "_forecast"), 
        get(paste0("actualcases", i))[(calibrationperiod + 1):(calibrationperiod + forecastinghorizon)])
      assign(paste0("medcurve", i, "_forecast"), 
        get(paste0("medcurve", i))[(calibrationperiod + 1):(calibrationperiod + forecastinghorizon)])
      assign(paste0("mysolutionaggr", i, "_forecast"), 
        get(paste0("my_solutionaggr", i))[, (calibrationperiod + 1):(calibrationperiod + forecastinghorizon)])
    }
    assign(paste0("mae", i, "_calibration"), 
        calculate_mae(get(paste0("actual", i, "_calibration")), get(paste0("medcurve", i, "_calibration"))))
    assign(paste0("mse", i, "_calibration"), 
        calculate_mse(get(paste0("actual", i, "_calibration")), get(paste0("medcurve", i, "_calibration"))))
    assign(paste0("wis", i, "_calibration"), 
           calculate_WIS(get(paste0("mysolutionaggr", i, "_calibration")), 
                         get(paste0("actual", i, "_calibration"))))
    assign(paste0("coverage", i, "_calibration"), calculate_percent_within_interval_calibration(
      get(paste0("actual", i, "_calibration")),
      get(paste0("mcmc_intervals_aggr", i))
    ))
    # Check if the forecast period is within the length of actual cases
    if ((calibrationperiod + forecastinghorizon) <= length(actualcases1) && forecastinghorizon > 0) {
    assign(paste0("mae", i, "_forecast"), calculate_mae(
      get(paste0("actual", i, "_forecast")),
      get(paste0("medcurve", i, "_forecast"))
      ))
      assign(paste0("mse", i, "_forecast"), calculate_mse(
        get(paste0("actual", i, "_forecast")),
        get(paste0("medcurve", i, "_forecast"))
      ))
      if(forecastinghorizon==1)
      {
        assign(paste0("mysolutionaggr", i, "_forecast"), matrix(
          get(paste0("mysolutionaggr", i, "_forecast")),
          ncol = 1
        ))
      }
      assign(paste0("wis", i, "_forecast"), calculate_WIS(
        get(paste0("mysolutionaggr", i, "_forecast")),
        get(paste0("actual", i, "_forecast"))
      ))
      assign(paste0("coverage", i, "_forecast"), calculate_percent_within_interval_forecast(
        get(paste0("actual", i, "_forecast")),
        get(paste0("mcmc_intervals_aggr", i))
      ))
    }
    else {
      # Set metrics to NA if the forecast period exceeds the length of actual cases
      assign(paste0("mae", i, "_forecast"),NA)
      assign(paste0("mse", i, "_forecast"),NA)
      assign(paste0("wis", i, "_forecast"),NA)
      assign(paste0("coverage", i, "_forecast"),NA)
    }
}
  
  ##############################################################################
  ##############################################################################
  
  
  ##############################################################################
  ##############################################################################
  #                         Generate excel files
  ##############################################################################
  ##############################################################################
  
  
    # Assuming series_cases is an array of strings
    for (i in 1:length(fitting_index)) {
      # Create the data frame for the current index
      data <- data.frame(
        Calibration = c(get(paste0("mae", i, "_calibration")), 
                        get(paste0("mse", i, "_calibration")), 
                        get(paste0("wis", i, "_calibration")), 
                        get(paste0("coverage", i, "_calibration"))),
        Forecasting = c(get(paste0("mae", i, "_forecast")), 
                        get(paste0("mse", i, "_forecast")), 
                        get(paste0("wis", i, "_forecast")), 
                        get(paste0("coverage", i, "_forecast"))),
        row.names = c("mae", "mse", "WIS", "Coverage")
      )
      
      # Create the file path with series_cases[i]
      file_path <- file.path(
        folder_name, 
        sprintf(
          "performance metrics-%s-%s-%s-%s-cal-%s-fcst-%s.xlsx",
          model_name,
          caddisease,
          errorstructure[errstrc],
          series_cases[i],  
          calibrationperiod,
          forecastinghorizon
        )
      )
      
      # Write the data frame to an Excel file
      write.xlsx(data, file_path, row.names = TRUE)
    }
    
##############################################################################
  for (i in 1:length(fitting_index)) {
    # Retrieve and process cases
    cases_i <- as.integer(get(paste0("actualcases", i))[1:(calibrationperiod+forecastinghorizon)])
    assign(paste0("cases", i), cases_i)
    
    # Create the data frame for the current index
    data <- data.frame(
      Date = 0:(calibrationperiod + forecastinghorizon - 1),
      Data = c(cases_i, rep(NA, (calibrationperiod + forecastinghorizon) - length(cases_i))),
      median = round(get(paste0("medcurve", i))[1:(calibrationperiod + forecastinghorizon)], 1),
      LB = pmax(0, round(get(paste0("mcmc_intervals_aggr", i))[1, 1:(calibrationperiod + forecastinghorizon)], 1)),
      UB = round(get(paste0("mcmc_intervals_aggr", i))[2, 1:(calibrationperiod + forecastinghorizon)], 1)
    )
    
    # Create the file path for Excel
    file_path <- file.path(
      folder_name, 
      sprintf(
        "data-%s-%s-%s-%s-cal-%s-fcst-%s.xlsx",
        model_name,
        caddisease,
        errorstructure[errstrc],
        series_cases[i], 
        calibrationperiod,
        forecastinghorizon
      )
    )
    
    # Write the data frame to an Excel file
    write.xlsx(data, file_path, row.names = FALSE)
    
    # Plotting
    plot <- ggplot(data, aes(x = Date)) +
      geom_point(aes(y = Data), shape = 21, color = "black", fill = NA, size = 1.5, stroke = 0.7, show.legend = FALSE) +
      geom_line(aes(y = median), color = "red", size = 1, show.legend = FALSE) +
      geom_line(aes(y = LB), color = "black", linetype = "dashed", size = 0.5, show.legend = FALSE) +
      geom_line(aes(y = UB), color = "black", linetype = "dashed", size = 0.5, show.legend = FALSE) +
      labs(title = '', x = datetype, y = series_cases[i]) + 
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
        series_cases[i],
        calibrationperiod,
        forecastinghorizon
      )
    )
    ggsave(filename = pdf_filename, plot = plot, width = 8, height = 6)
} 
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
  
  
 
  
  
  
  # Optimized evaluate_time_dependent function
  evaluate_time_dependent <- function(template, params, t_range) {
    # Process template once
    template <- gsub(";", "", template)
    template <- gsub("^return\\s+", "", template, perl = TRUE)
    template <- gsub("params(\\d+)", "params[\\1]", template, perl = TRUE)
    
    func_str <- paste0("function(t, params) {", template, "}")
    func <- eval(parse(text = func_str))
    
    # Vectorize evaluation over t_range
    sapply(t_range, function(t) func(t, params))
  }
  
  # Optimized plot function
  plot_time_dependent_params <- function(param_samples, params, paramsfix, time_dependent_templates,
                                         t_range, n_samples) {
    n_templates <- length(time_dependent_templates)
    if (n_templates == 0) {
      stop("No time-dependent templates provided")
    }
    
    plots <- list()
    
    # Pre-process all templates once
    processed_templates <- lapply(time_dependent_templates, function(template) {
      template <- gsub(";", "", template)
      template <- gsub("^return\\s+", "", template, perl = TRUE)
      template <- gsub("params(\\d+)", "params[\\1]", template, perl = TRUE)
      
      func_str <- paste0("function(t, params) {", template, "}")
      eval(parse(text = func_str))
    })
    
    # Prepare parameter samples once
    sample_params_list <- lapply(1:min(n_samples, nrow(param_samples)), function(j) {
      sample_params <- numeric(length(params))
      names(sample_params) <- params
      
      for (k in 1:length(params)) {
        if (paramsfix[k] == 1) {
          sample_params[k] <- get(paste0("params", k, "_prior"))
        } else {
          sample_params[k] <- param_samples[[params[k]]][j]
        }
      }
      sample_params
    })
    
    for (i in 1:n_templates) {
      template_name <- names(time_dependent_templates)[i]
      func <- processed_templates[[i]]
      
      # Pre-allocate results matrix
      results <- matrix(NA, nrow = min(n_samples, nrow(param_samples)), ncol = length(t_range))
      
      # Compute all results for this template
      for (j in 1:length(sample_params_list)) {
        sample_params <- sample_params_list[[j]]
        # Vectorize over t_range
        results[j, ] <- sapply(t_range, function(t) func(t, sample_params))
      }
      
      median_values <- apply(results, 2, median, na.rm = TRUE)
      
      summary_data <- data.frame(
        time = t_range,
        median = median_values
      )
      
      p <- ggplot() +
        geom_line(data = summary_data, aes(x = time, y = median), size = 1.5, color = "blue") +
        labs(title = paste("Time-dependent parameter:", template_name),
             x = "Time", y = "Parameter value") +
        theme_minimal()
      
      plots[[i]] <- p
    }
    
    return(plots)
  }
  
  # Main execution
  param_samples_df <- as.data.frame(param_samples)
  t_range <- seq(0, (calibrationperiod + forecastinghorizon - 1), by = 1)
  
  if (length(time_dependent_templates) > 0) {
    tryCatch({
      plots <- plot_time_dependent_params(param_samples_df, params, paramsfix,
                                          time_dependent_templates, t_range, n_samples = niter)
      
      # Save plots efficiently
      template_names <- names(time_dependent_templates)
      for (i in seq_along(plots)) {
        filename <- file.path(folder_name, paste0(template_names[i], ".pdf"))
        ggsave(filename, plots[[i]], width = 10, height = 6)
      }
    }, error = function(e) {
      message("Time-dependent plotting skipped due to error: ", conditionMessage(e))
    })
  } else {
    message("No time-dependent templates provided — skipping time-dependent plots.")
  }
  
  
  #################### Metrics: DIC-WAIC-LOO
  metrics_list <- list()
  errorstructure <- c("negative_binomial", "normal", "poisson")
  selected_error_structure <- errorstructure[errstrc]
  
  for (i in 1:length(fitting_index)) {
    observed_data <- get(paste0("actualcases", i))[1:(calibrationperiod + forecastinghorizon)]
    pred_matrix <- get(paste0("my_solutionaggr", i))
    niter <- nrow(pred_matrix)
    
    ## --- DIC ---
    deviances <- numeric(niter)
    for (iter in 1:niter) {
      pred_i <- pred_matrix[iter, ]
      if (selected_error_structure %in% c("poisson", "negative_binomial")) {
        pred_i[pred_i <= 0] <- 1e-10
      }
      
      if (selected_error_structure == "normal") {
        sigma_i <- param_samples[[paste0("sigma", i)]][iter]
        if (sigma_i <= 0) sigma_i <- 1e-6
        log_lik_i <- sum(dnorm(observed_data, mean = pred_i, sd = sigma_i, log = TRUE))
        
      } else if (selected_error_structure == "poisson") {
        log_lik_i <- sum(dpois(observed_data, lambda = pred_i, log = TRUE))
        
      } else if (selected_error_structure == "negative_binomial") {
        phi_i <- param_samples[[paste0("phi", i)]][iter]
        if (phi_i <= 0) phi_i <- 1e-6
        log_lik_i <- sum(dnbinom(observed_data, size = phi_i, mu = pred_i, log = TRUE))
      }
      
      deviances[iter] <- -2 * log_lik_i
    }
    
    D_bar <- mean(deviances)
    pred_mean <- apply(pred_matrix, 2, mean)
    
    if (selected_error_structure %in% c("poisson", "negative_binomial")) {
      pred_mean[pred_mean <= 0] <- 1e-10
    }
    
    if (selected_error_structure == "normal") {
      sigma_mean <- mean(param_samples[[paste0("sigma", i)]])
      if (sigma_mean <= 0) sigma_mean <- 1e-6
      log_lik_mean <- sum(dnorm(observed_data, mean = pred_mean, sd = sigma_mean, log = TRUE))
      
    } else if (selected_error_structure == "poisson") {
      log_lik_mean <- sum(dpois(observed_data, lambda = pred_mean, log = TRUE))
      
    } else if (selected_error_structure == "negative_binomial") {
      phi_mean <- mean(param_samples[[paste0("phi", i)]])
      if (phi_mean <= 0) phi_mean <- 1e-6
      log_lik_mean <- sum(dnbinom(observed_data, size = phi_mean, mu = pred_mean, log = TRUE))
    }
    
    D_hat <- -2 * log_lik_mean
    pD <- D_bar - D_hat
    dic_results <- list(DIC = D_bar + pD, pD = pD, D_bar = D_bar, D_hat = D_hat)
    
    ## --- Log-likelihood matrix ---
    n_obs <- length(observed_data)
    log_lik_matrix <- matrix(NA, nrow = niter, ncol = n_obs)
    
    for (iter in 1:niter) {
      pred_i <- pred_matrix[iter, ]
      if (any(is.na(pred_i)) || any(is.infinite(pred_i))) next
      
      if (selected_error_structure == "normal") {
        sigma_i <- param_samples[[paste0("sigma", i)]][iter]
        if (is.na(sigma_i) || sigma_i <= 0) next
        log_lik_matrix[iter, ] <- dnorm(observed_data, mean = pred_i, sd = sigma_i, log = TRUE)
        
      } else if (selected_error_structure == "poisson") {
        pred_i[pred_i <= 0] <- 1e-10
        log_lik_matrix[iter, ] <- dpois(observed_data, lambda = pred_i, log = TRUE)
        
      } else if (selected_error_structure == "negative_binomial") {
        phi_i <- param_samples[[paste0("phi", i)]][iter]
        if (is.na(phi_i) || phi_i <= 0) next
        pred_i[pred_i <= 0] <- 1e-10
        log_lik_matrix[iter, ] <- dnbinom(observed_data, size = phi_i, mu = pred_i, log = TRUE)
      }
    }
    
    ## --- Clean log-lik matrix ---
    valid_rows <- apply(log_lik_matrix, 1, function(x) {
      !all(is.na(x)) && !any(is.nan(x)) && !any(is.infinite(x) & x > 0)
    })
    
    if (sum(valid_rows) == 0) {
      stop(paste("No valid rows in log-likelihood matrix for dataset", i))
    }
    
    log_lik_clean <- log_lik_matrix[valid_rows, , drop = FALSE]
    log_lik_clean[is.na(log_lik_clean)] <- -1e10
    log_lik_clean[is.nan(log_lik_clean)] <- -1e10
    log_lik_clean[is.infinite(log_lik_clean) & log_lik_clean > 0] <- -1e10
    log_lik_clean[log_lik_clean < -1e10] <- -1e10
    
    ## --- WAIC & LOO ---
    waic_results <- waic(log_lik_clean)
    loo_results <- loo(log_lik_clean)
    
    metrics_list[[paste0("metrics", i)]] <- list(
      DIC = dic_results,
      WAIC = waic_results,
      LOO = loo_results
    )
  }
  
  ## --- Convert to DataFrame & Save ---
  metrics_df <- data.frame(
    Dataset = character(length(metrics_list)),
    DIC = numeric(length(metrics_list)),
    WAIC = numeric(length(metrics_list)),
    LOO = numeric(length(metrics_list)),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(metrics_list)) {
    metrics <- metrics_list[[i]]
    metrics_df[i, "Dataset"] <- names(metrics_list)[i]
    metrics_df[i, "DIC"] <- if (!is.null(metrics$DIC) && !is.null(metrics$DIC$DIC)) metrics$DIC$DIC else NA
    metrics_df[i, "WAIC"] <- if (!is.null(metrics$WAIC) && !is.null(metrics$WAIC$estimates)) metrics$WAIC$estimates["waic", "Estimate"] else NA
    metrics_df[i, "LOO"] <- if (!is.null(metrics$LOO) && !is.null(metrics$LOO$estimates)) metrics$LOO$estimates["looic", "Estimate"] else NA
  }
  
  write.xlsx(metrics_df, file = file.path(folder_name, "model_fit_metrics.xlsx"))
  
  
  
}


