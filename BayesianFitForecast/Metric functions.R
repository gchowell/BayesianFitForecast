# Calculate Mean Absolute Error (MAE)
calculate_mae <- function(observed, means) {
  return(mean(abs(observed - means)))
}
# Calculate Mean Squared Error (MSE)
calculate_mse <- function(observed, means) {
  return(mean((observed - means)^2))
}

# Function to find the percentiles
calculate_percentiles_matrix <- function(matrix_data) {
  num_columns <- ifelse(is.null(ncol(matrix_data)), 1, ncol(matrix_data))
  result_matrix <- matrix(NA, nrow = num_columns, ncol = 23)
  colnames(result_matrix) <- c("median", "lower_0.01", "lower_0.025", "lower_0.05", "lower_0.10", 
                               "lower_0.15", "lower_0.20", "lower_0.25", "lower_0.30", 
                               "lower_0.35", "lower_0.40", "lower_0.45", "upper_0.55", 
                               "upper_0.60", "upper_0.65", "upper_0.70", "upper_0.75", 
                               "upper_0.80", "upper_0.85", "upper_0.90", "upper_0.95", 
                               "upper_0.975", "upper_0.99")
  
  for (i in 1:num_columns) {
    column <- matrix_data[, i]
    result_matrix[i, "median"] <- median(column)
    result_matrix[i, "lower_0.01"] <- quantile(column, 0.01)
    result_matrix[i, "lower_0.025"] <- quantile(column, 0.025)
    result_matrix[i, "lower_0.05"] <- quantile(column, 0.05)
    result_matrix[i, "lower_0.10"] <- quantile(column, 0.10)
    result_matrix[i, "lower_0.15"] <- quantile(column, 0.15)
    result_matrix[i, "lower_0.20"] <- quantile(column, 0.20)
    result_matrix[i, "lower_0.25"] <- quantile(column, 0.25)
    result_matrix[i, "lower_0.30"] <- quantile(column, 0.30)
    result_matrix[i, "lower_0.35"] <- quantile(column, 0.35)
    result_matrix[i, "lower_0.40"] <- quantile(column, 0.40)
    result_matrix[i, "lower_0.45"] <- quantile(column, 0.45)
    result_matrix[i, "upper_0.55"] <- quantile(column, 0.55)
    result_matrix[i, "upper_0.60"] <- quantile(column, 0.60)
    result_matrix[i, "upper_0.65"] <- quantile(column, 0.65)
    result_matrix[i, "upper_0.70"] <- quantile(column, 0.70)
    result_matrix[i, "upper_0.75"] <- quantile(column, 0.75)
    result_matrix[i, "upper_0.80"] <- quantile(column, 0.80)
    result_matrix[i, "upper_0.85"] <- quantile(column, 0.85)
    result_matrix[i, "upper_0.90"] <- quantile(column, 0.90)
    result_matrix[i, "upper_0.95"] <- quantile(column, 0.95)
    result_matrix[i, "upper_0.975"] <- quantile(column, 0.975)
    result_matrix[i, "upper_0.99"] <- quantile(column, 0.99)
  }
  
  return(result_matrix)
}

# Calculate Weighted Interval Score (WIS)
calculate_WIS <- function(mysolutionaggr, actual) {
  # Calculate percentiles matrix
  data_WIU <- calculate_percentiles_matrix(mysolutionaggr)
  
  # Define alphas related to quantiles
  alphas <- c(0.02, 0.05, seq(0.1, 0.9, by = 0.1))
  
  # Initial weight
  w0 <- 1 / 2
  
  # Initialize empty vector for WIS values
  WIS_values <- numeric(length(actual))
  
  # Loop through each observed value in actual
  for (j in seq_along(actual)) {
    sum1 <- 0  # Reset sum1 for each observation
    
    # Observed data for the current week
    y <- actual[j]
    
    # Loop through alphas
    for (k in seq_along(alphas)) {
      alpha <- alphas[k]
      w_k <- alpha / 2
      
      # Get lower and upper quantiles
      lower_idx <- 1 + k
      upper_idx <- 24 - k
      
      Lt <- data_WIU[j, lower_idx]
      Ut <- data_WIU[j, upper_idx]
      
      # Calculate Interval Score
      IS <- (Ut - Lt) + (2 / alpha) * (Lt - y) * (y < Lt) + (2 / alpha) * (y - Ut) * (y > Ut)
      
      # Update sum1 for WIS calculation
      sum1 <- sum1 + w_k * IS
    }
    
    # Mean prediction for the current forecast
    m <- data_WIU[j, "median"]
    
    # Calculate WIS for the current observed value
    WIS_values[j] <- (1 / (length(alphas) + 1 / 2)) * (w0 * abs(y - m) + sum1)
  }
  
  # Mean of WIS values
  WIS <- mean(WIS_values)
  
  return(WIS)
}


check_within_interval <- function(actual, interval) {
  within_interval <- (actual >= interval[1] & actual <= interval[2])
  return(within_interval)
}



# Calculate Coverage of 95% CI for the Calibration Part
calculate_percent_within_interval_calibration <- function(actual_calibration, mcmc_intervals_aggr) {
  # Initialize a vector to store the results
  within_interval <- logical(length(actual_calibration))
  # Initialize a counter for the number of cases within the interval
  cases_within_interval <- 0
  # Loop through each time point
  for (i in 1:length(actual_calibration)) {
    # Check if actual case at time point i is within the bootstrap interval at the same time point
    within_interval[i] <- check_within_interval(actual_calibration[i], mcmc_intervals_aggr[, i])
    # Check if actual case at time point i is within the bootstrap interval at the same time point
    if (within_interval[i]) {
      cases_within_interval <- cases_within_interval + 1
    }
  }
  
  # Calculate the percentage of cases within the interval
  percent_within_interval_calibration <- cases_within_interval / length(actual_calibration) * 100
  
  return(percent_within_interval_calibration)
}


# Calculate Coverage of 95% CI for the Forecasting Part
calculate_percent_within_interval_forecast <- function(actual_forecast, mcmc_intervals_aggr) {
  # Initialize a vector to store the results
  within_interval <- logical(length(actual_forecast))
  # Initialize a counter for the number of cases within the interval
  cases_within_interval <- 0
  # Loop through each time point
  for (i in 1:length(actual_forecast)) {
    # Check if actual case at time point i is within the bootstrap interval at the same time point
    within_interval[i] <- check_within_interval(actual_forecast[i], mcmc_intervals_aggr[, i + calibrationperiod])
    # Check if actual case at time point i is within the bootstrap interval at the same time point
    if (within_interval[i]) {
      cases_within_interval <- cases_within_interval + 1
    }
  }
  # Calculate the percentage of cases within the interval
  percent_within_interval_forecast <- cases_within_interval / length(actual_forecast) * 100
  return(percent_within_interval_forecast)
}


###################################################################### DIC,..
