library(deSolve)
library(ggplot2)
library(readxl)
library(gridExtra)
library(dplyr)    
library(tidyr)    
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("options_SEIR_sanfrancisco_prior.R")

state_names <- vars
n_vars <- length(state_names)
init_values <- Ic

ode_rhs <- function(index, ode_system) {
  ode_lines <- strsplit(ode_system, "\n")[[1]]
  ode_lines <- ode_lines[nzchar(trimws(ode_lines))]
  line <- trimws(ode_lines[index])
  rhs <- sub("^.*=\\s*", "", line)
  return(rhs)
}

dynamic_model <- function(time, state, parameters) {
  param_names <- paste0("params", seq_along(parameters))
  state_names <- paste0("vars", seq_along(state))
  
  param_env <- setNames(as.list(parameters), param_names)
  state_env <- setNames(as.list(state), state_names)
  
  env <- list2env(c(param_env, state_env))
  
  derivs <- numeric(length(state))
  ode_lines <- strsplit(ode_system, "\n")[[1]]
  ode_lines <- ode_lines[nzchar(trimws(ode_lines))]
  
  for (i in seq_along(state)) {
    rhs_expr <- sub("^.*=\\s*", "", trimws(ode_lines[i]))
    derivs[i] <- eval(parse(text = rhs_expr), envir = env)
  }
  
  list(derivs)
}

sample_from_prior <- function(prior_string, n_samples = 1) {
  extract_numbers <- function(text) {
    matches <- gregexpr("-?\\d*\\.?\\d+(e-?\\d+)?", text, perl = TRUE)
    as.numeric(unlist(regmatches(text, matches)))
  }
  
  truncate_zero <- grepl("T\\[0,", prior_string)
  
  if (grepl("uniform", prior_string)) {
    bounds <- extract_numbers(prior_string)
    samples <- runif(n_samples, min = bounds[1], max = bounds[2])
    
  } else if (grepl("normal", prior_string)) {
    params <- extract_numbers(prior_string)
    samples <- rnorm(n_samples, mean = params[1], sd = params[2])
    
  } else if (grepl("cauchy", prior_string)) {
    params <- extract_numbers(prior_string)
    samples <- rt(n_samples, df = 1) * params[2] + params[1]
    
  } else if (grepl("exponential", prior_string)) {
    rate <- extract_numbers(prior_string)[1]
    samples <- rexp(n_samples, rate = rate)
    
  } else if (grepl("gamma", prior_string)) {
    params <- extract_numbers(prior_string)
    samples <- rgamma(n_samples, shape = params[1], rate = params[2])
    
  } else if (grepl("beta", prior_string)) {
    params <- extract_numbers(prior_string)
    samples <- rbeta(n_samples, shape1 = params[1], shape2 = params[2])
    
  } else {
    samples <- rep(as.numeric(prior_string), n_samples)
  }
  
  if (truncate_zero) {
    samples <- pmax(samples, 0)
  }
  
  return(samples)
}

set.seed(123)

get_param_values <- function(n_samples = 1) {
  param_matrix <- matrix(NA, nrow = n_samples, ncol = length(params))
  colnames(param_matrix) <- paste0("params", seq_along(params))
  
  for (i in seq_along(params)) {
    prior_val <- get(paste0("params", i, "_prior"))
    if (paramsfix[i] == 1) {
      param_matrix[, i] <- rep(as.numeric(prior_val), n_samples)
    } else {
      param_matrix[, i] <- sample_from_prior(prior_val, n_samples)
    }
  }
  
  return(param_matrix)
}

Mydata <- read_excel(paste0(cadfilename1, ".xlsx"))
times <- seq(0, max(Mydata$days), by = 1)

cat("=== SINGLE SAMPLE FROM PRIOR ===\n")

param_values <- get_param_values(1)[1, ]
names(param_values) <- paste0("params", seq_along(params))

cat("Sampled parameter values:\n")
for (i in seq_along(params)) {
  cat(sprintf("%s (params%d): %.4f\n", params[i], i, param_values[i]))
}

ode_out <- ode(y = init_values, times = times, func = dynamic_model, parms = param_values)
sim_result <- as.data.frame(ode_out)
colnames(sim_result) <- c("time", paste0("vars", seq_along(state_names)))

matplot(sim_result$time, sim_result[, -1], type = "l", lty = 1, lwd = 2,
        xlab = "Time", ylab = "Population", col = 1:length(state_names),
        main = "Model State Variables (Single Prior Sample)")
legend("right", legend = state_names, col = 1:length(state_names), lty = 1, lwd = 2)

n_fitting <- length(fitting_index)

for (fit_idx in 1:n_fitting) {
  var_index <- fitting_index[fit_idx]
  use_diff <- fitting_diff[fit_idx]
  
  if (use_diff == 1) {
    rhs_expr <- ode_rhs(var_index, ode_system)
    fit_values <- sapply(1:nrow(sim_result), function(i) {
      param_names <- paste0("params", seq_along(param_values))
      param_env <- setNames(as.list(param_values), param_names)
      state_cols <- paste0("vars", seq_along(state_names))
      state_values <- as.numeric(sim_result[i, state_cols])
      state_env <- setNames(as.list(state_values), state_cols)
      env <- list2env(c(param_env, state_env))
      eval(parse(text = rhs_expr), envir = env)
    })
    sim_result[[paste0("fit_var", fit_idx)]] <- fit_values
  } else {
    varname <- paste0("vars", var_index)
    sim_result[[paste0("fit_var", fit_idx)]] <- sim_result[[varname]]
  }
}

plot_list <- list()

for (fit_idx in 1:n_fitting) {
  var_index <- fitting_index[fit_idx]
  use_diff <- fitting_diff[fit_idx]
  obs_col <- paste0("cases", fit_idx)
  
  plot_title <- paste("Model vs Observed -", vars[var_index])
  y_label <- ifelse(use_diff == 1, 
                    paste0("d", vars[var_index], "/dt"), 
                    vars[var_index])
  
  p <- ggplot() +
    geom_line(data = sim_result, 
              aes_string(x = "time", y = paste0("fit_var", fit_idx)), 
              color = "blue", linewidth = 1.2) +
    geom_point(data = Mydata, 
               aes_string(x = "days", y = obs_col), 
               color = "red", size = 2) +
    labs(title = plot_title,
         x = "Time",
         y = y_label) +
    theme_minimal()
  
  plot_list[[fit_idx]] <- p
}

if (n_fitting == 1) {
  print(plot_list[[1]])
} else if (n_fitting == 2) {
  grid.arrange(plot_list[[1]], plot_list[[2]], ncol = 2)
} else {
  n_cols <- ceiling(sqrt(n_fitting))
  do.call(grid.arrange, c(plot_list, ncol = n_cols))
}

cat("\n=== MULTIPLE SAMPLES FROM PRIOR ===\n")

n_prior_samples <- 100
param_samples <- get_param_values(n_prior_samples)

cat(sprintf("Generated %d samples from prior distributions\n", n_prior_samples))

all_trajectories <- list()

cat("Running model for each prior sample...\n")
for (sample_idx in 1:n_prior_samples) {
  if (sample_idx %% 20 == 0) cat("Sample", sample_idx, "of", n_prior_samples, "\n")
  
  current_params <- param_samples[sample_idx, ]
  names(current_params) <- paste0("params", seq_along(params))
  
  tryCatch({
    ode_out <- ode(y = init_values, times = times, func = dynamic_model, parms = current_params)
    sim_result_temp <- as.data.frame(ode_out)
    colnames(sim_result_temp) <- c("time", paste0("vars", seq_along(state_names)))
    sim_result_temp$sample_id <- sample_idx
    all_trajectories[[sample_idx]] <- sim_result_temp
  }, error = function(e) {
    cat("Error in sample", sample_idx, ":", e$message, "\n")
  })
}

if (length(all_trajectories) > 0) {
  combined_trajectories <- do.call(rbind, all_trajectories)
  
  uncertainty_plots <- list()
  
  for (fit_idx in 1:n_fitting) {
    var_index <- fitting_index[fit_idx]
    use_diff <- fitting_diff[fit_idx]
    obs_col <- paste0("cases", fit_idx)
    
    if (use_diff == 1) {
      rhs_expr <- ode_rhs(var_index, ode_system)
      
      fit_values_all <- list()
      for (sample_idx in 1:length(all_trajectories)) {
        if (!is.null(all_trajectories[[sample_idx]])) {
          sim_data <- all_trajectories[[sample_idx]]
          current_params <- param_samples[sample_idx, ]
          names(current_params) <- paste0("params", seq_along(params))
          
          fit_values <- sapply(1:nrow(sim_data), function(i) {
            param_env <- setNames(as.list(current_params), names(current_params))
            state_cols <- paste0("vars", seq_along(state_names))
            state_values <- as.numeric(sim_data[i, state_cols])
            state_env <- setNames(as.list(state_values), state_cols)
            env <- list2env(c(param_env, state_env))
            eval(parse(text = rhs_expr), envir = env)
          })
          
          fit_df <- data.frame(time = sim_data$time, fit_value = fit_values, sample_id = sample_idx)
          fit_values_all[[sample_idx]] <- fit_df
        }
      }
      
      combined_fit_data <- do.call(rbind, fit_values_all)
      
      quantile_data <- combined_fit_data %>%
        group_by(time) %>%
        summarise(
          median = median(fit_value, na.rm = TRUE),
          q025 = quantile(fit_value, 0.025, na.rm = TRUE),
          q975 = quantile(fit_value, 0.975, na.rm = TRUE),
          .groups = 'drop'
        )
      
      y_label <- paste0("d", vars[var_index], "/dt")
      
    } else {
      varname <- paste0("vars", var_index)
      
      quantile_data <- combined_trajectories %>%
        group_by(time) %>%
        summarise(
          median = median(.data[[varname]], na.rm = TRUE),
          q025 = quantile(.data[[varname]], 0.025, na.rm = TRUE),
          q975 = quantile(.data[[varname]], 0.975, na.rm = TRUE),
          .groups = 'drop'
        )
      
      y_label <- vars[var_index]
    }
    
    p <- ggplot() +
      geom_ribbon(data = quantile_data, 
                  aes(x = time, ymin = q025, ymax = q975), 
                  alpha = 0.3, fill = "blue") +
      geom_line(data = quantile_data, 
                aes(x = time, y = median), 
                color = "blue", linewidth = 1.2) +
      geom_point(data = Mydata, 
                 aes_string(x = "days", y = obs_col), 
                 color = "red", size = 2) +
      labs(title = paste("Prior Uncertainty (95% CI) -", vars[var_index]),
           x = "Time",
           y = y_label) +
      theme_minimal()
    
    uncertainty_plots[[fit_idx]] <- p
  }
  
  if (n_fitting == 1) {
    print(uncertainty_plots[[1]])
  } else if (n_fitting == 2) {
    grid.arrange(uncertainty_plots[[1]], uncertainty_plots[[2]], ncol = 2)
  } else {
    n_cols <- ceiling(sqrt(n_fitting))
    do.call(grid.arrange, c(uncertainty_plots, ncol = n_cols))
  }
  
  cat("\n=== PARAMETER SAMPLE SUMMARY ===\n")
  param_summary <- apply(param_samples, 2, function(x) {
    c(Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x))
  })
  print(round(param_summary, 4))
}

cat("\n=== PLOTTING ALL STATE VARIABLES ===\n")

if (length(all_trajectories) > 0) {
  
  all_state_quantiles <- list()
  
  for (var_idx in 1:length(state_names)) {
    varname <- paste0("vars", var_idx)
    
    quantile_data <- combined_trajectories %>%
      group_by(time) %>%
      summarise(
        median = median(get(varname), na.rm = TRUE),
        q025 = quantile(get(varname), 0.025, na.rm = TRUE),
        q975 = quantile(get(varname), 0.975, na.rm = TRUE),
        variable = state_names[var_idx],
        .groups = 'drop'
      )
    
    all_state_quantiles[[var_idx]] <- quantile_data
  }
  
  all_states_data <- do.call(rbind, all_state_quantiles)
  
  n_states <- length(state_names)
  state_colors <- rainbow(n_states)
  names(state_colors) <- state_names
  
  all_states_plot <- ggplot(all_states_data) +
    geom_ribbon(aes(x = time, ymin = q025, ymax = q975, fill = variable), 
                alpha = 0.2) +
    geom_line(aes(x = time, y = median, color = variable), 
              linewidth = 1.2) +
    scale_color_manual(values = state_colors, name = "State Variables") +
    scale_fill_manual(values = state_colors, name = "95% CI") +
    labs(title = "All ODE State Variables with Prior Uncertainty (95% CI)",
         x = "Time",
         y = "Population") +
    theme_minimal() +
    theme(legend.position = "right")
  
  print(all_states_plot)
  
  cat("\n=== FACETED PLOT OF ALL STATE VARIABLES ===\n")
  
  faceted_plot <- ggplot(all_states_data) +
    geom_ribbon(aes(x = time, ymin = q025, ymax = q975), 
                alpha = 0.3, fill = "blue") +
    geom_line(aes(x = time, y = median), 
              color = "blue", linewidth = 1.2) +
    facet_wrap(~ variable, scales = "free_y", ncol = 2) +
    labs(title = "All ODE State Variables - Faceted View",
         x = "Time",
         y = "Population") +
    theme_minimal() +
    theme(strip.text = element_text(size = 12, face = "bold"))
  
  print(faceted_plot)
  
  cat("\n=== LOG SCALE VERSION ===\n")
  
  min_vals <- all_states_data %>%
    group_by(variable) %>%
    summarise(min_val = min(q025, na.rm = TRUE), .groups = 'drop')
  
  if (all(min_vals$min_val > 0)) {
    log_scale_plot <- ggplot(all_states_data) +
      geom_ribbon(aes(x = time, ymin = q025, ymax = q975, fill = variable), 
                  alpha = 0.2) +
      geom_line(aes(x = time, y = median, color = variable), 
                linewidth = 1.2) +
      scale_color_manual(values = state_colors, name = "State Variables") +
      scale_fill_manual(values = state_colors, name = "95% CI") +
      scale_y_log10() +
      labs(title = "All ODE State Variables (Log Scale) with Prior Uncertainty",
           x = "Time",
           y = "Population (Log Scale)") +
      theme_minimal() +
      theme(legend.position = "right")
    
    print(log_scale_plot)
  } else {
    cat("Log scale plot skipped: some variables have zero or negative values\n")
  }
  
  cat("\n=== FINAL STATE VALUES SUMMARY ===\n")
  
  final_time <- max(all_states_data$time)
  final_states <- all_states_data %>%
    filter(time == final_time) %>%
    select(variable, median, q025, q975)
  
  cat("Final state values at time", final_time, ":\n")
  print(final_states)
  
  cat("\n=== PEAK VALUES SUMMARY ===\n")
  
  peak_values <- all_states_data %>%
    group_by(variable) %>%
    summarise(
      peak_median = max(median, na.rm = TRUE),
      peak_time_median = time[which.max(median)],
      peak_upper = max(q975, na.rm = TRUE),
      .groups = 'drop'
    )
  
  cat("Peak values for each state variable:\n")
  print(peak_values)
}

cat("\n=== COMPARISON PLOT: SINGLE SAMPLE VS PRIOR UNCERTAINTY ===\n")

if (exists("sim_result") && length(all_trajectories) > 0) {
  
  single_sample_long <- sim_result %>%
    select(time, starts_with("vars")) %>%
    pivot_longer(cols = starts_with("vars"), 
                 names_to = "variable_code", 
                 values_to = "value") %>%
    mutate(variable = state_names[as.numeric(gsub("vars", "", variable_code))])
  
  comparison_plot <- ggplot() +
    geom_ribbon(data = all_states_data, 
                aes(x = time, ymin = q025, ymax = q975, fill = variable), 
                alpha = 0.2) +
    geom_line(data = all_states_data, 
              aes(x = time, y = median, color = variable), 
              linewidth = 1.2, linetype = "solid") +
    geom_line(data = single_sample_long, 
              aes(x = time, y = value, color = variable), 
              linewidth = 1, linetype = "dashed") +
    scale_color_manual(values = state_colors, name = "State Variables") +
    scale_fill_manual(values = state_colors, name = "95% CI") +
    labs(title = "Comparison: Single Sample (dashed) vs Prior Uncertainty (solid + ribbon)",
         x = "Time",
         y = "Population",
         caption = "Solid lines: Prior median, Dashed lines: Single sample, Ribbons: 95% CI") +
    theme_minimal() +
    theme(legend.position = "right")
  
  print(comparison_plot)
}

cat("\n=== ANALYSIS COMPLETE ===\n")