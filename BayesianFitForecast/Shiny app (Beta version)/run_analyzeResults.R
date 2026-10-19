#------------------------------------------------------------------------------#
# run_analyzeResults.R — Build figures, tables and diagnostics from the fits ----
#------------------------------------------------------------------------------#
# About: `run_analyzeResults()` receives the list returned by `run_MCMC2()`     #
# (ONE element PER calibration period) and returns, for every calibration       #
# period, the figures and tables displayed in the dashboard:                   #
#   * parameter / composite histograms, trace plots, posterior densities       #
#   * forecast plot + forecast table FOR EVERY FITTED TIME SERIES              #
#   * performance metrics (MAE, MSE, WIS, 95% PI coverage) per series          #
#   * parameter summary (median, mean, 95% CrI, n_eff, Rhat)                  #
#   * convergence summary (Rhat, ESS, divergences, treedepth, BFMI)            #
#   * model-fit metrics (DIC, WAIC, LOOIC) per series                          #
#   * time-dependent parameter curves                                          #
#   * posterior draws (for download)                                           #
#                                                                              #
# Fix (2026-10): the previous version used a single fit for every calibration  #
# period and overwrote the per-series outputs, so only the last series and the #
# last calibration period were (correctly) reported.                           #
#------------------------------------------------------------------------------#

  ###################
  # Needed packages #
  ###################
  library(rstan)
  library(ggplot2)
  library(bayesplot)

  ###########################
  # Needed source functions #
  ###########################
  source("Metric functions.R")
  if (!exists("bff_load_options", mode = "function")) source("run_MCMC2.R")

  `%||%` <- function(x, y) if (is.null(x)) y else x


#------------------------------------------------------------------------------#
# Time-dependent posterior curve helpers --------------------------------------
#------------------------------------------------------------------------------#
# These helpers are intentionally R-side only. They evaluate the already-written
# `time_dependent_templates` strings against posterior scalar-parameter samples
# and fixed parameter values from the options file.

tdp_analysis_draw_count <- function(param_samples, params, paramsfix) {
  if (is.null(params) || length(params) == 0) return(0)
  estimated <- params[as.numeric(paramsfix) == 0]
  lengths <- vapply(estimated, function(param) {
    sample <- param_samples[[param]]
    if (is.null(sample)) 0L else length(sample)
  }, integer(1))
  positive <- lengths[lengths > 0]
  if (length(positive) == 0) return(1L)
  min(positive)
}

tdp_analysis_fixed_value <- function(index, option_env) {
  prior_name <- paste0("params", index, "_prior")
  if (!exists(prior_name, envir = option_env, inherits = TRUE)) {
    stop("Missing fixed value for ", prior_name)
  }
  value <- get(prior_name, envir = option_env, inherits = TRUE)
  value <- suppressWarnings(as.numeric(value))
  if (length(value) == 0 || is.na(value[[1]])) {
    stop("Fixed value for ", prior_name, " is not numeric.")
  }
  value[[1]]
}

tdp_analysis_param_matrix <- function(param_samples, params, paramsfix,
                                      n_draws, option_env) {
  mat <- matrix(NA_real_, nrow = n_draws, ncol = length(params))
  colnames(mat) <- paste0("params", seq_along(params))

  for (idx in seq_along(params)) {
    if (as.numeric(paramsfix[[idx]]) == 1) {
      mat[, idx] <- tdp_analysis_fixed_value(idx, option_env)
    } else {
      samples <- suppressWarnings(as.numeric(param_samples[[params[[idx]]]]))
      if (length(samples) == 0 || all(is.na(samples))) {
        stop("Missing posterior samples for estimated parameter ", params[[idx]])
      }
      mat[, idx] <- samples[seq_len(n_draws)]
    }
  }
  mat
}

tdp_analysis_eval_template <- function(template, time_value, param_values) {
  template <- gsub("pi\\s*\\(\\s*\\)", "pi", template, perl = TRUE)
  env <- new.env(parent = baseenv())
  env$t <- time_value
  env$time <- time_value
  env$pow <- function(x, y) x^y
  for (idx in seq_along(param_values)) {
    assign(paste0("params", idx), param_values[[idx]], envir = env)
  }
  fn <- eval(parse(text = paste0("function() {\n", template, "\n}")), envir = env)
  as.numeric(fn())[[1]]
}

tdp_analysis_display_name <- function(template_name, display_names = NULL) {
  if (!is.null(display_names) && template_name %in% names(display_names)) {
    return(unname(display_names[[template_name]]))
  }
  template_name
}

# Convert a possibly-LaTeX display label (e.g. "\\beta(t)", "\\theta_1(t)") into
# a clean plain-text label for plot titles / axes (e.g. "beta(t)", "theta_1(t)").
tdp_analysis_plain_label <- function(label) {
  label <- trimws(as.character(label)[[1]])
  if (length(label) == 0 || is.na(label) || !nzchar(label)) return("f(t)")
  label <- gsub("\\\\", "", label)   # drop LaTeX backslashes: \beta -> beta
  label <- gsub("[{}$]", "", label)  # drop braces / math delimiters
  label <- trimws(label)
  if (!nzchar(label)) "f(t)" else label
}

tdp_analysis_outputs <- function(param_samples, params, paramsfix,
                                 time_dependent_templates, t_range,
                                 calibrationperiod, option_env,
                                 display_names = NULL,
                                 date_label = "time") {
  empty <- list(plots = list(), tables = data.frame())
  if (is.null(time_dependent_templates) || length(time_dependent_templates) == 0) {
    return(empty)
  }

  n_draws <- tdp_analysis_draw_count(param_samples, params, paramsfix)
  if (is.null(n_draws) || n_draws < 1) return(empty)

  param_matrix <- tdp_analysis_param_matrix(
    param_samples = param_samples,
    params = params,
    paramsfix = paramsfix,
    n_draws = n_draws,
    option_env = option_env
  )

  plots <- list()
  tables <- list()

  for (template_name in names(time_dependent_templates)) {
    template <- time_dependent_templates[[template_name]]
    values <- matrix(NA_real_, nrow = n_draws, ncol = length(t_range))

    for (draw_idx in seq_len(n_draws)) {
      for (time_idx in seq_along(t_range)) {
        values[draw_idx, time_idx] <- tdp_analysis_eval_template(
          template = template,
          time_value = t_range[[time_idx]],
          param_values = param_matrix[draw_idx, ]
        )
      }
    }

    # Summarize the posterior curves at each time point with a single median and
    # a single 95% percentile interval (2.5% / 97.5%). No 50% band -- the figure
    # mirrors the clean median + 95% interval style of the forecast plots.
    summary_df <- data.frame(
      CalibrationLength = calibrationperiod,
      TimeDependentParameter = template_name,
      DisplayName = tdp_analysis_display_name(template_name, display_names),
      time = t_range,
      median = apply(values, 2, median, na.rm = TRUE),
      lower_95 = apply(values, 2, quantile, probs = 0.025, na.rm = TRUE),
      upper_95 = apply(values, 2, quantile, probs = 0.975, na.rm = TRUE),
      check.names = FALSE
    )

    display_name <- tdp_analysis_plain_label(unique(summary_df$DisplayName)[[1]])
    plot_df <- summary_df
    p <- ggplot(plot_df, aes(x = time)) +
      geom_ribbon(aes(ymin = lower_95, ymax = upper_95), fill = "#5bc0de", alpha = 0.20) +
      geom_line(aes(y = median), color = "#337ab7", linewidth = 1) +
      labs(
        title = paste("Time-dependent parameter:", display_name),
        x = date_label,
        y = display_name
      ) +
      theme_classic()

    plots[[template_name]] <- p
    tables[[template_name]] <- summary_df
  }

  list(
    plots = plots,
    tables = do.call(rbind, tables)
  )
}


#------------------------------------------------------------------------------#
# Function: Obtaining Output for Bayesian Models -------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the function that outputs the needed analytic    #
# materials from the RStan results. This includes both figures and data sets.  #
# The results are saved in a list, and then exported to the primary dashboard. #
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Small formatting / plotting helpers -------------------------------------------
#------------------------------------------------------------------------------#

# Compact number formatting for plot titles (4 significant digits)
bff_fmt <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || is.na(x)) return("NA")
  formatC(signif(x, 4), format = "fg", big.mark = ",")
}

# ggplot histogram with the summary statistics in the title
bff_hist_plot <- function(samples, label) {
  samples <- as.numeric(samples)
  samples <- samples[is.finite(samples)]
  q   <- stats::quantile(samples, probs = c(0.025, 0.975))
  med <- stats::median(samples)
  mn  <- mean(samples)
  df  <- data.frame(x = samples)
  ggplot(df, aes(x = x)) +
    geom_histogram(bins = 40, fill = "lightblue", color = "black") +
    labs(
      title = sprintf("%s  |  Median: %s   Mean: %s   95%% CrI: (%s, %s)",
                      label, bff_fmt(med), bff_fmt(mn), bff_fmt(q[1]), bff_fmt(q[2])),
      x = label, y = "Frequency"
    ) +
    theme_classic() +
    theme(plot.title = element_text(size = 11))
}

# Forecast / fit plot (observed data, posterior median, 95% PI) with a legend
bff_forecast_plot <- function(data, datetype, series_label, calibrationperiod,
                              forecastinghorizon) {
  plot <- ggplot(data, aes(x = Date)) +
    geom_ribbon(aes(ymin = LB, ymax = UB), fill = "grey80", alpha = 0.45) +
    geom_line(aes(y = LB, color = "95% PI", linetype = "95% PI"), linewidth = 0.5) +
    geom_line(aes(y = UB, color = "95% PI", linetype = "95% PI"), linewidth = 0.5) +
    geom_line(aes(y = median, color = "Posterior median", linetype = "Posterior median"),
              linewidth = 1) +
    geom_point(aes(y = Data, shape = "Observed data"), color = "black", fill = NA,
               size = 1.8, stroke = 0.7, na.rm = TRUE) +
    scale_color_manual(values = c("Posterior median" = "red", "95% PI" = "black")) +
    scale_linetype_manual(values = c("Posterior median" = "solid", "95% PI" = "dashed")) +
    scale_shape_manual(values = c("Observed data" = 21)) +
    guides(
      shape = guide_legend(order = 1, override.aes = list(fill = NA)),
      color = guide_legend(order = 2),
      linetype = guide_legend(order = 2)
    ) +
    labs(title = sprintf("%s  (calibration: %s %s%s)", series_label, calibrationperiod,
                         tolower(datetype),
                         if (forecastinghorizon > 0) sprintf(", forecast: %s", forecastinghorizon) else ""),
         x = datetype, y = series_label, color = NULL, linetype = NULL, shape = NULL) +
    theme_classic() +
    theme(legend.position = "top", legend.direction = "horizontal",
          legend.box = "horizontal", legend.title = element_blank())
  if (forecastinghorizon > 0) {
    plot <- plot + geom_vline(xintercept = calibrationperiod - 1, linetype = "dotted",
                              linewidth = 1, color = "blue", show.legend = FALSE)
  }
  plot
}

#------------------------------------------------------------------------------#
# Convergence summary (Rhat, ESS, divergences, treedepth, BFMI) -----------------
#------------------------------------------------------------------------------#
bff_convergence_summary <- function(fit, pars, calibrationperiod) {

  sm <- tryCatch(rstan::summary(fit, pars = pars)$summary, error = function(e) NULL)
  max_rhat <- if (!is.null(sm) && "Rhat" %in% colnames(sm)) suppressWarnings(max(sm[, "Rhat"], na.rm = TRUE)) else NA_real_
  min_neff <- if (!is.null(sm) && "n_eff" %in% colnames(sm)) suppressWarnings(min(sm[, "n_eff"], na.rm = TRUE)) else NA_real_
  if (!is.finite(max_rhat)) max_rhat <- NA_real_
  if (!is.finite(min_neff)) min_neff <- NA_real_

  n_div  <- tryCatch(rstan::get_num_divergent(fit), error = function(e) NA_integer_)
  n_td   <- tryCatch(rstan::get_num_max_treedepth(fit), error = function(e) NA_integer_)
  n_bfmi <- tryCatch(length(rstan::get_low_bfmi_chains(fit)), error = function(e) NA_integer_)

  n_chains <- tryCatch(fit@sim$chains, error = function(e) NA_integer_)
  n_iter   <- tryCatch(fit@sim$iter,   error = function(e) NA_integer_)
  n_warm   <- tryCatch(fit@sim$warmup, error = function(e) NA_integer_)
  n_thin   <- tryCatch(fit@sim$thin,   error = function(e) 1L)
  n_draws  <- tryCatch(n_chains * floor((n_iter - n_warm) / n_thin), error = function(e) NA_integer_)

  level <- 0  # 0 = good, 1 = caution, 2 = poor
  advice <- character(0)

  if (!is.na(max_rhat)) {
    if (max_rhat > 1.1) {
      level <- max(level, 2)
      advice <- c(advice, sprintf("Rhat = %.2f > 1.1: chains have not mixed. Increase the number of iterations, check the priors / identifiability of the model.", max_rhat))
    } else if (max_rhat > 1.05) {
      level <- max(level, 1)
      advice <- c(advice, sprintf("Rhat = %.2f is slightly above 1.05: consider running more iterations.", max_rhat))
    }
  }
  if (!is.na(n_div) && n_div > 0) {
    frac <- if (!is.na(n_draws) && n_draws > 0) n_div / n_draws else NA
    if (!is.na(frac) && frac < 0.01) {
      level <- max(level, 1)
      advice <- c(advice, sprintf("%d divergent transition(s): increase adapt_delta (e.g. 0.95) in the sampler settings.", n_div))
    } else {
      level <- max(level, 2)
      advice <- c(advice, sprintf("%d divergent transitions: the posterior is hard to explore. Increase adapt_delta (e.g. 0.95-0.99), reparameterize or tighten weakly-informative priors.", n_div))
    }
  }
  if (!is.na(n_td) && n_td > 0) {
    level <- max(level, 1)
    advice <- c(advice, sprintf("%d transition(s) hit the maximum treedepth: increase max_treedepth (e.g. 12-15).", n_td))
  }
  if (!is.na(n_bfmi) && n_bfmi > 0) {
    level <- max(level, 1)
    advice <- c(advice, sprintf("%d chain(s) with low E-BFMI: the energy transitions are inefficient; reparameterization may help.", n_bfmi))
  }
  if (!is.na(min_neff) && !is.na(n_chains)) {
    if (min_neff < 50 * n_chains) {
      level <- max(level, 2)
      advice <- c(advice, sprintf("Smallest effective sample size is %s: the posterior summaries are unreliable. Run more iterations.", bff_fmt(min_neff)))
    } else if (min_neff < 100 * n_chains) {
      level <- max(level, 1)
      advice <- c(advice, sprintf("Smallest effective sample size is %s: consider running more iterations.", bff_fmt(min_neff)))
    }
  }
  status <- c("Good", "Caution", "Poor")[level + 1]
  if (length(advice) == 0) advice <- "No convergence problems detected."

  data.frame(
    CalibrationLength = calibrationperiod,
    Chains = n_chains,
    Iterations = n_iter,
    Warmup = n_warm,
    PosteriorDraws = n_draws,
    MaxRhat = round(max_rhat, 3),
    MinESS = round(min_neff, 1),
    Divergences = n_div,
    MaxTreedepthHits = n_td,
    LowBFMIChains = n_bfmi,
    Status = status,
    Advice = paste(advice, collapse = " "),
    stringsAsFactors = FALSE
  )
}

#------------------------------------------------------------------------------#
# Model-fit metrics (DIC, WAIC, LOOIC) for one fitted time series ---------------
#------------------------------------------------------------------------------#
# About: Uses the pointwise log-likelihood generated by Stan (`log_likK`) over   #
# the calibration window. DIC uses the plug-in deviance at the posterior mean   #
# of the expected value (`mu_casesK`) and of the dispersion / scale parameter.  #
#------------------------------------------------------------------------------#
bff_model_fit_metrics <- function(fit, series_index, observed, errstrc, aux_samples,
                                  calibrationperiod, series_label) {

  empty <- data.frame(CalibrationLength = calibrationperiod, Series = series_label,
                      DIC = NA_real_, pD = NA_real_, WAIC = NA_real_, p_waic = NA_real_,
                      LOOIC = NA_real_, p_loo = NA_real_, HighParetoK = NA_integer_,
                      Note = "log_lik not available in the Stan program",
                      stringsAsFactors = FALSE)

  log_lik <- tryCatch(rstan::extract(fit, pars = paste0("log_lik", series_index))[[1]],
                      error = function(e) NULL)
  if (is.null(log_lik)) return(empty)
  log_lik <- as.matrix(log_lik)

  ok <- apply(log_lik, 1, function(r) all(is.finite(r)))
  if (sum(ok) < 10) {
    empty$Note <- "Too few finite log-likelihood draws"
    return(empty)
  }
  log_lik <- log_lik[ok, , drop = FALSE]

  ## ---- DIC (plug-in at posterior means) ----
  deviance_draws <- -2 * rowSums(log_lik)
  D_bar <- mean(deviance_draws)

  mu_draws <- tryCatch(rstan::extract(fit, pars = paste0("mu_cases", series_index))[[1]],
                       error = function(e) NULL)
  D_hat <- NA_real_
  if (!is.null(mu_draws)) {
    mu_draws <- as.matrix(mu_draws)[ok, seq_len(ncol(log_lik)), drop = FALSE]
    mu_bar <- colMeans(mu_draws)
    mu_bar[mu_bar <= 0] <- 1e-10
    obs <- observed[seq_len(ncol(log_lik))]
    if (errstrc == 1) {
      phi_bar <- mean(aux_samples[ok])
      D_hat <- -2 * sum(dnbinom(round(obs), size = max(phi_bar, 1e-6), mu = mu_bar, log = TRUE))
    } else if (errstrc == 2) {
      sigma_bar <- mean(aux_samples[ok])
      D_hat <- -2 * sum(dnorm(obs, mean = mu_bar, sd = max(sigma_bar, 1e-6), log = TRUE))
    } else {
      D_hat <- -2 * sum(dpois(round(obs), lambda = mu_bar, log = TRUE))
    }
  }
  pD  <- if (is.finite(D_hat)) D_bar - D_hat else stats::var(deviance_draws) / 2
  DIC <- D_bar + pD

  ## ---- WAIC & LOO (loo package) ----
  waic_est <- c(NA_real_, NA_real_)
  loo_est  <- c(NA_real_, NA_real_)
  high_k   <- NA_integer_
  note     <- if (is.finite(D_hat)) "" else "DIC uses pD = var(deviance)/2"
  if (requireNamespace("loo", quietly = TRUE)) {
    w <- tryCatch(suppressWarnings(loo::waic(log_lik)), error = function(e) NULL)
    if (!is.null(w)) waic_est <- c(w$estimates["waic", "Estimate"], w$estimates["p_waic", "Estimate"])
    l <- tryCatch(suppressWarnings(loo::loo(log_lik, r_eff = NA)), error = function(e) NULL)
    if (!is.null(l)) {
      loo_est <- c(l$estimates["looic", "Estimate"], l$estimates["p_loo", "Estimate"])
      k <- tryCatch(loo::pareto_k_values(l), error = function(e) NULL)
      if (!is.null(k)) high_k <- sum(k > 0.7, na.rm = TRUE)
      if (!is.na(high_k) && high_k > 0) {
        note <- paste(note, sprintf("%d observation(s) with Pareto k > 0.7: LOOIC may be unreliable.", high_k))
      }
    }
  } else {
    note <- paste(note, "Package 'loo' not installed: WAIC/LOOIC skipped.")
  }

  data.frame(CalibrationLength = calibrationperiod, Series = series_label,
             DIC = round(DIC, 2), pD = round(pD, 2),
             WAIC = round(waic_est[1], 2), p_waic = round(waic_est[2], 2),
             LOOIC = round(loo_est[1], 2), p_loo = round(loo_est[2], 2),
             HighParetoK = high_k, Note = trimws(note),
             stringsAsFactors = FALSE)
}

#------------------------------------------------------------------------------#
# Main function ----------------------------------------------------------------
#------------------------------------------------------------------------------#
run_analyzeResults <- function(optionsFile, MCMCOutput, data.temp, stan_warnings = list()) {

  opt <- bff_load_options(optionsFile)

  calibrationperiods <- as.numeric(opt$calibrationperiods)
  forecastinghorizon <- as.numeric(opt$forecastinghorizon)
  fitting_index      <- opt$fitting_index
  errstrc            <- opt$errstrc
  params             <- opt$params
  paramsfix          <- as.numeric(opt$paramsfix)
  datetype           <- opt$datetype %||% "Time"
  series_cases       <- opt$series_cases %||% character(0)
  time_dependent_templates <- opt$time_dependent_templates %||% list()
  display_names <- if (exists("time_dependent_display_names", envir = opt, inherits = FALSE)) {
    opt$time_dependent_display_names
  } else NULL

  n_series <- length(fitting_index)
  series_labels <- vapply(seq_len(n_series), function(i) {
    lab <- if (i <= length(series_cases)) trimws(as.character(series_cases[i])) else ""
    if (!nzchar(lab) || is.na(lab)) paste0("Series ", i) else lab
  }, character(1))
  # Make labels unique (two series with the same label would collide)
  series_labels <- make.unique(series_labels, sep = " #")

  Mydata <- data.temp
  actualcases <- lapply(seq_len(n_series), function(i) as.numeric(Mydata[[paste0("cases", i)]]))

  # Accept the legacy single-fit structure as well (list with element names)
  if (!is.null(names(MCMCOutput)) && "param_samples" %in% names(MCMCOutput)) {
    MCMCOutput <- list(MCMCOutput)
    names(MCMCOutput) <- as.character(calibrationperiods[1])
  }
  if (length(MCMCOutput) != length(calibrationperiods)) {
    stop(sprintf("Expected results for %d calibration period(s) but received %d.",
                 length(calibrationperiods), length(MCMCOutput)))
  }

  calibration.period.list <- vector("list", length(calibrationperiods))
  names(calibration.period.list) <- as.character(calibrationperiods)

  for (index in seq_along(calibrationperiods)) {

    calibrationperiod <- calibrationperiods[index]
    res <- MCMCOutput[[index]]

    param_samples         <- res$param_samples
    pred_cases            <- res$pred_cases
    fit_ode_model         <- res$fit_ode_model
    pars                  <- unlist(res$pars)
    composite_expressions <- res$composite_expressions %||% list()
    composite_samples     <- res$composite_samples %||% list()

    n_total <- calibrationperiod + forecastinghorizon

    ## ---- Posterior predictive summaries per series ----
    my_solutionaggr <- lapply(seq_len(n_series), function(i) {
      m <- pred_cases[, , i]
      if (is.null(dim(m))) m <- matrix(m, ncol = 1)
      m
    })
    medcurve <- lapply(my_solutionaggr, function(m) apply(m, 2, stats::median))
    mcmc_intervals_aggr <- lapply(my_solutionaggr, function(m) {
      apply(m, 2, stats::quantile, probs = c(0.025, 0.975))
    })

    ## ---- Parameter histograms ----
    plot.list.params <- list()
    for (p in pars) {
      current_samples <- param_samples[[p]]
      if (length(current_samples) == 0) next
      plot.list.params[[p]] <- bff_hist_plot(current_samples, p)
    }

    ## ---- Composite-expression histograms ----
    plot.list.composite <- list()
    for (name in names(composite_expressions)) {
      current_samples <- composite_samples[[name]]
      if (length(current_samples) == 0) next
      plot.list.composite[[name]] <- bff_hist_plot(current_samples, name)
    }

    ## ---- Trace and posterior-density plots ----
    plot.list.trace <- tryCatch(rstan::traceplot(fit_ode_model, pars = pars),
                                error = function(e) NULL)
    plot.list.posterior <- tryCatch(rstan::stan_dens(fit_ode_model, pars = pars, separate_chains = TRUE),
                                    error = function(e) NULL)

    ## ---- Performance metrics, forecast tables and forecast plots (per series) ----
    performance.metrics <- data.frame()
    forecasts.fits <- list()
    forecasts.fits.plot <- list()
    model.fit.metrics <- data.frame()

    for (i in seq_len(n_series)) {

      obs_all <- actualcases[[i]]
      actual_calibration <- obs_all[seq_len(calibrationperiod)]
      medcurve_calibration <- medcurve[[i]][seq_len(calibrationperiod)]
      sol_calibration <- my_solutionaggr[[i]][, seq_len(calibrationperiod), drop = FALSE]

      mae_cal <- calculate_mae(actual_calibration, medcurve_calibration)
      mse_cal <- calculate_mse(actual_calibration, medcurve_calibration)
      wis_cal <- calculate_WIS(sol_calibration, actual_calibration)
      cov_cal <- calculate_percent_within_interval_calibration(actual_calibration, mcmc_intervals_aggr[[i]])

      mae_fc <- mse_fc <- wis_fc <- cov_fc <- NA_real_
      n_fc_eval <- 0L
      if (forecastinghorizon > 0) {
        fc_idx <- (calibrationperiod + 1):(calibrationperiod + forecastinghorizon)
        fc_idx <- fc_idx[fc_idx <= length(obs_all)]
        fc_idx <- fc_idx[!is.na(obs_all[fc_idx])]
        n_fc_eval <- length(fc_idx)
        if (n_fc_eval > 0) {
          actual_forecast <- obs_all[fc_idx]
          medcurve_forecast <- medcurve[[i]][fc_idx]
          sol_forecast <- my_solutionaggr[[i]][, fc_idx, drop = FALSE]
          mae_fc <- calculate_mae(actual_forecast, medcurve_forecast)
          mse_fc <- calculate_mse(actual_forecast, medcurve_forecast)
          wis_fc <- calculate_WIS(sol_forecast, actual_forecast)
          cov_fc <- calculate_percent_within_interval_forecast(actual_forecast,
                                                               mcmc_intervals_aggr[[i]],
                                                               calibrationperiod)
        }
      }

      performance.metrics <- rbind(performance.metrics, data.frame(
        Series = series_labels[i],
        Metrics = c("MAE", "MSE", "WIS", "Coverage of 95% PI"),
        Calibration = c(mae_cal, mse_cal, wis_cal, cov_cal),
        Forecasting = c(mae_fc, mse_fc, wis_fc, cov_fc),
        CalibrationLength = calibrationperiod,
        ForecastPointsEvaluated = n_fc_eval,
        stringsAsFactors = FALSE
      ))

      ## Forecast table (observed, median, 95% PI)
      cases_i <- obs_all[seq_len(min(n_total, length(obs_all)))]
      fc_table <- data.frame(
        Date   = 0:(n_total - 1),
        Data   = c(cases_i, rep(NA, n_total - length(cases_i))),
        median = round(medcurve[[i]][seq_len(n_total)], 1),
        LB     = pmax(0, round(mcmc_intervals_aggr[[i]][1, seq_len(n_total)], 1)),
        UB     = round(mcmc_intervals_aggr[[i]][2, seq_len(n_total)], 1),
        Period = c(rep("Calibration", calibrationperiod), rep("Forecast", forecastinghorizon)),
        stringsAsFactors = FALSE
      )
      forecasts.fits[[series_labels[i]]] <- fc_table
      forecasts.fits.plot[[series_labels[i]]] <- bff_forecast_plot(
        fc_table, datetype, series_labels[i], calibrationperiod, forecastinghorizon
      )

      ## Model-fit metrics (DIC / WAIC / LOOIC)
      aux <- if (errstrc == 1) param_samples[[paste0("phi", i)]] else if (errstrc == 2) param_samples[[paste0("sigma", i)]] else NULL
      mfm <- tryCatch(
        bff_model_fit_metrics(fit_ode_model, i, actual_calibration, errstrc, aux,
                              calibrationperiod, series_labels[i]),
        error = function(e) data.frame(CalibrationLength = calibrationperiod, Series = series_labels[i],
                                       DIC = NA, pD = NA, WAIC = NA, p_waic = NA, LOOIC = NA, p_loo = NA,
                                       HighParetoK = NA, Note = conditionMessage(e), stringsAsFactors = FALSE)
      )
      model.fit.metrics <- rbind(model.fit.metrics, mfm)
    }

    ## ---- Parameter summary tables ----
    result_data1 <- data.frame()
    result_data2 <- data.frame()
    fit_summary <- tryCatch(rstan::summary(fit_ode_model, pars = pars)$summary, error = function(e) NULL)
    for (p in pars) {
      current_samples <- param_samples[[p]]
      if (length(current_samples) == 0) next
      mcmc_interval <- stats::quantile(current_samples, prob = c(.025, .975))
      median_value  <- stats::median(current_samples)
      mean_value    <- mean(current_samples)
      result_data1 <- rbind(result_data1, data.frame(
        CalibrationLength = calibrationperiod,
        parameter = p,
        median = signif(median_value, 4),
        mean = signif(mean_value, 4),
        `95% CrI lower` = signif(mcmc_interval[1], 4),
        `95% CrI upper` = signif(mcmc_interval[2], 4),
        check.names = FALSE, row.names = NULL
      ))
      n_eff_value <- if (!is.null(fit_summary) && p %in% rownames(fit_summary)) round(fit_summary[p, "n_eff"], 1) else NA
      rhat_value  <- if (!is.null(fit_summary) && p %in% rownames(fit_summary)) round(fit_summary[p, "Rhat"], 3) else NA
      result_data2 <- rbind(result_data2, data.frame(
        CalibrationLength = calibrationperiod,
        Parameter = p,
        Mean = signif(mean_value, 4),
        Median = signif(median_value, 4),
        `95% CrI` = sprintf("(%s, %s)", bff_fmt(mcmc_interval[1]), bff_fmt(mcmc_interval[2])),
        N_eff = n_eff_value,
        Rhat = rhat_value,
        check.names = FALSE, row.names = NULL
      ))
    }
    # Composite expressions are reported in the same summary table
    for (name in names(composite_expressions)) {
      current_samples <- composite_samples[[name]]
      if (length(current_samples) == 0) next
      mcmc_interval <- stats::quantile(current_samples, prob = c(.025, .975))
      result_data2 <- rbind(result_data2, data.frame(
        CalibrationLength = calibrationperiod,
        Parameter = paste0(name, " (composite)"),
        Mean = signif(mean(current_samples), 4),
        Median = signif(stats::median(current_samples), 4),
        `95% CrI` = sprintf("(%s, %s)", bff_fmt(mcmc_interval[1]), bff_fmt(mcmc_interval[2])),
        N_eff = NA, Rhat = NA,
        check.names = FALSE, row.names = NULL
      ))
    }

    ## ---- Convergence summary ----
    convergence.summary <- tryCatch(
      bff_convergence_summary(fit_ode_model, pars, calibrationperiod),
      error = function(e) data.frame(CalibrationLength = calibrationperiod, Status = "Unknown",
                                     Advice = conditionMessage(e), stringsAsFactors = FALSE)
    )

    ## ---- Stan warnings captured during sampling (passed in from the app) ----
    warn_key <- as.character(calibrationperiod)
    this_warnings <- if (!is.null(stan_warnings[[warn_key]])) stan_warnings[[warn_key]] else character(0)

    ## ---- Posterior draws for download ----
    posterior.draws <- tryCatch({
      as.data.frame(fit_ode_model, pars = c(pars, names(composite_expressions)))
    }, error = function(e) {
      tryCatch(as.data.frame(param_samples), error = function(e2) data.frame())
    })

    ## ---- Time-dependent parameters ----
    tdp.result <- list(plots = list(), tables = data.frame())
    if (length(time_dependent_templates) > 0) {
      tdp.result <- tryCatch(
        tdp_analysis_outputs(
          param_samples = param_samples,
          params = params,
          paramsfix = paramsfix,
          time_dependent_templates = time_dependent_templates,
          t_range = 0:(n_total - 1),
          calibrationperiod = calibrationperiod,
          option_env = opt,
          display_names = display_names,
          date_label = datetype
        ),
        error = function(e) {
          message("Time-dependent posterior curves skipped: ", conditionMessage(e))
          list(plots = list(), tables = data.frame())
        }
      )
    }

    calibration.period.list[[index]] <- list(
      plot.list.params          = plot.list.params,
      plot.list.composite       = plot.list.composite,
      plot.list.trace           = plot.list.trace,
      plot.list.posterior       = plot.list.posterior,
      performance.metrics       = performance.metrics,
      forecasts.fits            = forecasts.fits,
      forecasts.fits.plot       = forecasts.fits.plot,
      plot.list.time.dependent  = tdp.result$plots,
      time.dependent.values     = tdp.result$tables,
      parameter.values          = result_data1,
      add.parameter.values      = result_data2,
      model.fit.metrics         = model.fit.metrics,
      convergence.summary       = convergence.summary,
      stan.warnings             = this_warnings,
      posterior.draws           = posterior.draws,
      series.labels             = series_labels
    )
  }

  calibration.period.list
}
