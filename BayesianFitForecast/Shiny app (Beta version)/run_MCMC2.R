#------------------------------------------------------------------------------#
# run_MCMC2.R — Extract posterior draws from the fitted Stan models -------------
#------------------------------------------------------------------------------#
# About: `run_MCMC2()` receives the list of fitted `stanfit` objects produced in #
# app.R (ONE fit PER calibration period) and returns a list with ONE element    #
# PER calibration period. Each element contains the posterior draws of the     #
# estimated parameters, the predicted trajectories, the fitted object, and the #
# composite-expression draws. `run_analyzeResults()` consumes this list.       #
#                                                                              #
# Fix (2026-10): the previous version overwrote its result inside the loop and #
# returned only the LAST calibration period, so every other calibration period #
# displayed mislabeled results. Results are now kept for every period.         #
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Helper: source an options file into its own environment ----------------------
#------------------------------------------------------------------------------#
# About: The option variables are kept out of the global environment so that   #
# several Shiny sessions never overwrite each other's settings.                #
#------------------------------------------------------------------------------#
bff_load_options <- function(optionsFile) {
  opt <- new.env(parent = globalenv())
  sys.source(normalizePath(optionsFile, winslash = "/", mustWork = TRUE), envir = opt)
  if (!exists("time_dependent_templates", envir = opt, inherits = FALSE)) {
    assign("time_dependent_templates", list(), envir = opt)
  }
  if (!exists("composite_expressions", envir = opt, inherits = FALSE)) {
    assign("composite_expressions", list(), envir = opt)
  }
  if (!exists("model_name", envir = opt, inherits = FALSE)) {
    assign("model_name", "model", envir = opt)
  }
  opt
}

#------------------------------------------------------------------------------#
# Helper: names of the sampled quantities (parameters + error-structure terms) #
#------------------------------------------------------------------------------#
bff_sampled_pars <- function(opt) {
  pars <- opt$params[as.numeric(opt$paramsfix) == 0]
  n_series <- length(opt$fitting_index)
  if (opt$errstrc == 1) {
    pars <- c(pars, paste0("phi", seq_len(n_series)))
  } else if (opt$errstrc == 2) {
    pars <- c(pars, paste0("sigma", seq_len(n_series)))
  }
  pars
}

#------------------------------------------------------------------------------#
# Main function ----------------------------------------------------------------
#------------------------------------------------------------------------------#
run_MCMC2 <- function(outputFile, data.temp, finalModlLIST) {

  opt <- bff_load_options(outputFile)

  calibrationperiods <- as.numeric(opt$calibrationperiods)
  fitting_index      <- opt$fitting_index
  errstrc            <- opt$errstrc
  composite_expressions <- opt$composite_expressions

  if (length(finalModlLIST) != length(calibrationperiods)) {
    stop(sprintf(paste0("Expected %d fitted model(s) (one per calibration period) ",
                        "but received %d."),
                 length(calibrationperiods), length(finalModlLIST)))
  }

  pars <- bff_sampled_pars(opt)

  results <- vector("list", length(calibrationperiods))
  names(results) <- as.character(calibrationperiods)

  for (h in seq_along(calibrationperiods)) {

    fit_ode_model <- finalModlLIST[[h]]
    if (is.null(fit_ode_model)) {
      stop(sprintf("No fitted model available for calibration period %s.",
                   calibrationperiods[h]))
    }

    s <- rstan::extract(fit_ode_model)

    # Posterior draws of every sampled quantity
    param_samples <- lapply(stats::setNames(pars, pars), function(p) s[[p]])

    # Predicted trajectories: iterations x time x series
    pred_cases_list <- lapply(seq_along(fitting_index), function(i) {
      s[[paste0("pred_cases", i)]]
    })
    pred_cases <- abind::abind(pred_cases_list, along = 3)

    phi_samples <- if (errstrc == 1) {
      sapply(seq_along(fitting_index), function(i) s[[paste0("phi", i)]])
    } else NULL

    sigma_samples <- if (errstrc == 2) {
      sapply(seq_along(fitting_index), function(i) s[[paste0("sigma", i)]])
    } else NULL

    composite_names <- names(composite_expressions)
    composite_samples <- lapply(stats::setNames(composite_names, composite_names),
                                function(nm) s[[nm]])

    results[[h]] <- list(
      param_samples         = param_samples,
      pred_cases            = pred_cases,
      fit_ode_model         = fit_ode_model,
      pars                  = pars,
      phi_samples           = phi_samples,
      sigma_samples         = sigma_samples,
      composite_expressions = composite_expressions,
      composite_samples     = composite_samples,
      calibrationperiod     = calibrationperiods[h]
    )
  }

  results
}
