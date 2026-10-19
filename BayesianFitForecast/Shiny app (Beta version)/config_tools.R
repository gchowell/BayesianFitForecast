#------------------------------------------------------------------------------#
# config_tools.R — Configuration (options.R) helpers ----------------------------
#------------------------------------------------------------------------------#
# About: Parsing of the calibration-length input, validation of an options.R    #
# configuration (syntax, required variables, consistency with the data, Stan    #
# generation) and Markdown helpers used by the run report.                      #
#------------------------------------------------------------------------------#

`%||%` <- function(x, y) if (is.null(x)) y else x

#------------------------------------------------------------------------------#
# Calibration length(s) --------------------------------------------------------
#------------------------------------------------------------------------------#
# "17" -> 17 ; "15, 17, 20" -> c(15, 17, 20) ; "15:20" or "15-20" -> 15..20       #
#------------------------------------------------------------------------------#
bff_parse_calibration_periods <- function(txt, n_rows) {
  txt <- trimws(as.character(txt %||% ""))
  if (!nzchar(txt)) return(list(values = n_rows, error = NULL))
  parts <- trimws(unlist(strsplit(txt, "[,;]+")))
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0) return(list(values = n_rows, error = NULL))
  vals <- integer(0)
  for (p in parts) {
    if (grepl("^[0-9]+\\s*[:-]\\s*[0-9]+$", p)) {
      ab <- as.integer(unlist(strsplit(p, "\\s*[:-]\\s*")))
      if (ab[1] > ab[2]) return(list(values = NULL, error = sprintf("Invalid range '%s'.", p)))
      if (ab[2] - ab[1] > 50) return(list(values = NULL, error = "A range can cover at most 50 calibration lengths."))
      vals <- c(vals, seq(ab[1], ab[2]))
    } else if (grepl("^[0-9]+$", p)) {
      vals <- c(vals, as.integer(p))
    } else {
      return(list(values = NULL, error = sprintf("'%s' is not a whole number (use e.g. 17 or 15, 17, 20 or 15:20).", p)))
    }
  }
  vals <- sort(unique(vals))
  if (any(vals < 3)) return(list(values = NULL, error = "A calibration length must be at least 3 time points."))
  if (any(vals > n_rows)) return(list(values = NULL, error = sprintf("A calibration length cannot exceed the %d time points in the data.", n_rows)))
  list(values = vals, error = NULL)
}

#------------------------------------------------------------------------------#
# Validation of an options.R text ------------------------------------------------
#------------------------------------------------------------------------------#
bff_validate_options_text <- function(txt, data = NULL) {

  errors <- character(0)
  warnings <- character(0)
  fail <- function(...) list(ok = FALSE, errors = c(errors, ...), warnings = warnings, summary = "", env = NULL)

  if (is.null(txt) || !nzchar(trimws(txt))) return(fail("The configuration is empty."))

  parsed <- tryCatch(parse(text = txt), error = function(e) e)
  if (inherits(parsed, "error")) return(fail(paste("R syntax error:", conditionMessage(parsed))))

  env <- new.env(parent = globalenv())
  res <- tryCatch(eval(parsed, envir = env), error = function(e) e)
  if (inherits(res, "error")) return(fail(paste("Error while evaluating the configuration:", conditionMessage(res))))

  required <- c("calibrationperiods", "forecastinghorizon", "vars", "params", "ode_system", "paramsfix",
                "fitting_index", "fitting_diff", "errstrc", "niter", "num_chain", "Ic", "vars.init")
  present <- vapply(required, function(v) exists(v, envir = env, inherits = FALSE), logical(1))
  if (any(!present)) return(fail(paste("Missing variable(s):", paste(required[!present], collapse = ", "))))

  if (!exists("time_dependent_templates", envir = env, inherits = FALSE)) assign("time_dependent_templates", list(), envir = env)
  if (!exists("composite_expressions", envir = env, inherits = FALSE)) assign("composite_expressions", list(), envir = env)
  if (!exists("model_name", envir = env, inherits = FALSE)) { assign("model_name", "model", envir = env); warnings <- c(warnings, "model_name was missing; 'model' is used.") }
  if (!exists("caddisease", envir = env, inherits = FALSE)) { assign("caddisease", "data", envir = env) }
  if (!exists("datetype", envir = env, inherits = FALSE)) { assign("datetype", "Time", envir = env) }

  vars <- as.character(env$vars); params <- as.character(env$params); paramsfix <- as.numeric(env$paramsfix)
  fitting_index <- as.numeric(env$fitting_index); fitting_diff <- as.numeric(env$fitting_diff)
  errstrc <- as.numeric(env$errstrc)

  if (length(paramsfix) != length(params)) errors <- c(errors, sprintf("paramsfix has %d entries but there are %d parameters.", length(paramsfix), length(params)))
  if (!all(paramsfix %in% c(0, 1))) errors <- c(errors, "paramsfix must contain only 0 (estimated) and 1 (fixed).")
  if (length(params) > 0 && all(paramsfix == 1)) errors <- c(errors, "At least one parameter must be estimated (paramsfix = 0).")
  if (any(duplicated(params))) errors <- c(errors, "Parameter names must be unique.")
  if (any(duplicated(vars))) errors <- c(errors, "State variable names must be unique.")
  if (any(params %in% vars)) errors <- c(errors, "A name is used both as a parameter and as a state variable.")
  bad_names <- c(vars, params)[!grepl("^[A-Za-z][A-Za-z0-9_]*$", c(vars, params))]
  if (length(bad_names) > 0) errors <- c(errors, paste("Invalid identifier(s):", paste(bad_names, collapse = ", ")))

  for (i in seq_along(params)) {
    prior_name <- paste0("params", i, "_prior")
    if (!exists(prior_name, envir = env, inherits = FALSE)) {
      errors <- c(errors, sprintf("%s (prior / fixed value of '%s') is missing.", prior_name, params[i]))
      next
    }
    v <- get(prior_name, envir = env)
    if (length(paramsfix) >= i) {
      if (paramsfix[i] == 1 && !is.numeric(v)) errors <- c(errors, sprintf("'%s' is fixed, so %s must be a number.", params[i], prior_name))
      if (paramsfix[i] == 0 && !is.character(v)) errors <- c(errors, sprintf("'%s' is estimated, so %s must be a prior such as \"normal(0, 1)T[0,]\".", params[i], prior_name))
      if (paramsfix[i] == 0 && is.character(v) && !grepl("\\(", v)) errors <- c(errors, sprintf("%s does not look like a Stan distribution (e.g. normal(0, 1)).", prior_name))
    }
  }

  ode_lines <- trimws(unlist(strsplit(as.character(env$ode_system), "\n")))
  ode_lines <- ode_lines[nzchar(ode_lines)]
  if (length(ode_lines) != length(vars)) errors <- c(errors, sprintf("ode_system has %d equation(s) but there are %d state variables.", length(ode_lines), length(vars)))
  for (k in seq_along(ode_lines)) {
    if (!grepl(paste0("^diff_var", k, "\\s*="), ode_lines[k])) errors <- c(errors, sprintf("Equation %d must start with 'diff_var%d ='.", k, k))
  }
  used_params <- as.integer(gsub("\\D", "", unlist(regmatches(env$ode_system, gregexpr("params[0-9]+", env$ode_system)))))
  used_vars   <- as.integer(gsub("\\D", "", unlist(regmatches(env$ode_system, gregexpr("vars[0-9]+", env$ode_system)))))
  if (length(used_params) && any(used_params > length(params))) errors <- c(errors, "ode_system refers to a parameter index larger than the number of parameters.")
  if (length(used_vars) && any(used_vars > length(vars))) errors <- c(errors, "ode_system refers to a state-variable index larger than the number of state variables.")
  tdp_used <- unique(unlist(regmatches(env$ode_system, gregexpr("time_dependent_param[0-9]+", env$ode_system))))
  tdp_missing <- setdiff(tdp_used, names(env$time_dependent_templates))
  if (length(tdp_missing)) errors <- c(errors, paste("Time-dependent parameter(s) used in the equations but not defined:", paste(tdp_missing, collapse = ", ")))

  if (length(fitting_index) == 0) errors <- c(errors, "fitting_index must contain at least one state variable index.")
  if (any(fitting_index < 1 | fitting_index > length(vars))) errors <- c(errors, "fitting_index must be between 1 and the number of state variables.")
  if (length(fitting_diff) != length(fitting_index)) errors <- c(errors, "fitting_diff must have the same length as fitting_index.")
  if (!(errstrc %in% 1:3)) errors <- c(errors, "errstrc must be 1 (negative binomial), 2 (normal) or 3 (Poisson).")
  for (k in seq_along(fitting_index)) {
    if (errstrc == 1 && !exists(paste0("negbinerror", k, "_prior"), envir = env, inherits = FALSE))
      errors <- c(errors, sprintf("negbinerror%d_prior is missing (negative-binomial error structure).", k))
    if (errstrc == 2 && !exists(paste0("normalerror", k, "_prior"), envir = env, inherits = FALSE))
      errors <- c(errors, sprintf("normalerror%d_prior is missing (normal error structure).", k))
  }

  Ic <- env$Ic
  if (length(Ic) != length(vars)) errors <- c(errors, sprintf("Ic has %d entries but there are %d state variables.", length(Ic), length(vars)))
  if (as.numeric(env$vars.init) == 1 && any(is.na(suppressWarnings(as.numeric(Ic)))))
    errors <- c(errors, "vars.init = 1 requires numeric initial conditions (use vars.init = 0 for expressions with parameters).")

  cal <- as.numeric(env$calibrationperiods); h <- as.numeric(env$forecastinghorizon)
  if (any(is.na(cal)) || any(cal < 3)) errors <- c(errors, "calibrationperiods must be whole numbers >= 3.")
  if (length(h) != 1 || is.na(h) || h < 0) errors <- c(errors, "forecastinghorizon must be a single number >= 0.")
  niter <- as.numeric(env$niter); nch <- as.numeric(env$num_chain)
  if (is.na(niter) || niter < 100) errors <- c(errors, "niter must be at least 100.")
  if (is.na(nch) || nch < 1) errors <- c(errors, "num_chain must be at least 1.")
  if (exists("nwarmup", envir = env, inherits = FALSE)) {
    nw <- as.numeric(env$nwarmup)
    if (!is.na(nw) && !is.na(niter) && nw >= niter) errors <- c(errors, "nwarmup must be smaller than niter.")
  }
  if (!is.na(niter) && niter < 1000) warnings <- c(warnings, sprintf("niter = %d is small; fine for a quick test, use more for final results.", niter))

  if (!is.null(data)) {
    n_cols <- ncol(data) - 1
    if (n_cols < length(fitting_index)) errors <- c(errors, sprintf("The configuration fits %d time series but the data has only %d (cases1..cases%d).", length(fitting_index), n_cols, n_cols))
    if (length(cal) && !any(is.na(cal)) && any(cal > nrow(data))) errors <- c(errors, sprintf("A calibration length exceeds the %d time points in the data.", nrow(data)))
    if (length(cal) && !any(is.na(cal)) && !is.na(h) && max(cal) + h > nrow(data)) warnings <- c(warnings, "The forecast horizon extends beyond the available data: forecast metrics will use only the observed points.")
  } else {
    warnings <- c(warnings, "No data loaded yet: upload the data (or load an example) before fitting.")
  }

  if (length(errors) > 0) return(list(ok = FALSE, errors = errors, warnings = warnings, summary = "", env = env))

  ## Try to generate the Stan program
  tmp_opt <- tempfile(fileext = ".R"); writeLines(txt, tmp_opt)
  stan <- tryCatch(generate_stan_file_delegated(optionsFile = tmp_opt, stan_file = tempfile(fileext = ".stan")),
                   error = function(e) e)
  if (inherits(stan, "error")) {
    return(list(ok = FALSE, errors = paste("The Stan program could not be generated:", conditionMessage(stan)),
                warnings = warnings, summary = "", env = env))
  }

  err_names <- c("negative binomial", "normal", "Poisson")
  summary <- sprintf(paste0("Model '%s': %d state variables (%s); %d parameters (%d estimated); %s error structure; ",
                            "%d time series fitted; calibration length(s): %s; forecast horizon: %d; %d chain(s) x %d iterations."),
                     env$model_name, length(vars), paste(vars, collapse = ", "), length(params), sum(paramsfix == 0),
                     err_names[errstrc], length(fitting_index), paste(cal, collapse = ", "), h, nch, niter)

  list(ok = TRUE, errors = character(0), warnings = warnings, summary = summary, env = env, stan_code = stan)
}

#------------------------------------------------------------------------------#
# Run report (Markdown, converted to HTML when pandoc is available) -------------
#------------------------------------------------------------------------------#
bff_md_table <- function(df, digits = 3) {
  if (is.null(df) || NROW(df) == 0) return("_(empty)_")
  df <- as.data.frame(df)
  for (nm in names(df)) if (is.numeric(df[[nm]])) df[[nm]] <- format(round(df[[nm]], digits), trim = TRUE)
  header <- paste0("| ", paste(names(df), collapse = " | "), " |")
  sep <- paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|")
  rows <- apply(df, 1, function(r) paste0("| ", paste(gsub("\\|", "/", as.character(r)), collapse = " | "), " |"))
  paste(c(header, sep, rows), collapse = "\n")
}

# bff_write_run_report() now lives in run_history.R (HTML + Markdown report saved with every run).
