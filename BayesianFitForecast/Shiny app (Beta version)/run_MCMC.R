#------------------------------------------------------------------------------#
# run_MCMC.R — Prepare the Stan model and data for every calibration period -----
#------------------------------------------------------------------------------#
# About: `run_MCMC()` generates the Stan program from the options file,         #
# compiles it (re-using a cached compilation when the Stan code did not change) #
# and prepares the `data` list for every calibration period. The actual         #
# sampling is launched from app.R (so that progress can be reported) with the   #
# sampler settings returned here.                                               #
#------------------------------------------------------------------------------#

  ###################
  # Needed packages #
  ###################
  library(rstan)

  ###########################
  # Needed source functions #
  ###########################
  source("diff.R")
  source("ode_rhs.R")
  source("generate_stan_delegate.R")
  if (!exists("bff_load_options", mode = "function")) source("run_MCMC2.R")

  # Re-use compiled models across runs of the same R session
  rstan::rstan_options(auto_write = TRUE)

  # In-memory cache of compiled models, keyed by the md5 hash of the Stan code
  .bff_model_cache <- new.env(parent = emptyenv())

#------------------------------------------------------------------------------#
# Compile (or fetch from cache) a Stan program ---------------------------------
#------------------------------------------------------------------------------#
bff_compile_stan <- function(stan_code, stan_file) {
  key <- if (requireNamespace("digest", quietly = TRUE)) {
    digest::digest(stan_code, algo = "md5")
  } else {
    paste0("len", nchar(stan_code), "_", sum(utf8ToInt(stan_code)))
  }
  cached <- .bff_model_cache[[key]]
  if (!is.null(cached)) {
    return(list(model = cached, from_cache = TRUE))
  }
  if (!file.exists(stan_file)) writeLines(stan_code, stan_file)
  model <- rstan::stan_model(stan_file, auto_write = TRUE)
  assign(key, model, envir = .bff_model_cache)
  list(model = model, from_cache = FALSE)
}

#------------------------------------------------------------------------------#
# Persistent cache for compiled Stan programs ----------------------------------
#------------------------------------------------------------------------------#
# About: The Stan program is written to <app>/stan_cache/<md5>.stan; with        #
# rstan_options(auto_write = TRUE) the compiled model is saved next to it and   #
# re-used in later sessions (and by the background sampling processes).        #
#------------------------------------------------------------------------------#
bff_stan_cache_dir <- function() {
  app_dir <- getOption("bff.app_dir", getwd())   # set in app.R; relative to the installed app folder
  dir <- file.path(app_dir, "stan_cache")
  ok <- tryCatch({ dir.create(dir, showWarnings = FALSE); file.access(dir, 2) == 0 }, error = function(e) FALSE)
  if (!isTRUE(ok)) {
    dir <- file.path(tempdir(), "stan_cache")
    dir.create(dir, showWarnings = FALSE)
  }
  normalizePath(dir, winslash = "/", mustWork = FALSE)
}

bff_stan_code_hash <- function(stan_code) {
  if (requireNamespace("digest", quietly = TRUE)) digest::digest(stan_code, algo = "md5") else
    paste0("len", nchar(stan_code), "_", sum(utf8ToInt(stan_code)))
}

#------------------------------------------------------------------------------#
# Prepare a run (no compilation): Stan program + data per calibration period --
#------------------------------------------------------------------------------#
bff_prepare_run <- function(outputFile, data.temp) {

  opt <- bff_load_options(outputFile)

  calibrationperiods <- as.numeric(opt$calibrationperiods)
  forecastinghorizon <- as.numeric(opt$forecastinghorizon)
  fitting_index      <- opt$fitting_index
  params             <- opt$params
  Ic                 <- opt$Ic
  vars.init          <- opt$vars.init
  niter              <- as.numeric(opt$niter)
  num_chain          <- as.numeric(opt$num_chain)

  get_opt <- function(name, default) {
    if (exists(name, envir = opt, inherits = FALSE)) {
      val <- get(name, envir = opt)
      if (is.null(val) || length(val) == 0 || (is.atomic(val) && all(is.na(val)))) default else val
    } else default
  }
  nwarmup       <- as.numeric(get_opt("nwarmup", floor(niter / 2)))
  seed          <- as.numeric(get_opt("seed", 0))
  adapt_delta   <- as.numeric(get_opt("adapt_delta", 0.8))
  max_treedepth <- as.numeric(get_opt("max_treedepth", 10))
  n_cores       <- as.numeric(get_opt("n_cores", 1))
  n_cores       <- max(1, min(n_cores, parallel::detectCores()))

  # Stan program, stored under its md5 so that compiled models are re-used
  stan_tmp <- file.path(tempdir(), "ode_model.stan")
  stan_code <- generate_stan_file_delegated(optionsFile = outputFile, stan_file = stan_tmp)
  cache_dir <- bff_stan_cache_dir()
  stan_file <- file.path(cache_dir, paste0("model_", bff_stan_code_hash(stan_code), ".stan"))
  if (!file.exists(stan_file)) writeLines(stan_code, stan_file)

  Mydata <- data.temp
  actualcases <- lapply(seq_along(fitting_index), function(i) Mydata[[paste0("cases", i)]])

  store.outputs <- vector("list", length(calibrationperiods))
  names(store.outputs) <- as.character(calibrationperiods)

  for (h in seq_along(calibrationperiods)) {
    calibrationperiod <- calibrationperiods[h]
    cases <- lapply(actualcases, function(x) as.integer(round(x[seq_len(calibrationperiod)])))
    n_days    <- length(cases[[1]])
    nfst_days <- forecastinghorizon
    data_ode <- list(n_days = n_days, nfst_days = nfst_days, t0 = 0, ts = seq(1, n_days + nfst_days, by = 1))
    if (vars.init == 1) {
      data_ode$y0 <- as.numeric(if (length(Ic) == 1) array(Ic[1], dim = 1) else Ic)
    }
    for (i in seq_along(fitting_index)) data_ode[[paste0("cases", i)]] <- cases[[i]]
    for (i in seq_along(params)) {
      prior_name <- paste0("params", i, "_prior")
      if (exists(prior_name, envir = opt, inherits = FALSE)) {
        prior_value <- get(prior_name, envir = opt)
        if (is.numeric(prior_value)) data_ode[[params[i]]] <- prior_value
      }
    }
    store.outputs[[h]] <- list(
      data    = data_ode,
      iter    = niter,
      warmup  = nwarmup,
      chains  = num_chain,
      cores   = n_cores,
      seed    = seed,
      control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
      calibrationperiod = calibrationperiod
    )
  }

  list(jobs = store.outputs, stan_code = stan_code, stan_file = stan_file, n_cores = n_cores)
}

#------------------------------------------------------------------------------#
# Main function (synchronous path: prepare + compile) --------------------------
#------------------------------------------------------------------------------#
run_MCMC <- function(outputFile, data.temp) {

  prep <- bff_prepare_run(outputFile, data.temp)

  compiled <- tryCatch(bff_compile_stan(prep$stan_code, prep$stan_file), error = function(e) e$message)
  if (is.character(compiled)) {
    return(compiled)  # the app shows the Stan compilation error
  }

  out <- lapply(prep$jobs, function(j) {
    c(j, list(model = compiled$model, stan_code = prep$stan_code, from_cache = compiled$from_cache))
  })
  names(out) <- names(prep$jobs)
  out
}
