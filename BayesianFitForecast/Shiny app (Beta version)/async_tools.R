#------------------------------------------------------------------------------#
# async_tools.R — Background sampling with real progress -----------------------
#------------------------------------------------------------------------------#
# About: The Stan program is compiled and every MCMC chain is sampled in its   #
# own background R process (package `callr`). Stan's console output of each    #
# chain ("Iteration: 150 / 300 [ 50%]") is written to a file that the Shiny    #
# session polls every second, so the progress shown in the dashboard is the    #
# real progress of the chains, the interface stays responsive and a run can be #
# cancelled. The chains of a calibration period are merged afterwards with     #
# rstan::sflist2stanfit(), which is equivalent to running them together.       #
#                                                                              #
# The functions passed to the background processes (bff_job_*) must be         #
# self-contained: they receive everything through their arguments.            #
#------------------------------------------------------------------------------#

`%||%` <- function(x, y) if (is.null(x)) y else x

#------------------------------------------------------------------------------#
# Jobs executed in background processes ------------------------------------------
#------------------------------------------------------------------------------#

# Compile the Stan program (or load it from the auto_write cache) and save the
# compiled model object so that the sampling processes can load it.
bff_job_compile <- function(stan_file, model_rds) {
  suppressPackageStartupMessages(library(rstan))
  rstan::rstan_options(auto_write = TRUE)
  cat("Compiling / loading the Stan program ...\n")
  model <- rstan::stan_model(stan_file, auto_write = TRUE)
  saveRDS(model, model_rds)
  cat("Model ready.\n")
  TRUE
}

# Sample ONE chain and save the stanfit object.
bff_job_chain <- function(model_rds, data, iter, warmup, chain_id, seed, control, refresh, out_rds) {
  suppressPackageStartupMessages(library(rstan))
  model <- readRDS(model_rds)
  fit <- rstan::sampling(model,
                         data     = data,
                         iter     = iter,
                         warmup   = warmup,
                         chains   = 1,
                         chain_id = chain_id,
                         seed     = seed,
                         control  = control,
                         refresh  = refresh,
                         cores    = 1)
  saveRDS(fit, out_rds)
  cat("CHAIN_DONE\n")
  TRUE
}

#------------------------------------------------------------------------------#
# Progress parsing ---------------------------------------------------------------
#------------------------------------------------------------------------------#

# Read the last bytes of a (growing) text file as lines
bff_read_tail <- function(path, n_bytes = 6000) {
  sz <- suppressWarnings(file.size(path))
  if (is.na(sz) || sz == 0) return(character(0))
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  if (sz > n_bytes) seek(con, sz - n_bytes)
  raw <- readBin(con, "raw", n = min(sz, n_bytes))
  txt <- iconv(rawToChar(raw[raw != as.raw(0)]), from = "UTF-8", to = "UTF-8", sub = "")
  strsplit(txt, "\n", fixed = TRUE)[[1]]
}

# Read the console output of a chain and return the iterations completed so far.
# Note: the C library buffers the output of the background process in blocks of
# a few KB, so with refresh = 1 (one line per iteration) the progress advances
# every ~90 iterations.
bff_parse_chain_progress <- function(stdout_file, iter) {
  if (!file.exists(stdout_file)) return(list(done = 0L, phase = "starting", finished = FALSE))
  lines <- tryCatch(bff_read_tail(stdout_file), error = function(e) character(0))
  if (length(lines) == 0) return(list(done = 0L, phase = "starting", finished = FALSE))
  finished <- any(grepl("CHAIN_DONE", lines, fixed = TRUE))
  it_lines <- grep("Iteration:", lines, value = TRUE)
  if (length(it_lines) == 0) {
    return(list(done = if (finished) iter else 0L, phase = if (finished) "done" else "starting", finished = finished))
  }
  last <- it_lines[length(it_lines)]
  m <- regmatches(last, regexec("Iteration:\\s*([0-9]+)\\s*/\\s*([0-9]+)", last))[[1]]
  done <- if (length(m) >= 2) as.integer(m[2]) else 0L
  phase <- if (grepl("Warmup", last)) "warm-up" else if (grepl("Sampling", last)) "sampling" else "running"
  if (finished) { done <- iter; phase <- "done" }
  list(done = done, phase = phase, finished = finished)
}

# Lines of a chain's output worth keeping as diagnostics
bff_filter_chain_messages <- function(files) {
  out <- character(0)
  for (f in files) {
    if (!file.exists(f)) next
    lines <- tryCatch(readLines(f, warn = FALSE), error = function(e) character(0))
    keep <- grepl("Exception|[Ee]rror|[Rr]ejecting|divergent|treedepth|Effective Samples|R-hat|Rhat|BFMI|WARNING|Warning|mc-stan.org",
                  lines) & !grepl("^Chain [0-9]+: *$", lines)
    out <- c(out, trimws(lines[keep]))
  }
  unique(out[nzchar(out)])
}

#------------------------------------------------------------------------------#
# Run manager (plain R environment; the Shiny session polls it) ---------------
#------------------------------------------------------------------------------#
# A run holds: the compile job, the queue of chain jobs (one per calibration   #
# period and chain), the running processes, and the results.                  #
#------------------------------------------------------------------------------#

bff_run_new <- function(prepared, stan_file, run_dir, n_cores, libpath = .libPaths()) {
  dir.create(run_dir, showWarnings = FALSE, recursive = TRUE)
  jobs <- list()
  for (h in seq_along(prepared)) {
    p <- prepared[[h]]
    for (k in seq_len(p$chains)) {
      jobs[[length(jobs) + 1]] <- list(
        id = sprintf("p%d_c%d", h, k),
        period_index = h,
        period = p$calibrationperiod,
        chain = k,
        iter = p$iter,
        warmup = p$warmup,
        seed = p$seed,
        control = p$control,
        data = p$data,
        status = "queued",     # queued / running / done / failed
        done = 0L,
        phase = "queued",
        stdout = file.path(run_dir, sprintf("chain_%s.out", sprintf("p%d_c%d", h, k))),
        stderr = file.path(run_dir, sprintf("chain_%s.err", sprintf("p%d_c%d", h, k))),
        out_rds = file.path(run_dir, sprintf("fit_%s.rds", sprintf("p%d_c%d", h, k))),
        proc = NULL
      )
    }
  }
  env <- new.env(parent = emptyenv())
  env$stan_file  <- stan_file
  env$model_rds  <- file.path(run_dir, "model.rds")
  env$run_dir    <- run_dir
  env$n_cores    <- max(1L, as.integer(n_cores))
  env$libpath    <- libpath
  env$jobs       <- jobs
  env$phase      <- "compiling"   # compiling / sampling / finished / failed / cancelled
  env$compile_proc <- NULL
  env$compile_out  <- file.path(run_dir, "compile.out")
  env$compile_err  <- file.path(run_dir, "compile.err")
  env$started    <- Sys.time()
  env$error      <- NULL
  env$prepared   <- prepared
  env
}

bff_run_start <- function(run) {
  run$compile_proc <- callr::r_bg(
    bff_job_compile,
    args = list(stan_file = run$stan_file, model_rds = run$model_rds),
    stdout = run$compile_out, stderr = run$compile_err,
    libpath = run$libpath, supervise = TRUE, package = FALSE
  )
  run$phase <- "compiling"
  invisible(run)
}

bff_run_launch_chain <- function(run, j) {
  job <- run$jobs[[j]]
  job$proc <- callr::r_bg(
    bff_job_chain,
    args = list(model_rds = run$model_rds, data = job$data, iter = job$iter, warmup = job$warmup,
                chain_id = job$chain, seed = job$seed, control = job$control,
                refresh = 1L, out_rds = job$out_rds),
    stdout = job$stdout, stderr = job$stderr,
    libpath = run$libpath, supervise = TRUE, package = FALSE
  )
  job$status <- "running"
  job$phase <- "starting"
  run$jobs[[j]] <- job
  invisible(run)
}

# Advance the run: launch queued chains when slots are free, update progress,
# detect failures. Returns the run (modified in place).
bff_run_poll <- function(run) {

  if (run$phase %in% c("finished", "failed", "cancelled")) return(run)

  ## ---- compilation ----
  if (run$phase == "compiling") {
    proc <- run$compile_proc
    if (is.null(proc)) { run$phase <- "failed"; run$error <- "Compilation process was not started."; return(run) }
    if (proc$is_alive()) return(run)
    status <- tryCatch(proc$get_exit_status(), error = function(e) 1L)
    if (!identical(as.integer(status), 0L) || !file.exists(run$model_rds)) {
      err <- tryCatch(readLines(run$compile_err, warn = FALSE), error = function(e) character(0))
      out <- tryCatch(readLines(run$compile_out, warn = FALSE), error = function(e) character(0))
      run$phase <- "failed"
      run$error <- paste(c("The Stan program could not be compiled.", tail(c(out, err), 25)), collapse = "\n")
      return(run)
    }
    run$phase <- "sampling"
  }

  ## ---- sampling ----
  for (j in seq_along(run$jobs)) {
    job <- run$jobs[[j]]
    if (job$status != "running") next
    prog <- bff_parse_chain_progress(job$stdout, job$iter)
    job$done <- prog$done
    job$phase <- prog$phase
    alive <- tryCatch(job$proc$is_alive(), error = function(e) FALSE)
    if (!alive) {
      status <- tryCatch(job$proc$get_exit_status(), error = function(e) 1L)
      if (identical(as.integer(status), 0L) && file.exists(job$out_rds)) {
        job$status <- "done"; job$done <- job$iter; job$phase <- "done"
      } else {
        job$status <- "failed"; job$phase <- "failed"
        err <- tryCatch(readLines(job$stderr, warn = FALSE), error = function(e) character(0))
        out <- tryCatch(readLines(job$stdout, warn = FALSE), error = function(e) character(0))
        run$phase <- "failed"
        run$error <- paste(c(sprintf("Chain %d of calibration period %s failed.", job$chain, job$period),
                             tail(c(out[grepl("Exception|[Ee]rror|[Rr]ejecting", out)], err), 20)), collapse = "\n")
      }
    }
    run$jobs[[j]] <- job
  }
  if (run$phase == "failed") { bff_run_kill(run); return(run) }

  # Launch queued jobs while slots are free
  n_running <- sum(vapply(run$jobs, function(x) x$status == "running", logical(1)))
  for (j in seq_along(run$jobs)) {
    if (n_running >= run$n_cores) break
    if (run$jobs[[j]]$status == "queued") {
      run <- bff_run_launch_chain(run, j)
      n_running <- n_running + 1
    }
  }

  if (all(vapply(run$jobs, function(x) x$status == "done", logical(1)))) {
    run$phase <- "finished"
    run$finished <- Sys.time()
  }
  run
}

bff_run_kill <- function(run) {
  tryCatch({ if (!is.null(run$compile_proc) && run$compile_proc$is_alive()) run$compile_proc$kill() }, error = function(e) NULL)
  for (j in seq_along(run$jobs)) {
    p <- run$jobs[[j]]$proc
    tryCatch({ if (!is.null(p) && p$is_alive()) p$kill() }, error = function(e) NULL)
    if (run$jobs[[j]]$status == "running") run$jobs[[j]]$status <- "failed"
  }
  invisible(run)
}

# Overall progress summary used by the status box
bff_run_summary <- function(run) {
  jobs <- run$jobs
  total_iter <- sum(vapply(jobs, function(x) x$iter, numeric(1)))
  done_iter  <- sum(vapply(jobs, function(x) if (x$status == "done") x$iter else x$done, numeric(1)))
  frac <- if (total_iter > 0) done_iter / total_iter else 0
  elapsed <- as.numeric(difftime(Sys.time(), run$started, units = "secs"))
  eta <- NA_real_
  if (run$phase == "sampling" && frac > 0.02) {
    sampling_elapsed <- as.numeric(difftime(Sys.time(), run$sampling_started %||% run$started, units = "secs"))
    eta <- sampling_elapsed * (1 - frac) / frac
  }
  list(phase = run$phase, fraction = frac, done_iter = done_iter, total_iter = total_iter,
       elapsed = elapsed, eta = eta,
       chains = lapply(jobs, function(x) list(id = x$id, period = x$period, chain = x$chain, status = x$status,
                                              phase = x$phase, done = if (x$status == "done") x$iter else x$done, iter = x$iter,
                                              fraction = if (x$status == "done") 1 else x$done / x$iter)))
}

# Collect the fitted chains into one stanfit per calibration period
bff_run_collect <- function(run) {
  periods <- unique(vapply(run$jobs, function(x) x$period_index, numeric(1)))
  fits <- vector("list", length(periods))
  warnings_by_period <- list()
  for (h in periods) {
    jobs_h <- Filter(function(x) x$period_index == h, run$jobs)
    sflist <- lapply(jobs_h, function(x) readRDS(x$out_rds))
    fits[[h]] <- if (length(sflist) == 1) sflist[[1]] else rstan::sflist2stanfit(sflist)
    key <- as.character(jobs_h[[1]]$period)
    warnings_by_period[[key]] <- bff_filter_chain_messages(c(vapply(jobs_h, function(x) x$stdout, character(1)),
                                                             vapply(jobs_h, function(x) x$stderr, character(1))))
  }
  list(fits = fits, warnings = warnings_by_period)
}
