#------------------------------------------------------------------------------#
# run_history.R — Completed runs saved on disk + run reports -------------------
#------------------------------------------------------------------------------#
# About: Every completed run is stored in <app>/runs/<timestamp>_<model>/:        #
#   options.R            configuration (reproduces the run here or in the toolbox)#
#   ode_model.stan       generated Stan program                                  #
#   data.csv             data used for fitting                                   #
#   figures/             all figures of the results area (PNG)                   #
#   tables/              all tables of the results area (CSV)                    #
# The fitted Stan objects (needed to show the results again without refitting)  #
# are kept out of sight in <app>/stan_cache/fits/<id>.rds, next to the compiled #
# models. The HTML report is produced on demand ('Download report').            #
#------------------------------------------------------------------------------#

`%||%` <- get0("%||%", mode = "function", ifnotfound = function(x, y) if (is.null(x)) y else x)

bff_writable_dir <- function(dir, fallback) {
  ok <- tryCatch({ dir.create(dir, showWarnings = FALSE, recursive = TRUE); file.access(dir, 2) == 0 }, error = function(e) FALSE)
  if (!isTRUE(ok)) {
    dir <- fallback
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  }
  normalizePath(dir, winslash = "/", mustWork = FALSE)
}

# User-facing folder of the saved runs (next to app.R, wherever the app is installed)
bff_runs_dir <- function() {
  app_dir <- getOption("bff.app_dir", getwd())   # set in app.R
  bff_writable_dir(file.path(app_dir, "runs"), file.path(tempdir(), "bff_runs"))
}

# Internal cache of the fitted objects of the saved runs
bff_fit_cache_dir <- function() {
  app_dir <- getOption("bff.app_dir", getwd())
  bff_writable_dir(file.path(app_dir, "stan_cache", "fits"), file.path(tempdir(), "bff_fits"))
}

bff_run_new_id <- function(model_name = "model") {
  nm <- gsub("[^A-Za-z0-9_.-]+", "-", as.character(model_name %||% "model"))
  if (!nzchar(nm)) nm <- "model"
  sprintf("%s_%s", format(Sys.time(), "%Y%m%d-%H%M%S"), nm)
}

bff_dir_size_mb <- function(dir) {
  f <- list.files(dir, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  if (length(f) == 0) return(0)
  round(sum(file.info(f)$size, na.rm = TRUE) / 1024^2, 1)
}

# Evaluate an options.R text into an environment (no side effects)
bff_load_options_text <- function(txt) {
  if (is.null(txt) || !nzchar(paste(txt, collapse = ""))) return(NULL)
  f <- tempfile(fileext = ".R")
  writeLines(txt, f)
  on.exit(unlink(f), add = TRUE)
  bff_load_options(f)
}

#------------------------------------------------------------------------------#
# Save a completed run -----------------------------------------------------------
#------------------------------------------------------------------------------#
# results    output of run_analyzeResults() (used only for the status / summary)
# run_meta   list(started, finished, options, stan_code, warnings, fits)
# data       data frame used for fitting; data_name its file name
#------------------------------------------------------------------------------#
bff_save_run <- function(results, run_meta, data, data_name = NULL,
                         runs_dir = bff_runs_dir(), cache_dir = bff_fit_cache_dir()) {
  opt <- tryCatch(bff_load_options_text(run_meta$options), error = function(e) NULL)
  model_name <- if (!is.null(opt) && exists("model_name", envir = opt, inherits = FALSE)) opt$model_name else "model"
  id <- bff_run_new_id(model_name)
  root <- file.path(runs_dir, id)
  dir.create(root, showWarnings = FALSE, recursive = TRUE)

  # Configuration, Stan program and data (figures/ and tables/ are written by the app right after)
  writeLines(run_meta$options %||% "", file.path(root, "options.R"))
  if (!is.null(run_meta$stan_code)) writeLines(run_meta$stan_code, file.path(root, "ode_model.stan"))
  if (!is.null(data)) utils::write.csv(data, file.path(root, "data.csv"), row.names = FALSE)

  # Internal state: fitted objects + what the listing shows
  conv <- tryCatch(do.call(rbind, lapply(results, function(r) r$convergence.summary)), error = function(e) NULL)
  status <- if (!is.null(conv) && "Status" %in% names(conv)) conv$Status else "Unknown"
  worst <- if ("Poor" %in% status) "Poor" else if ("Caution" %in% status) "Caution" else if (length(status) && all(status == "Good")) "Good" else "Unknown"
  state <- list(
    id = id,
    fits = run_meta$fits,
    warnings = run_meta$warnings %||% list(),
    started = run_meta$started,
    finished = run_meta$finished %||% Sys.time(),
    status = worst,
    data_name = data_name,
    model_name = model_name
  )
  tryCatch({
    saveRDS(state, file.path(cache_dir, paste0(id, ".rds")))
    info <- state; info$fits <- NULL
    saveRDS(info, file.path(cache_dir, paste0(id, ".info.rds")))   # small sidecar read by the listing
  }, error = function(e) message("The fitted objects could not be cached: ", conditionMessage(e)))

  list(id = id, dir = root)
}

#------------------------------------------------------------------------------#
# List / load / delete / zip -------------------------------------------------------
#------------------------------------------------------------------------------#
# Reads only the cheap part of the cache (no stanfit objects) for the listing
bff_run_cache_info <- function(id, cache_dir = bff_fit_cache_dir()) {
  f <- file.path(cache_dir, paste0(id, ".rds"))
  if (!file.exists(f)) return(NULL)
  f_info <- file.path(cache_dir, paste0(id, ".info.rds"))
  st <- if (file.exists(f_info)) tryCatch(readRDS(f_info), error = function(e) NULL) else NULL
  if (is.null(st)) st <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(st)) return(NULL)
  st$fits <- NULL
  st$has_fits <- TRUE
  st$cache_mb <- round(file.info(f)$size / 1024^2, 1)
  st
}

bff_list_runs <- function(runs_dir = bff_runs_dir(), cache_dir = bff_fit_cache_dir()) {
  dirs <- list.dirs(runs_dir, recursive = FALSE, full.names = TRUE)
  rows <- lapply(dirs, function(d) {
    f <- file.path(d, "options.R")
    if (!file.exists(f)) return(NULL)
    id <- basename(d)
    opt <- tryCatch(bff_load_options(f), error = function(e) NULL)
    get_opt <- function(name, default = NA) if (!is.null(opt) && exists(name, envir = opt, inherits = FALSE)) get(name, envir = opt) else default
    info <- bff_run_cache_info(id, cache_dir)
    data_file <- file.path(d, "data.csv")
    n_points <- NA_real_; n_series <- NA_real_
    if (file.exists(data_file)) {
      dd <- tryCatch(utils::read.csv(data_file, check.names = FALSE), error = function(e) NULL)
      if (!is.null(dd)) { n_points <- nrow(dd); n_series <- ncol(dd) - 1 }
    }
    finished <- if (!is.null(info$finished)) format(info$finished, "%Y-%m-%d %H:%M:%S") else {
      m <- regmatches(id, regexpr("^[0-9]{8}-[0-9]{6}", id))
      if (length(m) == 1) format(as.POSIXct(m, format = "%Y%m%d-%H%M%S"), "%Y-%m-%d %H:%M:%S") else format(file.info(d)$mtime, "%Y-%m-%d %H:%M:%S")
    }
    pf <- get_opt("paramsfix", NA)
    mt <- get_opt("model_template", NA)
    data.frame(
      id = id,
      dir = d,
      model_name = as.character(get_opt("model_name", "model")),
      model_template = if (length(mt) == 1 && !is.na(mt)) as.character(mt) else NA_character_,
      finished = finished,
      elapsed_min = if (!is.null(info$started) && !is.null(info$finished)) round(as.numeric(difftime(info$finished, info$started, units = "mins")), 1) else NA_real_,
      calibration = paste(get_opt("calibrationperiods", ""), collapse = ", "),
      horizon = as.numeric(get_opt("forecastinghorizon", NA)),
      niter = as.numeric(get_opt("niter", NA)),
      num_chain = as.numeric(get_opt("num_chain", NA)),
      errstrc = as.numeric(get_opt("errstrc", NA)),
      vars = paste(as.character(get_opt("vars", character(0))), collapse = ", "),
      n_params = length(get_opt("params", character(0))),
      n_estimated = if (all(is.na(pf))) NA_real_ else sum(as.numeric(pf) == 0),
      data_name = if (is.null(info$data_name)) NA_character_ else as.character(info$data_name),
      n_points = n_points,
      n_series = n_series,
      status = info$status %||% "Unknown",
      has_fits = isTRUE(info$has_fits),
      size_mb = bff_dir_size_mb(d) + (info$cache_mb %||% 0),
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame(id = character(0), dir = character(0), model_name = character(0), finished = character(0), stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, rows)
  out[order(out$finished, decreasing = TRUE), , drop = FALSE]
}

#------------------------------------------------------------------------------#
# Migration of run folders written by an earlier version that kept the fitted    #
# objects (stanfit_objects.rds) and other leftovers (results.rds, summary.json,   #
# stan_messages.txt, README.txt, report.*) inside the run folder: the fitted      #
# objects move to the cache and the leftovers are removed. figures/ and tables/   #
# are left untouched.                                                              #
#------------------------------------------------------------------------------#
bff_migrate_old_runs <- function(runs_dir = bff_runs_dir(), cache_dir = bff_fit_cache_dir()) {
  dirs <- list.dirs(runs_dir, recursive = FALSE, full.names = TRUE)
  migrated <- character(0)
  for (d in dirs) {
    extra_files <- c("results.rds", "stanfit_objects.rds", "summary.json", "README.txt", "report.html", "report.md", "stan_messages.txt")
    present <- file.path(d, extra_files)[file.exists(file.path(d, extra_files))]
    if (length(present) == 0) next
    id <- basename(d)
    fits_file <- file.path(d, "stanfit_objects.rds")
    cache_file <- file.path(cache_dir, paste0(id, ".rds"))
    ok <- TRUE
    if (file.exists(fits_file) && !file.exists(cache_file)) {
      ok <- tryCatch({
        fits <- readRDS(fits_file)
        summ <- if (file.exists(file.path(d, "summary.json"))) tryCatch(jsonlite::fromJSON(file.path(d, "summary.json"), simplifyVector = TRUE), error = function(e) list()) else list()
        warnings <- list()
        if (file.exists(file.path(d, "stan_messages.txt"))) {
          key <- NULL
          for (ln in readLines(file.path(d, "stan_messages.txt"), warn = FALSE)) {
            if (grepl("^== Calibration period ", ln)) { key <- sub("^== Calibration period (.*) ==$", "\\1", ln); warnings[[key]] <- character(0); next }
            if (!is.null(key) && nzchar(ln)) warnings[[key]] <- c(warnings[[key]], ln)
          }
        }
        as_time <- function(x) if (is.null(x)) NULL else tryCatch(as.POSIXct(x), error = function(e) NULL)
        state <- list(id = id, fits = fits, warnings = warnings,
                      started = as_time(summ$started), finished = as_time(summ$finished) %||% file.info(fits_file)$mtime,
                      status = summ$status %||% "Unknown", data_name = summ$data_name, model_name = summ$model_name %||% "model")
        saveRDS(state, cache_file)
        info <- state; info$fits <- NULL
        saveRDS(info, file.path(cache_dir, paste0(id, ".info.rds")))
        TRUE
      }, error = function(e) { message("Run ", id, " could not be migrated: ", conditionMessage(e)); FALSE })
    }
    if (!isTRUE(ok)) next
    unlink(present, force = TRUE)
    migrated <- c(migrated, id)
  }
  invisible(migrated)
}

bff_run_dir <- function(id, runs_dir = bff_runs_dir()) {
  id <- basename(as.character(id))      # no path components
  d <- file.path(runs_dir, id)
  if (!dir.exists(d)) stop(sprintf("Run '%s' was not found in %s.", id, runs_dir))
  d
}

# Everything needed to show a saved run again: configuration, Stan program, data + the cached fits
bff_load_run <- function(id, runs_dir = bff_runs_dir(), cache_dir = bff_fit_cache_dir()) {
  d <- bff_run_dir(id, runs_dir)
  options <- if (file.exists(file.path(d, "options.R"))) paste(readLines(file.path(d, "options.R"), warn = FALSE), collapse = "\n") else NULL
  stan_code <- if (file.exists(file.path(d, "ode_model.stan"))) paste(readLines(file.path(d, "ode_model.stan"), warn = FALSE), collapse = "\n") else NULL
  data <- if (file.exists(file.path(d, "data.csv"))) utils::read.csv(file.path(d, "data.csv"), check.names = FALSE, stringsAsFactors = FALSE) else NULL
  f <- file.path(cache_dir, paste0(basename(d), ".rds"))
  state <- if (file.exists(f)) tryCatch(readRDS(f), error = function(e) NULL) else NULL
  list(id = basename(d), dir = d, options = options, stan_code = stan_code, data = data,
       fits = state$fits, warnings = state$warnings %||% list(),
       started = state$started, finished = state$finished, status = state$status,
       data_name = state$data_name)
}

bff_delete_run <- function(id, runs_dir = bff_runs_dir(), cache_dir = bff_fit_cache_dir()) {
  d <- bff_run_dir(id, runs_dir)
  unlink(d, recursive = TRUE, force = TRUE)
  unlink(file.path(cache_dir, paste0(basename(d), c(".rds", ".info.rds"))), force = TRUE)
  !dir.exists(d)
}

bff_zip_run <- function(id, zipfile, runs_dir = bff_runs_dir()) {
  d <- bff_run_dir(id, runs_dir)
  zip::zip(zipfile = zipfile, files = list.files(d, recursive = TRUE), root = d)
  invisible(zipfile)
}

#------------------------------------------------------------------------------#
# Run report (HTML built with htmltools, plus Markdown) ---------------------------
#------------------------------------------------------------------------------#
bff_fmt_num <- function(x, digits = 3) {
  if (is.numeric(x)) {
    out <- ifelse(is.na(x), "", ifelse(abs(x - round(x)) < 1e-9 & abs(x) < 1e9, format(round(x), big.mark = ",", trim = TRUE),
                                      format(round(x, digits), trim = TRUE, scientific = FALSE)))
    return(out)
  }
  as.character(x)
}

bff_html_table <- function(df, digits = 3, math_cols = character(0), class = "bff-table") {
  if (is.null(df) || NROW(df) == 0) return(htmltools::tags$p(htmltools::tags$em("(empty)")))
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  htmltools::tags$table(
    class = class,
    htmltools::tags$thead(htmltools::tags$tr(lapply(names(df), htmltools::tags$th))),
    htmltools::tags$tbody(lapply(seq_len(nrow(df)), function(i) {
      htmltools::tags$tr(lapply(names(df), function(nm) {
        v <- df[[nm]][i]
        if (nm %in% math_cols) htmltools::tags$td(htmltools::HTML(paste0("\\(", v, "\\)"))) else htmltools::tags$td(bff_fmt_num(v, digits))
      }))
    }))
  )
}

bff_report_css <- "
  body { font-family: 'Segoe UI', Helvetica, Arial, sans-serif; max-width: 1100px; margin: 24px auto; padding: 0 20px; color: #222; line-height: 1.45; }
  h1 { border-bottom: 2px solid #3c8dbc; padding-bottom: 6px; }
  h2 { color: #3c8dbc; margin-top: 32px; border-bottom: 1px solid #ddd; padding-bottom: 4px; }
  h3 { margin-top: 22px; }
  table.bff-table { border-collapse: collapse; margin: 10px 0 16px 0; font-size: 13px; }
  table.bff-table th, table.bff-table td { border: 1px solid #ddd; padding: 4px 8px; text-align: left; vertical-align: top; }
  table.bff-table th { background: #f0f4f8; }
  .bff-meta { display: grid; grid-template-columns: repeat(auto-fill, minmax(220px, 1fr)); gap: 8px 20px; background: #f7f9fb; border: 1px solid #e3e8ee; padding: 12px 16px; border-radius: 4px; }
  .bff-meta div span { display: block; font-size: 11px; color: #666; text-transform: uppercase; }
  .bff-status { color: white; padding: 3px 10px; border-radius: 4px; font-weight: bold; display: inline-block; }
  .bff-status-Good { background: #00a65a; } .bff-status-Caution { background: #f39c12; } .bff-status-Poor { background: #dd4b39; } .bff-status-Unknown { background: #777; }
  .bff-eq { font-size: 16px; overflow-x: auto; margin: 10px 0; }
  figure { margin: 14px 0; } figure img { max-width: 100%; border: 1px solid #eee; } figcaption { font-size: 12px; color: #666; }
  pre { background: #f7f7f7; border: 1px solid #ddd; padding: 10px; font-size: 12px; overflow-x: auto; }
  .bff-note { color: #666; font-size: 13px; }
  .bff-figs { display: grid; grid-template-columns: repeat(auto-fill, minmax(480px, 1fr)); gap: 10px; }
"

bff_write_run_report <- function(root, results, run_meta, data, opt = NULL, summary = list(),
                                 embed_images = TRUE, write_md = FALSE, file = file.path(root, "report.html")) {
  if (is.null(opt)) opt <- tryCatch(bff_load_options_text(run_meta$options), error = function(e) NULL)
  info <- if (!is.null(opt)) tryCatch(bff_options_model_info(opt), error = function(e) NULL) else NULL
  get_opt <- function(name, default = NA) if (!is.null(opt) && exists(name, envir = opt, inherits = FALSE)) get(name, envir = opt) else default
  model_name <- as.character(get_opt("model_name", "model"))
  generated <- format(Sys.time(), "%Y-%m-%d %H:%M")
  elapsed <- if (!is.null(run_meta$started) && !is.null(run_meta$finished)) sprintf("%.1f min", as.numeric(difftime(run_meta$finished, run_meta$started, units = "mins"))) else "n/a"

  conv  <- tryCatch(do.call(rbind, lapply(results, function(r) r$convergence.summary)), error = function(e) NULL)
  pars  <- tryCatch(do.call(rbind, lapply(results, function(r) r$add.parameter.values)), error = function(e) NULL)
  perf  <- tryCatch(do.call(rbind, lapply(results, function(r) r$performance.metrics)), error = function(e) NULL)
  fitm  <- tryCatch(do.call(rbind, lapply(results, function(r) r$model.fit.metrics)), error = function(e) NULL)
  status <- summary$status %||% (if (!is.null(conv) && "Status" %in% names(conv)) { st <- conv$Status; if ("Poor" %in% st) "Poor" else if ("Caution" %in% st) "Caution" else "Good" } else "Unknown")

  figs <- sort(list.files(file.path(root, "figures"), pattern = "\\.png$"))
  # self-contained report: the PNG figures are embedded as data URIs
  fig_src <- function(f) {
    if (!embed_images) return(file.path("figures", f))
    path <- file.path(root, "figures", f)
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    paste0("data:image/png;base64,", jsonlite::base64_enc(bytes))
  }
  fig_title <- function(f) {
    nm <- tools::file_path_sans_ext(f)
    nm <- sub("-cal-([0-9.]+)$", " (calibration \\1)", nm)
    gsub("[-_]+", " ", nm)
  }

  # Library model card (full documentation) when the run was built from the library
  lib_card <- NULL
  if (!is.null(info$model_template) && exists("bff_model_library", mode = "function")) {
    models <- bff_model_library()
    if (!is.null(models[[info$model_template]])) {
      lib_card <- bff_model_card_ui(models[[info$model_template]], compact = FALSE, data = data)
    }
  }

  meta_item <- function(label, value) htmltools::tags$div(htmltools::tags$span(label), value)
  page <- htmltools::tags$html(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "utf-8"),
      htmltools::tags$title(paste0("BayesianFitForecast run report - ", model_name)),
      htmltools::tags$style(htmltools::HTML(bff_report_css)),
      # MathJax v2 (same build as the Shiny app): equations need an internet connection to render
      htmltools::tags$script(src = "https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
    ),
    htmltools::tags$body(
      htmltools::tags$h1("BayesianFitForecast run report: ", model_name),
      htmltools::tags$p(class = "bff-note", sprintf("Generated on %s by the BayesianFitForecast Shiny app.", generated)),
      htmltools::tags$div(class = "bff-meta",
        meta_item("Status", htmltools::tags$span(class = paste0("bff-status bff-status-", status), status)),
        meta_item("Fitting time", elapsed),
        meta_item("Calibration period(s)", paste(get_opt("calibrationperiods", "?"), collapse = ", ")),
        meta_item("Forecast horizon", as.character(get_opt("forecastinghorizon", "?"))),
        meta_item("Chains x iterations", sprintf("%s x %s (warm-up %s)", get_opt("num_chain", "?"), get_opt("niter", "?"), get_opt("nwarmup", "default"))),
        meta_item("Error structure", if (!is.null(info)) info$err_text else as.character(get_opt("errstrc", "?"))),
        meta_item("Data", sprintf("%s%s", summary$data_name %||% "uploaded data", if (!is.null(data)) sprintf(" (%d time points, %d series)", nrow(data), ncol(data) - 1) else "")),
        meta_item("Sampler", sprintf("adapt_delta %s, max_treedepth %s, seed %s", get_opt("adapt_delta", 0.8), get_opt("max_treedepth", 10), get_opt("seed", 0)))
      ),

      htmltools::tags$h2("1. Fitted model"),
      if (!is.null(info)) htmltools::tagList(
        if (!is.null(info$model_template)) htmltools::tags$p("Built from the model library: ", htmltools::tags$strong(info$model_template), ". The full model card is given in section 7."),
        if (nzchar(info$system_latex)) htmltools::tags$div(class = "bff-eq", paste0("\\[", info$system_latex, "\\]")),
        if (length(info$ic_text)) htmltools::tags$p(htmltools::tags$strong("Initial conditions: "), paste(info$ic_text, collapse = "; ")),
        if (length(info$fit_text)) htmltools::tags$p(htmltools::tags$strong("Fitted quantities: "), paste(info$fit_text, collapse = "; "),
                                                     sprintf(". Error structure: %s%s.", info$err_text, if (length(info$err_priors)) paste0(" (priors of the error parameters: ", paste(info$err_priors, collapse = "; "), ")") else "")),
        htmltools::tags$h3("Parameters, priors and fixed values"),
        bff_html_table(info$param_table, math_cols = "Parameter"),
        if (length(info$composites) > 0) htmltools::tags$p(htmltools::tags$strong("Composite quantities: "),
                                                            paste(sprintf("%s = %s", names(info$composites), unlist(info$composites)), collapse = "; ")),
        if (length(info$tdp_display) > 0) htmltools::tags$p(htmltools::tags$strong("Time-dependent parameters: "), paste(unname(info$tdp_display), collapse = ", "))
      ) else htmltools::tags$p(class = "bff-note", "The configuration could not be interpreted; see section 8."),

      htmltools::tags$h2("2. Convergence diagnostics"),
      bff_html_table(conv),
      htmltools::tags$p(class = "bff-note", "R-hat close to 1 (< 1.05), effective sample sizes of several hundred, no divergent transitions and no saturated tree depth indicate a reliable posterior sample."),

      htmltools::tags$h2("3. Parameter estimates"),
      bff_html_table(pars),

      htmltools::tags$h2("4. Performance metrics"),
      bff_html_table(perf),
      htmltools::tags$p(class = "bff-note", "MAE / MSE: mean absolute / squared error of the posterior median; WIS: weighted interval score; coverage: % of observations inside the 95% prediction interval. Calibration = fitted window, forecast = horizon after it."),

      htmltools::tags$h2("5. Model-fit metrics (calibration window)"),
      bff_html_table(fitm),
      htmltools::tags$p(class = "bff-note", "Lower DIC / WAIC / LOOIC indicate a better fit, penalised for complexity; compare models fitted to the same data and calibration period."),

      htmltools::tags$h2("6. Figures"),
      if (length(figs) > 0) htmltools::tags$div(class = "bff-figs", lapply(figs, function(f) {
        htmltools::tags$figure(htmltools::tags$img(src = fig_src(f), alt = f), htmltools::tags$figcaption(fig_title(f)))
      })) else htmltools::tags$p(class = "bff-note", "No figures were saved."),

      if (!is.null(lib_card)) htmltools::tagList(htmltools::tags$h2("7. Model card (model library)"), lib_card) else NULL,

      htmltools::tags$h2(if (!is.null(lib_card)) "8. Configuration (options.R)" else "7. Configuration (options.R)"),
      htmltools::tags$p(class = "bff-note", "Re-load this file in the app ('Examples & configurations') or use it with the BayesianFitForecast R toolbox to reproduce the run."),
      htmltools::tags$pre(run_meta$options %||% ""),
      if (length(run_meta$warnings) > 0) htmltools::tagList(
        htmltools::tags$h3("Stan messages"),
        htmltools::tags$pre(paste(unlist(lapply(names(run_meta$warnings), function(k) c(sprintf("== Calibration period %s ==", k), run_meta$warnings[[k]], ""))), collapse = "\n"))
      ) else NULL
    )
  )
  # Standalone HTML (MathJax from the CDN; the equations show as LaTeX source when offline)
  html <- htmltools::renderTags(page)$html
  writeLines(c("<!DOCTYPE html>", html), file, useBytes = TRUE)
  if (!write_md) return(invisible(file))

  # Markdown version
  md <- c(sprintf("# BayesianFitForecast run report: %s", model_name), "",
          sprintf("Generated: %s. Fitting time: %s. Status: %s.", generated, elapsed, status), "")
  if (!is.null(opt)) md <- c(md, tryCatch(bff_options_model_markdown(opt), error = function(e) character(0)))
  md <- c(md, "## Convergence", "", bff_md_table(conv), "",
          "## Parameter estimates", "", bff_md_table(pars), "",
          "## Performance metrics", "", bff_md_table(perf), "",
          "## Model-fit metrics (calibration window)", "", bff_md_table(fitm), "")
  if (length(figs) > 0) {
    md <- c(md, "## Figures", "")
    for (f in figs) md <- c(md, sprintf("### %s", fig_title(f)), "", sprintf("![](figures/%s)", f), "")
  }
  md <- c(md, "## Configuration (options.R)", "", "```r", run_meta$options %||% "", "```", "")
  writeLines(md, file.path(root, "report.md"))
  invisible(file)
}

#------------------------------------------------------------------------------#
# Standalone HTML documentation of a library model ------------------------------
#------------------------------------------------------------------------------#
bff_write_model_sheet <- function(model, file, data = NULL) {
  models <- if (!is.null(model$id)) list(model) else model   # one model or a list of models
  cards <- lapply(models, function(m) htmltools::tagList(bff_model_card_ui(m, compact = FALSE, data = data), htmltools::tags$hr()))
  page <- htmltools::tags$html(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "utf-8"),
      htmltools::tags$title(if (length(models) == 1) paste0("BayesianFitForecast model: ", models[[1]]$title) else "BayesianFitForecast model library"),
      htmltools::tags$style(htmltools::HTML(bff_report_css)),
      htmltools::tags$style(htmltools::HTML(".table { border-collapse: collapse; } .table th, .table td { border: 1px solid #ddd; padding: 4px 8px; } .bff-hint { color: #666; font-size: 12px; } .label { background: #777; color: white; padding: 2px 6px; border-radius: 3px; font-size: 12px; }")),
      # MathJax v2 (same build as the Shiny app): equations need an internet connection to render
      htmltools::tags$script(src = "https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
    ),
    htmltools::tags$body(
      htmltools::tags$h1("BayesianFitForecast model library"),
      if (length(models) > 1) htmltools::tags$p(class = "bff-note", "Ready-to-use models of the Shiny app: equations, state variables, parameters with default priors, assumptions and references.") else NULL,
      cards,
      htmltools::tags$p(class = "bff-note", sprintf("Generated on %s by the BayesianFitForecast Shiny app.", format(Sys.time(), "%Y-%m-%d %H:%M")))
    )
  )
  writeLines(c("<!DOCTYPE html>", htmltools::renderTags(page)$html), file, useBytes = TRUE)
  invisible(file)
}
