#------------------------------------------------------------------------------#
# prior_tools.R — Prior density preview and prior predictive check --------------
#------------------------------------------------------------------------------#
# About: (1) Parse the Stan-style prior strings used in the options file (e.g.    #
# "normal(0, 1)T[0,]") and draw their densities; (2) simulate the ODE system     #
# from parameter values sampled from the priors (no fitting) and compare the    #
# simulated trajectories of the fitted quantities with the data. This is a port #
# of prior_sltn.R from the R toolbox, adapted to the app's options files.       #
#------------------------------------------------------------------------------#

`%||%` <- function(x, y) if (is.null(x)) y else x

#------------------------------------------------------------------------------#
# Parsing of prior strings -------------------------------------------------------
#------------------------------------------------------------------------------#
# Returns list(dist, args, lower, upper) or NULL when the string is not          #
# understood. Truncation "T[a,b]", "T[a,]" and "T[,b]" is supported.             #
#------------------------------------------------------------------------------#
bff_parse_prior <- function(prior) {
  prior <- gsub("\\s+", "", as.character(prior))
  m <- regmatches(prior, regexec("^([A-Za-z_0-9]+)\\((.*?)\\)(T\\[(.*?)\\])?$", prior))[[1]]
  if (length(m) == 0 || !nzchar(m[1])) return(NULL)
  dist <- tolower(m[2])
  args <- suppressWarnings(as.numeric(unlist(strsplit(m[3], ","))))
  if (any(is.na(args))) return(NULL)
  lower <- -Inf; upper <- Inf
  if (nzchar(m[5] %||% "")) {
    tb <- unlist(strsplit(m[5], ",", fixed = TRUE))
    if (length(tb) >= 1 && nzchar(tb[1])) lower <- as.numeric(tb[1])
    if (length(tb) >= 2 && nzchar(tb[2])) upper <- as.numeric(tb[2])
    if (grepl(",$", m[5]) && length(tb) == 1) upper <- Inf
  }
  # natural supports
  if (dist %in% c("lognormal", "exponential", "gamma", "inv_gamma", "chi_square", "weibull")) lower <- max(lower, 0)
  if (dist == "beta") { lower <- max(lower, 0); upper <- min(upper, 1) }
  if (dist %in% c("poisson", "neg_binomial_2", "neg_binomial")) lower <- max(lower, 0)
  list(dist = dist, args = args, lower = lower, upper = upper, text = prior)
}

bff_prior_is_discrete <- function(p) p$dist %in% c("poisson", "neg_binomial_2", "neg_binomial")

# Density function of a parsed prior (unnormalised w.r.t. truncation; fine for plots)
bff_prior_density <- function(p, x) {
  a <- p$args
  d <- switch(p$dist,
    "normal"         = dnorm(x, a[1], a[2]),
    "uniform"        = dunif(x, a[1], a[2]),
    "beta"           = dbeta(x, a[1], a[2]),
    "cauchy"         = dcauchy(x, a[1], a[2]),
    "gamma"          = dgamma(x, shape = a[1], rate = a[2]),
    "exponential"    = dexp(x, rate = a[1]),
    "lognormal"      = dlnorm(x, meanlog = a[1], sdlog = a[2]),
    "student_t"      = dt((x - a[2]) / a[3], df = a[1]) / a[3],
    "inv_gamma"      = ifelse(x > 0, a[2]^a[1] / gamma(a[1]) * x^(-a[1] - 1) * exp(-a[2] / x), 0),
    "neg_binomial_2" = dnbinom(round(x), mu = a[1], size = a[2]),
    "poisson"        = dpois(round(x), lambda = a[1]),
    stop(sprintf("Distribution '%s' is not supported for the preview.", p$dist))
  )
  d[x < p$lower | x > p$upper] <- 0
  d
}

# Quantile-based plotting range of a parsed prior
bff_prior_range <- function(p) {
  a <- p$args
  q <- function(pr) switch(p$dist,
    "normal"         = qnorm(pr, a[1], a[2]),
    "uniform"        = qunif(pr, a[1], a[2]),
    "beta"           = qbeta(pr, a[1], a[2]),
    "cauchy"         = qcauchy(pr, a[1], a[2]),
    "gamma"          = qgamma(pr, shape = a[1], rate = a[2]),
    "exponential"    = qexp(pr, rate = a[1]),
    "lognormal"      = qlnorm(pr, meanlog = a[1], sdlog = a[2]),
    "student_t"      = a[2] + a[3] * qt(pr, df = a[1]),
    "inv_gamma"      = 1 / qgamma(1 - pr, shape = a[1], rate = a[2]),
    "neg_binomial_2" = qnbinom(pr, mu = a[1], size = a[2]),
    "poisson"        = qpois(pr, lambda = a[1]),
    NA_real_)
  probs <- if (p$dist %in% c("cauchy", "student_t")) c(0.03, 0.97) else c(0.001, 0.999)
  lo <- max(p$lower, q(probs[1])); hi <- min(p$upper, q(probs[2]))
  if (!is.finite(lo)) lo <- q(probs[1]); if (!is.finite(hi)) hi <- q(probs[2])
  if (!is.finite(lo) || !is.finite(hi) || hi <= lo) { lo <- if (is.finite(p$lower)) p$lower else 0; hi <- lo + 1 }
  c(lo, hi)
}

# Random draws from a parsed prior (truncation by rejection)
bff_sample_prior <- function(p, n, lower = -Inf, upper = Inf) {
  a <- p$args
  lo <- max(p$lower, lower, na.rm = TRUE); hi <- min(p$upper, upper, na.rm = TRUE)
  draw <- function(m) switch(p$dist,
    "normal"         = rnorm(m, a[1], a[2]),
    "uniform"        = runif(m, a[1], a[2]),
    "beta"           = rbeta(m, a[1], a[2]),
    "cauchy"         = rcauchy(m, a[1], a[2]),
    "gamma"          = rgamma(m, shape = a[1], rate = a[2]),
    "exponential"    = rexp(m, rate = a[1]),
    "lognormal"      = rlnorm(m, meanlog = a[1], sdlog = a[2]),
    "student_t"      = a[2] + a[3] * rt(m, df = a[1]),
    "inv_gamma"      = 1 / rgamma(m, shape = a[1], rate = a[2]),
    "neg_binomial_2" = rnbinom(m, mu = a[1], size = a[2]),
    "poisson"        = rpois(m, lambda = a[1]),
    stop(sprintf("Distribution '%s' is not supported for sampling.", p$dist))
  )
  out <- numeric(0); tries <- 0
  while (length(out) < n && tries < 50) {
    x <- draw(max(n, 100))
    out <- c(out, x[x >= lo & x <= hi])
    tries <- tries + 1
  }
  if (length(out) < n) stop(sprintf("Could not draw from '%s' within its bounds [%s, %s].", p$text, lo, hi))
  out[seq_len(n)]
}

#------------------------------------------------------------------------------#
# Prior density preview ---------------------------------------------------------
#------------------------------------------------------------------------------#
# spec: data.frame(label, prior, fixed) — fixed == 1 means `prior` is a number.  #
#------------------------------------------------------------------------------#
bff_prior_density_plot <- function(spec) {
  curves <- list(); lines <- list(); problems <- character(0)
  for (i in seq_len(nrow(spec))) {
    lab <- spec$label[i]
    if (as.numeric(spec$fixed[i]) == 1 || !grepl("\\(", spec$prior[i])) {
      val <- suppressWarnings(as.numeric(spec$prior[i]))
      lines[[length(lines) + 1]] <- data.frame(label = lab, value = val)
      next
    }
    p <- bff_parse_prior(spec$prior[i])
    if (is.null(p)) { problems <- c(problems, sprintf("%s: '%s' could not be parsed", lab, spec$prior[i])); next }
    rng <- tryCatch(bff_prior_range(p), error = function(e) NULL)
    if (is.null(rng)) { problems <- c(problems, sprintf("%s: unsupported distribution", lab)); next }
    x <- if (bff_prior_is_discrete(p)) seq(floor(rng[1]), ceiling(rng[2])) else seq(rng[1], rng[2], length.out = 300)
    dens <- tryCatch(bff_prior_density(p, x), error = function(e) NULL)
    if (is.null(dens)) { problems <- c(problems, sprintf("%s: unsupported distribution", lab)); next }
    curves[[length(curves) + 1]] <- data.frame(label = sprintf("%s ~ %s", lab, p$text), x = x, density = dens,
                                               discrete = bff_prior_is_discrete(p))
  }
  if (length(curves) == 0 && length(lines) == 0) stop(paste(c("No prior could be drawn.", problems), collapse = "\n"))
  curve_df <- if (length(curves)) do.call(rbind, curves) else NULL
  line_df <- if (length(lines)) do.call(rbind, lines) else NULL
  if (!is.null(line_df)) {
    line_df$label <- sprintf("%s = %s (fixed)", line_df$label, format(line_df$value))
    line_df$x <- line_df$value; line_df$density <- 1
  }
  all_labels <- unique(c(if (!is.null(curve_df)) unique(curve_df$label), if (!is.null(line_df)) line_df$label))
  g <- ggplot2::ggplot()
  if (!is.null(curve_df)) {
    cont <- curve_df[!curve_df$discrete, , drop = FALSE]; disc <- curve_df[curve_df$discrete, , drop = FALSE]
    if (nrow(cont)) g <- g + ggplot2::geom_area(data = cont, ggplot2::aes(x = x, y = density), fill = "#5bc0de", alpha = 0.35) +
      ggplot2::geom_line(data = cont, ggplot2::aes(x = x, y = density), color = "#337ab7", linewidth = 0.9)
    if (nrow(disc)) g <- g + ggplot2::geom_col(data = disc, ggplot2::aes(x = x, y = density), fill = "#5bc0de", color = "#337ab7", width = 0.8)
  }
  if (!is.null(line_df)) {
    g <- g + ggplot2::geom_vline(data = line_df, ggplot2::aes(xintercept = x), color = "#d9534f", linewidth = 1.2)
  }
  g + ggplot2::facet_wrap(~ label, scales = "free", ncol = 3) +
    ggplot2::labs(x = NULL, y = "density",
                  caption = if (length(problems)) paste("Not shown:", paste(problems, collapse = "; ")) else NULL) +
    ggplot2::theme_classic() + ggplot2::theme(strip.text = ggplot2::element_text(size = 10))
}

#------------------------------------------------------------------------------#
# Prior predictive check ----------------------------------------------------------
#------------------------------------------------------------------------------#
bff_prior_predictive_check <- function(optionsFile, data, n_sims = 200, seed = 1) {

  if (!requireNamespace("deSolve", quietly = TRUE)) stop("Package 'deSolve' is required for the prior predictive check.")
  set.seed(seed)

  opt <- bff_load_options(optionsFile)
  vars <- as.character(opt$vars); params <- as.character(opt$params); paramsfix <- as.numeric(opt$paramsfix)
  fitting_index <- as.numeric(opt$fitting_index); fitting_diff <- as.numeric(opt$fitting_diff)
  series_cases <- as.character(opt$series_cases %||% paste0("Series ", seq_along(fitting_index)))
  datetype <- opt$datetype %||% "Time"
  cal <- max(as.numeric(opt$calibrationperiods)); h <- as.numeric(opt$forecastinghorizon)
  n_times <- cal + h
  templates <- opt$time_dependent_templates %||% list()

  ## ---- RHS expressions (varsN / paramsN / time_dependent_paramN placeholders) ----
  ode_lines <- trimws(unlist(strsplit(as.character(opt$ode_system), "\n")))
  ode_lines <- ode_lines[nzchar(ode_lines)]
  rhs_text <- vapply(ode_lines, function(l) sub("^.*?=\\s*", "", l), character(1))
  rhs_text <- gsub(";\\s*$", "", rhs_text)
  rhs_expr <- lapply(rhs_text, function(s) parse(text = s)[[1]])

  tdp_fun <- lapply(templates, function(body) {
    body <- gsub("\\bpi\\s*\\(\\s*\\)", "pi", as.character(body), perl = TRUE)
    eval(parse(text = paste0("function() {\n", body, "\n}")))
  })

  base_env <- new.env(parent = baseenv())
  base_env$pow <- function(x, y) x^y
  base_env$fmax <- function(x, y) pmax(x, y)
  base_env$fmin <- function(x, y) pmin(x, y)
  base_env$fabs <- abs
  base_env$square <- function(x) x^2
  base_env$inv <- function(x) 1 / x

  make_env <- function(param_values) {
    env <- new.env(parent = base_env)
    for (k in seq_along(params)) {
      assign(paste0("params", k), param_values[k], envir = env)
      assign(params[k], param_values[k], envir = env)   # real names for the initial conditions
    }
    env
  }
  eval_tdp <- function(env, t) {
    assign("t", t, envir = env); assign("time", t, envir = env)
    for (nm in names(tdp_fun)) {
      f <- tdp_fun[[nm]]; environment(f) <- env
      assign(nm, as.numeric(f())[1], envir = env)
    }
  }
  rhs_fun <- function(t, state, env) {
    eval_tdp(env, t)
    for (k in seq_along(vars)) assign(paste0("vars", k), state[k], envir = env)
    vapply(rhs_expr, function(e) as.numeric(eval(e, envir = env))[1], numeric(1))
  }

  ## ---- Parameter draws from the priors ----
  draws <- matrix(NA_real_, nrow = n_sims, ncol = length(params)); colnames(draws) <- params
  for (k in seq_along(params)) {
    prior <- get(paste0("params", k, "_prior"), envir = opt)
    if (paramsfix[k] == 1 || is.numeric(prior)) {
      draws[, k] <- as.numeric(prior)
    } else {
      p <- bff_parse_prior(prior)
      if (is.null(p)) stop(sprintf("The prior of '%s' ('%s') could not be parsed.", params[k], prior))
      lb <- if (exists(paste0("params", k, "_LB"), envir = opt)) get(paste0("params", k, "_LB"), envir = opt) else NA
      ub <- if (exists(paste0("params", k, "_UB"), envir = opt)) get(paste0("params", k, "_UB"), envir = opt) else NA
      draws[, k] <- bff_sample_prior(p, n_sims,
                                     lower = if (is.na(lb)) -Inf else as.numeric(lb),
                                     upper = if (is.na(ub)) Inf else as.numeric(ub))
    }
  }

  ## ---- Simulate ----
  times <- 0:n_times
  sims <- vector("list", length(fitting_index))
  for (j in seq_along(fitting_index)) sims[[j]] <- matrix(NA_real_, nrow = n_sims, ncol = n_times)
  n_fail <- 0
  Ic <- opt$Ic
  for (s in seq_len(n_sims)) {
    env <- make_env(draws[s, ])
    y0 <- tryCatch(vapply(Ic, function(ic) {
      v <- suppressWarnings(as.numeric(ic))
      if (!is.na(v)) v else as.numeric(eval(parse(text = gsub("\\\\", "", as.character(ic))), envir = env))
    }, numeric(1)), error = function(e) NULL)
    if (is.null(y0) || any(!is.finite(y0))) { n_fail <- n_fail + 1; next }
    sol <- tryCatch(suppressWarnings(deSolve::ode(y = unname(y0), times = times,
                                                  func = function(t, y, parms) list(rhs_fun(t, y, env)),
                                                  parms = NULL, method = "lsoda")),
                    error = function(e) NULL)
    if (is.null(sol) || nrow(sol) < length(times) || any(!is.finite(sol))) { n_fail <- n_fail + 1; next }
    states <- sol[-1, -1, drop = FALSE]   # rows = times 1..n_times
    for (j in seq_along(fitting_index)) {
      idx <- fitting_index[j]
      if (fitting_diff[j] == 1) {
        vals <- vapply(seq_len(n_times), function(ti) rhs_fun(times[ti + 1], states[ti, ], env)[idx], numeric(1))
      } else {
        vals <- states[, idx]
      }
      sims[[j]][s, ] <- vals
    }
  }
  n_ok <- n_sims - n_fail
  if (n_ok < 5) stop(sprintf("Only %d of %d prior simulations could be solved. The priors or initial conditions produce invalid trajectories (negative, infinite or stiff solutions).", n_ok, n_sims))

  ## ---- Summaries and plot ----
  plot_df <- list(); data_df <- list(); coverage <- numeric(0)
  for (j in seq_along(fitting_index)) {
    m <- sims[[j]][stats::complete.cases(sims[[j]]), , drop = FALSE]
    q <- apply(m, 2, stats::quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)
    label <- if (j <= length(series_cases) && nzchar(series_cases[j])) series_cases[j] else paste0("Series ", j)
    plot_df[[j]] <- data.frame(series = label, time = 0:(n_times - 1),
                               lo95 = q[1, ], lo50 = q[2, ], med = q[3, ], hi50 = q[4, ], hi95 = q[5, ])
    obs <- as.numeric(data[[paste0("cases", j)]])
    n_obs <- min(length(obs), n_times)
    if (n_obs > 0) {
      data_df[[j]] <- data.frame(series = label, time = 0:(n_obs - 1), obs = obs[seq_len(n_obs)])
      inside <- obs[seq_len(n_obs)] >= q[1, seq_len(n_obs)] & obs[seq_len(n_obs)] <= q[5, seq_len(n_obs)]
      coverage <- c(coverage, mean(inside, na.rm = TRUE))
    }
  }
  plot_df <- do.call(rbind, plot_df); data_df <- if (length(data_df)) do.call(rbind, data_df) else NULL

  g <- ggplot2::ggplot(plot_df, ggplot2::aes(x = time)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo95, ymax = hi95), fill = "#5bc0de", alpha = 0.25) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo50, ymax = hi50), fill = "#337ab7", alpha = 0.35) +
    ggplot2::geom_line(ggplot2::aes(y = med), color = "#23527c", linewidth = 1)
  if (!is.null(data_df)) g <- g + ggplot2::geom_point(data = data_df, ggplot2::aes(x = time, y = obs), shape = 21, fill = "white", size = 1.8)
  if (h > 0) g <- g + ggplot2::geom_vline(xintercept = cal - 1, linetype = "dotted", color = "blue")
  g <- g + ggplot2::facet_wrap(~ series, scales = "free_y", ncol = 1) +
    ggplot2::labs(x = datetype, y = "Simulated value",
                  title = sprintf("Prior predictive check: %d simulations from the priors (%d failed)", n_sims, n_fail),
                  subtitle = "Dark band: 50% interval; light band: 95% interval; line: median; points: observed data") +
    ggplot2::theme_classic()

  cov_txt <- if (length(coverage)) sprintf(" %.0f%% of the observed points fall inside the 95%% prior band.", 100 * mean(coverage)) else ""
  message <- paste0(
    sprintf("%d of %d parameter draws produced a valid ODE solution.", n_ok, n_sims), cov_txt,
    if (length(coverage) && mean(coverage) < 0.5) " The priors seem too restrictive or mis-centred: the data fall mostly outside what the model can produce under the priors." else "",
    if (n_fail > 0.3 * n_sims) " Many draws failed: consider tighter or better-centred priors / bounds, or check the initial conditions." else ""
  )
  list(plot = g, message = message, draws = draws, n_fail = n_fail)
}
