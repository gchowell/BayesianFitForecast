#------------------------------------------------------------------------------#
# Time-dependent parameter template library (pure R, no Shiny)                  #
#------------------------------------------------------------------------------#
# About: This file defines the structured template library for time-dependent  #
# parameters and the serializer that turns a user's template selection into the #
# `time_dependent_templates <- list(...)` block written to the options file.    #
#                                                                               #
# Contract with the MAIN updated stancreator.R:                                 #
#   * Each template body is emitted using positional placeholders paramsN,      #
#     where N is the 1-based index of the chosen parameter in the `params`      #
#     vector. The main stancreator parses these with regex "params[0-9]+" and   #
#     maps them back to parameter names, so NO time-dependent Stan generation   #
#     is needed (or done) on the Shiny side.                                    #
#   * Bodies reference the free variable `t` and Stan math (exp, cos, pi()).    #
#------------------------------------------------------------------------------#

# small null-coalescing helper
`%||%` <- function(a, b) if (is.null(a)) b else a

#------------------------------------------------------------------------------#
# Template registry ------------------------------------------------------------
#------------------------------------------------------------------------------#
# Each template is a list with:
#   label : human-readable name shown in the UI dropdown
#   desc  : short plain-language description of the functional form
#   roles : ordered list of list(id=, label=) — each role maps to a parameter
#   body  : function(p) where p is a named list role_id -> token; returns the
#           Stan-style function body string (uses the tokens and `t`).
TDP_TEMPLATES <- list(

  const_step = list(
    label = "Constant before/after intervention",
    desc  = "Step change: one constant value before the intervention time, another at/after it.",
    roles = list(
      list(id = "before", label = "Value before intervention"),
      list(id = "after",  label = "Value at/after intervention"),
      list(id = "t_int",  label = "Intervention time")
    ),
    body = function(p) {
      sprintf("if (t < %s) { return (%s); } else { return (%s); }",
              p$t_int, p$before, p$after)
    }
  ),

  exp_transition = list(
    label = "Exponential transition after intervention",
    desc  = "Constant before the intervention, then exponential relaxation from the pre-value toward an asymptote (Example 7).",
    roles = list(
      list(id = "before",    label = "Value before intervention"),
      list(id = "asymptote", label = "Asymptote (value after intervention)"),
      list(id = "rate",      label = "Transition rate"),
      list(id = "t_int",     label = "Intervention time")
    ),
    body = function(p) {
      sprintf(paste0(
        "if (t < %s) { return (%s); } else { ",
        "return (%s + (%s - %s) * exp(-%s * (t - %s))); }"),
        p$t_int, p$before, p$asymptote, p$before, p$asymptote, p$rate, p$t_int)
    }
  ),

  piecewise_linear = list(
    label = "Piecewise linear transition",
    desc  = "Flat at the start value until t_start, linear ramp to the end value between t_start and t_end, flat afterward.",
    roles = list(
      list(id = "before",  label = "Value before t_start"),
      list(id = "after",   label = "Value after t_end"),
      list(id = "t_start", label = "Transition start time"),
      list(id = "t_end",   label = "Transition end time")
    ),
    body = function(p) {
      sprintf(paste0(
        "if (t < %s) { return (%s); } else if (t > %s) { return (%s); } else { ",
        "return (%s + (%s - %s) * (t - %s) / (%s - %s)); }"),
        p$t_start, p$before, p$t_end, p$after,
        p$before, p$after, p$before, p$t_start, p$t_end, p$t_start)
    }
  ),

  linear_trend = list(
    label = "Linear trend",
    desc  = "A straight line in time: intercept at t = 0 plus a constant slope.",
    roles = list(
      list(id = "intercept", label = "Intercept (value at t = 0)"),
      list(id = "slope",     label = "Slope per unit time")
    ),
    body = function(p) {
      sprintf("return (%s + %s * t);", p$intercept, p$slope)
    }
  ),

  seasonal_cosine = list(
    label = "Seasonal / cosine forcing",
    desc  = "Baseline value modulated by a cosine wave: mean * (1 + amplitude * cos(2*pi*(t - phase) / period)).",
    roles = list(
      list(id = "mean",   label = "Baseline (mean) value"),
      list(id = "amp",    label = "Relative amplitude"),
      list(id = "period", label = "Period"),
      list(id = "phase",  label = "Phase shift")
    ),
    body = function(p) {
      sprintf("return (%s * (1 + %s * cos(2 * pi() * (t - %s) / %s)));",
              p$mean, p$amp, p$phase, p$period)
    }
  ),

  logistic_transition = list(
    label = "Logistic transition",
    desc  = "Smooth logistic (sigmoid) change from an initial value to a final value, centered at t_mid with a given steepness.",
    roles = list(
      list(id = "before", label = "Initial value (lower asymptote)"),
      list(id = "after",  label = "Final value (upper asymptote)"),
      list(id = "rate",   label = "Steepness (rate)"),
      list(id = "t_mid",  label = "Midpoint time")
    ),
    body = function(p) {
      sprintf("return (%s + (%s - %s) / (1 + exp(-%s * (t - %s))));",
              p$before, p$after, p$before, p$rate, p$t_mid)
    }
  )
)

# Sentinel type id for the free-form advanced editor (not in TDP_TEMPLATES).
TDP_ADVANCED <- "advanced"

#------------------------------------------------------------------------------#
# Accessors --------------------------------------------------------------------
#------------------------------------------------------------------------------#

# Named choices vector for a selectInput: c("Label" = "type_id", ...),
# with the advanced option appended last.
tdp_choices <- function() {
  labels <- vapply(TDP_TEMPLATES, function(x) x$label, character(1))
  choices <- setNames(names(TDP_TEMPLATES), labels)
  c(choices, setNames(TDP_ADVANCED, "Advanced (custom expression)"))
}

# Role definitions for a given template type (empty for advanced/unknown).
tdp_roles <- function(type) {
  tpl <- TDP_TEMPLATES[[type]]
  if (is.null(tpl)) return(list())
  tpl$roles
}

# Plain-language description for a given template type.
tdp_desc <- function(type) {
  if (identical(type, TDP_ADVANCED)) {
    return("Free-form expression in terms of your parameter names and t. Advanced: validated only when the model is compiled.")
  }
  tpl <- TDP_TEMPLATES[[type]]
  if (is.null(tpl)) "" else tpl$desc
}

#------------------------------------------------------------------------------#
# User-facing LaTeX helpers ----------------------------------------------------
#------------------------------------------------------------------------------#

# `params_choices` is the named vector passed by the Shiny module:
#   names(params_choices)  : user-facing LaTeX/display labels, e.g. "\\beta_0"
#   unname(params_choices) : internal names, e.g. "beta0"
tdp_param_display_label <- function(param_internal, params_choices) {
  if (is.null(param_internal) || !nzchar(param_internal)) return("?")
  if (is.null(params_choices) || length(params_choices) == 0) return(param_internal)
  idx <- match(param_internal, unname(params_choices))
  if (is.na(idx)) return(param_internal)
  label <- names(params_choices)[[idx]]
  if (is.null(label) || !nzchar(label)) param_internal else label
}

tdp_param_placeholder_label <- function(param_internal, params_choices) {
  if (is.null(param_internal) || !nzchar(param_internal)) return("")
  if (is.null(params_choices) || length(params_choices) == 0) return("")
  idx <- match(param_internal, unname(params_choices))
  if (is.na(idx)) return("")
  paste0("params", idx)
}

tdp_clean_math_label <- function(label, fallback = "?") {
  label <- trimws(as.character(label %||% ""))
  if (!nzchar(label)) fallback else label
}

tdp_render_math_preview <- function(spec, params_choices) {
  if (is.null(spec) || is.null(spec$type)) return("")

  f <- tdp_clean_math_label(spec$display_name, spec$internal_name %||% "f(t)")
  role <- function(id) {
    tdp_clean_math_label(
      tdp_param_display_label(spec$mapping[[id]], params_choices)
    )
  }

  if (identical(spec$type, TDP_ADVANCED)) {
    return(sprintf(
      "%s = \\text{custom function of } t",
      f
    ))
  }

  switch(
    spec$type,
    const_step = sprintf(
      "%s = \\begin{cases} %s, & t < %s \\\\ %s, & t \\ge %s \\end{cases}",
      f, role("before"), role("t_int"), role("after"), role("t_int")
    ),
    exp_transition = sprintf(
      "%s = \\begin{cases} %s, & t < %s \\\\ %s + (%s - %s)\\exp\\{- %s(t - %s)\\}, & t \\ge %s \\end{cases}",
      f, role("before"), role("t_int"), role("asymptote"),
      role("before"), role("asymptote"), role("rate"), role("t_int"), role("t_int")
    ),
    piecewise_linear = sprintf(
      "%s = \\begin{cases} %s, & t < %s \\\\ %s + (%s - %s)\\dfrac{t - %s}{%s - %s}, & %s \\le t \\le %s \\\\ %s, & t > %s \\end{cases}",
      f, role("before"), role("t_start"), role("before"), role("after"),
      role("before"), role("t_start"), role("t_end"), role("t_start"),
      role("t_start"), role("t_end"), role("after"), role("t_end")
    ),
    linear_trend = sprintf(
      "%s = %s + %s t",
      f, role("intercept"), role("slope")
    ),
    seasonal_cosine = sprintf(
      "%s = %s\\left[1 + %s\\cos\\left(\\dfrac{2\\pi(t - %s)}{%s}\\right)\\right]",
      f, role("mean"), role("amp"), role("phase"), role("period")
    ),
    logistic_transition = sprintf(
      "%s = %s + \\dfrac{%s - %s}{1 + \\exp\\{- %s(t - %s)\\}}",
      f, role("before"), role("after"), role("before"), role("rate"), role("t_mid")
    ),
    sprintf("%s = \\text{unknown template}", f)
  )
}

#------------------------------------------------------------------------------#
# Placeholder helpers ----------------------------------------------------------
#------------------------------------------------------------------------------#

# Convert a parameter NAME to its positional placeholder "paramsN".
tdp_placeholder <- function(param_name, params) {
  idx <- match(param_name, params)
  if (is.na(idx)) {
    stop("Parameter '", param_name, "' is not one of the declared parameters.")
  }
  paste0("params", idx)
}

# Replace every whole-word occurrence of a declared parameter NAME in a
# free-form body with its positional placeholder. Longer names are replaced
# first so that a name is never partially matched inside another.
tdp_names_to_placeholders <- function(body, params) {
  if (length(params) == 0 || is.null(body) || !nzchar(body)) return(body)
  ord <- order(nchar(params), decreasing = TRUE)
  for (nm in params[ord]) {
    body <- gsub(paste0("\\b", nm, "\\b"), paste0("params", match(nm, params)),
                 body, perl = TRUE)
  }
  body
}

#------------------------------------------------------------------------------#
# Spec completeness + body rendering -------------------------------------------
#------------------------------------------------------------------------------#
# A spec is a list with:
#   internal_name : e.g. "time_dependent_param1" (list key / ODE symbol)
#   display_name  : cosmetic label (e.g. "beta(t)")
#   type          : a TDP_TEMPLATES id or TDP_ADVANCED
#   mapping       : named list role_id -> parameter name (structured templates)
#   custom_body   : free-form body string (advanced mode)

# Is a spec ready to serialize? (all roles mapped to real params, or a
# non-empty advanced body.)
tdp_spec_complete <- function(spec, params) {
  if (is.null(spec) || is.null(spec$type)) return(FALSE)
  if (identical(spec$type, TDP_ADVANCED)) {
    return(!is.null(spec$custom_body) && nzchar(trimws(spec$custom_body)))
  }
  roles <- tdp_roles(spec$type)
  if (length(roles) == 0) return(FALSE)
  for (r in roles) {
    val <- spec$mapping[[r$id]]
    if (is.null(val) || !nzchar(val) || is.na(match(val, params))) return(FALSE)
  }
  TRUE
}

# Render the body with actual parameter NAMES (human-readable preview).
tdp_render_body_named <- function(spec) {
  if (identical(spec$type, TDP_ADVANCED)) return(spec$custom_body %||% "")
  tpl <- TDP_TEMPLATES[[spec$type]]
  if (is.null(tpl)) return("")
  tpl$body(spec$mapping)
}

# Render the body with positional paramsN placeholders (for serialization).
tdp_render_body <- function(spec, params) {
  if (identical(spec$type, TDP_ADVANCED)) {
    return(tdp_names_to_placeholders(spec$custom_body, params))
  }
  tpl <- TDP_TEMPLATES[[spec$type]]
  if (is.null(tpl)) stop("Unknown template type: ", spec$type)
  ph <- lapply(spec$mapping, function(nm) tdp_placeholder(nm, params))
  tpl$body(ph)
}

#------------------------------------------------------------------------------#
# Serialization ----------------------------------------------------------------
#------------------------------------------------------------------------------#

# Turn a list of specs into the R code block for the options file. Incomplete
# specs are skipped. Returns "time_dependent_templates <- list()" when empty.
tdp_serialize <- function(specs, params) {
  complete <- Filter(function(s) tdp_spec_complete(s, params), specs)
  if (length(complete) == 0) {
    return("time_dependent_templates <- list()")
  }
  entries <- vapply(complete, function(spec) {
    body <- tdp_render_body(spec, params)
    body <- gsub("\\", "\\\\", body, fixed = TRUE)  # escape backslashes
    body <- gsub("\"", "\\\"", body, fixed = TRUE)   # escape double quotes
    sprintf('  %s = "%s"', spec$internal_name, body)
  }, character(1))
  paste0("time_dependent_templates <- list(\n",
         paste(entries, collapse = ",\n"), "\n)")
}

# Convenience: the ODE-usage hint shown in the preview panel.
tdp_ode_usage <- function(internal_name) {
  sprintf("Reference %s in your ODE equations, e.g.  dS/dt = -%s * S * I / N",
          internal_name, internal_name)
}
