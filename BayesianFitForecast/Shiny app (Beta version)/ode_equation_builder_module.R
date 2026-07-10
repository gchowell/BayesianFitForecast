#------------------------------------------------------------------------------#
# Token-based ODE equation builder --------------------------------------------#
#------------------------------------------------------------------------------#
# About: A structured replacement for free-text LaTeX ODE entry. Users compose  #
# ODE right-hand sides by clicking state-variable, scalar-parameter,             #
# time-dependent-parameter, operator/function, and numeric tokens. The module    #
# stores the exact internal tokens needed by the options file: varsN, paramsN,   #
# and time_dependent_paramN. No latex2r parsing is used for ODE serialization.   #
#------------------------------------------------------------------------------#

`%||%` <- get0(
  "%||%",
  mode = "function",
  ifnotfound = function(x, y) if (is.null(x)) y else x
)

#------------------------------------------------------------------------------#
# Small helpers ----------------------------------------------------------------
#------------------------------------------------------------------------------#

ode_builder_json <- function(x) {
  as.character(jsonlite::toJSON(as.character(x %||% ""), auto_unbox = TRUE))
}

ode_builder_clean_math_label <- function(label, fallback = "?") {
  label <- trimws(as.character(label %||% ""))
  if (!nzchar(label)) fallback else label
}

ode_builder_math_inline <- function(label, class = "") {
  label <- ode_builder_clean_math_label(label)
  withMathJax(tags$span(
    class = paste("ode-math-inline", class),
    paste0("\\(", label, "\\)")
  ))
}

ode_builder_typeset_script <- function(ns) {
  root_id <- ns("root")
  tags$script(HTML(sprintf(
    "
    (function() {
      var rootId = %s;
      function typesetOdeBuilder() {
        var root = document.getElementById(rootId);
        if (!root || !window.MathJax) return;
        try {
          if (window.MathJax.typesetPromise) {
            window.MathJax.typesetPromise([root]);
          } else if (window.MathJax.Hub) {
            window.MathJax.Hub.Queue(['Typeset', window.MathJax.Hub, root]);
          }
        } catch(e) {}
      }
      document.addEventListener('shiny:value', function(e) {
        if (e.target && e.target.id && e.target.id.indexOf(rootId) === 0) {
          setTimeout(typesetOdeBuilder, 40);
          setTimeout(typesetOdeBuilder, 250);
        }
      });
      document.addEventListener('click', function(e) {
        if (e.target && e.target.closest && e.target.closest('#' + rootId)) {
          setTimeout(typesetOdeBuilder, 40);
          setTimeout(typesetOdeBuilder, 250);
        }
      });
    })();
    ",
    ode_builder_json(root_id)
  )))
}

ode_builder_token_row <- function(label, display, serial, kind) {
  data.frame(
    label = as.character(label),
    display = as.character(display),
    serial = as.character(serial),
    kind = as.character(kind),
    stringsAsFactors = FALSE
  )
}

ode_builder_named_vector_to_tokens <- function(x, kind) {
  if (is.null(x) || length(x) == 0) {
    return(data.frame(label = character(0), display = character(0),
                      serial = character(0), kind = character(0)))
  }
  serial <- trimws(as.character(unname(x)))
  label <- names(x)
  if (is.null(label) || length(label) != length(serial) || all(!nzchar(label))) {
    label <- serial
  }
  label <- trimws(as.character(label))
  keep <- nzchar(label) & nzchar(serial)
  data.frame(
    label = label[keep],
    display = label[keep],
    serial = serial[keep],
    kind = rep(kind, sum(keep)),
    stringsAsFactors = FALSE
  )
}

ode_builder_operator_tokens <- function() {
  data.frame(
    label = c("+", "-", "*", "/", "(", ")", "^",
              "exp(", "log(", "sqrt(", "sin(", "cos(", "t"),
    display = c("+", "-", "\u00d7", "/", "(", ")", "^",
                "exp(", "log(", "sqrt(", "sin(", "cos(", "t"),
    serial = c("+", "-", "*", "/", "(", ")", "^",
               "exp(", "log(", "sqrt(", "sin(", "cos(", "t"),
    kind = c(rep("operator", 12), "time"),
    stringsAsFactors = FALSE
  )
}

ode_builder_digit_tokens <- function() {
  data.frame(
    label = c(as.character(0:9), "."),
    display = c(as.character(0:9), "."),
    serial = c(as.character(0:9), "."),
    kind = rep("number_digit", 11),
    stringsAsFactors = FALSE
  )
}

ode_builder_empty_catalog <- function() {
  data.frame(label = character(0), display = character(0),
             serial = character(0), kind = character(0))
}

ode_builder_tdp_tokens <- function(tdp_value) {
  if (is.null(tdp_value) || is.null(tdp_value$specs) || length(tdp_value$specs) == 0) {
    return(ode_builder_empty_catalog())
  }

  complete_names <- as.character(tdp_value$names %||% character(0))
  if (length(complete_names) == 0) {
    return(ode_builder_empty_catalog())
  }

  rows <- lapply(tdp_value$specs, function(spec) {
    internal <- spec$internal_name %||% ""
    if (!nzchar(internal) || !(internal %in% complete_names)) {
      return(NULL)
    }
    display <- trimws(spec$display_name %||% internal)
    if (!nzchar(display)) display <- internal
    ode_builder_token_row(display, display, internal, "tdp")
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(ode_builder_empty_catalog())
  do.call(rbind, rows)
}

ode_builder_math_button_label <- function(token) {
  token <- as.list(token)
  kind <- token$kind[[1]] %||% ""
  label <- token$label[[1]] %||% token$display[[1]] %||% token$serial[[1]] %||% ""

  if (kind %in% c("state", "param", "tdp", "time")) {
    return(ode_builder_math_inline(label, "ode-token-math"))
  }

  label
}

ode_builder_token_button <- function(ns, token, class_extra = "") {
  token <- as.list(token)
  payload <- paste0(
    "{display:", ode_builder_json(token$display[[1]] %||% ""),
    ",serial:", ode_builder_json(token$serial[[1]] %||% ""),
    ",kind:", ode_builder_json(token$kind[[1]] %||% "operator"),
    ",nonce:Math.random()}"
  )
  onclick <- sprintf(
    "Shiny.setInputValue(%s,%s,{priority:'event'});",
    ode_builder_json(ns("token_click")),
    payload
  )
  tags$button(
    type = "button",
    class = paste("btn btn-default btn-xs ode-token-btn", class_extra),
    onclick = onclick,
    ode_builder_math_button_label(token)
  )
}

ode_builder_select_button <- function(ns, row, label = "Edit") {
  payload <- paste0("{row:", as.integer(row), ",nonce:Math.random()}")
  onclick <- sprintf(
    "Shiny.setInputValue(%s,%s,{priority:'event'});",
    ode_builder_json(ns("select_row")),
    payload
  )
  tags$button(
    type = "button",
    class = "btn btn-primary btn-xs ode-card-action",
    onclick = onclick,
    label
  )
}

ode_builder_edit_button <- function(ns, action, label, class_extra = "", row = NULL) {
  payload <- if (is.null(row)) {
    paste0("{action:", ode_builder_json(action), ",nonce:Math.random()}")
  } else {
    paste0("{row:", as.integer(row),
           ",action:", ode_builder_json(action),
           ",nonce:Math.random()}")
  }
  onclick <- sprintf(
    "Shiny.setInputValue(%s,%s,{priority:'event'});",
    ode_builder_json(ns("edit_click")),
    payload
  )
  tags$button(
    type = "button",
    class = paste("btn btn-default btn-xs ode-card-action", class_extra),
    onclick = onclick,
    label
  )
}

ode_builder_number_button <- function(ns) {
  number_id <- ns("number_active")
  payload <- paste0(
    "{value:(document.getElementById(", ode_builder_json(number_id),
    ")?document.getElementById(", ode_builder_json(number_id), ").value:''),",
    "nonce:Math.random()}"
  )
  onclick <- sprintf(
    "Shiny.setInputValue(%s,%s,{priority:'event'});",
    ode_builder_json(ns("number_click")),
    payload
  )
  tags$button(
    type = "button",
    class = "btn btn-primary btn-xs ode-token-btn",
    onclick = onclick,
    "Insert number"
  )
}

ode_builder_button_group <- function(title, buttons, empty_text = NULL) {
  if (length(buttons) == 0 && !is.null(empty_text)) {
    buttons <- list(tags$span(class = "ode-token-empty", empty_text))
  }
  tags$div(
    class = "ode-token-group",
    tags$div(class = "ode-token-group-title", title),
    buttons
  )
}

ode_builder_get_tokens <- function(token_state, row) {
  token_state[[paste0("row_", row)]] %||% list()
}

ode_builder_set_tokens <- function(token_state, row, tokens) {
  token_state[[paste0("row_", row)]] <- tokens
}

ode_builder_token_display <- function(token, catalog) {
  if (NROW(catalog) > 0 && token$kind %in% c("state", "param", "tdp", "operator", "time")) {
    hit <- catalog[catalog$serial == token$serial & catalog$kind == token$kind, , drop = FALSE]
    if (NROW(hit) > 0) return(hit$display[[1]])
  }
  token$display %||% token$serial %||% ""
}

ode_builder_tokens_to_display <- function(tokens, catalog) {
  if (length(tokens) == 0) return("")
  paste(vapply(tokens, ode_builder_token_display, character(1), catalog = catalog),
        collapse = " ")
}

ode_builder_token_latex <- function(token, catalog) {
  label <- ode_builder_token_display(token, catalog)
  serial <- token$serial %||% ""
  kind <- token$kind %||% ""

  if (identical(kind, "operator")) {
    return(switch(
      serial,
      "*" = "\\,",
      "exp(" = "\\exp(",
      "log(" = "\\log(",
      "sqrt(" = "\\operatorname{sqrt}(",
      "sin(" = "\\sin(",
      "cos(" = "\\cos(",
      serial
    ))
  }

  ode_builder_clean_math_label(label, serial)
}

ode_builder_tokens_to_latex <- function(tokens, catalog) {
  if (length(tokens) == 0) return("")
  paste(vapply(tokens, ode_builder_token_latex, character(1), catalog = catalog),
        collapse = " ")
}

ode_builder_expression_math <- function(tokens, catalog) {
  latex <- ode_builder_tokens_to_latex(tokens, catalog)
  if (!nzchar(latex)) return("")
  ode_builder_math_inline(latex, "ode-builder-expression-math")
}

ode_builder_tokens_to_serial <- function(tokens) {
  if (length(tokens) == 0) return("")
  paste(vapply(tokens, function(t) t$serial %||% "", character(1)), collapse = " ")
}

ode_builder_append_token <- function(tokens, token) {
  token <- as.list(token)

  if (identical(token$kind, "number_digit")) {
    digit <- token$serial %||% ""

    if (length(tokens) > 0 && identical(tokens[[length(tokens)]]$kind, "number")) {
      last <- tokens[[length(tokens)]]
      if (identical(digit, ".") && grepl(".", last$serial, fixed = TRUE)) {
        return(tokens)
      }
      last$display <- paste0(last$display, digit)
      last$serial <- paste0(last$serial, digit)
      tokens[[length(tokens)]] <- last
      return(tokens)
    }

    if (identical(digit, ".")) {
      token$display <- "0."
      token$serial <- "0."
    }
    token$kind <- "number"
  }

  c(tokens, list(token))
}

ode_builder_valid_number <- function(x) {
  grepl("^-?(\\d+(\\.\\d*)?|\\.\\d+)([eE][+-]?\\d+)?$", x)
}

ode_builder_serial_index <- function(serial, prefix) {
  m <- regexec(paste0("^", prefix, "([0-9]+)$"), serial)
  hit <- regmatches(serial, m)[[1]]
  if (length(hit) < 2) return(NA_integer_)
  suppressWarnings(as.integer(hit[[2]]))
}

ode_builder_status_meta <- function(level) {
  switch(
    level,
    ok = list(icon = "\u2713", class = "ode-status-ok", label = "Looks valid"),
    warning = list(icon = "\u26a0", class = "ode-status-warning", label = "Possible issue"),
    error = list(icon = "\u2715", class = "ode-status-error", label = "Must fix"),
    info = list(icon = "\u24d8", class = "ode-status-info", label = "Info"),
    empty = list(icon = "\u25cb", class = "ode-status-empty", label = "Not started"),
    list(icon = "\u25cb", class = "ode-status-empty", label = "Not started")
  )
}

ode_builder_likely_cumulative_label <- function(label) {
  label <- tolower(trimws(label %||% ""))
  grepl("^(c|cum|cumul|cumulative|cases?|inc|incidence|y|z)$", label) ||
    grepl("cum|cumulative|cases|incidence", label)
}

ode_builder_rhs_has_negative_term <- function(tokens) {
  if (length(tokens) == 0) return(FALSE)
  serials <- vapply(tokens, function(t) t$serial %||% "", character(1))
  for (i in seq_along(serials)) {
    if (!identical(serials[[i]], "-")) next
    if (i == 1) next
    prev <- serials[[i - 1]]
    if (prev %in% c("(", "+", "-", "*", "/", "^")) next
    return(TRUE)
  }
  FALSE
}

ode_builder_extract_indices <- function(text, prefix) {
  if (length(text) == 0 || !nzchar(paste(text, collapse = ""))) return(integer(0))
  pattern <- paste0("\\b", prefix, "([0-9]+)\\b")
  hits <- gregexpr(pattern, text, perl = TRUE)
  pieces <- regmatches(text, hits)
  pieces <- unlist(pieces, use.names = FALSE)
  if (length(pieces) == 0) return(integer(0))
  suppressWarnings(as.integer(sub(pattern, "\\1", pieces, perl = TRUE)))
}

ode_builder_validate_equation <- function(tokens, catalog, n_states, n_params,
                                          tdp_names, state_label) {
  if (length(tokens) == 0) {
    return(list(level = "empty", blocking = TRUE, message = "Equation has not been started."))
  }

  for (token in tokens) {
    kind <- token$kind %||% ""
    serial <- token$serial %||% ""

    if (identical(kind, "state")) {
      idx <- ode_builder_serial_index(serial, "vars")
      if (is.na(idx) || idx < 1 || idx > n_states) {
        return(list(
          level = "error",
          blocking = TRUE,
          message = sprintf("State token %s is outside the available state variables.", serial)
        ))
      }
    }

    if (identical(kind, "param")) {
      idx <- ode_builder_serial_index(serial, "params")
      if (is.na(idx) || idx < 1 || idx > n_params) {
        return(list(
          level = "error",
          blocking = TRUE,
          message = sprintf("Parameter token %s is outside the available parameters.", serial)
        ))
      }
    }

    if (identical(kind, "tdp") && !(serial %in% tdp_names)) {
      return(list(
        level = "error",
        blocking = TRUE,
        message = sprintf("Time-dependent token %s is used, but its template is not complete.", serial)
      ))
    }

    if (kind %in% c("state", "param", "tdp")) {
      hit <- catalog[catalog$serial == serial & catalog$kind == kind, , drop = FALSE]
      if (NROW(hit) == 0) {
        return(list(
          level = "error",
          blocking = TRUE,
          message = sprintf("Token '%s' is no longer available. Clear or rebuild this equation.",
                            token$display %||% serial)
        ))
      }
    }
  }

  serial <- ode_builder_tokens_to_serial(tokens)
  parsed <- tryCatch(
    {
      parse(text = serial)
      TRUE
    },
    error = function(e) conditionMessage(e)
  )
  if (!isTRUE(parsed)) {
    return(list(
      level = "error",
      blocking = TRUE,
      message = paste0("Invalid token sequence: ", parsed)
    ))
  }

  if (ode_builder_likely_cumulative_label(state_label) &&
      ode_builder_rhs_has_negative_term(tokens)) {
    return(list(
      level = "warning",
      blocking = FALSE,
      message = "This looks like a cumulative/incidence state, but its equation contains a negative term."
    ))
  }

  list(level = "ok", blocking = FALSE, message = "Equation looks valid.")
}

ode_builder_duplicate_labels <- function(labels) {
  labels <- trimws(as.character(labels %||% character(0)))
  labels <- labels[nzchar(labels)]
  if (length(labels) == 0) return(character(0))
  dup <- duplicated(tolower(labels)) | duplicated(tolower(labels), fromLast = TRUE)
  unique(labels[dup])
}

ode_builder_model_checks <- function(n, tokens, statuses, states, params,
                                     tdp_value, fitting_diff) {
  checks <- list()
  add_check <- function(level, message) {
    checks[[length(checks) + 1]] <<- list(level = level, message = message)
  }

  state_labels <- states$display %||% character(0)
  param_labels <- params$display %||% character(0)
  tdp_tokens <- ode_builder_tdp_tokens(tdp_value)
  tdp_names <- as.character(tdp_tokens$serial %||% character(0))
  tdp_labels <- stats::setNames(as.character(tdp_tokens$display %||% character(0)), tdp_names)

  if (length(state_labels) == n && n > 0) {
    add_check("ok", sprintf("%d equations for %d state variables", n, length(state_labels)))
  } else {
    add_check("error", sprintf("Expected %d state variables, but %d are currently available",
                               n, length(state_labels)))
  }

  blocking <- vapply(statuses, function(x) isTRUE(x$blocking), logical(1))
  has_error <- vapply(statuses, function(x) identical(x$level, "error"), logical(1))
  has_empty <- vapply(statuses, function(x) identical(x$level, "empty"), logical(1))
  if (any(has_error)) {
    add_check("error", "One or more equations contain invalid or undefined tokens")
  } else if (any(has_empty)) {
    add_check("error", "One or more required equations are empty")
  } else if (!any(blocking)) {
    add_check("ok", "All ODE tokens are defined")
  }

  used_params <- unique(unlist(lapply(tokens, function(row) {
    vapply(row, function(token) {
      if (identical(token$kind, "param")) token$serial %||% "" else ""
    }, character(1))
  }), use.names = FALSE))
  used_params <- used_params[nzchar(used_params)]

  used_tdps <- unique(unlist(lapply(tokens, function(row) {
    vapply(row, function(token) {
      if (identical(token$kind, "tdp")) token$serial %||% "" else ""
    }, character(1))
  }), use.names = FALSE))
  used_tdps <- used_tdps[nzchar(used_tdps)]

  if (length(tdp_names) > 0) {
    missing_tdp <- setdiff(tdp_names, used_tdps)
    used_tdp <- intersect(tdp_names, used_tdps)
    if (length(used_tdp) > 0) {
      add_check("ok", sprintf("%s is used in the ODE",
                              paste(tdp_labels[used_tdp], collapse = ", ")))
    }
    if (length(missing_tdp) > 0) {
      add_check("warning", sprintf("%s is defined but not used in the ODE",
                                   paste(tdp_labels[missing_tdp], collapse = ", ")))
    }
  }

  state_dups <- ode_builder_duplicate_labels(state_labels)
  if (length(state_dups) > 0) {
    add_check("warning", sprintf("Duplicate state variable label%s: %s",
                                 if (length(state_dups) == 1) "" else "s",
                                 paste(state_dups, collapse = ", ")))
  }

  param_dups <- ode_builder_duplicate_labels(param_labels)
  if (length(param_dups) > 0) {
    add_check("warning", sprintf("Duplicate parameter label%s: %s",
                                 if (length(param_dups) == 1) "" else "s",
                                 paste(param_dups, collapse = ", ")))
  }

  params_in_templates <- ode_builder_extract_indices(tdp_value$code %||% "", "params")
  if (length(params_in_templates) > 0) {
    used_params <- unique(c(used_params, paste0("params", params_in_templates)))
  }
  if (length(params_in_templates) > 0) {
    outside <- params_in_templates[params_in_templates > NROW(params)]
    if (length(outside) > 0) {
      add_check("warning", sprintf("A time-dependent template references missing parameter token%s: %s",
                                   if (length(unique(outside)) == 1) "" else "s",
                                   paste(unique(paste0("params", outside)), collapse = ", ")))
    }
  }

  fit_vec <- suppressWarnings(as.integer(fitting_diff %||% integer(0)))
  if (length(fit_vec) > 0 && any(fit_vec == 1, na.rm = TRUE)) {
    has_cumulative <- any(vapply(state_labels, ode_builder_likely_cumulative_label,
                                 logical(1)))
    if (!has_cumulative) {
      add_check("warning", "fitting_diff = 1 but a cumulative/incidence state was not clearly identified")
    }
  }

  if (NROW(params) > 0) {
    all_params <- as.character(params$serial)
    unused <- setdiff(all_params, used_params)
    if (length(unused) > 0) {
      add_check("info", sprintf("%d scalar parameter%s appear%s unused in the ODE",
                                length(unused),
                                if (length(unused) == 1) "" else "s",
                                if (length(unused) == 1) "s" else ""))
    }
  }

  checks
}

# Expand serialized builder equations into internal-parameter names for the
# existing check.IC() helper. This remains parser-free for ODEs; it simply maps
# controlled placeholders back to the internal parameter names that check.IC()
# already expects.
ode_builder_expand_rhs_for_ic <- function(rhs, params, tdp_specs = list()) {
  out <- rhs

  if (length(tdp_specs) > 0) {
    ord <- order(vapply(tdp_specs, function(s) nchar(s$internal_name %||% ""),
                        integer(1)), decreasing = TRUE)
    for (idx in ord) {
      spec <- tdp_specs[[idx]]
      internal <- spec$internal_name %||% ""
      if (!nzchar(internal)) next
      named_body <- tryCatch(tdp_render_body_named(spec), error = function(e) internal)
      out <- gsub(internal, paste0("(", named_body, ")"), out, fixed = TRUE)
    }
  }

  if (!is.null(params) && length(params) > 0) {
    for (idx in rev(seq_along(params))) {
      out <- gsub(paste0("params", idx), params[[idx]], out, fixed = TRUE)
    }
  }

  out
}

#------------------------------------------------------------------------------#
# UI ---------------------------------------------------------------------------
#------------------------------------------------------------------------------#

odeBuilderUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      .ode-builder-root .ode-builder-help {
        margin-bottom: 8px;
      }
      .ode-builder-root .ode-builder-layout {
        margin-top: 4px;
      }
      .ode-builder-root .ode-builder-card {
        margin: 8px 0;
        padding: 10px;
        border: 1px solid #ddd;
        border-left: 5px solid #ddd;
        background-color: #f7f7f7;
        min-height: 58px;
        cursor: default;
        transition: border-color 0.12s ease, box-shadow 0.12s ease, background-color 0.12s ease;
      }
      .ode-builder-root .ode-builder-card-active {
        border-color: #337ab7;
        border-left-color: #337ab7;
        background-color: #f0f7fd;
        box-shadow: 0 1px 5px rgba(51, 122, 183, 0.25);
      }
      .ode-builder-root .ode-builder-card-head {
        display: flex;
        align-items: center;
        gap: 6px;
        flex-wrap: wrap;
      }
      .ode-builder-root .ode-builder-derivative {
        font-weight: bold;
        margin-right: 4px;
      }
      .ode-builder-root .ode-builder-expression {
        margin-top: 6px;
        padding: 8px;
        min-height: 36px;
        background-color: #fff;
        border: 1px dashed #ccc;
        font-size: 16px;
        word-break: break-word;
      }
      .ode-builder-root .ode-builder-expression-empty {
        color: #999;
        font-size: 13px;
      }
      .ode-builder-root .ode-builder-message {
        margin-top: 5px;
        font-size: 12px;
      }
      .ode-builder-root .ode-builder-keypad {
        margin-top: 8px;
        padding: 10px;
        border: 1px solid #ddd;
        background-color: #f4f4f4;
      }
      .ode-builder-root .ode-builder-keypad-title {
        font-weight: bold;
        margin-bottom: 4px;
      }
      .ode-builder-root .ode-builder-active-note {
        font-size: 12px;
        color: #666;
        margin-bottom: 8px;
      }
      .ode-builder-root .ode-token-group {
        margin-top: 9px;
      }
      .ode-builder-root .ode-token-group-title {
        font-size: 12px;
        font-weight: bold;
        color: #555;
        margin-bottom: 2px;
      }
      .ode-builder-root .ode-token-btn,
      .ode-builder-root .ode-card-action {
        margin: 2px 3px 2px 0;
      }
      .ode-builder-root .ode-token-state {
        border-color: #5bc0de;
      }
      .ode-builder-root .ode-token-param {
        border-color: #5cb85c;
      }
      .ode-builder-root .ode-token-tdp {
        border-color: #f0ad4e;
      }
      .ode-builder-root .ode-token-number {
        border-color: #337ab7;
      }
      .ode-builder-root .ode-token-empty {
        display: inline-block;
        font-size: 12px;
        color: #777;
        margin-top: 2px;
      }
      .ode-builder-root .ode-math-inline {
        display: inline-block;
      }
      .ode-builder-root .ode-token-math {
        min-width: 12px;
      }
      .ode-builder-root .ode-token-btn .MathJax,
      .ode-builder-root .ode-token-btn mjx-container {
        pointer-events: none;
      }
      .ode-builder-root .ode-builder-expression-math {
        font-size: 17px;
      }
      .ode-builder-root .ode-status-icon {
        display: inline-block;
        width: 20px;
        height: 20px;
        border-radius: 50%;
        text-align: center;
        line-height: 18px;
        font-weight: bold;
        border: 1px solid transparent;
      }
      .ode-builder-root .ode-status-ok {
        color: #2e7d32;
        background-color: #e8f5e9;
        border-color: #a5d6a7;
      }
      .ode-builder-root .ode-status-warning {
        color: #8a6d3b;
        background-color: #fcf8e3;
        border-color: #faebcc;
      }
      .ode-builder-root .ode-status-error {
        color: #a94442;
        background-color: #f2dede;
        border-color: #ebccd1;
      }
      .ode-builder-root .ode-status-info {
        color: #31708f;
        background-color: #d9edf7;
        border-color: #bce8f1;
      }
      .ode-builder-root .ode-status-empty {
        color: #777;
        background-color: #eee;
        border-color: #ddd;
      }
      .ode-builder-root .ode-builder-check-panel {
        margin: 12px 0 8px 0;
        padding: 10px;
        border: 1px solid #ddd;
        background-color: #fff;
      }
      .ode-builder-root .ode-builder-check-title {
        font-weight: bold;
        margin-bottom: 6px;
      }
      .ode-builder-root .ode-builder-check-row {
        display: flex;
        align-items: flex-start;
        gap: 6px;
        font-size: 13px;
        margin: 3px 0;
      }
      .ode-builder-root .ode-builder-dev-preview {
        margin: 10px 0;
        font-size: 12px;
      }
      .ode-builder-root .ode-builder-dev-preview summary {
        cursor: pointer;
        color: #337ab7;
      }
      .ode-builder-root .ode-builder-serialized {
        font-family: monospace;
        font-size: 12px;
        margin-top: 6px;
        white-space: pre-wrap;
        word-break: break-word;
        background-color: #f7f7f7;
        border: 1px solid #ddd;
        padding: 8px;
      }
      .ode-builder-root .ode-builder-error {
        margin-bottom: 8px;
      }
      @media (min-width: 992px) {
        .ode-builder-root .ode-builder-keypad {
          position: sticky;
          top: 8px;
        }
      }
    ")),
    ode_builder_typeset_script(ns),
    div(
      id = ns("root"),
      class = "ode-builder-root",
      div(
        class = "ode-builder-help",
        helpText(
          "Build each ODE by selecting an equation card, then clicking tokens ",
          "from the shared keypad. The app stores exact internal tokens behind ",
          "the scenes, so ODE equations no longer depend on LaTeX parsing."
        )
      ),
      uiOutput(ns("container"))
    )
  )
}

#------------------------------------------------------------------------------#
# Server -----------------------------------------------------------------------
#------------------------------------------------------------------------------#

odeBuilderServer <- function(id, n_equations, states_reactive,
                             params_reactive, tdp_reactive,
                             fitting_diff_reactive = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    token_state <- reactiveValues()
    active_equation <- reactiveVal(1L)
    last_error <- reactiveVal(NULL)

    current_catalog <- reactive({
      state_tokens <- ode_builder_named_vector_to_tokens(states_reactive(), "state")
      param_tokens <- ode_builder_named_vector_to_tokens(params_reactive(), "param")
      tdp_tokens <- tryCatch(ode_builder_tdp_tokens(tdp_reactive()),
                             error = function(e) ode_builder_empty_catalog())
      rbind(
        state_tokens,
        param_tokens,
        tdp_tokens,
        ode_builder_operator_tokens()
      )
    })

    current_tokens <- reactive({
      n <- n_equations()
      if (is.null(n) || is.na(n) || n < 1) return(list())
      lapply(seq_len(as.integer(n)), function(i) ode_builder_get_tokens(token_state, i))
    })

    equation_statuses <- reactive({
      n <- n_equations()
      if (is.null(n) || is.na(n) || n < 1) return(list())

      n <- as.integer(n)
      catalog <- current_catalog()
      states <- catalog[catalog$kind == "state", , drop = FALSE]
      params <- catalog[catalog$kind == "param", , drop = FALSE]
      tdps <- catalog[catalog$kind == "tdp", , drop = FALSE]
      tokens <- current_tokens()

      lapply(seq_len(n), function(i) {
        state_label <- if (NROW(states) >= i) states$display[[i]] else paste0("var", i)
        ode_builder_validate_equation(
          tokens[[i]],
          catalog = catalog,
          n_states = NROW(states),
          n_params = NROW(params),
          tdp_names = as.character(tdps$serial %||% character(0)),
          state_label = state_label
        )
      })
    })

    model_checks <- reactive({
      n <- n_equations()
      if (is.null(n) || is.na(n) || n < 1) return(list())
      catalog <- current_catalog()
      states <- catalog[catalog$kind == "state", , drop = FALSE]
      params <- catalog[catalog$kind == "param", , drop = FALSE]
      tdp_value <- tryCatch(tdp_reactive(), error = function(e) list())
      fit_vec <- tryCatch(fitting_diff_reactive(), error = function(e) NULL)
      ode_builder_model_checks(
        n = as.integer(n),
        tokens = current_tokens(),
        statuses = equation_statuses(),
        states = states,
        params = params,
        tdp_value = tdp_value,
        fitting_diff = fit_vec
      )
    })

    observe({
      n <- n_equations()
      if (is.null(n) || is.na(n) || n < 1) {
        active_equation(1L)
        return(NULL)
      }
      n <- as.integer(n)
      current <- active_equation()
      if (is.null(current) || is.na(current) || current < 1) {
        active_equation(1L)
      } else if (current > n) {
        active_equation(n)
      }
    })

    build_card_ui <- function(i, catalog, statuses, active) {
      states <- catalog[catalog$kind == "state", , drop = FALSE]
      state_label <- if (NROW(states) >= i) states$display[[i]] else paste0("var", i)
      tokens <- ode_builder_get_tokens(token_state, i)
      display <- ode_builder_tokens_to_display(tokens, catalog)
      status <- statuses[[i]]
      meta <- ode_builder_status_meta(status$level)

      div(
        id = ns(paste0("odeCard_", i)),
        class = paste("ode-builder-card",
                      if (identical(as.integer(active), as.integer(i))) "ode-builder-card-active" else ""),
        div(
          class = "ode-builder-card-head",
          tags$span(
            class = paste("ode-status-icon", meta$class),
            title = meta$label,
            meta$icon
          ),
          tags$span(class = "ode-builder-derivative",
                    ode_builder_math_inline(paste0("d", state_label, "/dt ="))),
          if (identical(as.integer(active), as.integer(i))) {
            tags$span(class = "label label-primary", "Active")
          } else {
            ode_builder_select_button(ns, i, "Edit/select")
          },
          ode_builder_edit_button(ns, "backspace", "Backspace", row = i),
          ode_builder_edit_button(ns, "clear", "Clear", "btn-warning", row = i)
        ),
        tags$div(
          class = "ode-builder-expression",
          if (nzchar(display)) ode_builder_expression_math(tokens, catalog) else tags$span(
            class = "ode-builder-expression-empty",
            "Select this equation and use the keypad to build its right-hand side."
          )
        ),
        if (!identical(status$level, "ok") && nzchar(status$message %||% "")) {
          tags$div(class = paste("ode-builder-message", meta$class), status$message)
        }
      )
    }

    build_keypad_ui <- function(catalog, active) {
      states <- catalog[catalog$kind == "state", , drop = FALSE]
      state_buttons <- lapply(seq_len(NROW(states)), function(j) {
        ode_builder_token_button(ns, states[j, ], "ode-token-state")
      })

      params <- catalog[catalog$kind == "param", , drop = FALSE]
      param_buttons <- lapply(seq_len(NROW(params)), function(j) {
        ode_builder_token_button(ns, params[j, ], "ode-token-param")
      })

      tdps <- catalog[catalog$kind == "tdp", , drop = FALSE]
      tdp_buttons <- lapply(seq_len(NROW(tdps)), function(j) {
        ode_builder_token_button(ns, tdps[j, ], "ode-token-tdp")
      })

      ops <- ode_builder_operator_tokens()
      op_buttons <- lapply(seq_len(NROW(ops)), function(j) {
        ode_builder_token_button(ns, ops[j, ])
      })

      digits <- ode_builder_digit_tokens()
      digit_buttons <- lapply(seq_len(NROW(digits)), function(j) {
        ode_builder_token_button(ns, digits[j, ], "ode-token-number")
      })

      active_label <- {
        state_rows <- catalog[catalog$kind == "state", , drop = FALSE]
        if (NROW(state_rows) >= active) state_rows$display[[active]] else paste0("Equation ", active)
      }

      div(
        class = "ode-builder-keypad",
        div(class = "ode-builder-keypad-title", "Token keypad"),
        div(class = "ode-builder-active-note",
            "Inserting into ",
            ode_builder_math_inline(paste0("d", active_label, "/dt")),
            "."),
        div(
          ode_builder_edit_button(ns, "backspace", "Backspace"),
          ode_builder_edit_button(ns, "clear", "Clear", "btn-warning")
        ),
        ode_builder_button_group(
          "State variables",
          state_buttons,
          "State-variable buttons appear after Variable Specification is complete."
        ),
        ode_builder_button_group(
          "Scalar parameters",
          param_buttons,
          "Parameter buttons appear after Parameter Specification is complete."
        ),
        ode_builder_button_group(
          "Time-dependent parameters",
          tdp_buttons,
          "Complete a time-dependent parameter template to make it available here."
        ),
        ode_builder_button_group("Operators and functions", op_buttons),
        ode_builder_button_group(
          "Numbers",
          c(
            digit_buttons,
            list(
              tags$span(style = "display:inline-block; width: 120px; margin-left: 4px;",
                        textInput(ns("number_active"), label = NULL,
                                  value = "", placeholder = "e.g. 0.5")),
              ode_builder_number_button(ns)
            )
          )
        )
      )
    }

    build_status_panel_ui <- function(checks) {
      if (length(checks) == 0) {
        return(NULL)
      }
      rows <- lapply(checks, function(check) {
        meta <- ode_builder_status_meta(check$level)
        div(
          class = "ode-builder-check-row",
          tags$span(class = paste("ode-status-icon", meta$class), meta$icon),
          tags$span(check$message)
        )
      })
      div(
        class = "ode-builder-check-panel",
        div(class = "ode-builder-check-title", "Model checks"),
        rows
      )
    }

    build_developer_preview_ui <- function(n, catalog) {
      tokens <- current_tokens()
      rhs <- vapply(tokens, ode_builder_tokens_to_serial, character(1))
      display <- vapply(tokens, ode_builder_tokens_to_display, character(1), catalog = catalog)
      lines <- paste0("diff_var", seq_len(n), " = ", rhs)
      pretty <- paste0("d", {
        states <- catalog[catalog$kind == "state", , drop = FALSE]
        labels <- if (NROW(states) >= n) states$display[seq_len(n)] else paste0("var", seq_len(n))
        labels
      }, "/dt = ", ifelse(nzchar(display), display, "(empty)"))

      tags$details(
        class = "ode-builder-dev-preview",
        tags$summary("Developer preview: show internal ODE code"),
        tags$div(class = "ode-builder-serialized",
                 paste(c(pretty, "", lines), collapse = "\n"))
      )
    }

    output$container <- renderUI({
      n <- n_equations()
      if (is.null(n) || is.na(n) || n < 1) {
        return(helpText("Set '# State Variables' to add ODE rows."))
      }
      n <- as.integer(n)
      catalog <- current_catalog()
      statuses <- equation_statuses()
      active <- active_equation()
      err <- last_error()

      fluidRow(
        class = "ode-builder-layout",
        column(
          7,
          if (!is.null(err) && nzchar(err)) {
            div(class = "alert alert-warning ode-builder-error", err)
          },
          do.call(tagList, lapply(seq_len(n), build_card_ui,
                                  catalog = catalog,
                                  statuses = statuses,
                                  active = active)),
          build_status_panel_ui(model_checks()),
          build_developer_preview_ui(n, catalog)
        ),
        column(
          5,
          build_keypad_ui(catalog, active)
        )
      )
    })

    observeEvent(input$select_row, {
      click <- input$select_row
      row <- suppressWarnings(as.integer(click$row))
      n <- suppressWarnings(as.integer(n_equations()))
      if (is.na(row) || row < 1 || is.na(n) || n < 1 || row > n) return(NULL)
      active_equation(row)
      last_error(NULL)
    })

    observeEvent(input$token_click, {
      click <- input$token_click
      row <- suppressWarnings(as.integer(active_equation()))
      n <- suppressWarnings(as.integer(n_equations()))
      if (is.na(row) || row < 1 || is.na(n) || row > n) return(NULL)
      token <- list(
        display = click$display %||% "",
        serial = click$serial %||% "",
        kind = click$kind %||% "operator"
      )
      tokens <- ode_builder_get_tokens(token_state, row)
      ode_builder_set_tokens(token_state, row, ode_builder_append_token(tokens, token))
      last_error(NULL)
    })

    observeEvent(input$number_click, {
      click <- input$number_click
      row <- suppressWarnings(as.integer(active_equation()))
      n <- suppressWarnings(as.integer(n_equations()))
      value <- trimws(as.character(click$value %||% ""))
      if (is.na(row) || row < 1 || is.na(n) || row > n) return(NULL)
      if (!ode_builder_valid_number(value)) {
        last_error("Please enter a valid number, e.g. 0.5, 12, or 1e-3.")
        return(NULL)
      }
      tokens <- ode_builder_get_tokens(token_state, row)
      token <- list(display = value, serial = value, kind = "number")
      ode_builder_set_tokens(token_state, row, c(tokens, list(token)))
      last_error(NULL)
    })

    observeEvent(input$edit_click, {
      click <- input$edit_click
      row <- suppressWarnings(as.integer(click$row %||% active_equation()))
      n <- suppressWarnings(as.integer(n_equations()))
      action <- as.character(click$action %||% "")
      if (is.na(row) || row < 1 || is.na(n) || row > n) return(NULL)
      active_equation(row)
      tokens <- ode_builder_get_tokens(token_state, row)
      if (identical(action, "clear")) {
        tokens <- list()
      } else if (identical(action, "backspace") && length(tokens) > 0) {
        tokens <- tokens[-length(tokens)]
      }
      ode_builder_set_tokens(token_state, row, tokens)
      last_error(NULL)
    })

    reactive({
      n <- n_equations()
      if (is.null(n) || is.na(n) || n < 1) {
        return(list(
          valid = FALSE,
          errors = "No ODE rows are available.",
          warnings = character(0),
          info = character(0),
          rhs = character(0),
          display = character(0),
          lines = character(0),
          ode_system = NULL,
          statuses = list(),
          checks = list()
        ))
      }

      n <- as.integer(n)
      catalog <- current_catalog()
      tokens <- current_tokens()
      rhs <- vapply(tokens, ode_builder_tokens_to_serial, character(1))
      display <- vapply(tokens, ode_builder_tokens_to_display, character(1), catalog = catalog)
      statuses <- equation_statuses()
      checks <- model_checks()

      status_errors <- unlist(lapply(seq_along(statuses), function(i) {
        status <- statuses[[i]]
        if (isTRUE(status$blocking)) {
          paste0("Equation ", i, ": ", status$message)
        } else {
          NULL
        }
      }), use.names = FALSE)

      check_errors <- unlist(lapply(checks, function(check) {
        if (identical(check$level, "error")) check$message else NULL
      }), use.names = FALSE)

      warnings <- unlist(lapply(c(statuses, checks), function(item) {
        if (identical(item$level, "warning")) item$message else NULL
      }), use.names = FALSE)

      info <- unlist(lapply(checks, function(check) {
        if (identical(check$level, "info")) check$message else NULL
      }), use.names = FALSE)

      errors <- unique(c(status_errors, check_errors))
      lines <- paste0("diff_var", seq_len(n), " = ", rhs)

      list(
        valid = length(errors) == 0,
        errors = errors,
        warnings = unique(warnings),
        info = unique(info),
        rhs = rhs,
        display = display,
        lines = lines,
        ode_system = paste0("'\n", paste(lines, collapse = "\n"), "'"),
        statuses = statuses,
        checks = checks
      )
    })
  })
}
