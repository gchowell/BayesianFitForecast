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

ode_builder_token_row <- function(label, display, serial, kind, key = serial) {
  data.frame(
    label = as.character(label),
    display = as.character(display),
    serial = as.character(serial),
    kind = as.character(kind),
    key = as.character(key),
    ic = FALSE,
    stringsAsFactors = FALSE
  )
}

# Catalog rows for state variables / parameters.
#
# `x` is either
#   * a named character vector: names = display labels, values = serials
#     ("vars1", "params2", ...); the serial doubles as the stable key, or
#   * a data.frame with the columns `key` (stable identity of the row, e.g. the
#     id of the form row) and `display` (current name, may still be empty). The
#     positional serial (varsN / paramsN) is derived from the row order, so the
#     tokens stored in the equations keep pointing at the right variable when
#     rows are added, removed or renamed.
ode_builder_named_vector_to_tokens <- function(x, kind) {
  empty <- ode_builder_empty_catalog()
  if (is.null(x) || length(x) == 0 || NROW(x) == 0) return(empty)

  prefix <- switch(kind, state = "vars", param = "params", "")

  if (is.data.frame(x)) {
    n <- NROW(x)
    display <- trimws(as.character(x$display %||% rep("", n)))
    display[is.na(display)] <- ""
    key <- as.character(x$key %||% paste0(prefix, seq_len(n)))
    serial <- if ("serial" %in% names(x)) as.character(x$serial) else paste0(prefix, seq_len(n))
    out <- data.frame(label = display, display = display, serial = serial,
                      kind = rep(kind, n), key = key, ic = FALSE, stringsAsFactors = FALSE)
    # parameters that appear in the initial conditions are not "unused" when absent from the ODE
    if ("ic" %in% names(x)) out$ic <- as.logical(x$ic) %in% TRUE
    return(out)
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
    key = serial[keep],
    ic = FALSE,
    stringsAsFactors = FALSE
  )
}

# Stable keys of the state rows (equations are stored per state key)
ode_builder_state_keys <- function(x) {
  if (is.null(x) || length(x) == 0 || NROW(x) == 0) return(character(0))
  if (is.data.frame(x)) return(as.character(x$key %||% paste0("vars", seq_len(NROW(x)))))
  as.character(unname(x))
}

#------------------------------------------------------------------------------#
# Token resolution ---------------------------------------------------------------
#------------------------------------------------------------------------------#
# Tokens of state variables and parameters remember the stable `key` of the form #
# row they were taken from. Before an equation is displayed, validated or        #
# serialized, every such token is refreshed from the current catalog: its        #
# positional serial (varsN / paramsN) and its display label follow the row, so   #
# removing or renaming a row never silently changes the meaning of an equation.  #
# A token whose row no longer exists is flagged as orphan (reported as an error).#
#------------------------------------------------------------------------------#
ode_builder_resolve_token <- function(token, catalog) {
  token <- as.list(token)
  kind <- token$kind %||% ""
  key <- token$key %||% NULL
  if (!(kind %in% c("state", "param")) || is.null(key) || !nzchar(key) || NROW(catalog) == 0) {
    return(token)
  }
  hit <- catalog[catalog$kind == kind & catalog$key == key, , drop = FALSE]
  if (NROW(hit) == 0) {
    # legacy tokens (created before keys existed) carry their serial as key
    hit <- catalog[catalog$kind == kind & catalog$serial == key, , drop = FALSE]
  }
  if (NROW(hit) == 0) {
    token$orphan <- TRUE
    # the stale positional serial could now belong to another row: neutralize it
    token$serial <- paste0("missing_", key)
    return(token)
  }
  token$orphan <- FALSE
  token$serial <- hit$serial[[1]]
  if (nzchar(hit$display[[1]])) token$display <- hit$display[[1]]
  token
}

ode_builder_resolve_tokens <- function(tokens, catalog) {
  if (length(tokens) == 0) return(list())
  lapply(tokens, ode_builder_resolve_token, catalog = catalog)
}

#------------------------------------------------------------------------------#
# Text -> tokens (used by the model library and by presets) ----------------------
#------------------------------------------------------------------------------#
# `text` is an expression written with internal names, e.g. "beta*S*I/N -       #
# gamma*I". `lookup` is a data.frame with the columns name (internal name used   #
# in the text), key (stable row key), kind ("state" / "param" / "tdp") and       #
# display (label shown in the builder). Explicit '*' between two operands are   #
# dropped because the builder multiplies adjacent operands implicitly (the      #
# serialized code always contains the '*' again).                               #
#------------------------------------------------------------------------------#
ode_builder_tokenize_text <- function(text, lookup) {
  text <- trimws(as.character(text %||% ""))
  if (!nzchar(text)) return(list())
  functions <- c("exp", "log", "sqrt", "sin", "cos")
  pattern <- "\\s*([A-Za-z_][A-Za-z0-9_]*\\s*\\(|[A-Za-z_][A-Za-z0-9_]*|[0-9]+\\.?[0-9]*(?:[eE][+-]?[0-9]+)?|\\.[0-9]+(?:[eE][+-]?[0-9]+)?|\\^|\\*|/|\\+|-|\\(|\\))"
  pos <- 1L
  tokens <- list()
  while (pos <= nchar(text)) {
    rest <- substr(text, pos, nchar(text))
    if (!nzchar(trimws(rest))) break
    m <- regmatches(rest, regexpr(paste0("^", pattern), rest, perl = TRUE))
    if (length(m) == 0 || !nzchar(m)) {
      stop(sprintf("Cannot read the expression near '%s'", substr(rest, 1, 12)))
    }
    piece <- trimws(m)
    pos <- pos + nchar(m)
    if (grepl("\\($", piece) && nchar(piece) > 1) {
      fname <- trimws(sub("\\s*\\($", "", piece))
      if (!(fname %in% functions)) stop(sprintf("Unknown function '%s' in '%s'", fname, text))
      tokens[[length(tokens) + 1]] <- list(display = paste0(fname, "("), serial = paste0(fname, "("), kind = "operator")
    } else if (grepl("^[0-9.]", piece)) {
      tokens[[length(tokens) + 1]] <- list(display = piece, serial = piece, kind = "number")
    } else if (piece %in% c("+", "-", "/", "^", "(", ")")) {
      tokens[[length(tokens) + 1]] <- list(display = piece, serial = piece, kind = "operator")
    } else if (identical(piece, "*")) {
      # implicit multiplication: only keep '*' when it is needed for parsing
      next
    } else if (identical(piece, "t")) {
      tokens[[length(tokens) + 1]] <- list(display = "t", serial = "t", kind = "time")
    } else {
      hit <- lookup[lookup$name == piece, , drop = FALSE]
      if (NROW(hit) == 0) stop(sprintf("Unknown symbol '%s' in '%s'", piece, text))
      tokens[[length(tokens) + 1]] <- list(display = as.character(hit$display[[1]]),
                                           serial = if ("serial" %in% names(hit)) as.character(hit$serial[[1]]) else "",
                                           kind = as.character(hit$kind[[1]]),
                                           key = as.character(hit$key[[1]]))
    }
  }
  tokens
}

ode_builder_operator_tokens <- function() {
  serial <- c("+", "-", "*", "/", "(", ")", "^",
              "exp(", "log(", "sqrt(", "sin(", "cos(", "t")
  data.frame(
    label = c("+", "-", "*", "/", "(", ")", "^",
              "exp(", "log(", "sqrt(", "sin(", "cos(", "t"),
    display = c("+", "-", "\u00d7", "/", "(", ")", "^",
                "exp(", "log(", "sqrt(", "sin(", "cos(", "t"),
    serial = serial,
    kind = c(rep("operator", 12), "time"),
    key = serial,
    ic = FALSE,
    stringsAsFactors = FALSE
  )
}

ode_builder_digit_tokens <- function() {
  data.frame(
    label = c(as.character(0:9), "."),
    display = c(as.character(0:9), "."),
    serial = c(as.character(0:9), "."),
    kind = rep("number_digit", 11),
    key = c(as.character(0:9), "."),
    ic = FALSE,
    stringsAsFactors = FALSE
  )
}

ode_builder_empty_catalog <- function() {
  data.frame(label = character(0), display = character(0),
             serial = character(0), kind = character(0), key = character(0),
             ic = logical(0), stringsAsFactors = FALSE)
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
    ",key:", ode_builder_json(if (!is.null(token$key)) token$key[[1]] else (token$serial[[1]] %||% "")),
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
    if (NROW(hit) > 0 && nzchar(hit$display[[1]])) return(hit$display[[1]])
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
      "*" = "\\cdot",
      "exp(" = "\\exp(",
      "log(" = "\\log(",
      "sqrt(" = "\\operatorname{sqrt}(",
      "sin(" = "\\sin(",
      "cos(" = "\\cos(",
      serial
    ))
  }
  if (identical(kind, "number")) return(ode_builder_number_latex(serial))

  ode_builder_clean_math_label(label, serial)
}

# Plain inline rendering (fallback while an expression is still incomplete)
ode_builder_tokens_to_latex <- function(tokens, catalog) {
  if (length(tokens) == 0) return("")
  paste(vapply(tokens, ode_builder_token_latex, character(1), catalog = catalog),
        collapse = " ")
}

#------------------------------------------------------------------------------#
# Implicit multiplication ---------------------------------------------------------
#------------------------------------------------------------------------------#
# Two operands written next to each other (e.g. beta S I) are multiplied, as in #
# ordinary mathematical notation. The '*' key still works. The serialized code  #
# always contains explicit '*' so that Stan / R evaluate exactly what is shown. #
#------------------------------------------------------------------------------#

ode_builder_is_operand_end <- function(tok) {
  kind <- tok$kind %||% ""
  kind %in% c("state", "param", "tdp", "time", "number") ||
    (identical(kind, "operator") && identical(tok$serial %||% "", ")"))
}

ode_builder_is_operand_start <- function(tok) {
  kind <- tok$kind %||% ""
  serial <- tok$serial %||% ""
  kind %in% c("state", "param", "tdp", "time", "number") ||
    (identical(kind, "operator") && (identical(serial, "(") || grepl("\\($", serial)))
}

# Insert the implicit '*' tokens
ode_builder_normalize_tokens <- function(tokens) {
  out <- list()
  for (tok in tokens) {
    if (length(out) > 0 && ode_builder_is_operand_end(out[[length(out)]]) && ode_builder_is_operand_start(tok)) {
      out[[length(out) + 1]] <- list(display = "*", serial = "*", kind = "operator", implicit = TRUE)
    }
    out[[length(out) + 1]] <- tok
  }
  out
}

# Numbers are written as real literals (2 -> 2.0) so that Stan never performs an
# integer division such as 1 / 2 = 0.
ode_builder_number_serial <- function(x) {
  x <- as.character(x)
  if (grepl("^[0-9]+$", x)) paste0(x, ".0") else if (grepl("^\\.", x)) paste0("0", x) else x
}

ode_builder_number_latex <- function(x) {
  x <- as.character(x)
  x <- sub("^\\.", "0.", x)
  x <- sub("\\.$", "", x)
  x
}

# Code string with explicit multiplication (used for the options file / Stan)
ode_builder_tokens_to_serial <- function(tokens) {
  if (length(tokens) == 0) return("")
  toks <- ode_builder_normalize_tokens(tokens)
  paste(vapply(toks, function(t) {
    if (identical(t$kind %||% "", "number")) ode_builder_number_serial(t$serial %||% "") else t$serial %||% ""
  }, character(1)), collapse = " ")
}

#------------------------------------------------------------------------------#
# Expression parser (for the mathematical rendering and friendly messages) ------
#------------------------------------------------------------------------------#
# Precedence, from low to high: + -  |  * / (and implicit multiplication,       #
# left-associative)  |  unary -  |  ^ (right-associative). This is exactly the  #
# precedence used by Stan and R for the serialized code, so what is displayed   #
# is what is computed.                                                          #
#------------------------------------------------------------------------------#

ode_builder_parse_tokens <- function(tokens) {
  toks <- ode_builder_normalize_tokens(tokens)
  n <- length(toks)
  pos <- 1L

  fail <- function(msg) stop(structure(list(message = msg, call = NULL),
                                       class = c("ode_parse_error", "error", "condition")))
  peek <- function() if (pos <= n) toks[[pos]] else NULL
  is_op <- function(tok, s) !is.null(tok) && identical(tok$kind %||% "", "operator") && identical(tok$serial %||% "", s)
  tok_label <- function(tok) tok$display %||% tok$serial %||% "?"

  parse_expr <- function() {
    node <- parse_term()
    repeat {
      tok <- peek()
      if (is_op(tok, "+") || is_op(tok, "-")) {
        pos <<- pos + 1L
        rhs <- parse_term()
        node <- list(type = "bin", op = tok$serial, l = node, r = rhs)
      } else break
    }
    node
  }
  parse_term <- function() {
    node <- parse_unary()
    repeat {
      tok <- peek()
      if (is_op(tok, "*") || is_op(tok, "/")) {
        pos <<- pos + 1L
        rhs <- parse_unary()
        node <- list(type = "bin", op = tok$serial, l = node, r = rhs, implicit = isTRUE(tok$implicit))
      } else break
    }
    node
  }
  parse_unary <- function() {
    tok <- peek()
    if (is_op(tok, "-") || is_op(tok, "+")) {
      pos <<- pos + 1L
      operand <- parse_unary()
      return(list(type = "unary", op = tok$serial, e = operand))
    }
    parse_power()
  }
  parse_power <- function() {
    base <- parse_atom()
    tok <- peek()
    if (is_op(tok, "^")) {
      pos <<- pos + 1L
      ex <- parse_unary()
      return(list(type = "pow", base = base, exp = ex))
    }
    base
  }
  parse_atom <- function() {
    tok <- peek()
    if (is.null(tok)) fail("the expression is incomplete (an operand is missing at the end)")
    kind <- tok$kind %||% ""
    serial <- tok$serial %||% ""
    if (kind %in% c("state", "param", "tdp", "time")) { pos <<- pos + 1L; return(list(type = "sym", token = tok)) }
    if (identical(kind, "number")) { pos <<- pos + 1L; return(list(type = "num", value = serial)) }
    if (is_op(tok, "(")) {
      pos <<- pos + 1L
      inner <- parse_expr()
      if (!is_op(peek(), ")")) fail("a closing parenthesis ')' is missing")
      pos <<- pos + 1L
      return(list(type = "paren", e = inner))
    }
    if (identical(kind, "operator") && nchar(serial) > 1 && grepl("\\($", serial)) {
      fname <- sub("\\($", "", serial)
      pos <<- pos + 1L
      inner <- parse_expr()
      if (!is_op(peek(), ")")) fail(sprintf("the closing parenthesis of %s( ) is missing", fname))
      pos <<- pos + 1L
      return(list(type = "func", name = fname, e = inner))
    }
    if (is_op(tok, ")")) fail("there is a ')' without a matching '('")
    fail(sprintf("'%s' cannot be used here (an operand is expected)", tok_label(tok)))
  }

  node <- parse_expr()
  if (pos <= n) fail(sprintf("'%s' cannot follow a complete expression", tok_label(peek())))
  node
}

ode_builder_symbol_latex <- function(token, catalog) {
  label <- ode_builder_clean_math_label(ode_builder_token_display(token, catalog), token$serial %||% "")
  # plain multi-letter names (e.g. Ih, tint) are set upright-ish so they read as one symbol
  if (grepl("^[A-Za-z][A-Za-z0-9]+$", label) && !grepl("\\\\", label)) paste0("\\mathit{", label, "}") else label
}

# compact = TRUE writes fractions with a slash (a/b) and plain parentheses, for a
# one-line rendering (model library cards); the default uses \frac and \left( \right).
ode_builder_ast_latex <- function(node, catalog, compact = FALSE) {
  rec <- function(x) ode_builder_ast_latex(x, catalog, compact = compact)
  # Parentheses are redundant when a fraction bar, a radical or a superscript
  # already groups the expression.
  grouped <- function(x) if (identical(x$type, "paren")) rec(x$e) else rec(x)
  lp <- if (compact) "(" else "\\left("
  rp <- if (compact) ")" else "\\right)"
  switch(node$type,
    sym   = ode_builder_symbol_latex(node$token, catalog),
    num   = ode_builder_number_latex(node$value),
    paren = paste0(lp, rec(node$e), rp),
    func  = switch(node$name,
                   sqrt = paste0("\\sqrt{", grouped(node$e), "}"),
                   exp  = paste0("\\exp", lp, rec(node$e), rp),
                   log  = paste0("\\log", lp, rec(node$e), rp),
                   sin  = paste0("\\sin", lp, rec(node$e), rp),
                   cos  = paste0("\\cos", lp, rec(node$e), rp),
                   paste0("\\operatorname{", node$name, "}", lp, rec(node$e), rp)),
    unary = paste0(if (identical(node$op, "-")) "-" else "+", rec(node$e)),
    pow   = paste0(rec(node$base), "^{", grouped(node$exp), "}"),
    bin   = {
      l <- node$l; r <- node$r
      if (node$op %in% c("+", "-")) {
        rhs <- if (identical(r$type, "unary")) paste0(lp, rec(r), rp) else rec(r)
        paste0(rec(l), " ", node$op, " ", rhs)
      } else if (identical(node$op, "/") && compact) {
        # slash form: parentheses around a sum in the numerator and around any
        # composite denominator, so that the reading order is unambiguous
        is_sum <- function(x) identical(x$type, "bin") && x$op %in% c("+", "-")
        num <- if (is_sum(l)) paste0("(", rec(l), ")") else grouped(l)
        den <- if (identical(r$type, "bin") || identical(r$type, "unary")) paste0("(", grouped(r), ")") else grouped(r)
        paste0(num, "/", den)
      } else if (identical(node$op, "/")) {
        num <- grouped(l)
        # a leading unary minus of the numerator is written in front of the fraction
        if (startsWith(num, "-")) paste0("-\\frac{", trimws(substring(num, 2)), "}{", grouped(r), "}") else
          paste0("\\frac{", num, "}{", grouped(r), "}")
      } else {
        rhs <- if (identical(r$type, "unary")) paste0("\\left(", rec(r), "\\right)") else rec(r)
        sep <- if (identical(r$type, "num") || identical(l$type, "num")) " \\cdot " else " \\, "
        paste0(rec(l), sep, rhs)
      }
    },
    "?"
  )
}

# Mathematical rendering of a token sequence (fraction bars, exponents, ...);
# falls back to the plain inline rendering while the expression is incomplete.
ode_builder_tokens_to_math_latex <- function(tokens, catalog, compact = FALSE) {
  if (length(tokens) == 0) return(list(latex = "", structured = FALSE))
  ast <- tryCatch(ode_builder_parse_tokens(tokens), ode_parse_error = function(e) NULL, error = function(e) NULL)
  if (is.null(ast)) return(list(latex = ode_builder_tokens_to_latex(tokens, catalog), structured = FALSE))
  list(latex = ode_builder_ast_latex(ast, catalog, compact = compact), structured = TRUE)
}

ode_builder_expression_math <- function(tokens, catalog) {
  rendered <- ode_builder_tokens_to_math_latex(tokens, catalog)
  if (!nzchar(rendered$latex)) return("")
  ode_builder_math_inline(paste0("\\displaystyle ", rendered$latex),
                          if (rendered$structured) "ode-builder-expression-math" else "ode-builder-expression-math ode-builder-expression-partial")
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
    if (prev %in% c("(", "+", "-", "*", "/", "^") || grepl("\\($", prev)) next
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

    if (isTRUE(token$orphan)) {
      return(list(
        level = "error",
        blocking = TRUE,
        message = sprintf("'%s' no longer exists (the %s was removed): delete it from the equation (Backspace / Clear).",
                          token$display %||% serial,
                          if (identical(kind, "state")) "state variable" else "parameter")
      ))
    }

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

  structure_check <- tryCatch({ ode_builder_parse_tokens(tokens); TRUE },
                              ode_parse_error = function(e) conditionMessage(e),
                              error = function(e) conditionMessage(e))
  if (!isTRUE(structure_check)) {
    return(list(
      level = "error",
      blocking = TRUE,
      message = paste0("Incomplete expression: ", structure_check, ".")
    ))
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
    add_check("ok", sprintf("%d equation%s for %d state variable%s", n, if (n == 1) "" else "s",
                            length(state_labels), if (n == 1) "" else "s"))
  } else {
    add_check("error", sprintf("Expected %d state variables, but %d are currently available",
                               n, length(state_labels)))
  }
  if (any(!nzchar(trimws(state_labels)))) {
    add_check("warning", "Some state variables have no name yet (step 4)")
  }
  if (any(!nzchar(trimws(param_labels)))) {
    add_check("warning", "Some parameters have no name yet (step 5)")
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
    if ("ic" %in% names(params)) all_params <- all_params[!(params$ic %in% TRUE)]
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
        font-size: 18px;
        line-height: 1.6;
      }
      .ode-builder-root .ode-builder-expression-partial {
        color: #777;
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
      uiOutput(ns("container"))
    )
  )
}

#------------------------------------------------------------------------------#
# Server -----------------------------------------------------------------------
#------------------------------------------------------------------------------#

odeBuilderServer <- function(id, n_equations = NULL, states_reactive,
                             params_reactive, tdp_reactive,
                             fitting_diff_reactive = reactive(NULL),
                             preset_reactive = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    token_state <- reactiveValues()
    active_equation <- reactiveVal(1L)
    last_error <- reactiveVal(NULL)

    # Stable keys of the state rows: one equation per key
    state_keys <- reactive({ ode_builder_state_keys(states_reactive()) })

    # Number of equations (defaults to the number of state rows)
    n_equations <- if (is.null(n_equations)) reactive({ length(state_keys()) }) else n_equations

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

    # Key of the i-th equation (falls back to the position for legacy callers)
    key_at <- function(i, keys = state_keys()) {
      i <- as.integer(i)
      if (!is.na(i) && i >= 1 && i <= length(keys)) keys[[i]] else as.character(i)
    }

    # Tokens of every equation, refreshed from the current catalog (see
    # ode_builder_resolve_token): serials follow the rows, orphans are flagged.
    current_tokens <- reactive({
      n <- n_equations()
      if (is.null(n) || is.na(n) || n < 1) return(list())
      catalog <- current_catalog()
      keys <- state_keys()
      lapply(seq_len(as.integer(n)), function(i) {
        ode_builder_resolve_tokens(ode_builder_get_tokens(token_state, key_at(i, keys)), catalog)
      })
    })

    # Presets (model library): list(nonce = <any>, equations = list(<state key> = <tokens>))
    observeEvent(preset_reactive(), {
      preset <- preset_reactive()
      if (is.null(preset) || is.null(preset$equations)) return(NULL)
      # drop the equations of rows that no longer exist
      for (nm in names(reactiveValuesToList(token_state))) token_state[[nm]] <- NULL
      for (k in names(preset$equations)) {
        ode_builder_set_tokens(token_state, k, preset$equations[[k]])
      }
      active_equation(1L)
      last_error(NULL)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

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

    build_card_ui <- function(i, catalog, statuses, active, tokens_all) {
      states <- catalog[catalog$kind == "state", , drop = FALSE]
      state_label <- if (NROW(states) >= i && nzchar(states$display[[i]])) states$display[[i]] else paste0("var", i)
      tokens <- if (length(tokens_all) >= i) tokens_all[[i]] else list()
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
                    ode_builder_math_inline(paste0("\\displaystyle \\frac{d", state_label, "}{dt} ="))),
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
      states <- states[nzchar(states$display), , drop = FALSE]
      state_buttons <- lapply(seq_len(NROW(states)), function(j) {
        ode_builder_token_button(ns, states[j, ], "ode-token-state")
      })

      params <- catalog[catalog$kind == "param", , drop = FALSE]
      params <- params[nzchar(params$display), , drop = FALSE]
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
        if (NROW(state_rows) >= active && nzchar(state_rows$display[[active]])) state_rows$display[[active]] else paste0("var", active)
      }

      div(
        class = "ode-builder-keypad",
        div(class = "ode-builder-keypad-title", "Token keypad ",
            tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#555; font-size: 12px;",
                   title = paste("Select an equation card, then click the tokens. Symbols placed next to each other are multiplied",
                                 "(\u03b2 S I means \u03b2\u00b7S\u00b7I; the \u00d7 key is optional). Division is displayed as a fraction and",
                                 "powers as superscripts; use parentheses to change the grouping (a / b c is (a / b)\u00b7c, a / (b c) puts both in the denominator)."))),
        div(class = "ode-builder-active-note",
            "Inserting into ",
            ode_builder_math_inline(paste0("\\frac{d", active_label, "}{dt}")),
            "."),
        div(
          ode_builder_edit_button(ns, "backspace", "Backspace"),
          ode_builder_edit_button(ns, "clear", "Clear", "btn-warning")
        ),
        ode_builder_button_group(
          "State variables",
          state_buttons,
          "Add and name the state variables in step 4 to get their buttons here."
        ),
        ode_builder_button_group(
          "Scalar parameters",
          param_buttons,
          "Add and name the parameters in step 5 to get their buttons here."
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

    build_developer_preview_ui <- function(n, catalog, tokens) {
      rhs <- vapply(tokens, ode_builder_tokens_to_serial, character(1))
      display <- vapply(tokens, ode_builder_tokens_to_display, character(1), catalog = catalog)
      lines <- paste0("diff_var", seq_len(n), " = ", rhs)
      pretty <- paste0("d", {
        states <- catalog[catalog$kind == "state", , drop = FALSE]
        labels <- if (NROW(states) >= n) states$display[seq_len(n)] else paste0("var", seq_len(n))
        labels[!nzchar(labels)] <- paste0("var", seq_len(n))[!nzchar(labels)]
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
        return(helpText("Add state variables in step 4 (or choose a model from the Model library) to get one equation per state variable here."))
      }
      n <- as.integer(n)
      catalog <- current_catalog()
      statuses <- equation_statuses()
      active <- active_equation()
      err <- last_error()
      tokens_all <- current_tokens()

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
                                  active = active,
                                  tokens_all = tokens_all)),
          build_status_panel_ui(model_checks()),
          build_developer_preview_ui(n, catalog, tokens_all)
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
      if (token$kind %in% c("state", "param")) token$key <- click$key %||% token$serial
      key <- key_at(row)
      tokens <- ode_builder_get_tokens(token_state, key)
      ode_builder_set_tokens(token_state, key, ode_builder_append_token(tokens, token))
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
      key <- key_at(row)
      tokens <- ode_builder_get_tokens(token_state, key)
      token <- list(display = value, serial = value, kind = "number")
      ode_builder_set_tokens(token_state, key, c(tokens, list(token)))
      last_error(NULL)
    })

    observeEvent(input$edit_click, {
      click <- input$edit_click
      row <- suppressWarnings(as.integer(click$row %||% active_equation()))
      n <- suppressWarnings(as.integer(n_equations()))
      action <- as.character(click$action %||% "")
      if (is.na(row) || row < 1 || is.na(n) || row > n) return(NULL)
      active_equation(row)
      key <- key_at(row)
      tokens <- ode_builder_get_tokens(token_state, key)
      if (identical(action, "clear")) {
        tokens <- list()
      } else if (identical(action, "backspace") && length(tokens) > 0) {
        tokens <- tokens[-length(tokens)]
      }
      ode_builder_set_tokens(token_state, key, tokens)
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
