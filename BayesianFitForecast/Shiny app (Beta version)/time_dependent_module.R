#------------------------------------------------------------------------------#
# Time-dependent parameters -- Shiny module                                     #
#------------------------------------------------------------------------------#
# About: Self-contained UI + server for defining time-dependent parameters with #
# structured templates. The module keeps three views separate:                  #
#   * pretty LaTeX/display labels for users                                     #
#   * safe internal parameter names and paramsN placeholders for serialization  #
#   * collapsed developer previews for debugging                                #
#------------------------------------------------------------------------------#

# Maximum number of time-dependent parameters (bounds the pre-created outputs).
TDP_MAX <- 12

tdp_module_json <- function(x) {
  as.character(jsonlite::toJSON(as.character(x %||% ""), auto_unbox = TRUE))
}

tdp_math_inline <- function(label) {
  label <- tdp_clean_math_label(label)
  withMathJax(tags$span(class = "tdp-math-inline", paste0("\\(", label, "\\)")))
}

tdp_math_block <- function(label) {
  label <- tdp_clean_math_label(label)
  withMathJax(tags$div(class = "tdp-math-block", paste0("$$", label, "$$")))
}

tdp_selectize_options <- function() {
  list(
    render = I(
      "{
        option: function(item, escape) {
          var internal = item.value ? '<div class=\"tdp-select-internal\">' + escape(item.value) + '</div>' : '';
          return '<div class=\"tdp-select-option\">\\\\(' + escape(item.label) + '\\\\)' + internal + '</div>';
        },
        item: function(item, escape) {
          return '<div class=\"tdp-select-item\">\\\\(' + escape(item.label) + '\\\\)</div>';
        }
      }"
    )
  )
}

tdp_typeset_script <- function(ns) {
  root_id <- ns("root")
  tags$script(HTML(sprintf(
    "
    (function() {
      var rootId = %s;
      function typesetTdp() {
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
          setTimeout(typesetTdp, 40);
        }
      });
      document.addEventListener('click', function(e) {
        if (e.target && e.target.closest && e.target.closest('#' + rootId)) {
          setTimeout(typesetTdp, 40);
          setTimeout(typesetTdp, 250);
        }
      });
      document.addEventListener('change', function(e) {
        if (e.target && e.target.closest && e.target.closest('#' + rootId)) {
          setTimeout(typesetTdp, 40);
        }
      });
    })();
    ",
    tdp_module_json(root_id)
  )))
}

#------------------------------------------------------------------------------#
# UI ---------------------------------------------------------------------------
#------------------------------------------------------------------------------#

tdpUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      .tdp-module-root .tdp-help {
        margin-bottom: 8px;
      }
      .tdp-module-root .tdp-card {
        margin: 10px 0;
        padding: 12px;
        border: 1px solid #ddd;
        border-left: 5px solid #f0ad4e;
        background-color: #f7f7f7;
      }
      .tdp-module-root .tdp-card-title {
        font-weight: bold;
        margin-bottom: 8px;
      }
      .tdp-module-root .tdp-subtle {
        font-size: 11px;
        color: #777;
      }
      .tdp-module-root .tdp-name-preview {
        margin: 8px 0;
        padding: 8px;
        background-color: #fff;
        border: 1px dashed #ccc;
      }
      .tdp-module-root .tdp-name-preview-title,
      .tdp-module-root .tdp-preview-title,
      .tdp-module-root .tdp-role-title {
        font-size: 12px;
        font-weight: bold;
        color: #555;
        margin-bottom: 4px;
      }
      .tdp-module-root .tdp-role-panel {
        padding: 8px;
        background-color: #fff;
        border: 1px solid #e5e5e5;
      }
      .tdp-module-root .tdp-role-selected {
        min-height: 24px;
        font-size: 12px;
        color: #555;
        margin: -8px 0 8px 0;
      }
      .tdp-module-root .tdp-preview-card {
        margin-top: 8px;
        padding: 10px;
        background-color: #fff;
        border: 1px dashed #ccc;
      }
      .tdp-module-root .tdp-math-block {
        overflow-x: auto;
        padding: 4px 0;
      }
      .tdp-module-root .tdp-ode-hint {
        margin-top: 8px;
        padding: 6px 8px;
        background-color: #eef7fb;
        border-left: 3px solid #5bc0de;
        font-size: 12px;
        color: #31708f;
      }
      .tdp-module-root .tdp-dev-preview {
        margin-top: 8px;
        font-size: 12px;
      }
      .tdp-module-root .tdp-dev-preview summary {
        cursor: pointer;
        color: #337ab7;
      }
      .tdp-module-root .tdp-dev-code {
        margin-top: 6px;
        padding: 8px;
        background-color: #f7f7f7;
        border: 1px solid #ddd;
        font-family: monospace;
        font-size: 12px;
        white-space: pre-wrap;
        word-break: break-word;
      }
      .tdp-module-root .tdp-select-internal {
        color: #777;
        font-size: 10px;
        margin-top: 1px;
      }
      .tdp-module-root .selectize-dropdown {
        z-index: 1000;
      }
    ")),
    tdp_typeset_script(ns),
    div(
      id = ns("root"),
      class = "tdp-module-root",
      div(
        class = "tdp-help",
        helpText(
          "Define parameters that vary with time t using structured templates. ",
          "Use LaTeX-style display labels for readability; the app keeps the ",
          "internal paramsN mapping safely hidden behind the scenes."
        )
      ),
      numericInput(ns("num_tdp"), "# Time-Dependent Parameters",
                   value = 0, min = 0, max = TDP_MAX),
      uiOutput(ns("tdpContainer"))
    )
  )
}

#------------------------------------------------------------------------------#
# Server -----------------------------------------------------------------------
#------------------------------------------------------------------------------#
# params_reactive : a reactive returning a named vector:
#                   display/LaTeX labels as names, internal names as values.
tdpServer <- function(id, params_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    all_role_ids <- unique(unlist(lapply(TDP_TEMPLATES, function(tpl) {
      vapply(tpl$roles, function(r) r$id, character(1))
    }), use.names = FALSE))

    valid_role_selection <- function(current, params_choices) {
      valid <- unname(params_choices %||% character(0))
      current <- current %||% ""
      if (length(current) > 0 && isTRUE(nzchar(current[[1]])) && current[[1]] %in% valid) {
        return(current[[1]])
      }
      if (length(valid) > 0) return(valid[[1]])
      character(0)
    }

    valid_type_selection <- function(current) {
      valid <- unname(tdp_choices())
      current <- current %||% ""
      if (length(current) > 0 && isTRUE(nzchar(current[[1]])) && current[[1]] %in% valid) {
        return(current[[1]])
      }
      names(TDP_TEMPLATES)[[1]]
    }

    #--------------------------------------------------------------------------#
    # Assemble a single spec from the inputs of row i --------------------------
    #--------------------------------------------------------------------------#
    build_spec <- function(i) {
      type <- input[[paste0("type_", i)]]
      if (is.null(type)) return(NULL)
      internal <- paste0("time_dependent_param", i)
      display <- trimws(input[[paste0("display_", i)]] %||% "")
      if (!nzchar(display)) display <- internal

      if (identical(type, TDP_ADVANCED)) {
        return(list(
          internal_name = internal,
          display_name  = display,
          type          = type,
          mapping       = list(),
          custom_body   = input[[paste0("custom_", i)]] %||% ""
        ))
      }

      roles <- tdp_roles(type)
      mapping <- list()
      for (r in roles) {
        mapping[[r$id]] <- input[[paste0("role_", i, "_", r$id)]]
      }
      list(
        internal_name = internal,
        display_name  = display,
        type          = type,
        mapping       = mapping,
        custom_body   = NULL
      )
    }

    #--------------------------------------------------------------------------#
    # One card's UI ------------------------------------------------------------
    #--------------------------------------------------------------------------#
    build_card_ui <- function(i) {
      display_value <- isolate(input[[paste0("display_", i)]]) %||%
        sprintf("\\theta_%d(t)", i)
      type_value <- valid_type_selection(isolate(input[[paste0("type_", i)]]))

      fluidRow(
        id = ns(paste0("tdpCard_", i)),
        class = "tdp-card",
        column(
          4,
          div(class = "tdp-card-title", sprintf("Time-dependent parameter %d", i)),
          textInput(ns(paste0("display_", i)), "Display name",
                    value = display_value,
                    placeholder = "\\beta(t)"),
          uiOutput(ns(paste0("name_preview_", i))),
          div(class = "tdp-subtle", paste0("internal: time_dependent_param", i)),
          selectizeInput(ns(paste0("type_", i)), "Template type",
                         choices = tdp_choices(),
                         selected = type_value,
                         options = list(create = FALSE))
        ),
        column(
          8,
          div(
            class = "tdp-role-panel",
            uiOutput(ns(paste0("mapping_", i))),
            uiOutput(ns(paste0("preview_", i)))
          )
        )
      )
    }

    #--------------------------------------------------------------------------#
    # Container: render as many cards as requested -----------------------------
    #--------------------------------------------------------------------------#
    output$tdpContainer <- renderUI({
      n <- input$num_tdp
      if (is.null(n) || is.na(n) || n < 1) {
        return(helpText("Set '# Time-Dependent Parameters' above to add one or more."))
      }
      n <- min(n, TDP_MAX)
      do.call(tagList, lapply(seq_len(n), build_card_ui))
    })

    #--------------------------------------------------------------------------#
    # Per-row mapping + preview (pre-created up to TDP_MAX) --------------------#
    #--------------------------------------------------------------------------#
    for (i in seq_len(TDP_MAX)) {
      local({
        ii <- i

        output[[paste0("name_preview_", ii)]] <- renderUI({
          spec <- build_spec(ii)
          req(spec)
          div(
            class = "tdp-name-preview",
            div(class = "tdp-name-preview-title", "Display preview"),
            tdp_math_inline(spec$display_name)
          )
        })

        # Role dropdowns (or advanced editor) for row ii
        output[[paste0("mapping_", ii)]] <- renderUI({
          type <- input[[paste0("type_", ii)]]
          req(type)
          params <- params_reactive()

          if (identical(type, TDP_ADVANCED)) {
            return(tagList(
              div(class = "tdp-role-title", "Custom expression"),
              div(class = "tdp-subtle", tdp_desc(type)),
              textAreaInput(
                ns(paste0("custom_", ii)),
                label = "Custom body (advanced)",
                value = isolate(input[[paste0("custom_", ii)]]) %||% "",
                rows = 3,
                placeholder = "e.g. return(params1 * exp(-params2 * t));"
              ),
              div(
                class = "tdp-subtle",
                "For now this advanced field accepts a raw R/Stan-like body. ",
                "Use the structured templates when possible."
              )
            ))
          }

          roles <- tdp_roles(type)
          role_inputs <- lapply(roles, function(r) {
            role_input_id <- paste0("role_", ii, "_", r$id)
            selected <- valid_role_selection(isolate(input[[role_input_id]]), params)
            column(
              6,
              selectizeInput(
                ns(role_input_id),
                label = r$label,
                choices = params %||% character(0),
                selected = selected,
                options = tdp_selectize_options()
              ),
              uiOutput(ns(paste0("role_selected_", ii, "_", r$id)))
            )
          })
          tagList(
            div(class = "tdp-role-title", "Template roles"),
            div(class = "tdp-subtle", tdp_desc(type)),
            do.call(fluidRow, role_inputs)
          )
        })

        for (role_id in all_role_ids) {
          local({
            rr <- role_id
            output[[paste0("role_selected_", ii, "_", rr)]] <- renderUI({
              selected <- input[[paste0("role_", ii, "_", rr)]]
              params <- params_reactive()
              valid <- unname(params %||% character(0))

              if (is.null(selected) || length(selected) == 0 ||
                  !nzchar(selected[[1]]) || !(selected[[1]] %in% valid)) {
                return(div(
                  class = "tdp-role-selected",
                  tags$span(class = "tdp-subtle", "Select a parameter for this role.")
                ))
              }

              selected <- selected[[1]]
              display <- tdp_param_display_label(selected, params)
              placeholder <- tdp_param_placeholder_label(selected, params)
              div(
                class = "tdp-role-selected",
                "Selected: ",
                tdp_math_inline(display),
                tags$span(class = "tdp-subtle",
                          sprintf("  (%s -> %s)", selected, placeholder))
              )
            })
          })
        }

        # Live preview (math form + collapsed developer serialization)
        output[[paste0("preview_", ii)]] <- renderUI({
          spec <- build_spec(ii)
          req(spec)

          params_choices <- params_reactive()
          params_internal <- unname(params_choices)

          math_preview <- tryCatch(
            tdp_render_math_preview(spec, params_choices),
            error = function(e) sprintf("%s = \\text{preview unavailable}",
                                        spec$display_name %||% spec$internal_name)
          )

          named <- tryCatch(tdp_render_body_named(spec),
                            error = function(e) paste("error:", conditionMessage(e)))
          serialized_body <- if (tdp_spec_complete(spec, params_internal)) {
            tryCatch(tdp_render_body(spec, params_internal),
                     error = function(e) paste("error:", conditionMessage(e)))
          } else {
            "(complete all fields to see the serialized form)"
          }
          serialized_block <- if (tdp_spec_complete(spec, params_internal)) {
            tryCatch(tdp_serialize(list(spec), params_internal),
                     error = function(e) paste("error:", conditionMessage(e)))
          } else {
            "time_dependent_templates <- list()"
          }

          ref_token <- trimws(spec$display_name %||% spec$internal_name)
          if (!nzchar(ref_token)) ref_token <- spec$internal_name

          div(
            class = "tdp-preview-card",
            div(class = "tdp-preview-title", "Mathematical preview"),
            tdp_math_block(math_preview),
            div(
              class = "tdp-ode-hint",
              "Use ",
              tdp_math_inline(ref_token),
              " in the ODE builder; internally it is stored as ",
              tags$code(spec$internal_name),
              "."
            ),
            tags$details(
              class = "tdp-dev-preview",
              tags$summary("Developer preview: show internal serialized template"),
              tags$div(
                class = "tdp-dev-code",
                paste(
                  paste0(spec$internal_name, "(t) named body:"),
                  named,
                  "",
                  "Serialized body:",
                  serialized_body,
                  "",
                  "Options-file block:",
                  serialized_block,
                  sep = "\n"
                )
              )
            )
          )
        })
      })
    }

    #--------------------------------------------------------------------------#
    # Collect specs and serialize ----------------------------------------------#
    #--------------------------------------------------------------------------#
    specs_reactive <- reactive({
      n <- input$num_tdp
      if (is.null(n) || is.na(n) || n < 1) return(list())
      n <- min(n, TDP_MAX)
      Filter(Negate(is.null), lapply(seq_len(n), build_spec))
    })

    # Returned reactive: code block + complete internal names + raw specs.
    reactive({
      params <- unname(params_reactive())
      specs <- specs_reactive()
      code <- tryCatch(tdp_serialize(specs, params),
                       error = function(e) "time_dependent_templates <- list()")
      names_vec <- vapply(
        Filter(function(s) tdp_spec_complete(s, params), specs),
        function(s) s$internal_name, character(1)
      )
      list(code = code, names = names_vec, specs = specs)
    })
  })
}
