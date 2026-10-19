
  ###################
  # Needed packages #
  ###################
  
  # List of required packages (only what the app actually uses)
  required_packages <- c(
    "shiny", "shinydashboard", "shinyWidgets", "shinyjs", "shinybusy", "shinyalert",
    "ggplot2", "plotly", "DT", "readxl", "openxlsx", "rstan", "bayesplot", "loo",
    "deSolve", "abind", "checkmate", "digest", "zip", "stringr", "glue", "jsonlite", "callr"
  )

  # Function to check and install if needed
  install_and_load <- function(pkg) {

    if (!requireNamespace(pkg, quietly = TRUE)) {
      message(paste("Installing", pkg))
      install.packages(pkg)
    }

    library(pkg, character.only = TRUE)
  }

  # Apply to all packages
  invisible(lapply(required_packages, install_and_load))

#------------------------------------------------------------------------------#
# Loading latex2r and shinymath ------------------------------------------------
#------------------------------------------------------------------------------#
# About: Both packages ship with the app (folders `latex2r-master` and           #
# `shinymath-master`). They are installed from source ONLY the first time the   #
# app runs on a computer; afterwards they are simply loaded, which makes the    #
# start-up much faster and no longer requires Rtools/compilers at every launch. #
#------------------------------------------------------------------------------#

  install_local_once <- function(pkg, folder) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message(paste("Installing", pkg, "from", folder))
      install.packages(folder, repos = NULL, type = "source")
    }
    library(pkg, character.only = TRUE)
  }
  install_local_once("latex2r", "latex2r-master")
  install_local_once("shinymath", "shinymath-master")

  # Stan: re-use compiled models and allow parallel chains
  rstan::rstan_options(auto_write = TRUE)

  # Folder of the app (wherever the toolbox was installed): shiny::runApp() makes
  # it the working directory while app.R is sourced. Everything the app writes
  # (compiled models in stan_cache/, saved runs in runs/) is placed relative to
  # it; no absolute path is used anywhere.
  options(bff.app_dir = normalizePath(getwd(), winslash = "/", mustWork = FALSE))

  ############################
  # Load necessary functions #
  ############################
  source("error.checking.R")
  source("run_MCMC2.R")          # bff_load_options(), run_MCMC2()
  source("run_MCMC.R")           # bff_compile_stan(), run_MCMC()
  source("run_analyzeResults.R") # run_analyzeResults() + diagnostics helpers
  source("check.IC.R")
  source("diff.R")
  source("ode_rhs.R")
  source("prior_tools.R")        # prior density preview + prior predictive check
  source("config_tools.R")       # configuration (options.R) validation
  source("async_tools.R")        # background sampling with live progress

  ###########################################
  # Time-dependent parameters (new feature) #
  ###########################################
  # Structured template library + Shiny module + Stan-generation delegation to
  # the MAIN updated stancreator.R (which supports time_dependent_templates).
  source("time_dependent_templates_lib.R")
  source("time_dependent_module.R")
  source("ode_equation_builder_module.R")
  source("generate_stan_delegate.R")
  source("model_library.R")        # ready-to-use models (EXP, GLM, Richards, Gompertz, SIR, SEIR, SEIRD) + model cards
  source("run_history.R")          # completed runs saved on disk (runs/), re-opened from the 'Run history' tab

  #############################################################################
  # Math input with an initial value                                          #
  #############################################################################
  # shinymath::mathInput() has no `value` argument. MathQuill renders the text #
  # content of the field as its initial LaTeX, so rows created by the model    #
  # library can start with their initial condition / parameter name filled in.#
  bff_math_input <- function(inputId, label, value = "", width = NULL) {
    ui <- shinymath::mathInput(inputId, label, width = width)
    value <- as.character(value %||% "")
    if (nzchar(value)) {
      # the editable span is the last child of the input container
      container <- ui[[2]]
      container$children[[length(container$children)]]$children <- list(value)
      ui[[2]] <- container
    }
    ui
  }

  #############################################################################
  # Display-name -> internal-identifier mapping                               #
  #############################################################################
  # Parameters may be entered as LaTeX/display names (e.g. "\\beta_0",         #
  # "t_{int}"). Stan needs valid identifiers ("beta0", "tint"). This helper    #
  # tries latex2r first (so valid LaTeX like "\\gamma" -> "gamma"); if latex2r #
  # fails (e.g. "t_{int}" throws) or does not yield a clean identifier (e.g.   #
  # plain "beta0" mangles to "b * exp(1) * t * a * 0"), it falls back to       #
  # stripping LaTeX punctuation. This is what fixes the "check parameters"     #
  # error for names like t_{int}.                                             #
  to_internal_name <- function(raw) {
    raw <- trimws(as.character(raw))
    if (!nzchar(raw)) return(raw)
    out <- tryCatch(
      paste0(latex2r(raw)),
      latex2r.error = function(e) NA_character_,
      error = function(e) NA_character_
    )
    if (length(out) != 1 || is.na(out) || !grepl("^[A-Za-z][A-Za-z0-9_]*$", out)) {
      out <- gsub("\\\\", "", raw)          # drop backslashes:  \beta_0 -> beta_0
      out <- gsub("[{}]", "", out)          # drop braces:       t_{int} -> t_int
      out <- gsub("_", "", out)             # drop underscores:  beta_0  -> beta0
      out <- gsub("[^A-Za-z0-9]", "", out)  # drop anything else
      if (!nzchar(out) || !grepl("^[A-Za-z]", out)) out <- paste0("p", out)
    }
    # Keep the established options-file convention: "\\beta_0" -> "beta0".
    out <- gsub("_", "", out, fixed = TRUE)
    out
  }


#------------------------------------------------------------------------------#
# Creating the dashboard interface ---------------------------------------------
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This section creates the user interface for the reporting delay dashboard.   #
# It provides the user-friendly features along with the structure of the       #
# dashboard itself.                                                            #
#------------------------------------------------------------------------------#

ui <- dashboardPage(
  
  
  ########################################
  # Creating the header of the dashboard #
  ########################################
  dashboardHeader(title = "BayesianFitForecast"),
  
  ################################
  # Creating the sidebar options #
  ################################
  dashboardSidebar(
    
    ######################
    # Producing sidebars #
    ######################
    sidebarMenu(id = "sidebar",
                
#------------------------------------------------------------------------------#
# Creating the `Data Details` sidebar option -----------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the side-bar options for specifying details      #
# about the data. These include loading the data, the process of interest,     #
# type of data, and date.                                                      #
#------------------------------------------------------------------------------#

    ################################
    # Menu Option for Data details #
    ################################
    menuItem("1. Data",

             # Menu ID
             tabName = "dataOptions",

             # Icon for tab
             icon = icon("table"),

             ###########################
             # Creating the data input #
             ###########################
             uiOutput("input.data"),

             ###########################
             # Topic of Interest Input #
             ###########################
             uiOutput("process.of.interest"),

             ######################################################
             # Creating the 'series_cases' and 'datetype' options #
             ######################################################
             uiOutput("seriescasesdatetype"),

             # Starting with this menu expanded
             startExpanded = TRUE

    ), # End of data parameters

#------------------------------------------------------------------------------#
# Setting up the `Forecasting/Fitting` sidebar options -------------------------
#------------------------------------------------------------------------------#
# About: This section creates the side-bar options for forecasting and fitting #
# the models to data. Options include the length of the calibration period and #
# the forecasting horizon.                                                     #
#------------------------------------------------------------------------------#

    ###############################################
    # Menu Option for Forecasting/Fitting details #
    ###############################################
    menuItem("2. Calibration & Forecast",

             # Menu ID
             tabName = "ForecastingOptions",

             # Icon for tab
             icon = icon("chart-line"),

             #############################
             # Calibration period length #
             #############################
             uiOutput("calibrationPeriodLength"),

             #######################
             # Forecasting Horizon #
             #######################
             uiOutput("forecast.horizon")

    ), # End of 'Forecasting/Fitting' details

#------------------------------------------------------------------------------#
# Setting up the model name select, variables, and parameters ------------------
#------------------------------------------------------------------------------#
# About: This section creates the model name indicator, parameter selection,   #
# and variable selection options.                                              #
#------------------------------------------------------------------------------#

    ##############################################
    # Menu Option for basic model specifications #
    ##############################################
    menuItem("3. Model & MCMC Settings",

             # Menu ID
             tabName = "baseModelOptions",

             # Icon for tab
             icon = icon("sliders"),

             ##############
             # Model name #
             ##############
             textInput("model_name",
                       tags$span("Model Name ",
                                 tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF;",
                                        title = "Used in the names of the downloaded files, e.g. 'SEIR-SF1918'.")),
                       value = "", placeholder = "e.g. SEIR"),

             ###################
             # Error structure #
             ###################
             pickerInput("errstrc",
                         tags$span("Error Structure ",
                                   tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF;",
                                          title = paste("Poisson: count data with variance = mean.",
                                                        "Negative binomial: over-dispersed counts (variance > mean), the safest default for case counts.",
                                                        "Normal: continuous or large-valued data."))),
                         choices = c("Negative binomial", "Normal", "Poisson")),

             ###################################################
             # Number of Markov Chain Monte Carlo (MCMC) steps #
             ###################################################
             numericInput("niter",
                          tags$span("MCMC iterations per chain ",
                                    tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF;",
                                           title = "Total iterations per chain (including warm-up). 1000-2000 is fine for a first look; use more for publication-quality estimates.")),
                          1000, min = 100, step = 100),

             ####################################################
             # Number of Markov Chain Monte Carlo (MCMC) chains #
             ####################################################
             numericInput("num_chain",
                          tags$span("Number of chains ",
                                    tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF;",
                                           title = "Independent chains, needed to assess convergence (Rhat). 2-4 chains are typical; they run in parallel when cores are available.")),
                          2, min = 1, step = 1),

             ##############################
             # Advanced sampler settings  #
             ##############################
             tags$details(
               style = "padding: 0 15px 10px 15px; color: #b8c7ce;",
               tags$summary(style = "cursor: pointer; outline: none;", "Advanced sampler settings"),
               numericInput("nwarmup",
                            tags$span("Warm-up iterations ",
                                      tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF;",
                                             title = "Iterations discarded at the start of each chain. Leave empty to use half of the iterations (Stan default).")),
                            value = NA, min = 50, step = 50),
               numericInput("adapt_delta",
                            tags$span("adapt_delta ",
                                      tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF;",
                                             title = "Target acceptance rate (0.8 default). Increase to 0.95-0.99 if divergent transitions are reported.")),
                            value = 0.8, min = 0.5, max = 0.999, step = 0.01),
               numericInput("max_treedepth",
                            tags$span("max_treedepth ",
                                      tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF;",
                                             title = "Maximum NUTS tree depth (10 default). Increase to 12-15 if the sampler hits the maximum treedepth.")),
                            value = 10, min = 5, max = 20, step = 1),
               numericInput("n_cores",
                            tags$span("Parallel cores ",
                                      tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF;",
                                             title = "Number of CPU cores used to run chains in parallel.")),
                            value = max(1, min(2, parallel::detectCores() - 1)), min = 1,
                            max = max(1, parallel::detectCores()), step = 1),
               numericInput("seed",
                            tags$span("Random seed ",
                                      tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF;",
                                             title = "Seed of the random number generator, for reproducible results.")),
                            value = 0, min = 0, step = 1)
             )

             ), # End of 'menuItem' for base model specifications

#------------------------------------------------------------------------------#
# Saved runs ---------------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Every completed run is saved in the runs/ folder (see run_history.R).   #
# This menu lists them (empty until the first run) and re-opens one: results,   #
# configuration and data are restored. The 'Run history' tab gives the details  #
# (report, download, delete).                                                   #
#------------------------------------------------------------------------------#

    menuItem("Saved runs",

             tabName = "savedRuns",

             icon = icon("clock-rotate-left"),

             pickerInput("saved_run",
                         tags$span("Saved runs ",
                                   tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF;",
                                          title = "Completed runs are saved automatically (folder 'runs' next to app.R). Choose one and press 'Open' to see its results again; the 'Run history' tab has the report, download and delete options.")),
                         choices = c("No saved run yet" = ""),
                         options = list(size = 10)),

             div(style = "padding: 0 15px 10px 15px;",
                 actionButton("open_saved_run_btn", "Open", icon = icon("folder-open"), width = "100%"))
    ),

#------------------------------------------------------------------------------#
# Adding the Run Button --------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the "Run" button for users. In order to get any  #
# output, the user must select the run button.                                 #
#------------------------------------------------------------------------------#

  ###########################
  # Creating the Run Button #
  ###########################
  div(style = "padding: 10px 15px;",
      shiny::actionButton("run",
                          label = "Fit model & forecast",
                          icon = icon("play"),
                          width = "100%",
                          class = "btn-success"))

    ) # End of `sidebarMenu`
  
  ), # End of dashboard side-bar
 
  ######################################
  # Creating the body of the dashboard #
  ######################################
  dashboardBody(

    # Styles used throughout the dashboard
    tags$head(
      tags$style(HTML("
      .content-wrapper, .right-side {
        min-height: 100vh !important;
        overflow-y: auto;
      }
      .bff-step { display: flex; align-items: center; padding: 2px 0; line-height: 1.35; cursor: default; }
      .bff-step .bff-icon { width: 22px; flex: 0 0 22px; font-size: 15px; margin-right: 6px; }
      .bff-ok   { color: #00a65a; }
      .bff-todo { color: #dd4b39; }
      .bff-opt  { color: #999999; }
      .bff-hint { color: #777; font-size: 12px; }
      .bff-status-badge { color: white; padding: 4px 12px; border-radius: 4px; font-weight: bold; display: inline-block; font-size: 15px; }
      .bff-status-Good    { background: #00a65a; }
      .bff-status-Caution { background: #f39c12; }
      .bff-status-Poor    { background: #dd4b39; }
      .bff-status-Unknown { background: #777777; }
      .bff-mono { font-family: monospace; font-size: 12px; white-space: pre-wrap; background: #f7f7f7; padding: 8px; border: 1px solid #ddd; max-height: 220px; overflow-y: auto; }
      .bff-kpi { text-align: center; padding: 6px 4px; }
      .bff-kpi .bff-kpi-value { font-size: 20px; font-weight: bold; }
      .bff-kpi .bff-kpi-label { font-size: 11px; color: #666; text-transform: uppercase; }
      #config_text { font-family: monospace; font-size: 12px; }
      .sidebar-menu .treeview-menu > li > div, .sidebar-menu .treeview-menu > li > .form-group { padding-left: 15px; padding-right: 15px; }
      .bff-row-remove { margin-top: 25px; }
      /* Aligned tables of rows (state variables, parameters, error parameters, composites) */
      .bff-grid-wrap { overflow-x: auto; padding-bottom: 4px; }
      .bff-grid { display: grid; column-gap: 6px; row-gap: 0; align-items: center; }
      .bff-grid-states    { grid-template-columns: minmax(120px, 1fr) minmax(140px, 1.2fr) 90px minmax(120px, 1fr) 100px 30px; min-width: 640px; }
      .bff-grid-params    { grid-template-columns: minmax(96px, 1fr) 96px 100px minmax(136px, 1.4fr) 54px 62px 62px 28px; min-width: 676px; }
      .bff-grid-err       { grid-template-columns: 170px 125px minmax(190px, 1fr); min-width: 500px; }
      .bff-grid-composite { grid-template-columns: 170px minmax(260px, 1fr) 30px; min-width: 480px; }
      .bff-cell > .shiny-html-output { width: 100%; }
      .bff-grid-head { font-size: 11.5px; font-weight: bold; color: #555; padding: 4px 0 6px 0; border-bottom: 2px solid #ddd; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
      .bff-grid-row { display: contents; }
      .bff-grid-row > .bff-cell { border-bottom: 1px solid #e3e3e3; padding: 8px 0; min-height: 56px; display: flex; align-items: center; }
      .bff-cell .form-group { margin-bottom: 0; width: 100%; }
      .bff-cell .shiny-input-container { width: 100% !important; }
      .bff-cell .selectize-control, .bff-cell .form-control, .bff-cell .bootstrap-select { min-width: 0; width: 100%; }
      .bff-cell .mathquill-editable { min-height: 34px; display: flex; align-items: center; }
      .bff-cell.bff-cell-center { justify-content: center; }
      .bff-cell .pretty { margin-right: 0; }
      .bff-args { display: flex; gap: 8px; width: 100%; }
      .bff-args .bff-arg { flex: 1 1 0; min-width: 65px; max-width: 150px; }
      .bff-args .bff-arg-label { font-size: 11px; color: #777; line-height: 1.1; margin-bottom: 2px; white-space: nowrap; }
      .bff-cell-name { font-size: 16px; line-height: 1.1; flex-direction: column; align-items: flex-start !important; justify-content: center; }
      .bff-cell-name .bff-hint { font-size: 10.5px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; max-width: 100%; }
      .bff-add-bar { margin: 12px 0 4px 0; display: flex; gap: 10px; flex-wrap: wrap; align-items: center; }
      .bff-model-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(360px, 1fr)); gap: 14px; }
      .bff-model-card { border: 1px solid #ddd; border-radius: 4px; padding: 12px 14px; background: #fafafa; display: flex; flex-direction: column; }
      .bff-model-card-head { display: flex; justify-content: space-between; align-items: flex-start; gap: 8px; flex-wrap: wrap; }
      .bff-model-card-actions { display: flex; gap: 6px; flex-wrap: wrap; }
      .bff-model-title { margin: 0 0 4px 0; }
      .bff-model-tagline { color: #555; margin: 8px 0 4px 0; }
      .bff-model-equations { font-size: 15px; overflow-x: auto; padding: 4px 0; }
      .bff-model-eq-line { margin: 8px 0 2px 0; line-height: 2; }
      .bff-model-family { margin: 18px 0 8px 0; color: #3c8dbc; border-bottom: 1px solid #ddd; padding-bottom: 4px; }
      .bff-model-eq-inline { display: inline-block; margin-right: 16px; font-size: 15px; white-space: nowrap; }
      .bff-model-table td, .bff-model-table th { font-size: 13px; vertical-align: top; }
      .bff-run-item { border: 1px solid #ddd; border-radius: 4px; padding: 10px 14px; margin-bottom: 10px; background: #fafafa; display: flex; justify-content: space-between; gap: 10px; flex-wrap: wrap; align-items: center; }
      .bff-run-item .bff-run-title { font-weight: bold; font-size: 15px; }
      .bff-run-item .bff-run-meta { color: #555; font-size: 12px; }
      .bff-run-actions { display: flex; gap: 6px; flex-wrap: wrap; }
    "))
    ),

    shinyjs::useShinyjs(),

    ###########################
    # Adding a loading circle #
    ###########################
    add_busy_spinner(spin = "fading-circle", position = "top-right"),

#------------------------------------------------------------------------------#
# Row 0: Readiness checklist and data preview ----------------------------------
#------------------------------------------------------------------------------#
# About: The checklist tells the user which steps are complete and what is      #
# still missing before the model can be fitted. The data preview shows the      #
# uploaded time series with the calibration / forecast windows.                #
#------------------------------------------------------------------------------#

  # Layout: the work panel (model library, steps 4-8, configuration, run history)
  # on the left; a persistent column on the right with the data preview, the
  # live status of a fit and the checklist. The results follow in full width.
  fluidRow(
    column(width = 8,

#------------------------------------------------------------------------------#
# Row 1: Specifying the parameter and variable information ---------------------
#------------------------------------------------------------------------------#
# About: This section creates the tabbed box to specify the parameter and      #
# variable information.                                                        #
#------------------------------------------------------------------------------#

  tabBox(

    # Width
    width = 12,

    # Id
    id = "EQUATIONS",

#------------------------------------------------------------------------------#
# Row 1-0: Model library ---------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Ready-to-use models. 'Use this model' fills steps 4, 5, 7 and 8 of the  #
# form (state variables, parameters & priors, equations, composite variables);   #
# 'Details & report' documents the model (equations, parameters, references).    #
#------------------------------------------------------------------------------#

  tabPanel(

    title = tags$span(icon("book-open"), " Model library"),

    value = "ModelLib",

    uiOutput("model_library_ui")

  ),

#------------------------------------------------------------------------------#
# Row 1A: Specifying the state variables, and their information ----------------
#------------------------------------------------------------------------------#

  tabPanel(

    # Title
    title = "4. State Variables",

    value = "state_variables",

    # Width
    width = 12,

    #####################################
    # Creating the inputs for variables #
    #####################################
    # Aligned table: one header row, then one row per state variable (inserted by the server)
    div(class = "bff-grid-wrap",
        div(id = "inputContainer2", class = "bff-grid bff-grid-states",
            div(class = "bff-grid-head", "State variable ",
                tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#555;",
                       title = "Pick a name or type your own (letters and digits, starting with a letter; e.g. S, Ih, Sv).")),
            div(class = "bff-grid-head", "Initial condition ",
                tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#555;",
                       title = "A number, or an expression with parameters (e.g. N - i_0); parameters used here get a row in step 5.")),
            div(class = "bff-grid-head", "Fit to data"),
            div(class = "bff-grid-head", "Time series"),
            div(class = "bff-grid-head", "Fit to derivative ",
                tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#555;",
                       title = paste("Yes: the data are compared with the rate of change of this compartment",
                                     "(e.g. new cases per day = dC/dt). No: the data are compared with the",
                                     "compartment itself (e.g. prevalence I)."))),
            div(class = "bff-grid-head"))),

    div(class = "bff-add-bar",
        actionButton("add_state", "Add state variable", icon = icon("plus"), class = "btn-primary"),
        uiOutput("state_count_hint", inline = TRUE))

  ),

#------------------------------------------------------------------------------#
# Row 1B: Specifying the parameters, and their information ---------------------
#------------------------------------------------------------------------------#

    tabPanel(

      # Title
      title = "5. Parameters & Priors",

      value = "paramsVariables",

      # Width
      width = 12,

      ######################################
      # Creating the inputs for parameters #
      ######################################
      # Aligned table: one header row, then one row per parameter (inserted by the server)
      div(class = "bff-grid-wrap",
          div(id = "inputContainer", class = "bff-grid bff-grid-params",
            div(class = "bff-grid-row bff-grid-header",
              div(class = "bff-grid-head", "Parameter ",
                  tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#555;",
                         title = "Name of the parameter; LaTeX is fine (\\beta, \\gamma, t_{int}). Parameters found in the initial conditions are listed automatically.")),
              div(class = "bff-grid-head", "Estimation"),
              div(class = "bff-grid-head", "Prior / value"),
              div(class = "bff-grid-head", "Prior arguments"),
              div(class = "bff-grid-head", title = "Truncate the prior at 0 (T[0,]), i.e. restrict it to positive values.", "T[0,]"),
              div(class = "bff-grid-head", title = "Lower bound declared in Stan (0 for positive parameters; NA = no bound).", "Lower"),
              div(class = "bff-grid-head", title = "Upper bound declared in Stan (NA = no upper bound; 1 for proportions).", "Upper"),
              div(class = "bff-grid-head")))),

      div(class = "bff-add-bar",
          actionButton("add_param", "Add parameter", icon = icon("plus"), class = "btn-primary"),
          uiOutput("param_count_hint", inline = TRUE)),

      ###########################
      # Creating the second row #
      ###########################
      fluidRow(

        # Creating the column for row cut-off
        column(

          # Width
          width = 12,

          # Creating the header
          div(

            conditionalPanel(

              condition = "(input.errstrc == 'Negative binomial' || input.errstrc == 'Normal')",

              h4("Error-structure parameters",
                  title = "One per fitted time series. Negative binomial: prior of the inverse over-dispersion (1/phi). Normal: prior of the standard deviation sigma.",
                  style = "font-weight: bold; margin: 18px 0 6px 0; font-size: 15px;"),

              # Creating the parameter options (one aligned row per fitted series)
              div(class = "bff-grid-wrap",
                  div(id = "reqParamsF", class = "bff-grid bff-grid-err",
                      div(class = "bff-grid-head", "Parameter"),
                      div(class = "bff-grid-head", "Prior"),
                      div(class = "bff-grid-head", "Prior arguments")))

             )

          ),

        ) # End of column

      ), # End of `fluidRow`

      div(style = "margin-top: 10px;",
          actionButton("previewPriors", "Preview prior distributions", icon = icon("chart-area")))

    ), # End of 'tabPanel'

#------------------------------------------------------------------------------#
# Time-Dependent Parameters Tab (placed before System of Equations) ------------#
#------------------------------------------------------------------------------#

  tabPanel(

    # Title
    title = "6. Time-Dependent Parameters (optional)",

    value = "TimeDep",

    # Width
    width = 12,

    # Module UI (see time_dependent_module.R)
    tdpUI("tdp")

  ),

#------------------------------------------------------------------------------#
# Row 1C: Specifying the equations ---------------------------------------------
#------------------------------------------------------------------------------#

  tabPanel(

    # Title
    title = "7. System of Equations",

    value = "ODESys",

    # Width
    width = 12,

    #########################################
    # Creating the inputs for ODE equations #
    #########################################
    odeBuilderUI("odeb"),

    div(style = "margin-top: 10px; display: flex; gap: 10px; flex-wrap: wrap;",
        actionButton("loadEquation", label = "Validate equations", icon = icon("check"), class = "btn-primary"),
        actionButton("priorPredictive", label = "Prior predictive check", icon = icon("flask"),
                     title = "Simulates trajectories from the priors only (no fitting) to check that the priors are plausible."))

  ), # End of 'tabPanel'

#------------------------------------------------------------------------------#
# Row 1D: Creating the Composite Variables Tab ---------------------------------
#------------------------------------------------------------------------------#

  tabPanel(

    # Title
    title = "8. Composite Variables (optional)",

    value = "Composite",

    ###############################################
    # Creating the inputs for composite equations #
    ###############################################
    div(class = "bff-grid-wrap",
        div(id = "compositeEquationContainer", class = "bff-grid bff-grid-composite",
            div(class = "bff-grid-head", "Name"),
            div(class = "bff-grid-head", "Expression (LaTeX, with the parameter names)"),
            div(class = "bff-grid-head"))),

    div(class = "bff-add-bar",
        actionButton("add_composite", "Add composite variable", icon = icon("plus"), class = "btn-primary"),
        actionButton("loadComposite", label = "Validate composite expressions", icon = icon("check")))

  ),

#------------------------------------------------------------------------------#
# Row 1E: Configuration file (expert mode / reproducibility) -------------------
#------------------------------------------------------------------------------#
# About: Shows the options.R produced from the form (or restored from a saved run / #
# uploaded file). It can be edited, validated, downloaded and run directly.     #
#------------------------------------------------------------------------------#

  tabPanel(

    title = "Configuration file",

    value = "ConfigTab",

    fluidRow(
      column(width = 8,
             textAreaInput("config_text", label = NULL, value = "", rows = 24, width = "100%",
                           placeholder = "# options.R will appear here (press 'Fit model & forecast', open a saved run, or upload a file)")),
      column(width = 4,
             div(style = "display: flex; flex-direction: column; gap: 8px;",
                 actionButton("validate_config", "Validate configuration", icon = icon("check")),
                 downloadButton("download_config", "Download options.R"),
                 prettyCheckbox("config_mode",
                                "Run from this configuration file (instead of the form)",
                                value = FALSE, status = "primary", bigger = TRUE)),
             uiOutput("config_status"),
             tags$hr(),
             fileInput("upload_config",
                       tags$span("Upload an options.R file ",
                                 tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#555;",
                                        title = "A configuration downloaded from this app or written for the R toolbox (the data file must be loaded separately in step 1).")),
                       accept = c(".R", ".r", ".txt")),
             actionButton("load_config", "Load the uploaded file", icon = icon("file-code")))
    )

  ),

#------------------------------------------------------------------------------#
# Row 1F: Run history (completed runs saved on disk) -----------------------------
#------------------------------------------------------------------------------#
# About: Every completed run is saved in the runs/ folder next to app.R           #
# (options.R, ode_model.stan, data.csv, figures/ PNG, tables/ CSV; the fitted     #
# objects are cached in stan_cache/fits so the results can be shown again).      #
# Saved runs can be re-opened (results + configuration), downloaded or deleted.  #
#------------------------------------------------------------------------------#

  tabPanel(

    title = tags$span(icon("clock-rotate-left"), " Run history"),

    value = "RunHistory",

    div(class = "bff-add-bar",
        actionButton("refresh_runs", "Refresh list", icon = icon("rotate"),
                     title = "Runs are saved automatically in the 'runs' folder of the app (options.R, ode_model.stan, data.csv); the fitted objects are cached so that 'Open' shows the results without refitting.")),

    uiOutput("run_history_ui")

  )

  ) # End of 'tabBox'

    ), # End of the left column

#------------------------------------------------------------------------------#
# Right column: data preview, fit status and readiness checklist ----------------
#------------------------------------------------------------------------------#
# About: The checklist tells the user which steps are complete and what is      #
# still missing before the model can be fitted. The data preview shows the      #
# uploaded time series with the calibration / forecast windows.                #
#------------------------------------------------------------------------------#

    column(width = 4,
           # Live status of a model fit (background processes), with Cancel
           uiOutput("run_status_box"),
           uiOutput("readiness_box"),
           uiOutput("data_preview_box"))

  ), # End of the two-column row

#------------------------------------------------------------------------------#
# Results (only shown once a model has been fitted) ----------------------------
#------------------------------------------------------------------------------#

  conditionalPanel(
    condition = "output.has_results == true",

    ############################
    # Convergence / diagnostics #
    ############################
    box(
      width = 12,
      title = "Convergence & Diagnostics",
      uiOutput("convergence_card")
    ),

    ########################################
    # Creating the box to hold the figures #
    ########################################
    box(

      # Box Width
      width = 12,

      title = "Figures",

      fluidRow(
        column(width = 12, uiOutput("figure_area"))
      ),

      fluidRow(

        column(
          width = 10,
          div(style = "display: flex; flex-wrap: wrap; gap: 10px; align-items: flex-end;",
              uiOutput("selectFigureALL"),
              uiOutput("select.calibration.period"),
              div(style = "margin-bottom: 15px;", uiOutput("download.figures"))
          )
        ),

        column(
          width = 2,
          div(style = "display: flex; justify-content: flex-end; align-items: center; margin-top: 25px; gap: 6px;",
              uiOutput("arrows.figures.previous"),
              uiOutput("arrows.figures.next"))
        )

      )

    ), # End of Figures Box

    ########################################
    # Creating the box to hold the tables  #
    ########################################
    box(

      # Width of box
      width = 12,

      # Title
      title = "Tables",

      column(
        width = 12,
        h4(textOutput("forecastDataTitle")),
        DT::dataTableOutput("dataTableRender")
      ),

      fluidRow(

        column(
          width = 10,
          div(style = "display: flex; flex-wrap: wrap; gap: 10px; align-items: flex-end;",
              uiOutput("selectDatall"),
              div(style = "margin-bottom: 15px;", uiOutput("download.data")),
              div(style = "margin-bottom: 15px;", uiOutput("download.everything"))
          )
        ),

        column(
          width = 2,
          div(style = "display: flex; justify-content: flex-end; align-items: center; margin-top: 25px; gap: 6px;",
              uiOutput("arrows.data.previous"),
              uiOutput("arrows.data.next"))
        )

      )

    ) # End of box

  ) # End of conditionalPanel (results)

  ) # End of dashboard body

) # End of UI side of dashboard


#------------------------------------------------------------------------------#
# Creating the server side of the dashboard ------------------------------------
#------------------------------------------------------------------------------#
# About: Below includes the code for the server side of BayesianFitForecast.   #
# It is the backbone behind the UI above.                                      #
#------------------------------------------------------------------------------#
server <- function(input, output, session) {
  
#------------------------------------------------------------------------------#
# Determining if the composite box should show ---------------------------------
#------------------------------------------------------------------------------#
# About: This section determines if the composite tab should show. It will     #
# show if the user selects more than zero composite variables.                 #
#------------------------------------------------------------------------------#

  #################################################
  # Keyed rows (state variables, parameters, composite variables)              #
  #################################################
  # Rows are added with 'Add' buttons and removed individually. Every row has a
  # stable key (s1, s2, ... / p1, p2, ... / c1, c2, ...) used in its input ids, so
  # removing a row in the middle never shifts the values of the other rows and
  # the equations keep pointing at the right variables (the ODE builder stores
  # the keys). The counters only ever increase: a key is never re-used, which
  # guarantees that a new row never inherits the stale value of a deleted one.
  row_counters <- new.env(parent = emptyenv())
  row_counters$state <- 0L; row_counters$param <- 0L; row_counters$composite <- 0L
  next_key <- function(kind) {
    row_counters[[kind]] <- row_counters[[kind]] + 1L
    paste0(substr(kind, 1, 1), row_counters[[kind]])
  }

  state_rows      <- reactiveValues(keys = character(0))   # state variables, in order
  user_param_keys <- reactiveVal(character(0))             # user-defined parameters (required ones are automatic)
  composite_rows  <- reactiveValues(keys = character(0))   # composite variables

  # Defaults of rows created by the model library (consulted when a row is built)
  state_defaults    <- new.env(parent = emptyenv())   # by state key
  param_defaults    <- new.env(parent = emptyenv())   # by parameter key (user rows)
  required_defaults <- new.env(parent = emptyenv())   # by parameter NAME (rows found in the initial conditions)
  composite_defaults <- new.env(parent = emptyenv())  # by composite key
  template_gen    <- reactiveVal(0L)                  # bumped when a model is applied: required rows get fresh keys
  active_template <- reactiveVal(NULL)                # id of the library model the form was filled from

  vars_n          <- reactive({ length(state_rows$keys) })
  num_composite_n <- reactive({ length(composite_rows$keys) })

  output$state_count_hint <- renderUI({
    n <- length(state_rows$keys)
    span(class = "bff-hint", if (n == 0) "No state variable yet: press 'Add state variable' (one per compartment) or choose a model in the Model library." else
      sprintf("%d state variable%s (one equation each in step 7).", n, if (n == 1) "" else "s"))
  })

  output$param_count_hint <- renderUI({
    n_req <- length(req.params.react()); n_user <- length(user_param_keys())
    span(class = "bff-hint", sprintf("%d from the initial conditions, %d added.", n_req, n_user))
  })
  
#------------------------------------------------------------------------------#
# Rendering the file input -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the 'fileInput' to the main UI. If an error      #
# is triggered at any point in the dashboard, including in the structure of    #
# the inputted data, this UI option will re-render.                            #    
#------------------------------------------------------------------------------#
  
  data_file_input <- function() {
    fileInput("cadfilename1", 
              tags$span("Load data (.xlsx or .csv)", # Shown label
                        # Creating the info circle 
                        tags$i(class = "glyphicon glyphicon-info-sign",
                               style = "color:#FFFFFF;",
                               title = paste("First column: time index 0, 1, 2, ...",
                                             "Following columns: cases1, cases2, ... (one per time series)."))),
              accept = c(".xlsx", ".xls", ".csv"),
              multiple = FALSE)
  }

  output$input.data <- renderUI({ data_file_input() })
  
  
#------------------------------------------------------------------------------#
# Reading in the data frame ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: The following section reads in the user-selected data file (Excel or   #
# CSV) and saves it under the reactive value 'data'. Example data sets loaded   #
# from the configuration menu are stored in the same reactive value.           #
#------------------------------------------------------------------------------#
  
  ###############################################
  # Creating the empty reactive value for files #
  ###############################################
  listTimeseries <- reactiveValues(data = NULL, name = NULL)

  read_data_file <- function(path, name) {
    ext <- tolower(tools::file_ext(name))
    if (ext == "csv") {
      utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
    } else {
      readxl::read_excel(path)
    }
  }

  ############################################
  # Observing changes in the reactive values #
  ############################################
  observeEvent(input$cadfilename1,{
    
    files <- input$cadfilename1
    req(files)

    tryCatch({
      data <- read_data_file(files$datapath[1], files$name[1])
      listTimeseries$data <- as.data.frame(data)
      listTimeseries$name <- files$name[1]
    }, error = function(e){
      shinyalert::shinyalert(title = "Error!",
                             text = paste("The file could not be read:", conditionMessage(e)),
                             type = "error")
      listTimeseries$data <- NULL
      listTimeseries$name <- NULL
    }) # End of 'tryCatch'
    
  }) # End of 'observeEvent'
  

#------------------------------------------------------------------------------#
# Data Checks ------------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section completes file checks to ensure that the data is         #
# formatted correctly. If it is not, the user will receive an error and the    #
# reactive value storing the data will clear.                                  #
#------------------------------------------------------------------------------#
  
  #############################################
  # Final data reactive value to proceed with #
  #############################################
  finalData <- reactiveValues(data = NULL)

  #############################################
  # Creating the observe" for the error check #
  #############################################
  observe({
    
    # Requiring a file to be loaded
    req(listTimeseries$data)
    
    # Calling the function
    errorResult <- error.checking(original.data = listTimeseries$data)
    
    ##################################################
    # Determining if an error should show, and which #
    ##################################################
    if(errorResult == "ERROR1"){

      shinyalert::shinyalert(title = "Data format problem",
                             text = paste("The first column must be the time index 0, 1, 2, ... (no gaps, no repeats,",
                                          "starting at 0). Please fix the file and upload it again."),
                             type = "error")
      listTimeseries$data <- NULL
      finalData$data <- NULL
      output$input.data <- renderUI({ data_file_input() })
      
    }else if(errorResult == "ERROR2"){
      
      shinyalert::shinyalert(title = "Data format problem",
                             text = paste("Column headers must be 'cases1' for the first time series, then",
                                          "'cases2', 'cases3', ... for additional series (after the time column)."),
                             type = "error")
      listTimeseries$data <- NULL
      finalData$data <- NULL
      output$input.data <- renderUI({ data_file_input() })
      
    }else{

      finalData$data <- listTimeseries$data

     }
    
  })
  
#------------------------------------------------------------------------------#
# Creating the process of interest input ---------------------------------------
#------------------------------------------------------------------------------#
  
  output$process.of.interest <- renderUI({
    
    req(finalData$data)
    
    textInput("caddisease", 
              tags$span("Process of interest ", # Shown label
                        tags$i(class = "glyphicon glyphicon-info-sign",
                               style = "color:#FFFFFF;",
                               title = "What the time series describes, e.g. 'COVID-19' or 'Influenza 1918'. Used in file names and titles.")),
              value = isolate(input$caddisease) %||% "",
              placeholder = "e.g. COVID-19")
  })

  # Effective process name (falls back to the data file name)
  caddisease_value <- reactive({
    val <- trimws(input$caddisease %||% "")
    if (!nzchar(val)) {
      nm <- listTimeseries$name %||% "data"
      val <- tools::file_path_sans_ext(nm)
    }
    gsub("[^A-Za-z0-9_.-]+", "-", val)
  })
  
#------------------------------------------------------------------------------#
# Creating the file name options -----------------------------------------------
#------------------------------------------------------------------------------#
# About: Labels for the time axis and for every time series. They are used as   #
# axis labels in the figures; sensible defaults are used when left empty.       #
#------------------------------------------------------------------------------#
  
  output$seriescasesdatetype <- renderUI({
    
    req(finalData$data)
    
    num_inputs <- ncol(finalData$data)
    col_names <- colnames(finalData$data)
    
    inputs <- lapply(1:num_inputs, function(i) {
      textInput(
        inputId = paste0("text_input_", i), 
        label = if (i == 1) "Time unit (x-axis label)" else paste0("Label of time series ", i-1, " (", col_names[i], ")"),
        value = isolate(input[[paste0("text_input_", i)]]) %||% "",
        placeholder = if (i == 1) "e.g. Days" else "e.g. New cases"
      )
    })
    
    tagList(inputs)
    
  })
      
#------------------------------------------------------------------------------#
# Creating the `series_cases` and `datetype` reactive values -------------------
#------------------------------------------------------------------------------#
  
  series_cases_temp <- reactiveValues()
  date_type_temp <- reactiveValues()
  
  observe({
    
    req(finalData$data)
    
    n_cols <- ncol(finalData$data)
    temporal <- trimws(input[["text_input_1"]] %||% "")
    date_type_temp$temporal <- if (nzchar(temporal)) temporal else "Time"

    labels <- vapply(seq_len(n_cols - 1), function(k) {
      lab <- trimws(input[[paste0("text_input_", k + 1)]] %||% "")
      if (nzchar(lab)) lab else paste0("Time series ", k)
    }, character(1))
    series_cases_temp$labels <- labels
    
  })
    
  
#------------------------------------------------------------------------------#
# Calibration period length(s) --------------------------------------------------
#------------------------------------------------------------------------------#
# About: One or several calibration lengths (number of time points used for     #
# fitting). Several lengths can be given separated by commas (e.g. 15, 17, 20)  #
# or as a range (e.g. 15:20); the model is fitted once per length.             #
#------------------------------------------------------------------------------#
  
  # Compact text for a set of lengths: 15, 16, 17, 20 -> "15:17, 20"
  bff_compact_lengths <- function(v) {
    v <- sort(unique(as.integer(v)))
    if (length(v) == 0) return("")
    runs <- split(v, cumsum(c(1, diff(v) != 1)))
    paste(vapply(runs, function(r) if (length(r) >= 3) sprintf("%d:%d", r[1], r[length(r)]) else paste(r, collapse = ", "), character(1)), collapse = ", ")
  }

  output$calibrationPeriodLength <- renderUI({
    
    req(finalData$data)
    n_rows <- nrow(finalData$data)
    choices <- seq(3, n_rows)
    current <- suppressWarnings(as.integer(isolate(input$calibration_select)))
    current <- current[!is.na(current) & current %in% choices]
    if (length(current) == 0) current <- n_rows
    
    tagList(
      pickerInput("calibration_select",
                  tags$span("Calibration length(s) ",
                            tags$i(class = "glyphicon glyphicon-info-sign",
                                   style = "color:#FFFFFF;",
                                   title = paste("Number of time points used to fit the model. Select one or more lengths:",
                                                 "the model is fitted once per length and the results can be compared."))),
                  choices = choices, selected = current, multiple = TRUE,
                  options = list(`live-search` = TRUE, size = 8, `selected-text-format` = "count > 3",
                                 `none-selected-text` = "Choose one or more lengths", `actions-box` = FALSE)),
      uiOutput("calibration_feedback")
    )
    
  })

  # Selected lengths (sorted, unique)
  calibration_periods <- reactive({
    req(finalData$data)
    n_rows <- nrow(finalData$data)
    vals <- suppressWarnings(as.integer(input$calibration_select))
    vals <- sort(unique(vals[!is.na(vals)]))
    if (length(vals) == 0) return(list(values = NULL, error = "Choose at least one calibration length."))
    if (any(vals < 3)) return(list(values = NULL, error = "A calibration length must be at least 3 time points."))
    if (any(vals > n_rows)) return(list(values = NULL, error = sprintf("A calibration length cannot exceed the %d time points in the data.", n_rows)))
    list(values = vals, error = NULL)
  })

  output$calibration_feedback <- renderUI({
    req(finalData$data)
    parsed <- calibration_periods()
    if (!is.null(parsed$error)) {
      div(style = "color:#f39c12; font-size: 12px; padding: 0 15px 6px 15px;", icon("exclamation-triangle"), parsed$error)
    } else {
      div(style = "color:#b8c7ce; font-size: 12px; padding: 0 15px 6px 15px;",
          if (length(parsed$values) > 1) sprintf("%d lengths (%s), one fit each", length(parsed$values), bff_compact_lengths(parsed$values)) else
            sprintf("%d of %d time points used for fitting", parsed$values, n_rows_data()))
    }
  })

  n_rows_data <- reactive({ if (is.null(finalData$data)) 0 else nrow(finalData$data) })
  
#------------------------------------------------------------------------------#
# Forecasting horizon ----------------------------------------------------------
#------------------------------------------------------------------------------#
  
  output$forecast.horizon <- renderUI({
    
    req(finalData$data)
    
    numericInput("forecastinghorizon",
                 tags$span("Forecasting horizon ",
                           tags$i(class = "glyphicon glyphicon-info-sign",
                                  style = "color:#FFFFFF;",
                                  title = "Number of time points predicted beyond the calibration period (0 = fit only).")),
                 value = isolate(input$forecastinghorizon) %||% 0, min = 0, step = 1)
    
  })

  forecasting_horizon <- reactive({
    h <- suppressWarnings(as.integer(input$forecastinghorizon))
    if (length(h) == 0 || is.na(h) || h < 0) 0L else h
  })

#------------------------------------------------------------------------------#
# Data preview -----------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Interactive plot of the uploaded series with the calibration window    #
# and forecast horizon marked, so that the settings can be checked visually.    #
#------------------------------------------------------------------------------#

  preview_max_charts <- 8

  # Calibration lengths and horizon used by the preview (configuration file when running from it)
  preview_settings <- reactive({
    cal <- tryCatch(calibration_periods(), error = function(e) list(values = NULL, error = "no data"))
    h <- forecasting_horizon()
    if (isTRUE(input$config_mode) && !is.null(config_state$env)) {
      env <- config_state$env
      cal <- list(values = sort(unique(as.numeric(env$calibrationperiods))), error = NULL)
      h <- as.numeric(env$forecastinghorizon %||% 0)
    }
    list(values = if (is.null(cal$error)) cal$values else NULL, h = h)
  })

  output$data_preview_box <- renderUI({
    # The preview appears once data are loaded
    if (is.null(finalData$data)) return(NULL)
    ps <- preview_settings()
    vals <- ps$values
    if (is.null(vals) || length(vals) == 0) vals <- NA
    shown <- head(vals, preview_max_charts)
    charts <- lapply(shown, function(v) {
      id <- if (is.na(v)) "data_preview_plot_all" else paste0("data_preview_plot_", v)
      plotly::plotlyOutput(id, height = if (length(shown) > 1) "190px" else "210px")
    })
    box(width = 12, title = if (length(shown) > 1) sprintf("Data preview (%d calibration lengths)", length(vals)) else "Data preview",
        status = "primary", solidHeader = FALSE, collapsible = TRUE,
        charts,
        if (length(vals) > preview_max_charts) div(class = "bff-hint", sprintf("Showing the first %d of %d calibration lengths.", preview_max_charts, length(vals))) else NULL)
  })

  # One chart: all series, the calibration window and the forecast horizon
  preview_chart <- function(cal_value) {
    df <- finalData$data
    time_lab <- date_type_temp$temporal %||% "Time"
    labels <- series_cases_temp$labels %||% paste0("Time series ", seq_len(ncol(df) - 1))
    if (isTRUE(input$config_mode) && !is.null(config_state$env)) {
      env <- config_state$env
      if (!is.null(env$datetype)) time_lab <- as.character(env$datetype)
      sc <- as.character(env$series_cases %||% character(0))
      if (length(sc) > 0) labels[seq_len(min(length(sc), length(labels)))] <- sc[seq_len(min(length(sc), length(labels)))]
    }
    h <- preview_settings()$h
    p <- plotly::plot_ly()
    for (k in seq_len(ncol(df) - 1)) {
      p <- plotly::add_trace(p, x = df[[1]], y = df[[k + 1]], type = "scatter", mode = "lines+markers", name = labels[k],
                             marker = list(size = 4))
    }
    shapes <- list()
    title_txt <- NULL
    if (!is.na(cal_value)) {
      shapes[[length(shapes) + 1]] <- list(type = "rect", xref = "x", yref = "paper",
                                           x0 = min(df[[1]]), x1 = cal_value - 1, y0 = 0, y1 = 1,
                                           fillcolor = "rgba(0,166,90,0.08)", line = list(width = 0))
      if (h > 0) {
        shapes[[length(shapes) + 1]] <- list(type = "rect", xref = "x", yref = "paper",
                                             x0 = cal_value - 1, x1 = cal_value - 1 + h, y0 = 0, y1 = 1,
                                             fillcolor = "rgba(243,156,18,0.10)", line = list(width = 0))
      }
      shapes[[length(shapes) + 1]] <- list(type = "line", xref = "x", yref = "paper",
                                           x0 = cal_value - 1, x1 = cal_value - 1, y0 = 0, y1 = 1,
                                           line = list(color = "blue", dash = "dot"))
      title_txt <- list(text = sprintf("Calibration: %d time points%s", cal_value, if (h > 0) sprintf(", forecast: %d", h) else ""),
                        font = list(size = 12), x = 0.02, xanchor = "left")
    }
    plotly::layout(p, xaxis = list(title = time_lab), yaxis = list(title = "Value"), title = title_txt,
                   shapes = shapes, legend = list(orientation = "h", y = -0.3, font = list(size = 10)),
                   margin = list(t = 28, b = 40, l = 50, r = 10))
  }

  # Register one plot output per calibration length (ids are stable, so re-registering is harmless)
  observe({
    req(finalData$data)
    ps <- preview_settings()
    vals <- ps$values
    if (is.null(vals) || length(vals) == 0) {
      output[["data_preview_plot_all"]] <- plotly::renderPlotly({ preview_chart(NA) })
      return(invisible(NULL))
    }
    for (v in head(vals, preview_max_charts)) {
      local({
        vv <- v
        output[[paste0("data_preview_plot_", vv)]] <- plotly::renderPlotly({ preview_chart(vv) })
      })
    }
  })
  
#------------------------------------------------------------------------------#
# Adding the variable inputs --------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the inputs for the variable select by the user.  #
# For each variable selected, the user can specify a variety of information.   #
#------------------------------------------------------------------------------#
  
  # Default compartment names offered in the drop-down (any other name can be typed)
  default_state_names <- c("S", "E", "I", "R", "C", "D", "H", "U", "A", "P", "Q", "V", "W", "X", "Y", "Z",
                           "B", "F", "G", "J", "K", "L", "M", "N", "O", "T")

  ##################################################################
  # Names of the state variables (in row order)                     #
  ##################################################################
  selectedValues <- reactive({
    keys <- state_rows$keys
    if (length(keys) == 0) return(character(0))
    vapply(keys, function(k) {
      v <- input[[paste0("varsID_", k)]]
      if (is.null(v) || length(v) == 0) "" else trimws(as.character(v[1]))
    }, character(1), USE.NAMES = FALSE)
  })

  # One UI row per state variable (keyed by `key`); `d` holds optional defaults
  # (name, ic, fit, series, diff) when the row comes from the model library.
  state_row_ui <- function(key, d = list()) {
    name <- d$name %||% ""
    div(
      id = paste0("varsLine_", key),
      class = "stateRow bff-grid-row",
      div(class = "bff-cell",
          selectizeInput(paste0("varsID_", key), label = NULL,
                         choices = unique(c(name, default_state_names)), selected = name,
                         options = list(create = TRUE, createOnBlur = TRUE, maxItems = 1, placeholder = "Select or type"))),
      div(class = "bff-cell", bff_math_input(paste0("Ic_", key), label = NULL, value = d$ic %||% "")),
      div(class = "bff-cell", selectInput(paste0("fitToData_", key), label = NULL, choices = c("Yes", "No"),
                                          selected = if (isTRUE(d$fit)) "Yes" else "No")),
      div(class = "bff-cell", uiOutput(paste0("fittingIndex_", key))),
      div(class = "bff-cell", uiOutput(paste0("fittingDiff_", key))),
      div(class = "bff-cell bff-cell-center",
          actionButton(paste0("rm_state_", key), label = NULL, icon = icon("trash"),
                       class = "btn-default btn-sm", title = "Remove this state variable"))
    )
  }

  state_remove_observers <- new.env(parent = emptyenv())

  add_state_row <- function(d = list()) {
    key <- next_key("state")
    if (is.null(d$name) || !nzchar(d$name)) {
      taken <- isolate(selectedValues())
      available <- setdiff(default_state_names, taken)
      d$name <- if (length(available) > 0) available[1] else paste0("X", row_counters$state)
    }
    assign(key, d, envir = state_defaults)
    insertUI(selector = "#inputContainer2", where = "beforeEnd", ui = state_row_ui(key, d), immediate = TRUE)
    state_rows$keys <- c(isolate(state_rows$keys), key)
    # remove button (one observer per row, created once)
    if (is.null(state_remove_observers[[key]])) {
      state_remove_observers[[key]] <- observeEvent(input[[paste0("rm_state_", key)]], {
        remove_state_row(key)
        active_template(NULL)
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
    }
    key
  }

  remove_state_row <- function(key) {
    removeUI(selector = paste0("#varsLine_", key), immediate = TRUE)
    state_rows$keys <- setdiff(isolate(state_rows$keys), key)
    yes.no.state$state[[key]] <- NULL
    labels.fit.state$state[[key]] <- NULL
    vector.fit.diff.state$state[[key]] <- NULL
  }

  clear_state_rows <- function() {
    for (k in isolate(state_rows$keys)) removeUI(selector = paste0("#varsLine_", k), immediate = TRUE)
    state_rows$keys <- character(0)
    yes.no.state$state <- list(); labels.fit.state$state <- list(); vector.fit.diff.state$state <- list()
  }

  observeEvent(input$add_state, {
    add_state_row()
    active_template(NULL)   # the model no longer matches a library model exactly
  })

  ###################################################
  # Clean typed state-variable names                 #
  ###################################################
  # Typed names are turned into valid identifiers (letters/digits, starting with
  # a letter). Duplicated names are reported by the checklist / validation
  # instead of being prevented by trimming the drop-downs of the other rows
  # (that approach produced a race between the user's choice and the update).
  sanitize_state_name <- function(x) {
    x <- trimws(as.character(x))
    x <- gsub("[^A-Za-z0-9_]", "", x)
    x <- gsub("_", "", x)
    if (!nzchar(x)) return(x)
    if (!grepl("^[A-Za-z]", x)) x <- paste0("X", x)
    x
  }

  observe({
    keys <- state_rows$keys
    for (k in keys) {
      current_value <- input[[paste0("varsID_", k)]]
      if (is.null(current_value) || !nzchar(current_value)) next
      clean_value <- sanitize_state_name(current_value)
      if (!identical(clean_value, current_value) && nzchar(clean_value)) {
        updateSelectizeInput(session, paste0("varsID_", k),
                             choices = unique(c(clean_value, default_state_names)),
                             selected = clean_value,
                             options = list(create = TRUE, createOnBlur = TRUE, maxItems = 1))
      }
    }
  })

  # Duplicated state-variable names (reported in the checklist and at validation)
  duplicated_state_names <- reactive({
    nm <- selectedValues()
    nm <- nm[nzchar(nm)]
    unique(nm[duplicated(nm)])
  })

#------------------------------------------------------------------------------#
# Determining initial values ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section determines how the initial value entry should be treated #
# for the state variables. If the initial values are known and fixed, then the #
# results are saved within a vector. However, if an initial condition must be  #
# estimated, it places it within a string (double quotes) and then creates the #
# vector.                                                                      #
#------------------------------------------------------------------------------#
  
   # Reactive container for initial conditions
   initial.conditions.vec <- reactiveValues()
   
   #############################################
   # Creating the vector of initial conditions #
   #############################################
   # The initial conditions are captured live (not only when switching tabs).
   observe({

     keys <- state_rows$keys
     ic <- lapply(keys, function(k) {
       val <- input[[paste0("Ic_", k)]]
       if (is.null(val)) "" else trimws(as.character(val))
     })
     initial.conditions.vec$IC <- ic

   })
    
#------------------------------------------------------------------------------#
# Creating "Select Column" option (Fitting Index) ------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the user input for selecting the column to fit   #
# to state variable. The option will only appear for "yes" options above. The  #
# option will show the column options (based on series_cases) for fitting the  #
# index. This section also creates a reactive value storing the vector of yes  #
# and no selections, and columns to be fitted to. This is used to set-up the   #
# fitting index option for the 'options' file.                                 # 
#------------------------------------------------------------------------------#
  
  #########################################
  # Reactive values for the fit mapping    #
  #########################################
  yes.no.state          <- reactiveValues(state = list())   # by state key: 1 = fitted to data
  labels.fit.state      <- reactiveValues(state = list())   # by state key: index of the fitted series
  vector.fit.diff.state <- reactiveValues(state = list())   # by state key: 1 = fit the derivative

  # Observers are created once per row key (a key is never re-used)
  fit_observers_created <- reactiveVal(character(0))

  observe({

    keys <- state_rows$keys
    already <- isolate(fit_observers_created())
    new_keys <- setdiff(keys, already)
    if (length(new_keys) == 0) return(invisible(NULL))
    fit_observers_created(c(already, new_keys))

    lapply(new_keys, function(k) {

      d <- get0(k, envir = state_defaults, ifnotfound = list())

      ###############################################
      # Tracking changes in the "Yes"/"No" selector #
      ###############################################
      observeEvent(input[[paste0("fitToData_", k)]], {

        if (identical(input[[paste0("fitToData_", k)]], "Yes")) {

          output[[paste0("fittingIndex_", k)]] <- renderUI({
            labels <- series_cases_temp$labels
            req(labels)
            sel <- isolate(input[[paste0("additional_input_", k)]]) %||% d$series
            if (!is.null(sel) && !(as.character(sel) %in% as.character(seq_along(labels)))) sel <- NULL
            selectInput(paste0("additional_input_", k),
                        label = NULL,
                        choices = stats::setNames(seq_along(labels), labels),
                        selected = sel)
          })

          output[[paste0("fittingDiff_", k)]] <- renderUI({
            selectInput(paste0("fittingToDiff_", k),
                        label = NULL,
                        choices = c("Yes", "No"),
                        selected = isolate(input[[paste0("fittingToDiff_", k)]]) %||%
                          (if (!is.null(d$diff)) (if (isTRUE(d$diff)) "Yes" else "No") else "Yes"))
          })
          outputOptions(output, paste0("fittingIndex_", k), suspendWhenHidden = FALSE)
          outputOptions(output, paste0("fittingDiff_", k), suspendWhenHidden = FALSE)

          yes.no.state$state[[k]] <- 1L

        } else {

          output[[paste0("fittingIndex_", k)]] <- renderUI(span(class = "bff-hint", "\u2014"))
          output[[paste0("fittingDiff_", k)]]  <- renderUI(span(class = "bff-hint", "\u2014"))
          yes.no.state$state[[k]] <- 0L

        }

      }, ignoreNULL = TRUE)

      ########################################
      # Tracking changes in additional input #
      ########################################
      observeEvent(input[[paste0("additional_input_", k)]], {
        labels.fit.state$state[[k]] <- suppressWarnings(as.integer(input[[paste0("additional_input_", k)]]))
      }, ignoreNULL = TRUE)

      ########################################
      # Tracking the "fit to derivative" flag #
      ########################################
      observeEvent(input[[paste0("fittingToDiff_", k)]], {
        vector.fit.diff.state$state[[k]] <- if (identical(input[[paste0("fittingToDiff_", k)]], "Yes")) 1L else 0L
      }, ignoreNULL = TRUE)

    }) # End of 'lapply'

  }) # End of observe
  

#------------------------------------------------------------------------------#
# Creating the `fitting_index` and `fitting_diff` vectors ----------------------
#------------------------------------------------------------------------------#
# About: fitting_index[j] is the state variable fitted to the j-th data column   #
# (cases_j) and fitting_diff[j] says whether its derivative is fitted. Both are  #
# built in DATA-COLUMN order (the previous version built fitting_diff in row    #
# order, which mismatched the series when rows were not in column order).      #
# Problems (unassigned or doubly-assigned series) are reported in the checklist.#
#------------------------------------------------------------------------------#
  
  fitting_index_values <- reactiveValues(fit = NULL, diff = NULL, problems = character(0), n_assigned = 0)

  observe({

    keys <- state_rows$keys
    n <- length(keys)
    labels <- series_cases_temp$labels
    n_series <- length(labels)

    pick <- function(lst, fill) {
      vapply(keys, function(k) { v <- lst[[k]]; if (is.null(v) || length(v) == 0 || is.na(v[1])) fill else as.integer(v[1]) }, integer(1), USE.NAMES = FALSE)
    }
    yes_vec    <- pick(yes.no.state$state, 0L)
    series_vec <- pick(labels.fit.state$state, NA_integer_)
    diff_vec   <- pick(vector.fit.diff.state$state, 1L)

    yes_rows <- which(yes_vec == 1L)
    problems <- character(0)
    fit <- integer(0)
    dif <- integer(0)

    if (n_series > 0) {
      assigned <- vapply(seq_len(n_series), function(k) sum(series_vec[yes_rows] == k, na.rm = TRUE), numeric(1))
      for (k in seq_len(n_series)) {
        rows <- yes_rows[which(series_vec[yes_rows] == k)]
        if (length(rows) > 1) {
          problems <- c(problems, sprintf("'%s' is assigned to %d state variables; assign it to one only.", labels[k], length(rows)))
        } else if (length(rows) == 1) {
          fit <- c(fit, rows)
          dif <- c(dif, diff_vec[rows])
        } else if (any(assigned[seq_len(n_series) > k] > 0)) {
          # a later series is assigned while this one is not: columns must be used in order
          problems <- c(problems, sprintf("'%s' (cases%d) must be assigned before later time series are used.", labels[k], k))
        }
      }
      if (length(yes_rows) > 0 && any(is.na(series_vec[yes_rows]))) {
        problems <- c(problems, "Choose the time series to fit for every state variable marked 'Yes'.")
      }
    }

    fitting_index_values$n_assigned <- length(fit)
    fitting_index_values$problems <- problems
    if (length(problems) == 0 && length(fit) > 0) {
      fitting_index_values$fit  <- fit
      fitting_index_values$diff <- dif
    } else {
      fitting_index_values$fit  <- NULL
      fitting_index_values$diff <- NULL
    }
    
  })

  # Fitting-diff vector in data-column order (used by the ODE builder and the options file)
  fitting_diff_vector <- reactive({ fitting_index_values$diff %||% integer(0) })
  
#------------------------------------------------------------------------------#
# Error Checking: At least one fitting index is selected -----------------------
#------------------------------------------------------------------------------#
  
  observeEvent(input$EQUATIONS, ignoreInit = TRUE, {
    
    req(series_cases_temp$labels)
    
    if(input$EQUATIONS %in% c("paramsVariables", "ODESys")){
      
      if(is.null(fitting_index_values$fit)){
        msg <- if (length(fitting_index_values$problems) > 0) {
          paste(fitting_index_values$problems, collapse = "\n")
        } else {
          "The data must be assigned to at least one state variable (set 'Fit to data' to Yes in step 4)."
        }
        showNotification(ui = tagList(strong("Data not assigned yet"), br(), msg), type = "warning", duration = 8)
      }
      
    }

  })
  
#------------------------------------------------------------------------------#
# Determining what are the "required" parameters -------------------------------
#------------------------------------------------------------------------------#
# About: This section looks at the list of initial conditions and determines   #
# if any included options are characters. If so, it then adds the parameters   #
# to the list of parameters that the user needs to specify.                    #
#------------------------------------------------------------------------------#
  
  #####################################
  # List to store required parameters #
  #####################################
  req.param.list <- reactiveValues()
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  # Identifiers that are not parameters (functions / constants allowed in initial conditions)
  ic_reserved_words <- c("exp", "log", "sqrt", "pow", "pi", "e", "abs", "max", "min", "fmax", "fmin")

  # Translate an initial-condition expression (LaTeX from the math field) into an
  # R/Stan expression with internal identifiers: "N-i_{0}" -> "N - i0",
  # "\\frac{N}{2}" -> "N / 2", "N\\cdot(1-\\rho)" -> "N * (1 - rho)".
  ic_to_expression <- function(ic_value) {
    txt <- trimws(as.character(ic_value %||% ""))
    if (!nzchar(txt)) return("")
    r <- tryCatch(paste0(latex2r(txt)), error = function(e) NA_character_)
    if (is.na(r) || !nzchar(r)) r <- gsub("[\\\\{}]", "", txt)
    r <- gsub("\\\\", "", r)
    toks <- unique(regmatches(r, gregexpr("[A-Za-z][A-Za-z0-9_]*", r))[[1]])
    for (tok in toks) {
      clean <- gsub("_", "", tok)
      if (!identical(clean, tok)) r <- gsub(paste0("\\b", tok, "\\b"), clean, r, perl = TRUE)
    }
    trimws(r)
  }

  # Extract the parameter names used in an initial-condition expression
  extract_ic_parameters <- function(ic_value) {
    r <- ic_to_expression(ic_value)
    if (!nzchar(r)) return(character(0))
    toks <- regmatches(r, gregexpr("[A-Za-z][A-Za-z0-9_]*", r))[[1]]
    toks <- toks[!tolower(toks) %in% ic_reserved_words]
    unique(toks)
  }

  observe({

    tempList <- c()
    temp.IC <- initial.conditions.vec$IC

    for(i in seq_along(temp.IC)){
      tempList <- unique(c(tempList, extract_ic_parameters(temp.IC[[i]])))
    }
    
    ###############################################
    # Adding the final list to the reactive value #
    ###############################################
    req.param.list$list <- tempList
    
    })
  
#------------------------------------------------------------------------------#
# Parameter rows (identity-keyed) -----------------------------------------------
#------------------------------------------------------------------------------#
# About: One row per parameter. Parameters found in the initial conditions     #
# ("required" parameters, e.g. N or i0) get a row keyed by their name; the     #
# user-defined parameters get rows keyed by their position (user_1, user_2,..). #
# Rows are therefore never re-labelled when the initial conditions or the      #
# number of parameters change (the previous index-based rows silently moved    #
# priors and fixed values from one parameter to another).                      #
#------------------------------------------------------------------------------#

  req.params.react <- reactive({ req.param.list$list %||% character(0) })

  # Prior catalogue: Stan name, argument labels and defaults, truncation default
  prior_catalog <- list(
    "Normal"        = list(stan = "normal",         args = c("Mean", "SD"),           defaults = c(1, 2),    trunc = TRUE),
    "Uniform"       = list(stan = "uniform",        args = c("Min", "Max"),           defaults = c(0, 1),    trunc = FALSE),
    "Beta"          = list(stan = "beta",           args = c("Alpha", "Beta"),        defaults = c(2, 2),    trunc = FALSE),
    "Cauchy"        = list(stan = "cauchy",         args = c("Location", "Scale"),    defaults = c(0, 5),    trunc = TRUE),
    "Gamma"         = list(stan = "gamma",          args = c("Shape", "Rate"),        defaults = c(2, 0.5),  trunc = FALSE),
    "Exponential"   = list(stan = "exponential",    args = c("Rate"),                 defaults = c(0.1),     trunc = FALSE),
    "Lognormal"     = list(stan = "lognormal",      args = c("Mean (log)", "SD (log)"), defaults = c(0, 1),  trunc = FALSE),
    "Neg. Binomial" = list(stan = "neg_binomial_2", args = c("Mean", "Dispersion"),   defaults = c(10, 2),   trunc = FALSE),
    "Poisson"       = list(stan = "poisson",        args = c("Rate"),                 defaults = c(0.1),     trunc = FALSE),
    "Custom"        = list(stan = NA,               args = character(0),              defaults = numeric(0), trunc = FALSE)
  )
  prior_choices <- names(prior_catalog)

  # Build the Stan prior string from a prior type and its arguments
  make_prior_string <- function(prior_type, args, custom_text = "", truncate0 = FALSE) {
    if (is.null(prior_type) || !(prior_type %in% prior_choices)) return(NA_character_)
    if (prior_type == "Custom") {
      txt <- trimws(as.character(custom_text %||% ""))
      if (!nzchar(txt) || !grepl("\\(", txt)) return(NA_character_)
      return(if (isTRUE(truncate0) && !grepl("T\\[", txt)) paste0(txt, "T[0,]") else txt)
    }
    spec <- prior_catalog[[prior_type]]
    args <- suppressWarnings(as.numeric(args))
    if (length(args) < length(spec$args) || any(is.na(args[seq_along(spec$args)]))) return(NA_character_)
    out <- sprintf("%s(%s)", spec$stan, paste(format(args[seq_along(spec$args)], trim = TRUE), collapse = ", "))
    if (isTRUE(truncate0)) out <- paste0(out, "T[0,]")
    out
  }

  # UI for the arguments of a prior (+ truncation and bounds for model parameters).
  # `d` holds the defaults of the row (model library): they are used only while
  # the inputs do not exist yet and only for the prior family they were made for.
  # Cells of the prior arguments (+ truncation and bounds for model parameters).
  # Always returns the same number of cells (4 with bounds, 1 without), so that
  # the rows of the grid stay aligned whatever the state of the row.
  prior_args_ui <- function(key, prior_type, prefix = "arg_", with_bounds = TRUE, trunc_id = NULL, d = list()) {
    spec <- prior_catalog[[prior_type]]
    empty <- div(class = "bff-cell")
    if (is.null(spec)) {
      return(if (with_bounds) tagList(empty, empty, empty, empty) else empty)
    }
    same_family <- identical(d$prior %||% "", prior_type)
    args_cell <- if (prior_type == "Custom") {
      div(class = "bff-cell",
          textInput(paste0(prefix, "1_", key), label = NULL,
                    value = isolate(input[[paste0(prefix, "1_", key)]]) %||% (if (same_family) d$custom else NULL) %||% "",
                    placeholder = "Stan distribution, e.g. normal(0, 5)"))
    } else {
      div(class = "bff-cell",
          div(class = "bff-args",
              lapply(seq_along(spec$args), function(a) {
                default_a <- if (same_family && length(d$args) >= a) d$args[a] else spec$defaults[a]
                div(class = "bff-arg",
                    div(class = "bff-arg-label", spec$args[a]),
                    numericInput(paste0(prefix, a, "_", key), label = NULL,
                                 value = isolate(input[[paste0(prefix, a, "_", key)]]) %||% default_a))
              })))
    }
    if (!with_bounds) return(args_cell)
    tagList(
      args_cell,
      div(class = "bff-cell bff-cell-center",
          prettyCheckbox(trunc_id, label = NULL,
                         value = isolate(input[[trunc_id]]) %||% (if (same_family) d$trunc else NULL) %||% spec$trunc, bigger = TRUE)),
      div(class = "bff-cell", textInput(paste0("LB_", key), label = NULL,
                                        value = isolate(input[[paste0("LB_", key)]]) %||% d$lb %||% "0")),
      div(class = "bff-cell", textInput(paste0("UB_", key), label = NULL,
                                        value = isolate(input[[paste0("UB_", key)]]) %||% d$ub %||% "NA"))
    )
  }

  #############################################
  # Row bookkeeping                            #
  #############################################
  param_rows <- reactiveValues(keys = character(0), labels = list())
  param_observers_created <- reactiveVal(character(0))
  param_remove_observers <- new.env(parent = emptyenv())

  # Key of the automatic row of a parameter found in the initial conditions. The
  # generation suffix changes whenever a model from the library is applied, so
  # that the rows are rebuilt with the defaults of that model.
  req_key <- function(name) paste0("req_", gsub("[^A-Za-z0-9_]", "", name), "_g", isolate(template_gen()))

  # Defaults of a parameter row (model library): user rows by key, required rows by name
  param_row_defaults <- function(key, label = NULL) {
    if (startsWith(key, "req_")) {
      nm <- label %||% sub("^req_(.*)_g[0-9]+$", "\\1", key)
      get0(nm, envir = required_defaults, ifnotfound = list())
    } else {
      get0(key, envir = param_defaults, ifnotfound = list())
    }
  }

  param_row_ui <- function(key, label, is_required) {
    d <- param_row_defaults(key, label)
    default_fixed <- if (!is.null(d$fixed)) (if (isTRUE(d$fixed) || identical(d$fixed, "Fixed")) "Fixed" else "Estimate") else
      (if (is_required) "Fixed" else "Estimate")
    name_cell <- if (is_required) {
      div(class = "bff-cell bff-cell-name", title = "Parameter found in the initial conditions (step 4); it is listed automatically.",
          withMathJax(paste0("\\(", label, "\\)")),
          span(class = "bff-hint", "from the initial conditions"))
    } else {
      div(class = "bff-cell", bff_math_input(paste0("paramsID_", key), label = NULL, value = d$name %||% ""))
    }
    div(
      id = paste0("paramsLine_", key),
      class = "paramRow bff-grid-row",
      name_cell,
      div(class = "bff-cell", pickerInput(paste0("fixed_", key), label = NULL, choices = c("Estimate", "Fixed"), selected = default_fixed)),
      div(class = "bff-cell", uiOutput(paste0("main.Prior_", key))),
      # 4 cells (arguments, truncation, lower bound, upper bound) rendered by the server
      uiOutput(paste0("priorOptions_", key), class = "bff-grid-row"),
      div(class = "bff-cell bff-cell-center",
          if (!is_required) actionButton(paste0("rm_param_", key), label = NULL, icon = icon("trash"),
                                         class = "btn-default btn-sm", title = "Remove this parameter") else NULL)
    )
  }

  # Outputs of one parameter row: they depend only on the row's own inputs, so a
  # change in another row never re-renders (and resets) this one.
  create_param_outputs <- function(key, label = NULL) {
    d <- param_row_defaults(key, label)
    output[[paste0("main.Prior_", key)]] <- renderUI({
      fx <- input[[paste0("fixed_", key)]]
      if (is.null(fx)) return(NULL)
      if (identical(fx, "Fixed")) {
        dv <- d$value; if (!is.null(dv) && (length(dv) != 1 || is.na(dv))) dv <- NULL
        numericInput(paste0("fixedval_", key), label = NULL,
                     value = isolate(input[[paste0("fixedval_", key)]]) %||% dv %||% 1)
      } else {
        selectizeInput(paste0("priors_", key), label = NULL, choices = prior_choices,
                       selected = isolate(input[[paste0("priors_", key)]]) %||% d$prior %||% "Normal",
                       multiple = FALSE, options = list(create = FALSE))
      }
    })
    output[[paste0("priorOptions_", key)]] <- renderUI({
      fx <- input[[paste0("fixed_", key)]]
      pt <- input[[paste0("priors_", key)]]
      empty <- div(class = "bff-cell")
      if (is.null(fx) || identical(fx, "Fixed") || is.null(pt) || !(pt %in% prior_choices)) {
        return(tagList(div(class = "bff-cell", if (identical(fx, "Fixed")) span(class = "bff-hint", "fixed value") else NULL), empty, empty, empty))
      }
      prior_args_ui(key, pt, prefix = "arg_", with_bounds = TRUE, trunc_id = paste0("truncateZero_", key), d = d)
    })
    # Render the inputs even while the tab is hidden, so that rows filled by the
    # model library are complete (checklist) without visiting the tab first
    outputOptions(output, paste0("main.Prior_", key), suspendWhenHidden = FALSE)
    outputOptions(output, paste0("priorOptions_", key), suspendWhenHidden = FALSE)
  }

  add_param_row <- function(d = list()) {
    key <- next_key("param")
    assign(key, d, envir = param_defaults)
    user_param_keys(c(isolate(user_param_keys()), key))
    if (is.null(param_remove_observers[[key]])) {
      param_remove_observers[[key]] <- observeEvent(input[[paste0("rm_param_", key)]], {
        user_param_keys(setdiff(isolate(user_param_keys()), key))
        active_template(NULL)
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
    }
    key
  }

  observeEvent(input$add_param, {
    add_param_row()
    active_template(NULL)
  })

  observe({
    required <- req.params.react()
    gen <- template_gen()
    req_keys <- if (length(required)) paste0("req_", gsub("[^A-Za-z0-9_]", "", required), "_g", gen) else character(0)
    user_keys <- user_param_keys()
    desired <- c(req_keys, user_keys)
    labels <- c(as.list(required), as.list(rep("", length(user_keys))))
    names(labels) <- desired

    current <- isolate(param_rows$keys)
    # Remove rows that are no longer needed
    for (k in setdiff(current, desired)) removeUI(selector = paste0("#paramsLine_", k), immediate = TRUE)
    # Insert new required rows at the top (in order), new user rows at the bottom
    new_req <- setdiff(req_keys, current)
    for (k in rev(new_req)) {
      insertUI(selector = "#inputContainer > .bff-grid-header", where = "afterEnd", ui = param_row_ui(k, labels[[k]], TRUE), immediate = TRUE)
    }
    new_user <- setdiff(user_keys, current)
    for (k in new_user) {
      insertUI(selector = "#inputContainer", where = "beforeEnd", ui = param_row_ui(k, "", FALSE), immediate = TRUE)
    }
    created <- isolate(param_observers_created())
    for (k in setdiff(c(new_req, new_user), created)) create_param_outputs(k, labels[[k]])
    param_observers_created(union(created, c(new_req, new_user)))

    param_rows$keys <- desired
    param_rows$labels <- labels
  })

#------------------------------------------------------------------------------#
# Error-structure parameters (phi for negative binomial, sigma for normal) -----
#------------------------------------------------------------------------------#
# About: One prior per fitted time series. The rows are keyed by the data       #
# column index (1..number of series in the data).                              #
#------------------------------------------------------------------------------#

  err_rows_created <- reactiveVal(character(0))
  # Row key of the error parameter of series k under error structure errnum: the
  # key includes the error structure so that switching it gives fresh inputs
  # (with the right defaults) instead of the stale values of the other structure.
  err_key <- function(k, errnum) paste0("e", errnum, "_", k)

  # Defaults of the error-structure priors (same as the toolbox examples):
  # negative binomial 1/phi ~ exponential(5); normal sigma ~ cauchy(0, 2.5)
  err_prior_defaults <- function(errnum) {
    if (errnum == 2) list(prior = "Cauchy", args = c(0, 2.5), trunc = TRUE) else list(prior = "Exponential", args = 5, trunc = FALSE)
  }

  err_row_ui <- function(k, symbol, label, d = list()) {
    div(
      id = paste0("paramsLineREQF_", k),
      class = "bff-grid-row",
      div(class = "bff-cell bff-cell-name",
          span(style = "font-size: 20px; font-weight: bold;", HTML(symbol)),
          span(class = "bff-hint", label)),
      div(class = "bff-cell",
          selectizeInput(paste0("Dist.priors_", k), label = NULL, choices = prior_choices,
                         selected = isolate(input[[paste0("Dist.priors_", k)]]) %||% d$prior %||% "Exponential",
                         multiple = FALSE, options = list(create = FALSE))),
      uiOutput(paste0("dist.priorOptions_", k), class = "bff-grid-row")
    )
  }

  observe({
    req(finalData$data)
    n_series <- ncol(finalData$data) - 1
    errnum <- errorStructureNumber()
    req(!is.null(errnum))
    # Rebuild the rows whenever the error structure or the number of series changes
    for (k in isolate(err_rows_created())) removeUI(selector = paste0("#paramsLineREQF_", k), immediate = TRUE)
    err_rows_created(character(0))
    if (errnum == 3) return(invisible(NULL))
    symbol <- if (errnum == 2) "&sigma;" else "1/&#x03D5;"
    label_txt <- if (errnum == 2) "SD of the normal errors" else "inverse over-dispersion"
    d_err <- err_prior_defaults(errnum)
    keys <- vapply(seq_len(n_series), err_key, character(1), errnum = errnum)
    for (k in seq_len(n_series)) {
      insertUI(selector = "#reqParamsF", where = "beforeEnd",
               ui = err_row_ui(keys[k], symbol, paste0(label_txt, " (series ", k, ")"), d = d_err), immediate = TRUE)
      local({
        kk <- keys[k]
        output[[paste0("dist.priorOptions_", kk)]] <- renderUI({
          pt <- input[[paste0("Dist.priors_", kk)]]
          if (is.null(pt)) return(div(class = "bff-cell"))
          prior_args_ui(kk, pt, prefix = "dist.arg_", with_bounds = FALSE, d = d_err)
        })
        outputOptions(output, paste0("dist.priorOptions_", kk), suspendWhenHidden = FALSE)
      })
    }
    err_rows_created(keys)
  })

  parameters.inital.dist.list <- reactiveValues(list = NULL)

  observe({
    req(finalData$data)
    errnum <- errorStructureNumber()
    if (is.null(errnum) || errnum == 3) { parameters.inital.dist.list$list <- character(0); return(invisible(NULL)) }
    n_series <- ncol(finalData$data) - 1
    out <- character(0)
    for (k in seq_len(n_series)) {
      ek <- err_key(k, errnum)
      pt <- input[[paste0("Dist.priors_", ek)]]
      if (is.null(pt)) { out <- c(out, NA_character_); next }
      args <- if (pt == "Custom") NULL else vapply(seq_along(prior_catalog[[pt]]$args), function(a) {
        v <- input[[paste0("dist.arg_", a, "_", ek)]]; if (is.null(v)) NA_real_ else as.numeric(v) }, numeric(1))
      out <- c(out, make_prior_string(pt, args, custom_text = input[[paste0("dist.arg_1_", ek)]], truncate0 = FALSE))
    }
    parameters.inital.dist.list$list <- out
  })

#------------------------------------------------------------------------------#
# Composite-variable rows ---------------------------------------------------------
#------------------------------------------------------------------------------#
# About: One row per composite variable (name + LaTeX expression).               #
#------------------------------------------------------------------------------#

  composite_remove_observers <- new.env(parent = emptyenv())

  composite_row_ui <- function(key, d = list()) {
    div(
      id = paste0("compositeLine_", key),
      class = "compositeRow bff-grid-row",
      div(class = "bff-cell", textInput(paste0("LeftEquation_", key), label = NULL, value = d$name %||% "", placeholder = "e.g. R0")),
      div(class = "bff-cell", bff_math_input(paste0("compositeID_", key), label = NULL, value = d$latex %||% "")),
      div(class = "bff-cell bff-cell-center",
          actionButton(paste0("rm_composite_", key), label = NULL, icon = icon("trash"),
                       class = "btn-default btn-sm", title = "Remove this composite variable"))
    )
  }

  add_composite_row <- function(d = list()) {
    key <- next_key("composite")
    assign(key, d, envir = composite_defaults)
    insertUI(selector = "#compositeEquationContainer", where = "beforeEnd", ui = composite_row_ui(key, d), immediate = TRUE)
    composite_rows$keys <- c(isolate(composite_rows$keys), key)
    if (is.null(composite_remove_observers[[key]])) {
      composite_remove_observers[[key]] <- observeEvent(input[[paste0("rm_composite_", key)]], {
        removeUI(selector = paste0("#compositeLine_", key), immediate = TRUE)
        composite_rows$keys <- setdiff(isolate(composite_rows$keys), key)
        composite.variable.list$list <- NULL
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
    }
    key
  }

  clear_composite_rows <- function() {
    for (k in isolate(composite_rows$keys)) removeUI(selector = paste0("#compositeLine_", k), immediate = TRUE)
    composite_rows$keys <- character(0)
    composite.variable.list$list <- NULL
  }

  observeEvent(input$add_composite, {
    add_composite_row()
  })

#------------------------------------------------------------------------------#
# Collecting the parameter table (names, fixed status, priors, bounds) ----------
#------------------------------------------------------------------------------#

  parameters.list       <- reactiveValues(list = NULL)   # display names (LaTeX allowed)
  parameters.fixed.list <- reactiveValues(list = NULL)   # 1 = fixed, 0 = estimated
  parameters.inital.list <- reactiveValues(list = NULL)  # prior string or fixed value
  parameters.LB.list    <- reactiveValues(list = NULL)
  parameters.UB.list    <- reactiveValues(list = NULL)
  parameters.complete   <- reactiveVal(FALSE)

  observe({
    keys <- param_rows$keys
    labels <- param_rows$labels
    names_out <- character(0); fixed_out <- numeric(0); prior_out <- character(0)
    lb_out <- character(0); ub_out <- character(0)
    complete <- length(keys) > 0

    for (k in keys) {
      is_required <- startsWith(k, "req_")
      nm <- if (is_required) labels[[k]] else trimws(as.character(input[[paste0("paramsID_", k)]] %||% ""))
      if (!nzchar(nm)) complete <- FALSE
      fx <- input[[paste0("fixed_", k)]]
      if (is.null(fx)) { complete <- FALSE; fx <- if (is_required) "Fixed" else "Estimate" }
      if (fx == "Fixed") {
        val <- suppressWarnings(as.numeric(input[[paste0("fixedval_", k)]]))
        if (length(val) != 1 || is.na(val)) { complete <- FALSE; val <- NA_real_ }
        prior <- as.character(val)
        lb <- "NA"; ub <- "NA"
      } else {
        pt <- input[[paste0("priors_", k)]]
        if (is.null(pt)) { complete <- FALSE; prior <- NA_character_ } else {
          args <- if (pt == "Custom") NULL else vapply(seq_along(prior_catalog[[pt]]$args), function(a) {
            v <- input[[paste0("arg_", a, "_", k)]]; if (is.null(v)) NA_real_ else as.numeric(v) }, numeric(1))
          prior <- make_prior_string(pt, args, custom_text = input[[paste0("arg_1_", k)]],
                                     truncate0 = isTRUE(input[[paste0("truncateZero_", k)]]))
          if (is.na(prior)) complete <- FALSE
        }
        lb <- as.character(input[[paste0("LB_", k)]] %||% "0")
        ub <- as.character(input[[paste0("UB_", k)]] %||% "NA")
      }
      names_out <- c(names_out, nm)
      fixed_out <- c(fixed_out, if (fx == "Fixed") 1 else 0)
      prior_out <- c(prior_out, prior)
      lb_out <- c(lb_out, lb); ub_out <- c(ub_out, ub)
    }

    parameters.list$list        <- if (length(keys)) names_out else NULL
    parameters.fixed.list$list  <- if (length(keys)) fixed_out else NULL
    parameters.inital.list$list <- if (length(keys)) prior_out else NULL
    parameters.LB.list$list     <- if (length(keys)) lb_out else NULL
    parameters.UB.list$list     <- if (length(keys)) ub_out else NULL
    parameters.complete(complete)
  })

#------------------------------------------------------------------------------#
# Formatting the system of ODEs ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section formats the system of ODEs from the UI to be used in the #
# options file.                                                                #
#------------------------------------------------------------------------------#
  
  ###################################
  # Saving the final system of ODEs #
  ###################################
  odeEquation <- reactiveValues()
  
  ###########################################
  # Saving the list of non-Latex parameters #
  ###########################################
  paramsFinal <- reactiveValues()

  #############################################
  # Reactive values for various IC parameters #
  #############################################
  ic.list.temp      <- reactiveValues()   # initial conditions formatted for the options file
  vars.init.temp    <- reactiveVal()      # 1 = fixed initial conditions, 0 = estimated
  fixed.params.temp <- reactiveValues()   # fixed/estimated status validated with the equations

  ############################################################
  # Time-dependent parameters module                         #
  ############################################################
  # Build choices directly from the LIVE Parameter Specification values.
  # `paramsFinal$params` is intentionally not used here because it is populated
  # only after "Upload Equations" is clicked. Names are display/LaTeX labels
  # (e.g. "\\beta_0") and values are internal identifiers (e.g. "beta0").
  # The same to_internal_name() conversion is used when paramsFinal$params is
  # committed, keeping dropdown values and options-file parameter positions
  # aligned (beta0 -> params1, ...).
  # Returns a reactive with $code (the time_dependent_templates block), $names, $specs.
  tdp <- tdpServer("tdp", reactive({
    disp <- parameters.list$list
    if (is.null(disp) || length(disp) == 0) {
      return(character(0))
    }
    disp <- trimws(as.character(disp))
    intl <- vapply(disp, to_internal_name, character(1))
    complete <- nzchar(disp) & nzchar(intl)
    stats::setNames(intl[complete], disp[complete])
  }))

  ############################################################
  # Token-based ODE equation builder                         #
  ############################################################
  # The builder receives display labels for buttons but stores exact positional
  # tokens. Example: label "\\gamma" -> serial "params4", label "S" -> "vars1",
  # label "\\beta(t)" -> "time_dependent_param1". This keeps the app's existing
  # options-file / stancreator.R contract while removing latex2r from ODE entry.
  # Equations filled in by the model library (list(nonce, equations = list(<state key> = tokens)))
  ode_preset <- reactiveVal(NULL)

  # Display names of the parameters in row order (same order as parameters.list$list)
  param_display_names <- reactive({
    keys <- param_rows$keys
    labels <- param_rows$labels
    if (length(keys) == 0) return(character(0))
    vapply(keys, function(k) {
      if (startsWith(k, "req_")) as.character(labels[[k]] %||% "") else trimws(as.character(input[[paste0("paramsID_", k)]] %||% ""))
    }, character(1), USE.NAMES = FALSE)
  })

  ode_builder <- odeBuilderServer(
    "odeb",
    n_equations = NULL,   # one equation per state row
    states_reactive = reactive({
      keys <- state_rows$keys
      if (length(keys) == 0) return(data.frame(key = character(0), display = character(0), stringsAsFactors = FALSE))
      data.frame(key = keys, display = selectedValues(), stringsAsFactors = FALSE)
    }),
    params_reactive = reactive({
      keys <- param_rows$keys
      if (length(keys) == 0) return(data.frame(key = character(0), display = character(0), stringsAsFactors = FALSE))
      data.frame(key = keys, display = param_display_names(), ic = startsWith(keys, "req_"), stringsAsFactors = FALSE)
    }),
    tdp_reactive = tdp,
    fitting_diff_reactive = reactive({
      fitting_diff_vector()
    }),
    preset_reactive = ode_preset
  )

  # Error catcher
  errorCatch.temp <- reactiveVal(0)

  # TRUE whenever the model definition changed since the last successful validation
  model_dirty <- reactiveVal(TRUE)

  observe({
    # Any change in the model definition requires a new validation
    selectedValues()
    parameters.list$list
    parameters.fixed.list$list
    parameters.inital.list$list
    initial.conditions.vec$IC
    tryCatch(ode_builder(), error = function(e) NULL)
    tryCatch(tdp()$code, error = function(e) NULL)
    fitting_index_values$fit
    model_dirty(TRUE)
  })

  #############################################################################
  # Model validation: parameters -> ODE system -> initial conditions          #
  #############################################################################
  # About: Checks the parameter names, collects the token-built ODE system and #
  # checks the initial conditions (check.IC). Returns TRUE when everything is  #
  # consistent. Called by the 'Validate equations' button and automatically   #
  # by 'Fit model & forecast' when the model changed since the last check.    #
  #############################################################################
  validate_model <- function(show_success = TRUE) {

    reset_model <- function() {
      paramsFinal$params <- NULL
      odeEquation$system <- NULL
      odeEquation$rhs <- NULL
      odeEquation$display <- NULL
      odeEquation$ic_rhs <- NULL
      fixed.params.temp$list <- NULL
      ic.list.temp$list <- NULL
    }

    fail <- function(title, text) {
      shinyalert::shinyalert(title = title, text = text, type = "error")
      reset_model()
      FALSE
    }

    ############################
    # Initial conditions       #
    ############################
    ic <- initial.conditions.vec$IC
    n_states <- vars_n() %||% 0
    state_names <- selectedValues()
    if (n_states == 0 || length(state_names) < n_states || any(!nzchar(state_names))) {
      return(fail("State variables incomplete", "Please name every state variable (step 4)."))
    }
    if (length(duplicated_state_names()) > 0) {
      return(fail("Duplicated state variables", paste0("These names are used more than once: ", paste(duplicated_state_names(), collapse = ", "), " (step 4).")))
    }
    if (is.null(ic) || length(ic) < n_states || any(!nzchar(unlist(ic)))) {
      missing_ic <- state_names[which(!nzchar(unlist(c(ic, rep("", n_states))[seq_len(n_states)])))]
      return(fail("Initial conditions missing",
                  paste0("Please enter the initial condition of: ", paste(missing_ic, collapse = ", "), " (step 4).")))
    }

    ############################
    # Parameters               #
    ############################
    disp_params <- parameters.list$list
    if (is.null(disp_params) || length(disp_params) == 0) {
      return(fail("No parameters", "Please define at least one parameter (step 5)."))
    }
    if (any(!nzchar(trimws(as.character(disp_params))))) {
      return(fail("Parameters incomplete", "Please name every parameter before validating (step 5)."))
    }
    if (!isTRUE(parameters.complete())) {
      return(fail("Parameters incomplete", "Please complete the prior / fixed value of every parameter (step 5)."))
    }
    params.temp.list <- vapply(seq_along(disp_params), function(i) {
      tryCatch(to_internal_name(as.character(disp_params[[i]])),
               latex2r.error = function(cnd) "ERROR1",
               error = function(cnd) "ERROR1")
    }, character(1))
    if (any(params.temp.list == "ERROR1") || any(!nzchar(params.temp.list))) {
      return(fail("Parameter names", "Please check the specification of your parameters; a LaTeX name could not be translated to a valid identifier."))
    }
    if (any(duplicated(params.temp.list))) {
      dups <- unique(params.temp.list[duplicated(params.temp.list)])
      return(fail("Duplicate parameters", paste0("These parameter names are used more than once: ", paste(dups, collapse = ", "), ".")))
    }
    if (any(params.temp.list %in% state_names)) {
      clash <- params.temp.list[params.temp.list %in% state_names]
      return(fail("Name clash", paste0("The same name is used for a parameter and a state variable: ", paste(clash, collapse = ", "), ".")))
    }
    paramsFinal$params <- params.temp.list

    ############################
    # ODE system               #
    ############################
    ode_built <- ode_builder()
    if (is.null(ode_built) || !isTRUE(ode_built$valid)) {
      msg <- paste(ode_built$errors %||% "Please complete all ODE equations.", collapse = "\n")
      return(fail("Equations incomplete", paste("Please check the ODE equations (step 7).", msg, sep = "\n")))
    }
    odeEquation$system <- ode_built$ode_system
    odeEquation$rhs <- ode_built$rhs
    odeEquation$display <- ode_built$display
    odeEquation$ic_rhs <- ode_builder_expand_rhs_for_ic(
      ode_built$rhs,
      paramsFinal$params,
      tryCatch(tdp()$specs, error = function(e) list())
    )

    ############################
    # Data assignment          #
    ############################
    if (is.null(fitting_index_values$fit)) {
      msg <- if (length(fitting_index_values$problems) > 0) paste(fitting_index_values$problems, collapse = "\n") else
        "Assign the data to at least one state variable ('Fit to data' = Yes in step 4)."
      return(fail("Data not assigned", msg))
    }

    ############################
    # Initial-condition checks #
    ############################
    fixed_status <- parameters.fixed.list$list
    init_param   <- parameters.inital.list$list
    if (is.null(fixed_status) || is.null(init_param) || length(fixed_status) != length(params.temp.list)) {
      return(fail("Priors incomplete", "Please complete the prior / fixed value of every parameter (step 5)."))
    }

    ic_internal <- vapply(unlist(ic), ic_to_expression, character(1), USE.NAMES = FALSE)
    IcOutput <- tryCatch(
      check.IC(ic.list = ic_internal,
               params.list = paramsFinal$params,
               equations.list = odeEquation$ic_rhs,
               fixedStatus = fixed_status,
               initParam = init_param),
      error = function(e) paste0("ERROR:", conditionMessage(e))
    )

    if (is.character(IcOutput) && length(IcOutput) == 1 && startsWith(IcOutput, "ERROR:")) {
      return(fail("Initial conditions", paste("The initial conditions could not be interpreted:", sub("^ERROR:", "", IcOutput))))
    }
    if (all(as.numeric(fixed_status) == 1)) {
      return(fail("Nothing to estimate", "At least one parameter must be estimated (step 5)."))
    }

    ic.list.temp$list <- IcOutput[[1]]
    vars.init.temp(IcOutput[[2]])
    fixed.params.temp$list <- fixed_status

    model_dirty(FALSE)
    if (show_success) {
      shinyalert::shinyalert(title = "Model validated",
                             text = "Parameters, equations and initial conditions are consistent. You can run the prior predictive check or fit the model.",
                             type = "success")
    }
    TRUE
  }

  observeEvent(input$loadEquation, {
    validate_model(show_success = TRUE)
  })

  # Kept for backwards compatibility with older code paths
  loadEquationTrigger <- reactive({ if (isTRUE(model_dirty())) 0 else 1 })

#------------------------------------------------------------------------------#
# Creating the composite variable list -----------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the input for the composite variables. It uses   #
# the line-by-line input from the user, to create the list needed for the      #
# `options` file below.                                                        #
#------------------------------------------------------------------------------#
  
  ##############################################
  # Empty list to store composite variables in #
  ##############################################
  composite.variable.list <- reactiveValues()
  
  # Translate a LaTeX composite expression into Stan code using the internal
  # parameter names (e.g. "\\beta_0 / \\gamma" -> "beta0 / gamma").
  composite_to_internal <- function(expr_latex, display_params) {
    expr_r <- paste0(latex2r(as.character(expr_latex)))
    display_params <- as.character(display_params %||% character(0))
    display_params <- display_params[nzchar(trimws(display_params))]
    mapping <- list()
    for (d in display_params) {
      internal <- to_internal_name(d)
      key1 <- tryCatch(paste0(latex2r(d)), error = function(e) NA_character_)
      key2 <- gsub("[\\\\{}]", "", d)
      for (k in unique(c(key1, key2, internal))) {
        if (!is.na(k) && nzchar(k)) mapping[[k]] <- internal
      }
    }
    # Replace identifiers token by token
    tokens <- regmatches(expr_r, gregexpr("[A-Za-z][A-Za-z0-9_]*", expr_r))[[1]]
    for (tok in unique(tokens)) {
      repl <- mapping[[tok]]
      if (is.null(repl)) repl <- mapping[[gsub("_", "", tok)]]
      if (is.null(repl) && grepl("_", tok)) repl <- gsub("_", "", tok)
      if (!is.null(repl) && !identical(repl, tok)) {
        expr_r <- gsub(paste0("\\b", tok, "\\b"), repl, expr_r, perl = TRUE)
      }
    }
    expr_r
  }

  ##############################################
  # Collecting the equations into one variable #
  ##############################################
  validate_composites <- function(show_success = TRUE) {
    
    composite.list.temp <- list()
    comp_keys <- composite_rows$keys
    n_comp <- length(comp_keys)
    problems <- character(0)

    for(i in seq_len(n_comp)){

      k <- comp_keys[i]
      left.side.temp <- trimws(as.character(input[[paste0("LeftEquation_", k)]] %||% ""))
      left.side.temp <- gsub("[^A-Za-z0-9_]", "", left.side.temp)
      rhs_latex <- as.character(input[[paste0("compositeID_", k)]] %||% "")

      if (!nzchar(left.side.temp)) {
        problems <- c(problems, sprintf("Composite variable %d has no name.", i))
        next
      }
      if (!grepl("^[A-Za-z]", left.side.temp)) {
        problems <- c(problems, sprintf("The name of composite variable %d must start with a letter.", i))
        next
      }
      if (!nzchar(trimws(rhs_latex))) {
        problems <- c(problems, sprintf("Composite variable '%s' has no expression.", left.side.temp))
        next
      }
      
      right.side.temp <- tryCatch(
        composite_to_internal(rhs_latex, parameters.list$list),
        latex2r.error = function(cnd2) NULL,
        error = function(cnd2) NULL
      )
      if (is.null(right.side.temp)) {
        problems <- c(problems, sprintf("The expression of '%s' could not be translated (check the LaTeX).", left.side.temp))
        next
      }
      composite.list.temp[[left.side.temp]] <- right.side.temp
    }
    
    if (length(problems) > 0) {
      shinyalert::shinyalert(title = "Composite variables", text = paste(problems, collapse = "\n"), type = "error")
      composite.variable.list$list <- NULL
      return(FALSE)
    }
    
    composite.variable.list$list <- composite.list.temp
    if (show_success && n_comp > 0) {
      shinyalert::shinyalert(title = "Composite variables validated",
                             text = paste(sprintf("%s = %s", names(composite.list.temp), unlist(composite.list.temp)), collapse = "\n"),
                             type = "success")
    }
    TRUE
  }

  observeEvent(input$loadComposite, {
    validate_composites(show_success = TRUE)
  })
  
#------------------------------------------------------------------------------#
# Setting the error structure --------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section formats the error structure for the options file. The    #
# user sees the "Negative Binomial", "Normal" and "Poisson" options. This      #
# code shifts that to the proper numeric for the options file.                 #
#------------------------------------------------------------------------------#
  
  ######################################################
  # Reactive value to store the error structure number #
  ######################################################
  errorStructureNumber <- reactiveVal()
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    # Switch statement to determine the number
    errorNum <- switch(input$errstrc,
                       
      "Negative binomial" = 1,
      "Normal" = 2,
      "Poisson" = 3)
    
    # Saving the value in the reactive value
    errorStructureNumber(errorNum)
    
  })
  
#------------------------------------------------------------------------------#
# Clearing the options file ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears the option file if either load equation buttons   #
# are clicked.                                                                 #
#------------------------------------------------------------------------------#
  
  # Creating reactive value
  optionsFileReactive <- reactiveValues()
  
  ######################################
  # Observing the button being clicked #
  ######################################
  observeEvent(input$loadEquation, {
    
    optionsFileReactive$file <- NULL
    
    modelFitResults$results <- NULL
    
    # Resetting the reactive list
    samplingReactiveValue$values <- NULL
    
    allOutput$everything <- NULL


  })
  
  ##################################################
  # Observing the composite equation being clicked #
  ##################################################
  observeEvent(input$loadComposite, {
    
    optionsFileReactive$file <- NULL
    
    modelFitResults$results <- NULL
    
    # Resetting the reactive list
    samplingReactiveValue$values <- NULL
    
    allOutput$everything <- NULL


  })
  

  
#------------------------------------------------------------------------------#
# Loading everything into the options file -------------------------------------
#------------------------------------------------------------------------------#
# About: This section loads everything into the options file. The options file #
# is then used by the "run_MCMC.R" and "run_analyzeResults.R" functions.       #
#------------------------------------------------------------------------------#
  
  #############################################################################
  # Readiness: which steps are complete, which are missing                     #
  #############################################################################
  # About: Returns a data frame with one row per step (label, done, detail,     #
  # optional). Used by the checklist box and by the 'Fit model' button.        #
  #############################################################################
  readiness <- reactive({

    steps <- list()
    add <- function(label, done, detail = "", optional = FALSE) {
      steps[[length(steps) + 1]] <<- data.frame(label = label, done = done, detail = detail,
                                                optional = optional, stringsAsFactors = FALSE)
    }

    # 1. Data
    has_data <- !is.null(finalData$data)
    add("1. Data", has_data,
        if (has_data) sprintf("%d time points, %d series", nrow(finalData$data), ncol(finalData$data) - 1) else "Upload a .xlsx/.csv file (step 1)")

    # 2. Calibration & forecast
    cal <- if (has_data) calibration_periods() else list(values = NULL, error = "no data")
    add("2. Calibration & Forecast", has_data && is.null(cal$error),
        if (has_data && is.null(cal$error)) sprintf("Calibration: %s; horizon: %d", bff_compact_lengths(cal$values), forecasting_horizon()) else
          if (!has_data) "Load the data first" else cal$error)

    # 4. State variables (names, initial conditions, data assignment)
    n_states <- vars_n() %||% 0
    state_names <- selectedValues()
    ic <- initial.conditions.vec$IC
    ic_ok <- n_states > 0 && length(ic) >= n_states && all(nzchar(unlist(ic[seq_len(n_states)])))
    names_ok <- n_states > 0 && length(state_names) >= n_states && all(nzchar(state_names))
    dups <- duplicated_state_names()
    fit_ok <- !is.null(fitting_index_values$fit)
    states_ok <- names_ok && ic_ok && length(dups) == 0 && fit_ok
    add("4. State Variables", states_ok,
        if (n_states == 0) "Add the state variables (step 4) or choose a model in the Model library" else
          if (!names_ok) "Name every state variable (step 4)" else
            if (length(dups) > 0) paste0("Duplicated name(s): ", paste(dups, collapse = ", "), " (step 4)") else
              if (!ic_ok) "Enter every initial condition (step 4)" else
                if (!fit_ok) (if (length(fitting_index_values$problems) > 0) fitting_index_values$problems[1] else "Set 'Fit to data' = Yes for the fitted compartment(s) (step 4)") else
                  sprintf("%s; %d series assigned", paste(state_names, collapse = ", "), fitting_index_values$n_assigned))

    # 5. Parameters & priors (incl. the error-structure priors)
    n_param_rows <- length(param_rows$keys)
    params_ok <- n_param_rows > 0 && isTRUE(parameters.complete())
    n_est <- sum(as.numeric(parameters.fixed.list$list %||% numeric(0)) == 0)
    err_priors <- parameters.inital.dist.list$list
    n_fit <- max(1, length(fitting_index_values$fit %||% integer(0)))
    err_ok <- errorStructureNumber() == 3 || (!is.null(err_priors) && length(err_priors) >= n_fit &&
                                                  all(!is.na(err_priors[seq_len(n_fit)])))
    add("5. Parameters & Priors", params_ok && isTRUE(err_ok),
        if (n_param_rows == 0) "Press 'Add parameter' (step 5) or use parameters in the initial conditions" else
          if (!params_ok && n_est == 0) "At least one parameter must be estimated (step 5)" else
            if (!params_ok) "Complete every parameter row (step 5)" else
              if (!isTRUE(err_ok)) "Complete the prior of the error parameter(s) (step 5)" else
                sprintf("%d parameters (%d estimated)", n_param_rows, n_est))

    # 7. System of equations (complete and validated)
    ode_state <- tryCatch(ode_builder(), error = function(e) NULL)
    ode_ok <- !is.null(ode_state) && isTRUE(ode_state$valid)
    validated <- !isTRUE(model_dirty()) && !is.null(odeEquation$system) && !is.null(fixed.params.temp$list)
    add("7. System of Equations", ode_ok && validated,
        if (!ode_ok) "Build every right-hand side (step 7)" else
          if (!validated) "Press 'Validate equations' in step 7 (done automatically when fitting)" else
            sprintf("%d equations, validated", n_states))

    # 8. Composite variables (optional)
    n_comp <- num_composite_n() %||% 0
    comp_ok <- n_comp == 0 || (!is.null(composite.variable.list$list) && length(composite.variable.list$list) == n_comp)
    add("8. Composite Variables", comp_ok,
        if (n_comp == 0) "none (optional)" else if (comp_ok) paste(names(composite.variable.list$list), collapse = ", ") else "Validate the composite expressions (step 8)",
        optional = TRUE)

    do.call(rbind, steps)
  })

  output$readiness_box <- renderUI({
    if (isTRUE(input$config_mode)) {
      status <- config_state$status %||% "unknown"
      cfg_ok <- status == "valid"; data_ok <- !is.null(finalData$data)
      return(box(width = 12, title = "Checklist", status = if (cfg_ok && data_ok) "success" else "primary", collapsible = TRUE,
                 div(class = "bff-step", title = if (cfg_ok) "Configuration validated" else "Validate the configuration in the 'Configuration file' tab",
                     span(class = paste("bff-icon", if (cfg_ok) "bff-ok" else "bff-todo"), icon(if (cfg_ok) "check-circle" else "exclamation-circle")),
                     div(strong("Configuration file"))),
                 div(class = "bff-step", title = if (data_ok) sprintf("%d time points, %d series", nrow(finalData$data), ncol(finalData$data) - 1) else "Load a data file (step 1)",
                     span(class = paste("bff-icon", if (data_ok) "bff-ok" else "bff-todo"), icon(if (data_ok) "check-circle" else "exclamation-circle")),
                     div(strong("1. Data"))),
                 div(class = "bff-hint", style = "margin-top: 6px;",
                     if (cfg_ok && data_ok) span(style = "color: #00a65a; font-weight: bold;", icon("play"), " Ready: press 'Fit model & forecast'.") else
                       "Running from the configuration file; the form is ignored.")))
    }
    rd <- readiness()
    n_done <- sum(rd$done | rd$optional)
    rows <- lapply(seq_len(nrow(rd)), function(k) {
      cls <- if (rd$done[k]) "bff-ok" else if (rd$optional[k]) "bff-opt" else "bff-todo"
      ic  <- if (rd$done[k]) "check-circle" else if (rd$optional[k]) "minus-circle" else "exclamation-circle"
      div(class = "bff-step", title = rd$detail[k],
          span(class = paste("bff-icon", cls), icon(ic)),
          div(strong(rd$label[k])))
    })
    ready <- all(rd$done | rd$optional)
    next_step <- rd[!rd$done & !rd$optional, , drop = FALSE]
    box(width = 12, title = sprintf("Checklist (%d / %d)", n_done, nrow(rd)),
        status = if (ready) "success" else "primary", collapsible = TRUE,
        tagList(rows),
        if (ready) div(style = "margin-top: 8px; color: #00a65a; font-weight: bold;",
                       icon("play"), " Ready: press 'Fit model & forecast'.") else
          if (nrow(next_step) > 0) div(class = "bff-hint", style = "margin-top: 8px;", icon("arrow-right"), " Next: ", next_step$detail[1]) else NULL)
  })

  #############################################################################
  # Build the options.R content from the form                                 #
  #############################################################################
  build_options_from_form <- function() {

    cal <- calibration_periods()
    if (!is.null(cal$error)) stop(cal$error)
    calibrationperiod_values <- paste(cal$values, collapse = ", ")

    vars_values   <- paste0('"', selectedValues(), '"', collapse = ", ")
    params_values <- paste0('"', paramsFinal$params, '"', collapse = ", ")
    paramsFixed_values <- paste(as.numeric(fixed.params.temp$list), collapse = ", ")
    disp_names <- as.character(parameters.list$list %||% character(0))
    disp_names <- gsub("\\", "\\\\", disp_names, fixed = TRUE)
    disp_names <- gsub("\"", "\\\"", disp_names, fixed = TRUE)
    params_display_values <- paste0('"', disp_names, '"', collapse = ", ")
    template_line <- if (!is.null(active_template())) sprintf('model_template <- "%s"  # model of the library the form was filled from', active_template()) else "model_template <- NA"

    comp <- composite.variable.list$list %||% list()
    result <- if (length(comp) > 0) paste(sprintf('%s = "%s"', names(comp), unlist(comp)), collapse = ",\n  ") else ""

    series_cases_values <- paste0('"', series_cases_temp$labels, '"', collapse = ", ")

    vec <- parameters.inital.list$list
    format_prior <- function(value, index){
      if(grepl("\\(", value)){
        sprintf('params%d_prior <- "%s"', index, value)
      }else{
        sprintf('params%d_prior <- %s', index, as.numeric(value))
      }
    }
    params_prior_assignments <- paste(mapply(format_prior, vec, seq_along(vec)), collapse = "\n")

    vec.params.dist <- parameters.inital.dist.list$list
    format_prior.dist <- function(value, index){
      if(errorStructureNumber() == 2){
        sprintf('normalerror%d_prior <- "%s"', index, value)
      }else if(errorStructureNumber() == 1){
        sprintf('negbinerror%d_prior <- "%s"', index, value)
      }else{
        ""
      }
    }
    params_prior_dist_assignments <- if (length(vec.params.dist) > 0) {
      paste(mapply(format_prior.dist, vec.params.dist, seq_along(vec.params.dist)), collapse = "\n")
    } else ""

    format_bound <- function(value, index, which){
      value <- as.character(value)
      if (value %in% c("-1", "NA", "", "NaN") || is.na(suppressWarnings(as.numeric(value)))) {
        sprintf('params%d_%s <- NA', index, which)
      } else {
        sprintf('params%d_%s <- %s', index, which, as.numeric(value))
      }
    }
    LB <- parameters.LB.list$list
    UB <- parameters.UB.list$list
    params_LB_assignments <- paste(mapply(format_bound, LB, seq_along(LB), MoreArgs = list(which = "LB")), collapse = "\n")
    params_UB_assignments <- paste(mapply(format_bound, UB, seq_along(UB), MoreArgs = list(which = "UB")), collapse = "\n")

    formatted <- sapply(ic.list.temp$list, function(item) {
      if (is.character(item)) sprintf('"%s"', item) else as.character(item)
    })
    Ic.Input.Temp <- sprintf("Ic <- c(%s)", paste(formatted, collapse = ", "))

    time_dependent_code <- tryCatch(tdp()$code, error = function(e) "time_dependent_templates <- list()")
    time_dependent_display_code <- tryCatch({
      tdp_value <- tdp()
      specs <- tdp_value$specs %||% list()
      complete_names <- as.character(tdp_value$names %||% character(0))
      complete_specs <- Filter(function(spec) {
        !is.null(spec$internal_name) && spec$internal_name %in% complete_names
      }, specs)
      if (length(complete_specs) == 0) {
        "time_dependent_display_names <- character(0)"
      } else {
        entries <- vapply(complete_specs, function(spec) {
          label <- spec$display_name %||% spec$internal_name
          label <- gsub("\\", "\\\\", label, fixed = TRUE)
          label <- gsub("\"", "\\\"", label, fixed = TRUE)
          sprintf('  %s = "%s"', spec$internal_name, label)
        }, character(1))
        paste0("time_dependent_display_names <- c(\n", paste(entries, collapse = ",\n"), "\n)")
      }
    }, error = function(e) "time_dependent_display_names <- character(0)")

    model_name_val <- trimws(input$model_name %||% "")
    if (!nzchar(model_name_val)) model_name_val <- "model"
    model_name_val <- gsub("[^A-Za-z0-9_.-]+", "-", model_name_val)

    niter_val  <- max(100, as.integer(input$niter %||% 1000))
    nchain_val <- max(1, as.integer(input$num_chain %||% 2))
    warm_val   <- suppressWarnings(as.integer(input$nwarmup))
    warm_line  <- if (length(warm_val) == 1 && !is.na(warm_val) && warm_val > 0 && warm_val < niter_val) sprintf("nwarmup <- %d", warm_val) else sprintf("nwarmup <- %d  # half of the iterations (Stan default)", floor(niter_val / 2))
    adapt_val  <- suppressWarnings(as.numeric(input$adapt_delta)); if (length(adapt_val) != 1 || is.na(adapt_val)) adapt_val <- 0.8
    tree_val   <- suppressWarnings(as.integer(input$max_treedepth)); if (length(tree_val) != 1 || is.na(tree_val)) tree_val <- 10L
    cores_val  <- suppressWarnings(as.integer(input$n_cores)); if (length(cores_val) != 1 || is.na(cores_val)) cores_val <- 1L
    seed_val   <- suppressWarnings(as.integer(input$seed)); if (length(seed_val) != 1 || is.na(seed_val)) seed_val <- 0L

    options_content <- sprintf('# BayesianFitForecast configuration (options.R) generated by the Shiny app on %s
# This file can be re-loaded in the app ("Examples & saved configurations") or used with the R toolbox.

# ---- Calibration and forecasting ----
calibrationperiods <- c(%s)
forecastinghorizon <- %d

# ---- Model ----
model_name <- "%s"
%s
vars <- c(%s)
params <- c(%s)
params_display <- c(%s)  # display names (LaTeX) of the parameters, used in the reports

# Time-dependent parameter templates
%s

# User-facing labels for time-dependent parameters
%s

ode_system <- %s

# 1 = fixed, 0 = estimated
paramsfix <- c(%s)

composite_expressions <- list(
  %s
)

# State variable(s) fitted to each data column and whether the derivative is fitted
fitting_index <- c(%s)
fitting_diff <- c(%s)

# Error structure: 1 = negative binomial, 2 = normal, 3 = Poisson
errstrc <- %d

# ---- Data labels ----
caddisease <- "%s"
series_cases <- c(%s)
datetype <- "%s"

# ---- Priors / fixed values ----
%s

# Lower bound of parameters
%s

# Upper bound of parameters
%s

# Prior distribution for the normal / negative-binomial error parameters
%s

# ---- Initial conditions (vars.init: 1 = fixed, 0 = estimated) ----
vars.init <- %d
%s

# ---- MCMC settings ----
niter <- %d
num_chain <- %d
%s
adapt_delta <- %s
max_treedepth <- %d
n_cores <- %d
seed <- %d
',
      format(Sys.time(), "%Y-%m-%d %H:%M"),
      calibrationperiod_values,
      forecasting_horizon(),
      model_name_val,
      template_line,
      vars_values,
      params_values,
      params_display_values,
      time_dependent_code,
      time_dependent_display_code,
      paste(odeEquation$system, collapse = "\n"),
      paramsFixed_values,
      result,
      paste(fitting_index_values$fit, collapse = ", "),
      paste(fitting_diff_vector(), collapse = ", "),
      errorStructureNumber(),
      caddisease_value(),
      series_cases_values,
      date_type_temp$temporal %||% "Time",
      params_prior_assignments,
      params_LB_assignments,
      params_UB_assignments,
      params_prior_dist_assignments,
      as.numeric(vars.init.temp()),
      Ic.Input.Temp,
      niter_val,
      nchain_val,
      warm_line,
      format(adapt_val),
      tree_val,
      cores_val,
      seed_val
    )

    options_content
  }

  #############################################################################
  # Pressing 'Fit model & forecast'                                            #
  #############################################################################
  observeEvent(input$run, {

    optionsFileReactive$file <- NULL

    if (is.null(finalData$data)) {
      shinyalert::shinyalert("No data", "Please upload a data file first (step 1).", type = "error")
      return(invisible(NULL))
    }

    #################################################
    # Expert mode: run from the configuration text #
    #################################################
    if (isTRUE(input$config_mode)) {
      txt <- input$config_text %||% ""
      check <- bff_validate_options_text(txt, finalData$data)
      config_state$status <- if (check$ok) "valid" else "invalid"
      config_state$messages <- c(check$errors, check$warnings)
      if (!check$ok) {
        shinyalert::shinyalert("Configuration problems", paste(check$errors, collapse = "\n"), type = "error")
        return(invisible(NULL))
      }
      options_content <- txt

    #################################################
    # Standard mode: build the options from the form #
    #################################################
    } else {

      # Validate the model automatically when it changed since the last check
      if (isTRUE(model_dirty()) || is.null(odeEquation$system) || is.null(fixed.params.temp$list)) {
        ok <- validate_model(show_success = FALSE)
        if (!isTRUE(ok)) return(invisible(NULL))
      }
      n_comp <- num_composite_n() %||% 0
      if (n_comp > 0 && (is.null(composite.variable.list$list) || length(composite.variable.list$list) != n_comp)) {
        ok <- validate_composites(show_success = FALSE)
        if (!isTRUE(ok)) return(invisible(NULL))
      }

      rd <- readiness()
      missing <- rd[!rd$done & !rd$optional, , drop = FALSE]
      if (nrow(missing) > 0) {
        shinyalert::shinyalert("Not ready yet",
                               paste0("Please complete:\n", paste(sprintf("• %s — %s", missing$label, missing$detail), collapse = "\n")),
                               type = "error")
        return(invisible(NULL))
      }

      options_content <- tryCatch(build_options_from_form(), error = function(e) e)
      if (inherits(options_content, "error")) {
        shinyalert::shinyalert("Configuration", conditionMessage(options_content), type = "error")
        return(invisible(NULL))
      }
      updateTextAreaInput(session, "config_text", value = options_content)
      config_state$pushed <- options_content
      config_state$status <- "valid"
      config_state$messages <- character(0)
    }

    ##################################
    # Saving the `temp` options file #
    ##################################
    options_file <- file.path(tempdir(), "options.R")
    writeLines(options_content, con = options_file)

    ##########################
    # Creating the STAN file #
    ##########################
    stan_code <- tryCatch(
      generate_stan_file_delegated(optionsFile = options_file,
                                   stan_file = file.path(tempdir(), "ode_model.stan")),
      error = function(e) e
    )
    if (inherits(stan_code, "error")) {
      shinyalert::shinyalert("Stan program could not be generated", conditionMessage(stan_code), type = "error")
      return(invisible(NULL))
    }
    optionsFileReactive$file <- options_file
    optionsFileReactive$content <- options_content
    optionsFileReactive$stan_code <- stan_code

    opt_env <- tryCatch(bff_load_options(options_file), error = function(e) NULL)
    n_cal <- if (!is.null(opt_env)) length(opt_env$calibrationperiods) else 1
    n_chain <- if (!is.null(opt_env)) as.integer(opt_env$num_chain) else NA
    n_iter <- if (!is.null(opt_env)) as.integer(opt_env$niter) else NA

    ###############################
    # Creating the pop-up message #
    ###############################
    showModal(modalDialog(
      title = "Ready to fit the model",
      p(sprintf("The Stan program was generated successfully. %d calibration period(s) will be fitted with %s chain(s) of %s iterations each.",
                n_cal, n_chain, n_iter)),
      p(class = "bff-hint", "Compiling the Stan program takes about a minute the first time; sampling time depends on the model and the number of iterations. The dashboard stays busy until sampling is finished."),
      div(style = "display: flex; gap: 10px; justify-content: center; margin: 10px 0; flex-wrap: wrap;",
          downloadButton("downloadOptionsFile", "Download options.R"),
          downloadButton("downloadSTANfile", "Download Stan program")),
      footer = div(
        style = "display: flex; gap: 20px; justify-content: center;",
        actionButton("yesPROCEED", label = "Fit the model", class = "btn-success", icon = icon("play"),
                     style = "flex: 1; padding: 10px; min-width: 120px;"),
        actionButton("NoPROCEED", label = "Not now",
                     style = "flex: 1; padding: 10px; min-width: 120px;")
      ),
      easyClose = TRUE
    ))

    output$downloadOptionsFile <- downloadHandler(
      filename = function() { paste0("options-", if (!is.null(opt_env)) opt_env$model_name else "model", ".R") },
      content = function(file) { writeLines(options_content, con = file) }
    )
    output$downloadSTANfile <- downloadHandler(
      filename = function() { paste0("ode_model-", if (!is.null(opt_env)) opt_env$model_name else "model", ".stan") },
      content = function(file) { writeLines(stan_code, file) }
    )

  })

  observeEvent(input$NoPROCEED, {
    removeModal()
  })
  
#------------------------------------------------------------------------------#
# Fitting the model ------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section downloads fits the Bayesian model based upon what the    #
# users specify in the other parameters needed to run the dashboard.           #
#------------------------------------------------------------------------------#
  
  #############################################
  # Reactive values holding the fitting chain #
  #############################################
  modelFitResults       <- reactiveValues(results = NULL)   # compiled model + data per calibration period
  samplingReactiveValue <- reactiveValues(values = NULL)    # stanfit objects (one per calibration period)
  finalMCMC             <- reactiveValues(results = NULL)   # extracted draws (run_MCMC2)
  allOutput             <- reactiveValues(everything = NULL) # figures / tables (run_analyzeResults)
  samplingIndicator     <- reactiveVal(0)
  run_meta              <- reactiveValues(fits = NULL, options = NULL, stan_code = NULL, warnings = list(),
                                          started = NULL, finished = NULL)

  # Lines of Stan console output worth keeping as diagnostics
  bff_filter_stan_console <- function(lines) {
    if (length(lines) == 0) return(character(0))
    keep <- grepl("Exception|[Ee]rror|[Rr]ejecting|divergent|treedepth|Effective Samples|R-hat|Rhat|BFMI|WARNING|Warning",
                  lines)
    unique(trimws(lines[keep]))
  }
    
  #############################################################################
  # Post-processing shared by the asynchronous and the synchronous paths       #
  #############################################################################
  # About: merges / extracts the draws, computes figures, tables and          #
  # diagnostics, stores everything and shows the completion message.          #
  #############################################################################
  finish_run <- function(fits, warnings_by_period, options_file, data_now) {
    ok <- withProgress(message = "Post-processing", value = 0.3, {
      incProgress(0.2, detail = "Extracting posterior draws...")
      extracted <- tryCatch(run_MCMC2(outputFile = options_file, data.temp = data_now, finalModlLIST = fits),
                            error = function(e) e)
      if (inherits(extracted, "error")) {
        shinyalert::shinyalert("Post-processing failed", conditionMessage(extracted), type = "error")
        return(FALSE)
      }
      finalMCMC$results <- extracted
      incProgress(0.3, detail = "Computing figures, tables and diagnostics...")
      out <- tryCatch(run_analyzeResults(optionsFile = options_file, MCMCOutput = extracted,
                                         data.temp = data_now, stan_warnings = warnings_by_period),
                      error = function(e) e)
      if (inherits(out, "error")) {
        shinyalert::shinyalert("Output generation failed", conditionMessage(out), type = "error")
        return(FALSE)
      }
      allOutput$everything <- out
      TRUE
    })
    if (!isTRUE(ok)) return(invisible(FALSE))

    samplingReactiveValue$values <- fits
    run_meta$fits <- fits
    run_meta$warnings <- warnings_by_period
    run_meta$options <- optionsFileReactive$content
    run_meta$stan_code <- optionsFileReactive$stan_code
    run_meta$finished <- Sys.time()
    samplingIndicator(1)

    conv <- tryCatch(do.call(rbind, lapply(allOutput$everything, function(x) x$convergence.summary)),
                     error = function(e) NULL)
    status <- if (!is.null(conv) && "Status" %in% names(conv)) conv$Status else "Unknown"
    worst <- if ("Poor" %in% status) "Poor" else if ("Caution" %in% status) "Caution" else if (all(status == "Good")) "Good" else "Unknown"
    elapsed <- round(as.numeric(difftime(run_meta$finished, run_meta$started, units = "mins")), 1)
    msg <- switch(worst,
                  "Good" = "No convergence problems were detected.",
                  "Caution" = "Some diagnostics deserve attention; see the 'Convergence & Diagnostics' box.",
                  "Poor" = "The sampler did NOT converge properly; the estimates should not be trusted yet. See the 'Convergence & Diagnostics' box for advice.",
                  "Diagnostics could not be computed.")

    # Save the completed run: runs/<id>/ gets options.R, ode_model.stan, data.csv,
    # figures/ (PNG) and tables/ (CSV); the fitted objects are cached internally
    # so the results can be shown again without refitting
    saved <- withProgress(message = "Saving the run", value = 0.1, {
      s <- tryCatch(isolate(bff_save_run(
        results = allOutput$everything,
        run_meta = list(started = run_meta$started, finished = run_meta$finished, options = run_meta$options,
                        stan_code = run_meta$stan_code, warnings = run_meta$warnings, fits = fits),
        data = data_now,
        data_name = listTimeseries$name,
        runs_dir = runs_dir)), error = function(e) e)
      if (!inherits(s, "error")) {
        incProgress(0.3, detail = "tables (CSV)")
        tryCatch(isolate({
          dir.create(file.path(s$dir, "tables"), showWarnings = FALSE)
          write_tables_to_dir(file.path(s$dir, "tables"))
        }), error = function(e) message("The tables of the run could not be written: ", conditionMessage(e)))
        incProgress(0.3, detail = "figures (PNG)")
        tryCatch(isolate(write_figures_to_dir(s$dir)),
                 error = function(e) message("The figures of the run could not be written: ", conditionMessage(e)))
      }
      s
    })
    saved_msg <- if (inherits(saved, "error")) {
      message("The run could not be saved: ", conditionMessage(saved))
      " The run could NOT be saved to the history (see the R console)."
    } else {
      run_history_tick(run_history_tick() + 1)
      sprintf(" Saved in the run history as '%s' (configuration, Stan program, data, figures and tables).", saved$id)
    }

    shinyalert::shinyalert(title = "Run completed",
                           text = sprintf("Finished in %s min. %s%s", elapsed, msg, saved_msg),
                           type = if (worst == "Good") "success" else if (worst == "Poor") "error" else "warning")
    invisible(TRUE)
  }

  #############################################################################
  # Asynchronous run: background processes + real progress + Cancel           #
  #############################################################################
  async_run <- reactiveValues(run = NULL, summary = NULL, active = FALSE, tick = 0, last_error = NULL)

  reset_run_state <- function() {
    allOutput$everything <- NULL
    finalMCMC$results <- NULL
    samplingReactiveValue$values <- NULL
    modelFitResults$results <- NULL
    run_meta$fits <- NULL
    run_meta$warnings <- list()
    run_meta$started <- Sys.time()
    run_meta$finished <- NULL
    samplingIndicator(0)
  }

  start_async_run <- function(prep, options_file, data_now) {
    run_dir <- file.path(tempdir(), paste0("bff_run_", format(Sys.time(), "%Y%m%d%H%M%S")))
    run <- bff_run_new(prep$jobs, prep$stan_file, run_dir, prep$n_cores)
    run$options_file <- options_file
    run$data <- data_now
    bff_run_start(run)
    async_run$run <- run
    async_run$active <- TRUE
    async_run$summary <- bff_run_summary(run)
    shinyjs::disable("run")
  }

  # Poll the background processes every second while a run is active
  observe({
    if (!isTRUE(async_run$active)) return(invisible(NULL))
    invalidateLater(1000, session)
    run <- isolate(async_run$run)
    if (is.null(run)) { async_run$active <- FALSE; return(invisible(NULL)) }

    was_compiling <- identical(run$phase, "compiling")
    run <- tryCatch(bff_run_poll(run), error = function(e) { run$phase <- "failed"; run$error <- conditionMessage(e); run })
    if (was_compiling && identical(run$phase, "sampling")) run$sampling_started <- Sys.time()
    async_run$summary <- bff_run_summary(run)

    if (run$phase == "failed") {
      async_run$active <- FALSE
      shinyjs::enable("run")
      bff_run_kill(run)
      shinyalert::shinyalert("Sampling failed",
                             paste(c(run$error %||% "Unknown error",
                                     "",
                                     "Typical causes: initial conditions or priors that make the ODE solution invalid (negative or infinite values), a fixed parameter equal to 0, or a package missing in the background R process."),
                                   collapse = "\n"),
                             type = "error")
      return(invisible(NULL))
    }

    if (run$phase == "finished") {
      async_run$active <- FALSE
      shinyjs::enable("run")
      collected <- tryCatch(bff_run_collect(run), error = function(e) e)
      if (inherits(collected, "error")) {
        shinyalert::shinyalert("Post-processing failed", conditionMessage(collected), type = "error")
        return(invisible(NULL))
      }
      names(collected$fits) <- names(run$prepared)
      finish_run(collected$fits, collected$warnings, run$options_file, run$data)
    }
  })

  observeEvent(input$cancel_run, {
    run <- async_run$run
    if (!is.null(run)) {
      bff_run_kill(run)
      run$phase <- "cancelled"
      async_run$summary <- bff_run_summary(run)
    }
    async_run$active <- FALSE
    shinyjs::enable("run")
    showNotification("The run was cancelled.", type = "warning", duration = 6)
  })

  # Status box with the real progress of every chain
  output$run_status_box <- renderUI({
    sm <- async_run$summary
    if (is.null(sm) || !(isTRUE(async_run$active) || sm$phase %in% c("cancelled", "failed"))) return(NULL)
    if (!isTRUE(async_run$active) && sm$phase %in% c("cancelled", "failed")) {
      return(box(width = 12, title = "Model fitting", status = "warning",
                 div(icon("ban"), sprintf(" Run %s after %s.", sm$phase, bff_format_duration(sm$elapsed)))))
    }
    fmt_pct <- function(x) sprintf("%d%%", round(100 * x))
    phase_txt <- switch(sm$phase,
                        "compiling" = "Compiling the Stan program (about a minute the first time; instant when cached)...",
                        "sampling"  = sprintf("Sampling: %s of %s iterations completed%s", format(sm$done_iter, big.mark = ","),
                                              format(sm$total_iter, big.mark = ","),
                                              if (!is.na(sm$eta)) sprintf("  |  about %s remaining", bff_format_duration(sm$eta)) else ""),
                        sm$phase)
    overall <- if (sm$phase == "compiling") 0.02 else 0.05 + 0.90 * sm$fraction
    chain_rows <- lapply(sm$chains, function(ch) {
      cls <- switch(ch$status, "done" = "progress-bar-success", "running" = "progress-bar-info progress-bar-striped active",
                    "failed" = "progress-bar-danger", "progress-bar-default")
      div(class = "row", style = "margin-bottom: 4px; align-items: center;",
          div(class = "col-sm-3", style = "font-size: 12px;", sprintf("Calibration %s - chain %d", ch$period, ch$chain)),
          div(class = "col-sm-7", div(class = "progress", style = "margin-bottom: 0; height: 14px;",
                                      div(class = paste("progress-bar", cls), role = "progressbar",
                                          style = sprintf("width: %s; min-width: 2px;", fmt_pct(ch$fraction))))),
          div(class = "col-sm-2", style = "font-size: 12px;",
              sprintf("%s (%s)", fmt_pct(ch$fraction), ch$phase), br(),
              span(class = "bff-hint", sprintf("%s / %s iterations", format(ch$done, big.mark = ","), format(ch$iter, big.mark = ",")))))
    })
    box(width = 12, title = "Model fitting in progress", status = "primary", solidHeader = TRUE,
        div(style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 10px;",
            div(strong(phase_txt), br(), span(class = "bff-hint", sprintf("Elapsed: %s", bff_format_duration(sm$elapsed)))),
            actionButton("cancel_run", "Cancel", icon = icon("stop"), class = "btn-danger")),
        div(class = "progress", style = "margin: 10px 0 12px 0; height: 22px;",
            div(class = "progress-bar progress-bar-striped active", role = "progressbar",
                style = sprintf("width: %s; min-width: 2em;", fmt_pct(overall)), fmt_pct(overall))),
        if (sm$phase == "sampling") tagList(chain_rows) else NULL,
        div(class = "bff-hint", style = "margin-top: 6px;",
            "Real progress read from the Stan output of each chain (one background R process per chain).",
            "The first warm-up iterations are usually the slowest, so a chain can stay at 0% for a while before speeding up.",
            "You can keep using the dashboard in the meantime."))
  })

  bff_format_duration <- function(secs) {
    if (is.null(secs) || is.na(secs)) return("?")
    secs <- round(secs)
    if (secs < 60) return(sprintf("%d s", secs))
    sprintf("%d min %02d s", secs %/% 60, secs %% 60)
  }

  #############################################################################
  # Synchronous fallback (used when background processes cannot be started)   #
  #############################################################################
  run_fit_synchronous <- function(options_file, data_now) {
    completed <- withProgress(message = "Fitting the model (synchronous mode)", value = 0, {
      incProgress(0.02, detail = "Generating and compiling the Stan program (about a minute the first time)...")
      prepared <- tryCatch(run_MCMC(outputFile = options_file, data.temp = data_now), error = function(e) e)
      if (inherits(prepared, "error")) {
        shinyalert::shinyalert("Model preparation failed", conditionMessage(prepared), type = "error"); return(FALSE)
      }
      if (is.character(prepared)) {
        shinyalert::shinyalert("Stan compilation failed", paste("The Stan program could not be compiled:", prepared), type = "error"); return(FALSE)
      }
      modelFitResults$results <- prepared
      n_cal <- length(prepared)
      fits <- vector("list", n_cal)
      warnings_by_period <- list()
      per_period <- 0.78 / n_cal
      for (i in seq_len(n_cal)) {
        p <- prepared[[i]]
        incProgress(per_period * 0.05,
                    detail = sprintf("Sampling calibration period %d of %d (%d time points; %d chain(s) x %d iterations)...",
                                     i, n_cal, p$calibrationperiod, p$chains, p$iter))
        stan_warnings <- character(0); stan_error <- NULL; fit <- NULL
        console_out <- utils::capture.output({
          fit <- tryCatch(
            withCallingHandlers(
              rstan::sampling(p$model, data = p$data, iter = p$iter, warmup = p$warmup, chains = p$chains,
                              cores = p$cores, seed = p$seed, control = p$control, refresh = max(1, floor(p$iter / 10))),
              warning = function(w) { stan_warnings <<- c(stan_warnings, conditionMessage(w)); invokeRestart("muffleWarning") }),
            error = function(e) { stan_error <<- conditionMessage(e); NULL })
        }, type = "output")
        diag_lines <- unique(c(stan_warnings, bff_filter_stan_console(console_out)))
        warnings_by_period[[as.character(p$calibrationperiod)]] <- diag_lines
        if (!is.null(stan_error) || is.null(fit)) {
          shinyalert::shinyalert(sprintf("Sampling failed (calibration period %d)", p$calibrationperiod),
                                 paste(c(stan_error, head(diag_lines, 8)), collapse = "\n"), type = "error"); return(FALSE)
        }
        n_draws <- tryCatch(length(rstan::extract(fit, pars = "lp__")[[1]]), error = function(e) 0L)
        if (n_draws == 0) {
          shinyalert::shinyalert(sprintf("Sampling failed (calibration period %d)", p$calibrationperiod),
                                 paste(c("Stan did not return any posterior draws.", head(diag_lines, 8)), collapse = "\n"), type = "error"); return(FALSE)
        }
        fits[[i]] <- fit
        incProgress(per_period * 0.95)
      }
      names(fits) <- names(prepared)
      list(fits = fits, warnings = warnings_by_period)
    })
    if (is.list(completed)) finish_run(completed$fits, completed$warnings, options_file, data_now)
  }

  ###########################################
  # Running only if the user confirms       #
  ###########################################
  observeEvent(input$yesPROCEED, {

    removeModal()
    req(optionsFileReactive$file)
    options_file <- optionsFileReactive$file
    data_now <- finalData$data
    if (isTRUE(async_run$active)) {
      showNotification("A run is already in progress.", type = "warning"); return(invisible(NULL))
    }
    reset_run_state()

    # Prepare the Stan program and the data (no compilation yet)
    prep <- tryCatch(bff_prepare_run(outputFile = options_file, data.temp = data_now), error = function(e) e)
    if (inherits(prep, "error")) {
      shinyalert::shinyalert("Model preparation failed", conditionMessage(prep), type = "error")
      return(invisible(NULL))
    }

    # Preferred path: background processes with real progress
    started <- FALSE
    if (requireNamespace("callr", quietly = TRUE)) {
      started <- tryCatch({ start_async_run(prep, options_file, data_now); TRUE },
                          error = function(e) { message("Background run could not be started: ", conditionMessage(e)); FALSE })
    }
    if (!started) {
      showNotification("Background processes are not available; fitting in the main process (no live progress).",
                       type = "warning", duration = 8)
      run_fit_synchronous(options_file, data_now)
    }
  })

  # Results flag used by the UI to show / hide the result boxes
  output$has_results <- reactive({ !is.null(allOutput$everything) })
  outputOptions(output, "has_results", suspendWhenHidden = FALSE)
      
#------------------------------------------------------------------------------#
# Results: helpers --------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: The output of run_analyzeResults() is a list with one element per      #
# calibration period. The helpers below give the UI a flat view of the figures  #
# and tables (figure type -> list of named plots, table type -> data frames).   #
#------------------------------------------------------------------------------#

  results_periods <- reactive({
    req(allOutput$everything)
    names(allOutput$everything)
  })

  current_period_results <- reactive({
    req(allOutput$everything)
    key <- input$selectCaliFig
    if (is.null(key) || !(key %in% names(allOutput$everything))) key <- names(allOutput$everything)[1]
    allOutput$everything[[key]]
  })

  # Figures of the selected calibration period as a named list for the selected type
  figure_catalog <- function(res, type) {
    switch(type,
           "Forecasts"                = res$forecasts.fits.plot,
           "Parameter Histograms"     = res$plot.list.params,
           "Composite Histograms"     = res$plot.list.composite,
           "Trace Plot"               = if (is.null(res$plot.list.trace)) list() else list("Trace plot" = res$plot.list.trace),
           "Posterior Plots"          = if (is.null(res$plot.list.posterior)) list() else list("Posterior densities" = res$plot.list.posterior),
           "Time-Dependent Functions" = res$plot.list.time.dependent,
           list())
  }

#------------------------------------------------------------------------------#
# Convergence card -------------------------------------------------------------
#------------------------------------------------------------------------------#

  output$convergence_card <- renderUI({
    res <- current_period_results()
    conv <- res$convergence.summary
    req(conv)
    status <- conv$Status[1] %||% "Unknown"
    kpi <- function(value, label) {
      div(class = "bff-kpi col-xs-6 col-sm-4 col-md-2",
          div(class = "bff-kpi-value", value),
          div(class = "bff-kpi-label", label))
    }
    fmt <- function(x, digits = 3) if (is.null(x) || length(x) == 0 || is.na(x)) "n/a" else format(round(x, digits))
    warn_lines <- res$stan.warnings %||% character(0)
    tagList(
      fluidRow(
        column(width = 12,
               div(style = "display: flex; align-items: center; gap: 14px; flex-wrap: wrap; margin-bottom: 8px;",
                   span(class = paste0("bff-status-badge bff-status-", status), status),
                   span(sprintf("Calibration period: %s time points", conv$CalibrationLength[1])),
                   span(class = "bff-hint", sprintf("%s chain(s), %s iterations (%s warm-up), %s posterior draws",
                                                   fmt(conv$Chains[1], 0), fmt(conv$Iterations[1], 0),
                                                   fmt(conv$Warmup[1], 0), fmt(conv$PosteriorDraws[1], 0))))
        )
      ),
      fluidRow(
        kpi(fmt(conv$MaxRhat[1]), "max R-hat (< 1.05)"),
        kpi(fmt(conv$MinESS[1], 0), "min ESS"),
        kpi(fmt(conv$Divergences[1], 0), "divergent transitions"),
        kpi(fmt(conv$MaxTreedepthHits[1], 0), "max treedepth hits"),
        kpi(fmt(conv$LowBFMIChains[1], 0), "low E-BFMI chains"),
        kpi(if (is.null(res$model.fit.metrics) || nrow(res$model.fit.metrics) == 0) "n/a" else fmt(res$model.fit.metrics$LOOIC[1], 1), "LOOIC (series 1)")
      ),
      div(style = "margin-top: 8px;", icon("info-circle"), " ", conv$Advice[1]),
      if (length(warn_lines) > 0) {
        tags$details(style = "margin-top: 8px;",
                     tags$summary(style = "cursor: pointer;", sprintf("Stan messages (%d)", length(warn_lines))),
                     div(class = "bff-mono", paste(warn_lines, collapse = "\n")))
      } else {
        div(class = "bff-hint", style = "margin-top: 6px;", "Stan reported no warnings during sampling.")
      }
    )
  })

#------------------------------------------------------------------------------#
# Figures: controls ------------------------------------------------------------
#------------------------------------------------------------------------------#

  output$select.calibration.period <- renderUI({
    req(allOutput$everything)
    periods <- names(allOutput$everything)
    pickerInput("selectCaliFig", "Calibration period", choices = periods,
                selected = isolate(input$selectCaliFig) %||% periods[1])
  })

  output$selectFigureALL <- renderUI({
    res <- current_period_results()
    choices <- c("Forecasts", "Parameter Histograms", "Trace Plot", "Posterior Plots")
    if (length(res$plot.list.composite) > 0) choices <- append(choices, "Composite Histograms", after = 2)
    if (length(res$plot.list.time.dependent) > 0) choices <- c(choices, "Time-Dependent Functions")
    pickerInput("selectFigure", "Figure", choices = choices, selected = isolate(input$selectFigure) %||% "Forecasts")
  })

  output$download.figures <- renderUI({
    req(allOutput$everything)
    actionButton("downloadFigures", "Download figures", icon = icon("download"))
  })

  # Figures of the selected type for the selected calibration period
  filtered.plots <- reactive({
    req(input$selectFigure)
    res <- current_period_results()
    plots <- figure_catalog(res, input$selectFigure)
    if (is.null(plots)) plots <- list()
    plots
  })

  figure_index_arrow <- reactiveVal(1)
  observeEvent(list(input$selectFigure, input$selectCaliFig), { figure_index_arrow(1) })
  observeEvent(input$PreviousFigure, { if (figure_index_arrow() > 1) figure_index_arrow(figure_index_arrow() - 1) })
  observeEvent(input$NextFigure, { if (figure_index_arrow() < length(filtered.plots())) figure_index_arrow(figure_index_arrow() + 1) })

  output$arrows.figures.previous <- renderUI({
    req(allOutput$everything, input$selectFigure)
    if (length(filtered.plots()) > 1) actionButton("PreviousFigure", label = icon("arrow-left")) else NULL
  })
  output$arrows.figures.next <- renderUI({
    req(allOutput$everything, input$selectFigure)
    if (length(filtered.plots()) > 1) actionButton("NextFigure", label = icon("arrow-right")) else NULL
  })

#------------------------------------------------------------------------------#
# Figures: display (interactive forecasts, static diagnostics) ------------------
#------------------------------------------------------------------------------#

  current_figure <- reactive({
    plots <- filtered.plots()
    req(length(plots) > 0)
    idx <- min(max(1, figure_index_arrow()), length(plots))
    list(plot = plots[[idx]], name = names(plots)[idx] %||% "", index = idx, n = length(plots))
  })

  output$figure_area <- renderUI({
    req(allOutput$everything, input$selectFigure)
    plots <- filtered.plots()
    if (length(plots) == 0) {
      return(div(class = "bff-hint", "No figure of this type is available for this run."))
    }
    fig <- current_figure()
    caption <- if (fig$n > 1) sprintf("%s  (%d / %d)", fig$name, fig$index, fig$n) else fig$name
    tagList(
      div(style = "text-align: center; font-weight: bold; margin-bottom: 4px;", caption),
      if (input$selectFigure == "Forecasts") {
        plotly::plotlyOutput("forecast_plotly", height = "480px")
      } else {
        plotOutput("allOutputFIGS", height = "480px")
      }
    )
  })

  output$allOutputFIGS <- renderPlot({
    fig <- current_figure()
    req(fig$plot)
    fig$plot
  })

  # Interactive forecast chart built directly with plotly (hover values, zoom)
  bff_forecast_plotly <- function(tbl, series_label, datetype, calibrationperiod, forecastinghorizon) {
    tbl <- as.data.frame(tbl)
    p <- plotly::plot_ly()
    p <- plotly::add_ribbons(p, data = tbl, x = ~Date, ymin = ~LB, ymax = ~UB, name = "95% PI",
                             fillcolor = "rgba(120,120,120,0.25)", line = list(color = "rgba(80,80,80,0.6)", dash = "dash", width = 1),
                             hovertemplate = "%{x}: %{y:.1f}<extra>95% PI</extra>")
    p <- plotly::add_lines(p, data = tbl, x = ~Date, y = ~median, name = "Posterior median",
                           line = list(color = "red", width = 2.5), hovertemplate = "%{x}: %{y:.1f}<extra>median</extra>")
    obs <- tbl[!is.na(tbl$Data), , drop = FALSE]
    if (nrow(obs) > 0) {
      p <- plotly::add_markers(p, data = obs, x = ~Date, y = ~Data, name = "Observed data",
                               marker = list(color = "white", size = 7, line = list(color = "black", width = 1.5)),
                               hovertemplate = "%{x}: %{y}<extra>observed</extra>")
    }
    shapes <- list()
    if (forecastinghorizon > 0) {
      shapes[[1]] <- list(type = "line", xref = "x", yref = "paper", x0 = calibrationperiod - 1, x1 = calibrationperiod - 1,
                          y0 = 0, y1 = 1, line = list(color = "blue", dash = "dot", width = 2))
    }
    plotly::layout(p,
                   title = list(text = sprintf("%s (calibration: %s %s%s)", series_label, calibrationperiod, tolower(datetype),
                                               if (forecastinghorizon > 0) sprintf(", forecast: %s", forecastinghorizon) else ""),
                                x = 0.02, font = list(size = 14)),
                   xaxis = list(title = datetype), yaxis = list(title = series_label),
                   shapes = shapes, legend = list(orientation = "h", y = -0.2), hovermode = "x unified",
                   margin = list(t = 50))
  }

  output$forecast_plotly <- plotly::renderPlotly({
    req(input$selectFigure == "Forecasts")
    fig <- current_figure()
    res <- current_period_results()
    tbl <- res$forecasts.fits[[fig$name]]
    req(tbl)
    opt <- tryCatch(bff_load_options(optionsFileReactive$file), error = function(e) NULL)
    datetype <- if (!is.null(opt)) opt$datetype %||% "Time" else "Time"
    h <- if (!is.null(opt)) as.numeric(opt$forecastinghorizon) else 0
    cal <- as.numeric(input$selectCaliFig %||% names(allOutput$everything)[1])
    bff_forecast_plotly(tbl, fig$name, datetype, cal, h)
  })

#------------------------------------------------------------------------------#
# Figures: download (all figures of all calibration periods) --------------------
#------------------------------------------------------------------------------#

  observeEvent(input$downloadFigures, {
    showModal(modalDialog(
      title = "Figure specifications",
      numericInput("dpi", "Resolution (dpi):", value = 300, min = 72),
      numericInput("width", "Width:", value = 9),
      numericInput("height", "Height:", value = 5),
      pickerInput("units", label = "Unit:", choices = c("in", "cm", "mm", "px")),
      pickerInput("extFig", label = "File type:", choices = c("png", "pdf", "svg", "tiff", "jpeg", "eps")),
      downloadButton("downloadFigsButton", "Download all figures (.zip)"),
      easyClose = TRUE
    ))
  })

  # Flat list of every figure: name -> ggplot
  plots.to.save <- reactive({
    req(allOutput$everything)
    out <- list()
    for (period in names(allOutput$everything)) {
      res <- allOutput$everything[[period]]
      for (type in c("Forecasts", "Parameter Histograms", "Composite Histograms", "Trace Plot", "Posterior Plots", "Time-Dependent Functions")) {
        plots <- figure_catalog(res, type)
        if (length(plots) == 0) next
        base <- switch(type,
                       "Forecasts" = "forecast", "Parameter Histograms" = "parameter",
                       "Composite Histograms" = "composite", "Trace Plot" = "traceplot",
                       "Posterior Plots" = "posterior", "Time-Dependent Functions" = "time-dependent")
        for (k in seq_along(plots)) {
          nm <- names(plots)[k] %||% as.character(k)
          nm <- gsub("[^A-Za-z0-9_.-]+", "_", nm)
          out[[sprintf("%s-%s-cal-%s", base, nm, period)]] <- plots[[k]]
        }
      }
    }
    out
  })

  save_plot_file <- function(plot_obj, path, ext, width, height, units, dpi) {
    if (ext == "tiff") {
      ggplot2::ggsave(path, plot = plot_obj, dpi = dpi, width = width, height = height,
                      units = units, device = ext, compression = "lzw")
    } else if (ext == "eps") {
      ggplot2::ggsave(path, plot = plot_obj, width = width, height = height, units = units,
                      device = grDevices::cairo_ps)
    } else {
      ggplot2::ggsave(path, plot = plot_obj, dpi = dpi, width = width, height = height,
                      units = units, device = ext)
    }
  }

  output$downloadFigsButton <- downloadHandler(
    filename = function() { sprintf("BayesianFitForecast-figures-%s.zip", format(Sys.time(), "%Y%m%d-%H%M")) },
    content = function(file) {
      removeModal()
      temp_directory <- file.path(tempdir(), paste0("figs_", as.integer(Sys.time())))
      dir.create(temp_directory, showWarnings = FALSE)
      plots <- plots.to.save()
      withProgress(message = "Saving figures", value = 0, {
        for (plot_name in names(plots)) {
          incProgress(1 / length(plots), detail = plot_name)
          tryCatch(
            save_plot_file(plots[[plot_name]], file.path(temp_directory, paste0(plot_name, ".", input$extFig)),
                           input$extFig, input$width, input$height, input$units, input$dpi),
            error = function(e) message("Could not save ", plot_name, ": ", conditionMessage(e))
          )
        }
      })
      zip::zip(zipfile = file, files = dir(temp_directory), root = temp_directory)
    },
    contentType = "application/zip"
  )

#------------------------------------------------------------------------------#
# Tables -----------------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Forecast tables are shown per calibration period and series; the other #
# tables are stacked over the calibration periods.                              #
#------------------------------------------------------------------------------#

  output$download.data <- renderUI({
    req(allOutput$everything)
    downloadButton("download_data", "Download tables (.zip of CSV)")
  })

  output$download.everything <- renderUI({
    req(allOutput$everything)
    tagList(downloadButton("download_everything", "Download everything (.zip)", class = "btn-primary"),
            downloadButton("download_report", "Download report (.html)"))
  })

  # Figures (PNG) of the current results written into <root>/figures, for the report / archive
  write_figures_to_dir <- function(root) {
    dir.create(file.path(root, "figures"), showWarnings = FALSE, recursive = TRUE)
    plots <- plots.to.save()
    for (plot_name in names(plots)) {
      tryCatch(save_plot_file(plots[[plot_name]], file.path(root, "figures", paste0(plot_name, ".png")), "png", 9, 5, "in", 150),
               error = function(e) NULL)
    }
    invisible(length(plots))
  }

  current_run_meta_list <- function() {
    list(started = run_meta$started, finished = run_meta$finished, options = run_meta$options,
         stan_code = run_meta$stan_code, warnings = run_meta$warnings)
  }

  output$download_report <- downloadHandler(
    filename = function() { sprintf("BayesianFitForecast-report-%s.html", format(Sys.time(), "%Y%m%d-%H%M")) },
    content = function(file) {
      root <- file.path(tempdir(), paste0("report_", as.integer(Sys.time())))
      dir.create(root, showWarnings = FALSE)
      withProgress(message = "Preparing the report", value = 0.2, {
        incProgress(0.3, detail = "figures")
        write_figures_to_dir(root)
        incProgress(0.4, detail = "report")
        bff_write_run_report(root, allOutput$everything, current_run_meta_list(), finalData$data,
                             embed_images = TRUE, file = file)
      })
    },
    contentType = "text/html"
  )

  output$selectDatall <- renderUI({
    req(allOutput$everything)
    res <- allOutput$everything[[1]]
    choices <- c("Forecasts", "Parameters", "Performance Metrics", "Model Fit Metrics (DIC / WAIC / LOO)",
                 "Convergence Summary", "Posterior Draws")
    if (!is.null(res$time.dependent.values) && NROW(res$time.dependent.values) > 0) {
      choices <- c(choices, "Time-Dependent Functions")
    }
    pickerInput("selectData", "Table", choices = choices, selected = isolate(input$selectData) %||% "Forecasts")
  })

  # All tables as a named list of data frames (used for display and download)
  data.to.save.final <- reactive({
    req(allOutput$everything)
    out <- list()
    stack <- function(field) {
      parts <- lapply(allOutput$everything, function(res) res[[field]])
      parts <- parts[!vapply(parts, function(x) is.null(x) || NROW(x) == 0, logical(1))]
      if (length(parts) == 0) return(data.frame())
      tryCatch(do.call(rbind, unname(parts)), error = function(e) parts[[1]])
    }
    for (period in names(allOutput$everything)) {
      res <- allOutput$everything[[period]]
      for (series in names(res$forecasts.fits)) {
        key <- sprintf("forecast-%s-cal-%s", gsub("[^A-Za-z0-9_.-]+", "_", series), period)
        out[[key]] <- res$forecasts.fits[[series]]
      }
    }
    out[["parameters"]]            <- stack("add.parameter.values")
    out[["performance-metrics"]]   <- stack("performance.metrics")
    out[["model-fit-metrics"]]     <- stack("model.fit.metrics")
    out[["convergence-summary"]]   <- stack("convergence.summary")
    out[["time-dependent-functions"]] <- stack("time.dependent.values")
    for (period in names(allOutput$everything)) {
      draws <- allOutput$everything[[period]]$posterior.draws
      if (!is.null(draws) && NROW(draws) > 0) out[[sprintf("posterior-draws-cal-%s", period)]] <- draws
    }
    out
  })

  # The data sets shown for the selected table type (list for the arrow navigation)
  fitlered.data.sets <- reactive({
    req(input$selectData)
    all <- data.to.save.final()
    switch(input$selectData,
           "Forecasts" = all[grepl("^forecast-", names(all))],
           "Parameters" = list("parameters" = all[["parameters"]]),
           "Performance Metrics" = list("performance-metrics" = all[["performance-metrics"]]),
           "Model Fit Metrics (DIC / WAIC / LOO)" = list("model-fit-metrics" = all[["model-fit-metrics"]]),
           "Convergence Summary" = list("convergence-summary" = all[["convergence-summary"]]),
           "Posterior Draws" = all[grepl("^posterior-draws-", names(all))],
           "Time-Dependent Functions" = list("time-dependent-functions" = all[["time-dependent-functions"]]),
           list())
  })

  data_index_arrow <- reactiveVal(1)
  observeEvent(input$selectData, { data_index_arrow(1) })
  observeEvent(input$PreviousData, { if (data_index_arrow() > 1) data_index_arrow(data_index_arrow() - 1) })
  observeEvent(input$NextData, { if (data_index_arrow() < length(fitlered.data.sets())) data_index_arrow(data_index_arrow() + 1) })

  output$arrows.data.previous <- renderUI({
    req(allOutput$everything, input$selectData)
    if (length(fitlered.data.sets()) > 1) actionButton("PreviousData", label = icon("arrow-left")) else NULL
  })
  output$arrows.data.next <- renderUI({
    req(allOutput$everything, input$selectData)
    if (length(fitlered.data.sets()) > 1) actionButton("NextData", label = icon("arrow-right")) else NULL
  })

  current_table <- reactive({
    sets <- fitlered.data.sets()
    req(length(sets) > 0)
    idx <- min(max(1, data_index_arrow()), length(sets))
    list(data = sets[[idx]], name = names(sets)[idx], index = idx, n = length(sets))
  })

  output$forecastDataTitle <- renderText({
    req(allOutput$everything, input$selectData)
    tb <- current_table()
    nm <- tb$name
    if (grepl("^forecast-", nm)) {
      series <- sub("^forecast-(.*)-cal-([0-9.]+)$", "\\1", nm)
      cal <- sub("^forecast-(.*)-cal-([0-9.]+)$", "\\2", nm)
      sprintf("Forecast table: %s (calibration %s)%s", gsub("_", " ", series), cal,
              if (tb$n > 1) sprintf("  [%d / %d]", tb$index, tb$n) else "")
    } else if (grepl("^posterior-draws-", nm)) {
      sprintf("Posterior draws (calibration %s)%s", sub("^posterior-draws-cal-", "", nm),
              if (tb$n > 1) sprintf("  [%d / %d]", tb$index, tb$n) else "")
    } else {
      switch(input$selectData,
             "Parameters" = "Posterior summary of the parameters (Rhat > 1.05 highlighted)",
             "Performance Metrics" = "Performance metrics (calibration and forecasting periods)",
             "Model Fit Metrics (DIC / WAIC / LOO)" = "Model-fit metrics on the calibration window (lower is better)",
             "Convergence Summary" = "Convergence diagnostics",
             "Time-Dependent Functions" = "Posterior median and 95% CrI of the time-dependent parameters",
             input$selectData)
    }
  })

  output$dataTableRender <- DT::renderDataTable({
    tb <- current_table()
    df <- as.data.frame(tb$data)
    req(NROW(df) > 0)
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    int_like <- num_cols[vapply(num_cols, function(nm) all(is.na(df[[nm]]) | df[[nm]] == round(df[[nm]])), logical(1))]
    dec_cols <- setdiff(num_cols, int_like)
    dt <- DT::datatable(df, options = list(scrollX = TRUE, pageLength = 15), rownames = FALSE)
    if (length(dec_cols) > 0) dt <- DT::formatRound(dt, dec_cols, digits = 3)
    if (length(int_like) > 0) dt <- DT::formatRound(dt, int_like, digits = 0)
    if ("Rhat" %in% names(df)) {
      dt <- DT::formatStyle(dt, "Rhat", target = "row",
                            backgroundColor = DT::styleInterval(c(1.05, 1.1), c("white", "#fff3cd", "#f8d7da")))
    }
    if ("Status" %in% names(df)) {
      dt <- DT::formatStyle(dt, "Status",
                            backgroundColor = DT::styleEqual(c("Good", "Caution", "Poor"), c("#d4edda", "#fff3cd", "#f8d7da")))
    }
    dt
  })

  write_tables_to_dir <- function(dir) {
    all <- data.to.save.final()
    for (nm in names(all)) {
      df <- all[[nm]]
      if (is.null(df) || NROW(df) == 0) next
      utils::write.csv(as.data.frame(df), file.path(dir, paste0(nm, ".csv")), row.names = FALSE)
    }
  }

  output$download_data <- downloadHandler(
    filename = function() { sprintf("BayesianFitForecast-tables-%s.zip", format(Sys.time(), "%Y%m%d-%H%M")) },
    content = function(file) {
      temp_directory <- file.path(tempdir(), paste0("tables_", as.integer(Sys.time())))
      dir.create(temp_directory, showWarnings = FALSE)
      write_tables_to_dir(temp_directory)
      zip::zip(zipfile = file, files = dir(temp_directory), root = temp_directory)
    },
    contentType = "application/zip"
  )

#------------------------------------------------------------------------------#
# Download everything: figures + tables + configuration + Stan + fits + report ---
#------------------------------------------------------------------------------#

  output$download_everything <- downloadHandler(
    filename = function() { sprintf("BayesianFitForecast-run-%s.zip", format(Sys.time(), "%Y%m%d-%H%M")) },
    content = function(file) {
      root <- file.path(tempdir(), paste0("run_", as.integer(Sys.time())))
      dir.create(root, showWarnings = FALSE)
      dir.create(file.path(root, "tables"), showWarnings = FALSE)
      withProgress(message = "Preparing the archive", value = 0, {
        incProgress(0.1, detail = "configuration, Stan program and data")
        if (!is.null(run_meta$options)) writeLines(run_meta$options, file.path(root, "options.R"))
        if (!is.null(run_meta$stan_code)) writeLines(run_meta$stan_code, file.path(root, "ode_model.stan"))
        if (!is.null(finalData$data)) utils::write.csv(finalData$data, file.path(root, "data.csv"), row.names = FALSE)
        incProgress(0.2, detail = "tables")
        write_tables_to_dir(file.path(root, "tables"))
        incProgress(0.4, detail = "figures")
        write_figures_to_dir(root)
        incProgress(0.2, detail = "report")
        tryCatch(bff_write_run_report(root, allOutput$everything, current_run_meta_list(), finalData$data, embed_images = TRUE),
                 error = function(e) message("Report skipped: ", conditionMessage(e)))
      })
      zip::zip(zipfile = file, files = dir(root, recursive = TRUE), root = root)
    },
    contentType = "application/zip"
  )

#------------------------------------------------------------------------------#
# Prior tools: density preview and prior predictive check ----------------------
#------------------------------------------------------------------------------#

  observeEvent(input$previewPriors, {
    priors <- parameters.inital.list$list
    disp <- parameters.list$list
    fixed <- parameters.fixed.list$list
    if (is.null(priors) || length(priors) == 0) {
      shinyalert::shinyalert("No priors yet", "Define the parameters and their priors first (step 5).", type = "warning")
      return(invisible(NULL))
    }
    labels <- vapply(seq_along(priors), function(i) {
      nm <- if (i <= length(disp)) as.character(disp[[i]]) else paste0("param", i)
      nm <- gsub("[\\\\{}]", "", nm)
      if (!nzchar(nm)) paste0("param", i) else nm
    }, character(1))
    spec <- data.frame(label = labels, prior = as.character(unlist(priors)),
                       fixed = as.numeric(fixed %||% rep(0, length(priors))),
                       stringsAsFactors = FALSE)
    err_priors <- parameters.inital.dist.list$list
    if (errorStructureNumber() %in% c(1, 2) && length(err_priors) > 0) {
      err_lab <- if (errorStructureNumber() == 1) "1/phi (inverse over-dispersion)" else "sigma"
      spec <- rbind(spec, data.frame(label = paste0(err_lab, " [series ", seq_along(err_priors), "]"),
                                     prior = as.character(unlist(err_priors)), fixed = 0, stringsAsFactors = FALSE))
    }
    p <- tryCatch(bff_prior_density_plot(spec), error = function(e) e)
    if (inherits(p, "error")) {
      shinyalert::shinyalert("Prior preview", conditionMessage(p), type = "error")
      return(invisible(NULL))
    }
    output$prior_preview_plot <- renderPlot({ p })
    showModal(modalDialog(
      title = "Prior distributions",
      p(class = "bff-hint", "Density of each prior (fixed parameters are shown as a vertical line). Truncation at zero is applied where specified."),
      plotOutput("prior_preview_plot", height = "520px"),
      size = "l", easyClose = TRUE, footer = modalButton("Close")
    ))
  })

  observeEvent(input$priorPredictive, {

    # The model must be consistent before simulating from the priors
    if (isTRUE(model_dirty()) || is.null(odeEquation$system)) {
      ok <- validate_model(show_success = FALSE)
      if (!isTRUE(ok)) return(invisible(NULL))
    }
    if (is.null(finalData$data)) {
      shinyalert::shinyalert("No data", "Please upload the data first so the simulated trajectories can be compared with it.", type = "warning")
      return(invisible(NULL))
    }
    rd <- readiness()
    missing <- rd[!rd$done & !rd$optional & rd$label != "Model validated", , drop = FALSE]
    if (nrow(missing) > 0) {
      shinyalert::shinyalert("Not ready yet",
                             paste0("Please complete:\n", paste(sprintf("• %s — %s", missing$label, missing$detail), collapse = "\n")),
                             type = "error")
      return(invisible(NULL))
    }
    options_content <- tryCatch(build_options_from_form(), error = function(e) e)
    if (inherits(options_content, "error")) {
      shinyalert::shinyalert("Configuration", conditionMessage(options_content), type = "error")
      return(invisible(NULL))
    }
    tmp <- file.path(tempdir(), "options_prior_check.R")
    writeLines(options_content, tmp)

    result <- withProgress(message = "Simulating from the priors", value = 0.3, {
      tryCatch(bff_prior_predictive_check(tmp, finalData$data, n_sims = 200), error = function(e) e)
    })
    if (inherits(result, "error")) {
      shinyalert::shinyalert("Prior predictive check failed", conditionMessage(result), type = "error")
      return(invisible(NULL))
    }
    output$prior_pred_plot <- renderPlot({ result$plot })
    showModal(modalDialog(
      title = "Prior predictive check",
      p(class = "bff-hint", result$message),
      plotOutput("prior_pred_plot", height = "520px"),
      size = "l", easyClose = TRUE, footer = modalButton("Close")
    ))
  })

#------------------------------------------------------------------------------#
# Configuration file: upload, validation ----------------------------------------
#------------------------------------------------------------------------------#

  config_state <- reactiveValues(status = "empty", messages = character(0), loaded_name = NULL)

  load_configuration <- function(text, data = NULL, name = "configuration") {
    updateTextAreaInput(session, "config_text", value = text)
    config_state$pushed <- text
    if (!is.null(data)) {
      listTimeseries$data <- as.data.frame(data)
      listTimeseries$name <- name
    }
    check <- bff_validate_options_text(text, if (!is.null(data)) data else finalData$data)
    config_state$status <- if (check$ok) "valid" else "invalid"
    config_state$messages <- c(check$errors, check$warnings)
    config_state$loaded_name <- name
    config_state$env <- check$env
    updatePrettyCheckbox(session, "config_mode", value = TRUE)
    updateTabsetPanel(session, "EQUATIONS", selected = "ConfigTab")
    if (check$ok) {
      shinyalert::shinyalert("Configuration loaded",
                             paste0(name, " was loaded", if (!is.null(data)) " together with its data set" else "",
                                    ". Review it in the 'Configuration file' tab and press 'Fit model & forecast'."),
                             type = "success")
    } else {
      shinyalert::shinyalert("Configuration loaded with problems", paste(check$errors, collapse = "\n"), type = "warning")
    }
  }

  observeEvent(input$load_config, {
    up <- input$upload_config
    if (is.null(up) || nrow(up) == 0) {
      shinyalert::shinyalert("Nothing to load", "Choose an options.R file first (Browse...).", type = "warning")
      return(invisible(NULL))
    }
    txt <- paste(readLines(up$datapath[1], warn = FALSE), collapse = "\n")
    load_configuration(txt, data = NULL, name = up$name[1])
  })

  observeEvent(input$validate_config, {
    txt <- input$config_text %||% ""
    if (!nzchar(trimws(txt))) {
      shinyalert::shinyalert("Empty configuration", "The configuration is empty. Press 'Fit model & forecast', open a saved run or upload an options.R file.", type = "warning")
      return(invisible(NULL))
    }
    check <- bff_validate_options_text(txt, finalData$data)
    config_state$status <- if (check$ok) "valid" else "invalid"
    config_state$messages <- c(check$errors, check$warnings)
    config_state$env <- check$env
    config_state$pushed <- txt
    if (check$ok) {
      shinyalert::shinyalert("Configuration valid",
                             paste0(check$summary, if (length(check$warnings) > 0) paste0("\n\nNotes:\n", paste(check$warnings, collapse = "\n")) else ""),
                             type = "success")
    } else {
      shinyalert::shinyalert("Configuration problems", paste(check$errors, collapse = "\n"), type = "error")
    }
  })

  # Any manual edit invalidates the previous validation (text pushed by the app itself is kept valid)
  observeEvent(input$config_text, {
    if (config_state$status == "valid" && !identical(input$config_text, config_state$pushed %||% "")) {
      config_state$status <- "edited"
    }
  }, ignoreInit = TRUE)

  output$config_status <- renderUI({
    status <- config_state$status
    msgs <- config_state$messages
    badge <- switch(status,
                    "valid"   = span(class = "bff-status-badge bff-status-Good", "Valid"),
                    "invalid" = span(class = "bff-status-badge bff-status-Poor", "Invalid"),
                    "edited"  = span(class = "bff-status-badge bff-status-Caution", "Edited - validate again"),
                    span(class = "bff-status-badge bff-status-Unknown", "Not validated"))
    tagList(
      div(style = "margin-top: 12px;", badge),
      if (length(msgs) > 0) div(class = "bff-mono", style = "margin-top: 8px;", paste(msgs, collapse = "\n")) else NULL,
      div(class = "bff-hint", style = "margin-top: 8px;",
          if (isTRUE(input$config_mode)) "The configuration file above will be used when fitting (the form is ignored)." else
            "Tick the box to fit the model from this file instead of the form.")
    )
  })

  output$download_config <- downloadHandler(
    filename = function() {
      nm <- tryCatch({
        env <- new.env(); eval(parse(text = input$config_text), envir = env); env$model_name
      }, error = function(e) NULL)
      paste0("options-", if (is.null(nm)) "model" else gsub("[^A-Za-z0-9_.-]+", "-", nm), ".R")
    },
    content = function(file) { writeLines(input$config_text %||% "", file) }
  )

#------------------------------------------------------------------------------#
# Model library: cards, details and 'Use this model' ------------------------------
#------------------------------------------------------------------------------#
# About: The library (model_library.R) describes each model. 'Use this model'     #
# rebuilds the form: state rows with initial conditions and fit assignment,       #
# parameter rows with priors / fixed values, composite rows, and the equations    #
# (sent to the ODE builder as keypad tokens). Parameters that appear in the       #
# initial conditions get their automatic rows; their defaults are stored by name. #
#------------------------------------------------------------------------------#

  model_library <- bff_model_library()

  output$model_library_ui <- renderUI({
    data_now <- finalData$data
    families <- unique(vapply(model_library, function(m) m$family, character(1)))
    groups <- lapply(families, function(f) {
      ms <- Filter(function(m) identical(m$family, f), model_library)
      tagList(h4(class = "bff-model-family", f),
              div(class = "bff-model-grid", lapply(ms, function(m) bff_model_card_ui(m, compact = TRUE, data = data_now))))
    })
    tagList(
      groups,
      div(class = "bff-add-bar", style = "margin-top: 14px;",
          downloadButton("download_model_library", "Download the model library report (.html, all models)"))
    )
  })

  # Fill the whole form from a library model
  apply_model_template <- function(model_id) {
    model <- model_library[[model_id]]
    if (is.null(model)) return(invisible(FALSE))
    model <- bff_model_resolve_defaults(model, finalData$data)

    # New generation: the automatic rows of the initial-condition parameters are rebuilt with this model's defaults
    template_gen(isolate(template_gen()) + 1L)

    # 1. Clear the current model
    clear_state_rows()
    clear_composite_rows()
    user_param_keys(character(0))
    rm(list = ls(envir = required_defaults), envir = required_defaults)
    ode_preset(list(nonce = stats::runif(1), equations = list()))

    # 2. Parameters (rows for the ones that appear in the initial conditions are automatic)
    ic_params <- bff_model_ic_params(model)
    param_key_map <- character(0)
    for (p in model$params) {
      d <- list(name = p$latex, fixed = p$fixed, value = p$value, prior = p$prior, args = p$args,
                trunc = p$trunc, lb = p$lb, ub = p$ub)
      if (p$name %in% ic_params) {
        assign(p$name, d, envir = required_defaults)
        param_key_map[p$name] <- req_key(p$name)
      } else {
        param_key_map[p$name] <- add_param_row(d)
      }
    }

    # 3. State variables
    state_key_map <- character(0)
    for (s in model$states) {
      state_key_map[s$name] <- add_state_row(list(name = s$name, ic = s$ic, fit = s$fit, series = s$series, diff = s$diff))
    }

    # 4. Composite variables
    for (cp in model$composites) add_composite_row(list(name = cp$name, latex = cp$latex))

    # 5. Equations (tokens keyed by the row keys; resolved live by the ODE builder)
    lookup <- bff_model_lookup(model, state_keys = state_key_map, param_keys = param_key_map)
    eqs <- lapply(model$states, function(s) ode_builder_tokenize_text(model$equations[[s$name]] %||% "", lookup))
    names(eqs) <- unname(state_key_map[vapply(model$states, function(s) s$name, character(1))])
    ode_preset(list(nonce = stats::runif(1), equations = eqs))

    # 6. Sidebar settings
    updateTextInput(session, "model_name", value = model$id)
    updatePickerInput(session, "errstrc", selected = model$errstrc)
    active_template(model$id)
    model_dirty(TRUE)

    # 7. Feedback
    updateTabsetPanel(session, "EQUATIONS", selected = "state_variables")
    n_est <- sum(!vapply(model$params, function(p) p$fixed, logical(1)))
    todo <- c(
      if (any(vapply(model$params, function(p) identical(p$name, "N"), logical(1)))) "set N (population size) in step 5" else NULL,
      if (is.null(finalData$data)) "load the data (step 1) and assign the fitted compartment to the time series (step 4)" else "check the time series assigned to the fitted compartment (step 4)",
      "review the priors (step 5)"
    )
    shinyalert::shinyalert(
      title = sprintf("%s loaded", model$title),
      text = sprintf("%d state variable%s, %d parameter%s (%d estimated) and the equations were filled in. Before fitting: %s. Press 'Validate equations' (step 7) or go directly to 'Fit model & forecast'.",
                     length(model$states), if (length(model$states) == 1) "" else "s",
                     length(model$params), if (length(model$params) == 1) "" else "s", n_est,
                     paste(todo, collapse = "; ")),
      type = "success")
    invisible(TRUE)
  }

  pending_model <- reactiveVal(NULL)

  observeEvent(input$use_model, {
    id <- input$use_model$id
    req(!is.null(id), id %in% names(model_library))
    has_content <- length(state_rows$keys) > 0 || length(user_param_keys()) > 0 || length(composite_rows$keys) > 0
    if (has_content) {
      pending_model(id)
      showModal(modalDialog(
        title = "Replace the current model?",
        p(sprintf("Loading '%s' replaces the current state variables, parameters, equations and composite variables.", model_library[[id]]$title)),
        p(class = "bff-hint", "The data, calibration settings and MCMC settings are kept."),
        footer = tagList(actionButton("confirm_use_model", "Replace", class = "btn-primary"), modalButton("Cancel")),
        easyClose = TRUE))
    } else {
      apply_model_template(id)
    }
  })

  observeEvent(input$confirm_use_model, {
    removeModal()
    id <- pending_model()
    req(!is.null(id))
    pending_model(NULL)
    apply_model_template(id)
  })

  details_model <- reactiveVal(NULL)

  observeEvent(input$model_details, {
    id <- input$model_details$id
    req(!is.null(id), id %in% names(model_library))
    details_model(id)
    showModal(modalDialog(
      title = NULL,
      bff_model_card_ui(model_library[[id]], compact = FALSE, data = finalData$data),
      size = "l", easyClose = TRUE,
      footer = tagList(
        downloadButton("download_model_sheet", "Download model sheet (.html)"),
        actionButton("use_model_from_details", "Use this model", class = "btn-primary", icon = icon("download")),
        modalButton("Close"))
    ))
  })

  observeEvent(input$use_model_from_details, {
    id <- details_model()
    removeModal()
    req(!is.null(id))
    has_content <- length(state_rows$keys) > 0 || length(user_param_keys()) > 0 || length(composite_rows$keys) > 0
    if (has_content) {
      pending_model(id)
      showModal(modalDialog(
        title = "Replace the current model?",
        p(sprintf("Loading '%s' replaces the current state variables, parameters, equations and composite variables.", model_library[[id]]$title)),
        footer = tagList(actionButton("confirm_use_model", "Replace", class = "btn-primary"), modalButton("Cancel")),
        easyClose = TRUE))
    } else {
      apply_model_template(id)
    }
  })

  output$download_model_library <- downloadHandler(
    filename = function() { "BayesianFitForecast-model-library.html" },
    content = function(file) { bff_write_model_sheet(model_library, file, data = finalData$data) },
    contentType = "text/html"
  )

  output$download_model_sheet <- downloadHandler(
    filename = function() { paste0("BayesianFitForecast-model-", details_model() %||% "model", ".html") },
    content = function(file) {
      id <- details_model()
      req(!is.null(id))
      bff_write_model_sheet(model_library[[id]], file, data = finalData$data)
    },
    contentType = "text/html"
  )

#------------------------------------------------------------------------------#
# Run history ---------------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: completed runs are saved by finish_run() (see bff_save_run); this        #
# section lists them and re-opens / downloads / deletes them.                     #
#------------------------------------------------------------------------------#

  runs_dir <- bff_runs_dir()
  # Run folders written by an earlier version still holding the fitted objects:
  # those move to the cache the first time the app starts (figures/tables stay)
  tryCatch(bff_migrate_old_runs(runs_dir), error = function(e) message("Run migration skipped: ", conditionMessage(e)))
  run_history_tick <- reactiveVal(0)


  observeEvent(input$refresh_runs, { run_history_tick(run_history_tick() + 1) })

  # Sidebar picker of the saved runs (empty until the first completed run)
  observe({
    run_history_tick()
    runs <- tryCatch(bff_list_runs(runs_dir), error = function(e) NULL)
    if (is.null(runs) || nrow(runs) == 0) {
      updatePickerInput(session, "saved_run", choices = c("No saved run yet" = ""), selected = "")
      return(invisible(NULL))
    }
    labels <- sprintf("%s  (%s, %s)", runs$model_name, runs$finished, runs$status)
    current <- isolate(input$saved_run)
    updatePickerInput(session, "saved_run", choices = stats::setNames(runs$id, labels),
                      selected = if (!is.null(current) && current %in% runs$id) current else runs$id[1])
  })

  observeEvent(input$open_saved_run_btn, {
    id <- input$saved_run
    if (is.null(id) || !nzchar(id)) {
      shinyalert::shinyalert("No saved run", "Runs are saved automatically after 'Fit model & forecast'; the list is empty until the first run is completed.", type = "info")
      return(invisible(NULL))
    }
    loaded <- tryCatch(bff_load_run(id, runs_dir), error = function(e) e)
    if (inherits(loaded, "error")) {
      shinyalert::shinyalert("Run could not be opened", conditionMessage(loaded), type = "error"); return(invisible(NULL))
    }
    open_saved_run(loaded)
  })

  output$run_history_ui <- renderUI({
    run_history_tick()
    runs <- tryCatch(bff_list_runs(runs_dir), error = function(e) NULL)
    if (is.null(runs) || nrow(runs) == 0) {
      return(div(class = "bff-hint", style = "margin-top: 10px;",
                 icon("info-circle"), " No saved run yet. Every completed 'Fit model & forecast' run is added here automatically."))
    }
    items <- lapply(seq_len(nrow(runs)), function(i) {
      r <- runs[i, ]
      err_txt <- if (is.na(r$errstrc)) "" else c("negative binomial", "normal", "Poisson")[r$errstrc]
      btn <- function(action, label, icon_name, class = "btn-default") {
        tags$button(type = "button", class = paste("btn btn-sm", class),
                    onclick = sprintf("Shiny.setInputValue('run_history_action', {action: '%s', id: '%s', nonce: Math.random()}, {priority: 'event'});", action, r$id),
                    icon(icon_name), paste0(" ", label))
      }
      div(class = "bff-run-item",
          div(
            div(class = "bff-run-title", r$model_name,
                if (!is.na(r$model_template)) span(class = "label label-default", style = "margin-left: 6px;", r$model_template) else NULL,
                span(class = paste0("bff-status-badge bff-status-", r$status), style = "margin-left: 8px; font-size: 12px; padding: 2px 8px;", r$status)),
            div(class = "bff-run-meta",
                sprintf("%s  |  %s  |  calibration %s, horizon %s  |  %s chain(s) x %s iterations  |  %s  |  %d parameter(s), %s estimated  |  %s MB",
                        r$finished, if (is.na(r$elapsed_min)) "" else sprintf("%.1f min", r$elapsed_min),
                        r$calibration, if (is.na(r$horizon)) "?" else r$horizon,
                        if (is.na(r$num_chain)) "?" else r$num_chain, if (is.na(r$niter)) "?" else format(r$niter, big.mark = ","),
                        err_txt, r$n_params, if (is.na(r$n_estimated)) "?" else r$n_estimated, format(r$size_mb, nsmall = 1)),
                if (!r$has_fits) span(style = "color: #dd4b39;", " | fitted objects not cached: 'Open' loads the configuration and data only (press 'Fit model & forecast' to reproduce)") else NULL),
            div(class = "bff-run-meta",
                sprintf("States: %s  |  data: %s (%s points, %s series)", r$vars,
                        if (is.na(r$data_name)) "uploaded data" else r$data_name,
                        if (is.na(r$n_points)) "?" else r$n_points, if (is.na(r$n_series)) "?" else r$n_series))),
          div(class = "bff-run-actions",
              btn("open", "Open", "folder-open", "btn-primary"),
              btn("download", "Download (.zip)", "download"),
              btn("delete", "Delete", "trash", "btn-danger")))
    })
    tagList(items)
  })

  pending_delete <- reactiveVal(NULL)
  pending_download <- reactiveVal(NULL)

  observeEvent(input$run_history_action, {
    act <- input$run_history_action
    id <- basename(as.character(act$id %||% ""))
    req(nzchar(id))
    if (identical(act$action, "open")) {
      loaded <- tryCatch(bff_load_run(id, runs_dir), error = function(e) e)
      if (inherits(loaded, "error")) {
        shinyalert::shinyalert("Run could not be opened", conditionMessage(loaded), type = "error"); return(invisible(NULL))
      }
      open_saved_run(loaded)
    } else if (identical(act$action, "delete")) {
      pending_delete(id)
      showModal(modalDialog(
        title = "Delete this run?",
        p(sprintf("The folder '%s' (configuration, Stan program, data, figures, tables) and its cached fitted objects will be deleted permanently.", id)),
        footer = tagList(actionButton("confirm_delete_run", "Delete", class = "btn-danger"), modalButton("Cancel")), easyClose = TRUE))
    } else if (identical(act$action, "download")) {
      pending_download(id)
      showModal(modalDialog(
        title = "Download run",
        p(sprintf("Archive of the folder '%s': options.R, ode_model.stan, data.csv, figures/ (PNG) and tables/ (CSV).", id)),
        footer = tagList(downloadButton("download_saved_run", "Download .zip"), modalButton("Close")), easyClose = TRUE))
    }
  })

  observeEvent(input$confirm_delete_run, {
    removeModal()
    id <- pending_delete()
    req(!is.null(id))
    ok <- tryCatch(bff_delete_run(id, runs_dir), error = function(e) FALSE)
    pending_delete(NULL)
    run_history_tick(run_history_tick() + 1)
    showNotification(if (isTRUE(ok)) sprintf("Run '%s' deleted.", id) else sprintf("Run '%s' could not be deleted.", id),
                     type = if (isTRUE(ok)) "message" else "error")
  })

  output$download_saved_run <- downloadHandler(
    filename = function() { paste0("BayesianFitForecast-run-", pending_download() %||% "run", ".zip") },
    content = function(file) {
      id <- pending_download()
      req(!is.null(id))
      removeModal()
      bff_zip_run(id, file, runs_dir)
    },
    contentType = "application/zip"
  )

  # Show a saved run again: configuration + data are restored; the results are
  # recomputed from the cached fitted objects (a few seconds, no refitting).
  open_saved_run <- function(loaded) {
    if (isTRUE(async_run$active)) {
      showNotification("A run is in progress; wait for it to finish before opening a saved run.", type = "warning"); return(invisible(NULL))
    }
    if (!is.null(loaded$data)) {
      listTimeseries$data <- as.data.frame(loaded$data)
      listTimeseries$name <- loaded$data_name %||% paste0(loaded$id, "/data.csv")
    }
    if (!is.null(loaded$options)) {
      updateTextAreaInput(session, "config_text", value = loaded$options)
      config_state$pushed <- loaded$options
      config_state$status <- "valid"
      config_state$messages <- character(0)
      config_state$loaded_name <- loaded$id
      updatePrettyCheckbox(session, "config_mode", value = TRUE)
    }

    if (is.null(loaded$fits) || is.null(loaded$options) || is.null(loaded$data)) {
      allOutput$everything <- NULL
      shinyalert::shinyalert("Configuration restored",
                             sprintf("The configuration and data of '%s' were loaded ('Run from this configuration file' is on). Its fitted objects are not in the cache any more, so press 'Fit model & forecast' to reproduce the results.", loaded$id),
                             type = "info")
      return(invisible(FALSE))
    }

    options_file <- file.path(tempdir(), paste0("options_", loaded$id, ".R"))
    writeLines(loaded$options, options_file)
    data_now <- as.data.frame(loaded$data)
    ok <- withProgress(message = "Restoring the results", value = 0.3, {
      incProgress(0.2, detail = "posterior draws")
      extracted <- tryCatch(run_MCMC2(outputFile = options_file, data.temp = data_now, finalModlLIST = loaded$fits),
                            error = function(e) e)
      if (inherits(extracted, "error")) {
        shinyalert::shinyalert("The results could not be restored", conditionMessage(extracted), type = "error"); return(FALSE)
      }
      incProgress(0.3, detail = "figures, tables and diagnostics")
      out <- tryCatch(run_analyzeResults(optionsFile = options_file, MCMCOutput = extracted, data.temp = data_now,
                                         stan_warnings = loaded$warnings %||% list()),
                      error = function(e) e)
      if (inherits(out, "error")) {
        shinyalert::shinyalert("The results could not be restored", conditionMessage(out), type = "error"); return(FALSE)
      }
      finalMCMC$results <- extracted
      allOutput$everything <- out
      TRUE
    })
    if (!isTRUE(ok)) return(invisible(FALSE))

    samplingReactiveValue$values <- loaded$fits
    run_meta$fits <- loaded$fits
    run_meta$options <- loaded$options
    run_meta$stan_code <- loaded$stan_code
    run_meta$warnings <- loaded$warnings %||% list()
    run_meta$started <- loaded$started
    run_meta$finished <- loaded$finished %||% Sys.time()
    samplingIndicator(1)
    shinyalert::shinyalert("Run opened",
                           sprintf("The results of '%s' are shown below; its configuration and data were loaded ('Run from this configuration file' is on, so 'Fit model & forecast' reproduces the run).", loaded$id),
                           type = "success")
    invisible(TRUE)
  }

  }

shinyApp(ui = ui, server = server)
