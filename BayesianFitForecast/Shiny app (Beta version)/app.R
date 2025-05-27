
  ###################
  # Needed packages #
  ###################
  
  # List of required packages
  required_packages <- c(
    "shiny", "shinydashboard", "shinyWidgets", "ggplot2", "plotly", "DT", "mathjaxr",
    "readxl", "shinyjqui", "devtools", "bayesplot", "xlsx", "openxlsx", "rstan", 
    "shinyjs", "BayesianFitForecast", "stringr", "shinybusy", "eList",
    "shinyalert", "glue", "remotes"
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
# Loading ShinyMath ------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section loads the shinyMath function locally from the downloaded #
# function. It then calls the library normally.                                #
#------------------------------------------------------------------------------#
  
  # Installing latex2r
  install.packages("latex2r-master", repos = NULL, type = "source")
  
  # Library latex2r
  library(latex2r)
  
  # Installing shinyMath
  install.packages("shinymath-master", repos = NULL, type = "source")
  
  # Loading shinyMath
  library(shinymath)
  
  ############################
  # Load necessary functions #
  ############################
  source("error.checking.R")
  source("run_MCMC.R")
  source("run_analyzeResults.R")
  source("check.IC.R")
  source("diff.R")
  source("ode_rhs.R")
  source("stancreator.R")
  source("run_MCMC2.R")


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
    menuItem("Data Options",
             
             # Menu ID
             tabName = "dataOptions",
             
             # Icon for tab
             icon = icon("chart-line"),
             
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
    menuItem("Forecasting Options",

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
             uiOutput("forecast.horizon"),
             
             # CSS to hide the blinking cursor
             tags$style(HTML("
                #forecastinghorizon {
                  caret-color: transparent;
                }
              ")), 
             
             # JS to prevent typing and pasting
             tags$script(HTML("
              $(document).on('shiny:connected', function() {
                $('#forecastinghorizon').on('keydown paste', function(e) {
                  e.preventDefault();
                });
              });
            "))
             
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
    menuItem("Base Model Options",
             
             # Menu ID
             tabName = "baseModelOptions",
             
             # Icon for tab
             icon = icon("chart-line"),
             
             ###################
             # Error structure #
             ###################
             pickerInput("errstrc", "Error Structure: ", choices = c("Negative binomial", "Normal", "Poisson")), 
             
             ####################
             # Variable options #
             ####################
             numericInput("vars", label = "# State Variables", value = 1, min = 1),
             
             # CSS to hide the blinking cursor
                         tags$style(HTML("
                #vars {
                  caret-color: transparent;
                }
              ")),
             
             # JS to prevent typing and pasting
             tags$script(HTML("
              $(document).on('shiny:connected', function() {
                $('#vars').on('keydown paste', function(e) {
                  e.preventDefault();
                });
              });
            ")), 
             
             #####################
             # Parameter options #
             #####################
             numericInput("num_params", label = "# Parameters", value = 0, min = 0),
             
             # CSS to hide the blinking cursor
             tags$style(HTML("
                #num_params {
                  caret-color: transparent;
                }
              ")),
             
             # JS to prevent typing and pasting
             tags$script(HTML("
              $(document).on('shiny:connected', function() {
                $('#num_params').on('keydown paste', function(e) {
                  e.preventDefault();
                });
              });
            ")), 
             
             ######################################
             # Entering the composite variable(s) #
             ######################################
             numericInput("numComposite", "# Composite Variables", value = 0, min = 0), 
             
             # CSS to hide the blinking cursor
             tags$style(HTML("
                #numComposite {
                  caret-color: transparent;
                }
              ")),
             
             # JS to prevent typing and pasting
             tags$script(HTML("
              $(document).on('shiny:connected', function() {
                $('#numComposite').on('keydown paste', function(e) {
                  e.preventDefault();
                });
              });
            ")), 
             
             ###################################################
             # Number of Markov Chain Monte Carol (MCMC) steps #
             ###################################################
             numericInput("niter", "Number of MCMC Steps: ", 1000),
             
             ####################################################
             # Number of Markov Chain Monte Carlo (MCMC) chains #
             ####################################################
             numericInput("num_chain", "Number of MCMC Chains: ", 2)
           
                         
             ), # End of 'menuItem' for base model specifications


#------------------------------------------------------------------------------#
# Adding the Run Button --------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the "Run" button for users. In order to get any  #
# output, the user must select the run button.                                 #
#------------------------------------------------------------------------------#

  ###########################
  # Creating the Run Button #
  ###########################
  shiny::actionButton("run",
                      label = "Run Forecasts",
                      icon = icon("play", class="fa-regular fa-play"),
                      width = 200)
  
    ) # End of `sidebarMenu`
  
  ), # End of dashboard side-bar
 
  ######################################
  # Creating the body of the dashboard #
  ######################################
  dashboardBody(
    
    # Fixing things extending pst the end
    tags$head(
      tags$style(HTML("
      .content-wrapper, .right-side {
        min-height: 100vh !important;
        overflow-y: auto;
      }
    "))
    ),
    
    ###########################
    # Adding a loading circle #
    ###########################
    add_busy_spinner(spin = "fading-circle"),
    
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
# Row 1A: Specifying the state variables, and their information ----------------
#------------------------------------------------------------------------------#
# About: This section creates the tab which creates the state variable, sets   #
# their fixed status, and determines if the derivative of the model's fitting  #
# variable should be fitted to the data.                                       #
#------------------------------------------------------------------------------#

  tabPanel(
    
    # Title 
    title = "Variable Specification",
    
    value = "state_variables", 
    
    # Width 
    width = 12,
    
    #####################################
    # Creating the inputs for variables #
    #####################################
    div(id = "inputContainer2")
    
  ), 

#------------------------------------------------------------------------------#
# Row 1B: Specifying the parameters, and their information ---------------------
#------------------------------------------------------------------------------#
# About: This section creates the tab which allows the users to specify info   #
# about the selected parameters, including their fixed status, bounds, and     #
# priors.                                                                      #
#------------------------------------------------------------------------------#

    tabPanel(
      
      # Title 
      title = "Parameter Specification",
      
      value = "paramsVariables", 
      
      # Width 
      width = 12,
      
      ######################################
      # Creating the inputs for parameters #
      ######################################
      div(id = "inputContainer"), 
      
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
                
              h4("Required Parameters", 
                  style = "font-weight: bold; text-align: center; margin-bottom: 10px;"),
                
              # Creating the parameter options 
              div(id = "reqParamsF")
                
             )
              
          ),
            
        ) # End of column 
            
      ) # End of `fluidRow`
      
    ), # End of 'tabPanel'

#------------------------------------------------------------------------------#
# Row 1C: Specifying the equations ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the tab which allows the users to specify info   #
# about the system of ODEs. It is then translated to the correct form for      #
# use of RStan.                                                                #
#------------------------------------------------------------------------------#

  tabPanel(
    
    # Title 
    title = "System of Equations",
    
    value = "ODESys", 
    
    # Width 
    width = 12,
    
    #########################################
    # Creating the inputs for ODE equations #
    #########################################
    div(id = "equationContainer"),
    
    # Loading the upload equation button
    actionButton("loadEquation", label = "Upload Equations")
    
  ), # End of 'tabPanel'

#------------------------------------------------------------------------------#
# Row 1D: Creating the Composite Variables Tab ---------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the tab that allows users to create the          #
# equations for the composite variables. It is then translated to the          #
# correct form for the `options` file.                                         #
#------------------------------------------------------------------------------#

  tabPanel(
    
    # Title 
    title = "Composite Variables",
    
    ###############################################
    # Creating the inputs for composite equations #
    ###############################################
    div(id = "compositeEquationContainer"),
    
    # Loading the upload equation button
    actionButton("loadComposite", label = "Upload Equations")
    
  )

  
  
  ), # End of 'tabBox'

#------------------------------------------------------------------------------#
# Row 2: Available Figures -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates a box for users to view any of the figures       #
# available as part of the toolbox in the dashboard. Additionally, it provides #
# options for downloading the figures and customizing them.                    #
#------------------------------------------------------------------------------#

  ########################################
  # Creating the box to hold the figures #
  ########################################
  box(
    
    # Box Width 
    width = 12, 
    
    title = "Available Figures", 
    
    ##################
    # Row for Figure #
    ##################
    fluidRow(
      
      # Width of row
      width = 12,
      
      # Creating column to keep figure in line
      column(
        
        # Width of column
        width = 12, 
        
        # Plotting the the selected figure 
        plotOutput("allOutputFIGS")
        
      ) # End of column 
      
    ), # First row that always shows up
    
    fluidRow(
      
      ####################
      # Alignment column #
      ####################
      column(
        
        # Width of column 
        width = 10,
        
        ########################
        # Overall style of row #
        ########################
        div(style = "display:flex; vertical-aline: top",
            
            # Rendering the download button 
            div(uiOutput("selectFigureALL"), style = "margin-right: 10px;"),
            
            div(style = "margin-right: 10px;", uiOutput("select.calibration.period")),
            
            div(style = "margin-top: 25px; vertical-aline: bottom", uiOutput("download.figures")),
            
        ) # End of main style
        
      ), # End of column for left-aligned buttons
      
      ##########################################
      # Creating the previous and next buttons #
      ##########################################
      column(
        
        # Width 
        width = 2,
        
        # Creating the buttons 
        div(style = "display: flex; justify-content: flex-end; align-items: center; margin-top: 25px;",
            uiOutput("arrows.figures.previous"),
            uiOutput("arrows.figures.next")))
      
    ) # End of 'fluidRow' creating the buttons 
    
    
  ), # End of Figures Box 
  
#------------------------------------------------------------------------------#
# Row 3: Available Data --------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates a box for users to view any of the data sets     #
# available as part of the toolbox in the dashboard. Additionally, it provides #
# options for downloading the data and navigating between them.                #
#------------------------------------------------------------------------------#

  ########################################
  # Creating the box to hold the figures #
  ########################################
  box(
    
    # Width of box
    width = 12, 
    
    # Title
    title = "Available Data Sets", 
    
    ############################
    # Rendering the data table #
    ############################
    
    # Column to hold data 
    column(
      
      # Width 
      width = 12,
      
      # Rendering the title
      textOutput("forecastDataTitle"),
      
      # Rendering the data
      dataTableOutput("dataTableRender")
      
    ),
    
    #########################################
    # Creating the style for bottom buttons #
    #########################################
    
    fluidRow(
      
      ####################
      # Alignment column #
      ####################
      column(
        
        # Width of column 
        width = 10,
        
        ########################
        # Overall style of row #
        ########################
        div(style = "display:flex; vertical-aline: top",
            
            # Rendering the download button 
            div(uiOutput("selectDatall", style = "margin-right: 10px;")),
            
            div(style = "margin-top: 25px;margin-right: 10px;vertical-aline: bottom", uiOutput("download.data")),

        ) # End of main style
        
      ), # End of column for left-aligned buttons
      
      ##########################################
      # Creating the previous and next buttons #
      ##########################################
      column(
        
        # Width 
        width = 2,
        
        # Creating the buttons 
        div(style = "display: flex; justify-content: flex-end; align-items: center; margin-top: 25px;",
            uiOutput("arrows.data.previous"),
            uiOutput("arrows.data.next")))
      
      ) # End of 'fluidRow' creating the buttons 
  
    ) # End of box 

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
  # Observing changes in composite count variable #
  #################################################
  observeEvent(input$numComposite, {

    ######################################################
    # Hiding tab if no composite variables are requested #
    ######################################################
    if(input$numComposite == 0){

      # Hiding the composite variables 
      hideTab(inputId = "EQUATIONS", target = "Composite Variables")

    ####################################################
    # Showing tab if composite variables are requested #
    ####################################################
    }else{
  
      # Showing the composite variables 
      showTab(inputId = "EQUATIONS", target = "Composite Variables")
      
    }

  }) # End of `observeEvent`
  
#------------------------------------------------------------------------------#
# Rendering the file input -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the 'fileInput' to the main UI. If an error      #
# is triggered at any point in the dashboard, including in the structure of    #
# the inputted data, this UI option will re-render.                            #    
#------------------------------------------------------------------------------#
  
  output$input.data <- renderUI({
    
    fileInput("cadfilename1", 
              tags$span("Load Data (.xlsx)", # Shown label
                        # Creating the info circle 
                        tags$i(class = "glyphicon glyphicon-info-sign",
                               style = "color:#FFFFFF;",
                               title = "Upload a '.xlsx' file of your data.")),
              multiple = TRUE)
    
  })
  
  
#------------------------------------------------------------------------------#
# Reading in the data frame ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: The following section reads in the user-selected data frame from the  #
# working directory. It then saves the data under the reactive element 'file'. #   
# The file is selected using the 'fileInput' picker.                           #
#------------------------------------------------------------------------------#
  
  ###############################################
  # Creating the empty reactive value for files #
  ###############################################
  listTimeseries <- reactiveValues()

  ############################################
  # Observing changes in the reactive values #
  ############################################
  observeEvent(input$cadfilename1,{
    
    ################################
    # Running if data is available #
    ################################
    tryCatch({
      
      # Empty list to store files
      filesOtherLst <- list()
      
      ###################################
      # Isolating the change in 'files' #
      ###################################
      isolate({
        
        # List of files read in
        files <- input$cadfilename1
        
      })
      
      ##############################
      # Looping through file names #
      ##############################
      for(i in 1:nrow(files)){
        
        # Indexed fie
        indexedFile <- files$datapath[i]
        
        # Indexed file name
        indexedFileName <- files$name[i]
        
        # Reading in the data
        data <- read_excel(indexedFile)
        
        # Saving the files to a list
        filesOtherLst[[i]] <- data
        
        # Adding the name of the file
        names(filesOtherLst)[i] <- indexedFileName
        
      } # End of file loop
      
      ##########################################################
      # Saving the list of files under the reactive value name #
      ##########################################################
      listTimeseries$data <- filesOtherLst[[1]]
      
    ####################################
    # Running if data is not available #
    ####################################
    }, error = function(e){
      
      print("The timeseries data did not load properly.")
      
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

      # Error to return
      shinyalert::shinyalert(title = "Error!", text = "Please check the first column of your data set. It should contain sequential time points. Either the current column does not start at zero, or there are missing or repeated values.", type = "error")

      # Clearing out the original data
      listTimeseries$data <- NULL
      
      # Keeping the final data reactive value clear
      finalData$data <- NULL

      # Re-rendering the file-input if an error occurs 
      output$input.data <- renderUI({
        
        fileInput("cadfilename1", 
                  tags$span("Load Data (.xlsx)", # Shown label
                            # Creating the info circle 
                            tags$i(class = "glyphicon glyphicon-info-sign",
                                   style = "color:#FFFFFF;",
                                   title = "Upload a '.xlsx' file of your data.")),
                  multiple = TRUE)
        
      })
      
    ########################################################
    # Error 2: The Non-Date Column headers are not correct #
    ########################################################
    }else if(errorResult == "ERROR2"){
      
      # Error to return
      shinyalert::shinyalert(title = "Error!", text = "Please check your column headers. It is important to start the second column with 'cases1', and if additional columns exist, they should follow the pattern 'cases2', 'cases3', etc.", type = "error")
      
      # Clearing out the original data
      listTimeseries$data <- NULL
      
      # Keeping the final data reactive value clear
      finalData$data <- NULL
      
      # Re-rendering the file-input if an error occurs 
      output$input.data <- renderUI({
        
        fileInput("cadfilename1", 
                  tags$span("Load Data (.xlsx)", # Shown label
                            # Creating the info circle 
                            tags$i(class = "glyphicon glyphicon-info-sign",
                                   style = "color:#FFFFFF;",
                                   title = "Upload a '.xlsx' file of your data.")),
                  multiple = TRUE)
        
      })
      
    #################################################
    # Doing nothing if the data is loaded correctly #
    #################################################
    }else{

      finalData$data <- listTimeseries$data

     }
    
    
  })
  
#------------------------------------------------------------------------------#
# Creating the process of interest `pickerInput` -------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the `pickerInput` for the user-typed process of  #
# interest.                                                                    #
#------------------------------------------------------------------------------#
  
  output$process.of.interest <- renderUI({
    
    # Requiring the input file
    req(listTimeseries$data)
    
    # Requiring the final data 
    req(finalData$data)
    
    ######################
    # Creating the input #
    ######################
    textInput("caddisease", 
              tags$span("Process of Interest: ", # Shown label
                        # Creating the info circle 
                        tags$i(class = "glyphicon glyphicon-info-sign",
                               style = "color:#FFFFFF;",
                               title = "This is where you can indicate what is contained within your time series. For example, 'COVID-19', if data contains counts of a COVID-19 related process.")))
  })
  
#------------------------------------------------------------------------------#
# Creating the file name options -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the `series_cases` and `datetype` options for    #
# the option file. It looks at the number of columns in the data set, and then #
# creates the corresponding number of rows to rename the columns.              #
#------------------------------------------------------------------------------#
  
  ##########################
  # Creating the UI option #
  ##########################
  output$seriescasesdatetype <- renderUI({
    
    # Requiring the input file
    req(listTimeseries$data)
    
    # Requiring the final data 
    req(finalData$data)
    
    # Requiring the process of interest input
    req(input$caddisease)
    
    ##############################################
    # Determining the number of inputs to create #
    ##############################################
    
    # Determining the number of columns in the data
    num_inputs <- ncol(finalData$data)
    
    ############################
    # Creating the text inputs #
    ############################
    inputs <- lapply(1:num_inputs, function(i) {
      
      # Creating the text inputs 
      textInput(
        
        inputId = paste0("text_input_", i), 
        label = if (i == 1) "Temporal Resolution" else paste("Time Series Type ", i-1), 
        value = if (i == 1) "Ex: Days" else "Ex: Infected"
        
      )
      
    })
    
    #############################
    # Returning the list option #
    #############################
    tagList(inputs)
    
  })
      
#------------------------------------------------------------------------------#
# Creating the `series_cases` and `datetype` reactive values -------------------
#------------------------------------------------------------------------------#
# About: This section creates the reactive values that stores the              #
# `series_cases` option and the 'datetype' options within reactive values.     #
#------------------------------------------------------------------------------#
  
  ########################################################
  # Reactive value to store the series cases information #
  ########################################################
  series_cases_temp <- reactiveValues()
  
  #####################################################
  # Reactive value to store the date type information #
  #####################################################
  date_type_temp <- reactiveValues()
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    # Requiring the input file
    req(listTimeseries$data)
    
    # Requiring the final data 
    req(finalData$data)
    
    # Requiring the process of interest input
    req(input$caddisease)
    
    # Vector to store case series labels
    series.list.temp <- NULL
    
    ################################
    # Creating the loop for inputs #
    ################################
    for(i in 1:ncol(finalData$data)){
      
      # Requiring the indexed input
      req(input[[paste0("text_input_", i)]])
      
      ##############################################################
      # Determining which type of input is indexed: Temporal Label #
      ##############################################################
      if(i == 1){
        
        # Saving the temporal resolution 
        date_type_temp$temporal <- input[[paste0("text_input_", i)]]
        
      ###########################################################
      # Determining which type of input is indexed: Case Series #
      ###########################################################
      }else{
        
        # Saving the series 
        series.list.temp <- c(series.list.temp, input[[paste0("text_input_", i)]])
        
      }
      
      ############################################
      # Saving the case series in reactive value #
      ############################################
      series_cases_temp$labels <- series.list.temp
      
    } # End of loop 
    
  })
    
  
#------------------------------------------------------------------------------#
# Creating the calibration period `pickerInput` --------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the `pickerInput` for the available calibration  #
# period lengths. The available lengths are dependent on the number of rows    #
# of data available.                                                           #
#------------------------------------------------------------------------------#
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    # Requiring the input file
    req(listTimeseries$data)
    
    # Requiring the final data 
    req(finalData$data)
    
    # Requiring the process of interest
    req(input$caddisease)
    
    # Reading in the input data into a 'temp'  
    inputFile <- finalData$data
    
    #########################################
    # Creating the calibration period input #
    #########################################
    output$calibrationPeriodLength <- renderUI({
      
      # Requiring the input file
      req(listTimeseries$data)
      
      # Requiring the final data 
      req(finalData$data)
      
      # Requiring the process of interest
      req(input$caddisease)
      
      # Creating the input
      pickerInput("calibrationperiods",
                  "Calibration Period Length: ",
                  choices = c(1:nrow(inputFile)),
                  selected = nrow(inputFile),
                  multiple = T,
                  options = list(
                    `actions-box` = TRUE
                  ))
      
    })

  })
  
  
#------------------------------------------------------------------------------#
# Creating the forecasting horizon `pickerInput` -------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the UI for entering the length of the forecast   #
# horizon. It ranges from zero to infinity.                                    #
#------------------------------------------------------------------------------#
  
  output$forecast.horizon <- renderUI({
    
    # Requiring the input file
    req(listTimeseries$data)
    
    # Requiring the final data 
    req(finalData$data)
    
    # Requiring the process of interest
    req(input$caddisease)
    
    ###################################################
    # Creating the numeric input for forecast horizon #
    ###################################################
    numericInput("forecastinghorizon", "Forecasting Horizon: ", value = 0, min = 0)
    
  })
  
#------------------------------------------------------------------------------#
# Adding the variable inputs --------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the inputs for the variable select by the user.  #
# For each variable selected, the user can specify a variety of information.   #
#------------------------------------------------------------------------------#
  
  ###############
  # Row counter #
  ###############
  rowCounter2 <- reactiveVal(0)
  
  ##################################################################
  # Reactive function to store selected values for state variables #
  ##################################################################
  selectedValues <- reactive({
    
    # Empty vector to store state variables 
    selected <- c()
    
    # Looping through rows in state variable box 
    for (i in 1:input$vars) {
      
      selected <- c(selected, input[[paste0("varsID_", i)]])
      
    }
    
    # Returning the list of selected 
    selected
    
  })
  
  ###########################################
  # Observing changes in the number of rows #
  ###########################################
  observeEvent(input$vars, {
    
    # The current number of rows 
    current_rows <- rowCounter2()
    
    # The number of rows there should be 
    new_rows <- input$vars
    
    #######################################
    # Add new rows if the count increases #
    #######################################
    if (new_rows > current_rows) {
      
      # Adding the number of rows based upon the number selected by the user
      for (i in (current_rows + 1):new_rows) {
        
        # Get available choices (removing selections from previous rows)
        available_choices <- setdiff(LETTERS, selectedValues())
        
        ############################
        # Creating the UI for rows #
        ############################
        insertUI(
          
          # Label for UI
          selector = "#inputContainer2",
          
          # Where to add the row 
          where = "beforeEnd",
          
          # Creating the UI 
          ui = fluidRow(
            
            # Label for each row 
            id = paste0("varsLine_", i),
            
            # Style for rows 
            style = "margin: 10px 0; padding: 10px; border: 1px solid #ddd; background-color: #f4f4f4; min-height: 50px;",
 
            # UI components for each column in the row
            column(2, selectInput(paste0("varsID_", i), label = "State Variable",choices = available_choices, selected = ifelse(length(available_choices) > 0, available_choices[1], NULL))),
            column(2, mathInput(paste0("Ic_", i), label = "Initial Condition")),
            column(2, selectInput(paste0("fitToData_", i), label = "Fit to Data", choices = c("Yes", "No"), selected = "No")),
            column(2, uiOutput(paste0("fittingIndex_", i))),
            column(2, uiOutput(paste0("fittingDiff_", i)))
    
          )
          
        ) # End of insertUI
        
      } # End of loop to create rows 
      
    } # End of indicator that new rows should be added 
    
    ############################################
    # Remove extra rows if the count decreases #
    ############################################
    if (new_rows < current_rows) {
      
      # Removing rows to reach the current row count specified 
      for (i in (new_rows + 1):current_rows) {
        
        # Removing the row 
        removeUI(selector = paste0("#varsLine_", i))
        
      } # End of loop to remove rows 
      
    } # End of indicator that existing rows should be removed 
    
    # Specifying the new row count 
    rowCounter2(new_rows)
    
  }) # End of 'observe event'
  
  ###################################################
  # Observe selections and update available choices #
  ###################################################
  observe({
    
    # All available letters 
    all_letters <- LETTERS
    
    # Letters selected by user 
    selected <- selectedValues()
    
    # Looping through state variables 
    for (i in 1:input$vars) {
      
      # Determining what letters can be selected 
      available_choices <- setdiff(all_letters, selected[selected != input[[paste0("varsID_", i)]]])  
      
      # Updating the select options based on previously selected options
      updateSelectInput(session, paste0("varsID_", i), choices = available_choices, selected = input[[paste0("varsID_", i)]])
      
    }
    
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
   
   ####################################
   # Tracking changes in the IC input #
   ####################################
   observeEvent(input$EQUATIONS,{
     
     lapply(1:input$vars, function(i) {
       
       updateMathInput(session = session, inputId = paste0("Ic_", i), value = NULL, label = NULL)
       
     })
    
   })
   
   #############################################
   # Creating the vector of initial conditions #
   #############################################
   observeEvent(input$EQUATIONS, {
     
     # Function going through each row
     lapply(1:input$vars, function(i) {
       
        initial.conditions.vec$IC[[i]] <- input[[paste0("Ic_", i)]]
       
     })
     
   })
    
#------------------------------------------------------------------------------#
# Determining how many "yes" options for fitting to the data -------------------
#------------------------------------------------------------------------------#
# About: This section customizes the number of "yes" responses allowed by the  #
# `Fit to Data` picker. Below determines how many "yes" responses have been    #
# selected by the user, and saves the value within a reactive value. The       #
# reactive value is utilized in determining how many "yes" and "No" options    #
# should be available to users.                                                #
#------------------------------------------------------------------------------#
  
  ##########################################################
  # Determining the maximum number of "Yes" values allowed #
  ##########################################################
  
  # Reactive value to store the max "yes" value
  max_yes <- reactiveVal()
  
  #############################
  # Calculating the max "yes" #
  #############################
  observe({
    
    # Requiring the final data 
    req(finalData$data)
    
    # Determining how many columns have data
    maxYes <- ncol(finalData$data) - 1
    
    # Updating the reactive value
    max_yes(maxYes)
    
  })

  #######################################
  # Reactive value to store 'yes' count #
  #######################################
  yesCount <- reactiveVal()

  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    # Requiring the final data 
    req(finalData$data)

    # Starting with zero 'yes'
    yes_count <- 0

    ########################
    # Looping through rows #
    ########################
    for(i in 1:input$vars) {

      # Requiring the indexed row
      req(input[[paste0("fitToData_", i)]])

      ####################################################
      # Determining if the 'yes' counter should increase #
      ####################################################
      if(input[[paste0("fitToData_", i)]] == "Yes") {

        yes_count <- yes_count + 1

      }

    } # End of `row` loop

    yesCount(yes_count)

  })

#------------------------------------------------------------------------------#
# Determining which options should show: Fit to Data ---------------------------
#------------------------------------------------------------------------------#
# About: This section determines what options should show for the "Fit to      #
# Data" option. Given that the number of state variables that can be fitted    #
# to data is dependent on the number of time series, the number of "Yes"       #
# options correspond to the number of time series available. If the number of  #
# options selected "Yes" reaches the number of time series, the remaining      #
# selectors will only show "No".                                               #
#------------------------------------------------------------------------------#
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    # Requiring the final data 
    req(finalData$data)
    
    ###################################
    # Looping through state variables #
    ###################################
    for(i in 1:input$vars) {
      
      # Ensure the input exists for the given row
      req(input[[paste0("fitToData_", i)]])
      
      ###############################################
      # Fetch the current count of "Yes" selections #
      ###############################################
      
      # Current yes count 
      current_yes_count <- yesCount()
      
      # Max count allowed 
      max_yes_limit <- max_yes()
      
      ########################################
      # Check the current value of the input #
      ########################################
      current_value <- input[[paste0("fitToData_", i)]]
      
      ###############################################################################
      # Option 1: The "Yes" limit has been reached and the option "Yes" is selected #
      ###############################################################################
      if(current_yes_count >= max_yes_limit && current_value == "Yes") {
        
        # If the max number of "Yes" options is reached, keep "Yes" and "No"
        updateSelectInput(session, inputId = paste0("fitToData_", i), choices = c("Yes", "No"), selected = NULL)
        
      ##############################################################################
      # Option 2: The "Yes" limit has been reached and the option "No" is selected #
      ##############################################################################
      }else if(current_yes_count >= max_yes_limit && current_value == "No"){
        
        # If the max number of "Yes" options is reached, keep "No"
        updateSelectInput(session, inputId = paste0("fitToData_", i), choices = c("No"))
        
      ########################################################################
      # Option 3: The "Yes" limit has not been reached, and "No" is selected #
      ########################################################################
      }else if(current_yes_count < max_yes_limit && current_value == "No"){
        
        # Keep default choices, and select no 
        updateSelectInput(session, inputId = paste0("fitToData_", i), choices = c("Yes", "No"), selected = "No")
        
      #########################################################################
      # Option 4: The "Yes" limit has not been reached, and "Yes" is selected #
      #########################################################################
      }else if(current_yes_count < max_yes_limit && current_value == "Yes"){
        
        # Keep default choices, and select yes
        updateSelectInput(session, inputId = paste0("fitToData_", i), choices = c("Yes", "No"), selected = "Yes")
        
      }
      
    }
    
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
  # Reactive value to store yes/no vector #
  #########################################
  yes.no.state <- reactiveValues()
  
  ##################################
  # Reactive value to store labels #
  ##################################
  labels.fit.state <- reactiveValues()
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    # Requiring the final data 
    req(series_cases_temp$labels)
    
    ##########################################
    # Function to go through each row option #
    ##########################################
    lapply(1:input$vars, function(i) {
      
      ###############################################
      # Tracking changes in the "Yes"/"No" selector #
      ###############################################
      observeEvent(input[[paste0("fitToData_", i)]], {
        
        ###################################################################
        # Determining if the state option should show: Showing the option #
        ###################################################################
        if (input[[paste0("fitToData_", i)]] == "Yes") {
          
          # Render additional input when "Yes" is selected
          output[[paste0("fittingIndex_", i)]] <- renderUI({
            
            selectInput(paste0("additional_input_", i), label = "Time Series to Fit", choices = c(series_cases_temp$labels))
            
          })
          
          # Adding the 1 to the vector
          yes.no.state$state[i] <- 1
          
        #######################################################################
        # Determining if the state option should show: Not Showing the option #
        #######################################################################
        }else{
          
          # Remove additional input when "No" is selected
          output[[paste0("fittingIndex_", i)]] <- renderUI(NULL)
          
          # Adding the 1 to the vector
          yes.no.state$state[i] <- 0
        
        }
        
      }) # End of 'observeEvent'
      
      ########################################
      # Tracking changes in additional input #
      ########################################
      observeEvent(input[[paste0("additional_input_", i)]], {
        
        # Requiring a column to be selected
        req(input[[paste0("additional_input_", i)]])
        
        # Store additional input in reactiveValues
        labels.fit.state$state[i] <- input[[paste0("additional_input_", i)]]
        
      }, ignoreNULL = TRUE, ignoreInit = TRUE) # Ensures only valid changes trigger update
      
    }) # End of 'lapply'
    
  }) # End of observe 
  

#------------------------------------------------------------------------------#
# Creating the input for the "fitting_index" -----------------------------------
#------------------------------------------------------------------------------#
# About: This section properly formats the fitting index for the options file  #
# that is used to get output.                                                  #
#------------------------------------------------------------------------------#
  
  ###########################################
  # Reactive value to store `fitting_index` #
  ###########################################
  fitting_index_values <- reactiveValues()

  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    # Column labels 
    column.labels <- series_cases_temp$labels
    
    # Vector of "yes/no"
    vector.fit <- yes.no.state$state
    
    # Vector columns to fit
    vector.data.fit <- labels.fit.state$state
    
    ########################
    # Cleaning up the data #
    ########################
    
    # Getting the "yes" indexes 
    row_indices <- which(vector.fit == 1)
    
    # Removing the NA responses 
    col_fit <- na.omit(vector.data.fit)
    
    ################################
    # Creating the array to export #
    ################################
    
    # Match each associated column with its position in column.labels
    column_positions <- match(col_fit, column.labels)
    
    # Create the output vector
    output <- row_indices[order(column_positions)]

    ###########################################
    # Saving the output in the reactive value #
    ###########################################
    fitting_index_values$fit <- output
    
    })
  
#------------------------------------------------------------------------------#
# Error Checking: At least one fitting index is selected -----------------------
#------------------------------------------------------------------------------#
# About: This section creates the error that will appear if the user finishes  #
# selecting parameters related to the state conditions and shifts to another   #
# page.                                                                        #
#------------------------------------------------------------------------------#
  
  observeEvent(input$EQUATIONS, ignoreInit = TRUE, {
    
    # Requiring the final data 
    req(series_cases_temp$labels)
    
    ###################################
    # Triggering only on certain tabs #
    ###################################
    if(input$EQUATIONS %in% c("paramsVariables", "ODESys")){
      
      ##############################
      # Creating the error message #
      ##############################
      if(length(fitting_index_values$fit) == 0 || is.null(fitting_index_values$fit)){
        
        shinyalert::shinyalert(title = "Warning!", text = "The input data must be assigned to at least one state variable. Please return to the state variable page and assign the data.", type = "warning")
        
      }
      
    }

  })
  
#------------------------------------------------------------------------------#
# Creating the fitting difference variable -------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the fitting difference variable options based on #
# if the user selects "yes" to fitting the data. Then, users have the option   #
# to associate the data with the derivative of the selected state variable.    #
#------------------------------------------------------------------------------#
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    # Requiring the final data 
    req(labels.fit.state$state)
    
    ##########################################
    # Function to go through each row option #
    ##########################################
    lapply(1:input$vars, function(i) {
      
      ###############################################
      # Tracking changes in the "Yes"/"No" selector #
      ###############################################
      observeEvent(input[[paste0("fitToData_", i)]], {
        
        ########################################################################
        # Determining if the derivative option should show: Showing the option #
        ########################################################################
        if (input[[paste0("fitToData_", i)]] == "Yes") {
          
          # Render additional input when "Yes" is selected
          output[[paste0("fittingDiff_", i)]] <- renderUI({
            
            selectInput(paste0("fittingToDiff_", i), label = "Fit to derivative", choices = c("Yes", "No"))
            
          })
          
        ############################################################################
        # Determining if the derivative option should show: Not showing the option #
        ############################################################################
        }else{
          
          # Remove additional input when "No" is selected
          output[[paste0("fittingDiff_", i)]] <- renderUI(NULL)

        }
        
      }) # End of 'observeEvent'
      
    }) # End of lapply 
    
  }) # End of observe 
  
#------------------------------------------------------------------------------#
# Creating the vector of `fitting_diff` values ---------------------------------
#------------------------------------------------------------------------------#
# About: This section creates a vector of 0/1 for the `fitting_diff` option in #
# the options file.                                                            #
#------------------------------------------------------------------------------#
  
  ########################################
  # Reactive value to store 0/1 for diff #
  ########################################
  vector.fit.diff.state <- reactiveValues()
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    # Requiring the final data 
    req(labels.fit.state$state)
    
    ##########################################
    # Function to go through each row option #
    ##########################################
    lapply(1:input$vars, function(i) {
      
      #####################################################
      # Observing changes in the fitting_diff user option #
      #####################################################
      observeEvent(input[[paste0("fittingToDiff_", i)]], {
        
        # Ensure the input exists
        if(!is.null(input[[paste0("fittingToDiff_", i)]])) {
          
          ####################################
          # Setting the vector to "Yes" or 1 #
          ####################################
          if(input[[paste0("fittingToDiff_", i)]] == "Yes") {
            
            # Assigning the "1" to the vector 
            vector.fit.diff.state$state[i] <- 1
            
          ###################################
          # Setting the vector to "No" or 0 #
          ###################################
          } else {
            
            # Assigning the "0" to the vector 
            vector.fit.diff.state$state[i] <- 0
            
          }
          
        }
        
      }) # End of 'observeEvent'
      
    }) # End of lapply 
    
  }) # End of observe 
  
  
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
  observe({

    # Temp list to store parameters
    tempList <- c()

    # Temp holder for parameters
    temp.param <- c()
    
    temp.IC <- initial.conditions.vec$IC
    
    ########################################
    # Loop through each element in IC list #
    ########################################
    for(i in seq_along(temp.IC)){

      # Get the current element
      IC_value <- temp.IC[[i]]

      if(nchar(IC_value) > 0){
      
      # Clean backslashes first
      cleaned.param <- gsub("\\\\", "", IC_value)

      #########################################
      # Case 1: If element is a numeric value #
      #########################################
      if(grepl("^[-0-9\\s\\+\\-\\*/\\.]+$", cleaned.param)){

        # Checking if can be evaluated
        result <- tryCatch({

          eval(parse(text = cleaned.param))

        }, error = function(e){cleaned.param})

      ###################################################
      # Case 2: If element is a character or expression #
      ###################################################
      }else if(is.character(IC_value) || is.expression(IC_value)){

        # Splitting the string
        split.string <- strsplit(IC_value, split = "[\\-\\+]", perl = TRUE)

        # Creating the loop for split string
        for(j in 1:length(split.string[[1]])){

          if(is.na(as.numeric(split.string[[1]][j]))){

            temp.param[[j]] <- split.string[[1]][j]

          }else{

            next

          }

        }
        
      } # End of `Case 2` if-else
      
      #################################################
      # Removing all NA and NULL values from the list #
      #################################################
      tempList <- na.omit(null.omit(unique(c(temp.param, tempList))))
      
      }
      
      } # End of loop through IC
    
    ###############################################
    # Adding the final list to the reactive value #
    ###############################################
    req.param.list$list <- tempList
    
    })
  
#------------------------------------------------------------------------------#
# Adding the parameter inputs --------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the inputs for the parameter select by the user. #
# For each parameter selected, the user can specify a variety of information.  #
#------------------------------------------------------------------------------#
  
  #############################
  # Initial row counter value #
  #############################``
  rowCounter1 <- reactiveVal(0)
  
  #######################################################
  # Creating the reactive value for required parameters #
  #######################################################
  req.params.react <- reactive({req.param.list$list})
  
  ###################################
  # Triggering the addition of rows #
  ###################################
  observeEvent(list(input$EQUATIONS, input$num_params, req.params.react()), {
    
    # Current rows 
    current_rows <- rowCounter1()
    
    # The number of required rows 
    new_rows_required <- length(req.params.react()) + as.numeric(input$num_params)
    
    ####################################################
    # Adding new row: Either Required or user selected #
    ####################################################
    if(current_rows < new_rows_required){
      
      #####################
      # Creating the rows #
      #####################
      lapply((current_rows+1):new_rows_required, function(i) {
        
        ##################################
        # Adding the required parameters #
        ##################################
        output[[paste0("param_", i)]] <- renderUI({
          
          # Adding the parameter rows
          if(i <= length(req.param.list$list)){
            
            # Specific symbol
            div(
              id = paste0("paramsID_", i),
              withMathJax(paste0("$$", req.param.list$list[[i]], "$$"))
            )
            
            # Adding a generic `mathInput`
          }else{mathInput(paste0("paramsID_", i), label = "Parameter")}
          
        }) # End of `renderUI`
        
        
        ###########################################
        # Creating the UI for the parameter input #
        ###########################################
        insertUI(
          
          # UI Selector ID
          selector = "#inputContainer",
          
          # How to add the rows
          where = "beforeEnd",
          
          # Creating the FLUID ROW for the parameter
          ui = fluidRow(
            
            # ID for UI row
            id = paste0("paramsLine_", i),
            
            # Row class for all
            class = "paramRow",
            
            # Style for box
            style = "margin: 10px 0; padding: 10px; border: 1px solid #ddd; background-color: #f4f4f4; min-height: 50px;",
            
            # Row options
            column(1, uiOutput(paste0("param_", i))),
            column(2, pickerInput(paste0("fixed_", i), label = "Estimation", choices = c("Fixed", "Estimate"))),
            column(2, uiOutput(paste0("main.Prior_", i))),
            column(7, uiOutput(paste0("priorOptions_", i)))
            
          )
          
        ) # End of `insertUI`
        
        
      }) # End of lapply 
      
    }
    
    #######################################################
    # Removing new rows: Either Required or user selected #
    #######################################################
    if(current_rows > new_rows_required){
      
      ##########################################
      # Remove previously inserted UI elements #
      ##########################################
      lapply((new_rows_required + 1):current_rows, function(i) {
        
        removeUI(selector = paste0("#paramsLine_", i), immediate = TRUE)
        
      })
      
    }
    
    #################################
    # Updating the row number count #
    #################################
    rowCounter1(new_rows_required)

  }) # End of 'observe event'
  
    
#------------------------------------------------------------------------------#
# Creating the required 'params' section ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the "required parameters" section. This is the   #
# section that contains the parameters needed for the different error          #
# structures.                                                                  #
#------------------------------------------------------------------------------#
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    # Requiring the data to be loaded 
    req(listTimeseries$data)
    
    # Only execute when error structure < 3
    if (errorStructureNumber() < 3) {
      
      ##########################################
      # Remove previously inserted UI elements #
      ##########################################
      lapply(1:(ncol(listTimeseries$data) - 1), function(i) {
        
        removeUI(selector = paste0("#paramsLineREQF_", i), immediate = TRUE)
        
      })
      
      #######################################
      # Insert new UI elements using lapply #
      #######################################
      lapply(1:(ncol(listTimeseries$data) - 1), function(i) {
        
        insertUI(
          
          # Label for UI
          selector = "#reqParamsF",
          
          # Where to add the row
          where = "beforeEnd",
          
          # Creating the UI
          ui = fluidRow(
            
            # Label for each row
            id = paste0("paramsLineREQF_", i),
            
            # Style for rows
            style = "margin: 10px 0; padding: 10px; border: 1px solid #ddd; background-color: #f4f4f4; min-height: 50px;",
            
            # UI components for each column in the row
            column(1, uiOutput(paste0("mathSym_", i))), 
            column(2, uiOutput(paste0("dist.UI.Prior_", i))),
            column(7, uiOutput(paste0("dist.priorOptions_", i)))
            
          ) # End of UI
          
        ) # End of `insertUI`
        
      }) # End of `lapply`
      
    #################################################################
    # Removing options if the "Poisson" error structure is selected #
    #################################################################
    }else {
      
      # Applies to all rows 
      lapply(1:(ncol(listTimeseries$data) - 1), function(i) {
        
        removeUI(selector = paste0("#paramsLineREQF_", i), immediate = TRUE)
        
      })
      
    } # End of 'if-else' for error structure 
    
  }) # End of 'observe'

#------------------------------------------------------------------------------#
# Determining what parameters options should show for the req. -----------------
#------------------------------------------------------------------------------#
# About: This section determines what parameter options should be shown in the #
# required parameters section. At a minimum, it will show the rows for the     #
# selected distribution. If 'Poisson' is selected, and additional parameter    #
# does not need to be updated and rows do not show.                            #
#------------------------------------------------------------------------------#
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    # Requiring an error structure to be selected 
    req(errorStructureNumber())
    
    # Requiring the data to be loaded 
    req(listTimeseries$data)
    
    #############################################################################
    # Creating the symbols for the "Normal" and "Neg. Binomial" error structure #
    #############################################################################
    lapply(1:(ncol(listTimeseries$data) - 1), function(i) {
      
      #####################
      # Rendering symbols #
      #####################
        
        # Rendering the UI 
        output[[paste0("mathSym_", i)]] <- renderUI({
          
          # Symbol for 'Normal' distribution
          symbol <- if (errorStructureNumber() == 2) {
            
            "&sigma;" 
            
          # Symbol for 'Neg. Binomial' distribution
          } else if (errorStructureNumber() != 3) {
            
            "&#x03D5;"  
            
          # Returning nothing 
          } else {
            
            return(NULL)
            
          }
        
          #####################################
          # Creating the style for the symbol #
          #####################################
          HTML(paste0(
            "<div style='display: flex; align-items: center; justify-content: center; height: 100%;
            padding-top: 20px;'>
            <span style='font-size: 23px; font-weight: bold;'>", symbol, "</span>
          </div>"
          ))
          
        }) # End of rendering the UI
        
      
    }) # End of `lapply`
    
  }) # End of 'observe'
  
#------------------------------------------------------------------------------#
# Creating the Distribution Prior Options --------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the distribution prior options. If the user      #
# selects either the negative binomial or normal distributions, an additional  #
# parameter needs to be estimated for each time series loaded by the user.     #
# This section creates the picker for the different prior options.             #
#------------------------------------------------------------------------------#

  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    # Requiring the data
    req(listTimeseries$data)
    
    ##########################################
    # Function to go through each row option #
    ##########################################
    lapply(1:(ncol(listTimeseries$data) - 1), function(i) {
      
      # Creating the select input 
      output[[paste0("dist.UI.Prior_", i)]] <- renderUI({
        
      # Creating the select input 
      selectizeInput(paste0("Dist.priors_", i), label = "Prior", choices = c("Normal",
                                                                             "Uniform",
                                                                             "Beta", 
                                                                             "Cauchy", 
                                                                             "Gamma", 
                                                                             "Exponential",
                                                                             "Lognormal",
                                                                             "Neg. Binomial",
                                                                             "Poisson",
                                                                             "Custom"), multiple = FALSE, options = list(create = FALSE))
        
      })
      
    }) # End of `lapply` 
    
  }) # End of 'observe' 
  
#------------------------------------------------------------------------------#
# Creating the prior-specific options to show ----------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the unique options to show for each distribution #
# that could be specified by the user. In this case, distribution refers to    #
# the prior distribution.                                                      #
#------------------------------------------------------------------------------#
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({

    # Requiring the data
    req(listTimeseries$data)

    ##########################################
    # Function to go through each row option #
    ##########################################
    lapply(1:(ncol(listTimeseries$data) - 1), function(i) {

      # Requiring a prior selection
      req(input[[paste0("Dist.priors_", i)]])
    
      ###########################################
      # Observing changes in the prior selected #
      ###########################################
      observeEvent(input[[paste0("Dist.priors_", i)]], {

        # Determining the main 'Prior' selected
        prior_selected <- input[[paste0("Dist.priors_", i)]]

        #################################################
        # Creating the unique options to output: Normal #
        #################################################
        if(prior_selected == "Normal"){

          # Creating the output
          output[[paste0("dist.priorOptions_", i)]] <- renderUI({

            # Row of options
            fluidRow(

              column(6, numericInput(paste0("dist.arg_1_", i), "Mean", value = 1)),
              column(6, numericInput(paste0("dist.arg_2_", i), "SD", value = 2))

            )

          })

        ##################################################
        # Creating the unique options to output: Uniform #
        ##################################################
        }else if(prior_selected == "Uniform"){

          # Creating the output
          output[[paste0("dist.priorOptions_", i)]] <- renderUI({

            # Row of options
            fluidRow(

              column(6, numericInput(paste0("dist.arg_1_", i), "Min", value = 0)),
              column(6, numericInput(paste0("dist.arg_2_", i), "Max", value = 1))
              
            )

          })

        ###############################################
        # Creating the unique options to output: Beta #
        ###############################################
        }else if(prior_selected == "Beta"){

          # Creating the output
          output[[paste0("dist.priorOptions_", i)]] <- renderUI({

            # Row of options
            fluidRow(

              column(6, numericInput(paste0("dist.arg_1_", i), "Alpha", value = 2)),
              column(6, numericInput(paste0("dist.arg_2_", i), "Beta", value = 2))

            )

          })

        #################################################
        # Creating the unique options to output: Cauchy #
        #################################################
        }else if(prior_selected == "Cauchy"){

          # Creating the output
          output[[paste0("dist.priorOptions_", i)]] <- renderUI({

            # Row of options
            fluidRow(

              column(6, numericInput(paste0("dist.arg_1_", i), "Location", value = 0)),
              column(6, numericInput(paste0("dist.arg_2_", i), "Scale", value = 5))
              
            )

          })

        ################################################
        # Creating the unique options to output: Gamma #
        ################################################
        }else if(prior_selected == "Gamma"){

          # Creating the output
          output[[paste0("dist.priorOptions_", i)]] <- renderUI({

            # Row of options
            fluidRow(

              column(6, numericInput(paste0("dist.arg_1_", i), "Shape", value = 2)),
              column(6, numericInput(paste0("dist.arg_2_", i), "Rate", value = 0.5))

            )

          })

        ######################################################
        # Creating the unique options to output: Exponential #
        ######################################################
        }else if(prior_selected == "Exponential"){

          # Creating the output
          output[[paste0("dist.priorOptions_", i)]] <- renderUI({

            # Row of options
            fluidRow(

              column(12, numericInput(paste0("dist.arg_1_", i), "Rate", value = 0.1))

            )

          })

        ####################################################
        # Creating the unique options to output: Lognormal #
        ####################################################
        }else if(prior_selected == "Lognormal"){

          # Creating the output
          output[[paste0("dist.priorOptions_", i)]] <- renderUI({

            # Row of options
            fluidRow(

              column(6, numericInput(paste0("dist.arg_1_", i), "Mean", value = 0)),
              column(6, numericInput(paste0("dist.arg_2_", i), "SD", value = 1))

            )

          })

        ########################################################
        # Creating the unique options to output: Neg. Binomial #
        ########################################################
        }else if(prior_selected == "Neg. Binomial"){

          # Creating the output
          output[[paste0("dist.priorOptions_", i)]] <- renderUI({

            # Row of options
            fluidRow(

              column(6, numericInput(paste0("dist.arg_1_", i), "Mean", value = 10)),
              column(6, numericInput(paste0("dist.arg_2_", i), "Dispersion", value = 2))

            )

          })

        ##################################################
        # Creating the unique options to output: Poisson #
        ##################################################
        }else if(prior_selected == "Poisson"){

          # Creating the output
          output[[paste0("dist.priorOptions_", i)]] <- renderUI({

            # Row of options
            fluidRow(

              column(12, numericInput(paste0("dist.arg_1_", i), "Rate", value = 0.1))

            )

          })

        #################################################
        # Creating the unique options to output: Custom #
        #################################################
        }else if(prior_selected == "Custom"){

          # Creating the output
          output[[paste0("dist.priorOptions_", i)]] <- renderUI({

            # Row of options
            fluidRow(

              column(12, textInput(paste0("dist.arg_1_", i), "Specify a distribution/args.", value = "Ex. normal(0, 5)"))

            )

          })

        }else{

          output[[paste0("dist.priorOptions_", i)]] <- renderUI({NULL})

        }

      }) # End of observeEvent

    }) # End of `lapply`

  }) # End of 'observe'

#------------------------------------------------------------------------------#
# Creating the main prior option -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the main prior option. If the user decides to    #
# estimate the parameter, this option will present various distributions. If   #
# this is fixed, the user will only be able to enter a numeric value.          #
#------------------------------------------------------------------------------#
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    ##########################################
    # Function to go through each row option #
    ##########################################
    lapply(1:(as.numeric(input$num_params) + length(req.params.react())), function(i) {
      
      # Observing changes in fixed selector 
      observeEvent(input[[paste0("fixed_", i)]], {
      
      #########################################################
      # Creating the main prior option: Fixed parameter value #
      #########################################################
      if(input[[paste0("fixed_", i)]] == "Fixed"){
        
        # Creating the numeric input 
        output[[paste0("main.Prior_", i)]] <- renderUI({
          
          numericInput(paste0("priors_", i), label = "Value", value = 1)
          
        })
        
      ############################################################
      # Creating the main prior option: Estimate parameter value #
      ############################################################
      }else{
        
        # Creating the select input 
        output[[paste0("main.Prior_", i)]] <- renderUI({
          
          selectizeInput(paste0("priors_", i), label = "Prior", choices = c("Normal",
                                                                            "Uniform",
                                                                            "Beta", 
                                                                            "Cauchy", 
                                                                            "Gamma", 
                                                                            "Exponential",
                                                                            "Lognormal",
                                                                            "Neg. Binomial",
                                                                            "Poisson",
                                                                            "Custom"), multiple = FALSE, options = list(create = FALSE))
          
        })
        
      }
        
      }) # End of 'observeEvent'
        
    }) # End of 'lapply' 
    
  }) # End of 'observe' 
  
#------------------------------------------------------------------------------#
# Creating the prior-specific options to show ----------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the unique options to show if a distribution is  #
# selected as a prior. For example, if the user the user selects "Normal",     #
# options for the mean, standard deviation, and truncating pops up.            #
#------------------------------------------------------------------------------#
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    ##########################################
    # Function to go through each row option #
    ##########################################
    lapply(1:(as.numeric(input$num_params) + length(req.params.react())), function(i) {
      
      ###########################################
      # Observing changes in the prior selected #
      ###########################################
      observeEvent(input[[paste0("priors_", i)]], {
        
        # Determining the main 'Prior' selected
        prior_selected <- input[[paste0("priors_", i)]]
        
        ################################################
        # Determining if the extra options should show #
        ################################################
        if(input[[paste0("fixed_", i)]] != "Fixed"){
            
          #################################################
          # Creating the unique options to output: Normal #
          #################################################
          if(prior_selected == "Normal"){
            
            # Creating the output 
            output[[paste0("priorOptions_", i)]] <- renderUI({
              
              # Row of options 
              fluidRow(
                
                column(2, numericInput(paste0("arg_1_", i), "Mean", value = 1)),
                column(2, numericInput(paste0("arg_2_", i), "SD", value = 2)),
                column(2,div(style = "padding-top: 32px; justify-content: flex-start;",
                             prettyCheckbox(paste0("truncateZero_", i), "Truncate at 0", value = TRUE, inline = T, bigger = T))),
                column(3, textInput(paste0("LB_", i), label = "Lower Bound", value = 0)), 
                column(3, textInput(paste0("UB_", i), label = "Upper Bound", value = NA))
                
              )
              
            })
            
            ##################################################
            # Creating the unique options to output: Uniform #
            ##################################################
            }else if(prior_selected == "Uniform"){
              
              # Creating the output 
              output[[paste0("priorOptions_", i)]] <- renderUI({
                
                # Row of options 
                fluidRow(
                  
                  column(2, numericInput(paste0("arg_1_", i), "Min", value = 0)),
                  column(2, numericInput(paste0("arg_2_", i), "Max", value = 1)),
                  column(2,div(style = "padding-top: 32px; justify-content: flex-start;",
                               prettyCheckbox(paste0("truncateZero_", i), "Truncate at 0", value = TRUE, inline = T, bigger = T))),
                  column(3, textInput(paste0("LB_", i), label = "Lower Bound", value = 0)), 
                  column(3, textInput(paste0("UB_", i), label = "Upper Bound", value = NA))
                  
                )
                
              })
              
              
            ###############################################
            # Creating the unique options to output: Beta #
            ###############################################
            }else if(prior_selected == "Beta"){
              
              # Creating the output 
              output[[paste0("priorOptions_", i)]] <- renderUI({
                
                # Row of options 
                fluidRow(
                  
                  column(2, numericInput(paste0("arg_1_", i), "Alpha", value = 2)),
                  column(2, numericInput(paste0("arg_2_", i), "Beta", value = 2)),
                  column(2,div(style = "padding-top: 32px; justify-content: flex-start;",
                               prettyCheckbox(paste0("truncateZero_", i), "Truncate at 0", value = TRUE, inline = T, bigger = T))),
                  column(3, textInput(paste0("LB_", i), label = "Lower Bound", value = 0)), 
                  column(3, textInput(paste0("UB_", i), label = "Upper Bound", value = NA))
                  
                )
                
              })
              
            #################################################
            # Creating the unique options to output: Cauchy #
            #################################################
            }else if(prior_selected == "Cauchy"){
              
              # Creating the output 
              output[[paste0("priorOptions_", i)]] <- renderUI({
                
                # Row of options 
                fluidRow(
                  
                  column(2, numericInput(paste0("arg_1_", i), "Location", value = 0)),
                  column(2, numericInput(paste0("arg_2_", i), "Scale", value = 5)),
                  column(2,div(style = "padding-top: 32px; justify-content: flex-start;",
                               prettyCheckbox(paste0("truncateZero_", i), "Truncate at 0", value = TRUE, inline = T, bigger = T))),
                  column(3, textInput(paste0("LB_", i), label = "Lower Bound", value = 0)), 
                  column(3, textInput(paste0("UB_", i), label = "Upper Bound", value = NA))
                  
                )
                
              })
              
            ################################################
            # Creating the unique options to output: Gamma #
            ################################################
            }else if(prior_selected == "Gamma"){
              
              # Creating the output 
              output[[paste0("priorOptions_", i)]] <- renderUI({
                
                # Row of options 
                fluidRow(
                  
                  column(2, numericInput(paste0("arg_1_", i), "Shape", value = 2)),
                  column(2, numericInput(paste0("arg_2_", i), "Rate", value = 0.5)),
                  column(2,div(style = "padding-top: 32px; justify-content: flex-start;",
                               prettyCheckbox(paste0("truncateZero_", i), "Truncate at 0", value = TRUE, inline = T, bigger = T))),
                  column(3, textInput(paste0("LB_", i), label = "Lower Bound", value = 0)), 
                  column(3, textInput(paste0("UB_", i), label = "Upper Bound", value = NA))
                  
                )
                
              })
              
            ######################################################
            # Creating the unique options to output: Exponential #
            ######################################################
            }else if(prior_selected == "Exponential"){
              
              # Creating the output 
              output[[paste0("priorOptions_", i)]] <- renderUI({
                
                # Row of options 
                fluidRow(
                  
                  column(2, numericInput(paste0("arg_1_", i), "Rate", value = 0.1)),
                  column(2,div(style = "padding-top: 32px; justify-content: flex-start;",
                               prettyCheckbox(paste0("truncateZero_", i), "Truncate at 0", value = TRUE, inline = T, bigger = T))),
                  column(3, textInput(paste0("LB_", i), label = "Lower Bound", value = 0)), 
                  column(3, textInput(paste0("UB_", i), label = "Upper Bound", value = NA))
                  
                )
                
              })
              
            ####################################################
            # Creating the unique options to output: Lognormal #
            ####################################################
            }else if(prior_selected == "Lognormal"){
            
              # Creating the output 
              output[[paste0("priorOptions_", i)]] <- renderUI({
                
                # Row of options 
                fluidRow(
                  
                  column(2, numericInput(paste0("arg_1_", i), "Mean", value = 0)),
                  column(2, numericInput(paste0("arg_2_", i), "SD", value = 1)),
                  column(2,div(style = "padding-top: 32px; justify-content: flex-start;",
                               prettyCheckbox(paste0("truncateZero_", i), "Truncate at 0", value = TRUE, inline = T, bigger = T))),
                  column(3, textInput(paste0("LB_", i), label = "Lower Bound", value = 0)), 
                  column(3, textInput(paste0("UB_", i), label = "Upper Bound", value = NA))
                  
                )
                
              })
              
            ########################################################
            # Creating the unique options to output: Neg. Binomial #
            ########################################################
            }else if(prior_selected == "Neg. Binomial"){
              
              # Creating the output 
              output[[paste0("priorOptions_", i)]] <- renderUI({
                
                # Row of options 
                fluidRow(
                  
                  column(2, numericInput(paste0("arg_1_", i), "Mean", value = 10)),
                  column(2, numericInput(paste0("arg_2_", i), "Dispersion", value = 2)),
                  column(2,div(style = "padding-top: 32px; justify-content: flex-start;",
                               prettyCheckbox(paste0("truncateZero_", i), "Truncate at 0", value = TRUE, inline = T, bigger = T))),
                  column(3, textInput(paste0("LB_", i), label = "Lower Bound", value = 0)), 
                  column(3, textInput(paste0("UB_", i), label = "Upper Bound", value = NA))
                  
                )
                
              })
              
            ##################################################
            # Creating the unique options to output: Poisson #
            ##################################################
            }else if(prior_selected == "Poisson"){
              
              # Creating the output 
              output[[paste0("priorOptions_", i)]] <- renderUI({
                
                # Row of options 
                fluidRow(
                  
                  column(2, numericInput(paste0("arg_1_", i), "Rate", value = 0.1)),
                  column(2,div(style = "padding-top: 32px; justify-content: flex-start;",
                               prettyCheckbox(paste0("truncateZero_", i), "Truncate at 0", value = TRUE, inline = T, bigger = T))),
                  column(3, textInput(paste0("LB_", i), label = "Lower Bound", value = 0)), 
                  column(3, textInput(paste0("UB_", i), label = "Upper Bound", value = NA))
                  
                )
                
              })
              
            #################################################
            # Creating the unique options to output: Custom #
            #################################################
            }else if(prior_selected == "Custom"){
              
              # Creating the output 
              output[[paste0("priorOptions_", i)]] <- renderUI({
                
                # Row of options 
                fluidRow(
                  
                  column(4, textInput(paste0("arg_1_", i), "Specify a distribution/args.", value = "Ex. normal(0, 5)")),
                  column(2,div(style = "padding-top: 32px; justify-content: flex-start;",
                               prettyCheckbox(paste0("truncateZero_", i), "Truncate at 0", value = TRUE, inline = T, bigger = T))),
                  column(3, textInput(paste0("LB_", i), label = "Lower Bound", value = 0)), 
                  column(3, textInput(paste0("UB_", i), label = "Upper Bound", value = NA))
                  
                )
                
              })
              
              ##################################################
              # Removing the prior options when input is fixed #
              ##################################################
              }else{
                
                removeUI(selector = paste0("#priorOptions_", i), immediate = TRUE)
                
              }
          
          ##############################
          # Removing the prior options #
          ##############################
          }else{
            
            output[[paste0("priorOptions_", i)]] <- renderUI({NULL})
            
          } # End of if for keeping/removing options
        
        }) # End of observeEvent
      
      }) # End of `lapply`
    
    }) # End of 'observe'

  
#------------------------------------------------------------------------------#
# Specifying the system of ODEs ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the rows for users to specify the system of ODEs.#
# The user selects how many equations are in the system, and then the number   #
# of rows appears. The user can delete rows as needed.                         #
#------------------------------------------------------------------------------#
  
  rowCounter <- reactiveVal(0)
  
  ###########################################
  # Observing changes in the number of rows #
  ###########################################
  observeEvent(input$vars, {
    
    # The current number of rows 
    current_rows <- rowCounter()
    
    # The number of rows there should be 
    new_rows <- input$vars
    
    #######################################
    # Add new rows if the count increases #
    #######################################
    if (new_rows > current_rows) {
      
      # Adding the number of rows based upon the number selected by the user
      for (i in (current_rows + 1):new_rows) {
        
        ############################
        # Creating the UI for rows #
        ############################
        insertUI(
          
          # Label for UI
          selector = "#equationContainer",
          
          # Where to add the row 
          where = "beforeEnd",
          
          # Creating the UI 
          ui = div(
            id = paste0("line_", i),
            class = "equation-line",
            style = "margin: 10px 0; padding: 10px; border: 1px solid #ddd; background-color: #f4f4f4; min-height: 50px;",
            contentEditable = "false", 
            mathInput(paste0("math_", i), label = paste0("Equation ", i))
            
          )
          
        ) # End of insert UI
        
      } # End of loop to create rows 
      
    } # End of indicator that new rows should be added 
    
    ############################################
    # Remove extra rows if the count decreases #
    ############################################
    if (new_rows < current_rows) {
      
      # Removing rows to reach the current row count specified 
      for (i in (new_rows + 1):current_rows) {
        
        # Removing the row 
        removeUI(selector = paste0("#line_", i))
        
      } # End of loop to remove rows 
      
    } # End of indicator that existing rows should be removed 
    
    # Specifying the new row count 
    rowCounter(new_rows)
        
  }) # End of 'observe event'
  
  
#------------------------------------------------------------------------------#
# Adding the equation options for composite variables --------------------------
#------------------------------------------------------------------------------#
# About: This section creates the composite variables equation inputs for the  #
# user. This is later transformed into R code to match the required format     #
# for the options file.                                                        #
#------------------------------------------------------------------------------#

  ###############
  # Row counter #
  ###############
  rowCounterComposite <- reactiveVal(0)
  
  ###########################################
  # Observing changes in the number of rows #
  ###########################################
  observeEvent(input$numComposite, {
    
    # The current number of rows 
    current_rows <- rowCounterComposite()
    
    # The number of rows there should be 
    new_rows <- input$numComposite
    
    #######################################
    # Add new rows if the count increases #
    #######################################
    if (new_rows > current_rows) {
      
      # Adding the number of rows based upon the number selected by the user
      for (i in (current_rows + 1):new_rows) {
        
        ############################
        # Creating the UI for rows #
        ############################
        insertUI(
          
          # Label for UI
          selector = "#compositeEquationContainer",
          
          # Where to add the row 
          where = "beforeEnd",
          
          # Creating the UI 
          ui = fluidRow(
            
            # Label for each row 
            id = paste0("compositeLine_", i),
            
            # Style for rows 
            style = "margin: 10px 0; padding: 10px; border: 1px solid #ddd; background-color: #f4f4f4; min-height: 50px;",
            
            # UI components for each column in the row
            column(1, textInput(paste0("LeftEquation_", i), label = "Variable", value = NULL)),  
            column(2, mathInput(paste0("compositeID_", i), label = "Equation"))
          )
          
        ) # End of insertUI
        
      } # End of loop to create rows 
      
    } # End of indicator that new rows should be added 
    
    ############################################
    # Remove extra rows if the count decreases #
    ############################################
    if (new_rows < current_rows) {
      
      # Removing rows to reach the current row count specified 
      for (i in (new_rows + 1):current_rows) {
        
        # Removing the row 
        removeUI(selector = paste0("#compositeLine_", i))
        
      } # End of loop to remove rows 
      
    } # End of indicator that existing rows should be removed 
    
    # Specifying the new row count 
    rowCounterComposite(new_rows)
    
  }) # End of 'observe event'
  
  
#------------------------------------------------------------------------------#
# Creating the list of parameters, LB and UB for the extra dist parameters -----
#------------------------------------------------------------------------------#
# About: This section creates the specific entries for the prior distributions #
# the user needs to select for the error structure specification. This only    #
# applies if the error structure is either selected as 'Normal' or 'Neg. Bin'. #
#------------------------------------------------------------------------------#
  
  ########################################
  # Empty list to store parameter values #
  ########################################
  parameters.inital.dist.list <- reactiveValues()

  ###################################
  # Creating the list of parameters #
  ###################################
  observe({

    # Requiring the data
    req(listTimeseries$data)

    # List of parameter initial values
    params.initial.temp <- NULL

    ################################################
    # Looping through parameter options: Version 1 #
    ################################################
    for(i in 1:(ncol(listTimeseries$data) - 1)){ 
      
      req(input[[paste0("Dist.priors_", i)]])
      
      ###################################################
      # Preparing the array of prior conditions: Normal #
      ###################################################
      if(input[[paste0("Dist.priors_", i)]] == "Normal"){

          prior.temp <- paste0("normal(", input[[paste0("dist.arg_1_", i)]], ",", input[[paste0("dist.arg_2_", i)]], ")")
          
      ####################################################
      # Preparing the array of prior conditions: Uniform #
      ####################################################
      }else if(input[[paste0("Dist.priors_", i)]] == "Uniform"){
        
        prior.temp <- paste0("uniform(", input[[paste0("dist.arg_1_", i)]], ",", input[[paste0("dist.arg_2_", i)]], ")")
        
      ###################################################
      # Preparing the array of prior conditions: Cauchy #
      ###################################################
      }else if(input[[paste0("Dist.priors_", i)]] == "Cauchy"){
        
        prior.temp <- paste0("cauchy(", input[[paste0("dist.arg_1_", i)]], ",", input[[paste0("dist.arg_2_", i)]], ")")
        
      ##################################################
      # Preparing the array of prior conditions: Gamma #
      ##################################################
      }else if(input[[paste0("Dist.priors_", i)]] == "Gamma"){
        
        prior.temp <- paste0("gamma(", input[[paste0("dist.arg_1_", i)]], ",", input[[paste0("dist.arg_2_", i)]], ")")
        
      ########################################################
      # Preparing the array of prior conditions: Exponential #
      ########################################################
      }else if(input[[paste0("Dist.priors_", i)]] == "Exponential"){
        
        prior.temp <- paste0("exponential(", input[[paste0("dist.arg_1_", i)]], ")")
        
      ######################################################
      # Preparing the array of prior conditions: Lognormal #
      ######################################################
      }else if(input[[paste0("Dist.priors_", i)]] == "Lognormal"){
        
        prior.temp <- paste0("lognormal(", input[[paste0("dist.arg_1_", i)]], ",", input[[paste0("dist.arg_2_", i)]], ")")
        
      ##############################################################
      # Preparing the array of prior conditions: Negative Binomial #
      ##############################################################
      }else if(input[[paste0("Dist.priors_", i)]] == "Neg. Binomial"){
        
        prior.temp <- paste0("neg_binomial_2(", input[[paste0("dist.arg_1_", i)]], ",", input[[paste0("dist.arg_2_", i)]], ")")
        
      ####################################################
      # Preparing the array of prior conditions: Poisson #
      ####################################################
      }else if(input[[paste0("Dist.priors_", i)]] == "Poisson"){
        
        prior.temp <- paste0("poisson(", input[[paste0("dist.arg_1_", i)]], ")")
        
      ###################################################
      # Preparing the array of prior conditions: Custom #
      ###################################################
      }else if(input[[paste0("Dist.priors_", i)]] == "Custom"){
        
        prior.temp <- paste0(input[[paste0("dist.arg_1_", i)]])
        
      ##################################################
      # Preparing the array of prior conditions: Fixed #
      ##################################################
      }else{

        prior.temp <- NULL

      }

      ####################################################
      # Adding the parameter characteristics to the list #
      ####################################################

      # Saving the parameter initial values
      params.initial.temp <- c(params.initial.temp, prior.temp)

    }

    ##########################################
    # Saving the list to the reactive values #
    ##########################################

    # Initial parameter values
    parameters.inital.dist.list$list <- params.initial.temp

  })
  
#------------------------------------------------------------------------------#
# Creating the list of parameters, fitting values, LB, and UB to index ---------
#------------------------------------------------------------------------------#
# About: This creates the list of parameters, LBs and UBs entered by the user. #
# It also creates the vector of initial values used to parameterize params.    #
#------------------------------------------------------------------------------#
  
  ##################################
  # Empty list to store parameters #
  ##################################
  parameters.list <- reactiveValues()
  
  ###########################################################
  # Empty list to store fixed/not fixed state of parameters #
  ###########################################################
  parameters.fixed.list <- reactiveValues()
  
  ########################################
  # Empty list to store parameter values #
  ########################################
  parameters.inital.list <- reactiveValues()
  
  ####################################
  # Empty list to store parameter LB #
  ####################################
  parameters.LB.list <- reactiveValues()
  
  ####################################
  # Empty list to store parameter UB #
  ####################################
  parameters.UB.list <- reactiveValues()
  
  ###################################
  # Creating the list of parameters #
  ###################################
  observe({
    
    # List of parameters 
    parameters.list.temp <- NULL
    
    # List of fixed status
    parameters.fixed.list.temp <- NULL
    
    # List of parameter initial values 
    params.initial.temp <- NULL
    
    # List of parameter LB values
    params.LB.list.temp <- NULL
    
    # List of parameter UB values
    params.UB.list.temp <- NULL
    
    #####################################
    # Looping through parameter options #
    #####################################
    for(i in 1:(input$num_params + length(req.params.react()))){
      
      # Requiring a row
      req(input[[paste0("fixed_", i)]])
      req(input[[paste0("priors_", i)]])
      
      ################################################
      # Calling the indexed parameter characteristic #
      ################################################
      
      # Saving the parameters output for the indexed parameter
      if(i <= length(req.params.react()) & !is.null(req.params.react())){
        
        parameters.temp <- req.params.react()[[i]]
        
      }else{
        
        parameters.temp <- input[[paste0("paramsID_", i)]]
        
      }
      
      # Saving the parameters fixed status output
      parameters.fixed.temp <- input[[paste0("fixed_", i)]]

      ##############################
      # Preparing the fixed status #
      ##############################
      paramNumeric <- switch(parameters.fixed.temp,
        "Fixed" = 1,
        "Estimate" = 0
      )
      
      ##########################################################
      # Preparing the LB and UB vector: Fixed Parameter Status #
      ##########################################################
      if(parameters.fixed.temp == "Fixed"){

        # Lower bound
        params.LB.temp <- -1

        # Upper bound
        params.UB.temp <- -1

      #############################################################
      # Preparing the LB and UB vector: Estimate Parameter Status #
      #############################################################
      }else{

        # Lower Bound
        params.LB.temp <- input[[paste0("LB_", i)]]

        # Upper Bound
        params.UB.temp <- input[[paste0("UB_", i)]]

      }
      
      ###################################################
      # Preparing the array of prior conditions: Normal #
      ###################################################
      if(input[[paste0("priors_", i)]] == "Normal"){

        # Requiring truncation entry
        req(input[[paste0("truncateZero_", i)]])

        ###########################################
        # Creating the prior entry: Truncate at 0 #
        ###########################################
        if(input[[paste0("truncateZero_", i)]]){

          prior.temp <- paste0("normal(", input[[paste0("arg_1_", i)]], ",", input[[paste0("arg_2_", i)]], ")T[0,]")

        ##############################################
        # Creating the prior entry: No truncate at 0 #
        ##############################################
        }else{

          prior.temp <- paste0("normal(", input[[paste0("arg_1_", i)]], ",", input[[paste0("arg_2_", i)]], ")")

        }
        
      ####################################################
      # Preparing the array of prior conditions: Uniform #
      ####################################################
      }else if(input[[paste0("priors_", i)]] == "Uniform"){

        # Requiring truncation entry
        req(input[[paste0("truncateZero_", i)]])

        ###########################################
        # Creating the prior entry: Truncate at 0 #
        ###########################################
        if(input[[paste0("truncateZero_", i)]]){

          prior.temp <- paste0("uniform(", input[[paste0("arg_1_", i)]], ",", input[[paste0("arg_2_", i)]], ")T[0,]")

        ##############################################
        # Creating the prior entry: No truncate at 0 #
        ##############################################
        }else{

          prior.temp <- paste0("uniform(", input[[paste0("arg_1_", i)]], ",", input[[paste0("arg_2_", i)]], ")")

        }

      ###################################################
      # Preparing the array of prior conditions: Cauchy #
      ###################################################
      }else if(input[[paste0("priors_", i)]] == "Cauchy"){

        # Requiring truncation entry
        req(input[[paste0("truncateZero_", i)]])

        ###########################################
        # Creating the prior entry: Truncate at 0 #
        ###########################################
        if(input[[paste0("truncateZero_", i)]]){

          prior.temp <- paste0("cauchy(", input[[paste0("arg_1_", i)]], ",", input[[paste0("arg_2_", i)]], ")T[0,]")

        ##############################################
        # Creating the prior entry: No truncate at 0 #
        ##############################################
        }else{

          prior.temp <- paste0("cauchy(", input[[paste0("arg_1_", i)]], ",", input[[paste0("arg_2_", i)]], ")")

        }

      ##################################################
      # Preparing the array of prior conditions: Gamma #
      ##################################################
      }else if(input[[paste0("priors_", i)]] == "Gamma"){

        # Requiring truncation entry
        req(input[[paste0("truncateZero_", i)]])

        ###########################################
        # Creating the prior entry: Truncate at 0 #
        ###########################################
        if(input[[paste0("truncateZero_", i)]]){

          prior.temp <- paste0("gamma(", input[[paste0("arg_1_", i)]], ",", input[[paste0("arg_2_", i)]], ")T[0,]")

        ##############################################
        # Creating the prior entry: No truncate at 0 #
        ##############################################
        }else{

          prior.temp <- paste0("gamma(", input[[paste0("arg_1_", i)]], ",", input[[paste0("arg_2_", i)]], ")")

        }

      ########################################################
      # Preparing the array of prior conditions: Exponential #
      ########################################################
      }else if(input[[paste0("priors_", i)]] == "Exponential"){

        # Requiring truncation entry
        req(input[[paste0("truncateZero_", i)]])

        ###########################################
        # Creating the prior entry: Truncate at 0 #
        ###########################################
        if(input[[paste0("truncateZero_", i)]]){

          prior.temp <- paste0("exponential(", input[[paste0("arg_1_", i)]], ")T[0,]")

        ##############################################
        # Creating the prior entry: No truncate at 0 #
        ##############################################
        }else{

          prior.temp <- paste0("exponential(", input[[paste0("arg_1_", i)]], ")")

        }

      ######################################################
      # Preparing the array of prior conditions: Lognormal #
      ######################################################
      }else if(input[[paste0("priors_", i)]] == "Lognormal"){

        # Requiring truncation entry
        req(input[[paste0("truncateZero_", i)]])

        ###########################################
        # Creating the prior entry: Truncate at 0 #
        ###########################################
        if(input[[paste0("truncateZero_", i)]]){

          prior.temp <- paste0("lognormal(", input[[paste0("arg_1_", i)]], ",", input[[paste0("arg_2_", i)]], ")T[0,]")

        ##############################################
        # Creating the prior entry: No truncate at 0 #
        ##############################################
        }else{

          prior.temp <- paste0("lognormal(", input[[paste0("arg_1_", i)]], ",", input[[paste0("arg_2_", i)]], ")")

        }

      ##############################################################
      # Preparing the array of prior conditions: Negative Binomial #
      ##############################################################
      }else if(input[[paste0("priors_", i)]] == "Neg. Binomial"){

        # Requiring truncation entry
        req(input[[paste0("truncateZero_", i)]])

        ###########################################
        # Creating the prior entry: Truncate at 0 #
        ###########################################
        if(input[[paste0("truncateZero_", i)]]){

          prior.temp <- paste0("neg_binomial_2(", input[[paste0("arg_1_", i)]], ",", input[[paste0("arg_2_", i)]], ")T[0,]")

        ##############################################
        # Creating the prior entry: No truncate at 0 #
        ##############################################
        }else{

          prior.temp <- paste0("neg_binomial_2(", input[[paste0("arg_1_", i)]], ",", input[[paste0("arg_2_", i)]], ")")

        }

      ####################################################
      # Preparing the array of prior conditions: Poisson #
      ####################################################
      }else if(input[[paste0("priors_", i)]] == "Poisson"){

        # Requiring truncation entry
        req(input[[paste0("truncateZero_", i)]])

        ###########################################
        # Creating the prior entry: Truncate at 0 #
        ###########################################
        if(input[[paste0("truncateZero_", i)]]){

          prior.temp <- paste0("poisson(", input[[paste0("arg_1_", i)]], ")T[0,]")

        ##############################################
        # Creating the prior entry: No truncate at 0 #
        ##############################################
        }else{

          prior.temp <- paste0("poisson(", input[[paste0("arg_1_", i)]], ")")

        }

      ###################################################
      # Preparing the array of prior conditions: Custom #
      ###################################################
      }else if(input[[paste0("priors_", i)]] == "Custom"){

        # Requiring truncation entry
        req(input[[paste0("truncateZero_", i)]])

        ###########################################
        # Creating the prior entry: Truncate at 0 #
        ###########################################
        if(input[[paste0("truncateZero_", i)]]){

          prior.temp <- paste0(input[[paste0("arg_1_", i)]],"T[0,]")

        ##############################################
        # Creating the prior entry: No truncate at 0 #
        ##############################################
        }else{

          prior.temp <- paste0(input[[paste0("arg_1_", i)]])

        }

      ##################################################
      # Preparing the array of prior conditions: Fixed #
      ##################################################
      }else{

        prior.temp <- input[[paste0("priors_", i)]]

      }

      ####################################################
      # Adding the parameter characteristics to the list #
      ####################################################

      # Saving the parameter initial values
      params.initial.temp <- c(params.initial.temp, prior.temp)

      # Saving the parameters in a variable
      parameters.list.temp <- c(parameters.list.temp, parameters.temp)

      # Saving the parameters fixed status in a variable
      parameters.fixed.list.temp <- c(parameters.fixed.list.temp, paramNumeric)

      # Saving the parameters LB in a variable
      params.LB.list.temp <- c(params.LB.list.temp, params.LB.temp)

      # Saving the parameter UB in a variable
      params.UB.list.temp <- c(params.UB.list.temp, params.UB.temp)

    }
    
    ##########################################
    # Saving the list to the reactive values #
    ##########################################
    
    # Parameters 
    parameters.list$list <- parameters.list.temp

    # Parameters fixed status
    parameters.fixed.list$list <- parameters.fixed.list.temp
    
    # Initial parameter values
    parameters.inital.list$list <- params.initial.temp

    # Parameter LB list 
    parameters.LB.list$list <- params.LB.list.temp 
 
    # Parameter UB list 
    parameters.UB.list$list <- params.UB.list.temp 

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
  
  # Error catcher
  errorCatch.temp <- reactiveVal(0)
  
  ########################################1
  # Observing changes in reactive values #
  ########################################
  observeEvent(input$loadEquation, {
    
    # Requiring the initial conditions to be specified 
    req(initial.conditions.vec$IC)
    
    ############################
    # Preparing the parameters #
    ############################

    # Looping through parameters 
    params.temp.list <- sapply(1:length(parameters.list$list), function(i) {
      
      # Adding each equation 
      param.index <- as.character(parameters.list$list[[i]])
      
      # Array to store parameters
      params.temp <- NULL
      
      if(any(nchar(parameters.list$list)) == 0){
        
        return("ERROR2")
        
      }
      
      # Building error to check parameter parsing 
      tryCatch({
        
        # Latex to text 
        params.text <- latex2r(param.index)

        # Adding the parameters to the array
        params.temp <- c(params.temp, params.text)
        
      # Error to check for latex
      }, latex2r.error = function(cnd) {
        
        return("ERROR1")
        
      },
      
      error = function(cnd) {
        
       return("ERROR1")
        
      })
      
    })
    
    #################################
    # Creating the parameter errors #
    #################################
    if(any(params.temp.list == "ERROR1")){
      
      shinyalert::shinyalert(title = "Error!", text = "Please check the specification of your parameters, the LaTeX specification may be incorrect. There was an error when translating to R code.", type = "error")
      
      params.temp.list <- NULL
      
    }else if(any(params.temp.list == "ERROR2")){
      
      shinyalert::shinyalert(title = "Error!", text = "Please specify all required parameters prior to loading equations.", type = "error")
      
      params.temp.list <- NULL
      
    }
    
    ################################################
    # Adding the parameters to the reactive values #
    ################################################
    paramsFinal$params <- params.temp.list
    
    ##############################################
    # Collecting the equations into one variable #
    ##############################################
    equations <- sapply(1:input$vars, function(i) {
      
      # Adding each equation 
      equation <- as.character(input[[paste0("math_", i)]])
      
      equation.temp <- NULL
      
      # Error checkers
      error.check.temp <- 0
      
      ###############################################
      # Checking if any equations have been entered #
      ###############################################
      if(nchar(equation) == 0){
        
        # Switching the error checker
        error.check.temp <- 1
        
        # Error
        shinyalert::shinyalert(title = "Error!", text = "Please specify all required equations prior to loading equations.", type = "error")
        
      }

      ############################################
      # Building error to check equation parsing #
      ############################################
      tryCatch({
        
        # Changing it to a R code
        equation.temp <- paste0(latex2r(equation))
        
      ###################
      # Error to return #
      ###################
      }, latex2r.error = function(cnd1) {
        
        if(error.check.temp == 0){
          
          shinyalert::shinyalert(title = "Error!", text = "Please check the specification of your equations, the LaTeX specification may be incorrect. There was an error when translating to R code.", type = "error")
          
          equation.temp <- NULL
          
        }
        
      },
      
      ###################
      # Error to return #
      ###################
      error = function(cnd1) {
        
        if(error.check.temp == 0){
          
          shinyalert::shinyalert(title = "Error!", text = "Please check the specification of your equations, the LaTeX specification may be incorrect. There was an error when translating to R code.", type = "error")
          
          equation.temp <- NULL
          
        }
        
      })
      
      # Creating the base for the final equation 
      equation.final <- equation.temp
      
      ##################################################
      # Replacing the parameters with "paramsi" labels #
      ##################################################
      for(b in 1:length(params.temp.list)) {
        
        # Requiring the final equation
        req(equation.final)
        
        # Requiring parameters
        req(params.temp.list)
        
        # Sub-setting the parameter 
        param <- params.temp.list[b]
        
        # Replacing the parameter
        replacement <- paste0("params", b)
        
        equation.final <- gsub(param, replacement, equation.final, fixed = TRUE)
        
      }
        
      ################################################
      # Replacing the parameters with "varsi" labels #
      ################################################
      for (c in 1:length(selectedValues())) {
        
        req(equation.final)
        
        vars <- selectedValues()[c]
        
        replacement <- paste0("vars", c)
        
        equation.final <- gsub(vars, replacement, equation.final, fixed = TRUE)
        
      }
      
      #############################
      # Adding the starting value #
      #############################
      equation.final <- paste0("diff_var", i, " = ", equation.final)
      
      return(equation.final)
        
    })

    ###############################################
    # Combine into a single string with new lines #
    ###############################################
    ode_system <- paste0("'\n",paste((equations), collapse = "\n"), "'")

    # Saving the system of ODE to the reactive value
    odeEquation$system <- ode_system

  })
  
#------------------------------------------------------------------------------#
# Requiring initial conditions -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks to see if the initial conditions have be entered. #
# if any have not been entered, the user will receive an error.                #
#------------------------------------------------------------------------------#
  
  #################################################
  # Observing clicks of the load equations button #
  #################################################
  observeEvent(input$loadEquation,{
    
    #############################################
    # Checking if the error should be triggered #
    #############################################
    if(is.null(initial.conditions.vec$IC) || length(initial.conditions.vec$IC) < input$vars){
      
      # Alert 
      shinyalert::shinyalert("Error!", "Please make sure all initial conditions have been entered. You are missing one or more initial condition entries.", type = "error")
    
      # Resetting the IC
      initial.conditions.vec$IC <- NULL
      
    }
    
  })
  
#------------------------------------------------------------------------------#
# Creating the IC conditions ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the appropriate IC conditions, including the     #
# vector of initial conditions, parameter fixed status, and vars.init value.   #
#------------------------------------------------------------------------------#
  
  #############################################
  # Reactive values for various IC parameters #
  #############################################
  
  # IC list
  ic.list.temp <- reactiveValues()
  
  # Estimating IC indicator
  vars.init.temp <- reactiveVal()
  
  # Fixed parameters list
  fixed.params.temp <- reactiveValues()
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observeEvent(input$loadEquation, {
    
    # Required list of initial conditions
    req(initial.conditions.vec$IC)
    
    # Required list of parameters 
    req(paramsFinal$params)
    
    # Requiring the system of equations
    req(odeEquation$system)
    
    # Requiring the list fixed status
    req(parameters.fixed.list$list)
    
    # Requiring the initial list 
    req(parameters.inital.list$list)
    
    ################################################
    # Preparing the list of equations for function #
    ################################################
    equations <- sapply(1:input$vars, function(i) {
      
      # Adding each equation 
      equation <- as.character(input[[paste0("math_", i)]])
      
      equation.temp <- NULL
      
      # Changing it to a R code
      equation.temp <- paste0(latex2r(equation))
      
    })
    
    ########################
    # Calling the function #
    ########################
    IcOutput <- check.IC(ic.list = initial.conditions.vec$IC,
                         params.list = paramsFinal$params,
                         equations.list = equations,
                         fixedStatus = parameters.fixed.list$list,
                         initParam = parameters.inital.list$list)

    #########################################################
    # Adding the output to their respective reactive values #
    #########################################################

      # Error 1: An estimated parameter is included
      if(all(!is.list(IcOutput) && IcOutput == 1 & length(paramsFinal$params) > 1)){
        
        shinyalert::shinyalert(title = "Error!", text = "A parameter currently included in your initial conditions AND system of ODEs is being estimated. Please update the impacted parameter to a fixed value.", type = "error")
      
        parameters.fixed.list$list <- NULL
        
        fixed.params.temp$list <- NULL
        
      }else if(all(!is.list(IcOutput) && IcOutput == 1 & length(paramsFinal$params) == 1)){
        
        shinyalert::shinyalert(title = "Error!", text = "A parameter currently included in your initial conditions AND system of ODEs is being estimated, and needs to be fixed. However, you only have one parameter specified, and at least one parameter must be estimated. Please either (1) add an estimated parameter or (2) remove the parameter from the initial conditions and estimate its initial value.", type = "error")
        
        parameters.fixed.list$list <- NULL
        
        fixed.params.temp$list <- NULL
        
      }else if(all(as.numeric(parameters.fixed.list$list) == 1)){
        
        shinyalert::shinyalert("Error!", "At least one parameter must be estimated.", type = "error")
        
        parameters.fixed.list$list <- NULL
        
        fixed.params.temp$list <- NULL
        
      }else if(all(is.list(IcOutput) & all(!is.numeric(unlist(IcOutput[[1]]))) & length(paramsFinal$params) == 1)){
        
        shinyalert::shinyalert(title = "Error!", text = "A parameter currently included in your initial conditions AND system of ODEs is fixed as it should be. However, you only have one parameter specified, and at least one parameter must be estimated. Please either (1) add an estimated parameter or (2) remove the parameter from the initial conditions and estimate its initial value.", type = "error")
        
        parameters.fixed.list$list <- NULL
        
        fixed.params.temp$list <- NULL
        
      }else if(all(!is.list(IcOutput) && IcOutput == 2)){
        
        shinyalert::shinyalert("Error!", "Any parameters included in the initial conditions list but not in the system of ODEs must be estimated. Please update the impacted parameter(s) prior to reloading the equations.", type = "error")
        
        parameters.fixed.list$list <- NULL
        
        fixed.params.temp$list <- NULL
   
      }else{
        
        # IC list
        ic.list.temp$list <- IcOutput[[1]]
        
        # Estimating IC indicator
        vars.init.temp(IcOutput[[2]])
        
        # Fixed parameters list
        fixed.params.temp$list <- parameters.fixed.list$list
        
      }

  })
  
#------------------------------------------------------------------------------#
# Creating the equation pop-up -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the pop-up users get when they click the button  #
# to load the equations. The pop-up will confirm for users that all eqs. have  #
# been uploaded successfully.                                                  #
#------------------------------------------------------------------------------#
  
  # Load indicator
  loadEquationTrigger <- reactiveVal(0)
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observeEvent(input$loadEquation, {
    
    # Requiring the new parameter fixed list
    req(fixed.params.temp$list)
    
    # Requiring the equations
    req(odeEquation$system)
    
    # Changing the indicator
    loadEquationTrigger(1)
  
    shinyalert::shinyalert(text = "Equations have been uploaded sucessfully.", type = "success")
    
    })

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
  
  ##############################################
  # Collecting the equations into one variable #
  ##############################################
  observeEvent(input$loadComposite, {
    
    ###########################################
    # Empty list to store composite variables #
    ###########################################
    composite.list.temp <- list()
    
    # Looping through provided equations 
    for(i in 1:input$numComposite){
      
      ######################
      # Indexing variables #
      ######################
      
      # Index left side
      left.side.temp <- paste0(as.character(input[[paste0("LeftEquation_", i)]]))
      
      right.side.temp <- NULL
      
      # Building error to check equation parsing 
      tryCatch({
        
        # Index right side 
        right.side.temp <- paste0(latex2r(as.character(input[[paste0("compositeID_", i)]]))) 
        
      ######################
      # Returning an Error #
      ######################
      }, latex2r.error = function(cnd2) {
        
        shinyalert::shinyalert(title = "Error!", text = "Please check the specification of your composite equations, the LaTeX specification may be incorrect. There was an error when translating to R code.", type = "error")
        
        right.side.temp <- NULL
        
        left.side.temp <- NULL
        
        composite.variable.list$list <- NULL
      },
      
      ###################
      # Catch-All Error #
      ###################
      error = function(cnd2) {
        
        shinyalert::shinyalert(title = "Error!", text = "Unexpected error with the composite equation specification. Please try again.", type = "error")
        
        right.side.temp <- NULL
        
        left.side.temp <- NULL
        
        composite.variable.list$list <- NULL
        
      })
      
      ###############################################################
      # Assigning the value (i.e., creating the composite equation) #
      ###############################################################
      if(!is.null(right.side.temp) & !is.null(left.side.temp)){
        
        # Adding the variable to the list
        composite.list.temp[i] <- right.side.temp
        
        # Adding the variable name
        names(composite.list.temp)[i] <- left.side.temp
        
      }else{
        
        composite.list.temp[i] <- NULL
        
        names(composite.list.temp)[i] <- NULL
        
        composite.variable.list$list <- NULL
        
      }
      
    }
    
    #########################################################
    # Adding the variables to the list for the options file #
    #########################################################
    composite.variable.list$list <- composite.list.temp
    
    })
    
#------------------------------------------------------------------------------#
# Creating the equation pop-up -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the pop-up users get when they click the button  #
# to load the equations. The pop-up will confirm for users that all eqs. have  #
# been uploaded successfully.                                                  #
#------------------------------------------------------------------------------#
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observeEvent(input$loadComposite, {
    
    # Requiring the new parameter fixed list
    req(composite.variable.list$list)

    if(length(composite.variable.list$list) > 0){
    
      shinyalert::shinyalert(text = "Composite equations have been uploaded sucessfully.", type = "success")
      
    }
    
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
  
  observeEvent(input$run, {
    
#------------------------------------------------------------------------------#
# Creating the alert if the dashboard is run before all objects are selected----
#------------------------------------------------------------------------------#
# About: This section checks to see if all items have been selected. If not,   #
# the user will see an error prompting them to review their inputs.            #
#------------------------------------------------------------------------------#

    ################################
    # Requiring files prior to run #
    ################################
    if (any(
      is.null(input$calibrationperiods),
      is.null(input$forecastinghorizon),
      is.null(selectedValues()),
      is.null(paramsFinal$params),
      is.null(paste(odeEquation$system, collapse = "\n")),
      is.null(fixed.params.temp$list),
      is.null(fitting_index_values$fit),
      is.null(na.omit(vector.fit.diff.state$state)),
      is.null(errorStructureNumber()),
      is.null(input$caddisease),
      is.null(series_cases_temp$labels),
      is.null(date_type_temp$temporal),
      is.null(parameters.inital.list$list),
      is.null(parameters.LB.list$list),
      is.null(parameters.UB.list$list),
      is.null(as.numeric(vars.init.temp())),
      is.null(ic.list.temp$list),
      is.null(input$niter),
      is.null(input$num_chain))){
      
      # Creating the error
      shinyalert::shinyalert("Error!", text = "Not all required options have been indicated. Prior to re-running the dashboard, please review your specifications to ensure all parameters are entered.", type = "error")
      
      # Clearing in options file entries
      optionsFileReactive$file <- NULL
      
#------------------------------------------------------------------------------#
# Setting up the `options.R` file ----------------------------------------------
#------------------------------------------------------------------------------#
# This section is dedicated to setting up the options.R file that is used in   #
# the remaining functions to receive all outputs.                              #
#------------------------------------------------------------------------------#
    }else{
      
    if(loadEquationTrigger() == 0){
        
    shinyalert(title = "Error!", text = "Please re-load the equations", type = "error")
      
    # Clearing the options file 
    optionsFileReactive$file <- NULL
        
    }else{
      
    # Resetting the equation load indicator
    loadEquationTrigger(0)
      
    # Define options file path
    options_file <- file.path(tempdir(), "options.R")
    
    ########################################
    # Formatting the calibration period(s) #
    ########################################
    calibrationperiod_values <- paste(as.numeric(input$calibrationperiods), collapse = ", ")

    ########################################
    # Formatting the state variables entry #
    ########################################
    vars_values <- paste0('"', selectedValues(), '"', collapse = ", ")
    
    ############################################
    # Formatting the parameter variables entry #
    ############################################
    params_values <- paste0('"', paramsFinal$params, '"', collapse = ", ")
    
    ##############################################
    # Formatting the fixed status for parameters #
    ##############################################
    paramsFixed_values <- paste(as.numeric(fixed.params.temp$list), collapse = ", ")
    
    ######################################
    # Formatting the composite variables #
    ######################################
    
    # Going from list to string 
    formatted_expressions <- sprintf('%s = "%s"', names(composite.variable.list$list), composite.variable.list$list)
    
    # Adding lines between statements 
    result <- paste(formatted_expressions, collapse = ",\n")
    
    ########################################
    # Formatting the `series_cases` option #
    ########################################
    series_cases_values <- paste0('"', series_cases_temp$labels, '"', collapse = ", ")
    
    #######################
    # Prior values vector #
    #######################
    vec <- parameters.inital.list$list
    
    ###################################
    # Function to format prior values #
    ###################################
    format_prior <- function(value, index){
      
      ##############################
      # Determining if its a prior #
      ##############################
      if(grepl("\\(", value)){
        
        # Returning the formatted prior 
        return(sprintf('params%d_prior <- "%s"', index, value))
        
      ##############################
      # Determining if its numeric #
      ##############################
      }else{
        
        # Returning the numeric value 
        return(sprintf('params%d_prior <- %s', index, as.numeric(value)))
        
      }
      
    } # End of function 
    
    ############################################
    # Generate formatted paramsX_prior strings #
    ############################################
    params_prior_assignments <- paste(mapply(format_prior, vec, seq_along(vec)), collapse = "\n")
    
    ###########################################################
    # Determining the number of columns errors are needed for #
    ###########################################################
    vec.params.dist <- parameters.inital.dist.list$list
    
    ####################################################
    # Function to format prior values for distribution #
    ####################################################
    format_prior.dist <- function(value, index){
      
      ################################
      # Prior type to return: Normal #
      ################################
      if(errorStructureNumber() == 2){
        
        # Returning the formatted prior 
        return(sprintf('normalerror%d_prior <- "%s"', index, value))
        
      ###########################################
      # Prior type to return: Negative Binomial #
      ########################################### 
      }else if(errorStructureNumber() == 1){
        
        # Returning the formatted prior 
        return(sprintf('negbinerror%d_prior <- "%s"', index, value))
        
      #################################
      # Prior type to return: Poisson #
      #################################
      }else{
        
        return(NULL)
        
      }
      
    } # End of function 
    
    ####################################################
    # Generate formatted errorStructureX_prior strings #
    ####################################################
    params_prior_dist_assignments <- paste(mapply(format_prior.dist, vec.params.dist, seq_along(vec.params.dist)), collapse = "\n")
    
    #######################
    # LB Parameter vector #
    #######################
    LB <- parameters.LB.list$list
    
    ######################################
    # Function to format prior LB values #
    ######################################
    format_LB <- function(value, index){
      
      ############################
      # Determining if its fixed #
      ############################
      if(value == -1){
        
        # Returning nothing
        return(sprintf('params%d_LB <- %s', index, NA))
        
      ###########################
      # Determining if its a NA #
      ###########################
      }else if(value == "NA"){
        
        # Returning the NA
        return(sprintf('params%d_LB <- %s', index, NA))
        
      ###############################
      # Determining if its a number #
      ###############################
      }else{
        
        # Returning the number
        return(sprintf('params%d_LB <- %s', index, as.numeric(value)))
        
      }
      
    } # End of function 
    
    #########################################
    # Generate formatted paramsX_LB strings #
    #########################################
    params_LB_assignments <- paste(mapply(format_LB, LB, seq_along(LB)), collapse = "\n")
    
    #######################
    # UB Parameter vector #
    #######################
    UB <- parameters.UB.list$list
    
    ######################################
    # Function to format prior UB values #
    ######################################
    format_UB <- function(value, index){
      
      ############################
      # Determining if its fixed #
      ############################
      if(value == -1){
        
        # Returning nothing
        return(sprintf('params%d_UB <- %s', index, NA))
        
      ###########################
      # Determining if its a NA #
      ###########################
      }else if(value == "NA"){
        
        # Returning the NA
        return(sprintf('params%d_UB <- %s', index, NA))
        
      ###############################
      # Determining if its a number #
      ###############################
      }else{
        
        # Returning the number
        return(sprintf('params%d_UB <- %s', index, as.numeric(value)))
        
      }
      
    } # End of function 
    
    #########################################
    # Generate formatted paramsX_UB strings #
    #########################################
    params_UB_assignments <- paste(mapply(format_UB, UB, seq_along(UB)), collapse = "\n")
    
    #####################################
    # Generating the initial conditions #
    #####################################
    
    # Function to determine if should be in quotes or not
    formatted <- sapply(ic.list.temp$list, function(item) {
      
      if (is.character(item)) {
        
        sprintf('"%s"', item)
        
      } else {
        
        as.character(item)
        
      }
      
    })
    
    # Create the IC input line 
    Ic.Input.Temp <- sprintf("Ic <- c(%s)", paste(formatted, collapse = ", "))
    
    #############################
    # Creating the options file #
    #############################
    options_content <- sprintf('
    
      calibrationperiods <- c(%s)
      
      forecastinghorizon <- %d
      
      model_name <- "FILLER"
      
      vars <- c(%s)
      
      params <- c(%s)
      
      ode_system <- %s
      
      paramsfix <- c(%s)
      
      composite_expressions <- list(%s)
      
      fitting_index <- c(%s)
      
      fitting_diff <- c(%s)
      
      errstrc <- %d
      
      caddisease <- "%s"
      
      series_cases <- c(%s)
      
      datetype <- "%s"
      
      # Parameter values
      %s
      
      # Lower bound of parameters
      %s
      
      # Upper bound of parameters
      %s
      
      # Prior distribution select for normal or neg. binomial errors
      %s
      
      vars.init <- %d
      
      %s
      
      niter <- %d
      
      num_chain = %d
    ',
                               calibrationperiod_values,
                               input$forecastinghorizon,
                               vars_values,
                               params_values,
                               paste(odeEquation$system, collapse = "\n"),
                               paramsFixed_values, 
                               result, 
                               fitting_index_values$fit,
                               na.omit(vector.fit.diff.state$state), 
                               errorStructureNumber(), 
                               input$caddisease, 
                               series_cases_values, 
                               date_type_temp$temporal,
                               params_prior_assignments,  
                               params_LB_assignments,
                               params_UB_assignments,
                               params_prior_dist_assignments ,
                               as.numeric(vars.init.temp()),
                               Ic.Input.Temp, 
                               input$niter,
                               input$num_chain
                             
                               )

#------------------------------------------------------------------------------#
# Determining if the users want to save the options file -----------------------
#------------------------------------------------------------------------------#
# About: This section determines if the users wants to save the options file or#
# not before proceeded with running the rest of the dashboard. If selected,    #
# the corresponding `options.R` file will save to the directory of their       #
# choosing. They will then be able to proceed with fitting the model.          #
#------------------------------------------------------------------------------#
    
    ###############################
    # Creating the pop-up message #
    ###############################
    isolate({showModal(modalDialog(
      
      title = "Prepared files.",
      
      h4("Do you want to download the produced 'options.m' file?"), 
      
      # Buttons in a flex row
      div(style = "text-align: center;", downloadButton("downloadOptionsFile", "Download Options File")),
      
      h4("Do you want to download the produced R-STAN file?"), 
      
      div(style = "text-align: center;", downloadButton("downloadSTANfile", "Download Stan File")), 
      
      h4("Proceed with fitting the model?"),
      
      div(
        style = "display: flex; gap: 20px; justify-content: center; margin-bottom: 15px;",
        actionButton("yesPROCEED", label = "Yes",
                     style = "flex: 1; padding: 10px; min-width: 100px;"),
        actionButton("NoPROCEED", label = "No",
                     style = "flex: 1; padding: 10px; min-width: 100px;")
      ),
      
      easyClose = FALSE
      
    ))})
    
    
    ###############################################################
    # Creating the download back-end to save the `options.R` file #
    ###############################################################
    output$downloadOptionsFile <- downloadHandler(
      
      ####################################
      # Function to create the file-name #
      ####################################
      filename = function() {
        
        # File name 
        paste("options.R")
        
      },
      
      #############################
      # Function to save the file #
      #############################
      content = function(file) {
        
        # Saving the file
        writeLines(options_content, con = file)
        
      }
      
    ) # End of download button     
    
    ##################################
    # Saving the `temp` options file #
    ##################################
    writeLines(options_content, con = options_file)
    
    
    # Storing in reactive value
    optionsFileReactive$file <- options_file
    
#------------------------------------------------------------------------------#
# Obtaining the STAN ODE file --------------------------------------------------
#------------------------------------------------------------------------------#
# This section creates the ODE file if selected by the user, and then provides #
# the user with the opportunity to download the file. Additionally, users can  #
# then select to proceed with model fitting and forecasting.                   #
#------------------------------------------------------------------------------#
    
    ##########################
    # Creating the STAN file #
    ##########################
    
    # Generating the STAN code 
    stan_code <- generate_stan_file(optionsFile = options_file)
    
    ###############################################################
    # Creating the download back-end to save the `options.R` file #
    ###############################################################
    output$downloadSTANfile <- downloadHandler(
      
      ####################################
      # Function to create the file-name #
      ####################################
      filename = function() {
        
        # File name 
        paste("ode_model_downloaded.stan")
        
      },
      
      #############################
      # Function to save the file #
      #############################
      content = function(file) {
        
        # Saving the STAN file 
        writeLines(stan_code, file)
        
      }
      
    ) # End of download button 
    
    }
      
   } # End of 'else' checking for options loaded 
    
  })
  
    
#------------------------------------------------------------------------------#
# Fitting the model ------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section downloads fits the Bayesian model based upon what the    #
# users specify in the other parameters needed to run the dashboard.           #
#------------------------------------------------------------------------------#
  
  #############################################
  # Reactive value to store model fit results #
  #############################################
  modelFitResults <- reactiveValues()
    
  #######################################
  # Running only if the user says "YES" #
  #######################################
  observeEvent(input$yesPROCEED, {
      
    # Requiring the options file
    req(optionsFileReactive$file)
      
    ###########################
    # Trying to fit the model #
    ###########################
    tryCatch({
        
     isolate({shinyalert::shinyalert("", text = paste0("The model fitting process has begun...."))})
        
     # Fitting the model 
     isolate({runMCMCResults <- run_MCMC(outputFile = optionsFileReactive$file,
                                         data.temp = listTimeseries$data)})
       
    ###########################
    # Runs if an error occurs #
    ###########################
    }, error = function(c){
  
      shinyalert::shinyalert(title = "Error!", text = "Somthing went wrong with fitting the model. Please review your user selections and try again.", type = "error")
  
      optionsFileReactive$file <- NULL
        
    }) # End of `tryCatch`
      
    ###########################################
    # Handling a known error in model fitting #
    ###########################################
    if(is.character(runMCMCResults)){
        
      shinyalert(title = "Error!", text =  paste(runMCMCResults), type = "error")
      
      modelFitResults$results <- NULL
        
    }else if(!is.character(runMCMCResults)){
      
      # Saving the results in the reactive value
      modelFitResults$results <- runMCMCResults
      
    }
    
  }) # End of `observeEvent`
  
#------------------------------------------------------------------------------#
# Sampling from the model fits -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section runs if the model fitting worked. It conducts all of the #
# sampling based upon the calibration periods selected by the user, model fit, #
# and options file.                                                            #
#------------------------------------------------------------------------------#
  
  ##################################
  # Indicator to move to next step #
  ##################################
  samplingIndicator <- reactiveVal(0)

  ####################################
  # Reactive value to store sampling #
  ####################################
  samplingReactiveValue <- reactiveValues()
  
  # Reactive value storing model fit
  reactiveModelFit <- reactive({modelFitResults$results})
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observeEvent(reactiveModelFit(), {
    
    # List to store the fitting output for each calibration period 
    fitting.output <- list()

    # Requiring the earlier model fitting results 
    req(modelFitResults$results)
    
    # Requiring the options file
    req(optionsFileReactive$file)

    #######################################
    # Looping through calibration periods #
    #######################################
    for(i in seq_along(input$calibrationperiods)) {
      
      # Indexed calibration period 
      index.cali <- input$calibrationperiods[i]
      
      # Start alert 
      shinyalert::shinyalert("", text = paste0("Sampling for the ",i, " (", index.cali, " time points) calibration period has begun."))
      
      ###############################
      # Set up for storing messages #
      ###############################
      
      # Storing messages 
      messages_and_errors <- NULL
      
      # Storing the results 
      result <- NULL
      
      ####################################################
      # Capturing the output and any associated messages #
      ####################################################
      messages_and_errors <- capture.output({
        
        ####################################################
        # Saving the results of the model sampling process #
        ####################################################
        result <- tryCatch({
          
          # Allowing for saving of messaging with code still running 
          withCallingHandlers({
            
            #####################
            # Starting sampling #
            #####################
            sampling(modelFitResults$results[[i]]$model,
                     data = modelFitResults$results[[i]]$data,
                     iter = modelFitResults$results[[i]]$iter,
                     chains = modelFitResults$results[[i]]$chain,
                     seed = 0)
            
          #################################
          # Catching any warning messages #
          #################################
          }, warning = function(w) {
            
            # Message to print 
            shinyalert(paste0("Stan Warning for the ", i, " calibration period (", index.cali, " time points):", w$message), type = "warning")
            
          })
          
          ########################################
          # Catching any fatal, R related errors #
          ########################################
          }, error = function(e) {
            
            # Message to occur 
            message(paste("Stan Error for the ", i, " calibration period (", index.cali, " time points):", e$message))
            
            # Restarting the sampling 
            samplingIndicator(0)
            
            # Returning no output
            return(NULL) 
            
        })
        
      }, type = "output")
      
      # Store both the fit and the log/output
      fitting.output[[i]] <- result
    
      #######################
      # Checking for errors #
      #######################
      
      # Combine all known error indicators
      error_lines <- messages_and_errors[grepl("Exception|[Ee]rror", messages_and_errors)]
      
      # Sharing the error 
      if (length(error_lines) > 0) {
        
        shinyalert::shinyalert(
          title = paste("Stan issue in calibration", index.cali),
          text = paste(error_lines, collapse = "\n"),
          type = "error"
        )
        
        #########################
        # Resetting the options #
        #########################

        # Resetting the ODE equations
        odeEquation$system <- NULL

        # Resetting the reactive list
        samplingReactiveValue$values <- NULL

        # Resetting model fit
        modelFitResults$results <- NULL
        
        # Clearing out the fitting output
        fitting.output <- NULL
        
        # Clearing out the composite equations
        composite.variable.list$list <- NULL
        
        # Sampling indicator  
        samplingIndicator(0)
        
        
      }else{
        
        # Assigning to the reactive value
        samplingReactiveValue$values <- fitting.output

        # Sampling indicator  
        samplingIndicator(1)
      
        }
      
    }
  
  })
  
  
#------------------------------------------------------------------------------#
# Finishing the model fitting process ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section finishing the model fitting process. It takes in the     #
# data, options file and results of sampling to then return the model fits.    #
# Next, the results of this section are used to obtain output.                 #
#------------------------------------------------------------------------------#
  
  ####################################################
  # Reactive values to store final model fit results #
  ####################################################
  finalMCMC <- reactiveValues()
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observeEvent(samplingIndicator(), {
    
    # Running only if the sampling indicator is 1 
    if(samplingIndicator() == 1){
    
      # Requiring the options file
      req(optionsFileReactive$file)
      
      # Requiring the results of sampling 
      req(odeEquation$system)
    
      ###############################
      # Finishing fitting the model #
      ###############################
      tryCatch({
          
      final.output.MCMC <- run_MCMC2(outputFile = optionsFileReactive$file,
                                     data.temp = listTimeseries$data, 
                                     finalModlLIST = samplingReactiveValue$values)
      
      # Saving the results in a reactive value
      finalMCMC$results <- final.output.MCMC
        
      # End alert
      shinyalert::shinyalert("", text = paste0("The model fitting process has finished."))
      
      ###########################
      # Runs if an error occurs #
      ###########################
      }, error = function(c){
          
        # Error alert 
        shinyalert::shinyalert(title = "Error!", text = "Somthing went wrong with fitting the model. Please review your user selections and try again.", type = "error")
          
        # Clearing out the reactive value
        finalMCMC$results <- NULL
        
      }) # End of `tryCatch`
      
    }

  })
      
#------------------------------------------------------------------------------#
# Obtaining output from the model fits -----------------------------------------
#------------------------------------------------------------------------------#
# About: This section takes the model fit, options file, and input data to     #
# produce all forecasts, data and figure outputs for each of the selected      #
# calibration periods. It then saves the output to the reactive value for      #
# later use.                                                                   #
#------------------------------------------------------------------------------#
  
  ##########################################
  # Reactive value to store plots and data #
  ##########################################
  allOutput <- reactiveValues()
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    # Running only if the sampling indicator is 1 
    if(samplingIndicator() == 1){
    
    # Requiring the options file
    req(optionsFileReactive$file)
    
    # Requiring the final model fits 
    req(finalMCMC$results)
    
    ###############################
    # Trying to obtain the output #
    ###############################
    tryCatch({
      
      # Obtaining the output
      output.Final <- run_analyzeResults(optionsFile = optionsFileReactive$file,
                                         MCMCOutput = finalMCMC$results,
                                         data.temp = listTimeseries$data)
        
      # Saving the results in a reactive value
      allOutput$everything <- output.Final
      
      # Setting the sampling indicator back to zero
      samplingIndicator(0)
    
      ###########################
      # Runs if an error occurs #
      ###########################
      }, error = function(c){
  
        shinyalert::shinyalert(title = "Error!", text = "Somthing went wrong with obtaining output. Please review your user selections and try again.", type = "error")
  
        allOutput$everything <- NULL
        
    }) # End of `tryCatch`
    
    }
  
  }) 
      
#------------------------------------------------------------------------------#
# Creating the calibration period filter: Figures ------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the calibration period filter for the figures    #
# box. It allows the users to filter the plots they are seeing by calibration  #
# period. The available calibration periods correspond to user selection in    #
# the side panel.                                                              #
#------------------------------------------------------------------------------#
  
  ############################################################
  # Creating the UI option for filtering calibration periods #
  ############################################################
  output$select.calibration.period <- renderUI({

    # Requiring the output information
    req(allOutput$everything)

    # Requiring the calibration period(s) to be selected
    req(input$calibrationperiods)

    #############################
    # Creating the picker input #
    #############################
    pickerInput("selectCaliFig", "Filter by Calibration Period: ", choices = c(input$calibrationperiods))
    
  })
  

#------------------------------------------------------------------------------#
# Creating the download button: Figures ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the download button for the figures box. It      #
# allows users to download all figures produced by the running of the model.   #
#------------------------------------------------------------------------------#

  ##################################################
  # Creating the UI option for downloading figures #
  ##################################################
  output$download.figures <- renderUI({

    # Requiring the output information
    req(allOutput$everything)

    ################################
    # Creating the download button #
    ################################
    actionButton("downloadFigures", "Download Figure", icon = icon("download"))

  })

#------------------------------------------------------------------------------#
# Creating the download options for the figures --------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the download options for the figures. Users can  #
# decide the DPI, width, height, units, and type of figure to download. The    #
# features specified will apply to all figures included in the 'zip'.          #
#------------------------------------------------------------------------------#

  #################################
  # Setting Figure Specifications #
  #################################
  observeEvent(input$downloadFigures, {

    ################################
    # Figure specification options #
    ################################
    isolate({

      showModal(modalDialog(

        title = "Figure Specifications",
        numericInput("dpi", "Figure DPI:", value = 900),
        numericInput("width", "Figure Width:", value = 9),
        numericInput('height', 'Figure Height:', value = 5),
        pickerInput("units", label = "Unit of Measurement:", choices = c("in", "cm", "mm", "px")),
        pickerInput("extFig", label = "Figure Type:", choices = c("png", "eps", "pdf", "tiff", "jpeg", "svg")),
        downloadButton("downloadFigsButton", "Download All Figures"),
        easyClose = TRUE

      ))

    }) # End of 'isolate' statement

  }) # End of 'observeEvent' statement

#------------------------------------------------------------------------------#
# Creating the plot selector ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the plot selector for the figures box. It allows #
# all options to show when at least one composite equation is included. If     #
# a user does not indicate a composite equation, the user will not see the     #
# composite equation option.                                                   #
#------------------------------------------------------------------------------#

  output$selectFigureALL <- renderUI({

    # Requiring the output
    req(allOutput$everything)

    ###########################################################################
    # Showing the `Composite Histogram` if composite information is available #
    ###########################################################################
    if(length(allOutput$everything[[1]][[2]]) > 0){

      # Creating the picker input
      return(pickerInput("selectFigure", "Select a Figure", choices = c("Parameter Histograms", "Composite Histograms", "Trace Plot", "Posterior Plots", "Forecasts")))

    #########################################
    # Not showing the `Composite Histogram` #
    #########################################
    }else{

      # Creating the picker input
      return(pickerInput("selectFigure", "Select a Figure", choices = c("Parameter Histograms", "Trace Plot", "Posterior Plots", "Forecasts")))

    }

  })

#------------------------------------------------------------------------------#
# Creating the arrows ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the arrows for cycling through figures. The      #
# figures in which multiple are included, will have the arrows to cycle        #
# through the images. If only one image is included, then the arrows will not  #
# show.                                                                        #
#------------------------------------------------------------------------------#

  ###############################
  # Creating the previous arrow #
  ###############################
  output$arrows.figures.previous <- renderUI({

    # Requiring the output information
    req(allOutput$everything)

    # Requiring a plot be selected
    req(input$selectFigure)

    #####################
    # Showing the arrow #
    #####################
    if(input$selectFigure == "Parameter Histograms" || input$selectFigure == "Composite Histograms"){

      return(actionButton(inputId = "PreviousFigure", label = icon("arrow-left")))

    #########################
    # Not showing the arrow #
    #########################
    }else{

      return(NULL)

    }

  })

  ###########################
  # Creating the Next arrow #
  ###########################
  output$arrows.figures.next <- renderUI({

    # Requiring the output information
    req(allOutput$everything)

    # Requiring a plot be selected
    req(input$selectFigure)

    #####################
    # Showing the arrow #
    #####################
    if(input$selectFigure == "Parameter Histograms" || input$selectFigure == "Composite Histograms"){

      return(actionButton(inputId = "NextFigure", label = icon("arrow-right")))

    #########################
    # Not showing the arrow #
    #########################
    }else{

      return(NULL)

    }

  })

#------------------------------------------------------------------------------#
# Create the nested select input -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the nested select inputs for showing a specific  #
# calibration period, and plots.                                               #
#------------------------------------------------------------------------------#

  #################################
  # Reactive value to store plots #
  #################################
  filtered.plots <- reactiveValues()

  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({

    # Requiring a plot be selected
    req(input$selectFigure)

    # Requiring the output information
    req(allOutput$everything)

    #############################
    # Calling the select option #
    #############################
    ui.tempss <- input$selectFigure

    ##################################
    # Determining which list to pull #
    ##################################
    figures.to.pull <- switch(ui.tempss,

                              "Parameter Histograms" = "plot.list.params",
                              "Forecasts" = "forecasts.fits.plot",
                              "Composite Histograms" = "plot.list.composite",
                              "Trace Plot" = "plot.list.trace",
                              "Posterior Plots" = "plot.list.posterior"

    )

    ########################
    # Sub-setting the data #
    ########################
    plot.to.show <- allOutput$everything[[input$selectCaliFig]][[figures.to.pull]]
    
    # Adding to the reactive value
    filtered.plots$plot <- plot.to.show

  })

#------------------------------------------------------------------------------#
# Setting up the forward and backwards arrows for available figures ------------
#------------------------------------------------------------------------------#
# About: This section sets up the forward and backwards buttons to go through  #
# the selected figures.                                                        #
#------------------------------------------------------------------------------#

  ##########################################################
  # Creating the reactive value to control the arrow index #
  ##########################################################
  figure_index_arrow <- reactiveVal(1)

  #################################################
  # Going backwards if the previous button is hit #
  #################################################
  observeEvent(input$PreviousFigure, {

    # Isolating the action to only when the button is clicked
    isolate({

      # Running if the current index is greater than one
      if(figure_index_arrow() > 1){

        # Changing the index of the reactive value
        figure_index_arrow(max(figure_index_arrow() - 1))

      }

    }) # End of 'isolate' statement

  }) # End of 'observeEvent' statement

  ############################################
  # Going forwards if the next button is hit #
  ############################################
  observeEvent(input$NextFigure, {

    # Isolating the action to only when the button is clicked
    isolate({

      # Run if the current index is less than the length of the list
      if(figure_index_arrow() < length(filtered.plots$plot)) {

        # Changing the index of the reactive value
        figure_index_arrow(min(figure_index_arrow() + 1))

      }

    }) # End of 'isolate' statement

  }) # End of 'observeEvent' statement

  #########################################################
  # Fixes the index when the figures are filtered by type #
  #########################################################
  observeEvent(input$selectFigure, {

    # Isolate the behavior to when the button is clicked
    isolate({

      figure_index_arrow(1)

    }) # End of isolate

  }) # End of 'observeEvent'

  #######################################################################
  # Fixes the index when the figures are filtered by calibration period #
  #######################################################################
  observeEvent(input$selectCaliFig, {

    # Isolate the behavior to when the button is clicked
    isolate({

      figure_index_arrow(1)

    }) # End of isolate

  }) # End of 'observeEvent'


#------------------------------------------------------------------------------#
# Preparing the final list of figures to render to the main dashboard ----------
#------------------------------------------------------------------------------#
# About: This section will prepare the final list of figures to render,        #
# including incorporating the forward and backwards arrows when needed.        #
#------------------------------------------------------------------------------#

  output$allOutputFIGS <- renderPlot({

    ####################
    # Requiring output #
    ####################
    req(input$selectFigure)
    
    req(filtered.plots$plot)
    
    #######################
    # Creating the output #
    #######################
    if(input$selectFigure == "Parameter Histograms" || input$selectFigure == "Composite Histograms"){

      return(filtered.plots$plot[[figure_index_arrow()]])

    }else{

      return(filtered.plots$plot)

    }

  })

#------------------------------------------------------------------------------#
# Creating the full list of figures for exporting ------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the full list of figures to be exported when the #
# user selects the 'Download' button. All of the figures will be downloaded as #
# a 'zip' file, and to the directory of the users choosing.                    #
#------------------------------------------------------------------------------#

  #############################################
  # Reactive value to store the plots to save #
  #############################################
  plots.to.save <- reactiveValues()

  ###########################################
  # Determining which plots should be saved #
  ###########################################
  observe({

    # Requiring output
    req(allOutput$everything)

    # Vector to store plots
    plot.to.save.list <- c()

    # List of figure names
    plotsList <- c("plot.list.params", "forecasts.fits.plot",
                   "plot.list.composite", "plot.list.trace",
                   "plot.list.posterior")

    #######################################
    # Looping through calibration periods #
    #######################################
    for(i in 1:length(allOutput$everything)){

      # Calibration period name
      calibration.period <- names(allOutput$everything[i])

      # Sub-setting the data
      calibration.filtered <- allOutput$everything[[i]]
      
      ##############################
      # Looping through plot types #
      ##############################
      for(p in 1:length(calibration.filtered)){

        # Sub-setting the plot name
        plot.name.temp <- names(calibration.filtered[p])

        # Skipping to next iteration if data file
        if(!(plot.name.temp %in% c(plotsList))){

          # Skip to next iteraction
          next

        }

        # Base names for files
        base.names.files <- switch(plot.name.temp,
                                   "plot.list.params" = "parameter-plots-",
                                   "forecasts.fits.plot" = "forecast-figures-",
                                   "plot.list.composite" = "composite-plots-",
                                   "plot.list.trace" = "trace-plots-",
                                   "plot.list.posterior" = "posterior-plots-"
        )

        ###############################################
        # Determining if an additional loop is needed #
        ###############################################
        if(plot.name.temp %in% c("plot.list.params", "plot.list.composite")){

          # Creating the needed loop
          for(a in 1:length(allOutput$everything[[i]][[plot.name.temp]])){

            # Sub-setting the plot name
            plot.name.sub.temp <- names(allOutput$everything[[i]][[plot.name.temp]][a])

            # Creating the new name
            new.plot.name <- paste0(base.names.files, plot.name.sub.temp, "-calibration-", calibration.period)

            # Adding the plot to the master list
            plot.to.save.list <- c(allOutput$everything[[i]][[plot.name.temp]][a], plot.to.save.list)

            # Adding the name
            names(plot.to.save.list)[[1]] <- c(new.plot.name)

          }

          ################################
          # No additional loop is needed #
          ################################
          }else{

            # Creating the new name
            new.plot.name <- paste0(base.names.files, "calibration-", calibration.period)

            # Adding the plot to the master list
            plot.to.save.list <- c(calibration.filtered[p], plot.to.save.list)

            # Adding the name
            names(plot.to.save.list)[[1]] <- c(new.plot.name)

          } # End of `else` creating loop

        } # End of plot-type loop

      } # End of calibration period loop

    ######################################
    # Reactive value storing the figures #
    ######################################
    plots.to.save$plots <- plot.to.save.list

  })
  
#------------------------------------------------------------------------------#
# Downloading the figures ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section provides the functionality to the download button. It    #
# allows users to save the figures within a 'zip' to the directory of their    #
# choosing.                                                                    #
#------------------------------------------------------------------------------#
  
   ###############################################
   # Creating the option to download the figures #
   ###############################################
   output$downloadFigsButton <- downloadHandler(

     #####################
     # File name for ZIP #
     #####################
     filename = function(){

       paste("BayesianFitForecast-Available-Figures.zip", sep = "")

     },

     ############################################
     # Determining what should be in the folder #
     ############################################
     content = function(file){

       # Removing the message
       removeModal()

       # Creating a temp directory for files
       temp_directory <- file.path(tempdir(), as.integer(Sys.time()))

       # Physically creating the directory
       dir.create(temp_directory)

       # Saving the ggplots
       for(plot_name in names(plots.to.save$plots)) {

         ###########################
         # Plot object to be saved #
         ###########################
         plot_obj <- plots.to.save$plots[[plot_name]]

         ###############################################
         # Handling the graphics pan: Pan Needs Closed #
         ###############################################

         # Check if a device is open
         if (dev.cur() != 1) {

           dev.off()

         }

         #################################
         # Code to run if plot was found #
         #################################
         if (!is.null(plot_obj)) {

           # File name
           file_name <- glue("{plot_name}.{input$extFig}")

           ###############################
           # Handling Non-GGPLOT Objects #
           ###############################
           if(length(plot_obj) == 3){

             ####################
             # Saving as a TIFF #
             ####################
             if(input$extFig == "tiff"){

               tiff(filename = file.path(temp_directory, file_name),
                    width = input$width,
                    height = input$height,
                    res = input$dpi,
                    units = input$units,
                    compression = "lzw")

               # Calling the plot
               print(plot_obj)

               # Closing the window
               dev.off()

             ###################
             # Saving as a PNG #
             ###################
             }else if(input$extFig == "png"){

               png(filename = file.path(temp_directory, file_name),
                   width = input$width,
                   height = input$height,
                   units = input$units,
                   res = input$dpi)

               # Calling the plot
               print(plot_obj)

               # Closing the window
               dev.off()

             ####################
             # Saving as a JPEG #
             ####################
             }else if(input$extFig == "jpeg") {

               jpeg(filename = file.path(temp_directory, file_name),
                    width = input$width,
                    height = input$height,
                    units = input$units,
                    res = input$dpi)

               # Calling the plot
               print(plot_obj)

               # Closing the window
               dev.off()

             ###################
             # Saving as a PDF #
             ###################
             }else if(input$extFig == "pdf") {

               pdf(file = file.path(temp_directory, file_name),
                   width = input$width,
                   height = input$height)

               # Calling the plot
               print(plot_obj)

               # Closing the window
               dev.off()

             ###################
             # Saving as a SVG #
             ###################
             }else if(input$extFig == "svg"){

               svg(filename = file.path(temp_directory, file_name),
                   width = input$width,
                   height = input$height)

               # Calling the plot
               print(plot_obj)

               # Closing the window
               dev.off()

             #################
             # Saving as EPS #
             #################
             }else if(input$extFig == "eps"){

               postscript(file = file.path(temp_directory, file_name),
                          width = input$width,
                          height = input$height,
                          horizontal = FALSE,
                          onefile = FALSE,
                          paper = "special")

               # Calling the plot
               print(plot_obj)

               # Closing the window
               dev.off()

             }

           ###########################
           # Handling GGPLOT Objects #
           ###########################
           }else{

             ###############################################
             # Running with compression if using a '.tiff' #
             ###############################################
             if(input$extFig == 'tiff'){

               # Saving the file
               ggsave(file.path(temp_directory, file_name),
                      plot = plot_obj,
                      dpi = input$dpi,
                      width = input$width,
                      height = input$height,
                      units = input$units,
                      device = input$extFig,
                      compression = "lzw")

             ######################################################
             # Running without compression if not using a '.tiff' #
             ######################################################
             }else{

               # Saving the file
               ggsave(file.path(temp_directory, file_name),
                      plot = plot_obj,
                      dpi = input$dpi,
                      width = input$width,
                      height = input$height,
                      device = input$extFig,
                      units = input$units)
             }

           }
           
         }
         
       }
         
         #####################
         # Create a zip file #
         #####################
         zip::zip(
           zipfile = file,
           files = dir(temp_directory),
           root = temp_directory
         )
         
       },
       
       contentType = "application/zip"
       
   )
  
#------------------------------------------------------------------------------#
# Creating the download button: Data -------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the download button for the data box. It         #
# allows users to download all data produced by the running of the model.      #
#------------------------------------------------------------------------------#

  ##################################################
  # Creating the UI option for downloading figures #
  ##################################################
  output$download.data <- renderUI({

    # Requiring the output information
    req(allOutput$everything)

    ################################
    # Creating the download button #
    ################################
    downloadButton("download_data", "Download Available Data")

  })

#------------------------------------------------------------------------------#
# Creating the data selector ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the date selector for the data box.              #
#------------------------------------------------------------------------------#

  output$selectDatall <- renderUI({

    # Requiring the output
    req(allOutput$everything)

    ##############################
    # Creating the `pickerInput` #
    ##############################
    pickerInput("selectData", "Select a Data Set", choices = c("Forecasts", "Parameters", "Performance Metrics"))

  })

#------------------------------------------------------------------------------#
# Creating the data sets -------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section combines the calibration-specific data sets, therefore,  #
# users can see all the results in one place for each type of data set. The    #
# data is then made available for download.                                    #
#------------------------------------------------------------------------------#

  ############################################
  # Reactive value to store the data to save #
  ############################################
  data.to.save.final <- reactiveValues()
  
  ##########################################
  # Determining which data should be saved #
  ##########################################
  observe({
    
    # Requiring output
    req(allOutput$everything)
    
    # Vector to store data
    data.to.save <- c()
    
    # Performance metrics to save
    performance.metrics.save <- c()
    
    # Parameters to save
    parameters.save <- c()
    
    # List of figure names 
    plotsList <- c("plot.list.params", "forecasts.fits.plot",
                   "plot.list.composite", "plot.list.trace",
                   "plot.list.posterior", "parameter.values")
    
    ########################
    # Looping through list #
    ########################
    for(i in 1:length(allOutput$everything)){
        
      # Calibration period name
      calibration.period <- names(allOutput$everything[i])
        
      # Sub-setting the data
      calibration.filtered <- allOutput$everything[[i]]
      
      ##############################
      # Looping through data types #
      ##############################
      for(p in 1:length(calibration.filtered)){
          
        # Sub-setting the data name
        data.name.temp <- names(calibration.filtered[p])
          
        # Skipping to next iteration if data file
        if(data.name.temp %in% c(plotsList)){
            
          # Skip to next iteration
          next
            
        }
        
        ############################
        # Performance Metrics Data #
        ############################
        if(data.name.temp == "performance.metrics"){
            
          # Reading in the data 
          performance.metrics.temp <- calibration.filtered[[p]]
            
          # Adding it to the list 
          performance.metrics.save <- rbind(performance.metrics.temp, performance.metrics.save)
            
        #########################
        # Parameter values Data #
        #########################
        }else if(data.name.temp == "add.parameter.values"){
            
          # Reading in the data 
          parameters.temp <- calibration.filtered[[p]]
            
          # Adding it to the list 
          parameters.save <- rbind(parameters.temp, parameters.save)
            
          #################
          # Forecast fits #
          #################
          }else{
            
          # Reading in the data 
          forecast.file <- calibration.filtered[p]
            
          # Name for the file in the list
          forecast.file.name <- paste0("forecast-fits-calibration-", calibration.period)
            
          # Adding the big list
          data.to.save <- c(forecast.file, data.to.save)
            
          # Adding the name
          names(data.to.save)[[1]] <- c(forecast.file.name)
            
        }
          
      } # End of data filter 
        
    } # End of calibration period loop
    
    ##############################################
    # Adding the parameters data set to the list #
    ##############################################
      
    # Adding the name
    data.to.save[["parameters"]] <- parameters.save
      
    #######################################################
    # Adding the performance metrics data set to the list #
    #######################################################
      
    # Cleaning of the performance metrics list
    performance.metrics.save <- performance.metrics.save %>%
      select(where(~ !all(is.na(.))))
      
    # Adding the name
    data.to.save[["performance-metrics"]] <- performance.metrics.save
      
    ############################################
    # Adding the data list to a reactive value #
    ############################################
    data.to.save.final$data <- data.to.save
      
  }) # End of `observe`
    
    
#------------------------------------------------------------------------------#
# Filtering the data sets ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section filters the list of data sets by type to then export     #
# them to the main screen. If working with the performance metrics files or    #
# the parameter files, the only filtering is to select the one frame. If the   #
# forecast file option is selected, all files containing the phrase 'fit'      #
# will be subsetted.                                                          #
#------------------------------------------------------------------------------#
  
  ##################################################
  # Reactive value to store the resulting data set #
  ##################################################
  fitlered.data.sets <- reactiveValues()
  
  # To store titles
  filtered.data.title <- reactiveValues()
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    # Requiring the data
    req(data.to.save.final$data)
    
    # Requiring the selected data input
    req(input$selectData)
    
    #############################
    # Calling the select option #
    #############################
    ui.tempss <- input$selectData
    
    ##################################
    # Determining which list to pull #
    ##################################
    data.to.pull <- switch(ui.tempss,
                              
                              "Forecasts" = "FORECASTFILLER",
                              "Parameters" = "parameters",
                              "Performance Metrics" = "performance-metrics"
                              
    )

    ######################
    # Filtering the data #
    ######################
    if(data.to.pull == "FORECASTFILLER"){
      
      # Sub-setting the data 
      forecast.list <-  data.to.save.final$data[grepl("forecast-fits", names(data.to.save.final$data))]
      
      # Title list 
      forecast.title.list <- names(data.to.save.final$data[grepl("forecast-fits", names(data.to.save.final$data))])
      
    }else{
      
      # Sub-setting the data 
      forecast.list <-  data.to.save.final$data[[data.to.pull]]
      
      # Title list 
      forecast.title.list <- NULL
      
    }

    #######################################
    # Saving the data in a reactive value #
    #######################################
    fitlered.data.sets$data <- forecast.list
    
    # Adding the titles 
    filtered.data.title$text <- forecast.title.list

  })
  
#------------------------------------------------------------------------------#
# Creating the arrows ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the arrows for cycling through the forecast      #
# files. If either of the other two data sets are selected by the user, then   #
# the arrows will not appear.                                                  #
#------------------------------------------------------------------------------#
  
  ###############################
  # Creating the previous arrow #
  ###############################
  output$arrows.data.previous <- renderUI({
    
    # Requiring the output information
    req(allOutput$everything)
    
    # Requiring a plot be selected
    req(input$selectData)
    
    #####################
    # Showing the arrow #
    #####################
    if(input$selectData == "Forecasts"){
      
      return(actionButton(inputId = "PreviousData", label = icon("arrow-left")))
      
    #########################
    # Not showing the arrow #
    #########################
    }else{
      
      return(NULL)
      
    }
    
  })
  
  ###########################
  # Creating the Next arrow #
  ###########################
  output$arrows.data.next <- renderUI({
    
    # Requiring the output information
    req(allOutput$everything)
    
    # Requiring a plot be selected
    req(input$selectData)
    
    #####################
    # Showing the arrow #
    #####################
    if(input$selectData == "Forecasts"){
      
      return(actionButton(inputId = "NextData", label = icon("arrow-right")))
      
    #########################
    # Not showing the arrow #
    #########################
    }else{
      
      return(NULL)
      
    }
    
  })
  
#------------------------------------------------------------------------------#
# Setting up the foward and backwards arrows for forecast files. ---------------
#------------------------------------------------------------------------------#
# About: This section sets up the foward and backwards buttons to navigate     #
# through the selected figures.                                                #
#------------------------------------------------------------------------------#

  ##########################################################
  # Creating the reactive value to control the arrow index #
  ##########################################################
  data_index_arrow <- reactiveVal(1)
  
  #################################################
  # Going backwards if the previous button is hit #
  #################################################
  observeEvent(input$PreviousData, {
    
    # Isolating the action to only when the button is clicked
    isolate({
      
      # Running if the current index is greater than one
      if(data_index_arrow() > 1){
        
        # Changing the index of the reactive value
        data_index_arrow(max(data_index_arrow() - 1))
        
      }
      
    }) # End of 'isolate' statement
    
  }) # End of 'observeEvent' statement
  
  ############################################
  # Going forwards if the next button is hit #
  ############################################
  observeEvent(input$NextData, {
    
    # Isolating the action to only when the button is clicked
    isolate({
      
      # Run if the current index is less than the length of the list
      if(data_index_arrow() < length(fitlered.data.sets$data)) {
        
        # Changing the index of the reactive value
        data_index_arrow(min(data_index_arrow() + 1))
        
      }
      
    }) # End of 'isolate' statement
    
  }) # End of 'observeEvent' statement
  
  ######################################################
  # Fixes the index when the data are filtered by type #
  ######################################################
  observeEvent(input$selectData, {
    
    # Isolate the behavior to when the button is clicked
    isolate({
      
      data_index_arrow(1)
      
    }) # End of isolate
    
  }) # End of 'observeEvent'
          
#------------------------------------------------------------------------------#
# Rendering the data frame -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the selected data frame to the main body of the  #
# dashboard. If the forecast files are selected, the list is also then         #
# navigable by arrows.                                                         #
#------------------------------------------------------------------------------#
  
  ##########################
  # Rending the data table #
  ##########################
  output$dataTableRender <- renderDataTable({
    
    # Requiring the filtered data
    req(fitlered.data.sets$data)

    # Rendering the data: Forecasts 
    if(input$selectData == "Forecasts"){
      
      return(datatable(as.data.frame(fitlered.data.sets$data[[data_index_arrow()]]),
                       options = list(scrollX = T)))
      
    # Rendering the data: All other files 
    }else{
      
      if(is.null(fitlered.data.sets$data)){
        
        return(NULL)
        
      }else{
        
      return(datatable(as.data.frame(fitlered.data.sets$data),
                       options = list(scrollX = T)))
        
      }
      
    }
    
  }) # End of render statement for the data frame
  
#------------------------------------------------------------------------------#
# Rendering the title ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the title for the forecast files. They are only  #
# rendered if the forecast figures are selected.                               #
#------------------------------------------------------------------------------#
  
  #############################
  # Rendering the text output #
  #############################
  output$forecastDataTitle <- renderText({
    
    # Requiring the filtered data
    req(filtered.data.title$text)
    
    # Rendering the data: Forecasts 
    if(input$selectData == "Forecasts"){
      
      # Formatting the title 
      title_text <- filtered.data.title$text[[data_index_arrow()]]
      calibration <- str_extract(title_text, "calibration-\\d+")
      formatted <- str_replace(calibration, "calibration-(\\d+)", "Calibration: \\1")
      
      return(formatted)
      
    # Rendering the data: All other files 
    }else{
      
      return(NULL)
      
    }
    
  })
  
#------------------------------------------------------------------------------#
# Downloading all available data as a zip --------------------------------------
#------------------------------------------------------------------------------#
# About: This section allows users to 'zip' the files that are loaded for      #
# as part of the BayesianFitForecastCode. Each file will be downloaded as a    #
# '.csv' in the large 'zip' folder.                                            #
#------------------------------------------------------------------------------#
  
  ##############################################
  # Backbone of download button for data files #
  ##############################################
  output$download_data <- downloadHandler(
    
    #####################
    # File name for ZIP #
    #####################
    filename = function(){
          
      paste("BayesianFitForecast-Available-Data.zip", sep = "")
          
    },
        
    ############################################
    # Determining what should be in the folder #
    ############################################
    content = function(file){
          
      # Removing the message
      removeModal()
          
      # Creating a temp directory for files 
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
          
      # Physically creating the directory 
      dir.create(temp_directory)
          
      # Saving the '.csv' files
      for (fileName in names(data.to.save.final$data)) {
            
        # Forecast file 
        file_obj <- data.frame(data.to.save.final$data[[fileName]])
            
        # If forecast file is found 
        if (!is.null(file_obj)) {
              
          # File name 
          file_name <- glue("{fileName}.csv")
              
          # Saving the '.csv'
          write.csv(file_obj, file.path(temp_directory, file_name))
              
        }
            
      }
          
      #####################
      # Create a zip file #
      #####################
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
          
    },
        
    contentType = "application/zip"
        
  ) # End of download handler 

  }
# Run the application
shinyApp(ui = ui, server = server)

