#------------------------------------------------------------------------------#
#                                                                              #
#                          Initial Conditions List                             #
#                                                                              #
#------------------------------------------------------------------------------#
#                          By: Amanda Bleichrodt                               #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This function prepares all of the necessary parameters associated with the   #
# initial conditions. It first updates the initial conditions list to be       #
# properly formatted. Additionally, it changes the vars.init variable if       #
# needed, and updates the parameter fixed status.                              #
#------------------------------------------------------------------------------#

check.IC <- function(ic.list, params.list, equations.list, fixedStatus, initParam){

#------------------------------------------------------------------------------#
# Renaming the inputs ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renames the inputs of the function, creates the empty    #
# vectors to fill with the final IC list, indicators, and updates the params   #
# fixed status.                                                                #
#------------------------------------------------------------------------------#

  ###########################
  # Initial Conditions List #
  ###########################
  ic.temp.list <- ic.list
  
  ###################
  # Parameters List #
  ###################
  params.temp.list <- params.list
  
  ##################
  # Equations list #
  ##################
  equation.temp.list <- equations.list
  
  ######################
  # Vector to store IC #
  ######################
  IC.list <- c()
  
  #############################################
  # Indicator for estimated initial condition #
  #############################################
  vars.init.indicator <- 1
  
  #######################################
  # Updating the parameter fixed status #
  #######################################
  param.fixed.status <- fixedStatus
  
  ############################
  # Initial parameter values #
  ############################
  param.init.temp <- initParam
  
  ###############
  # Error catch #
  ###############
  error.catch <- 0
  
#------------------------------------------------------------------------------#
# Updating the initial conditions list -----------------------------------------
#------------------------------------------------------------------------------#
# About: This section loops through the initial conditions to properly format  #
# them for the 'options' file. Additionally, it updates the vars.input         #
# indicator and fixed params indicator.                                        #
#------------------------------------------------------------------------------#
  
   for(i in 1:length(ic.temp.list)){
    
    ####################################
    # Subsetting the initial condition #
    ####################################
    ic.temp <- ic.temp.list[[i]]
    
    #########################
    # Condition 1: Equation #
    #########################
    
    # Splitting the equation into its pieces
    split.IC <- strsplit(ic.temp, "[+*/\\-]")[[1]]
      
    # Removing empty strings
    split.IC <-  split.IC[split.IC != ""]
      
    # Removing any numeric values
    final.IC <- split.IC[!grepl("^\\d+$", split.IC)]
    
    # Length check
    if(length(final.IC) == 0){
      
      final.IC <- split.IC
      
    }else{final.IC <- final.IC}
      
    # Saving the initial condition as a character 
    expr <- paste0(parse(text = gsub("\\\\", "", ic.temp)))
      
    ################################################################
    # Checking if any parameters are not fixed and in the equation #
    ################################################################
    
    # ICs in the equation 
    ic.in.equation <- final.IC[sapply(final.IC, function(x) {
      any(grepl(paste0("\\b", x, "\\b"), equation.temp.list))
    })]
    
    # Getting their indexes in the list of parameters
    params.ic.index <- sapply(ic.in.equation, function(x) {
      match(x, params.temp.list)
    })
    
    # Pulling the fixed status
    if(length(params.ic.index) == 0){fixed.status.pulled <- NULL}else{fixed.status.pulled <- param.fixed.status[c(params.ic.index)]}
    
    ################################################################
    # Checking if any parameters are fixed and not in the equation #
    ################################################################
    
    # ICs not in the equation 
    ic.not.in.equation <- final.IC[sapply(final.IC, function(x) {
      !any(grepl(paste0("\\b", x, "\\b"), equation.temp.list))
    })]
    
    # Getting their indexes in the list of parameters
    params.ic.not.index <- sapply(ic.not.in.equation, function(x) {
      match(x, params.temp.list)
    })
    
    # Pulling the fixed status
    if(length(params.ic.not.index) == 0 || !is.na(as.numeric(ic.not.in.equation))){fixed.status.pulled1 <- NULL}else{fixed.status.pulled1 <- param.fixed.status[c(params.ic.not.index)]}
    
    ############################
    # Creating the error catch #
    ############################
    if(length(fixed.status.pulled) > 0 & any(fixed.status.pulled == 0)){
      
      # Updating the error catch
      error.catch <- 1
      
      return(error.catch)
      
    ################################################
    # Error if IC is fixed and should be estimated #
    ################################################
    }else if(length(fixed.status.pulled1) > 0 & any(fixed.status.pulled1 == 1)){
      
      # Updating the error catch
      error.catch <- 2
      
      return(error.catch)

    ######################################
    # Replacing parameters when possible #
    ######################################
    }else if(any(sapply(final.IC, function(x) any(grepl(x, equation.temp.list))))){
        
      # Find which variables are used in equation.temp.list
      matching.vars <- final.IC[sapply(final.IC, function(x) any(grepl(x, equation.temp.list)))]
        
      # Replacing parameters with numbers
      for(var in matching.vars) {
        idx <- match(var, params.temp.list)
        if (!is.na(idx)) {
          val <- param.init.temp[idx]
          # Replace exact word (e.g., "beta", not "alphabetabeta")
          expr <- gsub(paste0("\\b", var, "\\b"), val, expr)
        }
      }
        
      ############################################################
      # Determining what to save: Estimating a initial condition #
      ############################################################
      if(length(matching.vars) != length(final.IC)){
          
        # Adding to the IC list 
        IC.list[[i]] <- expr
          
        # Updating the indicator
        vars.init.indicator <- 0
          
      #########################################################
      # Determining what to save: All Parameters are Replaced #
      #########################################################
      }else{
          
        # Solving and adding to the IC list 
        IC.list[[i]] <- eval(parse(text = expr))
          
      }
      
    #######################################################
    # If working with a singe estimated initial condition #
    #######################################################
    }else if(any(sapply(final.IC, function(x) !any(grepl(x, equation.temp.list)))) & 
             length(final.IC) == 1 & 
             length(split.IC[!grepl("^\\d+$", split.IC)]) != 0){
      
      # Adding to the IC list 
      IC.list[[i]] <- expr
      
      # Updating the indicator
      vars.init.indicator <- 0
      
    ########################
    # Only numeric options #
    ########################
    }else{
        
      # Solving and adding to the IC list 
      IC.list[[i]] <- eval(parse(text = expr))
        
    }
      
   }
  
#------------------------------------------------------------------------------#
# Updating the list for output -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the list to be outputted to the main dashboard.  #
# The output includes the final list of initial conditions, the vars.init,     #
# and params.fixed vectors.                                                    #
#------------------------------------------------------------------------------#

  ###################
  # List of outputs #
  ###################
  list.outputs <- list(IC.list, vars.init.indicator)
  
  # Returning the list
  return(list.outputs)
  
  }
   
