#------------------------------------------------------------------------------#
#                                                                              #
#                  Checking the Input File for Correct Format                  #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This function checks the input file for the correct formatting. It ensures   #
# that the first column begins at zero, and progresses by one at each point    #
# forward. Additionally, it confirms that the headers for the other columns    #
# follow the "cases#" format. If the format is not followed, the code will     #
# return an error.                                                             #
#------------------------------------------------------------------------------#
#                       Author: Amanda Bleichrodt                              #
#------------------------------------------------------------------------------#

error.checking <- function(original.data){
  
#------------------------------------------------------------------------------#
# Renaming the function inputs -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renames the data input variable to ensure no over-       #
# writing occurs.                                                              #
#------------------------------------------------------------------------------#
  
  ##############
  # Input Data #
  ##############
  input.data <- original.data
  
#------------------------------------------------------------------------------#
# Checking the structure of the first column -----------------------------------
#------------------------------------------------------------------------------#
# About: This section checks the structure of the first column of the inputted #
# data. If the column is not sequential, starting at zero, the function will   #
# return an error.                                                             #
#------------------------------------------------------------------------------#
  
  ###########################################
  # Creating the idea sequence for column 1 #
  ###########################################
  ideal.seq <- as.numeric(seq(0, nrow(input.data)-1))
  
  #####################################
  # Pulling the sequence for column 1 #
  #####################################
  
  # Saving the data under a temp name
  temp.data <- input.data
  
  # Renaming the first column 
  colnames(temp.data)[1] <- c("A")
  
  # Pulling the actual sequence of time indexes 
  actual.seq <- temp.data$A
  
  #####################################
  # Comparing the lists: Error Occurs #
  #####################################
  if(!identical(ideal.seq, actual.seq)){
    
    # Returning an error 
    return("ERROR1")
    
#------------------------------------------------------------------------------#
# Checking the remaining column names ------------------------------------------
#------------------------------------------------------------------------------#
# About: If the data passes the first check, this section will then be         #
# triggered. This section will return an error if the remaining columns do not #
# follow the "cases#" pattern. If no error occurs, the function returns        #
# nothing.                                                                     #
#------------------------------------------------------------------------------#
  }else{
    
    #########################################
    # Creating the sequence of column names #
    #########################################
    
    # Number of columns, or case numbers
    num.columns <- ncol(input.data) - 1
    
    # Creating the sequence of names
    ideal.names <- c(paste0("cases", seq(1:num.columns)))
    
    ###################################
    # Pulling the actual column names #
    ###################################
    actual.names <- colnames(input.data)[-1]
    
    #####################################
    # Comparing the lists: Error Occurs #
    #####################################
    if(!identical(ideal.names, actual.names)){
      
      # Returning an error
      return("ERROR2")
    
    #########################
    # No error has occurred #
    #########################
    }else{
      
      return("WORKED")
      
    }
    
  }

} # End of function 
