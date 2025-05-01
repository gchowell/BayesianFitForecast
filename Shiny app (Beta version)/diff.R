diff_var <- function(input_vars) {
  # Convert input_vars to character if it's not already
  input_vars <- as.character(input_vars)
  
  # Initialize an empty vector to store derivative names
  derivative_names <- character(length(input_vars))
  
  # Loop through each element in input_vars and construct derivative names
  for (i in seq_along(input_vars)) {
    derivative_names[i] <- paste0("d", input_vars[i], "_dt")
  }
  
  # Return the vector of derivative names
  return(derivative_names)
}
