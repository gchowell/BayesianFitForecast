ode_rhs <- function(index, ode_system) {
  # Split the ode_system string into lines
  ode_lines <- strsplit(ode_system, "\n")[[1]]
  
  # Remove empty first line if present
  if (ode_lines[1] == "") {
    ode_lines <- ode_lines[-1]
  }
  
  # Select the line corresponding to the given index (adjusted for 1-based index)
  line <- trimws(ode_lines[index])
  
  # Extract the right-hand side after the '=' or '<-' sign
  rhs <- sub("^.*=\\s*", "", line)
  
  # Remove;
  rhs <- substr(rhs, 1, nchar(rhs) - 1)
  
  # Return the string representation of the right-hand side
  return(rhs)
}