shinymath_file = function(...) {
  system.file(..., package = "shinymath", mustWork = TRUE)
}

drop_nulls = function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}
