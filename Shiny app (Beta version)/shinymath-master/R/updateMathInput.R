#' Chabnge the value of a mathematical input on the client
#'
#' @param session The `session` object passed to function given to `shinyServer`.
#' @param inputId The id of the input object.
#' @param label The label to set for the input object.
#' @param value The value to set for the input object.
#'
#' @export
updateMathInput = function(session = shiny::getDefaultReactiveDomain(),
                           inputId, label = NULL, value = NULL) {
  message = drop_nulls(list(label = label, value = value))
  session$sendInputMessage(inputId, message)
}



