#' Launch shiny demo that shows shinymath features
#' @export
launch_demo = function() {
  shiny::runApp(shinymath_file("shiny", "features"), display.mode = "normal")
}
