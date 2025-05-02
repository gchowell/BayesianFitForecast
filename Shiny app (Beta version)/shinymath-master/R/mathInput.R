#' Mathematical Input
#'
#' @param inputId The `input` slot that will be used to access the value.
#' @param label Display label for the control, or `NULL` for no label.
#' @param width The width of the input, e.g. `'400px'`, or `'100%'`;
#'
#' @return A mathematical input control that can be added to an UI definition.
#' @export
mathInput = function(inputId, label, width = NULL) {
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(
        mathInputDeps(),
      )
    ),
    shiny::div(
      class = "form-group shiny-input-container",
      style = if (!is.null(width)) paste0("width: ", shiny::validateCssUnit(width), ";"),
      shiny::tags$label(
        label, class = "control-label",
        class = if (is.null(label)) "shiny-label-null", `for` = inputId
      ),
      shiny::tags$span(
        class = "mathquill-editable form-control",
        style = "height:auto;",
        id = inputId
      )
    )
  )
}

mathInputDeps = function() {
  htmltools::htmlDependency(
    name = "mathInput",
    version = "1.0.0",
    src = c(file = shinymath_file("assets")),
    script = c("mathquill.min.js", "init.js", "binding.js"),
    stylesheet = c("mathquill.css", "shinymath.css")
  )
}
