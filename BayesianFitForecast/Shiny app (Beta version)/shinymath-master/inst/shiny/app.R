library(shiny)
library(shinymath)

ui = fluidPage(
  h3("mathInput() features"),
  hr(),
  h4("Returns a LaTeX expression"),
  mathInput("input1", "Formula"),
  verbatimTextOutput("output1"),
  hr(),
  h4("Can be updated by pre-defined content or by content in other input"),
  mathInput("input2", "Second formula"),
  textInput("input2_label", "Write a label", "New label"),
  mathInput("input2_math", "Write a new equation"),
  actionButton("update2_1", "Update with x*y", width = "300px"),
  actionButton("update2_2", "Update with arbitrary content", width = "300px"),
  br(), br(),
  verbatimTextOutput("output2"),
  hr(),
  h4("Can be translated to R code or function with latex2r() and latex2fun()"),
  mathInput("input3", "Equation"),
  actionButton("get_r_code", "Get R code", width = "300px"),
  actionButton("get_r_fun", "Get R function", width = "300px"),
  br(), br(),
  verbatimTextOutput("r_code"),
  verbatimTextOutput("r_function")
)

server = function(input, output, session) {
  output$output1 = renderText(input$input1)
  output$output2 = renderText(input$input2)

  observeEvent(input$update2_1, {
    updateMathInput(session, "input2", label = input$input2_label, value = "x \\cdot y")
  })
  observeEvent(input$update2_2, {
    updateMathInput(session, "input2", label = input$input2_label, value = input$input2_math)
  })

  r_fun = eventReactive(input$get_r_fun, {
    result = tryCatch({
      latex2fun(input$input3)
    },
    latex2r.error = function(cnd) {
      showNotification(
        paste("Error when translating to R code:", cnd$message),
        type = "error"
      )
      NULL
    },
    error = function(cnd) {
      showNotification(
        "Unexpected error",
        type = "error"
      )
      NULL
    })
    result
  })

  r_code = eventReactive(input$get_r_code, {
    result = tryCatch({
      latex2r(input$input3)
    },
    latex2r.error = function(cnd) {
      showNotification(
        paste("Error when translating to R function:", cnd$message),
        type = "error"
      )
      NULL
    },
    error = function(cnd) {
      showNotification(
        "Unexpected error",
        type = "error"
      )
      NULL
    })
    result
  })

  output$r_code = renderText({
    r_code()
  })

  output$r_function = renderPrint({
    r_fun()
  })

}

shinyApp(ui, server)
