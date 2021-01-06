
dropdown <- function(id, label, choices) {
  ns <- NS(id)
  tagList(selectInput(ns("dropdown"),
                      label,
                      choices))
}

dropdownServer <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 dropdown <- reactiveVal(0)
                 observeEvent(input$dropdown, {
                   dropdown(input$dropdown)
                 })
                 dropdown
               })
}
