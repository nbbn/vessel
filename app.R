library(shiny)
library(shiny.semantic)
library(leaflet)
library(dplyr)
library(readr)

dataset <- read_csv(
  file = "data/precalculated_dataset.csv",
  col_types = cols(
    ship_type = col_character(),
    ship_name = col_character(),
    lon = col_double(),
    lat = col_double(),
    p_lon = col_double(),
    p_lat = col_double(),
    datetime = col_datetime(format = ""),
    distance = col_double()
  )
)

ui <- semanticPage(title = "Vessel viewer",
                   div(
                     class = "ui stackable grid",
                     div(class = "one column row",
                         div(
                           class = "column",
                           h1("View longest distances of your vessels!")
                         )),
                     div(
                       class = "two column row",
                       div(
                         class = "five wide column",
                         div(
                           class = "ui raised segment",
                           dropdown(
                             id = "type",
                             label = "Type",
                             choices = dataset %>% distinct(ship_type) %>% pull()
                           ),
                           uiOutput("vessel_name_dropdown")
                         ),
                         div(class = "ui raised segment",
                             div(
                               class = "ui message",
                               div(class = "header",
                                   textOutput("note_header")),
                               uiOutput("note_content")
                             ))
                       ),
                       div(class = "eleven wide column",
                           div(class = "ui raised segment",
                               leafletOutput("map")))
                     )
                   ))

server <- function(input, output, session) {
  # for a little bit more convinient usage in tests
  loaded_dataset <- reactiveVal(dataset)
  selected_type <- dropdownServer("type")
  selected_ship <- dropdownServer("name")
  selected_trip <-
    reactive(loaded_dataset() %>% filter(ship_type == selected_type(), ship_name == selected_ship()))
  # hack to shape one-row tibble into two rows expected by leaflet
  # first row with previous localization, and second with next localization
  adjusted_dataset_shape_for_map <- reactive(tibble::tibble(
    long = c(selected_trip()$p_lon, selected_trip()$lon),
    lat = c(selected_trip()$p_lat, selected_trip()$lat)
  ))

  output$vessel_name_dropdown <- renderUI(
    dropdown(
      id = "name",
      label = "Ship Name",
      choices = loaded_dataset() %>%
        filter(ship_type == selected_type()) %>%
        distinct(ship_name) %>%
        pull()
    )
  )

  output$note_header <-
    renderText(glue::glue(
      "{selected_trip()$ship_name } ({ selected_trip()$ship_type})"
    ))


  output$note_content <- renderUI(
    prepare_note_content(selected_trip()$distance, selected_trip()$datetime)
  )

  output$map <- renderLeaflet({
    leaflet(adjusted_dataset_shape_for_map()) %>%
      addTiles() %>%
      addMarkers( ~ long, ~ lat) %>%
      addPolylines(lng = ~ long,
                   lat = ~ lat)
  })
  
  
  prepare_note_content <- function(distance, datetime) {
    tagList(p(
      glue::glue(
        "Moved distance: { distance %>% round(digits=2) }m"
      ),
      br(),
      glue::glue("Move ended at: { datetime }")
    ))
  }
}

shinyApp(ui = ui, server = server)
