context("app")
dataset <- tibble::tibble(
  ship_name = c("aa", "bb", "cc"),
  lon = c(50, 50, 50),
  p_lon = c(55, 45, 50),
  lat = c(18, 19, 20),
  p_lat = c(18,18,18),
  ship_type = c("cargo", "cargo", "fisher"),
  datetime = lubridate::as_datetime(c("2002-06-09 12:45:40","2003-01-29 09:30:40", "2002-09-04 16:45:40")),
  distance = c(55555.5555555, 0.00004, 4)
  )

testServer(app = NULL, expr = {
  expect_true(tibble::is_tibble(dataset))
  loaded_dataset(dataset)
  selected_type("cargo")
  selected_ship("aa")
  expect_equal(selected_trip(), loaded_dataset() %>% filter(row_number() == 1))
  expect_equal(session$getOutput("note_header"), "aa (cargo)")
  expect_equal(
    as.character(session$getOutput("note_content")$html),
    as.character(prepare_note_content(loaded_dataset()$distance[1], loaded_dataset()$datetime[1]))
  )
  selected_type("fisher")
  selected_ship("cc")
  session$flushReact()
  expect_equal(selected_trip(), loaded_dataset() %>% filter(row_number() == 3))
  expect_equal(session$getOutput("note_header"), "cc (fisher)")
  expect_equal(
    as.character(session$getOutput("note_content")$html),
    as.character(prepare_note_content(loaded_dataset()$distance[3], loaded_dataset()$datetime[3]))
  )

})
