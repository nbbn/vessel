# needed packages: assertthat readr dplyr tidyr geosphere
library(dplyr)

#' ETL function
#' 
#' @param path path to valid input csv file following column schema definced in task
#'
#' @return tibble with transformed input table. There is one record per ship with details of longest trip.
#' @export
#'
#' @examples
#' \dontrun{
#' etl("/home/x/ships.csv") %>% readr::write_csv("precalculated_dataset.csv")
#' }
etl <- function (path) {
  assertthat::assert_that(assertthat::is.readable(path))
  ships <- readr::read_csv(
    file = path,
    col_types = cols(
      .default = col_double(),
      DESTINATION = col_character(),
      FLAG = col_character(),
      SHIPNAME = col_character(),
      DATETIME = col_datetime(format = ""),
      PORT = col_character(),
      date = col_date(format = ""),
      ship_type = col_character(),
      port = col_character()
    )
  ) %>%
    # as some rows are duplicated, this potentially can speed up calculation
    distinct(SHIP_ID, DATETIME, .keep_all = TRUE)
  nested <-
    ships %>%
    select(LON, LAT, SHIPNAME, ship_type, SHIP_ID, DATETIME) %>%
    dplyr::arrange(DATETIME) %>%
    group_by(SHIP_ID) %>%
    tidyr::nest()
  calculated <- nested %>%
    mutate(new = list(purrr::map_df(
      data,
      function(df) {
        data <- df %>% mutate(
          p_lon = lag(LON),
          p_lat = lag(LAT)
        ) %>%
          rowwise() %>%
          mutate(distance = geosphere::distm(c(LON, LAT), c(p_lon, p_lat), fun = geosphere::distHaversine)[,1]) %>%
          ungroup()
        data <- data %>% slice_max(distance, n = 1) %>% slice_max(DATETIME)
        return(data)
      }
    ))) %>%
    tidyr::unnest(new) %>%
    select(-data)
  return(
    calculated %>%
      ungroup() %>%
      select(
        ship_type = ship_type,
        ship_name = SHIPNAME,
        lon = LON,
        lat = LAT,
        p_lon,
        p_lat,
        datetime = DATETIME,
        distance
      )
  )
}

