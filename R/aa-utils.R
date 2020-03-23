str_remove_x <- function(x) stringr::str_remove_all(x, "x")

covid_jhu_clean_date <- function(x) lubridate::mdy(str_remove_x(x))

add_continent <- function(x, country_source){
  x %>%
  dplyr::mutate(
    continent = countrycode::countrycode(
      sourcevar = {{ country_source }},
      origin = "country.name",
      destination = "continent",
      warn = FALSE
    )
  )
}

glue_date <- function(date){
  glue::glue("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-{date}.xlsx")
}

discard_null <- function(x) purrr::discard(x, is.null)

pluck_result <- function(x) purrr::pluck(x, "result")

inherits_data_frames <- function(x) purrr::map_lgl(x, inherits, "data.frame")
