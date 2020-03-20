str_remove_x <- function(x) stringr::str_remove_all(x, "x")

covid_clean_date <- function(x) lubridate::mdy(str_remove_x(x))

add_continent <- function(x){
  x %>%
  dplyr::mutate(
    continent = countrycode::countrycode(
      sourcevar = country_region,
      origin = "country.name",
      destination = "continent",
      warn = FALSE
    )
  )
}
