#these libraries are necessary
library(readxl)
library(httr)
library(glue)
library(lubridate)
#create the URL where the dataset is stored with automatic updates every day

glue_date <- function(date){
  glue::glue("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-{date}.xlsx")
}

#download the dataset from the website to a local temporary file
covid_pull_ecdc <- function(date){
  url <- glue_date(date)
  httr::GET(url = url,
      config = httr::authenticate(":", ":", type="ntlm"),
      httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
  readxl::read_excel(tf)
}

safe_covid_pull_ecdc <- purrr::safely(covid_pull_ecdc)

discard_null <- function(x) purrr::discard(x, is.null)
pluck_result <- function(x) purrr::pluck(x, "result")

try_covid_yesterday_today <- function(){

  todays_date <- format(lubridate::today(), "%Y-%m-%d")
  yesterday <- format(lubridate::today() - 1L, "%Y-%m-%d")

  purrr::flatten(
    list(
      today = discard_null(safe_covid_pull_ecdc(todays_date)),
      yesterday = discard_null(safe_covid_pull_ecdc(yesterday))
      )
  )
}

latest_covid <- function(){
  data <- pluck_result(try_covid_yesterday_today())

  message("covid data extracted from ",
          min(data$DateRep), " UTC",
          " to ",
          max(data$DateRep), " UTC")

  data
}

covid_data <- latest_covid()

