#' Download raw COVID19 data from European CDC
#'
#' This function will download raw COVID19 data, given a date.
#'   It will error if you enter a date that is later than the current date
#'   on the CDC site. See `latest_covid()` for a function that handles
#'   the full details of this.
#'
#' @param date in format "YYYY-MM-DD". So, "2020-01-01" is 1st Jan, 2020
#'
#' @return data.frame
#' @export
covid_pull_ecdc <- function(date){
  url <- glue_date(date)
  httr::GET(url = url,
      config = httr::authenticate(":", ":", type="ntlm"),
      httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
  readxl::read_excel(tf, .name_repair = janitor::make_clean_names)
}

#' Downloads both yesterday and today's COVID19 data
#'
#' Assuming CET time zones. For use within `latest_covid()`.
#'
#' @return list of yesterdays and today's COVID19 data
#' @export
try_covid_yesterday_today <- function(){

  todays_date <- format(lubridate::today(tz = "CET"), "%Y-%m-%d")
  yesterday <- format(lubridate::today(tz = "CET") - 2L, "%Y-%m-%d")

  safe_covid_pull_ecdc <- purrr::safely(covid_pull_ecdc)
  purrr::flatten(
    list(
      today = discard_null(safe_covid_pull_ecdc(todays_date)),
      yesterday = discard_null(safe_covid_pull_ecdc(yesterday))
      )
  )
}

#' Pull latest covid19 data from European CDC
#'
#' This pull data from \url{https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide}
#'
#' @return data.frame
#' @export
latest_covid <- function(){

  data <- try_covid_yesterday_today()

  # if we have data for both, take the latest date
  if ( all(inherits_data_frames(data)) ) {

    covid_latest_dates <- c(max(data[[1]]$date_rep),
                            max(data[[2]]$date_rep))

    which_is_latest <- which.max(covid_latest_dates)

    latest_data <- data[[which_is_latest]]

    # else, only the "result" has a data.frame
  } else {
    latest_data <- pluck_result(data)
  }

  message("covid data extracted from ",
          min(latest_data$date_rep), " UTC",
          " to ",
          max(latest_data$date_rep), " UTC")

  return(latest_data)
}


