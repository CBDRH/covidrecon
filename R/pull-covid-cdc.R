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
covid_ecdc <- function(date){
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
#' @param memoise do you want to memoise (cache the data?). Default is TRUE
#'
#' @return list of yesterdays and today's COVID19 data
#' @export
try_ecdc <- function(memoise = TRUE){

  todays_date <- format(lubridate::today(tz = "CET"), "%Y-%m-%d")
  yesterday <- format(lubridate::today(tz = "CET") - 1L, "%Y-%m-%d")

  # memoise covid19 data
  if (memoise) {
    covid_cache <- memoise::cache_filesystem(".covid_cache")
    memoise_covid <- memoise::memoise(covid_ecdc, cache = covid_cache)
  }

  if (!is.null(git2r::discover_repository("."))) {
    usethis::use_git_ignore(".covid_cache")
  }

  safe_covid_ecdc <- purrr::safely(memoise_covid)
  purrr::flatten(
    list(
      today = discard_null(safe_covid_ecdc(todays_date)),
      yesterday = discard_null(safe_covid_ecdc(yesterday))
      )
  )
}

#' Pull latest covid19 data from European CDC
#'
#' This pull data from \url{https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide}
#'
#' @param patch logical. Patch China miscounts see [patch_data()].
#'   Default is TRUE.
#' @param memoise do you want to memoise (cache the data?). Default is TRUE
#'
#' @return data.frame
#' @export
covid_latest <- function(patch = TRUE, memoise = TRUE){

  data <- try_ecdc(memoise)

  latest_data <- pluck_latest_ecdc(data)

  if (patch) {
    latest_data <- patch_data(latest_data)
  }

  tidy_covid <- latest_data %>%
    dplyr::rename(date = date_rep,
                  country_region = countries_and_territories) %>%
    # dplyr::arrange(country_region, desc(date)) %>%
    dplyr::arrange(date) %>%
    dplyr::group_by(geo_id) %>%
    dplyr::mutate(cumulative_cases = cumsum(cases),
                  week = lubridate::week(date)) %>%
    dplyr::ungroup() %>%
    dplyr::select(date,
                  country_region,
                  deaths,
                  cases,
                  cumulative_cases,
                  year,
                  month,
                  week,
                  day,
                  dplyr::everything()) %>%
    dplyr::arrange(geo_id, date)


  return(tidy_covid)
}

#' Pluck the latest ecdc data
#'
#' For internal use within covid19 extraction data
#'
#' @param data covid19 list of latest data pulled from `try_ecdc`.
#'
#' @return single data.frame
#' @note internal
#' @export
pluck_latest_ecdc <- function(data) {
  # if we have data for both, take the latest date
  if (all(inherits_data_frames(data))) {
    covid_latest_dates <- c(max(data[[1]]$date_rep),
                            max(data[[2]]$date_rep))

    which_is_latest <- which.max(covid_latest_dates)

    latest_data <- data[[which_is_latest]]

    # else, only the "result" has a data.frame
  } else {
    latest_data <- pluck_result(data)
  }

  message(
    "covid data extracted from ",
    min(latest_data$date_rep),
    " UTC",
    " to ",
    max(latest_data$date_rep),
    " UTC"
  )

  return(latest_data)

}
