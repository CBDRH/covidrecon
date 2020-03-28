#' Add Weekly average
#'
#' @param covid covid19 data
#' @param ... variables to compute weekly average of
#'
#' @return data frame with weekly average computed
#' @export
#' @examples
#' \dontrun{
#' covid <- covid_latest()
#' add_weekly_avg(covid, cases)
#' add_weekly_avg(covid, deaths)
#' add_weekly_avg(covid, cases, deaths)
#' }
add_weekly_avg <- function(covid, ...){
  dots <- rlang::enquos(...)
  covid %>%
    dplyr::mutate(week = lubridate::week(date)) %>%
    dplyr::group_by(country_region, week) %>%
    # mutate_at(vars(cases, deaths), list(avg_weekly = mean))  %>%
    dplyr::mutate_at(dplyr::vars(!!!dots), list(avg_weekly = mean))  %>%
    dplyr::ungroup()
}
