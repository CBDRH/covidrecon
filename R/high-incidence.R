#' Return only countries that have more than some limit
#'
#' @param covid_data data pulled from `covid_pull_data()`
#' @param limit when countries hit some limit. default is 100
#'
#' @return high incidence data
#' @export
covid_high_incidence <- function(covid_data, limit = 100) {

  high_incidence_countries <- covid_data %>%
    filter_country_over_limit(limit = limit) %>%
    add_country_row_id()

  days_start_limit <- high_incidence_countries %>%
    dplyr::filter(hit_limit) %>%
    dplyr::select(geo_id,
                  # the number of days since it started at limit
                  day_start_limit = n_days)

  dplyr::left_join(x = high_incidence_countries,
                   y = days_start_limit,
                   by = "geo_id") %>%
    dplyr::mutate(date_since_limit_cases = n_days - day_start_limit) %>%
    dplyr::select(-over_limit,
                  -day_start_limit,
                  -hit_limit)

}
