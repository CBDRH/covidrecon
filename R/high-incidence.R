#' Return only countries that have more than 100 cases
#'
#' @param covid_data data pulled from `covid_pull_data()`
#'
#' @return high incidence data
#' @export
covid_high_incidence <- function(covid_data) {

  high_incidence_countries <- covid_data %>%
    filter_country_over_100() %>%
    add_country_row_id()

  days_start_100 <- high_incidence_countries %>%
    dplyr::filter(hit_100) %>%
    dplyr::select(country_region,
                  # the number of days since it started at 100
                  day_start_100 = rnum)

  dplyr::left_join(x = high_incidence_countries,
                   y = days_start_100,
                   by = "country_region") %>%
    dplyr::mutate(date_since_100_cases = rnum - day_start_100) %>%
    dplyr::select(-over_100,
                  -day_start_100,
                  -rnum,
                  -hit_100)

}
