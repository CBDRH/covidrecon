#' get the covid_high_incidence data
#'
#' @param covid_data data pulled from `covid_pull_data()`
#'
#' @return high incidence data
#' @export
covid_high_incidence <- function(covid_data){

high_incidence_countries <- covid_data %>%
  filter_country_over_100() %>%
  add_country_row_id()

high_incidence_countries <-
  dplyr::left_join(x = high_incidence_countries,
            y = {
              high_incidence_countries %>%
                dplyr::filter(hit_100) %>%
                dplyr::mutate(offset = rnum) %>%
                dplyr::select(country_region, offset)
            },
            by = "country_region") %>%
  dplyr::mutate(normalised_date = rnum - offset) %>%
  dplyr::select(-c(over_100, offset)) %>%
  add_continent(country_source = country_region)

return(high_incidence_countries)
}
