#' get the covid_high_incidence data
#'
#' @param covid_data data pulled from `covid_pull_data()`
#'
#' @return high incidence data
#' @export
covid_high_incidence <- function(covid_data){

  incidence_countries <- covid_data %>%
  dplyr::group_by(country_region, date) %>%
  dplyr::summarise(cases = sum(cases),
                   cumulative_cases = sum(cumulative_cases))

incidence_global <- covid_data %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(cases = sum(cases),
                   cumulative_cases = sum(cumulative_cases))

# find dates on which 100 cumulative cases occurred
high_incidence_countries <- incidence_countries %>%
  dplyr::arrange(country_region, date) %>%
  dplyr::mutate(lag_cum_cases = lag(cumulative_cases),
         hit_100 = dplyr::if_else(
           condition = (cumulative_cases >= 100) & (lag_cum_cases < 100),
           true = TRUE,
           false = FALSE)) %>%
  dplyr::mutate(hit_100 = ifelse(country_region == "China" & date == min(date),
                                 TRUE,
                                 hit_100))


high_incidence_countries <-
  dplyr::left_join(x = high_incidence_countries,
            y = {
              high_incidence_countries %>%
                dplyr::group_by(country_region) %>%
                dplyr::summarise(over_100 = any(hit_100))
            },
            by = "country_region"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(over_100 == TRUE)

high_incidence_countries <- high_incidence_countries %>%
  dplyr::arrange(country_region, date) %>%
  dplyr::group_by(country_region) %>%
  dplyr::mutate(rnum = dplyr::row_number()) %>%
  dplyr::ungroup()

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
  dplyr::select(-c(lag_cum_cases, over_100, offset)) %>%
  add_continent(country_source = country_region)

return(high_incidence_countries)
}
