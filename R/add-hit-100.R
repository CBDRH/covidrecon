#' Add a column that indicates when a country hit 100
#'
#' Requires that country is called `country_region`, date is called `date`,
#'   cumulative cases are called `cumulative_cases`.
#'
#' @param covid_data covid19 data with cumulative cases for each country
#'
#' @return
#' @export
#'
#' @examples
#' # covid %>%
#  #  add_country_hit_100()
add_country_hit_100 <- function(covid_data){
  covid_data  %>%
  dplyr::arrange(country_region, date) %>%
  dplyr::mutate(lag_cum_cases = lag(cumulative_cases),
                hit_100 = dplyr::if_else(
                  condition = (cumulative_cases >= 100) & (lag_cum_cases < 100),
                  true = TRUE,
                  false = FALSE)) %>%
  dplyr::mutate(hit_100 = ifelse(country_region == "China" & date == min(date),
                                 TRUE,
                                 hit_100)) %>%
    # we don't need this anymore
    dplyr::select(-lag_cum_cases) %>%
    dplyr::ungroup()
}
