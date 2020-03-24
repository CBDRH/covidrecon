#' Create a summary for each country, and a flag of whether they are over 100
#'
#' @param covid_data covid19 data
#'
#' @return data.frame with two columns, `country_region` and `over_100`
#' @export
summarise_country_over_100 <- function(covid_data){
  covid_data %>%
    add_country_hit_100() %>%
    dplyr::group_by(country_region) %>%
    dplyr::summarise(over_100 = any(hit_100))
}

#' Filter data to only include countries that have more than 100 cases
#'
#' @param covid_data covid19 data
#'
#' @return data.frame with covid data that has more than 100 cases
#' @export
filter_country_over_100 <- function(covid_data){
  dplyr::left_join(
    x = add_country_hit_100(covid_data),
    y = summarise_country_over_100(covid_data),
    by = "country_region"
  ) %>%
    dplyr::filter(over_100)
}
