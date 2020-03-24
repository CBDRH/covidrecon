#' Add row ID for each country from start date
#'
#' @param covid_data covid19 data
#'
#' @return adds row ID column, `rnum`, for each country
#' @export
add_country_row_id <- function(covid_data){
  covid_data %>%
    dplyr::arrange(country_region, date) %>%
    dplyr::group_by(country_region) %>%
    dplyr::mutate(rnum = dplyr::row_number()) %>%
    dplyr::ungroup()
}