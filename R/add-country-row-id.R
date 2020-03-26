#' Add row ID for each country from start date
#'
#' @param covid_data covid19 data
#'
#' @return adds row ID column, `n_days`, for each country
#' @export
add_country_row_id <- function(covid_data){
  covid_data %>%
    dplyr::arrange(geo_id, date) %>%
    dplyr::group_by(geo_id) %>%
    dplyr::mutate(n_days = dplyr::row_number()) %>%
    dplyr::ungroup()
}
