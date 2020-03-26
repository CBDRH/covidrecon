#' Extract changepoint
#'
#' @param data data.frame
#' @param x variable name (as character) of cases. Default is "cases".
#' @param ... params to pass to `changepoint::cpt.meanvar`
#'
#' @return
#' @export
extract_change_point_mean_var <- function(data,
                                          x = "cases",
                                          ...){
  cpt_mean_var <- changepoint::cpt.meanvar(data[[x]],
                                           class = FALSE,
                                           ...)
  cpt_mean_var["cpt"]
}

#' Calculate changepoint date using `changepoint::cpt.meanvar`
#'
#' @param covid_data data.frame
#' @param x variable name (as character) of cases. Default is "cases".
#' @param ... params to pass to `changepoint::cpt.meanvar`
#'
#' @return
#' @export
covid_change_point <- function(covid_data,
                               x = "cases",
                               ...){

  covid_data %>%
    dplyr::group_by(country_region) %>%
    dplyr::filter(cumulative_cases > 0) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(n_obs = vctrs::vec_size(country_region)) %>%
    # remove countries with only 4 observations
    dplyr::filter(n_obs > 4) %>%
    # ungroup() %>%
    dplyr::group_by(country_region) %>%
    tidyr::nest() %>%
    dplyr::mutate(change_day = purrr::map_dbl(data,
                                              extract_change_point_mean_var,
                                              x,
                                              ...)) %>%
    tidyr::unnest(cols = c(data)) %>%
    dplyr::select(-c(year:geo_id),
                  -date_since_100_cases,
                  -n_obs)  %>%
    dplyr::filter(n_days == change_day) %>%
    dplyr::rename(change_point_date = date) %>%
    dplyr::select(country_region,
           change_point_date)
}

#' Adds changepoint date based on `changepoint::cpt.meanvar`
#'
#' @param covid_data data.frame
#' @param x variable name (as character) of cases. Default is "cases".
#' @param ... params to pass to `changepoint::cpt.meanvar`
#'
#' @return
#' @export
add_covid_change_point <- function(covid_data,
                                   x = "cases",
                                   ...){
  covid_data %>%
    covid_change_point(x, ...) %>%
    dplyr::left_join(covid_data, by = "country_region")
}
