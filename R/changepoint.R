#' Extract changepoint
#'
#' @param data data.frame
#' @param x variable name (as character) of cases. Default is "cases".
#' @param ... params to pass to `changepoint::cpt.meanvar`
#'
#' @return changepoint vars
#' @export
extract_change_point_mean_var <- function(data,
                                          x = "cases",
                                          ...){
  cpt_mean_var <- changepoint::cpt.meanvar(data[[x]],
                                           class = FALSE,
                                           ...)
  cpt_mean_var["cpt"]
}

#' Extract non-parametric changepoint
#'
#' @param data data.frame
#' @param x variable name (as character) of cases. Default is "cases".
#' @param ... params to pass to `changepoint.np::cpt.np`
#'
#' @return changepoint vars
#' @export
extract_first_np_change_point <- function(data,
                                    x = "cases",
                                    ...){
  cpt_np <- changepoint.np::cpt.np(data[[x]],
                                   method="PELT",
                                   minseglen=5,
                                   nquantiles =4*log(length(data)),
                                   class = FALSE,
                                           ...)
  cpt_np[1]
}

#' Calculate changepoint date using `changepoint::cpt.meanvar`
#'
#' @param covid_data data.frame
#' @param x variable name (as character) of cases. Default is "cases".
#' @param ... params to pass to `changepoint::cpt.meanvar`
#'
#' @return data.frame of changepoints
#' @export
covid_change_point <- function(covid_data,
                               x = "cases",
                               ...){

  covid_data <- covid_data %>%
    add_country_row_id() %>%
    dplyr::group_by(geo_id) %>%
    # dplyr::filter(cumulative_cases > 0) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(n_obs = vctrs::vec_size(geo_id)) %>%
    # remove countries with only 4 observations
    dplyr::filter(n_obs > 4) %>%
    # ungroup() %>%
    dplyr::group_by(geo_id) %>%
    tidyr::nest() %>%
    dplyr::mutate(change_day = purrr::map_dbl(data,
                                              extract_first_np_change_point,
                                              # was: extract_change_point_mean_var,
                                              x,
                                              ...)) %>%
    tidyr::unnest(cols = c(data)) %>%
    dplyr::select(-year,
                  -month,
                  -day,
                  -n_obs)

  covid_data %>%
    dplyr::filter(n_days == change_day) %>%
    dplyr::rename(change_point_date = date) %>%
    dplyr::select(geo_id,
                  change_point_date) %>%
    dplyr::ungroup()
}

#' Adds changepoint date based on `changepoint.np::cpt.np`
#'
#' @param covid_data data.frame
#' @param x variable name (as character) of cases. Default is "cases".
#' @param ... params to pass to `changepoint.np::cpt.np`
#'
#' @return ggplot2 plot
#' @export
add_covid_change_point <- function(covid_data,
                                   x = "cases",
                                   ...){
  covid_data %>%
    covid_change_point(x, ...) %>%
    dplyr::left_join(covid_data, by = "geo_id") %>%
    # monkey-patch the NZ changepoint
    # dplyr::mutate(change_point_date = ifelse(country_region == "New_Zealand", lubridate::ymd("2020-03-18", tz="UTC"), change_point_date)) %>%
    dplyr::mutate(change_point_date = if_else(country_region == "New_Zealand", lubridate::ymd_hm("2020-03-18 00:00"), change_point_date)) %>%
    dplyr::mutate(days_since_changepoint = as.integer(difftime(date,
                                                               change_point_date,
                                                               units="days")))
}
