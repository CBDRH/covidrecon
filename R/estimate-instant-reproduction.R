#' Prepare covid19 data for estimating instant repro number
#'
#' @param covid_data covid19 data
#'
#' @return dataframe with columns "dates" and "I", ready for
#'   `EpiEstim::estimate_R`
#' @export
covid_prepare_estimate_repro <- function(covid_data){
  covid_data %>%
    dplyr::ungroup() %>%
    # EpiEstim::estimate_R requires a specific format of data
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::select(dates = date,
                  I = cases)
}

#' Fit EpiEstim::estimate_R to covid19 data
#'
#' This fits EpiEstim::estimate_R, with config values `mean_si = 5.0` and
#'   `std_si = 3.0`. It assumes that the data is in the format provided by
#'   `covid_prepare_estimate_repro`.
#'
#' @param covid_data_prepared data is in the format provided by
#'   `covid_prepare_estimate_repro`, with columns "dates" and "I", where "dates"
#'   contains data of class "Date", and "I" contains cases.
#' @param estimate_method default is "parametric_si"
#'
#' @return
#' @export
covid_estimate_repro <- function(covid_data_prepared,
                                 estimate_method = "parametric_si"){
  EpiEstim::estimate_R(covid_data_prepared,
                       method = estimate_method,
                       # method="uncertain_si",
                       config = EpiEstim::make_config(list(mean_si = 5.0,
                                                           std_si = 3.0)))
  # config = make_config(list(
  # mean_si = 4.8,
  # std_mean_si = 3.0,
  # min_mean_si = 2,
  # max_mean_si = 7.5,
  # std_si = 3.0,
  # std_std_si = 1.0,
  # min_std_si = 0.5,
  # max_std_si = 4.0,
  # n1 = 1000,
  # n2 = 1000)))
}

#' Tidies up output from `EpiEstim::estimate_R`
#'
#' @param covid_estimated_reproduction output from `EpiEstim::estimate_R`
#'
#' @return returns columns on estiamted isntant reproduction, and quantiles,
#'   as well as dates.
#' @export
tidy_repro_estimate <- function(covid_estimated_reproduction){

  df_reproduction <- covid_estimated_reproduction$R

  date_range <- 8:length(covid_estimated_reproduction$dates)

  df_reproduction$date <- covid_estimated_reproduction$dates[date_range]

  tibble::as_tibble(df_reproduction,
                    .name_repair = janitor::make_clean_names)
}

#' Fits instnat reproduction for each country
#'
#' @param covid_data covid19 data
#'
#' @return data.frame with several list columns containing various output of
#'   `EpiEstim::estimate_R`.
#' @export
estimate_repro_all <- function(covid_data){
  covid_data %>%
    dplyr::group_by(country_region) %>%
    tidyr::nest() %>%
    dplyr::mutate(prepared_data = purrr::map(data, covid_prepare_estimate_repro),
                  repro_estimate = purrr::map(prepared_data,
                                              purrr::safely(covid_estimate_repro)),
                  repro_result = purrr::map(repro_estimate, pluck, "result"),
                  result = purrr::map(repro_result, tidy_repro_estimate))
}

#' Tidies output of `estimate_repro_all`
#'
#' @param covid_data_estimated covid19 data estimated with `estimate_repro_all`
#'
#' @return
#' @export
augment_estimate_repro <- function(covid_data_estimated){
  covid_data_estimated %>%
    dplyr::select(country_region,
                  result) %>%
    tidyr::unnest(cols = c(result))  %>%
    dplyr::select(country_region,
                  date,
                  dplyr::everything()) %>%
    dplyr::ungroup()
}

#' Extract (any) errors from `estimate_repro_all`
#'
#' @param covid_data_estimated data from `estimate_repro_all`
#'
#' @return vector of error messages
#' @export
country_repro_errors <- function(covid_data_estimated){
  covid_data_estimated %>%
    dplyr::select(country_region,
                  result) %>%
    dplyr::mutate(no_result = purrr::map_lgl(result, function(x) nrow(x) == 0)) %>%
    dplyr::filter(no_result) %>%
    dplyr::pull(country_region)

}
