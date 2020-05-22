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
    dplyr::mutate(cases = if_else(cases < 0, 0, cases)) %>%
    dplyr::select(dates = date,
                  I = cases)
}

#' Fit EpiEstim::estimate_R to covid19 data
#'
#' This fits EpiEstim::estimate_R, with config values `mean_si = 4.7` and
#'   `std_si = 2.9` from https://doi.org/10.1101/2020.02.03.20019497
#'   It assumes that the data is in the format provided by
#'   `covid_prepare_estimate_repro`.
#'
#' @param covid_data_prepared data is in the format provided by
#'   `covid_prepare_estimate_repro`, with columns "dates" and "I", where "dates"
#'   contains data of class "Date", and "I" contains cases.
#' @param estimate_method default is "parametric_si"
#' @param si_sample SI samples. Default is NULL.
#' @param config named list of values to pass to EpiEstim::make_config. Default
#'  is `mean_si = 4.7`, and `std_si = 2.9`.
#'
#' @return estimated reproductive number
#' @export
covid_estimate_repro <- function(covid_data_prepared,
                                 estimate_method = "parametric_si",
                                 si_sample = NULL,
                                 config = list(mean_si = 4.7,
                                               std_si = 2.9)){
  if (estimate_method ==  "parametric_si") {

    EpiEstim::estimate_R(covid_data_prepared,
                         method = estimate_method,
                         config = EpiEstim::make_config(config))

  } else if (estimate_method ==  "si_from_sample") {

    EpiEstim::estimate_R(covid_data_prepared,
                         method = estimate_method,
                         si_sample = si_sample,
                         config = EpiEstim::make_config(config))

  }
}

#' Tidies up output from `EpiEstim::estimate_R`
#'
#' @param covid_estimated_reproduction output from `EpiEstim::estimate_R`
#'
#' @return returns columns on estiamted instantaneous reproduction, and quantiles,
#'   as well as dates.
#' @export
tidy_repro_estimate <- function(covid_estimated_reproduction){

  df_reproduction <- covid_estimated_reproduction$R

  date_range <- 8:length(covid_estimated_reproduction$dates)

  df_reproduction$date <- covid_estimated_reproduction$dates[date_range]

  tibble::as_tibble(df_reproduction,
                    .name_repair = janitor::make_clean_names)
}

#' Fits instant reproduction for each country
#'
#' @param covid_data covid19 data
#' @param estimate_method default is "parametric_si"
#' @param si_sample SI samples. Default is NULL.
#' @param config named list of values to pass to EpiEstim::make_config. Default
#'  is `mean_si = 4.7`, and `std_si = 2.9`.
#' @name covid-estimate-repro
#'
#' @return data.frame with several list columns containing various output of
#'   `EpiEstim::estimate_R`.
#'
#' @export
estimate_repro_all <- function(covid_data,
                               estimate_method = "parametric_si",
                               si_sample = NULL,
                               config = list(mean_si = 4.7,
                                             std_si = 2.9)) {
  covid_data %>%
    dplyr::group_by(geo_id) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      prepared_data = purrr::map(data, covid_prepare_estimate_repro),
      repro_estimate = purrr::map(prepared_data,
                                  purrr::safely(covid_estimate_repro),
                                  estimate_method = estimate_method,
                                  si_sample = si_sample,
                                  config = EpiEstim::make_config(config)),
      repro_result = purrr::map(repro_estimate,
                                purrr::pluck,
                                "result"),
      result = purrr::map(repro_result, tidy_repro_estimate)
      )
}

#' Tidies output of `estimate_repro_all`
#'
#' @param covid_data_estimated covid19 data estimated with `estimate_repro_all`
#'
#' @return data.frame
#' @export
augment_estimate_repro <- function(covid_data_estimated){
  covid_data_estimated %>%
    dplyr::select(geo_id,
                  result) %>%
    tidyr::unnest(cols = c(result))  %>%
    dplyr::select(geo_id,
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
    dplyr::select(geo_id,
                  result) %>%
    dplyr::mutate(no_result = purrr::map_lgl(result, function(x) nrow(x) == 0)) %>%
    dplyr::filter(no_result) %>%
    dplyr::pull(geo_id)

}


#' Add instant reproduction number measures to provided covid19 data
#'
#' @param covid_data covid19 data
#' @inheritParams covid-estimate-repro
#'
#' @return data.frame with covid19 columns plus output of `EpiEstim::estimate_R`
#'  and columns on estimated mean R values and quantiles etc.
#' @export
add_instant_reproduction <- function(covid_data,
                                     estimate_method = "parametric_si",
                                     si_sample = NULL,
                                     config = list(mean_si = 4.7,
                                                   std_si = 2.9)) {

  tidy_instant <- covid_data %>%
    estimate_repro_all(estimate_method = estimate_method,
                       si_sample = si_sample,
                       config = EpiEstim::make_config(config)) %>%
    augment_estimate_repro()

  covid_data %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::left_join(tidy_instant,
                     by = c("geo_id", "date"))

}
