#' Patch the china case records
#'
#' In Mid February, China changed their case definition, from
#'   RT-PCR-swab-positive to test-positive or clinical signs of viral pneumonia.
#'   This led to a large spike not representative of cases. Numbers sourced from  \url{https://en.wikipedia.org/w/index.php?title=Template:2019–20_coronavirus_pandemic_data/China_medical_cases_by_province&oldid=946973173}.
#'   This changes the cases as follows date:
#'
#'     - 04/2/2020 to 1995 cases (from 3237),
#'     - 13/2/2020 to 1820 cases (from 15141),
#'     - 15/2/2020 to 1503 cases (from 2538),
#'     - 16/2/2020 to 1121 cases (from 2007),
#'
#'
#' @param data data pulled by `try_ecdc` and `pluck_latest`.
#' @note internal
#'
#' @return data.frame wtih adjusted cases
patch_china_data <- function(data){
  dplyr::mutate(data,
                cases = dplyr::case_when(
                  geo_id == "CN" & year == 2020 & month == 2 & day == 4 ~ 1995,
                  geo_id == "CN" & year == 2020 & month == 2 & day == 13 ~ 1820,
                  geo_id == "CN" & year == 2020 & month == 2 & day == 15 ~ 1503,
                  geo_id == "CN" & year == 2020 & month == 2 & day == 16 ~ 1121,
                  TRUE ~ cases
                ))
}
