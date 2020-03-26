#' Patch the china case records
#'
#' In Mid February, China changed their case definition, from
#'   RT-PCR-swab-positive to test-positive or clinical signs of viral pneumonia.
#'   This led to a large spike not representative of cases. some further thoughts: \url{https://www.worldometers.info/coronavirus/how-to-interpret-feb-12-case-surge/}
#'   This changes the cases from date: 13/2/2020 to 1820 cases (from 15141),
#'   and on date: 4/2/2020 to 1995 cases (from 3237).
#'
#' @param data data pulled by
#' @note internal
#'
#' @return data.frame wtih adjusted cases
patch_china_data <- function(data){
  dplyr::mutate(data,
                cases = dplyr::case_when(
                  geo_id == "CN" & year == 2020 & month == 2 & day == 13 ~ 1820,
                  geo_id == "CN" & year == 2020 & month == 2 & day == 4 ~ 1995,
                  TRUE ~ cases
                ))
}
