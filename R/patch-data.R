#' Patch the case records for China and Italy
#'
#' In Mid February 2020, China changed their case definition, from
#'   RT-PCR-swab-positive to test-positive or clinical signs of viral pneumonia.
#'   This led to a large spike not representative of cases.
#'   some further thoughts:
#'    \url{https://www.worldometers.info/coronavirus/how-to-interpret-feb-12-case-surge/}
#'   This changes the cases on the dates shown below to the verified lab-confirmed only counts
#'   available at
#'   https://en.wikipedia.org/w/index.php?title=Template:2019–20_coronavirus_pandemic_data/China_medical_cases_by_province&oldid=946973173
#'   Note that the EuroCDC data has dates a day later than the wikipedia data for China
#'   due to time zone differences. This adjustment is only valid for EuroCDC data, for the JHU
#'   data the dates for the adjustments need to be shifted back on day.

#'     - 13/2/2020 to 1820 cases (from 15141),
#'     - 14/2/2020 to 1995 cases (from 3237),
#'     - 15/2/2020 to 1503 cases (from 2538),
#'     - 16/2/2020 to 1121 cases (from 2007),
#'
#' In addition, there are some anomalies with Italy, resulting in these changes
#'
#'     - 15/3: to 3497 cases (from ...)
#'     - 16/3: to 3590 cases (from ...)
#'     - 17/3: to 3233 cases (from ...)
#'
#' These changes for Italy came from the WHO situation reports: \url{https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports}.
#'
#' @param data data pulled by `try_ecdc` and `pluck_latest`.
#' @note internal
#'
#' @return data.frame with adjusted cases
patch_data <- function(data){
  dplyr::mutate(data,
                cases = dplyr::case_when(
                  geo_id == "CN" & year == 2020 & month == 2 & day == 13 ~ 1820,
                  geo_id == "CN" & year == 2020 & month == 2 & day == 14 ~ 1995,
                  geo_id == "CN" & year == 2020 & month == 2 & day == 15 ~ 1503,
                  geo_id == "CN" & year == 2020 & month == 2 & day == 16 ~ 1121,
                  # now for italy
                  geo_id == "IT" & year == 2020 & month == 3 & day == 15 ~ 3497,
                  geo_id == "IT" & year == 2020 & month == 3 & day == 16 ~ 3590,
                  geo_id == "IT" & year == 2020 & month == 3 & day == 17 ~ 3233,
                  TRUE ~ cases
                ))
}
