#' Calculate the time to double covid19 data
#'
#' @param covid_data covid19 data
#'
#' @return data of country region, with `days_to_double`, and `double_target`
#'   being the "from x number of cases"
#'
#' @author Miles McBain
#'
#' @export
covid_summarise_doubling <- function(covid_data) {

  country_cases <-
    covid_data %>%
    dplyr::filter(cases > 0) %>%
    dplyr::group_by(date, country_region) %>%
    dplyr::summarise(cases = sum(cases))

  covid_double <-
    country_cases %>%
    dplyr::group_by(country_region) %>%
    dplyr::mutate(lead_date = lead(date),
                  n_double = floor(log(cases, 2) - log(min(cases), 2))) %>%
    ## a = b * 2^n => log(a,2) - log(b,2) = n
    dplyr::group_by(country_region, n_double) %>%
    dplyr::mutate(days_to_double = as.numeric(max(lead_date) - min(date),
                                              units = "days")) %>%
    ## lead_date is the date of change
    dplyr::ungroup() %>%
    dplyr::group_by(country_region) %>%
    dplyr::mutate(double_target = min(cases) * 2^n_double) %>%
    dplyr::group_by(country_region, n_double) %>%
    dplyr::summarise(
      days_to_double = max(days_to_double),
      cases = min(cases),
      double_target = min(double_target)
    ) %>%
    tidyr::drop_na(days_to_double) %>%
    dplyr::ungroup()

  ## NAs occur in the final rows for each country, where we're still counting
  ## toward the next double.

  covid_double

}
