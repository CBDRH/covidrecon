#' Summarise covid19 data to cases over time globally
#'
#' Requires that incidence cases are called `cases`, country is called
#'   `country_region`, and date is called `date`.
#'
#' @param covid_data covid19 data
#'
#' @return data.frame with covid19 cases, columns called
#'   `date`, `cases`, and `cumulative_cases`.
#' @export
summarise_covid_global <- function(covid_data){
  covid_data %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(cases = sum(cases),
                     cumulative_cases = sum(cumulative_cases))
}
