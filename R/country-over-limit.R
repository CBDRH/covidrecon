#' Add a column that indicates when a country hits a limit of cases
#'
#' Requires that country is called `country_region`, date is called `date`,
#'   cumulative cases are called `cumulative_cases`.
#'
#' @param covid_data covid19 data with cumulative cases for each country
#' @param limit defaults to 100
#'
#' @return
#' @export
#'
#' @examples
#' # covid %>%
#  #  add_country_hit_limit()
add_country_hit_limit <- function(covid_data, limit = 100){
  covid_data  %>%
    dplyr::arrange(geo_id, date) %>%
    dplyr::mutate(
      lag_cum_cases = lag(cumulative_cases),
      hit_limit = ((cumulative_cases >= limit) & (lag_cum_cases < limit)),
      # hit_limit = (cumulative_cases >= limit),
      # china was over the limit before data was collected (from before )
      hit_limit = dplyr::if_else(
        condition = geo_id == "CN" & date == min(date),
        true = TRUE,
        false = hit_limit
      )
    ) %>%
    # we don't need this anymore
    dplyr::select(-lag_cum_cases) %>%
    dplyr::ungroup()
}

#' Create a summary for each country, and a flag of whether they are over limit
#'
#' @param covid_data covid19 data
#' @param limit limit
#'
#' @return data.frame with two columns, `country_region` and `over_limit`
#' @export
summarise_country_over_limit <- function(covid_data, limit = 100){
  covid_data %>%
    add_country_hit_limit(limit = limit) %>%
    dplyr::group_by(geo_id) %>%
    dplyr::summarise(over_limit = any(hit_limit))
}

#' Filter data to only include countries that have more than a certain number
#'   of cases
#'
#' @param covid_data covid19 data
#' @param limit number of cases - default over 100
#'
#' @return data.frame with covid data that has more than 100 cases
#' @export
filter_country_over_limit <- function(covid_data, limit = 100){
  dplyr::left_join(
    x = add_country_hit_limit(covid_data, limit = limit),
    y = summarise_country_over_limit(covid_data, limit = limit),
    by = "geo_id"
  ) %>%
    dplyr::filter(over_limit)
}
