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
      hit_limit = (
        geo_id == "CN" & date == min(date)
        ),
      hit_limit = dplyr::if_else(
        condition = geo_id != "CN" &
          (cumulative_cases >= limit) &
          (lag_cum_cases < limit),
        true = TRUE,
        false = hit_limit
        ),
      # hit_limit = (cumulative_cases >= limit),
      # china was over the limit before data was collected (from before )
      # hit_limit = dplyr::if_else(
      #   condition = geo_id == "CN" & date == min(date),
      #   true = TRUE,
      #   false = FALSE
      # )
    ) %>%
    # we don't need this anymore
    dplyr::select(-lag_cum_cases) %>%
    dplyr::ungroup()
}


# possible refactor

#
# df_1 <- covid_over_limit %>%
#   filter(hit_limit) %>%
#   select(-hit_limit)
#
# df_2 <- covid %>%
#   group_by(country_region) %>%
#   slice(where_all_gte(cumulative_cases, 100))  %>%
#   ungroup()
#
# all.equal(names(df_1), names(df_2))
# all.equal(dim(df_1), dim(df_2))
# all.equal(df_1, df_2)
#
# visdat::vis_compare(df_1, df_2)
#
# df_1[14:19, ]
# df_2[14:19, ]
#
# df_1 <-
#   covid %>%
#   group_by(country_region) %>%
#   mutate(hit_limit = (
#     row_number() == where_all_gt(cumulative_cases, 100)
#   ))  %>%
#   ungroup()
#
# df_2 <- covid_over_limit
#
# identical(df_1, df_2)
# all.equal(df_1, df_2)


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

#' Add a column of the days since some limit of cumulative cases is reached
#'
#' @param covid_data covid19 data
#' @param limit limit for flagging
#'
#' @return
#' @export
#'
#' @examples
add_days_since_limit <- function(covid_data, limit = 100) {
  # return a dataset that just contains the country and the date they started
  # reaching certain cumulative cases
  days_at_start <- covid %>%
    add_country_hit_limit(limit = limit) %>%
    dplyr::filter(hit_limit) %>%
    dplyr::select(geo_id,
                  date_start_limit = date)

  dplyr::left_join(x = covid,
                   y = days_at_start,
                   by = "geo_id") %>%
    dplyr::arrange(geo_id, date) %>%
    dplyr::group_by(geo_id) %>%
    dplyr::mutate(days_since_limit = diff_days(date, date_start_limit)) %>%
    dplyr::ungroup()
}
