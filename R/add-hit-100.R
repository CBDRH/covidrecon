#' Add a column that indicates when a country hits a limit of cases
#'
#' Requires that country is called `country_region`, date is called `date`,
#'   cumulative cases are called `cumulative_cases`.
#'
#' @param covid_data covid19 data with cumulative cases for each country
#' @param limit defaults to 100
#'
#' @return data.frame
#' @export
#'
#' @examples
#' # covid %>%
#  #  add_country_hit_limit()
add_country_hit_limit <- function(covid_data, limit = 100){
  covid_data  %>%
  dplyr::arrange(country_region, date) %>%
  dplyr::mutate(
    lag_cum_cases = lag(cumulative_cases),
    hit_limit = ((cumulative_cases >= limit) & (lag_cum_cases < limit))) %>%
  dplyr::mutate(
    hit_limit = dplyr::if_else(
      condition = country_region == "China" & date == min(date),
      true = TRUE,
      false = hit_limit
      )
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
