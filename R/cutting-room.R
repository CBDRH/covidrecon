#' Add a country label for plotting
#'
#' This adds the appropriate lables for each country, to assist plotting
#'   with geom_label
#'
#' @param data covid19 data
#' @param countries default adds all countries. Otherwise, they are
#'   specified with character vector.
#'
#' @return data.frame with added columns of selected countries
#' @export
add_country_label <- function(data,
                              countries = unique(data$country_region)){
  data %>%
    dplyr::mutate(clabel_value = dplyr::if_else(
      condition = country_region %in% countries,
      true = country_region,
      false = NA_character_
    )) %>%
    dplyr::group_by(geo_id) %>%
    dplyr::arrange(geo_id, normalised_date) %>%
    dplyr::mutate(clabel_x = max(normalised_date),
                  clabel_y = dplyr::last(cumulative_cases)) %>%
    dplyr::ungroup()
}



#' Adds nice country labels to ggplot, based on `add_country_label()`
#'
#' @return ggplot label added to ggplot
#' @export
geom_covid_country_label <- function(){
  ggplot2::geom_label(ggplot2::aes(x = clabel_x,
                                   y = clabel_y,
                                   label = clabel_value),
                      hjust = 0,
                      nudge_x = 0.2)
}


#' Add alpha column which will highlight australia foremost, then ohters
#'
#' @param data covid19 data
#' @param plus additional countries to highlight. Defaults to just Canada.
#'
#' @return data with added column "alpha" with extra info
#' @export
covid_highlight_australia_plus <- function(data, plus = "Canada"){
  data %>%
    dplyr::mutate(alpha = dplyr::case_when(
      country_region == "Australia" ~ 1.0,
      country_region %in% plus ~ 0.2,
      TRUE ~ 0)
    )
}
