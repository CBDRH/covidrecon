#' Base plot for creating cumulative cases of covid19 data
#'
#' @param covid_data - covid19 data.frame
#'
#' @return ggplot plot
#' @import ggplot2
#' @export
gg_covid_cumulative_exceed_limit <- function(covid_data){

  covid_data_last <- covid_data %>%
    dplyr::group_by(geo_id) %>%
    dplyr::filter(date == dplyr::last(date))

  ggplot(data = covid_data,
         aes(x = date_since_limit_cases,
             y = cumulative_cases,
             colour = country_region)) +
    geom_line() +
    scale_y_log10(labels = scales::comma) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
    theme_minimal() +
    labs(y = "Cumulative cases (logarithmic scale)",
         x = "Days since cumulative cases exceeded a limit",
         title = create_title_date(covid_data)) +
    ggrepel::geom_label_repel(data = covid_data_last,
                              aes(label = country_region),
                              nudge_x = 15) +
    theme(legend.position = "none")
}


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
