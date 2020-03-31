#' misc-ggplot funs for covid19
#'
#' @param covid_data covid19 data
#'
#' @return ggplot image
#' @export
#' @rdname gg-covid
gg_covid_cases <- function(covid_data){
  ggplot(data = covid_data,
         aes(x = date,
             y = cases,
             group = country_region)) +
    geom_line() +
    scale_y_log10()
}

#' @export
#' @name gg-covid
gg_covid_cumulative_cases <- function(covid_data){
  ggplot(data = covid_data,
         aes(x = date,
             y = cumulative_cases,
             group = country_region)) +
    geom_line() +
    scale_y_log10()
}


#' Plot effective reproduction number
#'
#' @param covid_effective_r covid19 data with estimated effective R
#'
#' @return ggplot2 plot
#' @rdname effective-repro
#' @export
gg_effective_repro_all <- function(covid_effective_r){
  last_country_eff_rs <- covid_effective_r %>%
    dplyr::group_by(geo_id) %>%
    dplyr::filter(date == dplyr::last(date))

  covid_effective_r %>%
    ggplot(aes(x = date,
               y = mean_r,
               colour = country_region)) +
    geom_line(size = 1) +
    geom_hline(yintercept = 1.0, colour = "red") +
    ggrepel::geom_label_repel(
      data = last_country_eff_rs,
      aes(label = country_region),
      segment.alpha = 0.3,
      segment.size = 0.3,
      hjust = 0,
      # direction = "x",
      nudge_x = 0.5,
      xlim = c(
        max(covid_effective_r$date) + lubridate::days(1),
        max(covid_effective_r$date) + lubridate::days(5)
      )
    ) +
    scale_y_log10() +
    scale_x_date(
      date_breaks = "1 week",
      date_labels = "%d %b",
      expand = expansion(mult = c(0, 0.25))
    ) +
    scale_colour_brewer(palette = "Dark2") +
    # scale_colour_viridis_d() +
    labs(
      title = paste(
        "7-day sliding window of effective reproduction number up to",
        format(max(covid_effective_r$date), "%d %B %Y")
      ),
      subtitle = "Outbreak is under control if effective R is under red line",
      x = "End date of 7-day sliding window",
      y = "Effective R (log scale)",
      caption = "CC BY-NC-SA
               Tim Churches (UNSW)
               Nick Tierney (Monash)
               Data source: European CDC"
    ) +
    theme_minimal() +
    theme(legend.position = "none")

}

#' @name effective-repro
#' @export
gg_effective_repro_facet <- function(covid_effective_r){

  ggplot(covid_effective_r,
       aes(x = date,
           y = mean_r,
           colour = country_region)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 1.0, colour = "red") +
  facet_wrap( ~ country_region, ncol = 2) +
  scale_y_log10() +
  scale_x_date(date_breaks = "1 week",
               date_labels = "%d %b") +
  labs(
    title = paste(
      "7-day sliding window of effective reproduction number up to",
      format(max(covid_effective_r$date), "%d %B %Y")
    ),
    subtitle = "Outbreak is under control if effective R is under red line",
    x = "End date of 7-day sliding window",
    y = "Effective R (log scale)",
    caption = "CC BY-NC-SA
               Tim Churches (UNSW)
               Nick Tierney (Monash)
               Data source: European CDC"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

}

#' Base plot for creating cumulative cases of covid19 data
#'
#' @param covid_data_limit - covid19 with added limit
#'   (from [add_days_since_limit()])
#' @param limit the number of days since reached a limit (added for
#'   titling graphic). Default is 100.
#'
#' @return ggplot plot
#' @import ggplot2
#' @export
#' @examples
#' \dontrun{
#'  covid_data_since <- covid_data %>%
#'    add_days_since_limit(limit = 100) %>%
#'    dplyr::filter(days_since_limit >= 0) %>%
#'    dplyr::filter(country_region %in% c("Australia", "New Zealand"))
#'
#' gg_covid_cumulative_exceed_limit(covid_data_since)
#' }
gg_covid_cumulative_exceed_limit <- function(covid_data_limit,
                                             limit = 100){

  covid_data_last <- covid_data_limit %>%
    dplyr::group_by(geo_id) %>%
    dplyr::filter(date == dplyr::last(date))

  ggplot(data = covid_data_limit,
         aes(x = days_since_limit,
             y = cumulative_cases,
             colour = country_region)) +
    geom_line(size = 1, alpha = 0.75) +
    scale_y_log10(labels = scales::comma) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
    theme_minimal() +
    labs(y = "Cumulative cases (logarithmic scale)",
         x = glue::glue("Days since cumulative cases exceeded {limit}"),
         title = create_title_date(covid_data_limit)) +
    ggrepel::geom_text_repel(data = covid_data_last,
                              aes(label = underscore_to_space(country_region)),
                             size = 4,
                              nudge_x = 4,
                              # direction = "x",
                              segment.alpha = 0.2,
                              segment.size = 0.2
                             ) +
    geom_point(data = covid_data_last,
               size = 2,
               alpha = 0.75) +
    scale_colour_brewer(palette = "Paired") +
    theme(legend.position = "none") +
    labs(caption =
           "CC BY-NC-SA Tim Churches (UNSW)
  Nick Tierney (Monash)
  Data source: European CDC"
    )
}
