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
#' @param covid_effective_r
#'
#' @return
#' @export
#'
#' @examples
#' @rdname effective-repro
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
      nudge_x = 0.2,
      xlim = c(
        max(covid_effective_r$date) + lubridate::days(1),
        max(covid_effective_r$date) + lubridate::days(5)
      )
    ) +
    scale_y_log10() +
    scale_x_date(
      date_breaks = "1 week",
      date_labels = "%d %b",
      expand = expansion(mult = c(0, 1))
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
    theme_dark() +
    theme(legend.position = "none")

}

#' @name effective-repro
#' @export
gg_effective_repro_facet <- function(covid_effective_repro){

  ggplot(covid_effective_repro,
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
      format(max(covid_effective_repro$date), "%d %B %Y")
    ),
    subtitle = "Outbreak is under control if effective R is under red line",
    x = "End date of 7-day sliding window",
    y = "Effective R (log scale)",
    caption = "CC BY-NC-SA
               Tim Churches (UNSW)
               Nick Tierney (Monash)
               Data source: European CDC"
  ) +
  theme_dark() +
  theme(legend.position = "none")

}
