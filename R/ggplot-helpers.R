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
    scale_y_log10() +
    theme(panel.grid.minor = element_blank())
}

#' @export
#' @name gg-covid
gg_covid_cumulative_cases <- function(covid_data){
  ggplot(data = covid_data,
         aes(x = date,
             y = cumulative_cases,
             group = country_region)) +
    geom_line() +
    scale_y_log10() +
    theme(panel.grid.minor = element_blank())
}


#' Plot effective reproduction number
#'
#' @param covid_effective_r covid19 data with estimated effective R
#' @param highlight character vector of countries to highlight. Default, "Australia"
#'
#' @return ggplot2 plot
#' @rdname effective-repro
#' @export
gg_effective_repro_all <- function(covid_effective_r,
                                   highlight = "Australia"){

  covid_effective_r <- covid_effective_r %>%
    dplyr::mutate(
      alfa = dplyr::if_else(
        condition = country_region == highlight,
        true = 1.0,
        false = 0.5
        ),
      alfa2 = dplyr::if_else(
        condition = country_region == highlight,
        true = 1.0,
        false = 0.75
        )
      )

  last_country_eff_rs <- filter_last_country_date(covid_effective_r)

  last_date <- format(max(covid_effective_r$date), "%d %B %Y")

  covid_effective_r %>%
    ggplot(aes(x = date,
               y = median_r,
               colour = country_region)) +
    geom_line(aes(alpha=alfa),
              size = 1) +
    geom_point(data = last_country_eff_rs,
               size = 2,
               alpha = 0.75) +
    geom_hline(yintercept = 1.0, colour = "red") +
    ggrepel::geom_label_repel(
      data = last_country_eff_rs,
      aes(label = country_region, alpha = alfa2),
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
    scale_alpha(range=c(0.4, 1.0)) +
    labs(
      title = expression(paste(
        "7-day sliding window of effective reproduction number ", R[t])
      ),
      subtitle = expression(paste("Epidemic is in decay phase if ", R[t], "  is under red line")),
      x = "End date of 7-day sliding window",
      y = expression(paste("Effective reproduction number ", R[t], " (log scale)")),
      caption = paste("Tim Churches (UNSW) & Nick Tierney (Monash)
               Data source: European CDC up to", last_date)
    ) +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.minor = element_blank())

}

#' @name effective-repro
#' @export
gg_effective_repro_facet <- function(covid_effective_r){

  ggplot(covid_effective_r,
       aes(x = date,
           y = median_r,
           colour = country_region)) +
    geom_line(size = 1,
              alpha = 0.75) +
    geom_point(data = filter_last_country_date(covid_effective_r),
               size = 2,
               alpha = 0.75) +
  geom_hline(yintercept = 1.0, colour = "red") +
  facet_wrap( ~ country_region, ncol = 3) +
  scale_y_log10() +
  scale_x_date(date_breaks = "1 week",
               date_labels = "%d %b") +
  labs(
    title = expression(paste(
        "7-day sliding window of effective reproduction number ", R[t])
    ),
    subtitle = expression(paste("Epidemic is in decay phase if ", R[t], "  is under red line")),
    x = "End date of 7-day sliding window",
    y = expression(paste("Effective reproduction number ", R[t], " (log scale)")),
    caption = paste("Tim Churches (UNSW) & Nick Tierney (Monash)
               Data source: European CDC up to", format(max(covid_effective_r$date), "%d %B %Y"))
  ) +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.minor = element_blank())

}

#' Effective Reproductive pathwork
#'
#' @param covid_data covid19 data
#' @param covid_effective_r covid19 data with effective repro added with
#'   `covid_estimate_repro()`.
#' @param country country to display
#' @export
gg_effective_repro_incidence_patchwork <- function(covid_effective_r,
                                                   covid_data,
                                                   country){

      rplot <- covid_effective_r %>%
        dplyr::filter(country_region == country) %>%
        ggplot(
           aes(x = date,
               y = median_r,
               colour = country_region)) +
        geom_line(size = 1,
                  alpha = 0.75) +
        geom_point(data = filter_last_country_date(
                              covid_effective_r %>%
                                dplyr::filter(country_region == country)
                          ),
                   size = 2,
                   alpha = 0.75) +
      geom_hline(yintercept = 1.0, colour = "red") +
      scale_y_log10() +
      scale_x_date(date_breaks = "1 week",
                   date_labels = "%d %b") +
      labs(
        title = expression(paste(
            "7-day sliding window of effective reproduction number ", R[t])
        ),
        subtitle = expression(paste("Epidemic is in decay phase if ", R[t], "  is under red line")),
        x = "End date of 7-day sliding window",
        y = expression(paste( R[t], " (log scale)"))
      ) +
        theme_minimal() +
        theme(legend.position = "none",
              panel.grid.minor = element_blank()) +
        theme(axis.text.x=element_text(angle=45, hjust=1))

      first_date <- covid_effective_r %>%
          dplyr::filter(country_region == country) %>%
          dplyr::summarise(
            min_date = min(date) - lubridate::days(7)
            ) %>%
          dplyr::pull(min_date)

      iplot <-  covid_data %>%
        dplyr::mutate(date = as.Date(date)) %>%
        dplyr::filter(country_region == country,
               date >= first_date) %>%
        ggplot(
           aes(x = date,
               y = cases,
               fill = country_region)) +
      geom_col(stat="identity",
                  alpha = 0.75) +
      scale_x_date(date_breaks = "1 week",
                   date_labels = "%d %b") +
      scale_y_log10() +
      labs(
        title = "Incidence",
        x = "Date",
        y = "Incident cases",
        caption = paste("Tim Churches (UNSW) & Nick Tierney (Monash)
                   Data source: European CDC up to", format(max(covid_effective_r$date), "%d %B %Y"))
      ) +
        theme_minimal() +
        theme(legend.position = "none",
              panel.grid.minor = element_blank()) +
        theme(axis.text.x=element_text(angle=45, hjust=1))

      return( rplot / iplot )

}

#' Base plot for creating cumulative cases of covid19 data
#'
#' @param covid_data_limit - covid19 with added limit
#'   (from [add_days_since_limit()])
#' @param limit the number of days since reached a limit (added for
#'   titling graphic). Default is 100.
#' @param highlight the name of the country to highlight, default is Australia.
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
                                             limit = NULL,
                                             highlight = "Australia"){

  covid_data_last <- filter_last_country_date(covid_data_limit)

  if (!is.null(limit)) {
    x_axis_lab <- glue::glue("Days since cumulative cases exceeded {limit}")
  } else {
    x_axis_lab <- "Days since commencement of epidemic in each country"
  }

  covid_data_limit %>%
    dplyr::mutate(
      alfa = dplyr::if_else(
        condition = country_region == highlight,
        true = 1,
        false = 0.5
        )
      ) %>%
    ggplot(aes(x = days_since_limit,
             y = cumulative_cases,
             colour = country_region)) +
    geom_line(aes(alpha=alfa), size = 1) +
    geom_point(data = covid_data_last,
               size = 2) +
    scale_y_log10(labels = scales::comma) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
    scale_alpha(range=c(0.3, 1)) +
    theme_minimal() +
    labs(y = "Cumulative cases (logarithmic scale)",
         x = x_axis_lab,
         title = create_title_date(covid_data_limit)) +
    ggrepel::geom_text_repel(data = covid_data_last,
                              aes(label = underscore_to_space(country_region)),
                             size = 4,
                              nudge_x = 4,
                              # direction = "x",
                              segment.alpha = 0.2,
                              segment.size = 0.2
                             ) +
    theme(legend.position = "none") +
    labs(caption =
           "Tim Churches (UNSW) & Nick Tierney (Monash)
            Data source: European CDC"
    )
}

#' Base plot for creating cumulative cases vs deaths of covid19 data
#'
#' @param covid_data_limit - covid19 with added limit
#'   (from [add_days_since_limit()])
#' @param limit the number of days since reached a limit (added for
#'   titling graphic). Default is 100.
#' @param highlight the name of the country to highlight, default is Australia.
#'
#' @return ggplot plot
#' @import ggplot2
#' @export
gg_covid_cumulative_cases_deaths_exceed_limit <- function(covid_data_limit,
                                             limit = NULL,
                                             highlight = "Australia"){

  covid_data_last <- filter_last_country_date(covid_data_limit)

  if (!is.null(limit)) {
    x_axis_lab <- glue::glue("Days since cumulative cases exceeded {limit}")
  } else {
    x_axis_lab <- "Days since commencement of epidemic in each country"
  }

  covid_data_limit %>%
    dplyr::mutate(
      alfa = dplyr::if_else(
        condition = country_region == highlight,
        true = 1,
        false = 0.5
        )
      ) %>%
    ggplot(aes(x = days_since_limit,
             y = cumulative_cases)) +
    geom_line(aes(colour=cumulative_deaths), size = 2) +
    geom_point(data = covid_data_last,
               aes(colour=cumulative_deaths),
               size = 3) +
    facet_wrap(country_region~.) +
    scale_y_log10(labels = scales::comma) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
    # scale_alpha(range=c(0.3, 1)) +
    theme_minimal() +
    labs(y = "Cumulative cases (logarithmic scale)",
         x = x_axis_lab,
         title = create_title_date(covid_data_limit)) +
    # geom_text(data = covid_data_last,
    #                           aes(label = underscore_to_space(country_region)),
    #                          size = 4) +
    theme(legend.position = "right") +
    labs(caption =
           "Tim Churches (UNSW) & Nick Tierney (Monash)
            Data source: European CDC"
    )
}

#' Base plot for creating incident cases chart of covid19 data
#'
#' @param covid_data_limit - covid19 with added limit
#'   (from [add_days_since_limit()])
#' @param limit the number of days since reached a limit (added for
#'   titling graphic). Default is 100.
#' @param highlight the name of the country to highlight, default is Australia.
#' @param smooth add smoother, default is false
#' @param span span numeric, span to add to smoother.
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
gg_covid_incidence_exceed_limit <- function(covid_data_limit,
                                             limit = NULL,
                                             highlight = "Australia",
                                             smooth=FALSE,
                                             span=0.1){

  if (!is.null(limit)) {
    x_axis_lab <- glue::glue("Days since daily incident cases exceeded {limit}")
  } else {
    x_axis_lab <- "Days since commencement of epidemic in each country"
  }

  if (TRUE) { # was if (smooth)
    covid_data_limit <- covid_data_limit %>%
      dplyr::mutate(
        cases = ifelse(
          test = cases == 0,
          yes = NA,
          no = cases
          )
        ) %>%
      dplyr::group_by(country_region) %>%
      dplyr::filter(!(days_since_limit == max(days_since_limit) & is.na(cases)))
  }

  covid_data_last <- filter_last_country_date(covid_data_limit)

  p1 <- covid_data_limit %>%
    dplyr::mutate(
      alfa = dplyr::if_else(
        condition = country_region == highlight,
        true = 1,
        false = 0.5
        )
      ) %>%
    ggplot(aes(x = days_since_limit,
             y = cases,
             colour = country_region))

  if (!smooth) {
    p1 <- p1 +
      geom_line(aes(alpha=alfa), size = 1) +
      geom_point(data = covid_data_last,
                 size = 2)
    y_label <- "Daily incident cases (logarithmic scale)"
  } else {
    p1 <- p1 +
      geom_smooth(aes(alpha=alfa), se=FALSE, size = 1, span=span)
    y_label <- "Smoothed daily incident cases (logarithmic scale)"
  }

  p1 <- p1 +
    scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
    scale_alpha(range=c(0.3, 1)) +
    theme_minimal() +
    labs(y = "Daily incident cases (logarithmic scale)",
         x = x_axis_lab,
         title = create_title_date(covid_data_limit))

  if (!smooth) {
    p1 <- p1 +
    ggrepel::geom_text_repel(data = covid_data_last,
                              aes(label = underscore_to_space(country_region)),
                             size = 4,
                              nudge_x = 4,
                              # direction = "x",
                              segment.alpha = 0.2,
                              segment.size = 0.2
                             )
  } else {
     p1 <- p1 +
    ggrepel::geom_label_repel(data = covid_data_last,
                              aes(label = underscore_to_space(country_region)),
                             size = 4,
                              nudge_x = 4 ,
                              # direction = "x",
                              segment.alpha = 0.3,
                              # segment.size = 0.2,
                              # arrow = arrow(length = unit(0.02, "npc"))
                             )

  }

  p1 +
    theme(legend.position = "none") +
    labs(caption =
           "Tim Churches (UNSW) & Nick Tierney (Monash)
            Data source: European CDC"
    )
}
