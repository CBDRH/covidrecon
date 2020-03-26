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
