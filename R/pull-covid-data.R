#' read in raw covid19 data
#'
#' @return dataframe
#' @export
covid_read <- function(){
  # code adapted from https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide

  fetch_ecdc_data <- function(date_to_try) {
    url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-", format(date_to_try, "%Y-%m-%d"), ".xlsx", sep = "")
    # download the dataset from the website to a local temporary file
    GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
    #read the Dataset sheet and return the data frame
    df <- read_excel(tf)
    message(paste("Successfully obtained data for", format(date_to_try, "%Y-%m-%d")))
    return(df)
  }

  # first try today's date
  ecdc_data <- tryCatch(fetch_ecdc_data(Sys.time()),
                        error=function(c) {
                          message(paste("Unable to obtain data file for", format(Sys.time(), "%Y-%m-%d")))
                        })
  return(ecdc_data)
}

#' clean variable names of covid19 raw data (from `covid_read`)
#'
#' @param x covid19 raw data
#'
#' @return data.frame with tidied names
#' @export
covid_clean_names <- function(x){
  x %>%
    tibble::as_tibble(.name_repair = janitor::make_clean_names) %>%
    dplyr::rename(province = province_state)
}

#' pivots the covid19 data into longer form
#'
#' @param x data that has been imported and cleaned
#'
#' @return longer form pivoted data
#' @export
covid_pivot_longer <- function(x){
  tidyr::pivot_longer(data = x,
                      cols = -c(province:long),
                      names_to = "date",
                      values_to="cumulative_cases")
}


#' Pulls and tidies the latest covid19 data
#'
#' @return covid19 data with columns..
#' @export
covid_pull_data <- function(){
  covid_read() %>%
    covid_clean_names() %>%
    covid_pivot_longer() %>%
    dplyr::mutate(date = covid_clean_date(date)) %>%
    dplyr::arrange(country_region,
                   province,
                   date) %>%
    dplyr::group_by(country_region, province) %>%
    dplyr::mutate(incident_cases = c(0, diff(cumulative_cases))) %>%
    dplyr::ungroup() %>%
    add_continent()

}


#' Filter covid countries to provided countries
#'
#' @param data covid19 data
#' @param countries vector of countries
#'
#' @return dataset filtered to existing countries
#' @export
covid_filter_countres <- function(data, countries){
  data %>%
    dplyr::filter(country_region %in% countries)
}
