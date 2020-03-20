library(tidyverse)
library(lubridate)
covid_read <- function(){
  readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv",
           col_types = cols(
             .default = col_double(),
             `Province/State` = col_character(),
             `Country/Region` = col_character()
           ))
}

covid_clean_names <- function(x){
  x %>%
    tibble::as_tibble(.name_repair = janitor::make_clean_names) %>%
    dplyr::rename(province = province_state)
}

covid_pivot_longer <- function(x){
  tidyr::pivot_longer(data = x,
                      cols = -c(province:long),
                      names_to = "date",
                      values_to="cumulative_cases")
}

str_remove_x <- function(x) str_remove_all(x, "x")

covid_clean_date <- function(x) lubridate::mdy(str_remove_x(x))

covid_pull_data <- function(){
  covid_read() %>%
    covid_clean_names() %>%
    covid_pivot_longer() %>%
    dplyr::mutate(date = covid_clean_date(date)) %>%
    arrange(country_region,
            province,
            date) %>%
    group_by(country_region, province) %>%
    mutate(incident_cases = c(0, diff(cumulative_cases))) %>%
    ungroup()
}

provinces_confirmed_jh <- covid_pull_data()

incidence_countries <- provinces_confirmed_jh %>%
  group_by(country_region, date) %>%
  summarise(incident_cases = sum(incident_cases),
            cumulative_cases = sum(cumulative_cases))

incidence_global <- provinces_confirmed_jh %>%
  group_by(date) %>%
  summarise(incident_cases = sum(incident_cases),
            cumulative_cases = sum(cumulative_cases))

# find dates on which 100 cumulative cases occurred
high_incidence_countries <- incidence_countries %>%
  arrange(country_region, date) %>%
  mutate(lag_cum_cases = lag(cumulative_cases),
         hit_100 = if_else(
           condition = (cumulative_cases >= 100) & (lag(cumulative_cases) < 100),
           true = TRUE,
           false = FALSE))

high_incidence_countries <-  %>%
  left_join(x = high_incidence_countries,
            y = {
              high_incidence_countries %>%
                group_by(country_region) %>%
                summarise(over_100 = any(hit_100))
            }
  ) %>%
  ungroup() %>%
  filter(over_100 == TRUE)

high_incidence_countries <- high_incidence_countries %>%
  arrange(country_region, Date) %>%
  group_by(country_region) %>%
  mutate(rnum = row_number()) %>%
  ungroup()

high_incidence_countries <- high_incidence_countries %>%
  left_join(high_incidence_countries %>%
              filter(hit_100) %>%
              mutate(offset = rnum) %>%
              select(country_region, offset)) %>%
  mutate(normalised_date = rnum - offset) %>%
  select(-c(lag_cum_cases, over_100, offset)) %>%
  mutate(alpha = ifelse(country_region == "Australia", 1, 0.7))

high_incidence_countries %>%
  filter(normalised_date >= 0) %>%
  ggplot(aes(x=normalised_date, y=cumulative_cases,
             colour=country_region)) +
  geom_line(aes(alpha=alpha), size=1.2) +
  scale_y_log10() +
  theme_minimal() +
  theme(legend.position = "none")
