summarise_incident_cumulative <- function(data){
  data %>%
    summarise(incident_cases = sum(incident_cases),
              cumulative_cases = sum(cumulative_cases))
}
