library(here)
library(tidyverse)
library(dplyr)

source(file = "config/config.R")

extract_relative_change = function(data){
  
  # browser()
  data_df = data %>%
    mutate(total_obesity = obese + `morbidly obese`)
  
  baseline_total_obesity = data_df %>%
    filter(type == "Year 0") %>%
    pull(total_obesity)
  
  data_df = data_df %>%
    mutate(absolute_change = baseline_total_obesity - total_obesity) %>%
    mutate(relative_change = (absolute_change/baseline_total_obesity)*100)
  
  
  relative_change_obesity_prevalence = data_df %>%
    filter(type == "Year 5") %>%
    pull(relative_change)
  
  print(paste0("Relative reduction in obesity prevalence = ", round(relative_change_obesity_prevalence, 1), "%"))
  
  return(data_df)
  
  
}


extract_pound_benefit = function(data, cost, duration ){
  # browser()
  data_df = data %>%
    mutate(value_to_gov_per_year = (relative_change * cost)/100)
  
  average_annual_value_to_gov = data_df %>%
    select(value_to_gov_per_year) %>%
    sum()/duration
  
  print(paste0("Average annual value to government compared to baseline = £", round(average_annual_value_to_gov, 2), "billions"))
  
  return(data_df)
  
  
}

