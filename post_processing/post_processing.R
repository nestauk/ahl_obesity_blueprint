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


extract_value_to_gov = function(data, cost, duration, print_results = FALSE ){
  # browser()
  outputs = list()
  data_df = data %>%
    mutate(value_to_gov_per_year = (relative_change * cost)/100)
  
  average_annual_value_to_gov = data_df %>%
    select(value_to_gov_per_year) %>%
    sum()/duration
  
  outputs[["avg_annual_value"]] = average_annual_value_to_gov
  outputs[["full_table"]] = data_df
  
  if (print_results != FALSE) {
    
    print(paste0("Average annual value to government compared to baseline = £", round(average_annual_value_to_gov, 2), "billions"))
  }
  
  return(outputs)
  
}


extract_pound_benefit_by_class = function(data,
                                          total_cost = 74,
                                          cost_30_40,
                                          cost_over_40,
                                          duration, 
                                          option = "option_1" ){
  # browser()
  data_df = data
  
  baseline_total_bmi_30_40 = data_df %>%
    filter(type == "Year 0") %>%
    pull(obese)
  
  baseline_total_bmi_over_40 = data_df %>%
    filter(type == "Year 0") %>%
    pull(`morbidly obese`)
  
  baseline_total_obesity = baseline_total_bmi_30_40 + baseline_total_bmi_over_40
  
  data_df = data_df %>%
    mutate(absolute_change_bmi_30_40 = baseline_total_bmi_30_40 - obese) %>%
    mutate(absolute_change_bmi_over_40 = baseline_total_bmi_over_40 - `morbidly obese`) %>%
    mutate(relative_change_bmi_30_40 = (absolute_change_bmi_30_40/baseline_total_bmi_30_40)*100) %>%
    mutate(relative_change_bmi_over_40 = (absolute_change_bmi_over_40/baseline_total_bmi_over_40)*100) %>%
    mutate(total_obesity = `obese` + `morbidly obese`) %>%
    mutate(total_absolute_change = baseline_total_obesity - total_obesity + absolute_change_bmi_over_40) %>%
    mutate(relative_absolute_change = (total_absolute_change/baseline_total_obesity)*100)
  
  if (option == "option_1") {
    
    data_df = data_df %>%
      mutate(value_to_gov_per_year = (relative_absolute_change * total_cost)/100)
    
    average_annual_value_to_gov = data_df %>%
      select(value_to_gov_per_year) %>%
      sum()/duration
    
    total_average_annual_benefit = average_annual_value_to_gov
    
  }
  
  else {
  
  data_df = data_df %>%
    mutate(value_to_gov_per_year_30_40 = (relative_change_bmi_30_40 * cost_30_40)/100) %>%
    mutate(value_to_gov_per_year_over_40 = (relative_change_bmi_over_40 * cost_over_40)/100)
  
  average_annual_value_to_gov_30_40 = data_df %>%
    select(value_to_gov_per_year_30_40) %>%
    sum()/duration
  
  average_annual_value_to_gov_over_40 = data_df %>%
    select(value_to_gov_per_year_over_40) %>%
    sum()/duration
  
  total_average_annual_benefit = average_annual_value_to_gov_30_40 + average_annual_value_to_gov_over_40
  
  
  }
  

  return(total_average_annual_benefit)
  
  
}


extract_relative_change = function(data, print_results = FALSE){
  
  # browser()
  data_df = data %>%
    mutate(total_obesity = obese + `morbidly obese`)
  
  baseline_total_obesity = data_df %>%
    filter(type == "Year 0") %>%
    pull(total_obesity)
  
  baseline_class_3_obesity = data_df %>%
    filter(type == "Year 0") %>%
    pull(`morbidly obese`)
  
  data_df = data_df %>%
    mutate(absolute_change = baseline_total_obesity - total_obesity) %>%
    mutate(relative_change = (absolute_change/baseline_total_obesity)*100) %>%
    mutate(absolute_class_3 = baseline_class_3_obesity - `morbidly obese`) %>%
    mutate(relative_class_3 = (absolute_class_3/baseline_class_3_obesity)*100)
  
  
  relative_change_obesity_prevalence = data_df %>%
    filter(type == "Year 5") %>%
    pull(relative_change)
  
  relative_change_class_3 = data_df %>%
    filter(type == "Year 5") %>%
    pull(relative_class_3)
  
  if (print_results != FALSE)
    {  
  print(paste0("Relative reduction in obesity prevalence = ", round(relative_change_obesity_prevalence, 2), "%"))
  print(paste0("Relative reduction in Class 3 obesity prevalence = ", round(relative_change_class_3, 2), "%"))
  }
  
  return(data_df)
  
  
}



