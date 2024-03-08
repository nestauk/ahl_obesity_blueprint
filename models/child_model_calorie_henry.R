


#rm(list = ls())

library(tidyverse)
library(here)
library(bw)
library(survey)
library(sitar)


source(file = "models/model_utils.R")

# calculate_bmi_from_ei_change(df = read_csv(here("inputs/processed/hse_2019_children.csv")), intake_change = 50)

calculate_bmi_from_ei_change = function(df, daily_ei_change){
  
  print("This model uses Henry equations")
  
  #browser()
  output_list = list()

  effect_weighting = read.csv(here("inputs/ref_data/effect_weighting.csv"))
  
  library(sitar)
  
  uk90_bmi_refdata_3centiles = generate_bmi_refdata(sitar::uk90)
  
  uk90_bmi_refdata_100centiles = generate_bmi_refdata_100centiles(sitar::uk90)
  
  uk90_bmi_refdata_100centiles = uk90_bmi_refdata_100centiles %>%
    select(-c(L.bmi, M.bmi, S.bmi)) %>%
    unite(col = "sex_years", "sex", "years", sep = "_") %>%
    pivot_longer(cols= c(starts_with("p")), names_to = "centile", values_to = "bmi") %>%
    separate("sex_years", into = c("sex", "age"), sep = "_") %>%
    mutate(centile = substr(centile, 3, nchar(centile)))
  
  
  # read_csv(here("inputs/processed/hse_2019_children.csv"))
  df <- df %>%
    rowwise() %>%
    mutate(baseline_bmi_category = calculate_bmi_category(age = age, sex = sex, bmi = bmi, df_B = uk90_bmi_refdata_3centiles)) %>%
    mutate(bmi_centile = look_up_percentile(age = age, sex = sex, bmi = bmi, data_B = uk90_bmi_refdata_100centiles)) %>%
    mutate(intake_change = ifelse(baseline_bmi_category %in% c("normal", "overweight", "obese"),
                                  calculate_proportional_ei_change(age = age, sex = sex, bmi = bmi, intake_change = -daily_ei_change, 
                                                          prop_weight_data = effect_weighting, bmi_ref_data = uk90_bmi_refdata_100centiles), 0)) %>%
    ungroup() %>%
    mutate(policy_period_energy_intake = intake_hox + intake_change) %>%
    mutate(policy_period_rmr = policy_period_energy_intake/pal) %>%
    mutate(post_policy_weight = case_when(sex == 1 ~ (((policy_period_rmr * 4.184) - 2876)/66.9),
                                          sex == 2 ~ (((policy_period_rmr * 4.184) - 3230)/47.9))) %>%
    mutate(weight_diff = weight - post_policy_weight) %>%
    mutate(post_policy_bmi = post_policy_weight/(height/100)^2) %>%
    rowwise() %>%
    mutate(post_bmi_category = calculate_bmi_category(age = age, sex = sex, bmi = post_policy_bmi, df_B = uk90_bmi_refdata_3centiles)) %>%
    ungroup() %>%
    mutate(bmi_diff = bmi - post_policy_bmi)
  
  
  output_list[["post_df"]] = df

  child_bmi_change = rbind(
    df %>% 
      count(baseline_bmi_category, wt = wt_int) %>% 
      mutate(freq = n/sum(n)*100,
             type = "Baseline") %>% 
      rename(BMI = baseline_bmi_category),
    df %>% 
      count(post_bmi_category, wt = wt_int) %>% 
      mutate(freq = n/sum(n)*100,
             type = "Endline") %>% 
      rename(BMI = post_bmi_category))
  
  
  
  child_bmi_change = child_bmi_change %>%
    mutate(BMI = factor(BMI, levels = c("underweight", "normal", "overweight", "obese"))) %>%
    as.data.frame()
  
  
  # output bar plot of BMI categories distributions
  child_bar_plot = child_bmi_change %>%
    ggplot(., aes(y = freq, x = BMI, fill = type)) + 
    geom_bar(stat = "identity", position = "dodge") +
    theme_ipsum() +
    labs(fill = "", 
         title = "BMI Distribution:", 
         y = "Prevalence - %",
         subtitle = "Children - Proportional kcals") +
    theme_ipsum(base_size = 8, axis_title_size = 8) + #, base_family="Averta"
    theme(legend.position = "top")
  
  
  output_list[["bmi_prevalence_plot"]] = child_bar_plot
  
  
  
  # Output 2: Table of year wise prevalence of obesity
  child_bmi_change_year = child_bmi_change %>%
    select(-c(n)) %>%
    pivot_wider(., names_from = BMI, values_from = freq)
  
  output_list[["bmi_prevalence_table"]] = child_bmi_change_year
  
  return(output_list)
  
}
