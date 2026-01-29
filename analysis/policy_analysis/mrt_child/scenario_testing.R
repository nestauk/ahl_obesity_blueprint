
# Script for running policy scenarios and comparing 
# HSE and Nesta estimated baseline prevalence for HSE 2022

# Setup:
rm(list = ls())
library(tidyverse)
library(here)
library(writexl)
library(readxl)
library(aws.s3)
library(arrow)

source(file = "requirements.R")
source(file = "analysis/policy_analysis/mrt_child/mrt_child_utils.R")
source(file = "analysis/policy_analysis/mrt_child/load_data_files.R")

# Part A: Scenarios for testing:
# Please see point 3 on scenario and sensitivity testing here: https://docs.google.com/document/d/1_eE3OGPhF6VQmnzzY8NLCb9gmGtUT4y9gS_COxisssw/edit?tab=t.ik9c2fwwo40t
scenarios = tibble(
  daily_ei_change = c(78.6, 78.6, 78.6 * 0.9, 78.6 * 1.1, 78.6 * 0.85, 78.6 * 1.15),
  compensation_factor = c(0, 0.23, 0.23, 0.23, 0.23, 0.23),
  tags = c("78.6 kcals without compensation",
           "78.6 kcals with compensation",
           "78.6 kcals -10% with compensation",
           "78.6 kcals +10% with compensation",
           "78.6 kcals -15% with compensation",
           "78.6 kcals +15% with compensation"))



# runnng scenarios
results = run_policy_scenarios(df = hse_2022_child,
                               scenarios = scenarios,
                               effect_weight_df = new_effect_weighting)

print(results)

  
  
# Part B: Baseline comparisons:
# Please see point 4 about age grouping here: https://docs.google.com/document/d/1_eE3OGPhF6VQmnzzY8NLCb9gmGtUT4y9gS_COxisssw/edit?tab=t.ik9c2fwwo40t
policy_impact_baseline_comparison = calculate_bmi_from_eichange_hox(df = hse_2022_child,
                                                                      daily_ei_change = 78.6,
                                                                      compensation_factor = 0,
                                                                      effect_weighting_df = new_effect_weighting,
                                                                      nation = "England", 
                                                                      tags = "Baseline comparison" )

detailed_output = policy_impact_baseline_comparison$post_df

child_baseline_bmi_cats = rbind(
  detailed_output %>% 
    count(child_bmi_category, wt = wt_int) %>% 
    mutate(freq = round(n/sum(n)*100,1),
           type = "Baseline_hse") %>% 
    rename(BMI = child_bmi_category) %>%
    left_join(detailed_output %>% count(child_bmi_category) %>% rename(BMI = child_bmi_category, n_unweighted = n)),
  detailed_output %>% 
    count(baseline_bmi_category, wt = wt_int) %>% 
    mutate(freq = round(n/sum(n)*100, 1),
           type = "Baseline_nesta") %>% 
    rename(BMI = baseline_bmi_category) %>%
    left_join(detailed_output %>% count(baseline_bmi_category) %>% rename(BMI = baseline_bmi_category, n_unweighted = n)),
  detailed_output %>% 
    count(post_bmi_category, wt = wt_int) %>% 
    mutate(freq = round(n/sum(n)*100, 1),
           type = "Post-Implementation") %>% 
    rename(BMI = post_bmi_category) %>%
    left_join(detailed_output %>% count(post_bmi_category) %>% rename(BMI = post_bmi_category, n_unweighted = n))
)

print(child_baseline_bmi_cats)



