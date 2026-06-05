
#################################################################################################                      #
# MRT child impact modelling                                                                    #
#################################################################################################

# This is the script used to produce the England and Scotland child impact numbers
# see link: https://docs.google.com/document/d/1_eE3OGPhF6VQmnzzY8NLCb9gmGtUT4y9gS_COxisssw/edit?tab=t.uynfnhg45mrd


# Effect size/ change in daily energy intake (DEI):
# The change in energy intake = 83 kcals without compensation & 64 kcals with compensation
# Source: Scenario D1 (Main) in https://docs.google.com/spreadsheets/d/1tizxBbYHBedkmKfWfrEBCIO56sjHwX3uHZlwB5TB40M/edit?usp=sharing



# Setup and function:
rm(list = ls())
library(tidyverse)
library(here)
library(writexl)
library(readxl)
library(aws.s3)
library(arrow)

source(file = "requirements.R")
source(file = "analysis/policy_analysis/mrt_child/mrt_child_utils.R")
source(file = "analysis/policy_analysis/mrt_child/load_data_files.R") # all required data files are loaded from here


# inputs
# Kcal reduction per person per day without compensation:
kcal_reduction_without_compensation = 83

# Kcal reduction per person per day with compensation
kcal_reduction_with_compensation = 64
compensation_factor = 0
effect_weighting_factors = effect_weighting_16

# main ()

# running model with HSE 2019 data
policy_impact_england = calculate_bmi_from_eichange_hox(df = hse_2019_child %>% rename(baseline_intake = intake_hox),
                                                        daily_ei_change = kcal_reduction_without_compensation,
                                                        compensation_factor = compensation_factor,
                                                        effect_weighting_df = new_effect_weighting,
                                                        nation = "England", 
                                                        tags = "HFS - Without compensation" )


# running model with Scotland data
policy_impact_scotland = calculate_bmi_from_eichange_hox(df = shes_2019_child %>% rename(baseline_intake = intake_hox),
                                                         daily_ei_change = kcal_reduction_without_compensation,
                                                         compensation_factor = compensation_factor,
                                                         effect_weighting_df = new_effect_weighting,
                                                         nation = "Scotland", 
                                                         tags = "HFS - Without compensation" )


# Print reduction in child obesity prevalence:
calc_percent_reduction(policy_impact_england$bmi_prevalence_table, "obese")
calc_percent_reduction(policy_impact_scotland$bmi_prevalence_table, "obese")




policy_impact_england_with_comp = calculate_bmi_from_eichange_hox(
  df = hse_2019_child %>% rename(baseline_intake = intake_hox),
  daily_ei_change = kcal_reduction_with_compensation,
  compensation_factor = compensation_factor,
  effect_weighting_df = new_effect_weighting,
  nation = "England",
  tags = "HFS - With compensation")


policy_impact_scotland_with_comp = calculate_bmi_from_eichange_hox(
  df = shes_2019_child %>% rename(baseline_intake = intake_hox),
  daily_ei_change = kcal_reduction_with_compensation,
  compensation_factor = compensation_factor,
  effect_weighting_df = new_effect_weighting,
  nation = "Scotland",
  tags = "HFS - With compensation" )

print("% reduction in obesity - England - with compensation:")
calc_percent_reduction(policy_impact_england_with_comp$bmi_prevalence_table, "obese")
calc_percent_reduction(policy_impact_scotland_with_comp$bmi_prevalence_table, "obese")


# additional descriptive plots:
# for understaning why Scotland has a slightly higher impact than England:

cols_to_keep = c("id", "age_grp", "age", "sex", "wt_int", "weight", "height",  "bmi", "baseline_intake")

combined_hse_shes_2019 <- bind_rows(hse_2019_child %>%
                                      rename(baseline_intake = intake_hox) %>%
                                      select(all_of(cols_to_keep)) %>%
                                      mutate(nation = "England"),
                                    shes_2019_child %>%
                                      rename(baseline_intake = intake_hox) %>%
                                      select(all_of(cols_to_keep)) %>%
                                      mutate(nation = "Scotland"))


plot_density_by_nation(df = combined_hse_shes_2019, var = weight)

plot_density_by_nation(df = combined_hse_shes_2019, var = baseline_intake)
