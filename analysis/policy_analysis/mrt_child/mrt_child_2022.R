
#################################################################################################                      #
# MRT child impact modelling                                                                    #
#################################################################################################

# This is the script used to produce the England and Scotland child impact numbers
# see link: https://docs.google.com/document/d/1_eE3OGPhF6VQmnzzY8NLCb9gmGtUT4y9gS_COxisssw/edit?tab=t.uynfnhg45mrd


# Effect size/ change in daily energy intake (DEI):
# The change in energy intake = 78.61 kcals
# Source: Please see row 12, column E here - https://docs.google.com/spreadsheets/d/1QqID7tA-9ocNwHXX83C58qN2bShddMZJUJgguR4AqFw/edit?usp=sharing


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



# main ()

# running model with HSE 2019 data
policy_impact_england = calculate_bmi_from_eichange_hox(df = hse_2019_child %>% rename(baseline_intake = intake_hox),
                                                        daily_ei_change = 78.6,
                                                        compensation_factor = 0.23,
                                                        effect_weighting_df = new_effect_weighting,
                                                        nation = "England", 
                                                        tags = "MRT - New" )


# running model with Scotland data
policy_impact_scotland = calculate_bmi_from_eichange_hox(df = shes_2019_child %>% rename(baseline_intake = intake_hox),
                                                         daily_ei_change = 78.6,
                                                         compensation_factor = 0.23,
                                                         effect_weighting_df = new_effect_weighting,
                                                         nation = "Scotland", 
                                                         tags = "MRT - New" )


# Print reduction in child obesity prevalence:
calc_percent_reduction(policy_impact_england$bmi_prevalence_table, "obese")
calc_percent_reduction(policy_impact_scotland$bmi_prevalence_table, "obese")




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
