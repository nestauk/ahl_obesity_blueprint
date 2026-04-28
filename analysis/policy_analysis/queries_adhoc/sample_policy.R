
#############################################################################################
# SAMPLE POLICY                                                                             #
#############################################################################################

# Description:

# The evidence from the rapid review 
# (https://docs.google.com/document/d/1wzYZyzhIDE4O1LPRjgLaG1piHQISzQkYK2RgI7Y3B34/edit?usp=sharing) 
# (quality assured by the EAG) showed that the interventions led to a reduction of 0 - 54 kcals in daily
# energy intake of adults. Given the large variation, it was assumed to be 27 kcals.


# setup
rm(list = ls())
gc()
library(tidyverse)
library(here)

source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")
source(file = "models/child_model_calorie.R")

table_outputs = list() # creating a list of table outputs to be saved as an excel file

# Estimating the impact of the policy in:

# 1. Adults in England:

# 1.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab", nation = "England", population_group = "Adult")

# 1.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: ﹣27 kcal
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 23% of [A] = 6.21
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -20.79 kcals

policy_7_impact_england_adult = calculate_bmi_from_eichange(df = read_csv(here("inputs/processed/hse_2019.csv")),
                                                            intake_change = -11,
                                                            implmentation_duration = 365*5)

# 1.3. Outputs

# Bar plot of change in year on year distribution of different BMI categories
policy_7_impact_england_adult$bmi_category_plot


# Output table with year on year distribution of BMI categories
table = policy_7_impact_england_adult$bmi_percent_prevalence

table_outputs[["england_adult"]] = policy_7_impact_england_adult$bmi_percent_prevalence


# 2. Adults in Scotland

# 2.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/shes19i_eul.tab", nation = "Scotland", population_group = "Adult")

# 2.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: ﹣27 kcal
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 23% of [A] = 6.21
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -20.79 kcals

policy_7_impact_scotland_adult = calculate_bmi_from_eichange(df = read_csv(here("inputs/processed/shes_2019.csv")),
                                                             intake_change = -20.79,
                                                             implmentation_duration = 365*5)
# 2.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_7_impact_scotland_adult$bmi_category_plot

# ggsave(here("outputs/policy_7/policy_7_impact_Scotland_adult.png"), 
#        plot = policy_7_impact_scotland_adult$bmi_category_plot, 
#        width = 10, 
#        height = 6,
#        bg='#ffffff')


# Output table with year on year distrubution of BMI categories
policy_7_impact_scotland_adult$bmi_percent_prevalence

table_outputs[["scotland_adult"]] = policy_7_impact_scotland_adult$bmi_percent_prevalence



test_df = df

test_df = test_df %>%
  mutate(eligibility = case_when(bmi >= 35 ~ 1,
                                 (bmi >= 30 & bmi < 35) & (cardiovd == 1 | diabetes == 1) ~ 1,
                                 (bmi >= 27.5 & ethnicity %in% c(2, 3)) ~ 1,    # , 4, 5
                                 TRUE ~ 0))


sum_nice_elig = sum(test_df$wt_int[test_df$eligibility == 1])
sum_nice_tot = sum(test_df$wt_int)
pop_nice = (sum_nice_elig/ sum_nice_tot) * 44263393


test_df_1 = df

test_df_1 = test_df_1 %>%
  mutate(eligibility = case_when(bmi >= 30 ~ 1,
                                 # (bmi >= 30 & bmi < 35) & (cardiovd == 1 | diabetes == 1) ~ 1,
                                 (bmi >= 27.5 & ethnicity %in% c(2, 3)) ~ 1,    # , 4, 5
                                 TRUE ~ 0))

sum_nesta_elig = sum(test_df_1$wt_int[test_df_1$eligibility == 1])
sum_nesta_tot = sum(test_df_1$wt_int)
pop_nesta = (sum_nesta_elig/ sum_nesta_tot) * 44263393
