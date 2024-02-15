
#################################################################################################
# Policy 2 : Restrict all outdoor advertising on public transport (i.e. all bus stops, train    #
#            stations and tube stations, implemented via national regulation)                   #
#################################################################################################

# Description:


# The evidence from the rapid review  
# (https://docs.google.com/document/d/1U1JH_KI8IGBaN4cNXyW1A0H_k5LmW94C1VBATPDD8jA/edit?usp=sharing) 
# (quality assured by the EAG) showed that the intervention reduced daily calorie intake by 59.6 kcals.


# setup
rm(list = ls())
library(tidyverse)
library(here)

source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")
source(file = "models/child_model_calorie.R")

table_outputs = list()


# Estimating the impact of the policy in:

# 1. Adults in England:

# 1.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab", nation = "England", population_group = "Adult")

# 1.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: - 59.6 kcal
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 23% of [A] = 13.7 kcals
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = - 45.89 kcals

policy_2_impact_england_adult = calculate_bmi_from_eichange(df = read_csv(here("inputs/processed/hse_2019.csv")),
                                                            intake_change = -45.89,
                                                            implmentation_duration = 365*5)
# 1.3. Outputs

# Bar plot of change in year on year distribution of different BMI categories
policy_2_impact_england_adult$bmi_category_plot

ggsave(here("outputs/policy_2/policy_2_impact_England_adult.png"), 
       plot = policy_2_impact_england_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')

# Output table with year on year distrubution of BMI categories
policy_2_impact_england_adult$bmi_percent_prevalence

table_outputs[["england_adult"]] = policy_2_impact_england_adult$bmi_percent_prevalence


# 2. Adults in Scotland

# 2.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/shes19i_eul.tab", nation = "Scotland", population_group = "Adult")

# 2.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: - 59.6 kcal
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 23% of [A] = 13.7 kcals
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = - 45.89 kcals

policy_2_impact_scotland_adult = calculate_bmi_from_eichange(df = read_csv(here("inputs/processed/shes_2019.csv")),
                                                             intake_change = -45.89,
                                                             implmentation_duration = 365*5)
# 2.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_2_impact_scotland_adult$bmi_category_plot

ggsave(here("outputs/policy_2/policy_2_impact_Scotland_adult.png"), 
       plot = policy_2_impact_scotland_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')


# Output table with year on year distrubution of BMI categories
policy_2_impact_scotland_adult$bmi_percent_prevalence

table_outputs[["scotland_adult"]] = policy_2_impact_scotland_adult$bmi_percent_prevalence


write_xlsx(path = "outputs/policy_2/policy_2.xlsx", 
           x = table_outputs)




write.csv(policy_2_impact_england_adult$post_df, file = "outputs/policy_2/policy_2_adult_england_bmi.csv")

write.csv(policy_2_impact_scotland_adult$post_df, file = "outputs/policy_2/policy_2_adult_scotland_bmi.csv")


