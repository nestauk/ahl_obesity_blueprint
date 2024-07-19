
#################################################################################################
# Policy 5b : Ban all price promotions of discretionary foods for medium and large out of home  #
#             businesses (e.g. restaurants, coffee shops, fast food outlets).                  #
#                                                                                               #
#################################################################################################

# Description:


# The evidence from the rapid review  
# (https://docs.google.com/document/d/1Q2l1H2bHlEO2t6rK2fpbR43z3_qa8EJ_6h_CcjIEMDo/edit?usp=sharing) 
# (quality assured by the EAG) showed that the intervention led to reduction in daily calorie intake
# by 87.6 kcal for adults. The review did not include evidence on how this would affect daily calorie 
# intake in children.
# The source of the evidence also indicates that compensatory behaviour was accounted for while reporting
# out the final estimates of daily calorie reductions.
# Nesta Analysis has shown that the calorie contribution from OOH sector as a whole is ~300 kcals per 
# person per day on average. Large businesses contribute a third of the total OOH contribution to 
# people's diets on average and the in-home sector contributes ~1700 kcals to people's diets.
# A policy affecting 1700 kcals of people's diets is reduced DEI by 87.6 kcals due to the policy, therefore,
# a policy affecting large businesses that contribute 1/3rd of the calorie contribution of OOH sector
# would reduce energy intake by (1/3)*300*87.6*(1/1700) = 5.15 kcals per person per day.

# The source of the evidence also indicates that compensatory behaviour was accounted for while reporting
# out the final estimates of daily calorie reductions.


# setup
rm(list = ls())
library(tidyverse)
library(here)
library(writexl)

source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")
#source(file = "models/child_model_calorie.R")


table_outputs = list()

# Estimating the impact of the policy in:

# 1. Adults in England:

# 1.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab", nation = "England", population_group = "Adult")

# 1.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: 5.15 kcals
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 0 kcals (already accounted in the final estimates shared in the evidence)
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -21.9 kcals per person per day

policy_5b_impact_england_adult = calculate_bmi_from_eichange(df = read_csv(here("inputs/processed/hse_2019.csv")),
                                                             intake_change = -5.15,
                                                             implmentation_duration = 365*5)
# 1.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_5b_impact_england_adult$bmi_category_plot

ggsave(here("outputs/policy_5b/policy_5b_impact_England_adult.png"), 
       plot = policy_5b_impact_england_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')

# Output table with year on year distribution of BMI categories
policy_5b_impact_england_adult$bmi_percent_prevalence

table_outputs[["england_adult"]] = policy_5b_impact_england_adult$bmi_percent_prevalence


# 2. Adults in Scotland

# 2.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/shes19i_eul.tab", nation = "Scotland", population_group = "Adult")

# 2.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: 5.15 kcals
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 0 kcals (already accounted in the final estimates shared in the evidence)
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -21.9 kcals per person per day

policy_5b_impact_scotland_adult = calculate_bmi_from_eichange(df = read_csv(here("inputs/processed/shes_2019.csv")),
                                                              intake_change = -5.15,
                                                              implmentation_duration = 365*5)
# 2.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_5b_impact_scotland_adult$bmi_category_plot

ggsave(here("outputs/policy_5b/policy_5b_impact_Scotland_adult.png"), 
       plot = policy_5b_impact_scotland_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')


# Output table with year on year distrubution of BMI categories
policy_5b_impact_scotland_adult$bmi_percent_prevalence

table_outputs[["scotland_adult"]] = policy_5b_impact_scotland_adult$bmi_percent_prevalence


write_xlsx(path = "outputs/policy_5b/policy_5b.xlsx", x = table_outputs)

# outputs for cost modelling
write.csv(policy_5b_impact_england_adult$post_df, file = "outputs/policy_5b/policy_5b_adult_england_bmi.csv")

write.csv(policy_5b_impact_scotland_adult$post_df, file = "outputs/policy_5b/policy_5b_adult_scotland_bmi.csv")


