
################################################################################################### 
# Policy 12: Incentivise businesses to reformulate HFSS foods through a £500 million              #
#            reformulation grant fund                                                             #
#                                                                                                 #
###################################################################################################

# Description:


# The evidence is from the rapid review  
# (https://docs.google.com/document/d/1Q2l1H2bHlEO2t6rK2fpbR43z3_qa8EJ_6h_CcjIEMDo/edit?usp=sharing) 
# (quality assured by the EAG) supplemented by additional calculation:


# Number of in-home product categories = 1750 [1]
# Number of in-home products = 128,432


# number of OOH products = 13925 [1]

# 40% of British shopper baskets in in-home purchases are HFSS products = 40% of 128432 = 51373 [2]
# 66% of OOH products are HFSS = 66% of 13925 = 9152 [1] 

# Total number of products in-home + out of home = 142,357
# Total HFSS Products = 51373 (in-home) + 9152 (OOH) = 60,525


# Reformulation Pot = £500 million
# Assuming per product reformulation on average = £100,000 (Awaiting information from FDF Scotland on this)
# number of products that can be reformulated = 5000

# Percentage of HFSS products that can be reformulated = 5000/60525 = 8.26%
# Percentage of all products that can be reformulated = 5000/142357 = 3.5%

# Average calorie densiyt of OOH products = 1.92 kcal/gm
# average calorie density of in-home products = 2 kcal/gm

# On average, when participants consumed foods all foods in a day with lower energy density, they consumed 709 fewer
# calories per day compared to when they consumed all foods in a day with higher energy density.

# Therefore, if 8.26% of the HFSS products were reformulated, then individuals would consume:
# 8.26% x 709 = 58.56 kcals
# i.e. 58.6 kcals fewer per day on average


# [1] Nesta Analysis of OOH food and drink data from market research company (2024)
# [2] Kantar (2021). Competing effectively in a HFSS- regulated world. 
#     Access at: https://www.kantar.com/uki/  inspiration/fmcg/2021-wp-competing-effectively-in-ahfss-regulated-world
# [3] Nesta Analysis of In Home food and drink data from market research company (2023)

# setup
rm(list = ls())
library(tidyverse)
library(here)
library(writexl)

source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")
#source(file = "models/child_model_calorie.R")
source(file = "models/child_model_calorie_henry.R")


table_outputs = list()

# Estimating the impact of the policy in:

# 1. Adults in England:

# 1.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab", nation = "England", population_group = "Adult")

# 1.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: 58.6 kcals
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]:  23% of the change in daily energy intake = 23% * 58.6 = 13.48
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -45.1 kcals

policy_12_impact_england_adult = calculate_bmi_from_eichange(df = read_csv(here("inputs/processed/hse_2019.csv")),
                                                             intake_change = -45.1,
                                                             implmentation_duration = 365*5)
# 1.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_12_impact_england_adult$bmi_category_plot

ggsave(here("outputs/policy_12/policy_12_impact_England_adult.png"), 
       plot = policy_12_impact_england_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')

# Output table with year on year distribution of BMI categories
policy_12_impact_england_adult$bmi_percent_prevalence

table_outputs[["england_adult"]] = policy_12_impact_england_adult$bmi_percent_prevalence


# 2. Adults in Scotland

# 2.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/shes19i_eul.tab", nation = "Scotland", population_group = "Adult")

# 2.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: 58.6 kcals
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]:  23% of the change in daily energy intake = 23% * 58.6 = 13.48
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -45.1 kcals

policy_12_impact_scotland_adult = calculate_bmi_from_eichange(df = read_csv(here("inputs/processed/shes_2019.csv")),
                                                              intake_change = -45.1,
                                                              implmentation_duration = 365*5)
# 3.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_12_impact_scotland_adult$bmi_category_plot

ggsave(here("outputs/policy_12/policy_12_impact_Scotland_adult.png"), 
       plot = policy_12_impact_scotland_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')


# Output table with year on year distrubution of BMI categories
policy_12_impact_scotland_adult$bmi_percent_prevalence

table_outputs[["scotland_adult"]] = policy_12_impact_scotland_adult$bmi_percent_prevalence


# 3. Exporting tabular outputs:


write_xlsx(path = "outputs/policy_12/policy_12.xlsx", x = table_outputs)

# data files for cost modelling
write.csv(policy_12_impact_england_adult$post_df, file = "outputs/policy_12/policy_12_adult_england_bmi.csv")

write.csv(policy_12_impact_scotland_adult$post_df, file = "outputs/policy_12/policy_12_adult_scotland_bmi.csv")
