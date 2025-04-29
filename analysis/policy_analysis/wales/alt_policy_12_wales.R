
################################################################################################### 
# Policy 12: Incentivise businesses to reformulate HFSS foods through a £500 million              #
#            reformulation grant fund                                                             #
#                                                                                                 #
###################################################################################################


rm(list = ls())
library(tidyverse)
library(here)
library(writexl)
library(aws.s3)

# add access information


# Getting file with the number of products in-home and out of home:
number_of_products <- s3read_using(FUN = read.csv,
                                   bucket = "ahl-obesity-blueprint",
                                   object = "inputs/processed/policy_12/number_of_products.csv")


# Description:


# The evidence is from the rapid review  
# (https://docs.google.com/document/d/1Q2l1H2bHlEO2t6rK2fpbR43z3_qa8EJ_6h_CcjIEMDo/edit?usp=sharing) 
# (quality assured by the EAG) supplemented by additional calculation:

# Number of in-home product categories = 1750 [1]
# Number of in-home products = 96,800

# number of OOH products = 50,096 [1]

# Total number of products in-home + out of home = 146,896

# Reformulation Pot = £500 million
# Assuming per product reformulation on average = £500,000 (Awaiting information from FDF Scotland on this)
# number of products that can be reformulated = 1000

# Percentage of all products that can be reformulated = 1000/146896 = 0.7%

# On average, when participants consumed foods all foods in a day with lower energy density, they consumed 709 fewer
# calories per day compared to when they consumed all foods in a day with higher energy density.

# Therefore, if 0.7% of all products were reformulated, then individuals would consume:
# 0.7% x 709 = 4.96 kcals  i.e. 4.96 kcals fewer per day on average per day


# [1] Nesta Analysis of OOH food and drink data from market research company (2024)
# [2] Kantar (2021). Competing effectively in a HFSS- regulated world. 
#     Access at: https://www.kantar.com/uki/  inspiration/fmcg/2021-wp-competing-effectively-in-ahfss-regulated-world
# [3] Nesta Analysis of In Home food and drink data from market research company (2023)

# setup
rm(list = ls())
library(tidyverse)
library(here)
library(writexl)
library(aws.s3)

source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")



# access info

# add access information


table_outputs = list()

# Estimating the impact of the policy in:

# 1. Adults in Wales:

# 1.1. Cleaned input/ baseline data:

df_wales_cleaned = read.csv(here("inputs/processed/nsw_2019.csv"))

# 1.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: 4.96 kcals - Estimated above in the variable kcal_reduction
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]:  23% of the change in daily energy intake = 23% * 4.96 = 1.14
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -3.81 kcals

policy_12_impact_wales_adult = calculate_bmi_from_eichange(df = df_wales_cleaned,
                                                           intake_change = -3.81,
                                                           implmentation_duration = 365*5, tags = "Wales | Policy 12")

# 1.3. Outputs

# Display outputs:

# Bar plot of change in year on year distribution of different BMI categories
policy_12_impact_wales_adult$bmi_category_plot


# Output table with year on year distribution of BMI categories
policy_12_impact_wales_adult$bmi_percent_prevalence

table_outputs[["wales_adult"]] = policy_12_impact_wales_adult$bmi_percent_prevalence


# writing outputs to folders:

ggsave(here("outputs/policy_12/policy_12_impact_wales_adult.png"), 
       plot = policy_12_impact_wales_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')


write_xlsx(path = "outputs/policy_12/policy_12_wales.xlsx", x = table_outputs)

write.csv(policy_12_impact_wales_adult$post_df, file = "outputs/policy_12/policy_12_adult_wales_bmi.csv")


