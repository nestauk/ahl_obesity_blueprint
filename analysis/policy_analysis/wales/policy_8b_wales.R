
#################################################################################################
# Policy 8b : Restrict 'location' promotions of HFSS food and drink to food/ drink delivery     #
#             platforms                                                                         #
#################################################################################################

# Description:


# The evidence from the rapid review  
# (https://docs.google.com/document/d/1xYDMQdCBmFuSpww7D6qXdRaNTsfEbG5hByr1HBhVd3Q/edit?usp=sharing) 
# (quality assured by the EAG) showed that the intervention led to reduction in daily calorie intake
# by 38 kcals for adults. This relates to about 1700 kcals of peoples diets.
# Nesta's analysis of OOH sector purchase data has shown that 11% of the kcals purchased in the OOH sector
# is via delivery apps and restaurant owned apps.
# Therefore, a policy affecting 11% of ~300 kcals purchased in the OOH sector, would lead to
# 0.71 kcal reduction in daily calorie intake.


# References:
# [1] https://www.directlinegroup.co.uk/en/news/brand-news/2020/the-fast-food-generation--a-third-of-children-use-food-delivery-.html
# [2] Kantar 2021 report for Food Standards Scotland

# setup
rm(list = ls())
library(tidyverse)
library(here)
library(writexl)
library(aws.s3)


source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")

# add access information

 
# Getting file with the share of kcal purchases from different modes
purchase_mode <- s3read_using(FUN = read.csv,
                                  bucket = "ahl-obesity-blueprint",
                                  object = "inputs/processed/policy_8b/purchase_mode_aggregated.csv")

# from this we know that ~11% of the purchases are done via restaurant specific delivery apps or 
# third party delivery apps. This is approximately 33 kcals per person per day.

# The evidence from the rapid review  
# (https://docs.google.com/document/d/1xYDMQdCBmFuSpww7D6qXdRaNTsfEbG5hByr1HBhVd3Q/edit?usp=sharing) 
# (quality assured by the EAG) showed that the intervention led to reduction in daily calorie intake
# by 38 kcals for adults. This relates to about 1700 kcals of peoples diets.
# Nesta's analysis of OOH sector purchase data has shown that 11% of the kcals purchased in the OOH sector
# is via delivery apps and restaurant owned apps.
# Therefore, a policy affecting 11% of ~300 kcals purchased in the OOH sector, i.e. 33 kcals, would lead to
# 0.71 kcal reduction in daily calorie intake.


table_outputs = list()

# Estimating the impact of the policy in:

# 1. Adults in Wales:

# 1.1. Cleaned input/ baseline data:

df_wales_cleaned = read_csv(here("inputs/processed/nsw_2019.csv"))

# 1.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: 0.71 kcals
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 23% of [A] = 0.16 kcals
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -0.55 kcals

policy_8b_impact_wales_adult = calculate_bmi_from_eichange(df = df_wales_cleaned,
                                                           intake_change = -0.55,
                                                           implmentation_duration = 365*5, tags = "Wales | Policy 8b")


# 1.3. Outputs

# display outputs:

# Bar plot of change in year on year distribution of different BMI categories
policy_8b_impact_wales_adult$bmi_category_plot

# Output table with year on year distribution of BMI categories
policy_8b_impact_wales_adult$bmi_percent_prevalence


# writing outputs to folder:

ggsave(here("outputs/policy_8b/policy_8b_impact_wales_adult.png"), 
       plot = policy_8b_impact_wales_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')


table_outputs[["wales_adult"]] = policy_8b_impact_wales_adult$bmi_percent_prevalence

write_xlsx(path = "outputs/policy_8b/policy_8b_wales.xlsx", 
           x = table_outputs)


write.csv(policy_8b_impact_wales_adult$post_df, file = "outputs/policy_8b/policy_8b_adult_wales_bmi.csv")






