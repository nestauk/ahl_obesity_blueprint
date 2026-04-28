
###################################################################################################
# Policy 1 & 2 : Restrict advertising of HFSS products [screen advertising and public transport   #
#                advertising]                                                                     #
#                                                                                                 #
###################################################################################################


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

table_outputs = list()


# Estimating the impact of the policy in:

# 1. Adults in Wales:

# 1.1. Cleaned input/ baseline data:

df_wales_cleaned = read.csv(here("inputs/processed/nsw_2019.csv"))

# 1.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: - 59.6 kcal
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 23% of [A] = 13.7 kcals
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = - 45.89 kcals

policy_1_2_impact_wales_adult = calculate_bmi_from_eichange(df = df_wales_cleaned,
                                                              intake_change = -45.89,
                                                              implmentation_duration = 365*5,
                                                              tags = "Policy 1&2 | Wales")

# 1.3. Outputs

# Bar plot of change in year on year distribution of different BMI categories
policy_1_2_impact_wales_adult$bmi_category_plot


# Output table with year on year distrubution of BMI categories
table_outputs[["wales_adult"]] = policy_1_2_impact_wales_adult$bmi_percent_prevalence

policy_1_2_impact_wales_adult$bmi_percent_prevalence



# Writing outputs to folder:


ggsave(here("outputs/policy_1_2/policy_1_2_impact_wales_adult.png"), 
       plot = policy_1_2_impact_wales_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')



write_xlsx(path = "outputs/policy_1_2/wales_policy_1_2.xlsx", x = table_outputs)

write.csv(policy_1_2_impact_wales_adult$post_df, file = "outputs/policy_1_2/policy_1_2_adult_wales_bmi.csv")


