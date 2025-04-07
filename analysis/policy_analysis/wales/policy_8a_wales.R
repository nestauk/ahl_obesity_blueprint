
##############################################################################################
# Policy 8a: Restricting Checkout, End-of-Aisle, and Store Entrance Sales of Food and Drinks #
#            High in Fat, Salt, and Sugar (HFSS)                                             #
##############################################################################################

# Description:

# The evidence from the rapid review 
# (https://docs.google.com/document/d/1xYDMQdCBmFuSpww7D6qXdRaNTsfEbG5hByr1HBhVd3Q/edit?usp=sharing) 
# (quality assured by the EAG) showed that the interventions led to a reduction of 60 kcals in daily
# energy intake of adults and 67 kcals in daily energy intake of children.


# setup
rm(list = ls())
gc()
library(tidyverse)
library(here)

source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")

table_outputs = list() # creating a list of table outputs to be saved as an excel file


# Estimating the impact of the policy in:

# 1. Adults in Wales:

# 1.1. Cleaned input/ baseline data:

df_wales_cleaned = read.csv(here("inputs/processed/nsw_2019.csv"))

# 2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: ﹣60 kcal
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 23% of [A] = 13.8
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -46.2 kcals

policy_8a_impact_wales_adult = calculate_bmi_from_eichange(df = df_wales_cleaned,
                                                             intake_change = -46.2,
                                                             implmentation_duration = 365*5,
                                                             tags = "Wales | Policy 8a")

# 1.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_8a_impact_wales_adult$bmi_category_plot


# Output table with year on year distribution of BMI categories
policy_8a_impact_wales_adult$bmi_percent_prevalence

table_outputs[["wales_adult"]] = policy_8a_impact_wales_adult$bmi_percent_prevalence


# Writing output files to the folder:

ggsave(here("outputs/policy_8a/policy_8a_impact_wales_adult.png"), 
       plot = policy_8a_impact_wales_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')

write_xlsx(path = "outputs/policy_8a/policy_8a_wales.xlsx", x = table_outputs)

write.csv(policy_8a_impact_wales_adult$post_df, file = "outputs/policy_8a/policy_8a_adult_wales_bmi.csv")


