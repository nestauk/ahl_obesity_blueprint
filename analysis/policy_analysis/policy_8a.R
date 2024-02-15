
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
source(file = "models/child_model_calorie.R")

table_outputs = list() # creating a list of table outputs to be saved as an excel file


# Estimating the impact of the policy in:

# 1. Adults in England:

# 1.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab", nation = "England", population_group = "Adult")

# 2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: ﹣60 kcal
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 23% of [A] = 13.8
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -46.2 kcals

policy_8a_impact_england_adult = calculate_bmi_from_eichange(df = read_csv(here("inputs/processed/hse_2019.csv")),
                                                             intake_change = -46.2,
                                                             implmentation_duration = 365*5)

# 1.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_8a_impact_england_adult$bmi_category_plot

ggsave(here("outputs/policy_8a/policy_8a_impact_England_adult.png"), 
       plot = policy_8a_impact_england_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')

# Output table with year on year distribution of BMI categories
policy_8a_impact_england_adult$bmi_percent_prevalence

table_outputs[["england_adult"]] = policy_8a_impact_england_adult$bmi_percent_prevalence



# 2. Children in England

# 2.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab",
                   nation = "England",
                   population_group = "Children")


# 2.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: -67 kcal
# Population segment impacted by policy [B]: Children in age group 5 - 18 years
# Compensation effect [C]: 23% of [A] = 15.41
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -51.59 kcals

policy_8a_impact_england_child = calculate_child_bmi_from_eichange(df = read_csv(here("inputs/processed/hse_2019_children.csv")),
                                                                  intake_change = -51.59,
                                                                  implementation_duration = 365*5, 
                                                                  use_bodyfat_curves = 0)


# 2.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_8a_impact_england_child$bmi_category_plot

ggsave(here("outputs/policy_8a/policy_8a_impact_England_child.png"), 
       plot = policy_8a_impact_england_child$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')

# Output table with year on year distrubution of BMI categories
policy_8a_impact_england_child$bmi_percent_prevalence

table_outputs[["england_child"]] = policy_8a_impact_england_child$bmi_percent_prevalence


# 3. Adults in Scotland

# 3.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/shes19i_eul.tab", nation = "Scotland", population_group = "Adult")

# 3.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: ﹣60 kcal
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 23% of [A] = 13.8
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -46.2 kcals

policy_8a_impact_scotland_adult = calculate_bmi_from_eichange(df = read_csv(here("inputs/processed/shes_2019.csv")),
                                                             intake_change = -46.2,
                                                             implmentation_duration = 365*5)
# 3.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_8a_impact_scotland_adult$bmi_category_plot

ggsave(here("outputs/policy_8a/policy_8a_impact_Scotland_adult.png"), 
       plot = policy_8a_impact_scotland_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')


# Output table with year on year distrubution of BMI categories
policy_8a_impact_scotland_adult$bmi_percent_prevalence

table_outputs[["scotland_adult"]] = policy_8a_impact_scotland_adult$bmi_percent_prevalence


# 4. Children in Scotland

# 4.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/shes19i_eul.tab",
                   nation = "Scotland",
                   population_group = "Children")


# 4.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: -67 kcal
# Population segment impacted by policy [B]: Children in age group 5 - 18 years
# Compensation effect [C]: 23% of [A] = 15.41
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -51.59 kcals

policy_8a_impact_scotland_child = calculate_child_bmi_from_eichange(df = read_csv(here("inputs/processed/shes_2019_children.csv")),
                                                                   intake_change = -51.59,
                                                                   implementation_duration = 365*5, 
                                                                   use_bodyfat_curves = 1)


# 4.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_8a_impact_scotland_child$bmi_category_plot

ggsave(here("outputs/policy_8a/policy_8a_impact_Scotland_child.png"), 
       plot = policy_8a_impact_scotland_child$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')


# Output table with year on year distrubution of BMI categories
policy_8a_impact_scotland_child$bmi_percent_prevalence

table_outputs[["scotland_child"]] = policy_8a_impact_scotland_child$bmi_percent_prevalence

write_xlsx(path = "outputs/policy_8a/policy_8a.xlsx", x = table_outputs)




write.csv(policy_8a_impact_england_adult$post_df, file = "outputs/policy_8a/policy_8a_adult_england_bmi.csv")
write.csv(policy_8a_impact_england_child$post_df, file = "outputs/policy_8a/policy_8a_child_england_bmi.csv")

write.csv(policy_8a_impact_scotland_adult$post_df, file = "outputs/policy_8a/policy_8a_adult_scotland_bmi.csv")
write.csv(policy_8a_impact_scotland_child$post_df, file = "outputs/policy_8a/policy_8a_child_scotland_bmi.csv")


