

# setup
rm(list = ls())
library(tidyverse)
library(here)
library(writexl)

source(file = "requirements.R")
source(file = "models/child_model_calorie_henry.R")


table_outputs = list()

# Estimating the impact of the policy in:


# 1. Children in England

# 1.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab",
                   nation = "England",
                   population_group = "Children")


# 1.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: -(20% x 2.5 kcals) / 80% = 0.63 kcals
# Population segment impacted by policy [B]: Children in age group 5 - 18 years
# Compensation effect [C]:  23% of [A] = 0.15 kcals
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -0.5 kcals


test_4a = calculate_bmi_from_eichange_hox(df = read_csv(here("inputs/processed/hse_2019_children.csv")),
                                                                 daily_ei_change = 2.5,
                                                                 nation = "England", 
                                                                 tags = "Policy 5a Child" )


test_5a = calculate_bmi_from_eichange_hox(df = read_csv(here("inputs/processed/hse_2019_children.csv")),
                                          daily_ei_change = 0.5,
                                          nation = "England", 
                                          tags = "Policy 5a Child" )


tab_4a = test_4a$bmi_prevalence_table
tab_5a = test_5a$bmi_prevalence_table

test_4a$bmi_prevalence_table
test_5a$bmi_prevalence_table

test_A = calculate_bmi_from_eichange_hox(df = read_csv(here("inputs/processed/hse_2019_children.csv")),
                                          daily_ei_change = 2.9,
                                          nation = "England", 
                                          tags = "Policy 5a Child" )

test_A$bmi_prevalence_table

test_B = calculate_bmi_from_eichange_hox(df = read_csv(here("inputs/processed/hse_2019_children.csv")),
                                         daily_ei_change = 4,
                                         nation = "England", 
                                         tags = "Policy 5a Child" )

tab_B_1 = test_B$


# 1.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_5a_impact_england_child$bmi_prevalence_plot


ggsave(here("outputs/child_policy/policy_5a_child/policy_5a_impact_England_child.png"), 
       plot = policy_5a_impact_england_child$bmi_prevalence_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')

test = policy_5a_impact_england_child$bmi_prevalence_table

table_outputs[["england_child"]] = policy_5a_impact_england_child$bmi_prevalence_table

write_xlsx(path = "outputs/child_policy/policy_5a_child/policy_5a.xlsx", x = table_outputs)


write.csv(policy_5a_impact_england_child$post_df, file = "outputs/child_policy/policy_5a_child/policy_5a_child_england_bmi.csv")


