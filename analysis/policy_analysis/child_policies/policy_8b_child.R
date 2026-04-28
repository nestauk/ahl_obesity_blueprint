
#################################################################################################
# Policy 8b : Restrict location promotions (e.g. on home page, on ‘favourite product’ pages)    #
#             of HFSS food and drink on food/ drink delivery platforms with the largest         #
#             market share                                                                      #
#                                                                                               #
#################################################################################################


# Population group: 
# Children: Age: 5 - 18; BMI Group: ≥ 85th percentile
# 

# Daily energy intake:

# The evidence from the rapid review (https://docs.google.com/document/d/1xYDMQdCBmFuSpww7D6qXdRaNTsfEbG5hByr1HBhVd3Q/edit?usp=sharing)
# showed that the intervention led to reduction in daily calorie intake by 38 kcals for adults.
# There is evidence that on average, ~1700 kcals of daily calorie intake is from the retail sector. 

# Hence, when location promotions are applied in the retail sector, the reduction in daily calorie
# intake is approximately 

# The OOH sector accounts for approximately ~ 20% of an adult’s  daily calorie intake on average 
# (approximately 300kcal). Further, 10.6% of out of home purchases are via delivery platforms
# [OOH Analysis, Nesta, 2024 - https://docs.google.com/document/d/15hppZdgi9SUMXkfLGWBqvzqoggsm0WsABN-mLDjls0Y/edit]. 

# We assume that on average  xx of daily kcal consumed is from purchasing from delivery platforms.  
# We assume there would be a similar impact of location restrictions on delivery platforms as the 
# effect found in the retail sector (i.e. 38kcal reduction from 1700). 

# Hence we approximate the effect of a similar interventions on a delivery platform to result in a
# reduction of 0.7kcal per adult per day [10.6% x 300 x 38 / 1700 which is 0.7 kcals]

# Compensation effect of 23% = 0.14

# Net reduction in energy intake = 0.6 kcals




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
# Effect size [A]: 10.6% x 300 x 38 / 1700 = 0.7 kcal
# Population segment impacted by policy [B]: Children in age group 5 - 18 years
# Compensation effect [C]:  23% of [A] = 0.14 kcals
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -0.6 kcals


policy_8b_impact_england_child = calculate_bmi_from_eichange_hox(df = read_csv(here("inputs/processed/hse_2019_children.csv")),
                                                                 daily_ei_change = 0.6,
                                                                 nation = "England", 
                                                                 tags = "Policy 8b Child" )


# 1.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_8b_impact_england_child$bmi_prevalence_plot


ggsave(here("outputs/child_policy/policy_8b_child/policy_8b_impact_England_child.png"), 
       plot = policy_8b_impact_england_child$bmi_prevalence_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')

policy_8b_impact_england_child$bmi_prevalence_table

table_outputs[["england_child"]] = policy_8b_impact_england_child$bmi_prevalence_table

write_xlsx(path = "outputs/child_policy/policy_8b_child/policy_8b.xlsx", x = table_outputs)

write.csv(policy_8b_impact_england_child$post_df, file = "outputs/child_policy/policy_8b_child/policy_8b_child_england_bmi.csv")


