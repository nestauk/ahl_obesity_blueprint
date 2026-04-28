
#################################################################################################
# Policy 12 : Incentivise retailer and out of home businesses to reformulate HFSS foods         #
#             through a £500 million reformulation grant fund                                   #
#                                                                                               #
#################################################################################################

# 
# Population group: 
# Children: Age: 5 - 18; BMI Group: ≥ 85th percentile
# 
# Daily energy intake:
# From Kantar 2021 data,
#   - Number of in-home product categories = 1,750
#   - Number of in-home products = 128,432
#   - Number of OOH products = 13,925 
# Share of HFSS products:
#   - 40% of British shopper baskets in in-home purchases are HFSS products = 40% of 128,432 = 51,373
#   - 66% of OOH products are HFSS = 66% of 13,925 = 9,152
# Total number of products in-home + out of home = 142,357
# Total HFSS Products (OOH + in-home) = 51373 (in-home) + 9152 (OOH) = 60,525

# Reformulation Pot = £500 million
#   - Assuming per product reformulation on average = £500,000
#   - Number of products that can be reformulated = 1000
#   - Percentage of HFSS products that can be reformulated = 1000/60525 = 1.65%
#   - Percentage of all products that can be reformulated = 1000/142357 = 0.7%

# On average, when participants consumed all foods in a day with lower energy density, 
# they consumed 709 fewer calories per day compared to when they consumed all foods in a
# day with higher energy density (https://docs.google.com/document/d/1H0_AoK9Ok21wKThpb6FHGwsnd05nPWn7U3r-7CX6XII/edit?usp=sharing)

# Therefore, if 0.7% of all products were reformulated,
#   - then individuals would consume: 0.7% x 709 = 4.96 kcals  
#   - i.e. 4.96 kcals fewer per day on average per day

# We account for a 23% compensation effect = 23% x 4.96 = 1.14 kcals

# Net reduction in energy intake = 3.82 kcals

# We assume that the magnitude of effect is  proportional to the age of the child,
# and apply weights to calculate the adjusted calorie reduction.  For example, for 18 year olds
# we assume the same calorie reduction as is seen in adults. For 17 year olds, we assume less 
# impact/ calorie reduction due to differences in reported daily calorie intake. 


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
# Effect size [A]: 4.96 kcals
# Population segment impacted by policy [B]: Children in age group 5 - 18 years
# Compensation effect [C]:  23% of [A] = 1.14 kcals
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -3.82 kcals


policy_12_impact_england_child = calculate_bmi_from_eichange_hox(df = read_csv(here("inputs/processed/hse_2019_children.csv")),
                                                                 daily_ei_change = 3.82,
                                                                 nation = "England", 
                                                                 tags = "Policy 12 Child" )


# 1.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_12_impact_england_child$bmi_prevalence_plot


ggsave(here("outputs/child_policy/policy_12_child/policy_12_impact_England_child.png"), 
       plot = policy_12_impact_england_child$bmi_prevalence_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')

policy_12_impact_england_child$bmi_prevalence_table

table_outputs[["england_child"]] = policy_12_impact_england_child$bmi_prevalence_table

write_xlsx(path = "outputs/child_policy/policy_12_child/policy_12.xlsx", x = table_outputs)

write.csv(policy_12_impact_england_child$post_df, file = "outputs/child_policy/policy_12_child/policy_12_child_england_bmi.csv")

