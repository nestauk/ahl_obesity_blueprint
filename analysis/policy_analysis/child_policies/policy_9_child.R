
#################################################################################################
# Policy 9 : Mandate large businesses in the OOH sector to implement calorie reduction targets  #
#           to reach maximum calorie guideline for products in categories specified in Public   #
#           Health England's 2020 guidance (medium, small and micro enterprises are excluded)   #
#                                                                                               #
#################################################################################################


# Population group: 
# Children: Age: 5 - 18; BMI Group: ≥ 85th percentile
# 
# Daily energy intake:
# The evidence (https://blueprint.nesta.org.uk/intervention/mandate-the-out-of-home-ooh-sector-to-implement-calorie-reduction-targets/#Population-impact) is the same we’ve estimated using Kantar data for adults
# The change in energy intake = 4.3 kcals
# Compensation effect = 23% x 4.3 = 0.99 kcals
# Net reduction in energy intake = 3.3 kcals
# We assume that the magnitude of effect is  proportional to the age of the person. For example, for 18 year olds we assume the same calorie reduction as is seen in adults. For 17 year olds, we assume less impact/ calorie reduction due to differences in reported daily calorie intake. 




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
# Effect size [A]: 4.3 kcals
# Population segment impacted by policy [B]: Children in age group 5 - 18 years
# Compensation effect [C]:  23% of [A] = 0.99 kcals
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -3.3 kcals


policy_9_impact_england_child = calculate_bmi_from_eichange_hox(df = read_csv(here("inputs/processed/hse_2019_children.csv")),
                                                                 daily_ei_change = 3.3,
                                                                 nation = "England", 
                                                                 tags = "Policy 9 Child" )


# 1.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_9_impact_england_child$bmi_prevalence_plot


ggsave(here("outputs/child_policy/policy_9_child/policy_9_impact_England_child.png"), 
       plot = policy_9_impact_england_child$bmi_prevalence_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')

policy_9_impact_england_child$bmi_prevalence_table

table_outputs[["england_child"]] = policy_9_impact_england_child$bmi_prevalence_table

write_xlsx(path = "outputs/child_policy/policy_9_child/policy_9.xlsx", x = table_outputs)

write.csv(policy_9_impact_england_child$post_df, file = "outputs/child_policy/policy_9_child/policy_9_child_england_bmi.csv")


