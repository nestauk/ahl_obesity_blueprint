
#################################################################################################
# Policy 28 : Introduce a £3/kg tax on sugar and a £6/kg tax on salt sold for use in            #
#             processed foods or in restaurants and catering businesses                         #
#                                                                                               #
#################################################################################################

# 
# Population group: 
# Children: Age: 5 - 18; BMI Group: ≥ 85th percentile
# 
# Daily energy intake:
# The evidence from the IFS report (Page 16, Table 5.5 - https://ifs.org.uk/sites/default/files/output_url_files/WP202121-The-impact-of-a-tax-on-added-sugar-and-salt.pdf) 

# Taking an average across children in age group 5 - 18, the effect size is 38 kcals
# We assume that compensation is accounted for as the paper takes into account shifts
# in demand as a result of the reformulation and price changes

# We assume that the magnitude of effect is  proportional to the age of the person, and apply weights
# to calculate the adjusted calorie reduction.

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
# Effect size [A]: 38 kcals
# Population segment impacted by policy [B]: Children in age group 5 - 18 years
# Compensation effect [C]: 0 kcals

# Based on [A] and [C], the intake change = effect size - compensation effect = -38 kcals


policy_28_impact_england_child = calculate_bmi_from_eichange_hox(df = read_csv(here("inputs/processed/hse_2019_children.csv")),
                                                                 daily_ei_change = 38,
                                                                 nation = "England", 
                                                                 tags = "Policy 28 Child" )


# 1.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_28_impact_england_child$bmi_prevalence_plot


ggsave(here("outputs/child_policy/policy_28_child/policy_28_impact_England_child.png"), 
       plot = policy_28_impact_england_child$bmi_prevalence_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')

policy_28_impact_england_child$bmi_prevalence_table

table_outputs[["england_child"]] = policy_28_impact_england_child$bmi_prevalence_table

write_xlsx(path = "outputs/child_policy/policy_28_child/policy_28.xlsx", x = table_outputs)

write.csv(policy_28_impact_england_child$post_df, file = "outputs/child_policy/policy_28_child/policy_28_child_england_bmi.csv")

