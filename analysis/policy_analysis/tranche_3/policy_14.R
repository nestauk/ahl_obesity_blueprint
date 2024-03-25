
#################################################################################################
# Policy 14 : Restrict new opening of fast food restaurants                                     #
#                                                                                               #
#################################################################################################

# Description:


# The evidence is based on results from two manuscripts that are under review at the moment and is available 
# here - https://drive.google.com/drive/folders/1A-Sfoui6WfRI1GEBzzKNGW-fUrw0kn_1?usp=sharing
# We know that 20-25% of kcals come from OOH sector, and 18.75% of visits to OOH businesses are to fast food 
# restaurants and 36% of spending in the OOH sector at fast food restaurant (Nesta analysis of Kantar 2021 data, 2024).
# The paper finds that restriction zones lead to 30.3% fewer new outlets opening at 12 months compared with no 
# intervention.

# Therefore, in the absence of data availability of how stores composition might change (i.e. would new healthy stores
# open as a result of this intervention), we assume that since the opening of new takeaways are reduced, the 
# intake will remain constant. That is, in the absence of the intervention, if takeaways increased, people's calorie
# intake would have increased and now with the intervention while there could be a decrease with changes in food
# environments, we make the conservative estimate that daily calorie intake doesn't change.

# This policy is primarily targetted at children in schools.

# Reference:
# Burgione et al (2024 - preprint) https://drive.google.com/drive/folders/1A-Sfoui6WfRI1GEBzzKNGW-fUrw0kn_1?usp=sharing


# setup
rm(list = ls())
library(tidyverse)
library(here)
library(writexl)

source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/child_model_calorie_henry.R")


table_outputs = list()

# 1. Children in England

# 1.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab",
                   nation = "England",
                   population_group = "Children")


# 1.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: -0 kcals
# Population segment impacted by policy [B]: Children in age group 5 - 18 years
# Compensation effect [C]: 0 kcals
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -0 kcals


policy_14_impact_england_child = calculate_bmi_from_ei_change(df = read_csv(here("inputs/processed/hse_2019_children.csv")),
                                                              daily_ei_change = 0)



# 1.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_14_impact_england_child$bmi_prevalence_plot

ggsave(here("outputs/policy_14/policy_14_impact_England_child.png"), 
       plot = policy_14_impact_england_child$bmi_prevalence_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')

# Output table with year on year distrubution of BMI categories
policy_14_impact_england_child$bmi_prevalence_table

table_outputs[["england_child"]] = policy_14_impact_england_child$bmi_percent_prevalence



# 2. Children in Scotland

# 2.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/shes19i_eul.tab",
                   nation = "Scotland",
                   population_group = "Children")


# 2.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: -0 kcals
# Population segment impacted by policy [B]: Children in age group 5 - 18 years
# Compensation effect [C]: 0 kcals
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -0 kcals

policy_14_impact_scotland_child = calculate_bmi_from_ei_change(df = read_csv(here("inputs/processed/shes_2019_children.csv")),
                                                               daily_ei_change = 0)


# 2.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_14_impact_scotland_child$bmi_prevalence_plot



ggsave(here("outputs/policy_14/policy_14_impact_Scotland_child.png"), 
       plot = policy_14_impact_scotland_child$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')


# Output table with year on year distrubution of BMI categories
policy_14_impact_scotland_child$bmi_prevalence_table

table_outputs[["scotland_child"]] = policy_14_impact_scotland_child$bmi_prevalence_table



# 3. Exporting tabular outputs:


write_xlsx(path = "outputs/policy_14/policy_14.xlsx", x = table_outputs)



write.csv(policy_14_impact_england_child$post_df, file = "outputs/policy_14/policy_14_child_england_bmi.csv")


write.csv(policy_14_impact_scotland_child$post_df, file = "outputs/policy_14/policy_14_child_scotland_bmi.csv")


