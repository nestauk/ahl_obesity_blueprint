
#################################################################################################
# Policy 4a : End all volume offers for HFSS products which contribute significant sugar and    #
#             calories to children's diets and are of most concern for childhood obesity, in    #
#             the retail sector excluding small and micro businesses                            #
#################################################################################################

# Description:


# The evidence from the rapid review  
# (https://docs.google.com/document/d/1Q2l1H2bHlEO2t6rK2fpbR43z3_qa8EJ_6h_CcjIEMDo/edit?usp=sharing) 
# (quality assured by the EAG) showed that the intervention led to reduction in daily calorie intake
# by 22 kcal for adults. The policy was found to reduce daily calorie intake in children by 2.48 kcals.
# The source of the evidence also indicates that compensatory behaviour was accounted for while reporting
# out the final estimates of daily calorie reductions.


# setup
rm(list = ls())
library(tidyverse)
library(here)
library(writexl)

source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")
source(file = "models/child_model_calorie.R")


table_outputs = list()

# Estimating the impact of the policy in:

# 1. Adults in England:

# 1.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab", nation = "England", population_group = "Adult")

# 1.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: 2.6 kcals
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 0 kcals (already accounted in the final estimates shared in the evidence)
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -2.6 kcals

policy_4a_impact_england_adult = calculate_bmi_from_eichange(df = read_csv(here("inputs/processed/hse_2019.csv")),
                                                             intake_change = -2.6,
                                                             implmentation_duration = 365*5)
# 1.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_4a_impact_england_adult$bmi_category_plot

ggsave(here("outputs/policy_4a/policy_4a_impact_England_adult.png"), 
       plot = policy_4a_impact_england_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')

# Output table with year on year distribution of BMI categories
policy_4a_impact_england_adult$bmi_percent_prevalence

table_outputs[["england_adult"]] = policy_4a_impact_england_adult$bmi_percent_prevalence


# 2. Children in England

# 2.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab",
                   nation = "England",
                   population_group = "Children")


# 2.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: -2.48 kcals
# Population segment impacted by policy [B]: Children in age group 5 - 18 years
# Compensation effect [C]: 0 kcals
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -2.48 kcals


policy_4a_impact_england_child = calculate_bmi_from_ei_change(df = read_csv(here("inputs/processed/hse_2019_children.csv")), daily_ei_change = 2.48 )

policy_4a_impact_england_child$bmi_prevalence_table
policy_4a_impact_england_child$bmi_prevalence_plot

policy_4a_impact_england_child = calculate_child_bmi_from_eichange(df = read_csv(here("inputs/processed/hse_2019_children.csv")),
                                                                   intake_change = 2.48,
                                                                   implementation_duration = 365*5, 
                                                                   use_bodyfat_curves = 0)


# 2.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_4a_impact_england_child$bmi_category_plot

ggsave(here("outputs/policy_4a/policy_4a_impact_England_child.png"), 
       plot = policy_4a_impact_england_child$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')

# Output table with year on year distrubution of BMI categories
policy_4a_impact_england_child$bmi_percent_prevalence

table_outputs[["england_child"]] = policy_4a_impact_england_child$bmi_percent_prevalence


# 3. Adults in Scotland

# 3.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/shes19i_eul.tab", nation = "Scotland", population_group = "Adult")

# 3.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: 2.6 kcals
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 0 kcals
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -2.6 kcals

policy_4a_impact_scotland_adult = calculate_bmi_from_eichange(df = read_csv(here("inputs/processed/shes_2019.csv")),
                                                             intake_change = -2.6,
                                                             implmentation_duration = 365*5)
# 3.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_4a_impact_scotland_adult$bmi_category_plot

ggsave(here("outputs/policy_4a/policy_4a_impact_Scotland_adult.png"), 
       plot = policy_4a_impact_scotland_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')


# Output table with year on year distrubution of BMI categories
policy_4a_impact_scotland_adult$bmi_percent_prevalence

table_outputs[["scotland_adult"]] = policy_4a_impact_scotland_adult$bmi_percent_prevalence

# 4. Children in Scotland

# 4.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/shes19i_eul.tab",
                   nation = "Scotland",
                   population_group = "Children")


# 4.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: -2.48 kcals
# Population segment impacted by policy [B]: Children in age group 5 - 18 years
# Compensation effect [C]: 0 kcals
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -2.48 kcals

policy_4a_impact_scotland_child = calculate_child_bmi_from_eichange(df = read_csv(here("inputs/processed/shes_2019_children.csv")),
                                                                    intake_change = 2.48,
                                                                    implementation_duration = 365*5, 
                                                                    use_bodyfat_curves = 1)


# 4.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_4a_impact_scotland_child$bmi_category_plot



ggsave(here("outputs/policy_4a/policy_4a_impact_Scotland_child.png"), 
       plot = policy_4a_impact_scotland_child$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')


# Output table with year on year distrubution of BMI categories
policy_4a_impact_scotland_child$bmi_percent_prevalence

table_outputs[["scotland_child"]] = policy_4a_impact_scotland_child$bmi_percent_prevalence



# 5. Exporting tabular outputs:


write_xlsx(path = "outputs/policy_4a/policy_4a.xlsx", x = table_outputs)


write.csv(policy_4a_impact_england_adult$post_df, file = "outputs/policy_4a/policy_4a_adult_england_bmi.csv")

write.csv(policy_4a_impact_england_child$post_df, file = "outputs/policy_4a/policy_4a_child_england_bmi.csv")


write.csv(policy_4a_impact_scotland_adult$post_df, file = "outputs/policy_4a/policy_4a_adult_scotland_bmi.csv")

write.csv(policy_4a_impact_scotland_child$post_df, file = "outputs/policy_4a/policy_4a_child_scotland_bmi.csv")


