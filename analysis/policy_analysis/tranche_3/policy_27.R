
#################################################################################################
# Policy 27 : Extend the SDIL to unsweetened juice and sweetened milk based drinks              #
#                                                                                               #
#################################################################################################

# Description:


# The evidence is based on the effect of SDIL on daily calorie intake. Dickson et al. (2022) estimates
# that the SDIL removed 6,600 kcals per head per year in the UK. This is the equivalent of 18.08 kcals
# from an adult's daily calorie intake. An assumption is made that extending the SDIL to include
# sweetened milk based drinks and unsweetened juice will remove an additional 20% over the SDIL.
# This equals to 21.7 kcals from an adults daily energy intake. Compensation of 23% will be applied while
# modelling the impact.

# Reference:
# Dickson, Alex & Gehrsitz, Markus & Kemp, Jonathan, 2022. "Does a Spoonful of Sugar Levy Help the 
# Calories Go Down? An Analysis of the UK Soft Drinks Industry Levy," VfS Annual Conference 2022 (Basel):
# Big Data in Economics 264048, Verein für Socialpolitik / German Economic Association.
# <https://ideas.repec.org/p/zbw/vfsc22/264048.html>


# setup
rm(list = ls())
library(tidyverse)
library(here)
library(writexl)

source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")


table_outputs = list()

# Estimating the impact of the policy in:

# 1. Adults in England:

# 1.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab", nation = "England", population_group = "Adult")

# 1.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: 21.7 kcals
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 23% of [A] = 4.99
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -16.7 kcals

policy_27_impact_england_adult = calculate_bmi_from_eichange(df = read_csv(here("inputs/processed/hse_2019.csv")),
                                                             intake_change = -16.7,
                                                             implmentation_duration = 365*5)
# 1.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_27_impact_england_adult$bmi_category_plot

ggsave(here("outputs/policy_27/policy_27_impact_England_adult.png"), 
       plot = policy_27_impact_england_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')

# Output table with year on year distribution of BMI categories
policy_27_impact_england_adult$bmi_percent_prevalence

table_outputs[["england_adult"]] = policy_27_impact_england_adult$bmi_percent_prevalence


# 2. Adults in Scotland

# 2.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/shes19i_eul.tab", nation = "Scotland", population_group = "Adult")

# 2.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: 21.7 kcals
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 23% of [A] = 4.99
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -16.7 kcals


policy_27_impact_scotland_adult = calculate_bmi_from_eichange(df = read_csv(here("inputs/processed/shes_2019.csv")),
                                                              intake_change = -16.7,
                                                              implmentation_duration = 365*5)
# 2.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_27_impact_scotland_adult$bmi_category_plot

ggsave(here("outputs/policy_27/policy_27_impact_Scotland_adult.png"), 
       plot = policy_27_impact_scotland_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')


# Output table with year on year distrubution of BMI categories
policy_27_impact_scotland_adult$bmi_percent_prevalence

table_outputs[["scotland_adult"]] = policy_27_impact_scotland_adult$bmi_percent_prevalence



# 3. Exporting tabular outputs

write_xlsx(path = "outputs/policy_27/policy_27.xlsx", x = table_outputs)


write.csv(policy_27_impact_england_adult$post_df, file = "outputs/policy_27/policy_27_adult_england_bmi.csv")

write.csv(policy_27_impact_scotland_adult$post_df, file = "outputs/policy_27/policy_27_adult_scotland_bmi.csv")

