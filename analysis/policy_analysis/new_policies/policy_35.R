
#############################################################################################
# Policy 35 : Enforce provision of FOP labelling on retail packaging                        #
#                                                                                           #
#############################################################################################

# Description:

# The evidence from the rapid review 
# (https://docs.google.com/document/d/14JAqWmVOcdobuuGJeO4GIuxDPA28wtxb9UhIim7VP2Q/edit?usp=sharing) 
# (quality assured by the EAG) showed that the interventions led to a reduction of ~27 kcals in daily
# energy intake of adults.

# Our rapid review found FOPL could lead to 0.05 SMD difference in intake.
# Using Holland et al. (2018) (Page 27, Figure 4) we convert the SMD to equivalent kcal measure -27 kcal
# per person per day (b)
# As the conversion from SMD to kcals is based on NDNS data, we adjust for underreporting in the NDNS by uplifting the estimate by 32% (c)
# Next, as the estimate is on the overall diet, we exclude OOH calorie contribution (11%) (d)
# Simialrly, we exclude kcals from non-packaged food that wouldn't be covered by this policy - 11% (d)
# Next, we adjust for FOPL coverage at baseline. Govt Stats estimate that 
# approximately 66% of the food products have FOPL (e). Implying that this policy would only affect 34% of the remaining kcals

# Next we apply compensation sation effect of 23% to estimate the final change in energy intake to be 7.4 kcals

# References:
# (a) Rauber F, Louzada MLDC, Martinez Steele E, et alUltra-processed foods and excessive free sugar intake
#     in the UK: a nationally representative cross-sectional studyBMJ Open 2019;9:e027546. 
#     doi: 10.1136/bmjopen-2018-027546
#
# (b) Hollands GJ, Shemilt I, Marteau TM, Jebb SA, Lewis HB, Wei Y, Higgins JPT, Ogilvie D. Portion, package 
#     or tableware size for changing selection and consumption of food, alcohol and tobacco. Cochrane Database
#     of Systematic Reviews 2015, Issue 9. Art. No.: CD011045. DOI: 10.1002/14651858.CD011045.pub2.
# (c) A government statistical service perspective on official estimates of Calorie Consumption: 2019 update (2019) 
#    A Government Statistical Service perspective on official estimates of calorie consumption: 2019 update - Office for 
#    National Statistics. Available at: https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/methodologies/agovernmentstatisticalserviceperspectiveonofficialestimatesofcalorieconsumption2019update. 
# (d) Improving diets to halve obesity across Britain - Nesta (https://www.nesta.org.uk/data-visualisation-and-interactive/improving-diets-to-halve-obesity-across-britain/)
# (d) Nesta - Internal Analysis for (c) - https://docs.google.com/spreadsheets/d/1dYgYIV-uO0VL76T4jKeEcJx92ROKFhR4DvJYBLkZ660/edit?gid=1103962484#gid=1103962484
# (e) Skotarenko, L. (2018). The UK’s Voluntary Front of Pack Nutrition Labelling Scheme. [online] London: Department of Health. Available at:
#     https://food.ec.europa.eu/document/download/df7f5741-cc84-4071-8a4e-482acc0f552f_en?filename=comm_ahac_20180423_pres3.pdf.




# setup
rm(list = ls())
gc()
library(tidyverse)
library(here)

source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")
source(file = "models/child_model_calorie.R")
source(file = "post_processing/post_processing.R")

# constants:
NDNS_ADJUSTMENT = 1.32
OOH_CONTRIBUTION_TO_DIETS = 0.11
NON_PACKAGED_FOOD_CONTRBUTION_TO_DIETS = 0.11
BASELINE_FOPL_COVERAGE = 0.66
COMPENSATION_EFFECT = 0.23

table_outputs = list() # creating a list of table outputs to be saved as an excel file

# Estimating the impact of the policy in:

# 1. Adults in England:

# 1.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab", nation = "England", population_group = "Adult")

# 1.2. Estimating the impact of the intervention on prevalence of obesity:


# calculating the change in intake:
effect_size = 27
effect_size_after_ndns_adjustment = effect_size * NDNS_ADJUSTMENT
effect_size_ooh_adjustment = effect_size_after_ndns_adjustment * (1 - OOH_CONTRIBUTION_TO_DIETS)
effect_size_non_packaged_food_adjustment = effect_size_ooh_adjustment * (1 - NON_PACKAGED_FOOD_CONTRBUTION_TO_DIETS)
effect_size_baseline_coverage_adjustment = effect_size_non_packaged_food_adjustment * (1 - BASELINE_FOPL_COVERAGE)
effect_size_post_compensation = effect_size_baseline_coverage_adjustment * (1 - COMPENSATION_EFFECT)
change_in_intake = -round(effect_size_post_compensation,1)

policy_35_impact_england_adult = calculate_bmi_from_eichange(df = read_csv(here("inputs/processed/hse_2019.csv")),
                                                            intake_change = change_in_intake,
                                                            implmentation_duration = 365*5)

# 1.3. Outputs

# Bar plot of change in year on year distribution of different BMI categories
policy_35_impact_england_adult$bmi_category_plot

ggsave(here("outputs/new_policies/policy_35/policy_35_impact_England_adult.png"), 
       plot = policy_35_impact_england_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')

# Output table with year on year distribution of BMI categories
policy_35_impact_england_adult$bmi_percent_prevalence

table_outputs[["england_adult"]] = policy_35_impact_england_adult$bmi_percent_prevalence



# 2. Adults in Scotland

# 2.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/shes19i_eul.tab", nation = "Scotland", population_group = "Adult")

# 2.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: ﹣12.4 kcal
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 23% of [A] = 2.85
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -9.57 kcals

policy_35_impact_scotland_adult = calculate_bmi_from_eichange(df = read_csv(here("inputs/processed/shes_2019.csv")),
                                                             intake_change = change_in_intake,
                                                             implmentation_duration = 365*5)
# 2.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_35_impact_scotland_adult$bmi_category_plot

ggsave(here("outputs/new_policies/policy_35/policy_35_impact_Scotland_adult.png"), 
       plot = policy_35_impact_scotland_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')


# Output table with year on year distrubution of BMI categories
policy_35_impact_scotland_adult$bmi_percent_prevalence

table_outputs[["scotland_adult"]] = policy_35_impact_scotland_adult$bmi_percent_prevalence


# post processing
# extracting the reduction in obesity prevalence 

annual_obesity_prevalence_england = extract_relative_change(data = policy_35_impact_england_adult$bmi_percent_prevalence)
annual_obesity_prevalence_scotland = extract_relative_change(data = policy_35_impact_scotland_adult$bmi_percent_prevalence)

# Relative reduction in obesity prevalence in England = 2.6%
# Relative reduction in obesity prevalence in Scotland = 2.4%

# Adding to table outputs:
table_outputs[["annual_obesity_prevalence_eng"]] = annual_obesity_prevalence_england
table_outputs[["annual_obesity_prevalence_scot"]] = annual_obesity_prevalence_scotland

# Estimating the annual value to government (benefit):

annual_benefit_to_gov = extract_pound_benefit(data = annual_obesity_prevalence_england, 
                                              cost = MODEL_CONSTANTS$COST_OF_OBESITY_IN_BILLIONS,
                                              duration = MODEL_CONSTANTS$MODEL_DURATION)

# Average annual value to government compared to baseline = £1.58 billions

# Adding to table outputs:
table_outputs[["annual_benefit_to_gov"]] = annual_benefit_to_gov




# writing outputs to folder

write_xlsx(path = "outputs/new_policies/policy_35/policy_35.xlsx", x = table_outputs)

write.csv(policy_35_impact_england_adult$post_df, file = "outputs/new_policies/policy_35/policy_35_adult_england_bmi.csv")

write.csv(policy_35_impact_scotland_adult$post_df, file = "outputs/new_policies/policy_35/policy_35_adult_scotland_bmi.csv")


