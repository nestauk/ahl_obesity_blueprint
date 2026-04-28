
#################################################################################################
# Policy 4b : Ban all price promotions (e.g. was/now prices, introductory prices, temporary     #
#             price reductions) of HFSS foods in the retail sector excluding small and          #
#             micro businesses.                                                                 #
#################################################################################################

# Description:

# Population group: 
#   Children: Age: 5 - 18; BMI Group: ≥ 85th percentile

# Daily energy intake:
#   The effect size was 87.6 kcals (Page 33, Table 15, Scottish Gov impact assessment).
#   (https://www.gov.scot/publications/restricting-promotions-food-drink-high-fat-sugar-salt-partial-business-regulatory-impact-assessment/pages/2/)
#   This impact assessment focussed on a similar policy for take-home purchases.
#   We do not separately account for the compensation effect given that the
#   evidence source has also accounted for it as it was a randomised control trial. We assume
#   that the magnitude of effect is  proportional to the age of children, and apply 
#   weights to calculate the adjusted calorie reduction. Please see the appendix for these weights.



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
# Effect size [A]: -87.6 kcals
# Population segment impacted by policy [B]: Children in age group 5 - 18 years
# Compensation effect [C]: 0 kcals
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -87.6 kcals


policy_4b_impact_england_child = calculate_bmi_from_eichange_hox(df = read_csv(here("inputs/processed/hse_2019_children.csv")),
                                                                 daily_ei_change = 87.6,
                                                                 nation = "England", 
                                                                 tags = "Policy 4b Child" )



# 1.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_4b_impact_england_child$bmi_prevalence_plot


ggsave(here("outputs/child_policy/policy_4b_child/policy_4b_impact_England_child.png"), 
       plot = policy_4b_impact_england_child$bmi_prevalence_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')


table_outputs[["england_child"]] = policy_4b_impact_england_child$bmi_prevalence_table

write_xlsx(path = "outputs/child_policy/policy_4b_child/policy_4b.xlsx", x = table_outputs)


write.csv(policy_4b_impact_england_child$post_df, file = "outputs/child_policy/policy_4b_child/policy_4b_child_england_bmi.csv")


