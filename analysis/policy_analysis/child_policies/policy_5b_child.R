
#################################################################################################
# Policy 5b : Ban all price promotions (e.g. was/now prices, introductory prices, temporary     #
#             price reductions) of HFSS foods for large out of home businesses such as          #
#             restaurants, coffee shops, fast food outlets (medium, small and micro businesses  #
#             are excluded)                                                                     #
#################################################################################################



# Population group: 
# Children: Age: 5 - 18; BMI Group: ≥ 85th percentile

# Daily energy intake:
# The effect size is taken from the Scottish Impact Assessment of price promotions
# in retail settings (https://www.gov.scot/publications/restricting-promotions-food-drink-high-fat-sugar-salt-partial-business-regulatory-impact-assessment/pages/2/)
# The effect size reported in the rapid review indicates that a similar policy for 
# take-home purchases in supermarkets led to a 87.6 kcal reduction in daily calorie
# intake for adults. The OOH sector accounts for approximately ~ 20% of an adult’s  
# daily calorie intake on average (approximately 300kcal). To get a proportion of daily
# energy intake, we use baseline data kcal intake for all adults at baseline using HSE
# and SHS data [300/ average daily energy intake). 
# Considering that a policy that affects 80% of the intake leads to a 87.6kcal reduction 
# in daily energy intake, a similar policy affecting 20% of daily energy intake is likely
# to be reduced by 21.9 kcals per day (20% x 87.6)/ 80% = 21.9  kcals
# We do not separately account for the compensation effect given that the evidence source
# has also accounted for it.We assume that the magnitude of effect is  proportional to the
# age of the person, and apply weights to calculate the adjusted calorie reduction.


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
# Effect size [A]: (20% x 87.6)/ 80% = 5.15  kcals
# Population segment impacted by policy [B]: Children in age group 5 - 18 years
# Compensation effect [C]:  0
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -21.9 kcals


policy_5b_impact_england_child = calculate_bmi_from_eichange_hox(df = read_csv(here("inputs/processed/hse_2019_children.csv")),
                                                                 daily_ei_change = 5.15,
                                                                 nation = "England", 
                                                                 tags = "Policy 5b Child" )


# 1.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_5b_impact_england_child$bmi_prevalence_plot


ggsave(here("outputs/child_policy/policy_5b_child/policy_5b_impact_England_child.png"), 
       plot = policy_5b_impact_england_child$bmi_prevalence_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')

policy_5b_impact_england_child$bmi_prevalence_table

table_outputs[["england_child"]] = policy_5b_impact_england_child$bmi_prevalence_table

write_xlsx(path = "outputs/child_policy/policy_5b_child/policy_5b.xlsx", x = table_outputs)


write.csv(policy_5b_impact_england_child$post_df, file = "outputs/child_policy/policy_5b_child/policy_5b_child_england_bmi.csv")


