
#################################################################################################
# Policy 4a : End all volume offers for HFSS products which contribute significant sugar and    #
#             calories to children's diets and are of most concern for childhood obesity, in    #
#             the retail sector excluding small and micro businesses                            #
#################################################################################################

# Description:


# The evidence from the rapid review  
# (https://docs.google.com/document/d/1Q2l1H2bHlEO2t6rK2fpbR43z3_qa8EJ_6h_CcjIEMDo/edit?usp=sharing) 
# (quality assured by the EAG) showed that the intervention led to reduction in daily calorie intake
# by 2.6 kcal for adults. The policy was found to reduce daily calorie intake in children by 2.48 kcals.
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


table_outputs = list()

# Estimating the impact of the policy in:

# 1. Adults in Wales:

# 1.1. Cleaned input/ baseline data:

df_wales_cleaned = read.csv(here("inputs/processed/nsw_2019.csv"))

# 1.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: 2.6 kcals
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 0 kcals (already accounted in the final estimates shared in the evidence)
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -2.6 kcals

policy_4a_impact_wales_adult = calculate_bmi_from_eichange(df = df_wales_cleaned,
                                                             intake_change = -2.6,
                                                             implmentation_duration = 365*5,
                                                             tags = "Wales | Policy 4a")

# 1.3. Outputs

# viewing results:
# Bar plot of change in year on year distribution of different BMI categories
policy_4a_impact_wales_adult$bmi_category_plot

# Output table with year on year distribution of BMI categories
policy_4a_impact_wales_adult$bmi_percent_prevalence

table_outputs[["wales_adult"]] = policy_4a_impact_wales_adult$bmi_percent_prevalence


# writing results to folder:
ggsave(here("outputs/policy_4a/policy_4a_impact_wales_adult.png"), 
       plot = policy_4a_impact_wales_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')

write_xlsx(path = "outputs/policy_4a/policy_4a_wales.xlsx", x = table_outputs)

write.csv(policy_4a_impact_wales_adult$post_df, file = "outputs/policy_4a/policy_4a_adult_wales_bmi.csv")



