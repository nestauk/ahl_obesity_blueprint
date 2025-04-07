
#################################################################################################
# Policy 33 : Regulate large retailers to change their organisation-wide converted NPM score    #
#             to ≥ 69 across their entire food product portfolio                                #
#                                                                                               #
#################################################################################################

# Description:


# The evidence from 
#
# showed that the intervention led to reduction in daily calorie intake by 78 kcal for adults. 
# The review did not include evidence on how this would affect daily calorie intake in children.
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

df_wales_cleaned = read_csv(here("inputs/processed/nsw_2019.csv"))

# 1.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: 78 kcals
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 0 kcals (already accounted in the final estimates shared in the evidence)
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = 78 kcals per person per day


policy_33_impact_wales_adult = calculate_bmi_from_eichange(df = df_wales_cleaned,
                                                             intake_change = -78,
                                                             implmentation_duration = 365*5, tags = "Wales | Policy 33")

policy_33_impact_wales_adult$bmi_percent_prevalence

# Bar plot of change in year on year distribution of different BMI categories
policy_33_impact_wales_adult$bmi_category_plot


table_outputs[["wales_adult"]] = policy_33_impact_wales_adult$bmi_percent_prevalence

# Exporting outputs:

ggsave(here("outputs/policy_33/policy_33_impact_wales_adult.png"), 
       plot = policy_33_impact_wales_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')

write_xlsx(path = "outputs/policy_33/policy_33_wales.xlsx", x = table_outputs)

write.csv(policy_33_impact_wales_adult$post_df, file = "outputs/policy_33/policy_33_adult_wales_bmi.csv")

