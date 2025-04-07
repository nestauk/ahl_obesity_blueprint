
#################################################################################################
# Policy 20 : Mandate the inclusion of of health-based standards in catering contracts that     #
#             serve public spaces (e.g. hospitals, prisons, social care).                       #
#                                                                                               #
#################################################################################################

# Description:


# The evidence from the rapid review  
# (https://docs.google.com/document/d/16rShohZPMtiC_TiBe7mdIaJ76L2AfT9KWNlyWgpOCZc/edit) (quality assured
# by the EAG) showed that the intervention managed to remove 48.6 kcals available in public spaces.
# Workplace & canteens together form about 10.3% of the trips that individuals make in the out of home sector.
# In addition, it is also known that ~ 20% of the daily calorie intake comes from the out of home sector.
# The specific channel where this reduction in availability occurs is 10.3% of 20% of a persons daily calorie 
# intake which is 2.06% of a persons daily calorie intake.
# therefore, a 48.6 kcal reduction in availability could result in removal of 1.001 kcals from an adults daily
# calorie intake.


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

df_2019_wales = read_csv(here("inputs/processed/nsw_2019.csv"))

# 1.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: 1.001 kcals
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 23% of [A] = 0.23023
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -0.77077 kcals

policy_20_impact_wales_adult = calculate_bmi_from_eichange(df = df_2019_wales,
                                                             intake_change = -0.771,
                                                             implmentation_duration = 365*5, tags = "Wales | Policy 20")

# 1.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_20_impact_wales_adult$bmi_category_plot


# Output table with year on year distribution of BMI categories
policy_20_impact_wales_adult$bmi_percent_prevalence

table_outputs[["wales_adult"]] = policy_20_impact_wales_adult$bmi_percent_prevalence


# 3. Exporting outputs

ggsave(here("outputs/policy_20/policy_20_impact_wales_adult.png"), 
       plot = policy_20_impact_wales_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')


write_xlsx(path = "outputs/policy_20/policy_20_wales.xlsx", x = table_outputs)

write.csv(policy_20_impact_wales_adult$post_df, file = "outputs/policy_20/policy_20_adult_wales_bmi.csv")
