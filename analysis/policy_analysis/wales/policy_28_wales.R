
#################################################################################################
# Policy 28 : Implement a £3/kg tax on sugar and a £6/kg tax on salt sold for use in processed  #
#             foods or in restaurants and catering businesses                                   #
#                                                                                               #
#################################################################################################

# Description:


# The evidence from the IFS report
# (https://ifs.org.uk/sites/default/files/output_url_files/WP202121-The-impact-of-a-tax-on-added-sugar-and-salt.pdf) 
# indicates that the intervention removes 44.9 kcals from a persons daily calorie intake.
# It is not clear if a compensation effect has been applied, hence, before modelling the impact,
# a compensation effect will be applied.

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
# Effect size [A]: 44.9 kcals
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 23% of [A] = 10.327
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -34.573 kcals

policy_28_impact_wales_adult = calculate_bmi_from_eichange(df = df_wales_cleaned,
                                                             intake_change = -34.573,
                                                             implmentation_duration = 365*5, tags = "Wales | Policy 28")
# 1.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_28_impact_wales_adult$bmi_category_plot


# Output table with year on year distribution of BMI categories
policy_28_impact_wales_adult$bmi_percent_prevalence

table_outputs[["wales_adult"]] = policy_28_impact_wales_adult$bmi_percent_prevalence


# 3. Exporting outputs

ggsave(here("outputs/policy_28/policy_28_impact_wales_adult.png"), 
       plot = policy_28_impact_wales_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')

write_xlsx(path = "outputs/policy_28/policy_28_wales.xlsx", x = table_outputs)

write.csv(policy_28_impact_wales_adult$post_df, file = "outputs/policy_28/policy_28_adult_wales_bmi.csv")


