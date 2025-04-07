
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

#  And from the IFS report (Table 2.1), the calories from sugar in Milk drinks is 0.4% while that from soft drinks is 
# 1.2%. The ratio is one-third. Then assumed that if a similar level of reformulation is achieved then we'd expect to 
# see a reduction equivalent to one-third that of soft drinks (i.e. 1/3 x 18.08). And then as a conservative estimate 
# took 50% of that (1/2 x 6). So an additional ~3 kcals due to extending SDIL to milk based drinks bringing the total to ~21 kcals.

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
source(file = "models/adult_model_calorie.R")


table_outputs = list()

# Estimating the impact of the policy in:

# 1. Adults in Wales:

# 1.1. Cleaned input/ baseline data:

df_wales_cleaned = read_csv(here("inputs/processed/nsw_2019.csv"))

# 1.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: 3.01 kcals
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 23% of [A] = 0.69
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -2.31 kcals

policy_27_impact_wales_adult = calculate_bmi_from_eichange(df = df_wales_cleaned,
                                                             intake_change = -2.31,
                                                             implmentation_duration = 365*5, tags = "Wales | Policy 27")


# 1.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_27_impact_wales_adult$bmi_category_plot


# Output table with year on year distribution of BMI categories
policy_27_impact_wales_adult$bmi_percent_prevalence

table_outputs[["wales_adult"]] = policy_27_impact_wales_adult$bmi_percent_prevalence

# 3. Exporting outputs

ggsave(here("outputs/policy_27/policy_27_impact_wales_adult.png"), 
       plot = policy_27_impact_wales_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')

write_xlsx(path = "outputs/policy_27/policy_27_wales.xlsx", x = table_outputs)

write.csv(policy_27_impact_wales_adult$post_df, file = "outputs/policy_27/policy_27_adult_wales_bmi.csv")
