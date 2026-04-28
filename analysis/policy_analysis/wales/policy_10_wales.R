
#############################################################################################
# Policy 10 : Mandate maximum calorie reduction guidelines for retailers and manufacturers  #
#############################################################################################

# Description:
# The policy mandates all retailers and manufacturers reduce the calorie content of their products 
# to meet the maximum calorie guidelines. A similar policy, on a voluntary basis, was proposed by 
# Public Health England (PHE) under their Calorie reduction programme
# (https://www.gov.uk/government/publications/calorie-reduction-guidelines-for-the-food-industry). 
# The PHE guidelines indicate that for retailers and manufacturers the target was to reduce the calorie 
# content by 10% of the average calorie content of single serve portions.
# In addition, the evidence from Nesta's analysis of Kantar 2021 data shows that implementation of this policy
# leads to a 22 kcal reduction in intake at a population level among adults and 21.3 kcals amoong children.


# setup
rm(list = ls())
gc()
library(tidyverse)
library(here)
library(aws.s3)
library(writexl)

source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")


# add access information


pop_great_britain = 65121729
days_in_year = 365


# Getting file with the number of products in-home and out of home:
in_home_aggregated_product_table <- s3read_using(FUN = read.csv,
                                   bucket = "ahl-obesity-blueprint",
                                   object = "inputs/processed/policy_10/in_home_aggregated_product_table.csv")


# unique(final_df$prod_grp)

product_share = in_home_aggregated_product_table %>%
  group_by(prod_grp) %>%
  summarise(share = sum(cross_prod, na.rm = TRUE)) %>%
  mutate(percent_share = (share / sum(share)) * 100)



kcal_pp_pd_baseline = sum(in_home_aggregated_product_table$grossed_up_energy_kcal, na.rm = TRUE) / pop_great_britain / days_in_year
kcal_pp_pd_max_kcal_guideline = sum(in_home_aggregated_product_table$post_grossed_up_kcal_max_kcal_guideline, na.rm = TRUE) / pop_great_britain / days_in_year
kcal_pp_pd_kcal_pct_reduction = sum(in_home_aggregated_product_table$post_grossed_up_kcal_percent_reduction, na.rm = TRUE) / pop_great_britain / days_in_year

kcal_pp_pd_baseline - kcal_pp_pd_max_kcal_guideline

change_in_intake = kcal_pp_pd_baseline - kcal_pp_pd_kcal_pct_reduction

print(paste("change in energy intake (input for obesity modelling) =", change_in_intake))


table_outputs = list() # creating a list of table outputs to be saved as an excel file


# Estimating the impact of the policy in:

# 1. Adults in Wales:

# 1.1. Cleaned input/ baseline data:
df_wales_cleaned = read.csv(here("inputs/processed/nsw_2019.csv"))

# 1.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: ﹣22 kcal
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 23% of [A] = 5.06
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -16.94 kcals

policy_10_impact_wales_adult = calculate_bmi_from_eichange(df = df_wales_cleaned,
                                                           intake_change = -16.94,
                                                           implmentation_duration = 365*5, tags = "Wales | Policy 10")

# 1.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_10_impact_wales_adult$bmi_category_plot

# Output table with year on year distribution of BMI categories
policy_10_impact_wales_adult$bmi_percent_prevalence

table_outputs[["wales_adult"]] = policy_10_impact_wales_adult$bmi_percent_prevalence

# writing outputs to the folder:

ggsave(here("outputs/policy_10/policy_10_impact_wales_adult.png"), 
       plot = policy_10_impact_wales_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')

write_xlsx(path = "outputs/policy_10/policy_10_wales.xlsx", x = table_outputs)

write.csv(policy_10_impact_wales_adult$post_df, file = "outputs/policy_10/policy_10_adult_wales_bmi.csv")
