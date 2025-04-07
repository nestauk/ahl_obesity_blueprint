
#################################################################################################
# Policy 4a : End all volume offers for HFSS products which contribute significant sugar and    #
#             calories to children's diets and are of most concern for childhood obesity, in    #
#             the retail sector excluding small and micro businesses                            #
#################################################################################################

# Description:

# Population group: 
# Children: Age: 5 - 18; BMI Group: ≥ 85th percentile
# 
# Daily energy intake:
# Reduction in energy intake (averaged across age groups) = 2.5 kcals [Page 65, Table 23, UK Gov impact assessment,
# details also in rapid review - (https://docs.google.com/document/d/1Q2l1H2bHlEO2t6rK2fpbR43z3_qa8EJ_6h_CcjIEMDo/edit?usp=sharing]
# in the in-home sector.

# Compensation effects have been taken into account in the evidence. Therefore, we do not apply additional compensation.
# Net reduction in energy intake = 2.5 kcals

# We assume that the magnitude of effect is  proportional to the age of the person. For example, for 18 year olds we assume
# the same calorie reduction as is seen in adults. For 17 year olds, we assume less impact/ calorie reduction due to differences
# in reported daily calorie intake. Please see appendix for proportioned figures per age group and sex table.




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
# Effect size [A]: -2.5 kcals
# Population segment impacted by policy [B]: Children in age group 5 - 18 years
# Compensation effect [C]: 0 kcals
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -2.5 kcals


policy_4a_impact_england_child = calculate_bmi_from_eichange_hox(df = read_csv(here("inputs/processed/hse_2019_children.csv")),
                                                                 daily_ei_change = 2.5,
                                                                 nation = "England",
                                                                 tags = "Policy 4a Child" )


# 2.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_4a_impact_england_child$bmi_prevalence_plot


ggsave(here("outputs/child_policy/policy_4a_child/policy_4a_impact_England_child.png"), 
       plot = policy_4a_impact_england_child$bmi_prevalence_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')

# Output table with year on year distrubution of BMI categories
test = policy_4a_impact_england_child$bmi_prevalence_table

table_outputs[["england_child"]] = policy_4a_impact_england_child$bmi_prevalence_table


# writing outputs to folder:
write_xlsx(path = "outputs/child_policy/policy_4a_child/policy_4a.xlsx", x = table_outputs)

write.csv(policy_4a_impact_england_child$post_df, file = "outputs/child_policy/policy_4a_child/policy_4a_child_england_bmi.csv")




