
#################################################################################################
# Policy 27 : Incentivise retailer and out of home businesses to reformulate HFSS foods         #
#             through a £500 million reformulation grant fund                                   #
#                                                                                               #
#################################################################################################

# Population group: 
# Children: Age: 5 - 18; BMI Group: ≥ 85th percentile
# 
# Daily energy intake:
# The evidence is based on the effect of SDIL on daily calorie intake. Dickson et al. (2022)
# (https://www.econstor.eu/bitstream/10419/264048/1/vfs-2022-pid-69753.pdf) estimates that the SDIL
# removed 6,600 kcals per head per year in the UK. This is the equivalent of 18.08 kcals from an 
# adult's daily calorie intake. 

# And from the IFS report (Table 2.1), the calories from sugar in Milk drinks is 0.4% while that from
# soft drinks is 1.2%. The ratio is one-third. We assume that if a similar level of reformulation is 
# achieved then we'd expect to see a reduction equivalent to one-third that of soft drinks 
# (i.e. 1/3 x 18.08), equal to 6 kcals
# And then as a conservative estimate we take 50%, i.e. 3 kcals.
# In addition, we apply a compensation effect of 23%.

# We assume that the magnitude of effect is  proportional to the age of the person, and apply weights
# to calculate the adjusted calorie reduction.  For example, for 18 year olds we assume the same calorie
# reduction as is seen in adults. For 17 year olds, we assume less impact/ calorie reduction due to 
# differences in reported daily calorie intake.


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
# Effect size [A]: 3 kcals
# Population segment impacted by policy [B]: Children in age group 5 - 18 years
# Compensation effect [C]:  23% of [A] = 0.69 kcals
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -2.31 kcals


policy_27_impact_england_child = calculate_bmi_from_eichange_hox(df = read_csv(here("inputs/processed/hse_2019_children.csv")),
                                                                 daily_ei_change = 2.31,
                                                                 nation = "England", 
                                                                 tags = "Policy 27 Child" )


# 1.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_27_impact_england_child$bmi_prevalence_plot


ggsave(here("outputs/child_policy/policy_27_child/policy_27_impact_England_child.png"), 
       plot = policy_27_impact_england_child$bmi_prevalence_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')

policy_27_impact_england_child$bmi_prevalence_table

table_outputs[["england_child"]] = policy_27_impact_england_child$bmi_prevalence_table

write_xlsx(path = "outputs/child_policy/policy_27_child/policy_27.xlsx", x = table_outputs)

write.csv(policy_27_impact_england_child$post_df, file = "outputs/child_policy/policy_27_child/policy_27_child_england_bmi.csv")

