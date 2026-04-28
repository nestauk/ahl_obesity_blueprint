
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

source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")
# source(file = "models/child_model_calorie.R")
# source(file = "models/child_model_calorie_henry.R")

table_outputs = list() # creating a list of table outputs to be saved as an excel file


# Estimating the impact of the policy in:

# 1. Adults in England:

# 1.1. Cleaning the input/ baseline data:
process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab", nation = "England", population_group = "Adult")

# 1.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: ﹣22 kcal
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 23% of [A] = 5.06
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -16.94 kcals

policy_10_impact_england_adult = calculate_bmi_from_eichange(df = read_csv(here("inputs/processed/hse_2019.csv")),
                                                            intake_change = -16.94,
                                                            implmentation_duration = 365*5)

# 1.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_10_impact_england_adult$bmi_category_plot

ggsave(here("outputs/policy_10/policy_10_impact_England_adult.png"), 
       plot = policy_10_impact_england_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')

# Output table with year on year distribution of BMI categories
t_eng = policy_10_impact_england_adult$bmi_percent_prevalence

table_outputs[["england_adult"]] = policy_10_impact_england_adult$bmi_percent_prevalence



# 2. Children in England

# 2.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab",
                   nation = "England",
                   population_group = "Children")


# 2.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: -21.3 kcal
# Population segment impacted by policy [B]: Children in age group 5 - 18 years
# Compensation effect [C]: 23% of [A] = 5.4625
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -16.4 kcals

# policy_10_impact_england_child = calculate_child_bmi_from_eichange(df = read_csv(here("inputs/processed/hse_2019_children.csv")),
#                                                                   intake_change = -18.29,
#                                                                   implementation_duration = 365*5, 
#                                                                   use_bodyfat_curves = 0)


policy_10_impact_england_child = calculate_bmi_from_eichange_hox(df = read_csv(here("inputs/processed/hse_2019_children.csv")),
                                                                 daily_ei_change = 16.4,
                                                                 nation = "England",
                                                                 tags = "Policy 10")


# 2.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_10_impact_england_child$bmi_prevalence_plot

ggsave(here("outputs/policy_10/policy_10_impact_England_child.png"), 
       plot = policy_10_impact_england_child$bmi_prevalence_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')

# Output table with year on year distrubution of BMI categories
policy_10_impact_england_child$bmi_prevalence_table

table_outputs[["england_child"]] = policy_10_impact_england_child$bmi_prevalence_table


# 3. Adults in Scotland

# 3.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/shes19i_eul.tab", nation = "Scotland", population_group = "Adult")

# 3.2. Estimating the impact of the intervention on prevalence of obesity:


# Inputs to the model:
# Effect size [A]: ﹣22 kcal
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 23% of [A] = 5.06
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -16.94 kcals



policy_10_impact_scotland_adult = calculate_bmi_from_eichange(df = read_csv(here("inputs/processed/shes_2019.csv")),
                                                             intake_change = -16.94,
                                                             implmentation_duration = 365*5)
# 3.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_10_impact_scotland_adult$bmi_category_plot

ggsave(here("outputs/policy_10/policy_10_impact_Scotland_adult.png"), 
       plot = policy_10_impact_scotland_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')


# Output table with year on year distrubution of BMI categories
policy_10_impact_scotland_adult$bmi_percent_prevalence

table_outputs[["scotland_adult"]] = policy_10_impact_scotland_adult$bmi_percent_prevalence


# 4. Children in Scotland

# 4.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/shes19i_eul.tab",
                   nation = "Scotland",
                   population_group = "Children")


# 4.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: -21.3 kcal
# Population segment impacted by policy [B]: Children in age group 5 - 18 years
# Compensation effect [C]: 23% of [A] = 5.4625
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -16.4 kcals

# policy_10_impact_scotland_child = calculate_child_bmi_from_eichange(df = read_csv(here("inputs/processed/shes_2019_children.csv")),
#                                                                    intake_change = -18.29,
#                                                                    implementation_duration = 365*5, 
#                                                                    use_bodyfat_curves = 1)


policy_10_impact_scotland_child = calculate_bmi_from_eichange_hox(df = read_csv(here("inputs/processed/shes_2019_children.csv")),
                                                                  daily_ei_change = 16.4, 
                                                                  nation = "Scotland",
                                                                  tags = "POlicy 10")



# 4.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_10_impact_scotland_child$bmi_prevalence_plot

ggsave(here("outputs/policy_10/policy_10_impact_Scotland_child.png"), 
       plot = policy_10_impact_scotland_child$bmi_prevalence_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')


# Output table with year on year distrubution of BMI categories
policy_10_impact_scotland_child$bmi_prevalence_table

table_outputs[["scotland_child"]] = policy_10_impact_scotland_child$bmi_prevalence_table

write_xlsx(path = "outputs/policy_10/policy_10.xlsx", x = table_outputs)




# Output files for cost modelling
# Adult:
write.csv(policy_10_impact_england_adult$post_df, file = "outputs/policy_10/policy_10_adult_england_bmi.csv")
write.csv(policy_10_impact_scotland_adult$post_df, file = "outputs/policy_10/policy_10_adult_scotland_bmi.csv")

# Child:
write.csv(policy_10_impact_england_child$post_df, file = "outputs/policy_10/policy_10_child_england_bmi.csv")
write.csv(policy_10_impact_scotland_child$post_df, file = "outputs/policy_10/policy_10_child_scotland_bmi.csv")



# s3://ahl-private-data/data_requests/22-blueprint-inhome/kantar_2021_inhome_blueprint.csv

test_df <- s3read_using(FUN = read.csv,
                        bucket = "ahl-private-data",
                        object = "data_requests/22-blueprint-inhome/kantar_2021_inhome_blueprint.csv")
  
test_df_1 = test_df %>%
  mutate(kcal = grossed_up_energy_kcal/grossed_up_quantity,
         kcal_density = (grossed_up_energy_kcal/(grossed_up_volume*grossed_up_quantity))*100,
         guw = grossed_up_energy_kcal/ kcal)
  
  
  
  
library(arrow)
  




test_df_2 <- s3read_using(FUN = read_parquet,
                          bucket = "ahl-private-data",
                          object = "in_home/processed/targets/model_data.parquet")


colnames(test_df_2)


library(aws.s3)
library(writexl)

# add access information



test_df <- s3read_using(FUN = read.csv,
                        bucket = "ahl-private-data",
                        object = "data_requests/22-blueprint-inhome/kantar_2021_inhome_blueprint_2.csv")

n_distinct(test_df$Product.Long.Description)

products_df = test_df %>%
  select(Product.Long.Description) %>%
  distinct()

test_df_1 = test_df %>%
  dplyr::select(rst_4_extended, rst_4_market, rst_4_market_sector, rst_4_sub_market, rst_4_trading_area, Product.Long.Description) %>%
  distinct()


test_df_2 = test_df %>%
  dplyr::select(rst_4_extended, rst_4_market, rst_4_market_sector, rst_4_sub_market, rst_4_trading_area) %>%
  distinct() %>%
  writexl::write_xlsx(path = "outputs/policy_10/product_groups.xlsx")

test_df_1 %>%
  writexl::write_xlsx(path = "outputs/policy_10/product_groups_detailed.xlsx")


product_grouping_phe = readxl::read_xlsx(path = "outputs/policy_10/product_groups_final.xlsx") %>%
  rename(prod_long_desc = "Product.Long.Description")

key = c("rst_4_extended", "rst_4_market", "rst_4_market_sector", "rst_4_sub_market", "rst_4_trading_area", "prod_long_desc")


final_df = test_df %>%
  rename(prod_long_desc = "Product.Long.Description") %>%
  left_join(product_grouping_phe, by = key)

options(scipen = 999)
unique(final_df$phe_groups_final)

final_df = final_df %>%
  mutate(max_cal = case_when(phe_groups_final == "pastry_products" ~ 670,
                             phe_groups_final == "pizza" ~ 1230,
                             phe_groups_final == "chips_potato_products" ~ 270,
                             phe_groups_final == "cheese_garlic_bread" ~ 320,
                             phe_groups_final == "breaded_battered_products" ~ 320,
                             phe_groups_final == "complete_meal" ~ 570,
                             phe_groups_final == "meal_centre" ~ 410,
                             phe_groups_final == "crisps_savoury_snacks" ~ 205,
                             TRUE ~ energy_kcal)) %>%
  mutate(updated_kcal = case_when(phe_groups_final == "pastry_products" & energy_kcal > max_cal ~ 670,
                                  phe_groups_final == "pizza" & energy_kcal > max_cal ~ 1230 ,
                                  phe_groups_final == "chips_potato_products" & energy_kcal > max_cal ~ 270,
                                  phe_groups_final == "cheese_garlic_bread" & energy_kcal > max_cal ~ 320,
                                  phe_groups_final == "breaded_battered_products" & energy_kcal > max_cal ~ 320,
                                  phe_groups_final == "complete_meal" & energy_kcal > max_cal ~ 570,
                                  phe_groups_final == "meal_centre" & energy_kcal > max_cal ~ 410,
                                  phe_groups_final == "crisps_savoury_snacks" & energy_kcal > max_cal ~ 205,
                                  TRUE ~ energy_kcal)) %>%
  mutate(updated_kcal_2 = case_when(phe_groups_final == "pastry_products" & energy_kcal > max_cal ~ energy_kcal - 0.2*energy_kcal,
                                  phe_groups_final == "pizza" & energy_kcal > max_cal ~ energy_kcal - 0.2*energy_kcal,
                                  phe_groups_final == "chips_potato_products" & energy_kcal > max_cal ~ energy_kcal - 0.1*energy_kcal,
                                  phe_groups_final == "cheese_garlic_bread" & energy_kcal > max_cal ~ energy_kcal - 0.1*energy_kcal,
                                  phe_groups_final == "breaded_battered_products" & energy_kcal > max_cal ~ energy_kcal - 0.1*energy_kcal,
                                  phe_groups_final == "complete_meal" & energy_kcal > max_cal ~ energy_kcal - 0.1*energy_kcal,
                                  phe_groups_final == "meal_centre" & energy_kcal > max_cal ~ energy_kcal - 0.1*energy_kcal,
                                  phe_groups_final == "crisps_savoury_snacks" & energy_kcal > max_cal ~ energy_kcal - 0.05*energy_kcal,
                                  TRUE ~ energy_kcal)) %>%
  mutate(gross_up_wt = grossed_up_energy_kcal/ energy_kcal) %>%
  mutate(post_grossed_up_kcal = gross_up_wt * updated_kcal_2) %>%
  mutate(post_kcal = gross_up_wt * updated_kcal)



final_df = final_df %>%
  mutate(prod_grp = case_when(phe_groups_final == "pastry_products" & energy_kcal > max_cal ~ "pastry_over_670",
                                  phe_groups_final == "pizza" & energy_kcal > max_cal ~ "pizza_over_1230" ,
                                  phe_groups_final == "chips_potato_products" & energy_kcal > max_cal ~ "chips_over_270",
                                  phe_groups_final == "cheese_garlic_bread" & energy_kcal > max_cal ~ "garlic_bread_over_320",
                                  phe_groups_final == "breaded_battered_products" & energy_kcal > max_cal ~ "batter_over_320",
                                  phe_groups_final == "complete_meal" & energy_kcal > max_cal ~ "meal_over_570",
                                  phe_groups_final == "meal_centre" & energy_kcal > max_cal ~ "meal_cent_over_410",
                                  phe_groups_final == "crisps_savoury_snacks" & energy_kcal > max_cal ~ "crisps_over_205",
                                  TRUE ~ phe_groups_final))


unique(final_df$phe_groups_final)


product_share = final_df %>%
  group_by(prod_grp) %>%
  summarise(share = sum(gross_up_wt, na.rm = TRUE)) %>%
  mutate(percent_share = (share / sum(share)) * 100)


sum(final_df$gross_up_wt[final_df$prod_grp == "not_in_scope"])



sum(final_df$grossed_up_energy_kcal, na.rm = TRUE)
sum(final_df$post_grossed_up_kcal, na.rm = TRUE)

(sum(final_df$grossed_up_energy_kcal, na.rm = TRUE)*0.8 - sum(final_df$post_grossed_up_kcal, na.rm = TRUE)*0.8)/ 51718632 / 365


(sum(final_df$grossed_up_energy_kcal, na.rm = TRUE)*0.2 - sum(final_df$post_grossed_up_kcal, na.rm = TRUE)*0.2)/ 13403097 / 365


(sum(final_df$grossed_up_energy_kcal, na.rm = TRUE) - sum(final_df$post_grossed_up_kcal, na.rm = TRUE))/ 65121729 / 365


(sum(final_df$grossed_up_energy_kcal, na.rm = TRUE) - sum(final_df$post_kcal, na.rm = TRUE))/ 65121729 / 365


sum(final_df$grossed_up_energy_kcal, na.rm = TRUE)/ 65121729 / 365

