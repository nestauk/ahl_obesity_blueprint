
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

# read in in-home product data
in_home_data <- s3read_using(FUN = read.csv,
                             bucket = "ahl-private-data",
                             object = "data_requests/22-blueprint-inhome/kantar_2021_inhome_blueprint_2.csv")

# list of products:
products_df = in_home_data %>%
  select(Product.Long.Description) %>%
  distinct()

rst_4_market_sector = in_home_data %>%
  dplyr::select(rst_4_market_sector) %>%
  distinct() %>%
  writexl::write_xlsx(path = "outputs/policy_10/rst_4_market_sector.xlsx")

# detailed products list with product groups and categories:
in_home_data_product = in_home_data %>%
  dplyr::select(rst_4_extended, rst_4_market, rst_4_market_sector, rst_4_sub_market, rst_4_trading_area, Product.Long.Description) %>%
  distinct() %>%
  writexl::write_xlsx(path = "outputs/policy_10/product_groups_detailed.xlsx")

# list unique product categories:
in_home_data_product_groups = in_home_data %>%
  dplyr::select(rst_4_extended, rst_4_market, rst_4_market_sector, rst_4_sub_market, rst_4_trading_area) %>%
  distinct() %>%
  writexl::write_xlsx(path = "outputs/policy_10/product_groups.xlsx")

# Subsequently, we classify the products into product groups based on PHE product categorisations
# as in Page 17 into one of the following groups, based on product descriptions:
# not_in_scope, pastry_products, pizza, chips_potato_products, cheese_garlic_bread,
# breaded_battered_products, complete_meal, meal_centre, crisps_savoury_snacks
# The updated sheet is then read in
product_grouping_phe = readxl::read_xlsx(path = "outputs/policy_10/product_groups_final.xlsx") %>%
  rename(prod_long_desc = "Product.Long.Description")

# we use the combination of the following as the key to match the new product categorisations
# to the purchase data:
key = c("rst_4_extended", "rst_4_market", "rst_4_market_sector", "rst_4_sub_market", "rst_4_trading_area", "prod_long_desc")

# then the purchase data file is updated with the new product categorisations:
final_df = in_home_data %>%
  rename(prod_long_desc = "Product.Long.Description") %>%
  left_join(product_grouping_phe, by = key)

# check that the no products with missing phe_group_final:
unique(final_df$phe_groups_final)

# creating a column in the purchase data file with the max calories permitted as per guidance.
# then we create a variable where the energy content of products is updated to the maximum kcal per product as per guidance.
# additionally we also create a variable where the energy content of product is updated based on the % reduction required
# grossed_up_energy_kcal = [gross up weight * energy kcal * quantity]
# using this we calculate cross_prod = grossed_up_energy_kcal / energy kcal which gives us the grossed up number of units of products sold
# we then multiple cross_prod with the new kcal values to calculate the grossed up population kcals:

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
  mutate(updated_kcal_max_kcal = case_when(phe_groups_final == "pastry_products" & energy_kcal > max_cal ~ 670,
                                           phe_groups_final == "pizza" & energy_kcal > max_cal ~ 1230 ,
                                           phe_groups_final == "chips_potato_products" & energy_kcal > max_cal ~ 270,
                                           phe_groups_final == "cheese_garlic_bread" & energy_kcal > max_cal ~ 320,
                                           phe_groups_final == "breaded_battered_products" & energy_kcal > max_cal ~ 320,
                                           phe_groups_final == "complete_meal" & energy_kcal > max_cal ~ 570,
                                           phe_groups_final == "meal_centre" & energy_kcal > max_cal ~ 410,
                                           phe_groups_final == "crisps_savoury_snacks" & energy_kcal > max_cal ~ 205,
                                           TRUE ~ energy_kcal)) %>%
  mutate(updated_kcal_percent_reduction = case_when(phe_groups_final == "pastry_products" & energy_kcal > max_cal ~ energy_kcal - 0.2*energy_kcal,
                                                    phe_groups_final == "pizza" & energy_kcal > max_cal ~ energy_kcal - 0.2*energy_kcal,
                                                    phe_groups_final == "chips_potato_products" & energy_kcal > max_cal ~ energy_kcal - 0.1*energy_kcal,
                                                    phe_groups_final == "cheese_garlic_bread" & energy_kcal > max_cal ~ energy_kcal - 0.1*energy_kcal,
                                                    phe_groups_final == "breaded_battered_products" & energy_kcal > max_cal ~ energy_kcal - 0.1*energy_kcal,
                                                    phe_groups_final == "complete_meal" & energy_kcal > max_cal ~ energy_kcal - 0.1*energy_kcal,
                                                    phe_groups_final == "meal_centre" & energy_kcal > max_cal ~ energy_kcal - 0.1*energy_kcal,
                                                    phe_groups_final == "crisps_savoury_snacks" & energy_kcal > max_cal ~ energy_kcal - 0.05*energy_kcal,
                                                    TRUE ~ energy_kcal)) %>%
  mutate(cross_prod = grossed_up_energy_kcal/ energy_kcal) %>%
  mutate(post_grossed_up_kcal_max_kcal_guideline = cross_prod * updated_kcal_max_kcal) %>%
  mutate(post_grossed_up_kcal_percent_reduction = cross_prod * updated_kcal_percent_reduction)

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

product_share = final_df %>%
  group_by(prod_grp) %>%
  summarise(share = sum(cross_prod, na.rm = TRUE)) %>%
  mutate(percent_share = (share / sum(share)) * 100)



kcal_pp_pd_baseline = sum(final_df$grossed_up_energy_kcal, na.rm = TRUE) / 65121729 / 365
kcal_pp_pd_max_kcal_guideline = sum(final_df$post_grossed_up_kcal_max_kcal_guideline, na.rm = TRUE) / 65121729 / 365
kcal_pp_pd_kcal_pct_reduction = sum(final_df$post_grossed_up_kcal_percent_reduction, na.rm = TRUE) / 65121729 / 365

kcal_pp_pd_baseline - kcal_pp_pd_max_kcal_guideline

kcal_pp_pd_baseline - kcal_pp_pd_kcal_pct_reduction


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
