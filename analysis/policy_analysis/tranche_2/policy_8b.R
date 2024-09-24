
#################################################################################################
# Policy 8b : Restrict 'location' promotions of HFSS food and drink to food/ drink delivery     #
#             platforms                                                                         #
#################################################################################################

# Description:


# The evidence from the rapid review  
# (https://docs.google.com/document/d/1xYDMQdCBmFuSpww7D6qXdRaNTsfEbG5hByr1HBhVd3Q/edit?usp=sharing) 
# (quality assured by the EAG) showed that the intervention led to reduction in daily calorie intake
# by 38 kcals for adults. This relates to about 1700 kcals of peoples diets.
# Nesta's analysis of OOH sector purchase data has shown that 10.6% of the kcals purchased in the OOH sector
# is via delivery apps and restaurant owned apps.
# Therefore, a policy affecting 10.6% of ~300 kcals purchased in the OOH sector, would lead to
# 0.71 kcal reduction in daily calorie intake.


# ignore below ###
# 44.97% of the purchases out of home is via delivery platforms. Therefore,
# this policy results in 44.97% of (20% of 38) which is 3.41 kcals. [2]
# Similarly for children, the evidence doesn't provide a separate estimate, therefore we use the same effect size
# as adults but adjust for usage among children. The usage of delivery apps in children is ~21% lower than adults
# by adjusting this, the effect for children is 2.69 kcals.[1]
# The source of the evidence also indicates that compensatory behaviour was accounted for while reporting
# out the final estimates of daily calorie reductions.
# ignore below ###

# References:
# [1] https://www.directlinegroup.co.uk/en/news/brand-news/2020/the-fast-food-generation--a-third-of-children-use-food-delivery-.html
# [2] Kantar 2021 report for Food Standards Scotland

# setup
rm(list = ls())
library(tidyverse)
library(here)
library(writexl)

source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")
#source(file = "models/child_model_calorie.R")
source(file = "models/child_model_calorie_henry.R")

table_outputs = list()

# Estimating the impact of the policy in:

# 1. Adults in England:

# 1.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab", nation = "England", population_group = "Adult")

# 1.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: 0.71 kcals
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 23% of [A] = 0.16 kcals
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -0.55 kcals

policy_8b_impact_england_adult = calculate_bmi_from_eichange(df = read_csv(here("inputs/processed/hse_2019.csv")),
                                                             intake_change = -0.55,
                                                             implmentation_duration = 365*5)
# 1.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_8b_impact_england_adult$bmi_category_plot

ggsave(here("outputs/policy_8b/policy_8b_impact_England_adult.png"), 
       plot = policy_8b_impact_england_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')

# Output table with year on year distribution of BMI categories
policy_8b_impact_england_adult$bmi_percent_prevalence

table_outputs[["england_adult"]] = policy_8b_impact_england_adult$bmi_percent_prevalence

# 2. Children in England

# # 2.1. Cleaning the input/ baseline data:
# 
# process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab",
#                    nation = "England",
#                    population_group = "Children")
# 
# 
# # 2.2. Estimating the impact of the intervention on prevalence of obesity:
# 
# # Inputs to the model:
# # Effect size [A]: 2.69 kcals
# # Population segment impacted by policy [B]: Children in age group 5 - 17 years
# # Compensation effect [C]: 23% of [A] = 0.62 kcals
# # Duration [D]: 5 years ~ 365 * 5 days
# 
# # Based on [A] and [C], the intake change = effect size - compensation effect = -2.07 kcals
# 
# # policy_8b_impact_england_child = calculate_child_bmi_from_eichange(df = read_csv(here("inputs/processed/hse_2019_children.csv")),
# #                                                                    intake_change = 5.12,
# #                                                                    implementation_duration = 365*5, 
# #                                                                    use_bodyfat_curves = 0)
# 
# 
# policy_8b_impact_england_child = calculate_bmi_from_eichange_hox(df = read_csv(here("inputs/processed/hse_2019_children.csv")),
#                                                                  daily_ei_change = 2.07,
#                                                                  nation = "England", 
#                                                                  tags = "Policy 8b" )
# 
# test_df = policy_8b_impact_england_child$post_df
# 
# # 2.3. Outputs
# # Bar plot of change in year on year distribution of different BMI categories
# policy_8b_impact_england_child$bmi_prevalence_plot
# 
# ggsave(here("outputs/policy_8b/policy_8b_impact_England_child.png"), 
#        plot = policy_8b_impact_england_child$bmi_prevalence_plot, 
#        width = 10, 
#        height = 6,
#        bg='#ffffff')
# 
# # Output table with year on year distrubution of BMI categories
# policy_8b_impact_england_child$bmi_prevalence_table
# 
# table_outputs[["england_child"]] = policy_8b_impact_england_child$bmi_prevalence_table


# 3. Adults in Scotland

# 3.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/shes19i_eul.tab", nation = "Scotland", population_group = "Adult")

# 3.2. Estimating the impact of the intervention on prevalence of obesity:


# Inputs to the model:
# Effect size [A]: 0.71 kcals
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 23% of [A] = 0.16 kcals
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -0.55 kcals

policy_8b_impact_scotland_adult = calculate_bmi_from_eichange(df = read_csv(here("inputs/processed/shes_2019.csv")),
                                                              intake_change = -0.55,
                                                              implmentation_duration = 365*5)
# 3.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_8b_impact_scotland_adult$bmi_category_plot

ggsave(here("outputs/policy_8b/policy_8b_impact_Scotland_adult.png"), 
       plot = policy_8b_impact_scotland_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')


# Output table with year on year distrubution of BMI categories
policy_8b_impact_scotland_adult$bmi_percent_prevalence

table_outputs[["scotland_adult"]] = policy_8b_impact_scotland_adult$bmi_percent_prevalence

# 
# # 4. Children in Scotland
# 
# # 4.1. Cleaning the input/ baseline data:
# 
# process_clean_save(file_path = "inputs/raw/shes19i_eul.tab",
#                    nation = "Scotland",
#                    population_group = "Children")
# 
# 
# # 4.2. Estimating the impact of the intervention on prevalence of obesity:
# 
# # Inputs to the model:
# # Effect size [A]: 2.69 kcals
# # Population segment impacted by policy [B]: Children in age group 5 - 17 years
# # Compensation effect [C]: 23% of [A] = 0.62 kcals
# # Duration [D]: 5 years ~ 365 * 5 days
# 
# # Based on [A] and [C], the intake change = effect size - compensation effect = -2.07 kcals
# 
# # policy_8b_impact_scotland_child = calculate_child_bmi_from_eichange(df = read_csv(here("inputs/processed/shes_2019_children.csv")),
# #                                                                     intake_change = 2.48,
# #                                                                     implementation_duration = 365*5, 
# #                                                                     use_bodyfat_curves = 1)
# 
# policy_8b_impact_scotland_child = calculate_bmi_from_eichange_hox(df = read_csv(here("inputs/processed/shes_2019_children.csv")),
#                                                                   daily_ei_change =  2.07, 
#                                                                   nation = "Scotland",
#                                                                   tags = "Policy 8b")
# 
# 
# 
# # 4.3. Outputs
# # Bar plot of change in year on year distribution of different BMI categories
# policy_8b_impact_scotland_child$bmi_prevalence_plot
# 
# 
# 
# ggsave(here("outputs/policy_8b/policy_8b_impact_Scotland_child.png"), 
#        plot = policy_8b_impact_scotland_child$bmi_prevalence_plot, 
#        width = 10, 
#        height = 6,
#        bg='#ffffff')
# 
# 
# # Output table with year on year distrubution of BMI categories
# policy_8b_impact_scotland_child$bmi_prevalence_table
# 
# table_outputs[["scotland_child"]] = policy_8b_impact_scotland_child$bmi_prevalence_table

write_xlsx(path = "outputs/policy_8b/policy_8b.xlsx", 
           x = table_outputs)


write.csv(policy_8b_impact_england_adult$post_df, file = "outputs/policy_8b/policy_8b_adult_england_bmi.csv")
# write.csv(policy_8b_impact_england_child$post_df, file = "outputs/policy_8b/policy_8b_child_england_bmi.csv")

write.csv(policy_8b_impact_scotland_adult$post_df, file = "outputs/policy_8b/policy_8b_adult_scotland_bmi.csv")
# write.csv(policy_8b_impact_scotland_child$post_df, file = "outputs/policy_8b/policy_8b_child_scotland_bmi.csv")








# access info

# add access information

library(aws.s3)

# Getting file with the latest channel labels
channel_labelling <- s3read_using(FUN = read.csv,
                                  bucket = "ahl-private-data",
                                  object = "ooh/processed/channel_labelling_v1.csv") %>%
  dplyr::select(shopcode, store_name, channel_level_1, channel_level_2 ) %>%
  rename(updated_channel_level_2 = channel_level_2,
         updated_channel_level_1 = channel_level_1)




# Reading purchase datafile into a dataframe and updating with new channel variables:
ooh_purchase_df <- s3read_using(FUN = read.csv,
                                bucket = "ahl-private-data",
                                object = "ooh/processed/descriptive_analysis/purchases_trip_analysis_with_spend_v2.csv") %>%
  mutate(nation = case_when(region %in% c("South East", "London", "North East",
                                          "South West", "West Midlands", "East Midlands",
                                          "East of England", "Yorkshire and The Humber", "North West") ~ "England",
                            region %in% c("Wales") ~ "Wales",
                            region %in% c("Scotland") ~ "Scotland",
                            TRUE ~ "Unknown"),
         match_key = paste(channel, shop.description, sep = "_")) %>%
  left_join(channel_labelling, by = c("shop_code" =  "shopcode"))


hh_demog = s3read_using(FUN = read.csv,
                        bucket = "ahl-private-data",
                        object = "ooh/processed/household_demog_table.csv")



# Updating dataframe with purchase mode variable, total spend and kcal
processed_purchased_df = ooh_purchase_df %>%
  mutate(trip_num = paste(hh_no, ind_no, week_no, day, trip_id, sep = "_")) %>%
  mutate(purchase_mode = case_when(delivery.type %in% c("Just Eat - Delivery", "Deliveroo - Delivery", "Uber Eats - Delivery", "Just Eat - Collection",
                                                        "Deliveroo - Collection") ~ "delivery_apps",
                                   delivery.type %in% c("Restaurant's Web App Delivery",
                                                        "Restaurant's Web App - Collection") ~ "restaurant_app_delivery",
                                   delivery.type %in% c("Ordered at Counter - Collection") ~ "ordered_at_counter",
                                   delivery.type %in% c("Rang to Order - Collection", "Rang to Order - Delivery") ~ "rang_to_order",
                                   delivery.type %in% c("Not a Takeaway") ~ "in_premise")) %>%
  mutate(total_spend = spend*gross_up_weight) %>%
  mutate(kcal_tot = kcal_serving_combined * gross_up_weight * quantity) %>%
  mutate(hh_ind = paste(hh_no, ind_no, sep = "_")) %>%
  left_join(hh_demog, by = "hh_ind") %>%
  mutate(date_ymd = ymd(date)) %>%
  mutate(month_no = month(date_ymd),
         year_no = year(date_ymd),
         day_no = date(date_ymd)) %>%
  filter(Age > 17) %>%
  filter(year_no == 2021) %>%
  filter(month_no %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))



processed_purchased_df_4_12 = ooh_purchase_df %>%
  mutate(trip_num = paste(hh_no, ind_no, week_no, day, trip_id, sep = "_")) %>%
  mutate(purchase_mode = case_when(delivery.type %in% c("Just Eat - Delivery", "Deliveroo - Delivery", "Uber Eats - Delivery", "Just Eat - Collection",
                                                        "Deliveroo - Collection") ~ "delivery_apps",
                                   delivery.type %in% c("Restaurant's Web App Delivery",
                                                        "Restaurant's Web App - Collection") ~ "restaurant_app_delivery",
                                   delivery.type %in% c("Ordered at Counter - Collection") ~ "ordered_at_counter",
                                   delivery.type %in% c("Rang to Order - Collection", "Rang to Order - Delivery") ~ "rang_to_order",
                                   delivery.type %in% c("Not a Takeaway") ~ "in_premise")) %>%
  mutate(total_spend = spend*gross_up_weight) %>%
  mutate(kcal_tot = kcal_serving_combined * gross_up_weight * quantity) %>%
  mutate(hh_ind = paste(hh_no, ind_no, sep = "_")) %>%
  left_join(hh_demog, by = "hh_ind") %>%
  mutate(date_ymd = ymd(date)) %>%
  mutate(month_no = month(date_ymd),
         year_no = year(date_ymd),
         day_no = date(date_ymd)) %>%
  filter(Age > 17) %>%
  filter(year_no == 2021) %>%
  filter(month_no %in% c(4, 5, 6, 7, 8, 9, 10, 11, 12))

#  sum(processed_purchased_df$kcal_tot)/gb_pop_18/days_model
#  unique(processed_purchased_df$year)
# "ooh/processed/household_demog_table.csv"

purchase_mode_df = processed_purchased_df %>%
  group_by(purchase_mode) %>%
  summarise(purchase_mode_wise_kcal = sum(kcal_serving_combined * gross_up_weight * quantity)) %>%
  mutate(kcal_percent_share = (purchase_mode_wise_kcal/sum(purchase_mode_wise_kcal)) * 100)



purchase_mode_df_4_12 = processed_purchased_df_4_12 %>%
  group_by(purchase_mode) %>%
  summarise(purchase_mode_wise_kcal = sum(kcal_serving_combined * gross_up_weight * quantity)) %>%
  mutate(kcal_percent_share = (purchase_mode_wise_kcal/sum(purchase_mode_wise_kcal)) * 100)


















