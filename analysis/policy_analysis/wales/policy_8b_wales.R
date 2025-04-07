
#################################################################################################
# Policy 8b : Restrict 'location' promotions of HFSS food and drink to food/ drink delivery     #
#             platforms                                                                         #
#################################################################################################

# Description:


# The evidence from the rapid review  
# (https://docs.google.com/document/d/1xYDMQdCBmFuSpww7D6qXdRaNTsfEbG5hByr1HBhVd3Q/edit?usp=sharing) 
# (quality assured by the EAG) showed that the intervention led to reduction in daily calorie intake
# by 38 kcals for adults. This relates to about 1700 kcals of peoples diets.
# Nesta's analysis of OOH sector purchase data has shown that 11% of the kcals purchased in the OOH sector
# is via delivery apps and restaurant owned apps.
# Therefore, a policy affecting 11% of ~300 kcals purchased in the OOH sector, would lead to
# 0.71 kcal reduction in daily calorie intake.


# References:
# [1] https://www.directlinegroup.co.uk/en/news/brand-news/2020/the-fast-food-generation--a-third-of-children-use-food-delivery-.html
# [2] Kantar 2021 report for Food Standards Scotland

# setup
rm(list = ls())
library(tidyverse)
library(here)
library(writexl)
library(aws.s3)


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
# Effect size [A]: 0.71 kcals
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 23% of [A] = 0.16 kcals
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -0.55 kcals

policy_8b_impact_wales_adult = calculate_bmi_from_eichange(df = df_wales_cleaned,
                                                             intake_change = -0.55,
                                                             implmentation_duration = 365*5, tags = "Wales | Policy 8b")


# 1.3. Outputs

# display outputs:

# Bar plot of change in year on year distribution of different BMI categories
policy_8b_impact_wales_adult$bmi_category_plot

# Output table with year on year distribution of BMI categories
policy_8b_impact_wales_adult$bmi_percent_prevalence


# writing outputs to folder:

ggsave(here("outputs/policy_8b/policy_8b_impact_wales_adult.png"), 
       plot = policy_8b_impact_wales_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')


table_outputs[["wales_adult"]] = policy_8b_impact_wales_adult$bmi_percent_prevalence

write_xlsx(path = "outputs/policy_8b/policy_8b_wales.xlsx", 
           x = table_outputs)


write.csv(policy_8b_impact_wales_adult$post_df, file = "outputs/policy_8b/policy_8b_adult_wales_bmi.csv")








# add access information


# Getting file with the latest channel labels
channel_labelling <- s3read_using(FUN = read.csv,
                                  bucket = "ahl-private-data",
                                  object = "ooh/processed/channel_labelling_v1.csv") %>%
  dplyr::select(shopcode, store_name, channel_level_1, channel_level_2 ) %>%
  rename(updated_channel_level_2 = channel_level_2,
         updated_channel_level_1 = channel_level_1)



# Reading in OOH purchase datafile into a dataframe and updating with new channel variables:
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


purchase_mode_df = processed_purchased_df %>%
  group_by(purchase_mode) %>%
  summarise(purchase_mode_wise_kcal = sum(kcal_serving_combined * gross_up_weight * quantity)) %>%
  mutate(kcal_percent_share = (purchase_mode_wise_kcal/sum(purchase_mode_wise_kcal)) * 100)



purchase_mode_df_4_12 = processed_purchased_df_4_12 %>%
  group_by(purchase_mode) %>%
  summarise(purchase_mode_wise_kcal = sum(kcal_serving_combined * gross_up_weight * quantity)) %>%
  mutate(kcal_percent_share = (purchase_mode_wise_kcal/sum(purchase_mode_wise_kcal)) * 100)



