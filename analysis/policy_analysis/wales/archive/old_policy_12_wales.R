
################################################################################################### 
# Policy 12: Incentivise businesses to reformulate HFSS foods through a £500 million              #
#            reformulation grant fund                                                             #
#                                                                                                 #
###################################################################################################

# Description:


# The evidence is from the rapid review  
# (https://docs.google.com/document/d/1Q2l1H2bHlEO2t6rK2fpbR43z3_qa8EJ_6h_CcjIEMDo/edit?usp=sharing) 
# (quality assured by the EAG) supplemented by additional calculation:


# Number of in-home product categories = 1750 [1]
# Number of in-home products = 128,432


# number of OOH products = 13925 [1]

# 40% of British shopper baskets in in-home purchases are HFSS products = 40% of 128432 = 51373 [2]
# 66% of OOH products are HFSS = 66% of 13925 = 9152 [1] 

# Total number of products in-home + out of home = 142,357
# Total HFSS Products = 51373 (in-home) + 9152 (OOH) = 60,525


# Reformulation Pot = £500 million
# Assuming per product reformulation on average = £500,000 (Awaiting information from FDF Scotland on this)
# number of products that can be reformulated = 1000

# Percentage of HFSS products that can be reformulated = 1000/60525 = 1.65%
# Percentage of all products that can be reformulated = 1000/142357 = 0.7%

# Average calorie densiyt of OOH products = 1.92 kcal/gm
# average calorie density of in-home products = 2 kcal/gm

# On average, when participants consumed foods all foods in a day with lower energy density, they consumed 709 fewer
# calories per day compared to when they consumed all foods in a day with higher energy density.

# Therefore, if 0.7% of all products were reformulated, then individuals would consume:
# 0.7% x 709 = 4.96 kcals  i.e. 4.96 kcals fewer per day on average per day



# [1] Nesta Analysis of OOH food and drink data from market research company (2024)
# [2] Kantar (2021). Competing effectively in a HFSS- regulated world. 
#     Access at: https://www.kantar.com/uki/  inspiration/fmcg/2021-wp-competing-effectively-in-ahfss-regulated-world
# [3] Nesta Analysis of In Home food and drink data from market research company (2023)

# setup
rm(list = ls())
library(tidyverse)
library(here)
library(writexl)
library(aws.s3)

source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")



# access info

# add access information



# we want to get the number of products in the in-home and OOH sector: 

# reading in data files for estimating the number of products:

# We read in the following files to then subsequnetly merge them together to create the 
# final dataset that we will use to ascertain the number of products

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
  filter(month_no %in% c(4, 5, 6, 7, 8, 9, 10, 11, 12)) #1, 2, 3,


ooh_products = n_distinct(processed_purchased_df$unique_product_code)


in_home_purchase_df <- s3read_using(FUN = read.csv,
                                    bucket = "ahl-private-data",
                                    object = "data_requests/22-blueprint-inhome/kantar_2021_inhome_blueprint_2.csv")

in_home_products = n_distinct(in_home_purchase_df$Product.Long.Description)

total_products = in_home_products + ooh_products

percent_of_products_reformulated = (1000 / total_products) * 100

kcal_reduction = (round(percent_of_products_reformulated, 1) * 709) / 100








table_outputs = list()

# Estimating the impact of the policy in:

# 1. Adults in Wales:

# 1.1. Cleaned input/ baseline data:

df_wales_cleaned = read.csv(here("inputs/processed/nsw_2019.csv"))

# 1.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: 4.96 kcals - Estimated above in the variable kcal_reduction
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]:  23% of the change in daily energy intake = 23% * 4.96 = 1.14
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -3.81 kcals

policy_12_impact_wales_adult = calculate_bmi_from_eichange(df = df_wales_cleaned,
                                                             intake_change = -3.81,
                                                             implmentation_duration = 365*5, tags = "Wales | Policy 12")

# 1.3. Outputs

# Display outputs:

# Bar plot of change in year on year distribution of different BMI categories
policy_12_impact_wales_adult$bmi_category_plot


# Output table with year on year distribution of BMI categories
policy_12_impact_wales_adult$bmi_percent_prevalence

table_outputs[["wales_adult"]] = policy_12_impact_wales_adult$bmi_percent_prevalence


# writing outputs to folders:

ggsave(here("outputs/policy_12/policy_12_impact_wales_adult.png"), 
       plot = policy_12_impact_wales_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')


write_xlsx(path = "outputs/policy_12/policy_12_wales.xlsx", x = table_outputs)

write.csv(policy_12_impact_wales_adult$post_df, file = "outputs/policy_12/policy_12_adult_wales_bmi.csv")


