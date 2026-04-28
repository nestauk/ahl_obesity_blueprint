
#############################################################################################
# Policy 9 : Mandate maximum calorie per single serve portion guidelines for the OOH sector #
#############################################################################################

# Description:
# The policy mandates all out of home businesses to reduce the calorie content of their products 
# to meet the maximum calorie guidelines. A similar policy, on a voluntary basis, was proposed by 
# Public Health England (PHE) under their Calorie reduction programme
# (https://www.gov.uk/government/publications/calorie-reduction-guidelines-for-the-food-industry). 
# The PHE guidelines indicate that for out of home businesses the target was to reduce the calorie 
# content by 20% of the average calorie content of single serve portions.


# setup
rm(list = ls())
gc()
library(tidyverse)
library(here)
library(aws.s3)

source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")

# Step 1: Estimating the impact of the policy using purchase data:

# access info

# add access info


gb_pop_18 = 51718632
days_model = 275


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

# reading in household demographic information:
hh_demog = s3read_using(FUN = read.csv,
                        bucket = "ahl-private-data",
                        object = "ooh/processed/household_demog_table.csv")



# Updating dataframe with purchase mode variable, total spend and kcal
# then joining in the housegold demographic data and filtering to include only what is in scope as per the OOH Analysis report:
# (1) transactions for those aged 18 and over
# (2) transactions in the months April to December 2021
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

# checking the kcal per person per day
sum(processed_purchased_df$kcal_tot)/gb_pop_18/days_model


# Grouping business channels into business groups:
independent = c("full_service_independent", "fast_food_independent", "cafe_independent",
                "pubs_bars_independent", "bakery_independent", "convenience_stores_independent",
                "other_ooh", "supermarket_independent", "market_stalls", "leisure_venues", "workplace_canteen", 
                "hospital_food_courts", "vending_machines", "education_settings")

targets = c("pubs_bars_chain", "fast_food_chain", "full_service_chain", 
            "bakery_chain", "cafe_chain", "cafe_dessert")

retailers = c("supermarket_major", "convenience_stores_chain", "chemists", "discounters")

chain = c("pubs_bars_chain", "fast_food_chain", "full_service_chain", 
          "bakery_chain", "cafe_chain", "cafe_dessert", 
          "hotels") #

retailer_cafes = c("Asda Instore Cafe", "Debenhams Instore Café", "Tesco Cafe/Restaurant",
                   "Sainsbury's Instore Cafe", "Morrisons Instore Cafe", "John Lewis Cafe/Restaurant",
                   "Waitrose Instore Café", "M&S Instore Cafe")


# Grouping products to match product categories as per the Calorie reduction guidelines:
pizza = c("Pizza", "Pizza (slice)")

pastry = c("Croissants", "Danish Pastries", "Pasties", "Savoury Pastry")

sandwich = c("Breakfast roll / wrap / sandwich", "Sandwiches and wraps")

other = c("Apple", "Banana", "Blueberries", "Cake Bars", "Cakes",
          "Cherries", "Chewing Gum", "Chewy Bars", "Chilled Prepared Fruit",
          "Chocolate Assortments", "Chocolate Bars", "Chocolate Biscuit Bars",
          "Chocolate Confectionery", "Chocolate Spread", "Clementines",
          "Coffee", "Cookie", "Cream", "Cream Filled Eggs", 
          "Crunchy Bars", "Dips / Condiments", "Doughnuts", "Drink - fruit squash",
          "Drink - mixer", "Energy Drinks", "Flapjack", "Flavoured Milk",
          "Fruit Cherries+Peel", "Fruit Juice/Drink", "Fruit Snacking",
          "Fruit+Nut Snacking", "Grapes", "Ice Cream", "Iced Coffee", "Iced Tea",
          "Jams / Marmalades / Spreads / Honey", "Kiwi Fruit", "Melon", "Milk",
          "Mineral Water", "Mini Eggs", "Mozzarella Dpprs/Chs Side", "Muffins",
          "Nectarine", "Non Barcoded Prprd Fruit", "Nuts", "Olives", "Orange",
          "Other Fruit", "Other Hot Drinks", "Other Sweet", "Pain Au Chocolate",
          "Peach", "Pear", "Pineapple", "Plum", "Raspberries", "Ready to Serve Custard",
          "Ready to Serve Desserts", "Satsuma", "Sauces",
          "Small Swiss Roll", "Small Tarts", "Soft drink - carbonated flavours", "Sports Drinks",
          "Strawberries", "Sugar Candy", "Sugar Confectionery", "Sugar Fruits",
          "Sugar Liquorice Allsorts", "Sugar Mints", "Sugar Toffees", "Tea", "Teacakes",
          "Tinned Fruit", "Yoghurt Drinks And Juices")

savoury_snack = c("Savoury Crackers+Biscuits", "Crisps")


# adding a variable to include the new business groupings in the purchase df:
purchase_df_categorised = processed_purchased_df %>%
  mutate(test = case_when(updated_channel_level_1 %in% targets ~ "targets",
                          TRUE ~ "not_targets")) %>%
  mutate(type = case_when(updated_channel_level_1 %in% independent ~ "independent",
                          updated_channel_level_1 %in% chain ~ "chain",
                          updated_channel_level_1 %in% retailers ~ "retailer",
                          TRUE ~ "neither")) %>%
  mutate(type = case_when(store_name %in% retailer_cafes ~ "retailer",
                          TRUE ~ type))

# checking that the kcal per person per day is still 310.5 kcals
sum(purchase_df_categorised$kcal_tot)/gb_pop_18/days_model

# adding in the new product groupings in the purchase df:
purchase_products_df_categorised = purchase_df_categorised %>%
  mutate(product_category = case_when(kcal_serving_combined <=500 ~ "meal_side",
                                      kcal_serving_combined > 500 ~ "meal",
                                      TRUE ~ "to_update")) %>%
  mutate(product_category = case_when(Combined.category.cleaned %in% pastry ~ "pastry",
                                      Combined.category.cleaned %in% pizza ~ "pizza",
                                      Combined.category.cleaned %in% sandwich ~ "sandwich",
                                      Combined.category.cleaned %in% savoury_snack ~ "other",
                                      Combined.category.cleaned %in% other ~ "other",
                                      TRUE ~ product_category))

# checking that the kcal per person per day is still 310.5 kcals:
sum(purchase_products_df_categorised$pop_kcal)/ gb_pop_18/ days_model


# grouping the df by store, product type and kcals:
purchase_products_df_categorised_grouped = purchase_products_df_categorised %>%
  group_by(store, type, product_category, kcal_serving_combined) %>%
  summarise(cross_prod = sum(gross_up_weight*quantity)) %>%
  mutate(kcal_serving_wtd = (kcal_serving_combined*cross_prod/ cross_prod),
         kcal_serving_tot = kcal_serving_combined*cross_prod)

# checking that the kcal per person per day is still 310.5 kcals:
sum(purchase_products_df_categorised_grouped$kcal_serving_tot)/ gb_pop_18/ days_model


# Categorising products based on kcal thresholds on Page 18 of Calorie reduction programme technical guidance:
# Then, we adjust the kcal values of the products that are above the threshold to be just below the threshold.
# Then, we recalculate the total kcal per product = kcal per serving * cross prod where cross prod = quantity * gross up weight
purchase_products_df_categorised_grouped = purchase_products_df_categorised_grouped %>%
  ungroup()%>%
  mutate(updated_product_category = case_when(product_category == "meal" & kcal_serving_wtd < 860 ~ "meal_less_860",
                                              product_category == "meal" & kcal_serving_wtd > 860 & kcal_serving_wtd < 1345~ "meal_over_860",
                                              product_category == "meal" & kcal_serving_wtd > 1345 ~ "meal_over_1345",
                                              product_category == "meal_side" & kcal_serving_wtd < 375 ~ "meal_side_less_375",
                                              product_category == "meal_side" & kcal_serving_wtd >= 375 & kcal_serving_wtd < 600 ~ "meal_side_over_375",
                                              product_category == "meal_side" & kcal_serving_wtd >= 600 ~ "meal_side_over_600",
                                              product_category == "pizza" & kcal_serving_wtd < 830 ~ "pizza_less_830",
                                              product_category == "pizza" & kcal_serving_wtd >= 830 & kcal_serving_wtd < 1230 ~ "pizza_over_830",
                                              product_category == "pizza" & kcal_serving_wtd >= 1230 ~ "pizza_over_1230",
                                              product_category == "pastry" & kcal_serving_wtd < 430 ~ "pastry_less_430",
                                              product_category == "pastry" & kcal_serving_wtd >= 430 & kcal_serving_wtd < 670 ~ "pastry_over_430",
                                              product_category == "pastry" & kcal_serving_wtd >= 670 ~ "pastry_over_670",
                                              product_category == "sandwich" & kcal_serving_wtd < 480 ~ "sandwich_less_480",
                                              product_category == "sandwich" & kcal_serving_wtd >= 480 & kcal_serving_wtd < 580 ~ "sandwich_over_480",
                                              product_category == "sandwich" & kcal_serving_wtd >= 580 ~ "sandwich_over_580",
                                              TRUE ~ "unk")) %>%
  mutate(updated_kcal_serving = case_when(updated_product_category == "meal_over_1345" ~ 1344,
                                          updated_product_category == "meal_side_over_600" ~ 599,
                                          updated_product_category == "pizza_over_1230" ~ 1229,
                                          updated_product_category == "pastry_over_670" ~ 669,
                                          updated_product_category == "sandwich_over_580" ~ 579,
                                          TRUE ~ kcal_serving_wtd)) %>%
  mutate(post_kcals_tot = updated_kcal_serving*cross_prod,
         baseline_kcals_tot = kcal_serving_wtd*cross_prod)


# calculating the change in kcal per person per day:
sum(purchase_products_df_categorised_grouped$kcal_serving_tot)/ gb_pop_18/ days_model
sum(purchase_products_df_categorised_grouped$baseline_kcals_tot)/ gb_pop_18/ days_model
sum(purchase_products_df_categorised_grouped$post_kcals_tot)/ gb_pop_18/ days_model

# calculating the change in kcal per person per day grouped by new business groups
# we are only interested on the effect for large businesses:
reduc_kcal_pp_pd = purchase_products_df_categorised_grouped %>%
  ungroup() %>%
  group_by(type) %>%
  summarise(kcal_tot_baseline = sum(baseline_kcals_tot),
            kcal_tot_baseline_1 = sum(kcal_serving_tot),
            kcal_tot_post = sum(post_kcals_tot)) %>%
  mutate(kcal_pp_pd_baseline = kcal_tot_baseline/ gb_pop_18/ days_model,
         kcal_pp_pd_baseline_1 = kcal_tot_baseline_1/ gb_pop_18/ days_model,
         kcal_pp_pd_post = kcal_tot_post/ gb_pop_18/ days_model) %>%
  mutate(kcal_pp_pd_diff = kcal_pp_pd_baseline - kcal_pp_pd_post)

# reduction in daily kcals per person in Chain restaurants is 4.57 kcals
# In the OOH Analysis report, there were adjustments in the daily kcal per person figure to account for
# purchasing for others. The daily kcal per person is 296 kcals as per the report which is 4.5% below
# 310 kcals from this exercise. Therefore, the reduction in daily kcals is being reduced by 4.5%.
# Therefore, the reduction from this policy is 4.4 kcals


table_outputs = list() # creating a list of table outputs to be saved as an excel file


# Estimating the impact of the policy in:

# 1. Adults in Wales:

# 1.1. Cleaned input/ baseline data:
df_wales_cleaned = read.csv(here("inputs/processed/nsw_2019.csv"))


# 2. Estimating the impact of the intervention on prevalence of obesity:


# Inputs to the model:
# Effect size [A]: ﹣4.4 kcal
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 23% of [A] = 1 
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -3.4 kcals

policy_9_impact_wales_adult = calculate_bmi_from_eichange(df = df_wales_cleaned,
                                                            intake_change = -3.4,
                                                            implmentation_duration = 365*5, tags= "Wales | Policy 9")



# 1.3. Outputs

# Bar plot of change in year on year distribution of different BMI categories
policy_9_impact_wales_adult$bmi_category_plot

# Output table with year on year distribution of BMI categories
policy_9_impact_wales_adult$bmi_percent_prevalence

table_outputs[["wales_adult"]] = policy_9_impact_wales_adult$bmi_percent_prevalence


# writing outputs to the folder:
ggsave(here("outputs/policy_9/policy_9_impact_wales_adult.png"), 
       plot = policy_9_impact_wales_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')


write_xlsx(path = "outputs/policy_9/policy_9_wales.xlsx", x = table_outputs)

write.csv(policy_9_impact_wales_adult$post_df, file = "outputs/policy_9/policy_9_adult_wales_bmi.csv")



