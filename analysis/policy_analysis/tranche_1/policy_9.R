
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


# The evidence from the rapid review for Portions size interventions 
# (https://docs.google.com/document/d/1Mc9UahGny4g-gO_8mIzEHmMSgTQhhzZuDQ1Y-CQfYeg/edit?usp=sharing) 
# (quality assured by the EAG) that identified a meta-analysis showed that a 40% reduction in portion 
# sizes of products led to a reduction of 247 kcals in daily energy intake.

# !! ignore next two line!!:
# In case of this policy, for a 20% reduction, we assume that the reduction in daily energy intake is 
# approximately half of that reported by the review, that is 123.5 kcals `(intake_change)`.


# From OOH Analysis:
# Daily equivalent share of kcals from products > 1000 kcals in chains = 9.87%
# From the evidence we know that 40% reduction in portion sizes of all products results in 144 to 228 kcal
# reduction in daily energy intake.
# Now estimating the effect of a 10% reduction in portion sizes of products > 1000 kcals 
# Based on what we know:
# 1. 40% reduction in portion sizes of all products ----> 186 kcal reduction in DEI
# 2. 10% reduction in portion sizes of products > 1000 kcals in branded restaurants -----> ?
#    = [10% x 9.87% x 20% x (DEI) x 186]/[40% x DEI] = 0.92 kcals

# we do not apply compensation effect as the study has already accounted for it.



# setup
rm(list = ls())
gc()
library(tidyverse)
library(here)
library(aws.s3)

source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")
# source(file = "models/child_model_calorie.R")
source(file = "models/child_model_calorie_henry.R")

table_outputs = list() # creating a list of table outputs to be saved as an excel file


# Estimating the impact of the policy in:

# 1. Adults in England:

# 1.1. Cleaning the input/ baseline data:
process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab", nation = "England", population_group = "Adult")

# 2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: ﹣123.5 kcal [Updated to 0.92 kcals]
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 23% of [A] = 28.405 [Updated to 0 kcals]
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -95.1 kcals [Updated to 0.92 kcals]

# policy_9_impact_england_adult = calculate_bmi_from_eichange(df = read_csv(here("inputs/processed/hse_2019.csv")),
#                                                             intake_change = -95.1,
#                                                             implmentation_duration = 365*5)




# Inputs to the model:
# Effect size [A]: ﹣4.31 kcal
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 23% of [A] = 0.69 
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -3.62 kcals [updated to -0.92 kcals]


policy_9_impact_england_adult = calculate_bmi_from_eichange(df = read_csv(here("inputs/processed/hse_2019.csv")),
                                                            intake_change = -3.62,
                                                            implmentation_duration = 365*5)



# 1.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_9_impact_england_adult$bmi_category_plot

ggsave(here("outputs/policy_9/policy_9_impact_England_adult.png"), 
       plot = policy_9_impact_england_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')

# Output table with year on year distribution of BMI categories
policy_9_impact_england_adult$bmi_percent_prevalence

table_outputs[["england_adult"]] = policy_9_impact_england_adult$bmi_percent_prevalence



# 3. Adults in Scotland

# 3.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/shes19i_eul.tab", nation = "Scotland", population_group = "Adult")

# 3.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: ﹣123.5 kcal [updated to -0.92 kcal]
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 23% of [A] = 28.405 [updated to 0 kcals]
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -95.1 kcals [updated to -0.92 kcals]


# Inputs to the model:
# Effect size [A]: ﹣4.31 kcal
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 23% of [A] = 0.69 
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -3.62 kcals [updated to -0.92 kcals]




# policy_9_impact_scotland_adult = calculate_bmi_from_eichange(df = read_csv(here("inputs/processed/shes_2019.csv")),
#                                                              intake_change = -95.1,
#                                                              implmentation_duration = 365*5)

policy_9_impact_scotland_adult = calculate_bmi_from_eichange(df = read_csv(here("inputs/processed/shes_2019.csv")),
                                                             intake_change = -3.62,
                                                             implmentation_duration = 365*5)



# 3.3. Outputs
# Bar plot of change in year on year distribution of different BMI categories
policy_9_impact_scotland_adult$bmi_category_plot

ggsave(here("outputs/policy_9/policy_9_impact_Scotland_adult.png"), 
       plot = policy_9_impact_scotland_adult$bmi_category_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')


# Output table with year on year distrubution of BMI categories
policy_9_impact_scotland_adult$bmi_percent_prevalence

table_outputs[["scotland_adult"]] = policy_9_impact_scotland_adult$bmi_percent_prevalence



write_xlsx(path = "outputs/policy_9/policy_9.xlsx", x = table_outputs)



# Output files for cost modelling
# Adult:
write.csv(policy_9_impact_england_adult$post_df, file = "outputs/policy_9/policy_9_adult_england_bmi.csv")
write.csv(policy_9_impact_scotland_adult$post_df, file = "outputs/policy_9/policy_9_adult_scotland_bmi.csv")





# access info




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

#  sum(processed_purchased_df$kcal_tot)/gb_pop_18/days_model
#  unique(processed_purchased_df$year)
# "ooh/processed/household_demog_table.csv"

n_distinct(processed_purchased_df$unique_product_code)

n_distinct(processed_purchased_df$Combined.category.cleaned)

n_distinct(processed_purchased_df$product_code)

n_distinct(processed_purchased_df$Category.level.1)
n_distinct(processed_purchased_df$Meal.description)


  
  

calcs_df = processed_purchased_df

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


retailer_cafes = c("Asda Instore Cafe", "Debenhams Instore Café", "Tesco Cafe/Restaurant",
                   "Sainsbury's Instore Cafe", "Morrisons Instore Cafe", "John Lewis Cafe/Restaurant",
                   "Waitrose Instore Café", "M&S Instore Cafe")


test_df = processed_purchased_df %>%
  mutate(test = case_when(updated_channel_level_1 %in% targets ~ "targets",
                          TRUE ~ "not_targets")) %>%
  mutate(type = case_when(updated_channel_level_1 %in% independent ~ "independent",
                          updated_channel_level_1 %in% chain ~ "chain",
                          updated_channel_level_1 %in% retailers ~ "retailer",
                          TRUE ~ "neither")) %>%
  mutate(type = case_when(store_name %in% retailer_cafes ~ "retailer",
                          TRUE ~ type))

options(scipen = 999)
# ooh_calories = sum(test_df$pop_kcal)

# sum(test_df$pop_kcal[test_df$type == "chain"])

# proportion_chain = sum(test_df$pop_kcal[test_df$type == "chain"])/ sum(test_df$pop_kcal)




gb_pop_18 = 51718632
days_model = 275

chain_meals_2 = test_df 
#  %>%
#  filter(type == "chain")

# test_2 = as.data.frame(unique(chain_meals_2$store_name))



chain_meals_2 = chain_meals_2 %>%
  mutate(product_category = case_when(kcal_serving_combined <=500 ~ "meal_side",
                                      kcal_serving_combined > 500 ~ "meal",
                                      TRUE ~ "to_update")) %>%
  mutate(product_category = case_when(Combined.category.cleaned %in% pastry ~ "pastry",
                                      Combined.category.cleaned %in% pizza ~ "pizza",
                                      Combined.category.cleaned %in% sandwich ~ "sandwich",
                                      Combined.category.cleaned %in% savoury_snack ~ "other",
                                      Combined.category.cleaned %in% other ~ "other",
                                      TRUE ~ product_category))
#  mutate(updated_product_category = case_when(product_category == "meal" & kcal_serving_combined > 860))

sum(chain_meals_2$kcal_tot)/ gb_pop_18/days_model

chain_meals_3 = chain_meals_2 %>%
  group_by(store, type, product_category, kcal_serving_combined) %>%
  summarise(cross_prod = sum(gross_up_weight*quantity)) %>%
  mutate(kcal_serving_wtd = (kcal_serving_combined*cross_prod/ cross_prod),
         kcal_serving_tot = kcal_serving_combined*cross_prod)
  # summarise(kcal_mean_w = wtd.mean(kcal_serving_combined, weights = gross_up_weight*quantity, na.rm = TRUE))
  # summarise(kcals_serving = mean(kcal_serving_combined))

# unique(chain_meals_3$updated_product_category)
sum(chain_meals_3$kcal_serving_tot)/ gb_pop_18/ days_model


chain_meals_3 = chain_meals_3 %>%
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


sum(chain_meals_3$kcal_serving_tot)/ gb_pop_18/ days_model
sum(chain_meals_3$baseline_kcals_tot)/ gb_pop_18/ days_model
sum(chain_meals_3$post_kcals_tot)/ gb_pop_18/ days_model


reduc_kcal_pp_pd = chain_meals_3 %>%
  ungroup() %>%
  group_by(type) %>%
  summarise(kcal_tot_baseline = sum(baseline_kcals_tot),
         kcal_tot_baseline_1 = sum(kcal_serving_tot),
         kcal_tot_post = sum(post_kcals_tot)) %>%
  mutate(kcal_pp_pd_baseline = kcal_tot_baseline/ gb_pop_18/ days_model,
         kcal_pp_pd_baseline_1 = kcal_tot_baseline_1/ gb_pop_18/ days_model,
         kcal_pp_pd_post = kcal_tot_post/ gb_pop_18/ days_model) %>%
  mutate(kcal_pp_pd_diff = kcal_pp_pd_baseline - kcal_pp_pd_post)





product_share = chain_meals_3 %>%
  group_by(updated_product_category) %>%
  summarise(share = sum(cross_prod)) %>%
  mutate(percent_share = (share / sum(share)) * 100)
  
  
unique(chain_meals_2$type)

test_df_6 = chain_meals_2 %>%
  filter(nation %in% c("England", "Scotland", "Wales")) %>%
  filter(!is.na(updated_channel_level_1)) %>%
  group_by(type) %>%
  summarise(business_spend = sum(spend * gross_up_weight)) %>%
  mutate(spend_percent_share = (business_spend/sum(business_spend)) * 100) %>%
  arrange(desc(spend_percent_share)) 

  