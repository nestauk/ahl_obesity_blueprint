


# add access information


# Policy 8b: Saving required files:



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



s3write_using(purchase_mode_df_4_12, 
              bucket = "ahl-obesity-blueprint", 
              object = "inputs/processed/policy_8b/purchase_mode_aggregated.csv",
              FUN = utils::write.csv)





# Policy 12:

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

product_summary_table <- tibble(
  Category = c("In-Home Products", "Out-of-Home Products", "Total Products", 
               "Percent Reformulated", "kcal Reduction"),
  Value = c(in_home_products, ooh_products, total_products, 
            round(percent_of_products_reformulated, ), round(kcal_reduction, 2))
)



s3write_using(product_summary_table, 
              bucket = "ahl-obesity-blueprint", 
              object = "inputs/processed/policy_12/number_of_products.csv",
              FUN = utils::write.csv)




# Policy 10:


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
final_df_1 = in_home_data %>%
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

final_df = final_df_1 %>%
  select(-c(prod_long_desc)) %>%
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


s3write_using(final_df, 
              bucket = "ahl-obesity-blueprint", 
              object = "inputs/processed/policy_10/in_home_aggregated_product_table.csv",
              FUN = utils::write.csv)





# Policy 9:



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




s3write_using(purchase_products_df_categorised_grouped, 
              bucket = "ahl-obesity-blueprint", 
              object = "inputs/processed/policy_9/ooh_aggregated_product_table.csv",
              FUN = utils::write.csv)





