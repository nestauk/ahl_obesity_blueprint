
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

gb_pop_18 = 51718632
days_model = 275

# Step 1: Estimating the impact of the policy using aggregated product purchase data:

# access info


ooh_aggregated_product_table <- s3read_using(FUN = read.csv,
                                             bucket = "ahl-obesity-blueprint",
                                             object = "inputs/processed/policy_9/ooh_aggregated_product_table.csv")


# Categorising products based on kcal thresholds on Page 18 of Calorie reduction programme technical guidance:
# Then, we adjust the kcal values of the products that are above the threshold to be just below the threshold.
# Then, we recalculate the total kcal per product = kcal per serving * cross prod where cross prod = quantity * gross up weight
purchase_products_df_categorised_grouped = ooh_aggregated_product_table %>%
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



