


source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")
source(file = "config/config.R")
source(file = "post_processing/post_processing.R")
source(file = "analysis/policy_analysis/new_policies/testing/new_sampling_utils.R")




# Main analysis starts here:


# Step 1: reading in the cleaned processed baseline data file:
df_2019_adult = read_csv(here("inputs/processed/hse_2019.csv"))
# 
# df_2019_adult_eligibility = df_2019_adult %>%
#   mutate(cond_ascvd = case_when(cardiovd == 1 | platlets == 1 | ace_inhibitors == 1 | diuretics == 1 | lipid == 1 ~ 1,
#                                 TRUE ~ 0),
#          cond_hypertension =  case_when(hypertension == 1 ~ 1,
#                                         TRUE ~ 0),
#          cond_dyslipidaemia = case_when(lipid == 1 ~ 1,
#                                         TRUE ~ 0),
#          cond_diabetes = case_when(diabetes_type == 1 | metformin == 1 | anti_diabetics == 1 ~ 1,
#                                    TRUE ~ 0)) %>%
#   mutate(eligibility_score = cond_ascvd + cond_hypertension + cond_dyslipidaemia + cond_diabetes) %>%
#   mutate(eligibility = case_when(
#     (bmi >= 30) ~ 1,
#     # (bmi >= 32.5 & ethnicity %in% c(2, 3, 4, 5)) ~ 1,
#     TRUE ~ 0))

df_2019_adult_eligibility = df_2019_adult %>%
  mutate(cond_ascvd = case_when(cardiovd == 1 | platlets == 1 | ace_inhibitors == 1 | diuretics == 1 | lipid == 1 ~ 1,
                                TRUE ~ 0),
         cond_hypertension =  case_when(hypertension == 1 ~ 1,
                                        TRUE ~ 0),
         cond_dyslipidaemia = case_when(lipid == 1 ~ 1,
                                        TRUE ~ 0),
         cond_diabetes = case_when(diabetes_type == 1 | metformin == 1 | anti_diabetics == 1 ~ 1,
                                   TRUE ~ 0)) %>%
  mutate(eligibility_score = cond_ascvd + cond_hypertension + cond_dyslipidaemia + cond_diabetes)


df_2019_adult_with_cohorts = df_2019_adult_eligibility %>%
  mutate(eligibility = case_when(
    (bmi >= 40) & eligibility_score >= 3 ~ 1,
    (bmi >= 37.5 & ethnicity %in% c(2, 3, 4, 5)) & eligibility_score >= 3 ~ 1,
    (bmi >= 40) & eligibility_score == 2 ~ 2,
    (bmi >= 37.5 & ethnicity %in% c(2, 3, 4, 5)) & eligibility_score == 2 ~ 2,
    (bmi >= 40) & eligibility_score == 2 & cond_diabetes == 1 ~ 3,
    (bmi >= 37.5 & ethnicity %in% c(2, 3, 4, 5)) & eligibility_score == 2 & cond_diabetes == 1 ~ 3,
    (bmi >= 40) & eligibility_score == 1 & cond_diabetes == 1 ~ 4,
    (bmi >= 37.5 & ethnicity %in% c(2, 3, 4, 5)) & eligibility_score == 1 & cond_diabetes == 1 ~ 4,
    (bmi >= 35 & bmi < 40) & eligibility_score >= 3 ~ 5,
    (bmi >= 32.5 & bmi < 37.5 & ethnicity %in% c(2, 3, 4, 5)) & eligibility_score >= 3 ~ 5,
    (bmi >= 35 & bmi < 40) & eligibility_score == 2 ~ 6,
    (bmi >= 32.5 & bmi < 37.5 & ethnicity %in% c(2, 3, 4, 5)) & eligibility_score == 2 ~ 6,
    (bmi >= 35 & bmi < 40) & eligibility_score == 2 & cond_diabetes == 1 ~ 7,
    (bmi >= 32.5 & bmi < 37.5 & ethnicity %in% c(2, 3, 4, 5)) & eligibility_score == 2 & cond_diabetes == 1 ~ 7,
    TRUE ~ 0))




df_2019_adult_with_pop = df_2019_adult_with_cohorts %>%
  mutate(pop_share = wt_int/sum(wt_int)) %>%
  mutate(pop_estimate = round(pop_share * MODEL_CONSTANTS$ENGLAND_ADULT_POPULATION, 0))



set.seed(43)
cohort = COHORT_ALLOCATION_OP2
name = "option_2_efficacy"
weight_loss_with_t2d = TIRZ_MODEL_INPUTS_EFFICACY$WEIGHT_LOSS_WITH_T2D
weight_loss_without_t2d = TIRZ_MODEL_INPUTS_EFFICACY$WEIGHT_LOSS_WITHOUT_T2D

# Step 5: Selecting intervention sample:
# Selecting the intervention sample each year who will receive the intervention:
# From those eligible, we select a number of people for each cohort in an year
intervention_sample = select_intervention_sample_dt(data = df_2019_adult_with_pop, 
                                                    cohort_allocations = cohort,
                                                    #population_size = ENGLAND_ADULT_POPULATION,
                                                    weight_var = "wt_int",
                                                    num_years = MODEL_CONSTANTS$MODEL_DURATION,
                                                    cohort_var = "eligibility", weight_var_2 = "pop_estimate",
                                                    n_selection_per_draw = n_per_draw)

# Step 6: Assign weight loss
# Assign weight changes to individuals who were selected in the previous step
# We apply a weight loss of 18.5% for those without T2D and 13.8% for those with T2D
post_df_adult = assign_weight_loss(data = intervention_sample,
                                   bodyweight_var = "weight",
                                   num_years = MODEL_CONSTANTS$MODEL_DURATION,
                                   weight_loss_percent = weight_loss_without_t2d , # TIRZ_MODEL_INPUTS$WEIGHT_LOSS_WITHOUT_T2D,
                                   weight_loss_percent_with_diabetes = weight_loss_with_t2d) # TIRZ_MODEL_INPUTS$WEIGHT_LOSS_WITH_T2D


# Step 7: Calculate new weights and BMI
post_df_adult_updated_weights_bmi = estimate_new_weights_bmi(df = post_df_adult)

# Step 8: Get year wise BMI category percents and counts
year_wise_prevalence = get_yearwise_bmi_category_counts(data = post_df_adult_updated_weights_bmi,pop_weight_col = "pop_estimate",
                                                        intervention_cols = post_df_adult_updated_weights_bmi %>%
                                                          select(starts_with("intervention")) %>% colnames(),
                                                        treatment_cols = post_df_adult_updated_weights_bmi %>%
                                                          select(starts_with("actual")) %>% colnames(),
                                                        untreated_cat_col = "bmi_class",
                                                        treated_cat_col = "new_bmi_class" )



year_wise_prevalence_pivoted <- year_wise_prevalence %>%
  select(bmi_category, type, prevalence) %>%
  pivot_wider(
    names_from = bmi_category,
    values_from = prevalence,
  ) %>%
  select("type", "underweight", "normal",	"overweight",	"obese",	"morbidly obese")

# year and category wise counts:
year_wise_counts_pivoted <- year_wise_prevalence %>%
  select(bmi_category, type, total_weight) %>%
  pivot_wider(
    names_from = bmi_category,
    values_from = total_weight,
  ) %>%
  select("type", "underweight", "normal",	"overweight",	"obese",	"morbidly obese")



table_outputs = list()


# extract relative change:
bmi_change_year = extract_relative_change(data = year_wise_prevalence_pivoted)


table_outputs[["england_adult"]] = bmi_change_year


# extract pound benefit:
extract_pound_benefit_by_class(data = bmi_change_year, duration = 5)

# extract pound benefit:
table_outputs[["england_adult_with_benefits"]] = extract_pound_benefit(data = bmi_change_year, duration = 5, cost = MODEL_CONSTANTS$COST_OF_OBESITY_IN_BILLIONS)

benefit = extract_pound_benefit(data = bmi_change_year, duration = 5, cost = MODEL_CONSTANTS$COST_OF_OBESITY_IN_BILLIONS)

write_xlsx(path = paste0("outputs/new_policies/policy_38/method_2/", name, "_sum.xlsx"), x = table_outputs)
write.csv(post_df_adult, file = paste0("outputs/new_policies/policy_38/method_2/", name, "_bmi.csv"))


