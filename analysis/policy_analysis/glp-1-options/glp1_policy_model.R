# =============================================================================
# New GLP-1 policy: Treat ~1.3 million people with highest clinical need with 
# incretin memetics (GLP-1 drugs) through a digital first wrap around care model
# For policy details please refer to methods tab GLP-1 Policy:
# (https://docs.google.com/document/d/1HlwxabmrB4mv68lm74skqrQUoXc-bFzji2f57JTcWvA/edit?tab=t.chv3qxr2cnrg)
# =============================================================================

# Functions required for the model:
# Function to assign eligibility criteria for treatment based on BMI and number of comorbidities
assign_eligibility <- function(data,
                               bmi_high        = BMI_THRESHOLD_FOR_ELIGIBILITY,
                               bmi_low         = BMI_THRESHOLD_FOR_ELIGIBILITY_LOWER,
                               score_threshold = MINIMUM_NUMBER_OF_COMORBIDITIES) {
  data %>% mutate(eligibility = case_when(
    bmi >= bmi_high & eligibility_score >= score_threshold ~ 1,
    bmi >= bmi_high & eligibility_score <  score_threshold ~ 2,
    bmi >= bmi_low  & eligibility_score >= score_threshold ~ 3,
    TRUE ~ 0
  ))
}


# Function to run model for cohort allocation, implements five key steps:
# 1. selects intervention sample
# 2. Assigns weight loss based on type 2 diabetes status
# 3. Recomputes new BMI and weight based on the weight loss
# 4. Estimates BMI category wise prevalence
# 5. Estimates the relative change in BMI category prevalence
run_cohort <- function(cohort_allocation,
                       baseline_df,
                       n_selection_per_draw,
                       num_years = 5,
                       seed = 42) {
  
  set.seed(seed)
  
  # Step 1: select the intervention sample
  intervention_sample <- select_intervention_sample_dt(
    data = baseline_df,
    cohort_allocations = cohort_allocation,
    weight_var = "wt_int",
    num_years = num_years,
    cohort_var = "eligibility",
    weight_var_2 = "pop_estimate",
    n_selection_per_draw = n_selection_per_draw
  )
  
  # Step 2: assign weight loss (18.5% without T2D, 13.8% with T2D)
  post_df_adult <- assign_weight_loss(
    data = intervention_sample,
    bodyweight_var = "weight",
    num_years = num_years,
    weight_loss_percent = TIRZ_MEAN_EFFECTS$WEIGHT_LOSS_WITHOUT_T2D,
    weight_loss_percent_with_diabetes = TIRZ_MEAN_EFFECTS$WEIGHT_LOSS_WITH_T2D
  )
  
  # Step 3: recompute weights and BMI
  post_df_adult_updated <- estimate_new_weights_bmi(df = post_df_adult)
  
  # Step 4: year-wise BMI category
  year_wise_prevalence <- get_yearwise_bmi_category_counts(
    data = post_df_adult_updated,
    pop_weight_col = "pop_estimate",
    intervention_cols = post_df_adult_updated %>% select(starts_with("intervention")) %>% colnames(),
    treatment_cols = post_df_adult_updated %>% select(starts_with("actual")) %>% colnames(),
    untreated_cat_col = "bmi_class",
    treated_cat_col = "new_bmi_class"
  )
  
  bmi_cols <- c("type", "underweight", "normal", "overweight", "obese", "morbidly obese")
  
  prevalence_pivoted <- year_wise_prevalence %>%
    select(bmi_category, type, prevalence) %>%
    pivot_wider(names_from = bmi_category, values_from = prevalence) %>%
    select(all_of(bmi_cols))
  
  counts_pivoted <- year_wise_prevalence %>%
    select(bmi_category, type, total_weight) %>%
    pivot_wider(names_from = bmi_category, values_from = total_weight) %>%
    select(all_of(bmi_cols))
  
  # Step 5: Extract relative change in BMI categories:
  bmi_change_year <- extract_relative_change(data = prevalence_pivoted)
  
  # pound_benefit <- extract_pound_benefit_by_class(data = bmi_change_year, duration = num_years)
  
  list(
    prevalence    = prevalence_pivoted,
    counts        = counts_pivoted,
    bmi_change    = bmi_change_year,
    # pound_benefit = pound_benefit,
    detailed_table = post_df_adult_updated
  )
}

# loading all necessary data, functions and parameters
source(file = "analysis/policy_analysis/glp-1-options/utils.R")
source(file = "analysis/policy_analysis/glp-1-options/load_params.R")
source(file = "analysis/policy_analysis/glp-1-options/load_prep_data.R")
source(file = "post_processing/post_processing.R")


# Running the model
glp1_model_results <- purrr::imap(COHORT_ALLOCATIONS, function(alloc, op_name) {
  message("Running model for ", op_name, " ...")
  str(alloc)
  run_cohort(
    cohort_allocation    = alloc,
    baseline_df          = assign_eligibility(df_2019_adult_with_pop),
    n_selection_per_draw = NUMBER_OF_SELECTIONS_PER_DRAW,
    num_years            = NUMBER_OF_YEARS,
    seed                 = 42
  )
})

