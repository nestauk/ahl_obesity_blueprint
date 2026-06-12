# =============================================================================
# GLP-1 options: run the pipeline across all cohort allocations
# =============================================================================

source(file = "post_processing/post_processing.R")
source(file = "analysis/policy_analysis/glp-1-options/utils.R")
source(file = "analysis/policy_analysis/glp-1-options/load_data.R")
source(file = "analysis/policy_analysis/glp-1-options/load_params.R")


# -----------------------------------------------------------------------------
# Cohort-invariant prep (Steps 1-4)
# -----------------------------------------------------------------------------

# Step 1: cleaned, processed baseline data
df_2019_adult <- bp_hse_2019_adult

# Step 2: eligibility criteria
# Assign '1' if an individual has a condition, then sum to an eligibility
# score (1-4) where 4 = 4 comorbidities.
df_2019_adult_eligibility <- df_2019_adult %>%
  mutate(
    cond_ascvd = case_when(
      cardiovd == 1 | platlets == 1 | ace_inhibitors == 1 | diuretics == 1 | lipid == 1 ~ 1,
      TRUE ~ 0
    ),
    cond_hypertension = case_when(hypertension == 1 ~ 1, TRUE ~ 0),
    cond_dyslipidaemia = case_when(lipid == 1 ~ 1, TRUE ~ 0),
    cond_diabetes = case_when(
      diabetes_type == 1 | metformin == 1 | anti_diabetics == 1 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  mutate(
    eligibility_score = cond_ascvd + cond_hypertension + cond_dyslipidaemia + cond_diabetes
  )

# Step 3: assign cohort eligibility flag from BMI
df_2019_adult_with_cohorts <- df_2019_adult_eligibility %>%
  mutate(eligibility = case_when(bmi >= 30 ~ 1, TRUE ~ 0))

# Step 4: blow up survey weights to population counts
df_2019_adult_with_pop <- df_2019_adult_with_cohorts %>%
  mutate(pop_share = wt_int / sum(wt_int)) %>%
  mutate(pop_estimate = round(pop_share * ENGLAND_ADULT_POPULATION, 0))

# Sanity check: blown-up weights vs ONS mid-year 2019 estimate
message(
  "Pop check (blown-up - ONS): ",
  sum(df_2019_adult_with_pop$pop_estimate) - ENGLAND_ADULT_POPULATION
)


# -----------------------------------------------------------------------------
# Cohort-dependent pipeline (Steps 5-8)
# Takes one cohort allocation and returns that option's results.
# -----------------------------------------------------------------------------

run_cohort <- function(cohort_allocation,
                       baseline_df,
                       n_selection_per_draw,
                       num_years = 5,
                       seed = 42) {
  
  set.seed(seed)
  
  # Step 5: select the intervention sample for this allocation
  intervention_sample <- select_intervention_sample_dt(
    data = baseline_df,
    cohort_allocations = cohort_allocation,
    weight_var = "wt_int",
    num_years = num_years,
    cohort_var = "eligibility",
    weight_var_2 = "pop_estimate",
    n_selection_per_draw = n_selection_per_draw
  )
  
  # Step 6: assign weight loss (18.5% without T2D, 13.8% with T2D)
  post_df_adult <- assign_weight_loss(
    data = intervention_sample,
    bodyweight_var = "weight",
    num_years = num_years,
    weight_loss_percent = TIRZ_MEAN_EFFECTS$WEIGHT_LOSS_WITHOUT_T2D,
    weight_loss_percent_with_diabetes = TIRZ_MEAN_EFFECTS$WEIGHT_LOSS_WITH_T2D
  )
  
  # Step 7: recompute weights and BMI
  post_df_adult_updated <- estimate_new_weights_bmi(df = post_df_adult)
  
  # Step 8: year-wise BMI category counts
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
  
  bmi_change_year <- extract_relative_change(data = prevalence_pivoted)
  
  pound_benefit <- extract_pound_benefit_by_class(data = bmi_change_year, duration = num_years)
  
  list(
    prevalence    = prevalence_pivoted,
    counts        = counts_pivoted,
    bmi_change    = bmi_change_year,
    pound_benefit = pound_benefit
  )
}


# -----------------------------------------------------------------------------
# Driver: read all cohort options and run each
# -----------------------------------------------------------------------------

# One read gives a named list: list(OP_1 = ..., OP_2 = ..., ...). The names
# come along for free as labels, so no need for the per-option get_param() calls.
cohort_allocations <- yaml::read_yaml(
  "analysis/policy_analysis/glp-1-options/cohorts.yaml"
)

# NOTE on n_per_draw: this passes a single value to every option. If the draw
# size is logically tied to each option (e.g. different rollout sizes), move it
# into cohorts.yaml alongside each allocation and read it per option here so the
# two can never drift apart.
results <- purrr::imap(cohort_allocations, function(alloc, op_name) {
  message("Running ", op_name, " ...")
  run_cohort(
    cohort_allocation    = alloc,
    baseline_df          = df_2019_adult_with_pop,
    n_selection_per_draw = n_per_draw,
    num_years            = 5,
    seed                 = 42
  )
})


# -----------------------------------------------------------------------------
# Results comparison tables
# -----------------------------------------------------------------------------

# Headline: pound benefit by class, per option
pound_benefit_comparison <- purrr::map(results, "pound_benefit") %>%
  bind_rows(.id = "cohort_option")

# Prevalence by year/category, per option
prevalence_comparison <- purrr::map(results, "prevalence") %>%
  bind_rows(.id = "cohort_option")
