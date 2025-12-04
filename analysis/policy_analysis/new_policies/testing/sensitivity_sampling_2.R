# Sensitivity Analysis of random sampling for Tirzepatide roll-out
# This analysis is being carried out to test the sensitivity of the outcome -
# Relative reduction in obesity prevalence to weighted random sampling

# setup:
library(tidyverse)
library(here)
library(writexl)
library(parallel)

# source files:
source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")
source(file = "config/config.R")
source(file = "post_processing/post_processing.R")
source(file = "analysis/policy_analysis/new_policies/utils.R")
source(file = "analysis/policy_analysis/new_policies/testing/new_sampling_utils.R")

# constants:
# ENGLAND_ADULT_POPULATION = 44263393
# WEIGHT_LOSS_WITH_T2D = 0.138
# WEIGHT_LOSS_WITHOUT_T2D = 0.185
# WEIGHT_REGAIN_POST_TREATMENT = 0



path = "outputs/new_policies/policy_38/sens_samp_1/method_2/"

# Functions:

#' FUNCTION 1: Single iteration obesity impact estimation
#'
#' @description
#' Performs one iteration of a simulation to assess the impact of Tir. policy.
#' Simulates individual selection for treatment, weight loss, and calculates 
#' resulting changes in obesity prevalence and economic benefits over a 5-year
#' period. The function simulates one possible implementation scenario of the 
#' obesity intervention policy by:
#'    1. Randomly selecting eligible individuals according to cohort allocations
#'       - uses the same func. select_intervention_sample() as in policy 38
#'    2. Applying weight loss based on diabetes status (13.8% with T2D, 18.5% without)
#'       - uses the same func. assign_weight_changes() as in policy 38
#'    3. Calculating new BMI classifications over 5 years
#'    4. Computing the reduction in obesity prevalence
#'    5. Estimating economic benefits from reduced obesity
#'
#' @param seed_value Integer. Random seed for reproducible patient selection
#' @param df_2019_adult_with_cohorts Dataframe containing eligible adult population 
#'   with cohort assignments based on NICE/NHSE guidelines
#' @param total_cost_obesity Numeric. Total cost of obesity to the healthcare system
#' @param obesity_cost_30_40 Numeric. Cost associated with BMI 30-40 obesity
#' @param obesity_cost_over_40 Numeric. Cost associated with BMI >40 obesity
#' @param implementation_period Numeric. Number of years. Default = 5
#'
#' @return A dataframe with one row containing:
#'   \itemize{
#'     \item seed: The random seed used
#'     \item baseline_obesity: Baseline obesity prevalence (%)
#'     \item year5_obesity: Year 5 obesity prevalence (%)
#'     \item reduction: Absolute reduction in obesity prevalence
#'     \item relative_reduction: Relative reduction in obesity prevalence (%)
#'     \item benefit: Economic benefit in pounds
#'     \item n_year1-5: Sum of wt_int of people treated each year
#'     \item diabetes_year1-5: Diabetes prevalence by treatment year
#'     \item bmi_year1-5: Mean BMI by treatment year
#'     \item weighted_n: Total weighted sample size
#'   }
#'
#'
#' @examples
#' \dontrun{
#' result <- run_single_iteration(
#'   seed_value = 123,
#'   df_2019_adult_with_cohorts = data,
#'   total_cost_obesity = 1000000,
#'   obesity_cost_30_40 = 500000,
#'   obesity_cost_over_40 = 500000,
#'   implementation_period = 5
#' )
#' }
#'
#' @seealso 
#' \code{\link{select_intervention_sample}} for patient selection
#' \code{\link{assign_weight_changes}} for weight loss assignment
#' \code{\link{extract_pound_benefit_by_class}} for economic calculations
#'
#' @export

run_single_iteration <- function(seed_value,
                                 cohort_allocation_input,
                                 df_2019_adult_with_cohorts,
                                 total_cost_obesity,
                                 obesity_cost_30_40,
                                 obesity_cost_over_40,
                                 implementation_period = 5) {
  # browser()
  # Setting seed:
  set.seed(seed_value)
  
  # Selecting intervention sample:
  invisible(capture.output(
    intervention_sample <- select_intervention_sample_dt(
                      data = df_2019_adult_with_cohorts, 
                      cohort_allocations = cohort_allocation_input,
                      weight_var = "wt_int",
                      num_years = MODEL_CONSTANTS$MODEL_DURATION,
                      cohort_var = "eligibility",
                      weight_var_2 = "pop_estimate",
                      n_selection_per_draw = n_per_draw)
  ))

  
  # intervention_sample = select_intervention_sample_dt(data = df_2019_adult_with_pop, 
  #                                                     cohort_allocations = cohort,
  #                                                     #population_size = ENGLAND_ADULT_POPULATION,
  #                                                     weight_var = "wt_int",
  #                                                     num_years = MODEL_CONSTANTS$MODEL_DURATION,
  #                                                     cohort_var = "eligibility", weight_var_2 = "pop_estimate",
  #                                                     n_selection_per_draw = n_per_draw)
  
  # selecting the required output table from previous step
  selected_data <- intervention_sample
  
  # getting overall preavlence of diabetes
  prop_diabetes <- mean(intervention_sample$cond_diabetes)
  
  # creating a single variable to identify the year in which an individual
  # is treated
  selected_data <- selected_data %>%
    mutate(
      treatment_year = case_when(
        intervention_year1 == "Yes" ~ 1,
        intervention_year2 == "Yes" ~ 2,
        intervention_year3 == "Yes" ~ 3,
        intervention_year4 == "Yes" ~ 4,
        intervention_year5 == "Yes" ~ 5,
        TRUE ~ NA_real_
      )
    )
  
  # Summary table by treatment year
  year_summary <- selected_data %>%
    group_by(treatment_year) %>%
    summarise(
      n = n(),
      weighted_n = sum(pop_estimate),
      diabetes_rate = weighted.mean(cond_diabetes, pop_estimate),
      mean_bmi = weighted.mean(bmi, pop_estimate)
    )
  
  # Assigning weight changes:
  # post_df_adult <- assign_weight_changes(
  #   data = intervention_sample$data,
  #   bodyweight_var = "weight",
  #   num_years = MODEL_CONSTANTS$MODEL_DURATION,
  #   weight_loss_percent = TIRZ_MODEL_INPUTS$WEIGHT_LOSS_WITHOUT_T2D,
  #   weight_loss_percent_with_diabetes = TIRZ_MODEL_INPUTS$WEIGHT_LOSS_WITH_T2D,
  #   weight_regain = TIRZ_MODEL_INPUTS$WEIGHT_REGAIN_POST_TREATMENT
  # )
  
  post_df_adult = assign_weight_loss(data = intervention_sample,
                                     bodyweight_var = "weight",
                                     num_years = MODEL_CONSTANTS$MODEL_DURATION,
                                     weight_loss_percent = TIRZ_MODEL_INPUTS$WEIGHT_LOSS_WITHOUT_T2D,
                                     weight_loss_percent_with_diabetes = TIRZ_MODEL_INPUTS$WEIGHT_LOSS_WITH_T2D)
  
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
  

  # Calculating obesity prevalence for year 5:
  bmi_prevalence <- year_wise_prevalence %>%
    filter(type %in% c("Year 5")) %>%
    filter(bmi_category %in% c("obese", "morbidly obese")) %>%
    pull(prevalence) %>%
    sum()
  
  # bmi_prevalence <- post_df_adult %>%
  #   count(bmi_5_class, wt = pop_estimate) %>%
  #   mutate(freq = n/sum(n)*100) %>%
  #   filter(bmi_5_class %in% c("obese", "morbidly obese")) %>%
  #   summarise(obesity_prevalence_y5 = sum(freq))
  
  # Calculating obesity prevalence for baseline:
  baseline_prevalence <- year_wise_prevalence %>%
    filter(type %in% c("Year 0")) %>%
    filter(bmi_category %in% c("obese", "morbidly obese")) %>%
    pull(prevalence) %>%
    sum()

  # Calculate reduction
  reduction <- (baseline_prevalence - bmi_prevalence)

  bmi_change_year_updated = extract_relative_change(data = year_wise_prevalence_pivoted)
  
  value_to_gov = extract_value_to_gov(data = bmi_change_year_updated,
                                      cost = MODEL_CONSTANTS$COST_OF_OBESITY_IN_BILLIONS,
                                      duration = MODEL_CONSTANTS$MODEL_DURATION)
  
  
  # Return results
  return(data.frame(
    seed = seed_value,
    baseline_obesity = baseline_prevalence,
    year5_obesity = bmi_prevalence,
    reduction = reduction,
    relative_reduction = (reduction / baseline_prevalence) * 100,
    benefit = value_to_gov$avg_annual_value,
    
    # Year 1 metrics
    n_year1 = get_year_value(1, "weighted_n", df = year_summary),
    diabetes_year1 = get_year_value(1, "diabetes_rate", df = year_summary),
    bmi_year1 = get_year_value(1, "mean_bmi", df = year_summary),
    
    # Year 2 metrics
    n_year2 = get_year_value(2, "weighted_n", df = year_summary),
    diabetes_year2 = get_year_value(2, "diabetes_rate", df = year_summary),
    bmi_year2 = get_year_value(2, "mean_bmi", df = year_summary),
    
    # Year 3 metrics
    n_year3 = get_year_value(3, "weighted_n", df = year_summary),
    diabetes_year3 = get_year_value(3, "diabetes_rate", df = year_summary),
    bmi_year3 = get_year_value(3, "mean_bmi", df = year_summary),
    
    # Year 4 metrics
    n_year4 = get_year_value(4, "weighted_n", df = year_summary),
    diabetes_year4 = get_year_value(4, "diabetes_rate", df = year_summary),
    bmi_year4 = get_year_value(4, "mean_bmi", df = year_summary),
    
    # Year 5 metrics
    n_year5 = get_year_value(5, "weighted_n", df = year_summary),
    diabetes_year5 = get_year_value(5, "diabetes_rate", df = year_summary),
    bmi_year5 = get_year_value(5, "mean_bmi", df = year_summary),
    
    
    # Additional useful metrics
    n_selected = nrow(intervention_sample),
    weighted_n = sum(intervention_sample$wt_int),
    pop_numbers = sum(intervention_sample$pop_estimate)
  ))
}



#' FUNCTION 2: Main fucntion to run the sensitivity analysis
#'
#' @description
#' Performs multiple iterations of the obesity intervention simulation. Creates
#' cohort eligibility based on NICE/NHSE guidelines and runs simulations with
#' different random seeds to check the sensitivity of obesity reduction 
#' estimates to random selection
#' The function performs the following steps:
#'    Loads and processes the HSE 2019 adult population data
#'    Applies NICE/NHSE eligibility criteria to assign individuals to cohorts
#'    Runs n_iterations simulations with different random seeds
#'    Each iteration randomly selects individuals and models intervention impact
#'    Combines results for statistical analysis of policy uncertainty
#'
#' @param n_iterations Integer. Number of iterations to run (default: 1000)
#' @param input_file_path Character. Path to the processed HSE 2019 data file 
#'   (default: "inputs/processed/hse_2019.csv")
#' @param ob_total_cost Numeric. Total cost of obesity 
#' @param ob_costs_30_40 Numeric. Cost associated with BMI 30-40 obesity
#' @param ob_costs_over_40 Numeric. Cost associated with BMI >40 obesity  
#' @param implementation_period Numeric. Duration, default = 5
#'
#' @return A dataframe with n_iterations rows containing results from each simulation:
#'        Each row represents one simulation iteration
#'        Columns include obesity reduction - absolute and relative
#'        benefits, disbetes prevalence, mean BMI per year
#'
#' @details
#' }
#' 
#' Eligibility cohorts are based on BMI thresholds, comorbidity scores (ASCVD, 
#' hypertension, dyslipidaemia, diabetes), and ethnicity adjustments for 
#' South Asian populations.
#'
#' @examples
#' \dontrun{
#' # Run sensitivity analysis with 1000 iterations
#' results <- run_sensitivity_analysis(
#'   n_iterations = 1000,
#'   ob_total_cost = 10000000,
#'   ob_costs_30_40 = 5000000,
#'   ob_costs_over_40 = 5000000,
#'   implementation_period = 5
#' )
#' 
#' # Analyze results
#' mean(results$relative_reduction)
#' sd(results$relative_reduction)
#' }
#'
#' @note 
#' Progress is displayed via pbapply progress bar. Failed iterations are logged
#' and excluded from final results. Requires sufficient memory for parallel processing.
#'

run_sensitivity_analysis <- function(n_iterations = 1000,
                                     cohort_alloc_input,
                                     input_file_path= "inputs/processed/hse_2019.csv",
                                     ob_total_cost,
                                     ob_costs_30_40,
                                     ob_costs_over_40,
                                     implementation_period) {
  
  
  df_2019_adult <- read_csv(here(input_file_path))
  
  # Applying eligibility criteria to create cohorts:
  df_2019_adult_eligibility = df_2019_adult %>%
    mutate(cond_ascvd = case_when(cardiovd == 1 | platlets == 1 | ace_inhibitors == 1 | diuretics == 1 | lipid == 1 ~ 1,
                                  TRUE ~ 0),
           cond_hypertension =  case_when(hypertension == 1 ~ 1,
                                          TRUE ~ 0),
           cond_dyslipidaemia = case_when(lipid == 1 ~ 1,
                                          TRUE ~ 0),
           cond_diabetes = case_when(diabetes_type == 1 | metformin == 1 | anti_diabetics == 1 ~ 1,
                                     TRUE ~ 0)) %>%
    mutate(eligibility_score = cond_ascvd + cond_hypertension + cond_dyslipidaemia + cond_diabetes) %>%
    mutate(eligibility = case_when(
      (bmi >= 30) ~ 1,
      # (bmi >= 32.5 & ethnicity %in% c(2, 3, 4, 5)) ~ 1,
      TRUE ~ 0))
  
  df_2019_adult_with_cohorts = df_2019_adult_eligibility %>%
    mutate(pop_share = wt_int/sum(wt_int)) %>%
    mutate(pop_estimate = round(pop_share * MODEL_CONSTANTS$ENGLAND_ADULT_POPULATION, 0))
  
  # Creating a sequence of seeds:
  seeds <- 1:n_iterations
  
  # Running iterations:
  cat("Running sensitivity analysis with", n_iterations, "iterations...\n")
  # browser()
  results <- pbapply::pblapply(seeds, function(s) {
    tryCatch({
      run_single_iteration(s,
                           df_2019_adult_with_cohorts,
                           total_cost_obesity = ob_total_cost,
                           obesity_cost_30_40 = ob_costs_30_40,
                           obesity_cost_over_40 = ob_costs_over_40,
                           implementation_period = implementation_period,
                           cohort_allocation_input = cohort_alloc_input)
    }, error = function(e) {
      # browser()
      cat("\nError in iteration", s, ":", e$message, "\n")
      return(NULL)
    })
  })
  
  # Combining results:
  results_df <- do.call(rbind, results[!sapply(results, is.null)])
  
  return(results_df)
}

# Main analysis:




for (cohort_name in names(all_cohorts_1Y)) {
  
  cohort_data <- all_cohorts[[cohort_name]]
  
  print(paste("working on cohort = ", cohort_name))
  # df to store results of the sensitivity analysis:

  sensitivity_results <- run_sensitivity_analysis(n_iterations = 50,
                                                  input_file_path= "inputs/processed/hse_2019.csv",
                                                  ob_total_cost = MODEL_CONSTANTS$COST_OF_OBESITY_IN_BILLIONS,
                                                  ob_costs_30_40 = MODEL_CONSTANTS$COST_OBESITY_BMI_30_40,
                                                  ob_costs_over_40 = MODEL_CONSTANTS$COST_OBESITY_OVER_40,
                                                  implementation_period = MODEL_CONSTANTS$MODEL_DURATION,
                                                  cohort_alloc_input = COHORT_ALLOCATION_OP2)
  
  # 1. Summary results:
  summary_stats <- sensitivity_results %>%
    summarise(
      mean_relative_reduction = round(mean(relative_reduction), 3),
      sd_relative_reduction = round(sd(relative_reduction), 3),
      coeff_variation = round(sd(relative_reduction)/mean(relative_reduction), 3),
      min_relative_reduction = round(min(relative_reduction), 3),
      max_relative_reduction = round(max(relative_reduction), 3),
      mean_benefit = mean(benefit),
      sd_benefit = sd(benefit),
      mean_class_3_rel_reduction = round(mean(relative_reduction_class_3), 3),
      sd_class_3_rel_reduction = round(sd(relative_reduction_class_3), 3),
      min_class_3_rel_reduction = round(min(relative_reduction_class_3), 3),
      max_class_3_rel_reduction = round(max(relative_reduction_class_3), 3),
    )
  
  print(paste("Summary stats of mean obesity reduction: ", summary_stats$mean_relative_reduction) )
  print(summary_stats)
  
  summary_report <- list(
    summary_statistics = summary_stats,
    confidence_intervals = data.frame(
      metric = c("Relative Reduction"),
      CI_95_lower = c(quantile(sensitivity_results$relative_reduction, 0.025)),
      CI_95_upper = c(quantile(sensitivity_results$relative_reduction, 0.975))
    ))
  
  # 1. Summary results:
  write_xlsx(summary_report, 
             here( paste(path, cohort_name, "_summary.xlsx" )))
  
  # 2. Detailed results:
  write_csv(sensitivity_results, 
            here(paste(path, cohort_name, "_detailed.csv")))
  
}



# Outputs:



# 2. Density plot of relative reduction:
relative_reduction_plot <- 
  plot_metric(data = sensitivity_results, metric = "relative_reduction")

# plot_metric(data = sensitivity_results, metric = "relative_reduction_class_3")

# 3. Density plot of benefit:
plot_metric(data = sensitivity_results, metric = "benefit")


# Saving outputs:


# 3. Distribution plot of relative reduction:
ggsave(here("outputs/new_policies/policy_38/sensitivity_results/relative_reduction_distrib.png"), 
       plot = relative_reduction_plot,
       width = 180, height = 120, units = "mm",
       bg = "white")


# Using the kmeans() function to find clusters in the sensitivity analysis:

set.seed(42) 
kmeans_result <- kmeans(sensitivity_results$relative_reduction, centers = 2)

# 2. Add the cluster assignments back to your main dataframe
sensitivity_results_with_clusters <- sensitivity_results %>%
  mutate(peak_group = as.factor(kmeans_result$cluster))

comparison_by_cluster <- sensitivity_results_with_clusters %>%
  group_by(peak_group) %>%
  summarise(
    
    diff_diabetes_y1 = mean(diabetes_year1, na.rm = TRUE),
    diff_diabetes_y2 = mean(diabetes_year2, na.rm = TRUE),
    diff_diabetes_y3 = mean(diabetes_year3, na.rm = TRUE),
    diff_diabetes_y4 = mean(diabetes_year4, na.rm = TRUE),
    diff_diabetes_y5 = mean(diabetes_year5, na.rm = TRUE),
    
    diff_bmi_y1 = mean(bmi_year1, na.rm = TRUE),
    diff_bmi_y2 = mean(bmi_year2, na.rm = TRUE),
    diff_bmi_y3 = mean(bmi_year3, na.rm = TRUE),
    diff_bmi_y4 = mean(bmi_year4, na.rm = TRUE),
    diff_bmi_y5 = mean(bmi_year5, na.rm = TRUE)
  )

print(comparison_by_cluster)
