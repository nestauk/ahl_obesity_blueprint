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

# constants:
ENGLAND_ADULT_POPULATION = 44263393
WEIGHT_LOSS_WITH_T2D = 0.138
WEIGHT_LOSS_WITHOUT_T2D = 0.185
WEIGHT_REGAIN_POST_TREATMENT = 0
# COHORT_ALLOCATION <- list(year1 = list(c1 = 28000),
#                           year2 = list(c1 = 14000, c2 = 47500),
#                           year3 = list(c2 = 47500, c3 = 85714),
#                           year4 = list(c3 = 114285, c4 = 58462),
#                           year5 = list(c4 = 175384))

# current option 2
# COHORT_ALLOCATION <- list(year1 = list(c1 = 21000),
#                           year2 = list(c1 = 21000, c2 = 21000),
#                           year3 = list(c2 = 45500, c3 = 45500),
#                           year4 = list(c3 = 63000, c4 = 63000),
#                           year5 = list(c4 = 126000))

# new option 2
COHORT_ALLOCATION <- list(year1 = list(c1 = 21000),
                          year2 = list(c1 = 8400, c2 = 33600),
                          year3 = list(c2 = 32900, c3 = 58100),
                          year4 = list(c3 = 81900, c4 = 44100),
                          year5 = list(c4 = 126000))


# COHORT_ALLOCATION <- list(year1 = list(c1 = 19600),
#                           year2 = list(c1 = 9800, c2 = 33250),
#                           year3 = list(c2 = 33250, c3 = 60000),
#                           year4 = list(c3 = 80000, c4 = 40923),
#                           year5 = list(c4 = 122769))



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
                                 df_2019_adult_with_cohorts,
                                 total_cost_obesity,
                                 obesity_cost_30_40,
                                 obesity_cost_over_40,
                                 implementation_period = 5) {
  # browser()
  # Setting seed:
  set.seed(seed_value)
  
  # Selecting intervention sample:
  intervention_sample <- select_intervention_sample(
    data = df_2019_adult_with_cohorts, 
    cohort_allocations = COHORT_ALLOCATION,
    population_size = ENGLAND_ADULT_POPULATION,
    weight_var = "wt_int",
    num_years = MODEL_CONSTANTS$MODEL_DURATION,
    cohort_var = "eligibility"
  )
  
  # selecting the required output table from previous step
  selected_data <- intervention_sample$data
  
  # getting overall preavlence of diabetes
  prop_diabetes <- mean(intervention_sample$data$cond_diabetes)
  
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
      weighted_n = sum(wt_int),
      diabetes_rate = weighted.mean(cond_diabetes, wt_int),
      mean_bmi = weighted.mean(bmi, wt_int)
    )
  
  # Assigning weight changes:
  post_df_adult <- assign_weight_changes(
    data = intervention_sample$data,
    bodyweight_var = "weight",
    num_years = MODEL_CONSTANTS$MODEL_DURATION,
    weight_loss_percent = WEIGHT_LOSS_WITHOUT_T2D,
    weight_loss_percent_with_diabetes = WEIGHT_LOSS_WITH_T2D,
    weight_regain = WEIGHT_REGAIN_POST_TREATMENT
  )
  
  # Calculating new body weight, BMI, and BMI Class:
  post_df_adult = post_df_adult %>%
    mutate(bw_y1 = weight + weight_loss_y1 + weight_regain_y1,
           bw_y2 = bw_y1 + weight_loss_y2 + weight_regain_y2,
           bw_y3 = bw_y2 + weight_loss_y3 + weight_regain_y3,
           bw_y4 = bw_y3 + weight_loss_y4 + weight_regain_y4,
           bw_y5 = bw_y4 + weight_loss_y5 + weight_regain_y5) %>%
    mutate(bmi_y1 = bw_y1/ (height/100)^2,
           bmi_y2 = bw_y2/ (height/100)^2,
           bmi_y3 = bw_y3/ (height/100)^2,
           bmi_y4 = bw_y4/ (height/100)^2,
           bmi_y5 = bw_y5/ (height/100)^2) %>%
    mutate(bmi_1_class = case_when(bmi_y1 <= 18.5 ~ "underweight",
                                   bmi_y1 > 18.5 & bmi_y1 < 25 ~ "normal",
                                   bmi_y1 >= 25 & bmi_y1 < 30 ~ "overweight",
                                   bmi_y1 >= 30 & bmi_y1 < 40 ~ "obese",
                                   bmi_y1 >= 40 ~ "morbidly obese",
                                   TRUE ~ "NA"),
           bmi_2_class = case_when(bmi_y2 <= 18.5 ~ "underweight",
                                   bmi_y2 > 18.5 & bmi_y2 < 25 ~ "normal",
                                   bmi_y2 >= 25 & bmi_y2 < 30 ~ "overweight",
                                   bmi_y2 >= 30 & bmi_y2 < 40 ~ "obese",
                                   bmi_y2 >= 40 ~ "morbidly obese",
                                   TRUE ~ "NA"),
           bmi_3_class = case_when(bmi_y3 <= 18.5 ~ "underweight",
                                   bmi_y3 > 18.5 & bmi_y3 < 25 ~ "normal",
                                   bmi_y3 >= 25 & bmi_y3 < 30 ~ "overweight",
                                   bmi_y3 >= 30 & bmi_y3 < 40 ~ "obese",
                                   bmi_y3 >= 40 ~ "morbidly obese",
                                   TRUE ~ "NA"),
           bmi_4_class = case_when(bmi_y4 <= 18.5 ~ "underweight",
                                   bmi_y4 > 18.5 & bmi_y4 < 25 ~ "normal",
                                   bmi_y4 >= 25 & bmi_y4 < 30 ~ "overweight",
                                   bmi_y4 >= 30 & bmi_y4 < 40 ~ "obese",
                                   bmi_y4 >= 40 ~ "morbidly obese",
                                   TRUE ~ "NA"),
           bmi_5_class = case_when(bmi_y5 <= 18.5 ~ "underweight",
                                   bmi_y5 > 18.5 & bmi_y5 < 25 ~ "normal",
                                   bmi_y5 >= 25 & bmi_y5 < 30 ~ "overweight",
                                   bmi_y5 >= 30 & bmi_y5 < 40 ~ "obese",
                                   bmi_y5 >= 40 ~ "morbidly obese",
                                   TRUE ~ "NA"))
  
  
  # A new dataframe is created to capture population level prevalence of 
  # different BMI categories in each year and is saved as a dataframe
  bmi_change = rbind(
    post_df_adult %>% 
      count(bmi_5_class, wt = wt_int) %>% 
      mutate(freq = n/sum(n)*100,
             type = "Year 5") %>% 
      rename(BMI = bmi_5_class),
    post_df_adult %>% 
      count(bmi_4_class, wt = wt_int) %>% 
      mutate(freq = n/sum(n)*100,
             type = "Year 4") %>% 
      rename(BMI = bmi_4_class),
    post_df_adult %>% 
      count(bmi_3_class, wt = wt_int) %>% 
      mutate(freq = n/sum(n)*100,
             type = "Year 3") %>% 
      rename(BMI = bmi_3_class),
    post_df_adult %>% 
      count(bmi_2_class, wt = wt_int) %>% 
      mutate(freq = n/sum(n)*100,
             type = "Year 2") %>% 
      rename(BMI = bmi_2_class),
    post_df_adult %>% 
      count(bmi_1_class, wt = wt_int) %>% 
      mutate(freq = n/sum(n)*100,
             type = "Year 1") %>% 
      rename(BMI = bmi_1_class),
    post_df_adult %>% 
      count(bmi_class, wt = wt_int) %>% 
      mutate(freq = n/sum(n)*100,
             type = "Year 0") %>% 
      rename(BMI = bmi_class))
  
  
  # creating a table of year wise distribution of BMI categories
  bmi_change = bmi_change %>%
    mutate(BMI = factor(BMI, levels = c("underweight",
                                        "normal",
                                        "overweight",
                                        "obese",
                                        "morbidly obese"))) %>%
    as.data.frame()
  
  
  bmi_change_year = bmi_change %>%
    select(-c(n)) %>%
    pivot_wider(., names_from = BMI, values_from = freq) %>%
    select(type, underweight, normal, overweight, obese, `morbidly obese`)
  
  
  # Calculating obesity prevalence for year 5:
  bmi_prevalence <- post_df_adult %>%
    count(bmi_5_class, wt = wt_int) %>%
    mutate(freq = n/sum(n)*100) %>%
    filter(bmi_5_class %in% c("obese", "morbidly obese")) %>%
    summarise(obesity_prevalence_y5 = sum(freq))
  
  # Calculating obesity prevalence for baseline:
  baseline_prevalence <- post_df_adult %>%
    count(bmi_class, wt = wt_int) %>%
    mutate(freq = n/sum(n)*100) %>%
    filter(bmi_class %in% c("obese", "morbidly obese")) %>%
    summarise(baseline_obesity_prevalence = sum(freq))
  
  # Calculating class 3 prevalence for year 5:
  class_3_bmi_prevalence <- post_df_adult %>%
    count(bmi_5_class, wt = wt_int) %>%
    mutate(freq = n/sum(n)*100) %>%
    filter(bmi_5_class %in% c("morbidly obese")) %>%
    summarise(class_3_obesity_prevalence_y5 = sum(freq))
  
  # Calculating class 3 prevalence for baseline:
  class_3_baseline_prevalence <- post_df_adult %>%
    count(bmi_class, wt = wt_int) %>%
    mutate(freq = n/sum(n)*100) %>%
    filter(bmi_class %in% c("morbidly obese")) %>%
    summarise(baseline_class_3_obesity_prevalence = sum(freq))
  
  # Calculate reduction
  reduction <- (baseline_prevalence$baseline_obesity_prevalence - 
                  bmi_prevalence$obesity_prevalence_y5)
  
  reduction_class_3 <- (class_3_baseline_prevalence$baseline_class_3_obesity_prevalence -
    class_3_bmi_prevalence$class_3_obesity_prevalence_y5)
  
  value_to_gov = extract_pound_benefit_by_class(data = bmi_change_year,
                                                total_cost = total_cost_obesity,
                                                cost_30_40 = obesity_cost_30_40,
                                                cost_over_40 = obesity_cost_over_40,
                                                duration = implementation_period,
                                                option = "option_1")
  
  # Return results
  return(data.frame(
    seed = seed_value,
    baseline_obesity = baseline_prevalence$baseline_obesity_prevalence,
    year5_obesity = bmi_prevalence$obesity_prevalence_y5,
    reduction = reduction,
    relative_reduction = (reduction / baseline_prevalence$baseline_obesity_prevalence) * 100,
    baseline_class_3 = class_3_baseline_prevalence$baseline_class_3_obesity_prevalence,
    year5_class_3_obesity = class_3_bmi_prevalence$class_3_obesity_prevalence_y5,
    reduction_class_3 = reduction_class_3,
    relative_reduction_class_3 = (reduction_class_3/class_3_baseline_prevalence$baseline_class_3_obesity_prevalence) * 100,
    benefit = value_to_gov,
    
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
    n_selected = nrow(intervention_sample$data),
    weighted_n = sum(intervention_sample$data$wt_int)
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
                                     input_file_path= "inputs/processed/hse_2019.csv",
                                     ob_total_cost,
                                     ob_costs_30_40,
                                     ob_costs_over_40,
                                     implementation_period) {

  
  df_2019_adult <- read_csv(here(input_file_path))
  
  # Applying eligibility criteria to create cohorts:
  df_2019_adult_with_cohorts <- df_2019_adult %>%
    mutate(cond_ascvd = case_when(cardiovd == 1 | platlets == 1 | ace_inhibitors == 1 | 
                                    diuretics == 1 | lipid == 1 ~ 1,
                                  TRUE ~ 0),
           cond_hypertension = case_when(hypertension == 1 ~ 1,
                                         TRUE ~ 0),
           cond_dyslipidaemia = case_when(lipid == 1 ~ 1,
                                          TRUE ~ 0),
           cond_diabetes = case_when(diabetes_type == 1 | metformin == 1 | 
                                       anti_diabetics == 1 ~ 1,
                                     TRUE ~ 0)) %>%
    mutate(eligibility_score = cond_ascvd + cond_hypertension + 
             cond_dyslipidaemia + cond_diabetes) %>%
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
                           implementation_period = implementation_period)
    }, error = function(e) {
      cat("\nError in iteration", s, ":", e$message, "\n")
      return(NULL)
    })
  })
  
  # Combining results:
  results_df <- do.call(rbind, results[!sapply(results, is.null)])
  
  return(results_df)
}

# Main analysis:

# df to store results of the sensitivity analysis:
sensitivity_results <- run_sensitivity_analysis(n_iterations = 1000,
                                                input_file_path= "inputs/processed/hse_2019.csv",
                                                ob_total_cost = MODEL_CONSTANTS$COST_OF_OBESITY_IN_BILLIONS,
                                                ob_costs_30_40 = MODEL_CONSTANTS$COST_OBESITY_BMI_30_40,
                                                ob_costs_over_40 = MODEL_CONSTANTS$COST_OBESITY_OVER_40,
                                                implementation_period = MODEL_CONSTANTS$MODEL_DURATION)


# Outputs:

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

print("Summary stats of obesity reduction and benefits:")
print(summary_stats)

summary_report <- list(
  summary_statistics = summary_stats,
  confidence_intervals = data.frame(
    metric = c("Relative Reduction"),
    CI_95_lower = c(quantile(sensitivity_results$relative_reduction, 0.025)),
    CI_95_upper = c(quantile(sensitivity_results$relative_reduction, 0.975))
  ))


# 2. Density plot of relative reduction:
relative_reduction_plot = 
plot_metric(data = sensitivity_results, metric = "relative_reduction")

class_3_relative_reduction_plot =
plot_metric(data = sensitivity_results, metric = "relative_reduction_class_3")

# 3. Density plot of benefit:
plot_metric(data = sensitivity_results, metric = "benefit")


# Saving outputs:

# 1. Summary results:
write_xlsx(summary_report, 
           here("outputs/new_policies/policy_38_op2/sensitivity_analysis/option_2_sens.xlsx"))

# 2. Detailed results:
write_csv(sensitivity_results, 
          here("outputs/new_policies/policy_38_op2/sensitivity_analysis/detailed_option_2_sens.csv"))

# 3. Distribution plot of relative reduction:
ggsave(here("outputs/new_policies/policy_38_op2/sensitivity_analysis/distrib_option_2_sens.png"), 
       plot = relative_reduction_plot,
       width = 180, height = 120, units = "mm",
       bg = "white")

# 4. Distribution plot of relative reduction:
ggsave(here("outputs/new_policies/policy_38_op2/sensitivity_analysis/class_3_distrib_option_2_sens.png"), 
       plot = class_3_relative_reduction_plot,
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


df = read_csv(here("inputs/processed/hse_2019.csv"))

df_filtered_over_40 = df %>%
  filter(bmi>=40 & bmi <=45)

plot_metric(data = df_filtered_over_40, metric = "bmi")



