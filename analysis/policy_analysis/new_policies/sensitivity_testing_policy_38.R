# Sensitivity Analysis for Random Sampling
# This script runs 1000 iterations of the main analysis with different random seeds

# Load required libraries
library(tidyverse)
library(here)
library(writexl)
library(parallel)

# Source files
source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")
source(file = "config/config.R")
source(file = "post_processing/post_processing.R")
source(file = "analysis/policy_analysis/new_policies/utils.R")

# Constants
NUMBER_OF_PEOPLE_PER_YEAR = 1200000
ENGLAND_ADULT_POPULATION = 44263393
WEIGHT_LOSS_WITH_T2D = 0.185
WEIGHT_LOSS_WITHOUT_T2D = 0.138
WEIGHT_REGAIN_POST_TREATMENT = 0
COHORT_ALLOCATION <- list(year1 = list(c1 = 28000),
                          year2 = list(c1 = 14000, c2 = 47500),
                          year3 = list(c2 = 47500, c3 = 85714),
                          year4 = list(c3 = 114285, c4 = 58462),
                          year5 = list(c4 = 175384))


# Function to run a single iteration of the analysis
run_single_iteration <- function(seed_value, df_2019_adult_with_cohorts, obesity_cost_30_40, obesity_cost_over_40, implementation_period) {
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
  
  
  # A new dataframe is created to capture population level prevalence of different BMI categories in each year and is saved as a dataframe
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
    mutate(BMI = factor(BMI, levels = c("underweight", "normal", "overweight", "obese", "morbidly obese"))) %>%
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
  
  # Calculate reduction
  reduction <- baseline_prevalence$baseline_obesity_prevalence - bmi_prevalence$obesity_prevalence_y5
  
  value_to_gov = extract_pound_benefit_by_class(data = bmi_change_year,
                                                cost_30_40 = obesity_cost_30_40,
                                                cost_over_40 = obesity_cost_over_40,
                                                duration = implementation_period )
  
  # Return results
  return(data.frame(
    seed = seed_value,
    baseline_obesity = baseline_prevalence$baseline_obesity_prevalence,
    year5_obesity = bmi_prevalence$obesity_prevalence_y5,
    reduction = reduction,
    relative_reduction = (reduction / baseline_prevalence$baseline_obesity_prevalence) * 100,
    benefit = value_to_gov
  ))
}

# Main sensitivity analysis
run_sensitivity_analysis <- function(n_iterations = 1000, ob_costs_30_40, ob_costs_over_40, implementation_period) {

  
  df_2019_adult <- read_csv(here("inputs/processed/hse_2019.csv"))
  
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
  
  results <- pbapply::pblapply(seeds, function(s) {
    tryCatch({
      run_single_iteration(s,
                           df_2019_adult_with_cohorts,
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

# Running the sensitivity analysis:
sensitivity_results <- run_sensitivity_analysis(n_iterations = 1000,
                                                ob_costs_30_40 = MODEL_CONSTANTS$COST_OBESITY_BMI_30_40,
                                                ob_costs_over_40 = MODEL_CONSTANTS$COST_OBESITY_OVER_40,
                                                implementation_period = MODEL_CONSTANTS$MODEL_DURATION)

# Creating summary:
summary_stats <- sensitivity_results %>%
  summarise(
    mean_relative_reduction = round(mean(relative_reduction), 3),
    sd_relative_reduction = round(sd(relative_reduction), 3),
    min_relative_reduction = round(min(relative_reduction), 3),
    max_relative_reduction = round(max(relative_reduction), 3),
    mean_benefit = mean(benefit),
    sd_benefit = sd(benefit)
  )

print("Summary stats of obesity reduction and benefits:")
print(summary_stats)

# Create summary report
summary_report <- list(
  summary_statistics = summary_stats,
  confidence_intervals = data.frame(
    metric = c("Relative Reduction"),
    CI_95_lower = c(quantile(sensitivity_results$relative_reduction, 0.025)),
    CI_95_upper = c(quantile(sensitivity_results$relative_reduction, 0.975))
  ))

write_xlsx(summary_report, 
           here("outputs/new_policies/policy_38/sensitivity_analysis_summary.xlsx"))

# Saving detailed results
write_csv(sensitivity_results, 
          here("outputs/new_policies/policy_38/sensitivity_analysis_results.csv"))
