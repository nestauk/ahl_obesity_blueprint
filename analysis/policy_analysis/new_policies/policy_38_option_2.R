
################################################################################################### 
# Policy 38: NHS roll out of Tirzepatide                                                          #
###################################################################################################


# Scope: England

# Eligibility: Adults Age: ≥ 18; BMI Group: ≥ 35 + qualifying comorbidities split into cohorts (please see - Tirzepatide
# Modelling [https://docs.google.com/document/d/1AjMZWK9XV1lOqAGUcSZPaBYAKXHtOPfUz4zJRICpqFc/edit?usp=sharing])
# Note: BMI - 2.5 for Black, Asian and Other Ethnicity
# 
# Weight loss: 18.5% weight loss for those without T2D and 13.8% weight loss for those with T2D
# 
# Weight regain: No weight regain as Tirzepatide can be prescribed indefinitely [Source]
# 
# Number of people treated: We used the cohort allocation provided by NICE for roll out to estimate the number of people
# being treated in a given year. Please see [https://docs.google.com/document/d/1AjMZWK9XV1lOqAGUcSZPaBYAKXHtOPfUz4zJRICpqFc/edit?usp=sharing]

# Population estimate for adults equal and over 18 years of age = 44,263,393 (a)


# References:
# (a) ONS 2019 Mid-Year Population Estimates - https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/analysisofpopulationestimatestool)
# (b) Wilding JPH, Batterham RL, Davies M, Van Gaal LF, Kandler K, Konakli K, Lingvay I, McGowan BM, Oral TK, Rosenstock J, 
#     Wadden TA, Wharton S, Yokote K, Kushner RF; STEP 1 Study Group. Weight regain and cardiometabolic effects after withdrawal of 
#     semaglutide: The STEP 1 trial extension. Diabetes Obes Metab. 2022 Aug;24(8):1553-1564. doi: 10.1111/dom.14725. Epub 2022 May 19. 
#     PMID: 35441470; PMCID: PMC9542252.
# (c) NHS (2023). Treatment - Obesity. [online] NHS. Available at: https://www.nhs.uk/conditions/obesity/treatment/. [Note this matches with NICE Guidelines for each drug]




# setup
rm(list = ls())
library(tidyverse)
library(here)
library(writexl)


# source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")
source(file = "config/config.R")
source(file = "post_processing/post_processing.R")


# Constants:
ENGLAND_ADULT_POPULATION = 44263393  # (a)
WEIGHT_LOSS_WITH_T2D = 0.138
WEIGHT_LOSS_WITHOUT_T2D = 0.185
WEIGHT_REGAIN_POST_TREATMENT = 0
# COHORT_ALLOCATION <- list(year1 = list(c1 = 28000),
#                           year2 = list(c1 = 14000, c2 = 47500),
#                           year3 = list(c2 = 47500, c3 = 85714),
#                           year4 = list(c3 = 114285, c4 = 58462),
#                           year5 = list(c4 = 175384))


# # current option 2
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






table_outputs = list() # creating a list of table outputs to be saved as an excel file


# functions:

# function for choosing the intervention sample:
# The logic for this function is explained here: https://docs.google.com/document/d/1b8eo_wgedOrJ-D5AWqIjCxmCr_yu-ez3EA6uZkwMvmQ/edit?usp=sharing


# FUNCTION 1:

#' Select Intervention Sample with Cohort-Based Priority
#'
#' This function performs intervention sampling using a cohort-based priority system.
#' We select individuals for intervention based on eligibility cohorts (1 = highest priority)
#' and uses weighted random sampling. When target cohorts are insufficient, the function
#' automatically expands to include adjacent cohorts.

#' @param data A data frame containing individual-level survey data with
#'   eligibility scores, survey weights, and other relevant variables
#' @param cohort_allocations A nested list structure defining intervention
#'   allocations by year and cohort.
#' @param population_size Integer specifying the total population size that
#'   the survey data represents (used for scaling calculations)
#' @param weight_var Character string specifying the name of the survey weight
#'   variable in the data frame (e.g., "wt_int")
#' @param num_years Integer specifying the number of years to run the
#'   intervention selection process
#' @param cohort_var Character string specifying the name of the eligibility
#'   cohort variable in the data frame (e.g., "eligibility_score")
#'
#' @return A list containing two elements:
#'  1. The original data frame with added intervention columns with Yes/No
#'  2. A data frame with detailed logging information including:
#'       Year, Cohort, EligiblePopulation, PopulationRequired, PopulationSelected,
#'       DesiredWeightSum, ActualWeightSum, NumEligibleIndividuals, NumSelected, and comments

select_intervention_sample <- function(data, cohort_allocations, population_size, 
                                       weight_var, num_years, cohort_var) {
  
  op_list = list()
  running_log <- data.frame(
    Year = integer(),
    Cohort = integer(),
    EligiblePopulation = numeric(),
    PopulationRequired = numeric(),
    PopulationSelected = numeric(),
    DesiredWeightSum = numeric(),
    ActualWeightSum = numeric(),
    NumEligibleIndividuals = numeric(),
    NumSelected = integer(),
    stringsAsFactors = FALSE,
    comment = character()
  )
  
  # browser()
  comment_list = list()
  # Add intervention columns for each year into the dataset
  intervention_cols <- paste0("intervention_year", 1:num_years)
  data[, intervention_cols] <- "No"
  
  # Initialize a vector to store the individuals who have already received the intervention
  intervention_history <- rep(FALSE, nrow(data))
  
  # Calculate the total weight of the full dataset (needed for population calculations)
  total_weight <- sum(data[[weight_var]])
  
  for (year in 1:num_years) {
    year_name <- paste0("year", year)
    
    # Skip this year if no allocations are defined
    if (!year_name %in% names(cohort_allocations)) {
      next
    }
    
    year_allocations <- cohort_allocations[[year_name]]
    
    for (cohort in names(year_allocations)) {
      
      cohort_value = as.numeric(gsub("c", "", cohort))
      sample_size <- year_allocations[[cohort]]
      cohort_used = c(cohort_value)
      
      # Subset the data frame to include only individuals in this cohort who haven't received intervention
      subset_data <- data[data[[cohort_var]] == cohort_value & !intervention_history, ]
      # browser()
      
      # Calculate the proportion of eligible people in the population
      eligible_proportion <- sum(subset_data[[weight_var]]) / total_weight
      
      # Calculate the number of eligible people in the population
      eligible_population <- round(eligible_proportion * population_size)
      
      # If no eligible individuals remain in this cohort or if there aren't enough individuals to select
      # from in the cohort then we record this and include previous or subsequent cohorts for sampling
      if (nrow(subset_data) == 0 | sample_size > eligible_population) {
        
        if(nrow(subset_data) == 0) {
          
          # warning(paste0("No eligible individuals remain in cohort ", cohort, " for year ", year))
          msg = paste0("No eligible individuals remain in cohort ", cohort, " for year ", year, "//")
          comment_list = c(comment_list, msg)
        }
        
        if(sample_size > eligible_population) {
          
          msg = (paste0("The desired sample size (", sample_size, 
                        ") is greater than the eligible population (", 
                        eligible_population, ") for cohort ", cohort, " in year ", year, 
                        ". Using all available individuals and including previous or subsequent cohorts in sample."))
          
          comment_list = c(comment_list, msg)
          
        }
        
        
        cohort_to_consider = 1:(cohort_value)
        subset_data_1 <- data[data[[cohort_var]] %in% cohort_to_consider & !intervention_history, ]
        subset_data_updated = subset_data_1 %>%
          distinct()
        subset_data = subset_data_updated
        
        # Calculate the proportion of eligible people with previous cohorts added in:
        eligible_proportion_updated <- sum(subset_data[[weight_var]]) / total_weight
        
        # Calculate the number of eligible people in the population:
        eligible_population_updated <- round(eligible_proportion_updated * population_size)
        
        # Then we recheck if with addition of previous cohorts enough individuals are in the sample.
        # If not, individuals from the next cohort are added into the sample.
        if(sample_size > eligible_population_updated) {
          
          cohort_to_consider = 1:(cohort_value + 1)
          subset_data_2 <- data[data[[cohort_var]] %in% cohort_to_consider & !intervention_history, ]          
          
          subset_data_updated_1 = subset_data_2 %>%
            distinct()
          
          subset_data = subset_data_updated_1
          
        }
        
        # getting the cohort names from the subset data
        cohort_used = subset_data %>%
          select(cohort_var) %>%
          distinct() %>%
          pull(cohort_var)
        
        # next
      }
      
      # Calculate the proportion of eligible people in the population
      eligible_proportion = sum(subset_data[[weight_var]]) / total_weight
      
      # Calculate the number of eligible people in the population
      eligible_population = round(eligible_proportion * population_size)
      
      
      # Calculate the desired weight sum for this cohort allocation
      desired_weight_sum = sample_size / eligible_population * sum(subset_data[[weight_var]])
      
      # Initialize a vector to store the selected individuals for the current cohort
      selected_individuals = rep(FALSE, nrow(subset_data))
      
      intervention_history_table = intervention_history %>%
        as.data.frame()
      
      # Initialize a variable to keep track of the current weight sum
      current_weight_sum = 0
      
      # browser()
      
      # Perform weighted random sampling until we reach desired weight sum
      while (current_weight_sum < desired_weight_sum) {
        # We first filter to identify individuals who haven't receive treatment.
        remaining_indices <- which(!selected_individuals)
        
        # Then we check if there are enough individuals in the sample to choose from
        if (length(remaining_indices) == 0) {
          msg = (paste0("Ran out of eligible individuals in cohort ", cohort, 
                        " for year ", year, " before reaching target."))
          # break
          comment_list = c(comment_list, msg)
        }
        
        
        # sampling is done based on eligibility and the survey weight. Individuals from earlier cohort
        # are given priority in each year. Then among individuals of same level of eligibility, we
        # choose individuals with the highest survey weight to receive treatment
        remaining_eligibility = subset_data[[cohort_var]][remaining_indices]
        highest_priority = min(remaining_eligibility)
        
        # Get indices of individuals with highest available priority
        priority_indices <- remaining_indices[remaining_eligibility == highest_priority]
        
        if (length(priority_indices) == 1) {
          
          selected_index = priority_indices
          
        } else {
          
          selected_index = sample(priority_indices, size = 1, 
                                  prob = subset_data[[weight_var]][priority_indices])
        }
        
        # Indicating an individual as chosen for treatment from the sample
        selected_individuals[selected_index] = TRUE
        
        # Update the current weight sum to decide if the loop should terminate
        current_weight_sum = current_weight_sum + subset_data[[weight_var]][selected_index]
        
      }
      
      # Get the row indices of the selected individuals in the original data frame
      original_indices = which(data[[cohort_var]] %in% cohort_used & !intervention_history)
      selected_indices_original = original_indices[selected_individuals]
      
      # Update the intervention column for the current year
      data[selected_indices_original, intervention_cols[year]] = "Yes"
      
      # Update the intervention history
      intervention_history[selected_indices_original] = TRUE
      
      
      if (exists("msg") && length(msg) > 0 && tail(comment_list, 1) == msg) {
        comment_text <- msg
      } else {
        comment_text <- ""
      }
      
      # Debug information - you can remove or keep these
      running_log = log_running_info(log_df = running_log, year = year, cohort = paste(cohort_used, collapse = ","),
                                     eligible_population = eligible_population, subset_data = subset_data,
                                     sample_size = sample_size,
                                     desired_weight_sum =  desired_weight_sum,
                                     current_weight_sum = current_weight_sum,
                                     total_weight = total_weight,population_size = population_size,
                                     selected_individuals = sum(selected_individuals), comments = comment_text)
      
      print(tail(running_log,1))
      
    }
  }
  
  
  print(running_log)
  
  op_list[["running_log"]] = running_log
  op_list[["data"]] = data
  return(op_list)
}


# FUNCTION 2: Assign weight loss

#' This function assigns weight loss and weight regain values to individuals
#' based on their intervention status, diabetes condition, and timing of
#' intervention across multiple years. It models differential weight loss
#' effects for individuals with and without diabetes, with specific patterns
#' for weight loss duration and potential regain.
#' 

#' @param data A data frame containing individual-level data with intervention
#'   status columns (intervention_year1, intervention_year2, etc.) and diabetes
#'   condition indicators
#' @param bodyweight_var Character string specifying the name of the baseline
#'   body weight variable in the data frame (e.g., "baseline_weight_kg")
#' @param num_years Integer specifying the number of years in the simulation
#' @param weight_loss_percent Numeric value (0-1) representing the percentage
#'   of baseline body weight lost for individuals without diabetes
#' @param weight_loss_percent_with_diabetes Numeric value (0-1) representing
#'   the percentage of baseline body weight lost for individuals with diabetes
#' @param weight_regain Numeric value (0-1) representing the fraction of
#'   weight loss that is regained (currently set to 0 for indefinite treatment)

#' @return A data frame with the original data plus additional columns for weight
#'  loss and weight regain in each year

assign_weight_changes <- function(data, bodyweight_var, num_years, weight_loss_percent, weight_loss_percent_with_diabetes, weight_regain) {
  # browser()
  # Create weight loss and weight regain columns for each year
  weight_loss_cols <- paste0("weight_loss_y", 1:num_years)
  weight_regain_cols <- paste0("weight_regain_y", 1:num_years)
  data[, c(weight_loss_cols, weight_regain_cols)] <- 0
  
  for (year in 1:num_years) {
    intervention_col <- paste0("intervention_year", year)
    
    if(year < 5){
      
      # Assign weight loss for individuals who received the intervention in the current year
      # evidence shows weight loss values for two years. In this case, it is being assumed that the weight loss occurs in the first year
      data[data[[intervention_col]] == "Yes" & data[["cond_diabetes"]] == 1, weight_loss_cols[year]] = -weight_loss_percent_with_diabetes*1* data[data[[intervention_col]] == "Yes"  & data[["cond_diabetes"]] == 1, bodyweight_var]
      data[data[[intervention_col]] == "Yes" & data[["cond_diabetes"]] == 0, weight_loss_cols[year]] = -weight_loss_percent*1* data[data[[intervention_col]] == "Yes"  & data[["cond_diabetes"]] == 0, bodyweight_var]
      
      data[data[[intervention_col]] == "Yes" & data[["cond_diabetes"]] == 1, weight_loss_cols[year+1]] = -weight_loss_percent_with_diabetes*0* data[data[[intervention_col]] == "Yes"  & data[["cond_diabetes"]] == 1, bodyweight_var]
      data[data[[intervention_col]] == "Yes" & data[["cond_diabetes"]] == 0, weight_loss_cols[year+1]] = -weight_loss_percent*0* data[data[[intervention_col]] == "Yes"  & data[["cond_diabetes"]] == 0, bodyweight_var]
      
      #, weight_loss_cols[year+1] 
      
    } else{
      
      data[data[[intervention_col]] == "Yes" & data[["cond_diabetes"]] == 1, weight_loss_cols[year]] = -weight_loss_percent_with_diabetes*1* data[data[[intervention_col]] == "Yes" & data[["cond_diabetes"]] == 1, bodyweight_var]
      data[data[[intervention_col]] == "Yes" & data[["cond_diabetes"]] == 0, weight_loss_cols[year]] = -weight_loss_percent*1* data[data[[intervention_col]] == "Yes"  & data[["cond_diabetes"]] == 0, bodyweight_var]
      
      
    }
    # browser()
    # This part of the function is now redundant as we set weight regain = 0 given drugs are being prescribed indefinitely
    # will update and remove this loop if we are advised to include weight regain
    # In other cases (Semaglutide), weight regain to be assigned only from the year after intervention, so it would be two
    # years of weight loss and weight regain in third year
    if (year > 2) {
      prev_intervention_cols = paste0("intervention_year", 1:(year - 2))
      prev_intervention = apply(data[, prev_intervention_cols] == "Yes", 1, any)
      
      # Calculate weight regain based on weight loss in one of the two intervention years and calculated as 67% of two times
      # weight loss in any one of the years two times weight loss in any one year is the total weight loss over two years.
      weight_regain_amount = weight_regain *1* pmin(
        data[prev_intervention, weight_loss_cols[year - 1], drop = TRUE],
        data[prev_intervention, weight_loss_cols[year - 2], drop = TRUE]
      )
      
      data[prev_intervention, weight_regain_cols[year]] = -weight_regain_amount
    }
  } 
  
  return(data)
}



log_running_info <- function(log_df, year, cohort, eligible_population, sample_size, subset_data,
                             desired_weight_sum, current_weight_sum, total_weight,
                             population_size, selected_individuals, comments = "na") {
  new_row <- data.frame(
    Year = year,
    Cohort = cohort,
    EligiblePopulation = eligible_population,
    SampleSize = sample_size,
    DesiredWeightSum = desired_weight_sum,
    ActualWeightSum = current_weight_sum,
    NumEligibleIndividuals = nrow(subset_data),
    NumSelected = sum(selected_individuals),
    PopulationSelected = (current_weight_sum / total_weight) * population_size,
    stringsAsFactors = FALSE,
    comment = comments
  )
  return(rbind(log_df, new_row))
}






# Estimating the impact of the policy for adults in England:

# reading in HSE 2019 data and preparing it for implementing the Hall Model

# Cleaning the input/ baseline data:
# process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab", nation = "England", population_group = "Adult")


# reading in the cleaned processed baseline data file:
df_2019_adult = read_csv(here("inputs/processed/hse_2019.csv"))

# Applying eligibility criteria.

# First we assign '1' if an individual has a condition
# Then we calculate the eligibility score (1 - 4) where 4 = 4 comobidities and 1 = 1 comorbidity

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


# We will use the BMI value of individual and the eligibility score to assign them to cohorts
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


set.seed(593)

# From the NICE Guidelines we estimated the number of people in each year and cohort:
print(COHORT_ALLOCATION)



# Selecting the intervention sample each year who will receive the intervention:
# From those eligible, we select a number of people for each cohort in an year
intervention_sample = select_intervention_sample(data = df_2019_adult_with_cohorts, 
                                                 cohort_allocations = COHORT_ALLOCATION,
                                                 population_size = ENGLAND_ADULT_POPULATION,
                                                 weight_var = "wt_int",
                                                 num_years = MODEL_CONSTANTS$MODEL_DURATION,
                                                 cohort_var = "eligibility")


# Assign weight changes to individuals who were selected in the previous step
# We apply a weight loss of 18.5% for those without T2D and 13.8% for those with T2D
post_df_adult = assign_weight_changes(data = intervention_sample$data,
                                      bodyweight_var = "weight",
                                      num_years = MODEL_CONSTANTS$MODEL_DURATION,
                                      weight_loss_percent = WEIGHT_LOSS_WITHOUT_T2D,
                                      weight_loss_percent_with_diabetes = WEIGHT_LOSS_WITH_T2D,
                                      weight_regain = WEIGHT_REGAIN_POST_TREATMENT)


# Calculating the new body weight, BMI and BMI Class for each individual:
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

bmi_change_year

table_outputs[["england_adult"]] = bmi_change_year



# Outputs:

# Output 1: Table of year wise prevalence of obesity
bmi_change_year

# extracting the reduction in obesity prevalence 
annual_obesity_prevalence_england = extract_relative_change(data = bmi_change_year)

# Relative reduction in obesity prevalence in England = 0.2%

# Adding to table outputs:
table_outputs[["annual_obesity_prevalence_eng"]] = annual_obesity_prevalence_england

# Estimating the annual value to government (benefit):
annual_benefit_to_gov = extract_pound_benefit_by_class(data = bmi_change_year,
                                                       total_cost = MODEL_CONSTANTS$COST_OF_OBESITY_IN_BILLIONS,
                                                       cost_30_40 = obesity_cost_30_40,
                                                       cost_over_40 = obesity_cost_over_40,
                                                       duration = MODEL_CONSTANTS$MODEL_DURATION,
                                                       option = "option_1")

# Average annual value to government compared to baseline = £0.05 billions

# Adding to table outputs:
# table_outputs[["annual_benefit_to_gov"]] = annual_benefit_to_gov


bmi_change = bmi_change %>%
  mutate(BMI_1 = case_when(BMI == "morbidly obese" ~ "Obesity class 3",
                           BMI == "obese" ~ "Obesity class 1 & 2",
                           BMI == "normal" ~ "Healthy weight",
                           BMI == "underweight" ~ "Underweight",
                           BMI == "overweight" ~ "Overweight",
                           TRUE ~ BMI ),
         type_1 = case_when(type == "Year 0" ~ "Baseline",
                            TRUE ~ type))

bmi_change = bmi_change %>%
  mutate(BMI_1 = factor(BMI_1,
                        levels = c("Underweight",
                                   "Healthy weight",
                                   "Overweight",
                                   "Obesity class 1 & 2",
                                   "Obesity class 3")))



# Output 2: Plot of BMI distribution(bar charts)
# Plot of year on year BMI category distribution
adult_bar_plot = bmi_change %>%
  ggplot(., aes(y = freq, x = BMI_1, fill = type_1)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_ipsum() +
  labs(fill = "", 
       title = "NHS Rollout of Tirzepatide (Mounjaro)", 
       y = "Prevalence - %",
       x = "BMI group",
       subtitle = "Per year distribution by BMI Category") +
  theme_ipsum(base_size = 7, axis_title_size = 6, axis_text_size = 7) + #, base_family="Averta"
  theme(legend.position = "top",
        legend.text = element_text(size = 9),
        axis.title.y = element_text(size = 10, hjust = 0.5),  # y-axis title
        axis.title.x = element_text(size = 10, hjust = 0.5, vjust = 0.9),  # x-axis title
        axis.text.x = element_text(size = 7),    # x-axis tick labels
        axis.text.y = element_text(size = 7)  )   # y-axis tick labels)

adult_bar_plot

ggsave(here("outputs/new_policies/policy_38_op2/policy_38_impact_England_adult_593.png"), 
       plot = adult_bar_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')


# Outputs 3: summary results and detailed individual table:
# bmi year on year prevalence:
write_xlsx(path = "outputs/new_policies/policy_38_op2/policy_38_593.xlsx", x = table_outputs)
write.csv(post_df_adult, file = "outputs/new_policies/policy_38_op2/policy_38_adult_england_bmi_593.csv")


log_df = intervention_sample$running_log

write.table(log_df, pipe("pbcopy"), sep="\t", row.names = FALSE)



cohort_year_allocation = post_df_adult %>%
  mutate(year_allocation = case_when(intervention_year1 == "Yes" ~ "Y1",
                                     intervention_year2 == "Yes" ~ "Y2",
                                     intervention_year3 == "Yes" ~ "Y3",
                                     intervention_year4 == "Yes" ~ "Y4",
                                     intervention_year5 == "Yes" ~ "Y5",
                                     TRUE ~ "NA")) %>%
  group_by(eligibility, year_allocation) %>%
  summarise(share = sum(wt_int)) %>%
  ungroup() %>%
  mutate(pct_share = (share / sum(share))) %>%
  mutate(number_of_people = pct_share * ENGLAND_ADULT_POPULATION) %>%
  subset(year_allocation != "NA")

cohort_year_allocation = post_df_adult %>%
  mutate(year_allocation = case_when(intervention_year1 == "Yes" ~ "Y1",
                                     intervention_year2 == "Yes" ~ "Y2",
                                     intervention_year3 == "Yes" ~ "Y3",
                                     intervention_year4 == "Yes" ~ "Y4",
                                     intervention_year5 == "Yes" ~ "Y5",
                                     TRUE ~ "NA")) %>%
  rowwise() %>%
  mutate(bmi_class_change = n_distinct(c_across(matches("(?i)bmi_\\d+_class")), na.rm = TRUE) > 1) %>%
  ungroup()

cohort_allocation_subset = cohort_year_allocation %>%
  filter(year_allocation != "NA", !bmi_class_change)

440583-(21000+42000+91000+126000+126000)

bmi_over_40 = post_df_adult %>%
  filter(bmi >= 40)


plot_metric(data = bmi_over_40, metric = "bmi")


plot_metric(data = post_df_adult, metric = "bmi_y1")
plot_metric(data = post_df_adult, metric = "bmi_y2")
plot_metric(data = post_df_adult, metric = "bmi_y3")
plot_metric(data = post_df_adult, metric = "bmi_y4")
plot_metric(data = post_df_adult, metric = "bmi_y5")

df_bmi = post_df_adult %>%
  dplyr::select(bmi, bmi_y1, bmi_y2, bmi_y3, bmi_y4, bmi_y5)


df_bmi_over_40 = df_bmi %>%
  filter(bmi >= 40)

df_long <- pivot_longer(df_bmi_over_40,
                        cols = everything(),
                        names_to = "year",
                        values_to = "bmi")

ggplot(df_long, aes(x = bmi, color = year)) +
  
  # Add the density layer.
  # 'alpha' is set to 0.4 to make the fills semi-transparent,
  # so you can see the overlapping distributions.
  geom_density(alpha = 0.4, trim = TRUE) +
  
  # Add titles and labels for clarity
  labs(
    title = "Op 2: Dens Plot of BMI Over 5 Years",
    x = "BMI",
    y = "Density",
    fill = "Year", # Legend title for fill
    color = "Year" # Legend title for color
  ) +
  
  coord_cartesian(xlim = c(30, NA)) + 
  # Apply a clean theme
  theme_minimal() +
  
  # Add some extra styling for a polished look
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "bottom"
  )

