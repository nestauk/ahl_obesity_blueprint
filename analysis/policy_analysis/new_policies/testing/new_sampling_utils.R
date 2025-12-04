

source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")
source(file = "config/config.R")
source(file = "post_processing/post_processing.R")
library(data.table)


# functions:
get_bmi_category <- function(bmi) {
  case_when(
    bmi < 18.5 ~ "underweight",
    bmi >= 18.5 & bmi < 25 ~ "normal",
    bmi >= 25 & bmi < 30 ~ "overweight",
    bmi >= 30 & bmi < 40 ~ "obese",
    bmi >= 40 ~ "morbidly obese",
    TRUE ~ NA_character_
  )
}

# function to select intervention sample:
select_intervention_sample_dt <- function(data, cohort_allocations,
                                          weight_var, num_years, cohort_var, weight_var_2,
                                          n_selection_per_draw = 1) {
  
  # browser()
  # Convert to data.table and add tracking columns
  setDT(data)
  print(paste("start time = ", Sys.time()))
  
  intervention_cols <- paste0("intervention_year", 1:num_years)
  pop_selected_cols <- paste0("pop_selected_year", 1:num_years)
  rem_weight_cols <- paste0("rem_weight_year", 1:num_years)
  status_col <- "status"
  
  # Add tracking columns 
  data[, (intervention_cols) := "No"]
  data[, (pop_selected_cols) := 0]
  data[, (status_col) := "not_picked"]
  data[, remaining_weight := get(weight_var_2)]
  data[, (rem_weight_cols):= get(weight_var_2)]
  
  comment_list <- list()
  
  # Step 1: Iterating through years and cohorts in a year:
  for (year in 1:num_years) {
    year_name <- paste0("year", year)
    if (!year_name %in% names(cohort_allocations)) next
    
    year_allocations <- cohort_allocations[[year_name]]
    
    for (cohort in names(year_allocations)) {
      cohort_value <- as.numeric(gsub("c", "", cohort))
      sample_size <- year_allocations[[cohort]]
      
      # Step 2: Identifying eligible individuals
      cohorts_to_consider <- cohort_value
      eligible_population <- data[get(cohort_var) %in% cohorts_to_consider, sum(remaining_weight)]
      
      # If population is insufficient, expand the cohort pool, first look in previous cohorts then in later cohorts
      if (is.na(eligible_population) || sample_size > eligible_population) {
        
        cohorts_to_consider <- 1:cohort_value
        eligible_population <- data[get(cohort_var) %in% cohorts_to_consider, sum(remaining_weight)]
        
        if (is.na(eligible_population) || sample_size > eligible_population) {
          
          cohorts_to_consider <- 1:(cohort_value + 1)
          
        }
      }
      
      # Step 3: Sampling:
      desired_weight_sum <- sample_size
      current_weight_sum <- 0
      
      while (current_weight_sum < desired_weight_sum) {
        # Find remaining eligible indices for this iteration
        remaining_indices <- which(
          data[[cohort_var]] %in% cohorts_to_consider &
            data$remaining_weight > 0
        )
        
        # if enough observations not found then save message to be printed out later
        if (length(remaining_indices) == 0) {
          comment_list <- c(comment_list, paste0("Ran out of individuals for year ", year, ", cohort ", cohort))
          break
        }
        
        # Identify the highest priority (lowest cohort number) among those remaining
        highest_priority_cohort <- min(data[remaining_indices, get(cohort_var)])
        priority_indices <- which(
          data[[cohort_var]] == highest_priority_cohort &
            data$remaining_weight > 0
        )
        
        # if there is only one observation, then select it, if more than one exists, then
        # then use probability and population weight
        if (length(priority_indices) == 1) {
          
          # If there's only one choice, don't sample, select them
          selected_index <- priority_indices
          
        } else {
          
          # If there are multiple choices, run sample() with probability = population weight
          selected_index <- sample(priority_indices, size = 1, prob = data[priority_indices, remaining_weight])
          
        }
        
        # Step 4: Calculate the portion of the population weight to choose
        # if the remaining weight is < 100, all of those individuals are assigned to treatment to avoid
        # negative values (weight cannot go < 0)
        # in all other cases, we take a random number of people between 50 and 100 to receive treatment
        weight_available <- data[selected_index, remaining_weight]
        
        if (weight_available < 100) {
          
          n_chosen <- weight_available
          
        } else {
          
          n_chosen <- sample(50:100, 1)
          
        }
        
        # Additional checks on n_chosen to ensure we don't take more than what 
        # is required and stop if a value <=0 is chosen, in this case we resample
        if (current_weight_sum + n_chosen > desired_weight_sum) {
          n_chosen <- desired_weight_sum - current_weight_sum
        }
        if (n_chosen <= 0) break
        
        
        # Subtract the population taken for treatment from the 'remaining_weight' column
        data[selected_index, remaining_weight := remaining_weight - n_chosen]
        
        # Append to the status message
        current_status <- data[selected_index, get(status_col)]
        message_to_add <- paste0(n_chosen, " is chosen")
        new_status <- ifelse(current_status == "not_picked", message_to_add, paste(current_status, message_to_add, sep = ", "))
        data[selected_index, (status_col) := new_status]
        
        # update current weight sum with the n_chosen value
        current_weight_sum <- current_weight_sum + n_chosen
      }
      
      # print status message:
      print(paste0("completed Year =", year, ", cohort = ", cohort, ", selected = ", current_weight_sum))
      
      
    }
    
    # Step 5: Find all observations who were selected in a given year to update intervention status
    rows_updated_this_cohort <- which(data[[status_col]] != "not_picked")
    
    if (length(rows_updated_this_cohort) > 0) {
      
      data[rows_updated_this_cohort, (intervention_cols[year]) := "Yes"]
      data[rows_updated_this_cohort, (pop_selected_cols[year]) := pop_estimate - remaining_weight]
      data[rows_updated_this_cohort, (rem_weight_cols[year]) := remaining_weight ]
      
    }
  }
  
  # Step 6: After all years are processed, create new columns with the actual number of people selected for treatment each year
  actual_cols <- paste0("actual_selected_year", 1:num_years)
  
  # Year 1 is already correct (not cumulative)
  data[, (actual_cols[1]) := get(pop_selected_cols[1])]
  
  # For years 2 to 5, subtract the previous year's cumulative
  for (year in 2:num_years) {
    prev_year_col <- paste0("pop_selected_year", year - 1)
    curr_year_col <- paste0("pop_selected_year", year)
    
    data[, (actual_cols[year]) := get(curr_year_col) - get(prev_year_col)]
  }
  
  print(paste("finish time = ", Sys.time()))
  
  # returning output as dataframe
  df_output = as.data.frame(data)
  
  return(df_output)
}


# Function to assign weight loss:
assign_weight_loss <- function(data, bodyweight_var, num_years, weight_loss_percent, weight_loss_percent_with_diabetes) {
  # browser()
  data = data %>%
    mutate(intervention_status_overall = case_when(intervention_year1 == "Yes" ~ "Yes",
                                                   intervention_year2 == "Yes" ~ "Yes",
                                                   intervention_year3 == "Yes" ~ "Yes",
                                                   intervention_year4 == "Yes" ~ "Yes",
                                                   intervention_year5 == "Yes" ~ "Yes",
                                                   TRUE ~ "No")) %>%
    mutate(weight_loss = case_when(intervention_status_overall == "Yes" & cond_diabetes == 1 ~ -weight_loss_percent_with_diabetes*.data[[bodyweight_var]],
                                   intervention_status_overall == "Yes" & cond_diabetes == 0 ~ -weight_loss_percent*.data[[bodyweight_var]],
                                   TRUE ~ 0))
  
  return(data)
}


estimate_new_weights_bmi <- function(df,
                                     new_weight_col_name = "new_weight",
                                     new_bmi_col_name = "new_bmi",
                                     new_bmi_category = "new_bmi_class",
                                     baseline_weight_col = "weight",
                                     weight_loss_col = "weight_loss",
                                     baseline_height_col = "height") {
  df <- df %>%
    mutate(
      # Compute new weight
      {{ new_weight_col_name }} := .data[[baseline_weight_col]] + .data[[weight_loss_col]],
      # Compute new BMI
      {{new_bmi_col_name}} := round(.data[[new_weight_col_name]] / (.data[[baseline_height_col]] / 100)^2, 7)
    ) %>%
    mutate({{new_bmi_category}} := get_bmi_category(.data[[new_bmi_col_name]]))
  
  return(df)
}


get_yearwise_bmi_category_counts <- function(data, pop_weight_col, treatment_cols, intervention_cols, untreated_cat_col, treated_cat_col) {
  
  W_total <- sum(data[[pop_weight_col]], na.rm = TRUE)
  
  # Ensure there are exactly 5 treatment columns
  if (length(treatment_cols) != 5) {
    stop("Error: 'treatment_cols' must be a character vector with exactly 5 column names.")
  }
  
  # step 1: get cumulative number of people treated each year
  processed_df <- data %>%
    mutate(
      treated_y0 = 0,
      treated_y1 = .data[[treatment_cols[1]]],
      treated_y2 = .data[[treatment_cols[1]]] + .data[[treatment_cols[2]]],
      treated_y3 = .data[[treatment_cols[1]]] + .data[[treatment_cols[2]]] + .data[[treatment_cols[3]]],
      treated_y4 = .data[[treatment_cols[1]]] + .data[[treatment_cols[2]]] + .data[[treatment_cols[3]]] + .data[[treatment_cols[4]]],
      treated_y5 = .data[[treatment_cols[1]]] + .data[[treatment_cols[2]]] + .data[[treatment_cols[3]]] + .data[[treatment_cols[4]]] + .data[[treatment_cols[5]]]
    ) %>%
    # step 2: rename columns
    mutate(
      bmi_cat_untreated = .data[[untreated_cat_col]],
      bmi_cat_treated = .data[[treated_cat_col]]
    ) %>%
    # step 3: select only required columns
    select(id, wt_int, height, weight, pop_estimate, bmi, new_bmi,
           starts_with("intervention"), starts_with("treated_y"),
           bmi_cat_untreated, bmi_cat_treated) %>%
    # step 4: pivot longer so each row indicates the yearly status of observation
    pivot_longer(
      cols = starts_with("treated_y"),
      names_to = "year",
      values_to = "cumulative_treated",
      names_prefix = "treated_y"
    ) %>%
    # step 5: for each year get the number of people treated and untreated
    mutate(pop_untreated = pop_estimate - cumulative_treated,
           pop_treated = cumulative_treated) %>%
    # step 6: pivot longer - for each observation for each year has a treated and untreated row with counts
    pivot_longer(
      cols = c(pop_untreated, pop_treated),
      names_to = "group_type",
      values_to = "group_weight"
    ) %>%
    # step 7: assign bmi category based on treatment status
    mutate(
      bmi_category = if_else(group_type == "pop_untreated", bmi_cat_untreated, bmi_cat_treated)
    ) %>%
    # step 8: get the summary of year and bmi category
    summarise(
      total_weight = sum(group_weight),
      .by = c(year, bmi_category)
    ) %>%
    mutate(prevalence = (total_weight / W_total)*100) %>%
    arrange(year, bmi_category) %>%
    mutate(year = paste0("Year ", year)) %>%
    rename(type = year)
  
  
  return(processed_df)
}



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

select_intervention_sample_method_1 <- function(data, cohort_allocations, population_size, 
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
      # running_log = log_running_info(log_df = running_log, year = year, cohort = paste(cohort_used, collapse = ","),
      #                                eligible_population = eligible_population, subset_data = subset_data,
      #                                sample_size = sample_size,
      #                                desired_weight_sum =  desired_weight_sum,
      #                                current_weight_sum = current_weight_sum,
      #                                total_weight = total_weight,population_size = population_size,
      #                                selected_individuals = sum(selected_individuals), comments = comment_text)
      # 
      # print(tail(running_log,1))
      
    }
  }
  
  
  # print(running_log)
  
  # op_list[["running_log"]] = running_log
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


