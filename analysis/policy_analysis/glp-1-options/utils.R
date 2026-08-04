

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


# 
# # Function to assign weight loss based on cohort:
# assign_weight_loss <- function(data, bodyweight_var, num_years, weight_loss_percent, weight_loss_percent_with_diabetes) {
#   # browser()
#   data = data %>%
#     mutate(intervention_status_overall = case_when(intervention_year1 == "Yes" ~ "Yes",
#                                                    intervention_year2 == "Yes" ~ "Yes",
#                                                    intervention_year3 == "Yes" ~ "Yes",
#                                                    intervention_year4 == "Yes" ~ "Yes",
#                                                    intervention_year5 == "Yes" ~ "Yes",
#                                                    TRUE ~ "No")) %>%
#     mutate(weight_loss = case_when(intervention_status_overall == "Yes" &  == 1 ~ -weight_loss_percent_with_diabetes*.data[[bodyweight_var]],
#                                    intervention_status_overall == "Yes" & cond_diabetes == 0 ~ -weight_loss_percent*.data[[bodyweight_var]],
#                                    TRUE ~ 0))
#   
#   return(data)
# }



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



get_param <- function(x, key) {
  if (!key %in% names(x)) {
    stop(sprintf("Missing config key: '%s'. Available: %s",
                 key, paste(names(x), collapse = ", ")))
  }
  x[[key]]
}