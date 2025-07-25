


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
      # running_log = log_running_info(log_df = running_log, year = year, cohort = paste(cohort_used, collapse = ","),
      #                                eligible_population = eligible_population, subset_data = subset_data,
      #                                sample_size = sample_size,
      #                                desired_weight_sum =  desired_weight_sum,
      #                                current_weight_sum = current_weight_sum,
      #                                total_weight = total_weight,population_size = population_size,
      #                                selected_individuals = sum(selected_individuals), comments = comment_text)
      
      # print(tail(running_log,1))
      
    }
  }
  
  
  # print(running_log)
  
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
      
      data[data[[intervention_col]] == "Yes" & data[["cond_diabetes"]] == 1, weight_loss_cols[year]] = -weight_loss_percent*1* data[data[[intervention_col]] == "Yes" & data[["cond_diabetes"]] == 1, bodyweight_var]
      
      
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

