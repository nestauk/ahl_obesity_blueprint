






# setup
rm(list = ls())
library(tidyverse)
library(here)
library(writexl)


source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")

# functions:


select_intervention_sample <- function(data, bmi_threshold, required_proportion, 
                                       weight_var, bmi_var, num_years, 
                                       citeria_1=0, citeria_2=0, citeria_1_value=0, citeria_2_value=0) {
  browser()
  # Add intervention columns for each year to indicate intervention status, that is if an individual receives intervention.
  intervention_cols <- paste0("intervention_year", 1:num_years)
  data[, intervention_cols] <- "No"
  
  # Initialize a vector to store the individuals who have already received the intervention
  intervention_history <- rep(FALSE, nrow(data))
  
  for (year in 1:num_years) {
    # Subset the data frame to include only individuals meeting the criteria
    subset_data <- data[(data[[bmi_var]] >= bmi_threshold) &  !intervention_history, ]
    
    # Calculate the total weight of the full dataset
    total_weight <- sum(data[[weight_var]])
    eligible_weight = sum(subset_data[[weight_var]])
    
    # Check if the required proportion is not greater than 1
    if (required_proportion > 1) {
      stop("The desired sample is greater than the population meeting the criteria.")
    }
    
    
    # Calculate the desired weight sum for the sample
    desired_weight_sum <- required_proportion * sum(subset_data[[weight_var]])
    
    # Initialize a vector to store the selected individuals for the current year
    selected_individuals <- rep(FALSE, nrow(subset_data))
    
    # Initialize a variable to keep track of the current weight sum
    current_weight_sum <- 0
    
    # Perform weighted random sampling until the desired weight sum is reached
    while (current_weight_sum < desired_weight_sum) {
      remaining_indices <- which(!selected_individuals)
      
      if (length(remaining_indices) == 0) {
        break
      }
      
      selected_index <- sample(remaining_indices, size = 1, prob = subset_data[[weight_var]][remaining_indices])
      selected_individuals[selected_index] <- TRUE
      
      current_weight_sum <- current_weight_sum + subset_data[[weight_var]][selected_index]
    }
    
    # Check if the weighted sum of selected individuals is less than the desired weight sum
    selected_indices <- which(selected_individuals)
    if (sum(subset_data[[weight_var]][selected_indices]) < desired_weight_sum) {
      remaining_indices <- which(!selected_individuals)
      additional_index <- sample(remaining_indices, size = 1, prob = subset_data[[weight_var]][remaining_indices])
      selected_individuals[additional_index] <- TRUE
    }
    
    # Get the row indices of the selected individuals in the original data frame
    selected_indices_original <- which((data[[bmi_var]] >= 25) &
                                         !intervention_history)[selected_individuals]
    
    # Update the intervention column for the current year
    data[selected_indices_original, intervention_cols[year]] <- "Yes"
    
    # Update the intervention history
    intervention_history[selected_indices_original] <- TRUE
    
    final_wt = sum(data$wt_int[data$intervention_year1 == "Yes"])
    
  }
  
  return(data)
}




assign_weight_changes <- function(data, bodyweight_var, num_years, weight_loss_percent, weight_regain) {
  #browser()
  # Create weight loss and weight regain columns for each year
  weight_loss_cols <- paste0("weight_loss_y", 1:num_years)
  weight_regain_cols <- paste0("weight_regain_y", 1:num_years)
  data[, c(weight_loss_cols, weight_regain_cols)] <- 0
  
  for (year in 1:num_years) {
    intervention_col <- paste0("intervention_year", year)
    
    if(year < 5){
      
      # Assign weight loss for individuals who received the intervention in the current year
      # evidence shows weight loss values for two years. In this case, it is being assumed that the total weight loss is split equally over two years
      # instead of assigning all the weight loss in one year
      data[data[[intervention_col]] == "Yes", weight_loss_cols[year]] = -weight_loss_percent*0.5* data[data[[intervention_col]] == "Yes", bodyweight_var]
      
      data[data[[intervention_col]] == "Yes", weight_loss_cols[year+1]] = -weight_loss_percent*0.5* data[data[[intervention_col]] == "Yes", bodyweight_var]
      
      #, weight_loss_cols[year+1] 
      
    } else{
      
      data[data[[intervention_col]] == "Yes", weight_loss_cols[year]] = -weight_loss_percent*0.5* data[data[[intervention_col]] == "Yes", bodyweight_var]
      
      
    }
    # weight regain to be assigned only from the year after intervention, so in this case, it would be two years of weight loss and weight regain in third year
    if (year > 2) {
      prev_intervention_cols <- paste0("intervention_year", 1:(year - 2))
      prev_intervention <- apply(data[, prev_intervention_cols] == "Yes", 1, any)
      
      # Calculate weight regain based on weight loss in one of the two intervention years and calulated as 67% of two times weight loss in any one of the years
      # two times weight loss in any one year is the total weight loss over two years.
      weight_regain <- 0.67 *2* pmax(
        data[prev_intervention, weight_loss_cols[year - 1], drop = TRUE],
        data[prev_intervention, weight_loss_cols[year - 2], drop = TRUE]
      )
      
      data[prev_intervention, weight_regain_cols[year]] <- -weight_regain
    }
  } 
  
  return(data)
}



process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab", nation = "England", population_group = "Adult")

df = read_csv(here("inputs/processed/hse_2019.csv"))


df = df %>%
  mutate(eligibility = case_when(bmi >= 30 ~ 1,
                               #  (bmi >= 30 & bmi < 35) & (cardiovd == 1 | diabetes == 1) ~ 1,
                               #  (bmi >= 27.5 & ethnicity %in% c(2, 3)) ~ 1,    # , 4, 5
                                 TRUE ~ 0))



df_selected = select_intervention_sample(data = df,
                                         bmi_threshold = 30, # interrested in impact of policy on those living with excess weight
                                         required_proportion = 0.4, # assuming that 50% of those living with excess weight increase their physical activity
                                         weight_var = "wt_int",
                                         bmi_var = "bmi",
                                         num_years = 5)














