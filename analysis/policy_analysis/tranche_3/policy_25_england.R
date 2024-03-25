#################################################################################################
# Policy 25 : Expand NHS provision of bariatric surgery to all patients with BMI > 35 with      #
#             pre-existing condition (specifically expand 5-fold, from 7,000 people recieving   #
#             surgery per annum to 35,000 people)                                               #
#                                                                                               #
#################################################################################################

# Description:

# Scope: England

# The evidence comes from the results of the rapid review available here
# - https://docs.google.com/document/d/1HO14YfgenuQk4zy_ldeTILA0nrkv90yq/edit?usp=sharing&ouid=102713518635256687243&rtpof=true&sd=true
# Those receiving any form of bariatric surgery will have a weight loss of 23%.
# Weight regain is 0.81 kg per year. The selection criteria to receive bariatric surgery is a bmi of 35 and above
# along with a comorbidity which has been indicated by a doctor diagnosed diabetes or taking of medicines for
# cardiovascular diseases.



# setup
rm(list = ls())
library(tidyverse)
library(here)
library(writexl)

# functions:

# function for choosing the intervention sample:

select_intervention_sample <- function(data, bmi_threshold, sample_size, population_size, 
                                       weight_var, bmi_var, num_years, 
                                       citeria_1, citeria_2, citeria_1_value, citeria_2_value) {
  #browser()
  # Add intervention columns for each year
  intervention_cols <- paste0("intervention_year", 1:num_years)
  data[, intervention_cols] <- "No"
  
  # Initialize a vector to store the individuals who have already received the intervention
  intervention_history <- rep(FALSE, nrow(data))
  
  for (year in 1:num_years) {
    # Subset the data frame to include only individuals with BMI >= bmi_threshold and who haven't received the intervention before
    subset_data <- data[data[[bmi_var]] >= bmi_threshold & 
                          (data[[citeria_1]] == citeria_1_value | data[[citeria_2]] == citeria_2_value) &
                          !intervention_history, ]
    
    # Calculate the total weight of the subset
    total_weight <- sum(data[[weight_var]])
    
    # Calculate the proportion of people with BMI >= bmi_threshold in the population
    proportion_bmi_over_threshold <- sum(subset_data[[weight_var]]) / total_weight
    
    # Calculate the number of people with BMI >= bmi_threshold in the population
    population_bmi_over_threshold <- round(proportion_bmi_over_threshold * population_size)
    
    # Check if the desired sample size is greater than the population with BMI >= bmi_threshold
    if (sample_size > population_bmi_over_threshold) {
      stop("The desired sample size is greater than the population with BMI above the threshold.")
    }
    
    # Calculate the desired weight sum for the sample
    desired_weight_sum <- sample_size / population_bmi_over_threshold * sum(subset_data[[weight_var]])
    
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
    selected_indices_original <- which(data[[bmi_var]] >= bmi_threshold & 
                                         (data[[citeria_1]] == citeria_1_value | data[[citeria_2]] == citeria_2_value) &
                                         !intervention_history)[selected_individuals]
    
    # Update the intervention column for the current year
    data[selected_indices_original, intervention_cols[year]] <- "Yes"
    
    # Update the intervention history
    intervention_history[selected_indices_original] <- TRUE
  }
  
  return(data)
}


# function to assign weight loss and weight regain:

assign_weight_changes <- function(data, weight_var, num_years, weight_loss_percent, weight_regain) {
  #browser()
  # Create weight loss and weight regain columns for each year
  weight_loss_cols <- paste0("weight_loss_y", 1:num_years)
  weight_regain_cols <- paste0("weight_regain_y", 1:num_years)
  data[, c(weight_loss_cols, weight_regain_cols)] <- 0
  
  for (year in 1:num_years) {
    intervention_col <- paste0("intervention_year", year)
    
    # Assign weight loss for individuals who received the intervention in the current year
    data[data[[intervention_col]] == "Yes", weight_loss_cols[year]] = -weight_loss_percent * data[data[[intervention_col]] == "Yes", weight_var]
    
    # Assign weight regain for individuals who received the intervention in previous years
    if (year > 1) {
      prev_intervention_cols <- paste0("intervention_year", 1:(year - 1))
      prev_intervention <- apply(data[, prev_intervention_cols] == "Yes", 1, any)
      data[prev_intervention, weight_regain_cols[year]] <- weight_regain #* data[prev_intervention, weight_var]
    }
  } 
  
  return(data)
}

# other required files

source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")


table_outputs = list()

# Estimating the impact of the policy in:

# 1. Adults in England:

# 1.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab", nation = "England", population_group = "Adult")

# 1.2. Estimating the impact of the intervention on prevalence of obesity:


df = read_csv(here("inputs/processed/hse_2019.csv"))

# assigning intervention for the 7000 additional bariatric surgery. we are choosing 7000 additional people
# per year to receive the interventions
intervention_df = select_intervention_sample(data = df,
                                             bmi_threshold = 35,
                                             sample_size = 7000,
                                             population_size = 45000000,
                                             weight_var = "wt_int",
                                             bmi_var = "bmi",
                                             num_years = 5,
                                             citeria_1 = "cardiovd",
                                             citeria_1_value = 1,
                                             citeria_2 = "diabetes",
                                             citeria_2_value = 1)




# assigning weight loss and weight regain based on intervention cohort
post_df_adult = assign_weight_changes(data = intervention_df,
                                      weight_var = "weight",
                                      num_years = 5,
                                      weight_loss_percent = 0.23,
                                      weight_regain = 0.81)



# year wise weight

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



# survey design element created to account for survey weights and population level estimation of prevalance.
design <-  svydesign(ids=~post_df_adult$psu, 
                     nest = T,
                     data=post_df_adult,
                     weights=post_df_adult$wt_int)

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

bmi_change = bmi_change %>%
  mutate(BMI = factor(BMI, levels = c("underweight", "normal", "overweight", "obese", "morbidly obese"))) %>%
  as.data.frame()


bmi_change_year = bmi_change %>%
  select(-c(n)) %>%
  pivot_wider(., names_from = BMI, values_from = freq) %>%
  select(type, underweight, normal, overweight, obese, `morbidly obese`)

bmi_change_year

# bmi year on year prevalence:
write.csv(bmi_change_year, file = "outputs/policy_25/policy_25_adult_england.csv")


# Plot of year on year BMI category distribution
adult_bar_plot = bmi_change %>%
  ggplot(., aes(y = freq, x = BMI, fill = type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_ipsum() +
  labs(fill = "", 
       title = "BMI Categories Distribution", 
       y = "Frequency",
       subtitle = "Population") +
  theme_ipsum(base_size = 8, axis_title_size = 8) + #, base_family="Averta"
  theme(legend.position = "top")

adult_bar_plot

ggsave(here("outputs/policy_25/policy_25_impact_England_adult.png"), 
       plot = adult_bar_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')


write.csv(post_df_adult, file = "outputs/policy_25/policy_25_adult_england_bmi.csv")


