
#############################################################################################
# Policy 35 : Everyone with a BMI of 30 or above is offered a free referral to behavioural  #
#             weight management programme                                                   #
#                                                                                           #
#############################################################################################

# Description:

# The evidence from the rapid review 
# (https://docs.google.com/document/d/1K20dg2D-G9J6F439gegPRD8Mly58xSnJmGXJMto_GYY/edit?usp=sharing) 
# showed that the interventions led to a weight loss of -10.7 kgs at the
# end of one year for those on TDR. The take-up rate for the programme was 40% of which 13% opted in 
# to receive TDR.
# Effect size: 2.4 kgs in the first year
# weight regain: 0.32 kg per year
# Eligibility: Adults with a BMI >=30

# In the modelling we assume that those who receive the treatment once do not receive it again the
# subsequent years.



# setup
rm(list = ls())
library(tidyverse)
library(here)
library(writexl)
library(survey)


source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")

# functions:

# function for choosing the intervention sample:
# The logic for this function is explained here: https://docs.google.com/document/d/1b8eo_wgedOrJ-D5AWqIjCxmCr_yu-ez3EA6uZkwMvmQ/edit?usp=sharing
select_intervention_sample <- function(data, # sample_size, population_size, 
                                       weight_var, bmi_var, num_years, 
                                       criteria_1, criteria_1_value, 
                                       required_proportion_1, required_proportion_2) {
  browser()
  
  # Add intervention columns for each year into the dataset
  intervention_cols <- paste0("intervention_year", 1:num_years)
  data[, intervention_cols] <- 0
  
  # Initialize a vector to store the individuals who have already received the intervention
  intervention_history <- rep(FALSE, nrow(data))
  
  for (year in 1:num_years) {
    # Subset the data frame to include only individuals with BMI >= bmi_threshold and who haven't received the intervention before
    subset_data <- data[data[[criteria_1]] == criteria_1_value & !intervention_history , ] # 
    
    # Calculate the total weight of the full dataset
    total_weight <- sum(data[[weight_var]])
    
    # Calculate the proportion of eligible people in the population
    # eligible_proportion <- sum(subset_data[[weight_var]]) / total_weight
    
    # Calculate the number of people of eligible people in the population
    # eligible_population <- round(eligible_proportion * population_size)
    
    # browser()
    # Check if the desired sample size is greater than the eligible population
    if (required_proportion_1 > 1 | required_proportion_2 > 1) {
      stop("The desired sample size is greater than the population with BMI above the threshold.")
    }
    
    # Calculate the desired weight sum such that it would be representative of the sample size we want to choose
    # desired_weight_sum <- sample_size / eligible_population * sum(subset_data[[weight_var]])
    desired_weight_sum <- required_proportion_1 * sum(subset_data[[weight_var]])
    
    # Initialize a vector to store the selected individuals for the current year
    selected_individuals <- rep(FALSE, nrow(subset_data))
    
    # Initialize a variable to keep track of the current weight sum
    current_weight_sum <- 0
    
    # Perform weighted random sampling until the sum of weights of all the selected individuals is >= desired weight sum
    while (current_weight_sum < desired_weight_sum) {
      remaining_indices <- which(!selected_individuals)
      
      # checking that there are enough individuals to select from in the eligible population
      if (length(remaining_indices) == 0) {
        break
      }
      
      selected_index <- sample(remaining_indices, size = 1, prob = subset_data[[weight_var]][remaining_indices])
      selected_individuals[selected_index] <- TRUE
      
      current_weight_sum <- current_weight_sum + subset_data[[weight_var]][selected_index]
    }
    
    # browser()
    # Check if the weighted sum of selected individuals is less than the desired weight sum
    selected_indices <- which(selected_individuals)
    
    #if (sum(subset_data[[weight_var]][selected_indices]) < desired_weight_sum) {
    #  remaining_indices <- which(!selected_individuals)
    #  additional_index <- sample(remaining_indices, size = 1, prob = subset_data[[weight_var]][remaining_indices])
    #  selected_individuals[additional_index] <- TRUE
    #}
    
    # Get the row indices of the selected individuals in the original data frame
    selected_indices_original <- which(data[[criteria_1]] == criteria_1_value & !intervention_history)[selected_individuals]
    
    # Update the intervention column for the current year for those selected individuals
    data[selected_indices_original, intervention_cols[year]] <- 1 # "Yes"
    
    # Update the intervention history to record people from the previous years who received the intervention
    intervention_history[selected_indices_original] <- TRUE
    
    browser()
    
    subset_data_filt <- data[data[[intervention_cols[year]]] == 1,] #
    
    
    desired_weight_sum_1 <- required_proportion_2 * sum(subset_data_filt[[weight_var]])
    
    selected_individuals_1 <- rep(FALSE, nrow(subset_data_filt))
    
    # Initialize a variable to keep track of the current weight sum
    current_weight_sum_1 <- 0
    
    # browser()
    # Perform weighted random sampling until the sum of weights of all the selected individuals is >= desired weight sum
    while (current_weight_sum_1 < desired_weight_sum_1) {
      remaining_indices_1 <- which(!selected_individuals_1)
      
      # checking that there are enough individuals to select from in the eligible population
      if (length(remaining_indices_1) == 0) {
        break
      }
      
      selected_index_1 <- sample(remaining_indices_1, size = 1, prob = subset_data_filt[[weight_var]][remaining_indices_1])
      selected_individuals_1[selected_index_1] <- TRUE
      
      current_weight_sum_1 <- current_weight_sum_1 + subset_data_filt[[weight_var]][selected_index_1]
    }
    
    # browser()
    # Check if the weighted sum of selected individuals is less than the desired weight sum
    selected_indices_1 <- which(selected_individuals_1)
    
    #if (sum(subset_data[[weight_var]][selected_indices]) < desired_weight_sum) {
    #  remaining_indices <- which(!selected_individuals)
    #  additional_index <- sample(remaining_indices, size = 1, prob = subset_data[[weight_var]][remaining_indices])
    #  selected_individuals[additional_index] <- TRUE
    #}
    
    subset_data_filt <- data[data[[intervention_cols[year]]] == 1,] #
    
    # Get the row indices of the selected individuals in the original data frame
    selected_indices_original_1 <- which(data[[criteria_1]] == criteria_1_value &
                                           data[[intervention_cols[year]]] == 1)[selected_individuals_1]
    
    # Update the intervention column for the current year for those selected individuals
    data[selected_indices_original_1, intervention_cols[year]] <- 2 # "Yes"
    
    # Update the intervention history to record people from the previous years who received the intervention
    # intervention_history[selected_indices_original] <- TRUE
    # browser()
    
  }
  
  # browser()
  print(total_weight)
  print(sum(subset_data[[weight_var]]))
  print(desired_weight_sum)
  print(current_weight_sum)
  print(sum(data$wt_int[data$intervention_year1 == 1]))
  print(sum(data$wt_int[data$intervention_year1 == 2]))
  print(sum(data$wt_int[data$intervention_year1 != 0]))
  print(sum(data$wt_int[data$intervention_year2 == 1]))
  print(sum(data$wt_int[data$intervention_year2 == 2]))
  print(sum(data$wt_int[data$intervention_year2 != 0]))
  print(sum(data$wt_int[data$intervention_year3 == 1]))
  print(sum(data$wt_int[data$intervention_year3 == 2]))
  print(sum(data$wt_int[data$intervention_year3 != 0]))
  print(sum(data$wt_int[data$intervention_year4 == 1]))
  print(sum(data$wt_int[data$intervention_year4 == 2]))
  print(sum(data$wt_int[data$intervention_year4 != 0]))
  print(sum(data$wt_int[data$intervention_year5 == 1]))
  print(sum(data$wt_int[data$intervention_year5 == 2]))
  print(sum(data$wt_int[data$intervention_year5 != 0]))
  
  return(data)
}



assign_weight_changes_2 <- function(data, bodyweight_var, num_years, weight_loss_1, weight_loss_2, weight_regain) {
  browser()
  # Create weight loss and weight regain columns for each year
  weight_loss_cols <- paste0("weight_loss_y", 1:num_years)
  weight_regain_cols <- paste0("weight_regain_y", 1:num_years)
  data[, c(weight_loss_cols, weight_regain_cols)] <- 0
  
  for (year in 1:num_years) {
    intervention_col <- paste0("intervention_year", year)
    
    if(year < num_years + 1){
      
      # Assign weight loss for individuals who received the intervention in the current year
      # evidence shows weight loss values for two years. In this case, it is being assumed that the total weight loss is split equally over two years
      # instead of assigning all the weight loss in one year
      data[data[[intervention_col]] == 1, weight_loss_cols[year]] = -weight_loss_1 
      
      # data[data[[intervention_col]] == 2, weight_loss_cols[year]] = -weight_loss_2
      
      #, weight_loss_cols[year+1] 
      
    } else{
      
      # data[data[[intervention_col]] == "Yes", weight_loss_cols[year]] = -weight_loss_percent*0.5* data[data[[intervention_col]] == "Yes", bodyweight_var]
      
      
    }
    browser()
    # weight regain to be assigned only from the year after intervention, so in this case, it would be one year of weight loss and weight regain in second year
    if (year > 1) {
      # prev_intervention_cols <- paste0("intervention_year", 1:(year - 1))
      # prev_intervention_data <- data[, prev_intervention_cols]
      # prev_intervention_matrix = data[, prev_intervention_cols] == 1 | data[, prev_intervention_cols] == 2
      # prev_intervention <- apply(as.data.frame(data[, prev_intervention_cols] == 1 | data[, prev_intervention_cols] == 2),
      #                            1, any)
      # 
      # weight_regain_1 = weight_regain
      # 
      # 
      # data[prev_intervention & data[, paste0("weight_loss_y", year - 1)] < 0, weight_regain_cols[year]] <- weight_regain_1
      # data[prev_intervention & data[, paste0("weight_loss_y", year - 1)] < 0, weight_regain_cols[year+1]] <- weight_regain_1
      
      prev_intervention_cols <- paste0("intervention_year", 1:(year - 1))
      
      if (year == 2){
        
      #  prev_intervention_cols <- paste0("intervention_year", 1:(year - 1))
        
      } else {
        
        
      #  prev_intervention_cols <- paste0("intervention_year", (year - 2):(year - 1)) #   
        
        
      }
      
      prev_intervention <- apply(data[, prev_intervention_cols] == 1, 1, any) # data[, prev_intervention_cols] == 1 |
      
      data[prev_intervention, weight_regain_cols[year]] <- weight_regain # * data[prev_intervention, weight_var]
      
      
    }
    
    
  } 
  
  return(data)
  
}



process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab", nation = "England", population_group = "Adult")



df = read_csv(here("inputs/processed/hse_2019.csv"))


df = df %>%
  mutate(eligibility = case_when(bmi >= 30 ~ 1, TRUE ~ 0))


set.seed(371)


df_selected = select_intervention_sample(data = df, weight_var = "wt_int",
                                         bmi_var = "bmi", num_years = 5,
                                         criteria_1 = "eligibility", criteria_1_value = 1,
                                         required_proportion_1 = 0.4, required_proportion_2 = 0)


post_df_adult_2 = assign_weight_changes_2(data = df_selected, bodyweight_var = "weight",
                                          num_years = 5, weight_loss_1 = 2.4,
                                          weight_loss_2 = 0, weight_regain = 0.32)

post_df_adult_2 = post_df_adult_2 %>%
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
bmi_change_2 = rbind(
  post_df_adult_2 %>% 
    count(bmi_5_class, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 5") %>% 
    rename(BMI = bmi_5_class),
  post_df_adult_2 %>% 
    count(bmi_4_class, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 4") %>% 
    rename(BMI = bmi_4_class),
  post_df_adult_2 %>% 
    count(bmi_3_class, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 3") %>% 
    rename(BMI = bmi_3_class),
  post_df_adult_2 %>% 
    count(bmi_2_class, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 2") %>% 
    rename(BMI = bmi_2_class),
  post_df_adult_2 %>% 
    count(bmi_1_class, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 1") %>% 
    rename(BMI = bmi_1_class),
  post_df_adult_2 %>% 
    count(bmi_class, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 0") %>% 
    rename(BMI = bmi_class))

bmi_change_2 = bmi_change_2 %>%
  mutate(BMI = factor(BMI, levels = c("underweight", "normal", "overweight", "obese", "morbidly obese"))) %>%
  as.data.frame()


bmi_change_year_2 = bmi_change_2 %>%
  select(-c(n)) %>%
  pivot_wider(., names_from = BMI, values_from = freq) %>%
  select(type, underweight, normal, overweight, obese, `morbidly obese`)

bmi_change_year_2





# bmi year on year prevalence:
write.csv(bmi_change_year, file = "outputs/policy_6/policy_6_updated_1_adult_england.csv")


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

ggsave(here("outputs/policy_6/policy_6_updated_1_impact_England_adult.png"), 
       plot = adult_bar_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')


write.csv(post_df_adult, file = "outputs/policy_6/policy_6_updated_1_adult_england_bmi.csv")



result_df = read_csv(here("outputs/policy_6/policy_6_updated_1_adult_england_bmi.csv"))



number_treated = post_df_adult %>%
  select(wt_int, intervention_year1, intervention_year2, intervention_year3, intervention_year4, intervention_year5) %>%
  group_by(intervention_year1, intervention_year2, intervention_year3, intervention_year4, intervention_year5) %>%
  mutate(wt_val = sum(wt_int))


df_long <- post_df_adult %>%
  select(wt_int, intervention_year1, intervention_year2, intervention_year3, intervention_year4, intervention_year5) %>%
  pivot_longer(cols = starts_with("intervention"),
               names_to = "intervention",
               values_to = "value")

# Summarize the data
summary_table <- df_long %>%
  group_by(intervention, value) %>%
  summarize(sum_wt_int = sum(wt_int, na.rm = TRUE))


summary_table <- result_df %>%
  # Gather the intervention columns into a long format
  pivot_longer(
    cols = starts_with("intervention"),
    names_to = "intervention_type",
    values_to = "intervention_value"
  ) %>%
  # Group by intervention type and value
  group_by(intervention_type, intervention_value) %>%
  # Sum the wt_int for each group
  summarise(total_wt_int = sum(wt_int, na.rm = TRUE)) %>%
  # Spread the results back to a wide format
  pivot_wider(
    names_from = intervention_value,
    values_from = total_wt_int,
    names_prefix = "value_"
  ) %>%
  # Replace NA with 0 for cleaner output
  mutate(across(starts_with("value_"), ~replace_na(., 0))) 
#  mutate(total = rowSums(select(., starts_with("value_")), na.rm = TRUE)) %>%
#  select(intervention_type, everything())


