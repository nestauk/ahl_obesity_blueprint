
##############################################################################################
# Policy 16: Introducing universal free school meals for all primary school children         #
#            during term time                                                                #
#                                                                                            #
##############################################################################################

# Scope: England

# Description:

# The evidence from the rapid review (https://docs.google.com/document/d/1X0LBgHr6ikmySD_aLqv9U8GN-fKjOGEq1A1jaGFOzXQ/edit?usp=sharing)
# shows that a universal free school meals programme for primary school children reduced obesity prevalence
# by 5.6% among year 6 students.

# Eligibility:
# All students in primary school are eligible. However, we apply effect of the policy only to those in year 6
# 10 - 11 year olds = 1,385,486 [1]
# 8 - 12 year olds = 3,471,679  [1]
# proportion of Year 6 children among 8 - 12 year children = 1,385,486/3,471,679 = 0.391

# Modelling Approach:
# (1) set eligibility for all students.
# (2) From all the students, we select 8 - 12 year old children and then select 10 - 11 year old children.
# (3) We do this by calculating the sum of survey weights of 8 - 12 year olds and multiplying with 
#     proportion of 8 - 12 year olds that are 10 - 11 years old [1]
# (4) We then multiply this by the required proportion of children. This gives us the desired sum of sample weights of children.
# (5) Then, we loop through the rows of 8 - 12 year olds to select 5.6% of children living with obesity to receive the effect
# (6) These selected intervention children then have their bmi's updated to the 94th percentile for their age and sex
# (7) We then reestimate the obesity prevalence across the population

# References:
# [1] Office for National Statistics (2022). Estimates of the population for the UK, England and Wales, Scotland and Northern Ireland - Office for National Statistics. [online] Ons.gov.uk. 
#     Available at: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland.





# setup
rm(list = ls())
gc()
library(tidyverse)
library(here)

source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/model_utils.R")
# source(file = "models/adult_model_calorie.R")
# source(file = "models/child_model_calorie_henry.R")


table_outputs = list() # creating a list of table outputs to be saved as an excel file


# required functions:

select_intervention_sample <- function(data, bmi_threshold, required_proportion, 
                                       weight_var, bmi_var, num_years, 
                                       criteria_1=0, criteria_2=0, criteria_1_value=0, criteria_2_value=0,
                                       population_1 = 0, population_2 = 0, population_3 = 0) {
  # browser()
  # Add intervention columns for each year to indicate intervention status, that is if an individual receives intervention.
  intervention_cols <- paste0("intervention_year", 1:num_years)
  data[, intervention_cols] <- "No"
  
  # Initialize a vector to store the individuals who have already received the intervention
  intervention_history <- rep(FALSE, nrow(data))
  
  for (year in 1:num_years) {
    # Subset the data frame to include only individuals meeting the criteria
    # subset_data <- data[(data[[bmi_var]] == bmi_threshold) & (data[[criteria_1]] == criteria_1_value) &  !intervention_history, ]
    subset_data <- data[(data[[criteria_1]] == criteria_1_value) &  !intervention_history, ]
    
    
    #weight_1 = sum(subset_data[[weight_var]]) * (population_1/population_2) # sum of weights of all 10-11 year old
    
    
    # Calculate the total weight of the full dataset
    total_weight <- sum(data[[weight_var]])
    eligible_weight = sum(subset_data[[weight_var]])
    
    # Check if the required proportion is not greater than 1
    if (required_proportion > 1) {
      stop("The desired sample is greater than the population meeting the criteria.")
    }
    
    
    # Calculate the desired weight sum for the sample
    # We use the following formula to calculate the sum of weights of children we want to select to receive intervention =
    # [proportion of children no longer living with obesity] X  [sum of survey weights of children (8- 12 year old)] X 
    # [proportion of 10-11 year old children among 8 - 12 year old children]
    
    desired_weight_sum <- required_proportion * sum(subset_data[[weight_var]]) * (population_1/population_2)
    
    
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
      
      selected_index <- sample(remaining_indices[subset_data[[bmi_var]][remaining_indices] == bmi_threshold], size = 1,
                               prob = subset_data[[weight_var]][remaining_indices[subset_data[[bmi_var]][remaining_indices] == bmi_threshold]])
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
    # selected_indices_original <- which((data[[bmi_var]] == bmi_threshold) & (data[[criteria_1]] == criteria_1_value) &
    #                                      !intervention_history)[selected_individuals]
    
    selected_indices_original <- which((data[[criteria_1]] == criteria_1_value) &
                                         !intervention_history)[selected_individuals]
    
    
    # Update the intervention column for the current year
    data[selected_indices_original, intervention_cols[year]] <- "Yes"
    
    # Update the intervention history
    intervention_history[selected_indices_original] <- TRUE
    
    final_wt = sum(data$wt_int[data$intervention_year1 == "Yes"])
    
  }
  
  return(data)
}



bmi_refdata_100centiles = generate_bmi_refdata_100centiles(sitar::uk90)



process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab",
                   nation = "England",
                   population_group = "Children")

df = read_csv(here("inputs/processed/hse_2019_children.csv")) %>%
  mutate(eligibility = "Yes") %>%
  mutate(apply_effect = case_when(age_grp %in% c("8-10", "11-12") ~ "Yes",
                                  TRUE ~ "No")) %>%
  rowwise() %>%
  mutate(baseline_bmi_category = lookup_bmi_percentile_category(age = age, 
                                                                sex = sex, 
                                                                bmi = bmi,
                                                                data_B = bmi_refdata_100centiles,
                                                                value_to_calculate = "bmi_category")) %>%
  ungroup()



# for reproducibility
set.seed(161)

df_selected = select_intervention_sample(data = df,
                                         bmi_threshold = "obese",
                                         required_proportion = 0.056,
                                         weight_var = "wt_int",
                                         bmi_var = "baseline_bmi_category",
                                         num_years = 1,
                                         criteria_1 = "apply_effect", # selects all 8 - 12 year olds
                                         criteria_1_value = "Yes",
                                         population_1 = 1385486,
                                         population_2 = 3471679)


df_selected = df_selected %>%
  rowwise() %>%
  mutate(post_bmi_value = case_when(intervention_year1 == "Yes" ~ update_bmi(age = age,
                                                                             sex = sex,
                                                                             update_to_centile = 94,
                                                                             data_B = bmi_refdata_100centiles),
                                    TRUE ~ bmi)) %>%
  mutate(post_bmi_category = lookup_bmi_percentile_category(age = age, 
                                                            sex = sex, 
                                                            bmi = post_bmi_value,
                                                            data_B = bmi_refdata_100centiles,
                                                            value_to_calculate = "bmi_category")) %>%
  ungroup()




child_bmi_change = rbind(
  df_selected %>% 
    count(baseline_bmi_category, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Baseline") %>% 
    rename(BMI = baseline_bmi_category),
  df_selected %>% 
    count(post_bmi_category, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Post-Intervention") %>% 
    rename(BMI = post_bmi_category)) %>%
  mutate(BMI = factor(BMI, levels = c("underweight", "normal", "overweight", "obese")))
  




# plot of child BMI prevalance
child_bar_plot = child_bmi_change %>%
  ggplot(., aes(y = freq, x = BMI, fill = type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_ipsum() +
  labs(fill = "", 
       title = "BMI Distribution", 
       y = "Prevalence - %",
       subtitle = "Children - England | Policy 16") +
  theme_ipsum(base_size = 8, axis_title_size = 8) + #, base_family="Averta"
  theme(legend.position = "top")


child_bar_plot


ggsave(here("outputs/policy_16/policy_16_impact_England_child.png"), 
       plot = child_bar_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')



# Table of year wise prevalence of obesity
child_bmi_change_year = child_bmi_change %>%
  select(-c(n)) %>%
  pivot_wider(., names_from = BMI, values_from = freq) %>%
  select("type", "underweight", "normal", "overweight", "obese")

child_bmi_change_year


table_outputs[["england_child"]] = child_bmi_change_year

write_xlsx(path = "outputs/policy_16/policy_16_england.xlsx", x = table_outputs)





# full datafile outputs with intervention assignment
write.csv(df_selected, file = "outputs/policy_16/policy_16_child_england_bmi.csv")



