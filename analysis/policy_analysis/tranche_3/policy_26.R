
#################################################################################################
# Policy 26 : Provide £70 million of funding for increased roll-out of family-based programmes  #
# programme to the local authorities with the highest childhood obesity rates                   #
#                                                                                               #
#################################################################################################

# Description:

# The evidence for this policy comes from the reapid review - https://docs.google.com/document/d/1I93S-TEN7d0_qwIyOLpMztQVbnBBUKjn3nz1u_SZPbw/edit
# The cost of running the programme per family is £320 per year as per the costs of the HENRY Programme.
# The average number of children per family in the UK is 1.7
# £70 million @ £320 per family per year reaches 218,750 families. At 1.7 children per family, this programme
# will cover 371,875 children in an year.
# The eligibility for a child to be exposed to the programme will be that the child lives in a QIMD 4, 5 area and
# has a parent living with overweight/ obesity (BMI >=25)

# The intervention has an effect size of a reduction in child bmi by -0.01 at >= 6 months of intervention. We assume
# that this is the effect at one year.

# Note: We are able to model impacts only for England as the requisite variables to choose parent BMI is not 
# available for Scotland.

# setup
rm(list = ls())
library(tidyverse)
library(here)
library(writexl)

# required functions:

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
    # Subset the data frame to include only individuals meeting the criteria
    subset_data <- data[data[[citeria_1]] == citeria_1_value & 
                          (data[[citeria_2]] == citeria_2_value) &  !intervention_history, ]
    
    # Calculate the total weight of the full dataset
    total_weight <- sum(data[[weight_var]])
    
    # Calculate the proportion of people meeting criteria in the population
    proportion_over_threshold <- sum(subset_data[[weight_var]]) / total_weight
    
    # Calculate the number of people with BMI >= bmi_threshold in the population
    population_over_threshold <- round(proportion_over_threshold * population_size)
    
    # Check if the desired sample size is greater than the population with BMI >= bmi_threshold
    if (sample_size > population_over_threshold) {
      stop("The desired sample size is greater than the population meeting the criteria.")
    }
    
    # Calculate the desired weight sum for the sample
    desired_weight_sum <- sample_size / population_over_threshold * sum(subset_data[[weight_var]])
    
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
    selected_indices_original <- which((data[[citeria_1]] == citeria_1_value & data[[citeria_2]] == citeria_2_value) &
                                         !intervention_history)[selected_individuals]
    
    # Update the intervention column for the current year
    data[selected_indices_original, intervention_cols[year]] <- "Yes"
    
    # Update the intervention history
    intervention_history[selected_indices_original] <- TRUE
  }
  
  return(data)
}

# other required files:

source(file = "requirements.R")
source(file = "models/model_utils.R")
source(file = "pre_processing/pre_processing_adult.R")
#source(file = "models/adult_model_calorie.R")


table_outputs = list()

# Estimating the impact of the policy in:

# 1. Adults in England:

# 1.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab",
                   nation = "England",
                   population_group = "Children")

# 1.2. Estimating the impact of the intervention on prevalence of obesity:


df = read_csv(here("inputs/processed/hse_2019_children.csv"))



intervention_df = select_intervention_sample(data = df,
                                             bmi_threshold = 25,
                                             sample_size = 371875,
                                             population_size = 12000000,
                                             weight_var = "wt_int",
                                             bmi_var = "bmi",
                                             num_years = 1,
                                             citeria_1 = "qimd_updated",
                                             citeria_2 = "parent_bmi",
                                             citeria_1_value = 1,
                                             citeria_2_value = 1)




bmi_refdata_100centiles = generate_bmi_refdata_100centiles(sitar::uk90)

intervention_df = intervention_df %>%
  mutate(bmi_change = case_when(intervention_year1 == "Yes" ~ -0.01,
                              TRUE ~ 0)) %>%
  mutate(bmi_post = bmi + bmi_change) %>%
  rowwise() %>%
  mutate(post_bmi_category = lookup_bmi_percentile_category(age = age, 
                                                      sex = sex, 
                                                      bmi = bmi_post,
                                                      data_B = bmi_refdata_100centiles,
                                                      value_to_calculate = "bmi_category")) %>%
  mutate(baseline_bmi_category = lookup_bmi_percentile_category(age = age, 
                                                            sex = sex, 
                                                            bmi = bmi,
                                                            data_B = bmi_refdata_100centiles,
                                                            value_to_calculate = "bmi_category")) %>%
  
  ungroup() 



child_bmi_change = rbind(
  intervention_df %>% 
    count(baseline_bmi_category, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Baseline") %>% 
    rename(BMI = baseline_bmi_category),
  intervention_df %>% 
    count(post_bmi_category, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Endline") %>% 
    rename(BMI = post_bmi_category))




# plot of child BMI prevalance
child_bar_plot = child_bmi_change %>%
  ggplot(., aes(y = freq, x = BMI, fill = type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_ipsum() +
  labs(fill = "", 
       title = "BMI Distribution", 
       y = "Prevalence - %",
       subtitle = "Children - England") +
  theme_ipsum(base_size = 8, axis_title_size = 8) + #, base_family="Averta"
  theme(legend.position = "top")


child_bar_plot


ggsave(here("outputs/policy_26/policy_26_impact_England_child.png"), 
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

write.csv(child_bmi_change_year, file = "outputs/policy_26/policy_26_child_england.csv")


# full datafile outputs with intervention assignment
write.csv(intervention_df, file = "outputs/policy_26/policy_26_child_england_bmi.csv")





