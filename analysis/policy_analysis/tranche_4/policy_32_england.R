
#################################################################################################
# Policy 32 : Fund and support all neonatal, maternity and health visiting services, Children’s #
#             Centres and midwifery and health visiting courses in universities to become       #
#             Unicef UK Baby Friendly Initiative accredited.                                    #
#                                                                                               #
#################################################################################################

# Scope: England

# Description:

# The evidence is from the rapid review  
# (https://docs.google.com/document/d/1gsjyDEH-sIX1JMv1ulew1NjxDmHbXNUOL9YLOmkZ_bk/edit?usp=sharing) 
# (quality assured by the EAG) showed that the intervention led to reduction no change in  daily calorie
# intake/ BMI/ body weight of children in age group 5 - 18 years.

# Assumption:
# (1) Uptake rate of breastfeeding in England = 48% [2]

# Eligibility criteria:
# Children in age group 5 - 17 years
# Number of children in age group 5 - 17 years = 8,723,931 [2]
# Therefore, 48% children in this age group = 48% x 8,723,931 = 4,187,487


# Effect size
# The intervention has no effect on BMI/ energy intake/ body weight of children in 5 - 17

# References:
# [1] Office for National Statistics (2022). Estimates of the population for the UK, England and Wales, Scotland and Northern Ireland - Office for National Statistics. [online] Ons.gov.uk. 
#     Available at: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland.
# [2] Public Health England (2020). Breastfeeding prevalence at 6 to 8 weeks after birth (experimental statistics) - Annual data statistical commentary 2019 to 2020. [online] 
#     Available at: https://assets.publishing.service.gov.uk/media/60141ed4d3bf7f70c3a49598/OFF_SEN_Annual_Breastfeeding_Statistical_Commentary_2019_2020.pdf.


# setup
rm(list = ls())
gc()
library(tidyverse)
library(here)

source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")
source(file = "models/model_utils.R")



# required functions:

select_intervention_sample <- function(data, sample_size, population_size, 
                                       weight_var, bmi_var, num_years, 
                                       criteria_1, criteria_1_value) {
  #browser()
  
  # Add intervention columns for each year into the dataset
  intervention_cols <- paste0("intervention_year", 1:num_years)
  data[, intervention_cols] <- "No"
  
  # Initialize a vector to store the individuals who have already received the intervention
  intervention_history <- rep(FALSE, nrow(data))
  
  for (year in 1:num_years) {
    # Subset the data frame to include only individuals with BMI >= bmi_threshold and who haven't received the intervention before
    subset_data <- data[data[[criteria_1]] == criteria_1_value & !intervention_history , ] # 
    
    # Calculate the total weight of the full dataset
    total_weight <- sum(data[[weight_var]])
    
    # Calculate the proportion of eligible people in the population
    eligible_proportion <- sum(subset_data[[weight_var]]) / total_weight
    
    # Calculate the number of people of eligible people in the population
    eligible_population <- round(eligible_proportion * population_size)
    
    # Check if the desired sample size is greater than the eligible population
    if (sample_size > eligible_population) {
      stop("The desired sample size is greater than the population with BMI above the threshold.")
    }
    
    # Calculate the desired weight sum such that it would be representative of the sample size we want to choose
    desired_weight_sum <- sample_size / eligible_population * sum(subset_data[[weight_var]])
    
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
    
    # Check if the weighted sum of selected individuals is less than the desired weight sum
    selected_indices <- which(selected_individuals)
    
    if (sum(subset_data[[weight_var]][selected_indices]) < desired_weight_sum) {
      remaining_indices <- which(!selected_individuals)
      additional_index <- sample(remaining_indices, size = 1, prob = subset_data[[weight_var]][remaining_indices])
      selected_individuals[additional_index] <- TRUE
    }
    
    # Get the row indices of the selected individuals in the original data frame
    selected_indices_original <- which(data[[criteria_1]] == criteria_1_value & !intervention_history)[selected_individuals]
    
    # Update the intervention column for the current year for those selected individuals
    data[selected_indices_original, intervention_cols[year]] <- "Yes"
    
    # Update the intervention history to record people from the previous years who received the intervention
    intervention_history[selected_indices_original] <- TRUE
    
    
  }
  print(total_weight)
  print(sum(subset_data[[weight_var]]))
  print(desired_weight_sum)
  print(current_weight_sum)
  print(eligible_population)
  print(sum(data$wt_int[data$intervention_year1 == "Yes"]))
  print(sum(data$wt_int[data$intervention_year2 == "Yes"]))
  print(sum(data$wt_int[data$intervention_year3 == "Yes"]))
  print(sum(data$wt_int[data$intervention_year4 == "Yes"]))
  print(sum(data$wt_int[data$intervention_year5 == "Yes"]))
  
  
  
  return(data)
}






table_outputs = list() # creating a list of table outputs to be saved as an excel file


# Estimating the impact of the policy in:


# 1. Children in England

# 1.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab",
                   nation = "England",
                   population_group = "Children")


# 1.2. Identifying eligible children to receive intervention i.e. all children aged 5 - 17 years

df = read.csv(here("inputs/processed/hse_2019_children.csv")) %>%
  mutate(eligibility_criteria = case_when(age >= 5 & age < 18 ~ 1, # the highest age value in the dataset is 17.5 for children
                                          TRUE ~ 0))

# Eligibility criteria:
# Children in age group 5 - 17 years
# Number of children in age group 5 - 17 years = 8,723,931 [2]
# Therefore, 48% children in this age group = 48% x 8,723,931 = 4,187,487 (as breast feeding uptake is 48%)


set.seed(321)

intervention_df = select_intervention_sample(data = df,
                                             sample_size = 4187487,
                                             population_size = 8723931,
                                             weight_var = "wt_int",
                                             bmi_var = "bmi",
                                             num_years = 1,
                                             criteria_1 = "eligibility_criteria",
                                             criteria_1_value = 1)



# 1.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: -0 kg/m2

effect_size = -0.00


bmi_refdata_100centiles = generate_bmi_refdata_100centiles(sitar::uk90)

# calculating baseline bmi categories
intervention_df = intervention_df %>%
  rowwise() %>%
  mutate(baseline_bmi_category = lookup_bmi_percentile_category(age = age, 
                                                                sex = sex, 
                                                                bmi = bmi,
                                                                data_B = bmi_refdata_100centiles,
                                                                value_to_calculate = "bmi_category"))


# the effect is applied only to those children living with excess weight

intervention_df = intervention_df %>%
  mutate(bmi_change = case_when(intervention_year1 == "Yes" &
                                  baseline_bmi_category %in% c("overweight", "obese") ~ effect_size,
                                TRUE ~ 0)) %>%
  mutate(bmi_post = bmi + bmi_change) %>%
  rowwise() %>%
  mutate(post_bmi_category = lookup_bmi_percentile_category(age = age, 
                                                            sex = sex, 
                                                            bmi = bmi_post,
                                                            data_B = bmi_refdata_100centiles,
                                                            value_to_calculate = "bmi_category")) %>%
  mutate(baseline_bmi_category_1 = lookup_bmi_percentile_category(age = age, 
                                                                  sex = sex, 
                                                                  bmi = bmi,
                                                                  data_B = bmi_refdata_100centiles,
                                                                  value_to_calculate = "bmi_category")) %>%
  
  ungroup() 



# Table of year wise prevalence of obesity
child_bmi_change = rbind(
  intervention_df %>% 
    count(baseline_bmi_category, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Baseline") %>% 
    rename(BMI = baseline_bmi_category),
  intervention_df %>% 
    count(post_bmi_category, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Post-Intervention") %>% 
    rename(BMI = post_bmi_category))




# Output 1: plot of child BMI prevalance
child_bar_plot = child_bmi_change %>%
  ggplot(., aes(y = freq, x = BMI, fill = type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_ipsum() +
  labs(fill = "", 
       title = "BMI Distribution", 
       y = "Prevalence - %",
       subtitle = "Children - England | Policy 32") +
  theme_ipsum(base_size = 8, axis_title_size = 8) + #, base_family="Averta"
  theme(legend.position = "top")


child_bar_plot


ggsave(here("outputs/policy_32/policy_32_impact_England_child.png"), 
       plot = child_bar_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')



# Output 2: Table of year wise prevalence of obesity
child_bmi_change_year = child_bmi_change %>%
  select(-c(n)) %>%
  pivot_wider(., names_from = BMI, values_from = freq) %>%
  select("type", "underweight", "normal", "overweight", "obese")

child_bmi_change_year
table_outputs[["england_child"]] = child_bmi_change_year

write_xlsx(path = "outputs/policy_32/policy_32_england.xlsx", x = table_outputs)


# Output 3: Cost modelling input files:
write.csv(intervention_df, file = "outputs/policy_32/policy_32_child_england_bmi.csv")

