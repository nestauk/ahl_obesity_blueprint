
#################################################################################################
# Policy 15 : Provide business rates relief of 75% to new or expanded businesses selling fresh  #
#             fruit and vegetables opening in food deserts                                      #
#                                                                                               #
#################################################################################################

# Scope: Wales

# Description:

# The evidence is from the rapid review  
# (https://docs.google.com/document/d/17SbFv6wdhNi__94e4e00drK7RSs9hTXy6wa_rGo2-Gw/edit?usp=sharing) 
# (quality assured by the EAG) showed that the intervention led to reduction no statistically significant
# change in individual BMIs.

# Assumption:


# Eligibility criteria:
# (1) Adults 18+: Number of adults in age group 18+ years in England = 44,263,393 [1]
# (2) Living in IMD areas 4 and 5 and in a food desert in Great Britain = 1,200,000 [2]
# We then estimate the number of people per country based on the ratio of people from different nations as per UK mid year 2019 population:
# England = 87%, Scotland = 8%, Wales = 5%
# (3) Living in IMD areas 4 and 5 and in a food desert in Wales = 5% * 1,200,000 = 58,294 [2] 


# Effect size:
# The intervention has no effect on BMI/ body weight of Adults - 18+

# Modelling approach:
# (1) We first filter for those living in IMD 4 or 5, then randomly select 1.2 million individuals.
#     Of the selected individuals, the effect will be applied on only those who are living with excess weight.
# (2) Individuals may be exposed to the intervention multiple times

# References:
# [1] Office for National Statistics (2022). Estimates of the population for the UK, England and Wales, Scotland and Northern Ireland - Office for National Statistics. [online] Ons.gov.uk. 
#     Available at: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland.
# [2] Corfe, S. (2018). What are the barriers to eating healthily in the UK? [online] 
#     Available at: https://www.smf.co.uk/wp-content/uploads/2018/10/What-are-the-barriers-to-eating-healthy-in-the-UK.pdf.


# setup
rm(list = ls())
gc()
library(tidyverse)
library(here)

source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")


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
    # subset_data <- data[data[[criteria_1]] == criteria_1_value & !intervention_history , ] # 
    subset_data <- data[data[[criteria_1]] == criteria_1_value, ] # 
    
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
    # selected_indices_original <- which(data[[criteria_1]] == criteria_1_value & !intervention_history)[selected_individuals]
    selected_indices_original <- which(data[[criteria_1]] == criteria_1_value)[selected_individuals]
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


assign_weight_changes <- function(data, bodyweight_var, num_years, weight_loss, weight_regain) {
  # browser()
  # Create weight loss and weight regain columns for each year
  weight_loss_cols <- paste0("weight_loss_y", 1:num_years)
  weight_regain_cols <- paste0("weight_regain_y", 1:num_years)
  bmi_y_cols <- paste0("bmi_y", 1:num_years)
  data[, c(weight_loss_cols, weight_regain_cols)] <- 0
  
  for (year in 1:num_years) {
    intervention_col <- paste0("intervention_year", year)
    bmi_col <- paste0("bmi_y", year)
    
    if(year < 6){
      
      # Assign weight loss for individuals who received the intervention in the current year
      
      data[data[[intervention_col]] == "Yes" & data[, paste0("bmi_y", year - 1)] >=25, weight_loss_cols[year]] = weight_loss # *0.5* data[data[[intervention_col]] == "Yes", bodyweight_var]
      
      
    } else{
      
      print("unexpected entry into else of the if_else loop, check year inputs to the function")
      
      
    }
    
    # weight regain to be assigned only from the year after intervention, so in this case, it would be from the second year onwards:
    if (year > 1) {
      prev_intervention_cols <- paste0("intervention_year", 1:(year - 1))
      prev_intervention <- apply(as.data.frame(data[, prev_intervention_cols] == "Yes"), 1, any)
      
      weight_regain_1 = weight_regain
      
      
      data[prev_intervention & data[, paste0("weight_loss_y", year - 1)] < 0, weight_regain_cols[year]] <- weight_regain_1
    }
    
    # Calculate BMI for each year
    if (year == 1) {
      data[, bmi_y_cols[year]] <- data$bmi + data[, weight_loss_cols[year]] + data[, weight_regain_cols[year]]
    } else {
      data[, bmi_y_cols[year]] <- data[, bmi_y_cols[year - 1]] + data[, weight_loss_cols[year]] + data[, weight_regain_cols[year]]
    }
    
    
  } 
  
  return(data)
}






table_outputs = list() # creating a list of table outputs to be saved as an excel file


# Estimating the impact of the policy in:


# 1. Adults in Wales:

# 1.1. Cleaned input/ baseline data:

df_wales_cleaned = read_csv(here("inputs/processed/nsw_2019.csv"))



# 1.2. Selecting adults living in IMD 4 or 5

df = df_wales_cleaned %>%
  mutate(eligibility_criteria = case_when(simd_updated == 1 ~ 1,
                                          TRUE ~ 0))



# (1) Adults 18+: Number of adults in age group 18+ years in Wales = 3,152,879 [1]
# (2) Living in IMD areas 4 and 5 and in a food desert in Great Britain = 1,200,000 [2]
# We then estimate the number of people per country based on the ratio of people from different nations as per UK mid year 2019 population:
# England = 87%, Scotland = 8%, Wales = 5%
# (3) Living in IMD areas 4 and 5 and in a food desert in Wales = 5% * 1,200,000 = 58,294 [2] 

POPULATION_ENGLAND = 56286961
POPULATION_WALES = 3152879
POPULATION_SCOTLAND = 5463300

POP_DEPRIVED_AREAS_FOOD_DESERT = 1200000

POPULATION_GREAT_BRITAIN = POPULATION_ENGLAND + POPULATION_WALES + POPULATION_SCOTLAND
POP_DEPRIVED_AREAS_FOOD_DESERT_WALES = (POPULATION_WALES/POPULATION_GREAT_BRITAIN) * POP_DEPRIVED_AREAS_FOOD_DESERT
SAMPLE_REQUIRED = POP_DEPRIVED_AREAS_FOOD_DESERT_WALES


set.seed(153)

intervention_df = select_intervention_sample(data = df,
                                             sample_size = SAMPLE_REQUIRED,
                                             population_size = POPULATION_WALES, 
                                             weight_var = "wt_int",
                                             bmi_var = "bmi",
                                             num_years = 5,
                                             criteria_1 = "eligibility_criteria",
                                             criteria_1_value = 1)

intervention_df = intervention_df %>%
  mutate(bmi_y0 = bmi)



# 1.3. Estimating the impact of the intervention on prevalence of obesity:


# assigning effect sizes to individuals: the effect is applied only if the individual
# is living with excess weight in addition to living in the most deprived areas
# inluding sign (+/-)
effect_size = -0.00

post_df_adult = assign_weight_changes(data = intervention_df,
                                      bodyweight_var = "weight",
                                      num_years = 5,
                                      weight_loss = effect_size,
                                      weight_regain = 0)


post_df_adult = post_df_adult %>%
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

bmi_change = bmi_change %>%
  mutate(BMI = factor(BMI, levels = c("underweight", "normal", "overweight", "obese", "morbidly obese"))) %>%
  as.data.frame()



# display outputs:
bmi_change_year = bmi_change %>%
  select(-c(n)) %>%
  pivot_wider(., names_from = BMI, values_from = freq) %>%
  select(type, underweight, normal, overweight, obese, `morbidly obese`)

bmi_change_year

table_outputs[["wales_adult"]] = bmi_change_year


# Plot of year on year BMI category distribution
adult_bar_plot = bmi_change %>%
  ggplot(., aes(y = freq, x = BMI, fill = type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_ipsum() +
  labs(fill = "", 
       title = "BMI Categories Distribution", 
       y = "Frequency",
       subtitle = "Population | wales") +
  theme_ipsum(base_size = 8, axis_title_size = 8) + #, base_family="Averta"
  theme(legend.position = "top")

adult_bar_plot


# writing outputs to folder:

ggsave(here("outputs/policy_15/policy_15_impact_wales_adult.png"), 
       plot = adult_bar_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')

write.csv(post_df_adult, file = "outputs/policy_15/policy_15_adult_wales_bmi.csv")
write_xlsx(path = "outputs/policy_15/policy_15_wales.xlsx", x = table_outputs)



