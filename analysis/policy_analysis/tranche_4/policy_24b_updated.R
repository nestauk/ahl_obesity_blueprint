
################################################################################################### 
# Policy24b : Extend access to pharmacotherapy so that approximately 3 million people (BMI≥30)    #
#             receive Semaglutide each year.                                                      #
#                                                                                                 #
###################################################################################################

# Description:


# Scope: England

# The evidence comes from the results of the rapid review available here
# - https://docs.google.com/document/d/1hozT3EvH5fbl1W9CeIcrrr7TGpotYu07/edit?usp=sharing&ouid=102713518635256687243&rtpof=true&sd=true


# Effect size:
# Those receiving any of the drug (Semaglutide) on average experience weight loss of 15.83% of their bodyweight
# 100% of the weight loss assumed to be in first year, no weight change in year 2, weight gain will occur in year 3 when the individual goes off the drug
# Weight regain is two-thirds of the lost weight (b)

# Number of people reached:
# We assume that  annually the policy manages to reach ~ 3 million people to have the maximum impact
# So that over a period of five years the drug is rolled out to ~ 12.9 million people living with obesity as of 2019

# Population estimate for adults in England equal and over 18 years of age = 44,263,393 (a)

# Eligibility criteria for getting the drugs:
# There is overlapping eligibility criteria for the three drugs - Semaglutide, Liraglutide 
# which is summarised below to create a hybrid eligibility criteria that individuals must meet:
# adults with bmi >= 30

# Note: Also, people can receive the treatment only once in the modelling period of five years.
# i.e. if someone receives the drug for two years in year 1,
# they will not be eligible again in year 4 or year 5 to recieve the drug.


# References:
# (a) ONS 2019 Mid-Year Population Estimates - https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/analysisofpopulationestimatestool)
# (b) Wilding JPH, Batterham RL, Davies M, Van Gaal LF, Kandler K, Konakli K, Lingvay I, McGowan BM, Oral TK, Rosenstock J, 
#     Wadden TA, Wharton S, Yokote K, Kushner RF; STEP 1 Study Group. Weight regain and cardiometabolic effects after withdrawal of 
#     semaglutide: The STEP 1 trial extension. Diabetes Obes Metab. 2022 Aug;24(8):1553-1564. doi: 10.1111/dom.14725. Epub 2022 May 19. 
#     PMID: 35441470; PMCID: PMC9542252.
# (c) NHS (2023). Treatment - Obesity. [online] NHS. Available at: https://www.nhs.uk/conditions/obesity/treatment/. [Note this matches with NICE Guidelines for each drug]






# setup
rm(list = ls())
library(tidyverse)
library(here)
library(writexl)


source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
# source(file = "models/adult_model_calorie.R")

# functions:

# function for choosing the intervention sample:
# The logic for this function is explained here: https://docs.google.com/document/d/1b8eo_wgedOrJ-D5AWqIjCxmCr_yu-ez3EA6uZkwMvmQ/edit?usp=sharing
select_intervention_sample <- function(data, sample_size, population_size, 
                                       weight_var, bmi_var, num_years, 
                                       criteria_1, criteria_1_value) {
  # browser()
  
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
    # browser()
    # Check if the desired sample size is greater than the eligible population
    if (sample_size > eligible_population) {
      
      # browser()
      print(paste0("The desired sample size is greater than the population with BMI above the threshold. In year ",year ))
      
      sample_size = eligible_population - 10000
      
      # stop(paste0("The desired sample size is greater than the population with BMI above the threshold. In year",year ))
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
      data[data[[intervention_col]] == "Yes", weight_loss_cols[year]] = -weight_loss_percent*1* data[data[[intervention_col]] == "Yes", bodyweight_var]
      
      data[data[[intervention_col]] == "Yes", weight_loss_cols[year+1]] = -weight_loss_percent*0* data[data[[intervention_col]] == "Yes", bodyweight_var]
      
      #, weight_loss_cols[year+1] 
      
    } else{
      
      data[data[[intervention_col]] == "Yes", weight_loss_cols[year]] = -weight_loss_percent*1* data[data[[intervention_col]] == "Yes", bodyweight_var]
      
      
    }
    # weight regain to be assigned only from the year after intervention, so in this case, it would be two years of weight loss and weight regain in third year
    if (year > 2) {
      prev_intervention_cols <- paste0("intervention_year", 1:(year - 2))
      prev_intervention <- apply(data[, prev_intervention_cols] == "Yes", 1, any)
      
      # Calculate weight regain based on weight loss in one of the two intervention years and calulated as 67% of two times weight loss in any one of the years
      # two times weight loss in any one year is the total weight loss over two years.
      weight_regain <- 0.67 *1* pmin(
        data[prev_intervention, weight_loss_cols[year - 1], drop = TRUE],
        data[prev_intervention, weight_loss_cols[year - 2], drop = TRUE]
      )
      
      data[prev_intervention, weight_regain_cols[year]] <- -weight_regain
    }
  } 
  
  return(data)
}



########

# Only Semaglutide

#############



process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab", nation = "England", population_group = "Adult")

df = read_csv(here("inputs/processed/hse_2019.csv"))



df = df %>%
  mutate(eligibility = case_when(bmi >= 30 ~ 1,
                                 TRUE ~ 0))


test_df = df


set.seed(245)
# sample_size = 204918

df_1 = select_intervention_sample(data = test_df,
                                  sample_size = 3000000,
                                  population_size = 44263393,
                                  weight_var = "wt_int",
                                  bmi_var = "bmi",
                                  num_years = 5,
                                  criteria_1 = "eligibility",
                                  criteria_1_value = 1)


post_df_adult = assign_weight_changes(data = df_1, bodyweight_var = "weight", num_years = 5, weight_loss_percent = 0.1583, weight_regain = 0.67)

# function to assign weight loss and weight regain:


#post_df_adult = test_df_1


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


write_xlsx(path = "outputs/policy_24b_updated/policy_24b_updated_adult_england.xlsx", x = bmi_change_year)



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

ggsave(here("outputs/policy_24b_updated/policy_24b_updated_impact_England_adult.png"), 
       plot = adult_bar_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')


write.csv(post_df_adult, file = "outputs/policy_24b_updated/policy_24b_updated_adult_england_bmi.csv")
