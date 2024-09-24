
################################################################################################### 
# Policy 24 : Extend access to pharmacological interventions by providing an extra £500 million   #
#             of ring-fenced funding  to provide ~130,000 people with GLP-1s                      #
#                                                                                                 #
###################################################################################################

# Description: without Orlistat, 100% of weight loss occurs in the first year on the drug, weight gain in third year then plateaus

# Scope: Scotland

# The evidence comes from the results of the rapid review available here
# - https://docs.google.com/document/d/1hozT3EvH5fbl1W9CeIcrrr7TGpotYu07/edit?usp=sharing&ouid=102713518635256687243&rtpof=true&sd=true
# Those receiving any of the three drugs on average experience weight loss of 8.46% of their bodyweight across three
# drugs - Semaglutide, Liraglutide and Orlistat.
# The cost of the three drugs per month are - Semaglutide: £130, Liraglutide: £150
# Average of cost across the three drugs = £140 per month
# Assuming that individuals are on the drugs for a period of 2 years: cost per person for 2 years = £3,360
# Number of people the funding can reach = £50 million/£2,440 = 20,491 individuals
# Population estimate for adults equal and over 18 years of age = 4,434,138 (a)
# A weight loss of 11.1%.
# 100% of the weight loss assumed to be in first year, weight gain will occur in year 3 when the individual goes off the drug
# Weight regain is two-thirds of the weight loss. (b)

# There is overlapping eligibility criteria for the three drugs - Semaglutide, Liraglutide 
# which is summarised below to create a hybrid eligibility criteria that individuals must meet:
# (bmi >= 30) | (bmi >= 27.5 + ethnicity = Black (2), Asian (3))

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
source(file = "models/adult_model_calorie.R")

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
  # browser()
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
    # browser()
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







process_clean_save(file_path = "inputs/raw/shes19i_eul.tab", nation = "Scotland", population_group = "Adult")

df = read_csv(here("inputs/processed/shes_2019.csv"))


df = df %>%
  mutate(eligibility = case_when(bmi >= 30 ~ 1,
                                 # (bmi >= 30 & bmi < 35) & (cardiovd == 1 | diabetes == 1) ~ 1,
                                 (bmi >= 27.5 & ethnicity %in% c(4, 5)) ~ 1,    # , 4, 5
                                 TRUE ~ 0))


set.seed(2491)

df = select_intervention_sample(data = df,
                                sample_size = 20491, # 204918, 148810
                                population_size = 4434138,
                                weight_var = "wt_int",
                                bmi_var = "bmi",
                                num_years = 5,
                                criteria_1 = "eligibility",
                                criteria_1_value = 1)



post_df_adult = assign_weight_changes(data = df, bodyweight_var = "weight", num_years = 5, weight_loss_percent = 0.111, weight_regain = 0.67)

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

# bmi year on year prevalence:
write.csv(bmi_change_year, file = "outputs/policy_24_3/policy_24_3_updated_adult_scotland.csv")


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

ggsave(here("outputs/policy_24_3/policy_24_3_updated_impact_Scotland_adult.png"), 
       plot = adult_bar_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')


write.csv(post_df_adult, file = "outputs/policy_24_3/policy_24_3_updated_adult_scotland_bmi.csv")



test_df = post_df_adult %>%
  select(wt_int, bmi_class, intervention_year1, intervention_year2, intervention_year3, intervention_year4, intervention_year5,
         bmi_1_class, bmi_2_class, bmi_3_class, bmi_4_class, bmi_5_class)
