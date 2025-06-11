
################################################################################################### 
# Policy 37 : Option 6: Behaviourial weight management programme                                  #
#                                                                                                 #
###################################################################################################


# Scope: England

# The evidence comes from the results of the rapid review available here
# - https://docs.google.com/document/d/1K20dg2D-G9J6F439gegPRD8Mly58xSnJmGXJMto_GYY/edit?usp=sharing

# The evidence comes from the results of the rapid review available here
# - https://docs.google.com/document/d/1hozT3EvH5fbl1W9CeIcrrr7TGpotYu07/edit?usp=sharing&ouid=102713518635256687243&rtpof=true&sd=true

# 
# Eligibility:
#   Adults Age: ≥ 18; BMI Group: ≥ 30
# 
# Weight loss = 5.1 kg after the programme (c)
#   Effect size: 5.1 kgs at the end of 1 year after programme  Supporting Information, Figure 9, section 6.1.2 of Hartmann-Boyce 2014 [https://pmc.ncbi.nlm.nih.gov/articles/PMC4233997/pdf/obr0015-0920.pdf]
#     We take a weighted average of the effect sizes from different papers under Group-based Commercial, please see calculation of this below
#
# Weight regain = 0.46 kg per year (b)
#   To calculate the weight regain we subtract the weight regain after the programme from the weight
#   regain at the end of 5 years and divide it by 5 to get the mean weight regain per year.
#
#
# Number of people treated:
#   We use the allocated annual budget and per person cost of delivering the programme to estimate the number of people who will receive treatment each year
#     Number of people treated = ~1.2 million people per year
#     Allocated budget / Unit cost per year = £85 mil / £70 = 1,214,285
#
#   Allocated budget = £85 million per year from the policy specification
# 
#   Cost of delivering the programme: £70 per person
#     Unit cost of treatment in 2015 = £51.45 per person [Source]
#     Adjusting for inflation = Original price x (CPI in 2015 / CPI in base year)
#         51.45 x (135.4/100) = £70 per person per year
#   However, it is unlikely that everyone offered the programme will enroll. We use stats from OHID to estimate the number of people who enroll#
#   and the share of people who experience weight loss
#   Number of people enrolling into the programme = 65% of those offered
#   Number of people experiencing weight loss = 43% of those who enrolled the programme
#   Therefore, in our context, the number of people enrolled after and offer = 65% x 1,214,285 = 789,285
#   And number of people experiencing weight loss of those who enrolled = 43% x 789,285 = 339,393




# References:
# (a) ONS 2019 Mid-Year Population Estimates - https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/analysisofpopulationestimatestool)
# (b) Hartmann-Boyce J, Cobiac LJ, Theodoulou A, et al. Weight regain after behavioural weight management
#     programmes and its impact on quality of life and cost effectiveness: Evidence synthesis and health
#     economic analyses. Diabetes Obes Metab. 2023;25(2):526‐535. doi:10.1111/dom.14895
# (c) Hartmann-Boyce J, Johns DJ, Jebb SA, Summerbell C, Aveyard P; Behavioural Weight Management Review 
#     Group. Behavioural weight management programmes for adults assessed by trials conducted in everyday
#     contexts: systematic review and meta-analysis. Obes Rev. 2014 Nov;15(11):920-32. doi: 10.1111/obr.12220.
#     Epub 2014 Aug 11. PMID: 25112559; PMCID: PMC4233997.
# (d) OHID (2023) Adult tier 2 weight management services: Short statistical commentary September 2023, GOV.UK. Available at:
#     https://www.gov.uk/government/statistics/adult-tier-2-weight-management-services-final-data-for-april-2021-to-december-2022/adult-tier-2-weight-management-services-short-statistical-commentary-september-2023 (Accessed: 11 June 2025).




# setup
rm(list = ls())
library(tidyverse)
library(here)
library(writexl)


source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")
source(file = "config/config.R")
source(file = "post_processing/post_processing.R")


table_outputs = list() # creating a list of table outputs to be saved as an excel file


# calculating absolute weight loss:
# We create a data frame to show the results from (c). This table is created from Page  Pg 927, Figure 1, section 6.1.2 in (c) Hartmann-Boyce et al. (2014)
results_table <- data.frame(
  study = c("Heshka 2003", "Jebb 2011", "Jolly 2011", "Jolly 2011", "Jolly 2011"),
  mean = c(-4.9, -6.65, -3.3, -3.1, -4.4),
  sample = c(176, 230, 68, 62, 78)
)


results_table = results_table %>%
  mutate(mean_x_sample = mean * sample)

weighted_weight_loss = weighted.mean(results_table$mean, results_table$sample)

# calculating absolute weight regain:
# From Hartmann-Boyce (2023), we get the weight loss after the programme and the weight loss at the end of 5 years:
# (Please see section 3.4 in the paper)
# weight loss in intervention group at the end of the programme: -4.9 kg
weight_loss_treatment_group_programme_end = -4.9

# weight loss in intervention group at the end of 5 years: -2.6 kg
weight_loss_treatment_group_five_years = -2.6

# we then calculate weight regain per year = (weight loss at five years - weight loss at treatment end)/5
weight_regain_per_year = (weight_loss_treatment_group_five_years - weight_loss_treatment_group_programme_end)/ 5


# Number of people offered the programme:
budget_allocation = 85000000
unit_cost_bwmps = 70

number_of_people_offered = budget_allocation / unit_cost_bwmps


# calculating the number of people experiencing weight loss:
share_enrolled = 0.65
share_experiencing_weight_loss = 0.43

number_enrolled_experiencing_weight_loss = share_enrolled * share_experiencing_weight_loss * number_of_people_offered


# Constants

NUMBER_OF_PEOPLE_EXPERINCING_WEIGHT_LOSS = number_enrolled_experiencing_weight_loss
ENGLAND_ADULT_POPULATION = 44263393  # (a)
WEIGHT_LOSS_ON_TREATMENT = abs(weighted_weight_loss)
WEIGHT_REGAIN_POST_TREATMENT = abs(weight_regain_per_year)



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



# FUNCTION: Assign Weight Changes
#
# This function assigns weight loss and weight regain values to individuals
# based on their intervention status across multiple years. Individuals who
# receive intervention in a given year experience weight loss, while those
# who received intervention in previous years experience weight regain.

#' @param data A data frame containing individual-level data with intervention
#   status columns (intervention_year1, intervention_year2, etc.)
#' @param bodyweight_var Character string specifying the name of the body weight
#'   variable in the data frame (currently not used in function)
#' @param num_years Integer specifying the number of years in the simulation
#' @param weight_loss_1 Numeric value representing the amount of weight loss
#'   (in kg) for individuals receiving intervention. This value will be applied
#'    as negative weight change
#' @param weight_loss_2 Numeric value for alternative weight loss amount (currently
#'  not implemented in the function)
#' @param weight_regain Numeric value representing the amount of weight regain (in kg)
#'  for individuals who received intervention in previous years

#' @return A data frame with the original data plus additional columns of weight loss and weight regain

assign_weight_changes <- function(data, bodyweight_var, num_years, weight_loss_1, weight_loss_2, weight_regain) {
  # browser()
  # Create weight loss and weight regain columns for each year
  weight_loss_cols <- paste0("weight_loss_y", 1:num_years)
  weight_regain_cols <- paste0("weight_regain_y", 1:num_years)
  data[, c(weight_loss_cols, weight_regain_cols)] <- 0
  
  for (year in 1:num_years) {
    intervention_col <- paste0("intervention_year", year)
    
    if(year < num_years + 1){
      
      # Assign weight loss for individuals who received the intervention in the current year
      data[data[[intervention_col]] == "Yes", weight_loss_cols[year]] = -weight_loss_1 

    } else{
      
      
    }

    # weight regain to be assigned only from the year after intervention, so in this case, it would be weight loss in year 1,
    # followed by weight regain in every year until end of simulation.
    if (year > 1) {

      # choosing the intervention 
      prev_intervention_cols <- paste0("intervention_year", 1:(year - 1))
      
      prev_intervention <- apply(data[, prev_intervention_cols] == "Yes", 1, any)
      
      data[prev_intervention, weight_regain_cols[year]] <- weight_regain
      
    }
    
  } 
  
  return(data)
}



# Estimating the impact of the policy for adults in England:

# reading in HSE 2019 data and preparing it for implementing the Hall Model

# Cleaning the input/ baseline data:
process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab", nation = "England", population_group = "Adult")

# reading in the cleaned processed baseline data file:
df = read_csv(here("inputs/processed/hse_2019.csv"))

# Applying eligibility crietria. All those with a BMI over 30 are eligible to receive treatment
df = df %>%
  mutate(eligibility = case_when(bmi >= 30 ~ 1,
                                 TRUE ~ 0))

set.seed(370)

# Selecting the intervention sample each year who will receive the intervention:
df = select_intervention_sample(data = df,
                                sample_size = NUMBER_OF_PEOPLE_EXPERINCING_WEIGHT_LOSS,
                                population_size = ENGLAND_ADULT_POPULATION,
                                weight_var = "wt_int",
                                bmi_var = "bmi",
                                num_years = MODEL_CONSTANTS$MODEL_DURATION,
                                criteria_1 = "eligibility",
                                criteria_1_value = 1)


# Assign weight changes to individuals who were selected in the previous step
# We apply a weight loss of 3.5 kgs for those receiving the treatment and an annual weight regain of 0.46 kg
post_df_adult = assign_weight_changes(data = df,
                                      bodyweight_var = "weight",
                                      num_years = 5,
                                      weight_loss_1 = WEIGHT_LOSS_ON_TREATMENT,
                                      weight_loss_2 = 0,
                                      weight_regain = WEIGHT_REGAIN_POST_TREATMENT)


# Calculating the new body weight, BMI and BMI Class for each individual:
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

# creating a table of year wise distribution of BMI categories
bmi_change = bmi_change %>%
  mutate(BMI = factor(BMI, levels = c("underweight", "normal", "overweight", "obese", "morbidly obese"))) %>%
  as.data.frame()


bmi_change_year = bmi_change %>%
  select(-c(n)) %>%
  pivot_wider(., names_from = BMI, values_from = freq) %>%
  select(type, underweight, normal, overweight, obese, `morbidly obese`)

bmi_change_year

table_outputs[["england_adult"]] = bmi_change_year



# Outputs:

# Output 1: Table of year wise prevalence of obesity
bmi_change_year

# extracting the reduction in obesity prevalence 
annual_obesity_prevalence_england = extract_relative_change(data = bmi_change_year)

# Relative reduction in obesity prevalence in England = 3.2%

# Adding to table outputs:
table_outputs[["annual_obesity_prevalence_eng"]] = annual_obesity_prevalence_england

# Estimating the annual value to government (benefit):
annual_benefit_to_gov = extract_pound_benefit(data = annual_obesity_prevalence_england, 
                                              cost = MODEL_CONSTANTS$COST_OF_OBESITY_IN_BILLIONS,
                                              duration = MODEL_CONSTANTS$MODEL_DURATION)

# Average annual value to government compared to baseline = £1.58 billions

# Adding to table outputs:
table_outputs[["annual_benefit_to_gov"]] = annual_benefit_to_gov


# Output 2: Plot of BMI distribution(bar charts)
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

ggsave(here("outputs/new_policies/policy_37/policy_37_impact_England_adult.png"), 
       plot = adult_bar_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')


# Outputs 3: summary results and detailed individual table:
# bmi year on year prevalence:
write_xlsx(path = "outputs/new_policies/policy_37/policy_37.xlsx", x = table_outputs)
write.csv(post_df_adult, file = "outputs/new_policies/policy_37/policy_37_adult_england_bmi_1.csv")
