
################################################################################################### 
# Policy 17: Extend the Healthy Start Scheme to households in the most deprived local authorities #
#            by providing £85 million of funding per year for five years                          #
#                                                                                                 #
###################################################################################################

# Scope: England

# Description:


# The evidence comes from the results of the rapid review available here
# - https://docs.google.com/document/d/1t6SPV7rYB6YzDufRr24TIxrxPkOt5_2V6bRRiu0GxV8/edit
# The effect on BMI is found to be - 0.04kg/m2 but the effect size is not significant.
# Hence, for the modelling, we assume change in BMI = 0

# Information available on the healthy start scheme: 

# Eligibility:
# 1. Pregnancy and childcare criteria:
#     1.1. Pregnant female (assuming 30 weeks of pregnancy): £4.25 × 30 = £127.50
#     1.2. Children from birth to 1 year old: £8.50 × 52 = £442
#     1.3. Children between 1 and 4 years old (per year): £4.25 × 52 = £221
#
# 2. On Benefit support:
#     2.1. Female on job seekers allowance, income support, pension credit, chold tax credit or universal income

# Individuals must meet one of (1) and (2) to be eligible to receive healthy start vouchers

# Note: Impact will be modelled only for those living with excess weight (BMI >= 25 kg/m2)

# Assumptions & Modelling approach:
# [1] The HSE 2019 dataset is not representative of pregnant people in the England and as per the dataset the 
#     number of pregnant people in the dataset (~60,000 females) is way below the the number of maternity's  in
#     England (~ 500,000 per year). Therefore, to estimate the number of eligible people for treatment, we check,
#     if the individual female is on any income support and if there is a child in the household. If both these
#     conditions are satisfied, the individual is eligible to receive the voucher.
# [2] All those assigned to intervention in year 1 are assumed to be pregnant, as they receive the voucher for all
#     five years as per the Healthy Start Scheme


# Number of people that can be reached by the policy per year:
# Budget allocated to policy = £85 million

# Amounts Received as part of Healthy Start Vouchers per year:
#     1.1. Pregnant female (assuming 30 weeks of pregnancy): £4.25 × 30 = £127.50
#     1.2. Children from birth to 1 year old: £8.50 × 52 = £442
#     1.3. Children between 1 and 4 years old (per year): £4.25 × 52 = £221
# Therefore, average amount per female per year = [(127.5 x 1) + (442 x 1) + (221 x 3) /5] = £246.5

# Number of people reached by policy per year = £85 mil/ £246.5 = 344,828 females per year



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
    
    subset_data <- data[data[[criteria_1]] == criteria_1_value  , ] # & !intervention_history
    
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
    selected_indices_original <- which(data[[criteria_1]] == criteria_1_value )[selected_individuals] #& !intervention_history
    
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





assign_weight_changes_1 <- function(data, bodyweight_var, num_years, bmi_loss, bmi_regain) {
  # browser()
  as.data.frame(data)
  bmi_loss_cols <- paste0("bmi_loss_y", 1:num_years)
  bmi_regain_cols <- paste0("bmi_regain_y", 1:num_years)
  data[, c(bmi_loss_cols, bmi_regain_cols)] <- 0
  
  # Create columns to store BMI values for each year
  bmi_y_cols <- paste0("bmi_y", 1:num_years)
  data[, bmi_y_cols] <- NA
  
  for (year in 1:num_years) {
    intervention_col <- paste0("intervention_year", year)
    
    if (year == 1) {
      # Add a check for BMI >= 25 before assigning bmi_loss
      data[data[[intervention_col]] == "Yes" & data[, paste0("bmi_y", year - 1)] >= 25, bmi_loss_cols[year]] <- -bmi_loss
      data[data[[intervention_col]] == "Yes" & data[, paste0("bmi_y", year - 1)] >= 25, bmi_loss_cols[year + 1]] <- -bmi_loss
      data[data[[intervention_col]] == "Yes" & data[, paste0("bmi_y", year - 1)] >= 25, bmi_loss_cols[year + 2]] <- -bmi_loss
      data[data[[intervention_col]] == "Yes" & data[, paste0("bmi_y", year - 1)] >= 25, bmi_loss_cols[year + 3]] <- -bmi_loss
      data[data[[intervention_col]] == "Yes" & data[, paste0("bmi_y", year - 1)] >= 25, bmi_loss_cols[year + 4]] <- -bmi_loss
      
    } else { if (year == 2){
      
      data[data[[intervention_col]] == "Yes" & data[, paste0("bmi_y", year - 1)] >= 25, bmi_loss_cols[year]] <- -bmi_loss
      data[data[[intervention_col]] == "Yes" & data[, paste0("bmi_y", year - 1)] >= 25, bmi_loss_cols[year + 1]] <- -bmi_loss
      data[data[[intervention_col]] == "Yes" & data[, paste0("bmi_y", year - 1)] >= 25, bmi_loss_cols[year + 2]] <- -bmi_loss
      data[data[[intervention_col]] == "Yes" & data[, paste0("bmi_y", year - 1)] >= 25, bmi_loss_cols[year + 3]] <- -bmi_loss
      
      
    } else { if (year == 3){
      
      data[data[[intervention_col]] == "Yes" & data[, paste0("bmi_y", year - 1)] >= 25, bmi_loss_cols[year]] <- -bmi_loss
      data[data[[intervention_col]] == "Yes" & data[, paste0("bmi_y", year - 1)] >= 25, bmi_loss_cols[year + 1]] <- -bmi_loss
      data[data[[intervention_col]] == "Yes" & data[, paste0("bmi_y", year - 1)] >= 25, bmi_loss_cols[year + 2]] <- -bmi_loss
      
    } else { if (year == 4){
      
      
      data[data[[intervention_col]] == "Yes" & data[, paste0("bmi_y", year - 1)] >= 25, bmi_loss_cols[year]] <- -bmi_loss
      data[data[[intervention_col]] == "Yes" & data[, paste0("bmi_y", year - 1)] >= 25, bmi_loss_cols[year + 1]] <- -bmi_loss
      
    } else { if (year == 5){
      
      data[data[[intervention_col]] == "Yes" & data[, paste0("bmi_y", year - 1)] >= 25, bmi_loss_cols[year]] <- -bmi_loss
      
    } else {
      
      print("please check number of years are 5 or below")
      
      
    }
      
      }
      
      }

    }
      
     # print("entered the else option, check number if years are correct, check variable - `num_years` " )
    }
    
    if (year > 1) {
      prev_intervention_cols <- paste0("intervention_year", 1:(year - 1))
      prev_intervention <- apply(as.data.frame(data[, prev_intervention_cols] == "Yes"), 1, any)
      
      # Add a check for weight loss in previous year before assigning bmi_regain
      data[prev_intervention & data[, paste0("bmi_loss_y", year - 1)] > 0, bmi_regain_cols[year]] <- -bmi_regain
    }
    
    # Calculate BMI for each year
    if (year == 1) {
      data[, bmi_y_cols[year]] <- data$bmi + data[, bmi_loss_cols[year]] + data[, bmi_regain_cols[year]]
    } else {
      data[, bmi_y_cols[year]] <- data[, bmi_y_cols[year - 1]] + data[, bmi_loss_cols[year]] + data[, bmi_regain_cols[year]]
    }
  } 
  
  return(data)
}





table_outputs = list() # creating a list of table outputs to be saved as an excel file



# Estimating the impact of the policy in:


# 1. Adults in England

# 1.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab", nation = "England", population_group = "Adult")

df = read_csv(here("inputs/processed/hse_2019.csv"))



# 1.2. Selecting eligible individuals:

# Eligibility:
# 1. Must have a child
# 2. Sex = Female
# 3. Must be on income support

df = df %>%
  mutate(eligibility = case_when(children_updated == 1 & income_support_status == 1 & sex == 2 ~ 1,
                                 TRUE ~ 0))


# using this line to ensure reproducibility
set.seed(171)


# Amounts Received as part of Healthy Start Vouchers per year:
#     1.1. Pregnant female (assuming 30 weeks of pregnancy): £4.25 × 30 = £127.50
#     1.2. Children from birth to 1 year old: £8.50 × 52 = £442
#     1.3. Children between 1 and 4 years old (per year): £4.25 × 52 = £221
# Therefore, average amount per female per year = [(127.5 x 1) + (442 x 1) + (221 x 3) /5] = £246.5

# Number of people reached by policy per year = £85 mil/ £246.5 = 344,828 females per year


df = select_intervention_sample(data = df,
                                sample_size = 344828,
                                population_size = 44263393,
                                weight_var = "wt_int",
                                bmi_var = "bmi",
                                num_years = 5,
                                criteria_1 = "eligibility",
                                criteria_1_value = 1)



intervention_df = df %>%
  mutate(bmi_y0 = bmi)


# Assigning weight loss to those receiving the treatment and a weight regain in subsequent years


# assigning effect sizes to individuals: the effect is applied only if the individual
# is living with excess weight

effect_size = 0

post_df_adult = assign_weight_changes_1(data = intervention_df,
                                        bodyweight_var = "weight",
                                        num_years = 5,
                                        bmi_loss = effect_size,
                                        bmi_regain = 0)



# 1.3. Estimating the impact of the intervention on prevalence of obesity:

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

table_outputs[["england_adult"]] = bmi_change_year



# Plot of year on year BMI category distribution
adult_bar_plot = bmi_change %>%
  ggplot(., aes(y = freq, x = BMI, fill = type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_ipsum() +
  labs(fill = "", 
       title = "BMI Categories Distribution", 
       y = "Frequency",
       subtitle = "Population | England") +
  theme_ipsum(base_size = 8, axis_title_size = 8) + #, base_family="Averta"
  theme(legend.position = "top")

adult_bar_plot

ggsave(here("outputs/policy_17/policy_17_impact_England_adult.png"), 
       plot = adult_bar_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')

write.csv(post_df_adult, file = "outputs/policy_17/policy_17_adult_england_bmi.csv")
write_xlsx(path = "outputs/policy_17/policy_17_england.xlsx", x = table_outputs)





