
##############################################################################################
# Policy 19: Invest £500 million over 5 years in Local Authorities to plan and deliver       #
#            active transport                                                                #
#                                                                                            #
##############################################################################################


# setup
rm(list = ls())
gc()
library(tidyverse)
library(here)
library(aws.s3)

source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")
# source(file = "models/child_model_calorie.R")
#source(file = "models/child_model_calorie_henry.R")

table_outputs = list() # creating a list of table outputs to be saved as an excel file


# required functions:
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



# Estimating the impact of the policy in:

# 1. Adults in England:

# 1.1. Cleaning the input/ baseline data:

# process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab", nation = "England", population_group = "Adult")

df = s3read_using(FUN = read.csv,
                             bucket = "ahl-private-data",
                             object = "hse/processed/hse_2019_adult.csv")

# df = read_csv(here("inputs/processed/hse_2019.csv"))

# Selecting individuals to recive intervention

set.seed(10001)


adults_england = 45470282

# 
# df_selected = select_intervention_sample(data = df,
#                                          bmi_threshold = 25, # interrested in impact of policy on those living with excess weight
#                                          required_proportion = proportion_to_select, # 0.016, # 0.5, # assuming that 50% of those living with excess weight increase their physical activity
#                                          weight_var = "wt_int",
#                                          bmi_var = "bmi",
#                                          num_years = 1)




# df = df %>%
#   mutate(eligibility = case_when(bmi >= 25 ~ 1,
#                                  TRUE ~ 0))

df = df %>%
  mutate(eligibility = 1)


df_1 = select_intervention_sample(data = df,
                                  sample_size = 9033942,
                                  population_size = adults_england,
                                  weight_var = "wt_int",
                                  bmi_var = "bmi",
                                  num_years = 1,
                                  criteria_1 = "eligibility",
                                  criteria_1_value = 1)

# 1.2. Estimating the impact of the intervention on prevalence of obesity:


# We are interested in impacts over five years.
intake_change_ov = -1.4
intake_change_ob = -1.4
intake_change_mob = -1.4

implmentation_duration = 365 * 5 # five years


df_selected = df_1 %>%  # processed dataset with variables such as height, weight, bmi, daily energy intake passed as input to the function
  mutate(sex = ifelse(sex == 1, "male", "female")) %>%  # modifying the variable sex in a form expected by the Hall Model
  mutate(intake_diff = ifelse(bmi_class %in% c("underweight", "normal"), 0, 0)) %>% # creating a variable to store the change in energy intake due to intervention and applying it only to those with BMI >= 25
  mutate(intake_diff = case_when(bmi_class == "overweight" & intervention_year1 == "Yes" ~ intake_change_ov,
                                 bmi_class == "obese" & intervention_year1 == "Yes" ~ intake_change_ob,
                                 bmi_class == "morbidly obese" & intervention_year1 == "Yes" ~ intake_change_mob,
                                 TRUE ~ intake_diff)) 
#%>%
#  mutate(intervention = ifelse(bmi_class %in% c("underweight", "normal"), "No", "Yes"))




# For each individual/ observation in the HSE, we apply the energy expenditure as a result of the activity,
# (converting it into type numeric), then creating a vector, which essentially repeats the intake diff
# value 365*5 times (i.e. for a five year period), which is then transposed to match the input requirements
# of the Hall Model.
ee_change <- t(apply(df_selected, 1, function(x) rep(as.numeric(x["intake_diff"]), implmentation_duration)))


# A matrix of change in salt consumption set to zero is another input to the model. This is set to zero as
# information on change in salt consumption is not available from our rapid reviews
nachange <- t(apply(df_selected, 1, function(x) rep(0, implmentation_duration)))


# the bw package has a function called [adult_weight] that takes the following inputs:
# baseline body weight, height (in meters), age, sex and energy expenditure (for a five year period)
model_weight <- adult_weight(bw = df_selected$weight,
                             ht = df_selected$height/100,
                             age = df_selected$age,
                             sex = df_selected$sex,
                             EIchange = ee_change,
                             NAchange = nachange,
                             days = implmentation_duration)


# Extracting BMI values from the model and joining them to the HSE dataset for further analysis 
# and output generation. 'bmi_model' is a matrix of day wise change in BMI of the population as a result of
# the intervention. 
bmi_model = model_weight[["Body_Mass_Index"]]
post_df = cbind(df_selected, bmi_model)


# Creating a new dataframe with variables of interest and BMI values at the end of each of the five years
# of the intervention. Subsequently, categorising observations into BMI categories for each year.
post_df_adult = post_df %>%
  select("id", "weight", "height", "age", "sex", "bmi", "wt_int", "psu", "strata", "pal",
         "rmr", "bmi_class", "intake", "intervention_year1", "intake_diff", "1", "365", "730", "1095", "1460", "1825" ) %>%
  rename(bmi_5 = "1825", bmi_0 = "1", bmi_1 = "365", bmi_2 = "730", bmi_3 = "1095", bmi_4 = "1460" ) %>%
  mutate(bmi_0_class = case_when(bmi_0 <= 18.5 ~ "underweight",
                                 bmi_0 > 18.5 & bmi_0 < 25 ~ "normal",
                                 bmi_0 >= 25 & bmi_0 < 30 ~ "overweight",
                                 bmi_0 >= 30 & bmi_0 < 40 ~ "obese",
                                 bmi_0 >= 40 ~ "morbidly obese",
                                 TRUE ~ "NA"),
         bmi_1_class = case_when(bmi_1 <= 18.5 ~ "underweight",
                                 bmi_1 > 18.5 & bmi_1 < 25 ~ "normal",
                                 bmi_1 >= 25 & bmi_1 < 30 ~ "overweight",
                                 bmi_1 >= 30 & bmi_1 < 40 ~ "obese",
                                 bmi_1 >= 40 ~ "morbidly obese",
                                 TRUE ~ "NA"),
         bmi_2_class = case_when(bmi_2 <= 18.5 ~ "underweight",
                                 bmi_2 > 18.5 & bmi_2 < 25 ~ "normal",
                                 bmi_2 >= 25 & bmi_2 < 30 ~ "overweight",
                                 bmi_2 >= 30 & bmi_2 < 40 ~ "obese",
                                 bmi_2 >= 40 ~ "morbidly obese",
                                 TRUE ~ "NA"),
         bmi_3_class = case_when(bmi_3 <= 18.5 ~ "underweight",
                                 bmi_3 > 18.5 & bmi_3 < 25 ~ "normal",
                                 bmi_3 >= 25 & bmi_3 < 30 ~ "overweight",
                                 bmi_3 >= 30 & bmi_3 < 40 ~ "obese",
                                 bmi_3 >= 40 ~ "morbidly obese",
                                 TRUE ~ "NA"),
         bmi_4_class = case_when(bmi_4 <= 18.5 ~ "underweight",
                                 bmi_4 > 18.5 & bmi_4 < 25 ~ "normal",
                                 bmi_4 >= 25 & bmi_4 < 30 ~ "overweight",
                                 bmi_4 >= 30 & bmi_4 < 40 ~ "obese",
                                 bmi_4 >= 40 ~ "morbidly obese",
                                 TRUE ~ "NA"),
         bmi_5_class = case_when(bmi_5 <= 18.5 ~ "underweight",
                                 bmi_5 > 18.5 & bmi_5 < 25 ~ "normal",
                                 bmi_5 >= 25 & bmi_5 < 30 ~ "overweight",
                                 bmi_5 >= 30 & bmi_5 < 40 ~ "obese",
                                 bmi_5 >= 40 ~ "morbidly obese",
                                 TRUE ~ "NA"))

# survey design specification to estimate population level effects of the intervention. Survey design spec is
# created using weightings from HSE 2019.
design <-  svydesign(ids=~post_df_adult$psu, 
                     nest = T,
                     data=post_df_adult,
                     weights=post_df_adult$wt_int)



# creating a table of year wise distribution of BMI categories
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



# Output 1: Plot of year on year BMI category distribution
adult_bar_plot = bmi_change %>%
  ggplot(., aes(y = freq, x = BMI, fill = type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_ipsum() +
  labs(fill = "", 
       title = "BMI Categories Distribution", 
       y = "Frequency",
       subtitle = "England - Policy 19") +
  theme_ipsum(base_size = 8, axis_title_size = 8) + #, base_family="Averta"
  theme(legend.position = "top")

adult_bar_plot

ggsave(here("outputs/policy_19/policy_19_impact_England_adult.png"), 
       plot = adult_bar_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')




# Output 2: Table of year wise prevalence of obesity

bmi_change_year = bmi_change %>%
  select(-c(n)) %>%
  pivot_wider(., names_from = BMI, values_from = freq) %>%
  select(type, underweight, normal, overweight, obese, `morbidly obese`)

bmi_year = bmi_change_year


table_outputs[["england_adult"]] = bmi_change_year

# bmi year on year prevalence:

write_xlsx(path = "outputs/policy_19/policy_19_updated_england.xlsx", x = table_outputs)

# Output 3: Cost Modelling input files:
write.csv(post_df_adult, file = "outputs/policy_19/policy_19_updated_adult_england_bmi.csv")



