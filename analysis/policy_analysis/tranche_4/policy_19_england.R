
##############################################################################################
# Policy 19: Invest £500 million over 5 years in Local Authorities to plan and deliver       #
#            active transport                                                                #
#                                                                                            #
##############################################################################################

# Scope: England

# Description:

# The evidence from this cost benefit analysis (https://www.glasgow.gov.uk/CHttpHandler.ashx?id=55780&p=0)
# showed that the active transport interventions would lead to 50,000 additional trips across the population.
# The evaluation suggests that these additional trips would be 20% cycling and 80% walking trips.

# Assumptions:
# (1) Time spent on each trip:
#     Each additional cycling trip = 23 minutes of cycling [4]
#     Each additional walking trip = 17 minutes of walking [4]
# (2) 50% of those living with excess weight increase physical activity, make additional trips.
#     No reference available as searches didn't return any evidence on participation in active travel by BMI groups.
#     (Note: Can be changed, if value is too high)
# (3) Individuals don’t compensate EE with EI [7]

#
# Based on this physical activity we want to estimate the energy expenditure which is calculated as:

# Energy Expenditure = [(Metabolic Equivalent x 3.5 x body weight kg)/200] x time spent doing activity [1][2][3]

# As per the formula, we need to calculate/ estimate:

# (A) Metabolic Equivalent of Cycling & Walking
# Metabolic equivalent for walking = 3 [1][2]
# Metabolic equivalent for cycling = 6 [1][2]
# mean metabolic equivalent = 4.5 (equivalent of a moderate intensity activity)

# (B) time spent doing activity (per adult in minutes):
# Total time spent on activity = (time spent walking x walking trips) + (time spent cycling x cycling trips)
# = 10,000 trips x 23 minutes + 40,000 trips x 17 minutes
# Total time spent on trips = 910,000 active minutes

# Adult population of Glasgow = 521,522 [5]
# time spent doing activity (per adult in minutes) = 910,000/ 521,522 = 1.74 minutes (in Glasgow)

# Cost of active travel infrastructure in Glasgow = £475 million
# Increase in total time spent doing activity as a result of spending £500 million = (£500 x 910,000)/ £475 = 957,894.736 active minutes

# Adult population for England = 44,263,393 [6]
# Additional minutes spent on active transport = 957,894.736/44,263,393 = 0.022 minutes



# References:
# [1] Harvard T.H. Chan School of Public Health (2019). Staying Active. [online] The Nutrition Source. 
#     Available at: https://www.hsph.harvard.edu/nutritionsource/staying-active/.
# [2] Jetté M, Sidney K, Blümchen G. Metabolic equivalents (METS) in exercise testing, exercise prescription, and
#     evaluation of functional capacity. Clin Cardiol. 1990;13(8):555-565. doi:10.1002/clc.4960130809
# [3] Healthline (2019). What Are METs, and How Are They Calculated? [online] Healthline. 
#     Available at: https://www.healthline.com/health/what-are-mets#calorie-connection.
# [4] Department for Transport (2020). Statistical Release. [online] 
#     Available at: https://assets.publishing.service.gov.uk/media/5f294c478fa8f57acebf6792/walking-and-cycling-statistics-england-2019.pdf.
# [5] National Records of Scotland (2020). NRS Web Continuity Service. [online] webarchive.nrscotland.gov.uk. 
#     Available at: https://webarchive.nrscotland.gov.uk/20210313152406/https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/mid-year-population-estimates/mid-2019 [Accessed 8 May 2024].
# [6] ONS (2020). Population estimates for the UK, England and Wales, Scotland and Northern Ireland - Office for National Statistics. [online] www.ons.gov.uk. 
#     Available at: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/annualmidyearpopulationestimates/mid2019estimates.
# [7] Donnelly, J.E., Herrmann, S.D., Lambourne, K., Szabo, A.N., Honas, J.J. and Washburn, R.A. (2014). Does Increased Exercise or 
#     Physical Activity Alter Ad-Libitum Daily Energy Intake or Macronutrient Composition in Healthy Adults? A Systematic Review. 
#     PLoS ONE, 9(1), p.e83498. doi:https://doi.org/10.1371/journal.pone.0083498.


# setup
rm(list = ls())
gc()
library(tidyverse)
library(here)

source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")
# source(file = "models/child_model_calorie.R")
#source(file = "models/child_model_calorie_henry.R")

table_outputs = list() # creating a list of table outputs to be saved as an excel file


# required functions:

select_intervention_sample <- function(data, bmi_threshold, required_proportion, 
                                       weight_var, bmi_var, num_years, 
                                       citeria_1=0, citeria_2=0, citeria_1_value=0, citeria_2_value=0) {
  #browser()
  # Add intervention columns for each year to indicate intervention status, that is if an individual receives intervention.
  intervention_cols <- paste0("intervention_year", 1:num_years)
  data[, intervention_cols] <- "No"
  
  # Initialize a vector to store the individuals who have already received the intervention
  intervention_history <- rep(FALSE, nrow(data))
  
  for (year in 1:num_years) {
    # Subset the data frame to include only individuals meeting the criteria
    subset_data <- data[(data[[bmi_var]] >= bmi_threshold) &  !intervention_history, ]
    
    # Calculate the total weight of the full dataset
    total_weight <- sum(data[[weight_var]])
    eligible_weight = sum(subset_data[[weight_var]])
    
    # Check if the required proportion is not greater than 1
    if (required_proportion > 1) {
      stop("The desired sample is greater than the population meeting the criteria.")
    }
    
    
    # Calculate the desired weight sum for the sample
    desired_weight_sum <- required_proportion * sum(subset_data[[weight_var]])
    
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
    selected_indices_original <- which((data[[bmi_var]] >= 25) &
                                         !intervention_history)[selected_individuals]
    
    # Update the intervention column for the current year
    data[selected_indices_original, intervention_cols[year]] <- "Yes"
    
    # Update the intervention history
    intervention_history[selected_indices_original] <- TRUE
    
    final_wt = sum(data$wt_int[data$intervention_year1 == "Yes"])
    
  }
  
  return(data)
}



# Estimating the impact of the policy in:

# 1. Adults in England:

# 1.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab", nation = "England", population_group = "Adult")


df = read_csv(here("inputs/processed/hse_2019.csv"))

# Selecting individuals to recive intervention

set.seed(444)

df_selected = select_intervention_sample(data = df,
                                         bmi_threshold = 25, # interrested in impact of policy on those living with excess weight
                                         required_proportion = 0.5, # assuming that 50% of those living with excess weight increase their physical activity
                                         weight_var = "wt_int",
                                         bmi_var = "bmi",
                                         num_years = 1)


# 1.2. Estimating the impact of the intervention on prevalence of obesity:

# first we need to calculate the energy expenditure as a result of additonal trips

# Energy Expenditure = [(Metabolic Equivalent x 3.5 x body weight kg)/200] x time spent doing activity [1][2][3]

# As per the formula, we need to calculate/ estimate:

# (A) Metabolic Equivalent (MET) of Cycling & Walking
# Metabolic equivalent for walking = 3 [1][2]
# Metabolic equivalent for cycling = 6 [1][2]
# mean metabolic equivalent = 4.5 (equivalent of a moderate intensity activity)

met = 4.5


# (B) time spent doing activity (per adult in minutes):
# Total time spent on activity = (time spent walking x walking trips) + (time spent cycling x cycling trips)
# = 10,000 trips x 23 minutes + 40,000 trips x 17 minutes
# Total time spent on trips = 910,000 active minutes


# Cost of active travel infrastructure in Glasgow = £475 million
# Increase in total time spent doing activity as a result of spending £500 million = (£500 x 910,000)/ £475 = 957,894.736 active minutes

# Adult population for England = 44,263,393
# Additional minutes spent on active transport = 957,894.736/44,263,393 = 0.022 minutes


time = 0.022


# We are interested in impacts over five years.

implmentation_duration = 365*5


# updating the dataframe with the MET, time spent doing activity (per adult in minutes)
df_selected = df_selected %>%
  mutate(metabolic_equivalent = case_when(intervention_year1 == "Yes" ~ met,
                                          TRUE ~ 0),
         time_activity = case_when(intervention_year1 == "Yes" ~ time,
                                   TRUE ~ 0)) %>%
  mutate(energy_expenditure = -((metabolic_equivalent * 3.5 * weight)/ 200) * time_activity ) %>% # see energy expenditure formula above
  mutate(sex = ifelse(sex == 1, "male", "female"))


# For each individual/ observation in the HSE, we apply the energy expenditure as a result of the activity,
# (converting it into type numeric), then creating a vector, which essentially repeats the intake diff
# value 365*5 times (i.e. for a five year period), which is then transposed to match the input requirements
# of the Hall Model.
ee_change <- t(apply(df_selected, 1, function(x) rep(as.numeric(x["energy_expenditure"]), implmentation_duration)))


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
         "rmr", "bmi_class", "intake", "intervention_year1", "energy_expenditure", "1", "365", "730", "1095", "1460", "1825" ) %>%
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

bmi_change_year


table_outputs[["england_adult"]] = bmi_change_year

# bmi year on year prevalence:

write_xlsx(path = "outputs/policy_19/policy_19_england.xlsx", x = table_outputs)

# Output 3: Cost Modelling input files:
write.csv(post_df_adult, file = "outputs/policy_19/policy_19_adult_england_bmi.csv")

