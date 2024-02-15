#############################################################################################
# Policy 6 : Everyone with a BMI of 30 or above is offered a free referral to a behavioural #
#            weight loss programme (either 12-month TDR or 12-week education) via a primary #
#            care clinician                                                                 #
#############################################################################################

# Description:

# The evidence from the rapid review 
# (https://docs.google.com/document/d/1D1AFdzW5WrkVtzi1Tamy1PpMQwtAQeMQnCCWuwDJudk/edit?usp=sharing) 
# (quality assured by the EAG) showed that the interventions led to a weight loss of -5.6 kgs at the
# end of one year for those on TDR and 2.4 kgs for those receiving only education. The take-up rate 
# for the programme was 40% of which 13% opted in to receive TDR. The weight regain observed for those
# who underwent the programme is 0.01 kg per month.

# In the modelling we assume that those who receive the treatment once do not receive it again the
# subsequent years.


# setup
rm(list = ls())
gc()
library(tidyverse)
library(here)

source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")





## assuming no one from previous cohorts is offered treatment 

# Function to assign cohorts based on BMI and previous cohorts
assign_cohort <- function(df, bmi_col, prev_cohorts) {
  cohort <- ifelse(prev_cohorts == 0 & df[[bmi_col]] > 30, # not received treatment in previous cohorts and have a BMI >= 30
                   ifelse(runif(nrow(df)) <= 0.4, ifelse(runif(nrow(df)) <= 0.13, 2, 1), 0), 0) # randomly choose 40% to take-up and 13% of those to receive TDR
  return(cohort)
}

# Calculating the weight loss or gain post intervention:
# this works but same assignmnet for year 1 and year 2
calculate_weight_loss_gain <- function(df, year) {
  weight_loss_gain <- numeric(nrow(df))
  
  for (i in 1:nrow(df)) {
    if (year == 0) {
      bmi_col <- "bmi"
      prev_cohorts <- paste0("cohort_", 1)
    } else {
      bmi_col <- paste0("bmi_y", year)
      prev_cohorts <- paste0("cohort_", 1:year)
    }
    
    if (df[i, bmi_col] > 30) {
      if (year == 0) {
        if (df[i, "cohort_1"] == 1) {
          weight_loss_gain[i] <- 2.4
        } else if (df[i, "cohort_1"] == 2) {
          weight_loss_gain[i] <- 5.6
        } else {
          weight_loss_gain[i] <- -0.4
        }
      } else {
        
        if (df[i, paste0("cohort_", year+1)] == 1) {
          weight_loss_gain[i] <- 2.4
        } else if (df[i, paste0("cohort_", year+1)] == 2) {
          weight_loss_gain[i] <- 5.6
        } else if (any(df[i, prev_cohorts] %in% c(1, 2))) {
          weight_loss_gain[i] <- -0.12
        } else {
          weight_loss_gain[i] <- -0.4
        }
      }
    } else {
      
      if (any(df[i, prev_cohorts] %in% c(1, 2))) {
        weight_loss_gain[i] <- -0.12
      } else {     
        
        weight_loss_gain[i] <- 0
        
      }
    }
  }
  
  return(weight_loss_gain)
}




# Function to perform the cohort assignment, weight loss/ gain calculations 
perform_analysis <- function(df) {
  df <- df %>%
    mutate(cohort_1 = assign_cohort(., "bmi", 0)) # assigning to cohort 1 in year 1 to receive education or TDR
  
  
  #browser()
  df$weight_loss_y1 <- calculate_weight_loss_gain(df, year = 0) # setting weight loss or gain depending on assignment to TDR or education
  
  #browser()
  df <- df %>%
    mutate(weight_y1 = weight - weight_loss_y1, # calculating new weight at end of year 1
           bmi_y1 = weight_y1 / ((height/100)^2)) # calculating new BMI at end of year 1
  
  
  
  df <- df %>%
    mutate(cohort_2 = assign_cohort(., "bmi_y1", cohort_1)) # assigning to cohort 2 in year 2 to receive education or TDR
  
  #browser()
  
  df$weight_loss_y2 <- calculate_weight_loss_gain(df, year = 1) # setting weight loss or gain depending on assignment to TDR or education
  
  # browser()
  
  
  
  df <- df %>%
    mutate(weight_y2 = weight_y1 - weight_loss_y2, # calculating new weight at end of year 2
           bmi_y2 = weight_y2 / ((height/100)^2)) # calculating new BMI at end of year 2
  
  
  
  df <- df %>%
    mutate(cohort_3 = assign_cohort(., "bmi_y2", cohort_1 + cohort_2)) # assigning to cohort 3 in year 3 to receive education or TDR
  
  
  
  df$weight_loss_y3 <- calculate_weight_loss_gain(df, year = 2) # setting weight loss or gain depending on assignment to TDR or education
  
  
  
  df <- df %>%
    mutate(weight_y3 = weight_y2 - weight_loss_y3, # calculating new weight at end of year 3
           bmi_y3 = weight_y3 / ((height/100)^2)) # calculating new BMI at end of year 3
  
  
  
  df <- df %>%
    mutate(cohort_4 = assign_cohort(., "bmi_y3", cohort_1 + cohort_2 + cohort_3))  # assigning to cohort 4 in year 4 to receive education or TDR
  
  
  
  df$weight_loss_y4 <- calculate_weight_loss_gain(df, year = 3) # setting weight loss or gain depending on assignment to TDR or education
  
  
  
  df <- df %>%
    mutate(weight_y4 = weight_y3 - weight_loss_y4, # calculating new weight at end of year 4
           bmi_y4 = weight_y4 / ((height/100)^2)) # calculating new BMI at end of year 4
  
  
  
  df <- df %>%
    mutate(cohort_5 = assign_cohort(., "bmi_y4", cohort_1 + cohort_2 + cohort_3 + cohort_4)) # assigning to cohort 5 in year 5 to receive education or TDR
  
  
  
  df$weight_loss_y5 <- calculate_weight_loss_gain(df, year = 4) # setting weight loss or gain depending on assignment to TDR or education
  
  
  
  df <- df %>%
    mutate(weight_y5 = weight_y4 - weight_loss_y5, # calculating new weight at end of year 5
           bmi_y5 = weight_y5 / ((height/100)^2)) # calculating new BMI at end of year 5
  
  
  
  return(df)
}





# Estimating the impact of the policy in:

# 1. Adults in England:

# 1.1. Cleaning the input/ baseline data:

process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab", nation = "England", population_group = "Adult")

#set.seed(123)

# 1.2. Estimating the impact of the intervention on prevalence of obesity:

df <- perform_analysis(df = read_csv(here("inputs/processed/hse_2019.csv")))


# Selecting variables of interest. We are interested only in the BMI values at the end of each intervention year to 
# estimate the distribution of bodyweights. The BMI values at the end of Year 1, Year 2, Year 3, Year 4 and Year 5 
# are collected and labelled with BMI categories.

post_df_adult = df %>%
  select("id", "weight", "height", "age_grp", "age", "sex", "bmi", "wt_int", "psu", "age_grp",
         "strata", "pal", "rmr", "bmi_class", "cohort_1", "weight_y1", "bmi_y1",
         "cohort_2", "weight_y2", "bmi_y2", "cohort_3", "weight_y3", "bmi_y3", "cohort_4", "weight_y4", "bmi_y4",
         "cohort_5", "weight_y5", "bmi_y5") %>%
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

write_xlsx(path = "outputs/policy_6/policy6_england_yearwise_bmi.xlsx", x = post_df_adult)

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



# Outputs:

# Output 1: Plot of year on year BMI category distribution
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
ggsave(here("outputs/policy_6/policy_6_impact_England_adult.png"), plot = adult_bar_plot, width = 10, height = 6, bg='#ffffff')

# Output 2: Table of year wise prevalence of obesity
bmi_change_year = bmi_change %>%
  select(-c(n)) %>%
  pivot_wider(., names_from = BMI, values_from = freq) %>%
  select(type, underweight, normal, overweight, obese, `morbidly obese`)


write_xlsx(path = "outputs/policy_6/policy_6_england.xlsx", x = bmi_change_year)


