# adult model for body weight and BMI changes

library(tidyverse)
library(here)
library(bw)
library(survey)


df_adult <- read_csv(here("inputs/processed/hse_2019.csv")) 


df = df_adult

# assuming people from previous cohort receive tretament

cut_off = 30 # bmi cutoff for treatment
wt_change_tdr = 5.6
wt_change_educ = 2.4
gen_pop_wt_gain = 0.4
post_int_regain = 0.01*12 # 0.01 kg per month => 0.12 kg per year
takeup_educ = 0.4
takeup_tdr = 0.13

set.seed(123) # For reproducibility

result <- calculate_weight_bmi_changes(df, cut_off, takeup_educ, takeup_tdr, wt_change_tdr, wt_change_educ, gen_pop_wt_gain, post_int_regain)


calculate_weight_bmi_changes <- function(df, cut_off, takeup_educ, takeup_tdr, wt_change_tdr, wt_change_educ, gen_pop_wt_gain, post_int_regain) {
  df <- df %>%
    mutate(cohort_1 = ifelse(bmi >= cut_off, ifelse(runif(n()) < takeup_educ, 1, 0), 0)) %>%
    mutate(cohort_1 = ifelse(cohort_1 == 1 & runif(n()) < takeup_tdr, 2, cohort_1)) %>%
    mutate(weight_loss_y1 = case_when(
      cohort_1 == 2 ~ -wt_change_tdr,
      cohort_1 == 1 ~ -wt_change_educ,
      TRUE ~ gen_pop_wt_gain
    )) %>%
    mutate(weight_y1 = weight + weight_loss_y1) %>%
    mutate(bmi_y1 = weight_y1 / (height / 100)^2) %>%
    mutate(cohort_2 = ifelse(bmi_y1 >= cut_off, ifelse(runif(n()) < takeup_educ, 1, 0), 0)) %>%
    mutate(cohort_2 = ifelse(cohort_2 == 1 & runif(n()) < takeup_tdr, 2, cohort_2)) %>%
    mutate(weight_loss_y2 = case_when(
      cohort_2 == 2 ~ -wt_change_tdr,
      cohort_2 == 1 ~ -wt_change_educ,
      TRUE ~ ifelse(cohort_1 == 1 | cohort_1 == 2, post_int_regain, gen_pop_wt_gain)
    )) %>%
    mutate(weight_y2 = weight_y1 + weight_loss_y2) %>%
    mutate(bmi_y2 = weight_y2 / (height / 100)^2) %>%
    mutate(cohort_3 = ifelse(bmi_y2 >= cut_off, ifelse(runif(n()) < takeup_educ, 1, 0), 0)) %>%
    mutate(cohort_3 = ifelse(cohort_3 == 1 & runif(n()) < takeup_tdr, 2, cohort_3)) %>%
    mutate(weight_loss_y3 = case_when(
      cohort_3 == 2 ~ -wt_change_tdr,
      cohort_3 == 1 ~ -wt_change_educ,
      TRUE ~ ifelse(cohort_1 == 1 | cohort_2 == 1 | cohort_2 == 2, post_int_regain, gen_pop_wt_gain)
    )) %>%
    mutate(weight_y3 = weight_y2 + weight_loss_y3) %>%
    mutate(bmi_y3 = weight_y3 / (height / 100)^2) %>%
    mutate(cohort_4 = ifelse(bmi_y3 >= cut_off, ifelse(runif(n()) < takeup_educ, 1, 0), 0)) %>%
    mutate(cohort_4 = ifelse(cohort_4 == 1 & runif(n()) < takeup_tdr, 2, cohort_4)) %>%
    mutate(weight_loss_y4 = case_when(
      cohort_4 == 2 ~ -wt_change_tdr,
      cohort_4 == 1 ~ -wt_change_educ,
      TRUE ~ ifelse(cohort_1 == 1 | cohort_2 == 1 | cohort_3 == 1 | cohort_3 == 2, post_int_regain, gen_pop_wt_gain)
    )) %>%
    mutate(weight_y4 = weight_y3 + weight_loss_y4) %>%
    mutate(bmi_y4 = weight_y4 / (height / 100)^2) %>%
    mutate(cohort_5 = ifelse(bmi_y4 >= cut_off, ifelse(runif(n()) < takeup_educ, 1, 0), 0)) %>%
    mutate(cohort_5 = ifelse(cohort_5 == 1 & runif(n()) < takeup_tdr, 2, cohort_5)) %>%
    mutate(weight_loss_y5 = case_when(
      cohort_5 == 2 ~ -wt_change_tdr,
      cohort_5 == 1 ~ -wt_change_educ,
      TRUE ~ ifelse(cohort_1 == 1 | cohort_2 == 1 | cohort_3 == 1 | cohort_4 == 1 | cohort_4 == 2, post_int_regain, gen_pop_wt_gain)
    )) %>%
    mutate(weight_y5 = weight_y4 + weight_loss_y5) %>%
    mutate(bmi_y5 = weight_y5 / (height / 100)^2)
  
  return(df)
}




## Option 2: assuming no one from previous cohorts is offered treatment 
  
library(dplyr)
library(tidyr)

# Function to assign cohorts based on BMI and previous cohorts
assign_cohort <- function(df, bmi_col, prev_cohorts) {
  cohort <- ifelse(prev_cohorts == 0 & df[[bmi_col]] > 30, 
                   ifelse(runif(nrow(df)) <= 0.4, ifelse(runif(nrow(df)) <= 0.13, 2, 1), 0), 0)
  return(cohort)
}


calculate_weight_loss_gain <- function(cohort, prev_cohorts) {
  weight_loss_gain <- ifelse(cohort == 2, 5.6, 
                             ifelse(cohort == 1, 2.4, 
                                    ifelse( any(prev_cohorts != 0), -0.12, -0.4)))
  return(weight_loss_gain)
}


# Function to calculate new weights and BMI
calculate_new_weights_bmi <- function(df, weight_loss_gain, weight_col, height_col) {
  weight_y <- df[[weight_col]] - weight_loss_gain
  bmi_y <- weight_y / (df[[height_col]]^2)
  return(list(weight_y, bmi_y))
}


# Function to perform the cohort assignment and calculations
perform_analysis <- function(df) {
  df <- df %>%
    mutate(cohort_1 = assign_cohort(., "bmi", 0))
  
  df$weight_loss_y1 <- calculate_weight_loss_gain(df$cohort_1, c(0))
  
  df <- df %>%
    mutate(weight_y1 = weight - weight_loss_y1,
           bmi_y1 = weight_y1 / ((height/100)^2))
  
  df <- df %>%
    mutate(cohort_2 = assign_cohort(., "bmi_y1", cohort_1))
  
  df$weight_loss_y2 <- calculate_weight_loss_gain(df$cohort_2, c(df$cohort_1))
  
  df <- df %>%
    mutate(weight_y2 = weight_y1 - weight_loss_y2,
           bmi_y2 = weight_y2 / ((height/100)^2))
  
  df <- df %>%
    mutate(cohort_3 = assign_cohort(., "bmi_y2", cohort_1 + cohort_2))
  
  df$weight_loss_y3 <- calculate_weight_loss_gain(df$cohort_3, c(df$cohort_1, df$cohort_2))
  
  df <- df %>%
    mutate(weight_y3 = weight_y2 - weight_loss_y3,
           bmi_y3 = weight_y3 / ((height/100)^2))
  
  df <- df %>%
    mutate(cohort_4 = assign_cohort(., "bmi_y3", cohort_1 + cohort_2 + cohort_3))
  
  df$weight_loss_y4 <- calculate_weight_loss_gain(df$cohort_4, c(df$cohort_1, df$cohort_2, df$cohort_3))
  
  df <- df %>%
    mutate(weight_y4 = weight_y3 - weight_loss_y4,
           bmi_y4 = weight_y4 / ((height/100)^2))
  
  df <- df %>%
    mutate(cohort_5 = assign_cohort(., "bmi_y4", cohort_1 + cohort_2 + cohort_3 + cohort_4))
  
  df$weight_loss_y5 <- calculate_weight_loss_gain(df$cohort_5, c(df$cohort_1, df$cohort_2, df$cohort_3, df$cohort_4))
  
  df <- df %>%
    mutate(weight_y5 = weight_y4 - weight_loss_y5,
           bmi_y5 = weight_y5 / ((height/100)^2))
  
  return(df)
}


# Create a sample dataframe (replace it with your actual dataframe)
set.seed(123)


df <- perform_analysis(df_adult)






post_df_adult = df %>%
  select("id", "weight", "height", "age", "sex", "bmi", "wt_int", "psu", 
         "strata", "pal", "rmr", "bmi_class", "weight_y1", "bmi_y1",
         "weight_y2", "bmi_y2", "weight_y3", "bmi_y3", "weight_y4", "bmi_y4",
         "weight_y5", "bmi_y5") %>%
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
       subtitle = "Population") +
  theme_ipsum(base_size = 8, axis_title_size = 8) + #, base_family="Averta"
  theme(legend.position = "top")
adult_bar_plot
ggsave(here("outputs/figures/adult_bmi_distrib_sc_policy4a.png"), plot = adult_bar_plot, width = 10, height = 6, bg='#ffffff')

# Output 2: Table of year wise prevalence of obesity
bmi_change_year = bmi_change %>%
  select(-c(n)) %>%
  pivot_wider(., names_from = BMI, values_from = freq) %>%
  select(type, underweight, normal, overweight, obese, `morbidly obese`)



