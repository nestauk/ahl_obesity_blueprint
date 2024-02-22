
library(tidyverse)
library(here)
library(bw)
library(survey)
library(sitar)
library(jsonlite)



lookup_energy_intake = function(age, sex, data_B){
  #browser()
  
  if (age <19){
    
    age = ceiling(age)
    age_row_prev <- data_B[data_B$age == age - 1 & data_B$sex == sex, ]
    age_row <- data_B[data_B$age == age & data_B$sex == sex, ]
    
    ei_age_prev = age_row_prev$population_kcal
    ei_age = age_row$population_kcal
    
    ei_excess = case_when(age >= 5 & age <= 10 & sex == 1 ~ 21,
                          age >= 11 & age <= 15 & sex == 1 ~ 69,
                          age >= 16 & age <= 18 & sex == 1 ~ 104,
                          age >= 5 & age <= 10 & sex == 2 ~ 34,
                          age >= 11 & age <= 15 & sex == 2 ~ 63,
                          age >= 16 & age <= 18 & sex == 2 ~ 44)
    
    ei_excess = 0
    
    net_energy_intake = (ei_age - ei_age_prev) + ei_excess
  } else{
    
    net_energy_intake = 0
    
  }
  return(net_energy_intake)
  
}


generate_sacn_dietary_intake = function(input_path){
  
  df = read_csv(input_path)
  
  return(df)
  
}




generate_interpolated_energy <- function(row) {
  #browser()
  energy_values <- c(as.numeric(row["intake_hox"]),
                     as.numeric(row["ei_365"]),
                     as.numeric(row["ei_730"]),
                     as.numeric(row["ei_1095"]),
                     as.numeric(row["ei_1460"]),
                     as.numeric(row["ei_1825"]))
  
  interpolated_energy <- energy_build(energy_values, c(0, 365, 730, 1095, 1460, 1825), interpolation = "Brownian")
  
  return(interpolated_energy)
  
}



lookup_projected_height <- function(age, sex, height, data_B, years_added) {
  # Find the row in dataframe B corresponding to the given age
  #op_list = list()
  
  if (sex == "female"){
    sex = 2
  } else { sex = 1}
  
  
  age_row <- data_B[data_B$x == age & data_B$sex == sex, ]
  
  
  # Find the percentile with the closest height to the one in dataframe A
  closest_percentile_index <- which.min(abs(age_row$y - height))
  closest_percentile <- age_row$centile[closest_percentile_index]
  #op_list[1] = closest_percentile
  
  # Find the age 3 years later
  age_future <- age + years_added
  
  if (age_future <= 20){
    
    # Find the projected height for age + 3 and the identified percentile
    projected_height <- data_B$y[data_B$x == age_future & data_B$centile == closest_percentile & data_B$sex == sex]
    #op_list[2] = projected_height
    
  } else{   if(age_future == 21) {
    
    
    projected_height <- data_B$y[data_B$x == (age_future - 1) & data_B$centile == closest_percentile & data_B$sex == sex] 
    
    
  } else{ if(age_future == 22){
    
    projected_height <- data_B$y[data_B$x == (age_future - 2) & data_B$centile == closest_percentile & data_B$sex == sex]   
    
  } else{ if(age_future == 23){
    
    projected_height <- data_B$y[data_B$x == (age_future - 3) & data_B$centile == closest_percentile & data_B$sex == sex]
    
  }
    
  }
    
  }
    
  }
  
  return(projected_height)
}




calculate_bmi_category <- function(age, sex, bmi, df_B) {
  #browser()
  if (age <= 20){
    
    if (sex == "female"){
      sex = 2
    } else { sex = 1}
    
    
    #browser()
    percentile_2 <- df_B$p_2[which(df_B$age == age & df_B$sex == sex)]
    percentile_85 <- df_B$p_85[which(df_B$age == age & df_B$sex == sex)]
    percentile_95 <- df_B$p_95[which(df_B$age == age & df_B$sex == sex)]
    
    category <- case_when(
      bmi <= percentile_2 ~ "underweight",
      bmi > percentile_2 & bmi < percentile_85 ~ "normal",
      bmi >= percentile_85 & bmi < percentile_95 ~ "overweight",
      bmi >= percentile_95 ~ "obese"
    )
    
  } else{
    
    category <- case_when(
      bmi < 18.5 ~ "underweight",
      bmi>= 18.5 | bmi <25 ~ "normal",
      bmi >= 25 & bmi <30 ~ "overweight",
      bmi >= 30 ~ "obese"
      
    )
    
    
  }
  
  
  return(category)
}



generate_height_refdata = function(input_1, input_2){
  
  female_ht_json = read_json(input_1, simplifyVector = FALSE)
  
  male_ht_json = read_json(input_2, simplifyVector = FALSE)
  
  
  rcpch_4_uk90child_female_ht = female_ht_json[[4]][["uk90_child"]][["female"]][["height"]]
  rcpch_4_uk90child_male_ht = male_ht_json[[4]][["uk90_child"]][["male"]][["height"]]
  
  
  female_ht_df = tibble(height_percentile_f = rcpch_4_uk90child_female_ht)
  male_ht_df   = tibble(height_percentile_m = rcpch_4_uk90child_male_ht)
  
  
  female_ht_df_1 = as.data.frame (female_ht_df %>%
                                    unnest_wider(height_percentile_f) %>%
                                    select(sds, centile, data) %>%
                                    unnest_longer(data) %>%
                                    select(sds, centile, data) %>%
                                    unnest_wider(data)) %>%
    select(sds, centile, x, y) %>%
    mutate(sex = 2) %>%
    distinct()
  
  male_ht_df_1 = as.data.frame (male_ht_df %>%
                                  unnest_wider(height_percentile_m) %>%
                                  select(sds, centile, data) %>%
                                  unnest_longer(data) %>%
                                  select(sds, centile, data) %>%
                                  unnest_wider(data)) %>%
    select(sds, centile, x, y) %>%
    mutate(sex = 1) %>%
    distinct()
  
  height_refdata = rbind(female_ht_df_1, male_ht_df_1)
  
  return(height_refdata)
  
}




generate_bmi_refdata = function(data_B){
  
  bmi_refdata = data_B %>%
    select(years, sex, L.bmi, M.bmi, S.bmi) %>%
    subset(years >= as.double(4) & years <= as.double(20)) %>%
    mutate(p_2 = (M.bmi*(1 + L.bmi*S.bmi*-2.054)^(1/L.bmi)),
           p_85 = (M.bmi*(1 + L.bmi*S.bmi*1.036)^(1/L.bmi)),
           p_95 = (M.bmi*(1 + L.bmi*S.bmi*1.645)^(1/L.bmi))) %>%
    select(years, sex, p_2, p_85, p_95) %>%
    rename(age = years)
  
  return(bmi_refdata)
  
}



