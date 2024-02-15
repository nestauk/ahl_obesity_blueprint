
#rm(list = ls())

library(tidyverse)
library(here)
library(bw)
library(survey)

# function to calculate obesity prevalance based on change in calorie intake
calculate_child_bmi_from_eichange = function(df, intake_change, implementation_duration, use_bodyfat_curves){
  
  output_list = list()
  
  df <- df %>%  # we are reading a file that contains energy intake, BMI weight class and other variables reated during the pre-processing stage.
    mutate(sex = ifelse(sex == 1, "male", "female")) %>% 
    filter(fm_hudda > 0) %>%
    mutate(intake_diff = ifelse(bmi_cat_baseline %in% c("underweight", "normal"), intake_change, intake_change )) %>%
    mutate(intervention = ifelse(bmi_cat_baseline %in% c("underweight", "normal"), "Yes", "Yes"))
  
  
  energy_intake = apply(df, 1, function(x) rep(as.numeric(x["intake_hox"]) + as.numeric(x["intake_diff"]), implementation_duration))
  browser()
  # implementing Hall model
  child_model_weight = child_weight(age = df$age,
                                    sex =  df$sex, 
                                    FM = df$fm_hudda, 
                                    FFM = df$ffm_hudda, 
                                    EI = energy_intake,   
                                    days = implementation_duration,
                                    checkValues = TRUE)
  
  output_list[["hall_model_op"]] = child_model_weight
  
  # Extracting body weight from Hall model outputs
  child_bw_h = as.data.frame(child_model_weight[["Body_Weight"]]) %>%
    mutate(bw_diff = V1825 - V1) %>%
    select(c(V1, V365, V730, V1095, V1460, V1825, bw_diff)) %>%
    rename(bw_y0 = V1, bw_y1 = V365, bw_y2 = V730, 
           bw_y3 = V1095, bw_y4 = V1460, bw_y5 = V1825)
  
  
  # Extracting age from Hall model outputs
  child_age_h = as.data.frame(child_model_weight[["Age"]]) %>%
    mutate(age_diff = V1825 - V1) %>%
    select(c(V1, V365, V730, V1095, V1460, V1825, age_diff)) %>%
    rename(age_y0 = V1, age_y1 = V365, age_y2 = V730, 
           age_y3 = V1095, age_y4 = V1460, age_y5 = V1825)
  
  # Extracting fat free mass from Hall model outputs
  child_ffm_h = as.data.frame(child_model_weight[["Fat_Free_Mass"]]) %>%
    mutate(ffm_diff = V1825 - V1) %>%
    select(c(V1, V365, V730, V1095, V1460, V1825, ffm_diff)) %>%
    rename(ffm_y0 = V1, ffm_y1 = V365, ffm_y2 = V730,
           ffm_y3 = V1095, ffm_y4 = V1460, ffm_y5 = V1825)
  
  
  # Extracting fat mass from Hall model outputs
  child_fm_h = as.data.frame(child_model_weight[["Fat_Mass"]]) %>%
    mutate(fm_diff = V1825 - V1) %>%
    select(c(V1, V365, V730, V1095, V1460, V1825, fm_diff))  %>%
    rename(fm_y0 = V1, fm_y1 = V365, fm_y2 = V730,
           fm_y3 = V1095, fm_y4 = V1460, fm_y5 = V1825)
  
  
  # combining all the extracted tables and binding them to the original dataframe
  child_age_bw_ffm_fm = cbind(child_bw_h, child_age_h, child_ffm_h, child_fm_h) %>%
    select(age_y0, bw_y0, fm_y0, ffm_y0,
           age_y1, bw_y1, fm_y1, ffm_y1,
           age_y2, bw_y2, fm_y2, ffm_y2,
           age_y3, bw_y3, fm_y3, ffm_y3,
           age_y4, bw_y4, fm_y4, ffm_y4,
           age_y5, bw_y5, fm_y5, ffm_y5)
  
  
  df_child = cbind(df, child_age_bw_ffm_fm)
  
  # looping throuhg five years to calculate body fat percentage and categorise into BMI groups
  years <- c("y0","y1", "y2", "y3", "y4", "y5")
  
  for (year in years) {
    # Calculate bf_percent for each year
    bf_percent <- paste0("bf_percent_", year)
    bw_col <- paste0("bw_", year)
    fm_col <- paste0("fm_", year)
    
    # Check if the column already exists in df_child
    if (!(bf_percent %in% colnames(df_child))) {
      # Add bf_percent column to df_child
      df_child <- df_child %>%
        mutate(!!sym(bf_percent) := ((!!sym(fm_col) / !!sym(bw_col)) * 100))
      
      
      
      if (use_bodyfat_curves == 0) {
        
        # Categorize bf_percent using body fat reference curves from McCarthy et al. (2008)
        df_child <- df_child %>%
          mutate(
            !!sym(paste0("bmi_cat_", year)) := case_when(
              age == 6 ~ (
                ifelse(
                  sex == "male",
                  case_when(
                    !!sym(bf_percent) <= 12.4 ~ "underweight",
                    !!sym(bf_percent) >= 19.5 & !!sym(bf_percent) < 22.7 ~ "overweight",
                    !!sym(bf_percent) > 12.4 & !!sym(bf_percent) < 19.5 ~ "normal",
                    !!sym(bf_percent) >= 22.7 ~ "obese"
                  ),
                  case_when(
                    !!sym(bf_percent) <= 14.4 ~ "underweight",
                    !!sym(bf_percent) > 14.4 & !!sym(bf_percent) < 23 ~ "normal",
                    !!sym(bf_percent) >= 23 & !!sym(bf_percent) < 26.2 ~ "overweight",
                    !!sym(bf_percent) >= 26.2 ~ "obese"
                  )
                )
              ),
              age == 9 ~ (
                ifelse(
                  sex == "male",
                  case_when(
                    !!sym(bf_percent) <= 12.8 ~ "underweight",
                    !!sym(bf_percent) > 12.8 & !!sym(bf_percent) < 22.2 ~ "normal",
                    !!sym(bf_percent) >= 22.2 & !!sym(bf_percent) < 26.8 ~ "overweight",
                    !!sym(bf_percent) >= 26.8 ~ "obese"
                  ),
                  case_when(
                    !!sym(bf_percent) <= 15.7 ~ "underweight",
                    !!sym(bf_percent) > 15.7 & !!sym(bf_percent) < 27.2 ~ "normal",
                    !!sym(bf_percent) >= 27.2 & !!sym(bf_percent) < 31.2 ~ "overweight",
                    !!sym(bf_percent) >= 31.2 ~ "obese"
                  )
                )
              ),
              age == 11.5 ~ (
                ifelse(
                  sex == "male",
                  case_when(
                    !!sym(bf_percent) <= 12.6 ~ "underweight",
                    !!sym(bf_percent) > 12.6 & !!sym(bf_percent) < 23 ~ "normal",
                    !!sym(bf_percent) >= 23 & !!sym(bf_percent) < 28.3 ~ "overweight",
                    !!sym(bf_percent) >= 28.3 ~ "obese"
                  ),
                  case_when(
                    !!sym(bf_percent) <= 16.1 ~ "underweight",
                    !!sym(bf_percent) > 16.1 & !!sym(bf_percent) < 28.8 ~ "normal",
                    !!sym(bf_percent) >= 28.8 & !!sym(bf_percent) < 32.8 ~ "overweight",
                    !!sym(bf_percent) >= 32.8 ~ "obese"
                  )
                )
              ),
              age == 14 ~ (
                ifelse(
                  sex == "male",
                  case_when(
                    !!sym(bf_percent) <= 10.9 ~ "underweight",
                    !!sym(bf_percent) > 10.9 & !!sym(bf_percent) < 21.3 ~ "normal",
                    !!sym(bf_percent) >= 21.3 & !!sym(bf_percent) < 25.9 ~ "overweight",
                    !!sym(bf_percent) >= 25.9 ~ "obese"
                  ),
                  case_when(
                    !!sym(bf_percent) <= 16 ~ "underweight",
                    !!sym(bf_percent) > 16 & !!sym(bf_percent) < 29.6 ~ "normal",
                    !!sym(bf_percent) >= 29.6 & !!sym(bf_percent) < 33.6 ~ "overweight",
                    !!sym(bf_percent) >= 33.6 ~ "obese"
                  )
                )
              ),
              age == 17 ~ (
                ifelse(
                  sex == "male",
                  case_when(
                    !!sym(bf_percent) <= 9.8 ~ "underweight",
                    !!sym(bf_percent) > 9.8 & !!sym(bf_percent) < 20.1 ~ "normal",
                    !!sym(bf_percent) >= 20.1 & !!sym(bf_percent) < 23.9 ~ "overweight",
                    !!sym(bf_percent) >= 23.9 ~ "obese"
                  ),
                  case_when(
                    !!sym(bf_percent) <= 15.1 ~ "underweight",
                    !!sym(bf_percent) > 15.1 & !!sym(bf_percent) < 30.4 ~ "normal",
                    !!sym(bf_percent) >= 30.4 & !!sym(bf_percent) < 34.4 ~ "overweight",
                    !!sym(bf_percent) >= 34.4 ~ "obese"
                  )
                )
              ),
              TRUE ~ NA_character_
            )
          )
        
        
      } else if (use_bodyfat_curves == 1){
        # Categorize bf_percent using body fat reference curves from McCarthy et al. (2008)
        
        df_child <- df_child %>%
          mutate(
            !!sym(paste0("bmi_cat_", year)) := case_when(
              !!sym(bf_percent) < p2 ~ "underweight",
              !!sym(bf_percent) >= p95 ~ "obese",
              !!sym(bf_percent) >= p85 & !!sym(bf_percent) < p95 ~ "overweight",
              TRUE ~ "normal"
            )
          )
        
      }
      
      
    }
  }
  
  output_list[["post_df"]] = df_child # Dataframe added to outputs list
  
  child_bmi_change = rbind(
    df_child %>% 
      count(bmi_cat_y0, wt = wt_int) %>% 
      mutate(freq = n/sum(n)*100,
             type = "Baseline") %>% 
      rename(BMI = bmi_cat_y0),
    df_child %>% 
      count(bmi_cat_y1, wt = wt_int) %>% 
      mutate(freq = n/sum(n)*100,
             type = "Year 1") %>% 
      rename(BMI = bmi_cat_y1),
    df_child %>% 
      count(bmi_cat_y2, wt = wt_int) %>% 
      mutate(freq = n/sum(n)*100,
             type = "Year 2") %>% 
      rename(BMI = bmi_cat_y2),
    df_child %>% 
      count(bmi_cat_y3, wt = wt_int) %>% 
      mutate(freq = n/sum(n)*100,
             type = "Year 3") %>% 
      rename(BMI = bmi_cat_y3),
    df_child %>% 
      count(bmi_cat_y4, wt = wt_int) %>% 
      mutate(freq = n/sum(n)*100,
             type = "Year 4") %>% 
      rename(BMI = bmi_cat_y4),
    df_child %>% 
      count(bmi_cat_y5, wt = wt_int) %>% 
      mutate(freq = n/sum(n)*100,
             type = "Year 5") %>% 
      rename(BMI = bmi_cat_y5))
  
  child_bmi_change = child_bmi_change %>%
    mutate(BMI = factor(BMI, levels = c("underweight", "normal", "overweight", "obese", "morbidly obese"))) %>%
    as.data.frame()
  
  
  # output bar plot of BMI categories distributions
  child_bar_plot = child_bmi_change %>%
    ggplot(., aes(y = freq, x = BMI, fill = type)) + 
    geom_bar(stat = "identity", position = "dodge") +
    theme_ipsum() +
    labs(fill = "", 
         title = "BMI Categories Distribution", 
         y = "Frequency",
         subtitle = "Population") +
    theme_ipsum(base_size = 8, axis_title_size = 8) + #, base_family="Averta"
    theme(legend.position = "top")
  
  output_list[["bmi_category_plot"]] = child_bar_plot # BMI category distribution plot is added to the outputs list
  
  
  child_bmi_change_year = child_bmi_change %>%
    select(-c(n)) %>%
    pivot_wider(., names_from = BMI, values_from = freq)
  
  
  output_list[["bmi_percent_prevalence"]] = child_bmi_change_year # Year wise BMI category percentage prevalance table is added to outputs list
  
  return(output_list) # outputs are returned

}




