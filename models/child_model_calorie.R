
#rm(list = ls())

library(tidyverse)
library(here)
library(bw)
library(survey)


source(file = "models/model_utils.R")

# function to calculate obesity prevalance based on change in calorie intake
calculate_child_bmi_from_eichange = function(df, intake_change, implementation_duration, use_bodyfat_curves){
  
  output_list = list()
  
  # creating a dataframe with the estimated average requirement by age
  sacn_dietary_intake = generate_sacn_dietary_intake(here("inputs/ref_data/sacn_dietary_intake.csv"))
  
  # reading in the processed dataframe with child data and then estimating the energy intake at different ages for five years
  # using baseline energy intake and difference in energy intake required for growth.
  
  df <- df %>%  # we are reading a file that contains energy intake, BMI weight class and other variables reated during the pre-processing stage.
    #mutate(sex = ifelse(sex == 1, "male", "female")) %>% 
    filter(fm_hudda > 0) %>%
    rowwise() %>%
    mutate(ei_365 = as.numeric(intake_hox + lookup_energy_intake(age = age + 1, sex = sex, data_B = sacn_dietary_intake)),
           ei_730 = as.numeric(ei_365 + lookup_energy_intake(age = age + 2, sex = sex, data_B = sacn_dietary_intake)),
           ei_1095 = as.numeric(ei_730 + lookup_energy_intake(age = age + 3, sex = sex, data_B = sacn_dietary_intake)),
           ei_1460 = as.numeric(ei_1095 + lookup_energy_intake(age = age + 4, sex = sex, data_B = sacn_dietary_intake)),
           ei_1825 = as.numeric(ei_1460 + lookup_energy_intake(age = age + 5, sex = sex, data_B = sacn_dietary_intake))) %>%
    ungroup() %>%
    mutate(sex = ifelse(sex == 1, "male", "female")) 
  
  # creating the daily energy intake matrix for every day of five years
  energy_matrix = apply(df, 1, generate_interpolated_energy)
  
  # applying change in intake due to intervention
  energy_matrix = energy_matrix - intake_change
  
  #energy_intake = apply(df, 1, function(x) rep(as.numeric(x["intake_hox"]) + as.numeric(x["intake_diff"]), implementation_duration))
  #browser()
  # implementing Hall model
  child_model_weight = child_weight(age = df$age,
                                    sex =  df$sex, 
                                    FM = df$fm_hudda, 
                                    FFM = df$ffm_hudda, 
                                    EI = energy_matrix,   
                                    days = implementation_duration,
                                    checkValues = TRUE)
  
  output_list[["hall_model_op"]] = child_model_weight
  
  # Extracting body weight from Hall model outputs
  child_bw_h = as.data.frame(child_model_weight[["Body_Weight"]]) %>%
    mutate(bw_diff = V1825 - V1) %>%
    select(c(V1, V365, V730, V1095, V1460, V1825, bw_diff)) %>%
    rename(bw_1 = V1, bw_365 = V365, bw_730 = V730, 
           bw_1095 = V1095, bw_1460 = V1460, bw_1825 = V1825)
  
  
  # Extracting age from Hall model outputs
  child_age_h = as.data.frame(child_model_weight[["Age"]]) %>%
    mutate(age_diff = V1825 - V1) %>%
    select(c(V1, V365, V730, V1095, V1460, V1825, age_diff)) %>%
    rename(age_1 = V1, age_365 = V365, age_730 = V730, 
           age_1095 = V1095, age_1460 = V1460, age_1825 = V1825)
  
  # Extracting fat free mass from Hall model outputs
  child_ffm_h = as.data.frame(child_model_weight[["Fat_Free_Mass"]]) %>%
    mutate(ffm_diff = V1825 - V1) %>%
    select(c(V1, V365, V730, V1095, V1460, V1825, ffm_diff)) %>%
    rename(ffm_1 = V1, ffm_365 = V365, ffm_730 = V730,
           ffm_1095 = V1095, ffm_1460 = V1460, ffm_1825 = V1825)
  
  
  # Extracting fat mass from Hall model outputs
  child_fm_h = as.data.frame(child_model_weight[["Fat_Mass"]]) %>%
    mutate(fm_diff = V1825 - V1) %>%
    select(c(V1, V365, V730, V1095, V1460, V1825, fm_diff))  %>%
    rename(fm_1 = V1, fm_365 = V365, fm_730 = V730,
           fm_1095 = V1095, fm_1460 = V1460, fm_1825 = V1825)
  
  
  # combining all the extracted tables and binding them to the original dataframe
  child_age_bw_ffm_fm = cbind(child_bw_h, child_age_h, child_ffm_h, child_fm_h) %>%
    select(age_1, bw_1, fm_1, ffm_1,
           age_365, bw_365, fm_365, ffm_365,
           age_730, bw_730, fm_730, ffm_730,
           age_1095, bw_1095, fm_1095, ffm_1095,
           age_1460, bw_1460, fm_1460, ffm_1460,
           age_1825, bw_1825, fm_1825, ffm_1825)
  
  # creating the height reference data based on UK90 growth charts
  uk90_height_refdata = generate_height_refdata(here("inputs/ref_data/cole-nine-centiles-uk-who-female-height.json"),
                                                here("inputs/ref_data/cole-nine-centiles-uk-who-male-height.json"))
  
  # creating the bmi reference data based on UK90 growth charts
  uk90_bmi_refdata = generate_bmi_refdata(sitar::uk90)
  
  
  
  
  df_child = cbind(df, child_age_bw_ffm_fm) %>%
    rowwise() %>%
    # calculating the childs height each year assuming that they stay on the baseline percentile:
    mutate(ht_365  = lookup_projected_height(age, sex, height, data_B = uk90_height_refdata, years_added = 1)) %>%
    mutate(ht_730  = lookup_projected_height(age, sex, height, data_B = uk90_height_refdata, years_added = 2)) %>%
    mutate(ht_1095 = lookup_projected_height(age, sex, height, data_B = uk90_height_refdata, years_added = 3)) %>%
    mutate(ht_1460 = lookup_projected_height(age, sex, height, data_B = uk90_height_refdata, years_added = 4)) %>%
    mutate(ht_1825 = lookup_projected_height(age, sex, height, data_B = uk90_height_refdata, years_added = 5)) %>%
    # using the calculated heights, body weight from Hall model outputs to calculate BMI:
    mutate(bmi_365  = bw_365  / (ht_365/100)^2,
           bmi_730  = bw_730  / (ht_730/100)^2,
           bmi_1095 = bw_1095 / (ht_1095/100)^2,
           bmi_1460 = bw_1460 / (ht_1460/100)^2,
           bmi_1825 = bw_1825 / (ht_1825/100)^2) %>%
    # using the calculated BMIs and the bmi ref data to assign bmi categories to children:
    mutate(bmi_cat_1 = calculate_bmi_category(age = age, sex = sex, bmi = bmi, df_B = uk90_bmi_refdata),
           bmi_cat_365 = calculate_bmi_category(age = age + 1, sex = sex, bmi = bmi_365, df_B = uk90_bmi_refdata),
           bmi_cat_730 = calculate_bmi_category(age = age + 2, sex = sex, bmi = bmi_730, df_B = uk90_bmi_refdata),
           bmi_cat_1095 = calculate_bmi_category(age = age + 3, sex = sex, bmi = bmi_1095, df_B = uk90_bmi_refdata),
           bmi_cat_1460 = calculate_bmi_category(age = age + 4, sex = sex, bmi = bmi_1460, df_B = uk90_bmi_refdata),
           bmi_cat_1825 = calculate_bmi_category(age = age + 5, sex = sex, bmi = bmi_1825, df_B = uk90_bmi_refdata)) %>%
    ungroup()
  
  
  output_list[["post_df"]] = df_child # Dataframe added to outputs list
  
  
  child_bmi_change = rbind(
    df_child %>% 
      count(bmi_cat_1, wt = wt_int) %>% 
      mutate(freq = n/sum(n)*100,
             type = "Baseline") %>% 
      rename(BMI = bmi_cat_1),
    df_child %>% 
      count(bmi_cat_365, wt = wt_int) %>% 
      mutate(freq = n/sum(n)*100,
             type = "Year 1") %>% 
      rename(BMI = bmi_cat_365),
    df_child %>% 
      count(bmi_cat_730, wt = wt_int) %>% 
      mutate(freq = n/sum(n)*100,
             type = "Year 2") %>% 
      rename(BMI = bmi_cat_730),
    df_child %>% 
      count(bmi_cat_1095, wt = wt_int) %>% 
      mutate(freq = n/sum(n)*100,
             type = "Year 3") %>% 
      rename(BMI = bmi_cat_1095),
    df_child %>% 
      count(bmi_cat_1460, wt = wt_int) %>% 
      mutate(freq = n/sum(n)*100,
             type = "Year 4") %>% 
      rename(BMI = bmi_cat_1460),
    df_child %>% 
      count(bmi_cat_1825, wt = wt_int) %>% 
      mutate(freq = n/sum(n)*100,
             type = "Year 5") %>% 
      rename(BMI = bmi_cat_1825))
  
  child_bmi_change = child_bmi_change %>%
    mutate(BMI = factor(BMI, levels = c("underweight", "normal", "overweight", "obese"))) %>%
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
