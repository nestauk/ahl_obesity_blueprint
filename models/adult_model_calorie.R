library(tidyverse)
library(here)
library(bw)
library(survey)

# function to implement the Hall Model and generate outputs
# The Function takes as input 1) df : data frame with weight, height, bmi, sex, daily energy intake etc
#                             2) intake_change : change in daily energy intake
#                             3) implmentation_duration : duration of implementation of policy

# The function returns four outputs: 1) hall_model_op : Hall Model outputs
#                                    2) post_df : Dataframe with individual level BMI's in the intervention years
#                                    3) bmi_category_plot : Plot of year on year BMI category distribution
#                                    4) bmi_percent_prevalence : Table of year wise percentage prevalence of each BMI category


calculate_bmi_from_eichange = function(df, intake_change, implmentation_duration ) {
  
  output_list = list() # creating a list to store outputs to be returned from the function
  
  df = df %>%  # processed dataset with variables such as height, weight, bmi, daily energy intake passed as input to the function 
    mutate(sex = ifelse(sex == 1, "male", "female")) %>%  # modifying the variable sex in a form expected by the Hall Model
    mutate(intake_diff = ifelse(bmi_class %in% c("underweight", "normal"), 0, intake_change)) %>% # creating a variable to store the change in energy intake due to intervention and applying it only to those with BMI >= 25
    mutate(intervention = ifelse(bmi_class %in% c("underweight", "normal"), "No", "Yes"))
  
  eichange <- t(apply(df, 1, function(x) rep(as.numeric(x["intake_diff"]), implmentation_duration))) # creating the matrix (for the duration of implementation of the policy) with change in daily energy intake due to the policy
  
  nachange <- t(apply(df, 1, function(x) rep(0, implmentation_duration))) # creating the matrix (for the duration of implementation of the policy) with change in daily sodium intake due to the policy set to zero '0'
  
  # Implementing the Hall model described in Hall et al. (2011). Inputs to the model have self explanatory variable names
  model_weight <- adult_weight(bw = df$weight,
                               ht = df$height/100,
                               age = df$age,
                               sex = df$sex,
                               EIchange = eichange,
                               NAchange = nachange,
                               days = implmentation_duration)
  
  output_list[["hall_model_op"]] = model_weight # Hall Models output is added to the list
  
  bmi_model = model_weight[["Body_Mass_Index"]] # Extracting the table with body mass index from Hall Model outputs
  post_df_adult = cbind(df, bmi_model) # Combining the extracted body mass index table to the input dataframe
  post_df_adult_1 = cbind(df, bmi_model)
  output_list[["individual_full_bmi_matrix"]] = post_df_adult_1
  
  # Selecting variables of interest. The dataframe created in the previous line contains bmi values for each day of the implementation duration.
  # We are interested only in the BMI values at the end of each intervention year to estimate the distribution of bodyweights
  # the BMI values at the end of Year 1, Year 2, Year 3, Year 4 and Year 5 are collected and labelled with BMI categories.
  post_df_adult = post_df_adult %>%
    select("id", "weight", "height", "age", "sex", "bmi", "wt_int", "psu", "intervention",
           "strata", "pal", "rmr", "bmi_class", "intake", "intake_diff", "1", "365", "730", "1095", "1460", "1825" ) %>%
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
  
  output_list[["post_df"]] = post_df_adult # Dataframe added to outputs list
  
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
  
  output_list[["bmi_category_plot"]] = adult_bar_plot # BMI category distribution plot is added to the outputs list
  
  # Output 2: Table of year wise percentage prevalence of each BMI category
  bmi_change_year = bmi_change %>%
    select(-c(n)) %>%
    pivot_wider(., names_from = BMI, values_from = freq)%>%
    select(type, underweight, normal, overweight, obese, `morbidly obese`) 
  
  
  output_list[["bmi_percent_prevalence"]] = bmi_change_year # Year wise BMI category percentage prevalance table is added to outputs list
  
  return(output_list) # outputs are returned
  
}


