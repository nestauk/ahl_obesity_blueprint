
##########################################################
# Script for function required to run the models.        #
##########################################################


# Note: As of now only the child modelling related functions are written here, in subsequent fixes,
# all supporting functions called for implementing the Hall model will be moved here.

# References:

# 1. SACN (2011). Dietary Reference Values for Energy 2011. [online] Available at: 
#    https://assets.publishing.service.gov.uk/media/5a7edb37ed915d74e33f2d8f/SACN_Dietary_Reference_Values_for_Energy.pdf.
#
# 2. Dalia Camacho-Garcia-Formenti and Rodrigo Zepeda-Tello (2018). bw: Dynamic Body Weight Models for Children and
#    Adults. R package version 1.0.0.
#
# 3. NHS England (2023). National Child Measurement Programme, England, 2022/23 School Year. [online] NHS Digital. 
#    Available at: https://digital.nhs.uk/data-and-information/publications/statistical/national-child-measurement-programme/2022-23-school-year/introduction.
#
# 4. Chapman, S., Baw, M., & Cole, T. (2022). rcpchgrowth (Version 4.2.8) [Computer software]. https://doi.org/10.5281/zenodo.6303587
#
# 5. Cole, T. (2023). statist7/sitar. GitHub. Available at: https://github.com/statist7/sitar [Accessed 27 Feb. 2024].
#
# 6. Cole TJ. (2012). The development of growth references and growth charts. Ann Hum Biol. 2012 Sep;39(5):382-94. 
#    doi: 10.3109/03014460.2012.694475. Epub 2012 Jul 11. PMID: 22780429; PMCID: PMC3920659.



library(tidyverse)
library(here)
library(bw)
library(survey)
library(sitar)
library(jsonlite)



# 1. FUNCTION 1: Look up energy intake
# function to look-up energy intake for children as they grow (age)
# INPUTS:
# Function takes age, sex and reference data as input,
# the function the looks up the reference data to find the incremental energy intake  required for 
# the child to grow. The look-up table is generated based on the Estimated Average Requirement (EAR)
# estimated and published in Table 8 (SACN, 2011). All values refer to daily energy intake reported in
# kcals per day.

# OUTPUTS:
# Output is the value of the updated daily energy intake for age and sex in kcals.

lookup_energy_intake = function(age, sex, data_B){
  #browser()
  # checks that the inout is for a child i.e. less than 19 years of age
  if (age <19){
    
    age = ceiling(age) # round up the age as the published DRI values are for integer ages, eg. child is 5.5, it rounds up to 6.
    age_row_prev <- data_B[data_B$age == age - 1 & data_B$sex == sex, ] # get row for the previous age, eg. datarow for age 5
    age_row <- data_B[data_B$age == age & data_B$sex == sex, ] # get row for current age, eg. datarow for age 6
    
    ei_age_prev = age_row_prev$population_kcal # get the recommended DRI for previous age
    ei_age = age_row$population_kcal # get recommended DRI for current age
    
    # get population level mean excess energy intake for combinations of age and sex
    # excess energy intake values for sex and age group taken from Calorie Reformulation Report (DHSC - UK Gov, 2017)
    ei_excess = case_when(age >= 5 & age <= 10 & sex == 1 ~ 21,
                          age >= 11 & age <= 15 & sex == 1 ~ 69,
                          age >= 16 & age <= 18 & sex == 1 ~ 104,
                          age >= 5 & age <= 10 & sex == 2 ~ 34,
                          age >= 11 & age <= 15 & sex == 2 ~ 63,
                          age >= 16 & age <= 18 & sex == 2 ~ 44)
    
    ei_excess = 0 # set to zero for now as we decide who should receive excess calorie intake - specific BMI groups or all children
    
    net_energy_intake = (ei_age - ei_age_prev) + ei_excess # net increment in energy intake for child growth
  } else{
    # incase of children aged 19 and above, set incremental energy intake for growth = 0 kcals
    net_energy_intake = 0
    
  }
  return(net_energy_intake)
  
}

# FUNCTION 2: Generate SACN Estimated Average Requirements (EAR) age and sex wise reference data 

# INPUTS:
# input to the function is the path to a csv file store in the input refdata files.
# this csv was generated from Table 8 in Dietary Reference Values for Energy (SACN, 2011)

# OUTPUTS:
# Function reads a csv and returns a dataframe.

generate_sacn_dietary_intake = function(input_path){
  
  df = read_csv(input_path)
  
  return(df)
  
}


# FUNCTION 3: Generate Energy Matrix:
# function to generate the daily energy intake matrix for five years for children based on
# daily energy intake provided at day 0, 365, 730, 1095, 1460 and 1825 of the intervention
# which corresponds to baseline and end of each one of the five years of the intervention
# Within the function, we call 'energy_build' (Camacho-Garcia-Formenti & Zepeda-Tello, 2018)
# function written as part of the 'bw' package. The 'Brownian' interpolation is used as it 
# is thought to represent typical energy intake patterns in comparison to 'Linear', 
# 'Exponential', 'Stepwise', 'Lograthmic' etc.

# INPUTS:
# input to the matrix is a row that contains energy intakes at points in time listed above.

# OUTPUTS:
# output is a dataframe where each column represents an individual and each row represents
# a day in five years.

generate_interpolated_energy <- function(row) {
  #browser()
  
  # values in the row are added to a list
  energy_values <- c(as.numeric(row["intake_hox"]),
                     as.numeric(row["ei_365"]),
                     as.numeric(row["ei_730"]),
                     as.numeric(row["ei_1095"]),
                     as.numeric(row["ei_1460"]),
                     as.numeric(row["ei_1825"]))
  
  # energy build expects a list of energy values, along with time points (in days) and interpolation method
  interpolated_energy <- energy_build(energy_values, c(0, 365, 730, 1095, 1460, 1825), interpolation = "Brownian")
  
  return(interpolated_energy)
  
}

# FUNCTION 4: Look up projected height:

# INPUTS:
# function to estimate the future height of the child given current age, sex, height, years
# grown, and reference data. 


# The function takes the input age and sex and filters the ref 
# data to return rows with height and percentile values. Then compares the input height to
# each of the heights in the filtered row to return the percentile it matches the closest.
# This is the percentile at baseline. We assume that the child grows along this percentile.
# Reference data is available for until age 20 (inclusive). For future ages above that, we
# that the child's height stays the same as at 20.

# OUTPUTS:
# The function returns the value of the projected height at a different age.

#test_ht = lookup_projected_height(age = 6, sex = 2, height = 104.3, data_B = height_refdata_100centile, years_added = 1)
#test_ht_1 = lookup_projected_height(age = 6, sex = "female", height = 104.3, data_B = height_refdata_100centile, years_added = 1)

lookup_projected_height <- function(age, sex, height, data_B, years_added) {
  # browser()
  # recoding sex to numeric value from character
  if (sex == "female" | sex == 2){
    sex = 2
  } else { if(sex == "male" | sex == 1){
    
    sex = 1
    
  } }
  
  
  age_row <- data_B[data_B$age == age & data_B$sex == sex, ]
  
  closest_percentile_index <- which.min(abs(age_row$height - height))
  closest_percentile <- age_row$centile[closest_percentile_index]
  
  
  age_future <- age + years_added
  
  if (age_future <= 20){
    
    projected_height <- data_B$height[data_B$age == age_future & data_B$centile == closest_percentile & data_B$sex == sex]
    
    
  } else{   if(age_future == 21) {
    
    
    projected_height <- data_B$height[data_B$age == (age_future - 1) & data_B$centile == closest_percentile & data_B$sex == sex] 
    
    
  } else{ if(age_future == 22){
    
    projected_height <- data_B$height[data_B$age == (age_future - 2) & data_B$centile == closest_percentile & data_B$sex == sex]   
    
  } else{ if(age_future == 23){
    
    projected_height <- data_B$height[data_B$age == (age_future - 3) & data_B$centile == closest_percentile & data_B$sex == sex]
    
  }
    
  }
    
  }
    
  }
  
  return(projected_height)
}



# FUNCTION 5: Calculate BMI category:

# Function to calculate the BMI category of a child given the BMI value, age and sex
# by comparing it to reference values generated using LMS method from UK90 growth 
# charts. The cutoffs used are as per NCMP (NHS England, 2023).

# INPUTS:
# Inputs to the function are age, sex, bmi and reference table

# OUTPUTS:
# output is BMI category - underweight, normal, overweight and obese (NHS England, 2023).
# If the input age is over 20 years, then the adult thresholds are used for classification.


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


# FUNCTION 6: Generate height reference data

# INPUTS:
# function that takes two JSON files and combines them to create a dataframe. The two inputs are
# JSON files of UK90 height reference data taken from RCPCH digital growth charts repo for male
# and female (Chapman, Baw & Cole, 2022). They files can be accessed here:
# https://github.com/rcpch/digital-growth-charts-server/tree/live/chart-data
# The JSON files have been downloaded from the link and stored in inputs\ref_data:
# cole-nine-centiles-uk-who-female-height.json
# cole-nine-centiles-uk-who-male-height.json
# These files contain for males and females - z-score, centile, age and height

# OUTPUTS:
# The function outputs a dataframe where for each age and sex, centile wise height is recorded.


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
  
  height_refdata = rbind(female_ht_df_1, male_ht_df_1) %>%
    rename(age = x,
           height = y)
  
  return(height_refdata)
  
}

# FUNCTION 7: Generate BMI Reference Data using UK90 growth charts
# INPUTS:
# The function takes a dataframe as input which contains for each combination of age and sex,
# L, M and S values are recorded. The dataframe is taken from 'sitar' package (Cole, 2023)
# which contains a function 'uk90' (https://rdrr.io/cran/sitar/man/uk90.html) that is a dataframe
# which matches the necessary input format required by the function.
# The LMS formula to calculate z-score and thereby percentile is described in Cole (2012) as below:
# z = ((X / M) ^ L - 1) / (L * S)  
# where z is z-score; X is a measurement (of bmi, height, weight etc); L, M & S are parameters that summarise
# the normal distribution generated from a Box-Cox transformation of measurements at each age (Cole, 2012).
# This formula is then inverted to calculate the measurement (X) given that we know the L, M and S for each
# age and sex and also know the z-scores for which we would like to calculate the measurements.
# The formula used to calculate measurement (X) in the function is:
# X = M * (1 + Z * L * S) ^ (1 / L)


# OUTPUT:
# The function returns a dataframe with age and sex wise bmi values for the threshold percentiles
# for child BMI category cut-offs - 2nd, 85th and 95t as per NCMP (NHS England, 2023)

generate_bmi_refdata = function(data_B){
  
  bmi_refdata = data_B %>%
    select(years, sex, L.bmi, M.bmi, S.bmi) %>%
    subset(years >= as.double(4) & years <= as.double(20)) %>% # filtered to limit to ages between 4 and 20, both inclusive
    mutate(p_2 = (M.bmi*(1 + L.bmi*S.bmi*-2.054)^(1/L.bmi)),
           p_85 = (M.bmi*(1 + L.bmi*S.bmi*1.036)^(1/L.bmi)),
           p_95 = (M.bmi*(1 + L.bmi*S.bmi*1.645)^(1/L.bmi))) %>%
    select(years, sex, p_2, p_85, p_95) %>%
    rename(age = years)
  
  return(bmi_refdata)
  
  
}

# function to generate bmi ref data

generate_bmi_refdata_100centiles = function(data_B){
  
  #browser()
  
  bmi_refdata = data_B %>%
    select(years, sex, L.bmi, M.bmi, S.bmi) %>%
    subset(years >= as.double(4) & years <= as.double(20))

  for( x in 1:100){
    
    if (x <= 99) {
      
      x = x/100
      
     
        
        bmi_refdata = bmi_refdata %>%
          mutate(!!paste0("p","_",(x*100)) := M.bmi*(1 + L.bmi*S.bmi*qnorm(x))^(1/L.bmi))
          # rowwise() %>%
          # mutate(!!paste0("p","_",(x*100)) := M.bmi[y]*(1 + L.bmi[y]*S.bmi[y]*qnorm(x))^(1/L.bmi[y]))
        
    
        
     
      
    } else{
      #browser()
      x = 0.996
      
      bmi_refdata = bmi_refdata %>%
        mutate(!!paste0("p","_",(x*100)) := M.bmi*(1 + L.bmi*S.bmi*qnorm(x))^(1/L.bmi))
      
    }
  }
  
  return(bmi_refdata)
  
}



# function to generate height ref data

generate_height_refdata_100centiles = function(data_B){

  #browser()
  
  ht_refdata = data_B %>%
    select(years, sex, L.ht, M.ht, S.ht) %>%
    subset(years >= as.double(4) & years <= as.double(20))
  
  for( x in 1:100){
    
    if (x <= 99) {
      
      x = x/100
      
      
      
      ht_refdata = ht_refdata %>%
        mutate(!!paste0("p","_",(x*100)) := M.ht*(1 + L.ht*S.ht*qnorm(x))^(1/L.ht))
      # rowwise() %>%
      # mutate(!!paste0("p","_",(x*100)) := M.bmi[y]*(1 + L.bmi[y]*S.bmi[y]*qnorm(x))^(1/L.bmi[y]))
      
      
      
      
      
    } else{
      #browser()
      x = 0.996
      
      ht_refdata = ht_refdata %>%
        mutate(!!paste0("p","_",(x*100)) := M.ht*(1 + L.ht*S.ht*qnorm(x))^(1/L.ht))
      
    }
  }
  
  return(ht_refdata)

} 





# function to lookup projected bmi

lookup_projected_bmi <- function(age, sex, bmi, data_B, years_added) {
  
  # recoding sex to numeric value from character
  if (sex == "female"){
    sex = 2
  } else { sex = 1}
  
  #browser()
  age_row <- data_B[data_B$age == age & data_B$sex == sex, ]
  
  closest_percentile_index <- which.min(abs(age_row$bmi - bmi))
  closest_percentile <- age_row$centile[closest_percentile_index]
  
  
  age_future <- age + years_added
  
  if (age_future <= 20){
    
    projected_bmi <- data_B$bmi[data_B$age == age_future & data_B$centile == closest_percentile & data_B$sex == sex]
    
    
  } else{   if(age_future == 21) {
    
    projected_bmi <- data_B$bmi[data_B$age == (age_future - 1) & data_B$centile == closest_percentile & data_B$sex == sex]
    # projected_bmi <- data_B$y[data_B$x == (age_future - 1) & data_B$centile == closest_percentile & data_B$sex == sex] 
    
    
  } else{ if(age_future == 22){
    
    projected_bmi <- data_B$bmi[data_B$age == (age_future - 2) & data_B$centile == closest_percentile & data_B$sex == sex]
    # projected_height <- data_B$y[data_B$x == (age_future - 2) & data_B$centile == closest_percentile & data_B$sex == sex]   
    
  } else{ if(age_future == 23){
    
    projected_bmi <- data_B$bmi[data_B$age == (age_future - 3) & data_B$centile == closest_percentile & data_B$sex == sex]
    # projected_height <- data_B$y[data_B$x == (age_future - 3) & data_B$centile == closest_percentile & data_B$sex == sex]
    
  }
    
  }
    
  }
    
  }
  
  return(projected_bmi)
}



# t1 = lookup_projected_height(age = 6, sex = "female", height = 104.3, data_B = uk90_height_refdata, years_added = 1 )

