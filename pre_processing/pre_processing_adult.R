library(here)
library(tidyverse)
library(dplyr)

#References:
# 1. Hudda, M.T., Fewtrell, M.S., Haroun, D., Lum, S., Williams, J.E., Wells, J.C.K., Riley, R.D.,
# Owen, C.G., Cook, D.G., Rudnicka, A.R., Whincup, P.H., Nightingale, C.M., 2019. Development and 
# validation of a prediction model for fat mass in children and adolescents: meta-analysis using 
# individual participant data. BMJ l4293. https://doi.org/10.1136/bmj.l4293

# 2. McCarthy, H.D., Cole, T.J., Fry, T., Jebb, S.A., Prentice, A.M., 2006. Body fat reference curves 
# for children. Int J Obes 30, 598–602. https://doi.org/10.1038/sj.ijo.0803232

# 3. SACN (2011). Dietary Reference Values for Energy 2011. [online] Available at:
# https://assets.publishing.service.gov.uk/media/5a7edb37ed915d74e33f2d8f/SACN_Dietary_Reference_Values_for_Energy.pdf.


# function to clean and save the raw health survey data for both adults and children

process_clean_save = function(file_path, nation, population_group){
  
  if (nation %in% c("England", "Scotland") & population_group %in% c("Adult", "Children")) {
    
    # body fat reference curves to estimate BMI categories of children from McCarthy et al. (2006)
    body_fat_reference_tables = read_csv(file = here("inputs/body_fat_reference_tables.csv")) 
    
    # function to calculate fat mass given weight, height, ethnicity, age and sex from Hudda et al. (2019)
    calculate_fat_mass <- function(weight, height, BA, SA, AO, other, age, male) {
      fat_mass = weight - exp(0.3073 * height^2 - 10.0155 * weight^-1 + 
                                0.004571 * weight + 0.01408 * BA - 
                                0.06509 * SA - 0.02624 * AO - 0.01745 * other 
                              - 0.9180 * log(age) + 0.6488 * sqrt(age) + 0.04723 * male + 2.8055)
      return(fat_mass)
    }
    
    
    if (nation == "England" & population_group == "Adult") {
      
      # Age35g is a categorical variable of 5 year age bands for 16+, smallest possible grouping from HSE 2019
      # Age35g == 8 indicates all those in age group 20-24 years
      df_2019_adult <- read.table(here(file_path), sep = "\t", header = TRUE) %>% 
        filter(WtVal>0 & HtVal>0 & Age35g >7 ) %>% # remove missing height and weight and children; 
        mutate(age = case_when(#Age35g == 7 ~ (16+19)/2,
          Age35g == 8 ~ (20+24)/2, 
          Age35g == 9 ~ (25+29)/2,
          Age35g == 10 ~ (30+34)/2,
          Age35g == 11 ~ (35+39)/2,
          Age35g == 12 ~ (40+44)/2,
          Age35g == 13 ~ (45+49)/2,
          Age35g == 14 ~ (50+54)/2,
          Age35g == 15 ~ (55+59)/2,
          Age35g == 16 ~ (60+64)/2,
          Age35g == 17 ~ (65+69)/2,
          Age35g == 18 ~ (70+74)/2,
          Age35g == 19 ~ (75+79)/2,
          Age35g == 20 ~ (80+84)/2,
          Age35g == 21 ~ (85+89)/2,
          Age35g == 22 ~ (90),
          TRUE ~ 0)) %>%
        mutate(age_grp = case_when(#Age35g == 7 ~ (16+19)/2,
          Age35g == 8 ~ "20-24", 
          Age35g == 9 ~ "25-29",
          Age35g == 10 ~ "30-34",
          Age35g == 11 ~ "35-39",
          Age35g == 12 ~ "40-44",
          Age35g == 13 ~ "45-49",
          Age35g == 14 ~ "50-54",
          Age35g == 15 ~ "55-59",
          Age35g == 16 ~ "60-64",
          Age35g == 17 ~ "65-69",
          Age35g == 18 ~ "70-74",
          Age35g == 19 | Age35g == 20 | Age35g == 21 | Age35g == 22  ~ "75+",
          TRUE ~ "NA")) %>%
        rename(weight = WtVal,
               height = HtVal,
               sex = Sex,
               bmi = BMIVal,
               id = SerialA,
               psu = PSU_SCR,
               strata = cluster94) %>% 
        dplyr::select(id, weight, height, age_grp, age, sex, bmi, wt_int, psu, strata )  %>% # select variables needed
        mutate(pal = 1.6, # pal assumed to be 1.6 for the entire population to indicate a sendentary/ light active lifestyle
               rmr = case_when(sex == 1 ~ ((10 * weight) + (6.25 * height) - (5 * age) + 5),
                               TRUE ~ ((10 * weight) + (6.25 * height) - (5 * age) - 161))) %>% # sex = 2 female; rmr is calculated using Mifflin St Jeor Equations from Mifflin et al (1990)
        mutate(bmi_class = case_when(bmi <= 18.5 ~ "underweight",
                                     bmi > 18.5 & bmi < 25 ~ "normal",
                                     bmi >= 25 & bmi < 30 ~ "overweight",
                                     bmi >= 30 & bmi < 40 ~ "obese",
                                     bmi >= 40 ~ "morbidly obese",
                                     TRUE ~ "NA")) %>% 
        mutate(intake = pal*rmr)  # calculating value of energy intake 
      
      write_csv(df_2019_adult, here("inputs/processed/hse_2019.csv"))
      print("Output csv with processed data is saved here: inputs/processed/hse_2019.csv" )
      

    } else if (nation == "Scotland" & population_group == "Adult") {
      
      
      # Scotland
      
      # select(CPSerialA, SYear,PSU, Strata, int19wt, cint19wt, bmival, htval, wtval, BMIvg5, CBMIg5_new, age, Sex, SIMD20_SGa, Ethnic05, totinc, eqv5_15, hedqul08)
      
      # The data used for this analysis is Scottish Health Survey 2019 available on UK Data Service portal.
      # First, the dataset is filtered by weight, height and age to ensure that all observations have a height and weight > 0 and is limited to Adults (age>= 18)
      # Subsequently, variables are renamed for ease and further subset to include only the variables of interest for modelling obesity prevalance.
      
      # The next step is to calculate the energy intake at baseline. This energy intake can be calculated by multiplying an individuals physical activity level (PAL) and
      # basal metabolic rate (BMR). PAL values are available for different levels of physical activity but on average for the population we have assumed everyone to be
      # engaged in light work (PAL = 1.6). BMR is assumed to be equal to resting metabolic rate (RMR) which is the energy cost of maintaining metabolic homeostasis.
      
      # At the baseline, in that point in time, we assume that people are maintaining their weight, therefore, their energy intake equals the energy requirement for
      # physical activities and for metabolic homeostasis, which is given by the equation: PAL * BMR
      
      df_2019_adult <- read.table(here(file_path), sep = "\t", header = TRUE) %>% # reading in the raw data file into a dataframe
        filter(wtval>0 & htval>0 & age >= 18) %>% # removing observations with missing height and weight information as well as children as this is an adult model
        rename(weight = wtval,
               height = htval,
               sex = Sex,
               bmi_ur = bmi,
               bmi = bmival,
               id = CPSerialA,
               psu = PSU,
               wt_int = int19wt,
               strata = Strata) %>%
        mutate(age_grp = case_when(age >= 20 & age <= 24 ~ "20-24",
                                   age >= 25 & age <= 29 ~ "25-29",
                                   age >= 30 & age <= 34 ~ "30-34",
                                   age >= 35 & age <= 39 ~ "35-39",
                                   age >= 40 & age <= 44 ~ "40-44",
                                   age >= 45 & age <= 49 ~ "45-49", 
                                   age >= 50 & age <= 54 ~ "50-54",
                                   age >= 55 & age <= 59 ~ "55-59",
                                   age >= 60 & age <= 64 ~ "60-64",
                                   age >= 65 & age <= 69 ~ "65-69",
                                   age >= 70 & age <= 74 ~ "70-74",
                                   age >= 75 ~ "75+",
                                   TRUE ~ "NA")) %>%
        dplyr::select(id, weight, height, age_grp, age, sex, bmi, wt_int, psu, strata) %>% # select variables needed for analysis and would be inputs for modelling
        mutate(pal = 1.6,
               rmr = case_when(sex == 1 ~ ((10 * weight) + (6.25 * height) - (5 * age) + 5),
                               TRUE ~ ((10 * weight) + (6.25 * height) - (5 * age) - 161))) %>% # Calculating an individuals resting metabolic rate using equations published in Mifflin & St.Jeor (1990)
        mutate(intake = pal*rmr) %>%  # calculating energy intake at baseline
        mutate(bmi_class = case_when(bmi <= 18.5 ~ "underweight",
                                     bmi > 18.5 & bmi < 25 ~ "normal",
                                     bmi >= 25 & bmi < 30 ~ "overweight",
                                     bmi >= 30 & bmi < 40 ~ "obese",
                                     bmi >= 40 ~ "morbidly obese",
                                     TRUE ~ "NA"))  # classifying individuals into BMI categories based on BMI values at baseline
      
      write_csv(df_2019_adult, here("inputs/processed/shes_2019.csv")) # writing the output dataframe to a csv file to be read in later.
      print("Output csv with processed data is saved here: inputs/processed/shes_2019.csv" )
      
    } else if (nation == "England" & population_group == "Children"){
      
      
      # Age35g is a categorical variable of 3 year age bands for 0-15 year olds, smallest possible grouping from HSE 2019
      # Age35g ==  indicates all those in age group 20-24 years
      df_2019_children <- read.table(here(file_path), sep = "\t", header = TRUE) %>%
        filter(WtVal>0 & HtVal>0 & Age35g > 2 & Age35g < 8) %>% # age is set to 5 - 18 years old as the Hall Model is only validted for that age group
        mutate(age = case_when(Age35g == 1 ~ (0+1)/2,   # 0.5
                               Age35g == 2 ~ (2+4)/2,   # 3
                               Age35g == 3 ~ (5+7)/2,   # 6
                               Age35g == 4 ~ (8+10)/2,  # 9
                               Age35g == 5 ~ (11+12)/2, # 11.5
                               Age35g == 6 ~ (13+15)/2, # 14
                               Age35g == 7 ~ (16+18)/2, # 17
                               TRUE ~ 0)) %>%
        mutate(age_grp = case_when(#Age35g == 7 ~ (16+19)/2,
          Age35g == 1 ~ "0-1", 
          Age35g == 2 ~ "2-4",
          Age35g == 3 ~ "5-7",
          Age35g == 4 ~ "8-10",
          Age35g == 5 ~ "11-12",
          Age35g == 6 ~ "13-15",
          Age35g == 7 ~ "16-19",
          TRUE ~ "NA"))
      
      
      df_2019_children = df_2019_children %>%
        rename(weight = WtVal,
               height = HtVal,
               sex = Sex,
               bmi = BMIVal,
               id = SerialA,
               psu = PSU_SCR,
               strata = cluster94) %>% 
        dplyr::select(id, weight, height, age_grp, age, sex, bmi, wt_int, psu, strata, origin2 )  %>% # select variables needed
        mutate(pal = case_when(age < 3 ~ 1.40, 
                               age >=3 & age < 10 ~ 1.58,
                               age >=10 & age < 18 ~ 1.75), # pal set based on SACN (2011) guidelines
               rmr_hox = case_when(sex == 1 ~ (  ((66.9*weight) + 2876)/4.184  ),
                                   TRUE ~ ( ((47.9*weight) + 3230)/4.184 ) )) %>% # rmr for children calculated using Henry Oxford equations
        mutate(intake_hox = pal*rmr_hox) %>% # calculating value of energy intake and fat mass is calculated using Hudda et al (2019)
        mutate(fm_hudda = case_when(sex == 1 ~ (case_when(origin2 == 1 ~ (calculate_fat_mass(weight = weight, 
                                                                                             height = height/100,
                                                                                             BA = 0,
                                                                                             SA = 0,
                                                                                             AO = 0,
                                                                                             other = 0,
                                                                                             age = age,
                                                                                             male = 1)),
                                                          origin2 == 2 ~ (calculate_fat_mass(weight = weight, 
                                                                                             height = height/100,
                                                                                             BA = 1,
                                                                                             SA = 0,
                                                                                             AO = 0,
                                                                                             other = 0,
                                                                                             age = age,
                                                                                             male = 1)),
                                                          origin2 == 3 ~ (calculate_fat_mass(weight = weight, 
                                                                                             height = height/100,
                                                                                             BA = 0,
                                                                                             SA = 1,
                                                                                             AO = 0,
                                                                                             other = 0,
                                                                                             age = age,
                                                                                             male = 1)),
                                                          origin2 == 4 | 5 ~ (calculate_fat_mass(weight = weight, 
                                                                                                 height = height/100,
                                                                                                 BA = 0,
                                                                                                 SA = 0,
                                                                                                 AO = 0,
                                                                                                 other = 1,
                                                                                                 age = age,
                                                                                                 male = 1)))),
                                    sex == 2 ~ (case_when(origin2 == 1 ~ (calculate_fat_mass(weight = weight, 
                                                                                             height = height/100,
                                                                                             BA = 0,
                                                                                             SA = 0,
                                                                                             AO = 0,
                                                                                             other = 0,
                                                                                             age = age,
                                                                                             male = 0)),
                                                          origin2 == 2 ~ (calculate_fat_mass(weight = weight, 
                                                                                             height = height/100,
                                                                                             BA = 1,
                                                                                             SA = 0,
                                                                                             AO = 0,
                                                                                             other = 0,
                                                                                             age = age,
                                                                                             male = 0)),
                                                          origin2 == 3 ~ (calculate_fat_mass(weight = weight, 
                                                                                             height = height/100,
                                                                                             BA = 0,
                                                                                             SA = 1,
                                                                                             AO = 0,
                                                                                             other = 0,
                                                                                             age = age,
                                                                                             male = 0)),
                                                          origin2 == 4 | 5 ~ (calculate_fat_mass(weight = weight, 
                                                                                                 height = height/100,
                                                                                                 BA = 0,
                                                                                                 SA = 0,
                                                                                                 AO = 0,
                                                                                                 other = 1,
                                                                                                 age = age,
                                                                                                 male = 0)))))) %>%
        mutate(ffm_hudda = weight - fm_hudda) 
      
      write_csv(df_2019_children, here("inputs/processed/hse_2019_children.csv")) 
      print("Output csv with processed data is saved here: inputs/processed/hse_2019_children.csv")
      
    } else if (nation == "Scotland" & population_group == "Children") {
      
      df_2019_child_sc <- read.table(here(file_path), sep = "\t", header = TRUE) %>%
        filter(wtval>0 & htval>0 & age >= 5 & age <= 18 & !is.na(cint19wt)) %>%
        rename(weight = wtval,
               height = htval,
               sex = Sex,
               bmi_ur = bmi,
               bmi = bmival,
               id = CPSerialA,
               psu = PSU,
               wt_int = cint19wt,
               origin2 = Ethnic05,
               strata = Strata) %>%
        mutate(age_grp = case_when(age >= 5 & age <= 7 ~ "5-7",
                                   age >= 8 & age <= 10 ~ "8-10",
                                   age >= 11 & age <= 12 ~ "11-12",
                                   age >= 13 & age <= 15 ~ "13-15",
                                   age >= 16 & age <= 19 ~ "16-19",
                                   TRUE ~ "NA")) %>%
        dplyr::select(id, weight, height, age_grp, age, sex, bmi, wt_int, psu, strata, origin2) %>% # select variables needed
        mutate(pal = case_when(age < 3 ~ 1.40,
                               age >=3 & age < 10 ~ 1.58,
                               age >=10 & age < 18 ~ 1.75),
               rmr_hox = case_when(sex == 1 ~ (  ((66.9*weight) + 2876)/4.184  ),
                                   TRUE ~ ( ((47.9*weight) + 3230)/4.184 ) )) %>% 
        mutate(intake_hox = pal*rmr_hox) %>% # calculating value of energy intake 
        mutate(fm_hudda = case_when(sex == 1 ~ (case_when(origin2 == 1 | 2 | 3 ~ (calculate_fat_mass(weight = weight, 
                                                                                                     height = height/100,
                                                                                                     BA = 0,
                                                                                                     SA = 0,
                                                                                                     AO = 0,
                                                                                                     other = 0,
                                                                                                     age = age,
                                                                                                     male = 1)),
                                                          origin2 == 4 ~ (calculate_fat_mass(weight = weight, 
                                                                                             height = height/100,
                                                                                             BA = 0,
                                                                                             SA = 0,
                                                                                             AO = 1,
                                                                                             other = 0,
                                                                                             age = age,
                                                                                             male = 1)),
                                                          origin2 == 5 ~ (calculate_fat_mass(weight = weight, 
                                                                                             height = height/100,
                                                                                             BA = 0,
                                                                                             SA = 0,
                                                                                             AO = 0,
                                                                                             other = 1,
                                                                                             age = age,
                                                                                             male = 1)),
                                                          origin2 == -1 ~ (calculate_fat_mass(weight = weight, 
                                                                                              height = height/100,
                                                                                              BA = 0,
                                                                                              SA = 0,
                                                                                              AO = 0,
                                                                                              other = 0,
                                                                                              age = age,
                                                                                              male = 1)))),
                                    sex == 2 ~ (case_when(origin2 == 1 | 2 | 3 ~ (calculate_fat_mass(weight = weight, 
                                                                                                     height = height/100,
                                                                                                     BA = 0,
                                                                                                     SA = 0,
                                                                                                     AO = 0,
                                                                                                     other = 0,
                                                                                                     age = age,
                                                                                                     male = 0)),
                                                          origin2 == 4 ~ (calculate_fat_mass(weight = weight, 
                                                                                             height = height/100,
                                                                                             BA = 0,
                                                                                             SA = 0,
                                                                                             AO = 1,
                                                                                             other = 0,
                                                                                             age = age,
                                                                                             male = 0)),
                                                          origin2 == 5 ~ (calculate_fat_mass(weight = weight, 
                                                                                             height = height/100,
                                                                                             BA = 0,
                                                                                             SA = 1,
                                                                                             AO = 0,
                                                                                             other = 0,
                                                                                             age = age,
                                                                                             male = 0)),
                                                          origin2 == -1 ~ (calculate_fat_mass(weight = weight, 
                                                                                              height = height/100,
                                                                                              BA = 0,
                                                                                              SA = 0,
                                                                                              AO = 0,
                                                                                              other = 0,
                                                                                              age = age,
                                                                                              male = 0)))))) %>%
        mutate(ffm_hudda = weight - fm_hudda)
      
      write_csv(df_2019_child_sc, here("inputs/processed/shes_2019_children.csv"))
      print("Output csv with processed data is saved here: inputs/processed/shes_2019_children.csv")

    }

  } else {
    
    # If the country is neither "England" nor "Scotland", print an error message
    stop("Invalid nation or population group specified! Please specify either 'England' or 'Scotland' for nation and 'Adult' or 'Children' for population_group.")
    
  }
  
}


