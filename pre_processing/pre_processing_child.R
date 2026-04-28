library(here)
library(tidyverse)

## include access info for database


calculate_fat_mass <- function(weight, height, BA, SA, AO, other, age, male) {
  fat_mass = weight - exp(0.3073 * height^2 - 10.0155 * weight^-1 + 
                            0.004571 * weight + 0.01408 * BA - 
                            0.06509 * SA - 0.02624 * AO - 0.01745 * other 
                          - 0.9180 * log(age) + 0.6488 * sqrt(age) + 0.04723 * male + 2.8055)
  return(fat_mass)
}

test_df = as.data.frame(colnames(df_2019_children)) 

df_2019_children <- read.table(here("inputs/raw/hse_2019_eul_20211006.tab"), sep = "\t", header = TRUE) %>%
  filter(WtVal>0 & HtVal>0 & Age35g > 2 & Age35g < 8) %>%
  mutate(age = case_when(Age35g == 1 ~ (0+1)/2,   # 0.5
                         Age35g == 2 ~ (2+4)/2,   # 3
                         Age35g == 3 ~ (5+7)/2,   # 6
                         Age35g == 4 ~ (8+10)/2,  # 9
                         Age35g == 5 ~ (11+12)/2, # 11.5
                         Age35g == 6 ~ (13+15)/2, # 14
                         Age35g == 7 ~ (16+18)/2, # 17
                         TRUE ~ 0))


df_2019_children_1 = df_2019_children %>%
  rename(weight = WtVal,
         height = HtVal,
         sex = Sex,
         bmi = BMIVal,
         bmi_grp = BMICat1, 
         id = SerialA, mother_bmi = Moth_bmi, father_bmi = Fath_bmi, imd = qimd19, 
         psu = PSU_SCR,
         strata = cluster94) %>% 
  dplyr::select(id, weight, height, age, sex, bmi, bmi_grp, mother_bmi, father_bmi, imd, wt_int, psu, strata, origin2 )  %>% # select variables needed
  mutate(pal = case_when(age < 3 ~ 1.40,
                         age >=3 & age < 10 ~ 1.58,
                         age >=10 & age < 18 ~ 1.75),
         rmr_hox = case_when(sex == 1 ~ (  ((66.9*weight) + 2876)/4.184  ),
                             TRUE ~ ( ((47.9*weight) + 3230)/4.184 ) )) %>% 
  mutate(bmi_class = case_when(bmi <= 18.5 ~ "underweight",
                               bmi > 18.5 & bmi < 25 ~ "normal",
                               bmi >= 25 & bmi < 30 ~ "overweight",
                               bmi >= 30 & bmi < 40 ~ "obese",
                               bmi >= 40 ~ "morbidly obese",
                               TRUE ~ "NA")) %>% 
  mutate(intake_hox = pal*rmr_hox) 

total_weight = sum(df_2019_children_1$wt_int) *nrow(df_2019_children_1)



sampling_fraction = 7000/ total_weight

sampled_data = df_2019_children_1 %>%
  filter(runif(n()) < sampling_fraction)

#%>% # calculating value of energy intake 
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
  mutate(ffm_hudda = weight - fm_hudda) %>%
  mutate(bf_percent_baseline = (fm_hudda/weight)*100) %>%
  mutate(bmi_cat_baseline = case_when(age == 6 ~ (case_when(sex == 1 ~ case_when(bf_percent_baseline <= 12.4 ~ "underweight",
                                                                                      bf_percent_baseline >= 19.5 & bf_percent_baseline < 22.7 ~ "overweight",
                                                                                      bf_percent_baseline >12.4 & bf_percent_baseline < 19.5 ~ "normal",
                                                                                      bf_percent_baseline >= 22.7 ~ "obese"),
                                                            sex == 2 ~ case_when(bf_percent_baseline <= 14.4 ~ "underweight",
                                                                                        bf_percent_baseline >14.4 & bf_percent_baseline < 23 ~ "normal",
                                                                                        bf_percent_baseline >= 23 & bf_percent_baseline < 26.2 ~ "overweight",
                                                                                        bf_percent_baseline >= 26.2 ~ "obese") )),
                                      age == 9 ~ (case_when(sex == 1 ~ case_when(bf_percent_baseline <= 12.8 ~ "underweight",
                                                                                      bf_percent_baseline >12.8 & bf_percent_baseline < 22.2 ~ "normal",
                                                                                      bf_percent_baseline >= 22.2 & bf_percent_baseline < 26.8 ~ "overweight",
                                                                                      bf_percent_baseline >= 26.8 ~ "obese"),
                                                            sex == 2 ~ case_when(bf_percent_baseline <= 15.7 ~ "underweight",
                                                                                        bf_percent_baseline >15.7 & bf_percent_baseline < 27.2 ~ "normal",
                                                                                        bf_percent_baseline >= 27.2 & bf_percent_baseline < 31.2 ~ "overweight",
                                                                                        bf_percent_baseline >= 31.2 ~ "obese") )),
                                      age == 11.5 ~ (case_when(sex == 1 ~ case_when(bf_percent_baseline <= 12.6 ~ "underweight",
                                                                                         bf_percent_baseline >12.6 & bf_percent_baseline < 23 ~ "normal",
                                                                                         bf_percent_baseline >= 23 & bf_percent_baseline < 28.3 ~ "overweight",
                                                                                         bf_percent_baseline >= 28.3 ~ "obese"),
                                                               sex == 2 ~ case_when(bf_percent_baseline <= 16.1 ~ "underweight",
                                                                                           bf_percent_baseline >16.1 & bf_percent_baseline < 28.8 ~ "normal",
                                                                                           bf_percent_baseline >= 28.8 & bf_percent_baseline < 32.8 ~ "overweight",
                                                                                           bf_percent_baseline >= 32.8 ~ "obese") )),
                                      age == 14 ~ (case_when(sex == 1 ~ case_when(bf_percent_baseline <= 10.9 ~ "underweight",
                                                                                       bf_percent_baseline >10.9 & bf_percent_baseline < 21.3 ~ "normal",
                                                                                       bf_percent_baseline >= 21.3 & bf_percent_baseline < 25.9 ~ "overweight",
                                                                                       bf_percent_baseline >= 25.9 ~ "obese"),
                                                             sex == 2 ~ case_when(bf_percent_baseline <= 16 ~ "underweight",
                                                                                         bf_percent_baseline > 16 & bf_percent_baseline < 29.6 ~ "normal",
                                                                                         bf_percent_baseline >= 29.6 & bf_percent_baseline < 33.6 ~ "overweight",
                                                                                         bf_percent_baseline >= 33.6 ~ "obese") )),
                                      age == 17 ~ (case_when(sex == 1 ~ case_when(bf_percent_baseline <= 9.8 ~ "underweight",
                                                                                       bf_percent_baseline > 9.8 & bf_percent_baseline < 20.1 ~ "normal",
                                                                                       bf_percent_baseline >= 20.1 & bf_percent_baseline < 23.9 ~ "overweight",
                                                                                       bf_percent_baseline >= 23.9 ~ "obese"),
                                                             sex == 2 ~ case_when(bf_percent_baseline <= 15.1 ~ "underweight",
                                                                                         bf_percent_baseline > 15.1 & bf_percent_baseline < 30.4 ~ "normal",
                                                                                         bf_percent_baseline >= 30.4 & bf_percent_baseline < 34.4 ~ "overweight",
                                                                                         bf_percent_baseline >= 34.4 ~ "obese") )) ))



write_csv(df_2019_children, here("inputs/processed/hse_2019_children.csv"))





# Scotland



df_2019_child_sc <- read.table(here("C:/Users/Anish.Chacko/Downloads/hs_scotland/shes19i_eul.tab"), sep = "\t", header = TRUE) %>%
  filter(wtval>0 & htval>0 & age < 18) %>%
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
  dplyr::select(id, weight, height, age, sex, bmi, wt_int, psu, strata, origin2) %>% # select variables needed
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
  mutate(ffm_hudda = weight - fm_hudda) %>%
  mutate(bf_percent_baseline = (fm_hudda/weight)*100) %>%
  left_join(body_fat_reference_tables, by = c("age", "sex")) %>%
  mutate(bmi_cat_baseline = case_when(bf_percent_baseline < p2 ~ "underweight",
                                      bf_percent_baseline >= p95 ~ "obese",
                                      bf_percent_baseline >= p85 & bf_percent_baseline < p95 ~ "overweight",
                                      TRUE ~ "normal"))

write_csv(df_2019_children, here("inputs/processed/shes_2019_children.csv"))





body_fat_reference_tables = read_csv(file = here("inputs/body_fat_reference_tables.csv"))



