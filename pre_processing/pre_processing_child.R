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


df_2019_children = df_2019_children %>%
  rename(weight = WtVal,
         height = HtVal,
         sex = Sex,
         bmi = BMIVal,
         id = SerialA,
         psu = PSU_SCR,
         strata = cluster94) %>% 
  dplyr::select(id, weight, height, age, sex, bmi, wt_int, psu, strata, origin2 )  %>% # select variables needed
  mutate(pal = case_when(age < 3 ~ 1.40,
                         age >=3 & age < 10 ~ 1.58,
                         age >=10 & age < 18 ~ 1.75),
         rmr_msj = case_when(sex == 1 ~ ((10 * weight) + (6.25 * height) - (5 * age) + 5),
                         TRUE ~ ((10 * weight) + (6.25 * height) - (5 * age) - 161)),  # sex = 2 female Miffin & St.Jeor
         rmr_hox = case_when(sex == 1 ~ (  ((66.9*weight) + 2876)/4.184  ),
                             TRUE ~ ( ((47.9*weight) + 3230)/4.184 ) )) %>% 
  mutate(bmi_class = case_when(bmi <= 18.5 ~ "underweight",
                               bmi > 18.5 & bmi < 25 ~ "normal",
                               bmi >= 25 & bmi < 30 ~ "overweight",
                               bmi >= 30 & bmi < 40 ~ "obese",
                               bmi >= 40 ~ "morbidly obese",
                               TRUE ~ "NA")) %>% 
  mutate(intake_msj = pal*rmr_msj,
         intake_hox = pal*rmr_hox) %>% # calculating value of energy intake 
  # mutate(fm_deurenberg = case_when(sex == 1 ~ ((1.51*bmi) - (0.7*age) -  ((2.2/100)*weight)),
  #                                  sex == 2 ~ ((1.51*bmi) - (0.7*age) -  ((1.4/100)*weight)))) %>%
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
  # mutate(ffm_deurenberg = weight - fm_deurenberg) %>%
  mutate(ffm_hudda = weight - fm_hudda)


ggplot(df_2019_children, aes(x= intake_msj, y = intake_hox, fill = intake_msj)) +
  geom_point()


ggplot(df_2019_children, aes(x= weight, y = intake_msj)) +
  geom_point()



df_2019_children = df_2019_children %>%
  mutate(bf_percent = (fm_hudda/ weight)*100 )

write_csv(df_2019_children, here("inputs/processed/hse_2019_children.csv"))

unique((df_2019_children$age))
sum(df_2019_children$age == 17.0)
sum(df_2019_children$age == 16.0)
sum(df_2019_children$age == 15.0)
sum(df_2019_children$age == 14.0)
sum(df_2019_children$age == 13.0)



# Analysis of ffm and fm from Deurenberg et al. and Hudda et al.

df_2019_children = df_2019_children %>%
  mutate(outlier_fm_deurenberg_w = ifelse(fm_deurenberg > weight, 1, 0),
         outlier_fm_hudda_w = ifelse(fm_hudda > weight, 1, 0),
         outlier_fm_deurenbery_0 = ifelse(fm_deurenberg < 0, 1, 0),
         outlier_fm_hudda_0 = ifelse(fm_hudda < 0, 1, 0))

# fat mass > weight
ggplot(df_2019_children, aes(x = weight, y = ffm_deurenberg, color = outlier_fm_deurenberg_w)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~age, scales = "free") +
  labs(title = "Deurenbery et al. - fatmass > weight" )


# fat mass < 0
ggplot(df_2019_children, aes(x = weight, y = ffm_deurenberg, color = outlier_fm_deurenbery_0)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~age, scales = "free") +
  labs(title = "Deurenbery et al. - fatmass < 0" )


# fat mass > weight
ggplot(df_2019_children, aes(x = weight, y = ffm_hudda, color = outlier_fm_hudda_w)) +
  geom_point(alpha = 0.5)  +
  facet_wrap(~age, scales = "free") +
  labs(title = "Hudda et al. - fatmass > weight" )


# fat mass < 0
ggplot(df_2019_children, aes(x = weight, y = ffm_hudda, color = outlier_fm_hudda_0)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~age, scales = "free") +
  labs(title = "Hudda et al. - fatmass < 0" )



