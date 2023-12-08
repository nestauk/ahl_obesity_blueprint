rm(list = ls())
install.packages("survey")
install.packages("ggplot2")

library(tidyverse)
library(here)
library(bw)
library(survey)

browseVignettes("bw")


df_child <- read_csv(here("inputs/processed/hse_2019_children.csv")) %>%  # we are reading a file that contains energy intake, BMI weight class and other variables reated during the pre-processing stage.
  #head(20) %>% 
  mutate(sex = ifelse(sex == 1, "male", "female")) %>% 
  filter(fm_hudda > 0) %>%
  mutate(intake_diff = -intake*0.01)



#eichange_child <- t(apply(df_child, 1, function(x) rep(as.numeric(x["intake_diff"]), 365*1)))

#t_eichange_child <- t(apply(df_child, 1, function(x) rep(as.numeric(x["intake"]) + as.numeric(x["intake_diff"]), 365*1)))

nt_ei = apply(df_child, 1, function(x) rep(as.numeric(x["intake"]) + as.numeric(x["intake_diff"]), 365*5))


# model 1 with fm and ffm
child_model_weight = child_weight(age = df_child$age,
                                  sex =  df_child$sex, 
                                  FM = df_child$fm_hudda, 
                                  FFM = df_child$ffm_hudda, 
                                  EI = nt_ei,   
                                  days = (365*5),
                                  checkValues = TRUE)

# model 2 without inputting fm and ffm
# child_model_weight_1 = child_weight(age = df_child$age,
#                                     sex =  df_child$sex, 
#                                     #FM = df_child$fm_hudda, 
#                                     #FFM = df_child$ffm_hudda, 
#                                     EI = nt_ei,   
#                                     days = (365*5),
#                                     checkValues = TRUE)
# 



child_bw_h = as.data.frame(child_model_weight[["Body_Weight"]]) %>%
  mutate(bw_diff = V1825 - V1) %>%
  select(c(V1, V365, V730, V1095, V1460, V1825, bw_diff)) %>%
  rename(bw_1 = V1, bw_365 = V365, bw_730 = V730, 
         bw_1095 = V1095, bw_1460 = V1460, bw_1825 = V1825)



child_age_h = as.data.frame(child_model_weight[["Age"]]) %>%
  mutate(age_diff = V1825 - V1) %>%
  select(c(V1, V365, V730, V1095, V1460, V1825, age_diff)) %>%
  rename(age_1 = V1, age_365 = V365, age_730 = V730, 
         age_1095 = V1095, age_1460 = V1460, age_1825 = V1825)


child_ffm_h = as.data.frame(child_model_weight[["Fat_Free_Mass"]]) %>%
  mutate(ffm_diff = V1825 - V1) %>%
  select(c(V1, V365, V730, V1095, V1460, V1825, ffm_diff)) %>%
  rename(ffm_1 = V1, ffm_365 = V365, ffm_730 = V730,
         ffm_1095 = V1095, ffm_1460 = V1460, ffm_1825 = V1825)



child_fm_h = as.data.frame(child_model_weight[["Fat_Mass"]]) %>%
  mutate(fm_diff = V1825 - V1) %>%
  select(c(V1, V365, V730, V1095, V1460, V1825, fm_diff))  %>%
  rename(fm_1 = V1, fm_365 = V365, fm_730 = V730,
         fm_1095 = V1095, fm_1460 = V1460, fm_1825 = V1825)



child_age_bw_ffm_fm = cbind(child_bw_h, child_age_h, child_ffm_h, child_fm_h) %>%
  select(age_1, bw_1, fm_1, ffm_1,
         age_365, bw_365, fm_365, ffm_365,
         age_730, bw_730, fm_730, ffm_730,
         age_1095, bw_1095, fm_1095, ffm_1095,
         age_1460, bw_1460, fm_1460, ffm_1460,
         age_1825, bw_1825, fm_1825, ffm_1825)




# Deurenbery et al. (1991)

child_bw_end_1 = as.data.frame(child_model_weight_1[["Body_Weight"]])


child_bw_end_check_d = child_bw_end_1 %>%
  mutate(bw_diff = V1825 - V1) %>%
  select(c(V1, V365, V730, V1095, V1460, V1825, bw_diff))



sum(child_bw_end_check$V1 == 19.5)

new_df_child = cbind(df_child, child_bw_end_check)



df_new_child = 

model_plot(child_model_weight, c("Fat_Mass"))

model_plot(child_model_weight_1, c("Body_Weight"))





## Test:

mydata <- data.frame(
  id = 1:5,
  age = c(8, 10, 7, 7, 12),
  sex = c("male", "female", "female", "male", "male"),
  energy = runif(5, 1500, 2000),
  prob = c(0.1, 0.2, 0.2, 0.05, 0.45))

#Get energy change with energy build function
eichange <- energy_build(cbind(runif(5, 1500, 2000), mydata$energy), c(0, 365))

t_eichange = t(eichange)

#Returns a weight change matrix and other matrices
database_model <- child_weight(mydata$age, mydata$sex, EI = t(eichange))

cbind(runif(5, 1500, 2000), mydata$energy)

model_plot(database_model, "Body_Weight")

energy_build()


















