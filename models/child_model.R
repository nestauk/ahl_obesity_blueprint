rm(list = ls())
install.packages("svydesign")
install.packages("ggplot2")

library(tidyverse)
library(here)
library(bw)
library(survey)




df_child <- read_csv(here("inputs/processed/hse_2019_children.csv")) %>%  # we are reading a file that contains energy intake, BMI weight class and other variables reated during the pre-processing stage.
  #head(20) %>% 
  mutate(sex = ifelse(sex == 1, "male", "female")) %>% 
  filter(bf_percent > 5) %>%
  mutate(intake_diff = -intake*0.01)



eichange_child <- t(apply(df_child, 1, function(x) rep(as.numeric(x["intake_diff"]), 365*1)))

# model 1 with fm and ffm
child_model_weight = child_weight(age = df_child$age,
                                  sex =  df_child$sex, 
                                  FM = df_child$fm_hudda, 
                                  FFM = df_child$ffm_hudda, 
                                  EI = eichange_child,   
                                  days = (365*1),
                                  checkValues = TRUE)

#model 2 without inputting fm and ffm
child_model_weight_1 = child_weight(age = df_child$age,
                                    sex =  df_child$sex, 
                                    #FM = df_child$fm_hudda, 
                                    #FFM = df_child$ffm_hudda, 
                                    EI = eichange_child,   
                                    days = (365*1),
                                    checkValues = TRUE)

child_bw_end_1 = child_model_weight_1[["Body_Weight"]]

child_bw_end = child_model_weight[["Body_Weight"]]


model_plot(child_model_weight, c("Body_Weight", "Fat_Mass"))

model_plot(child_model_weight_1, c("Body_Weight", "Fat_Mass"))



# # EIchange = rep(-250, 365*10), NAchange = rep(0, 365*10), days = 365*10
# 
# 47.4
# 157.5
# 11.5
# #male
# 11.072056
# 36.32794
# 
# 
# test = child_weight(age = 6, sex = "female", FM = 3.898199, FFM = 13.10180, EI = rep(-10, 365*1),days = 365*1 )
# 
# 
# test_1 = child_weight(age = 11.5, sex = "male", FM = 11.072056, FFM = 36.32794, EI = rep(-10, 365*1), days = 365*1 )
# 
# model_plot(test, c("Body_Weight", "Fat_Mass"))
# 
# help("child_weight")
# 
# 
# 70.7
# 157.6
# 11.5
# #male
# 27.939474
# 42.76053
# 
# test_2 = child_weight(age = 11.5, sex = "male", FM = 27.939474, FFM = 42.76053, EI = rep(-10, 365*1), days = 365*1 )
# test_2_df = test_2[["Body_Weight"]]

