rm(list = ls())
#install.packages("svydesign")
install.packages("ggplot2")

library(tidyverse)
library(here)
library(bw)
library(survey)

# explore the vignettes
#browseVignettes("bw")

# policy 1: tv online advertising watershed = 0 kcals
# policy 2: volume price promotions = 22 kcals
# policy 3: nutri-score fopl = 50 kcals


intake_change = -22

df_adult <- read_csv(here("inputs/processed/hse_2019.csv")) %>%  # we are reading a file that contains energy intake, BMI weight class and other variables reated during the pre-processing stage.
  #head(20) %>% 
  mutate(sex = ifelse(sex == 1, "male", "female")) %>%
  #mutate(intake_diff = -intake*0.01) %>%
  mutate(intake_diff = ifelse(bmi_class %in% c("underweight", "normal"), 0, intake_change))


eichange <- t(apply(df_adult, 1, function(x) rep(as.numeric(x["intake_diff"]), 365*5)))
# for each individual in the hse, we are taking the difference in energy intake[intake_diff], (converting it into type numeric),
# then creating a vector, which essentially repeats the intake diff value 365 times (i.e. for a year)
# this is then done for each individual (row in the df) and then transposed.
# this matrix of intake difference for a year is input to create the weight model.

#, NAchange = rep(0, 365*3)
nachange <- t(apply(df_adult, 1, function(x) rep(0, 365*5)))

model_weight <- adult_weight(df_adult$weight, df_adult$height/100, df_adult$age, df_adult$sex, eichange, nachange, days = 365*5)
# the bw package has a function called [adult_weight] that takes the following inputs:
# weight of the individual, height f the individual in meters, age, sex and change in 
# energy intake (for a period of time)


bmi_model = model_weight[["Body_Mass_Index"]]

new_df = cbind(df_adult, bmi_model)


post_df_adult = new_df %>%
  select("id", "weight", "height", "age", "sex", "bmi", "wt_int", "psu", 
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


design <-  svydesign(ids=~post_df_adult$psu, 
                     nest = T,
                     data=post_df_adult,
                     weights=post_df_adult$wt_int)



# density plot

density_plot = ggplot(post_df_adult, aes(x= bmi, fill = "bmi_start")) +
  geom_density(alpha = 0.5) +
  geom_density(data = post_df_adult, aes(x = bmi_5, fill = "bmi_end", alpha = 0.5)) +
  labs(title = "bmi_start and bmi_end", x = "BMI", y = "density") +
  scale_fill_manual(values=c("#0000FF", "#F6A4B7"))

density_plot


## clustered bar plot

post_df_adult_long = post_df_adult %>%
  select(c(bmi_class, bmi_5_class)) %>%
  gather(key = "variable", value = "category")

percentage_df <- post_df_adult_long %>%
  group_by(variable, category) %>%
  summarise(freq = n()) %>%
  mutate(percentage = freq / sum(freq) * 100)

bar_plot <- ggplot(data = percentage_df, aes(x = category, y = percentage, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "BMI Categories Comparison", x = "BMI Category", y = "Percentage")
#  scale_fill_manual(values = c("#0000FF", "#F6A4B7"))

bar_plot




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
  mutate(BMI = factor(BMI, levels = c("underweight", "normal", "overweight", "obese", "morbidly obese")))

bmi_change = as.data.frame(bmi_change)

bmi_change_year = bmi_change %>%
  select(-c(n)) %>%
  pivot_wider(., names_from = BMI, values_from = freq)
 


bmi_change_year_1 = bmi_change %>%
  select(-c(freq)) %>%
  pivot_wider(., names_from = BMI, values_from = n)


write_csv(bmi_change_year_1, here("outputs/data/bmi_change_year_p2_1.csv"))



bmi_change %>%
  ggplot(., aes(y = freq, x = BMI, fill = type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_ipsum() +
  labs(fill = "", 
       title = "BMI Categories Distribution", 
       y = "Frequency",
       subtitle = "Population") +
  theme_ipsum(base_size = 8, axis_title_size = 8, base_family="Averta") +
  #scale_fill_manual(values=c("#0000FF", "#F6A4B7")) +
  theme(legend.position = "top")










