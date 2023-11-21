rm(list = ls())
install.packages("svydesign")
install.packages("ggplot2")

library(tidyverse)
library(here)
library(bw)
library(survey)

# explore the vignettes
#browseVignettes("bw")


df_adult <- read_csv(here("inputs/processed/hse_2019.csv")) %>%  # we are reading a file that contains energy intake, BMI weight class and other variables reated during the pre-processing stage.
  #head(20) %>% 
  mutate(sex = ifelse(sex == 1, "male", "female")) %>% 
  mutate(intake_diff = -intake*0.01)


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


bmi_end = model_weight[["Body_Mass_Index"]]

new_df = cbind(df_adult, bmi_end)

post_df_adult = new_df %>%
  select("id", "weight", "height", "age", "sex", "bmi", "wt_int", "psu", 
         "strata", "pal", "rmr", "bmi_class", "intake", "intake_diff", "1","1825" ) %>%
  rename(bmi_end = "1825", bmi_start = "1" ) %>%
  mutate(bmi_class_final = case_when(bmi_end <= 18.5 ~ "underweight",
                                     bmi_end > 18.5 & bmi_end < 25 ~ "normal",
                                     bmi_end >= 25 & bmi_end < 30 ~ "overweight",
                                     bmi_end >= 30 & bmi_end < 40 ~ "obese",
                                     bmi_end >= 40 ~ "morbidly obese",
                                     TRUE ~ "NA")) %>%
  mutate(bmi_class = factor(bmi_class, levels = c("underweight", "normal", "overweight",
                                                  "obese", "morbidly obese"))) %>%
  mutate(bmi_class_final = factor(bmi_class_final, levels = c("underweight", "normal", 
                                                              "overweight", "obese", "morbidly obese")))


# just for example - not actual


design <-  svydesign(ids=~df$psu, 
                     nest = T,
                     data=df,
                     weights=df$wt_int)


design <-  svydesign(ids=~post_df_adult$psu, 
                     nest = T,
                     data=post_df_adult,
                     weights=post_df_adult$wt_int)



# density plot

density_plot = ggplot(post_df_adult, aes(x= bmi, fill = "bmi_start")) +
  geom_density(alpha = 0.5) +
  geom_density(data = post_df_adult, aes(x = bmi_end, fill = "bmi_end", alpha = 0.5)) +
  labs(title = "bmi_start and bmi_end", x = "BMI", y = "density") +
  scale_fill_manual(values=c("#0000FF", "#F6A4B7"))

density_plot





## clustered bar plot

post_df_adult_long = post_df_adult %>%
  select(c(bmi_class, bmi_class_final)) %>%
  gather(key = "variable", value = "category")


percentage_df <- post_df_adult_long %>%
  group_by(variable, category) %>%
  summarise(freq = n()) %>%
  mutate(percentage = freq / sum(freq) * 100)

bar_plot <- ggplot(data = percentage_df, aes(x = category, y = percentage, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "BMI Categories Comparison", x = "BMI Category", y = "Percentage") +
  scale_fill_manual(values = c("bmi_start" = "blue", "bmi_end" = "red"))

bar_plot
