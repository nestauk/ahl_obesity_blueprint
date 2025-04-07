# Script for analysis of differences in prevalence rates of obesity in 
# post implementation period for England compared to Scotland & Wales

# the discussion of the results are here - https://docs.google.com/document/d/1hfw0mMLE1iki_3kZ10N1DRYIjGXvY9dWSXxP0mcjk6w/edit?usp=sharing


# setup
rm(list = ls())
gc()
library(tidyverse)
library(here)
source(file = "requirements.R")


# Reading in all necessary data files:


# baseline cleaned datafiles for England, Scotland & Wales:

df_england_cleaned = read_csv(here("inputs/processed/hse_2019.csv")) %>%
  mutate(type = "England") %>%
  select(c("id", "weight", "height", "age", "sex", "wt_int", "bmi", "bmi_class", "intake", "type"))

df_scotland_cleaned = read_csv(here("inputs/processed/shes_2019.csv")) %>%
  mutate(type = "Scotland") %>%
  select(c("id", "weight", "height", "age", "sex", "wt_int", "bmi", "bmi_class", "intake", "type"))

df_wales_cleaned = read_csv(here("inputs/processed/nsw_2019.csv")) %>%
  mutate(type = "Wales") %>%
  select(c("id", "weight", "height", "age", "sex", "wt_int", "bmi", "bmi_class", "intake", "type"))

# creating a single baseline df for England, Scotland & Wales
all_df = rbind(df_england_cleaned, df_scotland_cleaned, df_wales_cleaned)



# reading in datafiles related to policy 4b:

# policy 4b

df_4b_england = read_csv("C:/git/ahl_obesity_blueprint/outputs/policy_4b/policy_4b_adult_england_bmi.csv") %>%
  mutate(type = "England_policy") %>%
  select(c("id", "weight", "height", "age", "sex", "wt_int", "bmi_5_class", "intake", "type", "bmi_5"))


df_4b_scotland = read_csv("C:/git/ahl_obesity_blueprint/outputs/policy_4b/policy_4b_adult_scotland_bmi.csv") %>%
  mutate(type = "Scotland_policy") %>%
  select(c("id", "weight", "height", "age", "sex", "wt_int", "bmi_5_class", "intake", "type", "bmi_5"))


df_4b_wales = read_csv("C:/git/ahl_obesity_blueprint/outputs/policy_4b/policy_4b_adult_wales_bmi.csv") %>%
  mutate(type = "Wales_policy") %>%
  select(c("id", "weight", "height", "age", "sex", "wt_int", "bmi_5_class", "intake", "type", "bmi_5"))


# Creating a single endline df for England, Scotland and Wales data
all_4b_df = rbind(df_4b_england, df_4b_scotland, df_4b_wales) %>%
  rename("bmi" = "bmi_5",
         "bmi_class" = "bmi_5_class")

all_baseline_4b_df = rbind(all_df, all_4b_df)


df_baseline = all_baseline_4b_df %>%
  filter(type %in% c("England", "Scotland", "Wales"))

df_england = all_baseline_4b_df %>%
  filter(type %in% c("England", "England_policy"))

df_scotland = all_baseline_4b_df %>%
  filter(type %in% c("Scotland", "Scotland_policy"))

df_wales = all_baseline_4b_df %>%
  filter(type %in% c("Wales", "Wales_policy"))

df_scotland_wales = all_baseline_4b_df %>%
  filter(type %in% c("Scotland", "Scotland_policy", "Wales", "Wales_policy"))


df_endline = all_baseline_4b_df %>%
  filter(type %in% c("England_policy", "Scotland_policy", "Wales_policy"))





# policy 8a - reading in the output files:

# Endline at five years:
df_8a_england_5 = read_csv("C:/git/ahl_obesity_blueprint/outputs/policy_8a/policy_8a_adult_england_bmi.csv") %>%
  mutate(type = "England_policy") %>%
  select(c("id", "weight", "height", "age", "sex", "wt_int", "bmi_5_class", "intake", "type", "bmi_5"))

# Scotland
df_8a_scotland_5 = read_csv("C:/git/ahl_obesity_blueprint/outputs/policy_8a/policy_8a_adult_scotland_bmi.csv") %>%
  mutate(type = "Scotland_policy") %>%
  select(c("id", "weight", "height", "age", "sex", "wt_int", "bmi_5_class", "intake", "type", "bmi_5"))

# Wales
df_8a_wales_5 = read_csv("C:/git/ahl_obesity_blueprint/outputs/policy_8a/policy_8a_adult_wales_bmi.csv") %>%
  mutate(type = "Wales_policy") %>%
  select(c("id", "weight", "height", "age", "sex", "wt_int", "bmi_5_class", "intake", "type", "bmi_5"))

# Combied df of England, Scotland & Wales at five years:
all_8a_df_5 = rbind(df_8a_england_5, df_8a_scotland_5, df_8a_wales_5) %>%
  rename("bmi" = "bmi_5",
         "bmi_class" = "bmi_5_class")


# single baseline and endline df
all_baseline_8a_df_5 = rbind(all_df, all_8a_df_5)


df_baseline_5 = all_baseline_8a_df_5 %>%
  filter(type %in% c("England", "Scotland", "Wales"))

df_england_5_8a = all_baseline_8a_df_5 %>%
  filter(type %in% c("England", "England_policy"))

df_scotland_5_8a = all_baseline_8a_df_5 %>%
  filter(type %in% c("Scotland", "Scotland_policy"))

df_wales_5_8a = all_baseline_8a_df_5 %>%
  filter(type %in% c("Wales", "Wales_policy"))


df_scotland_wales_8a = all_baseline_8a_df_5 %>%
  filter(type %in% c("Scotland", "Scotland_policy", "Wales", "Wales_policy"))


df_endline_5_8a = all_baseline_8a_df_5 %>%
  filter(type %in% c("England_policy", "Scotland_policy", "Wales_policy"))


# creating a dataframe to add shading for different BMI groups:

data_breaks_1 <- data.frame(start = c(-Inf, 18.5, 25, 30, 32.5, 35, 37.5, 40),  # Create data with breaks
                            end =     c(18.5, 25, 30, 32.5, 35,   37.5, 40,   Inf),
                            colors = gray.colors(8),
                            labels = c("Underweight", "Normal", "Overweight", "Ob1", "Ob2", "Ob3", "Ob4", "Morbidly Obese"),
                            pos_x = c(15, 22, 27, 31, 33, 36, 38, 45),
                            pos_y = rep(0.15,8))




# Creating distributions of sex, age and height of individuals in the BMI 30 - 35 region:
# We want to create the baseline distributions of height, sex and age. So, we can use any of the policy files - 4b or 8a
# as the baseline age, sex and height remains the same irrespective of policy:

# using the files related to policy 8a:

# filter the df for individuals in Scotland & Wales with a BMI between 30 & 35:
df_case_height = df_scotland_wales_8a %>%
  filter(type %in% c("Scotland", "Wales")) %>%
  filter(bmi >= 30 & bmi <= 35)


# Summary stats of sex of individuals in Wales:
wales_sex_summary = df_case_height %>%
  filter(type %in% c("Wales")) %>%
  count(sex, wt = wt_int) %>% 
  mutate(freq = n/sum(n)*100)

# Summary stats of sex of individuals in Scotland:
scotland_sex_summary = df_case_height %>%
  filter(type %in% c("Scotland")) %>%
  count(sex, wt = wt_int) %>% 
  mutate(freq = n/sum(n)*100)





# Distribution of heights of individuals in BMI 30 - 35:
ggplot() +
  geom_density(data = df_case_height, aes(x = height, group = type, color = type, weight = wt_int), size = 1) +
  labs(x = "BMI",
       y = "Density",
       color = "",
       title = "Height Distribution for BMI 30 - 35") +
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta") +
  scale_color_manual(values=c("#FDB633", "#0000FF", "#f6a4b7", "#ff6e47")) +  # "#ff6e47",
  scale_fill_manual(values = rev(gray.colors(8))) +
  guides(fill = "none")

# Summary stats of heights of individuals in Wales:
wales_height_summary = df_case_height %>%
  filter(type %in% c("Wales")) %>%
  dplyr::summarize(
    mean = mean(height),
    min = min(height),
    median = median(height),
    max = max(height)
  )

# Summary stats of heights of individuals in Scotland:
scotland_height_summary = df_case_height %>%
  filter(type %in% c("Scotland")) %>%
  dplyr::summarize(
    mean = mean(height),
    min = min(height),
    median = median(height),
    max = max(height)
  )


# Age distribution of individuals in BMI 30 - 35 group:
ggplot() +
  geom_density(data = df_case_height, aes(x = age, group = type, color = type, weight = wt_int), size = 1) +
  labs(x = "Age",
       y = "Density",
       color = "",
       title = "Age Distribution for BMI 30 - 35") +
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta") +
  scale_color_manual(values=c("#FDB633", "#0000FF", "#f6a4b7", "#ff6e47")) +  # "#ff6e47",
  scale_fill_manual(values = rev(gray.colors(8))) +
  guides(fill = "none")

# Summary stats of age of individuals in Wales:
wales_age_summary = df_case_height %>%
  filter(type %in% c("Wales")) %>%
  dplyr::summarize(
    mean = mean(age),
    min = min(age),
    median = median(age),
    max = max(age)
  )

# Summary stats of age of individuals in Scotland:
scotland_age_summary = df_case_height %>%
  filter(type %in% c("Scotland")) %>%
  dplyr::summarize(
    mean = mean(age),
    min = min(age),
    median = median(age),
    max = max(age)
  )





# BMI distributions plots for policy 4b and 8a:

# policy 4b - baseline
ggplot() +
  geom_rect(data = data_breaks_1,
            aes(xmin = start,
                xmax = end,
                ymin = -Inf,
                ymax = Inf,
                fill = colors),
            alpha=0.5) +
  geom_text(data = data_breaks_1, aes(x = pos_x, y = pos_y, label = labels)) + 
  geom_density(data = df_baseline, aes(x = bmi, group = type, color = type, weight = wt_int), size = 1) +
  xlim(12,50) +
  labs(x = "BMI",
       y = "Density",
       color = "",
       title = "BMI Distribution - Baseline") +
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta") +
  scale_color_manual(values=c("#FDB633", "#0000FF", "#f6a4b7")) +  # "#ff6e47",
  scale_fill_manual(values = rev(gray.colors(8))) +
  guides(fill = "none")


# policy 4b - endline
ggplot() +
  geom_rect(data = data_breaks_1,
            aes(xmin = start,
                xmax = end,
                ymin = -Inf,
                ymax = Inf,
                fill = colors),
            alpha=0.5) +
  geom_text(data = data_breaks_1, aes(x = pos_x, y = pos_y, label = labels)) + 
  geom_density(data = df_endline, aes(x = bmi, group = type, color = type, weight = wt_int), size = 1) +
  xlim(12,50) +
  labs(x = "BMI",
       y = "Density",
       color = "",
       title = "BMI Distribution - policy 4b - 5 years") +
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta") +
  scale_color_manual(values=c("#FDB633", "#0000FF", "#f6a4b7")) +  # "#ff6e47",
  scale_fill_manual(values = rev(gray.colors(8))) +
  guides(fill = "none")



# policy 4b - Baseline + Endline - Scotland:
ggplot() +
  geom_rect(data = data_breaks_1,
            aes(xmin = start,
                xmax = end,
                ymin = -Inf,
                ymax = Inf,
                fill = colors),
            alpha=0.5) +
  geom_text(data = data_breaks_1, aes(x = pos_x, y = pos_y, label = labels)) + 
  geom_density(data = df_scotland, aes(x = bmi, group = type, color = type, weight = wt_int), size = 1) +
  xlim(12,50) +
  labs(x = "BMI",
       y = "Density",
       color = "",
       title = "BMI Distribution - policy 4b - 5 years - Scotland") +
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta") +
  scale_color_manual(values=c("#FDB633", "#0000FF", "#f6a4b7")) +  # "#ff6e47",
  scale_fill_manual(values = rev(gray.colors(8))) +
  guides(fill = "none")


# policy 4b - Baseline + Endline - Wales:
ggplot() +
  geom_rect(data = data_breaks_1,
            aes(xmin = start,
                xmax = end,
                ymin = -Inf,
                ymax = Inf,
                fill = colors),
            alpha=0.5) +
  geom_text(data = data_breaks_1, aes(x = pos_x, y = pos_y, label = labels)) + 
  geom_density(data = df_wales, aes(x = bmi, group = type, color = type, weight = wt_int), size = 1) +
  xlim(12,50) +
  labs(x = "BMI",
       y = "Density",
       color = "",
       title = "BMI Distribution - policy 4b - 5 years - Wales") +
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta") +
  scale_color_manual(values=c("#FDB633", "#0000FF", "#f6a4b7")) +  # "#ff6e47",
  scale_fill_manual(values = rev(gray.colors(8))) +
  guides(fill = "none")


# policy 4b - Baseline + Endline - Scotland & Wales:
ggplot() +
  geom_rect(data = data_breaks_1,
            aes(xmin = start,
                xmax = end,
                ymin = -Inf,
                ymax = Inf,
                fill = colors),
            alpha=0.5) +
  geom_text(data = data_breaks_1, aes(x = pos_x, y = pos_y, label = labels)) + 
  geom_density(data = df_scotland_wales, aes(x = bmi, group = type, color = type, weight = wt_int), size = 1) +
  xlim(12,50) +
  labs(x = "BMI",
       y = "Density",
       color = "",
       title = "BMI Distribution - policy 4b - 5 years - Scotland Wales") +
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta") +
  scale_color_manual(values=c("#FDB633", "#0000FF", "#f6a4b7", "#ff6e47")) +  # "#ff6e47",
  scale_fill_manual(values = rev(gray.colors(8))) +
  guides(fill = "none")



# policy 8a - Baseline + Endline - Scotland & Wales:
ggplot() +
  geom_rect(data = data_breaks_1,
            aes(xmin = start,
                xmax = end,
                ymin = -Inf,
                ymax = Inf,
                fill = colors),
            alpha=0.5) +
  geom_text(data = data_breaks_1, aes(x = pos_x, y = pos_y, label = labels)) + 
  geom_density(data = df_scotland_wales_8a, aes(x = bmi, group = type, color = type, weight = wt_int), size = 1) +
  xlim(12,50) +
  labs(x = "BMI",
       y = "Density",
       color = "",
       title = "BMI Distribution - policy 8a - 5 years - Scotland Wales") +
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta") +
  scale_color_manual(values=c("#FDB633", "#0000FF", "#f6a4b7", "#ff6e47")) +  # "#ff6e47",
  scale_fill_manual(values = rev(gray.colors(8))) +
  guides(fill = "none")


