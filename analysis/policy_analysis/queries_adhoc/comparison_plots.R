

# setup
rm(list = ls())
gc()
library(tidyverse)
library(here)
source(file = "requirements.R")

file_path_24 = "C:/git/ahl_obesity_blueprint/outputs/policy_24_3/policy_24_3_updated_adult_england_bmi.csv"
file_path_38 = "C:/git/ahl_obesity_blueprint/outputs/new_policies/policy_38/policy_38_adult_england_bmi.csv"


df_england_cleaned = read_csv(here("inputs/processed/hse_2019.csv")) %>%
  mutate(type = "baseline") %>%
  select(c("id", "weight", "height", "age", "sex", "wt_int", "bmi", "bmi_class", "intake", "type"))


df_24a_england = read_csv(file_path_24) %>%
  mutate(type = "ring_fenced_glp") %>%
  select(c("id", "weight", "height", "age", "sex", "wt_int", "bmi_5_class", "intake", "type", "bmi_y5")) %>%
  rename("bmi" = "bmi_y5",
         "bmi_class" = "bmi_5_class")

df_38_england = read_csv(file_path_38) %>%
  mutate(type = "nice_nhs_mounjaro") %>%
  select(c("id", "weight", "height", "age", "sex", "wt_int", "bmi_5_class", "intake", "type", "bmi_y5")) %>%
  rename("bmi" = "bmi_y5",
         "bmi_class" = "bmi_5_class")

all_df = rbind(df_england_cleaned, df_24a_england, df_38_england)




# creating a dataframe to add shading for different BMI groups:

data_breaks_1 <- data.frame(start = c(-Inf, 18.5, 25, 30, 32.5, 35, 37.5, 40),  # Create data with breaks
                            end =     c(18.5, 25, 30, 32.5, 35,   37.5, 40,   Inf),
                            colors = gray.colors(8),
                            labels = c("Underweight", "Normal", "Overweight", "Ob1", "Ob2", "Ob3", "Ob4", "Morbidly Obese"),
                            pos_x = c(15, 22, 27, 31, 33, 36, 38, 45),
                            pos_y = rep(0.15,8))



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
  geom_density(data = all_df, aes(x = bmi, group = type, color = type, weight = wt_int), size = 1) +
  xlim(25,50) +
  labs(x = "BMI",
       y = "Density",
       color = "",
       title = "BMI Distribution - Ring fenced policy vs NICE/NHS Rollout") +
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta") +
  scale_color_manual(values=c("#FDB633", "#0000FF", "#f6a4b7")) +  # "#ff6e47",
  scale_fill_manual(values = rev(gray.colors(8))) +
  guides(fill = "none")





