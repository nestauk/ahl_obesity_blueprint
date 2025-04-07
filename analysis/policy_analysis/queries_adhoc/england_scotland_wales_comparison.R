



df_england_cleaned = read_csv(here("inputs/processed/hse_2019.csv")) %>%
  mutate(type = "England") %>%
  select(c("id", "weight", "height", "age", "sex", "wt_int", "bmi", "bmi_class", "intake", "type"))

df_scotland_cleaned = read_csv(here("inputs/processed/shes_2019.csv")) %>%
  mutate(type = "Scotland") %>%
  select(c("id", "weight", "height", "age", "sex", "wt_int", "bmi", "bmi_class", "intake", "type"))

df_wales_cleaned = read_csv(here("inputs/processed/nsw_2019.csv")) %>%
  mutate(type = "Wales") %>%
  select(c("id", "weight", "height", "age", "sex", "wt_int", "bmi", "bmi_class", "intake", "type"))


all_df = rbind(df_england_cleaned, df_scotland_cleaned, df_wales_cleaned)






data_breaks_1 <- data.frame(start = c(-Inf, 18.5, 25, 30,   32.5, 35,   37.5, 40),  # Create data with breaks
                          end =     c(18.5, 25, 30, 32.5, 35,   37.5, 40,   Inf),
                          colors = gray.colors(8),
                          labels = c("Underweight", "Normal", "Overweight", "Ob1", "Ob2", "Ob3", "Ob4", "Morbidly Obese"),
                          pos_x = c(15, 22, 27, 31, 33, 36, 38, 45),
                          pos_y = rep(0.15,8))



# 2019 Plot - England
ggplot() +
  geom_rect(data = data_breaks,
            aes(xmin = start,
                xmax = end,
                ymin = -Inf,
                ymax = Inf,
                fill = colors),
            alpha=0.5) +
  geom_text(data = data_breaks, aes(x = pos_x, y = pos_y, label = labels)) + 
  geom_density(data = df_england_cleaned, aes(x = bmi, group = type, color = type, weight = wt_int), size = 1) +
  xlim(12,50) +
  labs(x = "BMI",
       y = "Density",
       color = "",
       title = "BMI Distribution") +
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta") +
  scale_color_manual(values=c("#0000FF")) +
  scale_fill_manual(values = rev(gray.colors(5))) +
  guides(fill = "none")


# Plot for 2019 and 1991
ggplot() +
  geom_rect(data = data_breaks,
            aes(xmin = start,
                xmax = end,
                ymin = -Inf,
                ymax = Inf,
                fill = colors),
            alpha=0.5) +
  geom_text(data = data_breaks, aes(x = pos_x, y = pos_y, label = labels)) + 
  geom_density(data = all_df, aes(x = bmi, group = type, color = type, weight = wt_int), size = 1) +
  xlim(12,50) +
  labs(x = "BMI",
       y = "Density",
       color = "",
       title = "BMI Distribution") +
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta") +
  scale_color_manual(values=c("#FDB633", "#0000FF", "#f6a4b7")) +  # "#ff6e47",
  scale_fill_manual(values = rev(gray.colors(5))) +
  guides(fill = "none") -> bmi_density_plot_england_scotland_wales




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


# colnames(df_4b_scotland)

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
       title = "BMI Distribution") +
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


# Plot for England, Scotland and Wales
ggplot() +
  geom_rect(data = data_breaks_1,
            aes(xmin = start,
                xmax = end,
                ymin = -Inf,
                ymax = Inf,
                fill = colors),
            alpha=0.5) +
  geom_text(data = data_breaks_1, aes(x = pos_x, y = pos_y, label = labels)) + 
  geom_density(data = df_england, aes(x = bmi, group = type, color = type, weight = wt_int), size = 1) +
  xlim(12,50) +
  labs(x = "BMI",
       y = "Density",
       color = "",
       title = "BMI Distribution") +
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta") +
  scale_color_manual(values=c("#FDB633", "#0000FF", "#f6a4b7")) +  # "#ff6e47",
  scale_fill_manual(values = rev(gray.colors(8))) +
  guides(fill = "none")


# Plot for England, Scotland and Wales
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
       title = "BMI Distribution") +
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta") +
  scale_color_manual(values=c("#FDB633", "#0000FF", "#f6a4b7")) +  # "#ff6e47",
  scale_fill_manual(values = rev(gray.colors(8))) +
  guides(fill = "none")


# Plot for England, Scotland and Wales
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
       title = "BMI Distribution") +
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta") +
  scale_color_manual(values=c("#FDB633", "#0000FF", "#f6a4b7")) +  # "#ff6e47",
  scale_fill_manual(values = rev(gray.colors(8))) +
  guides(fill = "none")





df_4b_england_3 = read_csv("C:/git/ahl_obesity_blueprint/outputs/policy_4b/policy_4b_adult_england_bmi.csv") %>%
  mutate(type = "England_policy") %>%
  select(c("id", "weight", "height", "age", "sex", "wt_int", "bmi_3_class", "intake", "type", "bmi_3"))


df_4b_scotland_3 = read_csv("C:/git/ahl_obesity_blueprint/outputs/policy_4b/policy_4b_adult_scotland_bmi.csv") %>%
  mutate(type = "Scotland_policy") %>%
  select(c("id", "weight", "height", "age", "sex", "wt_int", "bmi_3_class", "intake", "type", "bmi_3"))


df_4b_wales_3 = read_csv("C:/git/ahl_obesity_blueprint/outputs/policy_4b/policy_4b_adult_wales_bmi.csv") %>%
  mutate(type = "Wales_policy") %>%
  select(c("id", "weight", "height", "age", "sex", "wt_int", "bmi_3_class", "intake", "type", "bmi_3"))


# colnames(df_4b_scotland)

all_4b_df_3 = rbind(df_4b_england_3, df_4b_scotland_3, df_4b_wales_3) %>%
  rename("bmi" = "bmi_3",
         "bmi_class" = "bmi_3_class")

all_baseline_4b_df_3 = rbind(all_df, all_4b_df)


df_baseline_3 = all_baseline_4b_df_3 %>%
  filter(type %in% c("England", "Scotland", "Wales"))

df_england_3 = all_baseline_4b_df_3 %>%
  filter(type %in% c("England", "England_policy"))

df_scotland_3 = all_baseline_4b_df_3 %>%
  filter(type %in% c("Scotland", "Scotland_policy"))

df_wales_3 = all_baseline_4b_df %>%
  filter(type %in% c("Wales", "Wales_policy"))


df_endline_3 = all_baseline_4b_df_3 %>%
  filter(type %in% c("England_policy", "Scotland_policy", "Wales_policy"))



ggplot() +
  geom_rect(data = data_breaks_1,
            aes(xmin = start,
                xmax = end,
                ymin = -Inf,
                ymax = Inf,
                fill = colors),
            alpha=0.5) +
  geom_text(data = data_breaks_1, aes(x = pos_x, y = pos_y, label = labels)) + 
  geom_density(data = df_endline_3, aes(x = bmi, group = type, color = type, weight = wt_int), size = 1) +
  xlim(12,50) +
  labs(x = "BMI",
       y = "Density",
       color = "",
       title = "BMI Distribution") +
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta") +
  scale_color_manual(values=c("#FDB633", "#0000FF", "#f6a4b7")) +  # "#ff6e47",
  scale_fill_manual(values = rev(gray.colors(8))) +
  guides(fill = "none")




df_8a_england_3 = read_csv("C:/git/ahl_obesity_blueprint/outputs/policy_8a/policy_8a_adult_england_bmi.csv") %>%
  mutate(type = "England_policy") %>%
  select(c("id", "weight", "height", "age", "sex", "wt_int", "bmi_3_class", "intake", "type", "bmi_3"))


df_8a_scotland_3 = read_csv("C:/git/ahl_obesity_blueprint/outputs/policy_8a/policy_8a_adult_scotland_bmi.csv") %>%
  mutate(type = "Scotland_policy") %>%
  select(c("id", "weight", "height", "age", "sex", "wt_int", "bmi_3_class", "intake", "type", "bmi_3"))


df_8a_wales_3 = read_csv("C:/git/ahl_obesity_blueprint/outputs/policy_8a/policy_8a_adult_wales_bmi.csv") %>%
  mutate(type = "Wales_policy") %>%
  select(c("id", "weight", "height", "age", "sex", "wt_int", "bmi_3_class", "intake", "type", "bmi_3"))


# colnames(df_4b_scotland)

all_8a_df_3 = rbind(df_8a_england_3, df_8a_scotland_3, df_8a_wales_3) %>%
  rename("bmi" = "bmi_3",
         "bmi_class" = "bmi_3_class")

all_baseline_8a_df_3 = rbind(all_df, all_8a_df_3)


df_baseline_3 = all_baseline_8a_df_3 %>%
  filter(type %in% c("England", "Scotland", "Wales"))

df_england_3 = all_baseline_8a_df_3 %>%
  filter(type %in% c("England", "England_policy"))

df_scotland_3 = all_baseline_8a_df_3 %>%
  filter(type %in% c("Scotland", "Scotland_policy"))

df_wales_3 = all_baseline_8a_df_3 %>%
  filter(type %in% c("Wales", "Wales_policy"))


df_endline_3 = all_baseline_8a_df_3 %>%
  filter(type %in% c("England_policy", "Scotland_policy", "Wales_policy"))



# policy 8a

df_8a_england_5 = read_csv("C:/git/ahl_obesity_blueprint/outputs/policy_8a/policy_8a_adult_england_bmi.csv") %>%
  mutate(type = "England_policy") %>%
  select(c("id", "weight", "height", "age", "sex", "wt_int", "bmi_5_class", "intake", "type", "bmi_5"))


df_8a_scotland_5 = read_csv("C:/git/ahl_obesity_blueprint/outputs/policy_8a/policy_8a_adult_scotland_bmi.csv") %>%
  mutate(type = "Scotland_policy") %>%
  select(c("id", "weight", "height", "age", "sex", "wt_int", "bmi_5_class", "intake", "type", "bmi_5"))


df_8a_wales_5 = read_csv("C:/git/ahl_obesity_blueprint/outputs/policy_8a/policy_8a_adult_wales_bmi.csv") %>%
  mutate(type = "Wales_policy") %>%
  select(c("id", "weight", "height", "age", "sex", "wt_int", "bmi_5_class", "intake", "type", "bmi_5"))


# colnames(df_4b_scotland)

all_8a_df_5 = rbind(df_8a_england_5, df_8a_scotland_5, df_8a_wales_5) %>%
  rename("bmi" = "bmi_5",
         "bmi_class" = "bmi_5_class")

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



# policy 8a - endline
ggplot() +
  geom_rect(data = data_breaks_1,
            aes(xmin = start,
                xmax = end,
                ymin = -Inf,
                ymax = Inf,
                fill = colors),
            alpha=0.5) +
  geom_text(data = data_breaks_1, aes(x = pos_x, y = pos_y, label = labels)) + 
  geom_density(data = df_endline_5_8a, aes(x = bmi, group = type, color = type, weight = wt_int), size = 1) +
  xlim(12,50) +
  labs(x = "BMI",
       y = "Density",
       color = "",
       title = "BMI Distribution - 8a - 5 years") +
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta") +
  scale_color_manual(values=c("#FDB633", "#0000FF", "#f6a4b7")) +  # "#ff6e47",
  scale_fill_manual(values = rev(gray.colors(8))) +
  guides(fill = "none")


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

df_case_height = df_scotland_wales_8a %>%
  filter(type %in% c("Scotland", "Wales")) %>%
  filter(bmi >= 30 & bmi <= 35)

df_case_height %>%
  filter(type %in% c("Wales")) %>%
  pull(height) %>%
  min()

df_case_height %>%
  filter(type %in% c("Scotland")) %>%
  pull(height) %>%
  min()

ggplot() +
  # geom_rect(data = data_breaks_1,
  #           aes(xmin = start,
  #               xmax = end,
  #               ymin = -Inf,
  #               ymax = Inf,
  #               fill = colors),
  #           alpha=0.5) +
  # geom_text(data = data_breaks_1, aes(x = pos_x, y = pos_y, label = labels)) + 
  geom_density(data = df_case_height, aes(x = height, group = type, color = type, weight = wt_int), size = 1) +
  # xlim(12,50) +
  labs(x = "BMI",
       y = "Density",
       color = "",
       title = "Height Distribution for BMI 30 - 35") +
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta") +
  scale_color_manual(values=c("#FDB633", "#0000FF", "#f6a4b7", "#ff6e47")) +  # "#ff6e47",
  scale_fill_manual(values = rev(gray.colors(8))) +
  guides(fill = "none")


wales_height_summary = df_case_height %>%
  filter(type %in% c("Wales")) %>%
  dplyr::summarize(
    mean_height = mean(height),
    min_height = min(height),
    median_height = median(height),
    max_height = max(height)
  )


scotland_height_summary = df_case_height %>%
  filter(type %in% c("Scotland")) %>%
  dplyr::summarize(
    mean_height = mean(height),
    min_height = min(height),
    median_height = median(height),
    max_height = max(height)
  )

wales_age_summary = df_case_height %>%
  filter(type %in% c("Wales")) %>%
  dplyr::summarize(
    mean = mean(age),
    min = min(age),
    median = median(age),
    max = max(age)
  )

scotland_age_summary = df_case_height %>%
  filter(type %in% c("Scotland")) %>%
  dplyr::summarize(
    mean = mean(age),
    min = min(age),
    median = median(age),
    max = max(age)
  )



ggplot() +
  # geom_rect(data = data_breaks_1,
  #           aes(xmin = start,
  #               xmax = end,
  #               ymin = -Inf,
  #               ymax = Inf,
  #               fill = colors),
  #           alpha=0.5) +
  # geom_text(data = data_breaks_1, aes(x = pos_x, y = pos_y, label = labels)) + 
  geom_density(data = df_case_height, aes(x = age, group = type, color = type, weight = wt_int), size = 1) +
  # xlim(12,50) +
  labs(x = "Age",
       y = "Density",
       color = "",
       title = "Age Distribution for BMI 30 - 35") +
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta") +
  scale_color_manual(values=c("#FDB633", "#0000FF", "#f6a4b7", "#ff6e47")) +  # "#ff6e47",
  scale_fill_manual(values = rev(gray.colors(8))) +
  guides(fill = "none")




wales_sex_summary = df_case_height %>%
  filter(type %in% c("Wales")) %>%
  count(sex, wt = wt_int) %>% 
  mutate(freq = n/sum(n)*100)

scotland_sex_summary = df_case_height %>%
  filter(type %in% c("Scotland")) %>%
  count(sex, wt = wt_int) %>% 
  mutate(freq = n/sum(n)*100)


scotland_age_summary = df_case_height %>%
  filter(type %in% c("Scotland")) %>%
  dplyr::summarize(
    mean = mean(age),
    min = min(age),
    median = median(age),
    max = max(age)
  )