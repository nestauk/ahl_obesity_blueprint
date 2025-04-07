
library(tidyverse)
library(here)
library(bw)
library(survey)
library(sitar)
library(jsonlite)


intake_change = 50

effect_weighting = read.csv(file = "C:/Users/Anish.Chacko/Downloads/energy_ref_data/effect_weighting.csv")

uk90_bmi_refdata = generate_bmi_refdata(sitar::uk90)

bmi_refdata_100centiles = generate_bmi_refdata_100centiles(sitar::uk90)


bmi_refdata_100centiles = bmi_refdata_100centiles %>%
  select(-c(L.bmi, M.bmi, S.bmi)) %>%
  unite(col = "sex_years", "sex", "years", sep = "_") %>%
  pivot_longer(cols= c(starts_with("p")), names_to = "centile", values_to = "bmi") %>%
  separate("sex_years", into = c("sex", "age"), sep = "_") %>%
  mutate(centile = substr(centile, 3, nchar(centile)))



df_child <- read_csv(here("inputs/processed/hse_2019_children.csv")) %>%
  rowwise() %>%
  mutate(bmi_cat_baseline = calculate_bmi_category(age = age, sex = sex, bmi = bmi, df_B = uk90_bmi_refdata)) %>%
  mutate(bmi_centile = look_up_percentile(age = age, sex = sex, bmi = bmi, data_B = bmi_refdata_100centiles)) %>%
  mutate(intake_change = ifelse(bmi_cat_baseline %in% c("normal", "overweight", "obese"),
                                calculate_proportional_ei_change(age = age, sex = sex, bmi = bmi, intake_change = -intake_change, 
                                                        prop_weight_data = effect_weighting, bmi_ref_data = bmi_refdata_100centiles), 0)) %>%
  ungroup() %>%
  mutate(new_ei = intake_hox + intake_change) 



df_child = df_child %>%
  mutate(new_rmr = new_ei/pal) %>%
  mutate(new_wt = case_when(sex == 1 ~ (((new_rmr * 4.184) - 2876)/66.9),
                            sex == 2 ~ (((new_rmr * 4.184) - 3230)/47.9))) %>%
  mutate(wt_diff = weight - new_wt) %>%
  mutate(new_bmi = new_wt/(height/100)^2) %>%
  rowwise() %>%
  mutate(bmi_cat_1 = calculate_bmi_category(age = age, sex = sex, bmi = new_bmi, df_B = uk90_bmi_refdata)) %>%
  ungroup() 

df_child = df_child %>%
  mutate(bmi_diff = bmi - new_bmi)




child_bmi_change = rbind(
  df_child %>% 
    count(bmi_cat_baseline, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Baseline") %>% 
    rename(BMI = bmi_cat_baseline),
  df_child %>% 
    count(bmi_cat_1, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 1") %>% 
    rename(BMI = bmi_cat_1))




child_bmi_change = child_bmi_change %>%
  mutate(BMI = factor(BMI, levels = c("underweight", "normal", "overweight", "obese"))) %>%
  as.data.frame()


# output bar plot of BMI categories distributions
child_bar_plot = child_bmi_change %>%
  ggplot(., aes(y = freq, x = BMI, fill = type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_ipsum() +
  labs(fill = "", 
       title = "BMI Distribution: Henry Eq", 
       y = "Prevalence - %",
       subtitle = "Children - Proportional kcals") +
  theme_ipsum(base_size = 8, axis_title_size = 8) + #, base_family="Averta"
  theme(legend.position = "top")


child_bar_plot



# Output 2: Table of year wise prevalence of obesity
child_bmi_change_year = child_bmi_change %>%
  select(-c(n)) %>%
  pivot_wider(., names_from = BMI, values_from = freq)







sum(df_child$bmi_centile < 20)

calculate

write.csv(df_child, file = "C:/Users/Anish.Chacko/Downloads/blueprint/df_child_full_2.csv" )

calculate_stats <- function(data) {
  stats <- c(
    mean = mean(data),
    median = median(data),
    variance = var(data),
    sd = sd(data),
    min = min(data),
    max = max(data),
    q25 = quantile(data, probs = 0.25),
    q50 = quantile(data, probs = 0.5),
    q75 = quantile(data, probs = 0.75)
  )
  return(stats)
}
options(scipen = 999)
calculate_stats(df_child$bmi_diff)