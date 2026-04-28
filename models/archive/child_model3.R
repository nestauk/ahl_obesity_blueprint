

## appraoch using bmi, FM and FFM
# approach using projected bmi, height and weight


uk90_height_refdata = generate_height_refdata("C:/Users/Anish.Chacko/Downloads/ht_ref_data/cole-nine-centiles-uk-who-female-height.json",
                                              "C:/Users/Anish.Chacko/Downloads/ht_ref_data/cole-nine-centiles-uk-who-male-height.json")



uk90_bmi_refdata = generate_bmi_refdata(sitar::uk90)



bmi_refdata_100centiles = generate_bmi_refdata_100centiles(sitar::uk90)


bmi_refdata_100centiles = bmi_refdata_100centiles %>%
  select(-c(L.bmi, M.bmi, S.bmi)) %>%
  unite(col = "sex_years", "sex", "years", sep = "_") %>%
  pivot_longer(cols= c(starts_with("p")), names_to = "centile", values_to = "bmi") %>%
  separate("sex_years", into = c("sex", "age"), sep = "_") %>%
  mutate(centile = substr(centile, 3, nchar(centile)))

#test_ht_ref = generate_height_refdata_100centiles(sitar::uk90)


height_refdata_100centile = generate_height_refdata_100centiles(sitar::uk90)

height_refdata_100centile = height_refdata_100centile %>%
  select(-c(L.ht, M.ht, S.ht)) %>%
  unite(col = "sex_years", "sex", "years", sep = "_") %>%
  pivot_longer(cols= c(starts_with("p")), names_to = "centile", values_to = "height") %>%
  separate("sex_years", into = c("sex", "age"), sep = "_") %>%
  mutate(centile = substr(centile, 3, nchar(centile)))


write.csv(bmi_refdata_100centiles, file = "C:/git/ahl_obesity_blueprint/outputs/data/bmi_refdata.csv" )

df_child3 <- read_csv(here("inputs/processed/hse_2019_children.csv")) %>%  # we are reading a file that contains energy intake, BMI weight class and other variables reated during the pre-processing stage.
  #mutate(sex = ifelse(sex == 1, "male", "female")) %>% 
  filter(fm_hudda > 0) %>%
  #mutate(intake_diff = ifelse(bmi_cat_baseline %in% c("underweight", "normal"), 0, child_intake_change )) %>%
  rowwise() %>%
  #mutate(bmi_cat_b = calculate_bmi_category(age = age, sex = sex, bmi = bmi, df_B = generate_bmi_refdata(sitar::uk90))) %>%
  mutate(percentile = look_up_percentile(age = age, sex = sex, bmi = bmi, data_B = bmi_refdata_100centiles)) %>%
  mutate(proj_bmi_365 = lookup_projected_bmi(age = age, sex = sex, bmi = bmi, years_added = 1, data_B = bmi_refdata_100centiles),
         proj_bmi_730 = lookup_projected_bmi(age = age, sex = sex, bmi = bmi, years_added = 2, data_B = bmi_refdata_100centiles),
         proj_bmi_1095 = lookup_projected_bmi(age = age, sex = sex, bmi = bmi, years_added = 3, data_B = bmi_refdata_100centiles),
         proj_bmi_1460 = lookup_projected_bmi(age = age, sex = sex, bmi = bmi, years_added = 4, data_B = bmi_refdata_100centiles),
         proj_bmi_1825 = lookup_projected_bmi(age = age, sex = sex, bmi = bmi, years_added = 5, data_B = bmi_refdata_100centiles))%>%
  mutate(proj_ht_365  = lookup_projected_height(age, sex, height, data_B = height_refdata_100centile, years_added = 1),
         proj_ht_730  = lookup_projected_height(age, sex, height, data_B = height_refdata_100centile, years_added = 2),
         proj_ht_1095 = lookup_projected_height(age, sex, height, data_B = height_refdata_100centile, years_added = 3),
         proj_ht_1460 = lookup_projected_height(age, sex, height, data_B = height_refdata_100centile, years_added = 4),
         proj_ht_1825 = lookup_projected_height(age, sex, height, data_B = height_refdata_100centile, years_added = 5)) %>%
  ungroup() %>%
  mutate(proj_wt_365 = proj_bmi_365 * (proj_ht_365/100)^2,
         proj_wt_730 = proj_bmi_730 * (proj_ht_730/100)^2,
         proj_wt_1095 = proj_bmi_1095 * (proj_ht_1095/100)^2,
         proj_wt_1460 = proj_bmi_1460 * (proj_ht_1460/100)^2,
         proj_wt_1825 = proj_bmi_1825 * (proj_ht_1825/100)^2) %>%
  mutate(proj_rmr_365 = case_when(sex == 1 ~ (((66.9*proj_wt_365) + 2876)/4.184), TRUE ~ ( ((47.9*proj_wt_365) + 3230)/4.184 )),
         proj_rmr_730 = case_when(sex == 1 ~ (((66.9*proj_wt_730) + 2876)/4.184), TRUE ~ ( ((47.9*proj_wt_730) + 3230)/4.184 )),
         proj_rmr_1095 = case_when(sex == 1 ~ (((66.9*proj_wt_1095) + 2876)/4.184), TRUE ~ ( ((47.9*proj_wt_1095) + 3230)/4.184 )),
         proj_rmr_1460 = case_when(sex == 1 ~ (((66.9*proj_wt_1460) + 2876)/4.184), TRUE ~ ( ((47.9*proj_wt_1460) + 3230)/4.184 )),
         proj_rmr_1825 = case_when(sex == 1 ~ (((66.9*proj_wt_1825) + 2876)/4.184), TRUE ~ ( ((47.9*proj_wt_1825) + 3230)/4.184 ))) %>%
  mutate(proj_pal_365  = case_when(age + 1  < 3 ~ 1.40, age + 1 >=3 & age + 1 < 10 ~ 1.58, age + 1 >=10 & age + 1 <= 18 ~ 1.75, age + 1 >= 19 ~ 1.6),
         proj_pal_730  = case_when(age + 2  < 3 ~ 1.40, age + 2 >=3 & age + 2 < 10 ~ 1.58, age + 2 >=10 & age + 2 <= 18 ~ 1.75, age + 2 >= 19 ~ 1.6),
         proj_pal_1095 = case_when(age + 3  < 3 ~ 1.40, age + 3 >=3 & age + 3 < 10 ~ 1.58, age + 3 >=10 & age + 3 <= 18 ~ 1.75, age + 3 >= 19 ~ 1.6),
         proj_pal_1460 = case_when(age + 4  < 3 ~ 1.40, age + 4 >=3 & age + 4 < 10 ~ 1.58, age + 4 >=10 & age + 4 <= 18 ~ 1.75, age + 4 >= 19 ~ 1.6),
         proj_pal_1825 = case_when(age + 5  < 3 ~ 1.40, age + 5 >=3 & age + 5 < 10 ~ 1.58, age + 5 >=10 & age + 5 <= 18 ~ 1.75, age + 5 >= 19 ~ 1.6)) %>%
  mutate(ei_365 = proj_rmr_365 * proj_pal_365,
         ei_730 = proj_rmr_730 * proj_pal_730,
         ei_1095 = proj_rmr_1095 * proj_pal_1095,
         ei_1460 = proj_rmr_1460 * proj_pal_1460,
         ei_1825 = proj_rmr_1825 * proj_pal_1825)



#library("bw")

energy_matrix3 = apply(df_child3, 1, generate_interpolated_energy)

energy_matrix3 = t(energy_matrix3)


upd_df3 = cbind(df_child3, energy_matrix3)




mean_ei_5_10 = mean(child_energy_intake$`1`[child_energy_intake$bmi_cat_b == "obese" | child_energy_intake$bmi_cat_b == "overweight" 
                                            & child_energy_intake$age >= 5 & child_energy_intake$age <= 10])


mean_ei_11_15 = mean(child_energy_intake$`1`[child_energy_intake$bmi_cat_b == "obese" | child_energy_intake$bmi_cat_b == "overweight" 
                                             & child_energy_intake$age >= 11 & child_energy_intake$age < 15])




#browseVignettes("bw")

df_child3 <- df_child3 %>%
  mutate(sex = ifelse(sex == 1, "male", "female")) 

# model 1 with fm and ffm
child_model_weight3 = child_weight(age = df_child3$age,
                                  sex =  df_child3$sex, 
                                  FM = df_child3$fm_hudda, 
                                  FFM = df_child3$ffm_hudda, 
                                  EI = energy_matrix3,   
                                  days = (365*5),
                                  checkValues = TRUE)


child_bw_h3 = as.data.frame(child_model_weight3[["Body_Weight"]]) %>%
  mutate(bw_diff = V1825 - V1) %>%
  select(c(V1, V365, V730, V1095, V1460, V1825, bw_diff)) %>%
  rename(bw_1 = V1, bw_365 = V365, bw_730 = V730, 
         bw_1095 = V1095, bw_1460 = V1460, bw_1825 = V1825)



child_age_h3 = as.data.frame(child_model_weight3[["Age"]]) %>%
  mutate(age_diff = V1825 - V1) %>%
  select(c(V1, V365, V730, V1095, V1460, V1825, age_diff)) %>%
  rename(age_1 = V1, age_365 = V365, age_730 = V730, 
         age_1095 = V1095, age_1460 = V1460, age_1825 = V1825)


child_ffm_h3 = as.data.frame(child_model_weight3[["Fat_Free_Mass"]]) %>%
  mutate(ffm_diff = V1825 - V1) %>%
  select(c(V1, V365, V730, V1095, V1460, V1825, ffm_diff)) %>%
  rename(ffm_1 = V1, ffm_365 = V365, ffm_730 = V730,
         ffm_1095 = V1095, ffm_1460 = V1460, ffm_1825 = V1825)



child_fm_h3 = as.data.frame(child_model_weight3[["Fat_Mass"]]) %>%
  mutate(fm_diff = V1825 - V1) %>%
  select(c(V1, V365, V730, V1095, V1460, V1825, fm_diff))  %>%
  rename(fm_1 = V1, fm_365 = V365, fm_730 = V730,
         fm_1095 = V1095, fm_1460 = V1460, fm_1825 = V1825)



child_age_bw_ffm_fm3 = cbind(child_bw_h3, child_age_h3, child_ffm_h3, child_fm_h3) %>%
  select(age_1, bw_1, fm_1, ffm_1,
         age_365, bw_365, fm_365, ffm_365,
         age_730, bw_730, fm_730, ffm_730,
         age_1095, bw_1095, fm_1095, ffm_1095,
         age_1460, bw_1460, fm_1460, ffm_1460,
         age_1825, bw_1825, fm_1825, ffm_1825)



new_df_child3 = cbind(df_child3, child_age_bw_ffm_fm3) %>%
  rowwise() %>%
  mutate(ht_365  = lookup_projected_height(age, sex, height, data_B = height_refdata_100centile, years_added = 1)) %>%
  mutate(ht_730  = lookup_projected_height(age, sex, height, data_B = height_refdata_100centile, years_added = 2)) %>%
  mutate(ht_1095 = lookup_projected_height(age, sex, height, data_B = height_refdata_100centile, years_added = 3)) %>%
  mutate(ht_1460 = lookup_projected_height(age, sex, height, data_B = height_refdata_100centile, years_added = 4)) %>%
  mutate(ht_1825 = lookup_projected_height(age, sex, height, data_B = height_refdata_100centile, years_added = 5)) %>%
  ungroup() %>%
  mutate(bmi_365  = bw_365  / (ht_365/100)^2,
         bmi_730  = bw_730  / (ht_730/100)^2,
         bmi_1095 = bw_1095 / (ht_1095/100)^2,
         bmi_1460 = bw_1460 / (ht_1460/100)^2,
         bmi_1825 = bw_1825 / (ht_1825/100)^2) %>%
  rowwise() %>%
  mutate(bmi_cat_1 = calculate_bmi_category(age = age, sex = sex, bmi = bmi, df_B = uk90_bmi_refdata),
         bmi_cat_365 = calculate_bmi_category(age = age + 1, sex = sex, bmi = bmi_365, df_B = uk90_bmi_refdata),
         bmi_cat_730 = calculate_bmi_category(age = age + 2, sex = sex, bmi = bmi_730, df_B = uk90_bmi_refdata),
         bmi_cat_1095 = calculate_bmi_category(age = age + 3, sex = sex, bmi = bmi_1095, df_B = uk90_bmi_refdata),
         bmi_cat_1460 = calculate_bmi_category(age = age + 4, sex = sex, bmi = bmi_1460, df_B = uk90_bmi_refdata),
         bmi_cat_1825 = calculate_bmi_category(age = age + 5, sex = sex, bmi = bmi_1825, df_B = uk90_bmi_refdata)) %>%
  ungroup()



   


child_bmi_change3 = rbind(
  new_df_child3 %>% 
    count(bmi_cat_1, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Baseline") %>% 
    rename(BMI = bmi_cat_1),
  new_df_child3 %>% 
    count(bmi_cat_365, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 1") %>% 
    rename(BMI = bmi_cat_365),
  new_df_child3 %>% 
    count(bmi_cat_730, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 2") %>% 
    rename(BMI = bmi_cat_730),
  new_df_child3 %>% 
    count(bmi_cat_1095, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 3") %>% 
    rename(BMI = bmi_cat_1095),
  new_df_child3 %>% 
    count(bmi_cat_1460, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 4") %>% 
    rename(BMI = bmi_cat_1460),
  new_df_child3 %>% 
    count(bmi_cat_1825, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 5") %>% 
    rename(BMI = bmi_cat_1825))

child_bmi_change3 = child_bmi_change3 %>%
  mutate(BMI = factor(BMI, levels = c("underweight", "normal", "overweight", "obese"))) %>%
  as.data.frame()


# output bar plot of BMI categories distributions
child_bar_plot3 = child_bmi_change3 %>%
  ggplot(., aes(y = freq, x = BMI, fill = type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_ipsum() +
  labs(fill = "", 
       title = "BMI Distribution - Approach 2", 
       y = "Prevalence - %",
       subtitle = "Children - No policy applied") +
  theme_ipsum(base_size = 8, axis_title_size = 8) + #, base_family="Averta"
  theme(legend.position = "top")


child_bar_plot3



# Output 2: Table of year wise prevalence of obesity
child_bmi_change_year3 = child_bmi_change3 %>%
  select(-c(n)) %>%
  pivot_wider(., names_from = BMI, values_from = freq)

write.csv(child_bmi_change_year3, file = "C:/git/ahl_obesity_blueprint/outputs/data/child_bmi_3.csv"  )

browseVignettes("bw")
#model_plot(child_model_weight, "Body_Weight")

#model_plot(child_model_weight, "Fat_Mass")

