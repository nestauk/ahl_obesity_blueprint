
########################
########################
########################

## code for height reference data:


install.packages("listviewer")
library(jsonlite)
library(listviewer)

tab_output = list()


female_ht_json = read_json("C:/Users/Anish.Chacko/Downloads/cole-nine-centiles-uk-who-female-height.json")


female_ht_json = read_json("C:/Users/Anish.Chacko/Downloads/cole-nine-centiles-uk-who-female-height.json",
                           simplifyVector = FALSE)

female_ht_json_1 = female_ht_json

rcpch_4_uk90child_female_ht = female_ht_json_1[[4]][["uk90_child"]][["female"]][["height"]]

ht_df = tibble(height_percentile = rcpch_4_uk90child_female_ht)

test_1 = as.data.frame (ht_df %>%
                          unnest_wider(height_percentile) %>%
                          select(sds, centile, data) %>%
                          unnest_longer(data) %>%
                          select(sds, centile, data) %>%
                          unnest_wider(data)) %>%
  select(sds, centile, x, y) %>%
  distinct()

test_2 = test_1 %>%
  pivot_wider(names_from = c(sds, centile), values_from = y)

# Rename the columns
colnames(test_2) <- c("age", "0.4", "2", "9", "25", "50", "75", "91", "98", "99.6")


write.csv(test_3, file = "C:/git/ahl_obesity_blueprint/outputs/rcpch_growth.csv")






uk90_3centile = read_json("C:/Users/Anish.Chacko/Downloads/centile/three-percent-centiles-uk-who-female-height.json")


rcpch_3pc_4_uk90child_female_ht = uk90_3centile[[4]][["uk90_child"]][["female"]][["height"]]

ht_3pc_4_uk90child = tibble(height_percentile = rcpch_3pc_4_uk90child_female_ht)

test_1 = as.data.frame (ht_3pc_4_uk90child %>%
                          unnest_wider(height_percentile) %>%
                          select(sds, centile, data) %>%
                          unnest_longer(data) %>%
                          select(sds, centile, data) %>%
                          unnest_wider(data)) %>%
  select(sds, centile, x, y) %>%
  distinct()

test_2 = test_1 %>%
  pivot_wider(names_from = c(sds, centile), values_from = y)



### three-percent-centiles-uk-who-female-height

three_percent_centile_female = read_json("C:/Users/Anish.Chacko/Downloads/centile/three-percent-centiles-uk-who-female-height.json")


three_percent_centile_female_ht = three_percent_centile_female[[4]][["uk90_child"]][["female"]][["height"]]


ht_3pc_4_uk90child = tibble(height_percentile_1 = three_percent_centile_female_ht)

test_1 = as.data.frame (ht_3pc_4_uk90child %>%
                          unnest_wider(height_percentile_1) %>%
                          select(sds, centile, data) %>%
                          unnest_longer(data) %>%
                          select(sds, centile, data) %>%
                          unnest_wider(data)) %>%
  select(sds, centile, x, y) %>%
  distinct()

test_2 = test_1 %>%
  pivot_wider(names_from = c(sds, centile), values_from = y) %>%
  rename("age | sd_percentile -->" = `x`)


write.csv(test_2, file = "C:/Users/Anish.Chacko/Downloads/centile/three-percent-centiles-uk-who-female-height.csv")



### cole-nine-centiles-uk-who-female-height

cole_nine_centile_female = read_json("C:/Users/Anish.Chacko/Downloads/centile/cole-nine-centiles-uk-who-female-height.json")


cole_nine_centile_female_ht = cole_nine_centile_female[[4]][["uk90_child"]][["female"]][["height"]]


ht_cole_4_uk90child = tibble(height_percentile_2 = cole_nine_centile_female_ht)

test_3 = as.data.frame (ht_cole_4_uk90child %>%
                          unnest_wider(height_percentile_2) %>%
                          select(sds, centile, data) %>%
                          unnest_longer(data) %>%
                          select(sds, centile, data) %>%
                          unnest_wider(data)) %>%
  select(sds, centile, x, y) %>%
  distinct()

test_4 = test_3 %>%
  pivot_wider(names_from = c(sds, centile), values_from = y) %>%
  rename("age | sd_percentile -->" = `x`)


write.csv(test_4, file = "C:/Users/Anish.Chacko/Downloads/centile/cole-nine-centiles-uk-who-female-height.csv")




########################
########################
########################



######### prefrom analysis function as on 09 Feb
# Calculating the weight loss or gain post intervention:
calculate_weight_loss_gain <- function(cohort, prev_cohorts) {
  weight_loss_gain <- ifelse(cohort == 2, 5.6, # effect size for those receiving TDR = -5.6 kgs
                             ifelse(cohort == 1, 2.4, # effect size for those receiving only education = -2.4 kgs
                                    ifelse( any(prev_cohorts != 0), -0.12, -0.4)))
  return(weight_loss_gain)
}




# Function to perform the cohort assignment, weight loss/ gain calculations 
perform_analysis <- function(df) {
  df <- df %>%
    mutate(cohort_1 = assign_cohort(., "bmi", 0)) # assigning to cohort 1 in year 1 to receive education or TDR
  
  
  
  df$weight_loss_y1 <- calculate_weight_loss_gain(df$cohort_1, c(0)) # setting weight loss or gain depending on assignment to TDR or education
  
  
  
  df <- df %>%
    mutate(weight_y1 = weight - weight_loss_y1, # calculating new weight at end of year 1
           bmi_y1 = weight_y1 / ((height/100)^2)) # calculating new BMI at end of year 1
  
  
  
  df <- df %>%
    mutate(cohort_2 = assign_cohort(., "bmi_y1", cohort_1)) # assigning to cohort 2 in year 2 to receive education or TDR
  
  
  
  df$weight_loss_y2 <- calculate_weight_loss_gain(df$cohort_2, c(df$cohort_1)) # setting weight loss or gain depending on assignment to TDR or education
  
  
  
  
  df <- df %>%
    mutate(weight_y2 = weight_y1 - weight_loss_y2, # calculating new weight at end of year 2
           bmi_y2 = weight_y2 / ((height/100)^2)) # calculating new BMI at end of year 2
  
  
  
  df <- df %>%
    mutate(cohort_3 = assign_cohort(., "bmi_y2", cohort_1 + cohort_2)) # assigning to cohort 3 in year 3 to receive education or TDR
  
  
  
  df$weight_loss_y3 <- calculate_weight_loss_gain(df$cohort_3, c(df$cohort_1, df$cohort_2)) # setting weight loss or gain depending on assignment to TDR or education
  
  
  
  df <- df %>%
    mutate(weight_y3 = weight_y2 - weight_loss_y3, # calculating new weight at end of year 3
           bmi_y3 = weight_y3 / ((height/100)^2)) # calculating new BMI at end of year 3
  
  
  
  df <- df %>%
    mutate(cohort_4 = assign_cohort(., "bmi_y3", cohort_1 + cohort_2 + cohort_3))  # assigning to cohort 4 in year 4 to receive education or TDR
  
  
  
  df$weight_loss_y4 <- calculate_weight_loss_gain(df$cohort_4, c(df$cohort_1, df$cohort_2, df$cohort_3)) # setting weight loss or gain depending on assignment to TDR or education
  
  
  
  df <- df %>%
    mutate(weight_y4 = weight_y3 - weight_loss_y4, # calculating new weight at end of year 4
           bmi_y4 = weight_y4 / ((height/100)^2)) # calculating new BMI at end of year 4
  
  
  
  df <- df %>%
    mutate(cohort_5 = assign_cohort(., "bmi_y4", cohort_1 + cohort_2 + cohort_3 + cohort_4)) # assigning to cohort 5 in year 5 to receive education or TDR
  
  
  
  df$weight_loss_y5 <- calculate_weight_loss_gain(df$cohort_5, c(df$cohort_1, df$cohort_2, df$cohort_3, df$cohort_4)) # setting weight loss or gain depending on assignment to TDR or education
  
  
  
  df <- df %>%
    mutate(weight_y5 = weight_y4 - weight_loss_y5, # calculating new weight at end of year 5
           bmi_y5 = weight_y5 / ((height/100)^2)) # calculating new BMI at end of year 5
  
  
  
  return(df)
}

###################


mutate(age_grp = case_when(#Age35g == 7 ~ (16+19)/2,
  Age35g == 8 ~ "20-24", 
  Age35g == 9 ~ "25-29",
  Age35g == 10 ~ "30-34",
  Age35g == 11 ~ "35-39",
  Age35g == 12 ~ "40-44",
  Age35g == 13 ~ "45-49",
  Age35g == 14 ~ "50-54",
  Age35g == 15 ~ "55-59",
  Age35g == 16 ~ "60-64",
  Age35g == 17 ~ "65-69",
  Age35g == 18 ~ "70-74",
  Age35g == 19 | Age35g == 20 | Age35g == 21 | Age35g == 22  ~ "75+",
  TRUE ~ 0)) %>%




source("requirements.R")
library(haven)


welsh_data = read_dta(file = "C:/Users/Anish.Chacko/Downloads/nsfw_2019/UKDA-8591-stata/stata/stata13/nsfw_2019.dta")


age_table = as.data.frame(table(welsh_data$Age))


col_names = as.data.frame(colnames(welsh_data))


table(policy_1_impact_england_child$post_df$age)

bmi_rows = subset(col_names, grepl("bmi", colnames(welsh_data)))

bmi = welsh_data %>%
  select(Dvhtcm,Dvbmi2, Dvbmilev2, Dvbmimorb2, Dvbmihealthy2, Dvbmiowob2, Dvbmiobese2)




#---------------------------------------------
# sample code







20.5
145.2
9.0

library(readxl)

test = read_excel(path = "C:/Users/Anish.Chacko/Downloads/LMSgrowth_win64bit (1)/LMSgrowth_win64bit/XLSTART/British1990.xls")

write.csv(test, file = "C:/git/ahl_obesity_blueprint/outputs/data/growth.csv")

design <-  svydesign(ids=~df$psu, 
                     nest = T,
                     data=df,
                     weights=df$wt_int)

library(sitar)

test_1 = uk90







names(test_2)
test_3 = test_1 %>%
  dplyr::group_by(x, sds, centile) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) 


test_1[duplicated(test_1[c("x", "sds", "centile")]), ]

write.csv(test_2, file = "C:/Users/Anish.Chacko/Downloads/tes_2.csv")

test_1[duplicated(test_1[c("x", "sds", "centile")]) | duplicated(test_1[c("x", "sds", "centile")], fromLast = TRUE), ]


%>%
  hoist(ht_df, l, x, y)








help(pluck)

jsonedit(female_ht_json, height = "800px", mode = "view")


test_json = read_json("C:/Users/Anish.Chacko/Downloads/SAFI.json")


female_ht_df = as.data.frame(female_ht_json)


test_1_1 = calculate_fat_mass(weight = 20.5, 
                              height = 1.452, 
                              age = 9, 
                              BA = 0, 
                              SA = 0, 
                              AO = 0, 
                              other = 0,
                              male = 1)
test_1_1

((1.51*24.247980) - (0.7*14) -  ((2.2/100)*43.8))
((1.51*bmi) - (0.7*age) -  ((1.4/100)*weight))
((1.51*26.51349) - (0.7*6) -  ((1.4/100)*27.1))


27.1
101.1
6
26.51349




17.0
104.3
6.0
2
15.627169

43.8
134.4
14.0
1
24.247980
# sum(df_2019_children$ffm <0)

# 1.51⋅BMI0−0.7⋅a−2.2100⋅BW0

# 0.546×BMI+0.0413×age+1.29×sex−9.25



ggplot(df_2019_children, aes(x= intake_msj, y = intake_hox, fill = intake_msj)) +
  geom_point()


ggplot(df_2019_children, aes(x= weight, y = intake_msj)) +
  geom_point()




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



## Analysis on child model:

# model 2 without inputting fm and ffm
# child_model_weight_1 = child_weight(age = df_child$age,
#                                     sex =  df_child$sex, 
#                                     #FM = df_child$fm_hudda, 
#                                     #FFM = df_child$ffm_hudda, 
#                                     EI = nt_ei,   
#                                     days = (365*5),
#                                     checkValues = TRUE)
# 


child_bw_h_1 = as.data.frame(child_model_weight[["Body_Weight"]])


test_2 = cbind(df_child, child_bw_h_1)




test1 = child_age_bw_ffm_fm %>%
  select(age_1, bw_1, bw_365, bw_730, bw_1095, bw_1460, bw_1825, age_1825)



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

library(sitar)

test = uk90


## density and bar plot
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


# density plot

density_plot = ggplot(post_df_adult, aes(x= bmi, fill = "bmi_start")) +
  geom_density( alpha = 0.5) +
  geom_density(data = post_df_adult, aes(x = bmi_5, fill = "bmi_end", weight = post_df_adult$wt_int, alpha = 0.5)) +
  labs(title = "bmi_start and bmi_end", x = "BMI", y = "density") +
  scale_fill_manual(values=c("#0000FF", "#F6A4B7"))

density_plot

# geom_density(data = all_f, aes(x = bmi, group = type, color = type, weight = wt_int), size = 3)


bmi_change_year_1 = bmi_change %>%
  select(-c(freq)) %>%
  pivot_wider(., names_from = BMI, values_from = n)



variable_names <- ls()

# Use mget to retrieve values associated with variable names
variable_values <- mget(variable_names)

# Combine names and values into a data frame
variable_data <- data.frame(
  Variable = variable_names,
  Value = variable_values)






# Pre-processing:


df_2019_adult <- read.table(here("inputs/raw/hse_2019_eul_20211006.tab"), sep = "\t", header = TRUE) %>% 
  filter(WtVal>0 & HtVal>0 & Age35g >7 ) %>% # remove missing height and weight and children
  mutate(age = case_when(#Age35g == 7 ~ (16+19)/2,
    Age35g == 8 ~ (20+24)/2,
    Age35g == 9 ~ (25+29)/2,
    Age35g == 10 ~ (30+34)/2,
    Age35g == 11 ~ (35+39)/2,
    Age35g == 12 ~ (40+44)/2,
    Age35g == 13 ~ (45+49)/2,
    Age35g == 14 ~ (50+54)/2,
    Age35g == 15 ~ (55+59)/2,
    Age35g == 16 ~ (60+64)/2,
    Age35g == 17 ~ (65+69)/2,
    Age35g == 18 ~ (70+74)/2,
    Age35g == 19 ~ (75+79)/2,
    Age35g == 20 ~ (80+84)/2,
    Age35g == 21 ~ (85+89)/2,
    Age35g == 22 ~ (90),
    TRUE ~ 0)) %>% 
  rename(weight = WtVal,
         height = HtVal,
         sex = Sex,
         bmi = BMIVal,
         id = SerialA,
         psu = PSU_SCR,
         strata = cluster94) %>% 
  dplyr::select(id, weight, height, age, sex, bmi, wt_int, psu, strata )  %>% # select variables needed
  mutate(pal = 1.6,
         rmr = case_when(sex == 1 ~ ((10 * weight) + (6.25 * height) - (5 * age) + 5),
                         TRUE ~ ((10 * weight) + (6.25 * height) - (5 * age) - 161))) %>% # sex = 2 female Miffin & St.Jeor
  mutate(bmi_class = case_when(bmi <= 18.5 ~ "underweight",
                               bmi > 18.5 & bmi < 25 ~ "normal",
                               bmi >= 25 & bmi < 30 ~ "overweight",
                               bmi >= 30 & bmi < 40 ~ "obese",
                               bmi >= 40 ~ "morbidly obese",
                               TRUE ~ "NA")) %>% 
  mutate(intake = pal*rmr)  # calculating value of energy intake 

write_csv(df_2019_adult, here("inputs/processed/hse_2019.csv"))





# Scotland

# select(CPSerialA, SYear,PSU, Strata, int19wt, cint19wt, bmival, htval, wtval, BMIvg5, CBMIg5_new, age, Sex, SIMD20_SGa, Ethnic05, totinc, eqv5_15, hedqul08)

# The data used for this analysis is Scottish Health Survey 2019 available on UK Data Service portal.
# First, the dataset is filtered by weight, height and age to ensure that all observations have a height and weight > 0 and is limited to Adults (age>= 18)
# Subsequently, variables are renamed for ease and further subset to include only the variables of interest for modelling obesity prevalance.

# The next step is to calculate the energy intake at baseline. This energy intake can be calculated by multiplying an individuals physical activity level (PAL) and
# basal metabolic rate (BMR). PAL values are available for different levels of physical activity but on average for the population we have assumed everyone to be
# engaged in light work (PAL = 1.6). BMR is assumed to be equal to resting metabolic rate (RMR) which is the energy cost of maintaining metabolic homeostasis.

# At the baseline, in that point in time, we assume that people are maintaining their weight, therefore, their energy intake equals the energy requirement for
# physical activities and for metabolic homeostasis, which is given by the equation: PAL * BMR



df_2019_adult_sc <- read.table(here("C:/Users/Anish.Chacko/Downloads/hs_scotland/shes19i_eul.tab"), sep = "\t", header = TRUE) %>% # reading in the raw data file into a dataframe
  filter(wtval>0 & htval>0 & age >= 18) %>% # removing observations with missing height and weight information as well as children as this is an adult model
  rename(weight = wtval,
         height = htval,
         sex = Sex,
         bmi_ur = bmi,
         bmi = bmival,
         id = CPSerialA,
         psu = PSU,
         wt_int = int19wt,
         strata = Strata) %>% 
  dplyr::select(id, weight, height, age, sex, bmi, wt_int, psu, strata) %>% # select variables needed for analysis and would be inputs for modelling
  mutate(pal = 1.6,
         rmr = case_when(sex == 1 ~ ((10 * weight) + (6.25 * height) - (5 * age) + 5),
                         TRUE ~ ((10 * weight) + (6.25 * height) - (5 * age) - 161))) %>% # Calculating an individuals resting metabolic rate using equations published in Mifflin & St.Jeor (1990)
  mutate(intake = pal*rmr) %>%  # calculating energy intake at baseline
  mutate(bmi_class = case_when(bmi <= 18.5 ~ "underweight",
                               bmi > 18.5 & bmi < 25 ~ "normal",
                               bmi >= 25 & bmi < 30 ~ "overweight",
                               bmi >= 30 & bmi < 40 ~ "obese",
                               bmi >= 40 ~ "morbidly obese",
                               TRUE ~ "NA"))  # classifying individuals into BMI categories based on BMI values at baseline

write_csv(df_2019_adult_sc, here("inputs/processed/shes_2019.csv")) # writing the output dataframe to a csv file to be read in later.






# Function to calculate new weights and BMI
calculate_new_weights_bmi <- function(df, weight_loss_gain, weight_col, height_col) {
  weight_y <- df[[weight_col]] - weight_loss_gain
  bmi_y <- weight_y / (df[[height_col]]^2)
  return(list(weight_y, bmi_y))
}


#### child model
#eichange_child <- t(apply(df_child, 1, function(x) rep(as.numeric(x["intake_diff"]), 365*1)))

#t_eichange_child <- t(apply(df_child, 1, function(x) rep(as.numeric(x["intake"]) + as.numeric(x["intake_diff"]), 365*1)))




intake_change = -50
implementation_duration = 365*5
df_child = read_csv(here("inputs/processed/hse_2019_children.csv"))


test_child = calculate_child_bmi_from_eichange(df = read_csv(here("inputs/processed/hse_2019_children.csv")),
                                               intake_change = -50,
                                               implementation_duration = 365*5)


test_child$bmi_category_plot

test_child$bmi_percent_prevalence







df2019 <- read.table(here("C:/Users/Anish.Chacko/Downloads/UKDA-8591-tab/tab/nsfw_2018-19.tab"), sep = "\t", header = TRUE)

#%>% 
  filter(WtVal>0 & HtVal>0 & Age35g >=7 ) %>% # remove missing height and weight and children
  mutate(age = case_when(Age35g == 7 ~ (16+19)/2,
                         Age35g == 8 ~ (20+24)/2,
                         Age35g == 9 ~ (25+29)/2,
                         Age35g == 10 ~ (30+34)/2,
                         Age35g == 11 ~ (35+39)/2,
                         Age35g == 12 ~ (40+44)/2,
                         Age35g == 13 ~ (45+49)/2,
                         Age35g == 14 ~ (50+54)/2,
                         Age35g == 15 ~ (55+59)/2,
                         Age35g == 16 ~ (60+64)/2,
                         Age35g == 17 ~ (65+69)/2,
                         Age35g == 18 ~ (70+74)/2,
                         Age35g == 19 ~ (75+79)/2,
                         Age35g == 20 ~ (80+84)/2,
                         Age35g == 21 ~ (85+89)/2,
                         Age35g == 22 ~ (90),
                         TRUE ~ 0)) %>% 
  rename(weight = WtVal,
         height = HtVal,
         sex = Sex,
         bmi = BMIVal,
         id = SerialA,
         psu = PSU_SCR,
         strata = cluster94) %>% 
  dplyr::select(id, weight, height, age, sex, bmi, wt_int, psu, strata )  %>% # select variables needed
  mutate(pal = 1.6,
         rmr = case_when(sex == 1 ~ ((10 * weight) + (6.25 * height) - (5 * age) + 5),
                         TRUE ~ ((10 * weight) + (6.25 * height) - (5 * age) - 161))) %>% # sex = 2 female Miffin & St.Jeor
  mutate(bmi_class = case_when(bmi <= 18.5 ~ "underweight",
                               bmi > 18.5 & bmi < 25 ~ "normal",
                               bmi >= 25 & bmi < 30 ~ "overweight",
                               bmi >= 30 & bmi < 40 ~ "obese",
                               bmi >= 40 ~ "morbidly obese",
                               TRUE ~ "NA")) %>% 
  mutate(intake = pal*rmr) 

write_csv(df2019, here("inputs/processed/hse_2019.csv"))















