rm(list = ls())

library(here)
library(bw)


# explore the vignettes
browseVignettes("bw")

# replicate some of the example in the vignettes

female_model2 <- adult_weight(bw = 80, ht = 1.8, age = 40, sex = "female", 
                              EIchange = rep(-250, 365))

model_plot(female_model2, "Body_Weight")

model_plot(female_model2, "Energy_Intake")

# from EI to weight

# run with own data

df <- read_csv(here("inputs/processed/hse_2019.csv")) %>% 
  head(20) %>% 
  mutate(sex = ifelse(sex == 1, "male", "female")) %>% 
  mutate(intake_diff = -intake*0.2)

eichange <- t(apply(df, 1, function(x) rep(as.numeric(x["intake_diff"]), 365)))

model_weight <- adult_weight(df$weight, df$height/100, df$age, df$sex, eichange)

model_plot(model_weight, "Body_Weight")

model_plot(model_weight, "Energy_Intake")

# just for example - not actual
design <-  svydesign(ids=~df$psu, 
                     nest = T,
                     data=df,
                     weights=df$wt_int)

model_mean(model_weight, group = df$sex, design = design)


# from weight to EI

source(here("utils/find_EI.R"))


ei_model <- list(id = df$id,
     bw = df$weight,
     ht = df$height/100,
     age = df$age,
     sex = df$sex,
     weight_goal = df$weight*0.9,
     days = 365,
     ei_limit = 1000,
     pal = 1.6) %>% 
  pmap_dfr(., find_EI)
