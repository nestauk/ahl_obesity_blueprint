library(tidyverse)
library(here)
library(bw)
library(survey)
library(sitar)
library(hrbrthemes)


# FUNCTION 1: Generate BMI Reference Data using UK90 growth charts

#' Generate BMI Reference Data Using UK90 Growth Charts
#'
#' This function generates age- and sex-specific BMI reference data using the LMS 
#' method based on UK90 growth charts. It calculates BMI values corresponding 
#' to specific percentiles (2nd, 85th, and 95th) that are used for classifying 
#' child BMI categories as per NCMP (NHS England, 2023).
#'
#'INPUTS:
#' @param data_B Data frame. Input data containing age, sex, and the LMS parameters 
#'               for BMI (`L.bmi`, `M.bmi`, `S.bmi`). The data must include:
#'               - `years`: Age of the individual in years.
#'               - `sex`: Sex of the individual.
#'               - `L.bmi`, `M.bmi`, `S.bmi`: LMS parameters for BMI.
#'               This data format can be obtained from the `uk90` function in the 
#'               `sitar` package (Cole, 2023 - https://rdrr.io/cran/sitar/man/uk90.html).
#'
#' @return Data frame. A filtered and transformed dataset containing:
#'         - `age`: Age in years (limited to 4 to 20 years, inclusive).
#'         - `sex`: Sex of the individual.
#'         - `p_2`: BMI value corresponding to the 2nd percentile.
#'         - `p_85`: BMI value corresponding to the 85th percentile.
#'         - `p_95`: BMI value corresponding to the 95th percentile.
#'
#' @details 
#' - The LMS method is used to calculate z-scores and percentiles (Cole, 2012). 
#' 
#'   The formula for calculating a z-score is:
#'   \deqn{z = ((X / M)^L - 1) / (L * S)}
#'    where z is z-score;
#'          X is a measurement (of bmi, height, weight etc);
#'          L, M & S are parameters that summarise the normal distribution generated
#'                   from a Box-Cox transformation of measurements at each age (Cole, 2012).
#'                   (L = power of the Box-Cox transformation; M = Median; S = coefficient of variation)
#'           
#'   This formula is inverted to compute the measurement (X), given the L, M, 
#'   and S parameters and the z-score:
#'   \deqn{X = M * (1 + Z * L * S)^(1 / L)}
#' - Using this formula, the BMI values for the 2nd, 85th, and 95th percentiles are calculated 
#'   for each age and sex. We use these percentiles as they are the cutoffs used for 
#'   underweight, overweight and obese categories as part of NCMP (NHS England, 2023)
#'

generate_bmi_refdata = function(data_B){
  
  bmi_refdata = data_B %>%
    select(years, sex, L.bmi, M.bmi, S.bmi) %>%
    subset(years >= as.double(1) & years <= as.double(20)) %>% # filtered to limit to ages between 4 and 20, both inclusive
    mutate(p_2 = (M.bmi*(1 + L.bmi*S.bmi*-2.054)^(1/L.bmi)),
           p_85 = (M.bmi*(1 + L.bmi*S.bmi*1.036)^(1/L.bmi)),
           p_95 = (M.bmi*(1 + L.bmi*S.bmi*1.645)^(1/L.bmi))) %>%
    select(years, sex, p_2, p_85, p_95) %>%
    rename(age = years)
  
  return(bmi_refdata)
  
  
}



# FUNCTION 2: GENERATE BMI REFERENCE DATA FOR 100 PERCENTILES:

#' Generate BMI Reference Data with Percentiles
#' 
#' Calculates BMI reference data and percentiles for ages 4-20 years using the LMS method.
#' 
#' @param data_B DataFrame containing columns: years, sex, L.bmi, M.bmi, S.bmi. These inputs
#'                         are taken from UK90 Growth Charts package SITAR
#' @param format_table Logical, if TRUE returns wide format, if FALSE returns long format
#' 
#' @return DataFrame with:
#'   - Wide format (format_table=TRUE): age, sex, and percentile columns (p_1 to p_99.6)
#'   - Long format (format_table=FALSE): sex, age, centile, bmi columns
#' 
#' @details
#' Uses LMS method to calculate BMI percentiles:
#' Centile at age t = M * (1 + L*S*qnorm(centile/100))^(1/L)
#'  where,
#'    L, M & S are parameters that summarise the normal distribution generated
#'             from a Box-Cox transformation of measurements at each age (Cole, 2012)
#'             (L = power of the Box-Cox transformation; M = Median; S = coefficient of variation)
#'    qnorm(centile/100) is the z score
#' 
#' Calculates percentiles 1-99 and 99.6th percentile


generate_bmi_refdata_100centiles = function(data_B, format_table = FALSE){
  
  #browser()
  
  bmi_refdata = data_B %>%
    select(years, sex, L.bmi, M.bmi, S.bmi) %>%
    subset(years >= as.double(1) & years <= as.double(20))
  
  for( x in 1:100){
    
    if (x <= 99) {
      
      x = x/100
      
      bmi_refdata = bmi_refdata %>%
        mutate(!!paste0("p","_",(x*100)) := M.bmi*(1 + L.bmi*S.bmi*qnorm(x))^(1/L.bmi))
      # rowwise() %>%
      # mutate(!!paste0("p","_",(x*100)) := M.bmi[y]*(1 + L.bmi[y]*S.bmi[y]*qnorm(x))^(1/L.bmi[y]))
      
      
    } else{
      #browser()
      x = 0.996
      
      bmi_refdata = bmi_refdata %>%
        mutate(!!paste0("p","_",(x*100)) := M.bmi*(1 + L.bmi*S.bmi*qnorm(x))^(1/L.bmi))
      
    }
  }
  
  
  if(format_table == TRUE){
    
    bmi_refdata = bmi_refdata %>%
      rename(age = years)
    
  } else{
    
    bmi_refdata = bmi_refdata %>%
      select(-c(L.bmi, M.bmi, S.bmi)) %>%
      unite(col = "sex_years", "sex", "years", sep = "_") %>%
      pivot_longer(cols= c(starts_with("p")), names_to = "centile", values_to = "bmi") %>%
      separate("sex_years", into = c("sex", "age"), sep = "_") %>%
      mutate(centile = substr(centile, 3, nchar(centile)))
    
  }
  
  return(bmi_refdata)
  
}




# FUNCTION 4: CALCULATE PROPORTIONAL CHANGE IN ENERGY INTAKE:

# Calculate Proportional Energy Intake (EI) Change
#
#  This function calculates the proportional change in energy intake (EI) 
#  based on an individual's age, sex, BMI, and a specified change in intake. 
#' It uses reference BMI and proportional weight change data to determine 
#' the proportional change in energy intake.
#'
#' INPUTS:
#' @param age Numeric. The age of the individual (in years). Values are floored.
#' @param sex Character or numeric. The sex of the individual ("male", "female", 1, or 2). 
#'            "male" and 1 are treated as male, while "female" and 2 are treated as female.
#' @param bmi Numeric. The body mass index (BMI) of the individual. 
#'            If NULL, no calculations are performed.
#' @param intake_change Numeric. The change in energy intake to be applied.
#' @param bmi_ref_data Data frame. Reference data for BMI corresponding to age and sex. It contains the columns
#'                     `age`, `sex`, `bmi`, and `centile`. This reference data is from UK90 Growth charts. 
#'                     We use this to identify the closest BMI percentile of the child based on age, sex and current BMI.
#' @param prop_weight_data Data frame. Proportional weight data containing columns 
#'                         `age`, `sex`, and `weight`. Used to calculate the proportional 
#'                         change in energy intake. This reference data is located here:
#'                         (inputs\ref_data\effect_weighting.csv)
#'
#' @return Numeric. The proportional energy intake change. Returns 0 if the individual's BMI 
#'         percentile is at or below the 10th percentile.
#'
#' @details 
#' - The function determines the closest BMI percentile for the individual using the provided 
#'   BMI reference data.
#' - If the percentile is less than or equal to 10, the proportional intake change is set to 0.
#' - Otherwise, the function calculates the proportional intake change using the 
#'   effect weighting data for the corresponding age & sex and the change in energy intake.


calculate_proportional_ei_change = function(age, sex, bmi, intake_change, bmi_ref_data, prop_weight_data){
  
  #browser()
  
  age = floor(age)
  
  if (sex == "female" | sex == 2){
    sex = 2
  } else { if(sex == "male" | sex == 1){
    
    sex = 1
    
  } }
  
  
  if (!is.null(bmi)){
    
    age_row <- bmi_ref_data[bmi_ref_data$age == age & bmi_ref_data$sex == sex,]
    
    closest_percentile_index <- which.min(abs(age_row$bmi - bmi))
    closest_percentile <- age_row$centile[closest_percentile_index]
    
    
    if(closest_percentile <= 10){
      
      prop_intake_change = 0
      
    } else{
      
      effect_weight = prop_weight_data$weight[prop_weight_data$age == age & prop_weight_data$sex == sex]
      
      prop_intake_change = effect_weight*intake_change
      
      
    }
    
  }
  
  return(prop_intake_change)
  
}




# FUNCTION 3: CALCULATE BMI CATEGORY:

#' Calculate BMI Category
#'
#' This function determines the BMI category of a child or adult based on the 
#' BMI value, age, sex, and reference BMI percentiles (derived using the LMS 
#' method from UK90 growth charts). The categories are classified as 
#' "underweight," "normal," "overweight," or "obese" using cutoffs defined by 
#' the NCMP (NHS England, 2023).
#'
#' @param age Numeric. The age of the individual (in years). If the age is greater 
#'            than 20, adult BMI thresholds are used for classification.
#' @param sex Character or numeric. The sex of the individual ("male", "female", 1, or 2). 
#'            "male" and 1 are treated as male, while "female" and 2 are treated as female.
#' @param bmi Numeric. The body mass index (BMI) of the individual.
#' @param df_B Data frame. A reference table containing BMI percentile cutoffs with columns 
#'             `age`, `sex`, `p_2`, `p_85`, and `p_95`. These represent the 2nd, 85th, 
#'             and 95th BMI percentiles for the corresponding age and sex.
#'
#' @return Character. The BMI category: "underweight," "normal," "overweight," or "obese."
#'
#' @details 
#' - For individuals aged 20 years or younger, BMI is compared against the 
#'   percentiles from the reference table (`df_B`) to assign a category.
#' - The categories for children are:
#'   - `<= 2nd percentile`: "underweight"
#'   - `> 2nd & < 85th percentile`: "normal"
#'   - `>= 85th & < 95th percentile`: "overweight"
#'   - `>= 95th percentile`: "obese"
#' - For individuals over 20 years, adult BMI thresholds are used:
#'   - `< 18.5`: "underweight"
#'   - `18.5 to < 25`: "normal"
#'   - `25 to < 30`: "overweight"
#'   - `>= 30`: "obese"
#'

calculate_bmi_category <- function(age, sex, bmi, df_B) {
  #browser()
  
  if (age <= 20){
    
    if (sex == "female" | sex == 2){
      sex = 2
    } else { if(sex == "male" | sex == 1){
      
      sex = 1
      
    } }
    
    #browser()
    percentile_2 <- df_B$p_2[which(df_B$age == age & df_B$sex == sex)]
    percentile_85 <- df_B$p_85[which(df_B$age == age & df_B$sex == sex)]
    percentile_95 <- df_B$p_95[which(df_B$age == age & df_B$sex == sex)]
    
    category <- case_when(
      # bmi <= percentile_2 ~ "underweight",
      # bmi > percentile_2 & bmi < percentile_85 ~ "normal",
      bmi < percentile_85 ~ "normal",
      bmi >= percentile_85 & bmi < percentile_95 ~ "overweight",
      bmi >= percentile_95 ~ "obese"
    )
    
  } else{
    
    category <- case_when(
      bmi < 18.5 ~ "underweight",
      bmi>= 18.5 & bmi <25 ~ "normal",
      bmi >= 25 & bmi <30 ~ "overweight",
      bmi >= 30 ~ "obese"
      
    )
    
    
  }
  
  
  return(category)
}





# Function to calculate new bmi and weight category for children using Henry (2005) equations.

calculate_bmi_from_eichange_hox = function(df, daily_ei_change, compensation_factor, effect_weighting_df, nation, tags){
  
  #browser()
  # Initialising an empty list to store function outputs
  output_list = list()
  
  # reading in a table which assigns a proportional effect size based on age and sex of a child
  # This is based on the idea that any change in daily energy intake will be proportional to the 
  # age and sex of a child. So, for example, if a policy takes out 20 kcals of an 18 year old males diet,
  # the effect will not be same in case of a 7 year old male. Therefore, a fraction of the effect is applied
  # in case of the 7 year old male.
  # The weight (or proportion) to be multiplied to the effect size has been developed from the SACN 2011 
  # guidelines, by taking the ratio of recommended daily energy intake at each age and recommended daily
  # energy intake at 18. Therefore, the full reduction in daily energy intake applies at 18 years, and 
  # then reduces as the age lowers.
  effect_weighting = effect_weighting_df
  
  library(sitar)
  
  # generating bmi refdata for each centile
  uk90_bmi_refdata_3centiles = generate_bmi_refdata(sitar::uk90)
  
  uk90_bmi_refdata_100centiles = generate_bmi_refdata_100centiles(sitar::uk90)
  
  
  # applying the henry equations
  df <- df %>%
    rowwise() %>%
    mutate(baseline_bmi_category = calculate_bmi_category(age = age, sex = sex, bmi = bmi, df_B = uk90_bmi_refdata_3centiles)) %>% # calculating baseline bmi catgeories
    #mutate(bmi_centile = look_up_percentile(age = age, sex = sex, bmi = bmi, data_B = uk90_bmi_refdata_100centiles)) %>%
    mutate(intake_change = ifelse(baseline_bmi_category %in% c( "overweight", "obese"), # "normal",
                                  calculate_proportional_ei_change(age = age,
                                                                   sex = sex,
                                                                   bmi = bmi, 
                                                                   intake_change = -daily_ei_change * (1-compensation_factor), 
                                                                   prop_weight_data = effect_weighting, 
                                                                   bmi_ref_data = uk90_bmi_refdata_100centiles), 0)) %>% # assigning proportional daily intake change based on age, sex and bmi
    ungroup() %>%
    mutate(intervention_status = ifelse(intake_change == 0, "No", "Yes")) %>% # creating a dummy variable to indicate intervetion status for each child
    mutate(policy_period_energy_intake = baseline_intake + intake_change) %>% # calculating the new energy intake once policy is in effect, by applying the intake change
    mutate(policy_period_rmr = policy_period_energy_intake/pal) %>% # calculating the resting metabolic rate
    mutate(post_policy_weight = case_when(sex == 1 ~ (((policy_period_rmr * 4.184) - 2876)/66.9),
                                          sex == 2 ~ (((policy_period_rmr * 4.184) - 3230)/47.9))) %>% # calculating the new body weight based on the new energy intake 
    mutate(weight_diff = weight - post_policy_weight) %>%
    mutate(post_policy_bmi = post_policy_weight/(height/100)^2) %>% # calculating the new bmi as a result of the new body weight
    rowwise() %>%
    mutate(post_bmi_category = calculate_bmi_category(age = age, sex = sex, bmi = post_policy_bmi, df_B = uk90_bmi_refdata_3centiles)) %>% # calculating the new bmi category
    ungroup() %>%
    mutate(bmi_diff = bmi - post_policy_bmi)
  
  
  # Output 1:  adding the model dataframe to outputs
  output_list[["post_df"]] = df
  
  
  # Creating a table for generating plots
  child_bmi_change = rbind(
    df %>% 
      count(baseline_bmi_category, wt = wt_int) %>% 
      mutate(freq = n/sum(n)*100,
             type = "Baseline") %>% 
      rename(BMI = baseline_bmi_category),
    df %>% 
      count(post_bmi_category, wt = wt_int) %>% 
      mutate(freq = n/sum(n)*100,
             type = "Post-Implementation") %>% 
      rename(BMI = post_bmi_category))
  
  
  
  child_bmi_change = child_bmi_change %>%
    mutate(BMI = factor(BMI, levels = c("underweight", "normal", "overweight", "obese"))) %>%
    as.data.frame()
  
  
  # Creating a readable output table
  child_bmi_change_year = child_bmi_change %>%
    select(-c(n)) %>%
    pivot_wider(., names_from = BMI, values_from = freq)
  
  # Output 3: Table of prevalence of obesity
  output_list[["bmi_prevalence_table"]] = child_bmi_change_year
  
  
  
  # bar plot of BMI categories distributions
  child_bar_plot = child_bmi_change %>%
    ggplot(., aes(y = freq, x = BMI, fill = type)) + 
    geom_bar(stat = "identity", position = "dodge") +
    theme_ipsum() +
    labs(fill = "", 
         title = paste("BMI Distribution - ", tags), 
         y = "Prevalence - %",
         subtitle = paste("Children | Nation:", nation)) +
    theme_ipsum(base_size = 8, axis_title_size = 8) + #, base_family="Averta"
    theme(legend.position = "top")
  
  # Output 3: Bar plot of BMI category distributions
  output_list[["bmi_prevalence_plot"]] = child_bar_plot
  
  
  
  
  return(output_list)
  
}



#' Calculate percentage reduction in a prevalence category
#'
#' @param df Dataframe with 'type' column containing "Baseline" and "Post-Implementation"
#' @param category Column name to calculate reduction for (default "obese")
#' @return Percentage reduction
#'
calc_percent_reduction <- function(df, category = "obese") {
  
  baseline <- df %>% filter(type == "Baseline") %>% pull({{ category }})
  post <- df %>% filter(type == "Post-Implementation") %>% pull({{ category }})
  
  reduction <- round(((baseline - post) / baseline) * 100, 1)
  
  print(paste("Percentage reduction:", round(reduction,1), "%"))
  
  return(reduction)
}


run_policy_scenarios = function(df, scenarios, effect_weight_df, nation = "England") {
  
  pmap_dfr(scenarios, function(daily_ei_change, compensation_factor, tags) {
    result = calculate_bmi_from_eichange_hox(
      df = df,
      daily_ei_change = daily_ei_change,
      compensation_factor = compensation_factor,
      effect_weighting_df = effect_weight_df,
      nation = nation,
      tags = tags
    )
    
    baseline_obese = result$bmi_prevalence_table %>% 
      filter(type == "Baseline") %>% 
      pull(obese)
    
    post_obese = result$bmi_prevalence_table %>% 
      filter(type == "Post-Implementation") %>% 
      pull(obese)
    
    tibble(
      scenario = tags,
      baseline_obesity = round(baseline_obese,1),
      post_obesity = round(post_obese, 1),
      pct_change_obesity = round((post_obese - baseline_obese) / baseline_obese * 100, 1)
    )
  })
}



#' Create weighted density plot for a variable split by nation
#'
#' @param df Dataframe with 'nation' and 'wt_int' columns
#' @param var Variable to plot (unquoted)
#' @param title Optional title (defaults to variable name)
#' @param fill_colours Named vector of colours for each country
#' @return ggplot object
#'
plot_density_by_nation <- function(df, 
                                   var, 
                                   title = NULL,
                                   fill_colours = c("England" = "#4575b4", "Scotland" = "#d73027")) {
  
  var_name <- deparse(substitute(var))
  
  df %>%
    filter(!is.na({{ var }}), !is.na(wt_int)) %>%
    ggplot(aes(x = {{ var }}, fill = nation, weight = wt_int)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = fill_colours) +
    labs(
      title = title %||% paste("Distribution of", var_name, "(weighted)"),
      x = var_name,
      y = "Density",
      fill = "Country"
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      plot.title = element_text(face = "bold")
    )
}
