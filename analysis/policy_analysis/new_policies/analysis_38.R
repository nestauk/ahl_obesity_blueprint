


# functions:


determine_total_weight_loss_treatment_status <- function(df,
                                                         weight_loss_cols,
                                                         yearly_treatment_status_cols,
                                                         total_col_name = "total_weight_loss", 
                                                         status_col_name = "overall_treatment_status",
                                                         year_allocation = "year_allocation") {
  df_output <- df %>%
    mutate(
      # Sum the weight loss columns
      !!total_col_name := rowSums(across({{ weight_loss_cols }}), na.rm = TRUE),
      
      # Determine the status from categorical intervention year columns
      !!status_col_name := case_when(
        if_any({{ yearly_treatment_status_cols }}, ~ .x == "Yes") ~ "Yes",
        TRUE ~ "No"),
      !!year_allocation := case_when(intervention_year1 == "Yes" ~ "year_1",
                                     intervention_year2 == "Yes" ~ "year_2",
                                     intervention_year3 == "Yes" ~ "year_3",
                                     intervention_year4 == "Yes" ~ "year_4",
                                     intervention_year5 == "Yes" ~ "year_5",
                                     TRUE ~ "not_selected")
    )
  return(df_output)
}



calculate_weights_thresholds <- function(df,
                                         height_col,
                                         target_bmi = 40,
                                         pct_margin = 18.5) {
  df_output <- df %>%
    mutate(
      weight_for_target_bmi = target_bmi * ({{ height_col }} / 100)^2,
      weight_with_margin = weight_for_target_bmi / (1 - (pct_margin / 100))
    )
  return(df_output)
}


filter_and_pivot_bmi_data <- function(df, bmi_col, bmi_threshold, pivot_cols) {
  df_output <- df %>%
    filter({{ bmi_col }} >= bmi_threshold) %>%
    pivot_longer(
      cols = {{ pivot_cols }},
      names_to = "weight_type",
      values_to = "weight_value"
    )
  return(df_output)
}


create_weight_distribution_plot <- function(df, x_col, y_col, shape_col, color_col, point_type_to_color, tags) {
  
  plot <- ggplot(df, aes(x = {{ x_col }}, y = {{ y_col }}, shape = {{ shape_col }})) +
    
    # Layer 1: Plot ONLY the specified point_type_to_color and color it
    geom_point(
      # A neat trick to filter data for a specific layer inside the plot code
      data = . %>% filter({{ shape_col }} == point_type_to_color),
      aes(color = {{ color_col }}), # Color mapping is ONLY in this layer's aes()
      alpha = 0.5,
      size = 2
    ) +
    
    # Layer 2: Plot EVERYTHING ELSE with a static grey color
    geom_point(
      data = . %>% filter({{ shape_col }} != point_type_to_color),
      color = "grey50", # Note: color is OUTSIDE aes() for a static value
      alpha = 0.6,
      size = 0.9
    ) +
    
    # Add some nice labels and a theme
    labs(
      title = paste("Weight Distribution", tags),
      subtitle = paste("Model year:", rlang::as_label(rlang::enquo(color_col))),
      # subtitle = paste0(color_col),
      x = "Height in cm",
      y = "Weight in kg",
      color = "Treatment status",
      shape = "Weight type"
    ) +
    theme_minimal()
  
  return(plot)
}

prep_data_for_plots <- function(df,
                                weight_loss_cols,
                                treatment_status_cols,
                                total_weight_loss_col_name,
                                overall_treatment_status_col_name,
                                bmi_threshold,
                                bmi_col,
                                height_col,
                                pivot_cols,
                                weight_loss_pct){
  
  df %>%
    # Step 1: Calculate total weight loss and overall treatment status
    determine_total_weight_loss_treatment_status(
      weight_loss_cols = {{ weight_loss_cols }},
      yearly_treatment_status_cols = {{ treatment_status_cols }},
      total_col_name = "total_weight_loss",
      status_col_name = "overall_treatment_status"
    ) %>%
    # Step 2: Calculate target weight thresholds
    calculate_weights_thresholds(
      height_col = {{ height_col }},
      target_bmi = bmi_threshold,
      pct_margin = weight_loss_pct
    ) %>%
    # Step 3: Filter by BMI and pivot to long format
    filter_and_pivot_bmi_data(
      bmi_threshold = bmi_threshold,
      bmi_col = {{ bmi_col }},
      pivot_cols = {{ pivot_cols }}
    )
}


create_density_plot <- function(data, 
                                bmi_col = "bmi", 
                                weight_col = "wt_int", 
                                option_col = "option", 
                                year_col = "year_allocation", 
                                year_value = NULL) {
  
  plot_data <- data
  plot_title <- paste("Density Plot of", bmi_col)
  plot_sub_title <- paste("Modelling year = ", "years 1 to 5")
  
  # If a year_value is provided, filter the data and update the title
  if (!is.null(year_value)) {
    # Check if the filter column exists
    if(!year_col %in% names(data)) {
      stop(paste("Error: Column '", year_col, "' not found in the data frame.", sep=""))
    }
    

    plot_data <- data %>% 
      filter(.data[[year_col]] == year_value)
    
    plot_sub_title <- paste("Modelling year = ", year_value)
  }
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = .data[[bmi_col]], 
                             weight = .data[[weight_col]], 
                             color = .data[[option_col]])) +
    geom_density(alpha = 0.5, trim = TRUE) +
    labs(
      title = plot_title,
      subtitle = plot_sub_title,
      x = bmi_col,
      y = "weighted density",
      color = tools::toTitleCase(option_col)
    ) +
    theme_minimal()
  
  return(p)
}




# required datasets:


df_output_option_1 = read_csv("/Users/anish.chacko/Documents/git/ahl_obesity_blueprint/outputs/new_policies/policy_38/policy_38_adult_england_bmi.csv")

df_output_option_2 = read_csv("/Users/anish.chacko/Documents/git/ahl_obesity_blueprint/outputs/new_policies/policy_38_op2/policy_38_adult_england_bmi_593.csv")

# plot_metric(data = df_output_option_1, metric = "wt_int")

df_op_1_for_plots = prep_data_for_plots(df = df_output_option_1,
                                        weight_loss_cols = c(weight_loss_y1,
                                                             weight_loss_y2,
                                                             weight_loss_y3,
                                                             weight_loss_y4,
                                                             weight_loss_y5),
                                        treatment_status_cols = c(intervention_year1,
                                                                  intervention_year2,
                                                                  intervention_year3,
                                                                  intervention_year4,
                                                                  intervention_year5),
                                        total_weight_loss_col_name = "total_weight_loss",
                                        overall_treatment_status_col_name = "overall_treatment_status",
                                        bmi_threshold = 40,
                                        weight_loss_pct = 18.5,
                                        bmi_col = bmi,
                                        height_col = height,
                                        pivot_cols = c(weight,
                                                       weight_for_target_bmi,
                                                       weight_with_margin))

df_op_2_for_plots = prep_data_for_plots(df = df_output_option_2,
                                        weight_loss_cols = c(weight_loss_y1,
                                                             weight_loss_y2,
                                                             weight_loss_y3,
                                                             weight_loss_y4,
                                                             weight_loss_y5),
                                        treatment_status_cols = c(intervention_year1,
                                                                  intervention_year2,
                                                                  intervention_year3,
                                                                  intervention_year4,
                                                                  intervention_year5),
                                        total_weight_loss_col_name = "total_weight_loss",
                                        overall_treatment_status_col_name = "overall_treatment_status",
                                        bmi_threshold = 40,
                                        weight_loss_pct = 18.5,
                                        bmi_col = bmi,
                                        height_col = height,
                                        pivot_cols = c(weight,
                                                       weight_for_target_bmi,
                                                       weight_with_margin))



create_weight_distribution_plot(df= df_op_1_for_plots,
                                x_col = height,
                                y_col = weight_value,
                                shape_col = weight_type,
                                color_col = overall_treatment_status,
                                point_type_to_color = "weight",
                                tags = "Option 1")


create_weight_distribution_plot(df= df_op_1_for_plots,
                                x_col = height,
                                y_col = weight_value,
                                shape_col = weight_type,
                                color_col = intervention_year1,
                                point_type_to_color = "weight",
                                tags = "Option 1")

create_weight_distribution_plot(df= df_op_1_for_plots,
                                x_col = height,
                                y_col = weight_value,
                                shape_col = weight_type,
                                color_col = intervention_year2,
                                point_type_to_color = "weight",
                                tags = "Option 1")

create_weight_distribution_plot(df= df_op_1_for_plots,
                                x_col = height,
                                y_col = weight_value,
                                shape_col = weight_type,
                                color_col = intervention_year3,
                                point_type_to_color = "weight",
                                tags = "Option 1")

create_weight_distribution_plot(df= df_op_1_for_plots,
                                x_col = height,
                                y_col = weight_value,
                                shape_col = weight_type,
                                color_col = intervention_year4,
                                point_type_to_color = "weight",
                                tags = "Option 1")

create_weight_distribution_plot(df= df_op_1_for_plots,
                                x_col = height,
                                y_col = weight_value,
                                shape_col = weight_type,
                                color_col = intervention_year5,
                                point_type_to_color = "weight",
                                tags = "Option 1")



create_weight_distribution_plot(df= df_op_2_for_plots,
                                x_col = height,
                                y_col = weight_value,
                                shape_col = weight_type,
                                color_col = overall_treatment_status,
                                point_type_to_color = "weight",
                                tags = "Option 2")


create_weight_distribution_plot(df= df_op_2_for_plots,
                                x_col = height,
                                y_col = weight_value,
                                shape_col = weight_type,
                                color_col = intervention_year1,
                                point_type_to_color = "weight",
                                tags = "Option 2")

create_weight_distribution_plot(df= df_op_2_for_plots,
                                x_col = height,
                                y_col = weight_value,
                                shape_col = weight_type,
                                color_col = intervention_year2,
                                point_type_to_color = "weight",
                                tags = "Option 2")

create_weight_distribution_plot(df= df_op_2_for_plots,
                                x_col = height,
                                y_col = weight_value,
                                shape_col = weight_type,
                                color_col = intervention_year3,
                                point_type_to_color = "weight",
                                tags = "Option 2")

create_weight_distribution_plot(df= df_op_2_for_plots,
                                x_col = height,
                                y_col = weight_value,
                                shape_col = weight_type,
                                color_col = intervention_year4,
                                point_type_to_color = "weight",
                                tags = "Option 2")

create_weight_distribution_plot(df= df_op_2_for_plots,
                                x_col = height,
                                y_col = weight_value,
                                shape_col = weight_type,
                                color_col = intervention_year5,
                                point_type_to_color = "weight",
                                tags = "Option 2")



df_op1_updated = determine_total_weight_loss_treatment_status(df=df_output_option_1,
                                                              weight_loss_cols =  c(weight_loss_y1, weight_loss_y2, weight_loss_y3,weight_loss_y4,weight_loss_y5),
                                                              yearly_treatment_status_cols = c(intervention_year1, intervention_year2,intervention_year3,intervention_year4,intervention_year5))



df_op1_filtered_sample = df_op1_updated %>%
  filter(overall_treatment_status == "Yes") %>%
  mutate(option = "option_1")





df_op2_updated = determine_total_weight_loss_treatment_status(df=df_output_option_2,
                                                              weight_loss_cols =  c(weight_loss_y1, weight_loss_y2, weight_loss_y3,weight_loss_y4,weight_loss_y5),
                                                              yearly_treatment_status_cols = c(intervention_year1, intervention_year2,intervention_year3,intervention_year4,intervention_year5))



df_op2_filtered_sample = df_op2_updated %>%
  filter(overall_treatment_status == "Yes") %>%
  mutate(option = "option_2")


combined_df = rbind(df_op1_filtered_sample, df_op2_filtered_sample)



create_density_plot(data = combined_df)
create_density_plot(data = combined_df, year_value = "year_1")
create_density_plot(data = combined_df, year_value = "year_2")
create_density_plot(data = combined_df, year_value = "year_3")
create_density_plot(data = combined_df, year_value = "year_4")
create_density_plot(data = combined_df, year_value = "year_5")




ggplot(combined_df, aes(x = bmi, weight = wt_int, color = option)) +
  geom_density(alpha = 0.5, trim = TRUE) +
  labs(
    title = "Density Plot of BMI options 1 & 2",
    x = "bmi",
    y = "density",
    color = "option" # Legend title for color
  ) +
  theme_minimal()


ggplot(combined_df %>% filter(year_allocation == "option_1_y1"), aes(x = bmi, weight = wt_int, color = option)) +
  geom_density(alpha = 0.5, trim = TRUE) +
  labs(
    title = "Density Plot of BMI options 1 & 2",
    x = "bmi",
    y = "density",
    color = "option" # Legend title for color
  ) +
  theme_minimal()


ggplot(combined_df %>% filter(intervention_year2 == "Yes"), aes(x = bmi, weight = wt_int, color = option)) +
  geom_density(alpha = 0.5, trim = TRUE) +
  labs(
    title = "Density Plot of BMI options 1 & 2",
    x = "bmi",
    y = "density",
    color = "option" # Legend title for color
  ) +
  theme_minimal()


ggplot(combined_df %>% filter(intervention_year3 == "Yes"), aes(x = bmi, weight = wt_int, color = option)) +
  geom_density(alpha = 0.5, trim = TRUE) +
  labs(
    title = "Density Plot of BMI options 1 & 2",
    x = "bmi",
    y = "density",
    color = "option" # Legend title for color
  ) +
  theme_minimal()


ggplot(combined_df %>% filter(intervention_year4 == "Yes"), aes(x = bmi, weight = wt_int, color = option)) +
  geom_density(alpha = 0.5, trim = TRUE) +
  labs(
    title = "Density Plot of BMI options 1 & 2",
    x = "bmi",
    y = "density",
    color = "option" # Legend title for color
  ) +
  theme_minimal()

ggplot(combined_df %>% filter(intervention_year5 == "Yes"), aes(x = bmi, weight = wt_int, color = option)) +
  geom_density(alpha = 0.5, trim = TRUE) +
  labs(
    title = "Density Plot of BMI options 1 & 2",
    x = "bmi",
    y = "density",
    color = "option" # Legend title for color
  ) +
  theme_minimal()


ggplot(df_op1_filtered_sample, aes(x=bmi)) + 
  geom_density(alpha = 0.4, trim = TRUE)


ggplot(df_op2_filtered_sample, aes(x=bmi)) + 
  geom_density(alpha = 0.4, trim = TRUE)





df_output_option_1 = df_output_option_1 %>%
  mutate(weight_loss = weight_loss_y1 + weight_loss_y2 + weight_loss_y3 + weight_loss_y4 + weight_loss_y5) %>%
  mutate(five_year_treatment_status = case_when(weight_loss == 0 ~ "No",
                                                weight_loss != 0 ~ "Yes"))

df_output_option_2 = df_output_option_2 %>%
  mutate(weight_loss = weight_loss_y1 + weight_loss_y2 + weight_loss_y3 + weight_loss_y4 + weight_loss_y5) %>%
  mutate(five_year_treatment_status = case_when(weight_loss == 0 ~ "No",
                                                weight_loss != 0 ~ "Yes"))


df_output_option_1_subset = df_output_option_1 %>%
  filter(weight_loss != 0)

df_output_option_2_subset = df_output_option_2 %>%
  filter(weight_loss != 0)


sum(df_output_option_2_subset$wt_int)

mean(df_output_option_1_subset$weight_loss)

mean(df_output_option_2_subset$weight_loss)


option_1_crossing = process_and_filter_cohort_crossing(df = df_output_option_1)

option_2_crossing = process_and_filter_cohort_crossing(df = df_output_option_2)


option_1_not_crossing = process_and_filter_cohort_not_crossing(df = df_output_option_1)

option_2_not_crossing = process_and_filter_cohort_not_crossing(df = df_output_option_2)


((sum(option_1_crossing$wt_int) + sum(option_1_not_crossing$wt_int)) / sum(df_output_option_1$wt_int))*44263393

((sum(option_2_crossing$wt_int) + sum(option_2_not_crossing$wt_int)) / sum(df_output_option_2$wt_int))*44263393

mean(option_1_crossing$bmi)
mean(option_1_crossing$height)
mean(option_1_crossing$weight)

mean(option_1_not_crossing$bmi)
mean(option_1_not_crossing$height)
mean(option_1_not_crossing$weight)

mean(option_2_crossing$bmi)
mean(option_2_crossing$height)
mean(option_2_crossing$weight)

mean(option_2_not_crossing$bmi)
mean(option_2_not_crossing$height)
mean(option_2_not_crossing$weight)


sum(subset_option_1$wt_int)/sum(df_output_option_1$wt_int)

sum(subset_option_2$wt_int)/sum(df_output_option_2$wt_int)

cohort_year_allocation = post_df_adult %>%
  mutate(year_allocation = case_when(intervention_year1 == "Yes" ~ "Y1",
                                     intervention_year2 == "Yes" ~ "Y2",
                                     intervention_year3 == "Yes" ~ "Y3",
                                     intervention_year4 == "Yes" ~ "Y4",
                                     intervention_year5 == "Yes" ~ "Y5",
                                     TRUE ~ "NA")) %>%
  rowwise() %>%
  mutate(bmi_class_change = n_distinct(c_across(matches("(?i)bmi_\\d+_class")), na.rm = TRUE) > 1) %>%
  ungroup()

cohort_allocation_subset = cohort_year_allocation %>%
  filter(year_allocation != "NA", !bmi_class_change)







process_and_filter_cohort_not_crossing <- function(df) {
  
  # Ensure the input is a dataframe
  if (!is.data.frame(df)) {
    stop("Error: The input 'df' must be a dataframe.")
  }
  
  # The main data processing and filtering pipeline
  result <- df %>%
    # Step 1: Create the 'year_allocation' column
    mutate(year_allocation = case_when(
      # The .data pronoun is used to be explicit that columns are from the input df
      .data$intervention_year1 == "Yes" ~ "Y1",
      .data$intervention_year2 == "Yes" ~ "Y2",
      .data$intervention_year3 == "Yes" ~ "Y3",
      .data$intervention_year4 == "Yes" ~ "Y4",
      .data$intervention_year5 == "Yes" ~ "Y5",
      TRUE ~ "NA"  # Default case if no intervention year is "Yes"
    )) %>%
    # Step 2: Add a flag for BMI class changes (row by row)
    rowwise() %>%
    mutate(
      bmi_class_change = n_distinct(c_across(matches("(?i)bmi_\\d+_class")), na.rm = TRUE) > 1
    ) %>%
    ungroup() %>%
    # Step 3: Filter the data based on the required conditions
    filter(
      year_allocation != "NA", # Keep rows where allocation is NOT "NA"
      !bmi_class_change        # Keep rows where bmi_class_change is FALSE
    ) # %>%
    # filter(bmi >= 40)
  
  # Return the final, filtered dataframe
  return(result)
}

process_and_filter_cohort_crossing <- function(df) {
  
  # Ensure the input is a dataframe
  if (!is.data.frame(df)) {
    stop("Error: The input 'df' must be a dataframe.")
  }
  
  # The main data processing and filtering pipeline
  result <- df %>%
    # Step 1: Create the 'year_allocation' column
    mutate(year_allocation = case_when(
      # The .data pronoun is used to be explicit that columns are from the input df
      .data$intervention_year1 == "Yes" ~ "Y1",
      .data$intervention_year2 == "Yes" ~ "Y2",
      .data$intervention_year3 == "Yes" ~ "Y3",
      .data$intervention_year4 == "Yes" ~ "Y4",
      .data$intervention_year5 == "Yes" ~ "Y5",
      TRUE ~ "NA"  # Default case if no intervention year is "Yes"
    )) %>%
    # Step 2: Add a flag for BMI class changes (row by row)
    rowwise() %>%
    mutate(
      bmi_class_change = n_distinct(c_across(matches("(?i)bmi_\\d+_class")), na.rm = TRUE) > 1
    ) %>%
    ungroup() %>%
    # Step 3: Filter the data based on the required conditions
    filter(
      year_allocation != "NA", # Keep rows where allocation is NOT "NA"
      bmi_class_change        # Keep rows where bmi_class_change is FALSE
    ) # %>%
    # filter(bmi >= 40)
  
  # Return the final, filtered dataframe
  return(result)
}



df_output_option_1_over_40 = df_output_option_1 %>%
  filter(bmi >= 40)


df_output_option_2_over_40 = df_output_option_2 %>%
  filter(bmi >= 40)


ggplot(df_output_option_2_over_40, aes(x=eligibility_score, y=bmi)) + 
  geom_jitter(
  aes(size = wt_int, colour = as.factor(cond_diabetes), alpha = 0.7), # Map size to weight, add transparency
  # color = "steelblue",
  width = 0.1 # Controls the amount of horizontal jitter
  ) +
  scale_color_manual(values = c("1" = "firebrick", "0" = "steelblue"))






plot_metric(data = df_output_option_2, metric = "wt_int")

mean(df_output_option_2$wt_int)
median(df_output_option_2$wt_int)
min(df_output_option_2$wt_int)
max(df_output_option_2$wt_int)


install.packages("splitstackshape")
library(splitstackshape)

df_output_option_2_upd = df_output_option_2 %>%
  mutate(test_freq = (round(((wt_int/ sum(wt_int)) * 44263393),0)))


sum(df_output_option_2_upd$test_freq)

test_df = expandRows(df_output_option_2_upd, "test_freq")


df_output_option_2

df_output_option_2_over_40 = df_output_option_2_over_40 %>%
  mutate(treatment_status = case_when(weight_loss == 0 ~ "No",
                          weight_loss != 0 ~ "Yes"))


ggplot(df_output_option_2_over_40, aes(x=wt_int, y=bmi)) + 
  geom_point(
    aes(colour = as.factor(treatment_status), alpha = 0.7), # Map size to weight, add transparency
  ) +
 scale_color_manual(values = c("Yes" = "firebrick", "No" = "steelblue"))




df_output_option_1_over_40 = df_output_option_1_over_40 %>%
  mutate(treatment_status = case_when(weight_loss == 0 ~ "No",
                                      weight_loss != 0 ~ "Yes"))


ggplot(df_output_option_1_over_40, aes(x=wt_int, y=bmi)) + 
  geom_point(
    aes(colour = as.factor(treatment_status), alpha = 0.7), # Map size to weight, add transparency
  ) +
  scale_color_manual(values = c("Yes" = "firebrick", "No" = "steelblue"))




ggplot(df_output_option_1, aes(x=wt_int, y=bmi)) + 
  geom_point( aes(colour = as.factor(treatment_status), alpha = 0.7)) +
 scale_color_manual(values = c("Yes" = "firebrick", "No" = "steelblue"))


ggplot(df_output_option_2, aes(x=wt_int, y=bmi)) + 
  geom_point( aes(colour = as.factor(treatment_status), alpha = 0.7)) +
  scale_color_manual(values = c("Yes" = "firebrick", "No" = "steelblue"))



df_output_option_1 = df_output_option_1 %>%
  mutate(weight_for_bmi_40 = (40 * (height/100)^2)) %>%
  mutate(weight_within_185 = (40 * (height/100)^2 / 0.815))


df_output_option_1_40 = df_output_option_1 %>%
  filter(bmi >= 40)


df_long <- pivot_longer(df_output_option_1_40, 
                        cols = c(weight, weight_for_bmi_40, weight_within_185), 
                        names_to = "weight_type", 
                        values_to = "weight_value")

# Create the scatter plot
ggplot(df_long, aes(x = height, y = weight_value, color = treatment_status, shape = weight_type)) +
  geom_point(alpha = 0.7) + # `alpha` makes points slightly transparent
  labs(
    title = "Scatter Plot of Height vs. Weight",
    x = "Height in cm",
    y = "Weight in kg",
    color = "Weight Measurement" # This renames the legend title
  ) +
  theme_minimal() # A clean, simple theme for the plot


# Base plot setup - aesthetics for shape, x, and y are shared by all layers
ggplot(df_long, aes(x = height, y = weight_value, shape = weight_type)) +
  
  # Layer 1: Plot ONLY 'weight_1' and color it by treatment_status
  geom_point(
    data = subset(df_long, weight_type == "weight"), 
    aes(color = treatment_status), # Color mapping is ONLY in this layer's aes()
    alpha = 0.8, 
    size = 2.5
  ) +
  
  # Layer 2: Plot EVERYTHING ELSE and set a static color (e.g., grey)
  geom_point(
    data = subset(df_long, weight_type != "weight"),
    color = "grey50", # Note: color is OUTSIDE aes() for a static value
    alpha = 0.6,
    size = 2.5
  )



df_output_option_2 = df_output_option_2 %>%
  mutate(weight_for_bmi_40 = (40 * (height/100)^2)) %>%
  mutate(weight_within_185 = (40 * (height/100)^2 * 0.815))


df_output_option_2_40 = df_output_option_2 %>%
  filter(bmi >= 40)


df_long_2 <- pivot_longer(df_output_option_2_40, 
                        cols = c(weight, weight_for_bmi_40, weight_within_185), 
                        names_to = "weight_type", 
                        values_to = "weight_value")


# Base plot setup - aesthetics for shape, x, and y are shared by all layers
ggplot(df_long_2, aes(x = height, y = weight_value, shape = weight_type)) +
  
  # Layer 1: Plot ONLY 'weight_1' and color it by treatment_status
  geom_point(
    data = subset(df_long_2, weight_type == "weight"), 
    aes(color = intervention_year4), # Color mapping is ONLY in this layer's aes()
    alpha = 0.8, 
    size = 2.5
  ) +
  
  # Layer 2: Plot EVERYTHING ELSE and set a static color (e.g., grey)
  geom_point(
    data = subset(df_long_2, weight_type != "weight"),
    color = "grey50", # Note: color is OUTSIDE aes() for a static value
    alpha = 0.6,
    size = 2.5
  )



