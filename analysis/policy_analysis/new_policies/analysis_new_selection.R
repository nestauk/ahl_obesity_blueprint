

source(file = "requirements.R")

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



create_density_plot_new <- function(data, 
                                    baseline_data,
                                bmi_col = "bmi", 
                                weight_col = "pop_estimate", 
                                option_col = "option",
                                year_col = "year_allocation",
                                year_val = 1) {
  
  # browser()
  plot_data <- data
  plot_title <- paste("Density Plot of", bmi_col)
  intervention_col = paste0("intervention_year",year_val)
  actual_selection_cols = paste0("actual_selected_year",year_val)
  
  
  if(year_val == 1){
    data_to_add = baseline_data %>%
      filter(bmi >= 40)
  } else {
    
    if(year_val == 5){
      data_to_add = baseline_data %>%
        filter(bmi >= 30)
      
    } else {
      data_to_add = baseline_data %>%
        filter(bmi >= 35)
    }
  }
  
  # If a year_value is provided, filter the data and update the title
  if (!is.null(year_val)) {
    # Check if the filter column exists
    if(!year_col %in% names(data)) {
      stop(paste("Error: Column '", year_col, "' not found in the data frame.", sep=""))
    }
    
    plot_data <- data %>% 
      filter(.data[[intervention_col]] == "Yes")
    
    
    n_selected_op_1 = plot_data %>%
      filter(option == "option_1") %>%
      select(all_of(actual_selection_cols)) %>%
      sum()

    n_selected_op_2 = plot_data %>%
      filter(option == "option_2") %>%
      select(all_of(actual_selection_cols)) %>%
      sum()
        
    plot_sub_title <- paste("Modelling year = ", year_val, "; Option 1 selection = ", n_selected_op_1, "; Option 2 selection = ", n_selected_op_2)
  }
  
  plot_data = rbind(plot_data, data_to_add)
  
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


# Main analysis:

# reading in required datasets:

df_output_option_1 = read_csv("outputs/new_policies/new_selection/option_1_detailed.csv")
df_output_option_2 = read_csv("outputs/new_policies/new_selection/option_2_detailed.csv")


# data preparation:
df_op1_updated = determine_total_weight_loss_treatment_status(df=df_output_option_1,
                                                              weight_loss_cols =  c(weight_loss),
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

df_baseline = df_op1_updated %>%
  mutate(option = "baseline")

combined_df = rbind(df_op1_filtered_sample, df_op2_filtered_sample)


# plots:
create_density_plot_new(data = combined_df, baseline_data = df_baseline, year_val = 1)
create_density_plot_new(data = combined_df, baseline_data = df_baseline, year_val = 2)
create_density_plot_new(data = combined_df, baseline_data = df_baseline, year_val = 3)
create_density_plot_new(data = combined_df, baseline_data = df_baseline, year_val = 4)
create_density_plot_new(data = combined_df, baseline_data = df_baseline, year_val = 5)





