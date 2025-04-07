
rm(list = ls())
gc()

library(tidyverse)
library(readxl)
library(writexl)
# Set the directory path containing the CSV files
folder_path <- "C:/Users/Anish.Chacko/Downloads/ci_avoided_update"

# List all CSV files in the folder
file_list <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)

# Read and merge all CSV files into a single dataframe
merged_df <- suppressMessages(file_list %>%
  map_df(read_csv))

options(scipen = 999)

disease_list = read_excel(path = paste0(folder_path,"/disease_list.xlsx")) #  "C:/Users/Anish.Chacko/Downloads/ci_csv/disease_list.xlsx")

disease_list = disease_list %>%
  mutate(disease_id = row_number())




avoided_df <- merged_df %>%
  # dplyr::select(-c(ci)) %>%
  mutate(avoided = as.numeric(avoided)) %>%
  mutate(disease_state = str_replace_all(tolower(disease_state), " ", "_"),
         scenario = str_replace_all(scenario, c("Policy" = "", " " = ""))) %>%
  left_join(disease_list, by = "disease_state") %>%
  pivot_wider(names_from = year, values_from = c(avoided, ci)) %>%
  rowwise() %>%
  # mutate(`total_2019_2023` = sum(c_across(5:9), na.rm = TRUE),
  #        `total_2019_2024` = sum(c_across(5:10), na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(scenario) %>%
  arrange(disease_id, .by_group = TRUE) %>%
  ungroup() %>%
  select(-c(disease_state, disease_id))


create_new_avoided <- function(data, years) {
  
  # browser()
  
  # new_avoided_cols_list = list()
  
  for (year in years) {
    
    avoided_col <- paste0("avoided_", year)
    ci_col <- paste0("ci_", year)
    new_avoided_col <- paste0("updated_avoided_", year)
    
    # new_avoided_cols_list[[year-2018]] = new_avoided_col
    
    data <- data %>%
      mutate(!!new_avoided_col := ifelse((!!sym(avoided_col) - !!sym(ci_col)) < 0 & (!!sym(avoided_col) + !!sym(ci_col)) > 0, paste0("NS [", round(!!sym(avoided_col),0), "]*"), round(!!sym(avoided_col),0)))
    
  }
  
  # new_avoided_cols_list = as.character(new_avoided_cols_list)
  
  # data <- data %>%
  #   rowwise() %>%
  #   mutate(avoided_2019_2024 = sum(c_across(all_of(new_avoided_cols_list)), na.rm = TRUE))
  
  return(data)
}


# List of years to process
years <- 2019:2024

# Apply the function to each year

df_result <- create_new_avoided(data = avoided_df, years = years)



write_xlsx(path = paste0(folder_path, "/disease_data_latest_1.xlsx"), x = df_result)












# 
# 
# 
# 
# 
# ci_df <- merged_df %>%
#   dplyr::select(-c(avoided)) %>%
#   mutate(ci = as.numeric(ci)) %>%
#   mutate(disease_state = str_replace_all(tolower(disease_state), " ", "_"),
#          scenario = str_replace_all(scenario, c("Policy" = "", " " = ""))) %>%
#   left_join(disease_list, by = "disease_state") %>%
#   pivot_wider(names_from = year, values_from = ci, names_prefix = "ci_") %>%
#   group_by(scenario) %>%
#   arrange(disease_id, .by_group = TRUE) %>%
#   ungroup() %>%
#   select(-c(disease_state, disease_id))
# 
# 
# final_df <- left_join(avoided_df, ci_df, by = c("scenario", "disease"))





