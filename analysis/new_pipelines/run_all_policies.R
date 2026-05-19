#' ============================================================================
#' run_all_policies.R
#' ============================================================================
#'
#' Loops over every YAML config in policy_context/ and runs the obesity impact model
#' against each dataset listed in `datasets` below. Each (policy x nation)
#' run is independent. It produces a single summary table with one row per
#' (policy, nation) combination.
#'
#' USAGE:
#'   source("run_all_policies.R")
#'
#' OUTPUTS:
#'   - all_results : nested list, all_results[[policy_id]][[nation]] -> full
#'                   model output
#'   - summary_table : tibble written to outputs/summary_table.csv with cols:
#'       policy_id, nation, policy_name, kcal_change, obesity_impact, benefit
#' ============================================================================


source("analysis/new_pipelines/load_files.R")
source("analysis/new_pipelines/run_model.R")

library(purrr)
library(dplyr)
library(tibble)
library(readr)


# Datasets:
datasets <- list(
  England  = bp_hse_2019_adult,
  Scotland = bp_shes_2019_adult
)



config_files <- list.files("analysis/new_pipelines/policy_context",
                           pattern = "\\.ya?ml$",
                           full.names = TRUE)

if (length(config_files) == 0) {
  stop("No YAML files found in configs/")
}

message("Found ", length(config_files), " config file(s).")
message("Running against ", length(datasets), " dataset(s): ",
        paste(names(datasets), collapse = ", "))


# Wrap run_model_for_one_policy so errors in one config don't kill the batch.
safe_run <- safely(run_model_for_one_policy)

raw_results <- map(config_files, function(path) {
  safe_run(path, survey_data = datasets)
})
names(raw_results) <- tools::file_path_sans_ext(basename(config_files))

# Report errors:
errors <- map(raw_results, "error")
errors <- errors[!sapply(errors, is.null)]
if (length(errors) > 0) {
  message("\n", length(errors), " policy run(s) failed:")
  for (nm in names(errors)) {
    message("  - ", nm, ": ", errors[[nm]]$message)
  }
}

# all_results[[policy_id]][[nation]] -> full model output
all_results <- map(raw_results, "result")
all_results <- all_results[!sapply(all_results, is.null)]


# ---- Build the summary table ----
#
# One row per (policy, nation). Columns:
#   policy_id, nation, policy_name, kcal_change, obesity_impact, benefit

build_summary_row <- function(nation_result, policy_id, nation) {
  params  <- nation_result$table_outputs$model_params
  benefit <- nation_result$table_outputs$annual_benefit_to_gov
  
  get_param <- function(name) {
    val <- params$Value[params$Parameter == name]
    if (length(val) == 0) NA else val
  }
  
  # Final year is the last row of the benefit table
  final_row <- if (!is.null(benefit) && nrow(benefit) > 0) tail(benefit, 1) else NULL
  
  pick <- function(row, candidates) {
    if (is.null(row)) return(NA)
    hit <- intersect(candidates, names(row))
    if (length(hit) == 0) NA else row[[hit[1]]]
  }
  
  tibble::tibble(
    policy_id          = policy_id,
    nation             = nation,
    policy_name        = get_param("POLICY_NAME"),
    kcal_change        = get_param("KCAL_INTAKE_CHANGE"),
    obesity_impact     = pick(final_row,
                              c("relative_change")),
    benefits = pick(final_row, c("value_to_gov_per_year"))
  )
}


summary_table <- imap_dfr(all_results, function(policy_result, policy_id) {
  imap_dfr(policy_result, function(nation_result, nation) {
    build_summary_row(nation_result, policy_id, nation)
  })
})

summary_wide <- summary_table %>%
  pivot_wider(
    id_cols     = c(policy_id, policy_name, kcal_change),
    names_from  = nation,
    values_from = obesity_impact,
    names_glue  = "{nation}_obesity_impact"
  ) %>%
  left_join(
    summary_table %>%
      filter(nation == "England") %>%
      select(policy_id, final_year_benefit),
    by = "policy_id"
  )



dir.create("outputs", showWarnings = FALSE)
readr::write_csv(summary_table, "outputs/summary_table.csv")

message("\nSummary table written to outputs/summary_table.csv")
print(summary_table)

