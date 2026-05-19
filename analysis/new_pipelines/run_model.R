#' ============================================================================
#' run_model.R
#' ============================================================================
#'
#' PURPOSE:
#' Loads a single policy configuration from a YAML file and runs the obesity
#' impact model with those parameters.
#'
#' USAGE:
#'   source("run_model.R")
#'   result <- run_model_for_one_policy("policy_context/policy_01.yaml")
#'
#' HOW IT WORKS:
#' - Reads the YAML file into a named list.
#' - Filters the list to keys that match arguments of run_obesity_model().
#'   Any extra keys (e.g. notes, author) are silently dropped.
#' - Calls run_obesity_model() with do.call(), which maps named list elements
#'   to function arguments.
#' - Any argument omitted from the YAML stays NULL, so run_obesity_model()
#'   falls back to its defaults in config.R.
#' ============================================================================

library(yaml)
source("analysis/new_pipelines/load_files.R")
source("analysis/new_pipelines/main.R")
source("analysis/new_pipelines/config.R")
source("analysis/new_pipelines/utils.R")



run_model_for_one_policy <- function(config_path, survey_data) {
  if (!file.exists(config_path)) {
    stop("Config file not found: ", config_path)
  }
  if (!is.list(survey_data) || is.null(names(survey_data))) {
    stop("survey_data must be a named list, e.g. list(England = bp_hse_2019_adult)")
  }
  
  cfg <- yaml::read_yaml(config_path)
  
  # Keep only keys that match run_obesity_model() arguments
  valid_args <- names(formals(run_obesity_model))
  unknown_keys <- setdiff(names(cfg), valid_args)
  if (length(unknown_keys) > 0) {
    message("Ignoring unknown keys in ", basename(config_path), ": ",
            paste(unknown_keys, collapse = ", "))
  }
  cfg <- cfg[names(cfg) %in% valid_args]
  
  message("Running policy: ", cfg$policy_name %||% basename(config_path))
  
  # The model runs once for each dataset
  results <- lapply(names(survey_data), function(nation) {
    message("  -> ", nation)
    args <- cfg
    args$survey_data <- survey_data[[nation]]
    do.call(run_obesity_model, args)
  })
  
  names(results) <- names(survey_data)
  results
}
