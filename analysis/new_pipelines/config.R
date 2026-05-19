#' ============================================================================
#' Model configurations - Constants and parameters
#' ============================================================================
#'
#' This file contains the constants and parameters used throughout the
#' obesity policy impact modeling analysis.
#'
#' @section Model Constants:
#' Economic parameters for calculating policy benefits:
#' \describe{
#'   \item{COST_OF_OBESITY_IN_BILLIONS}{Annual cost of obesity to UK government
#'     in billions of pounds.}
#'   \item{COST_OF_OBESITY}{Annual cost of obesity to UK government in pounds
#'     (exact value).}
#' }
#'
#' @section Model Parameters:
#' Core modeling assumptions and intervention specifications:
#' \describe{
#'   \item{MODEL_DURATION}{Duration of policy implementation in years. Default
#'     is 5 years.}
#'   \item{COMPENSATION_FACTOR}{Proportion of intake change effect lost due to
#'     behavioral compensation (0-1). Value of 0.23 represents
#'     a 23% compensation.}
#'   \item{INTAKE_CHANGE}{Daily energy intake change in kcal applied as
#'     intervention. Negative values represent intake reduction.}
#' }
#'
#' #' @section Text Parameters:
#' Policy name
#' \describe{
#'   \item{POLICY_NAME}{Name of the policy}
#'
#' ============================================================================


MODEL_CONSTANTS <- list(
  COST_OF_OBESITY_IN_BILLIONS = 104.1
)

MODEL_PARAMETERS <- list(
  MODEL_DURATION = 5,
  COMPENSATION_FACTOR = 0.23,
  SODIUM_INTAKE_CHANGE = 0,
  UPWEIGHTING_BY_BMI = FALSE
)

TEXT_PARAMETERS <- list(
  POLICY_NAME = "Policy Name"
)
