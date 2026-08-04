# Load modelling parameters:

# Model settings:
BMI_THRESHOLD_FOR_ELIGIBILITY <- get_param(yaml::read_yaml("analysis/policy_analysis/glp-1-options/config.yaml"),
                                           "MODEL_SETTINGS")$BMI_THRESHOLD_FOR_ELIGIBILITY

MINIMUM_NUMBER_OF_COMORBIDITIES <- get_param(yaml::read_yaml("analysis/policy_analysis/glp-1-options/config.yaml"),
                                             "MODEL_SETTINGS")$MINIMUM_NUMBER_OF_COMORBIDITIES

NUMBER_OF_YEARS  <- get_param(yaml::read_yaml("analysis/policy_analysis/glp-1-options/config.yaml"),
                              "MODEL_SETTINGS")$NUMBER_OF_YEARS

NUMBER_OF_SELECTIONS_PER_DRAW  <- get_param(yaml::read_yaml("analysis/policy_analysis/glp-1-options/config.yaml"),
                                            "MODEL_SETTINGS")$NUMBER_OF_SELECTIONS_PER_DRAW



# Effect sizes:
TIRZ_MEAN_EFFECTS <- get_param(yaml::read_yaml("analysis/policy_analysis/glp-1-options/config.yaml"),
                               "TIRZEPATIDE_EFFECTS")
TIRZ_EFFICACY <- get_param(yaml::read_yaml("analysis/policy_analysis/glp-1-options/config.yaml"),
                           "TIRZEPATIDE_EFFICACY")

# Cohort allocation/ number of people to be treated per year:
COHORT_ALLOCATIONS <- get_param(yaml::read_yaml("analysis/policy_analysis/glp-1-options/config.yaml"),
                                "COHORT_ALLOCATIONS")

# England population - Adults (18+) in 2019:
ENGLAND_ADULT_POPULATION =  get_param(yaml::read_yaml("analysis/policy_analysis/glp-1-options/config.yaml"),
                                      "POPULATION_CONSTANTS")$ENGLANG_ADULT_2019_OVER_18
