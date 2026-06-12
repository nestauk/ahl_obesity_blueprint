
# Constants:
TIRZ_MEAN_EFFECTS <- get_param(yaml::read_yaml("analysis/policy_analysis/glp-1-options/treatment_effects.yaml"),
                               "TIRZEPATIDE_EFFECTS")
TIRZ_EFFICACY <- get_param(yaml::read_yaml("analysis/policy_analysis/glp-1-options/treatment_effects.yaml"),
                           "TIRZEPATIDE_EFFICACY")

OP_1 = get_param(yaml::read_yaml("analysis/policy_analysis/glp-1-options/cohorts.yaml"),
                 "OP_1")

OP_2 = get_param(yaml::read_yaml("analysis/policy_analysis/glp-1-options/cohorts.yaml"),
                 "OP_2")


OP_3 = get_param(yaml::read_yaml("analysis/policy_analysis/glp-1-options/cohorts.yaml"),
                 "OP_3")

OP_4 = get_param(yaml::read_yaml("analysis/policy_analysis/glp-1-options/cohorts.yaml"),
                 "OP_4")

OP_5 = get_param(yaml::read_yaml("analysis/policy_analysis/glp-1-options/cohorts.yaml"),
                 "OP_5")


ENGLAND_ADULT_POPULATION =  get_param(yaml::read_yaml("analysis/policy_analysis/glp-1-options/constants.yaml"),
                                  "POPULATION_CONSTANTS")$ENGLANG_ADULT_2019_OVER_18