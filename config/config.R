

MODEL_CONSTANTS = list(
  COST_OF_OBESITY_IN_BILLIONS = 74,
  COST_OF_OBESITY = 74000000000,
  MODEL_DURATION = 5,
  COST_OBESITY_BMI_30_40 = 42.1,
  COST_OBESITY_OVER_40 = 31.9,
  ENGLAND_ADULT_POPULATION = 44263393
)

TIRZ_MODEL_INPUTS = list(
  WEIGHT_LOSS_WITH_T2D = 0.138,
  WEIGHT_LOSS_WITHOUT_T2D = 0.185,
  WEIGHT_REGAIN_POST_TREATMENT = 0
)

TIRZ_MODEL_INPUTS_EFFICACY = list(
  WEIGHT_LOSS_WITH_T2D = 0.146,
  WEIGHT_LOSS_WITHOUT_T2D = 0.20,
  WEIGHT_REGAIN_POST_TREATMENT = 0
)


COHORT_ALLOCATION_OP1 <- list(year1 = list(c1 = 28000),
                              year2 = list(c1 = 14000, c2 = 47500),
                              year3 = list(c2 = 47500, c3 = 85714),
                              year4 = list(c3 = 114285, c4 = 58462),
                              year5 = list(c4 = 175384))

COHORT_ALLOCATION_OP2 <- list(year1 = list(c1 = 21000),
                              year2 = list(c1 = 8400, c2 = 33600),
                              year3 = list(c2 = 32900, c3 = 58100),
                              year4 = list(c3 = 81900, c4 = 44100),
                              year5 = list(c4 = 126000))

# COHORT_10K <- list(year1 = list(c1 = 10000),
#                    year2 = list(c1 = 0, c2 = 0),
#                    year3 = list(c2 = 0, c3 = 0),
#                    year4 = list(c3 = 0, c4 = 0),
#                    year5 = list(c4 = 0))
# 
# 
# 
# COHORT_50K <- list(year1 = list(c1 = 50000),
#                    year2 = list(c1 = 0, c2 = 0),
#                    year3 = list(c2 = 0, c3 = 0),
#                    year4 = list(c3 = 0, c4 = 0),
#                    year5 = list(c4 = 0))
# 
# 
# COHORT_100K <- list(year1 = list(c1 = 100000),
#                     year2 = list(c1 = 0, c2 = 0),
#                     year3 = list(c2 = 0, c3 = 0),
#                     year4 = list(c3 = 0, c4 = 0),
#                     year5 = list(c4 = 0))

# 10K
COHORT_10K_5Y <- list(year1 = list(c1 = 2000),
                      year2 = list(c1 = 2000),
                      year3 = list(c1 = 2000),
                      year4 = list(c1 = 2000),
                      year5 = list(c1 = 2000))


# 20K
COHORT_20K_5Y <- list(year1 = list(c1 = 4000),
                      year2 = list(c1 = 4000),
                      year3 = list(c1 = 4000),
                      year4 = list(c1 = 4000),
                      year5 = list(c1 = 4000))


# 50K
COHORT_50K_5Y <- list(year1 = list(c1 = 10000),
                      year2 = list(c1 = 10000),
                      year3 = list(c1 = 10000),
                      year4 = list(c1 = 10000),
                      year5 = list(c1 = 10000))


# 100K
COHORT_100K_5Y <- list(year1 = list(c1 = 20000),
                       year2 = list(c1 = 20000),
                       year3 = list(c1 = 20000),
                       year4 = list(c1 = 20000),
                       year5 = list(c1 = 20000))

# 150K
COHORT_150K_5Y <- list(year1 = list(c1 = 30000),
                       year2 = list(c1 = 30000),
                       year3 = list(c1 = 30000),
                       year4 = list(c1 = 30000),
                       year5 = list(c1 = 30000))


# 200K
COHORT_200K_5Y <- list(year1 = list(c1 = 40000),
                       year2 = list(c1 = 40000),
                       year3 = list(c1 = 40000),
                       year4 = list(c1 = 40000),
                       year5 = list(c1 = 40000))



# 600K
COHORT_600K_5Y <- list(year1 = list(c1 = 120000),
                       year2 = list(c1 = 120000),
                       year3 = list(c1 = 120000),
                       year4 = list(c1 = 120000),
                       year5 = list(c1 = 120000))



# 750K
COHORT_750K_5Y <- list(year1 = list(c1 = 150000),
                       year2 = list(c1 = 150000),
                       year3 = list(c1 = 150000),
                       year4 = list(c1 = 150000),
                       year5 = list(c1 = 150000))


# 1MIL
COHORT_1MIL_5Y <- list(year1 = list(c1 = 200000),
                       year2 = list(c1 = 200000),
                       year3 = list(c1 = 200000),
                       year4 = list(c1 = 200000),
                       year5 = list(c1 = 200000))


# 1.5MIL
COHORT_1_5_MIL_5Y <- list(year1 = list(c1 = 300000),
                          year2 = list(c1 = 300000),
                          year3 = list(c1 = 300000),
                          year4 = list(c1 = 300000),
                          year5 = list(c1 = 300000))



# 2.5 mil
COHORT_2_5_MIL_5Y <- list(year1 = list(c1 = 500000),
                          year2 = list(c1 = 500000),
                          year3 = list(c1 = 500000),
                          year4 = list(c1 = 500000),
                          year5 = list(c1 = 500000))



all_cohorts <- list(
  COHORT_10K_5Y = COHORT_10K_5Y,
  COHORT_20K_5Y = COHORT_20K_5Y,
  COHORT_50K_5Y = COHORT_50K_5Y,
  COHORT_100K_5Y = COHORT_100K_5Y,
  COHORT_150K_5Y = COHORT_150K_5Y,
  COHORT_200K_5Y = COHORT_200K_5Y,
  COHORT_600K_5Y = COHORT_600K_5Y,
  COHORT_750K_5Y = COHORT_750K_5Y,
  COHORT_1MIL_5Y = COHORT_1MIL_5Y,
  COHORT_1_5_MIL_5Y = COHORT_1_5_MIL_5Y,
  COHORT_2_5_MIL_5Y = COHORT_2_5_MIL_5Y
)

# 1 Year:

# 10K
COHORT_10K_1Y <- list(year1 = list(c1 = 10000),
                      year2 = list(c1 = 0),
                      year3 = list(c1 = 0),
                      year4 = list(c1 = 0),
                      year5 = list(c1 = 0))


# 20K
COHORT_20K_1Y <- list(year1 = list(c1 = 20000),
                      year2 = list(c1 = 0),
                      year3 = list(c1 = 0),
                      year4 = list(c1 = 0),
                      year5 = list(c1 = 0))


# 50K
COHORT_50K_1Y <- list(year1 = list(c1 = 50000),
                      year2 = list(c1 = 0),
                      year3 = list(c1 = 0),
                      year4 = list(c1 = 0),
                      year5 = list(c1 = 0))


# 100K
COHORT_100K_1Y <- list(year1 = list(c1 = 100000),
                       year2 = list(c1 = 0),
                       year3 = list(c1 = 0),
                       year4 = list(c1 = 0),
                       year5 = list(c1 = 0))

# 150K
COHORT_150K_1Y <- list(year1 = list(c1 = 150000),
                       year2 = list(c1 = 0),
                       year3 = list(c1 = 0),
                       year4 = list(c1 = 0),
                       year5 = list(c1 = 0))


# 200K
COHORT_200K_1Y <- list(year1 = list(c1 = 200000),
                       year2 = list(c1 = 0),
                       year3 = list(c1 = 0),
                       year4 = list(c1 = 0),
                       year5 = list(c1 = 0))


# 300K
COHORT_300K_1Y <- list(year1 = list(c1 = 300000),
                       year2 = list(c1 = 0),
                       year3 = list(c1 = 0),
                       year4 = list(c1 = 0),
                       year5 = list(c1 = 0))



# 600K
COHORT_600K_1Y <- list(year1 = list(c1 = 600000),
                       year2 = list(c1 = 0),
                       year3 = list(c1 = 0),
                       year4 = list(c1 = 0),
                       year5 = list(c1 = 0))



# 750K
COHORT_750K_1Y <- list(year1 = list(c1 = 750000),
                       year2 = list(c1 = 0),
                       year3 = list(c1 = 0),
                       year4 = list(c1 = 0),
                       year5 = list(c1 = 0))


# 1MIL
COHORT_1MIL_1Y <- list(year1 = list(c1 = 1000000),
                       year2 = list(c1 = 0),
                       year3 = list(c1 = 0),
                       year4 = list(c1 = 0),
                       year5 = list(c1 = 0))


# 1.5MIL
COHORT_1_5_MIL_1Y <- list(year1 = list(c1 = 1500000),
                          year2 = list(c1 = 0),
                          year3 = list(c1 = 0),
                          year4 = list(c1 = 0),
                          year5 = list(c1 = 0))



# 2.5 mil
COHORT_2_5_MIL_1Y <- list(year1 = list(c1 = 2500000),
                          year2 = list(c1 = 0),
                          year3 = list(c1 = 0),
                          year4 = list(c1 = 0),
                          year5 = list(c1 = 0))


all_cohorts_1Y <- list(
  COHORT_10K_1Y = COHORT_10K_1Y,
  COHORT_20K_1Y = COHORT_20K_1Y,
  COHORT_50K_1Y = COHORT_50K_1Y,
  COHORT_100K_1Y = COHORT_100K_1Y,
  COHORT_150K_1Y = COHORT_150K_1Y,
  COHORT_200K_1Y = COHORT_200K_1Y,
  COHORT_300K_1Y = COHORT_300K_1Y,
  COHORT_600K_1Y = COHORT_600K_1Y,
  COHORT_750K_1Y = COHORT_750K_1Y,
  COHORT_1MIL_1Y = COHORT_1MIL_1Y,
  COHORT_1_5_MIL_1Y = COHORT_1_5_MIL_1Y,
  COHORT_2_5_MIL_1Y = COHORT_2_5_MIL_1Y
)
