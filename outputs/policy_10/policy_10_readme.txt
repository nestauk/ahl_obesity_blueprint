
#############################################################################################
# Policy 10 : Mandate maximum calorie reduction guidelines for retailers and manufacturers  #
#############################################################################################

# Description:
# The policy mandates all retailers and manufacturers reduce the calorie content of their products 
# to meet the maximum calorie guidelines. A similar policy, on a voluntary basis, was proposed by 
# Public Health England (PHE) under their Calorie reduction programme
# (https://www.gov.uk/government/publications/calorie-reduction-guidelines-for-the-food-industry). 
# The PHE guidelines indicate that for retailers and manufacturers the target was to reduce the calorie 
# content by 10% of the average calorie content of single serve portions.


# The evidence from the rapid review for Portions size interventions 
# (https://docs.google.com/document/d/1Mc9UahGny4g-gO_8mIzEHmMSgTQhhzZuDQ1Y-CQfYeg/edit?usp=sharing) 
# (quality assured by the EAG) that identified a meta-analysis showed that a 40% reduction in portion 
# sizes of products led to a reduction of 247 kcals in daily energy intake.

# In case of this policy, for a 10% reduction, we assume that the reduction in daily energy intake is 
# approximately half of that reported by the review, that is 61.75 kcals `(intake_change)`.

# Adults in England and Scotland:

# Inputs to the model:
# Effect size [A]: ﹣61.75 kcal
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 23% of [A] = 14.2
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -47.5475 kcals


# Children in England and Scotland:

# Inputs to the model:
# Effect size [A]: -95 kcal
# Population segment impacted by policy [B]: Children in age group 5 - 18 years
# Compensation effect [C]: 23% of [A] = 21.85
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -73.15 kcals
