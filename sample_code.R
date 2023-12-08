# sample code


20.5
145.2
9.0



design <-  svydesign(ids=~df$psu, 
                     nest = T,
                     data=df,
                     weights=df$wt_int)




test_1_1 = calculate_fat_mass(weight = 20.5, 
                              height = 1.452, 
                              age = 9, 
                              BA = 0, 
                              SA = 0, 
                              AO = 0, 
                              other = 0,
                              male = 1)
test_1_1

((1.51*24.247980) - (0.7*14) -  ((2.2/100)*43.8))
((1.51*bmi) - (0.7*age) -  ((1.4/100)*weight))
((1.51*26.51349) - (0.7*6) -  ((1.4/100)*27.1))


27.1
101.1
6
26.51349




17.0
104.3
6.0
2
15.627169

43.8
134.4
14.0
1
24.247980
# sum(df_2019_children$ffm <0)

# 1.51⋅BMI0−0.7⋅a−2.2100⋅BW0

# 0.546×BMI+0.0413×age+1.29×sex−9.25




