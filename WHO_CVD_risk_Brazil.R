# Calculation of 10-year cardiovascular disease* risk
# according to the 2019 WHO CVD Risk Chart
# calibrated for "Tropical Latin America" (Brazil + Paraguay)

# * coronary artery disease or stroke, fatal or non-fatal events

# applies to individuals between 40 and 74 years of age

#### library ####
library(dplyr)

#### import database and rename it ####
base_WHO_cv_risk <- base # enter database name

#### create variable h_smoking = current smoking: 0 = No; 1 = Yes ####
base_WHO_cv_risk <- base_WHO_cv_risk %>%
  mutate(h_smoking = case_when(
    # enter variable "smoking" from the database
    # in this example: 0 = never smoked; 1 = former smoker; 2 = current smoker
    smoking == 0  | smoking == 1 ~ 0,
    smoking == 2 ~ 1
  ))

#### rename database variables #### 
base_WHO_cv_risk <- base_WHO_cv_risk %>%
  rename (h_age = age, # enter variable "age"
          h_sex = sex, # enter variable "sex": 1 = Male; 2 = Female
          h_sbp = sbp, # enter variable "systolic blood pressure" (mmHg)
          h_tc = tc, # enter variable "total cholesterol" (mg/dL)
          h_dm = dm) # enter variable "diabetes mellitus": 0 = No; 1 = Yes

#### RUN ALL THE CODE BELOW ####  
# create variable h_tc_mmoll = total cholesterol in mmol/L
base_WHO_cv_risk$h_tc_mmoll = base_WHO_cv_risk$h_tc * 0.02586

# create h_tc_mmoll categories
base_WHO_cv_risk <- base_WHO_cv_risk %>%
  mutate(cat_tc_mmoll = case_when(h_tc_mmoll < 4 ~ 1,
                                  h_tc_mmoll >= 4 & h_tc_mmoll < 5 ~ 2,
                                  h_tc_mmoll >= 5 & h_tc_mmoll < 6 ~ 3,
                                  h_tc_mmoll >= 6 & h_tc_mmoll < 7 ~ 4,
                                  h_tc_mmoll >= 7 ~ 5))

# create h_sbp categories
base_WHO_cv_risk <- base_WHO_cv_risk %>%
  mutate(cat_sbp = case_when(h_sbp < 120 ~ 1,
                             h_sbp >= 120 & h_sbp < 140 ~ 2,
                             h_sbp >= 140 & h_sbp < 160 ~ 3,
                             h_sbp >= 160 & h_sbp < 180 ~ 4,
                             h_sbp >= 180 ~ 5))

# create age categories
base_WHO_cv_risk <- base_WHO_cv_risk %>%
  mutate(cat_age = case_when(h_age >= 40 & h_age < 45 ~ 1,
                             h_age >= 45 & h_age < 50 ~ 2,
                             h_age >= 50 & h_age < 55 ~ 3,
                             h_age >= 55 & h_age < 60 ~ 4,
                             h_age >= 60 & h_age < 65 ~ 5,
                             h_age >= 65 & h_age < 70 ~ 6,
                             h_age >= 70 & h_age < 75 ~ 7))

# create categories according to sex, age, DM, smoking
base_WHO_cv_risk <- base_WHO_cv_risk %>%
  mutate(cat_multi = case_when(
    # no DM, male, no smoking
    h_dm == 0 & h_sex == 1 & h_smoking == 0 & cat_age == 1 ~ 1,
    h_dm == 0 & h_sex == 1 & h_smoking == 0 & cat_age == 2 ~ 2,
    h_dm == 0 & h_sex == 1 & h_smoking == 0 & cat_age == 3 ~ 3,
    h_dm == 0 & h_sex == 1 & h_smoking == 0 & cat_age == 4 ~ 4,
    h_dm == 0 & h_sex == 1 & h_smoking == 0 & cat_age == 5 ~ 5,
    h_dm == 0 & h_sex == 1 & h_smoking == 0 & cat_age == 6 ~ 6,
    h_dm == 0 & h_sex == 1 & h_smoking == 0 & cat_age == 7 ~ 7,
    
    # no DM, male, with smoking
    h_dm == 0 & h_sex == 1 & h_smoking == 1 & cat_age == 1 ~ 8,
    h_dm == 0 & h_sex == 1 & h_smoking == 1 & cat_age == 2 ~ 9,
    h_dm == 0 & h_sex == 1 & h_smoking == 1 & cat_age == 3 ~ 10,
    h_dm == 0 & h_sex == 1 & h_smoking == 1 & cat_age == 4 ~ 11,
    h_dm == 0 & h_sex == 1 & h_smoking == 1 & cat_age == 5 ~ 12,
    h_dm == 0 & h_sex == 1 & h_smoking == 1 & cat_age == 6 ~ 13,
    h_dm == 0 & h_sex == 1 & h_smoking == 1 & cat_age == 7 ~ 14,
    
    # no DM, female, no smoking
    h_dm == 0 & h_sex == 2 & h_smoking == 0 & cat_age == 1 ~ 15,
    h_dm == 0 & h_sex == 2 & h_smoking == 0 & cat_age == 2 ~ 16,
    h_dm == 0 & h_sex == 2 & h_smoking == 0 & cat_age == 3 ~ 17,
    h_dm == 0 & h_sex == 2 & h_smoking == 0 & cat_age == 4 ~ 18,
    h_dm == 0 & h_sex == 2 & h_smoking == 0 & cat_age == 5 ~ 19,
    h_dm == 0 & h_sex == 2 & h_smoking == 0 & cat_age == 6 ~ 20,
    h_dm == 0 & h_sex == 2 & h_smoking == 0 & cat_age == 7 ~ 21,
    
    # no DM, female, with smoking
    h_dm == 0 & h_sex == 2 & h_smoking == 1 & cat_age == 1 ~ 22,
    h_dm == 0 & h_sex == 2 & h_smoking == 1 & cat_age == 2 ~ 23,
    h_dm == 0 & h_sex == 2 & h_smoking == 1 & cat_age == 3 ~ 24,
    h_dm == 0 & h_sex == 2 & h_smoking == 1 & cat_age == 4 ~ 25,
    h_dm == 0 & h_sex == 2 & h_smoking == 1 & cat_age == 5 ~ 26,
    h_dm == 0 & h_sex == 2 & h_smoking == 1 & cat_age == 6 ~ 27,
    h_dm == 0 & h_sex == 2 & h_smoking == 1 & cat_age == 7 ~ 28,
    
    # with DM, male, no smoking
    h_dm == 1 & h_sex == 1 & h_smoking == 0 & cat_age == 1 ~ 29,
    h_dm == 1 & h_sex == 1 & h_smoking == 0 & cat_age == 2 ~ 30,
    h_dm == 1 & h_sex == 1 & h_smoking == 0 & cat_age == 3 ~ 31,
    h_dm == 1 & h_sex == 1 & h_smoking == 0 & cat_age == 4 ~ 32,
    h_dm == 1 & h_sex == 1 & h_smoking == 0 & cat_age == 5 ~ 33,
    h_dm == 1 & h_sex == 1 & h_smoking == 0 & cat_age == 6 ~ 34,
    h_dm == 1 & h_sex == 1 & h_smoking == 0 & cat_age == 7 ~ 35,
    
    # with DM, male, with smoking
    h_dm == 1 & h_sex == 1 & h_smoking == 1 & cat_age == 1 ~ 36,
    h_dm == 1 & h_sex == 1 & h_smoking == 1 & cat_age == 2 ~ 37,
    h_dm == 1 & h_sex == 1 & h_smoking == 1 & cat_age == 3 ~ 38,
    h_dm == 1 & h_sex == 1 & h_smoking == 1 & cat_age == 4 ~ 39,
    h_dm == 1 & h_sex == 1 & h_smoking == 1 & cat_age == 5 ~ 40,
    h_dm == 1 & h_sex == 1 & h_smoking == 1 & cat_age == 6 ~ 41,
    h_dm == 1 & h_sex == 1 & h_smoking == 1 & cat_age == 7 ~ 42,
    
    # with DM, female, no smoking
    h_dm == 1 & h_sex == 2 & h_smoking == 0 & cat_age == 1 ~ 43,
    h_dm == 1 & h_sex == 2 & h_smoking == 0 & cat_age == 2 ~ 44,
    h_dm == 1 & h_sex == 2 & h_smoking == 0 & cat_age == 3 ~ 45,
    h_dm == 1 & h_sex == 2 & h_smoking == 0 & cat_age == 4 ~ 46,
    h_dm == 1 & h_sex == 2 & h_smoking == 0 & cat_age == 5 ~ 47,
    h_dm == 1 & h_sex == 2 & h_smoking == 0 & cat_age == 6 ~ 48,
    h_dm == 1 & h_sex == 2 & h_smoking == 0 & cat_age == 7 ~ 49,
    
    # with DM, female, with smoking
    h_dm == 1 & h_sex == 2 & h_smoking == 1 & cat_age == 1 ~ 50,
    h_dm == 1 & h_sex == 2 & h_smoking == 1 & cat_age == 2 ~ 51,
    h_dm == 1 & h_sex == 2 & h_smoking == 1 & cat_age == 3 ~ 52,
    h_dm == 1 & h_sex == 2 & h_smoking == 1 & cat_age == 4 ~ 53,
    h_dm == 1 & h_sex == 2 & h_smoking == 1 & cat_age == 5 ~ 54,
    h_dm == 1 & h_sex == 2 & h_smoking == 1 & cat_age == 6 ~ 55,
    h_dm == 1 & h_sex == 2 & h_smoking == 1 & cat_age == 7 ~ 56))

# create variable WHO_cv_risk
base_WHO_cv_risk <- base_WHO_cv_risk %>%
  mutate(WHO_cv_risk = case_when(
    # no DM, male, no smoking, age 40-44
    cat_multi == 1 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 1,
    cat_multi == 1 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 2,
    cat_multi == 1 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 2,
    cat_multi == 1 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 3,
    cat_multi == 1 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 5,
    
    cat_multi == 1 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 1,
    cat_multi == 1 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 2,
    cat_multi == 1 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 3,
    cat_multi == 1 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 4,
    cat_multi == 1 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 5,
    
    cat_multi == 1 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 2,
    cat_multi == 1 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 2,
    cat_multi == 1 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 3,
    cat_multi == 1 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 4,
    cat_multi == 1 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 6,
    
    cat_multi == 1 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 2,
    cat_multi == 1 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 3,
    cat_multi == 1 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 3,
    cat_multi == 1 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 5,
    cat_multi == 1 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 6,
    
    cat_multi == 1 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 2,
    cat_multi == 1 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 3,
    cat_multi == 1 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 4,
    cat_multi == 1 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 5,
    cat_multi == 1 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 7,
    
    # no DM, male, no smoking, age 45-49
    cat_multi == 2 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 2,
    cat_multi == 2 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 2,
    cat_multi == 2 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 3,
    cat_multi == 2 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 4,
    cat_multi == 2 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 6,
    
    cat_multi == 2 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 2,
    cat_multi == 2 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 3,
    cat_multi == 2 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 4,
    cat_multi == 2 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 5,
    cat_multi == 2 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 6,
    
    cat_multi == 2 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 2,
    cat_multi == 2 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 3,
    cat_multi == 2 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 4,
    cat_multi == 2 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 5,
    cat_multi == 2 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 7,
    
    cat_multi == 2 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 3,
    cat_multi == 2 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 3,
    cat_multi == 2 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 4,
    cat_multi == 2 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 6,
    cat_multi == 2 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 8,
    
    cat_multi == 2 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 3,
    cat_multi == 2 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 4,
    cat_multi == 2 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 5,
    cat_multi == 2 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 7,
    cat_multi == 2 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 9,
    
    # no DM, male, no smoking, age 50-54
    cat_multi == 3 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 2,
    cat_multi == 3 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 3,
    cat_multi == 3 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 4,
    cat_multi == 3 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 6,
    cat_multi == 3 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 7,
    
    cat_multi == 3 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 3,
    cat_multi == 3 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 4,
    cat_multi == 3 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 5,
    cat_multi == 3 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 6,
    cat_multi == 3 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 8,
    
    cat_multi == 3 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 3,
    cat_multi == 3 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 4,
    cat_multi == 3 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 5,
    cat_multi == 3 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 7,
    cat_multi == 3 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 9,
    
    cat_multi == 3 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 3,
    cat_multi == 3 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 4,
    cat_multi == 3 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 6,
    cat_multi == 3 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 8,
    cat_multi == 3 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 10,
    
    cat_multi == 3 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 4,
    cat_multi == 3 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 5,
    cat_multi == 3 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 7,
    cat_multi == 3 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 9,
    cat_multi == 3 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 11,
    
    # no DM, male, no smoking, age 55-59
    cat_multi == 4 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 3,
    cat_multi == 4 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 4,
    cat_multi == 4 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 6,
    cat_multi == 4 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 7,
    cat_multi == 4 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 9,
    
    cat_multi == 4 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 4,
    cat_multi == 4 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 5,
    cat_multi == 4 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 6,
    cat_multi == 4 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 8,
    cat_multi == 4 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 10,
    
    cat_multi == 4 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 4,
    cat_multi == 4 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 5,
    cat_multi == 4 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 7,
    cat_multi == 4 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 9,
    cat_multi == 4 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 11,
    
    cat_multi == 4 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 5,
    cat_multi == 4 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 6,
    cat_multi == 4 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 8,
    cat_multi == 4 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 10,
    cat_multi == 4 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 12,
    
    cat_multi == 4 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 5,
    cat_multi == 4 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 7,
    cat_multi == 4 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 8,
    cat_multi == 4 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 11,
    cat_multi == 4 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 14,
    
    # no DM, male, no smoking, age 60-64
    cat_multi == 5 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 5,
    cat_multi == 5 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 6,
    cat_multi == 5 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 7,
    cat_multi == 5 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 9,
    cat_multi == 5 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 12,
    
    cat_multi == 5 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 5,
    cat_multi == 5 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 6,
    cat_multi == 5 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 8,
    cat_multi == 5 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 10,
    cat_multi == 5 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 13,
    
    cat_multi == 5 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 6,
    cat_multi == 5 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 7,
    cat_multi == 5 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 9,
    cat_multi == 5 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 11,
    cat_multi == 5 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 14,
    
    cat_multi == 5 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 6,
    cat_multi == 5 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 8,
    cat_multi == 5 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 10,
    cat_multi == 5 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 12,
    cat_multi == 5 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 15,
    
    cat_multi == 5 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 7,
    cat_multi == 5 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 9,
    cat_multi == 5 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 11,
    cat_multi == 5 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 14,
    cat_multi == 5 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 17,
    
    # no DM, male, no smoking, age 65-69
    cat_multi == 6 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 6,
    cat_multi == 6 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 8,
    cat_multi == 6 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 10,
    cat_multi == 6 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 12,
    cat_multi == 6 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 15,
    
    cat_multi == 6 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 7,
    cat_multi == 6 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 9,
    cat_multi == 6 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 11,
    cat_multi == 6 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 13,
    cat_multi == 6 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 16,
    
    cat_multi == 6 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 8,
    cat_multi == 6 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 9,
    cat_multi == 6 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 12,
    cat_multi == 6 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 14,
    cat_multi == 6 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 17,
    
    cat_multi == 6 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 8,
    cat_multi == 6 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 10,
    cat_multi == 6 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 13,
    cat_multi == 6 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 15,
    cat_multi == 6 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 19,
    
    cat_multi == 6 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 9,
    cat_multi == 6 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 11,
    cat_multi == 6 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 14,
    cat_multi == 6 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 17,
    cat_multi == 6 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 21,
    
    # no DM, male, no smoking, age 70-74
    cat_multi == 7 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 9,
    cat_multi == 7 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 11,
    cat_multi == 7 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 13,
    cat_multi == 7 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 15,
    cat_multi == 7 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 18,
    
    cat_multi == 7 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 9,
    cat_multi == 7 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 11,
    cat_multi == 7 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 14,
    cat_multi == 7 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 17,
    cat_multi == 7 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 20,
    
    cat_multi == 7 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 10,
    cat_multi == 7 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 12,
    cat_multi == 7 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 15,
    cat_multi == 7 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 18,
    cat_multi == 7 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 21,
    
    cat_multi == 7 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 11,
    cat_multi == 7 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 14,
    cat_multi == 7 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 16,
    cat_multi == 7 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 19,
    cat_multi == 7 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 23,
    
    cat_multi == 7 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 12,
    cat_multi == 7 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 15,
    cat_multi == 7 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 18,
    cat_multi == 7 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 21,
    cat_multi == 7 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 25,
    
    # no DM, male, with smoking, age 40-44
    cat_multi == 8 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 2,
    cat_multi == 8 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 3,
    cat_multi == 8 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 4,
    cat_multi == 8 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 6,
    cat_multi == 8 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 8,
    
    cat_multi == 8 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 3,
    cat_multi == 8 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 4,
    cat_multi == 8 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 5,
    cat_multi == 8 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 7,
    cat_multi == 8 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 9,
    
    cat_multi == 8 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 3,
    cat_multi == 8 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 4,
    cat_multi == 8 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 5,
    cat_multi == 8 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 7,
    cat_multi == 8 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 10,
    
    cat_multi == 8 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 3,
    cat_multi == 8 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 5,
    cat_multi == 8 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 6,
    cat_multi == 8 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 8,
    cat_multi == 8 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 11,
    
    cat_multi == 8 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 4,
    cat_multi == 8 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 6,
    cat_multi == 8 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 7,
    cat_multi == 8 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 10,
    cat_multi == 8 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 13,
    
    # no DM, male, with smoking, age 45-49
    cat_multi == 9 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 3,
    cat_multi == 9 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 4,
    cat_multi == 9 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 5,
    cat_multi == 9 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 7,
    cat_multi == 9 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 10,
    
    cat_multi == 9 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 3,
    cat_multi == 9 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 4,
    cat_multi == 9 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 6,
    cat_multi == 9 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 8,
    cat_multi == 9 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 11,
    
    cat_multi == 9 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 4,
    cat_multi == 9 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 5,
    cat_multi == 9 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 7,
    cat_multi == 9 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 9,
    cat_multi == 9 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 12,
    
    cat_multi == 9 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 4,
    cat_multi == 9 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 6,
    cat_multi == 9 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 8,
    cat_multi == 9 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 10,
    cat_multi == 9 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 13,
    
    cat_multi == 9 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 5,
    cat_multi == 9 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 7,
    cat_multi == 9 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 9,
    cat_multi == 9 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 12,
    cat_multi == 9 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 15,
    
    # no DM, male, with smoking, age 50-54
    cat_multi == 10 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 4,
    cat_multi == 10 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 5,
    cat_multi == 10 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 7,
    cat_multi == 10 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 9,
    cat_multi == 10 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 11,
    
    cat_multi == 10 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 4,
    cat_multi == 10 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 6,
    cat_multi == 10 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 7,
    cat_multi == 10 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 10,
    cat_multi == 10 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 13,
    
    cat_multi == 10 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 5,
    cat_multi == 10 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 6,
    cat_multi == 10 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 8,
    cat_multi == 10 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 11,
    cat_multi == 10 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 14,
    
    cat_multi == 10 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 6,
    cat_multi == 10 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 7,
    cat_multi == 10 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 9,
    cat_multi == 10 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 12,
    cat_multi == 10 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 16,
    
    cat_multi == 10 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 6,
    cat_multi == 10 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 8,
    cat_multi == 10 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 11,
    cat_multi == 10 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 14,
    cat_multi == 10 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 18,
    
    # no DM, male, with smoking, age 55-59
    cat_multi == 11 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 5,
    cat_multi == 11 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 6,
    cat_multi == 11 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 8,
    cat_multi == 11 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 11,
    cat_multi == 11 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 14,
    
    cat_multi == 11 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 6,
    cat_multi == 11 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 7,
    cat_multi == 11 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 9,
    cat_multi == 11 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 12,
    cat_multi == 11 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 15,
    
    cat_multi == 11 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 6,
    cat_multi == 11 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 8,
    cat_multi == 11 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 10,
    cat_multi == 11 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 13,
    cat_multi == 11 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 16,
    
    cat_multi == 11 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 7,
    cat_multi == 11 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 9,
    cat_multi == 11 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 11,
    cat_multi == 11 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 14,
    cat_multi == 11 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 18,
    
    cat_multi == 11 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 8,
    cat_multi == 11 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 10,
    cat_multi == 11 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 13,
    cat_multi == 11 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 16,
    cat_multi == 11 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 20,
    
    # no DM, male, with smoking, age 60-64
    cat_multi == 12 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 7,
    cat_multi == 12 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 8,
    cat_multi == 12 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 10,
    cat_multi == 12 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 13,
    cat_multi == 12 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 16,
    
    cat_multi == 12 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 7,
    cat_multi == 12 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 9,
    cat_multi == 12 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 11,
    cat_multi == 12 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 14,
    cat_multi == 12 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 18,
    
    cat_multi == 12 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 8,
    cat_multi == 12 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 10,
    cat_multi == 12 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 12,
    cat_multi == 12 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 16,
    cat_multi == 12 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 19,
    
    cat_multi == 12 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 9,
    cat_multi == 12 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 11,
    cat_multi == 12 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 14,
    cat_multi == 12 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 17,
    cat_multi == 12 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 21,
    
    cat_multi == 12 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 10,
    cat_multi == 12 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 12,
    cat_multi == 12 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 15,
    cat_multi == 12 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 19,
    cat_multi == 12 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 23,
    
    # no DM, male, with smoking, age 65-69
    cat_multi == 13 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 8,
    cat_multi == 13 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 10,
    cat_multi == 13 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 13,
    cat_multi == 13 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 16,
    cat_multi == 13 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 19,
    
    cat_multi == 13 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 9,
    cat_multi == 13 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 11,
    cat_multi == 13 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 14,
    cat_multi == 13 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 17,
    cat_multi == 13 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 21,
    
    cat_multi == 13 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 10,
    cat_multi == 13 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 12,
    cat_multi == 13 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 15,
    cat_multi == 13 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 19,
    cat_multi == 13 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 23,
    
    cat_multi == 13 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 11,
    cat_multi == 13 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 14,
    cat_multi == 13 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 17,
    cat_multi == 13 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 20,
    cat_multi == 13 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 25,
    
    cat_multi == 13 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 12,
    cat_multi == 13 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 15,
    cat_multi == 13 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 18,
    cat_multi == 13 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 22,
    cat_multi == 13 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 27,
    
    # no DM, male, with smoking, age 70-74
    cat_multi == 14 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 11,
    cat_multi == 14 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 13,
    cat_multi == 14 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 16,
    cat_multi == 14 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 19,
    cat_multi == 14 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 23,
    
    cat_multi == 14 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 12,
    cat_multi == 14 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 14,
    cat_multi == 14 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 17,
    cat_multi == 14 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 21,
    cat_multi == 14 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 24,
    
    cat_multi == 14 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 13,
    cat_multi == 14 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 16,
    cat_multi == 14 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 19,
    cat_multi == 14 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 22,
    cat_multi == 14 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 26,
    
    cat_multi == 14 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 14,
    cat_multi == 14 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 17,
    cat_multi == 14 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 20,
    cat_multi == 14 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 24,
    cat_multi == 14 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 29,
    
    cat_multi == 14 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 15,
    cat_multi == 14 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 18,
    cat_multi == 14 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 22,
    cat_multi == 14 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 26,
    cat_multi == 14 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 31,
    
    # no DM, female, no smoking, age 40-44
    cat_multi == 15 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 1,
    cat_multi == 15 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 1,
    cat_multi == 15 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 2,
    cat_multi == 15 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 2,
    cat_multi == 15 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 3,
    
    cat_multi == 15 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 1,
    cat_multi == 15 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 1,
    cat_multi == 15 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 2,
    cat_multi == 15 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 2,
    cat_multi == 15 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 3,
    
    cat_multi == 15 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 1,
    cat_multi == 15 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 2,
    cat_multi == 15 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 2,
    cat_multi == 15 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 3,
    cat_multi == 15 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 4,
    
    cat_multi == 15 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 1,
    cat_multi == 15 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 2,
    cat_multi == 15 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 2,
    cat_multi == 15 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 3,
    cat_multi == 15 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 4,
    
    cat_multi == 15 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 1,
    cat_multi == 15 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 2,
    cat_multi == 15 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 2,
    cat_multi == 15 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 3,
    cat_multi == 15 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 4,
    
    # no DM, female, no smoking, age 45-49
    cat_multi == 16 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 1,
    cat_multi == 16 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 2,
    cat_multi == 16 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 2,
    cat_multi == 16 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 3,
    cat_multi == 16 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 4,
    
    cat_multi == 16 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 1,
    cat_multi == 16 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 2,
    cat_multi == 16 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 2,
    cat_multi == 16 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 3,
    cat_multi == 16 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 4,
    
    cat_multi == 16 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 2,
    cat_multi == 16 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 2,
    cat_multi == 16 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 3,
    cat_multi == 16 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 3,
    cat_multi == 16 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 4,
    
    cat_multi == 16 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 2,
    cat_multi == 16 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 2,
    cat_multi == 16 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 3,
    cat_multi == 16 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 4,
    cat_multi == 16 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 5,
    
    cat_multi == 16 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 2,
    cat_multi == 16 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 2,
    cat_multi == 16 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 3,
    cat_multi == 16 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 4,
    cat_multi == 16 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 5,
    
    # no DM, female, no smoking, age 50-54
    cat_multi == 17 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 2,
    cat_multi == 17 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 2,
    cat_multi == 17 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 3,
    cat_multi == 17 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 4,
    cat_multi == 17 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 5,
    
    cat_multi == 17 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 2,
    cat_multi == 17 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 3,
    cat_multi == 17 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 3,
    cat_multi == 17 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 4,
    cat_multi == 17 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 5,
    
    cat_multi == 17 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 2,
    cat_multi == 17 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 3,
    cat_multi == 17 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 3,
    cat_multi == 17 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 4,
    cat_multi == 17 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 5,
    
    cat_multi == 17 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 2,
    cat_multi == 17 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 3,
    cat_multi == 17 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 4,
    cat_multi == 17 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 5,
    cat_multi == 17 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 6,
    
    cat_multi == 17 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 2,
    cat_multi == 17 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 3,
    cat_multi == 17 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 4,
    cat_multi == 17 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 5,
    cat_multi == 17 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 6,
    
    # no DM, female, no smoking, age 55-59
    cat_multi == 18 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 3,
    cat_multi == 18 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 3,
    cat_multi == 18 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 4,
    cat_multi == 18 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 5,
    cat_multi == 18 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 6,
    
    cat_multi == 18 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 3,
    cat_multi == 18 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 3,
    cat_multi == 18 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 4,
    cat_multi == 18 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 5,
    cat_multi == 18 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 6,
    
    cat_multi == 18 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 3,
    cat_multi == 18 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 4,
    cat_multi == 18 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 4,
    cat_multi == 18 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 6,
    cat_multi == 18 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 7,
    
    cat_multi == 18 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 3,
    cat_multi == 18 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 4,
    cat_multi == 18 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 5,
    cat_multi == 18 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 6,
    cat_multi == 18 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 7,
    
    cat_multi == 18 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 3,
    cat_multi == 18 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 4,
    cat_multi == 18 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 5,
    cat_multi == 18 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 6,
    cat_multi == 18 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 8,
    
    # no DM, female, no smoking, age 60-64
    cat_multi == 19 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 4,
    cat_multi == 19 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 4,
    cat_multi == 19 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 5,
    cat_multi == 19 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 6,
    cat_multi == 19 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 8,
    
    cat_multi == 19 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 4,
    cat_multi == 19 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 5,
    cat_multi == 19 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 6,
    cat_multi == 19 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 7,
    cat_multi == 19 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 8,
    
    cat_multi == 19 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 4,
    cat_multi == 19 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 5,
    cat_multi == 19 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 6,
    cat_multi == 19 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 7,
    cat_multi == 19 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 9,
    
    cat_multi == 19 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 4,
    cat_multi == 19 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 5,
    cat_multi == 19 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 6,
    cat_multi == 19 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 7,
    cat_multi == 19 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 9,
    
    cat_multi == 19 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 4,
    cat_multi == 19 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 5,
    cat_multi == 19 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 7,
    cat_multi == 19 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 8,
    cat_multi == 19 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 10,
    
    # no DM, female, no smoking, age 65-69
    cat_multi == 20 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 5,
    cat_multi == 20 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 6,
    cat_multi == 20 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 7,
    cat_multi == 20 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 8,
    cat_multi == 20 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 10,
    
    cat_multi == 20 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 5,
    cat_multi == 20 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 6,
    cat_multi == 20 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 7,
    cat_multi == 20 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 9,
    cat_multi == 20 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 10,
    
    cat_multi == 20 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 5,
    cat_multi == 20 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 6,
    cat_multi == 20 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 8,
    cat_multi == 20 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 9,
    cat_multi == 20 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 11,
    
    cat_multi == 20 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 6,
    cat_multi == 20 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 7,
    cat_multi == 20 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 8,
    cat_multi == 20 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 10,
    cat_multi == 20 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 11,
    
    cat_multi == 20 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 6,
    cat_multi == 20 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 7,
    cat_multi == 20 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 8,
    cat_multi == 20 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 10,
    cat_multi == 20 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 12,
    
    # no DM, female, no smoking, age 70-74
    cat_multi == 21 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 7,
    cat_multi == 21 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 8,
    cat_multi == 21 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 9,
    cat_multi == 21 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 11,
    cat_multi == 21 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 13,
    
    cat_multi == 21 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 7,
    cat_multi == 21 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 8,
    cat_multi == 21 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 10,
    cat_multi == 21 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 11,
    cat_multi == 21 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 13,
    
    cat_multi == 21 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 7,
    cat_multi == 21 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 8,
    cat_multi == 21 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 10,
    cat_multi == 21 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 12,
    cat_multi == 21 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 14,
    
    cat_multi == 21 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 8,
    cat_multi == 21 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 9,
    cat_multi == 21 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 10,
    cat_multi == 21 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 12,
    cat_multi == 21 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 14,
    
    cat_multi == 21 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 8,
    cat_multi == 21 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 9,
    cat_multi == 21 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 11,
    cat_multi == 21 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 13,
    cat_multi == 21 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 15,
    
    # no DM, female, with smoking, age 40-44
    cat_multi == 22 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 2,
    cat_multi == 22 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 3,
    cat_multi == 22 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 4,
    cat_multi == 22 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 5,
    cat_multi == 22 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 6,
    
    cat_multi == 22 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 2,
    cat_multi == 22 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 3,
    cat_multi == 22 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 4,
    cat_multi == 22 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 5,
    cat_multi == 22 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 7,
    
    cat_multi == 22 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 2,
    cat_multi == 22 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 3,
    cat_multi == 22 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 4,
    cat_multi == 22 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 6,
    cat_multi == 22 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 7,
    
    cat_multi == 22 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 3,
    cat_multi == 22 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 4,
    cat_multi == 22 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 5,
    cat_multi == 22 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 6,
    cat_multi == 22 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 8,
    
    cat_multi == 22 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 3,
    cat_multi == 22 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 4,
    cat_multi == 22 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 5,
    cat_multi == 22 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 7,
    cat_multi == 22 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 9,
    
    # no DM, female, with smoking, age 45-49
    cat_multi == 23 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 3,
    cat_multi == 23 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 3,
    cat_multi == 23 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 4,
    cat_multi == 23 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 6,
    cat_multi == 23 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 7,
    
    cat_multi == 23 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 3,
    cat_multi == 23 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 4,
    cat_multi == 23 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 5,
    cat_multi == 23 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 6,
    cat_multi == 23 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 8,
    
    cat_multi == 23 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 3,
    cat_multi == 23 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 4,
    cat_multi == 23 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 5,
    cat_multi == 23 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 7,
    cat_multi == 23 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 8,
    
    cat_multi == 23 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 3,
    cat_multi == 23 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 4,
    cat_multi == 23 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 6,
    cat_multi == 23 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 7,
    cat_multi == 23 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 9,
    
    cat_multi == 23 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 4,
    cat_multi == 23 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 5,
    cat_multi == 23 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 6,
    cat_multi == 23 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 8,
    cat_multi == 23 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 10,
    
    # no DM, female, with smoking, age 50-54
    cat_multi == 24 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 3,
    cat_multi == 24 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 4,
    cat_multi == 24 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 5,
    cat_multi == 24 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 7,
    cat_multi == 24 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 9,
    
    cat_multi == 24 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 4,
    cat_multi == 24 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 5,
    cat_multi == 24 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 6,
    cat_multi == 24 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 7,
    cat_multi == 24 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 9,
    
    cat_multi == 24 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 4,
    cat_multi == 24 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 5,
    cat_multi == 24 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 6,
    cat_multi == 24 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 8,
    cat_multi == 24 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 10,
    
    cat_multi == 24 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 4,
    cat_multi == 24 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 5,
    cat_multi == 24 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 7,
    cat_multi == 24 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 9,
    cat_multi == 24 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 11,
    
    cat_multi == 24 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 5,
    cat_multi == 24 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 6,
    cat_multi == 24 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 7,
    cat_multi == 24 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 9,
    cat_multi == 24 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 12,
    
    # no DM, female, with smoking, age 55-59
    cat_multi == 25 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 4,
    cat_multi == 25 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 5,
    cat_multi == 25 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 7,
    cat_multi == 25 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 8,
    cat_multi == 25 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 10,
    
    cat_multi == 25 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 5,
    cat_multi == 25 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 6,
    cat_multi == 25 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 7,
    cat_multi == 25 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 9,
    cat_multi == 25 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 11,
    
    cat_multi == 25 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 5,
    cat_multi == 25 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 6,
    cat_multi == 25 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 8,
    cat_multi == 25 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 9,
    cat_multi == 25 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 12,
    
    cat_multi == 25 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 5,
    cat_multi == 25 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 7,
    cat_multi == 25 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 8,
    cat_multi == 25 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 10,
    cat_multi == 25 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 12,
    
    cat_multi == 25 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 6,
    cat_multi == 25 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 7,
    cat_multi == 25 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 9,
    cat_multi == 25 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 11,
    cat_multi == 25 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 13,
    
    # no DM, female, with smoking, age 60-64
    cat_multi == 26 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 6,
    cat_multi == 26 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 7,
    cat_multi == 26 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 8,
    cat_multi == 26 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 10,
    cat_multi == 26 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 12,
    
    cat_multi == 26 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 6,
    cat_multi == 26 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 7,
    cat_multi == 26 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 9,
    cat_multi == 26 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 11,
    cat_multi == 26 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 13,
    
    cat_multi == 26 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 6,
    cat_multi == 26 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 8,
    cat_multi == 26 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 9,
    cat_multi == 26 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 11,
    cat_multi == 26 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 14,
    
    cat_multi == 26 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 7,
    cat_multi == 26 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 8,
    cat_multi == 26 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 10,
    cat_multi == 26 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 12,
    cat_multi == 26 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 14,
    
    cat_multi == 26 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 7,
    cat_multi == 26 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 9,
    cat_multi == 26 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 11,
    cat_multi == 26 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 13,
    cat_multi == 26 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 15,
    
    # no DM, female, with smoking, age 65-69
    cat_multi == 27 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 7,
    cat_multi == 27 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 9,
    cat_multi == 27 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 10,
    cat_multi == 27 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 12,
    cat_multi == 27 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 14,
    
    cat_multi == 27 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 8,
    cat_multi == 27 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 9,
    cat_multi == 27 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 11,
    cat_multi == 27 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 13,
    cat_multi == 27 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 15,
    
    cat_multi == 27 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 8,
    cat_multi == 27 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 9,
    cat_multi == 27 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 11,
    cat_multi == 27 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 13,
    cat_multi == 27 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 16,
    
    cat_multi == 27 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 8,
    cat_multi == 27 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 10,
    cat_multi == 27 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 12,
    cat_multi == 27 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 14,
    cat_multi == 27 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 17,
    
    cat_multi == 27 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 9,
    cat_multi == 27 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 11,
    cat_multi == 27 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 13,
    cat_multi == 27 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 15,
    cat_multi == 27 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 18,
    
    # no DM, female, with smoking, age 70-74
    cat_multi == 28 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 9,
    cat_multi == 28 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 11,
    cat_multi == 28 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 13,
    cat_multi == 28 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 15,
    cat_multi == 28 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 17,
    
    cat_multi == 28 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 10,
    cat_multi == 28 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 11,
    cat_multi == 28 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 13,
    cat_multi == 28 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 15,
    cat_multi == 28 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 18,
    
    cat_multi == 28 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 10,
    cat_multi == 28 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 12,
    cat_multi == 28 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 14,
    cat_multi == 28 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 16,
    cat_multi == 28 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 19,
    
    cat_multi == 28 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 10,
    cat_multi == 28 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 12,
    cat_multi == 28 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 14,
    cat_multi == 28 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 17,
    cat_multi == 28 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 19,
    
    cat_multi == 28 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 11,
    cat_multi == 28 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 13,
    cat_multi == 28 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 15,
    cat_multi == 28 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 17,
    cat_multi == 28 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 20,
    
    # with DM, male, no smoking, age 40-44
    cat_multi == 29 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 3,
    cat_multi == 29 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 4,
    cat_multi == 29 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 5,
    cat_multi == 29 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 7,
    cat_multi == 29 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 9,
    
    cat_multi == 29 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 3,
    cat_multi == 29 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 4,
    cat_multi == 29 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 5,
    cat_multi == 29 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 7,
    cat_multi == 29 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 10,
    
    cat_multi == 29 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 3,
    cat_multi == 29 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 4,
    cat_multi == 29 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 6,
    cat_multi == 29 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 8,
    cat_multi == 29 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 11,
    
    cat_multi == 29 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 4,
    cat_multi == 29 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 5,
    cat_multi == 29 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 7,
    cat_multi == 29 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 9,
    cat_multi == 29 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 12,
    
    cat_multi == 29 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 4,
    cat_multi == 29 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 6,
    cat_multi == 29 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 8,
    cat_multi == 29 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 11,
    cat_multi == 29 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 14,
    
    # with DM, male, no smoking, age 45-49
    cat_multi == 30 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 3,
    cat_multi == 30 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 4,
    cat_multi == 30 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 6,
    cat_multi == 30 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 8,
    cat_multi == 30 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 11,
    
    cat_multi == 30 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 4,
    cat_multi == 30 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 5,
    cat_multi == 30 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 7,
    cat_multi == 30 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 9,
    cat_multi == 30 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 12,
    
    cat_multi == 30 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 4,
    cat_multi == 30 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 5,
    cat_multi == 30 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 7,
    cat_multi == 30 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 10,
    cat_multi == 30 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 13,
    
    cat_multi == 30 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 5,
    cat_multi == 30 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 6,
    cat_multi == 30 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 8,
    cat_multi == 30 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 11,
    cat_multi == 30 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 14,
    
    cat_multi == 30 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 5,
    cat_multi == 30 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 7,
    cat_multi == 30 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 9,
    cat_multi == 30 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 12,
    cat_multi == 30 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 16,
    
    # with DM, male, no smoking, age 50-54
    cat_multi == 31 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 4,
    cat_multi == 31 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 6,
    cat_multi == 31 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 7,
    cat_multi == 31 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 10,
    cat_multi == 31 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 13,
    
    cat_multi == 31 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 5,
    cat_multi == 31 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 6,
    cat_multi == 31 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 8,
    cat_multi == 31 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 11,
    cat_multi == 31 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 14,
    
    cat_multi == 31 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 5,
    cat_multi == 31 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 7,
    cat_multi == 31 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 9,
    cat_multi == 31 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 12,
    cat_multi == 31 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 15,
    
    cat_multi == 31 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 6,
    cat_multi == 31 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 8,
    cat_multi == 31 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 10,
    cat_multi == 31 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 13,
    cat_multi == 31 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 17,
    
    cat_multi == 31 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 7,
    cat_multi == 31 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 9,
    cat_multi == 31 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 11,
    cat_multi == 31 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 15,
    cat_multi == 31 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 19,
    
    # with DM, male, no smoking, age 55-59
    cat_multi == 32 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 5,
    cat_multi == 32 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 7,
    cat_multi == 32 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 9,
    cat_multi == 32 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 12,
    cat_multi == 32 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 15,
    
    cat_multi == 32 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 6,
    cat_multi == 32 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 8,
    cat_multi == 32 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 10,
    cat_multi == 32 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 13,
    cat_multi == 32 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 16,
    
    cat_multi == 32 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 7,
    cat_multi == 32 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 9,
    cat_multi == 32 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 11,
    cat_multi == 32 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 14,
    cat_multi == 32 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 18,
    
    cat_multi == 32 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 8,
    cat_multi == 32 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 10,
    cat_multi == 32 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 12,
    cat_multi == 32 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 15,
    cat_multi == 32 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 19,
    
    cat_multi == 32 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 9,
    cat_multi == 32 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 11,
    cat_multi == 32 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 14,
    cat_multi == 32 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 17,
    cat_multi == 32 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 22,
    
    # with DM, male, no smoking, age 60-64
    cat_multi == 33 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 7,
    cat_multi == 33 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 9,
    cat_multi == 33 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 11,
    cat_multi == 33 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 14,
    cat_multi == 33 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 17,
    
    cat_multi == 33 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 8,
    cat_multi == 33 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 10,
    cat_multi == 33 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 12,
    cat_multi == 33 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 15,
    cat_multi == 33 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 19,
    
    cat_multi == 33 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 9,
    cat_multi == 33 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 11,
    cat_multi == 33 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 13,
    cat_multi == 33 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 17,
    cat_multi == 33 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 21,
    
    cat_multi == 33 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 10,
    cat_multi == 33 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 12,
    cat_multi == 33 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 15,
    cat_multi == 33 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 18,
    cat_multi == 33 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 23,
    
    cat_multi == 33 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 11,
    cat_multi == 33 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 13,
    cat_multi == 33 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 16,
    cat_multi == 33 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 20,
    cat_multi == 33 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 25,
    
    # with DM, male, no smoking, age 65-69
    cat_multi == 34 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 9,
    cat_multi == 34 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 11,
    cat_multi == 34 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 14,
    cat_multi == 34 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 17,
    cat_multi == 34 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 20,
    
    cat_multi == 34 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 10,
    cat_multi == 34 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 12,
    cat_multi == 34 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 15,
    cat_multi == 34 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 18,
    cat_multi == 34 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 22,
    
    cat_multi == 34 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 11,
    cat_multi == 34 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 13,
    cat_multi == 34 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 16,
    cat_multi == 34 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 20,
    cat_multi == 34 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 24,
    
    cat_multi == 34 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 12,
    cat_multi == 34 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 15,
    cat_multi == 34 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 18,
    cat_multi == 34 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 22,
    cat_multi == 34 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 26,
    
    cat_multi == 34 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 13,
    cat_multi == 34 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 16,
    cat_multi == 34 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 20,
    cat_multi == 34 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 24,
    cat_multi == 34 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 29,
    
    # with DM, male, no smoking, age 70-74
    cat_multi == 35 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 12,
    cat_multi == 35 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 14,
    cat_multi == 35 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 17,
    cat_multi == 35 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 20,
    cat_multi == 35 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 24,
    
    cat_multi == 35 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 13,
    cat_multi == 35 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 15,
    cat_multi == 35 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 18,
    cat_multi == 35 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 22,
    cat_multi == 35 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 26,
    
    cat_multi == 35 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 14,
    cat_multi == 35 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 16,
    cat_multi == 35 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 20,
    cat_multi == 35 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 23,
    cat_multi == 35 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 28,
    
    cat_multi == 35 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 15,
    cat_multi == 35 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 18,
    cat_multi == 35 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 21,
    cat_multi == 35 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 26,
    cat_multi == 35 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 30,
    
    cat_multi == 35 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 16,
    cat_multi == 35 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 20,
    cat_multi == 35 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 23,
    cat_multi == 35 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 28,
    cat_multi == 35 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 33,
    
    # with DM, male, with smoking, age 40-44
    cat_multi == 36 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 5,
    cat_multi == 36 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 6,
    cat_multi == 36 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 8,
    cat_multi == 36 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 11,
    cat_multi == 36 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 15,
    
    cat_multi == 36 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 5,
    cat_multi == 36 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 7,
    cat_multi == 36 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 9,
    cat_multi == 36 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 13,
    cat_multi == 36 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 17,
    
    cat_multi == 36 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 6,
    cat_multi == 36 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 8,
    cat_multi == 36 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 11,
    cat_multi == 36 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 14,
    cat_multi == 36 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 19,
    
    cat_multi == 36 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 7,
    cat_multi == 36 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 9,
    cat_multi == 36 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 12,
    cat_multi == 36 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 16,
    cat_multi == 36 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 22,
    
    cat_multi == 36 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 8,
    cat_multi == 36 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 11,
    cat_multi == 36 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 14,
    cat_multi == 36 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 19,
    cat_multi == 36 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 25,
    
    # with DM, male, with smoking, age 45-49
    cat_multi == 37 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 5,
    cat_multi == 37 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 7,
    cat_multi == 37 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 10,
    cat_multi == 37 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 13,
    cat_multi == 37 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 17,
    
    cat_multi == 37 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 6,
    cat_multi == 37 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 8,
    cat_multi == 37 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 11,
    cat_multi == 37 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 14,
    cat_multi == 37 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 19,
    
    cat_multi == 37 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 7,
    cat_multi == 37 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 9,
    cat_multi == 37 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 12,
    cat_multi == 37 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 16,
    cat_multi == 37 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 21,
    
    cat_multi == 37 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 8,
    cat_multi == 37 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 11,
    cat_multi == 37 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 14,
    cat_multi == 37 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 18,
    cat_multi == 37 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 24,
    
    cat_multi == 37 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 10,
    cat_multi == 37 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 12,
    cat_multi == 37 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 16,
    cat_multi == 37 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 21,
    cat_multi == 37 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 27,
    
    # with DM, male, with smoking, age 50-54
    cat_multi == 38 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 7,
    cat_multi == 38 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 9,
    cat_multi == 38 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 11,
    cat_multi == 38 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 15,
    cat_multi == 38 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 19,
    
    cat_multi == 38 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 7,
    cat_multi == 38 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 10,
    cat_multi == 38 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 13,
    cat_multi == 38 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 16,
    cat_multi == 38 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 21,
    
    cat_multi == 38 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 8,
    cat_multi == 38 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 11,
    cat_multi == 38 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 14,
    cat_multi == 38 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 18,
    cat_multi == 38 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 23,
    
    cat_multi == 38 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 10,
    cat_multi == 38 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 12,
    cat_multi == 38 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 16,
    cat_multi == 38 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 20,
    cat_multi == 38 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 26,
    
    cat_multi == 38 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 11,
    cat_multi == 38 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 14,
    cat_multi == 38 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 18,
    cat_multi == 38 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 23,
    cat_multi == 38 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 29,
    
    # with DM, male, with smoking, age 55-59
    cat_multi == 39 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 8,
    cat_multi == 39 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 10,
    cat_multi == 39 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 13,
    cat_multi == 39 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 17,
    cat_multi == 39 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 21,
    
    cat_multi == 39 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 9,
    cat_multi == 39 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 12,
    cat_multi == 39 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 15,
    cat_multi == 39 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 18,
    cat_multi == 39 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 23,
    
    cat_multi == 39 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 10,
    cat_multi == 39 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 13,
    cat_multi == 39 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 16,
    cat_multi == 39 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 20,
    cat_multi == 39 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 26,
    
    cat_multi == 39 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 11,
    cat_multi == 39 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 15,
    cat_multi == 39 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 18,
    cat_multi == 39 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 23,
    cat_multi == 39 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 28,
    
    cat_multi == 39 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 13,
    cat_multi == 39 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 16,
    cat_multi == 39 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 21,
    cat_multi == 39 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 26,
    cat_multi == 39 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 32,
    
    # with DM, male, with smoking, age 60-64
    cat_multi == 40 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 10,
    cat_multi == 40 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 12,
    cat_multi == 40 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 15,
    cat_multi == 40 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 19,
    cat_multi == 40 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 24,
    
    cat_multi == 40 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 11,
    cat_multi == 40 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 14,
    cat_multi == 40 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 17,
    cat_multi == 40 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 21,
    cat_multi == 40 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 26,
    
    cat_multi == 40 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 12,
    cat_multi == 40 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 15,
    cat_multi == 40 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 19,
    cat_multi == 40 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 23,
    cat_multi == 40 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 28,
    
    cat_multi == 40 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 14,
    cat_multi == 40 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 17,
    cat_multi == 40 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 21,
    cat_multi == 40 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 25,
    cat_multi == 40 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 31,
    
    cat_multi == 40 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 15,
    cat_multi == 40 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 19,
    cat_multi == 40 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 23,
    cat_multi == 40 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 28,
    cat_multi == 40 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 34,
    
    # with DM, male, with smoking, age 65-69
    cat_multi == 41 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 12,
    cat_multi == 41 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 15,
    cat_multi == 41 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 18,
    cat_multi == 41 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 22,
    cat_multi == 41 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 26,
    
    cat_multi == 41 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 13,
    cat_multi == 41 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 16,
    cat_multi == 41 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 20,
    cat_multi == 41 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 24,
    cat_multi == 41 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 29,
    
    cat_multi == 41 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 14,
    cat_multi == 41 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 18,
    cat_multi == 41 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 21,
    cat_multi == 41 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 26,
    cat_multi == 41 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 31,
    
    cat_multi == 41 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 16,
    cat_multi == 41 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 19,
    cat_multi == 41 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 24,
    cat_multi == 41 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 28,
    cat_multi == 41 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 34,
    
    cat_multi == 41 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 18,
    cat_multi == 41 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 22,
    cat_multi == 41 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 26,
    cat_multi == 41 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 31,
    cat_multi == 41 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 37,
    
    # with DM, male, with smoking, age 70-74
    cat_multi == 42 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 14,
    cat_multi == 42 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 17,
    cat_multi == 42 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 21,
    cat_multi == 42 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 25,
    cat_multi == 42 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 29,
    
    cat_multi == 42 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 16,
    cat_multi == 42 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 19,
    cat_multi == 42 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 22,
    cat_multi == 42 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 27,
    cat_multi == 42 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 32,
    
    cat_multi == 42 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 17,
    cat_multi == 42 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 21,
    cat_multi == 42 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 24,
    cat_multi == 42 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 29,
    cat_multi == 42 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 34,
    
    cat_multi == 42 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 19,
    cat_multi == 42 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 22,
    cat_multi == 42 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 27,
    cat_multi == 42 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 31,
    cat_multi == 42 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 37,
    
    cat_multi == 42 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 21,
    cat_multi == 42 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 25,
    cat_multi == 42 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 29,
    cat_multi == 42 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 34,
    cat_multi == 42 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 40,
    
    # with DM, female, no smoking, age 40-44
    cat_multi == 43 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 2,
    cat_multi == 43 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 3,
    cat_multi == 43 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 4,
    cat_multi == 43 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 5,
    cat_multi == 43 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 6,
    
    cat_multi == 43 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 2,
    cat_multi == 43 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 3,
    cat_multi == 43 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 4,
    cat_multi == 43 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 5,
    cat_multi == 43 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 7,
    
    cat_multi == 43 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 2,
    cat_multi == 43 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 3,
    cat_multi == 43 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 4,
    cat_multi == 43 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 6,
    cat_multi == 43 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 7,
    
    cat_multi == 43 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 3,
    cat_multi == 43 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 4,
    cat_multi == 43 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 5,
    cat_multi == 43 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 6,
    cat_multi == 43 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 8,
    
    cat_multi == 43 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 3,
    cat_multi == 43 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 4,
    cat_multi == 43 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 5,
    cat_multi == 43 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 7,
    cat_multi == 43 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 9,
    
    # with DM, female, no smoking, age 45-49
    cat_multi == 44 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 3,
    cat_multi == 44 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 3,
    cat_multi == 44 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 4,
    cat_multi == 44 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 6,
    cat_multi == 44 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 7,
    
    cat_multi == 44 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 3,
    cat_multi == 44 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 4,
    cat_multi == 44 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 5,
    cat_multi == 44 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 6,
    cat_multi == 44 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 8,
    
    cat_multi == 44 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 3,
    cat_multi == 44 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 4,
    cat_multi == 44 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 5,
    cat_multi == 44 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 7,
    cat_multi == 44 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 9,
    
    cat_multi == 44 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 3,
    cat_multi == 44 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 4,
    cat_multi == 44 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 6,
    cat_multi == 44 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 7,
    cat_multi == 44 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 9,
    
    cat_multi == 44 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 4,
    cat_multi == 44 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 5,
    cat_multi == 44 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 6,
    cat_multi == 44 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 8,
    cat_multi == 44 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 10,
    
    # with DM, female, no smoking, age 50-54
    cat_multi == 45 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 3,
    cat_multi == 45 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 4,
    cat_multi == 45 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 6,
    cat_multi == 45 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 7,
    cat_multi == 45 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 9,
    
    cat_multi == 45 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 4,
    cat_multi == 45 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 5,
    cat_multi == 45 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 6,
    cat_multi == 45 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 7,
    cat_multi == 45 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 9,
    
    cat_multi == 45 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 4,
    cat_multi == 45 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 5,
    cat_multi == 45 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 6,
    cat_multi == 45 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 8,
    cat_multi == 45 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 10,
    
    cat_multi == 45 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 4,
    cat_multi == 45 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 5,
    cat_multi == 45 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 7,
    cat_multi == 45 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 9,
    cat_multi == 45 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 11,
    
    cat_multi == 45 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 5,
    cat_multi == 45 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 6,
    cat_multi == 45 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 8,
    cat_multi == 45 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 9,
    cat_multi == 45 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 12,
    
    # with DM, female, no smoking, age 55-59
    cat_multi == 46 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 4,
    cat_multi == 46 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 6,
    cat_multi == 46 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 7,
    cat_multi == 46 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 8,
    cat_multi == 46 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 10,
    
    cat_multi == 46 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 5,
    cat_multi == 46 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 6,
    cat_multi == 46 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 7,
    cat_multi == 46 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 9,
    cat_multi == 46 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 11,
    
    cat_multi == 46 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 5,
    cat_multi == 46 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 6,
    cat_multi == 46 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 8,
    cat_multi == 46 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 10,
    cat_multi == 46 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 12,
    
    cat_multi == 46 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 6,
    cat_multi == 46 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 7,
    cat_multi == 46 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 8,
    cat_multi == 46 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 10,
    cat_multi == 46 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 13,
    
    cat_multi == 46 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 6,
    cat_multi == 46 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 7,
    cat_multi == 46 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 9,
    cat_multi == 46 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 11,
    cat_multi == 46 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 14,
    
    # with DM, female, no smoking, age 60-64
    cat_multi == 47 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 6,
    cat_multi == 47 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 7,
    cat_multi == 47 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 9,
    cat_multi == 47 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 10,
    cat_multi == 47 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 13,
    
    cat_multi == 47 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 6,
    cat_multi == 47 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 7,
    cat_multi == 47 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 9,
    cat_multi == 47 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 11,
    cat_multi == 47 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 13,
    
    cat_multi == 47 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 7,
    cat_multi == 47 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 8,
    cat_multi == 47 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 10,
    cat_multi == 47 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 12,
    cat_multi == 47 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 14,
    
    cat_multi == 47 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 7,
    cat_multi == 47 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 8,
    cat_multi == 47 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 10,
    cat_multi == 47 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 12,
    cat_multi == 47 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 15,
    
    cat_multi == 47 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 7,
    cat_multi == 47 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 9,
    cat_multi == 47 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 11,
    cat_multi == 47 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 13,
    cat_multi == 47 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 16,
    
    # with DM, female, no smoking, age 65-69
    cat_multi == 48 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 8,
    cat_multi == 48 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 9,
    cat_multi == 48 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 11,
    cat_multi == 48 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 13,
    cat_multi == 48 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 15,
    
    cat_multi == 48 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 8,
    cat_multi == 48 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 9,
    cat_multi == 48 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 11,
    cat_multi == 48 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 13,
    cat_multi == 48 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 16,
    
    cat_multi == 48 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 8,
    cat_multi == 48 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 10,
    cat_multi == 48 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 12,
    cat_multi == 48 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 14,
    cat_multi == 48 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 17,
    
    cat_multi == 48 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 9,
    cat_multi == 48 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 11,
    cat_multi == 48 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 13,
    cat_multi == 48 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 15,
    cat_multi == 48 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 18,
    
    cat_multi == 48 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 9,
    cat_multi == 48 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 11,
    cat_multi == 48 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 13,
    cat_multi == 48 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 16,
    cat_multi == 48 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 19,
    
    # with DM, female, no smoking, age 70-74
    cat_multi == 49 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 10,
    cat_multi == 49 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 11,
    cat_multi == 49 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 13,
    cat_multi == 49 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 16,
    cat_multi == 49 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 18,
    
    cat_multi == 49 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 10,
    cat_multi == 49 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 12,
    cat_multi == 49 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 14,
    cat_multi == 49 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 16,
    cat_multi == 49 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 19,
    
    cat_multi == 49 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 11,
    cat_multi == 49 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 12,
    cat_multi == 49 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 15,
    cat_multi == 49 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 17,
    cat_multi == 49 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 20,
    
    cat_multi == 49 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 11,
    cat_multi == 49 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 13,
    cat_multi == 49 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 15,
    cat_multi == 49 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 18,
    cat_multi == 49 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 21,
    
    cat_multi == 49 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 12,
    cat_multi == 49 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 14,
    cat_multi == 49 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 16,
    cat_multi == 49 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 19,
    cat_multi == 49 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 22,
    
    # with DM, female, with smoking, age 40-44
    cat_multi == 50 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 4,
    cat_multi == 50 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 6,
    cat_multi == 50 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 7,
    cat_multi == 50 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 10,
    cat_multi == 50 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 13,
    
    cat_multi == 50 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 5,
    cat_multi == 50 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 6,
    cat_multi == 50 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 8,
    cat_multi == 50 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 11,
    cat_multi == 50 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 14,
    
    cat_multi == 50 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 5,
    cat_multi == 50 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 7,
    cat_multi == 50 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 9,
    cat_multi == 50 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 12,
    cat_multi == 50 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 15,
    
    cat_multi == 50 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 6,
    cat_multi == 50 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 8,
    cat_multi == 50 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 10,
    cat_multi == 50 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 13,
    cat_multi == 50 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 17,
    
    cat_multi == 50 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 7,
    cat_multi == 50 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 9,
    cat_multi == 50 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 12,
    cat_multi == 50 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 15,
    cat_multi == 50 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 19,
    
    # with DM, female, with smoking, age 45-49
    cat_multi == 51 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 5,
    cat_multi == 51 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 7,
    cat_multi == 51 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 9,
    cat_multi == 51 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 11,
    cat_multi == 51 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 14,
    
    cat_multi == 51 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 6,
    cat_multi == 51 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 7,
    cat_multi == 51 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 9,
    cat_multi == 51 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 12,
    cat_multi == 51 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 15,
    
    cat_multi == 51 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 6,
    cat_multi == 51 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 8,
    cat_multi == 51 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 10,
    cat_multi == 51 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 13,
    cat_multi == 51 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 17,
    
    cat_multi == 51 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 7,
    cat_multi == 51 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 9,
    cat_multi == 51 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 12,
    cat_multi == 51 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 15,
    cat_multi == 51 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 19,
    
    cat_multi == 51 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 8,
    cat_multi == 51 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 10,
    cat_multi == 51 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 13,
    cat_multi == 51 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 16,
    cat_multi == 51 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 21,
    
    # with DM, female, with smoking, age 50-54
    cat_multi == 52 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 6,
    cat_multi == 52 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 8,
    cat_multi == 52 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 10,
    cat_multi == 52 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 13,
    cat_multi == 52 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 16,
    
    cat_multi == 52 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 7,
    cat_multi == 52 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 9,
    cat_multi == 52 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 11,
    cat_multi == 52 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 14,
    cat_multi == 52 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 17,
    
    cat_multi == 52 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 8,
    cat_multi == 52 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 10,
    cat_multi == 52 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 12,
    cat_multi == 52 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 15,
    cat_multi == 52 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 18,
    
    cat_multi == 52 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 8,
    cat_multi == 52 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 10,
    cat_multi == 52 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 13,
    cat_multi == 52 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 16,
    cat_multi == 52 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 20,
    
    cat_multi == 52 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 9,
    cat_multi == 52 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 12,
    cat_multi == 52 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 14,
    cat_multi == 52 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 18,
    cat_multi == 52 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 22,
    
    # with DM, female, with smoking, age 55-59
    cat_multi == 53 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 8,
    cat_multi == 53 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 9,
    cat_multi == 53 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 12,
    cat_multi == 53 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 14,
    cat_multi == 53 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 18,
    
    cat_multi == 53 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 8,
    cat_multi == 53 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 10,
    cat_multi == 53 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 13,
    cat_multi == 53 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 15,
    cat_multi == 53 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 19,
    
    cat_multi == 53 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 9,
    cat_multi == 53 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 11,
    cat_multi == 53 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 14,
    cat_multi == 53 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 17,
    cat_multi == 53 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 20,
    
    cat_multi == 53 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 10,
    cat_multi == 53 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 12,
    cat_multi == 53 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 15,
    cat_multi == 53 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 18,
    cat_multi == 53 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 22,
    
    cat_multi == 53 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 11,
    cat_multi == 53 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 13,
    cat_multi == 53 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 16,
    cat_multi == 53 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 20,
    cat_multi == 53 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 24,
    
    # with DM, female, with smoking, age 60-64
    cat_multi == 54 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 9,
    cat_multi == 54 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 11,
    cat_multi == 54 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 14,
    cat_multi == 54 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 16,
    cat_multi == 54 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 20,
    
    cat_multi == 54 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 10,
    cat_multi == 54 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 12,
    cat_multi == 54 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 14,
    cat_multi == 54 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 17,
    cat_multi == 54 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 21,
    
    cat_multi == 54 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 11,
    cat_multi == 54 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 13,
    cat_multi == 54 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 15,
    cat_multi == 54 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 19,
    cat_multi == 54 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 22,
    
    cat_multi == 54 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 11,
    cat_multi == 54 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 14,
    cat_multi == 54 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 17,
    cat_multi == 54 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 20,
    cat_multi == 54 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 24,
    
    cat_multi == 54 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 12,
    cat_multi == 54 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 15,
    cat_multi == 54 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 18,
    cat_multi == 54 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 21,
    cat_multi == 54 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 26,
    
    # with DM, female, with smoking, age 65-69
    cat_multi == 55 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 11,
    cat_multi == 55 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 13,
    cat_multi == 55 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 16,
    cat_multi == 55 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 19,
    cat_multi == 55 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 22,
    
    cat_multi == 55 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 12,
    cat_multi == 55 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 14,
    cat_multi == 55 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 17,
    cat_multi == 55 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 20,
    cat_multi == 55 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 23,
    
    cat_multi == 55 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 12,
    cat_multi == 55 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 15,
    cat_multi == 55 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 18,
    cat_multi == 55 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 21,
    cat_multi == 55 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 24,
    
    cat_multi == 55 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 13,
    cat_multi == 55 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 16,
    cat_multi == 55 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 19,
    cat_multi == 55 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 22,
    cat_multi == 55 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 26,
    
    cat_multi == 55 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 14,
    cat_multi == 55 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 17,
    cat_multi == 55 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 20,
    cat_multi == 55 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 23,
    cat_multi == 55 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 27,
    
    # with DM, female, with smoking, age 70-74
    cat_multi == 56 & cat_tc_mmoll == 1 & cat_sbp == 1 ~ 13,
    cat_multi == 56 & cat_tc_mmoll == 1 & cat_sbp == 2 ~ 16,
    cat_multi == 56 & cat_tc_mmoll == 1 & cat_sbp == 3 ~ 18,
    cat_multi == 56 & cat_tc_mmoll == 1 & cat_sbp == 4 ~ 21,
    cat_multi == 56 & cat_tc_mmoll == 1 & cat_sbp == 5 ~ 25,
    
    cat_multi == 56 & cat_tc_mmoll == 2 & cat_sbp == 1 ~ 14,
    cat_multi == 56 & cat_tc_mmoll == 2 & cat_sbp == 2 ~ 16,
    cat_multi == 56 & cat_tc_mmoll == 2 & cat_sbp == 3 ~ 19,
    cat_multi == 56 & cat_tc_mmoll == 2 & cat_sbp == 4 ~ 22,
    cat_multi == 56 & cat_tc_mmoll == 2 & cat_sbp == 5 ~ 26,
    
    cat_multi == 56 & cat_tc_mmoll == 3 & cat_sbp == 1 ~ 15,
    cat_multi == 56 & cat_tc_mmoll == 3 & cat_sbp == 2 ~ 17,
    cat_multi == 56 & cat_tc_mmoll == 3 & cat_sbp == 3 ~ 20,
    cat_multi == 56 & cat_tc_mmoll == 3 & cat_sbp == 4 ~ 23,
    cat_multi == 56 & cat_tc_mmoll == 3 & cat_sbp == 5 ~ 27,
    
    cat_multi == 56 & cat_tc_mmoll == 4 & cat_sbp == 1 ~ 15,
    cat_multi == 56 & cat_tc_mmoll == 4 & cat_sbp == 2 ~ 18,
    cat_multi == 56 & cat_tc_mmoll == 4 & cat_sbp == 3 ~ 21,
    cat_multi == 56 & cat_tc_mmoll == 4 & cat_sbp == 4 ~ 24,
    cat_multi == 56 & cat_tc_mmoll == 4 & cat_sbp == 5 ~ 28,
    
    cat_multi == 56 & cat_tc_mmoll == 5 & cat_sbp == 1 ~ 16,
    cat_multi == 56 & cat_tc_mmoll == 5 & cat_sbp == 2 ~ 19,
    cat_multi == 56 & cat_tc_mmoll == 5 & cat_sbp == 3 ~ 22,
    cat_multi == 56 & cat_tc_mmoll == 5 & cat_sbp == 4 ~ 25,
    cat_multi == 56 & cat_tc_mmoll == 5 & cat_sbp == 5 ~ 29))

# create variable risk_percentile
base_WHO_cv_risk <- base_WHO_cv_risk %>%
  mutate(risk_percentile = case_when(
    h_sex == 1 & cat_age == 1 & WHO_cv_risk == 1 ~ "0",
    h_sex == 1 & cat_age == 1 & WHO_cv_risk == 2 ~ "24",
    h_sex == 1 & cat_age == 1 & WHO_cv_risk == 3 ~ "71",
    h_sex == 1 & cat_age == 1 & WHO_cv_risk == 4 ~ "89",
    h_sex == 1 & cat_age == 1 & WHO_cv_risk == 5 ~ "95",
    h_sex == 1 & cat_age == 1 & WHO_cv_risk == 6 ~ "98",
    h_sex == 1 & cat_age == 1 & WHO_cv_risk == 7 ~ "98",
    h_sex == 1 & cat_age == 1 & WHO_cv_risk == 8 ~ "99",
    h_sex == 1 & cat_age == 1 & WHO_cv_risk >= 9 ~ "100",
    
    h_sex == 1 & cat_age == 2 & WHO_cv_risk == 2 ~ "0",
    h_sex == 1 & cat_age == 2 & WHO_cv_risk == 3 ~ "33",
    h_sex == 1 & cat_age == 2 & WHO_cv_risk == 4 ~ "67",
    h_sex == 1 & cat_age == 2 & WHO_cv_risk == 5 ~ "83",
    h_sex == 1 & cat_age == 2 & WHO_cv_risk == 6 ~ "92",
    h_sex == 1 & cat_age == 2 & WHO_cv_risk == 7 ~ "95",
    h_sex == 1 & cat_age == 2 & WHO_cv_risk == 8 ~ "97",
    h_sex == 1 & cat_age == 2 & WHO_cv_risk == 9 ~ "98",
    h_sex == 1 & cat_age == 2 & WHO_cv_risk == 10 ~ "99",
    h_sex == 1 & cat_age == 2 & WHO_cv_risk == 11 ~ "99",
    h_sex == 1 & cat_age == 2 & WHO_cv_risk == 12 ~ "99",
    h_sex == 1 & cat_age == 2 & WHO_cv_risk >= 13 ~ "100",
    
    h_sex == 1 & cat_age == 3 & WHO_cv_risk == 2 ~ "0",
    h_sex == 1 & cat_age == 3 & WHO_cv_risk == 3 ~ "2",
    h_sex == 1 & cat_age == 3 & WHO_cv_risk == 4 ~ "28",
    h_sex == 1 & cat_age == 3 & WHO_cv_risk == 5 ~ "59",
    h_sex == 1 & cat_age == 3 & WHO_cv_risk == 6 ~ "72",
    h_sex == 1 & cat_age == 3 & WHO_cv_risk == 7 ~ "82",
    h_sex == 1 & cat_age == 3 & WHO_cv_risk == 8 ~ "88",
    h_sex == 1 & cat_age == 3 & WHO_cv_risk == 9 ~ "91",
    h_sex == 1 & cat_age == 3 & WHO_cv_risk == 10 ~ "94",
    h_sex == 1 & cat_age == 3 & WHO_cv_risk == 11 ~ "96",
    h_sex == 1 & cat_age == 3 & WHO_cv_risk == 12 ~ "97",
    h_sex == 1 & cat_age == 3 & WHO_cv_risk == 13 ~ "98",
    h_sex == 1 & cat_age == 3 & WHO_cv_risk == 14 ~ "99",
    h_sex == 1 & cat_age == 3 & WHO_cv_risk == 15 ~ "99",
    h_sex == 1 & cat_age == 3 & WHO_cv_risk == 16 ~ "99",
    h_sex == 1 & cat_age == 3 & WHO_cv_risk == 17 ~ ">=99",
    h_sex == 1 & cat_age == 3 & WHO_cv_risk >= 18 ~ "100",
    
    h_sex == 1 & cat_age == 4 & WHO_cv_risk == 3 ~ "0",
    h_sex == 1 & cat_age == 4 & WHO_cv_risk == 4 ~ "2",
    h_sex == 1 & cat_age == 4 & WHO_cv_risk == 5 ~ "22",
    h_sex == 1 & cat_age == 4 & WHO_cv_risk == 6 ~ "48",
    h_sex == 1 & cat_age == 4 & WHO_cv_risk == 7 ~ "61",
    h_sex == 1 & cat_age == 4 & WHO_cv_risk == 8 ~ "72",
    h_sex == 1 & cat_age == 4 & WHO_cv_risk == 9 ~ "82",
    h_sex == 1 & cat_age == 4 & WHO_cv_risk == 10 ~ "87",
    h_sex == 1 & cat_age == 4 & WHO_cv_risk == 11 ~ "90",
    h_sex == 1 & cat_age == 4 & WHO_cv_risk == 12 ~ "93",
    h_sex == 1 & cat_age == 4 & WHO_cv_risk == 13 ~ "95",
    h_sex == 1 & cat_age == 4 & WHO_cv_risk == 14 ~ "96",
    h_sex == 1 & cat_age == 4 & WHO_cv_risk == 15 ~ "97",
    h_sex == 1 & cat_age == 4 & WHO_cv_risk == 16 ~ "98",
    h_sex == 1 & cat_age == 4 & WHO_cv_risk == 17 ~ "98-99",
    h_sex == 1 & cat_age == 4 & WHO_cv_risk == 18 ~ "99",
    h_sex == 1 & cat_age == 4 & WHO_cv_risk == 19 ~ "99",
    h_sex == 1 & cat_age == 4 & WHO_cv_risk == 20 ~ ">=99",
    h_sex == 1 & cat_age == 4 & WHO_cv_risk >= 21 ~ "100",
    
    h_sex == 1 & cat_age == 5 & WHO_cv_risk == 5 ~ "0",
    h_sex == 1 & cat_age == 5 & WHO_cv_risk == 6 ~ "10",
    h_sex == 1 & cat_age == 5 & WHO_cv_risk == 7 ~ "32",
    h_sex == 1 & cat_age == 5 & WHO_cv_risk == 8 ~ "47",
    h_sex == 1 & cat_age == 5 & WHO_cv_risk == 9 ~ "57",
    h_sex == 1 & cat_age == 5 & WHO_cv_risk == 10 ~ "67",
    h_sex == 1 & cat_age == 5 & WHO_cv_risk == 11 ~ "75",
    h_sex == 1 & cat_age == 5 & WHO_cv_risk == 12 ~ "83",
    h_sex == 1 & cat_age == 5 & WHO_cv_risk == 13 ~ "88",
    h_sex == 1 & cat_age == 5 & WHO_cv_risk == 14 ~ "92",
    h_sex == 1 & cat_age == 5 & WHO_cv_risk == 15 ~ "94",
    h_sex == 1 & cat_age == 5 & WHO_cv_risk == 16 ~ "96",
    h_sex == 1 & cat_age == 5 & WHO_cv_risk == 17 ~ "97",
    h_sex == 1 & cat_age == 5 & WHO_cv_risk == 18 ~ "99",
    h_sex == 1 & cat_age == 5 & WHO_cv_risk == 19 ~ "99",
    h_sex == 1 & cat_age == 5 & WHO_cv_risk == 20 ~ "99",
    h_sex == 1 & cat_age == 5 & WHO_cv_risk >= 21 ~ "100",
    
    h_sex == 1 & cat_age == 6 & WHO_cv_risk == 6 ~ "0",
    h_sex == 1 & cat_age == 6 & WHO_cv_risk == 7 ~ "3",
    h_sex == 1 & cat_age == 6 & WHO_cv_risk == 8 ~ "10",
    h_sex == 1 & cat_age == 6 & WHO_cv_risk == 9 ~ "21",
    h_sex == 1 & cat_age == 6 & WHO_cv_risk == 10 ~ "45",
    h_sex == 1 & cat_age == 6 & WHO_cv_risk == 11 ~ "53",
    h_sex == 1 & cat_age == 6 & WHO_cv_risk == 12 ~ "66",
    h_sex == 1 & cat_age == 6 & WHO_cv_risk == 13 ~ "76",
    h_sex == 1 & cat_age == 6 & WHO_cv_risk == 14 ~ "82",
    h_sex == 1 & cat_age == 6 & WHO_cv_risk == 15 ~ "86",
    h_sex == 1 & cat_age == 6 & WHO_cv_risk == 16 ~ "90",
    h_sex == 1 & cat_age == 6 & WHO_cv_risk == 17 ~ "94",
    h_sex == 1 & cat_age == 6 & WHO_cv_risk == 18 ~ "95",
    h_sex == 1 & cat_age == 6 & WHO_cv_risk == 19 ~ "97",
    h_sex == 1 & cat_age == 6 & WHO_cv_risk == 20 ~ "97",
    h_sex == 1 & cat_age == 6 & WHO_cv_risk == 21 ~ "99",
    h_sex == 1 & cat_age == 6 & WHO_cv_risk == 22 ~ "99",
    h_sex == 1 & cat_age == 6 & WHO_cv_risk == 23 ~ "99",
    h_sex == 1 & cat_age == 6 & WHO_cv_risk == 24 ~ "99",
    h_sex == 1 & cat_age == 6 & WHO_cv_risk == 25 ~ ">=99",
    h_sex == 1 & cat_age == 6 & WHO_cv_risk >= 26 ~ "100",
    
    h_sex == 1 & cat_age == 7 & WHO_cv_risk == 9 ~ "0",
    h_sex == 1 & cat_age == 7 & WHO_cv_risk == 10 ~ "10",
    h_sex == 1 & cat_age == 7 & WHO_cv_risk == 11 ~ "15",
    h_sex == 1 & cat_age == 7 & WHO_cv_risk == 12 ~ "29",
    h_sex == 1 & cat_age == 7 & WHO_cv_risk == 13 ~ "40",
    h_sex == 1 & cat_age == 7 & WHO_cv_risk == 14 ~ "44",
    h_sex == 1 & cat_age == 7 & WHO_cv_risk == 15 ~ "59",
    h_sex == 1 & cat_age == 7 & WHO_cv_risk == 16 ~ "70",
    h_sex == 1 & cat_age == 7 & WHO_cv_risk == 17 ~ "75",
    h_sex == 1 & cat_age == 7 & WHO_cv_risk == 18 ~ "80",
    h_sex == 1 & cat_age == 7 & WHO_cv_risk == 19 ~ "86",
    h_sex == 1 & cat_age == 7 & WHO_cv_risk == 20 ~ "89",
    h_sex == 1 & cat_age == 7 & WHO_cv_risk == 21 ~ "93",
    h_sex == 1 & cat_age == 7 & WHO_cv_risk == 22 ~ "95",
    h_sex == 1 & cat_age == 7 & WHO_cv_risk == 23 ~ "97",
    h_sex == 1 & cat_age == 7 & WHO_cv_risk == 24 ~ "97-98",
    h_sex == 1 & cat_age == 7 & WHO_cv_risk == 25 ~ "98",
    h_sex == 1 & cat_age == 7 & WHO_cv_risk == 26 ~ "99",
    h_sex == 1 & cat_age == 7 & WHO_cv_risk == 27 ~ ">=99",
    h_sex == 1 & cat_age == 7 & WHO_cv_risk >= 28 ~ "100",
    
    h_sex == 2 & cat_age == 1 & WHO_cv_risk == 1 ~ "0",
    h_sex == 2 & cat_age == 1 & WHO_cv_risk == 2 ~ "76",
    h_sex == 2 & cat_age == 1 & WHO_cv_risk == 3 ~ "96",
    h_sex == 2 & cat_age == 1 & WHO_cv_risk == 4 ~ "98",
    h_sex == 2 & cat_age == 1 & WHO_cv_risk == 5 ~ "99",
    h_sex == 2 & cat_age == 1 & WHO_cv_risk >= 6 ~ "100",
    
    h_sex == 2 & cat_age == 2 & WHO_cv_risk == 1 ~ "0",
    h_sex == 2 & cat_age == 2 & WHO_cv_risk == 2 ~ "29",
    h_sex == 2 & cat_age == 2 & WHO_cv_risk == 3 ~ "77",
    h_sex == 2 & cat_age == 2 & WHO_cv_risk == 4 ~ "92",
    h_sex == 2 & cat_age == 2 & WHO_cv_risk == 5 ~ "97",
    h_sex == 2 & cat_age == 2 & WHO_cv_risk == 6 ~ "99",
    h_sex == 2 & cat_age == 2 & WHO_cv_risk == 7 ~ "99",
    h_sex == 2 & cat_age == 2 & WHO_cv_risk == 8 ~ "99",
    h_sex == 2 & cat_age == 2 & WHO_cv_risk >= 9 ~ "100",
    
    h_sex == 2 & cat_age == 3 & WHO_cv_risk == 2 ~ "0",
    h_sex == 2 & cat_age == 3 & WHO_cv_risk == 3 ~ "47",
    h_sex == 2 & cat_age == 3 & WHO_cv_risk == 4 ~ "73",
    h_sex == 2 & cat_age == 3 & WHO_cv_risk == 5 ~ "88",
    h_sex == 2 & cat_age == 3 & WHO_cv_risk == 6 ~ "95",
    h_sex == 2 & cat_age == 3 & WHO_cv_risk == 7 ~ "97",
    h_sex == 2 & cat_age == 3 & WHO_cv_risk == 8 ~ "98",
    h_sex == 2 & cat_age == 3 & WHO_cv_risk == 9 ~ "99",
    h_sex == 2 & cat_age == 3 & WHO_cv_risk == 10 ~ "99",
    h_sex == 2 & cat_age == 3 & WHO_cv_risk >= 11 ~ "100",
    
    h_sex == 2 & cat_age == 4 & WHO_cv_risk == 3 ~ "0",
    h_sex == 2 & cat_age == 4 & WHO_cv_risk == 4 ~ "47",
    h_sex == 2 & cat_age == 4 & WHO_cv_risk == 5 ~ "68",
    h_sex == 2 & cat_age == 4 & WHO_cv_risk == 6 ~ "80",
    h_sex == 2 & cat_age == 4 & WHO_cv_risk == 7 ~ "90",
    h_sex == 2 & cat_age == 4 & WHO_cv_risk == 8 ~ "94",
    h_sex == 2 & cat_age == 4 & WHO_cv_risk == 9 ~ "96",
    h_sex == 2 & cat_age == 4 & WHO_cv_risk == 10 ~ "97",
    h_sex == 2 & cat_age == 4 & WHO_cv_risk == 11 ~ "99",
    h_sex == 2 & cat_age == 4 & WHO_cv_risk == 12 ~ "99",
    h_sex == 2 & cat_age == 4 & WHO_cv_risk >= 13 ~ "100",
    
    h_sex == 2 & cat_age == 5 & WHO_cv_risk == 4 ~ "0",
    h_sex == 2 & cat_age == 5 & WHO_cv_risk == 5 ~ "35",
    h_sex == 2 & cat_age == 5 & WHO_cv_risk == 6 ~ "63",
    h_sex == 2 & cat_age == 5 & WHO_cv_risk == 7 ~ "76",
    h_sex == 2 & cat_age == 5 & WHO_cv_risk == 8 ~ "84",
    h_sex == 2 & cat_age == 5 & WHO_cv_risk == 9 ~ "91",
    h_sex == 2 & cat_age == 5 & WHO_cv_risk == 10 ~ "93",
    h_sex == 2 & cat_age == 5 & WHO_cv_risk == 11 ~ "96",
    h_sex == 2 & cat_age == 5 & WHO_cv_risk == 12 ~ "97",
    h_sex == 2 & cat_age == 5 & WHO_cv_risk == 13 ~ "98",
    h_sex == 2 & cat_age == 5 & WHO_cv_risk == 14 ~ "99",
    h_sex == 2 & cat_age == 5 & WHO_cv_risk == 15 ~ "99",
    h_sex == 2 & cat_age == 5 & WHO_cv_risk == 16 ~ ">=99",
    h_sex == 2 & cat_age == 5 & WHO_cv_risk >= 17 ~ "100",
    
    h_sex == 2 & cat_age == 6 & WHO_cv_risk == 5 ~ "0",
    h_sex == 2 & cat_age == 6 & WHO_cv_risk == 6 ~ "18",
    h_sex == 2 & cat_age == 6 & WHO_cv_risk == 7 ~ "45",
    h_sex == 2 & cat_age == 6 & WHO_cv_risk == 8 ~ "57",
    h_sex == 2 & cat_age == 6 & WHO_cv_risk == 9 ~ "70",
    h_sex == 2 & cat_age == 6 & WHO_cv_risk == 10 ~ "81",
    h_sex == 2 & cat_age == 6 & WHO_cv_risk == 11 ~ "86",
    h_sex == 2 & cat_age == 6 & WHO_cv_risk == 12 ~ "92",
    h_sex == 2 & cat_age == 6 & WHO_cv_risk == 13 ~ "96",
    h_sex == 2 & cat_age == 6 & WHO_cv_risk == 14 ~ "98",
    h_sex == 2 & cat_age == 6 & WHO_cv_risk == 15 ~ "98",
    h_sex == 2 & cat_age == 6 & WHO_cv_risk == 16 ~ "99",
    h_sex == 2 & cat_age == 6 & WHO_cv_risk >= 17 ~ "100",
    
    h_sex == 2 & cat_age == 7 & WHO_cv_risk == 7 ~ "0",
    h_sex == 2 & cat_age == 7 & WHO_cv_risk == 8 ~ "14",
    h_sex == 2 & cat_age == 7 & WHO_cv_risk == 9 ~ "39",
    h_sex == 2 & cat_age == 7 & WHO_cv_risk == 10 ~ "48",
    h_sex == 2 & cat_age == 7 & WHO_cv_risk == 11 ~ "63",
    h_sex == 2 & cat_age == 7 & WHO_cv_risk == 12 ~ "71",
    h_sex == 2 & cat_age == 7 & WHO_cv_risk == 13 ~ "85",
    h_sex == 2 & cat_age == 7 & WHO_cv_risk == 14 ~ "89",
    h_sex == 2 & cat_age == 7 & WHO_cv_risk == 15 ~ "93",
    h_sex == 2 & cat_age == 7 & WHO_cv_risk == 16 ~ "97",
    h_sex == 2 & cat_age == 7 & WHO_cv_risk == 17 ~ "97-99",
    h_sex == 2 & cat_age == 7 & WHO_cv_risk == 18 ~ "97-99",
    h_sex == 2 & cat_age == 7 & WHO_cv_risk == 19 ~ "99",
    h_sex == 2 & cat_age == 7 & WHO_cv_risk == 20 ~ ">=99",
    h_sex == 2 & cat_age == 7 & WHO_cv_risk == 21 ~ ">=99",
    h_sex == 2 & cat_age == 7 & WHO_cv_risk >= 22 ~ "100"
  )
  )

#### variables "WHO_cv_risk" and "risk_percentile" created in the database "base_WHO_cv_risk" ####