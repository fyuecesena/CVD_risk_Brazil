# CALCULATION OF CARDIOVASCULAR DISEASE (CVD) RISK AND
# PERCENTILES OF RISK DISTRIBUTION IN THE ELSA-BRASIL POPULATION 

# Calculation of 10-year cardiovascular disease* risk
# according to the 2019 WHO CVD Risk Chart
# calibrated for "Tropical Latin America" (Brazil + Paraguay)
# * first non-fatal or fatal coronary artery disease or stroke event
# applies to individuals between 40 and 74 years of age without CVD
# reference: Lancet Glob Health. 2019 Oct;7(10):e1332-e1345
# doi: 10.1016/S2214-109X(19)30318-3

# Calculation of long-term atherosclerotic CVD (ASCVD)** risk
# according to the Multinational Cardiovascular Risk Consortium tool
# ** first non-fatal or fatal coronary heart disease or ischemic stroke event
# applies to individuals between 35 and 70 years of age without coronary artery disease or stroke
# reference: Lancet. 2019 Dec 14;394(10215):2173-2183
# doi: 10.1016/S0140-6736(19)32519-X

# library
library(dplyr)

#### PREPARING DATA FRAME ####
# import data frame and rename it as "cvd_df1" 
cvd_df1 <- original_df # enter the original data frame name

# create variable "cvd_smoking" = current smoking: 0 = No; 1 = Yes
cvd_df1 <- cvd_df1 %>%
  mutate(cvd_smoking = case_when(
    # enter variable "smoking" from the data frame
    # in this example: 0 = never smoked; 1 = former smoker; 2 = current smoker
    smoking == 0  | smoking == 1 ~ 0,
    smoking == 2 ~ 1
  ))

# rename data frame variables 
cvd_df1 <- cvd_df1 %>%
  rename (cvd_age = age, # enter variable "age"
          cvd_sex = sex, # enter variable "sex": 1 = Male; 2 = Female
          cvd_sbp = sbp, # enter variable "systolic blood pressure" (mmHg)
          cvd_tc = tc, # enter variable "total cholesterol" (mg/dL)
          cvd_hdl = hdl, # enter variable "HDL-c cholesterol" (mg/dL)
          cvd_bmi = bmi, # enter variable "body mass index" (kg/m2)
          cvd_hypert = hypert, # enter variable "hypertension": 0 = No; 1 = Yes
          cvd_dm = dm) # enter variable "diabetes mellitus": 0 = No; 1 = Yes

#### RUN ALL THE CODE BELOW ####
cvdrisk <- function(df) {
  ### WHO CVD LABORATORY-BASED RISK ####
  # creating variable "cvd_tc_mmoll" = total cholesterol in mmol/L
  df$cvd_tc_mmoll = df$cvd_tc * 0.02586
  
  # creating "cvd_tc_mmoll" categories
  df <- df %>%
    mutate(cvd_tc_mmoll_cat = case_when(cvd_tc_mmoll < 4 ~ 1,
                                        cvd_tc_mmoll >= 4 & cvd_tc_mmoll < 5 ~ 2,
                                        cvd_tc_mmoll >= 5 & cvd_tc_mmoll < 6 ~ 3,
                                        cvd_tc_mmoll >= 6 & cvd_tc_mmoll < 7 ~ 4,
                                        cvd_tc_mmoll >= 7 ~ 5))
  
  # creating "cvd_sbp" categories
  df <- df %>%
    mutate(cvd_sbp_cat = case_when(cvd_sbp < 120 ~ 1,
                                   cvd_sbp >= 120 & cvd_sbp < 140 ~ 2,
                                   cvd_sbp >= 140 & cvd_sbp < 160 ~ 3,
                                   cvd_sbp >= 160 & cvd_sbp < 180 ~ 4,
                                   cvd_sbp >= 180 ~ 5))
  
  # creating age categories
  df <- df %>%
    mutate(cvd_age_cat = case_when(cvd_age >= 40 & cvd_age < 45 ~ 1,
                                   cvd_age >= 45 & cvd_age < 50 ~ 2,
                                   cvd_age >= 50 & cvd_age < 55 ~ 3,
                                   cvd_age >= 55 & cvd_age < 60 ~ 4,
                                   cvd_age >= 60 & cvd_age < 65 ~ 5,
                                   cvd_age >= 65 & cvd_age < 70 ~ 6,
                                   cvd_age >= 70 & cvd_age < 75 ~ 7))
  
  # creating categories according to sex, age, DM, smoking
  df <- df %>%
    mutate(cat_multi = case_when(
      # no DM, male, no smoking
      cvd_dm == 0 & cvd_sex == 1 & cvd_smoking == 0 & cvd_age_cat == 1 ~ 1,
      cvd_dm == 0 & cvd_sex == 1 & cvd_smoking == 0 & cvd_age_cat == 2 ~ 2,
      cvd_dm == 0 & cvd_sex == 1 & cvd_smoking == 0 & cvd_age_cat == 3 ~ 3,
      cvd_dm == 0 & cvd_sex == 1 & cvd_smoking == 0 & cvd_age_cat == 4 ~ 4,
      cvd_dm == 0 & cvd_sex == 1 & cvd_smoking == 0 & cvd_age_cat == 5 ~ 5,
      cvd_dm == 0 & cvd_sex == 1 & cvd_smoking == 0 & cvd_age_cat == 6 ~ 6,
      cvd_dm == 0 & cvd_sex == 1 & cvd_smoking == 0 & cvd_age_cat == 7 ~ 7,
      
      # no DM, male, with smoking
      cvd_dm == 0 & cvd_sex == 1 & cvd_smoking == 1 & cvd_age_cat == 1 ~ 8,
      cvd_dm == 0 & cvd_sex == 1 & cvd_smoking == 1 & cvd_age_cat == 2 ~ 9,
      cvd_dm == 0 & cvd_sex == 1 & cvd_smoking == 1 & cvd_age_cat == 3 ~ 10,
      cvd_dm == 0 & cvd_sex == 1 & cvd_smoking == 1 & cvd_age_cat == 4 ~ 11,
      cvd_dm == 0 & cvd_sex == 1 & cvd_smoking == 1 & cvd_age_cat == 5 ~ 12,
      cvd_dm == 0 & cvd_sex == 1 & cvd_smoking == 1 & cvd_age_cat == 6 ~ 13,
      cvd_dm == 0 & cvd_sex == 1 & cvd_smoking == 1 & cvd_age_cat == 7 ~ 14,
      
      # no DM, female, no smoking
      cvd_dm == 0 & cvd_sex == 2 & cvd_smoking == 0 & cvd_age_cat == 1 ~ 15,
      cvd_dm == 0 & cvd_sex == 2 & cvd_smoking == 0 & cvd_age_cat == 2 ~ 16,
      cvd_dm == 0 & cvd_sex == 2 & cvd_smoking == 0 & cvd_age_cat == 3 ~ 17,
      cvd_dm == 0 & cvd_sex == 2 & cvd_smoking == 0 & cvd_age_cat == 4 ~ 18,
      cvd_dm == 0 & cvd_sex == 2 & cvd_smoking == 0 & cvd_age_cat == 5 ~ 19,
      cvd_dm == 0 & cvd_sex == 2 & cvd_smoking == 0 & cvd_age_cat == 6 ~ 20,
      cvd_dm == 0 & cvd_sex == 2 & cvd_smoking == 0 & cvd_age_cat == 7 ~ 21,
      
      # no DM, female, with smoking
      cvd_dm == 0 & cvd_sex == 2 & cvd_smoking == 1 & cvd_age_cat == 1 ~ 22,
      cvd_dm == 0 & cvd_sex == 2 & cvd_smoking == 1 & cvd_age_cat == 2 ~ 23,
      cvd_dm == 0 & cvd_sex == 2 & cvd_smoking == 1 & cvd_age_cat == 3 ~ 24,
      cvd_dm == 0 & cvd_sex == 2 & cvd_smoking == 1 & cvd_age_cat == 4 ~ 25,
      cvd_dm == 0 & cvd_sex == 2 & cvd_smoking == 1 & cvd_age_cat == 5 ~ 26,
      cvd_dm == 0 & cvd_sex == 2 & cvd_smoking == 1 & cvd_age_cat == 6 ~ 27,
      cvd_dm == 0 & cvd_sex == 2 & cvd_smoking == 1 & cvd_age_cat == 7 ~ 28,
      
      # with DM, male, no smoking
      cvd_dm == 1 & cvd_sex == 1 & cvd_smoking == 0 & cvd_age_cat == 1 ~ 29,
      cvd_dm == 1 & cvd_sex == 1 & cvd_smoking == 0 & cvd_age_cat == 2 ~ 30,
      cvd_dm == 1 & cvd_sex == 1 & cvd_smoking == 0 & cvd_age_cat == 3 ~ 31,
      cvd_dm == 1 & cvd_sex == 1 & cvd_smoking == 0 & cvd_age_cat == 4 ~ 32,
      cvd_dm == 1 & cvd_sex == 1 & cvd_smoking == 0 & cvd_age_cat == 5 ~ 33,
      cvd_dm == 1 & cvd_sex == 1 & cvd_smoking == 0 & cvd_age_cat == 6 ~ 34,
      cvd_dm == 1 & cvd_sex == 1 & cvd_smoking == 0 & cvd_age_cat == 7 ~ 35,
      
      # with DM, male, with smoking
      cvd_dm == 1 & cvd_sex == 1 & cvd_smoking == 1 & cvd_age_cat == 1 ~ 36,
      cvd_dm == 1 & cvd_sex == 1 & cvd_smoking == 1 & cvd_age_cat == 2 ~ 37,
      cvd_dm == 1 & cvd_sex == 1 & cvd_smoking == 1 & cvd_age_cat == 3 ~ 38,
      cvd_dm == 1 & cvd_sex == 1 & cvd_smoking == 1 & cvd_age_cat == 4 ~ 39,
      cvd_dm == 1 & cvd_sex == 1 & cvd_smoking == 1 & cvd_age_cat == 5 ~ 40,
      cvd_dm == 1 & cvd_sex == 1 & cvd_smoking == 1 & cvd_age_cat == 6 ~ 41,
      cvd_dm == 1 & cvd_sex == 1 & cvd_smoking == 1 & cvd_age_cat == 7 ~ 42,
      
      # with DM, female, no smoking
      cvd_dm == 1 & cvd_sex == 2 & cvd_smoking == 0 & cvd_age_cat == 1 ~ 43,
      cvd_dm == 1 & cvd_sex == 2 & cvd_smoking == 0 & cvd_age_cat == 2 ~ 44,
      cvd_dm == 1 & cvd_sex == 2 & cvd_smoking == 0 & cvd_age_cat == 3 ~ 45,
      cvd_dm == 1 & cvd_sex == 2 & cvd_smoking == 0 & cvd_age_cat == 4 ~ 46,
      cvd_dm == 1 & cvd_sex == 2 & cvd_smoking == 0 & cvd_age_cat == 5 ~ 47,
      cvd_dm == 1 & cvd_sex == 2 & cvd_smoking == 0 & cvd_age_cat == 6 ~ 48,
      cvd_dm == 1 & cvd_sex == 2 & cvd_smoking == 0 & cvd_age_cat == 7 ~ 49,
      
      # with DM, female, with smoking
      cvd_dm == 1 & cvd_sex == 2 & cvd_smoking == 1 & cvd_age_cat == 1 ~ 50,
      cvd_dm == 1 & cvd_sex == 2 & cvd_smoking == 1 & cvd_age_cat == 2 ~ 51,
      cvd_dm == 1 & cvd_sex == 2 & cvd_smoking == 1 & cvd_age_cat == 3 ~ 52,
      cvd_dm == 1 & cvd_sex == 2 & cvd_smoking == 1 & cvd_age_cat == 4 ~ 53,
      cvd_dm == 1 & cvd_sex == 2 & cvd_smoking == 1 & cvd_age_cat == 5 ~ 54,
      cvd_dm == 1 & cvd_sex == 2 & cvd_smoking == 1 & cvd_age_cat == 6 ~ 55,
      cvd_dm == 1 & cvd_sex == 2 & cvd_smoking == 1 & cvd_age_cat == 7 ~ 56))
  
  # creating variable "who_risk_lab"
  df <- df %>%
    mutate(who_risk_lab = case_when(
      # no DM, male, no smoking, age 40-44
      cat_multi == 1 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 1,
      cat_multi == 1 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 2,
      cat_multi == 1 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 2,
      cat_multi == 1 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 3,
      cat_multi == 1 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 5,
      
      cat_multi == 1 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 1,
      cat_multi == 1 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 2,
      cat_multi == 1 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 3,
      cat_multi == 1 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 4,
      cat_multi == 1 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 5,
      
      cat_multi == 1 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 2,
      cat_multi == 1 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 2,
      cat_multi == 1 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 3,
      cat_multi == 1 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 4,
      cat_multi == 1 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 6,
      
      cat_multi == 1 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 2,
      cat_multi == 1 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 3,
      cat_multi == 1 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 3,
      cat_multi == 1 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 5,
      cat_multi == 1 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 6,
      
      cat_multi == 1 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 2,
      cat_multi == 1 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 3,
      cat_multi == 1 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 4,
      cat_multi == 1 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 5,
      cat_multi == 1 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 7,
      
      # no DM, male, no smoking, age 45-49
      cat_multi == 2 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 2,
      cat_multi == 2 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 2,
      cat_multi == 2 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 3,
      cat_multi == 2 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 4,
      cat_multi == 2 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 6,
      
      cat_multi == 2 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 2,
      cat_multi == 2 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 3,
      cat_multi == 2 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 4,
      cat_multi == 2 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 5,
      cat_multi == 2 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 6,
      
      cat_multi == 2 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 2,
      cat_multi == 2 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 3,
      cat_multi == 2 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 4,
      cat_multi == 2 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 5,
      cat_multi == 2 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 7,
      
      cat_multi == 2 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 2 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 3,
      cat_multi == 2 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 4,
      cat_multi == 2 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 6,
      cat_multi == 2 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 8,
      
      cat_multi == 2 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 2 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 4,
      cat_multi == 2 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 5,
      cat_multi == 2 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 7,
      cat_multi == 2 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 9,
      
      # no DM, male, no smoking, age 50-54
      cat_multi == 3 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 2,
      cat_multi == 3 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 3,
      cat_multi == 3 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 4,
      cat_multi == 3 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 6,
      cat_multi == 3 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 7,
      
      cat_multi == 3 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 3 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 4,
      cat_multi == 3 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 5,
      cat_multi == 3 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 6,
      cat_multi == 3 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 8,
      
      cat_multi == 3 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 3 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 4,
      cat_multi == 3 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 5,
      cat_multi == 3 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 7,
      cat_multi == 3 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 9,
      
      cat_multi == 3 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 3 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 4,
      cat_multi == 3 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 6,
      cat_multi == 3 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 8,
      cat_multi == 3 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 10,
      
      cat_multi == 3 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 4,
      cat_multi == 3 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 5,
      cat_multi == 3 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 7,
      cat_multi == 3 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 9,
      cat_multi == 3 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 11,
      
      # no DM, male, no smoking, age 55-59
      cat_multi == 4 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 4 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 4,
      cat_multi == 4 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 6,
      cat_multi == 4 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 7,
      cat_multi == 4 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 9,
      
      cat_multi == 4 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 4,
      cat_multi == 4 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 5,
      cat_multi == 4 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 6,
      cat_multi == 4 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 8,
      cat_multi == 4 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 10,
      
      cat_multi == 4 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 4,
      cat_multi == 4 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 5,
      cat_multi == 4 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 7,
      cat_multi == 4 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 9,
      cat_multi == 4 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 11,
      
      cat_multi == 4 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 5,
      cat_multi == 4 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 6,
      cat_multi == 4 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 8,
      cat_multi == 4 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 10,
      cat_multi == 4 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 12,
      
      cat_multi == 4 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 5,
      cat_multi == 4 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 7,
      cat_multi == 4 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 8,
      cat_multi == 4 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 11,
      cat_multi == 4 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 14,
      
      # no DM, male, no smoking, age 60-64
      cat_multi == 5 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 5,
      cat_multi == 5 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 6,
      cat_multi == 5 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 7,
      cat_multi == 5 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 9,
      cat_multi == 5 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 12,
      
      cat_multi == 5 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 5,
      cat_multi == 5 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 6,
      cat_multi == 5 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 8,
      cat_multi == 5 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 10,
      cat_multi == 5 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 13,
      
      cat_multi == 5 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 6,
      cat_multi == 5 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 7,
      cat_multi == 5 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 9,
      cat_multi == 5 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 11,
      cat_multi == 5 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 14,
      
      cat_multi == 5 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 6,
      cat_multi == 5 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 8,
      cat_multi == 5 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 10,
      cat_multi == 5 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 12,
      cat_multi == 5 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 15,
      
      cat_multi == 5 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 7,
      cat_multi == 5 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 9,
      cat_multi == 5 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 11,
      cat_multi == 5 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 14,
      cat_multi == 5 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 17,
      
      # no DM, male, no smoking, age 65-69
      cat_multi == 6 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 6,
      cat_multi == 6 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 8,
      cat_multi == 6 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 10,
      cat_multi == 6 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 12,
      cat_multi == 6 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 15,
      
      cat_multi == 6 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 7,
      cat_multi == 6 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 9,
      cat_multi == 6 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 11,
      cat_multi == 6 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 13,
      cat_multi == 6 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 16,
      
      cat_multi == 6 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 8,
      cat_multi == 6 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 9,
      cat_multi == 6 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 12,
      cat_multi == 6 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 14,
      cat_multi == 6 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 17,
      
      cat_multi == 6 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 8,
      cat_multi == 6 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 10,
      cat_multi == 6 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 13,
      cat_multi == 6 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 15,
      cat_multi == 6 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 19,
      
      cat_multi == 6 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 9,
      cat_multi == 6 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 11,
      cat_multi == 6 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 14,
      cat_multi == 6 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 17,
      cat_multi == 6 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 21,
      
      # no DM, male, no smoking, age 70-74
      cat_multi == 7 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 9,
      cat_multi == 7 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 11,
      cat_multi == 7 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 13,
      cat_multi == 7 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 15,
      cat_multi == 7 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 18,
      
      cat_multi == 7 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 9,
      cat_multi == 7 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 11,
      cat_multi == 7 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 14,
      cat_multi == 7 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 17,
      cat_multi == 7 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 20,
      
      cat_multi == 7 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 10,
      cat_multi == 7 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 12,
      cat_multi == 7 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 15,
      cat_multi == 7 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 18,
      cat_multi == 7 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 21,
      
      cat_multi == 7 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 11,
      cat_multi == 7 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 14,
      cat_multi == 7 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 16,
      cat_multi == 7 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 19,
      cat_multi == 7 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 23,
      
      cat_multi == 7 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 12,
      cat_multi == 7 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 15,
      cat_multi == 7 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 18,
      cat_multi == 7 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 21,
      cat_multi == 7 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 25,
      
      # no DM, male, with smoking, age 40-44
      cat_multi == 8 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 2,
      cat_multi == 8 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 3,
      cat_multi == 8 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 4,
      cat_multi == 8 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 6,
      cat_multi == 8 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 8,
      
      cat_multi == 8 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 8 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 4,
      cat_multi == 8 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 5,
      cat_multi == 8 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 7,
      cat_multi == 8 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 9,
      
      cat_multi == 8 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 8 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 4,
      cat_multi == 8 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 5,
      cat_multi == 8 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 7,
      cat_multi == 8 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 10,
      
      cat_multi == 8 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 8 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 5,
      cat_multi == 8 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 6,
      cat_multi == 8 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 8,
      cat_multi == 8 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 11,
      
      cat_multi == 8 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 4,
      cat_multi == 8 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 6,
      cat_multi == 8 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 7,
      cat_multi == 8 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 10,
      cat_multi == 8 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 13,
      
      # no DM, male, with smoking, age 45-49
      cat_multi == 9 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 9 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 4,
      cat_multi == 9 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 5,
      cat_multi == 9 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 7,
      cat_multi == 9 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 10,
      
      cat_multi == 9 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 9 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 4,
      cat_multi == 9 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 6,
      cat_multi == 9 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 8,
      cat_multi == 9 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 11,
      
      cat_multi == 9 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 4,
      cat_multi == 9 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 5,
      cat_multi == 9 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 7,
      cat_multi == 9 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 9,
      cat_multi == 9 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 12,
      
      cat_multi == 9 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 4,
      cat_multi == 9 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 6,
      cat_multi == 9 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 8,
      cat_multi == 9 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 10,
      cat_multi == 9 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 13,
      
      cat_multi == 9 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 5,
      cat_multi == 9 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 7,
      cat_multi == 9 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 9,
      cat_multi == 9 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 12,
      cat_multi == 9 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 15,
      
      # no DM, male, with smoking, age 50-54
      cat_multi == 10 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 4,
      cat_multi == 10 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 5,
      cat_multi == 10 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 7,
      cat_multi == 10 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 9,
      cat_multi == 10 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 11,
      
      cat_multi == 10 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 4,
      cat_multi == 10 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 6,
      cat_multi == 10 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 7,
      cat_multi == 10 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 10,
      cat_multi == 10 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 13,
      
      cat_multi == 10 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 5,
      cat_multi == 10 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 6,
      cat_multi == 10 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 8,
      cat_multi == 10 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 11,
      cat_multi == 10 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 14,
      
      cat_multi == 10 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 6,
      cat_multi == 10 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 7,
      cat_multi == 10 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 9,
      cat_multi == 10 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 12,
      cat_multi == 10 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 16,
      
      cat_multi == 10 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 6,
      cat_multi == 10 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 8,
      cat_multi == 10 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 11,
      cat_multi == 10 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 14,
      cat_multi == 10 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 18,
      
      # no DM, male, with smoking, age 55-59
      cat_multi == 11 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 5,
      cat_multi == 11 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 6,
      cat_multi == 11 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 8,
      cat_multi == 11 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 11,
      cat_multi == 11 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 14,
      
      cat_multi == 11 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 6,
      cat_multi == 11 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 7,
      cat_multi == 11 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 9,
      cat_multi == 11 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 12,
      cat_multi == 11 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 15,
      
      cat_multi == 11 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 6,
      cat_multi == 11 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 8,
      cat_multi == 11 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 10,
      cat_multi == 11 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 13,
      cat_multi == 11 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 16,
      
      cat_multi == 11 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 7,
      cat_multi == 11 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 9,
      cat_multi == 11 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 11,
      cat_multi == 11 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 14,
      cat_multi == 11 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 18,
      
      cat_multi == 11 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 8,
      cat_multi == 11 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 10,
      cat_multi == 11 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 13,
      cat_multi == 11 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 16,
      cat_multi == 11 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 20,
      
      # no DM, male, with smoking, age 60-64
      cat_multi == 12 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 7,
      cat_multi == 12 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 8,
      cat_multi == 12 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 10,
      cat_multi == 12 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 13,
      cat_multi == 12 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 16,
      
      cat_multi == 12 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 7,
      cat_multi == 12 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 9,
      cat_multi == 12 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 11,
      cat_multi == 12 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 14,
      cat_multi == 12 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 18,
      
      cat_multi == 12 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 8,
      cat_multi == 12 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 10,
      cat_multi == 12 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 12,
      cat_multi == 12 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 16,
      cat_multi == 12 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 19,
      
      cat_multi == 12 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 9,
      cat_multi == 12 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 11,
      cat_multi == 12 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 14,
      cat_multi == 12 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 17,
      cat_multi == 12 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 21,
      
      cat_multi == 12 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 10,
      cat_multi == 12 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 12,
      cat_multi == 12 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 15,
      cat_multi == 12 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 19,
      cat_multi == 12 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 23,
      
      # no DM, male, with smoking, age 65-69
      cat_multi == 13 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 8,
      cat_multi == 13 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 10,
      cat_multi == 13 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 13,
      cat_multi == 13 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 16,
      cat_multi == 13 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 19,
      
      cat_multi == 13 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 9,
      cat_multi == 13 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 11,
      cat_multi == 13 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 14,
      cat_multi == 13 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 17,
      cat_multi == 13 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 21,
      
      cat_multi == 13 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 10,
      cat_multi == 13 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 12,
      cat_multi == 13 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 15,
      cat_multi == 13 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 19,
      cat_multi == 13 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 23,
      
      cat_multi == 13 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 11,
      cat_multi == 13 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 14,
      cat_multi == 13 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 17,
      cat_multi == 13 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 20,
      cat_multi == 13 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 25,
      
      cat_multi == 13 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 12,
      cat_multi == 13 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 15,
      cat_multi == 13 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 18,
      cat_multi == 13 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 22,
      cat_multi == 13 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 27,
      
      # no DM, male, with smoking, age 70-74
      cat_multi == 14 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 11,
      cat_multi == 14 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 13,
      cat_multi == 14 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 16,
      cat_multi == 14 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 19,
      cat_multi == 14 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 23,
      
      cat_multi == 14 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 12,
      cat_multi == 14 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 14,
      cat_multi == 14 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 17,
      cat_multi == 14 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 21,
      cat_multi == 14 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 24,
      
      cat_multi == 14 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 13,
      cat_multi == 14 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 16,
      cat_multi == 14 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 19,
      cat_multi == 14 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 22,
      cat_multi == 14 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 26,
      
      cat_multi == 14 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 14,
      cat_multi == 14 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 17,
      cat_multi == 14 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 20,
      cat_multi == 14 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 24,
      cat_multi == 14 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 29,
      
      cat_multi == 14 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 15,
      cat_multi == 14 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 18,
      cat_multi == 14 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 22,
      cat_multi == 14 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 26,
      cat_multi == 14 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 31,
      
      # no DM, female, no smoking, age 40-44
      cat_multi == 15 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 1,
      cat_multi == 15 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 1,
      cat_multi == 15 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 2,
      cat_multi == 15 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 2,
      cat_multi == 15 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 3,
      
      cat_multi == 15 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 1,
      cat_multi == 15 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 1,
      cat_multi == 15 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 2,
      cat_multi == 15 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 2,
      cat_multi == 15 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 3,
      
      cat_multi == 15 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 1,
      cat_multi == 15 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 2,
      cat_multi == 15 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 2,
      cat_multi == 15 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 3,
      cat_multi == 15 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 4,
      
      cat_multi == 15 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 1,
      cat_multi == 15 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 2,
      cat_multi == 15 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 2,
      cat_multi == 15 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 3,
      cat_multi == 15 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 4,
      
      cat_multi == 15 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 1,
      cat_multi == 15 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 2,
      cat_multi == 15 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 2,
      cat_multi == 15 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 3,
      cat_multi == 15 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 4,
      
      # no DM, female, no smoking, age 45-49
      cat_multi == 16 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 1,
      cat_multi == 16 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 2,
      cat_multi == 16 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 2,
      cat_multi == 16 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 3,
      cat_multi == 16 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 4,
      
      cat_multi == 16 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 1,
      cat_multi == 16 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 2,
      cat_multi == 16 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 2,
      cat_multi == 16 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 3,
      cat_multi == 16 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 4,
      
      cat_multi == 16 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 2,
      cat_multi == 16 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 2,
      cat_multi == 16 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 3,
      cat_multi == 16 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 3,
      cat_multi == 16 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 4,
      
      cat_multi == 16 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 2,
      cat_multi == 16 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 2,
      cat_multi == 16 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 3,
      cat_multi == 16 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 4,
      cat_multi == 16 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 5,
      
      cat_multi == 16 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 2,
      cat_multi == 16 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 2,
      cat_multi == 16 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 3,
      cat_multi == 16 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 4,
      cat_multi == 16 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 5,
      
      # no DM, female, no smoking, age 50-54
      cat_multi == 17 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 2,
      cat_multi == 17 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 2,
      cat_multi == 17 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 3,
      cat_multi == 17 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 4,
      cat_multi == 17 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 5,
      
      cat_multi == 17 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 2,
      cat_multi == 17 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 3,
      cat_multi == 17 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 3,
      cat_multi == 17 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 4,
      cat_multi == 17 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 5,
      
      cat_multi == 17 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 2,
      cat_multi == 17 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 3,
      cat_multi == 17 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 3,
      cat_multi == 17 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 4,
      cat_multi == 17 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 5,
      
      cat_multi == 17 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 2,
      cat_multi == 17 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 3,
      cat_multi == 17 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 4,
      cat_multi == 17 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 5,
      cat_multi == 17 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 6,
      
      cat_multi == 17 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 2,
      cat_multi == 17 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 3,
      cat_multi == 17 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 4,
      cat_multi == 17 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 5,
      cat_multi == 17 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 6,
      
      # no DM, female, no smoking, age 55-59
      cat_multi == 18 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 18 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 3,
      cat_multi == 18 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 4,
      cat_multi == 18 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 5,
      cat_multi == 18 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 6,
      
      cat_multi == 18 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 18 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 3,
      cat_multi == 18 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 4,
      cat_multi == 18 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 5,
      cat_multi == 18 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 6,
      
      cat_multi == 18 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 18 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 4,
      cat_multi == 18 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 4,
      cat_multi == 18 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 6,
      cat_multi == 18 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 7,
      
      cat_multi == 18 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 18 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 4,
      cat_multi == 18 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 5,
      cat_multi == 18 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 6,
      cat_multi == 18 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 7,
      
      cat_multi == 18 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 18 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 4,
      cat_multi == 18 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 5,
      cat_multi == 18 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 6,
      cat_multi == 18 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 8,
      
      # no DM, female, no smoking, age 60-64
      cat_multi == 19 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 4,
      cat_multi == 19 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 4,
      cat_multi == 19 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 5,
      cat_multi == 19 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 6,
      cat_multi == 19 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 8,
      
      cat_multi == 19 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 4,
      cat_multi == 19 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 5,
      cat_multi == 19 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 6,
      cat_multi == 19 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 7,
      cat_multi == 19 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 8,
      
      cat_multi == 19 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 4,
      cat_multi == 19 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 5,
      cat_multi == 19 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 6,
      cat_multi == 19 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 7,
      cat_multi == 19 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 9,
      
      cat_multi == 19 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 4,
      cat_multi == 19 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 5,
      cat_multi == 19 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 6,
      cat_multi == 19 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 7,
      cat_multi == 19 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 9,
      
      cat_multi == 19 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 4,
      cat_multi == 19 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 5,
      cat_multi == 19 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 7,
      cat_multi == 19 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 8,
      cat_multi == 19 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 10,
      
      # no DM, female, no smoking, age 65-69
      cat_multi == 20 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 5,
      cat_multi == 20 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 6,
      cat_multi == 20 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 7,
      cat_multi == 20 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 8,
      cat_multi == 20 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 10,
      
      cat_multi == 20 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 5,
      cat_multi == 20 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 6,
      cat_multi == 20 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 7,
      cat_multi == 20 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 9,
      cat_multi == 20 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 10,
      
      cat_multi == 20 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 5,
      cat_multi == 20 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 6,
      cat_multi == 20 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 8,
      cat_multi == 20 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 9,
      cat_multi == 20 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 11,
      
      cat_multi == 20 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 6,
      cat_multi == 20 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 7,
      cat_multi == 20 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 8,
      cat_multi == 20 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 10,
      cat_multi == 20 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 11,
      
      cat_multi == 20 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 6,
      cat_multi == 20 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 7,
      cat_multi == 20 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 8,
      cat_multi == 20 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 10,
      cat_multi == 20 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 12,
      
      # no DM, female, no smoking, age 70-74
      cat_multi == 21 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 7,
      cat_multi == 21 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 8,
      cat_multi == 21 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 9,
      cat_multi == 21 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 11,
      cat_multi == 21 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 13,
      
      cat_multi == 21 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 7,
      cat_multi == 21 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 8,
      cat_multi == 21 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 10,
      cat_multi == 21 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 11,
      cat_multi == 21 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 13,
      
      cat_multi == 21 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 7,
      cat_multi == 21 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 8,
      cat_multi == 21 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 10,
      cat_multi == 21 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 12,
      cat_multi == 21 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 14,
      
      cat_multi == 21 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 8,
      cat_multi == 21 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 9,
      cat_multi == 21 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 10,
      cat_multi == 21 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 12,
      cat_multi == 21 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 14,
      
      cat_multi == 21 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 8,
      cat_multi == 21 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 9,
      cat_multi == 21 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 11,
      cat_multi == 21 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 13,
      cat_multi == 21 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 15,
      
      # no DM, female, with smoking, age 40-44
      cat_multi == 22 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 2,
      cat_multi == 22 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 3,
      cat_multi == 22 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 4,
      cat_multi == 22 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 5,
      cat_multi == 22 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 6,
      
      cat_multi == 22 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 2,
      cat_multi == 22 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 3,
      cat_multi == 22 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 4,
      cat_multi == 22 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 5,
      cat_multi == 22 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 7,
      
      cat_multi == 22 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 2,
      cat_multi == 22 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 3,
      cat_multi == 22 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 4,
      cat_multi == 22 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 6,
      cat_multi == 22 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 7,
      
      cat_multi == 22 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 22 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 4,
      cat_multi == 22 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 5,
      cat_multi == 22 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 6,
      cat_multi == 22 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 8,
      
      cat_multi == 22 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 22 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 4,
      cat_multi == 22 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 5,
      cat_multi == 22 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 7,
      cat_multi == 22 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 9,
      
      # no DM, female, with smoking, age 45-49
      cat_multi == 23 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 23 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 3,
      cat_multi == 23 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 4,
      cat_multi == 23 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 6,
      cat_multi == 23 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 7,
      
      cat_multi == 23 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 23 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 4,
      cat_multi == 23 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 5,
      cat_multi == 23 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 6,
      cat_multi == 23 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 8,
      
      cat_multi == 23 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 23 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 4,
      cat_multi == 23 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 5,
      cat_multi == 23 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 7,
      cat_multi == 23 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 8,
      
      cat_multi == 23 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 23 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 4,
      cat_multi == 23 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 6,
      cat_multi == 23 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 7,
      cat_multi == 23 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 9,
      
      cat_multi == 23 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 4,
      cat_multi == 23 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 5,
      cat_multi == 23 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 6,
      cat_multi == 23 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 8,
      cat_multi == 23 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 10,
      
      # no DM, female, with smoking, age 50-54
      cat_multi == 24 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 24 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 4,
      cat_multi == 24 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 5,
      cat_multi == 24 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 7,
      cat_multi == 24 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 9,
      
      cat_multi == 24 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 4,
      cat_multi == 24 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 5,
      cat_multi == 24 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 6,
      cat_multi == 24 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 7,
      cat_multi == 24 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 9,
      
      cat_multi == 24 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 4,
      cat_multi == 24 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 5,
      cat_multi == 24 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 6,
      cat_multi == 24 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 8,
      cat_multi == 24 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 10,
      
      cat_multi == 24 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 4,
      cat_multi == 24 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 5,
      cat_multi == 24 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 7,
      cat_multi == 24 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 9,
      cat_multi == 24 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 11,
      
      cat_multi == 24 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 5,
      cat_multi == 24 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 6,
      cat_multi == 24 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 7,
      cat_multi == 24 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 9,
      cat_multi == 24 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 12,
      
      # no DM, female, with smoking, age 55-59
      cat_multi == 25 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 4,
      cat_multi == 25 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 5,
      cat_multi == 25 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 7,
      cat_multi == 25 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 8,
      cat_multi == 25 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 10,
      
      cat_multi == 25 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 5,
      cat_multi == 25 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 6,
      cat_multi == 25 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 7,
      cat_multi == 25 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 9,
      cat_multi == 25 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 11,
      
      cat_multi == 25 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 5,
      cat_multi == 25 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 6,
      cat_multi == 25 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 8,
      cat_multi == 25 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 9,
      cat_multi == 25 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 12,
      
      cat_multi == 25 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 5,
      cat_multi == 25 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 7,
      cat_multi == 25 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 8,
      cat_multi == 25 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 10,
      cat_multi == 25 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 12,
      
      cat_multi == 25 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 6,
      cat_multi == 25 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 7,
      cat_multi == 25 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 9,
      cat_multi == 25 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 11,
      cat_multi == 25 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 13,
      
      # no DM, female, with smoking, age 60-64
      cat_multi == 26 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 6,
      cat_multi == 26 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 7,
      cat_multi == 26 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 8,
      cat_multi == 26 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 10,
      cat_multi == 26 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 12,
      
      cat_multi == 26 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 6,
      cat_multi == 26 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 7,
      cat_multi == 26 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 9,
      cat_multi == 26 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 11,
      cat_multi == 26 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 13,
      
      cat_multi == 26 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 6,
      cat_multi == 26 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 8,
      cat_multi == 26 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 9,
      cat_multi == 26 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 11,
      cat_multi == 26 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 14,
      
      cat_multi == 26 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 7,
      cat_multi == 26 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 8,
      cat_multi == 26 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 10,
      cat_multi == 26 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 12,
      cat_multi == 26 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 14,
      
      cat_multi == 26 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 7,
      cat_multi == 26 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 9,
      cat_multi == 26 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 11,
      cat_multi == 26 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 13,
      cat_multi == 26 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 15,
      
      # no DM, female, with smoking, age 65-69
      cat_multi == 27 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 7,
      cat_multi == 27 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 9,
      cat_multi == 27 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 10,
      cat_multi == 27 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 12,
      cat_multi == 27 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 14,
      
      cat_multi == 27 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 8,
      cat_multi == 27 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 9,
      cat_multi == 27 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 11,
      cat_multi == 27 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 13,
      cat_multi == 27 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 15,
      
      cat_multi == 27 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 8,
      cat_multi == 27 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 9,
      cat_multi == 27 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 11,
      cat_multi == 27 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 13,
      cat_multi == 27 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 16,
      
      cat_multi == 27 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 8,
      cat_multi == 27 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 10,
      cat_multi == 27 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 12,
      cat_multi == 27 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 14,
      cat_multi == 27 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 17,
      
      cat_multi == 27 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 9,
      cat_multi == 27 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 11,
      cat_multi == 27 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 13,
      cat_multi == 27 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 15,
      cat_multi == 27 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 18,
      
      # no DM, female, with smoking, age 70-74
      cat_multi == 28 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 9,
      cat_multi == 28 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 11,
      cat_multi == 28 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 13,
      cat_multi == 28 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 15,
      cat_multi == 28 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 17,
      
      cat_multi == 28 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 10,
      cat_multi == 28 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 11,
      cat_multi == 28 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 13,
      cat_multi == 28 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 15,
      cat_multi == 28 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 18,
      
      cat_multi == 28 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 10,
      cat_multi == 28 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 12,
      cat_multi == 28 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 14,
      cat_multi == 28 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 16,
      cat_multi == 28 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 19,
      
      cat_multi == 28 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 10,
      cat_multi == 28 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 12,
      cat_multi == 28 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 14,
      cat_multi == 28 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 17,
      cat_multi == 28 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 19,
      
      cat_multi == 28 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 11,
      cat_multi == 28 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 13,
      cat_multi == 28 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 15,
      cat_multi == 28 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 17,
      cat_multi == 28 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 20,
      
      # with DM, male, no smoking, age 40-44
      cat_multi == 29 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 29 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 4,
      cat_multi == 29 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 5,
      cat_multi == 29 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 7,
      cat_multi == 29 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 9,
      
      cat_multi == 29 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 29 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 4,
      cat_multi == 29 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 5,
      cat_multi == 29 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 7,
      cat_multi == 29 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 10,
      
      cat_multi == 29 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 29 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 4,
      cat_multi == 29 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 6,
      cat_multi == 29 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 8,
      cat_multi == 29 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 11,
      
      cat_multi == 29 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 4,
      cat_multi == 29 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 5,
      cat_multi == 29 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 7,
      cat_multi == 29 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 9,
      cat_multi == 29 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 12,
      
      cat_multi == 29 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 4,
      cat_multi == 29 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 6,
      cat_multi == 29 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 8,
      cat_multi == 29 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 11,
      cat_multi == 29 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 14,
      
      # with DM, male, no smoking, age 45-49
      cat_multi == 30 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 30 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 4,
      cat_multi == 30 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 6,
      cat_multi == 30 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 8,
      cat_multi == 30 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 11,
      
      cat_multi == 30 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 4,
      cat_multi == 30 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 5,
      cat_multi == 30 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 7,
      cat_multi == 30 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 9,
      cat_multi == 30 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 12,
      
      cat_multi == 30 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 4,
      cat_multi == 30 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 5,
      cat_multi == 30 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 7,
      cat_multi == 30 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 10,
      cat_multi == 30 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 13,
      
      cat_multi == 30 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 5,
      cat_multi == 30 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 6,
      cat_multi == 30 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 8,
      cat_multi == 30 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 11,
      cat_multi == 30 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 14,
      
      cat_multi == 30 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 5,
      cat_multi == 30 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 7,
      cat_multi == 30 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 9,
      cat_multi == 30 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 12,
      cat_multi == 30 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 16,
      
      # with DM, male, no smoking, age 50-54
      cat_multi == 31 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 4,
      cat_multi == 31 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 6,
      cat_multi == 31 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 7,
      cat_multi == 31 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 10,
      cat_multi == 31 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 13,
      
      cat_multi == 31 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 5,
      cat_multi == 31 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 6,
      cat_multi == 31 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 8,
      cat_multi == 31 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 11,
      cat_multi == 31 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 14,
      
      cat_multi == 31 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 5,
      cat_multi == 31 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 7,
      cat_multi == 31 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 9,
      cat_multi == 31 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 12,
      cat_multi == 31 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 15,
      
      cat_multi == 31 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 6,
      cat_multi == 31 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 8,
      cat_multi == 31 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 10,
      cat_multi == 31 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 13,
      cat_multi == 31 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 17,
      
      cat_multi == 31 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 7,
      cat_multi == 31 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 9,
      cat_multi == 31 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 11,
      cat_multi == 31 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 15,
      cat_multi == 31 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 19,
      
      # with DM, male, no smoking, age 55-59
      cat_multi == 32 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 5,
      cat_multi == 32 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 7,
      cat_multi == 32 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 9,
      cat_multi == 32 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 12,
      cat_multi == 32 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 15,
      
      cat_multi == 32 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 6,
      cat_multi == 32 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 8,
      cat_multi == 32 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 10,
      cat_multi == 32 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 13,
      cat_multi == 32 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 16,
      
      cat_multi == 32 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 7,
      cat_multi == 32 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 9,
      cat_multi == 32 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 11,
      cat_multi == 32 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 14,
      cat_multi == 32 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 18,
      
      cat_multi == 32 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 8,
      cat_multi == 32 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 10,
      cat_multi == 32 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 12,
      cat_multi == 32 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 15,
      cat_multi == 32 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 19,
      
      cat_multi == 32 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 9,
      cat_multi == 32 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 11,
      cat_multi == 32 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 14,
      cat_multi == 32 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 17,
      cat_multi == 32 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 22,
      
      # with DM, male, no smoking, age 60-64
      cat_multi == 33 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 7,
      cat_multi == 33 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 9,
      cat_multi == 33 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 11,
      cat_multi == 33 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 14,
      cat_multi == 33 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 17,
      
      cat_multi == 33 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 8,
      cat_multi == 33 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 10,
      cat_multi == 33 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 12,
      cat_multi == 33 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 15,
      cat_multi == 33 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 19,
      
      cat_multi == 33 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 9,
      cat_multi == 33 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 11,
      cat_multi == 33 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 13,
      cat_multi == 33 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 17,
      cat_multi == 33 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 21,
      
      cat_multi == 33 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 10,
      cat_multi == 33 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 12,
      cat_multi == 33 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 15,
      cat_multi == 33 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 18,
      cat_multi == 33 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 23,
      
      cat_multi == 33 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 11,
      cat_multi == 33 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 13,
      cat_multi == 33 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 16,
      cat_multi == 33 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 20,
      cat_multi == 33 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 25,
      
      # with DM, male, no smoking, age 65-69
      cat_multi == 34 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 9,
      cat_multi == 34 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 11,
      cat_multi == 34 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 14,
      cat_multi == 34 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 17,
      cat_multi == 34 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 20,
      
      cat_multi == 34 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 10,
      cat_multi == 34 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 12,
      cat_multi == 34 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 15,
      cat_multi == 34 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 18,
      cat_multi == 34 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 22,
      
      cat_multi == 34 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 11,
      cat_multi == 34 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 13,
      cat_multi == 34 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 16,
      cat_multi == 34 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 20,
      cat_multi == 34 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 24,
      
      cat_multi == 34 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 12,
      cat_multi == 34 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 15,
      cat_multi == 34 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 18,
      cat_multi == 34 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 22,
      cat_multi == 34 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 26,
      
      cat_multi == 34 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 13,
      cat_multi == 34 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 16,
      cat_multi == 34 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 20,
      cat_multi == 34 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 24,
      cat_multi == 34 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 29,
      
      # with DM, male, no smoking, age 70-74
      cat_multi == 35 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 12,
      cat_multi == 35 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 14,
      cat_multi == 35 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 17,
      cat_multi == 35 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 20,
      cat_multi == 35 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 24,
      
      cat_multi == 35 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 13,
      cat_multi == 35 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 15,
      cat_multi == 35 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 18,
      cat_multi == 35 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 22,
      cat_multi == 35 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 26,
      
      cat_multi == 35 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 14,
      cat_multi == 35 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 16,
      cat_multi == 35 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 20,
      cat_multi == 35 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 23,
      cat_multi == 35 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 28,
      
      cat_multi == 35 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 15,
      cat_multi == 35 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 18,
      cat_multi == 35 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 21,
      cat_multi == 35 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 26,
      cat_multi == 35 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 30,
      
      cat_multi == 35 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 16,
      cat_multi == 35 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 20,
      cat_multi == 35 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 23,
      cat_multi == 35 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 28,
      cat_multi == 35 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 33,
      
      # with DM, male, with smoking, age 40-44
      cat_multi == 36 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 5,
      cat_multi == 36 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 6,
      cat_multi == 36 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 8,
      cat_multi == 36 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 11,
      cat_multi == 36 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 15,
      
      cat_multi == 36 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 5,
      cat_multi == 36 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 7,
      cat_multi == 36 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 9,
      cat_multi == 36 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 13,
      cat_multi == 36 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 17,
      
      cat_multi == 36 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 6,
      cat_multi == 36 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 8,
      cat_multi == 36 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 11,
      cat_multi == 36 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 14,
      cat_multi == 36 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 19,
      
      cat_multi == 36 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 7,
      cat_multi == 36 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 9,
      cat_multi == 36 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 12,
      cat_multi == 36 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 16,
      cat_multi == 36 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 22,
      
      cat_multi == 36 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 8,
      cat_multi == 36 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 11,
      cat_multi == 36 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 14,
      cat_multi == 36 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 19,
      cat_multi == 36 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 25,
      
      # with DM, male, with smoking, age 45-49
      cat_multi == 37 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 5,
      cat_multi == 37 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 7,
      cat_multi == 37 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 10,
      cat_multi == 37 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 13,
      cat_multi == 37 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 17,
      
      cat_multi == 37 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 6,
      cat_multi == 37 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 8,
      cat_multi == 37 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 11,
      cat_multi == 37 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 14,
      cat_multi == 37 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 19,
      
      cat_multi == 37 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 7,
      cat_multi == 37 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 9,
      cat_multi == 37 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 12,
      cat_multi == 37 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 16,
      cat_multi == 37 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 21,
      
      cat_multi == 37 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 8,
      cat_multi == 37 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 11,
      cat_multi == 37 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 14,
      cat_multi == 37 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 18,
      cat_multi == 37 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 24,
      
      cat_multi == 37 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 10,
      cat_multi == 37 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 12,
      cat_multi == 37 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 16,
      cat_multi == 37 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 21,
      cat_multi == 37 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 27,
      
      # with DM, male, with smoking, age 50-54
      cat_multi == 38 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 7,
      cat_multi == 38 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 9,
      cat_multi == 38 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 11,
      cat_multi == 38 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 15,
      cat_multi == 38 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 19,
      
      cat_multi == 38 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 7,
      cat_multi == 38 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 10,
      cat_multi == 38 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 13,
      cat_multi == 38 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 16,
      cat_multi == 38 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 21,
      
      cat_multi == 38 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 8,
      cat_multi == 38 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 11,
      cat_multi == 38 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 14,
      cat_multi == 38 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 18,
      cat_multi == 38 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 23,
      
      cat_multi == 38 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 10,
      cat_multi == 38 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 12,
      cat_multi == 38 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 16,
      cat_multi == 38 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 20,
      cat_multi == 38 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 26,
      
      cat_multi == 38 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 11,
      cat_multi == 38 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 14,
      cat_multi == 38 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 18,
      cat_multi == 38 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 23,
      cat_multi == 38 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 29,
      
      # with DM, male, with smoking, age 55-59
      cat_multi == 39 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 8,
      cat_multi == 39 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 10,
      cat_multi == 39 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 13,
      cat_multi == 39 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 17,
      cat_multi == 39 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 21,
      
      cat_multi == 39 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 9,
      cat_multi == 39 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 12,
      cat_multi == 39 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 15,
      cat_multi == 39 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 18,
      cat_multi == 39 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 23,
      
      cat_multi == 39 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 10,
      cat_multi == 39 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 13,
      cat_multi == 39 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 16,
      cat_multi == 39 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 20,
      cat_multi == 39 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 26,
      
      cat_multi == 39 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 11,
      cat_multi == 39 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 15,
      cat_multi == 39 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 18,
      cat_multi == 39 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 23,
      cat_multi == 39 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 28,
      
      cat_multi == 39 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 13,
      cat_multi == 39 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 16,
      cat_multi == 39 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 21,
      cat_multi == 39 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 26,
      cat_multi == 39 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 32,
      
      # with DM, male, with smoking, age 60-64
      cat_multi == 40 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 10,
      cat_multi == 40 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 12,
      cat_multi == 40 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 15,
      cat_multi == 40 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 19,
      cat_multi == 40 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 24,
      
      cat_multi == 40 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 11,
      cat_multi == 40 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 14,
      cat_multi == 40 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 17,
      cat_multi == 40 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 21,
      cat_multi == 40 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 26,
      
      cat_multi == 40 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 12,
      cat_multi == 40 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 15,
      cat_multi == 40 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 19,
      cat_multi == 40 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 23,
      cat_multi == 40 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 28,
      
      cat_multi == 40 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 14,
      cat_multi == 40 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 17,
      cat_multi == 40 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 21,
      cat_multi == 40 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 25,
      cat_multi == 40 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 31,
      
      cat_multi == 40 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 15,
      cat_multi == 40 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 19,
      cat_multi == 40 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 23,
      cat_multi == 40 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 28,
      cat_multi == 40 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 34,
      
      # with DM, male, with smoking, age 65-69
      cat_multi == 41 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 12,
      cat_multi == 41 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 15,
      cat_multi == 41 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 18,
      cat_multi == 41 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 22,
      cat_multi == 41 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 26,
      
      cat_multi == 41 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 13,
      cat_multi == 41 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 16,
      cat_multi == 41 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 20,
      cat_multi == 41 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 24,
      cat_multi == 41 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 29,
      
      cat_multi == 41 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 14,
      cat_multi == 41 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 18,
      cat_multi == 41 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 21,
      cat_multi == 41 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 26,
      cat_multi == 41 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 31,
      
      cat_multi == 41 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 16,
      cat_multi == 41 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 19,
      cat_multi == 41 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 24,
      cat_multi == 41 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 28,
      cat_multi == 41 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 34,
      
      cat_multi == 41 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 18,
      cat_multi == 41 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 22,
      cat_multi == 41 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 26,
      cat_multi == 41 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 31,
      cat_multi == 41 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 37,
      
      # with DM, male, with smoking, age 70-74
      cat_multi == 42 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 14,
      cat_multi == 42 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 17,
      cat_multi == 42 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 21,
      cat_multi == 42 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 25,
      cat_multi == 42 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 29,
      
      cat_multi == 42 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 16,
      cat_multi == 42 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 19,
      cat_multi == 42 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 22,
      cat_multi == 42 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 27,
      cat_multi == 42 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 32,
      
      cat_multi == 42 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 17,
      cat_multi == 42 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 21,
      cat_multi == 42 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 24,
      cat_multi == 42 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 29,
      cat_multi == 42 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 34,
      
      cat_multi == 42 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 19,
      cat_multi == 42 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 22,
      cat_multi == 42 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 27,
      cat_multi == 42 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 31,
      cat_multi == 42 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 37,
      
      cat_multi == 42 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 21,
      cat_multi == 42 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 25,
      cat_multi == 42 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 29,
      cat_multi == 42 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 34,
      cat_multi == 42 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 40,
      
      # with DM, female, no smoking, age 40-44
      cat_multi == 43 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 2,
      cat_multi == 43 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 3,
      cat_multi == 43 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 4,
      cat_multi == 43 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 5,
      cat_multi == 43 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 6,
      
      cat_multi == 43 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 2,
      cat_multi == 43 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 3,
      cat_multi == 43 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 4,
      cat_multi == 43 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 5,
      cat_multi == 43 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 7,
      
      cat_multi == 43 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 2,
      cat_multi == 43 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 3,
      cat_multi == 43 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 4,
      cat_multi == 43 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 6,
      cat_multi == 43 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 7,
      
      cat_multi == 43 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 43 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 4,
      cat_multi == 43 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 5,
      cat_multi == 43 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 6,
      cat_multi == 43 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 8,
      
      cat_multi == 43 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 43 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 4,
      cat_multi == 43 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 5,
      cat_multi == 43 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 7,
      cat_multi == 43 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 9,
      
      # with DM, female, no smoking, age 45-49
      cat_multi == 44 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 44 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 3,
      cat_multi == 44 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 4,
      cat_multi == 44 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 6,
      cat_multi == 44 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 7,
      
      cat_multi == 44 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 44 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 4,
      cat_multi == 44 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 5,
      cat_multi == 44 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 6,
      cat_multi == 44 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 8,
      
      cat_multi == 44 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 44 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 4,
      cat_multi == 44 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 5,
      cat_multi == 44 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 7,
      cat_multi == 44 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 9,
      
      cat_multi == 44 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 44 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 4,
      cat_multi == 44 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 6,
      cat_multi == 44 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 7,
      cat_multi == 44 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 9,
      
      cat_multi == 44 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 4,
      cat_multi == 44 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 5,
      cat_multi == 44 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 6,
      cat_multi == 44 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 8,
      cat_multi == 44 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 10,
      
      # with DM, female, no smoking, age 50-54
      cat_multi == 45 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 3,
      cat_multi == 45 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 4,
      cat_multi == 45 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 6,
      cat_multi == 45 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 7,
      cat_multi == 45 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 9,
      
      cat_multi == 45 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 4,
      cat_multi == 45 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 5,
      cat_multi == 45 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 6,
      cat_multi == 45 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 7,
      cat_multi == 45 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 9,
      
      cat_multi == 45 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 4,
      cat_multi == 45 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 5,
      cat_multi == 45 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 6,
      cat_multi == 45 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 8,
      cat_multi == 45 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 10,
      
      cat_multi == 45 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 4,
      cat_multi == 45 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 5,
      cat_multi == 45 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 7,
      cat_multi == 45 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 9,
      cat_multi == 45 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 11,
      
      cat_multi == 45 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 5,
      cat_multi == 45 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 6,
      cat_multi == 45 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 8,
      cat_multi == 45 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 9,
      cat_multi == 45 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 12,
      
      # with DM, female, no smoking, age 55-59
      cat_multi == 46 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 4,
      cat_multi == 46 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 6,
      cat_multi == 46 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 7,
      cat_multi == 46 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 8,
      cat_multi == 46 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 10,
      
      cat_multi == 46 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 5,
      cat_multi == 46 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 6,
      cat_multi == 46 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 7,
      cat_multi == 46 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 9,
      cat_multi == 46 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 11,
      
      cat_multi == 46 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 5,
      cat_multi == 46 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 6,
      cat_multi == 46 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 8,
      cat_multi == 46 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 10,
      cat_multi == 46 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 12,
      
      cat_multi == 46 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 6,
      cat_multi == 46 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 7,
      cat_multi == 46 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 8,
      cat_multi == 46 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 10,
      cat_multi == 46 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 13,
      
      cat_multi == 46 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 6,
      cat_multi == 46 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 7,
      cat_multi == 46 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 9,
      cat_multi == 46 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 11,
      cat_multi == 46 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 14,
      
      # with DM, female, no smoking, age 60-64
      cat_multi == 47 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 6,
      cat_multi == 47 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 7,
      cat_multi == 47 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 9,
      cat_multi == 47 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 10,
      cat_multi == 47 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 13,
      
      cat_multi == 47 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 6,
      cat_multi == 47 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 7,
      cat_multi == 47 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 9,
      cat_multi == 47 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 11,
      cat_multi == 47 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 13,
      
      cat_multi == 47 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 7,
      cat_multi == 47 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 8,
      cat_multi == 47 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 10,
      cat_multi == 47 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 12,
      cat_multi == 47 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 14,
      
      cat_multi == 47 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 7,
      cat_multi == 47 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 8,
      cat_multi == 47 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 10,
      cat_multi == 47 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 12,
      cat_multi == 47 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 15,
      
      cat_multi == 47 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 7,
      cat_multi == 47 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 9,
      cat_multi == 47 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 11,
      cat_multi == 47 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 13,
      cat_multi == 47 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 16,
      
      # with DM, female, no smoking, age 65-69
      cat_multi == 48 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 8,
      cat_multi == 48 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 9,
      cat_multi == 48 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 11,
      cat_multi == 48 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 13,
      cat_multi == 48 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 15,
      
      cat_multi == 48 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 8,
      cat_multi == 48 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 9,
      cat_multi == 48 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 11,
      cat_multi == 48 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 13,
      cat_multi == 48 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 16,
      
      cat_multi == 48 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 8,
      cat_multi == 48 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 10,
      cat_multi == 48 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 12,
      cat_multi == 48 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 14,
      cat_multi == 48 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 17,
      
      cat_multi == 48 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 9,
      cat_multi == 48 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 11,
      cat_multi == 48 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 13,
      cat_multi == 48 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 15,
      cat_multi == 48 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 18,
      
      cat_multi == 48 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 9,
      cat_multi == 48 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 11,
      cat_multi == 48 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 13,
      cat_multi == 48 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 16,
      cat_multi == 48 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 19,
      
      # with DM, female, no smoking, age 70-74
      cat_multi == 49 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 10,
      cat_multi == 49 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 11,
      cat_multi == 49 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 13,
      cat_multi == 49 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 16,
      cat_multi == 49 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 18,
      
      cat_multi == 49 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 10,
      cat_multi == 49 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 12,
      cat_multi == 49 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 14,
      cat_multi == 49 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 16,
      cat_multi == 49 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 19,
      
      cat_multi == 49 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 11,
      cat_multi == 49 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 12,
      cat_multi == 49 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 15,
      cat_multi == 49 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 17,
      cat_multi == 49 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 20,
      
      cat_multi == 49 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 11,
      cat_multi == 49 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 13,
      cat_multi == 49 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 15,
      cat_multi == 49 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 18,
      cat_multi == 49 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 21,
      
      cat_multi == 49 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 12,
      cat_multi == 49 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 14,
      cat_multi == 49 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 16,
      cat_multi == 49 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 19,
      cat_multi == 49 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 22,
      
      # with DM, female, with smoking, age 40-44
      cat_multi == 50 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 4,
      cat_multi == 50 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 6,
      cat_multi == 50 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 7,
      cat_multi == 50 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 10,
      cat_multi == 50 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 13,
      
      cat_multi == 50 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 5,
      cat_multi == 50 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 6,
      cat_multi == 50 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 8,
      cat_multi == 50 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 11,
      cat_multi == 50 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 14,
      
      cat_multi == 50 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 5,
      cat_multi == 50 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 7,
      cat_multi == 50 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 9,
      cat_multi == 50 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 12,
      cat_multi == 50 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 15,
      
      cat_multi == 50 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 6,
      cat_multi == 50 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 8,
      cat_multi == 50 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 10,
      cat_multi == 50 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 13,
      cat_multi == 50 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 17,
      
      cat_multi == 50 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 7,
      cat_multi == 50 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 9,
      cat_multi == 50 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 12,
      cat_multi == 50 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 15,
      cat_multi == 50 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 19,
      
      # with DM, female, with smoking, age 45-49
      cat_multi == 51 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 5,
      cat_multi == 51 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 7,
      cat_multi == 51 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 9,
      cat_multi == 51 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 11,
      cat_multi == 51 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 14,
      
      cat_multi == 51 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 6,
      cat_multi == 51 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 7,
      cat_multi == 51 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 9,
      cat_multi == 51 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 12,
      cat_multi == 51 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 15,
      
      cat_multi == 51 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 6,
      cat_multi == 51 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 8,
      cat_multi == 51 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 10,
      cat_multi == 51 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 13,
      cat_multi == 51 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 17,
      
      cat_multi == 51 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 7,
      cat_multi == 51 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 9,
      cat_multi == 51 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 12,
      cat_multi == 51 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 15,
      cat_multi == 51 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 19,
      
      cat_multi == 51 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 8,
      cat_multi == 51 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 10,
      cat_multi == 51 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 13,
      cat_multi == 51 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 16,
      cat_multi == 51 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 21,
      
      # with DM, female, with smoking, age 50-54
      cat_multi == 52 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 6,
      cat_multi == 52 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 8,
      cat_multi == 52 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 10,
      cat_multi == 52 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 13,
      cat_multi == 52 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 16,
      
      cat_multi == 52 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 7,
      cat_multi == 52 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 9,
      cat_multi == 52 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 11,
      cat_multi == 52 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 14,
      cat_multi == 52 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 17,
      
      cat_multi == 52 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 8,
      cat_multi == 52 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 10,
      cat_multi == 52 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 12,
      cat_multi == 52 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 15,
      cat_multi == 52 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 18,
      
      cat_multi == 52 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 8,
      cat_multi == 52 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 10,
      cat_multi == 52 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 13,
      cat_multi == 52 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 16,
      cat_multi == 52 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 20,
      
      cat_multi == 52 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 9,
      cat_multi == 52 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 12,
      cat_multi == 52 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 14,
      cat_multi == 52 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 18,
      cat_multi == 52 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 22,
      
      # with DM, female, with smoking, age 55-59
      cat_multi == 53 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 8,
      cat_multi == 53 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 9,
      cat_multi == 53 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 12,
      cat_multi == 53 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 14,
      cat_multi == 53 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 18,
      
      cat_multi == 53 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 8,
      cat_multi == 53 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 10,
      cat_multi == 53 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 13,
      cat_multi == 53 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 15,
      cat_multi == 53 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 19,
      
      cat_multi == 53 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 9,
      cat_multi == 53 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 11,
      cat_multi == 53 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 14,
      cat_multi == 53 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 17,
      cat_multi == 53 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 20,
      
      cat_multi == 53 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 10,
      cat_multi == 53 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 12,
      cat_multi == 53 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 15,
      cat_multi == 53 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 18,
      cat_multi == 53 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 22,
      
      cat_multi == 53 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 11,
      cat_multi == 53 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 13,
      cat_multi == 53 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 16,
      cat_multi == 53 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 20,
      cat_multi == 53 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 24,
      
      # with DM, female, with smoking, age 60-64
      cat_multi == 54 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 9,
      cat_multi == 54 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 11,
      cat_multi == 54 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 14,
      cat_multi == 54 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 16,
      cat_multi == 54 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 20,
      
      cat_multi == 54 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 10,
      cat_multi == 54 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 12,
      cat_multi == 54 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 14,
      cat_multi == 54 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 17,
      cat_multi == 54 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 21,
      
      cat_multi == 54 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 11,
      cat_multi == 54 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 13,
      cat_multi == 54 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 15,
      cat_multi == 54 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 19,
      cat_multi == 54 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 22,
      
      cat_multi == 54 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 11,
      cat_multi == 54 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 14,
      cat_multi == 54 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 17,
      cat_multi == 54 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 20,
      cat_multi == 54 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 24,
      
      cat_multi == 54 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 12,
      cat_multi == 54 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 15,
      cat_multi == 54 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 18,
      cat_multi == 54 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 21,
      cat_multi == 54 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 26,
      
      # with DM, female, with smoking, age 65-69
      cat_multi == 55 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 11,
      cat_multi == 55 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 13,
      cat_multi == 55 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 16,
      cat_multi == 55 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 19,
      cat_multi == 55 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 22,
      
      cat_multi == 55 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 12,
      cat_multi == 55 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 14,
      cat_multi == 55 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 17,
      cat_multi == 55 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 20,
      cat_multi == 55 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 23,
      
      cat_multi == 55 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 12,
      cat_multi == 55 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 15,
      cat_multi == 55 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 18,
      cat_multi == 55 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 21,
      cat_multi == 55 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 24,
      
      cat_multi == 55 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 13,
      cat_multi == 55 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 16,
      cat_multi == 55 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 19,
      cat_multi == 55 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 22,
      cat_multi == 55 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 26,
      
      cat_multi == 55 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 14,
      cat_multi == 55 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 17,
      cat_multi == 55 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 20,
      cat_multi == 55 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 23,
      cat_multi == 55 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 27,
      
      # with DM, female, with smoking, age 70-74
      cat_multi == 56 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 1 ~ 13,
      cat_multi == 56 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 2 ~ 16,
      cat_multi == 56 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 3 ~ 18,
      cat_multi == 56 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 4 ~ 21,
      cat_multi == 56 & cvd_tc_mmoll_cat == 1 & cvd_sbp_cat == 5 ~ 25,
      
      cat_multi == 56 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 1 ~ 14,
      cat_multi == 56 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 2 ~ 16,
      cat_multi == 56 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 3 ~ 19,
      cat_multi == 56 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 4 ~ 22,
      cat_multi == 56 & cvd_tc_mmoll_cat == 2 & cvd_sbp_cat == 5 ~ 26,
      
      cat_multi == 56 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 1 ~ 15,
      cat_multi == 56 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 2 ~ 17,
      cat_multi == 56 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 3 ~ 20,
      cat_multi == 56 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 4 ~ 23,
      cat_multi == 56 & cvd_tc_mmoll_cat == 3 & cvd_sbp_cat == 5 ~ 27,
      
      cat_multi == 56 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 1 ~ 15,
      cat_multi == 56 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 2 ~ 18,
      cat_multi == 56 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 3 ~ 21,
      cat_multi == 56 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 4 ~ 24,
      cat_multi == 56 & cvd_tc_mmoll_cat == 4 & cvd_sbp_cat == 5 ~ 28,
      
      cat_multi == 56 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 1 ~ 16,
      cat_multi == 56 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 2 ~ 19,
      cat_multi == 56 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 3 ~ 22,
      cat_multi == 56 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 4 ~ 25,
      cat_multi == 56 & cvd_tc_mmoll_cat == 5 & cvd_sbp_cat == 5 ~ 29))
  
  # creating variable "who_risk_lab_perc"
  df <- df %>%
    mutate(who_risk_lab_perc = case_when(
      cvd_sex == 1 & cvd_age_cat == 1 & who_risk_lab == 1 ~ "0",
      cvd_sex == 1 & cvd_age_cat == 1 & who_risk_lab == 2 ~ "24",
      cvd_sex == 1 & cvd_age_cat == 1 & who_risk_lab == 3 ~ "71",
      cvd_sex == 1 & cvd_age_cat == 1 & who_risk_lab == 4 ~ "89",
      cvd_sex == 1 & cvd_age_cat == 1 & who_risk_lab == 5 ~ "95",
      cvd_sex == 1 & cvd_age_cat == 1 & who_risk_lab == 6 ~ "98",
      cvd_sex == 1 & cvd_age_cat == 1 & who_risk_lab == 7 ~ "98",
      cvd_sex == 1 & cvd_age_cat == 1 & who_risk_lab == 8 ~ "99",
      cvd_sex == 1 & cvd_age_cat == 1 & who_risk_lab >= 9 ~ "100",
      
      cvd_sex == 1 & cvd_age_cat == 2 & who_risk_lab == 2 ~ "0",
      cvd_sex == 1 & cvd_age_cat == 2 & who_risk_lab == 3 ~ "33",
      cvd_sex == 1 & cvd_age_cat == 2 & who_risk_lab == 4 ~ "67",
      cvd_sex == 1 & cvd_age_cat == 2 & who_risk_lab == 5 ~ "83",
      cvd_sex == 1 & cvd_age_cat == 2 & who_risk_lab == 6 ~ "92",
      cvd_sex == 1 & cvd_age_cat == 2 & who_risk_lab == 7 ~ "95",
      cvd_sex == 1 & cvd_age_cat == 2 & who_risk_lab == 8 ~ "97",
      cvd_sex == 1 & cvd_age_cat == 2 & who_risk_lab == 9 ~ "98",
      cvd_sex == 1 & cvd_age_cat == 2 & who_risk_lab == 10 ~ "99",
      cvd_sex == 1 & cvd_age_cat == 2 & who_risk_lab == 11 ~ "99",
      cvd_sex == 1 & cvd_age_cat == 2 & who_risk_lab == 12 ~ "99",
      cvd_sex == 1 & cvd_age_cat == 2 & who_risk_lab >= 13 ~ "100",
      
      cvd_sex == 1 & cvd_age_cat == 3 & who_risk_lab == 2 ~ "0",
      cvd_sex == 1 & cvd_age_cat == 3 & who_risk_lab == 3 ~ "2",
      cvd_sex == 1 & cvd_age_cat == 3 & who_risk_lab == 4 ~ "28",
      cvd_sex == 1 & cvd_age_cat == 3 & who_risk_lab == 5 ~ "59",
      cvd_sex == 1 & cvd_age_cat == 3 & who_risk_lab == 6 ~ "72",
      cvd_sex == 1 & cvd_age_cat == 3 & who_risk_lab == 7 ~ "82",
      cvd_sex == 1 & cvd_age_cat == 3 & who_risk_lab == 8 ~ "88",
      cvd_sex == 1 & cvd_age_cat == 3 & who_risk_lab == 9 ~ "91",
      cvd_sex == 1 & cvd_age_cat == 3 & who_risk_lab == 10 ~ "94",
      cvd_sex == 1 & cvd_age_cat == 3 & who_risk_lab == 11 ~ "96",
      cvd_sex == 1 & cvd_age_cat == 3 & who_risk_lab == 12 ~ "97",
      cvd_sex == 1 & cvd_age_cat == 3 & who_risk_lab == 13 ~ "98",
      cvd_sex == 1 & cvd_age_cat == 3 & who_risk_lab == 14 ~ "99",
      cvd_sex == 1 & cvd_age_cat == 3 & who_risk_lab == 15 ~ "99",
      cvd_sex == 1 & cvd_age_cat == 3 & who_risk_lab == 16 ~ "99",
      cvd_sex == 1 & cvd_age_cat == 3 & who_risk_lab == 17 ~ ">=99",
      cvd_sex == 1 & cvd_age_cat == 3 & who_risk_lab >= 18 ~ "100",
      
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_lab == 3 ~ "0",
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_lab == 4 ~ "2",
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_lab == 5 ~ "22",
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_lab == 6 ~ "48",
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_lab == 7 ~ "61",
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_lab == 8 ~ "72",
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_lab == 9 ~ "82",
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_lab == 10 ~ "87",
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_lab == 11 ~ "90",
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_lab == 12 ~ "93",
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_lab == 13 ~ "95",
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_lab == 14 ~ "96",
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_lab == 15 ~ "97",
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_lab == 16 ~ "98",
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_lab == 17 ~ "98-99",
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_lab == 18 ~ "99",
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_lab == 19 ~ "99",
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_lab == 20 ~ ">=99",
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_lab >= 21 ~ "100",
      
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_lab == 5 ~ "0",
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_lab == 6 ~ "10",
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_lab == 7 ~ "32",
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_lab == 8 ~ "47",
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_lab == 9 ~ "57",
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_lab == 10 ~ "67",
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_lab == 11 ~ "75",
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_lab == 12 ~ "83",
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_lab == 13 ~ "88",
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_lab == 14 ~ "92",
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_lab == 15 ~ "94",
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_lab == 16 ~ "96",
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_lab == 17 ~ "97",
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_lab == 18 ~ "99",
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_lab == 19 ~ "99",
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_lab == 20 ~ "99",
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_lab >= 21 ~ "100",
      
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_lab == 6 ~ "0",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_lab == 7 ~ "3",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_lab == 8 ~ "10",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_lab == 9 ~ "21",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_lab == 10 ~ "45",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_lab == 11 ~ "53",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_lab == 12 ~ "66",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_lab == 13 ~ "76",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_lab == 14 ~ "82",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_lab == 15 ~ "86",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_lab == 16 ~ "90",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_lab == 17 ~ "94",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_lab == 18 ~ "95",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_lab == 19 ~ "97",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_lab == 20 ~ "97",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_lab == 21 ~ "99",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_lab == 22 ~ "99",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_lab == 23 ~ "99",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_lab == 24 ~ "99",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_lab == 25 ~ ">=99",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_lab >= 26 ~ "100",
      
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_lab == 9 ~ "0",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_lab == 10 ~ "10",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_lab == 11 ~ "15",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_lab == 12 ~ "29",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_lab == 13 ~ "40",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_lab == 14 ~ "44",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_lab == 15 ~ "59",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_lab == 16 ~ "70",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_lab == 17 ~ "75",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_lab == 18 ~ "80",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_lab == 19 ~ "86",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_lab == 20 ~ "89",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_lab == 21 ~ "93",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_lab == 22 ~ "95",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_lab == 23 ~ "97",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_lab == 24 ~ "97-98",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_lab == 25 ~ "98",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_lab == 26 ~ "99",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_lab == 27 ~ ">=99",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_lab >= 28 ~ "100",
      
      cvd_sex == 2 & cvd_age_cat == 1 & who_risk_lab == 1 ~ "0",
      cvd_sex == 2 & cvd_age_cat == 1 & who_risk_lab == 2 ~ "76",
      cvd_sex == 2 & cvd_age_cat == 1 & who_risk_lab == 3 ~ "96",
      cvd_sex == 2 & cvd_age_cat == 1 & who_risk_lab == 4 ~ "98",
      cvd_sex == 2 & cvd_age_cat == 1 & who_risk_lab == 5 ~ "99",
      cvd_sex == 2 & cvd_age_cat == 1 & who_risk_lab >= 6 ~ "100",
      
      cvd_sex == 2 & cvd_age_cat == 2 & who_risk_lab == 1 ~ "0",
      cvd_sex == 2 & cvd_age_cat == 2 & who_risk_lab == 2 ~ "29",
      cvd_sex == 2 & cvd_age_cat == 2 & who_risk_lab == 3 ~ "77",
      cvd_sex == 2 & cvd_age_cat == 2 & who_risk_lab == 4 ~ "92",
      cvd_sex == 2 & cvd_age_cat == 2 & who_risk_lab == 5 ~ "97",
      cvd_sex == 2 & cvd_age_cat == 2 & who_risk_lab == 6 ~ "99",
      cvd_sex == 2 & cvd_age_cat == 2 & who_risk_lab == 7 ~ "99",
      cvd_sex == 2 & cvd_age_cat == 2 & who_risk_lab == 8 ~ "99",
      cvd_sex == 2 & cvd_age_cat == 2 & who_risk_lab >= 9 ~ "100",
      
      cvd_sex == 2 & cvd_age_cat == 3 & who_risk_lab == 2 ~ "0",
      cvd_sex == 2 & cvd_age_cat == 3 & who_risk_lab == 3 ~ "47",
      cvd_sex == 2 & cvd_age_cat == 3 & who_risk_lab == 4 ~ "73",
      cvd_sex == 2 & cvd_age_cat == 3 & who_risk_lab == 5 ~ "88",
      cvd_sex == 2 & cvd_age_cat == 3 & who_risk_lab == 6 ~ "95",
      cvd_sex == 2 & cvd_age_cat == 3 & who_risk_lab == 7 ~ "97",
      cvd_sex == 2 & cvd_age_cat == 3 & who_risk_lab == 8 ~ "98",
      cvd_sex == 2 & cvd_age_cat == 3 & who_risk_lab == 9 ~ "99",
      cvd_sex == 2 & cvd_age_cat == 3 & who_risk_lab == 10 ~ "99",
      cvd_sex == 2 & cvd_age_cat == 3 & who_risk_lab >= 11 ~ "100",
      
      cvd_sex == 2 & cvd_age_cat == 4 & who_risk_lab == 3 ~ "0",
      cvd_sex == 2 & cvd_age_cat == 4 & who_risk_lab == 4 ~ "47",
      cvd_sex == 2 & cvd_age_cat == 4 & who_risk_lab == 5 ~ "68",
      cvd_sex == 2 & cvd_age_cat == 4 & who_risk_lab == 6 ~ "80",
      cvd_sex == 2 & cvd_age_cat == 4 & who_risk_lab == 7 ~ "90",
      cvd_sex == 2 & cvd_age_cat == 4 & who_risk_lab == 8 ~ "94",
      cvd_sex == 2 & cvd_age_cat == 4 & who_risk_lab == 9 ~ "96",
      cvd_sex == 2 & cvd_age_cat == 4 & who_risk_lab == 10 ~ "97",
      cvd_sex == 2 & cvd_age_cat == 4 & who_risk_lab == 11 ~ "99",
      cvd_sex == 2 & cvd_age_cat == 4 & who_risk_lab == 12 ~ "99",
      cvd_sex == 2 & cvd_age_cat == 4 & who_risk_lab >= 13 ~ "100",
      
      cvd_sex == 2 & cvd_age_cat == 5 & who_risk_lab == 4 ~ "0",
      cvd_sex == 2 & cvd_age_cat == 5 & who_risk_lab == 5 ~ "35",
      cvd_sex == 2 & cvd_age_cat == 5 & who_risk_lab == 6 ~ "63",
      cvd_sex == 2 & cvd_age_cat == 5 & who_risk_lab == 7 ~ "76",
      cvd_sex == 2 & cvd_age_cat == 5 & who_risk_lab == 8 ~ "84",
      cvd_sex == 2 & cvd_age_cat == 5 & who_risk_lab == 9 ~ "91",
      cvd_sex == 2 & cvd_age_cat == 5 & who_risk_lab == 10 ~ "93",
      cvd_sex == 2 & cvd_age_cat == 5 & who_risk_lab == 11 ~ "96",
      cvd_sex == 2 & cvd_age_cat == 5 & who_risk_lab == 12 ~ "97",
      cvd_sex == 2 & cvd_age_cat == 5 & who_risk_lab == 13 ~ "98",
      cvd_sex == 2 & cvd_age_cat == 5 & who_risk_lab == 14 ~ "99",
      cvd_sex == 2 & cvd_age_cat == 5 & who_risk_lab == 15 ~ "99",
      cvd_sex == 2 & cvd_age_cat == 5 & who_risk_lab == 16 ~ ">=99",
      cvd_sex == 2 & cvd_age_cat == 5 & who_risk_lab >= 17 ~ "100",
      
      cvd_sex == 2 & cvd_age_cat == 6 & who_risk_lab == 5 ~ "0",
      cvd_sex == 2 & cvd_age_cat == 6 & who_risk_lab == 6 ~ "18",
      cvd_sex == 2 & cvd_age_cat == 6 & who_risk_lab == 7 ~ "45",
      cvd_sex == 2 & cvd_age_cat == 6 & who_risk_lab == 8 ~ "57",
      cvd_sex == 2 & cvd_age_cat == 6 & who_risk_lab == 9 ~ "70",
      cvd_sex == 2 & cvd_age_cat == 6 & who_risk_lab == 10 ~ "81",
      cvd_sex == 2 & cvd_age_cat == 6 & who_risk_lab == 11 ~ "86",
      cvd_sex == 2 & cvd_age_cat == 6 & who_risk_lab == 12 ~ "92",
      cvd_sex == 2 & cvd_age_cat == 6 & who_risk_lab == 13 ~ "96",
      cvd_sex == 2 & cvd_age_cat == 6 & who_risk_lab == 14 ~ "98",
      cvd_sex == 2 & cvd_age_cat == 6 & who_risk_lab == 15 ~ "98",
      cvd_sex == 2 & cvd_age_cat == 6 & who_risk_lab == 16 ~ "99",
      cvd_sex == 2 & cvd_age_cat == 6 & who_risk_lab >= 17 ~ "100",
      
      cvd_sex == 2 & cvd_age_cat == 7 & who_risk_lab == 7 ~ "0",
      cvd_sex == 2 & cvd_age_cat == 7 & who_risk_lab == 8 ~ "14",
      cvd_sex == 2 & cvd_age_cat == 7 & who_risk_lab == 9 ~ "39",
      cvd_sex == 2 & cvd_age_cat == 7 & who_risk_lab == 10 ~ "48",
      cvd_sex == 2 & cvd_age_cat == 7 & who_risk_lab == 11 ~ "63",
      cvd_sex == 2 & cvd_age_cat == 7 & who_risk_lab == 12 ~ "71",
      cvd_sex == 2 & cvd_age_cat == 7 & who_risk_lab == 13 ~ "85",
      cvd_sex == 2 & cvd_age_cat == 7 & who_risk_lab == 14 ~ "89",
      cvd_sex == 2 & cvd_age_cat == 7 & who_risk_lab == 15 ~ "93",
      cvd_sex == 2 & cvd_age_cat == 7 & who_risk_lab == 16 ~ "97",
      cvd_sex == 2 & cvd_age_cat == 7 & who_risk_lab == 17 ~ "97-99",
      cvd_sex == 2 & cvd_age_cat == 7 & who_risk_lab == 18 ~ "97-99",
      cvd_sex == 2 & cvd_age_cat == 7 & who_risk_lab == 19 ~ "99",
      cvd_sex == 2 & cvd_age_cat == 7 & who_risk_lab == 20 ~ ">=99",
      cvd_sex == 2 & cvd_age_cat == 7 & who_risk_lab == 21 ~ ">=99",
      cvd_sex == 2 & cvd_age_cat == 7 & who_risk_lab >= 22 ~ "100"
    )
    )
  
  
  ### WHO CVD NON-LABORATORY-BASED RISK ####
  # creating "cvd_bmi" categories
  df <- df %>%
    mutate(cvd_bmi_cat = case_when(cvd_bmi < 20 ~ 1,
                                   cvd_bmi >= 20 & cvd_bmi < 25 ~ 2,
                                   cvd_bmi >= 25 & cvd_bmi < 30 ~ 3,
                                   cvd_bmi >= 30 & cvd_bmi < 35 ~ 4,
                                   cvd_bmi >= 35 ~ 5))
  
  
  # creating categories according to sex, age, smoking
  df <- df %>%
    mutate(cat_multi2 = case_when(
      # male, no smoking
      cvd_sex == 1 & cvd_smoking == 0 & cvd_age_cat == 1 ~ 1,
      cvd_sex == 1 & cvd_smoking == 0 & cvd_age_cat == 2 ~ 2,
      cvd_sex == 1 & cvd_smoking == 0 & cvd_age_cat == 3 ~ 3,
      cvd_sex == 1 & cvd_smoking == 0 & cvd_age_cat == 4 ~ 4,
      cvd_sex == 1 & cvd_smoking == 0 & cvd_age_cat == 5 ~ 5,
      cvd_sex == 1 & cvd_smoking == 0 & cvd_age_cat == 6 ~ 6,
      cvd_sex == 1 & cvd_smoking == 0 & cvd_age_cat == 7 ~ 7,
      
      # male, with smoking
      cvd_sex == 1 & cvd_smoking == 1 & cvd_age_cat == 1 ~ 8,
      cvd_sex == 1 & cvd_smoking == 1 & cvd_age_cat == 2 ~ 9,
      cvd_sex == 1 & cvd_smoking == 1 & cvd_age_cat == 3 ~ 10,
      cvd_sex == 1 & cvd_smoking == 1 & cvd_age_cat == 4 ~ 11,
      cvd_sex == 1 & cvd_smoking == 1 & cvd_age_cat == 5 ~ 12,
      cvd_sex == 1 & cvd_smoking == 1 & cvd_age_cat == 6 ~ 13,
      cvd_sex == 1 & cvd_smoking == 1 & cvd_age_cat == 7 ~ 14,
      
      # female, no smoking
      cvd_sex == 2 & cvd_smoking == 0 & cvd_age_cat == 1 ~ 15,
      cvd_sex == 2 & cvd_smoking == 0 & cvd_age_cat == 2 ~ 16,
      cvd_sex == 2 & cvd_smoking == 0 & cvd_age_cat == 3 ~ 17,
      cvd_sex == 2 & cvd_smoking == 0 & cvd_age_cat == 4 ~ 18,
      cvd_sex == 2 & cvd_smoking == 0 & cvd_age_cat == 5 ~ 19,
      cvd_sex == 2 & cvd_smoking == 0 & cvd_age_cat == 6 ~ 20,
      cvd_sex == 2 & cvd_smoking == 0 & cvd_age_cat == 7 ~ 21,
      
      # female, with smoking
      cvd_sex == 2 & cvd_smoking == 1 & cvd_age_cat == 1 ~ 22,
      cvd_sex == 2 & cvd_smoking == 1 & cvd_age_cat == 2 ~ 23,
      cvd_sex == 2 & cvd_smoking == 1 & cvd_age_cat == 3 ~ 24,
      cvd_sex == 2 & cvd_smoking == 1 & cvd_age_cat == 4 ~ 25,
      cvd_sex == 2 & cvd_smoking == 1 & cvd_age_cat == 5 ~ 26,
      cvd_sex == 2 & cvd_smoking == 1 & cvd_age_cat == 6 ~ 27,
      cvd_sex == 2 & cvd_smoking == 1 & cvd_age_cat == 7 ~ 28
    ))
  
  # creating variable "who_risk_non_lab"
  df <- df %>%
    mutate(who_risk_non_lab = case_when(
      # male, no smoking, age 40-44
      cat_multi2 == 1 & cvd_bmi_cat == 1 & cvd_sbp_cat == 1 ~ 1,
      cat_multi2 == 1 & cvd_bmi_cat == 1 & cvd_sbp_cat == 2 ~ 2,
      cat_multi2 == 1 & cvd_bmi_cat == 1 & cvd_sbp_cat == 3 ~ 2,
      cat_multi2 == 1 & cvd_bmi_cat == 1 & cvd_sbp_cat == 4 ~ 3,
      cat_multi2 == 1 & cvd_bmi_cat == 1 & cvd_sbp_cat == 5 ~ 5,
      
      cat_multi2 == 1 & cvd_bmi_cat == 2 & cvd_sbp_cat == 1 ~ 1,
      cat_multi2 == 1 & cvd_bmi_cat == 2 & cvd_sbp_cat == 2 ~ 2,
      cat_multi2 == 1 & cvd_bmi_cat == 2 & cvd_sbp_cat == 3 ~ 3,
      cat_multi2 == 1 & cvd_bmi_cat == 2 & cvd_sbp_cat == 4 ~ 4,
      cat_multi2 == 1 & cvd_bmi_cat == 2 & cvd_sbp_cat == 5 ~ 5,
      
      cat_multi2 == 1 & cvd_bmi_cat == 3 & cvd_sbp_cat == 1 ~ 2,
      cat_multi2 == 1 & cvd_bmi_cat == 3 & cvd_sbp_cat == 2 ~ 2,
      cat_multi2 == 1 & cvd_bmi_cat == 3 & cvd_sbp_cat == 3 ~ 3,
      cat_multi2 == 1 & cvd_bmi_cat == 3 & cvd_sbp_cat == 4 ~ 4,
      cat_multi2 == 1 & cvd_bmi_cat == 3 & cvd_sbp_cat == 5 ~ 6,
      
      cat_multi2 == 1 & cvd_bmi_cat == 4 & cvd_sbp_cat == 1 ~ 2,
      cat_multi2 == 1 & cvd_bmi_cat == 4 & cvd_sbp_cat == 2 ~ 3,
      cat_multi2 == 1 & cvd_bmi_cat == 4 & cvd_sbp_cat == 3 ~ 4,
      cat_multi2 == 1 & cvd_bmi_cat == 4 & cvd_sbp_cat == 4 ~ 5,
      cat_multi2 == 1 & cvd_bmi_cat == 4 & cvd_sbp_cat == 5 ~ 7,
      
      cat_multi2 == 1 & cvd_bmi_cat == 5 & cvd_sbp_cat == 1 ~ 2,
      cat_multi2 == 1 & cvd_bmi_cat == 5 & cvd_sbp_cat == 2 ~ 3,
      cat_multi2 == 1 & cvd_bmi_cat == 5 & cvd_sbp_cat == 3 ~ 4,
      cat_multi2 == 1 & cvd_bmi_cat == 5 & cvd_sbp_cat == 4 ~ 6,
      cat_multi2 == 1 & cvd_bmi_cat == 5 & cvd_sbp_cat == 5 ~ 8,
      
      # male, no smoking, age 45-49
      cat_multi2 == 2 & cvd_bmi_cat == 1 & cvd_sbp_cat == 1 ~ 2,
      cat_multi2 == 2 & cvd_bmi_cat == 1 & cvd_sbp_cat == 2 ~ 2,
      cat_multi2 == 2 & cvd_bmi_cat == 1 & cvd_sbp_cat == 3 ~ 3,
      cat_multi2 == 2 & cvd_bmi_cat == 1 & cvd_sbp_cat == 4 ~ 4,
      cat_multi2 == 2 & cvd_bmi_cat == 1 & cvd_sbp_cat == 5 ~ 6,
      
      cat_multi2 == 2 & cvd_bmi_cat == 2 & cvd_sbp_cat == 1 ~ 2,
      cat_multi2 == 2 & cvd_bmi_cat == 2 & cvd_sbp_cat == 2 ~ 3,
      cat_multi2 == 2 & cvd_bmi_cat == 2 & cvd_sbp_cat == 3 ~ 4,
      cat_multi2 == 2 & cvd_bmi_cat == 2 & cvd_sbp_cat == 4 ~ 5,
      cat_multi2 == 2 & cvd_bmi_cat == 2 & cvd_sbp_cat == 5 ~ 7,
      
      cat_multi2 == 2 & cvd_bmi_cat == 3 & cvd_sbp_cat == 1 ~ 2,
      cat_multi2 == 2 & cvd_bmi_cat == 3 & cvd_sbp_cat == 2 ~ 3,
      cat_multi2 == 2 & cvd_bmi_cat == 3 & cvd_sbp_cat == 3 ~ 4,
      cat_multi2 == 2 & cvd_bmi_cat == 3 & cvd_sbp_cat == 4 ~ 6,
      cat_multi2 == 2 & cvd_bmi_cat == 3 & cvd_sbp_cat == 5 ~ 8,
      
      cat_multi2 == 2 & cvd_bmi_cat == 4 & cvd_sbp_cat == 1 ~ 2,
      cat_multi2 == 2 & cvd_bmi_cat == 4 & cvd_sbp_cat == 2 ~ 3,
      cat_multi2 == 2 & cvd_bmi_cat == 4 & cvd_sbp_cat == 3 ~ 5,
      cat_multi2 == 2 & cvd_bmi_cat == 4 & cvd_sbp_cat == 4 ~ 6,
      cat_multi2 == 2 & cvd_bmi_cat == 4 & cvd_sbp_cat == 5 ~ 8,
      
      cat_multi2 == 2 & cvd_bmi_cat == 5 & cvd_sbp_cat == 1 ~ 3,
      cat_multi2 == 2 & cvd_bmi_cat == 5 & cvd_sbp_cat == 2 ~ 4,
      cat_multi2 == 2 & cvd_bmi_cat == 5 & cvd_sbp_cat == 3 ~ 5,
      cat_multi2 == 2 & cvd_bmi_cat == 5 & cvd_sbp_cat == 4 ~ 7,
      cat_multi2 == 2 & cvd_bmi_cat == 5 & cvd_sbp_cat == 5 ~ 10,
      
      # male, no smoking, age 50-54
      cat_multi2 == 3 & cvd_bmi_cat == 1 & cvd_sbp_cat == 1 ~ 2,
      cat_multi2 == 3 & cvd_bmi_cat == 1 & cvd_sbp_cat == 2 ~ 3,
      cat_multi2 == 3 & cvd_bmi_cat == 1 & cvd_sbp_cat == 3 ~ 4,
      cat_multi2 == 3 & cvd_bmi_cat == 1 & cvd_sbp_cat == 4 ~ 6,
      cat_multi2 == 3 & cvd_bmi_cat == 1 & cvd_sbp_cat == 5 ~ 8,
      
      cat_multi2 == 3 & cvd_bmi_cat == 2 & cvd_sbp_cat == 1 ~ 3,
      cat_multi2 == 3 & cvd_bmi_cat == 2 & cvd_sbp_cat == 2 ~ 4,
      cat_multi2 == 3 & cvd_bmi_cat == 2 & cvd_sbp_cat == 3 ~ 5,
      cat_multi2 == 3 & cvd_bmi_cat == 2 & cvd_sbp_cat == 4 ~ 6,
      cat_multi2 == 3 & cvd_bmi_cat == 2 & cvd_sbp_cat == 5 ~ 8,
      
      cat_multi2 == 3 & cvd_bmi_cat == 3 & cvd_sbp_cat == 1 ~ 3,
      cat_multi2 == 3 & cvd_bmi_cat == 3 & cvd_sbp_cat == 2 ~ 4,
      cat_multi2 == 3 & cvd_bmi_cat == 3 & cvd_sbp_cat == 3 ~ 5,
      cat_multi2 == 3 & cvd_bmi_cat == 3 & cvd_sbp_cat == 4 ~ 7,
      cat_multi2 == 3 & cvd_bmi_cat == 3 & cvd_sbp_cat == 5 ~ 9,
      
      cat_multi2 == 3 & cvd_bmi_cat == 4 & cvd_sbp_cat == 1 ~ 3,
      cat_multi2 == 3 & cvd_bmi_cat == 4 & cvd_sbp_cat == 2 ~ 4,
      cat_multi2 == 3 & cvd_bmi_cat == 4 & cvd_sbp_cat == 3 ~ 6,
      cat_multi2 == 3 & cvd_bmi_cat == 4 & cvd_sbp_cat == 4 ~ 8,
      cat_multi2 == 3 & cvd_bmi_cat == 4 & cvd_sbp_cat == 5 ~ 10,
      
      cat_multi2 == 3 & cvd_bmi_cat == 5 & cvd_sbp_cat == 1 ~ 4,
      cat_multi2 == 3 & cvd_bmi_cat == 5 & cvd_sbp_cat == 2 ~ 5,
      cat_multi2 == 3 & cvd_bmi_cat == 5 & cvd_sbp_cat == 3 ~ 7,
      cat_multi2 == 3 & cvd_bmi_cat == 5 & cvd_sbp_cat == 4 ~ 9,
      cat_multi2 == 3 & cvd_bmi_cat == 5 & cvd_sbp_cat == 5 ~ 12,
      
      # male, no smoking, age 55-59
      cat_multi2 == 4 & cvd_bmi_cat == 1 & cvd_sbp_cat == 1 ~ 3,
      cat_multi2 == 4 & cvd_bmi_cat == 1 & cvd_sbp_cat == 2 ~ 4,
      cat_multi2 == 4 & cvd_bmi_cat == 1 & cvd_sbp_cat == 3 ~ 6,
      cat_multi2 == 4 & cvd_bmi_cat == 1 & cvd_sbp_cat == 4 ~ 7,
      cat_multi2 == 4 & cvd_bmi_cat == 1 & cvd_sbp_cat == 5 ~ 10,
      
      cat_multi2 == 4 & cvd_bmi_cat == 2 & cvd_sbp_cat == 1 ~ 4,
      cat_multi2 == 4 & cvd_bmi_cat == 2 & cvd_sbp_cat == 2 ~ 5,
      cat_multi2 == 4 & cvd_bmi_cat == 2 & cvd_sbp_cat == 3 ~ 6,
      cat_multi2 == 4 & cvd_bmi_cat == 2 & cvd_sbp_cat == 4 ~ 8,
      cat_multi2 == 4 & cvd_bmi_cat == 2 & cvd_sbp_cat == 5 ~ 11,
      
      cat_multi2 == 4 & cvd_bmi_cat == 3 & cvd_sbp_cat == 1 ~ 4,
      cat_multi2 == 4 & cvd_bmi_cat == 3 & cvd_sbp_cat == 2 ~ 5,
      cat_multi2 == 4 & cvd_bmi_cat == 3 & cvd_sbp_cat == 3 ~ 7,
      cat_multi2 == 4 & cvd_bmi_cat == 3 & cvd_sbp_cat == 4 ~ 9,
      cat_multi2 == 4 & cvd_bmi_cat == 3 & cvd_sbp_cat == 5 ~ 12,
      
      cat_multi2 == 4 & cvd_bmi_cat == 4 & cvd_sbp_cat == 1 ~ 5,
      cat_multi2 == 4 & cvd_bmi_cat == 4 & cvd_sbp_cat == 2 ~ 6,
      cat_multi2 == 4 & cvd_bmi_cat == 4 & cvd_sbp_cat == 3 ~ 8,
      cat_multi2 == 4 & cvd_bmi_cat == 4 & cvd_sbp_cat == 4 ~ 10,
      cat_multi2 == 4 & cvd_bmi_cat == 4 & cvd_sbp_cat == 5 ~ 13,
      
      cat_multi2 == 4 & cvd_bmi_cat == 5 & cvd_sbp_cat == 1 ~ 5,
      cat_multi2 == 4 & cvd_bmi_cat == 5 & cvd_sbp_cat == 2 ~ 7,
      cat_multi2 == 4 & cvd_bmi_cat == 5 & cvd_sbp_cat == 3 ~ 8,
      cat_multi2 == 4 & cvd_bmi_cat == 5 & cvd_sbp_cat == 4 ~ 11,
      cat_multi2 == 4 & cvd_bmi_cat == 5 & cvd_sbp_cat == 5 ~ 14,
      
      # male, no smoking, age 60-64
      cat_multi2 == 5 & cvd_bmi_cat == 1 & cvd_sbp_cat == 1 ~ 5,
      cat_multi2 == 5 & cvd_bmi_cat == 1 & cvd_sbp_cat == 2 ~ 6,
      cat_multi2 == 5 & cvd_bmi_cat == 1 & cvd_sbp_cat == 3 ~ 8,
      cat_multi2 == 5 & cvd_bmi_cat == 1 & cvd_sbp_cat == 4 ~ 10,
      cat_multi2 == 5 & cvd_bmi_cat == 1 & cvd_sbp_cat == 5 ~ 12,
      
      cat_multi2 == 5 & cvd_bmi_cat == 2 & cvd_sbp_cat == 1 ~ 5,
      cat_multi2 == 5 & cvd_bmi_cat == 2 & cvd_sbp_cat == 2 ~ 6,
      cat_multi2 == 5 & cvd_bmi_cat == 2 & cvd_sbp_cat == 3 ~ 8,
      cat_multi2 == 5 & cvd_bmi_cat == 2 & cvd_sbp_cat == 4 ~ 10,
      cat_multi2 == 5 & cvd_bmi_cat == 2 & cvd_sbp_cat == 5 ~ 13,
      
      cat_multi2 == 5 & cvd_bmi_cat == 3 & cvd_sbp_cat == 1 ~ 6,
      cat_multi2 == 5 & cvd_bmi_cat == 3 & cvd_sbp_cat == 2 ~ 7,
      cat_multi2 == 5 & cvd_bmi_cat == 3 & cvd_sbp_cat == 3 ~ 9,
      cat_multi2 == 5 & cvd_bmi_cat == 3 & cvd_sbp_cat == 4 ~ 11,
      cat_multi2 == 5 & cvd_bmi_cat == 3 & cvd_sbp_cat == 5 ~ 14,
      
      cat_multi2 == 5 & cvd_bmi_cat == 4 & cvd_sbp_cat == 1 ~ 6,
      cat_multi2 == 5 & cvd_bmi_cat == 4 & cvd_sbp_cat == 2 ~ 8,
      cat_multi2 == 5 & cvd_bmi_cat == 4 & cvd_sbp_cat == 3 ~ 10,
      cat_multi2 == 5 & cvd_bmi_cat == 4 & cvd_sbp_cat == 4 ~ 12,
      cat_multi2 == 5 & cvd_bmi_cat == 4 & cvd_sbp_cat == 5 ~ 16,
      
      cat_multi2 == 5 & cvd_bmi_cat == 5 & cvd_sbp_cat == 1 ~ 7,
      cat_multi2 == 5 & cvd_bmi_cat == 5 & cvd_sbp_cat == 2 ~ 8,
      cat_multi2 == 5 & cvd_bmi_cat == 5 & cvd_sbp_cat == 3 ~ 11,
      cat_multi2 == 5 & cvd_bmi_cat == 5 & cvd_sbp_cat == 4 ~ 14,
      cat_multi2 == 5 & cvd_bmi_cat == 5 & cvd_sbp_cat == 5 ~ 17,
      
      # male, no smoking, age 65-69
      cat_multi2 == 6 & cvd_bmi_cat == 1 & cvd_sbp_cat == 1 ~ 7,
      cat_multi2 == 6 & cvd_bmi_cat == 1 & cvd_sbp_cat == 2 ~ 8,
      cat_multi2 == 6 & cvd_bmi_cat == 1 & cvd_sbp_cat == 3 ~ 10,
      cat_multi2 == 6 & cvd_bmi_cat == 1 & cvd_sbp_cat == 4 ~ 13,
      cat_multi2 == 6 & cvd_bmi_cat == 1 & cvd_sbp_cat == 5 ~ 15,
      
      cat_multi2 == 6 & cvd_bmi_cat == 2 & cvd_sbp_cat == 1 ~ 7,
      cat_multi2 == 6 & cvd_bmi_cat == 2 & cvd_sbp_cat == 2 ~ 9,
      cat_multi2 == 6 & cvd_bmi_cat == 2 & cvd_sbp_cat == 3 ~ 11,
      cat_multi2 == 6 & cvd_bmi_cat == 2 & cvd_sbp_cat == 4 ~ 13,
      cat_multi2 == 6 & cvd_bmi_cat == 2 & cvd_sbp_cat == 5 ~ 17,
      
      cat_multi2 == 6 & cvd_bmi_cat == 3 & cvd_sbp_cat == 1 ~ 8,
      cat_multi2 == 6 & cvd_bmi_cat == 3 & cvd_sbp_cat == 2 ~ 9,
      cat_multi2 == 6 & cvd_bmi_cat == 3 & cvd_sbp_cat == 3 ~ 12,
      cat_multi2 == 6 & cvd_bmi_cat == 3 & cvd_sbp_cat == 4 ~ 14,
      cat_multi2 == 6 & cvd_bmi_cat == 3 & cvd_sbp_cat == 5 ~ 18,
      
      cat_multi2 == 6 & cvd_bmi_cat == 4 & cvd_sbp_cat == 1 ~ 8,
      cat_multi2 == 6 & cvd_bmi_cat == 4 & cvd_sbp_cat == 2 ~ 10,
      cat_multi2 == 6 & cvd_bmi_cat == 4 & cvd_sbp_cat == 3 ~ 13,
      cat_multi2 == 6 & cvd_bmi_cat == 4 & cvd_sbp_cat == 4 ~ 16,
      cat_multi2 == 6 & cvd_bmi_cat == 4 & cvd_sbp_cat == 5 ~ 19,
      
      cat_multi2 == 6 & cvd_bmi_cat == 5 & cvd_sbp_cat == 1 ~ 9,
      cat_multi2 == 6 & cvd_bmi_cat == 5 & cvd_sbp_cat == 2 ~ 11,
      cat_multi2 == 6 & cvd_bmi_cat == 5 & cvd_sbp_cat == 3 ~ 14,
      cat_multi2 == 6 & cvd_bmi_cat == 5 & cvd_sbp_cat == 4 ~ 17,
      cat_multi2 == 6 & cvd_bmi_cat == 5 & cvd_sbp_cat == 5 ~ 21,
      
      # male, no smoking, age 70-74
      cat_multi2 == 7 & cvd_bmi_cat == 1 & cvd_sbp_cat == 1 ~ 9,
      cat_multi2 == 7 & cvd_bmi_cat == 1 & cvd_sbp_cat == 2 ~ 11,
      cat_multi2 == 7 & cvd_bmi_cat == 1 & cvd_sbp_cat == 3 ~ 13,
      cat_multi2 == 7 & cvd_bmi_cat == 1 & cvd_sbp_cat == 4 ~ 16,
      cat_multi2 == 7 & cvd_bmi_cat == 1 & cvd_sbp_cat == 5 ~ 20,
      
      cat_multi2 == 7 & cvd_bmi_cat == 2 & cvd_sbp_cat == 1 ~ 10,
      cat_multi2 == 7 & cvd_bmi_cat == 2 & cvd_sbp_cat == 2 ~ 12,
      cat_multi2 == 7 & cvd_bmi_cat == 2 & cvd_sbp_cat == 3 ~ 14,
      cat_multi2 == 7 & cvd_bmi_cat == 2 & cvd_sbp_cat == 4 ~ 17,
      cat_multi2 == 7 & cvd_bmi_cat == 2 & cvd_sbp_cat == 5 ~ 21,
      
      cat_multi2 == 7 & cvd_bmi_cat == 3 & cvd_sbp_cat == 1 ~ 10,
      cat_multi2 == 7 & cvd_bmi_cat == 3 & cvd_sbp_cat == 2 ~ 12,
      cat_multi2 == 7 & cvd_bmi_cat == 3 & cvd_sbp_cat == 3 ~ 15,
      cat_multi2 == 7 & cvd_bmi_cat == 3 & cvd_sbp_cat == 4 ~ 18,
      cat_multi2 == 7 & cvd_bmi_cat == 3 & cvd_sbp_cat == 5 ~ 22,
      
      cat_multi2 == 7 & cvd_bmi_cat == 4 & cvd_sbp_cat == 1 ~ 11,
      cat_multi2 == 7 & cvd_bmi_cat == 4 & cvd_sbp_cat == 2 ~ 13,
      cat_multi2 == 7 & cvd_bmi_cat == 4 & cvd_sbp_cat == 3 ~ 16,
      cat_multi2 == 7 & cvd_bmi_cat == 4 & cvd_sbp_cat == 4 ~ 19,
      cat_multi2 == 7 & cvd_bmi_cat == 4 & cvd_sbp_cat == 5 ~ 23,
      
      cat_multi2 == 7 & cvd_bmi_cat == 5 & cvd_sbp_cat == 1 ~ 12,
      cat_multi2 == 7 & cvd_bmi_cat == 5 & cvd_sbp_cat == 2 ~ 14,
      cat_multi2 == 7 & cvd_bmi_cat == 5 & cvd_sbp_cat == 3 ~ 17,
      cat_multi2 == 7 & cvd_bmi_cat == 5 & cvd_sbp_cat == 4 ~ 21,
      cat_multi2 == 7 & cvd_bmi_cat == 5 & cvd_sbp_cat == 5 ~ 25,
      
      # male, with smoking, age 40-44
      cat_multi2 == 8 & cvd_bmi_cat == 1 & cvd_sbp_cat == 1 ~ 2,
      cat_multi2 == 8 & cvd_bmi_cat == 1 & cvd_sbp_cat == 2 ~ 3,
      cat_multi2 == 8 & cvd_bmi_cat == 1 & cvd_sbp_cat == 3 ~ 4,
      cat_multi2 == 8 & cvd_bmi_cat == 1 & cvd_sbp_cat == 4 ~ 6,
      cat_multi2 == 8 & cvd_bmi_cat == 1 & cvd_sbp_cat == 5 ~ 8,
      
      cat_multi2 == 8 & cvd_bmi_cat == 2 & cvd_sbp_cat == 1 ~ 3,
      cat_multi2 == 8 & cvd_bmi_cat == 2 & cvd_sbp_cat == 2 ~ 4,
      cat_multi2 == 8 & cvd_bmi_cat == 2 & cvd_sbp_cat == 3 ~ 5,
      cat_multi2 == 8 & cvd_bmi_cat == 2 & cvd_sbp_cat == 4 ~ 7,
      cat_multi2 == 8 & cvd_bmi_cat == 2 & cvd_sbp_cat == 5 ~ 10,
      
      cat_multi2 == 8 & cvd_bmi_cat == 3 & cvd_sbp_cat == 1 ~ 3,
      cat_multi2 == 8 & cvd_bmi_cat == 3 & cvd_sbp_cat == 2 ~ 4,
      cat_multi2 == 8 & cvd_bmi_cat == 3 & cvd_sbp_cat == 3 ~ 6,
      cat_multi2 == 8 & cvd_bmi_cat == 3 & cvd_sbp_cat == 4 ~ 8,
      cat_multi2 == 8 & cvd_bmi_cat == 3 & cvd_sbp_cat == 5 ~ 11,
      
      cat_multi2 == 8 & cvd_bmi_cat == 4 & cvd_sbp_cat == 1 ~ 4,
      cat_multi2 == 8 & cvd_bmi_cat == 4 & cvd_sbp_cat == 2 ~ 5,
      cat_multi2 == 8 & cvd_bmi_cat == 4 & cvd_sbp_cat == 3 ~ 7,
      cat_multi2 == 8 & cvd_bmi_cat == 4 & cvd_sbp_cat == 4 ~ 9,
      cat_multi2 == 8 & cvd_bmi_cat == 4 & cvd_sbp_cat == 5 ~ 13,
      
      cat_multi2 == 8 & cvd_bmi_cat == 5 & cvd_sbp_cat == 1 ~ 4,
      cat_multi2 == 8 & cvd_bmi_cat == 5 & cvd_sbp_cat == 2 ~ 6,
      cat_multi2 == 8 & cvd_bmi_cat == 5 & cvd_sbp_cat == 3 ~ 8,
      cat_multi2 == 8 & cvd_bmi_cat == 5 & cvd_sbp_cat == 4 ~ 11,
      cat_multi2 == 8 & cvd_bmi_cat == 5 & cvd_sbp_cat == 5 ~ 15,
      
      # male, with smoking, age 45-49
      cat_multi2 == 9 & cvd_bmi_cat == 1 & cvd_sbp_cat == 1 ~ 3,
      cat_multi2 == 9 & cvd_bmi_cat == 1 & cvd_sbp_cat == 2 ~ 4,
      cat_multi2 == 9 & cvd_bmi_cat == 1 & cvd_sbp_cat == 3 ~ 6,
      cat_multi2 == 9 & cvd_bmi_cat == 1 & cvd_sbp_cat == 4 ~ 8,
      cat_multi2 == 9 & cvd_bmi_cat == 1 & cvd_sbp_cat == 5 ~ 10,
      
      cat_multi2 == 9 & cvd_bmi_cat == 2 & cvd_sbp_cat == 1 ~ 3,
      cat_multi2 == 9 & cvd_bmi_cat == 2 & cvd_sbp_cat == 2 ~ 5,
      cat_multi2 == 9 & cvd_bmi_cat == 2 & cvd_sbp_cat == 3 ~ 6,
      cat_multi2 == 9 & cvd_bmi_cat == 2 & cvd_sbp_cat == 4 ~ 9,
      cat_multi2 == 9 & cvd_bmi_cat == 2 & cvd_sbp_cat == 5 ~ 12,
      
      cat_multi2 == 9 & cvd_bmi_cat == 3 & cvd_sbp_cat == 1 ~ 4,
      cat_multi2 == 9 & cvd_bmi_cat == 3 & cvd_sbp_cat == 2 ~ 5,
      cat_multi2 == 9 & cvd_bmi_cat == 3 & cvd_sbp_cat == 3 ~ 7,
      cat_multi2 == 9 & cvd_bmi_cat == 3 & cvd_sbp_cat == 4 ~ 10,
      cat_multi2 == 9 & cvd_bmi_cat == 3 & cvd_sbp_cat == 5 ~ 13,
      
      cat_multi2 == 9 & cvd_bmi_cat == 4 & cvd_sbp_cat == 1 ~ 5,
      cat_multi2 == 9 & cvd_bmi_cat == 4 & cvd_sbp_cat == 2 ~ 6,
      cat_multi2 == 9 & cvd_bmi_cat == 4 & cvd_sbp_cat == 3 ~ 8,
      cat_multi2 == 9 & cvd_bmi_cat == 4 & cvd_sbp_cat == 4 ~ 11,
      cat_multi2 == 9 & cvd_bmi_cat == 4 & cvd_sbp_cat == 5 ~ 15,
      
      cat_multi2 == 9 & cvd_bmi_cat == 5 & cvd_sbp_cat == 1 ~ 5,
      cat_multi2 == 9 & cvd_bmi_cat == 5 & cvd_sbp_cat == 2 ~ 7,
      cat_multi2 == 9 & cvd_bmi_cat == 5 & cvd_sbp_cat == 3 ~ 10,
      cat_multi2 == 9 & cvd_bmi_cat == 5 & cvd_sbp_cat == 4 ~ 13,
      cat_multi2 == 9 & cvd_bmi_cat == 5 & cvd_sbp_cat == 5 ~ 17,
      
      # male, with smoking, age 50-54
      cat_multi2 == 10 & cvd_bmi_cat == 1 & cvd_sbp_cat == 1 ~ 4,
      cat_multi2 == 10 & cvd_bmi_cat == 1 & cvd_sbp_cat == 2 ~ 5,
      cat_multi2 == 10 & cvd_bmi_cat == 1 & cvd_sbp_cat == 3 ~ 7,
      cat_multi2 == 10 & cvd_bmi_cat == 1 & cvd_sbp_cat == 4 ~ 9,
      cat_multi2 == 10 & cvd_bmi_cat == 1 & cvd_sbp_cat == 5 ~ 12,
      
      cat_multi2 == 10 & cvd_bmi_cat == 2 & cvd_sbp_cat == 1 ~ 4,
      cat_multi2 == 10 & cvd_bmi_cat == 2 & cvd_sbp_cat == 2 ~ 6,
      cat_multi2 == 10 & cvd_bmi_cat == 2 & cvd_sbp_cat == 3 ~ 8,
      cat_multi2 == 10 & cvd_bmi_cat == 2 & cvd_sbp_cat == 4 ~ 10,
      cat_multi2 == 10 & cvd_bmi_cat == 2 & cvd_sbp_cat == 5 ~ 14,
      
      cat_multi2 == 10 & cvd_bmi_cat == 3 & cvd_sbp_cat == 1 ~ 5,
      cat_multi2 == 10 & cvd_bmi_cat == 3 & cvd_sbp_cat == 2 ~ 7,
      cat_multi2 == 10 & cvd_bmi_cat == 3 & cvd_sbp_cat == 3 ~ 9,
      cat_multi2 == 10 & cvd_bmi_cat == 3 & cvd_sbp_cat == 4 ~ 12,
      cat_multi2 == 10 & cvd_bmi_cat == 3 & cvd_sbp_cat == 5 ~ 15,
      
      cat_multi2 == 10 & cvd_bmi_cat == 4 & cvd_sbp_cat == 1 ~ 6,
      cat_multi2 == 10 & cvd_bmi_cat == 4 & cvd_sbp_cat == 2 ~ 8,
      cat_multi2 == 10 & cvd_bmi_cat == 4 & cvd_sbp_cat == 3 ~ 10,
      cat_multi2 == 10 & cvd_bmi_cat == 4 & cvd_sbp_cat == 4 ~ 13,
      cat_multi2 == 10 & cvd_bmi_cat == 4 & cvd_sbp_cat == 5 ~ 17,
      
      cat_multi2 == 10 & cvd_bmi_cat == 5 & cvd_sbp_cat == 1 ~ 6,
      cat_multi2 == 10 & cvd_bmi_cat == 5 & cvd_sbp_cat == 2 ~ 9,
      cat_multi2 == 10 & cvd_bmi_cat == 5 & cvd_sbp_cat == 3 ~ 11,
      cat_multi2 == 10 & cvd_bmi_cat == 5 & cvd_sbp_cat == 4 ~ 15,
      cat_multi2 == 10 & cvd_bmi_cat == 5 & cvd_sbp_cat == 5 ~ 19,
      
      # male, with smoking, age 55-59
      cat_multi2 == 11 & cvd_bmi_cat == 1 & cvd_sbp_cat == 1 ~ 5,
      cat_multi2 == 11 & cvd_bmi_cat == 1 & cvd_sbp_cat == 2 ~ 7,
      cat_multi2 == 11 & cvd_bmi_cat == 1 & cvd_sbp_cat == 3 ~ 9,
      cat_multi2 == 11 & cvd_bmi_cat == 1 & cvd_sbp_cat == 4 ~ 11,
      cat_multi2 == 11 & cvd_bmi_cat == 1 & cvd_sbp_cat == 5 ~ 15,
      
      cat_multi2 == 11 & cvd_bmi_cat == 2 & cvd_sbp_cat == 1 ~ 6,
      cat_multi2 == 11 & cvd_bmi_cat == 2 & cvd_sbp_cat == 2 ~ 7,
      cat_multi2 == 11 & cvd_bmi_cat == 2 & cvd_sbp_cat == 3 ~ 10,
      cat_multi2 == 11 & cvd_bmi_cat == 2 & cvd_sbp_cat == 4 ~ 12,
      cat_multi2 == 11 & cvd_bmi_cat == 2 & cvd_sbp_cat == 5 ~ 16,
      
      cat_multi2 == 11 & cvd_bmi_cat == 3 & cvd_sbp_cat == 1 ~ 6,
      cat_multi2 == 11 & cvd_bmi_cat == 3 & cvd_sbp_cat == 2 ~ 8,
      cat_multi2 == 11 & cvd_bmi_cat == 3 & cvd_sbp_cat == 3 ~ 11,
      cat_multi2 == 11 & cvd_bmi_cat == 3 & cvd_sbp_cat == 4 ~ 14,
      cat_multi2 == 11 & cvd_bmi_cat == 3 & cvd_sbp_cat == 5 ~ 18,
      
      cat_multi2 == 11 & cvd_bmi_cat == 4 & cvd_sbp_cat == 1 ~ 7,
      cat_multi2 == 11 & cvd_bmi_cat == 4 & cvd_sbp_cat == 2 ~ 9,
      cat_multi2 == 11 & cvd_bmi_cat == 4 & cvd_sbp_cat == 3 ~ 12,
      cat_multi2 == 11 & cvd_bmi_cat == 4 & cvd_sbp_cat == 4 ~ 15,
      cat_multi2 == 11 & cvd_bmi_cat == 4 & cvd_sbp_cat == 5 ~ 20,
      
      cat_multi2 == 11 & cvd_bmi_cat == 5 & cvd_sbp_cat == 1 ~ 8,
      cat_multi2 == 11 & cvd_bmi_cat == 5 & cvd_sbp_cat == 2 ~ 10,
      cat_multi2 == 11 & cvd_bmi_cat == 5 & cvd_sbp_cat == 3 ~ 13,
      cat_multi2 == 11 & cvd_bmi_cat == 5 & cvd_sbp_cat == 4 ~ 17,
      cat_multi2 == 11 & cvd_bmi_cat == 5 & cvd_sbp_cat == 5 ~ 22,
      
      # male, with smoking, age 60-64
      cat_multi2 == 12 & cvd_bmi_cat == 1 & cvd_sbp_cat == 1 ~ 7,
      cat_multi2 == 12 & cvd_bmi_cat == 1 & cvd_sbp_cat == 2 ~ 9,
      cat_multi2 == 12 & cvd_bmi_cat == 1 & cvd_sbp_cat == 3 ~ 11,
      cat_multi2 == 12 & cvd_bmi_cat == 1 & cvd_sbp_cat == 4 ~ 14,
      cat_multi2 == 12 & cvd_bmi_cat == 1 & cvd_sbp_cat == 5 ~ 17,
      
      cat_multi2 == 12 & cvd_bmi_cat == 2 & cvd_sbp_cat == 1 ~ 7,
      cat_multi2 == 12 & cvd_bmi_cat == 2 & cvd_sbp_cat == 2 ~ 9,
      cat_multi2 == 12 & cvd_bmi_cat == 2 & cvd_sbp_cat == 3 ~ 12,
      cat_multi2 == 12 & cvd_bmi_cat == 2 & cvd_sbp_cat == 4 ~ 15,
      cat_multi2 == 12 & cvd_bmi_cat == 2 & cvd_sbp_cat == 5 ~ 19,
      
      cat_multi2 == 12 & cvd_bmi_cat == 3 & cvd_sbp_cat == 1 ~ 8,
      cat_multi2 == 12 & cvd_bmi_cat == 3 & cvd_sbp_cat == 2 ~ 10,
      cat_multi2 == 12 & cvd_bmi_cat == 3 & cvd_sbp_cat == 3 ~ 13,
      cat_multi2 == 12 & cvd_bmi_cat == 3 & cvd_sbp_cat == 4 ~ 16,
      cat_multi2 == 12 & cvd_bmi_cat == 3 & cvd_sbp_cat == 5 ~ 21,
      
      cat_multi2 == 12 & cvd_bmi_cat == 4 & cvd_sbp_cat == 1 ~ 9,
      cat_multi2 == 12 & cvd_bmi_cat == 4 & cvd_sbp_cat == 2 ~ 11,
      cat_multi2 == 12 & cvd_bmi_cat == 4 & cvd_sbp_cat == 3 ~ 14,
      cat_multi2 == 12 & cvd_bmi_cat == 4 & cvd_sbp_cat == 4 ~ 18,
      cat_multi2 == 12 & cvd_bmi_cat == 4 & cvd_sbp_cat == 5 ~ 22,
      
      cat_multi2 == 12 & cvd_bmi_cat == 5 & cvd_sbp_cat == 1 ~ 10,
      cat_multi2 == 12 & cvd_bmi_cat == 5 & cvd_sbp_cat == 2 ~ 12,
      cat_multi2 == 12 & cvd_bmi_cat == 5 & cvd_sbp_cat == 3 ~ 16,
      cat_multi2 == 12 & cvd_bmi_cat == 5 & cvd_sbp_cat == 4 ~ 20,
      cat_multi2 == 12 & cvd_bmi_cat == 5 & cvd_sbp_cat == 5 ~ 25,
      
      # male, with smoking, age 65-69
      cat_multi2 == 13 & cvd_bmi_cat == 1 & cvd_sbp_cat == 1 ~ 9,
      cat_multi2 == 13 & cvd_bmi_cat == 1 & cvd_sbp_cat == 2 ~ 11,
      cat_multi2 == 13 & cvd_bmi_cat == 1 & cvd_sbp_cat == 3 ~ 14,
      cat_multi2 == 13 & cvd_bmi_cat == 1 & cvd_sbp_cat == 4 ~ 17,
      cat_multi2 == 13 & cvd_bmi_cat == 1 & cvd_sbp_cat == 5 ~ 21,
      
      cat_multi2 == 13 & cvd_bmi_cat == 2 & cvd_sbp_cat == 1 ~ 10,
      cat_multi2 == 13 & cvd_bmi_cat == 2 & cvd_sbp_cat == 2 ~ 12,
      cat_multi2 == 13 & cvd_bmi_cat == 2 & cvd_sbp_cat == 3 ~ 15,
      cat_multi2 == 13 & cvd_bmi_cat == 2 & cvd_sbp_cat == 4 ~ 18,
      cat_multi2 == 13 & cvd_bmi_cat == 2 & cvd_sbp_cat == 5 ~ 22,
      
      cat_multi2 == 13 & cvd_bmi_cat == 3 & cvd_sbp_cat == 1 ~ 10,
      cat_multi2 == 13 & cvd_bmi_cat == 3 & cvd_sbp_cat == 2 ~ 13,
      cat_multi2 == 13 & cvd_bmi_cat == 3 & cvd_sbp_cat == 3 ~ 16,
      cat_multi2 == 13 & cvd_bmi_cat == 3 & cvd_sbp_cat == 4 ~ 19,
      cat_multi2 == 13 & cvd_bmi_cat == 3 & cvd_sbp_cat == 5 ~ 24,
      
      cat_multi2 == 13 & cvd_bmi_cat == 4 & cvd_sbp_cat == 1 ~ 11,
      cat_multi2 == 13 & cvd_bmi_cat == 4 & cvd_sbp_cat == 2 ~ 14,
      cat_multi2 == 13 & cvd_bmi_cat == 4 & cvd_sbp_cat == 3 ~ 17,
      cat_multi2 == 13 & cvd_bmi_cat == 4 & cvd_sbp_cat == 4 ~ 21,
      cat_multi2 == 13 & cvd_bmi_cat == 4 & cvd_sbp_cat == 5 ~ 26,
      
      cat_multi2 == 13 & cvd_bmi_cat == 5 & cvd_sbp_cat == 1 ~ 12,
      cat_multi2 == 13 & cvd_bmi_cat == 5 & cvd_sbp_cat == 2 ~ 15,
      cat_multi2 == 13 & cvd_bmi_cat == 5 & cvd_sbp_cat == 3 ~ 18,
      cat_multi2 == 13 & cvd_bmi_cat == 5 & cvd_sbp_cat == 4 ~ 23,
      cat_multi2 == 13 & cvd_bmi_cat == 5 & cvd_sbp_cat == 5 ~ 28,
      
      # male, with smoking, age 70-74
      cat_multi2 == 14 & cvd_bmi_cat == 1 & cvd_sbp_cat == 1 ~ 12,
      cat_multi2 == 14 & cvd_bmi_cat == 1 & cvd_sbp_cat == 2 ~ 14,
      cat_multi2 == 14 & cvd_bmi_cat == 1 & cvd_sbp_cat == 3 ~ 17,
      cat_multi2 == 14 & cvd_bmi_cat == 1 & cvd_sbp_cat == 4 ~ 20,
      cat_multi2 == 14 & cvd_bmi_cat == 1 & cvd_sbp_cat == 5 ~ 24,
      
      cat_multi2 == 14 & cvd_bmi_cat == 2 & cvd_sbp_cat == 1 ~ 12,
      cat_multi2 == 14 & cvd_bmi_cat == 2 & cvd_sbp_cat == 2 ~ 15,
      cat_multi2 == 14 & cvd_bmi_cat == 2 & cvd_sbp_cat == 3 ~ 18,
      cat_multi2 == 14 & cvd_bmi_cat == 2 & cvd_sbp_cat == 4 ~ 22,
      cat_multi2 == 14 & cvd_bmi_cat == 2 & cvd_sbp_cat == 5 ~ 26,
      
      cat_multi2 == 14 & cvd_bmi_cat == 3 & cvd_sbp_cat == 1 ~ 13,
      cat_multi2 == 14 & cvd_bmi_cat == 3 & cvd_sbp_cat == 2 ~ 16,
      cat_multi2 == 14 & cvd_bmi_cat == 3 & cvd_sbp_cat == 3 ~ 19,
      cat_multi2 == 14 & cvd_bmi_cat == 3 & cvd_sbp_cat == 4 ~ 23,
      cat_multi2 == 14 & cvd_bmi_cat == 3 & cvd_sbp_cat == 5 ~ 27,
      
      cat_multi2 == 14 & cvd_bmi_cat == 4 & cvd_sbp_cat == 1 ~ 14,
      cat_multi2 == 14 & cvd_bmi_cat == 4 & cvd_sbp_cat == 2 ~ 17,
      cat_multi2 == 14 & cvd_bmi_cat == 4 & cvd_sbp_cat == 3 ~ 20,
      cat_multi2 == 14 & cvd_bmi_cat == 4 & cvd_sbp_cat == 4 ~ 24,
      cat_multi2 == 14 & cvd_bmi_cat == 4 & cvd_sbp_cat == 5 ~ 29,
      
      cat_multi2 == 14 & cvd_bmi_cat == 5 & cvd_sbp_cat == 1 ~ 15,
      cat_multi2 == 14 & cvd_bmi_cat == 5 & cvd_sbp_cat == 2 ~ 18,
      cat_multi2 == 14 & cvd_bmi_cat == 5 & cvd_sbp_cat == 3 ~ 22,
      cat_multi2 == 14 & cvd_bmi_cat == 5 & cvd_sbp_cat == 4 ~ 26,
      cat_multi2 == 14 & cvd_bmi_cat == 5 & cvd_sbp_cat == 5 ~ 31,
      
      # female, no smoking, age 40-44
      cat_multi2 == 15 & cvd_bmi_cat == 1 & cvd_sbp_cat == 1 ~ 1,
      cat_multi2 == 15 & cvd_bmi_cat == 1 & cvd_sbp_cat == 2 ~ 1,
      cat_multi2 == 15 & cvd_bmi_cat == 1 & cvd_sbp_cat == 3 ~ 2,
      cat_multi2 == 15 & cvd_bmi_cat == 1 & cvd_sbp_cat == 4 ~ 3,
      cat_multi2 == 15 & cvd_bmi_cat == 1 & cvd_sbp_cat == 5 ~ 4,
      
      cat_multi2 == 15 & cvd_bmi_cat == 2 & cvd_sbp_cat == 1 ~ 1,
      cat_multi2 == 15 & cvd_bmi_cat == 2 & cvd_sbp_cat == 2 ~ 2,
      cat_multi2 == 15 & cvd_bmi_cat == 2 & cvd_sbp_cat == 3 ~ 2,
      cat_multi2 == 15 & cvd_bmi_cat == 2 & cvd_sbp_cat == 4 ~ 3,
      cat_multi2 == 15 & cvd_bmi_cat == 2 & cvd_sbp_cat == 5 ~ 4,
      
      cat_multi2 == 15 & cvd_bmi_cat == 3 & cvd_sbp_cat == 1 ~ 1,
      cat_multi2 == 15 & cvd_bmi_cat == 3 & cvd_sbp_cat == 2 ~ 2,
      cat_multi2 == 15 & cvd_bmi_cat == 3 & cvd_sbp_cat == 3 ~ 2,
      cat_multi2 == 15 & cvd_bmi_cat == 3 & cvd_sbp_cat == 4 ~ 3,
      cat_multi2 == 15 & cvd_bmi_cat == 3 & cvd_sbp_cat == 5 ~ 4,
      
      cat_multi2 == 15 & cvd_bmi_cat == 4 & cvd_sbp_cat == 1 ~ 1,
      cat_multi2 == 15 & cvd_bmi_cat == 4 & cvd_sbp_cat == 2 ~ 2,
      cat_multi2 == 15 & cvd_bmi_cat == 4 & cvd_sbp_cat == 3 ~ 2,
      cat_multi2 == 15 & cvd_bmi_cat == 4 & cvd_sbp_cat == 4 ~ 3,
      cat_multi2 == 15 & cvd_bmi_cat == 4 & cvd_sbp_cat == 5 ~ 4,
      
      cat_multi2 == 15 & cvd_bmi_cat == 5 & cvd_sbp_cat == 1 ~ 1,
      cat_multi2 == 15 & cvd_bmi_cat == 5 & cvd_sbp_cat == 2 ~ 2,
      cat_multi2 == 15 & cvd_bmi_cat == 5 & cvd_sbp_cat == 3 ~ 2,
      cat_multi2 == 15 & cvd_bmi_cat == 5 & cvd_sbp_cat == 4 ~ 3,
      cat_multi2 == 15 & cvd_bmi_cat == 5 & cvd_sbp_cat == 5 ~ 4,
      
      # female, no smoking, age 45-49
      cat_multi2 == 16 & cvd_bmi_cat == 1 & cvd_sbp_cat == 1 ~ 1,
      cat_multi2 == 16 & cvd_bmi_cat == 1 & cvd_sbp_cat == 2 ~ 2,
      cat_multi2 == 16 & cvd_bmi_cat == 1 & cvd_sbp_cat == 3 ~ 3,
      cat_multi2 == 16 & cvd_bmi_cat == 1 & cvd_sbp_cat == 4 ~ 3,
      cat_multi2 == 16 & cvd_bmi_cat == 1 & cvd_sbp_cat == 5 ~ 5,
      
      cat_multi2 == 16 & cvd_bmi_cat == 2 & cvd_sbp_cat == 1 ~ 2,
      cat_multi2 == 16 & cvd_bmi_cat == 2 & cvd_sbp_cat == 2 ~ 2,
      cat_multi2 == 16 & cvd_bmi_cat == 2 & cvd_sbp_cat == 3 ~ 3,
      cat_multi2 == 16 & cvd_bmi_cat == 2 & cvd_sbp_cat == 4 ~ 4,
      cat_multi2 == 16 & cvd_bmi_cat == 2 & cvd_sbp_cat == 5 ~ 5,
      
      cat_multi2 == 16 & cvd_bmi_cat == 3 & cvd_sbp_cat == 1 ~ 2,
      cat_multi2 == 16 & cvd_bmi_cat == 3 & cvd_sbp_cat == 2 ~ 2,
      cat_multi2 == 16 & cvd_bmi_cat == 3 & cvd_sbp_cat == 3 ~ 3,
      cat_multi2 == 16 & cvd_bmi_cat == 3 & cvd_sbp_cat == 4 ~ 4,
      cat_multi2 == 16 & cvd_bmi_cat == 3 & cvd_sbp_cat == 5 ~ 5,
      
      cat_multi2 == 16 & cvd_bmi_cat == 4 & cvd_sbp_cat == 1 ~ 2,
      cat_multi2 == 16 & cvd_bmi_cat == 4 & cvd_sbp_cat == 2 ~ 2,
      cat_multi2 == 16 & cvd_bmi_cat == 4 & cvd_sbp_cat == 3 ~ 3,
      cat_multi2 == 16 & cvd_bmi_cat == 4 & cvd_sbp_cat == 4 ~ 4,
      cat_multi2 == 16 & cvd_bmi_cat == 4 & cvd_sbp_cat == 5 ~ 5,
      
      cat_multi2 == 16 & cvd_bmi_cat == 5 & cvd_sbp_cat == 1 ~ 2,
      cat_multi2 == 16 & cvd_bmi_cat == 5 & cvd_sbp_cat == 2 ~ 2,
      cat_multi2 == 16 & cvd_bmi_cat == 5 & cvd_sbp_cat == 3 ~ 3,
      cat_multi2 == 16 & cvd_bmi_cat == 5 & cvd_sbp_cat == 4 ~ 4,
      cat_multi2 == 16 & cvd_bmi_cat == 5 & cvd_sbp_cat == 5 ~ 5,
      
      # female, no smoking, age 50-54
      cat_multi2 == 17 & cvd_bmi_cat == 1 & cvd_sbp_cat == 1 ~ 2,
      cat_multi2 == 17 & cvd_bmi_cat == 1 & cvd_sbp_cat == 2 ~ 3,
      cat_multi2 == 17 & cvd_bmi_cat == 1 & cvd_sbp_cat == 3 ~ 3,
      cat_multi2 == 17 & cvd_bmi_cat == 1 & cvd_sbp_cat == 4 ~ 4,
      cat_multi2 == 17 & cvd_bmi_cat == 1 & cvd_sbp_cat == 5 ~ 6,
      
      cat_multi2 == 17 & cvd_bmi_cat == 2 & cvd_sbp_cat == 1 ~ 2,
      cat_multi2 == 17 & cvd_bmi_cat == 2 & cvd_sbp_cat == 2 ~ 3,
      cat_multi2 == 17 & cvd_bmi_cat == 2 & cvd_sbp_cat == 3 ~ 3,
      cat_multi2 == 17 & cvd_bmi_cat == 2 & cvd_sbp_cat == 4 ~ 5,
      cat_multi2 == 17 & cvd_bmi_cat == 2 & cvd_sbp_cat == 5 ~ 6,
      
      cat_multi2 == 17 & cvd_bmi_cat == 3 & cvd_sbp_cat == 1 ~ 2,
      cat_multi2 == 17 & cvd_bmi_cat == 3 & cvd_sbp_cat == 2 ~ 3,
      cat_multi2 == 17 & cvd_bmi_cat == 3 & cvd_sbp_cat == 3 ~ 4,
      cat_multi2 == 17 & cvd_bmi_cat == 3 & cvd_sbp_cat == 4 ~ 5,
      cat_multi2 == 17 & cvd_bmi_cat == 3 & cvd_sbp_cat == 5 ~ 6,
      
      cat_multi2 == 17 & cvd_bmi_cat == 4 & cvd_sbp_cat == 1 ~ 2,
      cat_multi2 == 17 & cvd_bmi_cat == 4 & cvd_sbp_cat == 2 ~ 3,
      cat_multi2 == 17 & cvd_bmi_cat == 4 & cvd_sbp_cat == 3 ~ 4,
      cat_multi2 == 17 & cvd_bmi_cat == 4 & cvd_sbp_cat == 4 ~ 5,
      cat_multi2 == 17 & cvd_bmi_cat == 4 & cvd_sbp_cat == 5 ~ 6,
      
      cat_multi2 == 17 & cvd_bmi_cat == 5 & cvd_sbp_cat == 1 ~ 2,
      cat_multi2 == 17 & cvd_bmi_cat == 5 & cvd_sbp_cat == 2 ~ 3,
      cat_multi2 == 17 & cvd_bmi_cat == 5 & cvd_sbp_cat == 3 ~ 4,
      cat_multi2 == 17 & cvd_bmi_cat == 5 & cvd_sbp_cat == 4 ~ 5,
      cat_multi2 == 17 & cvd_bmi_cat == 5 & cvd_sbp_cat == 5 ~ 7,
      
      # female, no smoking, age 55-59
      cat_multi2 == 18 & cvd_bmi_cat == 1 & cvd_sbp_cat == 1 ~ 3,
      cat_multi2 == 18 & cvd_bmi_cat == 1 & cvd_sbp_cat == 2 ~ 3,
      cat_multi2 == 18 & cvd_bmi_cat == 1 & cvd_sbp_cat == 3 ~ 4,
      cat_multi2 == 18 & cvd_bmi_cat == 1 & cvd_sbp_cat == 4 ~ 6,
      cat_multi2 == 18 & cvd_bmi_cat == 1 & cvd_sbp_cat == 5 ~ 7,
      
      cat_multi2 == 18 & cvd_bmi_cat == 2 & cvd_sbp_cat == 1 ~ 3,
      cat_multi2 == 18 & cvd_bmi_cat == 2 & cvd_sbp_cat == 2 ~ 4,
      cat_multi2 == 18 & cvd_bmi_cat == 2 & cvd_sbp_cat == 3 ~ 5,
      cat_multi2 == 18 & cvd_bmi_cat == 2 & cvd_sbp_cat == 4 ~ 6,
      cat_multi2 == 18 & cvd_bmi_cat == 2 & cvd_sbp_cat == 5 ~ 7,
      
      cat_multi2 == 18 & cvd_bmi_cat == 3 & cvd_sbp_cat == 1 ~ 3,
      cat_multi2 == 18 & cvd_bmi_cat == 3 & cvd_sbp_cat == 2 ~ 4,
      cat_multi2 == 18 & cvd_bmi_cat == 3 & cvd_sbp_cat == 3 ~ 5,
      cat_multi2 == 18 & cvd_bmi_cat == 3 & cvd_sbp_cat == 4 ~ 6,
      cat_multi2 == 18 & cvd_bmi_cat == 3 & cvd_sbp_cat == 5 ~ 8,
      
      cat_multi2 == 18 & cvd_bmi_cat == 4 & cvd_sbp_cat == 1 ~ 3,
      cat_multi2 == 18 & cvd_bmi_cat == 4 & cvd_sbp_cat == 2 ~ 4,
      cat_multi2 == 18 & cvd_bmi_cat == 4 & cvd_sbp_cat == 3 ~ 5,
      cat_multi2 == 18 & cvd_bmi_cat == 4 & cvd_sbp_cat == 4 ~ 6,
      cat_multi2 == 18 & cvd_bmi_cat == 4 & cvd_sbp_cat == 5 ~ 8,
      
      cat_multi2 == 18 & cvd_bmi_cat == 5 & cvd_sbp_cat == 1 ~ 3,
      cat_multi2 == 18 & cvd_bmi_cat == 5 & cvd_sbp_cat == 2 ~ 4,
      cat_multi2 == 18 & cvd_bmi_cat == 5 & cvd_sbp_cat == 3 ~ 5,
      cat_multi2 == 18 & cvd_bmi_cat == 5 & cvd_sbp_cat == 4 ~ 7,
      cat_multi2 == 18 & cvd_bmi_cat == 5 & cvd_sbp_cat == 5 ~ 8,
      
      # female, no smoking, age 60-64
      cat_multi2 == 19 & cvd_bmi_cat == 1 & cvd_sbp_cat == 1 ~ 4,
      cat_multi2 == 19 & cvd_bmi_cat == 1 & cvd_sbp_cat == 2 ~ 5,
      cat_multi2 == 19 & cvd_bmi_cat == 1 & cvd_sbp_cat == 3 ~ 6,
      cat_multi2 == 19 & cvd_bmi_cat == 1 & cvd_sbp_cat == 4 ~ 7,
      cat_multi2 == 19 & cvd_bmi_cat == 1 & cvd_sbp_cat == 5 ~ 9,
      
      cat_multi2 == 19 & cvd_bmi_cat == 2 & cvd_sbp_cat == 1 ~ 4,
      cat_multi2 == 19 & cvd_bmi_cat == 2 & cvd_sbp_cat == 2 ~ 5,
      cat_multi2 == 19 & cvd_bmi_cat == 2 & cvd_sbp_cat == 3 ~ 6,
      cat_multi2 == 19 & cvd_bmi_cat == 2 & cvd_sbp_cat == 4 ~ 7,
      cat_multi2 == 19 & cvd_bmi_cat == 2 & cvd_sbp_cat == 5 ~ 9,
      
      cat_multi2 == 19 & cvd_bmi_cat == 3 & cvd_sbp_cat == 1 ~ 4,
      cat_multi2 == 19 & cvd_bmi_cat == 3 & cvd_sbp_cat == 2 ~ 5,
      cat_multi2 == 19 & cvd_bmi_cat == 3 & cvd_sbp_cat == 3 ~ 6,
      cat_multi2 == 19 & cvd_bmi_cat == 3 & cvd_sbp_cat == 4 ~ 8,
      cat_multi2 == 19 & cvd_bmi_cat == 3 & cvd_sbp_cat == 5 ~ 9,
      
      cat_multi2 == 19 & cvd_bmi_cat == 4 & cvd_sbp_cat == 1 ~ 4,
      cat_multi2 == 19 & cvd_bmi_cat == 4 & cvd_sbp_cat == 2 ~ 5,
      cat_multi2 == 19 & cvd_bmi_cat == 4 & cvd_sbp_cat == 3 ~ 6,
      cat_multi2 == 19 & cvd_bmi_cat == 4 & cvd_sbp_cat == 4 ~ 8,
      cat_multi2 == 19 & cvd_bmi_cat == 4 & cvd_sbp_cat == 5 ~ 10,
      
      cat_multi2 == 19 & cvd_bmi_cat == 5 & cvd_sbp_cat == 1 ~ 4,
      cat_multi2 == 19 & cvd_bmi_cat == 5 & cvd_sbp_cat == 2 ~ 5,
      cat_multi2 == 19 & cvd_bmi_cat == 5 & cvd_sbp_cat == 3 ~ 7,
      cat_multi2 == 19 & cvd_bmi_cat == 5 & cvd_sbp_cat == 4 ~ 8,
      cat_multi2 == 19 & cvd_bmi_cat == 5 & cvd_sbp_cat == 5 ~ 10,
      
      # female, no smoking, age 65-69
      cat_multi2 == 20 & cvd_bmi_cat == 1 & cvd_sbp_cat == 1 ~ 5,
      cat_multi2 == 20 & cvd_bmi_cat == 1 & cvd_sbp_cat == 2 ~ 6,
      cat_multi2 == 20 & cvd_bmi_cat == 1 & cvd_sbp_cat == 3 ~ 8,
      cat_multi2 == 20 & cvd_bmi_cat == 1 & cvd_sbp_cat == 4 ~ 9,
      cat_multi2 == 20 & cvd_bmi_cat == 1 & cvd_sbp_cat == 5 ~ 11,
      
      cat_multi2 == 20 & cvd_bmi_cat == 2 & cvd_sbp_cat == 1 ~ 5,
      cat_multi2 == 20 & cvd_bmi_cat == 2 & cvd_sbp_cat == 2 ~ 6,
      cat_multi2 == 20 & cvd_bmi_cat == 2 & cvd_sbp_cat == 3 ~ 8,
      cat_multi2 == 20 & cvd_bmi_cat == 2 & cvd_sbp_cat == 4 ~ 10,
      cat_multi2 == 20 & cvd_bmi_cat == 2 & cvd_sbp_cat == 5 ~ 11,
      
      cat_multi2 == 20 & cvd_bmi_cat == 3 & cvd_sbp_cat == 1 ~ 6,
      cat_multi2 == 20 & cvd_bmi_cat == 3 & cvd_sbp_cat == 2 ~ 7,
      cat_multi2 == 20 & cvd_bmi_cat == 3 & cvd_sbp_cat == 3 ~ 8,
      cat_multi2 == 20 & cvd_bmi_cat == 3 & cvd_sbp_cat == 4 ~ 10,
      cat_multi2 == 20 & cvd_bmi_cat == 3 & cvd_sbp_cat == 5 ~ 12,
      
      cat_multi2 == 20 & cvd_bmi_cat == 4 & cvd_sbp_cat == 1 ~ 6,
      cat_multi2 == 20 & cvd_bmi_cat == 4 & cvd_sbp_cat == 2 ~ 7,
      cat_multi2 == 20 & cvd_bmi_cat == 4 & cvd_sbp_cat == 3 ~ 8,
      cat_multi2 == 20 & cvd_bmi_cat == 4 & cvd_sbp_cat == 4 ~ 10,
      cat_multi2 == 20 & cvd_bmi_cat == 4 & cvd_sbp_cat == 5 ~ 12,
      
      cat_multi2 == 20 & cvd_bmi_cat == 5 & cvd_sbp_cat == 1 ~ 6,
      cat_multi2 == 20 & cvd_bmi_cat == 5 & cvd_sbp_cat == 2 ~ 7,
      cat_multi2 == 20 & cvd_bmi_cat == 5 & cvd_sbp_cat == 3 ~ 9,
      cat_multi2 == 20 & cvd_bmi_cat == 5 & cvd_sbp_cat == 4 ~ 11,
      cat_multi2 == 20 & cvd_bmi_cat == 5 & cvd_sbp_cat == 5 ~ 13,
      
      # female, no smoking, age 70-74
      cat_multi2 == 21 & cvd_bmi_cat == 1 & cvd_sbp_cat == 1 ~ 7,
      cat_multi2 == 21 & cvd_bmi_cat == 1 & cvd_sbp_cat == 2 ~ 8,
      cat_multi2 == 21 & cvd_bmi_cat == 1 & cvd_sbp_cat == 3 ~ 10,
      cat_multi2 == 21 & cvd_bmi_cat == 1 & cvd_sbp_cat == 4 ~ 12,
      cat_multi2 == 21 & cvd_bmi_cat == 1 & cvd_sbp_cat == 5 ~ 14,
      
      cat_multi2 == 21 & cvd_bmi_cat == 2 & cvd_sbp_cat == 1 ~ 7,
      cat_multi2 == 21 & cvd_bmi_cat == 2 & cvd_sbp_cat == 2 ~ 9,
      cat_multi2 == 21 & cvd_bmi_cat == 2 & cvd_sbp_cat == 3 ~ 10,
      cat_multi2 == 21 & cvd_bmi_cat == 2 & cvd_sbp_cat == 4 ~ 12,
      cat_multi2 == 21 & cvd_bmi_cat == 2 & cvd_sbp_cat == 5 ~ 14,
      
      cat_multi2 == 21 & cvd_bmi_cat == 3 & cvd_sbp_cat == 1 ~ 8,
      cat_multi2 == 21 & cvd_bmi_cat == 3 & cvd_sbp_cat == 2 ~ 9,
      cat_multi2 == 21 & cvd_bmi_cat == 3 & cvd_sbp_cat == 3 ~ 11,
      cat_multi2 == 21 & cvd_bmi_cat == 3 & cvd_sbp_cat == 4 ~ 13,
      cat_multi2 == 21 & cvd_bmi_cat == 3 & cvd_sbp_cat == 5 ~ 15,
      
      cat_multi2 == 21 & cvd_bmi_cat == 4 & cvd_sbp_cat == 1 ~ 8,
      cat_multi2 == 21 & cvd_bmi_cat == 4 & cvd_sbp_cat == 2 ~ 9,
      cat_multi2 == 21 & cvd_bmi_cat == 4 & cvd_sbp_cat == 3 ~ 11,
      cat_multi2 == 21 & cvd_bmi_cat == 4 & cvd_sbp_cat == 4 ~ 13,
      cat_multi2 == 21 & cvd_bmi_cat == 4 & cvd_sbp_cat == 5 ~ 15,
      
      cat_multi2 == 21 & cvd_bmi_cat == 5 & cvd_sbp_cat == 1 ~ 8,
      cat_multi2 == 21 & cvd_bmi_cat == 5 & cvd_sbp_cat == 2 ~ 10,
      cat_multi2 == 21 & cvd_bmi_cat == 5 & cvd_sbp_cat == 3 ~ 11,
      cat_multi2 == 21 & cvd_bmi_cat == 5 & cvd_sbp_cat == 4 ~ 13,
      cat_multi2 == 21 & cvd_bmi_cat == 5 & cvd_sbp_cat == 5 ~ 16,
      
      # female, with smoking, age 40-44
      cat_multi2 == 22 & cvd_bmi_cat == 1 & cvd_sbp_cat == 1 ~ 2,
      cat_multi2 == 22 & cvd_bmi_cat == 1 & cvd_sbp_cat == 2 ~ 3,
      cat_multi2 == 22 & cvd_bmi_cat == 1 & cvd_sbp_cat == 3 ~ 4,
      cat_multi2 == 22 & cvd_bmi_cat == 1 & cvd_sbp_cat == 4 ~ 6,
      cat_multi2 == 22 & cvd_bmi_cat == 1 & cvd_sbp_cat == 5 ~ 8,
      
      cat_multi2 == 22 & cvd_bmi_cat == 2 & cvd_sbp_cat == 1 ~ 2,
      cat_multi2 == 22 & cvd_bmi_cat == 2 & cvd_sbp_cat == 2 ~ 3,
      cat_multi2 == 22 & cvd_bmi_cat == 2 & cvd_sbp_cat == 3 ~ 4,
      cat_multi2 == 22 & cvd_bmi_cat == 2 & cvd_sbp_cat == 4 ~ 6,
      cat_multi2 == 22 & cvd_bmi_cat == 2 & cvd_sbp_cat == 5 ~ 8,
      
      cat_multi2 == 22 & cvd_bmi_cat == 3 & cvd_sbp_cat == 1 ~ 3,
      cat_multi2 == 22 & cvd_bmi_cat == 3 & cvd_sbp_cat == 2 ~ 3,
      cat_multi2 == 22 & cvd_bmi_cat == 3 & cvd_sbp_cat == 3 ~ 5,
      cat_multi2 == 22 & cvd_bmi_cat == 3 & cvd_sbp_cat == 4 ~ 6,
      cat_multi2 == 22 & cvd_bmi_cat == 3 & cvd_sbp_cat == 5 ~ 9,
      
      cat_multi2 == 22 & cvd_bmi_cat == 4 & cvd_sbp_cat == 1 ~ 3,
      cat_multi2 == 22 & cvd_bmi_cat == 4 & cvd_sbp_cat == 2 ~ 4,
      cat_multi2 == 22 & cvd_bmi_cat == 4 & cvd_sbp_cat == 3 ~ 5,
      cat_multi2 == 22 & cvd_bmi_cat == 4 & cvd_sbp_cat == 4 ~ 7,
      cat_multi2 == 22 & cvd_bmi_cat == 4 & cvd_sbp_cat == 5 ~ 9,
      
      cat_multi2 == 22 & cvd_bmi_cat == 5 & cvd_sbp_cat == 1 ~ 3,
      cat_multi2 == 22 & cvd_bmi_cat == 5 & cvd_sbp_cat == 2 ~ 4,
      cat_multi2 == 22 & cvd_bmi_cat == 5 & cvd_sbp_cat == 3 ~ 5,
      cat_multi2 == 22 & cvd_bmi_cat == 5 & cvd_sbp_cat == 4 ~ 7,
      cat_multi2 == 22 & cvd_bmi_cat == 5 & cvd_sbp_cat == 5 ~ 10,
      
      # female, with smoking, age 45-49
      cat_multi2 == 23 & cvd_bmi_cat == 1 & cvd_sbp_cat == 1 ~ 3,
      cat_multi2 == 23 & cvd_bmi_cat == 1 & cvd_sbp_cat == 2 ~ 4,
      cat_multi2 == 23 & cvd_bmi_cat == 1 & cvd_sbp_cat == 3 ~ 5,
      cat_multi2 == 23 & cvd_bmi_cat == 1 & cvd_sbp_cat == 4 ~ 7,
      cat_multi2 == 23 & cvd_bmi_cat == 1 & cvd_sbp_cat == 5 ~ 9,
      
      cat_multi2 == 23 & cvd_bmi_cat == 2 & cvd_sbp_cat == 1 ~ 3,
      cat_multi2 == 23 & cvd_bmi_cat == 2 & cvd_sbp_cat == 2 ~ 4,
      cat_multi2 == 23 & cvd_bmi_cat == 2 & cvd_sbp_cat == 3 ~ 5,
      cat_multi2 == 23 & cvd_bmi_cat == 2 & cvd_sbp_cat == 4 ~ 7,
      cat_multi2 == 23 & cvd_bmi_cat == 2 & cvd_sbp_cat == 5 ~ 9,
      
      cat_multi2 == 23 & cvd_bmi_cat == 3 & cvd_sbp_cat == 1 ~ 3,
      cat_multi2 == 23 & cvd_bmi_cat == 3 & cvd_sbp_cat == 2 ~ 4,
      cat_multi2 == 23 & cvd_bmi_cat == 3 & cvd_sbp_cat == 3 ~ 6,
      cat_multi2 == 23 & cvd_bmi_cat == 3 & cvd_sbp_cat == 4 ~ 8,
      cat_multi2 == 23 & cvd_bmi_cat == 3 & cvd_sbp_cat == 5 ~ 10,
      
      cat_multi2 == 23 & cvd_bmi_cat == 4 & cvd_sbp_cat == 1 ~ 3,
      cat_multi2 == 23 & cvd_bmi_cat == 4 & cvd_sbp_cat == 2 ~ 5,
      cat_multi2 == 23 & cvd_bmi_cat == 4 & cvd_sbp_cat == 3 ~ 6,
      cat_multi2 == 23 & cvd_bmi_cat == 4 & cvd_sbp_cat == 4 ~ 8,
      cat_multi2 == 23 & cvd_bmi_cat == 4 & cvd_sbp_cat == 5 ~ 11,
      
      cat_multi2 == 23 & cvd_bmi_cat == 5 & cvd_sbp_cat == 1 ~ 4,
      cat_multi2 == 23 & cvd_bmi_cat == 5 & cvd_sbp_cat == 2 ~ 5,
      cat_multi2 == 23 & cvd_bmi_cat == 5 & cvd_sbp_cat == 3 ~ 6,
      cat_multi2 == 23 & cvd_bmi_cat == 5 & cvd_sbp_cat == 4 ~ 9,
      cat_multi2 == 23 & cvd_bmi_cat == 5 & cvd_sbp_cat == 5 ~ 11,
      
      # female, with smoking, age 50-54
      cat_multi2 == 24 & cvd_bmi_cat == 1 & cvd_sbp_cat == 1 ~ 4,
      cat_multi2 == 24 & cvd_bmi_cat == 1 & cvd_sbp_cat == 2 ~ 5,
      cat_multi2 == 24 & cvd_bmi_cat == 1 & cvd_sbp_cat == 3 ~ 6,
      cat_multi2 == 24 & cvd_bmi_cat == 1 & cvd_sbp_cat == 4 ~ 8,
      cat_multi2 == 24 & cvd_bmi_cat == 1 & cvd_sbp_cat == 5 ~ 10,
      
      cat_multi2 == 24 & cvd_bmi_cat == 2 & cvd_sbp_cat == 1 ~ 4,
      cat_multi2 == 24 & cvd_bmi_cat == 2 & cvd_sbp_cat == 2 ~ 5,
      cat_multi2 == 24 & cvd_bmi_cat == 2 & cvd_sbp_cat == 3 ~ 7,
      cat_multi2 == 24 & cvd_bmi_cat == 2 & cvd_sbp_cat == 4 ~ 9,
      cat_multi2 == 24 & cvd_bmi_cat == 2 & cvd_sbp_cat == 5 ~ 11,
      
      cat_multi2 == 24 & cvd_bmi_cat == 3 & cvd_sbp_cat == 1 ~ 4,
      cat_multi2 == 24 & cvd_bmi_cat == 3 & cvd_sbp_cat == 2 ~ 5,
      cat_multi2 == 24 & cvd_bmi_cat == 3 & cvd_sbp_cat == 3 ~ 7,
      cat_multi2 == 24 & cvd_bmi_cat == 3 & cvd_sbp_cat == 4 ~ 9,
      cat_multi2 == 24 & cvd_bmi_cat == 3 & cvd_sbp_cat == 5 ~ 12,
      
      cat_multi2 == 24 & cvd_bmi_cat == 4 & cvd_sbp_cat == 1 ~ 4,
      cat_multi2 == 24 & cvd_bmi_cat == 4 & cvd_sbp_cat == 2 ~ 6,
      cat_multi2 == 24 & cvd_bmi_cat == 4 & cvd_sbp_cat == 3 ~ 7,
      cat_multi2 == 24 & cvd_bmi_cat == 4 & cvd_sbp_cat == 4 ~ 9,
      cat_multi2 == 24 & cvd_bmi_cat == 4 & cvd_sbp_cat == 5 ~ 12,
      
      cat_multi2 == 24 & cvd_bmi_cat == 5 & cvd_sbp_cat == 1 ~ 5,
      cat_multi2 == 24 & cvd_bmi_cat == 5 & cvd_sbp_cat == 2 ~ 6,
      cat_multi2 == 24 & cvd_bmi_cat == 5 & cvd_sbp_cat == 3 ~ 8,
      cat_multi2 == 24 & cvd_bmi_cat == 5 & cvd_sbp_cat == 4 ~ 10,
      cat_multi2 == 24 & cvd_bmi_cat == 5 & cvd_sbp_cat == 5 ~ 13,
      
      # female, with smoking, age 55-59
      cat_multi2 == 25 & cvd_bmi_cat == 1 & cvd_sbp_cat == 1 ~ 5,
      cat_multi2 == 25 & cvd_bmi_cat == 1 & cvd_sbp_cat == 2 ~ 6,
      cat_multi2 == 25 & cvd_bmi_cat == 1 & cvd_sbp_cat == 3 ~ 8,
      cat_multi2 == 25 & cvd_bmi_cat == 1 & cvd_sbp_cat == 4 ~ 10,
      cat_multi2 == 25 & cvd_bmi_cat == 1 & cvd_sbp_cat == 5 ~ 12,
      
      cat_multi2 == 25 & cvd_bmi_cat == 2 & cvd_sbp_cat == 1 ~ 5,
      cat_multi2 == 25 & cvd_bmi_cat == 2 & cvd_sbp_cat == 2 ~ 6,
      cat_multi2 == 25 & cvd_bmi_cat == 2 & cvd_sbp_cat == 3 ~ 8,
      cat_multi2 == 25 & cvd_bmi_cat == 2 & cvd_sbp_cat == 4 ~ 10,
      cat_multi2 == 25 & cvd_bmi_cat == 2 & cvd_sbp_cat == 5 ~ 13,
      
      cat_multi2 == 25 & cvd_bmi_cat == 3 & cvd_sbp_cat == 1 ~ 5,
      cat_multi2 == 25 & cvd_bmi_cat == 3 & cvd_sbp_cat == 2 ~ 7,
      cat_multi2 == 25 & cvd_bmi_cat == 3 & cvd_sbp_cat == 3 ~ 8,
      cat_multi2 == 25 & cvd_bmi_cat == 3 & cvd_sbp_cat == 4 ~ 11,
      cat_multi2 == 25 & cvd_bmi_cat == 3 & cvd_sbp_cat == 5 ~ 13,
      
      cat_multi2 == 25 & cvd_bmi_cat == 4 & cvd_sbp_cat == 1 ~ 6,
      cat_multi2 == 25 & cvd_bmi_cat == 4 & cvd_sbp_cat == 2 ~ 7,
      cat_multi2 == 25 & cvd_bmi_cat == 4 & cvd_sbp_cat == 3 ~ 9,
      cat_multi2 == 25 & cvd_bmi_cat == 4 & cvd_sbp_cat == 4 ~ 11,
      cat_multi2 == 25 & cvd_bmi_cat == 4 & cvd_sbp_cat == 5 ~ 14,
      
      cat_multi2 == 25 & cvd_bmi_cat == 5 & cvd_sbp_cat == 1 ~ 6,
      cat_multi2 == 25 & cvd_bmi_cat == 5 & cvd_sbp_cat == 2 ~ 7,
      cat_multi2 == 25 & cvd_bmi_cat == 5 & cvd_sbp_cat == 3 ~ 9,
      cat_multi2 == 25 & cvd_bmi_cat == 5 & cvd_sbp_cat == 4 ~ 12,
      cat_multi2 == 25 & cvd_bmi_cat == 5 & cvd_sbp_cat == 5 ~ 15,
      
      # female, with smoking, age 60-64
      cat_multi2 == 26 & cvd_bmi_cat == 1 & cvd_sbp_cat == 1 ~ 6,
      cat_multi2 == 26 & cvd_bmi_cat == 1 & cvd_sbp_cat == 2 ~ 8,
      cat_multi2 == 26 & cvd_bmi_cat == 1 & cvd_sbp_cat == 3 ~ 9,
      cat_multi2 == 26 & cvd_bmi_cat == 1 & cvd_sbp_cat == 4 ~ 12,
      cat_multi2 == 26 & cvd_bmi_cat == 1 & cvd_sbp_cat == 5 ~ 14,
      
      cat_multi2 == 26 & cvd_bmi_cat == 2 & cvd_sbp_cat == 1 ~ 6,
      cat_multi2 == 26 & cvd_bmi_cat == 2 & cvd_sbp_cat == 2 ~ 8,
      cat_multi2 == 26 & cvd_bmi_cat == 2 & cvd_sbp_cat == 3 ~ 10,
      cat_multi2 == 26 & cvd_bmi_cat == 2 & cvd_sbp_cat == 4 ~ 12,
      cat_multi2 == 26 & cvd_bmi_cat == 2 & cvd_sbp_cat == 5 ~ 15,
      
      cat_multi2 == 26 & cvd_bmi_cat == 3 & cvd_sbp_cat == 1 ~ 7,
      cat_multi2 == 26 & cvd_bmi_cat == 3 & cvd_sbp_cat == 2 ~ 8,
      cat_multi2 == 26 & cvd_bmi_cat == 3 & cvd_sbp_cat == 3 ~ 10,
      cat_multi2 == 26 & cvd_bmi_cat == 3 & cvd_sbp_cat == 4 ~ 13,
      cat_multi2 == 26 & cvd_bmi_cat == 3 & cvd_sbp_cat == 5 ~ 15,
      
      cat_multi2 == 26 & cvd_bmi_cat == 4 & cvd_sbp_cat == 1 ~ 7,
      cat_multi2 == 26 & cvd_bmi_cat == 4 & cvd_sbp_cat == 2 ~ 9,
      cat_multi2 == 26 & cvd_bmi_cat == 4 & cvd_sbp_cat == 3 ~ 11,
      cat_multi2 == 26 & cvd_bmi_cat == 4 & cvd_sbp_cat == 4 ~ 13,
      cat_multi2 == 26 & cvd_bmi_cat == 4 & cvd_sbp_cat == 5 ~ 16,
      
      cat_multi2 == 26 & cvd_bmi_cat == 5 & cvd_sbp_cat == 1 ~ 7,
      cat_multi2 == 26 & cvd_bmi_cat == 5 & cvd_sbp_cat == 2 ~ 9,
      cat_multi2 == 26 & cvd_bmi_cat == 5 & cvd_sbp_cat == 3 ~ 11,
      cat_multi2 == 26 & cvd_bmi_cat == 5 & cvd_sbp_cat == 4 ~ 14,
      cat_multi2 == 26 & cvd_bmi_cat == 5 & cvd_sbp_cat == 5 ~ 17,
      
      # female, with smoking, age 65-69
      cat_multi2 == 27 & cvd_bmi_cat == 1 & cvd_sbp_cat == 1 ~ 8,
      cat_multi2 == 27 & cvd_bmi_cat == 1 & cvd_sbp_cat == 2 ~ 9,
      cat_multi2 == 27 & cvd_bmi_cat == 1 & cvd_sbp_cat == 3 ~ 11,
      cat_multi2 == 27 & cvd_bmi_cat == 1 & cvd_sbp_cat == 4 ~ 14,
      cat_multi2 == 27 & cvd_bmi_cat == 1 & cvd_sbp_cat == 5 ~ 17,
      
      cat_multi2 == 27 & cvd_bmi_cat == 2 & cvd_sbp_cat == 1 ~ 8,
      cat_multi2 == 27 & cvd_bmi_cat == 2 & cvd_sbp_cat == 2 ~ 10,
      cat_multi2 == 27 & cvd_bmi_cat == 2 & cvd_sbp_cat == 3 ~ 12,
      cat_multi2 == 27 & cvd_bmi_cat == 2 & cvd_sbp_cat == 4 ~ 14,
      cat_multi2 == 27 & cvd_bmi_cat == 2 & cvd_sbp_cat == 5 ~ 17,
      
      cat_multi2 == 27 & cvd_bmi_cat == 3 & cvd_sbp_cat == 1 ~ 8,
      cat_multi2 == 27 & cvd_bmi_cat == 3 & cvd_sbp_cat == 2 ~ 10,
      cat_multi2 == 27 & cvd_bmi_cat == 3 & cvd_sbp_cat == 3 ~ 12,
      cat_multi2 == 27 & cvd_bmi_cat == 3 & cvd_sbp_cat == 4 ~ 15,
      cat_multi2 == 27 & cvd_bmi_cat == 3 & cvd_sbp_cat == 5 ~ 18,
      
      cat_multi2 == 27 & cvd_bmi_cat == 4 & cvd_sbp_cat == 1 ~ 9,
      cat_multi2 == 27 & cvd_bmi_cat == 4 & cvd_sbp_cat == 2 ~ 11,
      cat_multi2 == 27 & cvd_bmi_cat == 4 & cvd_sbp_cat == 3 ~ 13,
      cat_multi2 == 27 & cvd_bmi_cat == 4 & cvd_sbp_cat == 4 ~ 15,
      cat_multi2 == 27 & cvd_bmi_cat == 4 & cvd_sbp_cat == 5 ~ 19,
      
      cat_multi2 == 27 & cvd_bmi_cat == 5 & cvd_sbp_cat == 1 ~ 9,
      cat_multi2 == 27 & cvd_bmi_cat == 5 & cvd_sbp_cat == 2 ~ 11,
      cat_multi2 == 27 & cvd_bmi_cat == 5 & cvd_sbp_cat == 3 ~ 13,
      cat_multi2 == 27 & cvd_bmi_cat == 5 & cvd_sbp_cat == 4 ~ 16,
      cat_multi2 == 27 & cvd_bmi_cat == 5 & cvd_sbp_cat == 5 ~ 19,
      
      # female, with smoking, age 70-74
      cat_multi2 == 28 & cvd_bmi_cat == 1 & cvd_sbp_cat == 1 ~ 10,
      cat_multi2 == 28 & cvd_bmi_cat == 1 & cvd_sbp_cat == 2 ~ 12,
      cat_multi2 == 28 & cvd_bmi_cat == 1 & cvd_sbp_cat == 3 ~ 14,
      cat_multi2 == 28 & cvd_bmi_cat == 1 & cvd_sbp_cat == 4 ~ 16,
      cat_multi2 == 28 & cvd_bmi_cat == 1 & cvd_sbp_cat == 5 ~ 19,
      
      cat_multi2 == 28 & cvd_bmi_cat == 2 & cvd_sbp_cat == 1 ~ 10,
      cat_multi2 == 28 & cvd_bmi_cat == 2 & cvd_sbp_cat == 2 ~ 12,
      cat_multi2 == 28 & cvd_bmi_cat == 2 & cvd_sbp_cat == 3 ~ 14,
      cat_multi2 == 28 & cvd_bmi_cat == 2 & cvd_sbp_cat == 4 ~ 17,
      cat_multi2 == 28 & cvd_bmi_cat == 2 & cvd_sbp_cat == 5 ~ 20,
      
      cat_multi2 == 28 & cvd_bmi_cat == 3 & cvd_sbp_cat == 1 ~ 11,
      cat_multi2 == 28 & cvd_bmi_cat == 3 & cvd_sbp_cat == 2 ~ 13,
      cat_multi2 == 28 & cvd_bmi_cat == 3 & cvd_sbp_cat == 3 ~ 15,
      cat_multi2 == 28 & cvd_bmi_cat == 3 & cvd_sbp_cat == 4 ~ 18,
      cat_multi2 == 28 & cvd_bmi_cat == 3 & cvd_sbp_cat == 5 ~ 21,
      
      cat_multi2 == 28 & cvd_bmi_cat == 4 & cvd_sbp_cat == 1 ~ 11,
      cat_multi2 == 28 & cvd_bmi_cat == 4 & cvd_sbp_cat == 2 ~ 13,
      cat_multi2 == 28 & cvd_bmi_cat == 4 & cvd_sbp_cat == 3 ~ 15,
      cat_multi2 == 28 & cvd_bmi_cat == 4 & cvd_sbp_cat == 4 ~ 18,
      cat_multi2 == 28 & cvd_bmi_cat == 4 & cvd_sbp_cat == 5 ~ 21,
      
      cat_multi2 == 28 & cvd_bmi_cat == 5 & cvd_sbp_cat == 1 ~ 11,
      cat_multi2 == 28 & cvd_bmi_cat == 5 & cvd_sbp_cat == 2 ~ 13,
      cat_multi2 == 28 & cvd_bmi_cat == 5 & cvd_sbp_cat == 3 ~ 16,
      cat_multi2 == 28 & cvd_bmi_cat == 5 & cvd_sbp_cat == 4 ~ 19,
      cat_multi2 == 28 & cvd_bmi_cat == 5 & cvd_sbp_cat == 5 ~ 22
    ))
  
  # creating variable "who_risk_non_lab_perc"
  df <- df %>%
    mutate(who_risk_non_lab_perc = case_when(
      cvd_sex == 1 & cvd_age_cat == 1 & who_risk_non_lab == 1 ~ "0",
      cvd_sex == 1 & cvd_age_cat == 1 & who_risk_non_lab == 2 ~ "20",
      cvd_sex == 1 & cvd_age_cat == 1 & who_risk_non_lab == 3 ~ "73",
      cvd_sex == 1 & cvd_age_cat == 1 & who_risk_non_lab == 4 ~ "91",
      cvd_sex == 1 & cvd_age_cat == 1 & who_risk_non_lab == 5 ~ "97",
      cvd_sex == 1 & cvd_age_cat == 1 & who_risk_non_lab == 6 ~ "98",
      cvd_sex == 1 & cvd_age_cat == 1 & who_risk_non_lab == 7 ~ "99",
      cvd_sex == 1 & cvd_age_cat == 1 & who_risk_non_lab == 8 ~ "99",
      cvd_sex == 1 & cvd_age_cat == 1 & who_risk_non_lab >= 9 ~ "100",
      
      cvd_sex == 1 & cvd_age_cat == 2 & who_risk_non_lab == 2 ~ "0",
      cvd_sex == 1 & cvd_age_cat == 2 & who_risk_non_lab == 3 ~ "38",
      cvd_sex == 1 & cvd_age_cat == 2 & who_risk_non_lab == 4 ~ "76",
      cvd_sex == 1 & cvd_age_cat == 2 & who_risk_non_lab == 5 ~ "86",
      cvd_sex == 1 & cvd_age_cat == 2 & who_risk_non_lab == 6 ~ "95",
      cvd_sex == 1 & cvd_age_cat == 2 & who_risk_non_lab == 7 ~ "98",
      cvd_sex == 1 & cvd_age_cat == 2 & who_risk_non_lab == 8 ~ "99",
      cvd_sex == 1 & cvd_age_cat == 2 & who_risk_non_lab == 9 ~ "99",
      cvd_sex == 1 & cvd_age_cat == 2 & who_risk_non_lab >= 10 ~ "100",
      
      cvd_sex == 1 & cvd_age_cat == 3 & who_risk_non_lab == 2 ~ "0",
      cvd_sex == 1 & cvd_age_cat == 3 & who_risk_non_lab == 3 ~ "1",
      cvd_sex == 1 & cvd_age_cat == 3 & who_risk_non_lab == 4 ~ "31",
      cvd_sex == 1 & cvd_age_cat == 3 & who_risk_non_lab == 5 ~ "70",
      cvd_sex == 1 & cvd_age_cat == 3 & who_risk_non_lab == 6 ~ "81",
      cvd_sex == 1 & cvd_age_cat == 3 & who_risk_non_lab == 7 ~ "88",
      cvd_sex == 1 & cvd_age_cat == 3 & who_risk_non_lab == 8 ~ "93",
      cvd_sex == 1 & cvd_age_cat == 3 & who_risk_non_lab == 9 ~ "96",
      cvd_sex == 1 & cvd_age_cat == 3 & who_risk_non_lab == 10 ~ "98",
      cvd_sex == 1 & cvd_age_cat == 3 & who_risk_non_lab == 11 ~ "99",
      cvd_sex == 1 & cvd_age_cat == 3 & who_risk_non_lab == 12 ~ "99",
      cvd_sex == 1 & cvd_age_cat == 3 & who_risk_non_lab >= 13 ~ "100",
      
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_non_lab == 3 ~ "0",
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_non_lab == 4 ~ "1",
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_non_lab == 5 ~ "25",
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_non_lab == 6 ~ "58",
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_non_lab == 7 ~ "73",
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_non_lab == 8 ~ "86",
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_non_lab == 9 ~ "93",
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_non_lab == 10 ~ "95",
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_non_lab == 11 ~ "96",
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_non_lab == 12 ~ "98",
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_non_lab == 13 ~ "99",
      cvd_sex == 1 & cvd_age_cat == 4 & who_risk_non_lab >= 14 ~ "100",
      
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_non_lab == 5 ~ "0",
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_non_lab == 6 ~ "10",
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_non_lab == 7 ~ "37",
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_non_lab == 8 ~ "59",
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_non_lab == 9 ~ "74",
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_non_lab == 10 ~ "84",
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_non_lab == 11 ~ "91",
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_non_lab == 12 ~ "95",
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_non_lab == 13 ~ "96",
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_non_lab == 14 ~ "98",
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_non_lab == 15 ~ "99",
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_non_lab == 16 ~ "99",
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_non_lab == 17 ~ ">=99",
      cvd_sex == 1 & cvd_age_cat == 5 & who_risk_non_lab >= 18 ~ "100",
      
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_non_lab == 7 ~ "0",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_non_lab == 8 ~ "9",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_non_lab == 9 ~ "24",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_non_lab == 10 ~ "56",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_non_lab == 11 ~ "68",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_non_lab == 12 ~ "75",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_non_lab == 13 ~ "85",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_non_lab == 14 ~ "90",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_non_lab == 15 ~ "95",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_non_lab == 16 ~ "96",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_non_lab == 17 ~ "97",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_non_lab == 18 ~ "98",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_non_lab == 19 ~ "99",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_non_lab == 20 ~ ">=99",
      cvd_sex == 1 & cvd_age_cat == 6 & who_risk_non_lab >= 21 ~ "100",
      
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_non_lab == 9 ~ "0",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_non_lab == 10 ~ "0",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_non_lab == 11 ~ "18",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_non_lab == 12 ~ "22",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_non_lab == 13 ~ "54",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_non_lab == 14 ~ "61",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_non_lab == 15 ~ "70",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_non_lab == 16 ~ "83",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_non_lab == 17 ~ "86",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_non_lab == 18 ~ "90",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_non_lab == 19 ~ "96",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_non_lab == 20 ~ "97",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_non_lab == 21 ~ "98",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_non_lab == 22 ~ "98",
      cvd_sex == 1 & cvd_age_cat == 7 & who_risk_non_lab >= 23 ~ "100",
      
      cvd_sex == 2 & cvd_age_cat == 1 & who_risk_non_lab == 1 ~ "0",
      cvd_sex == 2 & cvd_age_cat == 1 & who_risk_non_lab == 2 ~ "70",
      cvd_sex == 2 & cvd_age_cat == 1 & who_risk_non_lab == 3 ~ "93",
      cvd_sex == 2 & cvd_age_cat == 1 & who_risk_non_lab == 4 ~ "99",
      cvd_sex == 2 & cvd_age_cat == 1 & who_risk_non_lab >= 5 ~ "100",
      
      cvd_sex == 2 & cvd_age_cat == 2 & who_risk_non_lab == 1 ~ "0",
      cvd_sex == 2 & cvd_age_cat == 2 & who_risk_non_lab == 2 ~ "3",
      cvd_sex == 2 & cvd_age_cat == 2 & who_risk_non_lab == 3 ~ "81",
      cvd_sex == 2 & cvd_age_cat == 2 & who_risk_non_lab == 4 ~ "96",
      cvd_sex == 2 & cvd_age_cat == 2 & who_risk_non_lab == 5 ~ "98",
      cvd_sex == 2 & cvd_age_cat == 2 & who_risk_non_lab == 6 ~ "99",
      cvd_sex == 2 & cvd_age_cat == 2 & who_risk_non_lab >= 7 ~ "100",
      
      cvd_sex == 2 & cvd_age_cat == 3 & who_risk_non_lab == 2 ~ "0",
      cvd_sex == 2 & cvd_age_cat == 3 & who_risk_non_lab == 3 ~ "51",
      cvd_sex == 2 & cvd_age_cat == 3 & who_risk_non_lab == 4 ~ "79",
      cvd_sex == 2 & cvd_age_cat == 3 & who_risk_non_lab == 5 ~ "93",
      cvd_sex == 2 & cvd_age_cat == 3 & who_risk_non_lab == 6 ~ "97",
      cvd_sex == 2 & cvd_age_cat == 3 & who_risk_non_lab == 7 ~ "99",
      cvd_sex == 2 & cvd_age_cat == 3 & who_risk_non_lab >= 8 ~ "100",
      
      cvd_sex == 2 & cvd_age_cat == 4 & who_risk_non_lab == 3 ~ "0",
      cvd_sex == 2 & cvd_age_cat == 4 & who_risk_non_lab == 4 ~ "47",
      cvd_sex == 2 & cvd_age_cat == 4 & who_risk_non_lab == 5 ~ "76",
      cvd_sex == 2 & cvd_age_cat == 4 & who_risk_non_lab == 6 ~ "90",
      cvd_sex == 2 & cvd_age_cat == 4 & who_risk_non_lab == 7 ~ "95",
      cvd_sex == 2 & cvd_age_cat == 4 & who_risk_non_lab == 8 ~ "98",
      cvd_sex == 2 & cvd_age_cat == 4 & who_risk_non_lab == 9 ~ "99",
      cvd_sex == 2 & cvd_age_cat == 4 & who_risk_non_lab == 10 ~ "99",
      cvd_sex == 2 & cvd_age_cat == 4 & who_risk_non_lab >= 11 ~ "100",
      
      cvd_sex == 2 & cvd_age_cat == 5 & who_risk_non_lab == 4 ~ "0",
      cvd_sex == 2 & cvd_age_cat == 5 & who_risk_non_lab == 5 ~ "39",
      cvd_sex == 2 & cvd_age_cat == 5 & who_risk_non_lab == 6 ~ "75",
      cvd_sex == 2 & cvd_age_cat == 5 & who_risk_non_lab == 7 ~ "88",
      cvd_sex == 2 & cvd_age_cat == 5 & who_risk_non_lab == 8 ~ "91",
      cvd_sex == 2 & cvd_age_cat == 5 & who_risk_non_lab == 9 ~ "97",
      cvd_sex == 2 & cvd_age_cat == 5 & who_risk_non_lab == 10 ~ "98",
      cvd_sex == 2 & cvd_age_cat == 5 & who_risk_non_lab == 11 ~ "99",
      cvd_sex == 2 & cvd_age_cat == 5 & who_risk_non_lab >= 12 ~ "100",
      
      cvd_sex == 2 & cvd_age_cat == 6 & who_risk_non_lab == 5 ~ "0",
      cvd_sex == 2 & cvd_age_cat == 6 & who_risk_non_lab == 6 ~ "11",
      cvd_sex == 2 & cvd_age_cat == 6 & who_risk_non_lab == 7 ~ "39",
      cvd_sex == 2 & cvd_age_cat == 6 & who_risk_non_lab == 8 ~ "69",
      cvd_sex == 2 & cvd_age_cat == 6 & who_risk_non_lab == 9 ~ "89",
      cvd_sex == 2 & cvd_age_cat == 6 & who_risk_non_lab == 10 ~ "91",
      cvd_sex == 2 & cvd_age_cat == 6 & who_risk_non_lab == 11 ~ "97",
      cvd_sex == 2 & cvd_age_cat == 6 & who_risk_non_lab == 12 ~ "99",
      cvd_sex == 2 & cvd_age_cat == 6 & who_risk_non_lab >= 13 ~ ">=99",
      
      cvd_sex == 2 & cvd_age_cat == 7 & who_risk_non_lab == 7 ~ "0",
      cvd_sex == 2 & cvd_age_cat == 7 & who_risk_non_lab == 8 ~ "6",
      cvd_sex == 2 & cvd_age_cat == 7 & who_risk_non_lab == 9 ~ "25",
      cvd_sex == 2 & cvd_age_cat == 7 & who_risk_non_lab == 10 ~ "63",
      cvd_sex == 2 & cvd_age_cat == 7 & who_risk_non_lab == 11 ~ "74",
      cvd_sex == 2 & cvd_age_cat == 7 & who_risk_non_lab == 12 ~ "89",
      cvd_sex == 2 & cvd_age_cat == 7 & who_risk_non_lab == 13 ~ "93",
      cvd_sex == 2 & cvd_age_cat == 7 & who_risk_non_lab == 14 ~ "97",
      cvd_sex == 2 & cvd_age_cat == 7 & who_risk_non_lab == 15 ~ "98",
      cvd_sex == 2 & cvd_age_cat == 7 & who_risk_non_lab == 16 ~ ">=98",
      cvd_sex == 2 & cvd_age_cat == 7 & who_risk_non_lab == 17 ~ ">=98",
      cvd_sex == 2 & cvd_age_cat == 7 & who_risk_non_lab >= 18 ~ "100"
    )
    )
  
  ### LONG-TERM ASCVD RISK ####
  # creating variable "cvd_age_cat_long"
  # age categories:
  # 1 = <45 years
  # 2 = 45-59 years
  # 3 = >=60 years
  df <- df %>%
    mutate(cvd_age_cat_long = case_when(
      cvd_age < 45 ~ 1,
      cvd_age >= 45 & cvd_age <= 59 ~ 2,
      cvd_age >= 60 ~ 3))
  
  # creating variable "cvd_non_hdl" = non-HDL-c
  df$cvd_non_hdl = df$cvd_tc - df$cvd_hdl
  
  # creating variable "cvd_non_hdl_mmoll" = non-HDL-c in mmol/L
  df$cvd_non_hdl_mmoll = df$cvd_non_hdl * 0.02586
  
  # creating variable "cvd_non_hdl_cat" 
  # 1 = <2·6 mmol/L
  # 2 = 2·6 to <3·7 mmol/L
  # 3 = 3·7 to <4·8 mmol/L
  # 4 = 4·8 to <5·7 mmol/L
  # 5 = >=5·7 mmol/L
  df <- df %>%
    mutate(cvd_non_hdl_cat = case_when(
      cvd_non_hdl_mmoll < 2.6 ~ 1,
      cvd_non_hdl_mmoll >= 2.6 & cvd_non_hdl_mmoll < 3.7 ~ 2,
      cvd_non_hdl_mmoll >= 3.7 & cvd_non_hdl_mmoll < 4.8 ~ 3,
      cvd_non_hdl_mmoll >= 4.8 & cvd_non_hdl_mmoll < 5.7 ~ 4,
      cvd_non_hdl_mmoll >= 5.7 ~ 5))
  
  # creating variable "cvd_obesity"
  df <- df %>%
    mutate(cvd_obesity = case_when(
      cvd_bmi >= 30 ~ 1,
      cvd_bmi < 30 ~ 0))
  
  # summing risk factors (smoking, hypertension, DM, and obesity)
  df <- df %>%
    mutate(sum1 = cvd_smoking + cvd_hypert + cvd_dm + cvd_obesity)
  
  df <- df %>%
    mutate(sum2 = case_when(
      sum1 < 2 ~ 1,
      sum1 >= 2 ~ 2))
  
  # creating variable "long_term_risk"
  df <- df %>%
    mutate(long_term_risk = case_when(
      cvd_sex == 1 & cvd_age_cat_long == 1 & cvd_non_hdl_cat == 1 & sum2 == 1 ~ 11.8,
      cvd_sex == 1 & cvd_age_cat_long == 1 & cvd_non_hdl_cat == 1 & sum2 == 2 ~ 18.9,
      cvd_sex == 1 & cvd_age_cat_long == 2 & cvd_non_hdl_cat == 1 & sum2 == 1 ~ 10.6,
      cvd_sex == 1 & cvd_age_cat_long == 2 & cvd_non_hdl_cat == 1 & sum2 == 2 ~ 19.3,
      cvd_sex == 1 & cvd_age_cat_long == 3 & cvd_non_hdl_cat == 1 & sum2 == 1 ~ 7.8,
      cvd_sex == 1 & cvd_age_cat_long == 3 & cvd_non_hdl_cat == 1 & sum2 == 2 ~ 15.5,
      
      cvd_sex == 1 & cvd_age_cat_long == 1 & cvd_non_hdl_cat == 2 & sum2 == 1 ~ 15.0,
      cvd_sex == 1 & cvd_age_cat_long == 1 & cvd_non_hdl_cat == 2 & sum2 == 2 ~ 24.2,
      cvd_sex == 1 & cvd_age_cat_long == 2 & cvd_non_hdl_cat == 2 & sum2 == 1 ~ 13.2,
      cvd_sex == 1 & cvd_age_cat_long == 2 & cvd_non_hdl_cat == 2 & sum2 == 2 ~ 23.0,
      cvd_sex == 1 & cvd_age_cat_long == 3 & cvd_non_hdl_cat == 2 & sum2 == 1 ~ 9.9,
      cvd_sex == 1 & cvd_age_cat_long == 3 & cvd_non_hdl_cat == 2 & sum2 == 2 ~ 17.2,
      
      cvd_sex == 1 & cvd_age_cat_long == 1 & cvd_non_hdl_cat == 3 & sum2 == 1 ~ 19.0,
      cvd_sex == 1 & cvd_age_cat_long == 1 & cvd_non_hdl_cat == 3 & sum2 == 2 ~ 28.8,
      cvd_sex == 1 & cvd_age_cat_long == 2 & cvd_non_hdl_cat == 3 & sum2 == 1 ~ 16.4,
      cvd_sex == 1 & cvd_age_cat_long == 2 & cvd_non_hdl_cat == 3 & sum2 == 2 ~ 27.0,
      cvd_sex == 1 & cvd_age_cat_long == 3 & cvd_non_hdl_cat == 3 & sum2 == 1 ~ 12.3,
      cvd_sex == 1 & cvd_age_cat_long == 3 & cvd_non_hdl_cat == 3 & sum2 == 2 ~ 21.0,
      
      cvd_sex == 1 & cvd_age_cat_long == 1 & cvd_non_hdl_cat == 4 & sum2 == 1 ~ 23.4,
      cvd_sex == 1 & cvd_age_cat_long == 1 & cvd_non_hdl_cat == 4 & sum2 == 2 ~ 33.4,
      cvd_sex == 1 & cvd_age_cat_long == 2 & cvd_non_hdl_cat == 4 & sum2 == 1 ~ 20.3,
      cvd_sex == 1 & cvd_age_cat_long == 2 & cvd_non_hdl_cat == 4 & sum2 == 2 ~ 31.8,
      cvd_sex == 1 & cvd_age_cat_long == 3 & cvd_non_hdl_cat == 4 & sum2 == 1 ~ 14.8,
      cvd_sex == 1 & cvd_age_cat_long == 3 & cvd_non_hdl_cat == 4 & sum2 == 2 ~ 24.7,
      
      cvd_sex == 1 & cvd_age_cat_long == 1 & cvd_non_hdl_cat == 5 & sum2 == 1 ~ 29.8,
      cvd_sex == 1 & cvd_age_cat_long == 1 & cvd_non_hdl_cat == 5 & sum2 == 2 ~ 43.0,
      cvd_sex == 1 & cvd_age_cat_long == 2 & cvd_non_hdl_cat == 5 & sum2 == 1 ~ 27.0,
      cvd_sex == 1 & cvd_age_cat_long == 2 & cvd_non_hdl_cat == 5 & sum2 == 2 ~ 40.9,
      cvd_sex == 1 & cvd_age_cat_long == 3 & cvd_non_hdl_cat == 5 & sum2 == 1 ~ 19.6,
      cvd_sex == 1 & cvd_age_cat_long == 3 & cvd_non_hdl_cat == 5 & sum2 == 2 ~ 31.6,
      
      cvd_sex == 2 & cvd_age_cat_long == 1 & cvd_non_hdl_cat == 1 & sum2 == 1 ~ 5.7,
      cvd_sex == 2 & cvd_age_cat_long == 1 & cvd_non_hdl_cat == 1 & sum2 == 2 ~ 12.3,
      cvd_sex == 2 & cvd_age_cat_long == 2 & cvd_non_hdl_cat == 1 & sum2 == 1 ~ 5.6,
      cvd_sex == 2 & cvd_age_cat_long == 2 & cvd_non_hdl_cat == 1 & sum2 == 2 ~ 11.3,
      cvd_sex == 2 & cvd_age_cat_long == 3 & cvd_non_hdl_cat == 1 & sum2 == 1 ~ 5.0,
      cvd_sex == 2 & cvd_age_cat_long == 3 & cvd_non_hdl_cat == 1 & sum2 == 2 ~ 9.1,
      
      cvd_sex == 2 & cvd_age_cat_long == 1 & cvd_non_hdl_cat == 2 & sum2 == 1 ~ 6.9,
      cvd_sex == 2 & cvd_age_cat_long == 1 & cvd_non_hdl_cat == 2 & sum2 == 2 ~ 13.4,
      cvd_sex == 2 & cvd_age_cat_long == 2 & cvd_non_hdl_cat == 2 & sum2 == 1 ~ 6.7,
      cvd_sex == 2 & cvd_age_cat_long == 2 & cvd_non_hdl_cat == 2 & sum2 == 2 ~ 12.8,
      cvd_sex == 2 & cvd_age_cat_long == 3 & cvd_non_hdl_cat == 2 & sum2 == 1 ~ 5.7,
      cvd_sex == 2 & cvd_age_cat_long == 3 & cvd_non_hdl_cat == 2 & sum2 == 2 ~ 9.8,
      
      cvd_sex == 2 & cvd_age_cat_long == 1 & cvd_non_hdl_cat == 3 & sum2 == 1 ~ 8.8,
      cvd_sex == 2 & cvd_age_cat_long == 1 & cvd_non_hdl_cat == 3 & sum2 == 2 ~ 15.6,
      cvd_sex == 2 & cvd_age_cat_long == 2 & cvd_non_hdl_cat == 3 & sum2 == 1 ~ 8.2,
      cvd_sex == 2 & cvd_age_cat_long == 2 & cvd_non_hdl_cat == 3 & sum2 == 2 ~ 14.6,
      cvd_sex == 2 & cvd_age_cat_long == 3 & cvd_non_hdl_cat == 3 & sum2 == 1 ~ 6.7,
      cvd_sex == 2 & cvd_age_cat_long == 3 & cvd_non_hdl_cat == 3 & sum2 == 2 ~ 11.9,
      
      cvd_sex == 2 & cvd_age_cat_long == 1 & cvd_non_hdl_cat == 4 & sum2 == 1 ~ 10.9,
      cvd_sex == 2 & cvd_age_cat_long == 1 & cvd_non_hdl_cat == 4 & sum2 == 2 ~ 18.1,
      cvd_sex == 2 & cvd_age_cat_long == 2 & cvd_non_hdl_cat == 4 & sum2 == 1 ~ 10.0,
      cvd_sex == 2 & cvd_age_cat_long == 2 & cvd_non_hdl_cat == 4 & sum2 == 2 ~ 16.9,
      cvd_sex == 2 & cvd_age_cat_long == 3 & cvd_non_hdl_cat == 4 & sum2 == 1 ~ 8.2,
      cvd_sex == 2 & cvd_age_cat_long == 3 & cvd_non_hdl_cat == 4 & sum2 == 2 ~ 13.8,
      
      cvd_sex == 2 & cvd_age_cat_long == 1 & cvd_non_hdl_cat == 5 & sum2 == 1 ~ 14.0,
      cvd_sex == 2 & cvd_age_cat_long == 1 & cvd_non_hdl_cat == 5 & sum2 == 2 ~ 24.1,
      cvd_sex == 2 & cvd_age_cat_long == 2 & cvd_non_hdl_cat == 5 & sum2 == 1 ~ 12.9,
      cvd_sex == 2 & cvd_age_cat_long == 2 & cvd_non_hdl_cat == 5 & sum2 == 2 ~ 22.1,
      cvd_sex == 2 & cvd_age_cat_long == 3 & cvd_non_hdl_cat == 5 & sum2 == 1 ~ 10.7,
      cvd_sex == 2 & cvd_age_cat_long == 3 & cvd_non_hdl_cat == 5 & sum2 == 2 ~ 17.7
    ))
  
  # creating variable "who_risk_non_lab_perc"
  df <- df %>%
    mutate(long_term_risk_perc = case_when(
      cvd_sex == 1 & cvd_age_cat_long == 1 & long_term_risk == 11.8 ~ "0",
      cvd_sex == 1 & cvd_age_cat_long == 1 & long_term_risk == 15.0 ~ "8",
      cvd_sex == 1 & cvd_age_cat_long == 1 & long_term_risk == 18.9 ~ "43",
      cvd_sex == 1 & cvd_age_cat_long == 1 & long_term_risk == 19.0 ~ "44",
      cvd_sex == 1 & cvd_age_cat_long == 1 & long_term_risk == 23.4 ~ "75",
      cvd_sex == 1 & cvd_age_cat_long == 1 & long_term_risk == 24.2 ~ "85",
      cvd_sex == 1 & cvd_age_cat_long == 1 & long_term_risk == 28.8 ~ "89",
      cvd_sex == 1 & cvd_age_cat_long == 1 & long_term_risk == 29.8 ~ "94",
      cvd_sex == 1 & cvd_age_cat_long == 1 & long_term_risk == 33.4 ~ "97",
      cvd_sex == 1 & cvd_age_cat_long == 1 & long_term_risk == 43.0 ~ "99",
      
      cvd_sex == 1 & cvd_age_cat_long == 2 & long_term_risk == 10.6 ~ "0",
      cvd_sex == 1 & cvd_age_cat_long == 2 & long_term_risk == 13.2 ~ "5",
      cvd_sex == 1 & cvd_age_cat_long == 2 & long_term_risk == 16.4 ~ "31",
      cvd_sex == 1 & cvd_age_cat_long == 2 & long_term_risk == 19.3 ~ "60",
      cvd_sex == 1 & cvd_age_cat_long == 2 & long_term_risk == 20.3 ~ "62",
      cvd_sex == 1 & cvd_age_cat_long == 2 & long_term_risk == 23.0 ~ "72",
      cvd_sex == 1 & cvd_age_cat_long == 2 & long_term_risk == 27.0 ~ "80",
      cvd_sex == 1 & cvd_age_cat_long == 2 & long_term_risk == 31.8 ~ "94",
      cvd_sex == 1 & cvd_age_cat_long == 2 & long_term_risk == 40.9 ~ "98",
      
      cvd_sex == 1 & cvd_age_cat_long == 3 & long_term_risk == 7.8 ~ "0",
      cvd_sex == 1 & cvd_age_cat_long == 3 & long_term_risk == 9.9 ~ "5",
      cvd_sex == 1 & cvd_age_cat_long == 3 & long_term_risk == 12.3 ~ "30",
      cvd_sex == 1 & cvd_age_cat_long == 3 & long_term_risk == 14.8 ~ "55",
      cvd_sex == 1 & cvd_age_cat_long == 3 & long_term_risk == 15.5 ~ "61",
      cvd_sex == 1 & cvd_age_cat_long == 3 & long_term_risk == 17.2 ~ "65",
      cvd_sex == 1 & cvd_age_cat_long == 3 & long_term_risk == 19.6 ~ "79",
      cvd_sex == 1 & cvd_age_cat_long == 3 & long_term_risk == 21.0 ~ "82",
      cvd_sex == 1 & cvd_age_cat_long == 3 & long_term_risk == 24.7 ~ "95",
      cvd_sex == 1 & cvd_age_cat_long == 3 & long_term_risk == 31.6 ~ "99",
      
      cvd_sex == 2 & cvd_age_cat_long == 1 & long_term_risk == 5.7 ~ "0",
      cvd_sex == 2 & cvd_age_cat_long == 1 & long_term_risk == 6.9 ~ "18",
      cvd_sex == 2 & cvd_age_cat_long == 1 & long_term_risk == 8.8 ~ "65",
      cvd_sex == 2 & cvd_age_cat_long == 1 & long_term_risk == 10.9 ~ "86",
      cvd_sex == 2 & cvd_age_cat_long == 1 & long_term_risk == 12.3 ~ "90",
      cvd_sex == 2 & cvd_age_cat_long == 1 & long_term_risk == 13.4 ~ "91",
      cvd_sex == 2 & cvd_age_cat_long == 1 & long_term_risk == 14.0 ~ "95",
      cvd_sex == 2 & cvd_age_cat_long == 1 & long_term_risk == 15.6 ~ "96",
      cvd_sex == 2 & cvd_age_cat_long == 1 & long_term_risk == 18.1 ~ "99",
      cvd_sex == 2 & cvd_age_cat_long == 1 & long_term_risk == 24.1 ~ "100",
      
      cvd_sex == 2 & cvd_age_cat_long == 2 & long_term_risk == 5.6 ~ "0",
      cvd_sex == 2 & cvd_age_cat_long == 2 & long_term_risk == 6.7 ~ "7",
      cvd_sex == 2 & cvd_age_cat_long == 2 & long_term_risk == 8.2 ~ "40",
      cvd_sex == 2 & cvd_age_cat_long == 2 & long_term_risk == 10.0 ~ "68",
      cvd_sex == 2 & cvd_age_cat_long == 2 & long_term_risk == 11.3 ~ "76",
      cvd_sex == 2 & cvd_age_cat_long == 2 & long_term_risk == 12.8 ~ "77",
      cvd_sex == 2 & cvd_age_cat_long == 2 & long_term_risk == 12.9 ~ "85",
      cvd_sex == 2 & cvd_age_cat_long == 2 & long_term_risk == 14.6 ~ "87",
      cvd_sex == 2 & cvd_age_cat_long == 2 & long_term_risk == 16.9 ~ "96",
      cvd_sex == 2 & cvd_age_cat_long == 2 & long_term_risk == 22.1 ~ "99",
      
      cvd_sex == 2 & cvd_age_cat_long == 3 & long_term_risk == 5.0 ~ "0",
      cvd_sex == 2 & cvd_age_cat_long == 3 & long_term_risk == 5.7 ~ "5",
      cvd_sex == 2 & cvd_age_cat_long == 3 & long_term_risk == 6.7 ~ "32",
      cvd_sex == 2 & cvd_age_cat_long == 3 & long_term_risk == 8.2 ~ "57",
      cvd_sex == 2 & cvd_age_cat_long == 3 & long_term_risk == 9.1 ~ "65",
      cvd_sex == 2 & cvd_age_cat_long == 3 & long_term_risk == 9.8 ~ "68",
      cvd_sex == 2 & cvd_age_cat_long == 3 & long_term_risk == 10.7 ~ "80",
      cvd_sex == 2 & cvd_age_cat_long == 3 & long_term_risk == 11.9 ~ "83",
      cvd_sex == 2 & cvd_age_cat_long == 3 & long_term_risk == 13.8 ~ "94",
      cvd_sex == 2 & cvd_age_cat_long == 3 & long_term_risk == 17.7 ~ "98"
    ))
}

cvd_df2 <- cvdrisk(cvd_df1)

#### END OF CODE ####

#### CREATING NEW DATA FRAME WITH RISKS AND PERCENTILES (NEW VARIABLES) ####
new_df <- # choose a name for the new data frame
  cbind(
  original_df, # enter the original data frame name 
  who_risk_lab = cvd_df2$who_risk_lab,
  who_risk_lab_perc = cvd_df2$who_risk_lab_perc,
  who_risk_non_lab = cvd_df2$who_risk_non_lab,
  who_risk_non_lab_perc = cvd_df2$who_risk_non_lab_perc,
  long_term_risk = cvd_df2$long_term_risk,
  long_term_risk_perc = cvd_df2$long_term_risk_perc
)

#### Variables added to the original data frame ####
# "who_risk_lab" (10-year WHO CVD laboratory-based risk)
# "who_risk_lab_perc" (percentile of WHO CVD laboratory-based risk distribution)
# "who_risk_non_lab" (10-year WHO CVD non-laboratory-based risk)
# "who_risk_non_lab_perc" (percentile of WHO CVD non-laboratory-based risk distribution)
# "long_term_risk" (ASCVD risk up to 75 years of age)
# "long_term_risk_perc" (percentile of ASCVD risk up to 75 years of age)