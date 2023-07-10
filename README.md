# WHO_CVD_risk_Brazil
Resources for calculating the 10-year risk of cardiovascular disease (CVD) and the corresponding sex- and age-specific percentile.

Files and descriptions:

“calculator_WHO_CVD_risk_Brazil_EN.xlsx”: a Microsoft Excel file for calculating the 10-year CVD risk and the corresponding percentile for sex and age. Ten-year CVD risk corresponds to the risk of a first fatal or nonfatal CVD event (coronary heart disease event or stroke) according to the 2019 Updated WHO CVD risk laboratory-based chart calibrated for Tropical Latin America (Brazil and Paraguay). Risk percentiles were determined in the ELSA-Brasil study population. The tool also estimates plasma total cholesterol levels without medication for patients taking statins and/or ezetimibe. A web app version of this calculator is available at https://fernando-cesena.shinyapps.io/cv_risk_calculator_Brazil/.

“calculator_WHO_CVD_risk_Brazil_POR.xlsx”: a Microsoft Excel file for the same calculator described above in Portuguese.

“WHO_CVD_risk_Brazil.R”: R code to create new variables in a dataset for 10-year CVD risk and the corresponding sex- and age-specific percentile as described above. These new variables are created after setting variables for sex, age, systolic blood pressure, plasma total cholesterol level, presence of diabetes mellitus, and smoking status.
