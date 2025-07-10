# Load Required Libraries
library(dplyr)
library(readxl)
library(geosphere)
library(lubridate)
library(ggplot2)
library(sandwich)
library(lmtest)
library(estimatr)
library(fastDummies)
library(writexl)
library(texreg)
library(stargazer)
setwd("/Users/abdullahaljubayed/Desktop/Capstone Project")
# combined_data should include offender proximity counts (1 mile and 0.1 mile)
combined_data <- read_excel("combined_final_data.xlsx")

# Prepare Main Analysis Dataset

prime_data <- combined_data %>%
  mutate(
    ln_soldprice = log(soldprice),
    ln_lotsize = log(lotsize_use)
  ) %>%
  select(ln_soldprice, daysonmarket, soldprice, beds, fb, hb, lotsize_use, ln_lotsize,
         AC_electric, attached_garage, units, appearance_fair,
         age_05_or_less:age_81, age_unknown,
         sqft_1001_1200:sqft_5001_5500,
         offender_lived_1 = n_offenders_near_house_1mi,
         offenders_lived_0.1 = n_offenders_near_house_0.1mi,
         starts_with("month_"), starts_with("year__"), starts_with("tract_")) %>%
  na.omit()

# 1 Mile Proximity Models
model_1mi_basic <- lm(ln_soldprice ~ offender_lived_1, data = prime_data)
model_1mi_features <- lm(ln_soldprice ~ offender_lived_1 + beds + fb + hb + lotsize_use + AC_electric +
                         attached_garage + units + appearance_fair + age_05_or_less + age_06_10 +
                         age_11_15 + age_16_20 + age_21_25 + age_26_30 + age_31_35 + age_36_40 +
                         age_41_50 + age_51_60 + age_61_70 + age_71_80 + age_81 + sqft_1001_1200 +
                         sqft_1201_1400 + sqft_1401_1600 + sqft_1601_1800 + sqft_1801_2000 +
                         sqft_2001_2200 + sqft_2201_2500 + sqft_2501_2750 + sqft_2751_3000 +
                         sqft_3001_3300 + sqft_3301_3750 + sqft_3751_4250 + sqft_4251_5000 + sqft_5001_5500,
                         data = prime_data)
model_1mi_full <- lm(ln_soldprice ~ offender_lived_1 + beds + fb + hb + lotsize_use + AC_electric +
                     attached_garage + units + appearance_fair + age_05_or_less + age_06_10 +
                     age_11_15 + age_16_20 + age_21_25 + age_26_30 + age_31_35 + age_36_40 +
                     age_41_50 + age_51_60 + age_61_70 + age_71_80 + age_81 + sqft_1001_1200 +
                     sqft_1201_1400 + sqft_1401_1600 + sqft_1601_1800 + sqft_1801_2000 +
                     sqft_2001_2200 + sqft_2201_2500 + sqft_2501_2750 + sqft_2751_3000 +
                     sqft_3001_3300 + sqft_3301_3750 + sqft_3751_4250 + sqft_4251_5000 + sqft_5001_5500 +
                     month_1 + month_2 + month_3 + month_4 + month_5 + month_6 + month_7 + month_8 +
                     month_9 + month_10 + month_11 + year__2013 + year__2014 + year__2015 + year__2016 +
                     year__2017 + year__2018 + year__2019 + year__2020 + year__2021 +
                     tract__212270102 + tract__212270103 + tract__212270104 + tract__212270105 + tract__212270106 +
                     tract__212270107 + tract__212270108 + tract__212270109 + tract__212270110 + tract__212270111 +
                     tract__212270112 + tract__212270113 + tract__212270114 + tract__212270115 + tract__212270116 +
                     tract__212270117 + tract__212270118 + tract__212270119,
                     data = prime_data)

# 0.1 Mile Proximity Models
model_0.1mi_basic <- lm(ln_soldprice ~ offenders_lived_0.1, data = prime_data)
model_0.1mi_features <- lm(ln_soldprice ~ offenders_lived_0.1 + beds + fb + hb + lotsize_use + AC_electric +
                           attached_garage + units + appearance_fair + age_05_or_less + age_06_10 +
                           age_11_15 + age_16_20 + age_21_25 + age_26_30 + age_31_35 + age_36_40 +
                           age_41_50 + age_51_60 + age_61_70 + age_71_80 + age_81 + sqft_1001_1200 +
                           sqft_1201_1400 + sqft_1401_1600 + sqft_1601_1800 + sqft_1801_2000 +
                           sqft_2001_2200 + sqft_2201_2500 + sqft_2501_2750 + sqft_2751_3000 +
                           sqft_3001_3300 + sqft_3301_3750 + sqft_3751_4250 + sqft_4251_5000 + sqft_5001_5500,
                           data = prime_data)
model_0.1mi_full <- lm(ln_soldprice ~ offenders_lived_0.1 + beds + fb + hb + lotsize_use + AC_electric +
                       attached_garage + units + appearance_fair + age_05_or_less + age_06_10 +
                       age_11_15 + age_16_20 + age_21_25 + age_26_30 + age_31_35 + age_36_40 +
                       age_41_50 + age_51_60 + age_61_70 + age_71_80 + age_81 + sqft_1001_1200 +
                       sqft_1201_1400 + sqft_1401_1600 + sqft_1601_1800 + sqft_1801_2000 +
                       sqft_2001_2200 + sqft_2201_2500 + sqft_2501_2750 + sqft_2751_3000 +
                       sqft_3001_3300 + sqft_3301_3750 + sqft_3751_4250 + sqft_4251_5000 + sqft_5001_5500 +
                       month_1 + month_2 + month_3 + month_4 + month_5 + month_6 + month_7 + month_8 +
                       month_9 + month_10 + month_11 + year__2013 + year__2014 + year__2015 + year__2016 +
                       year__2017 + year__2018 + year__2019 + year__2020 + year__2021 +
                       tract__212270101 + tract__212270102 + tract__212270103 + tract__212270104 + tract__212270105 +
                       tract__212270106 + tract__212270107 + tract__212270108 + tract__212270109 + tract__212270110 +
                       tract__212270111 + tract__212270112 + tract__212270113 + tract__212270114 + tract__212270115 +
                       tract__212270116 + tract__212270117 + tract__212270118 + tract__212270119,
                       data = prime_data)

# Model Summaries (Robust SEs)
summary(model_1mi_full)
summary(model_0.1mi_full)

# Output Side-by-Side Comparison
screenreg(list(model_1mi_basic, model_1mi_features, model_1mi_full,
               model_0.1mi_basic, model_0.1mi_features, model_0.1mi_full))

stargazer(model_1mi_basic, model_1mi_features, model_1mi_full,
          model_0.1mi_basic, model_0.1mi_features, model_0.1mi_full,
          type = "text", title = "Regression Models for Offender Proximity")
