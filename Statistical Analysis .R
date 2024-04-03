library(dplyr)
library(readxl)
library(geosphere)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(sandwich)
library(lmtest)
library(estimatr)
library(readr)
library(AER)
library(plm)
library(stargazer)
library(writexl)
library(texreg)
setwd("/Users/abdullahaljubayed/Desktop/Capstone Project")
prime_data<- combined_data%>%
  select("age_05_or_less":"soldprice","daysonmarket":"year__2021", "offender_lived_1":"basement_dummy")
colnames(prime_data)[colnames(prime_data) == "offender_lived_nearby"] <- "offenders_lived_0.1"
prime_data$ln_lotsize<- log(prime_data$lotsize_use)
prime_data<- prime_data%>%select(-basement)
regprice1<- lm(ln_soldprice~offender_lived_1, data = prime_data)
regprice1<-coeftest(regprice1, vcov. = vcovHC(regprice1, type= "HC2"))
regprice1_fix_house<-lm(ln_soldprice~offender_lived_1+
   beds+fb+hb+ lotsize_use+AC_electric+attached_garage+units+appearance_fair+age_05_or_less+age_06_10+age_11_15+age_16_20+age_21_25+age_26_30+age_31_35+age_36_40+age_41_50+age_51_60+ age_61_70+age_71_80+age_81+sqft_1001_1200+sqft_1201_1400+sqft_1401_1600+sqft_1601_1800+sqft_1801_2000+
                          sqft_2001_2200+sqft_2201_2500+sqft_2501_2750+sqft_2751_3000+sqft_3001_3300+sqft_3301_3750+
                          sqft_3751_4250+sqft_4251_5000+sqft_5001_5500, data = prime_data)
regprice1_fix_house<-coeftest(regprice1_fix_house, vcov. = vcovHC(regprice1_fix_house, type= "HC2"))
regprice1_house_time<-lm(ln_soldprice~offender_lived_1+beds+fb+hb+ lotsize_use+AC_electric+attached_garage+units+appearance_fair+age_05_or_less+age_06_10+age_11_15+age_16_20+age_21_25+age_26_30+age_31_35+age_36_40+age_41_50+age_51_60+ age_61_70+age_71_80+age_81+sqft_1001_1200+sqft_1201_1400+sqft_1401_1600+sqft_1601_1800+sqft_1801_2000+
                           sqft_2001_2200+sqft_2201_2500+sqft_2501_2750+sqft_2751_3000+sqft_3001_3300+sqft_3301_3750+
                           sqft_3751_4250+sqft_4251_5000+sqft_5001_5500+month_1+month_2+month_3+month_4+month_5+month_6+month_7+month_8+month_9+month_10+month_11+year__2013+year__2014+year__2015+year__2016+year__2017+year__2018+year__2019+
                       year__2020+year__2021, data = prime_data)
regprice1_house_time<-coeftest(regprice1_house_time, vcov. = vcovHC(regprice1_house_time, type= "HC2"))
summary(regprice1_house_time)
regprice1_location<-lm(ln_soldprice~offender_lived_1+beds+fb+hb+ lotsize_use+AC_electric+attached_garage+units+appearance_fair+age_05_or_less+age_06_10+age_11_15+age_16_20+age_21_25+age_26_30+age_31_35+age_36_40+age_41_50+age_51_60+ age_61_70+age_71_80+age_81+sqft_1001_1200+sqft_1201_1400+sqft_1401_1600+sqft_1601_1800+sqft_1801_2000+
                         sqft_2001_2200+sqft_2201_2500+sqft_2501_2750+sqft_2751_3000+sqft_3001_3300+sqft_3301_3750+
                         sqft_3751_4250+sqft_4251_5000+sqft_5001_5500+month_1+month_2+month_3+month_4+month_5+month_6+month_7+month_8+month_9+month_10+month_11+year__2013+year__2014+year__2015+year__2016+year__2017+year__2018+year__2019+
                year__2020+year__2021+tract__212270102+tract__212270103+tract__212270104+tract__212270105+tract__212270106+tract__212270107+tract__212270108+tract__212270109+tract__212270110+tract__212270111+tract__212270112+tract__212270113+tract__212270114+
                tract__212270115+tract__212270116+tract__212270117+tract__212270118+tract__212270119, data = prime_data)
regprice_location<-coeftest(regprice1_location, vcov. = vcovHC(regprice1_location, type= "HC2"))
summary(regprice4)
screenreg(list(regprice1,regprice1_fix_house,regprice1_house_time, regprice1_location))
stargazer(regprice1,regprice1_fix_house,regprice1_house_time, regprice1_location, type = "text", na.omit())
reg0.3_1<-lm(ln_soldprice~offenders_within_0.3, data = combined_data)
summary(reg0.3)
reg0.3_2<- lm(ln_soldprice~offenders_within_0.3+year__2013+year__2014+year__2015+year__2016+year__2017+year__2018+year__2019+
                 year__2020+year__2021, data = combined_data)
summary(regtenth2)
reg0.3_3<- lm(ln_soldprice~offenders_within_0.3+age_05_or_less+age_06_10+age_11_15+age_16_20+age_21_25+age_26_30+age_31_35+age_36_40+age_41_50+age_51_60+ age_61_70+age_71_80+age_81+
                 age_unknown+beds+fb+hb+sqft_1001_1200+sqft_1201_1400+sqft_1401_1600+sqft_1601_1800+sqft_1801_2000+
                 sqft_2001_2200+sqft_2201_2500+sqft_2501_2750+sqft_2751_3000+sqft_3001_3300+sqft_3301_3750+
                 sqft_3751_4250+sqft_4251_5000+sqft_5001_5500+sqft_5501_6000+sqft_6000+ lotsize_use+year__2013+year__2014+year__2015+year__2016+year__2017+year__2018+year__2019+
                 year__2020+year__2021, data = combined_data)
reg_0.3_4<-lm(ln_soldprice~offenders_within_0.3+age_05_or_less+age_06_10+age_11_15+age_16_20+age_21_25+age_26_30+age_31_35+age_36_40+age_41_50+age_51_60+ age_61_70+age_71_80+age_81+
                age_unknown+beds+fb+hb+sqft_1001_1200+sqft_1201_1400+sqft_1401_1600+sqft_1601_1800+sqft_1801_2000+
                sqft_2001_2200+sqft_2201_2500+sqft_2501_2750+sqft_2751_3000+sqft_3001_3300+sqft_3301_3750+
                sqft_3751_4250+sqft_4251_5000+sqft_5001_5500+lotsize_use+year__2013+year__2014+year__2015+year__2016+year__2017+year__2018+year__2019+
                year__2020+year__2021+tract_212270102+tract_212270103+tract_212270104+tract_212270105+tract_212270106+tract_212270107+tract_212270108+
                tract_212270109+tract_212270110+tract_212270111+tract_212270112+tract_212270113+tract_212270114+
                tract_212270115+tract_212270116+tract_212270117+tract_212270118+tract_212270119, data = combined_data)
summary(reg_0.3_4)
regl<-lm(ln_soldprice~ln_lotsize, data = prime_data
         )
library(descr)
library(summarytools)
install.packages("descr")
data(iris)
statd<- prime_data%>% select("beds":"hb",'lotsize_use':"offender_lived_1", -tract_)
summary_stats <- summary(desstat)
summary_stats_transposed <- t(summary_stats)
summary_stats_transposed
desstat$soldyear<-final_data$year_sold
regtenth1<-lm(ln_soldprice~offenders_lived_0.1, data = prime_data)
regtenth1<-coeftest(regtenth1, vcov. = vcovHC(regtenth1, type = "HC2"))
regtenth2<- lm(ln_soldprice~offenders_lived_0.1+beds+fb+hb+ lotsize_use+AC_electric+attached_garage+units+appearance_fair+age_05_or_less+age_06_10+age_11_15+age_16_20+age_21_25+age_26_30+age_31_35+age_36_40+age_41_50+age_51_60+ age_61_70+age_71_80+age_81+sqft_1001_1200+sqft_1201_1400+sqft_1401_1600+sqft_1601_1800+sqft_1801_2000+
                 sqft_2001_2200+sqft_2201_2500+sqft_2501_2750+sqft_2751_3000+sqft_3001_3300+sqft_3301_3750+
                 sqft_3751_4250+sqft_4251_5000+sqft_5001_5500, data = prime_data)
summary(regtenth2)
regtenth2<-coeftest(regtenth2, vcov. = vcovHC(regtenth2, type = "HC2"))
regtenth3<- lm(ln_soldprice~offenders_lived_0.1+beds+fb+hb+ lotsize_use+AC_electric+attached_garage+units+appearance_fair+age_05_or_less+age_06_10+age_11_15+age_16_20+age_21_25+age_26_30+age_31_35+age_36_40+age_41_50+age_51_60+ age_61_70+age_71_80+age_81+sqft_1001_1200+sqft_1201_1400+sqft_1401_1600+sqft_1601_1800+sqft_1801_2000+
                 sqft_2001_2200+sqft_2201_2500+sqft_2501_2750+sqft_2751_3000+sqft_3001_3300+sqft_3301_3750+
                 sqft_3751_4250+sqft_4251_5000+sqft_5001_5500+month_1+month_2+month_3+month_4+month_5+month_6+month_7+month_8+month_9+month_10+month_11+year__2013+year__2014+year__2015+year__2016+year__2017+year__2018+year__2019+
                 year__2020+year__2021, data = prime_data)
summary(regtenth3)
regtenth3<-coeftest(regtenth3, vcov. = vcovHC(regtenth3, type = "HC2"))
regtenth4<-lm(ln_soldprice~offenders_lived_0.1+beds+fb+hb+ lotsize_use+AC_electric+attached_garage+units+appearance_fair+age_05_or_less+age_06_10+age_11_15+age_16_20+age_21_25+age_26_30+age_31_35+age_36_40+age_41_50+age_51_60+ age_61_70+age_71_80+age_81+sqft_1001_1200+sqft_1201_1400+sqft_1401_1600+sqft_1601_1800+sqft_1801_2000+
                sqft_2001_2200+sqft_2201_2500+sqft_2501_2750+sqft_2751_3000+sqft_3001_3300+sqft_3301_3750+
                sqft_3751_4250+sqft_4251_5000+sqft_5001_5500+month_1+month_2+month_3+month_4+month_5+month_6+month_7+month_8+month_9+month_10+month_11+year__2013+year__2014+year__2015+year__2016+year__2017+year__2018+year__2019+
                year__2020+year__2021+tract__212270101+tract__212270102+tract__212270103+tract__212270104+tract__212270105+tract__212270106+tract__212270107+tract__212270108+tract__212270109+tract__212270110+tract__212270111+tract__212270112+tract__212270113+tract__212270114+
                tract__212270115+tract__212270116+tract__212270117+tract__212270118+tract__212270119, data = prime_data)
regtenth4<-coeftest(regtenth4, vcov. = vcovHC(regtenth4, type = "HC2"))
summary(regtenth4)
screenreg(list(regtenth1,regtenth2,regtenth3,regtenth4))
stargazer(regtenth1,regtenth2,regtenth3, type = "text")
summary(regtenth)
day_reg1<-lm(daysonmarket~offenders_within_1mile, data = combined_data)
summary(day_reg1)
day_reg0.1_1<-lm(daysonmarket~offenders_lived_0.1, data = prime_data)
day_reg2<-lm(daysonmarket~offenders_lived_0.1+age_05_or_less+age_06_10+age_11_15+age_16_20+age_21_25+age_26_30+age_31_35+age_36_40+age_41_50+age_51_60+ age_61_70+age_71_80+age_81+
                                 beds+fb+hb+sqft_1001_1200+sqft_1201_1400+sqft_1401_1600+sqft_1601_1800+sqft_1801_2000+
                                 sqft_2001_2200+sqft_2201_2500+sqft_2501_2750+sqft_2751_3000+sqft_3001_3300+sqft_3301_3750+
                                 sqft_3751_4250+sqft_4251_5000+sqft_5001_5500+lotsize_use+AC_electric+attached_garage+units+appearance_fair+year__2013+year__2014+year__2015+year__2016+year__2017+year__2018+year__2019+
                                 year__2020+year__2021+tract__212270102+tract__212270103+tract__212270104+tract__212270105+tract__212270106+tract__212270107+tract__212270108+tract__212270109+tract__212270110+tract__212270111+tract__212270112+tract__212270113+tract__212270114+
               tract__212270115+tract__212270116+tract__212270117+tract__212270118+tract__212270119, data = prime_data)
summary(day_reg2)
coeftest(day_reg2, vcov. = vcovHC(day_reg2, type = "HC2"))
screenreg(list(regprice1, regprice2_fix_year, regprice3_fix_house,regprice_house_y, regprice4))
prime_data$sold_year<- combined_data$year_sold 
reg_presence<-lm(ln_soldprice~offender_presence+beds+fb+hb+ lotsize_use+AC_electric+attached_garage+units+appearance_fair+age_05_or_less+age_06_10+age_11_15+age_16_20+age_21_25+age_26_30+age_31_35+age_36_40+age_41_50+age_51_60+ age_61_70+age_71_80+age_81+sqft_1001_1200+sqft_1201_1400+sqft_1401_1600+sqft_1601_1800+sqft_1801_2000+
                  sqft_2001_2200+sqft_2201_2500+sqft_2501_2750+sqft_2751_3000+sqft_3001_3300+sqft_3301_3750+
                  sqft_3751_4250+sqft_4251_5000+sqft_5001_5500+month_1+month_2+month_3+month_4+month_5+month_6+month_7+month_8+month_9+month_10+month_11+year__2013+year__2014+year__2015+year__2016+year__2017+year__2018+year__2019+
                  year__2020+year__2021+tract__212270106+tract__212270107+tract__212270108+tract__212270109+tract__212270110+tract__212270111+tract__212270112+tract__212270113+tract__212270114+
                   tract__212270115+tract__212270116+tract__212270117+tract__212270118+tract__212270119, data = prime_data)
summary(reg_presence)
stats<- prime_data%>%
  select('soldprice',"offender_lived_1",'offenders_lived_0.1', "lotsize_use", "beds",'fb','hb','distance')
statd<-statd%>%na.omit()
stargazer(summary = prime_data_no_na_numeric, missing=TRUE,logical=TRUE
          )
stargazer(stats, type = "text", title="Descriptive statistics", digits=1, out="table1.txt")
stargazer(regprice1, regprice1_fix_house, regprice1_house_time, regprice1_location, type="html", out="models.html")
prime_data_L<-prime_data%>%
  select('age_05_or_less':'age_81','age_unknown','appearance_fair','beds':'offender_lived_1','AC_electric':'units',"month_sold":"offender_presence")