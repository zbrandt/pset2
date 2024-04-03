install.packages(c("dplyr", "stargazer", "knitr", "ivreg"))
library(dplyr)
library(stargazer)
library(knitr)
library(ivreg)

setwd(paste("/home/zachary/Desktop/ECON 140/Problem Set 2/code", sep=""))
mexico <- read.csv("Mexico_PS2.csv")

# part a
stargazer(mexico)

# part b
ols <- lm(log(inc_m) ~ educ_years, mexico)
stargazer(ols)

# part e
first_stage <- lm(educ_years ~ log(dist_us_km), mexico)
stargazer(ols, first_stage)

# part f
second_stage <- ivreg(log(inc_m) ~ educ_years | log(dist_us_km), data = mexico)
stargazer(ols, first_stage, second_stage)

# part g

first_stage_controls <- lm(educ_years ~ log(dist_us_km) + log(sales_hotel) + logtemp + logprecip + ind_lang, mexico)
reduced_form <- lm(log(inc_m) ~ log(dist_us_km), mexico)
reduced_form_controls <- lm(log(inc_m) ~ log(dist_us_km) + log(sales_hotel) + logtemp + logprecip + ind_lang, mexico)
stargazer(first_stage, first_stage_controls, reduced_form, reduced_form_controls)