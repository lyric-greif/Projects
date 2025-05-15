setwd("C:/Users/C2KSD/Desktop/capstone")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(DIDmultiplegtDYN)


# Load dataset 
rf = read.csv("rent_fire_df_3.3.25_MOST_RECENT2.csv") # SHELDUS fire and Zillow ZORI merged rent data, along with urban-rural continuum codes

# Code first treatment to equal the first month where fire_count = 1
rf<- rf %>%
  group_by(state, county, fips) %>%
  mutate(first_treatment = ifelse(any(fire_count == 1), min(period[fire_count == 1], na.rm = TRUE), NA))

# Make bins for heterogeneity tests
# 1) Property damage: find median, the code 1 = over median, 0 = under median 
rf = rf %>% # median split
  mutate(over_med_dmg = ifelse(PropertyDmg_ADJ_2023 > 266024.99, 1, 0))

#log(266024) # 12.49134

rf = rf %>%  # median split
  mutate(over_med_log_dmg = ifelse(log_prop_dmg > 12.49, 1,0))

rf = rf %>%  # median split
  mutate(under_med_log_dmg = ifelse(log_prop_dmg <= 12.49, 1,0))


# urban rural dummies - USDA U/R classification: 1-3 = metro counties, 4-9 = nonmetro counties 
rf = rf %>%
  mutate(metro = ifelse(rur_urb_code <= 3, 1, 0))

rf = rf %>%
  mutate(nonmetro = ifelse(rur_urb_code >= 4, 1,0))

# interact metro + nonmetro dummies with prop dmg
rf = rf %>%
  mutate(metro_over_logdmg = over_med_log_dmg * metro)

rf = rf %>%
  mutate(nonmetro_over_logdmg = over_med_log_dmg * nonmetro)

# Fire county * metro/nonmetro dummies 
rf = rf %>%
  mutate(fire_dummy_metro_int = fire_count * metro)

rf = rf %>%
  mutate(fire_dummy_nonmetro_int = fire_count * nonmetro)

write.csv(rf, file = "rf_3.18.25.csv")

#================================================================================
# Summary Stats 
library(stargazer)
install.packages("summarytools")
library(summarytools)

stargazer(rf, type = "text", title = "Descriptive Statistics", digits = 2)
stargazer(rf[, sapply(rf, is.numeric)], type = "text", title = "Descriptive Statistics", digits = 2)
dfSummary(rf_stats)

rf_stats = rf %>%
  select(PropertyDmg_ADJ_2023, fire_count, rent, metro, nonmetro, log_prop_dmg)
stargazer(rf_stats, type = "text", title = "Descriptive Statistics", digits = 2)
summary(rf_stats)


#==================================================================================
# Regressions
#1
reg1 = did_multiplegt_dyn(df = rf, 
                          outcome = "log_rent", 
                          group = "first_treatment", 
                          time = "period", 
                          treatment = "fire_count",
                          effects = 12,
                          placebo = 10,
                          same_switchers = T,
                          same_switchers_pl = T)

summary(reg1)
reg1_plot = reg1$plot

reg1_plot + 
  labs(title = "Impact of Wildfire Occurence on Log Rent (All U.S Counties)",
       x = "Months before and after wildfire", 
       y = "Log of Rent") +
  theme_grey() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13))


# 2 ------------------------------------------------------------------------
reg2 = did_multiplegt_dyn(df = rf, 
                          outcome = "log_rent", 
                          group = "first_treatment", 
                          time = "period", 
                          treatment = "fire_prop_dmg_interaction",
                          effects = 10,
                          placebo = 10,
                          same_switchers = T,
                          same_switchers_pl = T)
summary(reg2)
reg2_plot = reg2$plot

reg2_plot + 
  labs(title = "Impact of Wildfires with Property Damage on Rents",
       x = "Months before and after wildfire", 
       y = "Log of Rent") +
  theme_grey() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13))
