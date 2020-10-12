# load libraries
#install.packages("coronavirus")
library (coronavirus)
library(tidyverse)
coronavirus <- read_csv("coronavirus_dataset.csv")


install.packages("coronavirus")
install.packages("devtools")
devtools::install_github("covid19r/coronavirus", force = TRUE)
library (coronavirus)

# look at data
data("coronavirus")
data(coronavirus)
update_datasets()
library(flexdashboard)
coronavirus::update_datasets(silence = TRUE)

library(coronavirus)
data(coronavirus)

head(coronavirus)
# Make df from coronavirus data
df_US2 <- coronavirus %>% 
  # dplyr::filter(date == max(date)) %>%
  # Just US Data
  dplyr::filter(Country.Region == "US") %>%
  # Just deaths
  dplyr::filter(type == "death") #%>%

# DFs for deaths rates of .005 to .015
df_US2$newCases_005 <- 0
df_US2$newCases_006 <- 0
df_US2$newCases_007 <- 0
df_US2$newCases_008 <- 0
df_US2$newCases_009 <- 0
df_US2$newCases_010 <- 0
df_US2$newCases_011 <- 0
df_US2$newCases_012 <- 0
df_US2$newCases_013 <- 0
df_US2$newCases_014 <- 0
df_US2$newCases_015 <- 0

# Turn cases into deaths and get rid of extra columns
df_US2$deaths <- df_US2$cases
len_df <- length(df_US2$deaths)
drops <- c("cases","type")
df_US2 <- df_US2[ , !(names(df_US2) %in% drops)]

# Make new columns for predicted deaths
df_US2$predictedDeaths <- 0


# This model was done on 19 March.  Only actual data March 19 or earlier.
# Data in rows 39 to 58.
# Model says to use data from 23 days prior.
# This will estimate the numbers of cases that were new 23 days prior for each rate.
for (i in 39:58) {
  df_US2$newCases_005[i-23] <- round(df_US2$deaths[i]/.005)
  df_US2$newCases_006[i-23] <- round(df_US2$deaths[i]/.006)
  df_US2$newCases_007[i-23] <- round(df_US2$deaths[i]/.007)
  df_US2$newCases_008[i-23] <- round(df_US2$deaths[i]/.008)
  df_US2$newCases_009[i-23] <- round(df_US2$deaths[i]/.009)
  df_US2$newCases_010[i-23] <- round(df_US2$deaths[i]/.010)
  df_US2$newCases_011[i-23] <- round(df_US2$deaths[i]/.011)
  df_US2$newCases_012[i-23] <- round(df_US2$deaths[i]/.012)
  df_US2$newCases_013[i-23] <- round(df_US2$deaths[i]/.013)
  df_US2$newCases_014[i-23] <- round(df_US2$deaths[i]/.014)
  df_US2$newCases_015[i-23] <- round(df_US2$deaths[i]/.015)
}

# Make new DFs to then estimate new cases based on doubling rate.
df_US_double3days <- df_US2
df_US_double4days <- df_US2
df_US_double5days <- df_US2
df_US_double6days <- df_US2
df_US_double7days <- df_US2
df_US_double8days <- df_US2

# Get starting number of cases for doubling.
sum_005 <- sum(df_US2$newCases_005)
sum_006 <- sum(df_US2$newCases_006)
sum_007 <- sum(df_US2$newCases_007)
sum_008 <- sum(df_US2$newCases_008)
sum_009 <- sum(df_US2$newCases_009)
sum_010 <- sum(df_US2$newCases_010)
sum_011 <- sum(df_US2$newCases_011)
sum_012 <- sum(df_US2$newCases_012)
sum_013 <- sum(df_US2$newCases_013)
sum_014 <- sum(df_US2$newCases_014)
sum_015 <- sum(df_US2$newCases_015)

#### Based on cases by March 19th, estimate new cases based on doubling rate.

# For 3 day doubling.
df_US_double3days$newCases_005[36] <- round(sum_005*(2^(1/3)))-sum_005
df_US_double3days$newCases_006[36] <- round(sum_006*(2^(1/3)))-sum_006
df_US_double3days$newCases_007[36] <- round(sum_007*(2^(1/3)))-sum_007
df_US_double3days$newCases_008[36] <- round(sum_008*(2^(1/3)))-sum_008
df_US_double3days$newCases_009[36] <- round(sum_009*(2^(1/3)))-sum_009
df_US_double3days$newCases_010[36] <- round(sum_010*(2^(1/3)))-sum_010
df_US_double3days$newCases_011[36] <- round(sum_011*(2^(1/3)))-sum_011
df_US_double3days$newCases_012[36] <- round(sum_012*(2^(1/3)))-sum_012
df_US_double3days$newCases_013[36] <- round(sum_013*(2^(1/3)))-sum_013
df_US_double3days$newCases_014[36] <- round(sum_014*(2^(1/3)))-sum_014
df_US_double3days$newCases_015[36] <- round(sum_015*(2^(1/3)))-sum_015

for (i in (1:(len_df-36))) {
  df_US_double3days$newCases_005[i+36] <- round(sum_005*(2^((i+1)/3)))-round(sum_005*(2^((i)/3)))
  df_US_double3days$newCases_006[i+36] <- round(sum_006*(2^((i+1)/3)))-round(sum_006*(2^((i)/3)))
  df_US_double3days$newCases_007[i+36] <- round(sum_007*(2^((i+1)/3)))-round(sum_007*(2^((i)/3)))
  df_US_double3days$newCases_008[i+36] <- round(sum_008*(2^((i+1)/3)))-round(sum_008*(2^((i)/3)))
  df_US_double3days$newCases_009[i+36] <- round(sum_009*(2^((i+1)/3)))-round(sum_009*(2^((i)/3)))
  df_US_double3days$newCases_010[i+36] <- round(sum_010*(2^((i+1)/3)))-round(sum_010*(2^((i)/3)))
  df_US_double3days$newCases_011[i+36] <- round(sum_011*(2^((i+1)/3)))-round(sum_011*(2^((i)/3)))
  df_US_double3days$newCases_012[i+36] <- round(sum_012*(2^((i+1)/3)))-round(sum_012*(2^((i)/3)))
  df_US_double3days$newCases_013[i+36] <- round(sum_013*(2^((i+1)/3)))-round(sum_013*(2^((i)/3)))
  df_US_double3days$newCases_014[i+36] <- round(sum_014*(2^((i+1)/3)))-round(sum_014*(2^((i)/3)))
  df_US_double3days$newCases_015[i+36] <- round(sum_015*(2^((i+1)/3)))-round(sum_015*(2^((i)/3)))
}

# For 4 day doubling.
df_US_double4days$newCases_005[36] <- round(sum_005*(2^(1/4)))-sum_005
df_US_double4days$newCases_006[36] <- round(sum_006*(2^(1/4)))-sum_006
df_US_double4days$newCases_007[36] <- round(sum_007*(2^(1/4)))-sum_007
df_US_double4days$newCases_008[36] <- round(sum_008*(2^(1/4)))-sum_008
df_US_double4days$newCases_009[36] <- round(sum_009*(2^(1/4)))-sum_009
df_US_double4days$newCases_010[36] <- round(sum_010*(2^(1/4)))-sum_010
df_US_double4days$newCases_011[36] <- round(sum_011*(2^(1/4)))-sum_011
df_US_double4days$newCases_012[36] <- round(sum_012*(2^(1/4)))-sum_012
df_US_double4days$newCases_013[36] <- round(sum_013*(2^(1/4)))-sum_013
df_US_double4days$newCases_014[36] <- round(sum_014*(2^(1/4)))-sum_014
df_US_double4days$newCases_015[36] <- round(sum_015*(2^(1/4)))-sum_015

for (i in (1:(len_df-36))) {
  df_US_double4days$newCases_005[i+36] <- round(sum_005*(2^((i+1)/4)))-round(sum_005*(2^((i)/4)))
  df_US_double4days$newCases_006[i+36] <- round(sum_006*(2^((i+1)/4)))-round(sum_006*(2^((i)/4)))
  df_US_double4days$newCases_007[i+36] <- round(sum_007*(2^((i+1)/4)))-round(sum_007*(2^((i)/4)))
  df_US_double4days$newCases_008[i+36] <- round(sum_008*(2^((i+1)/4)))-round(sum_008*(2^((i)/4)))
  df_US_double4days$newCases_009[i+36] <- round(sum_009*(2^((i+1)/4)))-round(sum_009*(2^((i)/4)))
  df_US_double4days$newCases_010[i+36] <- round(sum_010*(2^((i+1)/4)))-round(sum_010*(2^((i)/4)))
  df_US_double4days$newCases_011[i+36] <- round(sum_011*(2^((i+1)/4)))-round(sum_011*(2^((i)/4)))
  df_US_double4days$newCases_012[i+36] <- round(sum_012*(2^((i+1)/4)))-round(sum_012*(2^((i)/4)))
  df_US_double4days$newCases_013[i+36] <- round(sum_013*(2^((i+1)/4)))-round(sum_013*(2^((i)/4)))
  df_US_double4days$newCases_014[i+36] <- round(sum_014*(2^((i+1)/4)))-round(sum_014*(2^((i)/4)))
  df_US_double4days$newCases_015[i+36] <- round(sum_015*(2^((i+1)/4)))-round(sum_015*(2^((i)/4)))
}


# For 5 day doubling.
df_US_double5days$newCases_005[36] <- round(sum_005*(2^(1/5)))-sum_005
df_US_double5days$newCases_006[36] <- round(sum_006*(2^(1/5)))-sum_006
df_US_double5days$newCases_007[36] <- round(sum_007*(2^(1/5)))-sum_007
df_US_double5days$newCases_008[36] <- round(sum_008*(2^(1/5)))-sum_008
df_US_double5days$newCases_009[36] <- round(sum_009*(2^(1/5)))-sum_009
df_US_double5days$newCases_010[36] <- round(sum_010*(2^(1/5)))-sum_010
df_US_double5days$newCases_011[36] <- round(sum_011*(2^(1/5)))-sum_011
df_US_double5days$newCases_012[36] <- round(sum_012*(2^(1/5)))-sum_012
df_US_double5days$newCases_013[36] <- round(sum_013*(2^(1/5)))-sum_013
df_US_double5days$newCases_014[36] <- round(sum_014*(2^(1/5)))-sum_014
df_US_double5days$newCases_015[36] <- round(sum_015*(2^(1/5)))-sum_015

for (i in (1:(len_df-36))) {
  df_US_double5days$newCases_005[i+36] <- round(sum_005*(2^((i+1)/5)))-round(sum_005*(2^((i)/5)))
  df_US_double5days$newCases_006[i+36] <- round(sum_006*(2^((i+1)/5)))-round(sum_006*(2^((i)/5)))
  df_US_double5days$newCases_007[i+36] <- round(sum_007*(2^((i+1)/5)))-round(sum_007*(2^((i)/5)))
  df_US_double5days$newCases_008[i+36] <- round(sum_008*(2^((i+1)/5)))-round(sum_008*(2^((i)/5)))
  df_US_double5days$newCases_009[i+36] <- round(sum_009*(2^((i+1)/5)))-round(sum_009*(2^((i)/5)))
  df_US_double5days$newCases_010[i+36] <- round(sum_010*(2^((i+1)/5)))-round(sum_010*(2^((i)/5)))
  df_US_double5days$newCases_011[i+36] <- round(sum_011*(2^((i+1)/5)))-round(sum_011*(2^((i)/5)))
  df_US_double5days$newCases_012[i+36] <- round(sum_012*(2^((i+1)/5)))-round(sum_012*(2^((i)/5)))
  df_US_double5days$newCases_013[i+36] <- round(sum_013*(2^((i+1)/5)))-round(sum_013*(2^((i)/5)))
  df_US_double5days$newCases_014[i+36] <- round(sum_014*(2^((i+1)/5)))-round(sum_014*(2^((i)/5)))
  df_US_double5days$newCases_015[i+36] <- round(sum_015*(2^((i+1)/5)))-round(sum_015*(2^((i)/5)))
}


# For 6 day doubling.
df_US_double6days$newCases_005[36] <- round(sum_005*(2^(1/6)))-sum_005
df_US_double6days$newCases_006[36] <- round(sum_006*(2^(1/6)))-sum_006
df_US_double6days$newCases_007[36] <- round(sum_007*(2^(1/6)))-sum_007
df_US_double6days$newCases_008[36] <- round(sum_008*(2^(1/6)))-sum_008
df_US_double6days$newCases_009[36] <- round(sum_009*(2^(1/6)))-sum_009
df_US_double6days$newCases_010[36] <- round(sum_010*(2^(1/6)))-sum_010
df_US_double6days$newCases_011[36] <- round(sum_011*(2^(1/6)))-sum_011
df_US_double6days$newCases_012[36] <- round(sum_012*(2^(1/6)))-sum_012
df_US_double6days$newCases_013[36] <- round(sum_013*(2^(1/6)))-sum_013
df_US_double6days$newCases_014[36] <- round(sum_014*(2^(1/6)))-sum_014
df_US_double6days$newCases_015[36] <- round(sum_015*(2^(1/6)))-sum_015

for (i in (1:(len_df-36))) {
  df_US_double6days$newCases_005[i+36] <- round(sum_005*(2^((i+1)/6)))-round(sum_005*(2^((i)/6)))
  df_US_double6days$newCases_006[i+36] <- round(sum_006*(2^((i+1)/6)))-round(sum_006*(2^((i)/6)))
  df_US_double6days$newCases_007[i+36] <- round(sum_007*(2^((i+1)/6)))-round(sum_007*(2^((i)/6)))
  df_US_double6days$newCases_008[i+36] <- round(sum_008*(2^((i+1)/6)))-round(sum_008*(2^((i)/6)))
  df_US_double6days$newCases_009[i+36] <- round(sum_009*(2^((i+1)/6)))-round(sum_009*(2^((i)/6)))
  df_US_double6days$newCases_010[i+36] <- round(sum_010*(2^((i+1)/6)))-round(sum_010*(2^((i)/6)))
  df_US_double6days$newCases_011[i+36] <- round(sum_011*(2^((i+1)/6)))-round(sum_011*(2^((i)/6)))
  df_US_double6days$newCases_012[i+36] <- round(sum_012*(2^((i+1)/6)))-round(sum_012*(2^((i)/6)))
  df_US_double6days$newCases_013[i+36] <- round(sum_013*(2^((i+1)/6)))-round(sum_013*(2^((i)/6)))
  df_US_double6days$newCases_014[i+36] <- round(sum_014*(2^((i+1)/6)))-round(sum_014*(2^((i)/6)))
  df_US_double6days$newCases_015[i+36] <- round(sum_015*(2^((i+1)/6)))-round(sum_015*(2^((i)/6)))
}


# For 7 day doubling.
df_US_double7days$newCases_005[36] <- round(sum_005*(2^(1/7)))-sum_005
df_US_double7days$newCases_006[36] <- round(sum_006*(2^(1/7)))-sum_006
df_US_double7days$newCases_007[36] <- round(sum_007*(2^(1/7)))-sum_007
df_US_double7days$newCases_008[36] <- round(sum_008*(2^(1/7)))-sum_008
df_US_double7days$newCases_009[36] <- round(sum_009*(2^(1/7)))-sum_009
df_US_double7days$newCases_010[36] <- round(sum_010*(2^(1/7)))-sum_010
df_US_double7days$newCases_011[36] <- round(sum_011*(2^(1/7)))-sum_011
df_US_double7days$newCases_012[36] <- round(sum_012*(2^(1/7)))-sum_012
df_US_double7days$newCases_013[36] <- round(sum_013*(2^(1/7)))-sum_013
df_US_double7days$newCases_014[36] <- round(sum_014*(2^(1/7)))-sum_014
df_US_double7days$newCases_015[36] <- round(sum_015*(2^(1/7)))-sum_015

for (i in (1:(len_df-36))) {
  df_US_double7days$newCases_005[i+36] <- round(sum_005*(2^((i+1)/7)))-round(sum_005*(2^((i)/7)))
  df_US_double7days$newCases_006[i+36] <- round(sum_006*(2^((i+1)/7)))-round(sum_006*(2^((i)/7)))
  df_US_double7days$newCases_007[i+36] <- round(sum_007*(2^((i+1)/7)))-round(sum_007*(2^((i)/7)))
  df_US_double7days$newCases_008[i+36] <- round(sum_008*(2^((i+1)/7)))-round(sum_008*(2^((i)/7)))
  df_US_double7days$newCases_009[i+36] <- round(sum_009*(2^((i+1)/7)))-round(sum_009*(2^((i)/7)))
  df_US_double7days$newCases_010[i+36] <- round(sum_010*(2^((i+1)/7)))-round(sum_010*(2^((i)/7)))
  df_US_double7days$newCases_011[i+36] <- round(sum_011*(2^((i+1)/7)))-round(sum_011*(2^((i)/7)))
  df_US_double7days$newCases_012[i+36] <- round(sum_012*(2^((i+1)/7)))-round(sum_012*(2^((i)/7)))
  df_US_double7days$newCases_013[i+36] <- round(sum_013*(2^((i+1)/7)))-round(sum_013*(2^((i)/7)))
  df_US_double7days$newCases_014[i+36] <- round(sum_014*(2^((i+1)/7)))-round(sum_014*(2^((i)/7)))
  df_US_double7days$newCases_015[i+36] <- round(sum_015*(2^((i+1)/7)))-round(sum_015*(2^((i)/7)))
}

# Predict new deaths
# start at row 59 because that is March 20th.  This model had real data for March 19th and earlier.
# All deaths predictions will be the same because starting with a ratio to multiply and then
# dividing by same ratio at the last step.
for ( i in (59:len_df)) {
  # 3 day doubling
  df_US_double3days$predictedDeaths[i] <- round(df_US_double3days$newCases_005[i-23]*.005)
  df_US_double4days$predictedDeaths[i] <- round(df_US_double4days$newCases_005[i-23]*.005)
  df_US_double5days$predictedDeaths[i] <- round(df_US_double5days$newCases_005[i-23]*.005)
  df_US_double6days$predictedDeaths[i] <- round(df_US_double6days$newCases_005[i-23]*.005)
  df_US_double7days$predictedDeaths[i] <- round(df_US_double7days$newCases_005[i-23]*.005)

}




#   dplyr::group_by(Country.Region, type) %>%
#   dplyr::summarise(total = sum(cases)) %>%
#   tidyr::pivot_wider(names_from =  type, 
#                      values_from = total) %>%
#   dplyr::mutate(unrecovered = confirmed - ifelse(is.na(recovered), 0, recovered) - ifelse(is.na(death), 0, death)) %>%
#   dplyr::arrange(-confirmed) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(country = dplyr::if_else(Country.Region == "United Arab Emirates", "UAE", Country.Region)) %>%
#   dplyr::mutate(country = dplyr::if_else(country == "Mainland China", "China", country)) %>%
#   dplyr::mutate(country = dplyr::if_else(country == "North Macedonia", "N.Macedonia", country)) %>%
#   dplyr::mutate(country = trimws(country)) %>%
#   dplyr::mutate(country = factor(country, levels = country))
# 
# 
# df_daily_US <- coronavirus %>% 
#   dplyr::filter(Country.Region == "US") %>%
#   dplyr::group_by(date, type) %>%
#   dplyr::summarise(total = sum(cases, na.rm = TRUE)) %>%
#   tidyr::pivot_wider(names_from = type,
#                      values_from = total) %>%
#   dplyr::arrange(date) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(active =  confirmed - death - recovered) %>%
#   dplyr::mutate(confirmed_cum = cumsum(confirmed),
#                 death_cum = cumsum(death),
#                 recovered_cum = cumsum(recovered),
#                 active_cum = cumsum(active))
# 
# df_daily2_US <- coronavirus %>% 
#   dplyr::filter(Country.Region == "US") %>%
#   dplyr::filter(date == max(date)) %>%
#   dplyr::group_by(date, type) %>%
#   dplyr::summarise(total = sum(cases, na.rm = TRUE)) %>%
#   tidyr::pivot_wider(names_from = type,
#                      values_from = total) %>%
#   dplyr::arrange(date) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(active =  confirmed - death - recovered) %>%
#   dplyr::mutate(confirmed_cum = cumsum(confirmed),
#                 death_cum = cumsum(death),
#                 recovered_cum = cumsum(recovered),
#                 active_cum = cumsum(active))