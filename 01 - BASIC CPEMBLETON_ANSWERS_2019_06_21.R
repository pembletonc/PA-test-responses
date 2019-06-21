library(tidyverse)
#1. read in the data

paths <- paste0("fertility-challenge/", list.files("fertility-challenge/", pattern = "*.txt"))
births.1995.2017 <- map_dfr(paths, ~ read_tsv(.x))


#2. Exploratory Data Analysis 



#2.1 Data Cleaning


#3. Time series plot of total number of births with state with highest average highlighted in red


#4. Regression Model Observing how state and year impact birth rates

#4. a) Did birth rates increase or decrease over time?


#4. b) Interpretation of the intercept term



