library(tidyverse)
library(naniar)
library(kableExtra)
library(DataExplorer)
#1. read in the data

paths <- paste0("fertility-challenge/", list.files("fertility-challenge/", pattern = "*.txt"))
Births <- map_dfr(paths, ~ read_tsv(.x))



# 2. Exploratory Data Analysis 

# 2.1 Overview of dataset 

glimpse(Births)
# 1,473 rows and 10 columns

plot_intro(Births, ggtheme = theme_minimal())

plot_missing(Births)

Births %>% 
  group_by(Notes) %>% 
  tally %>% 
  arrange(desc(n)) 
# 2.2 Where are the NA values in the variables?

Births %>% 
  group_by(Year) %>% 
  naniar::gg_miss_var(show_pct = TRUE, facet = Year) +
  ggtitle("% missing values, by sex and variable")

Births <- Births %>% filter(is.na(Notes)) %>% select(-Notes)

Births %>% 
  group_by(Year) %>% 
  naniar::gg_miss_var(show_pct = TRUE, facet = Year) +
  ggtitle("% missing values, by Year")

#3. Time series plot of total number of births with state with highest average highlighted in red





#4. Regression Model Observing how state and year impact birth rates

#4. a) Did birth rates increase or decrease over time?


#4. b) Interpretation of the intercept term



