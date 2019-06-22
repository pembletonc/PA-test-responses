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

ggplot(data = ss, aes(x = date, y = pop)) + 
  geom_line(color = "#FC4E07", size = 2)

Births$Births

Births %>% 
  group_by(Year) %>% 
  summarise(total = sum(Births)) %>% 
  ggplot(aes(x = Year, y = total)) +
  geom_line(size = 1, color = "lightgray") + 
  coord_cartesian(xlim = c(1995, 2017)) +
  ylab("Total Births")+
  scale_y_continuous(label=scales::comma)+
  scale_x_continuous(breaks = seq(1995, 2017, 3))+
  theme_minimal(12) + 
  theme(legend.position = "none",
        axis.text.x = element_text(hjust = .75),
        plot.title = element_text(size=14, hjust = 0.5),
        plot.caption = element_text(size = 12, hjust = 1),
        axis.text.y = element_text(hjust = 1),
        plot.margin = unit(c(1,1,0.5,1), "cm")) +
  ggtitle("Total Annual US Births, 1995 - 2017")


seq_along(c(1995:2017))

#4. Regression Model Observing how state and year impact birth rates

#4. a) Did birth rates increase or decrease over time?


#4. b) Interpretation of the intercept term



