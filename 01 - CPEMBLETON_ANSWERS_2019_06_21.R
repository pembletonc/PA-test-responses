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


#4. Regression Model Observing how state and year impact birth rates

#4. a) Did birth rates increase or decrease over time?

Births_2002_over <- Births %>% filter(Year >= 2003)
Births_2002_over <- Births_2002_over %>% mutate(Year_New = Year-2003)

model1 <- lm(data = Births_2002_over, formula = `Birth Rate` ~ Year_New)

tmp <- capture.output(summary(model1))
cat(tmp[c(3:4, 9:13, 16:17)], sep='\n')

model2 <- lm(data = Births_2002_over, formula = `Birth Rate` ~ Year_New + State)

tmp2 <- capture.output(summary(model2))

cat(tmp2[c(3:4, 9:68)], sep='\n')

confint(model2)
#4. b) Interpretation of the intercept term


#DD test questions

#select only states that have population data
Births <- Births %>% 
  filter(`Total Population` > 0) 

Births_treated <- Births %>% 
  mutate(
    states_treated = case_when(
      State %in% c("New Jersey", "Georgia", "Texas") ~ 1,
      TRUE ~ 0
      ),
    post2008 = Year >= 2008 
      )

ggplot(Births_treated, aes(Year, `Fertility Rate`, color = as.logical(states_treated))) +
  stat_summary(geom = "line") +
  geom_vline(xintercept = 2008) +
  theme_minimal(12)

#4. Estimate the impact of the policy using a DID approach on the fertility rate. 

model = lm(`Fertility Rate` ~ states_treated*post2008, data = Births_treated)
model2 = lm(`Fertility Rate` ~ Year + states_treated + states_treated*post2008, data = Births_treated)

