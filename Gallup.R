setwd("/Users/mikey/Downloads/")
library(dplyr)
library(naniar)
data = data.table(read.csv("Gallup.csv"))
data$depressed = (1-(data$MH7A-1)) # data[, depressed := fifelse(MH7A == 1, 0, fifelse(MH7A == 2, 1, NA))]
summary = data %>%
  filter(depressed != -97) %>%
  group_by(COUNTRYNEW)  %>% 
  summarise(weighted_depression = weighted.mean(depressed, WGT, na.rm=TRUE))  
print(summary, n=113)

summary_old = data %>%
  filter(depressed != -97) %>%
  filter(Age >= 55) %>%
  group_by(COUNTRYNEW)  %>% 
  summarise(weighted_depression = weighted.mean(depressed, WGT, na.rm=TRUE))
summary_old2 =  data[depressed != -97 & Age >= 55, weighted.mean(depressed, WGT, na.rm = TRUE) , COUNTRYNEW]
print(summary_old, n=113)

global_old = data %>%
  filter(depressed != -97) %>%
  filter(Age >= 55) %>%
  summarise(weighted_depression = weighted.mean(depressed, PROJWT, na.rm=TRUE))  
global_old

global = data %>%
  filter(depressed != -97) %>%
  summarise(weighted_depression = weighted.mean(depressed, PROJWT, na.rm=FALSE))  
global
