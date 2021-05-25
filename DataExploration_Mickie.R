# EVE 298 Group Project - Ducks
# author: Mickie Tang
# Date: May 24, 2021

# Questions: 
# How does duck body condition (lipid) change over time?
# How does species and sex affect body condition over time?

# Working model: 
# lipid ~ year + date*species + sex + molt? + (1|site)

### Data Exploration ----

# read in the data
ducks <- read.csv("EVEducks.csv")
str(ducks)
head(ducks)
tail(ducks)

ducks$year <- as.factor(ducks$ï..year) # make year into a factor instead of num

# how to convert dates to "dates" in R?
# was there a species we wanted to remove from the analysis?

# plot the response variable, lipid
hist(ducks$lipid)
dotchart(ducks$lipid)

# plot the predictor variables

library(ggplot2)

# species
ggplot(ducks, aes(x = species, y = lipid)) +
  geom_jitter(alpha=0.2) +
  geom_boxplot(alpha=0.5)
  
# sex grouped by species - there might be a sex*species interaction
ggplot(ducks, aes(x = sex, y = lipid)) +
  geom_jitter(alpha=0.2) +
  geom_boxplot(alpha=0.5) +
  facet_wrap(~ species)

# year grouped by species
ggplot(ducks, aes(x = year, y = lipid)) +
  geom_jitter(alpha=0.2) +
  geom_boxplot(alpha=0.5) +
  facet_wrap(~ species)

# year grouped by species and sex
ggplot(ducks, aes(x = year, y = lipid, color=sex)) +
  geom_jitter(alpha=0.2) +
  geom_boxplot(alpha=0.5) +
  facet_wrap(~ species)

# site grouped by species
ggplot(ducks, aes(x = site, y = lipid)) +
  geom_jitter(alpha=0.2) +
  geom_boxplot(alpha=0.5) +
  facet_wrap(~ species) +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.5))



### Diagnostics ----

# need to figure out the model before we can do diagnostics...
