library(ggplot2)
library(tidyverse)

ducks <- read.csv("EVEducks.csv", header = T)
head(data)

data %>%
  ggplot(aes(x = species, y = lipid, fill = species, alpha = 0.5))+
  geom_boxplot()+
  facet_wrap(~year,2)
