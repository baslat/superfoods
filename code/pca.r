library(tidyverse)
library(readxl)
library(plotly)
library(shiny)


foods <- read_excel("data/AUSNUT.xlsx", sheet = "Food Nutrient Database")

data <- select(foods, 7:59)

pc <- prcomp(data, scale. = T)
names(pc)

biplot(pc, scale = 0)

std_dev <- pc$sdev
pr_var <- std_dev^2
pr_var[1:10]
prop_varex <- pr_var/sum(pr_var)
plot(prop_varex, xlab = "PC",
     ylab = "Prop var explained",
     type = "b")
plot(cumsum(prop_varex), xlab = "PC",
     ylab = "Cum prop of var",
     type = "b")
cumsum(prop_varex[1:35])
