library(tidyverse)
library(readxl)

df <- read_excel("AUSNUT_BI_collapser.xlsx", sheet = "food nutrients")

glimpse(df)

df2 <- df %>%
  gather("nutrient", "value", 3:55) %>%
  group_by(`common name`, nutrient) %>%
  summarise(id = max(`Food ID`), val = mean(value)) %>%
  spread(nutrient, val)

glimpse(df2)

write.csv(df2, "collapsed.csv")
