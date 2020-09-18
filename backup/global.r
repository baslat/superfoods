library(shiny)
library(tidyverse)
library(readxl)
library(DT)
library(scales)
library(ggrepel)

#### import: bring in the data, keep just the food name, categories and macro ratios ####
food_nutrients <- 
  read_excel("AUSNUT_BI.xlsx", sheet = "food nutrients") %>%
  select(`Food ID`, ends_with("ratio)")) %>%
  rename(Protein = `Protein (cal ratio)`, Fats = `Fats (cal ratio)`, Alcohol = `Alcohol (cal ratio)`, Carbs = `Carbs (cal ratio)`)

food_list <- 
  read_excel("AUSNUT_BI.xlsx", sheet = "food list") %>%
  select(-`Survey ID`)

food_df <- 
  left_join(food_nutrients, food_list) %>%
  select(-c(`Food ID`, code))

rm(food_nutrients, food_list)
gc()

food_names <- sort(food_df$`Food Name`)

food_wide <- food_df

food_df <- 
  gather(food_df, macro, pct, Protein:Carbs)


#### end import, food_df and food_names are good to go ####

#### testing ####

# sample data for testing

# test <- 
#   sample_n(food_df, 4) %>%
#   gather(macro, pct, 1:4)
# 
# test$macro <- as.factor(test$macro)

#glimpse(test)

# ggplot(data = test, aes(x = `Food Name`, y = pct, fill = macro, label = scales::percent(pct))) +
#   geom_bar(stat = "identity", position = "fill") + 
#   scale_fill_brewer(palette = "Accent", name = "", labels = c("Alcohol", "Carbs", "Fats", "Protein")) + 
#   labs(title = "Where do the calories come from?", subtitle = "Estimated macro ratios per 100g", y = "Calorie percent", x = "") + 
#   coord_flip() +
#   scale_y_continuous(labels = percent) + 
#   scale_x_discrete(labels = function(x) str_wrap(x, width = 40))

#### end testing ####
          
