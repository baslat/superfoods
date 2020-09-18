library(tidyverse)
library(readxl)
library(plotly)
library(shiny)

Sys.setenv("plotly_username"="baslat")
Sys.setenv("plotly_api_key"="n3yonLEdhRZO4ZGULYmA")


foods <- read_excel("AUSNUT.xlsx", sheet = "Food Nutrient Database")
unique(foods$`Major group`)
glimpse(foods)

df <- filter(foods, `Major group` == "Meat, poultry and game products and dishes")
df <- foods

# g <- ggplot(df, aes(`Zinc (Zn) (mg)`, `Magnesium (Mg) (mg)`, colour = `Minor group`)) + 
#   geom_jitter() 
#   #geom_smooth()
# g
# 
# #convert ggplot to plotly
# p <- ggplotly(g)

#make plotly from scratch
p <- plot_ly(
  df, x = ~`Zinc (Zn) (mg)`, y = ~`Magnesium (Mg) (mg)`,
  color = ~`Minor group`,
  text = ~`Food Name`) %>%
  layout(
    title = "Food Nutrients",
    updatemenus = list(
      list(
        buttons = list(
          list(method = "restyle",
               args = list("x", list(df$`Total fat (g)`)),  # put it in a list
               label = "Show fat"),
          
          list(method = "restyle",
               args = list("y", list(df$`Dietary fibre (g)`)),  # put it in a list
               label = "Show fibre")))
    ))



p


# publish the chart
api_create(p, filename = "nutrients")




# to do with shiny?

# Variables that can be put on the x and y axes

# go here for more https://github.com/rstudio/shiny-examples/tree/master/051-movie-explorer

# axis_vars <- c(
#   "Tomato Meter" = "Meter",
#   "Numeric Rating" = "Rating",
#   "Number of reviews" = "Reviews",
#   "Dollars at box office" = "BoxOffice",
#   "Year" = "Year",
#   "Length (minutes)" = "Runtime"
# )
axis_vars <- colnames(foods[7:59])
