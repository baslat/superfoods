library(shiny)
library(tidyverse)
library(readxl)
library(DT)
library(plotly)
library(scales)
library(ggvis)

#### import: bring in the data, keep just the food name, categories and macro ratios ####
food_nutrients <- 
  read_excel("AUSNUT_BI.xlsx", sheet = "food nutrients") %>%
  select(`Food ID`, ends_with("ratio)"))

food_list <- 
  read_excel("AUSNUT_BI.xlsx", sheet = "food list") %>%
  select(-`Survey ID`)


food_groups <- read_excel("AUSNUT_BI.xlsx", sheet = "food groups")

food_df <- 
  left_join(food_nutrients, food_list) %>%
  left_join(food_groups) %>%
  select(-c(`Food ID`, code))


rm(food_nutrients, food_list, food_groups)
gc()

food_names <- food_df$`Food Name`

food_df <- 
  gather(food_df, macro, pct, 1:4) %>%
  filter(pct > 0)

glimpse(food_df)
#### end import, food_df and food_names are good to go ####

#### testing ####

# sample data for testing

# test <- 
#   sample_n(food_df, 4) %>%
#   gather(macro, pct, 1:4)
# 
# test$macro <- as.factor(test$macro)

#glimpse(test)

ggplot(data = test, aes(x = `Food Name`, y = pct, fill = macro, label = scales::percent(pct))) +
  geom_bar(stat = "identity", position = "fill") + 
  scale_fill_brewer(palette = "Accent", name = "", labels = c("Alcohol", "Carbs", "Fats", "Protein")) + 
  labs(title = "Where do the calories come from?", subtitle = "Estimated macro ratios per 100g", y = "Calorie percent", x = "") + 
  coord_flip() +
  scale_y_continuous(labels = percent) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 40))

#### end testing ####
            
#### start UI ####
ui <- fluidPage(# Sidebar layout with a input and output definitions
  sidebarLayout(
    # Inputs
    sidebarPanel(
      
      # Select variable for x-axis
      selectizeInput(
        inputId = "foods",
        label = "What foods do you want to compare? \nYou can search too :)",
        choices = food_names,
        selected = food_names[996],
        multiple = T
      )
    ),
    
    # Outputs
    mainPanel(
      plotOutput(outputId = "barchart")
      #dataTableOutput(outputId = "nutrient_table")
    )
  ))

#### end UI ####


#### start server ####
# Define server function required to create the scatterplot

server <- function(input, output) {
  # Create scatterplot object the plotOutput function is expecting
  output$barchart <- renderPlot({
    req(input$foods)
    ggplot(data = filter(food_df, `Food Name` %in% input$foods), aes(x = `Food Name`, y = pct, fill = macro, label = scales::percent(pct))) +
      geom_bar(stat = "identity", position = "fill") + 
      scale_fill_brewer(palette = "Accent", name = "", labels = c("Alcohol", "Carbs", "Fats", "Protein")) + 
      labs(title = "Where do the calories come from?", subtitle = "Estimated macro ratios per 100g", y = "Calorie percent", x = "") + 
      coord_flip() +
      scale_y_continuous(labels = percent) + 
      scale_x_discrete(labels = function(x) str_wrap(x, width = 40))
  })
  
  # # create table
  # output$nutrient_table <- renderDataTable({
  #   req(input$x)
  #   table_foods <- food_df %>%
  #     filter(Major.group %in% input$fg) %>%
  #     select(Food.Name, Major.group:Group, input$x, input$y)
  #   datatable(table_foods, options = list(pageLength = 10), rownames = FALSE)
  # })
}


#### end server ####