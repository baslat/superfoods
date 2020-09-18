library(shiny)
library(tidyverse)
library(readxl)
library(DT)
library(plotly)

food_df <- read_csv("AUSNUT.csv")

names(food_df) <- make.names(names(food_df))

food_df$Major.group <- as.factor(food_df$Major.group)
food_df$Sub.major.group <- as.factor(food_df$Sub.major.group)
food_df$Minor.Group <- as.factor(food_df$Minor.Group)

axis_vars <- sort(colnames(food_df[5:57]))
col_vars <- colnames(food_df[1:2])
major_groups <- sort(unique(food_df$Major.group))


### to do: make groups factors?


ui <- fluidPage(# Sidebar layout with a input and output definitions
  sidebarLayout(
    # Inputs
    sidebarPanel(
      # Select variable for y-axis
      selectInput(
        inputId = "y",
        label = "Y-axis:",
        choices = axis_vars,
        selected = axis_vars[23]
      ),
      
      # slider for y axis
      sliderInput(inputId = "y_th",
                  label = "You only want to look at foods that are greater that what proportion of the Y values?",
                  min = 0, max = 1,
                  value = 0.8),
      
      # Select variable for x-axis
      selectInput(
        inputId = "x",
        label = "X-axis:",
        choices = axis_vars,
        selected = axis_vars[32]
      ),
      
      # slider for x axis
      sliderInput(inputId = "x_th",
                  label = "You only want to look at foods that are greater that what proportion of the X values?",
                  min = 0, max = 1,
                  value = 1),
      
      # Select variable for color
      selectInput(
        inputId = "colour",
        label = "Colour by:",
        choices = col_vars,
        selected = col_vars[1]
      ),
      
      # Select variable for food group
      selectInput(
        inputId = "fg",
        label = "Food group:",
        choices = major_groups,
        selected = major_groups[23],
        multiple = TRUE
      )
    ),
    
    # Outputs
    mainPanel(
      plotlyOutput(outputId = "scatterplot"),
      dataTableOutput(outputId = "nutrient_table")
      )
  ))

# Define server function required to create the scatterplot
server <- function(input, output) {
  

  
  # Create scatterplot object the plotlyOutput function is expecting
  output$scatterplot <- renderPlotly({
    req(input$fg)
    req(input$x_th)
    ggplotly(ggplot(data = filter(food_df, Major.group %in% input$fg), aes_string(x = input$x, y = input$y, colour = input$colour)) +
      geom_point())
  })
  
  # create table
  output$nutrient_table <- renderDataTable({
    req(input$fg)
    table_foods <- food_df %>%
      filter(Major.group %in% input$fg) %>%
      select(Food.Name, Major.group:Minor.Group, input$x, input$y)
    datatable(table_foods, filter = 'top', options = list(pageLength = 10), rownames = FALSE)
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
