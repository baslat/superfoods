library(shiny)
library(tidyverse)
library(DT)
library(plotly)

food_df <- read.csv("AUSNUT3.csv", stringsAsFactors = FALSE)



glimpse(food_df)
#names(food_df) <- make.names(names(food_df))


colnames(food_df) <- tolower(colnames(food_df))
axis_vars <- sort(colnames(food_df[4:34]))
col_vars <- colnames(food_df[1:2])
major_groups <- sort(unique(food_df$major.group))
major_groups
axis_vars


ptile <- 0.0

### test

df <- food_df %>%
  filter(major.group == "Vegetable products and dishes", dietary.fibre..g. >= quantile(dietary.fibre..g., ptile)) %>%
  arrange(desc(dietary.fibre..g.))

#df


ggplotly(ggplot(data = df, aes(x = reorder(minor.group, -dietary.fibre..g.), y = dietary.fibre..g., fill = sub.major.group, text = minor.group)) + 
  geom_bar(stat = "identity"), tooltip = c("y", "text"))

# 
# 
# fg <- major_groups[23]
# y <- axis_vars[2]
# y_th <- 0.5
# colour <- col_vars[1]
# 
# ### here is the problem
# bar_df <- food_df %>%
#   filter(major.group %in% fg, food_df$fg >= quantile(y, y_th)) %>%
#   arrange(desc(y))  
# 
# 
# ggplotly(ggplot(data = df, aes(x = reorder(minor.group, -y), y = y, fill = colour, text = minor.group)) +
#            geom_bar(stat = "identity"), tooltip = c("y", "text"))
# 



#### end test



ui <- fluidPage(# Sidebar layout with a input and output definitions
  sidebarLayout(
    # Inputs
    sidebarPanel(
      # Select variable for y-axis
      selectInput(
        inputId = "nut",
        label = "Nutrient:",
        choices = axis_vars,
        selected = axis_vars[2]
      ),
      
      # slider for y axis
      sliderInput(inputId = "y_th",
                  label = "You only want to look at foods that are greater that what proportion of the Y values?",
                  min = 0, max = 1,
                  value = 0.5),
      
      
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
      plotlyOutput(outputId = "bars"),
      dataTableOutput(outputId = "table")
    )
  ))

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  
  
  # Create scatterplot object the plotlyOutput function is expecting
  output$bars <- renderPlotly({
    req(input$fg)
    req(input$y_th)
    
    # bar_df <- food_df %>%
    #   filter(major.group %in% input$fg) %>%
    #   arrange(desc(input$y))  
    
    
    ggplotly(ggplot(data = filter(food_df, major.group %in% input$fg), aes(x = minor.group, y = input$nut, fill = input$colour, text = minor.group)) +
               geom_bar(stat = "identity"), tooltip = c("input$nut", "text"))
  })
  
  # create table
  output$table <- renderDataTable({
    req(input$fg)
    table_foods <- food_df %>%
      filter(major.group %in% input$fg) %>%
      select(major.group:minor.group, input$nut)
    datatable(table_foods, filter = 'top', options = list(pageLength = 10), rownames = FALSE)
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
