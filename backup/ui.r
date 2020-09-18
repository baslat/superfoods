#### start UI ####
ui <- fluidPage(# Sidebar layout with a input and output definitions
  titlePanel("Estimated macro ratios"),
  
  
  sidebarLayout(
    # Inputs
    sidebarPanel(
      
      # Select variable for x-axis
      selectizeInput(
        inputId = "foods",
        label = "What foods do you want to compare?",
        choices = food_names,
        selected = c(food_names[440],food_names[3600]),
        multiple = T
      )
    ),
    
    # Outputs
    mainPanel(
      plotOutput(outputId = "barchart")
      # br(),br(),
      # dataTableOutput(outputId = "datatable")
      #dataTableOutput(outputId = "nutrient_table")
    )
  ))
#### end UI ####