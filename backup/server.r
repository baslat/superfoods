#### start server ####
# Define server function required to create the scatterplot

server <- function(input, output) {

  
  # Create bar plot object the plotOutput function is expecting
  output$barchart <- renderPlot({
    req(input$foods)
    ggplot(data = filter(food_df, `Food Name` %in% input$foods), aes(x = `Food Name`, y = pct, fill = macro, label = ifelse(pct != 0, paste(scales::percent(pct), macro, sep = "\n"), NA))) +
      geom_bar(stat = "identity", position = "fill") + 
      scale_fill_brewer(palette = "Accent", name = "") + 
      labs(title = "Where do the calories come from?", subtitle = "Estimated macro ratios per 100g", y = "Calorie percent", x = "") + 
      coord_flip() +
      scale_y_continuous(labels = percent) +
      theme_minimal() + 
      theme(legend.position = "top", axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size = 12), plot.subtitle = element_text(size = 12), plot.title = element_text(size = 16)) + 
      geom_text(position = position_stack(vjust = 0.5)) + 
      scale_x_discrete(labels = function(x) str_wrap(x, width = 30))
  })

  #### data table ####
  
  # output$datatable <- renderDataTable({
  #   req(input$foods)
  #   dat <- food_wide[,c(5,1,2,4,3)] %>%
  #     filter(`Food Name` %in% input$foods) %>%
  #     datatable() %>%
  #     formatPercentage(2:5, 1)
  #   
  #   
  # })
  #### end data table ####
}


#### end server ####