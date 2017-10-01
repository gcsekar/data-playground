#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  
  dataset <- reactive( {
    mtcars[sample(nrow(mtcars), input$sampleSize),]
  })
  
  output$plot <- reactivePlot(function() {
    
    g <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point()
    
    if (input$color != 'None')
      g <- g + aes_string(color=input$color)
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .')
      g <- g + facet_grid(facets)
    
    if (input$smooth)
      g <- g + geom_smooth()
    
    g <- g + ggtitle("Analysis for MT Cars dataset", "(with Shiny)")
    print(g)
    
  }, height=600
  )
})