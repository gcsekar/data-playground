#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

analyzeData <- mtcars
# Define UI for application that draws a histogram
shinyUI(pageWithSidebar(
  headerPanel("MTCars Data Analyzer"),
  sidebarPanel(
    
    sliderInput('sampleSize', 'Sample Size', min=10, max=nrow(analyzeData),
                value=nrow(analyzeData), step=5, round=0),
    
    selectInput('x', 'X', names(analyzeData),names(analyzeData)[1]),
    selectInput('y', 'Y', names(analyzeData), names(analyzeData)[6]),
    selectInput('color', 'Color', c('None', names(analyzeData)), names(analyzeData)[2]),
    
    checkboxInput('smooth', 'Smooth', value = 1),
    
    selectInput('facet_row', 'Facet Row', c(None='.', names(analyzeData))),
    selectInput('facet_col', 'Facet Column', c(None='.', names(analyzeData)))
  ),
  
  mainPanel(
    plotOutput('plot')
  )
  
  
))
