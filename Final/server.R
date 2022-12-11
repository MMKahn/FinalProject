# Load the required packages
library(tidyverse)
library(dplyr)
library(caret)
library(shiny)
library(DT)
library(ggplot2)

# Read in and manipulate data
math <- read_csv("./Final/student-mat.csv")
# Numeric variables
numericVars <- math %>% 
  select_if(is.numeric)
# Character variables
charVars <- math %>%
  select_if(is.character)

#Set up server
shinyServer(function(input, output, session){
  
  # Create data table 
  output$dataTable<-renderDataTable({
    numericType <- input$numericType
    
    if(numericType == "mean"){
      math[math]
    } 
    
    else if(numericType == "boxPlot"){
      g2 <- ggplot(data = math, aes(x = get(input$boxXvar), y = get(input$boxYvar)))
      g2 + geom_boxplot() +
        geom_jitter(aes(color = get(input$boxXvar))) +
        labs(title = "Boxplot")
    } 
    
    else{
      g3 <- ggplot(data = math, aes(x = get(input$scatterXvar), y = get(input$scatterYvar)))
      g3 + geom_point() +
        geom_smooth(method = lm, col = "Blue") +
        labs(title = "Scatter Plot")
    }
  })

  # Create bar plot to visualize EDA data
  output$dataPlot<-renderPlot({
    plotType <- input$plotType
    if(plotType == "barGraph"){
      g <- ggplot(data = math, aes(x = get(input$barXvar)))
      g + geom_bar() +
        labs(title = "Bar Graph")
    } 

    else if(plotType == "boxPlot"){
      g2 <- ggplot(data = math, aes(x = get(input$boxXvar), y = get(input$boxYvar)))
      g2 + geom_boxplot() +
        geom_jitter(aes(color = get(input$boxXvar))) +
        labs(title = "Boxplot")
    } 
    
    else{
      g3 <- ggplot(data = math, aes(x = get(input$scatterXvar), y = get(input$scatterYvar)))
      g3 + geom_point() +
        geom_smooth(method = lm, col = "Blue") +
        labs(title = "Scatter Plot")
    }
  })
  
})