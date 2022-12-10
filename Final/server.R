# Load the required `tidyverse`, `caret`, `shiny`, `DT` and `ggplot2` packages
library(tidyverse)
library(caret)
library(shiny)
library(DT)
library(ggplot2)

math <- read_csv("./Final/student-mat.csv")

numericVars <- math %>% 
  select_if(is.numeric)

charVars <- math %>%
  select_if(is.character)

#Set up server
shinyServer(function(input, output, session){
  
  # Create bar plot to visualize data
  output$dataPlot<-renderPlot({
    plotType <- input$plotType
    if(plotType == "Bar Graph"){
      g <- ggplot(data = math, aes(x = get(input$barXvar)))
      g + geom_bar() +
        labs(title = "Bar Graph")
    } 

    else if(plotType == "Boxplot"){
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
  
  # Create data table for class, installment rate percentage, and mean
  output$dataTable<-renderDataTable({
    var <- input$var
    round <- input$round
    tab <- GermanCredit %>%
      select("Class", "InstallmentRatePercentage", var) %>%
      group_by(Class, InstallmentRatePercentage) %>%
      summarize(mean = round(mean(get(var)), round))
  })
})
