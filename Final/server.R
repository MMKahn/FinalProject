# Load the required packages
library(tidyverse)
library(dplyr)
library(caret)
library(shiny)
library(DT)
library(ggplot2)

# Read in and manipulate data
math <- read_csv("student-mat.csv")
# Numeric variables
numericVars <- math %>% 
  select_if(is.numeric)
# Character variables
charVars <- math %>%
  select_if(is.character)

#Set up server
shinyServer(function(input, output, session){
  
  # Create plots to visualize EDA data
  output$dataPlot<-renderPlot({
    plotType <- input$plotType
    barXvar <- input$barXvar
    boxXvar <- input$boxXvar
    boxYvar <- input$boxYvar
    scatterXvar <- input$scatterXvar
    scatterYvar <- input$scatterYvar
    if(plotType == "barGraph"){
      g <- ggplot(data = math, aes(x = get(barXvar)))
      g + geom_bar() +
        labs(title = "Bar Graph")
    } 

    else if(plotType == "boxPlot"){
      g2 <- ggplot(data = math, aes(x = get(boxXvar), y = get(boxYvar)))
      g2 + geom_boxplot() +
        geom_jitter(aes(color = get(barXvar))) +
        labs(title = "Boxplot")
    } 
    
    else{
      g3 <- ggplot(data = math, aes(x = get(scatterXvar), y = get(scatterYvar)))
      g3 + geom_jitter() +
        geom_smooth(method = lm, col = "Blue") +
        labs(title = "Scatter Plot")
    }
  })
  
  # Create text when splitting data into training and test sets
  output$mathTrain<-renderText({
    paste("The training subset has", input$train*100, "% of the observations from the data set.", sep = " ")
  })
  output$mathTest<-renderText({
    paste("The test subset has the remaining", (1-input$train)*100, "%.", sep = " ")
  })
  
  # Model fitting
  fitting <- eventReactive(input$button, {
    set.seed(100)
    mathIndex <- createDataPartition(input$target, p = input$train, list = FALSE)
    mathTrn <- math[mathIndex, ]
    mathTst <- math[-mathIndex, ]
    linear <- train(input$target ~ input$predictors, 
                 data = newsTrain,
                 method = "lm",
                 preProcess = c("center", "scale"),
                 trControl = trainControl(method = "cv"))
  })
  output$summaries <- renderTable({
    summary(fitting())
  })
})