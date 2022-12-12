# Load the required packages
library(tidyverse)
library(dplyr)
library(caret)
library(shiny)
library(DT)
library(ggplot2)
library(corrplot)

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
        geom_jitter(aes(color = get(boxXvar))) +
        labs(title = "Boxplot")
    } 
    
    else{
      g3 <- ggplot(data = math, aes(x = get(scatterXvar), y = get(scatterYvar)))
      g3 + geom_jitter() +
        geom_smooth(method = lm, col = "Blue") +
        labs(title = "Scatter Plot")
    }
  })
  
  #Numeric Summaries
  cntgTbl <- reactive({
    ct <- input$ct
    typeCT <- input$typeCT
    oneWay <- input$oneWay
    twoWayXvar <- input$twoWayXvar
    twoWayYvar <- input$twoWayYvar
    if (typeCT == 'ow'){
      data.frame(table(get(oneWay)))
    }
    else {
      data.frame(table(get(twoWayXvar), get(twoWayYvar)))
    }
  })
  
  output$continTable<-renderDataTable({
    cntgTbl()
  })

  # Numerical summaries working
  numSum <- reactive({
    numVar <- input$numVar
    groupby <- input$groupby
    groupVar <- input$groupVar
    if (groupby){
      if (groupVar == 'school'){
        summaries <- math %>%
          select("school", numVar) %>%
          group_by(school) %>%
          summarize(sd = round(sd(get(numVar)), 2),
                    minimum = round(min(get(numVar)), 2), 
                    Q1 = round(quantile(get(numVar), prob = 0.25), 2),
                    median = round(median(get(numVar)), 2),
                    mean = round(mean(get(numVar)), 2),
                    Q3 = round(quantile(get(numVar), prob = 0.75), 2),
                    maximum = round(max(get(numVar)), 2))
      }
      else if (groupVar == 'sex'){
        summaries <- math %>%
          select("sex", numVar) %>%
          group_by(sex) %>%
          summarize(sd = round(sd(get(numVar)), 2),
                    minimum = round(min(get(numVar)), 2), 
                    Q1 = round(quantile(get(numVar), prob = 0.25), 2),
                    median = round(median(get(numVar)), 2),
                    mean = round(mean(get(numVar)), 2),
                    Q3 = round(quantile(get(numVar), prob = 0.75), 2),
                    maximum = round(max(get(numVar)), 2))
      }
      else {
        summaries <- math %>%
          select("school", "sex", numVar) %>%
          group_by(school, sex) %>%
          summarize(sd = round(sd(get(numVar)), 2), 
                    minimum = round(min(get(numVar)), 2), 
                    Q1 = round(quantile(get(numVar), prob = 0.25), 2),
                    median = round(median(get(numVar)), 2),
                    mean = round(mean(get(numVar)), 2),
                    Q3 = round(quantile(get(numVar), prob = 0.75), 2),
                    maximum = round(max(get(numVar)), 2))
      }
    }
    else {
      summaries <- math %>%
        select(all_of(numVar)) %>%
        summarize(sd = round(sd(get(numVar)), 2), 
                  minimum = round(min(get(numVar)), 2), 
                  Q1 = round(quantile(get(numVar), prob = 0.25), 2),
                  median = round(median(get(numVar)), 2),
                  mean = round(mean(get(numVar)), 2),
                  Q3 = round(quantile(get(numVar), prob = 0.75), 2),
                  maximum = round(max(get(numVar)), 2)
        )
    }
  })
  
  
  output$summary <- renderDataTable({
    numSum()
  })
  
  # Create text when splitting data into training and test sets
  output$mathTrain<-renderText({
    paste("The training subset has", input$train*100, "% of the observations from the data set.", sep = " ")
  })
  output$mathTest<-renderText({
    paste("The test subset has the remaining", (1-input$train)*100, "%.", sep = " ")
  })
  
  # Data split
  set.seed(100)
  mathIndex  <- reactive({
    createDataPartition(math$G3, p = input$train, list = FALSE)
  })
  # Training set
  mathTrn <- reactive({
    math[mathIndex, ]
  })
  # Test set
  mathTst <- reactive({
    math[-mathIndex, ]
  })
  
  # GLM Model fitting
  # Training
  glm <- reactive({
    predictors1 <- input$predictors1
    genLinear <- train(math$G3 ~ get(predictors1), 
                       data = mathTrn,
                       method = "glmStepAIC",
                       preProcess = c("center", "scale"),
                       trControl = trainControl(method = "cv", number = 10),
                       family = "binomial",
                       direction = "backward")
    genLinear
  })
  # Output
  output$glmModel <- renderPrint({
    glm()
  })
  
  # Math  type in classification tree explanation
  output$gini <- renderUI({
    withMathJax(helpText('$$Gini:2p(1-p)$$'))
  })
 
  output$deviance <- renderUI({
    withMathJax(helpText('$$Deviance:−2plog(p)−2(1−p)log(1−p)$$'))
  })
  
  # Classification Tree Model fitting
  # Training
  classTreeModel <- reactive({
    predictors2 <- input$predictors2
    classTree <- train(math$G3 ~ get(predictors2), 
                       data = mathTrn,
                       method = "rpart",
                       preProcess = c("center", "scale"),
                       trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3))
    classTree
  }) 
  # Output
  output$ctModel <- renderPrint({
    classTreeModel()
  })
  
  # Random Forest Model fitting
  # Training
  rf <- reactive({
    predictors3 <- input$predictors3
    randForest <- train(math$G3 ~ get(predictors3), 
                        data = mathTrn,
                        method = "rf",
                        preProcess = c("center", "scale"),
                        trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
                        tuneGrid = data.frame(mtry = 1:5))
    randForest
  })
  # Output
  output$rfModel <- renderPrint({
    rf()
  })
  
  # Subsetting data
  subsetVars <- reactive({
    colSubset <- input$subset
    rowSubset <- input$skewl
    school <- math$school
    if (rowSubset == "gp"){
      math[school == "GP", colSubset]
    }
    else if (rowSubset == "mds"){
      math[school == "MS", colSubset]
    }
    else {
      math[,colSubset]
    }
  })
  
  # Scroll through the data set
  output$tbl <- renderDataTable({
    subsetVars()
  })
  
  #Save the data set
  output$save <- downloadHandler(
    filename = "mathStudents.csv",
    content = function(file) {
      write.csv(subsetVars(), file, row.names = FALSE)
    }
  )
})