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
# New data set for GLM model
glmMath <- math %>%
  mutate(glmSex = ifelse(sex == "F", 0, 1),
         glmAddress = ifelse(address == "U", 0, 1),
         glmFamSize = ifelse(famsize == "LE3", 0, 1),
         glmPstatus = ifelse(Pstatus == "A", 0, 1),
         glmSchoolSup = ifelse(schoolsup == "no", 0, 1),
         glmFamSup = ifelse(famsup == "no", 0, 1),
         glmPaid = ifelse(paid == "no", 0, 1),
         glmActivities = ifelse(activities == "no", 0, 1),
         glmNursery = ifelse(nursery == "no", 0, 1),
         glmHigher = ifelse(higher == "no", 0, 1),
         glmInternet = ifelse(internet == "no", 0, 1),
         glmRomantic = ifelse(romantic == "no", 0, 1)) %>%
  select(starts_with("glm")) %>%
  rename(sex = glmSex,
         address = glmAddress,
         famsize = glmFamSize,
         Pstatus = glmPstatus,
         schoolsup = glmSchoolSup,
         famsup = glmFamSup,
         paid = glmPaid,
         activities = glmActivities,
         nursery = glmNursery,
         higher = glmHigher,
         internet = glmInternet,
         romantic = glmRomantic)


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
  output$dataTable<-renderDataTable({
    numericType <- input$numericType
    avg <- input$avg
    stdev <- input$stdev
    cntgTbl <- input$cntgTbl
    oneWay <- input$oneWay
    twoWayXvar <- input$twoWayXvar
    twoWayYvar <- input$twoWayYvar
    if (numericType == 'ct') {
      if (cntgTbl == 'ow'){
        data.frame(table(oneWay))
      }
      else {
        data.frame(table(twoWayXvar, twoWayYvar))
      }
    }
    
    else if (numericType == 'mean') {
      data.frame(mean(avg))
    }
    else {
      data.frame(sd(stdev))
    }
    
  })
  
  # Create text when splitting data into training and test sets
  output$mathTrain<-renderText({
    paste("The training subset has", input$train*100, "% of the observations from the data set.", sep = " ")
  })
  output$mathTest<-renderText({
    paste("The test subset has the remaining", (1-input$train)*100, "%.", sep = " ")
  })
  
  # GLM Model fitting
  # Training
  glm <- reactive({
    target1 <- input$target1
    train <- input$train
    predictors1 <- input$predictors1
    set.seed(100)
    mathIndex <- createDataPartition(get(target1), p = train, list = FALSE)
    mathTrn <- math[mathIndex, ]
    mathTst <- math[-mathIndex, ]
    genLinear <- train(get(target1) ~ get(predictors1), 
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
    target2 <- input$target2
    train <- input$train
    predictors2 <- input$predictors2
    set.seed(100)
    mathIndex <- createDataPartition(get(target2), p = train, list = FALSE)
    mathTrn <- math[mathIndex, ]
    mathTst <- math[-mathIndex, ]
    classTree <- train(get(target2) ~ get(predictors2), 
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
    target3 <- input$target3
    train <- input$train
    predictors3 <- input$predictors3
    set.seed(100)
    mathIndex <- createDataPartition(get(target3), p = train, list = FALSE)
    mathTrn <- math[mathIndex, ]
    mathTst <- math[-mathIndex, ]
    randForest <- train(get(target3) ~ get(predictors3), 
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
})