# Load the required `tidyverse`, `caret`, `shiny`, `DT` and `ggplot2` packages
library(tidyverse)
library(caret)
library(shiny)
library(DT)
library(ggplot2)

# This project will use data from Project 1 on public school enrollment in the United States from the 1970's to the early 2000's. 
# Acquired from the U.S. Census Bureau, the data includes enrollment data on national, state, and county level.
# The data set read in below is one section of public school enrollment data
sheet1 <- read_csv("https://www4.stat.ncsu.edu/~online/datasets/EDU01a.csv")

#Set up server
shinyServer(function(input, output){
  
  # Create bar plot to visualize the relationship between good and bad classes
  output$dataPlot<-renderPlot({
    plotType <- input$plotType
    # When "Just Classification" is chosen on the radio button widget in sidebar, produce this bar plot
    if(plotType == "Just Classification"){
      g <- ggplot(data = GermanCredit, aes(x = Class))
      g + geom_bar()
    } 
    # When "Classification and Unemployed" is chosen on the radio button widget in sidebar, produce this side-by-side bar plot
    else if(plotType == "Classification and Unemployed"){
      employCred <- GermanCredit %>% 
        mutate(unemployment = if_else(EmploymentDuration.lt.1 == 1 | EmploymentDuration.1.to.4 == 1 | EmploymentDuration.4.to.7 == 1 | EmploymentDuration.gt.7 == 1, "Employed", "Unemployed"))
      g2 <- ggplot(data = employCred, aes(x = Class, fill = unemployment))
      g2 + geom_bar(stat = "count", position = "dodge") +
        scale_fill_discrete(name = "Umemployment status")
    } # When "Classification and Foreign" is chosen on the radio button widget in sidebar, produce this side-by-side bar plot
    else{
      nationCred <- GermanCredit %>% 
        mutate(foreignStatus = if_else(ForeignWorker == 0, "No", "Yes"))
      g3 <- ggplot(data = nationCred, aes(x = Class, fill = foreignStatus))
      g3 + geom_bar(stat = "count", position = "dodge") +
        scale_fill_discrete(name = "Status", labels = c("German", "Foreign"))
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
