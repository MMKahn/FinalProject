# Load the required `caret`, `shiny`, and `DT` packages
library(readr)
library(caret)
library(shiny)
library(DT)

# This project will use data from Project 1 on public school enrollment in the United States from the 1970's to the early 2000's. 
# Acquired from the U.S. Census Bureau, the data includes enrollment data on national, state, and county level.
# The data set read in below is one section of public school enrollment data
sheet1 <- read_csv("https://www4.stat.ncsu.edu/~online/datasets/EDU01a.csv")

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Summaries for German Credit Data"),
  
  # Create Sidebar
  sidebarLayout(
    # Customize Sidebar
    sidebarPanel(
      # Third level header
      h3("This data set comes from the", a(href = "https://topepo.github.io/caret/", target="_blank", "caret package"),"- originally from the UCI machine learning repository"),
      
      # Line break
      br(),
      
      # Fourth level header
      h4("You can create a few bar plots using the radio buttons below."),
      
      # Radio button widget in sidebar
      radioButtons("plotType", label="Select the Plot Type", choices = c("Just Classification", "Classification and Unemployed", "Classification and Foreign"), selected = "Just Classification"),
      
      # Line break
      br(),
      
      # Fourth level header
      h4("You can find the", strong("sample mean"), "for a few variables below:"), # Third level header
      
      # Select box widget in sidebar
      selectInput("var", label="Select the Plot Type", choices = c("Duration", "Amount", "Age"), selected = "Age"),
      
      # Numeric input widget in sidebar
      numericInput("round", label="Select the number of digits for rounding", value = 2, min = 0, max = 5, step = 1),
    ),
    
    # Customize Main panel
    mainPanel(
      #dataPlot is name of "plot" object in server
      plotOutput("dataPlot"),
      
      #dataTable is name of "data" object in server
      dataTableOutput("dataTable")
      
    )
  )
))
