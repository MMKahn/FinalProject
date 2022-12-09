# Load the required `caret`, `shiny`, and `DT` packages
library(readr)
library(caret)
library(shiny)
library(DT)


math <- read_csv("student-mat.csv")

# Define UI for application
shinyUI(navbarPage("ST558 Final Project",
  
  # Create first tab: About page
  tabPanel("About",
           fluidRow(
             column(12, 
                    h2("Mathematics Student Information App"),
                    h3("Created by Melanie Kahn")
                    ),
             column(12,
                    h4("About the Data"),
                    p("The data used in this project includes student information for those attending math courses in secondary education at two Portuguese schools, Gabriel Pereira and Mousinho da Silveira. The data was collected by using school reports and questionnaires. Attributes for this data frame are defined as:"),
                    tags$ul(
                      tags$li(strong("school:"), "student's school ('GP' - Gabriel Pereira or 'MS' - Mousinho da Silveira)"),
                      tags$li(strong("sex:"), "student's sex ('F' - female or 'M' - male)"),
                      tags$li(strong("age"), "student's age (from 15 to 22)"),
                      tags$li(strong("address"), "student's home address type ('U' - urban or 'R' - rural)"),
                      tags$li(strong("famsize"), "family size ('LE3' - less or equal to 3 or 'GT3' - greater than 3)"),
                      tags$li(strong("Pstatus"), "parent's cohabitation status ('T' - living together or 'A' - apart)"),
                      tags$li(strong("Medu"), "mother's education (0 - none,  1 - primary education (4th grade), 2 – 5th to 9th grade, 3 - secondary education or 4 – higher education)"),
                      tags$li(strong("Fedu"), "father's education (0 - none,  1 - primary education (4th grade), 2 – 5th to 9th grade, 3 – secondary education or 4 – higher education)"),
                      tags$li(strong("Mjob"), "mother's job ('teacher', 'health' care related, civil 'services' (e.g. administrative or police), 'at_home' or 'other')"),
                      tags$li(strong("Fjob"), "father's job ('teacher', 'health' care related, civil 'services' (e.g. administrative or police), 'at_home' or 'other')"),
                      tags$li(strong("reason"), "reason to choose this school (close to 'home', school 'reputation', 'course' preference or 'other')"),
                      tags$li(strong("guardian"), "student's guardian ('mother', 'father' or 'other')"),
                      tags$li(strong("traveltime"), "home to school travel time (1 - <15 min., 2 - 15 to 30 min., 3 - 30 min. to 1 hour, or 4 - >1 hour)"),
                      tags$li(strong("studytime"), "weekly study time (1 - <2 hours, 2 - 2 to 5 hours, 3 - 5 to 10 hours, or 4 - >10 hours)"),
                      tags$li(strong("failures"), "number of past class failures (n if 1<=n<3, else 4)"),
                      tags$li(strong("schoolsup"), "extra educational support (yes or no)"),
                      tags$li(strong("famsup"), "family educational support (yes or no)"),
                      tags$li(strong("paid"), "extra paid classes within the Math course subject (yes or no)"),
                      tags$li(strong("activities"), "extra-curricular activities (yes or no)"),
                      tags$li(strong("nursery"), "attended nursery school (yes or no)"),
                      tags$li(strong("higher"), "wants to take higher education (yes or no)"),
                      tags$li(strong("internet"), "Internet access at home (yes or no)"),
                      tags$li(strong("romantic"), "with a romantic relationship (yes or no)"),
                      tags$li(strong("famrel"), "quality of family relationships (from 1 - very bad to 5 - excellent)"),
                      tags$li(strong("freetime"), "free time after school (from 1 - very low to 5 - very high)"),
                      tags$li(strong("goout"), "going out with friends (from 1 - very low to 5 - very high)"),
                      tags$li(strong("Dalc"), "workday alcohol consumption (from 1 - very low to 5 - very high)"),
                      tags$li(strong("Walc"), "weekend alcohol consumption (from 1 - very low to 5 - very high)"),
                      tags$li(strong("health"), "current health status (from 1 - very bad to 5 - very good)"),
                      tags$li(strong("absences"), "number of school absences (from 0 to 93)"),
                      tags$li(strong("G1"), "first period grade (from 0 to 20)"),
                      tags$li(strong("G2"), "second period grade (from 0 to 20)"),
                      tags$li(strong("G3"), "final grade (from 0 to 20, output target)"),
                    ),
                    p("More information on this dataset can be found on the ", a(href = "https://www.kaggle.com/datasets/uciml/student-alcohol-consumption?resource=download", target="_blank", "Kaggle"),"website.")
             ),
             column(12,
                    h4("About the App"),
                    p("The goal of this project is to create a nice looking shiny app that can be used to explore data and model it")
             )
           )
  ),
  
  # Create second tab: Data Exploration page
  tabPanel("Data Exploration",
           # Title of page
           titlePanel("Exploratory Data Analysis (EDA)"),
           
           # Create Sidebar
           sidebarLayout(
             # Customize Sidebar
             sidebarPanel(
               # Fourth level header
               h4("Create your custom graphical summaries below:"),
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
  ),
  # Create third tab: Modeling page
  tabPanel("Modeling",
           
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
  ),
  
  # Create fourth tab: Data page
  tabPanel("Data",
           
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
  ),
  
))
