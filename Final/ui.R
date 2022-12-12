# Load the required packages
library(readr)
library(caret)
library(shiny)
library(DT)

# Define UI for application
shinyUI(navbarPage("ST558 Final Project",
  
  # Create first tab: About page
  tabPanel("About",
           fluidRow(
             column(12, 
                    h2("Mathematics Student Information App"),
                    h3("Created by Melanie Kahn"),
                    img(src='https://wp-media.petersons.com/blog/wp-content/uploads/2019/01/10123556/iStock-944038668.jpg', align = "center", width = "500px")
                    ),
             column(12,
                    h4("About the Data"),
                    p("The data used in this project includes student information for those attending math courses in secondary education at two Portuguese schools, Gabriel Pereira and Mousinho da Silveira. The data was collected by using school reports and questionnaires. Attributes for this data frame are defined as:"),
                    tags$ul(
                      tags$li(strong("school:"), "student's school ('GP' - Gabriel Pereira or 'MS' - Mousinho da Silveira)"),
                      tags$li(strong("sex:"), "student's sex ('F' - female or 'M' - male)"),
                      tags$li(strong("age:"), "student's age (from 15 to 22)"),
                      tags$li(strong("address:"), "student's home address type ('U' - urban or 'R' - rural)"),
                      tags$li(strong("famsize:"), "family size ('LE3' - less or equal to 3 or 'GT3' - greater than 3)"),
                      tags$li(strong("Pstatus:"), "parent's cohabitation status ('T' - living together or 'A' - apart)"),
                      tags$li(strong("Medu:"), "mother's education (0 - none,  1 - primary education (4th grade), 2 – 5th to 9th grade, 3 - secondary education or 4 – higher education)"),
                      tags$li(strong("Fedu:"), "father's education (0 - none,  1 - primary education (4th grade), 2 – 5th to 9th grade, 3 – secondary education or 4 – higher education)"),
                      tags$li(strong("Mjob:"), "mother's job ('teacher', 'health' care related, civil 'services' (e.g. administrative or police), 'at_home' or 'other')"),
                      tags$li(strong("Fjob:"), "father's job ('teacher', 'health' care related, civil 'services' (e.g. administrative or police), 'at_home' or 'other')"),
                      tags$li(strong("reason:"), "reason to choose this school (close to 'home', school 'reputation', 'course' preference or 'other')"),
                      tags$li(strong("guardian:"), "student's guardian ('mother', 'father' or 'other')"),
                      tags$li(strong("traveltime:"), "home to school travel time (1 - <15 min., 2 - 15 to 30 min., 3 - 30 min. to 1 hour, or 4 - >1 hour)"),
                      tags$li(strong("studytime:"), "weekly study time (1 - <2 hours, 2 - 2 to 5 hours, 3 - 5 to 10 hours, or 4 - >10 hours)"),
                      tags$li(strong("failures:"), "number of past class failures (n if 1<=n<3, else 4)"),
                      tags$li(strong("schoolsup:"), "extra educational support (yes or no)"),
                      tags$li(strong("famsup:"), "family educational support (yes or no)"),
                      tags$li(strong("paid:"), "extra paid classes within the Math course subject (yes or no)"),
                      tags$li(strong("activities:"), "extra-curricular activities (yes or no)"),
                      tags$li(strong("nursery:"), "attended nursery school (yes or no)"),
                      tags$li(strong("higher:"), "wants to take higher education (yes or no)"),
                      tags$li(strong("internet:"), "Internet access at home (yes or no)"),
                      tags$li(strong("romantic:"), "with a romantic relationship (yes or no)"),
                      tags$li(strong("famrel:"), "quality of family relationships (from 1 - very bad to 5 - excellent)"),
                      tags$li(strong("freetime:"), "free time after school (from 1 - very low to 5 - very high)"),
                      tags$li(strong("goout:"), "going out with friends (from 1 - very low to 5 - very high)"),
                      tags$li(strong("Dalc:"), "workday alcohol consumption (from 1 - very low to 5 - very high)"),
                      tags$li(strong("Walc:"), "weekend alcohol consumption (from 1 - very low to 5 - very high)"),
                      tags$li(strong("health:"), "current health status (from 1 - very bad to 5 - very good)"),
                      tags$li(strong("absences:"), "number of school absences (from 0 to 93)"),
                      tags$li(strong("G1:"), "first period grade (from 0 to 20)"),
                      tags$li(strong("G2:"), "second period grade (from 0 to 20)"),
                      tags$li(strong("G3:"), "final grade (from 0 to 20, output target)"),
                    ),
                    p("More information on this ", a(href = "https://www.kaggle.com/datasets/uciml/student-alcohol-consumption?resource=download", target="_blank", "dataset"), " can be found on the ", img(src='data:image/png;base64,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', align = "center", width = "100px"), " website.")
             ),
             column(12,
                    h4("About the App"),
                    p("The purpose of the following analysis is to conduct exploratory analysis and create predictive models for this math student data set to find which one performs the best."),
                    p("After splitting the data into a training and test set, the performance of a generalized linear regression model (GLM), a classification tree model, and a random forest model will be compared based on the root-mean-square error (RMSE) calculation."),
                    p("The best model will have the smallest RMSE from the test set."),
                    p("This process can be customized by the user of the app.")
             )
           )
  ),
  
  # Create second tab: Data Exploration page
  tabPanel("Data Exploration",
           # Title of page
           titlePanel("Exploratory Data Analysis (EDA)"),
           
           # Create Sidebar with options for the data set
           sidebarLayout(
             # Customize Sidebar
             sidebarPanel(
               
               # Graphical summaries
               h4("Select options to create graphical summaries:"),
               # Radio button widget in sidebar
               radioButtons("plotType", label = "Select the Plot Type", choices = c("Bar Graph" = "barGraph", "Boxplot" = "boxPlot", "Scatter Plot" = "scatterPlot"), selected = "barGraph"),
               # Select options based on radio button selection
               conditionalPanel(condition = "input.plotType == 'barGraph'",
                                selectInput("barXvar", "Variable on X-axis:", choices = names(charVars), selected = "school")
               ),
               conditionalPanel(condition = "input.plotType == 'boxPlot'",
                                selectInput("boxXvar", "Variable on X-axis:", choices = names(charVars), selected = "school"),
                                selectInput("boxYvar", "Variable on Y-axis:", choices = names(numericVars), selected = "age")
               ),
               conditionalPanel(condition = "input.plotType == 'scatterPlot'",
                                selectInput("scatterXvar", "Variable on X-axis:", choices = names(numericVars), selected = "age"),
                                selectInput("scatterYvar", "Variable on Y-axis:", choices = names(numericVars), selected = "absences")
               ),
               
               br(),
               
               # Numerical summaries
               h4("Select options to create numerical summaries:"),
               selectInput("numVar", label = "Select a variable to summarize:", choices = names(numericVars), selected = "age"),
               p("Would you like to group your results based on a categorical variable?"),
               checkboxInput("groupby", label = "Yes", value = FALSE),
               conditionalPanel(condition = "input.groupby",
                                selectInput("groupVar", label = "Select a variable(s) to group by:", choices = c("school" = "school", "sex" = "sex", "both school and sex" = "both"), selected = "school")
               ),
               p("Would you like to create a contingency table?"),
               checkboxInput("ct", label = "Yes", value = FALSE),
               conditionalPanel(condition = "input.ct",
                                selectInput("typeCT", label = "Type of Contingency Table:", choices = c("One-Way" = "ow", "Two-Way" = "tw"), selected = 'ow'),
                                conditionalPanel(condition = "input.typeCT == 'ow'",
                                                 selectInput("oneWay", label = "Select a variable:", choices = c("school", "sex", "age", "address"), selected = "school")
                                ),
                                conditionalPanel(condition = "input.typeCT == 'tw'",
                                                 selectInput("twoWayXvar", "First Variable:", choices = c("school", "sex"), selected = "sex"),
                                                 selectInput("twoWayYvar", "Second Variable:", choices = c("age", "address"), selected = "address")
                                )
               ),
             ),
             
             # Customize Main panel
             mainPanel(
               #dataPlot is name of "plot" object in server
               h4("Graphs of Data", style = "color:blue;"),
               plotOutput("dataPlot"),
               h4("Numerical Summaries of Data", style = "color:blue;"),
               dataTableOutput("summary"),
               h4("Frequency of Data", style = "color:blue;"),
               dataTableOutput("continTable"),
             )
           )
  ),
  # Create third tab: Modeling page
  tabPanel("Modeling",
           # Title of page
           titlePanel("Modeling"),
           
           # Tabs within page
           tabsetPanel(
             tabPanel("Modeling Info",
                      fluidRow(
                        column(12, 
                               h3("This section uses three different approaches to conduct model fitting on our data set."),
                               h4("You will select options for running these models on the following page entitled", strong("Model Fitting"), "."),
                               h5("Note that the response variable,", strong("higher"), " - indicating if a math student plans to take higher education - is fixed and cannot be changed by the user."),
                               p("This is done due to the binomial nature of the GLM model used in this project.")
                        ),
                        column(4, 
                               h4("Generalized Linear Regression Model"),
                               p("Since all variables in this data set are constrained, we can use a GLM to fit our training set because the range of the response variable is guaranteed to be fixed."),
                               p("For this model, a binary response variable is required, which is why the higher education variable has been selected"),
                               br(),
                               p("The specific GLM model used for this app is a logistic regression model known as backward stepwise regression."),
                               p("When it comes to traditional variable selection techniques with statistical stopping rules, backward selection is preferred over forward, especially when dealing with collinearity, because it starts with the assumedly unbaised global model."),
                               p("The problem with stepwise selection is that it produces narrow confidence limits and does not play nice in the presence of redundant predictors."),
                               br()
                               p("To train this model, the method argument was specified as 'glmStepAIC', family as 'binomial', direction to 'backward' and metric to 'Accuracy'. The data was pre-processed by centering and scaling. Lastly, trainControl() was used within the trControl argument to do 10 fold cross-validation using the 'cv' method.")
                        ),
                        column(4, 
                               h4("Classification Tree Model"),
                               p("Classification trees split your predictor spaces into regions in order to develop different predictions based off of those regions.  They typically use the most prevalent class as the prediction."),
                               p("They differ from regression trees by predicting a group membership, as opposed to predicting the value of some continuous variable."),
                               p("Instead of using the residual sum of squares for splits, classification trees try to minimize the Gini index or entropy/deviance across the splits:"),
                               uiOutput('gini'),
                               uiOutput('deviance'),
                               p("Classification trees
                                 are nice as they are easy to understand and interpret, there is no need to scale predictors or include interaction terms, and the algorithm itself essentially conducts variable selection."),
                               p("Unfortunately, they also tend to have high variance and there's no proven optimal algorithm designed for fitting these trees as they’re greedy and do not look steps ahead."),
                               p("Trees also require the extra step of pruning as not to overfit to the training data. While this decreases the variance,it increases the bias a little bit, but hopefully gains overall prediction strength."),
                               p("Prediction improvements may be made by using our next model, random forest."),
                               br(),
                               p("To train this model,  the method argument was specified as 'rpart'. The data was preprocessed by centering and scaling. Lastly, trainControl() was used within the trControl argument to do repeated 5 fold cross-validation 3 times using the 'repeatedcv' method.")
                        ),
                        column(4, 
                               h4("Random Forest Model"),
                               p("Generally speaking, random forests models predictions usually outperform regression models and tree models."),
                               p("They are an extension of bagging, but instead of including every predictor in each one of our trees, we're only going to include a random subset of predictors."),
                               p("This means that instead of calculating a statistic, we're going to get a bootstrap sample with random predictors to fit one tree; another bootstrap sample with random predictors to fit another tree; and do that repeatedly."),
                               p("After the trees are created from the bootstrap samples, we disaggregate the trees, and take the average."),
                               p("Using an average from a plethora of models tends to predict better than a single model does. "),
                               p("By averaging the predictions, random forests decrease the variance our prediction, however they do lose interpretability."),
                               p("Random forest models also take a lot of processing power and computation time, slowing down results."),
                               br(),
                               p("To train this model,  the method argument was specified as 'rf'. The data was preprocessed by centering and scaling. The argument tuneGrid was then used to replicate the random forest model a total of five (5) times. Lastly, trainControl() was used within the trControl argument to do repeated 5 fold cross-validation 3 times using the 'repeatedcv' method.")
                        )
                      )
             ),
             tabPanel("Model Fitting",
                      # Create Sidebar
                      sidebarLayout(
                        # Customize Sidebar
                        sidebarPanel(
                          # Split to data into train and test
                          h4("Splitting into training and test sets:"),
                          # Numeric input widget in sidebar
                          numericInput("train", label="Select a number 0-1 to indicate the proportion of total observations you'd like in the training set:", value = .7, min = 0, max = 1, step = .1),
                          # mathTrain and mathTest are names of "text" objects in server
                          textOutput("mathTrain"),
                          textOutput("mathTest"),
                          
                          # Line break
                          br(),
                          
                          # GLM model
                          h4("Variable selection for GLM:"),
                          # Checkboxes in sidebar
                          selectInput("predictors1", label = "Select predictor variables:", choices = names(math), selected = c("school", "address", "famsize", "Pstatus", "schoolsup", "famsup"), multiple = TRUE),
                          
                          # Classification tree model
                          h4("Variable selection for classification tree:"),
                          # Checkboxes in sidebar
                          selectInput("predictors2", label = "Select predictor variables:", choices = names(math), selected = c("school", "address", "famsize", "Pstatus", "schoolsup", "famsup"), multiple = TRUE),
                          
                          # Random forest model
                          h4("Variable selection for random forest:"),
                          # Checkboxes in sidebar
                          selectInput("predictors3", label = "Select predictor variables:", choices = names(math), selected = c("school", "address", "famsize", "Pstatus", "schoolsup", "famsup"), multiple = TRUE),
                          
                          # Run all models at the same time
                          h4("Click the button when ready to run all three models"),
                          # Action button to fit models
                          actionButton("button", "Fit Models")
                        ),
                        
                        #Customize Main panel
                        mainPanel(
                          verbatimTextOutput("glmModel"),
                          verbatimTextOutput("ctModel"),
                          verbatimTextOutput("rfModel")
                        )
                      )
             ),
             tabPanel("Prediction",
             )
           )
  ),
  # Create fourth tab: Data page
  tabPanel("Data",
           # Title of page
           titlePanel("Subset and Save the Data"),
           
           # Create Sidebar
           sidebarLayout(
             # Customize Sidebar
             sidebarPanel(
               # Working with data set
               h4("Subset the data set:"),
               checkboxGroupInput("subset", "Select variables:", choices = names(math), selected = names(math)),
               selectInput("skewl", "Select the school", choices = c("Gabriel Pereira" = "gp", "Mousinho da Silveira" = "mds", "Both" = "both"), selected = "both"),
               br(),
               h4("Save the data set:"),
               p("The resulting file will be in .csv format."),
               downloadButton("save", "Download")
             ),
             
             #Customize Main panel
             mainPanel(
               dataTableOutput("tbl")
             )
           )
  )
))
