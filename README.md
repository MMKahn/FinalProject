# ST 558 Final Project

## Purpose of App

The purpose of this application is to conduct exploratory analysis and create predictive models for a mathematics student information data set to find which one performs the best. After splitting the data into a training and test set, the performance of a generalized linear regression model (GLM), a classification tree model, and a random forest model will be compared based on the root-mean-square error (RMSE) calculation. The best model will have the smallest RMSE from the test set. This process can be customized by the user of the app.

## Packages Used

The R packages required for this project:

* `readr`  
* `caret`  
* `shiny`  
* `DT`  
* `tidyverse`  
* `dplyr`  
* `ggplot2`  

You can download and install these packages by running the code below:

``` r
install.packages(c("readr", "caret", "shiny", "DT", "tidyverse", "dplyr", "ggplot2"))
```

## Run the App

You can open and use the application by copying the following code and pasting it into your personal RStudio.

``` r
shiny::runGitHub("MMKahn/FinalProject", ref = "main", subdir = "Final")
```