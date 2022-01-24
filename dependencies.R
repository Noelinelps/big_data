required_packages <- c(
  "checkpoint"
)

new.packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(new.packages)) {
  install.packages(new.packages)
}

rm(new.packages)

library(devtools)
require (devtools) 
install_version("shinydashboardPlus", version="0.7.5",repos = "http://cran.us.r-project.org")
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)
library(mathjaxr)
library(shinycssloaders)
library(ggplot2)
library(cowplot)
library(caret)
library(glmnet)
library(pROC)
library(UBL)
library(randomForest)
library(adabag)
