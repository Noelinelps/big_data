required_packages <- c(
  "checkpoint"
)

new.packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(new.packages)) {
  install.packages(new.packages)
}

rm(new.packages)

library(ggplot2)
library(cowplot)
library(caret)
library(glmnet)
library(pROC)
library(UBL)
library(randomForest)
library(adabag)
