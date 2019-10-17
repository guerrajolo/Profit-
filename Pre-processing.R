#Loading required packages
pacman::p_load(caret, tidyverse, readr)

#Importing data
existingProducts <- read.csv("Data/existingproductattributes2017.csv")


#I DID THIS MODIFICATION
# and i did this one

# Project name: Predicted sales Volumes
# File name:    Existing Product Attributes (75% - train and 25% -test ds) 
#               New Product Attributes (predict ds)      
###############


rm(list = ls()) # Clear objects if necessary
getwd()         # get working directory
# set working directory 
existingprod <- read.csv("~/Desktop/UBIQUM/multiple regression /existingproductattributes2017.csv")
newprod <- read.csv("~/Desktop/UBIQUM/multiple regression /newproductattributes2017.csv")

library(ggplot2)
library(caret)
library(corrplot)
library(dummy)

any(is.na(existingprod))  # confirm if any "NA" values in ds
any(is.na(newprod)) 


str(existingprod)
attributes(newprod)
setwd


# dummify the data

newDataFrame <- dummy(existingprod, p = "existingprod$producttype", object = NULL, int = FALSE, verbose = FALSE)

readyData <- data.frame(predict(newDataFrame, newdata = existingprod))


# correlation matrix to spot relationships
corrplot(existingprod)

corrData <- cor(readyData)

corrData
# another change