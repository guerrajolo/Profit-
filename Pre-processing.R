#Loading required packages
pacman::p_load(caret, tidyverse, readr)

#Importing data
existingProducts <- read.csv("Data/existingproductattributes2017.csv")
