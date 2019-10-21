# Project name: Predicted sales Volumes
# File name:    
#               New Product Attributes (predict ds)      
###############



getwd()         # get working directory
# set working directory 
existingprod <- read.csv("~/Desktop/UBIQUM/multiple regression /existingproductattributes2017.csv")
newprod <- read.csv("~/Desktop/UBIQUM/multiple regression /newproductattributes2017.csv")

# Import Libraries
library(ggplot2)
library(caret)
library(corrplot)
library(dummy)

#Check for Missing Values
any(is.na(newprod)) 
any(is.na(existingprod))  # confirm if any "NA" values in ds
#Lets substitute the missing NA with 
sum(is.na(existingprod["BestSellersRank"]))   
sum(is.na(existingprod))   


existingprod$BestSellersRank[is.na(existingprod$BestSellersRank)] <- mean(existingprod$BestSellersRank, na.rm = TRUE)
#how do I insert mean for each product type instead of the generalised one ? 
str(existingprod$BestSellersRank)

# Removing useless attributes
existingprod$ProfitMargin <- NULL

# Setting Volume as Numeric from Integer for both new and existing products
existingprod$Volume <- as.numeric(existingprod$Volume)
str(existingprod)
newprod$Volume <- as.numeric(newprod$Volume)
str(newprod)

# correlation matrix to spot relationships
correlations <- cor(existingprod[,c(2,3,4,5,6,7,8,9,10,12,13,14,15,16,17)])
corrplot(correlations, method = "pie") 
print(correlations)

existingprod$x5StarReviews <- NULL
existingprod$x1StarReviews <- NULL
existingprod$x3StarReviews <- NULL
existingprod$NegativeServiceReview <- NULL

# correlation matrix to spot relationships for new products
correlationsnew <- cor(newprod[,c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)])
corrplot(correlationsnew, method = "pie") 
print(correlationsnew)

# dummify the data
dummy_matrix <- dummyVars("~ .", data = existingprod)

existingprod_dum <- data.frame(predict(dummy_matrix, newdata = existingprod))

ggplot(newprod, aes(x = ProductNum, fill = Volume)) + geom_histogram(colour = "black") + stat_bin(existingprod$ProductNum, 30)




# Define an 75%/25% train/test split
inTraining <- createDataPartition(existingprod, p = .75, list = FALSE)
training <- existingprod[inTraining,]
testing <- existingprod[-inTraining,]

subset(existingprod, ProductType =='PC')

# create a subset for product type to calculate better the mean 
newdata <- existingprod[existingprod$productType == 2,]
droplevels(existingprod$ProductType)
str(existingprod)