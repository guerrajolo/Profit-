# Project name: Predicted sales Volumes
# File name:    
#               New Product Attributes (predict ds)      
###############


rm(list = ls()) 
getwd()         # 
# set working directory 
setwd("/Users/gherardolattanzi/MultipleReg")
existingprod <- read.csv("Data/existingproductattributes2017.csv")
newprod <- read.csv("Data/newproductattributes2017.csv")

# Import Libraries
library(ggplot2)
library(caret)
library(corrplot)
library(dummy)
library(rstudioapi)
library(caretEnsemble)

# set working directory 
current_path
existingprod <- read.csv("~/Desktop/UBIQUM/multiple regression /existingproductattributes2017.csv")
newprod <- read.csv("~/Desktop/UBIQUM/multiple regression /newproductattributes2017.csv")

current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))

#Check for Missing Values
any(is.na(newprod)) 
any(is.na(existingprod))  # confirm if any "NA" values in ds
#Lets substitute the missing NA with 
sum(is.na(existingprod["BestSellersRank"]))   
sum(is.na(existingprod))   

existingprod$BestSellersRank[is.na(existingprod$BestSellersRank)] <- mean(existingprod$BestSellersRank, na.rm = TRUE)
#how do I insert mean for each product type instead of the generalised one ? 

# create a subset for product type to calculate better the mean 
#subset(existingprod, ProductType =='Laptops',)
#droplevels(existingprod$ProductType)
#attributes(existingprod$ProductType)
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
exidum <- data.frame(predict(dummy_matrix, newdata = existingprod))


set.seed(98)

# Define an 75%/25% train/test split
inTraining <- createDataPartition(existingprod$ProductType, p = .75, list = FALSE)
training <- existingprod[inTraining,]
testing <- existingprod[-inTraining,]

# create submodels
control <- trainControl(method="repeatedcv", number=10, repeats=2, savePredictions=TRUE, classProbs=TRUE)
algorithmList <- c('glm', 'knn', 'svmRadial')

models <- caretList(Volume~., data=training, trControl=control, methodList=algorithmList)
print(models)
results <- resamples(models)
summary(results)
dotplot(results)

modelCor(results)
splom(results)

#Let’s combine the predictions of the classifiers using a RF
set.seed(123)
stackControl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
stack.rf <- caretStack(models, method="rf", metric="", trControl=stackControl)
print(stack.rf)

# Apply models to testset
PRED1 <- predict(stack.rf, testing)
PRED1 <- as.data.frame(PRED1)
str(PRED1)

testing$glm <- PRED1$glm
testing$knn <- PRED1$knn
testing$svmRadial <- PRED1$svmRadial
testing$PredictionAVG <- (testing$glm + testing$knn + testing$svmRadial)/3
#for high vol prod use the glm, for mid size prod use the knn. 
ggplot(testing, aes(x = testing$x4StarReviews) + geom_point(y = testing$knn)


BOXPLOT ORIZZONTALE PER CONFIDENCE INTERVALS FOR THE 3 MD
