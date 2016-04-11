#Pui Fung Lam
#Titantic: Machine Learning From Disaster

#install the caret package and the randomForest package
install.packages("caret", dependencies = TRUE) #Classification And REgression Training

install.packages("pbkrtest")
install.packages("randomForest")
install.packages("fields")

#loading library
library(caret) 
library(pbkrtest)
library(randomForest)
library(fields)

setwd("~/Desktop/Titanic/")
trainSet <- read.table("train.csv", sep = ",", header = TRUE)#load the data into list format, 418 obs
testSet <- read.table("test.csv", sep = ",", header = TRUE)#891 obs

typeof(trainSet)

head(trainSet)
head(testSet)

Pclass_survive <- table(trainSet[,c("Survived", "Pclass")])

Pclass_survive[2, ]/colSums(Pclass_survive[1:2, ])
#Survival Rate from Class 1,2,3 passengers respectively
#0.6296296 0.4728261 0.2423625 
#Pclass heavily determine the survvial rate of passengers

# Comparing Age and Survived.

bplot.xy(trainSet$Survived, trainSet$Age)
#X-axis: survived, Y-axis: Age
# Age probably did not have a large effect on whether one survived or not. 


#Taking out NA variables from AGE
summary(trainSet$Age) #177 NA's

# Comparing Survival Rate and Fare
#unique(trainSet$Fare)
#factor(trainSet$Fare) 248 levels

bplot.xy(trainSet$Survived, trainSet$Fare)
summary(trainSet$Fare)


#Training a model

# Convert Survived to Factor
trainSet$Survived <- factor(trainSet$Survived)
# Set a random seed (so you will get the same results as me)
set.seed(42)
# Train the model using a "random forest" algorithm
model <- train(Survived ~ Pclass + Sex + SibSp +   
                 Embarked + Parch + Fare, # Survived is a function of the variables we decided to include
               data = trainSet, # Use the trainSet dataframe as the training data
               method = "rf",# Use the "random forest" algorithm
               trControl = trainControl(method = "cv", # Use cross-validation
                                        number = 5) # Use 5 folds for cross-validation
)

model


#Making prediction on test set
testSet$Survived <- predict(model, newdata = testSet)#1 missing NA value

testSet$Fare <- ifelse(is.na(testSet$Fare), mean(testSet$Fare, na.rm = TRUE), testSet$Fare)

testSet$Survived <- predict(model, newdata = testSet)

submission <- testSet[,c("PassengerId", "Survived")]
write.table(submission, file = "submission.csv", col.names = TRUE, row.names = FALSE, sep = ",")
