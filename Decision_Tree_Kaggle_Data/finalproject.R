library("ggplot2")
library("dplyr")
library("randomForest")
library("rattle")
library("caret")
library("rpart.plot")

# Describe the data
hr <- read.csv("HR_comma_sep.csv")
hr$left <- NULL
hr$type <- hr$sales
hr$sales <- NULL

View(head(hr))

#### Unaltered dataset
### Decision Tree
# Partition the dataset to into training data and test data.
set.seed(3792)
inTraining <- createDataPartition(y=hr$salary, p=0.6, list=FALSE)
trainingData <- hr[inTraining,]
testData <- hr[-inTraining,]
# Run the training data with the Decision Tree method.
hrModel <- train(salary~., data=trainingData, method="rpart")
hrModel
# Plot the tree and text.
fancyRpartPlot(hrModel$finalModel, main="Employee Salary Decision Tree", sub="Daniel Izaguirre", palettes="Blues")

## Evaluate the Model
#In-Sample
trainPrediction <- predict(hrModel, trainingData)
confusionMatrix(trainPrediction, trainingData$salary)$table
confusionMatrix(trainPrediction, trainingData$salary)$overall[1]
#Out-of-Sample
testPrediction <- predict(hrModel, testData)
confusionMatrix(testPrediction,testData$salary)$table
confusionMatrix(testPrediction,testData$salary)$overall[1]

### Random Forest
# Partition the dataset to into training data and test data.
set.seed(3792)
inTraining <- createDataPartition(y=hr$salary, p=0.2, list=FALSE)
trainingData <- hr[inTraining,]
testData <- hr[-inTraining,]
# Run the training data with the Random Forest method.
hrModel_rf <- train(salary~., data = trainingData, method = "rf", prox=TRUE)
hrModel_rf

# Evaluate the Model
hrModel_rf$finalModel
testPrediction <- predict(hrModel_rf,testData)
confusionMatrix(testPrediction,testData$salary)$table
confusionMatrix(testPrediction,testData$salary)$overall[1]



#### Generalize the dataset more to lower complexity of the model and impove the accuracy.
hr <- read.csv("HR_comma_sep.csv")
salary_classifier <- function(n){
    if (n == 'high')
      return("high")
    else if(n == 'medium' || n == 'low')
      return("average and low")
  }
hr$salary <- sapply(hr$salary, salary_classifier)
View(head(hr))

### Decision Tree
set.seed(3792)
inTraining <- createDataPartition(y=hr$salary, p=0.6, list=FALSE)
trainingData <- hr[inTraining,]
testData <- hr[-inTraining,]

hrModel <- train(salary~., data=trainingData, method="rpart")
hrModel

fancyRpartPlot(hrModel$finalModel, main="Employee Turnover Decision Tree", sub="Daniel Izaguirre", palettes="Blues")

# In-Sample
trainPrediction <- predict(hrModel, trainingData)
confusionMatrix(trainPrediction, trainingData$salary)$table
confusionMatrix(trainPrediction, trainingData$salary)$overall[1]

# Out-of-Sample
testPrediction <- predict(hrModel, testData)
confusionMatrix(testPrediction,testData$salary)$table
confusionMatrix(testPrediction,testData$salary)$overall[1]

### Random Forest
# Partition the dataset to into training data and test data.
set.seed(3792)
inTraining <- createDataPartition(y=hr$salary, p=0.2, list=FALSE)
trainingData <- hr[inTraining,]
testData <- hr[-inTraining,]
# Run the training data with the Random Forest method.
hrModel_rf <- train(salary~., data = trainingData, method = "rf", prox=TRUE)
hrModel_rf

# Evaluate the data
hrModel_rf$finalModel
testPrediction <- predict(hrModel_rf,testData)
confusionMatrix(testPrediction,testData$salary)$table
confusionMatrix(testPrediction,testData$salary)$overall[1]







