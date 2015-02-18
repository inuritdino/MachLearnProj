# Scratch piece of code of the project work for the Machine Learning

# Load the data, #DIV/0! is treated as NA
data <- 
  read.csv("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",na.strings=c("NA","NaN","#DIV/0!"))
deval <- 
  read.csv("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",na.strings=c("NA","NaN","#DIV/0!"))

# Load the needed libararies
library(caret)

# Cleaning data
# Identify the variable with NA's
nObs <- dim(data)[1] # number of observations
nNA <- function(x){
  return(sum(is.na(x)))
}
nas <- sapply(data[,8:159],function(x)nNA(x))
fracNA <- nas/nObs
# In fact, there are vars having either 0 NA's or something > 0.9 (almost all obs)
dataClean <- data[,7+which(fracNA == 0.0)] # no NA allowed
dataClean <- data.frame(dataClean,data$classe)
names(dataClean)[dim(dataClean)[2]] <- c("classe")

# Make the testing subset (deval for the evaluation purposes)
inTrain <- createDataPartition(dataClean$classe,p = 0.6,list=FALSE)
dtrain = dataClean[inTrain,]
dtest = dataClean[-inTrain,]

# Exploratory analysis
#head(dtrain)
dim(dtrain)
dim(dtest)

# A little function, which gives a formula with var num supplied
# getFormula <- function(data,nameVec){
#   return(paste(names(data)[dim(dtrain)[2]]," ~ ",paste(names(data)[nameVec],collapse = " + ")))
# }

library(rpart)
library(partykit)

# SINGLE-TREE LEARNING
# A simple tree model that works
fit <- train(classe ~ ., method = "rpart", data = dtrain) # not that good as the simpler version
# Acc=0.50, Kappa=0.34.

# The same but directly
fit <- rpart(classe ~ ., data = dtrain) # Acc=0.74,Kappa=0.67
fit <- as.party(fit)
#plot(fit)

# Let us try some tuning now with caret
fit <- train(classe ~ ., data = dtrain, method="rpart", tuneLength = 10, 
             trControl = trainControl(method = "repeatedcv", repeats = 3))
# tuneLength=10: Acc=0.68, Kappa=0.60
# tuneLength=30: Acc=0.84, Kappa=0.80
# tuneLength=50: Acc=0.90, Kappa=0.87
# tuneLength=100: Acc=0.92, Kappa=0.90 (10-fold cv with 3 repeats did not increase Accuracy)

# RANDOM FOREST
fit <- train(classe ~ ., data = dtrain, method = "rf")
# This call gives the best model: Acc=0.99, Kappa=0.99

# Simpler and faster call
fit <- randomForest(classe ~ ., data = dtrain)
# Acc=0.99, Kappa=0.99

# ACCURACY CHECK on the TEST data set
confusionMatrix(predict(fit,newdata = dtest),dtest$classe)
plot.train(fit)
#print.train(fit)
#summary(fit$finalModel)