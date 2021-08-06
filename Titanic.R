library(tidyverse)
library(caret)
library(mice)
library(skimr)

original_train <- read.csv("C:\\R\\Competitions\\Titanic_Machine Learning from Disaster\\train.csv")
original_test <- read.csv("C:\\R\\Competitions\\Titanic_Machine Learning from Disaster\\test.csv")

##################################################################################################
# Initial cleaning
train_imp <- original_train
test_imp <- original_test

train_imp$Name <- NULL
test_imp$Name <- NULL
train_imp$PassengerId <- NULL
test_imp$PassengerId <- NULL
train_imp$Ticket <- NULL
test_imp$Ticket <- NULL

train_imp$Cabin <- ifelse(train_imp$Cabin != "", TRUE, FALSE)
test_imp$Cabin <- ifelse(test_imp$Cabin != "", TRUE, FALSE)

train_imp$Sex <- ifelse(train_imp$Sex != "", 1, 2)
test_imp$Sex <- ifelse(test_imp$Sex != "", 1, 2)

train_imp$Embarked <- case_when(
  train_imp$Embarked == "C" ~ 1,
  train_imp$Embarked == "Q" ~ 2,
  train_imp$Embarked == "S" ~ 3
)
test_imp$Embarked <- case_when(
  test_imp$Embarked == "C" ~ 1,
  test_imp$Embarked == "Q" ~ 2,
  test_imp$Embarked == "S" ~ 3
)

# Impute NAs
train_imp <- mice(train_imp)
train_imp <- complete(train_imp, 1)
test_imp <- mice(test_imp)
test_imp <- complete(test_imp, 1)



##################################################################################################
# Take out a limited sample
set.seed(1234)
train.index <- createDataPartition(train_imp$Survived, p=0.2, list=FALSE)
train <- train_imp[ train.index,]


##################################################################################################
# Create limited sample models
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

set.seed(1234)
model_gbm <- train(Survived ~ .,
                   data = train, 
                   method = "gbm", 
                   trControl = ctrl
)

set.seed(1234)
model_ranger <- train(Survived ~ .,
                      data = train, 
                      method = "ranger",
                      ntree = 5,
                      trControl = ctrl
)

set.seed(1234)
model_SVM <- train(Survived ~ .,
                   data = train, 
                   method = "svmPoly",
                   trControl = ctrl
)

set.seed(1234)
model_NNET <- train(Survived ~ .,
                    data = train, 
                    method = "nnet",
                    trControl = ctrl
)

set.seed(1234)
model_GLM <- train(Survived ~ .,
                    data = train, 
                    method = "glm",
                    trControl = ctrl
)


##################################################################################################
# Evaluate the limited sample models
results <- resamples(list(GBM=model_gbm, Ranger=model_ranger, SVM=model_SVM, NNET=model_NNET, GLM=model_GLM))
summary(results)
#bwplot(results)
dotplot(results)



##################################################################################################
# GBM is chosen as best model
set.seed(1234)
model_gbm_full <- train(Survived ~ .,
                   data = train_imp, 
                   method = "gbm", 
                   trControl = ctrl
)

result <- predict()



