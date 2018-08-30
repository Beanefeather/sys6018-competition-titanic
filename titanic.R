# Lucas Beane - lhb7tz@virginia.edu
# Titanic Kaggle Competition
# SYS 6018

library(tidyverse)
library(MASS)

# Read in csvs

g_train <- read.csv("train.csv")
g_test <- read.csv("test.csv")


# Fill in NAs in age with median value

g_train$Age[is.na(g_train$Age)] <- median(g_train$Age, na.rm=T)
g_test$Age[is.na(g_test$Age)] <- median(g_test$Age, na.rm=T)

# Try a linear model with most promising features just to check

surv.lm1 <- lm(Survived ~ factor(Pclass) + factor(Sex) + Age + SibSp, data=g_train)


# Make copy of g_train and add predictions (rounding to integer values)

train_copy <- g_train
train_copy$prediction <- predict(surv.lm1, newdata = train_copy)
train_copy$prediction[train_copy$prediction > 0.5] <- 1
train_copy$prediction[train_copy$prediction <= 0.5] <- 0


# Check training accuracy... ~80% 

train_accuracy <- nrow(train_copy[train_copy$Survived == train_copy$prediction,])/nrow(train_copy)


# Make copy of g_test and add predictions (rounding to integer values) and get test accuracy

test_copy <- g_test
test_copy$prediction <- predict(surv.lm1, newdata = test_copy)
test_copy$prediction[test_copy$prediction > 0.5] <- 1
test_copy$prediction[test_copy$prediction <= 0.5] <- 0

test_accuracy <- nrow(test_copy[test_copy$Survived == test_copy$prediction,])/nrow(test_copy)


