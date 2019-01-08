
# Training and comparison of models
# Hugo Andres Dorado B.
# 04-01-2019

rm(list = ls())

library(here)
library(caret)
library(caretEnsemble)
library(randomForest)
library(nnet)
library(kernlab)
library(plyr)

dataset <- read.csv('RESULTS/Process_dataset.csv')

summary(dataset)

dimMat <- ncol(dataset)

#--------------------------Training model--------------------------------------

set.seed(123)

model_train <- train(x = dataset[,-dimMat], y = dataset[,dimMat], 
                     trControl = trainControl(method = 'repeatedcv',
                                              number = 3,repeats = 5),
                     model='rf')


model_train

save(model_train ,file = 'RESULTS/Final_model.RData')


