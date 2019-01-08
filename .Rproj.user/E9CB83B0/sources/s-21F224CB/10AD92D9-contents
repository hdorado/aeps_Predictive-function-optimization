

# Prueba de algoritmo GHB sobre un modelo de aprendizaje
# Hugo Andres Dorado B.

rm(list = ls())

library(here)
library(caret)
library(caretEnsemble)
library(randomForest)
library(nnet)
library(kernlab)
library(plyr)

source(here::here("SCRIPTS","GHS_FUNS.R"))


#-------------------------Preprocessing data-----------------------------------

dataset <- read.csv(here::here('DATA/matriz_reg_chiapas.csv'),row.names = 1)

nams <- read.csv(here::here('DATA/nams_matriz_mexico.csv'))

### Fix varieties

dataset$variedad <- as.character(dataset$variedad)

table(dataset$variedad)

dataset$variedad [dataset$variedad  == 'CLTHW11002'] <- "OTRAS"

dataset$variedad [ dataset$variedad  == 'CRIOLLOAMARILLO' | dataset$variedad  == 'CRIOLLOBLANCO'] <- "CRIOLLO"

dataset$variedad [ dataset$variedad  == 'DEKALB357' | dataset$variedad  == 'DEKALB370' | dataset$variedad  == 'DEKALB380'|dataset$variedad  == 'DEKALB390'|dataset$variedad  == 'DEKALB395'|dataset$variedad  == 'DEKALB7500'|dataset$variedad  == 'DEKALB935'] <- "DEKAL"

dataset$variedad <- factor(dataset$variedad)

dataset <- dataset[,-nearZeroVar(dataset)]

numDataset  <- dataset[sapply(dataset,is.numeric)]

cualDataset <- dataset[sapply(dataset,is.factor)]

HighCorr <- findCorrelation(cor(numDataset),cutoff = 0.7)

numDataset <- numDataset[,-HighCorr]

dataset <- data.frame(cualDataset,numDataset)x

names(dataset) <- nams$name_short_name

cbind(nmds=names(dataset))

# datasets <- dummyProcess(dataset=dataset,response='rendimiento')

write.csv(dataset,'RESULTS/Process_dataset.csv',row.names = F)

