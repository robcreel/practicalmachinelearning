# Machine Learning Project

remove(list = ls())
current_path <- "/home/robert/Data Science/ML/project/practicalmachinelearning/"
setwd(current_path)

library(caret)

trainingURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testingURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
infoURL <- "http://groupware.les.inf.puc-rio.br/har"
paperURL <- "http://groupware.les.inf.puc-rio.br/public/papers/2013.Velloso.QAR-WLE.pdf"


# If the files aint got, git 'em.
if(!file.exists("pml-training.csv")){
  download.file(trainingURL, "pml-training.csv")
}

if(!file.exists("pml-testing.csv")){
  download.file(testingURL, "pml-testing.csv")
  time_data_downloaded <- Sys.time()
}

# If the sets aint loaded, load 'em.
if(!exists("training")){
  training <- read.csv("pml-training.csv")
}

if(!exists("testing")){
  testing <- read.csv("pml-testing.csv")
}

# Fitcher model.
# modFit <- train(classe ~ ., method = "glm", data = training)
# modFit <- train(classe ~ ., method = "lm", data = trainingTidy1)
# # Note: This model won't work, nor will any model likely, 
# # until the date are cleaned.
# # Impute the NAs?

NAs <- sapply(testing, function(x) sum(is.na(x)))
hasNAs <- which(NAs > 0)
trainingTidy1 <- training[-hasNAs]
trainingTidy1B <- trainingTidy1[, -(1:6)]

# PCA attempt
# M <- abs(cor(trainingTidy1[, -60]))
# diag(M) <- 0
# which(M > 0.8, arr.ind = TRUE)
# preProc <- preProcess(log10(trainingTidy1B[, -54] + 1), 
#                       method = "pca", scale = FALSE, pcaComp = 2)
# trainPC <- predict(preProc, log10(trainingTidy1B[, -54] +1))
# modelFit <- train(trainingTidy1B$classe ~ ., method = "glm", data = trainingTidy1B)

# # From the forum
# library(randomForest)
# rFModel <- randomForest(classe ~ ., data=trainingTidy1B)
# 
# pred2 <- predict(rFModel, testing, type = "class")
# # cm2 <- confusionMatrix(pred2, testing$classe)
# # cm2
# final <- predict(rFModel, testing, type = "class")
# save(rFModel, file = "rFModel.RData")

# From the Random Forest lecture slide
# modFit <- train(classe ~ ., data = trainingTidy1B, method = "rf", prox = TRUE)
# Note: This failed due insufficient memory after ~100 minutes.
# pred <- predict(modFit, testing)

# # From the lgreski (Len the Coursera Mod) github
# 
# library(mlbench)
# data(Sonar)
# # library(caret)
# set.seed(95014)
# 
# # create training & testing data sets
# 
# # inTraining <- createDataPartition(Sonar$Class, p = .75, list=FALSE)
# # training <- Sonar[inTraining,]
# # testing <- Sonar[-inTraining,]
# 
# # set up training run for x / y syntax because model format performs poorly
# # x <- training[,-61]
# # y <- training[,61]
# 
# library(parallel)
# library(doParallel)
# cluster <- makeCluster(detectCores()) # convention to leave 1 core for OS
# registerDoParallel(cluster)
# 
# fitControl <- trainControl(method = "cv",
#                            number = 10,
#                            allowParallel = TRUE)
# 
# trainModel <- train(classe ~ ., method="rf",data = trainingTidy1B, trControl = fitControl)
# save(trainModel, file = "trainModel.RData")
# 
# # Beep!
# library(beepr)
# beep()
# 
# stopCluster(cluster)
# registerDoSEQ()
# 
# # trainModel
# # trainModel$resample
# # confusionMatrix.train(trainModel)


# MODIFIED FOR CROSS VALIDATION
# From the lgreski (Len the Coursera Mod) github
# MODIFIED FOR CROSS VALIDATION

library(mlbench)
# data(Sonar)
# library(caret)
set.seed(95014)

# create training & testing data sets

inTraining <- createDataPartition(trainingTidy1B$classe, p = .8, list=FALSE)
trainingCV <- trainingTidy1B[inTraining,]
testingCV <- trainingTidy1B[-inTraining,]

# set up training run for x / y syntax because model format performs poorly
x <- trainingTidy1B[,-54]
y <- trainingTidy1B[,54]

library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores()) # convention to leave 1 core for OS
registerDoParallel(cluster)

fitControl <- trainControl(method = "cv",
                           number = 10,
                           allowParallel = TRUE)

if(!exists("trainModelCV")){
  if(file.exists("trainModelCV.RData")){
    load("trainModelCV.RData")
  } else {
    trainModelCV <- train(x, y, method="rf", data = trainingTidy1B, trControl = fitControl)
    save(trainModelCV, file = "trainModelCV.RData")
  }
}

stopCluster(cluster)
registerDoSEQ()

pred <- predict(trainModelCV, testingCV)
table(pred, testingCV$classe)

# Beep!
library(beepr)
beep()
