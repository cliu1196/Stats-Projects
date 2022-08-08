library(ggplot2)
library(dplyr)
library(reshape2)
library(caret)
library(boot)
library(readxl)
library(nnet) #for more than 2 response values
# ================================ #
# READ IN THE DATA
# ================================ #
setwd("~/101C/Kaggle1/")
training <- read.csv("training.csv", header = TRUE)
attach(training)
dim(training)

# ================================ #
# CREATE RESPONSE FACTOR
# ================================ #
traintags <- c("NG","OG","TSG")[as.integer(class)+1]
training$classfactor <- factor(traintags, levels=c("NG","OG","TSG"))

# ================================ #
# CREATE PROPORTIONAL PARTITION
# ================================ #
limit <- 120
indexNG <- sample(which(class==0))[1:limit]
indexOG <- sample(which(class==1))[1:limit]
indexTSG <- sample(which(class==2))[1:limit]
trainindex <- c(indexNG,indexOG, indexTSG)

# ================================ #
# TRAIN-TRAIN TRAIN-TEST SPLIT
# ================================ #
train_training <- training[trainindex,]
# We assume we have not seen this.
train_test <- training[-trainindex,]

# ================================ #
# TEST ON MODELS
# ================================ #
train_control <- trainControl(method="cv", number = 5, 
                              classProbs = TRUE, 
                              savePredictions = TRUE)
modelfit <- train(classfactor ~ BioGRID_log_degree + N_LOF + N_Splice + LOF_KB_Ratio + 
                    Polyphen2 + H4K20me1_height + LOF_TO_Silent_Ratio + 
                    VEST_score, data = train_training, method = "multinom", family = "multinomial", 
                  preProc = c("center", "scale"), trControl = train_control) 
modelfit
#LDAfitCV_misrisk <- sum((as.integer(LDAfit$pred$pred)-as.integer(LDAfit$pred$obs))^2)/ntrain
#LDAfitCV_misrisk

# ================================ #
# PREDICT ON MODELS
# ================================ #
glm.train.train <- predict(model_fit, training_, type="class")
trainError <- table(glm.train.train, training_train$class)
trainError
points <- trainError[1,1] + 20* (trainError[2,2] + trainError[3,3])
totalPoints <- table(training_train$class)[[1]] + 20 * (table(training_train$class)[[2]] + table(training_train$class)[[3]])
points/totalPoints