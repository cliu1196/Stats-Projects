# ================================ #
# LOAD IN THE LIBRARIES
# ================================ #
library(ggplot2)
library(dplyr)
library(reshape2)
library(caret)
library(boot)
library(readxl)
library(nnet) #for more than 2 response values
# ================================ #
# CREATE separate dataset
# ================================ #
setwd("~/101C/Kaggle1/")
training <- read.csv("training.csv", header = TRUE)
attach(training)
# ================================ #
# CREATE separate dataset
# ================================ #
trainOGTSG.data <- training[class!=0,]
attach(trainOGTSG.data)
traintags <- c("OG", "TSG")[as.integer(class!=1)+1]
trainOGTSG.data$OGTSGboolean <- factor(traintags, levels=c("OG","TSG"))
attach(trainOGTSG.data)
# ================================ #
# TRAIN-TRAIN TRAIN-TEST SPLIT
# ================================ #
# We assume we have not seen this.
train_control <- trainControl(method="cv", number = 5, 
                              classProbs = TRUE, 
                              savePredictions = TRUE)
modelOGTSG.train <- train(OGTSGboolean ~ 
                          Missense_Entropy
                          +Frameshift_indel_fraction
                          +S50_score_replication_timing
                          +VEST_score
                          +Cell_proliferation_rate_CRISPR_KD
                          +H3K27me3_height
                          +H3K9me3_height
                          +Broad_H3K9me2_percentage
                          +H3K9me2_height
                          +Broad_H4K20me1_percentage
                       ,data = trainOGTSG.data, 
                       method='glm', family='binomial',
                       #method='qda',# preProc=c("center", "scale"),
                       #method='knn', preProc=c("center", "scale"), 
                       #method = "lda", preProc = c("center", "scale"),
                       trControl = train_control)
modelOGTSG.train

# ================================ #
# WRITE CSV FILE
# ================================ #
predLR <- predict(modelOGTSG.train, newdata = OGTSG.test)
predictions.NG <- cbind(test$id, predLR) %>% as.data.frame()
names(predictions.NG) <- c("id", "class") #for some reason the values added by one
predictions.NG$class <- predictions.NG$class-1


#creating csv file to turn in
write.csv(predictions, "predictions.csv", row.names = FALSE)