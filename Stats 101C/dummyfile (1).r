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
# READ IN THE DATA
# ================================ #
setwd("~/101C/Kaggle1/")
training <- read.csv("training.csv", header = TRUE)
test <- read.csv('test.csv', header = TRUE)
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
limit <- 130
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
lda.train <- train(classfactor ~ 
                     Polyphen2
                  + VEST_score
                  + Missense_Entropy
                  + Nonsense_fraction
                  + CDS_length
                  + Exon_Cons
                  + MGAentropy
                  + S50_score_replication_timing
                  + Super_Enhancer_percentage
                  + BioGRID_clossness
                  + BioGRID_log_degree # adding Cassandra's predictors
                  + N_LOF 
                  + Polyphen2 
                  + H3K79me2_width 
                  + Gene_age 
                  + Promoter_hypermethylation_in_cancer # Cassandra's predictors end
                  + intolerant_pNull
                  + Missense_Zscore
                  + pLOF_Zscore
                  + RVIS_percentile
                  + ncGERP
                  + Length_H3K4me3
                  + H3K4me3_height
                  + H3K4me1_height
                   ,data = train_training, 
                  #method='qda',# preProc=c("center", "scale"),
                  #method='knn', preProc=c("center", "scale"), 
                  #method='multinom', family='multinomial', #preProc=c("center", "scale"), 
                  method = "lda", preProc = c("center", "scale")
                  ,trControl = train_control)
lda.train
#LDAfitCV_misrisk <- sum((as.integer(LDAfit$pred$pred)-as.integer(LDAfit$pred$obs))^2)/ntrain
#LDAfitCV_misrisk

# ================================ #
# PREDICT ON MODELS
# ================================ #
predLR <- predict(lda.train, newdata = train_test)
testError <- confusionMatrix(data = predLR, reference = train_test$classfactor, mode="everything")$table

#counting points (20 points for correctly evaluating 1 and 2, 1 point for class 0) 
points <- testError[1,1] + 20* (testError[2,2] + testError[3,3]) 
totalPoints <- sum((testError)[,1]) + 20 * sum((testError)[,-1]) 
points/totalPoints

# ================================ #
# WRITE CSV FILE
# ================================ #
predLR <- predict(lda.train, newdata = test)
predictions = cbind(test$id, predLR) %>% as.data.frame()
names(predictions) <- c("id", "class") #for some reason the values added by one
predictions$class = predictions$class - 1
#creating csv file to turn in
write.csv(predictions, "predictions.csv", row.names = FALSE)

