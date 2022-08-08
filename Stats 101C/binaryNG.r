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
setwd("~/101C/Kaggle1/")
training <- read.csv("training.csv", header = TRUE)
attach(training)
# ================================ #
# CREATE separate dataset
# ================================ #
traintags <- c("NG", "notNG")[as.integer(class!=0)+1]
training$NGboolean <- factor(traintags, levels=c("NG","notNG"))
attach(training)
# ================================ #
# CREATE PROPORTIONAL PARTITION
# ================================ #
limit <- 337
indexNG <- sample(which(class==0))[1:limit]
indexOG <- sample(which(class==1))
indexTSG <- sample(which(class==2))
trainindex <- c(indexNG,indexOG, indexTSG)
# ================================ #
# TRAIN-TRAIN TRAIN-TEST SPLIT
# ================================ #
trainNG.data <- training[sample(trainindex),]
# We assume we have not seen this.
train_control <- trainControl(method="cv", number = 5, 
                              classProbs = TRUE, 
                              savePredictions = TRUE)
trainNG.train <- train(NGboolean ~ 
                       Splice_TO_Benign_Ratio
                       +Polyphen2
                       +Silent_fraction
                       +CDS_length
                       +Exon_Cons
                       +S50_score_replication_timing
                       +BioGRID_clossness
                       +Promoter_hypermethylation_in_cancer
                       +intolerant_pNull
                       +Broad_H4K20me1_percentage
                     ,data = trainNG.data, 
                     method='glm', family='binomial',
                     #method='qda',# preProc=c("center", "scale"),
                     #method='knn', preProc=c("center", "scale"), 
                     #method = "lda", preProc = c("center", "scale"),
                     trControl = train_control)
trainNG.train

# ================================ #
# WRITE CSV FILE
# ================================ #
test <- read.csv('test.csv', header = TRUE)
predNG <- predict(trainNG.train, newdata = test)
predictions.NG <- cbind(test$id, predNG) %>% as.data.frame()
names(predictions.NG) <- c("id", "class") #for some reason the values added by one
predictions.NG$class <- predictions.NG$class-1

# ================================ #
# SAMPLE VARIABLES NOT NG
# ================================ #
OGTSG.test <- test[which(predictions.NG$class==1),]

