rm(list=ls())

library(FNN)
library(Metrics)
library(glmnet)
library(randomForest)

data <- read.csv("num_preds.csv")

# We will use an 80/20 split for training and test sets.

n <- nrow(data)
n_train <- round(0.8*n)
n_test <- n - n_train

# Set seed for reproducibility
set.seed(351)

train_indices <- sample(1:n, n_train)
test_indices <- setdiff(1:n, train_indices)

data_train <- data[train_indices, ]
data_test <- data[test_indices, ]

# Create matrices
data_mat <- data

data_mat_train <- as.matrix(data_mat[train_indices, ])
data_mat_test <- as.matrix(data_mat[test_indices, ])

print("Fitting KNN model...")

t0 <- Sys.time()

# KNN
knn_model <- FNN::knn.reg(train=data_train[, colnames(data_train) != "growth_2_6"],
                          test=data_test[, colnames(data_train) != "growth_2_6"],
                          y=data_train$growth_2_6,
                          k=3)
# Test set predictions
preds_knn <- knn_model$pred

print("Done! Time to fit model:")

print(Sys.time() - t0)

test_errors <- data.frame("KNN Model (k=3)", Metrics::mse(data_test$growth_2_6, 
                                                          preds_knn))
colnames(test_errors) <- c("Model", "Mean Squared Test Error")

print(test_errors)

# Linear regression

print("Fitting linear regression model...")

t0 <- Sys.time()

lin_model <- lm(growth_2_6 ~., data=data_train)

# Test set predictions
preds_lin <- predict(lin_model, newdata=data_test)

print("Done! Time to fit model:")

print(Sys.time() - t0)

lin_errors <- data.frame("Linear regression", Metrics::mse(data_test$growth_2_6,
                                                           preds_lin))
colnames(lin_errors) <- c("Model", "Mean Squared Test Error")

test_errors <- rbind(test_errors, lin_errors)

print(test_errors)

# Lasso

print("Fitting lasso model...")

t0 <- Sys.time()

lasso_model <- glmnet::cv.glmnet(x=data_mat_train[, colnames(data_mat_train) != "growth_2_6"],
                                 y=data_train$growth_2_6,
                                 family="gaussian")

# Test set predictions
preds_lasso <- predict(lasso_model,
                       newx=data_mat_test[, colnames(data_mat_test) != "growth_2_6"],
                       s="lambda.1se")

print("Done! Time to fit model:")

print(Sys.time() - t0)

lasso_errors <- data.frame("Lasso", Metrics::mse(data_test$growth_2_6,
                                                 preds_lasso))
colnames(lasso_errors) <- c("Model", "Mean Squared Test Error")

test_errors <- rbind(test_errors, lasso_errors)

print(test_errors)

# Random forest

print("Fitting random forest model...")

t0 <- Sys.time()

rf_model <- randomForest(growth_2_6 ~., data=data_train)

# Test set predictions
preds_rf <- predict(rf_model,
                       newdata=data_test[, colnames(data_test) != "growth_2_6"])

print("Done! Time to fit model:")

print(Sys.time() - t0)

rf_errors <- data.frame("Random Forest", Metrics::mse(data_test$growth_2_6,
                                                 preds_rf))
colnames(rf_errors) <- c("Model", "Mean Squared Test Error")

test_errors <- rbind(test_errors, rf_errors)

print(test_errors)



