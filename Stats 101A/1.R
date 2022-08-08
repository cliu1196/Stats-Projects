library(readr)
library(tidyverse)
library(car)
FifaNoY <- read.csv("UCLA Works/UCLA Winter 2020/Stats 101A/Project/Project Attempts/FifaNoY.csv", stringsAsFactors = FALSE)
FifaTrainNew <- read.csv("UCLA Works/UCLA Winter 2020/Stats 101A/Project/Project Attempts/FifaTrainNew.csv", stringsAsFactors = FALSE)
attach(FifaTrainNew)
View(FifaTrainNew$LS)
# WageNew ~ Overall + Potential + Club + CM + CAM + CF + ST --> 90.18%
# WageNew ~ Overall + Potential + Club + RW + LS + RS + LDM + RDM --> 90.25%
m1 <- lm(WageNew ~ Overall + Potential + Club + RW + LS + RS + LDM + RDM, data = FifaTrainNew)
summary(m1)
par(mfrow = c(2, 2))
plot(m1)


m2 <- lm(log(WageNew) ~ 0 + Overall + Positioning, data = FifaTrainNew)
summary(m2)
par(mfrow = c(2, 2))
plot(m2)



leverage <- hatvalues(m2)
nrow(FifaTrainNew) 
leverage_points <- which(leverage >= 2 * (2 + 1)/12745 & abs(rstandard(m2)) >= 2)
outlier_points <- which(leverage >= 2 * mean(leverage) & abs(rstandard(m2)) >= 2)
bad_leverages <- intersect(leverage_points, outlier_points)


m3 <- lm(log(WageNew) ~ 0 + Overall + Positioning, data = FifaTrainNew[-bad_leverages, ])
summary(m3)
par(mfrow = c(2, 2))
plot(m3)








FifaTrainNew %>%
  group_by(Position) %>%
  summarise(
    average.wage = sum(WageNew)
  ) %>%
  arrange(desc(average.wage)) %>%
  head(20)




new_data <- data.frame(FifaNoY$Overall, FifaNoY$Positioning)
names(new_data) <- c("Overall", "Positioning")
predicts <- predict(m3, new_data) %>% as.data.frame()
predictions <- cbind(FifaNoY$Ob, predicts)

names(predictions) <- c("Ob","WageNew")
predictions$WageNew <- ifelse(is.na(predictions$WageNew), mean(FifaTrainNew$WageNew), predictions$WageNew)


wageNew <- predict(m3, FifaNoY)
ifelse(is.na(wageNew), mean(FifaTrainNew$wageNew), wageNew)

which(is.na(wageNew))

write.csv(predictions, 'kaggleattempt3.csv', row.names = FALSE)
