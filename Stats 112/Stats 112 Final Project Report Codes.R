# Libraries to Load Up in RStudio:
library(readr)
library(car)
library(PerformanceAnalytics)
library(effects)
library(gplots)

# RStudio Codes:
stemfinal <- read_csv("UCLA Works/UCLA Fall 2019/Stats 112/R Codes/Raw Data/stemfinal.csv")
attach(stemfinal)
View(stemfinal)
attach(stemfinal)
table(Ethnic)
Ethnicr <- recode(Ethnic, "'Latino/Hispanic' = 'Other';'Mixed Race'  = 'Other'")
table(Ethnicr)
m1 <- lm(Belonging~ Majorr + Stress * Ethnicr)
summary(m1)
plot(m1)

m2 <- lm(Belonging ~ Stress * Ethnicr)
summary(m2)
plot(allEffects(m2), ask = FALSE)

m1vif <- lm(Belonging ~ Majorr + Stress + Ethnicr)
vif(m1vif)
residualPlots(m1)
influencePlot(m1)
NewDataSet <- stemfinal[-c(54, 69, 701, 733), ] 
summary(NewDataSet)

scatterplot(Belonging, Stress)
cor(Belonging, Stress, use = "complete.obs")

table(Majorr, Ethnicr)
plot(Stress, Belonging, main="Scatterplot of Students' Stress and Sense of Belonging to UCLA"),
abline(line(Stress,Belonging), col="red")
hist(Stress)
hist(Belonging)

plotmeans(Belonging~Majorr, main="Plot of the means and error bars for
          Sense of Belonging to UCLA and Major of students")
plotmeans(Belonging~Ethnicr, main="Plot of the means and error bars for
          Sense of Belonging to UCLA and Ethnicity of students")
summary(Stress)
cutstress <- cut(Stress, br=c(0, 37.50, 62.50, 100.00),
                 labels=c("Low Stress", "Medium Stress", "High Stress"), right = FALSE)
Stress_Levels <- as.factor(cutstress)
plotmeans(Belonging ~ Stress_Levels, main="Plot of the means and error bars for
          Sense of Belonging to UCLA and Stress of students")

summary(Belonging)
summary(Stress)
cutbelong <- cut(Belonging, br=c(0, 61.10, 97.20),
                 labels=c("Doesn't Belong","Belongs"),right=FALSE)
cutstress <- cut(Stress, br=c(0, 37.50, 62.50, 100.00),
                 labels=c("Low Stress", "Medium Stress", "High Stress"), right = FALSE)
mytable <- table(cutbelong, cutstress)
prop.table(mytable, 2)
