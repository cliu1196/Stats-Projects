library(readr)
fpd <- read_csv("UCLA Works/UCLA Fall 2019/Stats 112/R Codes/Raw Data/stemfinal.csv")
attach(fpd)

### Belonging ~ Majorr + Stress x Ethnicity (using attach() function)
library(car)
table(Ethnic)
Ethnicr <- recode(Ethnic, "'Latino/Hispanic' = 'Other';'Mixed Race'  = 'Other'")
table(Ethnicr)
m1 <- lm(Belonging ~ Majorr + Stress * Ethnicr)
summary(m1)
plot(m1)

# Therefore, there is not enough evidence to conclude significance with Ethnicity 
# and Stress.

# Ethnicity has been recoded to combine Latino/Hispanic & Mixed Race into Other.

# We can see that for Mixed Majors is NOT statistically significant, which means
# it has too high of a p-value (0.26). For every 1 unit of increase for sense of Beloging at UCLA,
# we see ON AVERAGE a 1.78 decrease.

# We can see that for Physical Science Majors is statistically significant, 
# which means it has p-value close to zero (0.0009). For every 1 unit of increase 
# for sense of Beloging at UCLA, we see ON AVERAGE a 2.66 decrease.

# We can see that for Social Science Major is NOT statistically significant, which means
# it has too high of a p-value (0.94). For every 1 unit of increase for sense of Beloging at UCLA,
# we see ON AVERAGE a 0.11 increase.

# We can see that for Stress levels is statistically significant, 
# which means it has p-value close to zero (< 2e-16). For every 1 unit of increase 
# for sense of Beloging at UCLA, we see ON AVERAGE a 0.50 decrease.

# We can see that for Caucasian Ethnicity is NOT statistically significant, which means
# it has too high of a p-value (0.36). For every 1 unit of increase for sense of Beloging at UCLA,
# we see ON AVERAGE a 2.59 increase.

# We can see that for Other Ethnicity is NOT statistically significant, which means
# it has too high of a p-value (0.96). For every 1 unit of increase for sense of Beloging at UCLA,
# we see ON AVERAGE a 0.15 decrease.

# For our INTERACTION EFFECT:

# For Ethnicity, our baseline is "Asian" Ethnicity. 
# On average, the respondents who are of Caucasian Ethnicity  score 2.58700 points 
# higher on their sense of Belonging here at UCLA. This difference is not statistically 
# significant (p = 0.363045). 
# Thus, we can say that being EthnicrCaucasian is not related to their sense of Belonging 
# here at UCLA.

# For Ethnicity, our baseline is "Asian" Ethnicity. 
# On average, the respondents who are of Other Ethnicity  score 0.15041 points 
# lower on their sense of Belonging here at UCLA. This difference is not statistically 
# significant (p = 0.957147). 
# Thus, we can say that being EthnicrOther is not related to their sense of Belonging 
# here at UCLA. 

# For the Interaction Effect part,we can say they are NOT related to Belonging 
# because Asian, Caucasian, Other are NOT statistically significant.

# We are unable to draw any conclusions due to the high p-value and NOT being statistically
# significant.







### INTERACTION PLOTS:
library(car)
library(effects)
m2 <- lm(Belonging ~ Stress * Ethnicr)
summary(m2)
plot(allEffects(m2), ask=FALSE)

# The interaction effect is not statistically
# significant indicating that the effect of Stress on Belonging is similar
# for different levels of Ethnicity.





### CHECKING CORRELATION:
scatterplot(Belonging, Stress)
# There is a linear relationship between Stress and Belonging
cor(Belonging, Stress, use = "complete.obs")
cor(Stress, Belonging, use = "complete.obs")



# Checking Major variable:
table(b)
recode()



# Install Necessary Packages:
install.packages("PerformanceAnalytics")
install.packages("car")


# Testing the Correlation of my Data to see if Predictor or if Outcome
# EXAMPLE:
library(car)
library(PerformanceAnalytics)
mynewdata <- fpd[ ,c("Belonging","GPA","Stress")]
cor(mynewdata, use="complete.obs")
scatterplotMatrix(mynewdata)
chart.Correlation(mynewdata)



# Checking for Leverages:
library(car)
residualPlots(m1)
influencePlot(m1)


# Check VIF:
library(car)
library(PerformanceAnalytics)
m2 <- lm(D ~ b + E + d)
vif(m2)
# Not over 5 or under -5 so we are good!
hatvalues(m1)[hatvalues(m1) > 3*mean(hatvalues(m1))]



# Check Odds ratio:
attach(fpd)
table()

# EX. for cutting table numeric to factor
# E = Stress, D = Belonging
summary(Belonging)
summary(Stress)
cutbelong <- cut(Belonging, br=c(0, 61.10, 97.20),
                    labels=c("Doesn't Belong","Belongs"),right=FALSE)
cutstress <- cut(Stress, br=c(0, 37.50, 62.50, 100.00),
              labels=c("Low Stress", "Medium Stress", "High Stress"),right=FALSE)


mytable <- table(cutbelong, cutstress)
# 1 = row %, 2 = column %
prop.table(mytable,2)
# Of the respondents with Low Stress, 11% feel like they Don't Belong here at UCLA, and
# 89% feel like they do Belong.

# Of the respondents with Medium Stress, 32% feel like they Don't Belong here at UCLA, and
# 68% feel like they do Belong.

# Of the respondents with High Stress, 70% feel like they Don't Belong here at UCLA, and
# 30% feel like they do Belong.

# We can reach the conclusion that as Stress levels elevate higher for students, their
# sense of Belonging here at UCLA lessens.





### TABLES:
table(Majorr, Ethnicr)
table(Ethnicr)
table(Majorr)






### Plot of Means:
install.packages(gplots)
library(gplots)
plotmeans(Belonging ~ Majorr , Stress , Ethnicr, main="plot of the means and error bars for perception 
+ of exclusion at UCLA and planning to leave UCLA")
Stress_Levels <- as.factor(cutstress)
plotmeans(Belonging ~ Stress_Levels)






### SIDE PROJECT:
# We could've used model selection to find a better fitting model that may not include interaction.
# We could use LRT to test which model fits better.
lr.test


# Checking Data
View(fpd)

# Checking Leverage Values (4/n):
nrow(fpd)
4/866

# Checking levels (need minimum of 5 levels)
levels(as.factor(b))

# Other Checking:
is.factor(b)
is.numeric(b)
levels(b)
class(b)
as.factor(b)
table(as.factor(b))
as.factor(b)
na.omit(b)
barplot(as.factor(b))

install.packages("gplots")
library(gplots)
plotmeans(Belonging ~ Majorr, main="plot of the means and error bars for perception 
+ of exclusion at UCLA and planning to leave UCLA")
