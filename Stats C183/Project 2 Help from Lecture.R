library(readr)

# Read your csv file:
a <- read.csv("~/UCLA Works/UCLA Spring 2020/Stats C183/Project/stockData.csv",
              sep=",", header=TRUE)

# Convert adjusted close prices into returns:
r <- (a[-1,3:ncol(a)]-a[-nrow(a),3:ncol(a)])/a[-nrow(a),3:ncol(a)]

# Compute mean vector:
means <- colMeans(r[-1]) # Without ^GSPC

# Compute variance covariance matrix:
covmat <- cov(r[-1]) # Without ^GSPC

# Compute correlation matrix:
cormat <- cor(r[-1]) # Without ^GSPC

# Compute the vector of variances:
variances <- diag(covmat)

# Compute the vector of standard deviations:
stdev <- diag(covmat)^.5

# Set up A - E formulas & column of Ones:
ones <- rep(1, 30)
A <- t(ones) %*% solve(covmat) %*% means
B <- t(means) %*% solve(covmat) %*% means
C <- t(ones) %*% solve(covmat) %*% ones
D <- B * C - A^2
E <- seq(-5,5,.1)



### Parabola:
#Compute sigma2 as a function of A,B,C,D, and E:
sigma2 <- (C*E^2 - 2*A*E +B) /D

#Plot sigma2 against E:
plot(E, sigma2, type="l", ylab=expression(sigma^2))

#Add the minimum risk portfolio:
points(A/C, 1/C, pch=19)

#Or plot E against sigma2:
plot(sigma2, E,type="l", xlab=expression(sigma^2))

#Add the minimum risk portfolio:
points(1/C, A/C, pch=19)




### Hyperbola:
plot(0, A/C, main = "Portfolio possibilities curve", xlab = "Risk (standard deviation)",
     ylab = "Expected Return", type = "n",
     xlim = c(-2*sqrt(1/C), 4*sqrt(1/C)), 
     ylim = c(-2*A/C, 4*A/C))

#Plot center of the hyperbola:
points(0, A/C, pch = 19)

#Plot transverse and conjugate axes:
abline(v = 0) #Also this is the y-axis.
abline(h = A/C)

#Plot the x-axis:
abline(h = 0)

#Plot the minimum risk portfolio:
points(sqrt(1/C), A/C, pch=19)

#Find the asymptotes:
V <- seq(-1, 1, 0.001)
A1 <- A/C + V * sqrt(D/C)
A2 <- A/C - V * sqrt(D/C)
points(V, A1, type = "l")
points(V, A2, type = "l")

#Efficient frontier:
minvar <- 1/C
minE <- A/C
sdeff <- seq((minvar)^0.5, 1, by = 0.0001)
options(warn = -1)
y1 <- (A + sqrt(D*(C*sdeff^2 - 1)))*(1/C) 
y2 <- (A - sqrt(D*(C*sdeff^2 - 1)))*(1/C) 
options(warn = 0)

points(sdeff, y1, type = "l")
points(sdeff, y2, type = "l")
