



#Lesson 1 Swirl - intro
install.packages("swirl")
library(swirl)
install_from_swirl("Regression Models")
plot(child ~ parent, galton)
plot(jitter(child, 4) ~ parent, galton)
regrline <- lm(child ~ parent, galton)
abline(regrline, lwd=3, col='red')
summary(regrline)

#Lesson 2 swirl - residuals
#Display regression data
summary(fit)
mean(fit$residuals)
-2.359884e-15

#correlation between residuals and predictors
cov(fit$residuals, galton$parent)
-1.790153e-13

# mch = mean of galton childrens heights 
# mph mean of galton parents heights
#ic = intercept
#slope = slope of regression line
# mch = ic + slope * mph
sqe(ols.slope+sl,ols.intercept+ic) == deviance(fit) + sum(est(sl,ic)ˆ2 )
sqe(ols.slope+sl,ols.intercept+ic) == sqe(ols.slope, ols.intercept) + sum(est(sl,ic)ˆ2 )

#extract the intercept from fit$coef and put it in a variable called ols.ic
ols.ic <- fit$coef[1]

#slope from fit$coef and put it in the variable ols.slope
ols.slope <- fit$coef[2]

generate left and right sides of this equaltion


#Here are the vectors of variations or tweaks
sltweak <- c(.01, .02, .03, -.01, -.02, -.03) #one for the slope
ictweak <- c(.1, .2, .3, -.1, -.2, -.3)  #one for the intercept
lhs <- numeric()
rhs <- numeric()
#left side of eqn is the sum of squares of residuals of the tweaked regression line
for (n in 1:6) lhs[n] <- sqe(ols.slope+sltweak[n],ols.ic+ictweak[n])
#right side of eqn is the sum of squares of original residuals + sum of squares of two tweaks
for (n in 1:6) rhs[n] <- sqe(ols.slope,ols.ic) + sum(est(sltweak[n],ictweak[n])^2)


#Subtract the right side, the vector rhs, from the left, the vector lhs, to see
#the relationship between them
lhs-rhs
[1]  1.264198e-09  2.527486e-09  3.801688e-09 -1.261469e-09 -2.522938e-09
[6] -3.767127e-09

#You could also use the R function all.equal with lhs and rhs as arguments to
#test for equality. Try it now.
> all.equal(lhs, rhs)
[1] TRUE

#Now we'll show that the variance in the children's heights is the sum of the
#variance in the OLS estimates and the variance in the OLS residuals. First use
#the R function var to calculate the variance in the children's heights and
#store it in the variable varChild.

varChild <- var(galton$child)

#Use the R function var to calculate the variance in these
#residuals now and store it in the variable varRes.

varRes <- var(fit$residuals)

#Recall that the function "est" calculates the estimates (y-coordinates) of
#values along the regression line defined by the variables "ols.slope" and
#ols.ic". Compute the variance in the estimates and store it in the variable
#varEst.

varEst <- var(est(ols.slope, ols.ic))

#compare varChild and the Sum and VarRes
all.equal(varChild, varRes + varEst)


# Lesson 3 - Least Squares
#changing the slope of a regression line and how it affects the mean squared error between actual and predicted values
myPlot <- function(beta){
  y <- galton$child - mean(galton$child)
  x <- galton$parent - mean(galton$parent)
  freqData <- as.data.frame(table(x, y))
  names(freqData) <- c("child", "parent", "freq")
  plot(
    as.numeric(as.vector(freqData$parent)), 
    as.numeric(as.vector(freqData$child)),
    pch = 21, col = "black", bg = "lightblue",
    cex = .15 * freqData$freq, 
    xlab = "parent", 
    ylab = "child"
  )
  abline(0, beta, lwd = 3)
  points(0, 0, cex = 2, pch = 19)
  mse <- mean( (y - beta * x)^2 )
  title(paste("beta = ", beta, "mse = ", round(mse, 3)))
}
manipulate(myPlot(beta), beta = manipulate::slider(0.4, .8, step = 0.02))

#correlation between these normalized data sets
cor(gpa_nor,gch_nor)

#linear mold of it
l_nor <- lm(gch_nor ~ gpa_nor)

#original regression line, children as outcome, parents as predictor
abline(mean(y) - mean(x) * cor(y, x) * sd(y) / sd(x), #intercept
       sd(y) / sd(x) * cor(y, x),  #slope
       lwd = 3, col = "red")

#new regression line, parents as outcome, children as predictor
abline(mean(y) - mean(x) * sd(y) / sd(x) / cor(y, x), #intercept
       sd(y) / cor(y, x) / sd(x), #slope
       lwd = 3, col = "blue")

#assume correlation is 1 so slope is ratio of std deviations
abline(mean(y) - mean(x) * sd(y) / sd(x), #intercept
       sd(y) / sd(x),  #slope
       lwd = 2)
points(mean(x), mean(y), cex = 2, pch = 19) #big point of intersection



### first measurement if x were normalized to have a mean 0 and variance 1
(x - mean(x))/sd(x))[1]

###both the predictor and response have mean 0 what can be said when fit a linear regression
#it must be identically 0



#Swirl Residual variation

# fit linear model
fit <- lm(child ~ parent, galton)

#calculate sum of squared residuals divided by the quantity (n-2), then take the square root
sqrt(sum(fit$residuals^2) / (n - 2))

#look at sigma of fit
summary(fit)$sigma

#square root of deviance(fit)/(n-2)
sqrt(deviance(fit)/(n-2))
#Total Variation = Residual Variation + Regression Variation
#find the mean in children
mu <- mean(children)

# Total Variation of the data - sum of the squares of the centered childrens heights 
sTot <- sum((galton$child - mu)^2)

#sum of the squares of the residuals
sRes <- deviance(fit)

#Total Variation of the residuals
1- sRes/sTot

#same as 
summary(fit)$r.squared

#compute the square of the correlation of the galton data
cor(galton$parent, galton$child)^2

| We'll now summarize useful facts about R^2. It is the percentage of variation explained
| by the regression model. As a percentage it is between 0 and 1. It also equals the
| sample correlation squared. However, R^2 doesn't tell the whole story





