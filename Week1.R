install.packages("UsingR")

library(UsingR); data(galton); library(reshape);
long <- melt(galton)
g <- ggplot(long, aes(x = value, fill= variable))
g <- g + geom_histogram(colour="black", binwidth=1)
g <- g + facet_grid(.~ variable)
g

library(manipulate)
myHist <- function(mu){
  mse <- mean((galton$child - mu)^2)
  g <- ggplot(galton, aes(x = child)) + geom_histogram(fill = "salmon", colour = "black", binwidth =1)
  g <- g + ggtitle(paste("mu = ",mu,",MSE =", round(mse, 2), sep = ""))
  g
}
manipulate(myHist(mu), mu= slider(62, 74, step = .05))

#new vid - intro data example
ggplot(galton, aes(x=parent, y=child)) + geom_point()

#look up mse for linear regression

#look up r markdown for this video

lm(I(child - mean(child))~ I(parent - mean(parent)) - 1, data = galton)

#coefficients:
I(parent- mean(parent))

# new vid - basic notation and background
# if you subtract the mean from data points, you get data that has mean 0. this is called centering
#scaling - divide by standard deviation, will have standard deviation of 1. this is normalizing the data
#empirical covariance and correlation

#new vid least squares estimation of regression lines
#minimize sum of the squares 

#new vid linear least squares coding example
y <- galton$child
x <- galton$parent
beta1 <- cor(y,x) * sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
rbind(c(beta0, beta1), coef(lm(y ~ x)))



