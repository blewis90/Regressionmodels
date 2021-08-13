library(UsingR)
data(diamond)
library(ggplot2)
g = ggplot(diamond, aes(x = carat, y = price),)
g = g + xlab("Mass (carats)")
g = g + ylab("price (SIN $)")
g = g+ geom_point(size = 6, colour = "black", alpha =0.2)
g = g + geom_smooth(method = "lm", colour = "black")
g

fit <- lm(price ~ carat, data = diamond)
fit2 <- lm(price ~ I(carat - mean(carat)), data = diamond)
fit3 <- lm(price ~ I(carat * 10), data = diamond)
newx <- c(0.16, 0.27, 0.34)
coef(fit)[1] + coef(fit)[2] * newx
predict(fit, newdata = data.frame(carat = newx))

####

data(diamond)
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y~x)
e <- resid(fit)
yhat <- predict(fit)
max(abs(e -(y - yhat)))
max(abs(e - (y - coef(fit)[1] - coef(fit)[2] * x)))

###
plot(diamond$carat, diamond$price,)
> plot(diamond$carat, diamond$price,  
       xlab = "Mass (carats)",
       +     ylab = "Price (SIN $)",
       +     bg = "lightblue",
       +     col = "black", cex = 1.1, pch = 21, frame = FALSE)

abline(fit, lwd = 2)
for (i in 1 : n)
    lines(c(x[i], x[i]), c(y[i], yhat[i]), col = "red", lwd = 2)

##### Plot Residuals
plot(x, e,  
       xlab = "Mass (carats)",
       ylab = "Residuals (SIN $)",
       bg = "lightblue",
       col = "black", cex = 2, pch = 21, frame = FALSE)

> abline(h = 0, lwd = 2)
> for (i in 1 : n)
  +     lines(c(x[i], x[i]), c(e[i], 0), col = "red", lwd = 2)
