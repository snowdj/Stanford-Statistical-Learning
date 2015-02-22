load("7.R.RData")
plot(x,y)
fit<-lm(y~x)
fit2 <- lm(y ~ x + I(x^2))
