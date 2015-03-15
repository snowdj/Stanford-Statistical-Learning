load("10.R.RData")

# Concatenate x and s.test and do pca
xCat <- rbind(x, x.test)

pca.x <- prcomp(xCat, scale=TRUE)
summary(pca.x)

# percentage variance explained from first 5 PC found in summary
vars = pca.x$sdev^2
PVE = sum(vars[1:5])/sum(vars)

# use first five principal components for regression
pcScores <- pca.x$rotation[,1:5]
pcs.train <- as.matrix(x) %*% pcScores
pcs.test <- as.matrix(x.test) %*% pcScores
trainData <- data.frame(xvals = pcs.train, y = y)
testData <- data.frame(xvals = pcs.test, y = y.test)

fit <- lm(y ~ ., data=trainData)
testPred <- predict(fit, newdata=testData)

# Calculate MSE of fit
require(Metrics)
print(mse(y.test,testPred))


# conduct OLS
OLStrain <- data.frame(x,y=y)
OLStest <- data.frame(x.test, y=y.test)
fitOLS <- lm(y ~ ., data = OLStrain)
predOLS <- predict(fitOLS, newdata = OLStest)
print(mse(y.test, predOLS))