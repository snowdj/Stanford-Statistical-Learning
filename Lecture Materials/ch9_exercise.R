library(e1071)
library(MASS)

mu0 = rep(0,10)
mu1 = c(1,1,1,1,1,0,0,0,0,0)
numTrain = 50
numTest = 10000
numTrials = 1000
testError = numeric(numTrials)


for (i in 1:numTrials) {
     set.seed(i)
     x0_train = mvrnorm(n=numTrain, mu=mu0, Sigma=diag(10))
     x1_train = mvrnorm(n=numTrain, mu=mu1, Sigma=diag(10))
     y0_train=rep(-1,numTrain)
     y1_train=rep(1,numTrain)
     x_train=rbind(x0_train,x1_train)
     y_train=c(y0_train,y1_train)
     
     x0_test = mvrnorm(n=numTest, mu=mu0, Sigma=diag(10))
     x1_test = mvrnorm(n=numTest, mu=mu1, Sigma=diag(10))
     y0_test=rep(-1,numTest)
     y1_test=rep(1,numTest)
     x_test=rbind(x0_test,x1_test)
     y_test=c(y0_test,y1_test)

     # convert y's to [0,1] for logistic regression
     y_train[y_train==-1] = 0    
     y_test[y_test==-1] = 0
     
     trainDat = data.frame(x_train, y = as.factor(y_train))
     testDat = data.frame(x_test, y = as.factor(y_test))
     
#      modelfit = svm(y~., data=trainDat, kernel="linear")
#      testPred = predict(modelfit, newdata = testDat)

     modelfit = glm(y~., data=trainDat, family=binomial)
     testPred = predict(modelfit, newdata = testDat, type="response")
     
     testPred = as.numeric(testPred >= 0.5)
     error = as.numeric(testDat$y != testPred)
#     error = as.numeric(testDat$y != sign(as.numeric(as.character(testPred))))
     testError[i] = sum(error)/length(testDat$y)
         
} 
expectedTestError = mean(testError)


