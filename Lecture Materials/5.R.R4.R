require(boot)
load("5.R.RData")

bootfun = function(dat,index){
     fitski = lm(y~., data=dat[index,])
     out = summary(fitski)
     return(out$coefficients[2,1])
}

new.rows = c(101:200, 401:500, 101:200, 901:1000, 301:400, 1:100, 1:100, 801:900, 201:300, 701:800)
new.Xy = Xy[new.rows, ]

# block bootstrap
tsboot.out = tsboot(Xy, bootfun, R=1000, l=100, sim="fixed")