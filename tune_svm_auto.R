source("clean.R")
library(e1071)
library(caret)
library(pROC)#for classification
pkgs <- c('foreach', 'doParallel')
lapply(pkgs, require, character.only = T)
registerDoParallel(cores = 4)

lagDD<-lagx("DD",lag=5 ,insample = TRUE)

## OPTIM
tuneSVMR<-function(x){
  gamma<-x[1];cost<-x[2]
  m<-svm(DD~.,data =lagDD ,type="eps-regression"
         ,kernel="radial",scale=FALSE, cost=cost,gamma=gamma)  
    new <- predict(m, lagDD[,-1])
    sum((new-lagDD[,1])^2)/length(new)
}

optim(c(0.2,1),tuneSVMR,lower = 0)

### FOREACH
cost <- seq(0.1,5,by=0.1)
gamma <- seq(0.1,5,by=0.1)
parms <- expand.grid(cost = cost, gamma = gamma)
result <- foreach(i = 1:nrow(parms), .combine = rbind) %dopar% {
  c <- parms[i, ]$cost
  g <- parms[i, ]$gamma
  mse<-tuneSVMR(c(g,c))
  data.frame(parms[i, ], MSE = mse)
}

head(arrange(result,MSE))



