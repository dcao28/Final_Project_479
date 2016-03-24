source("clean.R")
library(e1071)
library(caret)
library(pROC)#for classification
pkgs <- c('foreach', 'doParallel')
lapply(pkgs, require, character.only = T)
registerDoParallel(cores = 4)

HighCor<-colnames(highcorr)[highcorr[which.max(rowSums(highcorr)),]]
lagx2 <- function(stock,lag,insample=TRUE){
  x <- eval(expr = parse(text = paste("cl$",stock,sep = "")))
  if(insample){
    org.len<-length(x)
    lagxs<-lapply(c(0,seq(lag)), function(i) lag(x,i))
    as.data.frame(lagxs)[-c(1:lag),-1]
  }else{
    x.t <- eval(expr = parse(text = paste("cl.test$",stock,sep = "")))
    x <- rbind(tail(x,lag),x.t)
    org.len<-length(x)
    lagxs<-lapply(c(0,seq(lag)), function(i)lag(x,i))
    as.data.frame(lagxs)[-c(1:lag),-1]
  }
}
corlags = 3
lag_num = 7
kernel="radial"
lagX<-lapply(HighCor, function(x,lag_num,insample) {lagx2(x,lag_num,insample)} 
             , lag_num=corlags,insample = TRUE)
lagX <- as.data.frame(lagX)
lagDD<-lagx("DD",lag_num,insample = TRUE)
lagDD <- cbind(lagDD,head(lagX,dim(lagDD)[1]))##combine highly correlated stocks
## OPTIM
tuneSVMR<-function(x){
  gamma<-x[1];cost<-x[2]
  m<-svm(DD~.,data =lagDD ,type="eps-regression"
         ,kernel=kernel,scale=FALSE
         , cost=cost,gamma=gamma)  
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

print(arrange(result,MSE)[1:10,])



