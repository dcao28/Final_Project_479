source("clean.R")
library(e1071)
clDD<-rbind(cl$DD,cl.test$DD)
dif<-diff(clDD,1);dif[1]<-0
classDD<-as.factor(ifelse(dif >0,1,-1))#1:up;-1:down
names(classDD)<-c(rep("train",12),rep("test",13))

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

### static svm outsample estimate
mixSVMR<-function(lag_num,kernel,insampletest=TRUE,corlags){
  if(corlags <=lag_num) {
    lagX<-lapply(HighCor, function(x,lag_num,insample) {lagx2(x,lag_num,insample)} 
                 , lag_num=corlags,insample = TRUE)
    lagX <- as.data.frame(lagX)
  }else{
    stop("larger corlags than lag_num")
  }
  
  lagDD<-lagx("DD",lag_num,insample = TRUE)
  lagDD <- mutate(lagDD,DD= classDD[(lag_num+1):12])
  lagDD <- cbind(lagDD,head(lagX,dim(lagDD)[1]))##combine highly correlated stocks
  m<-svm(DD~.,data =lagDD ,kernel=kernel,scale=TRUE)  
  
  ## whether insampletest
  if(insampletest){
    new <- predict(m, lagDD[,-1])
    print(paste(kernel,"static svm insample","the P.E. raio is"
                ,sum(new != classDD[(lag_num+1):12])/length(new),sep=" "))
  }else{
    #TEST data
    lagX<-lapply(HighCor, function(x,lag_num,insample) {lagx2(x,lag_num,insample)} 
                 , lag_num=corlags,insample = FALSE)
    lagX <- as.data.frame(lagX)
    lagDD<-lagx("DD",lag_num,insample=FALSE)
    lagDD <- mutate(lagDD,DD= tail(classDD,dim(lagDD)[1]))
    lagDD <- cbind(lagDD,head(lagX,dim(lagDD)[1]))##combine highly correlated stocks
    new <- predict(m,lagDD[,-1])
    print(paste(kernel,"static svm outsample","the P.E. raio is"
                ,sum(new != tail(classDD,dim(lagDD)[1]))/length(new),sep=" "))
  }
  return(new)
}


lag_num=5
mixSVMR(lag_num,kernel = "radial",insampletest = FALSE,corlags=4)
mixSVMR(lag_num,kernel = "polynomial",insampletest = FALSE,corlags=4)  




### mixed rolling svm outsample estimate(windows<=7)
mixRollSVMR<-function(lag_num,kernel,roll_num,insampletest=TRUE,corlags){
  if(insampletest) mixSVMR(lag_num,kernel,insampletest=TRUE,corlags)
  else {
    lagX<-lapply(HighCor, function(x,lag_num,insample) {lagx2(x,lag_num,insample)} 
                 , lag_num=corlags,insample = TRUE)
    lagX <- as.data.frame(lagX)
    lagX.test<-lapply(HighCor, function(x,lag_num,insample) {lagx2(x,lag_num,insample)} 
                      , lag_num=corlags,insample = FALSE)
    lagX.test <- as.data.frame(lagX.test)
    lagX.all <- rbind(lagX,lagX.test)
    
    lagDD<-lagx("DD",lag_num,insample = TRUE)
    lagDD <- mutate(lagDD,DD= classDD[(lag_num+1):12])
    lagDD.test<-lagx("DD",lag_num,insample=FALSE)
    lagDD.test <- mutate(lagDD.test,DD= tail(classDD,dim(lagDD.test)[1]))
    lagDD.all <- rbind(lagDD,lagDD.test)
    
    lagDD.all <- cbind(lagDD.all,head(lagX.all,dim(lagDD.all)[1]))##combine highly correlated stocks
    new<-c()
    
    for (i in 1:length(index(lagDD.test))) {
      #trainnow <-  lagDD.all[1:(i+length(index(lagDD))-1),]
      trainnow  <-  lagDD.all[(i+length(index(lagDD))-roll_num):(i+length(index(lagDD))-1),]
      m <- svm(DD~.,data =trainnow ,kernel=kernel,scale=TRUE)  
      p<-predict(m,lagDD.all[i+length(index(lagDD)),-1])
      p<-as.character(p)
      new <-c(new,p)
    }
    new <- as.factor(as.numeric(new))
    print(paste(kernel,"rolling svm outsample","the P.E. raio is"
                ,sum(new != tail(classDD,length(new)))/length(new),sep=" "))
  }
  return(new)
}

mixRollSVMR(lag_num = 5,"radial",insampletest = FALSE,roll_num = 8,corlags=3)  
mixRollSVMR(lag_num = 5,"polynomial",insampletest = FALSE,roll_num = 8,corlags=3)  



