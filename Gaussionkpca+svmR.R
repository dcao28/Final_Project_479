source("./clean.R")  
library(e1071)
suppressMessages(library(kernlab))


######mixed lags#######
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
  lagDD <- cbind(lagDD,head(lagX,dim(lagDD)[1]))##combine highly correlated stocks
  lagDD<-.xts(x = lagDD,index = as.Date(rownames(lagDD),format="%Y-%m-%d")) 
  kpc <- kpca(~.,data=lagDD[,-1],kernel="rbfdot",
              kpar=list(sigma=0.2),features=0)
  lagDD2 <- as.data.frame(rotated(kpc))#kpca scores
  m<-svm(lagDD$DD~.,data =lagDD2 ,type="eps-regression",kernel=kernel,scale=FALSE)  
  
  ## whether insampletest
  if(insampletest){
    new <- predict(m, lagDD2) # INSAMPLE KPCA
    new <-.xts(new,index = as.Date(rownames(lagDD),format="%Y-%m-%d"))
    
    plot(lagDD[,1],ylim=c(45,60),main="DD and in_sample estimate")
    lines(new,type = "b") 
    print(paste(kernel,"static mixed-kpca+svm insample","the MSE is",sum((new-lagDD[,1])^2)/length(new),sep=" "))
  }else{
    #TEST data
    lagX<-lapply(HighCor, function(x,lag_num,insample) {lagx2(x,lag_num,insample)} 
                 , lag_num=corlags,insample = FALSE)
    lagX <- as.data.frame(lagX)
    lagDD<-lagx("DD",lag_num,insample=FALSE)
    lagDD <- cbind(lagDD,head(lagX,dim(lagDD)[1]))##combine highly correlated stocks
    lagDD<-.xts(x = lagDD,index = as.Date(rownames(lagDD),format="%Y-%m-%d"))  
    
    lagDD2 <- predict(kpc,lagDD[,-1])# outsample KPCA
    new <- predict(m,lagDD2)
    new <-.xts(new,index = as.Date(rownames(lagDD),format="%Y-%m-%d"))
    
    lim<-c(min(c(as.vector(new),as.vector(lagDD[,1]))),max(c(as.vector(new),as.vector(lagDD[,1]))))
    plot(lagDD[,1],main="DD and out_sample estimate",type ="l",ylim=lim)
    lines(new,type = "b") 
    print(paste(kernel,"static mixed-kpca+svm outsample","the MSE is",sum((new-lagDD[,1])^2)/length(new),sep=" "))
  }
  return(new)
}


lag_num=5
mixSVMR(lag_num,"radial",insampletest = FALSE,corlags=4)
mixSVMR(lag_num,"polynomial",insampletest = FALSE,corlags=4)  




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
    lagDD.test<-lagx("DD",lag_num,insample=FALSE)
    lagDD.all <- rbind(lagDD,lagDD.test)
    
    lagDD.all <- cbind(lagDD.all,head(lagX.all,dim(lagDD.all)[1]))##combine highly correlated stocks
    lagDD.all<-.xts(x = lagDD.all,index = as.Date(rownames(lagDD.all),format="%Y-%m-%d")) 
    
    lagDD<-.xts(x = lagDD,index = as.Date(rownames(lagDD),format="%Y-%m-%d")) 
    lagDD.test<-.xts(x = lagDD.test,index = as.Date(rownames(lagDD.test),format="%Y-%m-%d")) 
    new<-c()
    
    for (i in 1:length(index(lagDD.test))) {
      #trainnow <-  lagDD.all[1:(i+length(index(lagDD))-1),]
      trainnow  <-  lagDD.all[(i+length(index(lagDD))-roll_num):(i+length(index(lagDD))-1),]
      kpc <- kpca(~.,data=trainnow[,-1],kernel="rbfdot",
                  kpar=list(sigma=0.2),features=0)
      lagDD2 <- as.data.frame(rotated(kpc))#kpca scores
      m<-svm(trainnow$DD~.,data =lagDD2 ,type="eps-regression",kernel=kernel,scale=FALSE)  
      lagDD2 <- predict(kpc,lagDD.all[i+length(index(lagDD)),-1])# outsample KPCA
      p<-predict(m,lagDD2)
      new<-c(new,p)
    }
    #return(new)
    new <-.xts(new,index = as.Date(rownames(lagDD.test),format="%Y-%m-%d"))
    
    lim<-c(min(c(as.vector(new),as.vector(lagDD.test[,1]))),max(c(as.vector(new),as.vector(lagDD.test[,1]))))
    plot(lagDD.test[,1],main="DD and out_sample estimate",type ="l",ylim=lim)
    lines(new,type = "b") 
    print(paste(kernel,"rolling mixed-kpca+svm outsample","the MSE is",sum((new-lagDD.test[,1])^2)/length(new)),sep="")
  }
  return(new)
}

mixRollSVMR(lag_num = 4,"radial",insampletest = FALSE,roll_num = 5,corlags=3)  
mixRollSVMR(lag_num = 4,"polynomial",insampletest = FALSE,roll_num = 5,corlags=3)  



