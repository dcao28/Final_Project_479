source("clean.R")
clDD<-rbind(cl$DD,cl.test$DD)
as.factor(ifelse((cl$DD-Lag(cl$DD,1))>0,1,-1))#1:up;-1:down



autoSVMR<-function(lag_num,kernel,insampletest=TRUE){
  lagDD<-lagx("DD",lag_num,insample = TRUE)
  lagDD <- mutate(lagDD,DD=ifelse((DD-Lag(DD,1))>0,1,-1))
  lagDD<-.xts(x = lagDD,index = as.Date(rownames(lagDD),format="%Y-%m-%d"))  
  
  m<-svm(DD~.,data =lagDD ,type="eps-regression"
         ,kernel=kernel,scale=FALSE)  
  
  if(insampletest){
    new <- predict(m, lagDD[,-1])
    new <-.xts(new,index = as.Date(rownames(lagDD),format="%Y-%m-%d"))
    
    plot(lagDD[,1],ylim=c(45,60),main="DD and in_sample estimate")
    lines(new,type = "b") 
    print(paste(kernel,"static svm insample","the S.E is",sum((new-lagDD[,1])^2),sep = " "))
  }else{
    
    lagDD<-lagx("DD",lag_num,insample=FALSE)
    lagDD<-.xts(x = lagDD,index = as.Date(rownames(lagDD),format="%Y-%m-%d"))  
    
    new <- predict(m,lagDD[,-1])
    new <-.xts(new,index = as.Date(rownames(lagDD),format="%Y-%m-%d"))
    
    lim<-c(min(c(as.vector(new),as.vector(lagDD[,1]))),max(c(as.vector(new),as.vector(lagDD[,1]))))
    plot(lagDD[,1],main="DD and out_sample estimate",type ="l",ylim=lim)
    lines(new,type = "b") 
    print(paste(kernel,"static svm outsample","the S.E is",sum((new-lagDD[,1])^2),sep=" "))
  }
  return(new)
}

lag_num=5
autoSVMR(lag_num,"radial",insampletest = FALSE)
autoSVMR(lag_num,"polynomial",insampletest = FALSE)