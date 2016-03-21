source("./clean.R")  
library(e1071)

### static svm outsample estimate
autoSVMR<-function(lag_num,kernel,insampletest=TRUE){
  lagDD<-lagx("DD",lag_num,insample = TRUE)
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

### rolling svm outsample estimate(windows<=7)
rollSVMR<-function(lag_num,kernel,roll_num,insampletest=TRUE){
  if(insampletest) autoSVMR(lag_num,kernel,insampletest=TRUE)
  else {
    lagDD<-lagx("DD",lag_num,insample = TRUE)
    lagDD.test<-lagx("DD",lag_num,insample=FALSE)
    lagDD.all <- rbind(lagDD,lagDD.test)
    lagDD.all<-.xts(x = lagDD.all,index = as.Date(rownames(lagDD.all),format="%Y-%m-%d")) 
    
    lagDD<-.xts(x = lagDD,index = as.Date(rownames(lagDD),format="%Y-%m-%d")) 
    lagDD.test<-.xts(x = lagDD.test,index = as.Date(rownames(lagDD.test),format="%Y-%m-%d")) 
    new<-c()
    
    for (i in 1:length(index(lagDD.test))) {
      #trainnow <-  lagDD.all[1:(i+length(index(lagDD))-1),]
      trainnow <-  lagDD.all[(i+length(index(lagDD))-roll_num):(i+length(index(lagDD))-1),]
      m<-svm(DD~.,data =trainnow ,type="eps-regression"
             ,kernel=kernel,scale=FALSE)  
      p<-predict(m,lagDD.test[i,-1])
      new<-c(new,p)
    }
    #return(new)
    new <-.xts(new,index = as.Date(rownames(lagDD.test),format="%Y-%m-%d"))
    
    lim<-c(min(c(as.vector(new),as.vector(lagDD.test[,1]))),max(c(as.vector(new),as.vector(lagDD.test[,1]))))
    plot(lagDD.test[,1],main="DD and out_sample estimate",type ="l",ylim=lim)
    lines(new,type = "b") 
    print(paste(kernel,"rolling svm outsample","the S.E is",sum((new-lagDD.test[,1])^2)),sep="")
  }
  return(new)
}

rollSVMR(lag_num = 5,"radial",insampletest = FALSE,roll_num = 7)  
rollSVMR(lag_num = 5,"polynomial",insampletest = FALSE,roll_num = 7)  


