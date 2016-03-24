source("clean.R")
library(e1071)
clDD<-rbind(cl$DD,cl.test$DD)
dif<-diff(clDD,1);dif[1]<-0
classDD<-as.factor(ifelse(dif >0,1,-1))#1:up;-1:down
names(classDD)<-c(rep("train",12),rep("test",13))

autoSVMR<-function(lag_num,kernel,insampletest=TRUE){
  lagDD<-lagx("DD",lag_num,insample = TRUE)
  lagDD <- mutate(lagDD,DD= classDD[(lag_num+1):12])
  m<-svm(DD~.,data =lagDD
           ,kernel=kernel,scale=TRUE) 
  
  if(insampletest){
    new <- predict(m, lagDD[,-1])
    print(paste(kernel,"static svm insample","the P.E. raio is"
                ,sum(new != classDD[(lag_num+1):12])/length(new),sep=" "))
  }else{
    #TEST
    lagDD<-lagx("DD",lag_num,insample=FALSE)
    lagDD <- mutate(lagDD,DD= tail(classDD,dim(lagDD)[1]))
    new <- predict(m,lagDD[,-1])
    print(paste(kernel,"static svm outsample","the P.E. raio is"
                ,sum(new != tail(classDD,dim(lagDD)[1]))/length(new),sep=" "))
  }
  return(new)
}

lag_num=6
autoSVMR(lag_num,"radial",insampletest = TRUE)
autoSVMR(lag_num,"polynomial",insampletest = TRUE)


### rolling svm outsample estimate(windows<=7)
rollSVMR<-function(lag_num,kernel,roll_num,insampletest=TRUE){
  
  if(insampletest) autoSVMR(lag_num,kernel,insampletest=TRUE)
  else {
    
    lagDD<-lagx("DD",lag_num,insample = TRUE)
    lagDD <- mutate(lagDD,DD= classDD[(lag_num+1):12])
    lagDD.test<-lagx("DD",lag_num,insample=FALSE)
    lagDD.test <- mutate(lagDD.test,DD= tail(classDD,dim(lagDD.test)[1]))
    lagDD.all <- rbind(lagDD,lagDD.test)
    new<-c()
    
    for (i in 1:length(index(lagDD.test))) {
      #trainnow <-  lagDD.all[1:(i+length(index(lagDD))-1),]
      trainnow <-  lagDD.all[(i+length(index(lagDD))-roll_num):(i+length(index(lagDD))-1),]
      m<-svm(DD~.,data =trainnow ,kernel=kernel,scale=TRUE)  
      p<-predict(m,lagDD.test[i,-1])
      p<-as.character(p)
      new <-c(new,p)
    }
    
    new <- as.factor(as.numeric(new))
    print(paste(kernel,"rolling svm outsample","the P.E. raio is"
                ,sum(new != tail(classDD,length(new)))/length(new),sep=" "))
  }
  return(new)
}

rollSVMR(lag_num = 5,"radial",insampletest = FALSE,roll_num = 8)  
rollSVMR(lag_num = 5,"polynomial",insampletest = FALSE,roll_num = 8)  





