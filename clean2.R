dji<-read.table(file = "dow_jones_index/dow_jones_index.data",sep = ",",header = TRUE
                ,colClasses = c("factor","factor","character","character","character"
                                ,"character","character","numeric","numeric","numeric","numeric"
                                ,"character","character","numeric","numeric","numeric"))
head(dji)
suppressMessages(library(dplyr))
suppressMessages(library(quantmod))
dji<-mutate(dji,date = as.Date(dji$date,format= "%m/%d/%Y"))  
price_loc <- c(4:7,12:13)

s<-sapply(dji[price_loc], function(x){ strip<-sub("^\\$",replacement = "",x = x)
return(as.numeric(strip))})
dji[price_loc] <- s

### split the stock  
quarter12<-split(dji,f = dji$quarter)
train<-quarter12$`1`;test<-quarter12$`2`
stocks.train<-split(train,f = train$stock)
stocks.test<-split(test,f = test$stock)

cl<- as.data.frame(lapply(stocks.train,function(x) x$close))#closing price
cl<-.xts(cl,index = stocks.train$DD$date)

cl.test<- as.data.frame(lapply(stocks.test,function(x) x$close))#closing price
cl.test<-.xts(cl.test,index = stocks.test$DD$date)
###choose highly related stocks
highcorr<-(cor(cl)>0.9 & cor(cl)<1 )
which.max(rowSums(highcorr))
print(paste("the highly correlated stocks associated with DD are"
            ,paste(colnames(highcorr)[highcorr[which.max(rowSums(highcorr)),]]
                   ,collapse =" ")))
print(cor(cl)[8,highcorr[which.max(rowSums(highcorr)),]])




plot(cl$DD,lty=1,ylim = c(30,130),
     main= "the highly correlated stocks in train_period")
lines(cl$CAT, lty = 2,lwd=2)
lines(cl$DIS,lty=3,lwd=2)
lines(cl$TRV, lty=4,lwd=2)  
legend("topleft",c("DD","CAT","DIS","TRV")
       ,lty = c(1,2,3,4)
       ,horiz = T)

plot(cl.test$DD,lty=1,ylim = c(30,130),
     main= "the highly correlated stocks in test_period")
lines(cl.test$CAT, lty = 2,lwd=2)
lines(cl.test$DIS,lty=3,lwd=2)
lines(cl.test$TRV, lty=4,lwd=2)  
legend("topleft",c("DD","CAT","DIS","TRV")
       ,lty = c(1,2,3,4)
       ,horiz = T)

### create lagged inputs  (lag() in quantmod)
lagx<-function(stock,lag,insample=TRUE){
  x <- eval(expr = parse(text = paste("cl$",stock,sep = "")))
  if(insample){
    org.len<-length(x)
    lagxs<-lapply(c(0,seq(lag)), function(i) lag(x,i))
    as.data.frame(lagxs)[-c(1:lag),]
  }else{
    x.t <- eval(expr = parse(text = paste("cl.test$",stock,sep = "")))
    x <- rbind(tail(x,lag),x.t)
    org.len<-length(x)
    lagxs<-lapply(c(0,seq(lag)), function(i)lag(x,i))
    as.data.frame(lagxs)[-c(1:lag),]
  }
}

lagx("DD",3,insample = FALSE)
