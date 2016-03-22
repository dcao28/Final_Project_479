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
print(paste("the highly correlated stocks are"
            ,paste(colnames(highcorr)[highcorr[which.max(rowSums(highcorr)),]]
                   ,collapse =" ")))



plot(cl$DD,type = "l",ylim = c(30,130))
lines(cl$CAT)
lines(cl$DIS)
lines(cl$TRV)

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
