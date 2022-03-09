library(devtools)
library(regtools)
library(rectools)
library(qeML)
data<-mlbd
yName<-"Weight"
colname(data_w[,1])
summary(qeout)
lmAlpha<-function(data, yName, nReplic, holdout){

  qeout<-qeLin(data,yName)
  names <- c(1:(ncol(data)-1))
  psum <- summary(qeout)$coefficients[,4]
  psum <- psum[-1]
  allp <-data.frame(feature = names, p = psum)
  orderp <- allp[order(allp$p),]
  data_w <- data[,which(colnames(data)!=yName)]
  finalresult <- c() 
  for (i in 2:(nrow(orderp)+1)){
    answer <- c(1:nReplic)
    for (times in 1:nReplic){
      new <- data.frame(data[,which(colnames(data)==yName)])
      for (j in 1:(i-1)){
        new <- cbind(new,data_w[,orderp[j,1]])
      }
      count <- ncol(new)
      colnames(new)<-c(yName,1:(count-1))
      answer[times]<-qeLin(new,yName,holdout = holdout)$testAcc
    }
    vars <- orderp[1:(count-1),1]
    vars <- colnames(data_w)[vars]
    
    testAcc<-mean(answer)
    finalresult <- c(finalresult,newclass(vars,testAcc))
  }
  
  return(finalresult)
}
#class
newclass <- function(x,y){
  finalresult <- structure(list(vars= x, testAcc = y), class="stderr")
  class(finalresult)<-"1"
  return(finalresult)
}




data(pef)
head(pef)
pefd <- factorsToDummies(pef,omitLast=T)
pefd <- as.data.frame(pefd)
result <- lmAlpha(pefd,"wageinc",10,250)
result
result <- lmAlpha(pefd,"wageinc",10,500)
result


data(mlb)
head(mlb)
#mlb, Position, Height, Weight, Age.
mlb <- mlb[,3:6]
mlbd <- factorsToDummies(mlb,omitLast=T)
mlbd <- as.data.frame(mlbd)
result <- lmAlpha(mlbd,'Weight',50,250)
result
