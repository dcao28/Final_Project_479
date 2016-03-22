source("clean.R")
library(e1071)
library(caret)
library(pROC)#for classification


#the tune is based on the insample data
pkgs <- c('foreach', 'doParallel')
lapply(pkgs, require, character.only = T)
registerDoParallel(cores = 4)
lagDD<-lagx("DD",lag=5 ,insample = TRUE)
lagDD<-.xts(x = lagDD,index = as.Date(rownames(lagDD),format="%Y-%m-%d")) 
### SPLIT DATA INTO K FOLDS ###
set.seed(2016)
lagDD$fold <- caret::createFolds(1:nrow(lagDD), k = 4, list = FALSE)

### PARAMETER LIST ###
cost <- c(10, 100)
gamma <- c(1, 2)
parms <- expand.grid(cost = cost, gamma = gamma)
### LOOP THROUGH PARAMETER VALUES ###
result <- foreach(i = 1:nrow(parms), .combine = rbind) %do% {
  i=1
  c <- parms[i, ]$cost
  g <- parms[i, ]$gamma
  ### K-FOLD VALIDATION ###
  out <- foreach(j = 1:max(lagDD$fold), .combine = rbind, .inorder = FALSE) %dopar% {
    j=1
    deve <- lagDD[lagDD$fold != j, -dim(lagDD)[2]]
    test <- lagDD[lagDD$fold == j, -dim(lagDD)[2]]
    mdl <- e1071::svm(DD~., data = deve, kernel = "radial"
                      , cost = c, gamma = g, probability = TRUE)
    pred <- predict(mdl, test[,-1], decision.values = TRUE, probability = TRUE)
    data.frame(y = test$DD, prob = attributes(pred)$probabilities[, 2])
  }
  ### CALCULATE SVM PERFORMANCE ###
  roc <- pROC::roc(as.factor(out$y), out$prob) ##ROC: only suitable for binary classification problems
  data.frame(parms[i, ], roc = roc$auc[1])
}