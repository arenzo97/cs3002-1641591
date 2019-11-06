#install.packages("rpart")
#library(rpart)
library(class)

winedata = read.csv('winedata.csv', sep=",")

wineclass = winedata[,1]
winevalues = winedata[,-1]

#set training set
wineclassTrain = wineclass[1:100]
winevaluesTrain = winevalues[1:100,]

#set testset
wineclassTest = wineclass[100:178]
winevaluesTest = winevalues[100:178,]

#DecisionTree
a<-KNN

#Decision trees
DecisionTree<-function()
{
  fit <- rpart(wineclassTrain~., method = "class", data=winevaluesTrain)
  plot(fit, uniform=TRUE, main="Decision Tree for WineData")
  text(fit, use.n=TRUE, all=TRUE, cex=.8)
  
  #calculate predictions for testcases
  treepred<-predict(fit,winevaluesTest,type='class')
  
  #return accuracy
  AccuracyFunc(length(wineclassTest),sum(treepred==wineclassTest))
  
  #confusion matrix results
  table_mat=table(wineclassTest,treepred)
  print(table_mat)
  
  
  #pruning
  pfit<-prune(fit,cp=0.1)
  plot(pfit,uniform=TRUE,main="Pruned Decision Tree for WineData")
  text(pfit,use.n=TRUE, all=TRUE,cex=.8)
}

#KNN
KNN<-function()
{
  knn3pred = knn(winevaluesTrain, winevaluesTest, wineclassTrain, k=3)
  
  #calculate accuracy
  n=length(wineclassTest)
  ncorrect=sum(knn3pred==wineclassTest)
  AccuracyFunc(ncorrect,n)
  
}


AccuracyFunc<-function(ncorrect,n)
{
  accuracy=ncorrect/n
  print(accuracy)
}