#install.packages("rpart")
#library(rpart)
library(class)

#Set Directory to this current directory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

#set data file, class and values
iris = read.csv('iris.csv', sep=",")
irisClass = iris[,1]
irisValues = iris[.-1]

TrainingSet<-function(idataClass,idataValues,setValues)
{
    idataClassTrain = idataClass[1:setvalues]
    idataValuesTrain = idataValues[1:setvalues]
}

#DecisionTree
KNN

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
}

Prune<-function(fit,pruneValue)
{
    #pruning
    pfit<-prune(fit,cp=pruneValue)
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