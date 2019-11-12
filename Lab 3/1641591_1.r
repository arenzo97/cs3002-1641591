#install.packages("rpart")
#library(rpart)
library(class)

#Set Directory to this current directory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

#set data file, class and values
iris = read.csv('iris.csv', sep=",")
irisClass = iris[,1]
irisValues = iris[,-1]
iris_rand=iris[sample(150,150),]

TrainingSet<-function(idataClass,idataValues,setValues)
{
    idataClassTrain = idataClass[setValues]
    idataValuesTrain = idataValues[setValues]
}

TestSet<-function(idataClass,idataValues,setValues)
{
    idataClassTest = idataClass[setValues]
    idataValuesTest= idataValues[setValues]
}


#DecisionTree
KNN<-function(valuesTrain,valuesTest,classTrain,kvalue)
{
    knn3pred = knn(valuesTrain,valuesTest,classTrain,kvalue)
    return(knn3pred)
}

#Decision trees
DecisionTree<-function(trainSet,methodType,dataset)
{
    fit <- rpart(trainSet~., method = methodType, data=dataset)
    plot(fit, uniform=TRUE, main="Decision Tree for",trainSet)
    text(fit, use.n=TRUE, all=TRUE, cex=.8)

    return(fit)
}

Prune<-function(fit,pruneValue)
{
    #pruning
    pfit<-prune(fit,cp=pruneValue)
    plot(pfit,uniform=TRUE,main="Pruned Decision Tree for WineData")
    text(pfit,use.n=TRUE, all=TRUE,cex=.8)
}
#calculate accuracy
Accuracy<-function(knnGenValues,testValues)
{
    
    n=length(testValues)
    ncorrect=sum(knnTestedValues==testValues)
    result = ncorrect/n
    
    print(result)
    return(result)

}

#1
for(i in 1:3 )
{
    irisTrain[i]=TrainingSet
    dTree[i]=DecisionTree(asd,"class",iris_rand)
}