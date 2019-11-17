#install.packages("rpart")
library(rpart)
library(class)

#Set Directory to this current directory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

#set data file, class and values
iris = read.csv('iris.csv', sep=",")


TrainingSet<-function(idataClass,idataValues)
{
    idataClassTrain = idataClass[1:100]
    idataValuesTrain = idataValues[1:100,]
    result <- list(classTrain = idataClassTrain, classValues = idataClassTrain)
    return(result)
}

TestSet<-function(idataClass,idataValues,setValues)
{
    idataClassTest = idataClass[setValues]
    idataValuesTest= idataValues[setValues,]
}


#KNN Function
KNN<-function(valuesTrain,valuesTest,classTrain,kvalue)
{
    knn3pred = knn(valuesTrain,valuesTest,classTrain,kvalue)
    return(knn3pred)
}

#Decision trees
DecisionTree<-function(trainSet,methodType,dataset)
{
    
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


iris_rand=iris[sample(150,150),]
irisClass = iris_rand[,1]
irisValues = iris_rand[,-1]


irisTrain1=TrainingSet(irisClass,irisValues)
irisClassTrain = irisTrain1[1]
# irisTrain1fit <- rpart(irisClassTrain~., method = "class", data=irisTrain1[2])
