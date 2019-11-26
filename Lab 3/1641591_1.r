#install.packages("rpart")
library(rpart)
library(rpart.plot)
library(class)

#Set Directory to this current directory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

splitScreen <- function(x ,y){
    par(mfrow=c(y,x))
}


#set data file, class and values
iris = read.csv('iris.csv', head = FALSE,sep=",")
irisReal = read.csv('iris_real.csv',head = FALSE, sep=",")

# returns a list


#KNN Function
KNN<-function(valuesTrain,valuesTest,classTrain,kvalue)
{
    knn3pred = knn(valuesTrain,valuesTest,classTrain,kvalue)
    return(knn3pred)
}

#Decision trees


PlotTree <- function(fit, type, title)
{
    rpart.plot(x = fit, type = type, main = title, box.palette = list("#552586","#804FB3","#B589D6"))  
}

# Confusion Matr

# Calculate accuracy
Accuracy<-function(actualValues,testValues)
{
    
    n=length(testValues)
    ncorrect=sum(actualValues==testValues)
    result = ncorrect/n
    
    print(result)
    return(result)

}

#1 Create Decision Tree for Iris Classes
iris_rand<-cbind(irisReal,iris)
iris_rand=iris_rand[sample(149,149),]
irisClass = iris_rand[,1]
irisValues = iris_rand[,-1]

irisClassTrain = irisClass[1:100]
irisValuesTrain = irisValues[1:100,]

irisClassTest = irisClass[101:149]
irisValuesTest = irisValues[101:149,]



irisTrainfit <- rpart(irisClassTrain~., method = "class", data=irisValuesTrain)
treepred <- predict(irisTrainfit,irisValuesTest, type = 'class')
rpart.plot(x = irisTrainfit, type = 4, main = "Decision Tree for IrisData",
           box.palette = list("#325aa8","#325ee8","#325ae8"))  



#2 Test accuracy by pruning
splitScreen(2, 3)
cpvalues <- c(0.04, 0.08, 0.1,0.4,0.5)
cpLength = length(cpvalues)

prunedTreesAccuracies <- vector( mode = "numeric", length = cpLength)
prunedTreesFitValues<-list()

for (i in 1:5)
{
    prunedTree<-prune(irisTrainfit,cp=cpvalues[i])
    PlotTree(prunedTree, 4,sprintf('Decision tree with cp = %g', cpvalues[i]))
    prunedTreesAccuracies[i]=Accuracy(predict(prunedTree,irisValuesTest,type = 'class'),irisClassTest)
    
}

#3 Scatter plot 2 selected variables and colour code based on decision trees

splitScreen(3,1)
plot(x = irisValuesTrain$V1, y = irisValuesTrain$V2, col = c(irisClassTrain), main = "ValuesTrain Values of V1 vs V2")  
plot(x = irisValuesTest$V1, y = irisValuesTest$V2,
     col = c(irisClassTest), main = "Test values")
plot(x = irisValuesTest$V1, y = irisValuesTest$V2,
     col = c(treepred), main = "Predicted values")

# 4. Compare the accuracy of the different pruned trees to KNN with different values of k ####

rep = 10

knnAccuracies <- vector( mode = "numeric", length = rep)
KValues <- vector( mode = "integer", length = rep)

#Print Accuracy values of KNN and Pruned Trees using different values of k
for (i in 1 : rep) {
    
    knnPrediction = knn(irisValuesTrain, irisValuesTest, irisClassTrain, k = i)
    knnAccuracies[i] = Accuracy(knnPrediction, irisClassTest)
    KValues[i] = i
}

df = data.frame("K" = KValues, "KNN Accuracies" = knnAccuracies,
                "CP 0.04" = prunedTreesAccuracies[1], "CP 0.08" = prunedTreesAccuracies[2], "CP 0.1" = prunedTreesAccuracies[3], "CP 0.5" = prunedTreesAccuracies[4],"CP 0.6" = prunedTreesAccuracies[5]) 

print(df)

