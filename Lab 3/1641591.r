#install.packages("rpart")
library(rpart)
library(class)

#Set Directory to this current directory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

#set data file, class and values
winedata = read.csv('winedata.csv', sep=",")
wineclass = winedata[,1]
winevalues = winedata[,-1]

#set up a training set
wineclassTrain = wineclass[1:100]
winevaluesTrain = winevalues[1:100,]
#and testset
wineclassTest = wineclass[101:178]
winevaluesTest = winevalues[101:178,]

fit <- rpart(wineclassTrain~., method="class", data=winevaluesTrain)

plot(fit, uniform=TRUE, main="Decision Tree for WineData3")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

treepred <-predict(fit, winevaluesTest, type = 'class')

n = length(wineclassTest) #the number of test cases
ncorrect = sum(treepred==wineclassTest) #the number of correctly
predicted
accuracy=ncorrect/n
print(accuracy)

table_mat = table(wineclassTest, treepred)
print(table_mat)

pfit<- prune(fit, cp=0.1)
plot(pfit, uniform=TRUE, main="Pruned Decision Tree for WineData3")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
