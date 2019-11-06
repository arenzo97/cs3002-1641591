#Set Directory to this current directory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

#WK Source
source("WK_R.r")

mydata = read.csv('iris.csv', sep=",")
mydata = na.omit(mydata)
mydata = scale(mydata)

mydata2 = read.csv('iris_real.csv', sep=",")

#Hierarchical Clustering
HierarchicalPlot<-function(idata,fitMethod)
{
  d <- dist(idata, method = "euclidean")
  fit <- hclust(d, method = fitMethod)
  plot(fit)
  Hgroups <-cutree(fit,k=5)
  rect.hclust(fit,k=5, border="red")
  return(fit)
}


#create clusters by cutting dendogram




#KMeans Clustering
KMeansFit<-function(idata,kvalue)
{
  fit<-kmeans(idata,kvalue)
  aggregate(idata,by=list(fit$cluster),FUN=mean)
  Kgroups=fit$cluster
  plot(idata,col=Kgroups)
  return(Kgroups)
}

#Calculate WK for K means
CalcWK<-function(kgroup1,kgroup2)
{
  wk=WK_R(kgroup1,kgroup2)
  return(wk)
}


#Run Answers:
#1 & 5
wk1 = WK_R(KMeansFit(mydata,3),mydata2$X1)

K=5

WKVector <-vector(mode="integer",length = K)
KVector <-vector(mode="integer",length = K)

for (i in 1: K)
{
  wk=WK_R(KMeansFit(mydata,i+1),mydata2$X1)
  WKVector[i]=wk
  KVector[i]=i+1
  
}

output = data.frame(KVector,WKVector)
plot(output)


#2 & 4
h1 = HierarchicalPlot(mydata,"single")
h2 = HierarchicalPlot(mydata,"complete")
h3 = HierarchicalPlot(mydata,"average")

#3
scatteredCluster = KMeansFit(mydata,3)

