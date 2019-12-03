install.packages("neuralnet")
library("neuralnet")

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# Functions
Accuracy<-function(actualValues,testValues)
{
  
  n=length(testValues)
  ncorrect=sum(actualValues==testValues)
  result = ncorrect/n
  
  print(result)
  return(result)
  
}

#1
#OR gate input data
trainin = rbind(c(1,1), c(1,-1), c(-1,1), c(-1,-1));
#XOR gate output data
trainout = rbind(1, 1, 1, 0);
#Combined XOR gate data
XORdat=cbind(trainout,trainin)
# fit neural network with no hidden layers
set.seed(2)
NN = neuralnet(ORdat[,1]~., ORdat[,-1], hidden = 0 , threshold = 0.001,
               stepmax = 1e+05, linear.output = FALSE)
#visualise the NN
plot(NN)
NN$weights
testin= rbind(c(1,1))
predict_testNN = compute(NN, testin)
predict_testNN$net.result
predict_out = as.numeric(predict_testNN$net.result>0.5)
print(predict_out)

###########################################################





# fit neural network with no hidden layers
set.seed(2)
NN = neuralnet(XORdat[,1]~., XORdat[,-1], hidden = 0 , threshold = 0.001,
               stepmax = 1e+05, linear.output = FALSE)
#visualise the NN
plot(NN)
#######################################################

#XOR gate input data
trainin = cbind(winedata$Malic.acid,winedata$Color.intensity);
trainin = trainin[sample(130,130)]
#XOR gate output data
trainout = rbind(0, 1);
#Combined XOR gate data
XORdat=cbind(trainout,trainin)
#train a neural network on the XOR data
set.seed(2)


#1. Read in “winedata2.csv” from last week’s labs
winedata = read.csv('winedata2.csv',sep = ",")

#2. Build the architecture of your neural network. The output must be between one and zero.
NN = neuralnet(XORdat[,1]~., XORdat[,-1], hidden = c(5,5,3) , threshold =
                 0.001, stepmax = 1e+05, linear.output = FALSE)

testout=rbind(0,1,1,0)
predict_testNN = compute(NN, testin)
predict_testNN$neurons
predict_testNN$net.result
predict_out = as.numeric(predict_testNN$net.result>0.5)
predict_out

#3. Using any two variables from the wine data, set up the data as you did for the linear classifier with a train and test set
wine_sample <- cbind(winedata$Malic.acid,winedata$Color.intensity)
wine_rand = wine_sample[sample(130,130),]
wine_rand
wineClass = wine_rand[,1]
wineValues = wine_rand[,-1]
wineValues

ClassTrain = wineClass[1:65]
wineValuesTrain = wineValues[1:65]

wineClassTest = wineClass[66:130]
wineValuesTest = wineValues[66:130]

#4. Train the neural network on half of the data and test it on the remaining
train_NN=compute()

#5. Calculate the accuracy

wineAccuracy = Accuracy()