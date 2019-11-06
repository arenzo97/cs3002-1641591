#Set Directory to this current directory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
mydata=read.csv('forestfires.csv', sep=",")


#Functions
PlotLine<-function(idata)
{
  plotline = plot(idata, type="l")
  return(plotline)
}

PlotHistograms<-function(idata)
{
  plothist = hist(idata)
  return(plothist)
}

PlotMean<-function(idata)
{
  plotmean = mean(idata)
  return(plotmean)
}
PlotScattered<-function(idata1,idata2,idata3)
{
  plotscattered = plot(idata1,idata2,col = idata3)
  return(plotscattered)
}

PlotRegressiveLinearModel<-function(idata1,idata2)
{
  regLM = plot(idata1,idata2)
  lmfire=line(idata2,idata2)
  regabline = abline(coef(lmfire))
}

OutputToCSV<-function(idata,filename)
{
  write.csv(idata, file = paste0(filename,".csv"))
}

#Plot Data
meanplot = PlotMean(mydata$temp)
histplot = PlotHistograms(mydata$temp)
lineplot = PlotLine(mydata$temp)
scatplot = PlotScattered(mydata$X,mydata$Y,mydata$wind)
regplot = PlotRegressiveLinearModel(mydata$temp,mydata$ISI)

#Output to a CSV file
OutputToCSV(meanplot,"mean2")



