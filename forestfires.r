this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

mydata = read.csv(this.dr + '\\forestfires.csv', sep=",")
plot(mydata)

