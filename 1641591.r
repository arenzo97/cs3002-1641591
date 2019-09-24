plot(mydata$temp,mydata$ISI)
lmfire=line(mydata$ISI~mydata$temp)
abline(coef(lmfire))
