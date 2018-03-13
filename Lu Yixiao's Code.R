#data=read.table('gauge.txt',header=T)
density=data$density
gain=data$gain
loggain=log(gain)
logdata=data.frame(density,loggain)
library(ggplot2)
#install.packages("chemCal")
library(chemCal)

#least square linear regression
fit1 <- lm(log(gain)~density, data)
plot(data[,1],log(data[,2]))
abline(fit1, col="red")
summary(fit1)

#confidence and prediction bands
pred=predict(fit1, interval="prediction")
ggplot(cbind(data,pred),aes(x=density, y=log(gain)))+
  geom_point() +
  geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y=upr), color = "red", linetype = "dashed")+
  geom_smooth(method=lm, se=TRUE)

#Inverse predicting/calibrating gain based on observed mean at each density level
y=data.frame(rep(0,9),rep(0,9),rep(0,9),rep(0,9),rep(0,9))
for (i in 1:9) {
  y[i,1]=density[10*i]
  y[i,2]=mean(gain[(10*i-9):(10*i)])
}
logy=log(y[,2])
invpred=t(sapply(logy,function(loggain) inverse.predict(fit1,loggain)[c(1,4)]))
for (i in 1:9){
  y[i,3]=invpred[[i]]
  y[i,4:5]=invpred[[9+i]]
}

#Confidence and Prediction bands of inverse prediction
ggplot(y,aes(x=log(y[,2]), y=y[,1]))+
  geom_point() +
  geom_line(aes(y=y[,5]), color = "red", linetype = "dashed")+
  geom_line(aes(y=y[,4]), color = "red", linetype = "dashed")+
  geom_smooth(method=lm, se=TRUE)

#inverse prediction of any gain
inverse.predict(fit1,log(38.6))
