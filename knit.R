## Question 3
mypath = "/Users/safurasuleymanovs/Desktop/4B/Stat/A5/"
data.set ="knit_y.txt"
linear.y = scan(paste(mypath,data.set,sep=""))
linear.ts = ts(scan(paste(mypath,data.set,sep="")))
knit.future = linear.ts[91:95]
## Part a - Slowly drifting mean model
knit.train1 = linear.ts[1:90]
knit.s1 = HoltWinters(knit.train1,beta=FALSE,gamma=FALSE)
alpha1 = knit.s1$alpha
pred.s1 = predict(knit.s1,1,prediction.interval = FALSE)

knit.train2 = linear.ts[1:91]
knit.s2 = HoltWinters(knit.train2,beta=FALSE,gamma=FALSE)
alpha2 = knit.s2$alpha
pred.s2 = predict(knit.s2,1,prediction.interval = FALSE)

knit.train3 = linear.ts[1:92]
knit.s3 = HoltWinters(knit.train3,beta=FALSE,gamma=FALSE)
alpha3 = knit.s3$alpha
pred.s3 = predict(knit.s3,1,prediction.interval = FALSE)

knit.train4 = linear.ts[1:93]
knit.s4 = HoltWinters(knit.train4,beta=FALSE,gamma=FALSE)
alpha4 = knit.s4$alpha
pred.s4 = predict(knit.s4,1,prediction.interval = FALSE)

knit.train5 = linear.ts[1:94]
knit.s5 = HoltWinters(knit.train5,beta=FALSE,gamma=FALSE)
alpha5 = knit.s5$alpha
pred.s5 = predict(knit.s5,1,prediction.interval = FALSE)

pred.s <- c(pred.s1,pred.s2,pred.s3,pred.s4,pred.s5)
alpha <- c(alpha1,alpha2,alpha3,alpha4,alpha5)
SOS.s <- sum((knit.future-pred.s)^2)

## Part b - Constant mean with ARMA (p,q) residuals
knit.mean1 <- mean(knit.train1)
knit.res1 <- knit.train1 - knit.mean1
plot(knit.res1,type="l", main = "Residuals plot")
qqnorm(knit.res1)
qqline(knit.res1)
acf(knit.res1)
pacf(knit.res1)

knit.arma1 <- arima(knit.train1,order=c(2,0,2),method="ML")
#tsdiag(knit.arma1)
pacf(knit.arma1$resid)
qqnorm(knit.arma1$resid)
qqline(knit.arma1$resid)
pred.arma1 <- predict(knit.arma1,n.ahead=1,se.fit=FALSE)

knit.arma2 <- arima(knit.train2,order=c(2,0,2),method="ML")
pred.arma2 <- predict(knit.arma2,n.ahead=1,se.fit=FALSE)

knit.arma3 <- arima(knit.train3,order=c(2,0,2),method="ML")
pred.arma3 <- predict(knit.arma3,n.ahead=1,se.fit=FALSE)

knit.arma4 <- arima(knit.train4,order=c(2,0,2),method="ML")
pred.arma4 <- predict(knit.arma4,n.ahead=1,se.fit=FALSE)

knit.arma5 <- arima(knit.train5,order=c(2,0,2),method="ML")
pred.arma5 <- predict(knit.arma5,n.ahead=1,se.fit=FALSE)

pred.arma <- c(pred.arma1,pred.arma2,pred.arma3,pred.arma4,pred.arma5)
SOS.arma <- sum((knit.future - pred.arma)^2)
