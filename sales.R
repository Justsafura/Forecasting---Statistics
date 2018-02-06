## Question 4
mypath = "/Users/safurasuleymanovs/Desktop/4B/Stat/A5/"
data.set ="sales2.txt"
sales.y = scan(paste(mypath,data.set,sep=""))
sales.ts = ts(scan(paste(mypath,data.set,sep="")))
plot(sales.ts, main="Plot of Sales")
acf(sales.ts)
sales.d = diff(sales.ts)
plot(sales.d, type="l", main = "Plot of Differenced Sales")
acf(sales.d)
pacf(sales.d)
sales.ar3 <- arima(sales.ts,order=c(3,1,0),method="ML")
pacf(sales.ar3$residuals)
#tsdiag(sales.ar3)
sales.ma3 <- arima(sales.ts,order=c(0,1,3),method="ML")
pacf(sales.ma3$residuals)
#tsdiag(sales.ma3)
sales.arma21 <- arima(sales.ts,order=c(2,1,1),method="ML")
pacf(sales.arma21$residuals)
#tsdiag(sales.arma21)
sales.pred <- predict(sales.arma21,n.ahead=6,se.fit=TRUE)
u <- sales.pred$pred + 1.96*sales.pred$se
l <- sales.pred$pred - 1.96*sales.pred$se
plot(sales.ts,xlim=c(1,120),type="l",xlab="Time")
lines(sales.pred$pred,col="red")
lines(u,col='blue',lty='dashed')
lines(l,col='blue',lty='dashed')
abline(v=114,lty="dotted")





