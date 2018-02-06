mypath = "/Users/safurasuleymanovs/Desktop/4B/Stat/A2/"
data.set ="telephone_y.txt"
value.y = scan(paste(mypath,data.set,sep=""))
value.ts = ts(scan(paste(mypath,data.set,sep="")))
# When we plot the data we see that there is an upward trend.
# Also, we notice a seasonal component with increasing seasonal variation
# and period m = 6.
plot(value.ts,type="l")
title("Old Data")
# To stabilize the variance we decide to transform the data to log(Y(t)).
# According to the graph it worked well.
plot(log(value.ts),type="l")
title("Transformed Data")
# Creating Matrix for explanatory variable X
b <-diag(12)
x <-rbind(b,b,b,b,b,b,b,b,b,b,b)
x[,1] <- 1:132
telephone.ls <-lsfit(x,log(value.y))
ls.print(telephone.ls)

# Residual Analysis
# plot(c(1:132),telephone.ls$residuals,type="l",xlab="t",ylab="Residuals",main="Plot of Residuals")

# qqnorm(telephone.ls$residuals)
# qqline(telephone.ls$residuals)

# acf(telephone.ls$residuals)

# Model 2
Y2 <- value.y[73:132]
plot(c(1:60),Y2,type="l")
b2 <- diag(12)
X2 <- rbind(b2,b2,b2,b2,b2)
X2[,1] <- (c(1:60))^2

model2.ls <-lsfit(X2,Y2)
ls.print(model2.ls)

# Residual Analysis
# plot(c(1:60),model2.ls$residuals,type="l",xlab="t",ylab="Residuals",main="Plot of Residuals")

# qqnorm(model2.ls$residuals)
# qqline(model2.ls$residuals)

# acf(model2.ls$residuals)

# Prediction Interval 1
new.X1 <-c(1,63^2,0,1,0,0,0,0,0,0,0,0,0)
Y.hat1 <-sum(new.X1*model2.ls$coefficients)
XTX.inv <-ls.diag(model2.ls)$cov.unscaled
est.stdev1 <- ls.diag(model2.ls)$std.dev*sqrt(new.X1%*%XTX.inv%*%new.X1 +1)
PI1 <- c(Y.hat1 -2*est.stdev1,Y.hat1 + 2*est.stdev1)
# [1] 64108.37 67089.45

# Prediction Interval 2
new.X2 <-c(1,75^2,0,1,0,0,0,0,0,0,0,0,0)
Y.hat2 <-sum(new.X2*model2.ls$coefficients)
XTX.inv <-ls.diag(model2.ls)$cov.unscaled
est.stdev2 <- ls.diag(model2.ls)$std.dev*sqrt(new.X2%*%XTX.inv%*%new.X2 +1)
PI2 <- c(Y.hat2 -2*est.stdev2,Y.hat2 + 2*est.stdev2)
# [1] 73839.11 77018.56

# Part j
data.set2 ="telephone_future.txt"
Y.future = scan(paste(mypath,data.set2,sep=""))
# SOS for Model 1
X.forecast1 <- diag(12)
X.forecast1[,1] <- c(133:144)
X.forecast1 <- cbind(rep(1,12),X.forecast1)
Y.hat.forecast1 <-exp(colSums(t(X.forecast1)*telephone.ls$coefficients))
SOS1 <- sum((Y.future-Y.hat.forecast1)^2)
# SOS for Model 2
X.forecast2 <- diag(12)
X.forecast2[,1] <- (c(61:72))^2
X.forecast2 <- cbind(rep(1,12),X.forecast2)
Y.hat.forecast2 <- colSums(t(X.forecast2)*model2.ls$coefficients)
SOS2<- sum((Y.future-Y.hat.forecast2)^2)
