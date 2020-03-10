library(MASS)


z <- ts(bicoal.tons,star=1920)

plot(z,type="b",pch="???",main="Annual production of bitumous coal during period of 1920 to 1968",
     ylab="Net tons(in millions)",
     xlab="Year")

lines(lowess(time(z),z,f=2/5),col="red")


#log-transformation doesnt scale and help much
plot(log(z),type="b",pch="???",main="Annual production of bitumous coal during period of 1920 to 1968",
     ylab="Net tons(in millions)",
     xlab="Year")

#Since we have annual data, there is no "seasonal" component so stl crashes:
#Error in stl(z): Time series must have frequency > 1
#Dumped


x <- time(z)-1920
fit <- rlm(z~cbind(x,x^2,x^3))
plot(z,type="b",pch="???",main="Annual production of bitumous coal during period of 1920 to 1968",
     ylab="Net tons(in millions)",
     xlab="Year")
lines(as.vector(time(z)),cbind(1,x,x^2,x^3) %*% fit$coef)

fit <- rlm(diff(z,1,3)~cbind(x,x^2,x^3)) 
plot(z,type="b",pch="???",main="Annual production of bitumous coal during period of 1920 to 1968",
     ylab="Net tons(in millions)",
     xlab="Year")
lines(as.vector(time(z)),cbind(1,x,x^2,x^3) %*% fit$coef)


summary(fit)

fit$coefficients
Ident(fit$residuals)

par(mfrow=c(1,3),oma=c(0,0,3,0))

plot(diff(z),type="b",pch="???",
     main="Plot of First Differenced Quarterly Private Expenditure on Alcohol in Australia from 1974 to 1990",
     ylab="10^6 AUD in 1984/85 prices",
     xlab="Quarter")
abline(h=0)
lines(lowess(diff(z)),col="red")

plot(diff(z,1,2),type="b",pch="???",
     main="Plot of Second Differenced Quarterly Private Expenditure on Alcohol in Australia from 1974 to 1990",
     ylab="10^6 AUD in 1984/85 prices",
     xlab="Quarter")
abline(h=0)
lines(lowess(diff(z,1,2)),col="red")
diff2<-diff(z,difference=2)



plot(diff(z,1,3),type="b",pch="???",
     main="Plot of Third Differenced",
     ylab="net tons(in millions)",
     xlab="Years")
lines(lowess(diff(z,1,3)),col="red")
abline(h=0)


par(mfrow=c(1,2),oma=c(0,0,3,0))

plot(diff(z,1,4),type="b",pch="???",
     main="Plot of Fourth Differenced",
     ylab="net tons(in millions",
     xlab="Years")
abline(h=0)
lines(lowess(diff(z,1,4)),col="red")

plot(diff(z,1,5),type="b",pch="???",
     main="Plot of Fifth Differenced",
     ylab="net tons(in millions",
     xlab="Years")
abline(h=0)
lines(lowess(diff(z,1,5)),col="red")

diff2=diff(z,1,2)
x=time(diff2)-1922
diff2<-rlm(diff2~x)
plot(diff2$residuals,type="b",pch="???")
abline(h=0)

diff3=diff(z,1,3)
x=time(diff3)-1923

diff31<-rlm(diff3~x)
diff32<-rlm(diff3~cbind(x,x^2))
diff33<-rlm(diff3~cbind(x,x^2,x^3))

summary(diff31)
summary(diff32)
summary(diff33)

anova(diff31)
anova(diff32)
anova(diff33)

Ident(diff31$residuals)
Ident(diff32$residuals)


fin=Raic(diff32$residuals)
res <- fin$resid[,2]
Ident(res)
fv <- fits$resid[-1] - res

par(mfcol=c(2,2),oma = c(6,0,6,0))

plot(c(1:45),res,type="b",
     main="Residual Series",
     xlab="Time",
     ylab="Residuals")

qqnorm(res,
       main="Quantile-Quantile Plot",
       xlab="Gaussian Quantiles",
       ylab="Residuals")

id <- qqnorm(res,plot=F)
identify(id$x,id$y,c(1:45))

plot(fv,res,
     main="Residual Plot",
     xlab="Lagged Irregular Values",
     ylab="Residuals")

plot(fv,abs(res),
     main="Absolute Residual Plot",
     xlab="Lagged Irregular Values",
     ylab="Absolute Residuals")
lines(lowess(fv,abs(res)))


par(mfrow=c(1,2),oma=c(0,0,3,0))

plot(diff31$residuals,type="b",pch="???")
abline(h=0)

plot(diff32$residuals,type="b",pch="???")
abline(h=0)

par(mfrow=c(1,2),oma=c(0,0,3,0))

