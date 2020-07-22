#Question1

library(xts)
dat=read.table("HW4-Q1.txt", header=TRUE, sep = ",")
VIX=xts(dat[,-1], order.by=as.Date(dat[, 1]))
names(VIX) = c("VIXCLS")
dim(VIX)

#(a)
ols1 = as.data.frame(log(VIX))
names(ols1)[1] <- "Y1"
ols1$X1 = lag(log(VIX), n = 1)
ols1=ols1[2:6359,1:2]
fit_OLS = lm(ols1$Y1 ~ ols1$X1)
summary(fit_OLS)
plot(as.numeric(ols1$X1),ols1$Y1)
abline(fit_OLS,col="red")

#(b)
library(corrplot)
corrplot.mixed(cor(ols1),upper="ellipse")
#VIX and lagged VIX are highly correlated.

par(mfrow=c(1,2));par(mar=c(5,4,1,4))
plot(fit_OLS$fitted,abs(rstudent(fit_OLS)),xlab = "fitted values", ylab = "abs(rstudent)")
fit_loess  = loess(abs(rstudent(fit_OLS))~ fit_OLS$fitted,span=1,deg=1)
ord = order(fit_OLS$fitted)
lines(fit_OLS$fitted[ord],fit_loess$fit[ord], col="red")
qqnorm(rstudent(fit_OLS),datax=T,main="(d)",xlab="theoretical quantiles",ylab="sample quantiles of rstudent")
qqline(rstudent(fit_OLS),datax=T, col="red")
#Residual analysis indicates non-constant variance and non-normality.
acf(log(VIX), main="Sample ACF for VIX")
#The sample ACF indicate some significant autocorrelation at multiple levels.

#(c)
#inclusion of lagged terms as predictor creates a collinearity problem.
#non-constant variance and non-normality are observed.
plot(log(VIX))
acf(log(VIX), main="Sample ACF for VIX")
#Both VIX and lagged VIX are not stationary.
#The sample ACF indicate some significant autocorrelation at multiple levels.
#Therefore, linear regression is not the ideal model to forecast future VIX.

#Question2

#(a)
gr=diff(VIX)
gr=gr[2:6359]
t_stat=mean(gr)/(sqrt(sd(gr)/nrow(gr)))
abs(t_stat)>abs(qt(1-0.05/2, nrow(gr)-1))
#under 0.05 significant level, fail to reject H0. Thus, ¦Ì is approximately 0.

#(b)
acf(ar1$gr, main="Sample ACF for growth rate")
#The sample ACF indicate some significant autocorrelation at lags 1, 2, and 10
Box.test(ar1$gr, lag = 5, type = "Ljung-Box")
Box.test(ar1$gr, lag = 10, type = "Ljung-Box") 
Box.test(ar1$gr, lag = 20, type = "Ljung-Box") 
#According to Ljung-Box test, there are non-zero autocorrelation in the first 5, 10 and 20 lags.
#Thus, {zt} is autocorrelated.

#(c)
library(quantmod) 
getSymbols("VIXCLS", src="FRED")
VIX = na.omit(VIXCLS["1990-01/2015-03"])
VIXgr = na.omit(diff(VIX))

fitAR1 = arima(VIXgr, order = c(1,0,0))
print(fitAR1)
#(1???(-0.1069)B)(rt+0.0003)=¦Åt;¦Ò??2¦Å=2.29

AR1_resid = resid(fitAR1)
plot(xts(AR1_resid, index(VIXgr)),
     main="Residuals of AR(1) model fitted to Growth rate",
     major.ticks = FALSE, minor.ticks = FALSE)
#the residuals are stationary.

acf(AR1_resid, main="Sample ACF for growth rate")
Box.test(AR1_resid, lag = 5, type = "Ljung-Box", fitdf = 1)
#there are autocorrelation in the residuals.

par(mfrow=c(1,2));par(mar=c(3,3,3,3))
plot(resid(fitAR1)**2, type="l", col=1, main = expression(residual^2))
smoother = loess((resid(fitAR1)**2) ~ seq(1,length(resid(fitAR1))), span=0.1)
lines(seq(1,length(resid(fitAR1))),fitted(smoother),col=2)
acf((resid(fitAR1)**2), main=expression("sample ACF of "~ residual^2))
Box.test(resid(fitAR1)**2, lag = 5, type = "Ljung-Box", fitdf = 1)
#there are autocorrelation in the residuals squared, volatility is not constant.

qqnorm(AR1_resid, datax = TRUE,
       xlab = "normal quantile",
       ylab = "sample quantile of residuals",
       main = "normal probability plot for residuals")
qqline(AR1_resid, datax=TRUE, col = 2)
#The result indicate non-normality.
#Based on those tests, AR(1) might not be the ideal model for this.

#Question3

#(a)
root <- polyroot(c(20,(-1/90),(1/30),(19/90),(-3/10)))
root = Mod(root)
abs(1/root) < 1
#the model is weakly stationary.

#Question4

library(xts)
dat=read.table("HW4-Q4.txt", header=TRUE, sep = ",")
P=xts(dat[,2], order.by=as.Date(dat[, 1]));names(P) = c("Oil")
r = diff(P)[-1]

#(a)
t_stat=mean(P)/(sqrt(sd(P)/nrow(P)))
abs(t_stat)>abs(qt(1-0.05/2, nrow(P)-1))
#reject H0, ¦Ì is not 0.

acf(P, main="Sample ACF for Oil price")
Box.test(P, lag = 5, type = "Ljung-Box")
#there are autocorrelation in the residuals.
#{Pt} is not a weak white noise.

t_stat=mean(r)/(sqrt(sd(r)/nrow(r)))
abs(t_stat)>abs(qt(1-0.05/2, nrow(r)-1))
#Fail to reject H0, ¦Ì is approximately 0.

acf(r, main="Sample ACF for Oil price changes")
Box.test(r, lag = 5, type = "Ljung-Box")
#there are autocorrelation in the residuals.
#{rt} is not a weak white noise.

#(b)
pacf(r,main="Sample PACF for oil price changes")
#AR(1) can be a good fit.
ARfit = arima(r, order = c(1,0,0))
print(ARfit)
#(1???0.3861B)(rt-0.1289)=¦Åt;¦Ò??2¦Å=15.87

res = resid(ARfit)
chartSeries(xts(res, index(r)),
            name = paste("Residuals of AR(", 1, ") model fitted to r"),
            theme=chartTheme("white", up.col='black'))
#The residuals doesn't seem stationary and may have non-constant volatility

acf(res, main="Sample ACF for the residuals")
Box.test(res, lag = 5, type = "Ljung-Box", fitdf = 1)
Box.test(res, lag = 10, type = "Ljung-Box", fitdf = 1)
#The residuals are autocorrelated for the first 10 lag, but passed the test for first 5 lag.
# AR(1) model is not a ideal fit.

#(c)
library(forecast)

auto.arima(r, max.p=0, max.q = 20, d=0, ic="aic")
MAfit = auto.arima(r, max.p=0, max.q = 20, d=0, ic="bic")
#MA(2) is chosen by both methods.

res = resid(MAfit)

chartSeries(xts(res, index(r)),
            name = paste("Residuals of AR(", 2, ") model fitted to P"),
            theme=chartTheme("white", up.col='black'))
#The residuals doesn't seem stationary and may have non-constant volatility

acf(res, main="Sample ACF for the residuals")
Box.test(res, lag = 5, type = "Ljung-Box", fitdf = 2)
Box.test(res, lag = 10, type = "Ljung-Box", fitdf = 2)
#The residuals are not autocorrelated for the first 5 and 10 lag.

par(mfrow=c(1,2));par(mar=c(3,3,3,3))
plot(res**2, type="l", col=1, main = expression(residual^2))
smoother = loess((res**2) ~ seq(1,length(res)), span=0.1)
lines(seq(1,length(res)),fitted(smoother),col=2)
acf((res**2), main=expression("sample ACF of "~ residual^2))
Box.test(res**2, lag = 5, type = "Ljung-Box", fitdf = 2)
#there are autocorrelation in the residuals squared, volatility is not constant.

qqnorm(res, datax = TRUE,
       xlab = "normal quantile",
       ylab = "sample quantile of residuals",
       main = "normal probability plot for residuals")
qqline(res, datax=TRUE, col = 2)
#The result indicate non-normality.
#Based on those tests, MA(2) might not be the ideal model for this. 

#(d)
library(forecast)
auto.arima(P, max.p = 20, max.q = 0, d = 0, ic = "aic")
auto.arima(P, max.p = 20, max.q = 0, d = 0, ic = "bic")
#AR(2) is chosen by both methods.

fitAR = arima(P, order = c(2,0,0))
res = resid(fitAR)
chartSeries(xts(res, index(P)),
            name = paste("Residuals of AR(", 2, ") model fitted to P"),
            theme=chartTheme("white", up.col='black'))
#The residuals doesn't seem stationary and may have non-constant volatility

acf(res, main="Sample ACF for the residuals")
Box.test(res, lag = 5, type = "Ljung-Box", fitdf = 2)
Box.test(res, lag = 10, type = "Ljung-Box", fitdf = 2)
#The residuals are not autocorrelated for the first 5 and 10 lag.

par(mfrow=c(1,2));par(mar=c(3,3,3,3))
plot(res**2, type="l", col=1, main = expression(residual^2))
smoother = loess((res**2) ~ seq(1,length(res)), span=0.1)
lines(seq(1,length(res)),fitted(smoother),col=2)
acf((res**2), main=expression("sample ACF of "~ residual^2))
Box.test(res**2, lag = 5, type = "Ljung-Box", fitdf = 2)
#there are autocorrelation in the residuals squared, volatility is not constant.

qqnorm(res, datax = TRUE,
       xlab = "normal quantile",
       ylab = "sample quantile of residuals",
       main = "normal probability plot for residuals")
qqline(res, datax=TRUE, col = 2)
#The result indicate non-normality.
#Based on those tests, AR(2) might not be the ideal model for this.

#Question5

dat = read.table("HW4-Q5.txt",header=T)
dd = dat[,2:13]
x = c(t(dd))

#(a)
t_stat=mean(x)/(sqrt(sd(x)/length(x)))
abs(t_stat)>abs(qt(1-0.05/2, length(x)-1))
#reject H0, ¦Ì is not 0.

acf(x, main="Sample ACF for Oil price")
Box.test(x, lag = 5, type = "Ljung-Box")
#there are autocorrelation in the residuals.
#{xt} is not a stationary process.

#(b)
z=  na.omit(diff(x))

t_stat=mean(z)/(sqrt(sd(z)/length(z)))
abs(t_stat)>abs(qt(1-0.05/2, length(z)-1))
#fail to reject H0, ¦Ì is approximately 0.

acf(z, main="Sample ACF for Oil price")
Box.test(z, lag = 5, type = "Ljung-Box")
#there are autocorrelation in the residuals.
#{zt} is not a stationary process.

#(c)
auto.arima(x, max.p = 20, max.q = 0, d = 0, ic = "aic")
auto.arima(x, max.p = 20, max.q = 0, d = 0, ic = "bic")
Temfit = arima(x, order = c(7,0,0))
print(Temfit)
#(1???0.1136B)(rt-6.8167)=¦Åt;¦Ò??2¦Å=355

res = resid(Temfit)
chartSeries(xts(res, attr(dat,"Index")),
            name = paste("Residuals of AR(", 7, ") model fitted to P"),
            theme=chartTheme("white", up.col='black'))
#The residuals doesn't seem stationary and may have non-constant volatility

acf(res, main="Sample ACF for the residuals")
Box.test(res, lag = 10, type = "Ljung-Box", fitdf = 7)
#The residuals are not autocorrelated for the first 10 lag.

par(mfrow=c(1,2));par(mar=c(3,3,3,3))
plot(res**2, type="l", col=1, main = expression(residual^2))
smoother = loess((res**2) ~ seq(1,length(res)), span=0.1)
lines(seq(1,length(res)),fitted(smoother),col=2)
acf((res**2), main=expression("sample ACF of "~ residual^2))
Box.test(res**2, lag = 5, type = "Ljung-Box", fitdf = 7)
#there are autocorrelation in the residuals squared, volatility is not constant.

qqnorm(res, datax = TRUE,
       xlab = "normal quantile",
       ylab = "sample quantile of residuals",
       main = "normal probability plot for residuals")
qqline(res, datax=TRUE, col = 2)
#The result indicate non-normality.
#Based on those tests, AR(7) might not be the ideal model for this.

