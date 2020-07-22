#Question1

#(a)
library('quantmod')
getSymbols(c("AMRMX","IWD","IWF","IWO","VONG"),from="2011-01-03", to="2016-12-29",src="yahoo", auto.assign=TRUE)

logrtn.AMRMX = diff(log(as.vector(AMRMX$AMRMX.Adjusted)))
logrtn.IWD = diff(log(as.vector(IWD$IWD.Adjusted)))
logrtn.IWF = diff(log(as.vector(IWF$IWF.Adjusted)))
logrtn.IWO = diff(log(as.vector(IWO$IWO.Adjusted)))
logrtn.VONG = diff(log(as.vector(VONG$VONG.Adjusted)))

plot.new()
pairs(~logrtn.AMRMX+logrtn.IWD+logrtn.IWF+logrtn.IWO+logrtn.VONG)

library('corrplot')
rates <- data.frame(logrtn.AMRMX,logrtn.IWD,logrtn.IWF,logrtn.IWO,logrtn.VONG)
corrplot.mixed(cor(rates),upper="ellipse")

#According to the scatter plot and correlation plot, there are various linear relationships existing, 
#and relatively strong correlations existing among all the variables.

#(b)
fit <- lm(logrtn.AMRMX ~ logrtn.IWD+logrtn.IWF+logrtn.IWO+logrtn.VONG)
summary(fit)
#Coefficient for Intercept is 3.609e-05. It's not significant under 5% significance level.
#Coefficient for logrtn.IWD is 5.434e-01. It's significant under 5% significance level.
#Coefficient for logrtn.IWF is 3.377e-01. It's significant under 5% significance level.
#Coefficient for logrtn.IWO is -7.343e-02. It's significant under 5% significance level.
#Coefficient for logrtn.VONG is 2.870e-02. It's not significant under 5% significance level.
#The sample correlation between fitted and observed values is the R-squared, which is 0.9383.
#it indicates the percentage of changes in Y thats explained by the changes in X's.

#(c)
anova(fit)
#According to the F-test, it's significant to add logrtn.IWD,logrtn.IWF and logrtn.IWO to provide
#new information. But logrtn.VONG is insignificant to be added into the model.
#The  total sum of squares of the model is the sum of SSR and SSE, which is 0.0986.

#(d)
fit2 <- lm(logrtn.AMRMX ~ logrtn.IWO+logrtn.VONG)
summary(fit2)
#We have more than 95% of confidence that at least one coefficent out of ¡°IWO¡± and ¡°VONG¡± and the 
#intercept is siginficant and not equal to 0. Reject H0, accept H1.

#(e)
library('leaps')
subsets = regsubsets(logrtn.AMRMX~.,
                     data=as.data.frame(cbind(logrtn.IWD,logrtn.IWF,logrtn.IWO,logrtn.VONG)),
                     nbest=1)
b = summary(subsets)
b
#One predictor: logrtn.IWD
#Two predictor: logrtn.IWD and logrtn.IWF
#Three predictor: logrtn.IWD, logrtn.IWF and logrtn.IWO

#(f)
plot(1:3,b$bic[1:3],type="b",xlab="number of variables",
     ylab="BIC")
#Based on BIC, the model with three predictors of logrtn.IWD, logrtn.IWF and logrtn.IWO is the best set.

#(g)
library(faraway)
options(digits=2)
vif(fit)
#Based on VIF scores, the VIF for logrtn.IWF and logrtn.VONG are really high. And the VIF for
#logrtn.IWD is much larger than 1 and close to 10. The scores reveals there are Multicollinearity.
#Also some other evidences are by comparing fit2 to fit1, we see coeffiect and variance changed a
#lot while including new variables.

#Question3

dat = read.table("HW2-data.txt",header=TRUE)
dat = dat[order(dat$T),]
EMP = -diff(log(dat$price))/diff(dat$T)
MAT = dat$T[-1]

#(a)
fit3 <- lm(EMP ~ MAT)
summary(fit3)
plot(MAT,EMP,xlab="MAT",
     ylab="EMP",pch=19,col=rgb(0,0,100,50,maxColorValue=255))
abline(fit3, col=2, lwd=2)
abline(h=0,v=0)

#(b)
plot(hatvalues(fit3), ylab="leverage",main="Leverage plot", ylim=c(0,0.1))
abline(fit3,lwd=0.5,col=2)
#There's no high-leverage point observed.

sigma = summary(fit3)$sigma
min_rstudent = min(rstudent(fit3))
max_rstudent = max(rstudent(fit3))
plot(rstudent(fit3), ylim = c(min(-5, min_rstudent), max(5, max_rstudent)),
     ylab="rstudent", ,main="rstudent residual plot")
abline(h=c(-3, 3),lwd=0.5,col=2)
#There's a residual outlier observed in the model.

halfnorm(sqrt(cooks.distance(fit3)), xlim=c(0,3),
         ylab=("square root Cook's D"), main="Half normal plots of square-root of Cook's D")
#There is high-influence points in the model.

#(c)
par(mfrow=c(1,2))
qqnorm(rstudent(fit3), datax = TRUE,
       xlab = "normal quantile", 
       ylab = "rstudent", 
       main = "normal probability plot for rstudent")
qqline(rstudent(fit3), datax=TRUE, col = 2)
d <- density(rstudent(fit3), adjust = 1, na.rm = TRUE)
plot(d, type = "n", xlim=c(-5,5), main="KDE for rstudent residuals and N(0,1) density")
polygon(d, col = "wheat")
z = seq(from=-5,to=5,by=.01)
lines(z,dnorm(z), lty=2,lwd=3,col="red")
#The qqplot indicates heavy-tails than normal distribution. The KDE shows the distribution is
#negetively skewed.

plot(fitted(fit3), abs(rstudent(fit3)),
     xlab = "fitted values", ylab = "abs(rstudent)",
     main = "Absolute residual plot with a smoother")
smoother = loess(abs(rstudent(fit3)) ~ fitted(fit3))
ord = order(fitted(fit3)) # why do we need this line? 
lines(fitted(fit3)[ord], fitted(smoother)[ord], col="red", lwd=2)
#The pattern appear to have a nonlinear trend. The curvature of the loess fit is evident 
#and indicates that Y is not linear.A possible remedy is to add quadric predictor.

plot(MAT, rstudent(fit3),
     xlab = MAT, ylab = "rstudent)")
smoother = loess(rstudent(fit3) ~ MAT); ord = order(MAT)
lines(MAT[ord], fitted(smoother)[ord], col="red", lwd=2)
#There's a systematic nonlinear trend in this plot indicates that the effect of predictors on 
#response is nonlinear.

acf(resid(fit3))
#Autocorrelation is significant at many levels. This indicates that the residuals are highly correlated.

#(d)
fit4 <- lm(EMP ~ MAT+I(MAT^2)+I(MAT^3))
summary(fit4)
plot(MAT,EMP,xlab="MATdata",
     ylab="EMP",pch=19,col=rgb(0,0,100,50,maxColorValue=255))
lines(MAT, fitted(fit4), col='red', type='b')

#(f)
plot(hatvalues(fit4), ylab="leverage",main="Leverage plot", ylim=c(0,1))
abline(fit4,lwd=0.5,col=2)
#There are high-leverage points observed.

sigma = summary(fit4)$sigma
min_rstudent = min(rstudent(fit4))
max_rstudent = max(rstudent(fit4))
plot(rstudent(fit4), ylim = c(min(-5, min_rstudent), max(5, max_rstudent)),
     ylab="rstudent", ,main="rstudent residual plot")
abline(h=c(-3, 3),lwd=0.5,col=2)
#There's are residual outliers observed in the model.

halfnorm(sqrt(cooks.distance(fit4)), xlim=c(0,3),
         ylab=("square root Cook's D"), main="Half normal plots of square-root of Cook's D")
#There are high-influence points in the model.

par(mfrow=c(1,2))
qqnorm(rstudent(fit4), datax = TRUE,
       xlab = "normal quantile", 
       ylab = "rstudent", 
       main = "normal probability plot for rstudent")
qqline(rstudent(fit4), datax=TRUE, col = 2)
d <- density(rstudent(fit4), adjust = 1, na.rm = TRUE)
plot(d, type = "n", xlim=c(-5,5), main="KDE for rstudent residuals and N(0,1) density")
polygon(d, col = "wheat")
z = seq(from=-5,to=5,by=.01)
lines(z,dnorm(z), lty=2,lwd=3,col="red")
#The qqplot indicates lighter-tails than normal distribution. The KDE shows the distribution is
#not skewed, but has thinner tail than normal distribution.

plot(fitted(fit4), abs(rstudent(fit4)),
     xlab = "fitted values", ylab = "abs(rstudent)",
     main = "Absolute residual plot with a smoother")
smoother = loess(abs(rstudent(fit4)) ~ fitted(fit4))
ord = order(fitted(fit4)) # why do we need this line? 
lines(fitted(fit4)[ord], fitted(smoother)[ord], col="red", lwd=2)
#The pattern appear to have a decreasing linear trend, which is an indication of non-constant variance.

plot(MAT, rstudent(fit4),
     xlab = MAT, ylab = "rstudent)")
smoother = loess(rstudent(fit4) ~ MAT); ord = order(MAT)
lines(MAT[ord], fitted(smoother)[ord], col="red", lwd=2)
#The graph show a linearity.

acf(resid(fit4))
#The ACF function has small values and mostly insignificant, This indicates that the residuals are uncorrelated.
