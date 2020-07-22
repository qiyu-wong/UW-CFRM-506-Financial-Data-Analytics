#Question1

dat = read.table("HW3-Q1.txt",header=TRUE)
dat = dat[order(dat$T),]
EMP = -diff(log(dat$price))/diff(dat$T)
MAT = dat$T[-1]

MAT2 = MAT**2
MAT3 = MAT**3
fit <- lm(EMP ~ MAT + MAT2 + MAT3)
summary(fit)

resid = rstudent(fit); yhat = fitted(fit)
par(mfrow=c(1,2));par(mar=c(5,4,1,4))
plot(yhat,abs(resid),xlab = "fitted values", ylab = "abs(rstudent)")
fit_loess  = loess(abs(resid)~ yhat,span=1,deg=1)
ord = order(yhat)
lines(yhat[ord],fit_loess$fit[ord], col="red")
qqnorm(resid,datax=T,xlab="theoretical quantiles",ylab="sample quantiles of rstudent")
qqline(resid,datax=T, col="red")
#The residual diagnostic revealed that the residuals are not normal and do not have constant variance.

library(faraway)
vif(fit)
#There's high collinearity between the variables, so decide to eliminate MAT2.

fit2 <- lm(EMP ~ MAT + MAT3)
summary(fit2)
vif(fit2)

plot(MAT,EMP,ylim=c(0.025,.075),lwd=2,xlab="maturity",
     ylab="empirical forward-rate",cex=.75)
ord = order(MAT)
lines(MAT[ord], fitted(fit2)[ord], col="red", lwd=2)

resid = rstudent(fit2); yhat = fitted(fit2)
par(mfrow=c(1,2));par(mar=c(5,4,1,4))
plot(yhat,abs(resid),xlab = "fitted values", ylab = "abs(rstudent)")
fit_loess  = loess(abs(resid)~ yhat,span=1,deg=1)
ord = order(yhat)
lines(yhat[ord],fit_loess$fit[ord], col="red")
qqnorm(resid,datax=T,xlab="theoretical quantiles",ylab="sample quantiles of rstudent")
qqline(resid,datax=T, col="red")
#The residual diagnostic revealed that the residuals still are not normal and do not have constant variance.

library(MASS)
L_max = boxcox(EMP ~ MAT + MAT3,ylab="log-likelihood", lambda = seq(2, 4, 1/10))
alphahat = L_max$x[which.max(L_max$y)]
EMP2 = EMP^alphahat
fit_BoxCox = lm(EMP2 ~ MAT + MAT3)
summary(fit_BoxCox)

resid = rstudent(fit_BoxCox); yhat = fitted(fit_BoxCox)
par(mfrow=c(1,2));par(mar=c(5,4,1,4))
plot(yhat,abs(resid),xlab = "fitted values", ylab = "abs(rstudent)")
fit_loess  = loess(abs(resid)~ yhat,span=1,deg=1)
ord = order(yhat)
lines(yhat[ord],fit_loess$fit[ord], col="red")
qqnorm(resid,datax=T,xlab="theoretical quantiles",ylab="sample quantiles of rstudent")
qqline(resid,datax=T, col="red")
#The residual analysis shows that the issues are corrected.

#Question2

#(a)
dat2 = read.table("HW3-Q2.dat",header=TRUE)
DUR = dat2[,"duration"]
N = dat2[,'trades']
N[N!=0]=2
N[N!=2]=1
N[N!=1]=0
fit_OLS <- lm(N ~ log(DUR))
summary(fit_OLS)
plot(DUR,N,ylim=c(0,1),lwd=2,xlab="DUR",
     ylab="P(N)",cex=.75)
ord = order(DUR)
lines(DUR[ord], fitted(fit_OLS)[ord], col="red", lwd=2)

#(b)
#The model using OLS ignores the constraint that response has to be bewteen -1 and 1.
#So the predicted value went above 1 and below -1.
#To fix this, we use Logit regression through GLM method, which limits the result bewteen -1 and 1.
fit_GLM = glm(N ~ log(DUR),
    family= binomial(link = "logit"))
summary(fit_GLM)
plot(DUR,N,ylim=c(0,1),lwd=2,xlab="DUR",
     ylab="P(N)",cex=.75)
ord = order(DUR)
lines(DUR[ord], fitted(fit_OLS)[ord], col="red", lwd=2)
lines(DUR[ord], fitted(fit_GLM)[ord], col="blue", lwd=2)

#Question3
SP = read.table("HW3-Q3.txt",header=TRUE)[,2]
SP
#(a)
Y = diff(SP,lag = 5)
Y = Y[61:1000]
X1 = diff(SP)
X1 = X1[60:999]
X2 = diff(SP,lag = 5)
X2 = X2[56:995]
X3 = diff(SP,lag = 15)
X3 = X3[46:985]
X4 = diff(SP,lag = 60)
X4 = X4[1:940]
train = as.data.frame(X1)
names(train)[1] <- "c1"
train$c2 = X2
train$c3 = X3
train$c4 = X4
head(train)
fit_loess <- loess(Y~c1+c2+c3+c4,train ,control = loess.control(surface = "direct"))

#(b)
y1 = diff(SP,lag = 5)
y1 = y1[1001:1254]
a1 = diff(SP)
a1 = a1[1000:1253]
a2 = diff(SP,lag = 5)
a2 = a2[996:1249]
a3 = diff(SP,lag = 15)
a3 = a3[986:1239]
a4 = diff(SP,lag = 60)
a4 = a4[941:1194]

test = as.data.frame(a1)
names(test)[1] <- "c1"
test$c2 = a2
test$c3 = a3
test$c4 = a4
dim(test)
yh = predict(fit_loess, test)
yh

#(c)
ss1 = sum((y1-mean(y1))^2)
ss1
ss2 = sum((yh-mean(yh))^2)
ss2
# model in (a) is better, it has smaller sse.