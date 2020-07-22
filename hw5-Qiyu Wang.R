
#Question1

#(c)
library(xts)
library(forecast)
dat=read.table("HW4-Q4.txt", header=TRUE, sep = ",")
P=xts(dat[,2], order.by=as.Date(dat[, 1]));names(P) = c("Oil")
r = diff(P)[-1]
r1 = r[1:251,]
# AR(1) fit
ARfit = arima(r1, order = c(1,0,0))
# MA(2) fit 
MAfit = auto.arima(r1, max.p=0, max.q = 20, d=0, ic="bic")

ar_12 = predict(ARfit,12)
ma_12 = predict(MAfit,12)

ts.plot(r1,  type='l',
        main="one-year forecast value from AR(1)")
lines(ts(ar_12$pred, star=252, frequency=1), type='p', lty=1, pch='*')
lines(ts(ar_12$pred+1.96*ar_12$se, star=252, frequency=1), type='l', lty=2, col=2)
lines(ts(ar_12$pred-1.96*ar_12$se, , star=252, frequency=1), type='l', lty=2, col=2)
legend("topleft",c("data","predictions","lower CL","upper CL"),cex=1.2,
       box.lty=1,pch=c(NA,"*",NA,NA),lty=c(1,NA,2,2), col=c(1,1,2,2))

ts.plot(r1,  type='l',
        main="one-year forecast value from MA(2)")
lines(ts(ma_12$pred, star=252, frequency=1), type='p', lty=1, pch='*')
lines(ts(ma_12$pred+1.96*ma_12$se, star=252, frequency=1), type='l', lty=2,col=2)
lines(ts(ma_12$pred-1.96*ma_12$se, , star=252, frequency=1), type='l', lty=2,col=2)
legend("topleft",c("data","predictions","lower CL","upper CL"),cex=1.2,
       box.lty=1,pch=c(NA,"*",NA,NA),lty=c(1,NA,2,2), col=c(1,1,2,2))

#(b)
library(xts)
library(dplyr)

dat=read.table("HW4-Q4.txt", header=TRUE, sep = ",")
P=xts(dat[,2], order.by=as.Date(dat[, 1]));names(P) = c("Oil")
r = diff(P)[-1]
ar_err_all <- data.frame()
ma_err_all <- data.frame()
for(i in seq(0,118)){
    r1 = r[(251 + i):372,]
    # AR(1) fit
    ARfit = arima(r, order = c(1,0,0))
    
    # MA(2) fit 
    MAfit = auto.arima(r, max.p=0, max.q = 20, d=0, ic="bic")
    
    ar_12 = predict(ARfit, 3)
    ma_12 = predict(MAfit, 3)  
    
    RMSE_ar_1 = (ar_12$pred[1] - as.numeric( r[252 + i,1]))^2
    RMSE_ar_2 = (ar_12$pred[2] - as.numeric( r[253 + i,1]))^2
    RMSE_ar_3 = (ar_12$pred[3] - as.numeric( r[254 + i,1]))^2
    ar_err = data.frame(ar1 = RMSE_ar_1,
                        ar2 = RMSE_ar_2,
                        ar3 = RMSE_ar_3)
    ar_err_all <- rbind(ar_err_all, ar_err)
    RMSE_ma_1 = (ma_12$pred[1] - as.numeric( r[252 + i,1]))^2
    RMSE_ma_2 = (ma_12$pred[2] - as.numeric( r[253 + i,1]))^2
    RMSE_ma_3 = (ma_12$pred[3] - as.numeric( r[254 + i,1]))^2
    ma_err = data.frame(ma1 = RMSE_ma_1,
                        ma2 = RMSE_ma_2,
                        ma3 = RMSE_ma_3)
    ma_err_all <- rbind(ma_err_all, ma_err)
}

rmse_1_ar = sqrt(sum(ar_err_all$ar1) * (1/(372 - 1- 251 + 1) ))
print(paste0("RMSE_AR_1 is ", rmse_1_ar))
rmse_2_ar = sqrt(sum(ar_err_all$ar2) * (1/(372 - 2- 251 + 1) ))
print(paste0("RMSE_AR_2 is ", rmse_2_ar))
rmse_3_ar = sqrt(sum(ar_err_all$ar3) * (1/(372 - 3- 251 + 1) ))
print(paste0("RMSE_AR_3 is ", rmse_3_ar))
rmse_1_ma = sqrt(sum(ma_err_all$ma1) * (1/(372 - 1- 251 + 1) ))
print(paste0("RMSE_MA_1 is ", rmse_1_ma))
rmse_2_ma = sqrt(sum(ma_err_all$ma2) * (1/(372 - 2- 251 + 1) ))
print(paste0("RMSE_MA_2 is ", rmse_2_ma))
rmse_3_ma = sqrt(sum(ma_err_all$ma3) * (1/(372 - 3- 251 + 1) ))
print(paste0("RMSE_MA_3 is ", rmse_3_ma))

#The "RMSE_MA_3 is 6.7299654125249" which is the lowest.The MA(2) model with 3-steps ahead forcast 
#are the best model based on the RMSE.

#Question2

#(a)
library(xts)
library(quantmod)
HS = getSymbols("HSN1FNSA",src="FRED", auto.assign=FALSE)
HS = ts(as.numeric(HS), start = c(1963,1), frequency = 12)
library(fracdiff)
d=1
fdiff = diff(HS, differences=1)
par(mfrow=c(1,2));par(mar=c(3,3,3,3))
ts.plot(fdiff, main=paste("frac. diff. house-sold data with d = ", d))
acf(as.numeric(fdiff))
seasonalACF = function(ts, slag, name){
    par(mfrow=c(2,2),mar=c(2,3,4,4))
    acf(as.numeric(ts), main=paste("sample ACF of ", name))
    acf(as.numeric(diff(ts)), main="first difference")
    acf(as.numeric(diff(ts,slag)), main="seasonal difference")
    acf(as.numeric(diff(diff(ts),slag)), main="regular+seasonal differene")
}
seasonalACF(HS, 12,"house-sold" )

#The periodicity of dHS could be 12 based on the plot. First, we set the differnce is 1, then we 
#got this ACF plot. We notice that not all are bounded within the range, but after trying many 
#options of d, this could be one of the best situation.  Based on the plot, the periodicity of dHS 
#could be 12.  And then we set periodicity is 12 to check the acf plot for the seasonal difference, 
#then we can see that it goes slowly into the band, which removes the periodicity in the seasonal 
#difference.

#(b)
library(xts)
library(quantmod)
HS = getSymbols("HSN1FNSA",src="FRED", auto.assign=FALSE)
HS = ts(as.numeric(HS), start = c(1963,1), frequency = 12)

## air line model
hs_airline = arima(HS, order=c(0,1,1),
                   seasonal=list(order=c(0,1,1), period=12))
hs_airline

#The model is:
#(1???B)(1???Bs)rt=(1-0.223B)(1-0.8722Bs)¦Åt
#¦Åt¡«weak WN(0,20.68)

bic_hs_model =AIC(hs_airline,k = log(length(HS)))
bic_hs_model
#The model's BIC is 3979.349

par(mar=c(2,3,4,4))
tsdiag(hs_airline,gof=12)

# Next, we check the residuals for constant volatility and normality.
tsdiag_vol = function(model, lag, fitdf, span=0.5){
    par(mfrow=c(1,2));par(mar=c(3,3,3,3))
    plot(resid(model)**2, type="l", col=1,
         main = expression(residual^2))
    smoother = loess((resid(model)**2) ~ seq(1,length(resid(model))),
                     span=span)
    lines(seq(1,length(resid(model))),fitted(smoother),col=2)
    acf((resid(model)**2), main=expression("sample ACF of "~ residual^2))
    Box.test(resid(model)**2, lag = lag, type = "Ljung-Box", fitdf = fitdf)
}
tsdiag_norm = function(model){
    qqnorm(resid(model), datax = FALSE,
           xlab = "normal quantile",
           ylab = "sample quantile of residuals",
           main = "normal probability plot for residuals")
    qqline(resid(model), datax=FALSE, col = 2)
    shapiro.test(resid(model))
}
tsdiag_vol(hs_airline, lag=12, fitdf=2)
tsdiag_norm(hs_airline)

hs_pred = predict(hs_airline,12)

ts.plot(HS,  type='l',
        main="HS data", ylab="")
lines(ts(hs_pred$pred, start=2020, frequency=12), type='p', lty=1, pch='*')
lines(ts(hs_pred$pred+1.96*hs_pred$se, start=2020, frequency=12), type='l', lty=2,col=2)
lines(ts(hs_pred$pred-1.96*hs_pred$se, start=2020, frequency=12), type='l', lty=2,col=2)
legend("topleft",c("data","predictions","lower CL","upper CL"),cex=1.2,
       box.lty=1,pch=c(NA,"*",NA,NA),lty=c(1,NA,2,2), col=c(1,1,2,2))

#From the first three plots, we notice that the standardized Residuals looks normally distributed,
#the ACF of the residuals can be bounded within the range, and the p-values are smaller when x-axis
#goes further. Moreover, residuals are normally distributed based on qq plot.The model is adequate.

#(c)
library(xts)
library(quantmod)
HS = getSymbols("HSN1FNSA",src="FRED", auto.assign=FALSE)
HS = ts(as.numeric(HS), start = c(1963,1), frequency = 12)
HS_new = diff(HS, differences=1)
## air line model
hs_airline_new = auto.arima(HS_new, ic="bic",
                            max.p=20, max.q = 20, max.d=20,
                            max.P=20, max.Q = 20, max.D=20)
hs_airline_new

par(mar=c(2,3,4,4))
tsdiag(hs_airline_new,gof=12)

# Next, we check the residuals for constant volatility and normality.
tsdiag_vol = function(model, lag, fitdf, span=0.5){
    par(mfrow=c(1,2));par(mar=c(3,3,3,3))
    plot(resid(model)**2, type="l", col=1,
         main = expression(residual^2))
    smoother = loess((resid(model)**2) ~ seq(1,length(resid(model))),
                     span=span)
    lines(seq(1,length(resid(model))),fitted(smoother),col=2)
    acf((resid(model)**2), main=expression("sample ACF of "~ residual^2))
    Box.test(resid(model)**2, lag = lag, type = "Ljung-Box", fitdf = fitdf)
}
tsdiag_norm = function(model){
    qqnorm(resid(model), datax = FALSE,
           xlab = "normal quantile",
           ylab = "sample quantile of residuals",
           main = "normal probability plot for residuals")
    qqline(resid(model), datax=FALSE, col = 2)
    shapiro.test(resid(model))
}
tsdiag_vol(hs_airline_new, lag=12, fitdf=2)
tsdiag_norm(hs_airline_new)

hs_pred_new = predict(hs_airline_new,12)

ts.plot(HS_new,  type='l',
        main="Delta HS data", ylab="")
lines(ts(hs_pred_new$pred, start=2020, frequency=12), type='p', lty=1, pch='*')
lines(ts(hs_pred_new$pred+1.96*hs_pred_new$se, start=2020, frequency=12), type='l', lty=2,col=2)
lines(ts(hs_pred_new$pred-1.96*hs_pred_new$se, start=2020, frequency=12), type='l', lty=2,col=2)
legend("topleft",c("data","predictions","lower CL","upper CL"),cex=1.2,
       box.lty=1,pch=c(NA,"*",NA,NA),lty=c(1,NA,2,2), col=c(1,1,2,2))
#Simialr to the last subquestion, the QQ plot shows that the residual is normal distributed,
#and it don't seem to have volatility issues.The model is adequate.

#(d)
library(xts)
library(quantmod)

# From the previous steps, the HS airline model's BIC is 3979.349.
# The Delta HS model's BIC is 4109.85.
hs1_err_all <- data.frame()
hs2_err_all <- data.frame()

for(i in seq(1,214)){
  j = i%%12
  k = floor(i/12)
  HS = getSymbols("HSN1FNSA",src="FRED", auto.assign=FALSE)
  HS = ts(as.numeric(HS), start = c((2000+k), (1+j)), end = c(2018, 11), frequency = 12)
  HS_diff = diff(HS, differences=1)

  data1 = HS
  data2 = HS_diff

  # model 1
  hs_airline

  # model 2
  hs_airline_new

  hs_12 = predict(hs_airline, 12)
  hs_new_12 = predict(hs_airline_new, 12)
  data1 = as.data.frame(HS)
  RMSE_hs_1 = (hs_12$pred[1] - as.numeric( data1[i,1]))^2
  RMSE_hs_2 = (hs_12$pred[2] - as.numeric( data1[i+1,1]))^2
  RMSE_hs_3 = (hs_12$pred[3] - as.numeric( data1[i+2,1]))^2
  RMSE_hs_4 = (hs_12$pred[1] - as.numeric( data1[i+3,1]))^2
  RMSE_hs_5 = (hs_12$pred[2] - as.numeric( data1[i+4,1]))^2
  RMSE_hs_6 = (hs_12$pred[3] - as.numeric( data1[i+5,1]))^2
  RMSE_hs_7 = (hs_12$pred[1] - as.numeric( data1[i+6,1]))^2
  RMSE_hs_8 = (hs_12$pred[2] - as.numeric( data1[i+7,1]))^2
  RMSE_hs_9 = (hs_12$pred[3] - as.numeric( data1[i+8,1]))^2
  RMSE_hs_10 = (hs_12$pred[1] - as.numeric( data1[i+9,1]))^2
  RMSE_hs_11 = (hs_12$pred[2] - as.numeric( data1[i+10,1]))^2
  RMSE_hs_12 = (hs_12$pred[3] - as.numeric( data1[i+11,1]))^2

  hs_err = data.frame(ar1 = RMSE_hs_1,
                      ar2 = RMSE_hs_2,
                      ar3 = RMSE_hs_3,
                      ar4 = RMSE_hs_4,
                      ar5 = RMSE_hs_5,
                      ar6 = RMSE_hs_6,
                      ar7 = RMSE_hs_7,
                      ar8 = RMSE_hs_8,
                      ar9 = RMSE_hs_9,
                      ar10 = RMSE_hs_10,
                      ar11 = RMSE_hs_11,
                      ar12 = RMSE_hs_12)

  hs1_err_all <- rbind(hs1_err_all, hs_err)

  data2 = as.data.frame(HS_diff)
  RMSE_hs_new_1 = (hs_new_12$pred[1] - as.numeric( data2[i,1]))^2
  RMSE_hs_new_2 = (hs_new_12$pred[2] - as.numeric( data2[i+1,1]))^2
  RMSE_hs_new_3 = (hs_new_12$pred[3] - as.numeric( data2[i+2,1]))^2
  RMSE_hs_new_4 = (hs_new_12$pred[1] - as.numeric( data2[i+3,1]))^2
  RMSE_hs_new_5 = (hs_new_12$pred[2] - as.numeric( data2[i+4,1]))^2
  RMSE_hs_new_6 = (hs_new_12$pred[3] - as.numeric( data2[i+5,1]))^2
  RMSE_hs_new_7 = (hs_new_12$pred[1] - as.numeric( data2[i+6,1]))^2
  RMSE_hs_new_8 = (hs_new_12$pred[2] - as.numeric( data2[i+7,1]))^2
  RMSE_hs_new_9 = (hs_new_12$pred[3] - as.numeric( data2[i+8,1]))^2
  RMSE_hs_new_10 = (hs_new_12$pred[1] - as.numeric( data2[i+9,1]))^2
  RMSE_hs_new_11 = (hs_new_12$pred[2] - as.numeric( data2[i+10,1]))^2
  RMSE_hs_new_12 = (hs_new_12$pred[3] - as.numeric( data2[i+11,1]))^2

  hs_new_err = data.frame(ar1 = RMSE_hs_new_1,
                      ar2 = RMSE_hs_new_2,
                      ar3 = RMSE_hs_new_3,
                      ar4 = RMSE_hs_new_4,
                      ar5 = RMSE_hs_new_5,
                      ar6 = RMSE_hs_new_6,
                      ar7 = RMSE_hs_new_7,
                      ar8 = RMSE_hs_new_8,
                      ar9 = RMSE_hs_new_9,
                      ar10 = RMSE_hs_new_10,
                      ar11 = RMSE_hs_new_11,
                      ar12 = RMSE_hs_new_12)

  hs2_err_all <- rbind(hs2_err_all, hs_new_err)
}

sum1 <- function(x){sum(x,na.rm = T)}

rmse_1_hs = sqrt(sum1(hs1_err_all$ar1) * (1/(215-1) ))
rmse_2_hs = sqrt(sum1(hs1_err_all$ar2) * (1/(215-2) ))
rmse_3_hs = sqrt(sum1(hs1_err_all$ar3) * (1/(215-3) ))
rmse_4_hs = sqrt(sum1(hs1_err_all$ar4) * (1/(215-4) ))
rmse_5_hs = sqrt(sum1(hs1_err_all$ar5) * (1/(215-5) ))
rmse_6_hs = sqrt(sum1(hs1_err_all$ar6) * (1/(215-6) ))
rmse_7_hs = sqrt(sum1(hs1_err_all$ar7) * (1/(215-7) ))
rmse_8_hs = sqrt(sum1(hs1_err_all$ar8) * (1/(215-8) ))
rmse_9_hs = sqrt(sum1(hs1_err_all$ar9) * (1/(215-9) ))
rmse_10_hs = sqrt(sum1(hs1_err_all$ar10) * (1/(215-10) ))
rmse_11_hs = sqrt(sum1(hs1_err_all$ar11) * (1/(215-11) ))
rmse_12_hs = sqrt(sum1(hs1_err_all$ar12) * (1/(215-12) ))

rmse_1_hs_new = sqrt(sum1(hs2_err_all$ar1) * (1/(215-1) ))
rmse_2_hs_new = sqrt(sum1(hs2_err_all$ar2) * (1/(215-2) ))
rmse_3_hs_new = sqrt(sum1(hs2_err_all$ar3) * (1/(215-3) ))
rmse_4_hs_new = sqrt(sum1(hs2_err_all$ar4) * (1/(215-4) ))
rmse_5_hs_new = sqrt(sum1(hs2_err_all$ar5) * (1/(215-5) ))
rmse_6_hs_new = sqrt(sum1(hs2_err_all$ar6) * (1/(215-6) ))
rmse_7_hs_new = sqrt(sum1(hs2_err_all$ar7) * (1/(215-7) ))
rmse_8_hs_new = sqrt(sum1(hs2_err_all$ar8) * (1/(215-8) ))
rmse_9_hs_new = sqrt(sum1(hs2_err_all$ar9) * (1/(215-9) ))
rmse_10_hs_new = sqrt(sum1(hs2_err_all$ar10) * (1/(215-10) ))
rmse_11_hs_new = sqrt(sum1(hs2_err_all$ar11) * (1/(215-11) ))
rmse_12_hs_new = sqrt(sum1(hs2_err_all$ar12) * (1/(215-12) ))

#rmse_9_hs_new is the smallest, the value is 4.64641625590516
#The Delta HS's model, which is ` ARIMA(1,0,1)(3,0,0)[12] with zero mean for 9 steps ahead is the 
#best model. Because its RMSE is the lowest.

#Question3

#(a)
dat = read.table("HW5-Q3.txt",header=T)
rtn = xts(dat[,2], order.by=as.Date(as.character(dat[, 1]),"%Y%m%d"))
a = abs(rtn)

library(tseries)
adf.test(a)
pp.test(a)
kpss.test(a)
#All the p-values are smaller than 0.05, then it seems stationary.

library(fracdiff)
d= 0.2
fda = diffseries(a,d)
par(mfrow=c(1,2));par(mar=c(3,3,3,3))
ts.plot(fda, main=paste("frac. diff. with d = ", d))
acf(as.numeric(fda))
#First, based on the ADF, PP and KPSS test, all the p-values are smaller than 0.05, which means 
#the null-hypothsis cannot be rejected evidently. Therefore, it is stationary.
#With delta equal to 0.2 (which we checked, and it will be 0.1715568 from the next question), 
#it seems to be a weak WN. It has a long memory.

#(b)
dat = read.table("HW5-Q3.txt",header=T)
rtn = xts(dat[,2], order.by=as.Date(as.character(dat[, 1]),"%Y%m%d"))
a = abs(rtn)
fracdiff(a,nar=0,nma=0)
# The estimated fractional difference is  0.1715568 

library("forecast")
d=0.1715568
fdiff = diffseries(a,d)
farima = auto.arima(as.numeric(fdiff), max.p=20, max.q = 20, max.d=0, ic="bic")
farima

#¦¤It=¦Åt-0.0702¦Åt???1,
#¦Åt¡«weak WN(0,0.0001305).

par(mar=c(2,3,4,4))
tsdiag(farima,gof=12)

# Next, we check the residuals for constant volatility and normality.
tsdiag_vol = function(model, lag, fitdf, span=0.5){
    par(mfrow=c(1,2));par(mar=c(3,3,3,3))
    plot(resid(model)**2, type="l", col=1,
         main = expression(residual^2))
    smoother = loess((resid(model)**2) ~ seq(1,length(resid(model))),
                     span=span)
    lines(seq(1,length(resid(model))),fitted(smoother),col=2)
    acf((resid(model)**2), main=expression("sample ACF of "~ residual^2))
    Box.test(resid(model)**2, lag = lag, type = "Ljung-Box", fitdf = fitdf)
}
tsdiag_norm = function(model){
    qqnorm(resid(model), datax = FALSE,
           xlab = "normal quantile",
           ylab = "sample quantile of residuals",
           main = "normal probability plot for residuals")
    qqline(resid(model), datax=FALSE, col = 2)
    #  shapiro.test(resid(model))
}
tsdiag_vol(farima, lag=1, fitdf=1, span=0.25)
tsdiag_norm(farima)
a_pred = predict(farima,10)
model_a <- a %>% as.data.frame() %>% select(V1) %>% mutate_all(as.numeric) %>% sample_n(5000)
shapiro.test(model_a$V1)
ts.plot(a,  type='l',
        main="Abs IBM stocka", ylab="")
lines(ts(a_pred$pred, start=9845, frequency=1), type='p', lty=1, pch='*')
lines(ts(a_pred$pred+1.96*a_pred$se, start=9845, frequency=1), type='l', lty=2,col=2)
lines(ts(a_pred$pred-1.96*a_pred$se, start=9845, frequency=1), type='l', lty=2,col=2)
legend("topleft",c("data","predictions","lower CL","upper CL"),cex=1.2,
       box.lty=1,pch=c(NA,"*",NA,NA),lty=c(1,NA,2,2), col=c(1,1,2,2))

#The first three plots look good, since the ACF Residuals are bounded, also the p-values are colse 
#to 0. However, the QQ plot shows the residuals are not normally distributed, since the p value is 
#smaller than 0.05 which rejects the null hypothesis - the data is normally distributed. It probably
#is because the dataset is too large and our model is quite simple to catch every trend of the data.

#Question4

#(a)
library(ggplot2)

dat = read.table("HW5-Q4.txt",header=T)
dat <- dat %>% 
    select(-cost) %>% 
    mutate(row_number = row_number())
s = dat[,1];f = dat[,2]

y = diff(f,1)
x = diff(s,1)

par(mfrow=c(1,2));par(mar=c(4,4,4,4))
ggplot(dat)+
    geom_line(aes(row_number, lnfuture, color = "lnfuture"))+
    geom_line(aes(row_number, lnspot, color = "lnspot"))
plot(dat, legend.loc='topright')
plot(x,y,xlab="x", ylab="y")
cor(x,y)

#Based on the correlation test, x and y are correlated, the correlation is 0.3884863.


#(b)
fit_ols = lm(y~x)
summary(fit_ols)

par(mfrow=c(1,2));par(mar=c(3,3,3,3))
plot(resid(fit_ols), type="l")
acf(resid(fit_ols))
Box.test(resid(fit_ols), lag=10, fitdf=1)

library(tseries)
adf.test(resid(fit_ols))
pp.test(resid(fit_ols))
kpss.test(resid(fit_ols))

#After checking the results from ADF, PP and KPSS tests, all the p-values are smaller than 0.05. 
#This regression model may not be spurious regression.

#(c)
fit_arimax = auto.arima(y, max.p=20, xreg = x, max.q = 20, max.d=3, ic="bic")
fit_arimax
#¦¤It=0.6557¦¤It???1+¦Åt-0.5549¦Åt???1 , where (¦Åt) is weak WN(0,¦Ò2¦Å=3.344e-08)

par(mar=c(2,3,4,4))
tsdiag(fit_arimax, gof=10)

# Next, we check the residuals for constant volatility and normality.
tsdiag_vol = function(model, lag, fitdf, span=0.5){
    par(mfrow=c(1,2));par(mar=c(3,3,3,3))
    plot(resid(model)**2, type="l", col=1,
         main = expression(residual^2))
    smoother = loess((resid(model)**2) ~ seq(1,length(resid(model))),
                     span=span)
    lines(seq(1,length(resid(model))),fitted(smoother),col=2)
    acf((resid(model)**2), main=expression("sample ACF of "~ residual^2))
    Box.test(resid(model)**2, lag = lag, type = "Ljung-Box", fitdf = fitdf)
}
tsdiag_norm = function(model){
    qqnorm(resid(model), datax = FALSE,
           xlab = "normal quantile",
           ylab = "sample quantile of residuals",
           main = "normal probability plot for residuals")
    qqline(resid(model), datax=FALSE, col = 2)
    # shapiro.test(resid(model))
}
model_fit_arimax <- fit_arimax$residuals %>% as.data.frame()  %>% mutate_all(as.numeric) %>% sample_n(5000)
shapiro.test(model_fit_arimax$x)

tsdiag_vol(fit_arimax, lag=1, fitdf=2, span=0.25)
tsdiag_norm(fit_arimax)

#From the forst three plots, the model seem adequate. But, the volatility is not constant and the 
#residuals are not normal. Also, from the QQ plot and the shapiro test, the residual is not nornally 
#distributed, which means the model still needs to be improved. (But this is what we are asked to do 
#according to the question.)
