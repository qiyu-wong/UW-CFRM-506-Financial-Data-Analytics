#Question1
library(tseries)
library(xts)
library(aTSA)
library(rugarch)
library(forecast)
library(fDMA)
dat = read.table("HW6-Q1.txt", header=TRUE, sep = ",")
KO = xts(dat[,2], order.by=as.Date(dat[, 1]));names(KO) = c("KO")
r = 100*diff(log(KO))[-1]

#(a)
# Box-Ljung test for autocorrelation
print(Box.test(r,type = "Ljung-Box"))
# Engle test for ARCH
print(fDMA::archtest(as.vector(r)))

#Yes. Box-Pierce Test for autocorrelation shows p-value = 2e-4< 0.001. And Engle test for ARCH effect 
#shows p-value < 2.2e-16 which inidicates it has significant both autocorrelation and ARCH effect.

#(b)
spec = ugarchspec(variance.model = list(model = "sGARCH", 
                                        garchOrder = c(1, 1)),  
                  mean.model    = list(armaOrder = c(1, 1)),
                  distribution.model = "norm")

garch = ugarchfit(spec = spec, data = r, solver.control = list(trace=0))
print(garch)

#rt = 0.058 + 0.874rt-1 - 0.897¦Åt-1
#ut-1 = ¦Òt-1*¦Åt-1
#¦Òt^2 = 0.039 + 0.099(ut-1)^2 + 0.868(¦Òt-1)^2
#¦Åt-1 ¡« N(0, 1)

#All coefficient are significant at level of 0.05. With likelihood value -3895. All three test: 
#Weighted Ljung-Box Test on Standardized/Squared Residuals, Weighted ARCH LM Tests gives us 
#insignificant results which suggest the series has no auto-correlation and ARCH effec remaining in residuals.

#However, the Adjusted Pearson Goodness-of-Fit Test gives us significant p-values to reject the null 
#hypothesis that residual follows normal distribution. Therefore, a new assumption of distribution is needed.

#(c)
spec = ugarchspec(variance.model = list(model = "sGARCH", 
                                        garchOrder = c(1, 1)),  
                  mean.model    = list(armaOrder = c(1, 1)),
                  distribution.model = "std")

garch = ugarchfit(spec = spec, data = r, solver.control = list(trace=0))
print(garch)

#rt = 0.063 + 0.906rt-1 - 0.925¦Åt-1
#ut-1 = ¦Òt-1*¦Åt-1
#¦Òt^2 =  0.019 + 0.076(ut-1)^2 + 0.912(¦Òt-1)^2
#¦Åt-1 ¡« T(0, 4.816)

#All coeffcient above are significant at level 0.05 with estimated degree of freedom in student-t distribution
#¦Í = 4.816. The log-likelihood value -3786 > -3895 (normal distributed) shows it is slightly bettern than
#normal distributed assumptions.

#Akaike information is 2.72 < 2.79 (normal distributed) also suggest the model is still underfitted.

#All 3 tests: Weighted Ljung-Box Test on Standardized/Squared Residuals, Weighted ARCH LM Tests gives us
#insignificant results which suggest the series has no auto-correlation and ARCH effec remaining in residuals.

#Adjusted Pearson Goodness-of-Fit Test on different group gives us insignificant values which suggest student-t
#distribution is good enough to fit the residual distribution.

#(d)
spec = ugarchspec(variance.model = list(model = "sGARCH", 
                                        garchOrder = c(1, 1)),  
                  mean.model    = list(armaOrder = c(1, 1)),
                  distribution.model = "sstd")

garch = ugarchfit(spec = spec, data = r, solver.control = list(trace=0))
print(garch)

#rt = 0.053 + 0.901rt-1 - 0.922¦Åt-1
#ut-1 = ¦Òt-1*¦Åt-1
#¦Òt^2 =  0.020 + 0.077(ut-1)^2 + 0.910(¦Òt-1)^2
#¦Åt-1 ¡« SkewT(0, 4.83, 0.95)

#All coeffcient above are significant at level 0.05 with estimated skewness k = 0.95 and ¦Í = 4.83. The coefficient
#is very similar to student t distribution and log-likelihood is -3785 > -3796 (normal student-t) with same
#Akaike information. It suggest the two models are more or less similar.

#All 3 tests: Weighted Ljung-Box Test on Standardized/Squared Residuals, Weighted ARCH LM Tests gives us
#insignificant results which suggest the series have no auto-correlation and ARCH effec remaining in residuals.

#Adjusted Pearson Goodness-of-Fit Test on different group gives us insignificant values which suggests
#skewed-student-t distribution is good enough to fit the residual distribution.

#The estimation of skewness is 0.95 which suggest the skew of residuals is significant but small.

#(e)
spec = ugarchspec(variance.model=list(model = 'sGARCH',
                                      garchOrder=c(1,1)),
                  mean.model=list(armaOrder=c(0,0),
                                  archm=TRUE,
                                  archpow=1),
                  distribution.model = 'sstd')
garch = ugarchfit(spec = spec, data = r, solver.control = list(trace=0))
print(garch)

#rt = 0.053 + 0.047¦Òt + at
#ut-1 = ¦Òt-1*¦Åt-1
#¦Òt^2 = 0.020 + 0.078(ut-1)^2 + 0.91(¦Òt-1)^2
#¦Åt-1 ¡«  SkewT(0, 4.82, 0.96)

#All coeffcient above are significant at level 0.05 with estimated skewness k = 0.96 and ¦Í = 4.82. Similar to
#previous model, Weighted Ljung-Box Test on Standardized/Squared Residuals, Weighted ARCH LM Tests
#and Adjusted Pearson Goodness-of-Fit Test did not return significant p-value that suggests the assumptions
#of model are satisfied.

#According to the interpretation of the model, delta = 0.047 which is the daily risk premium, It is significant
#larger than 0.

#(f)
spec = ugarchspec(variance.model=list(model="apARCH",
                                      garchOrder=c(1,1)),
                  mean.model=list(armaOrder=c(1,1)),
                  distribution.model = "sstd")
garch = ugarchfit(spec = spec, data = r, solver.control = list(trace=0))
print(garch)

#rt = 0.004 + 0.86rt-1 - 0.88¦Åt-1
#ut-1 = ¦Òt-1*¦Åt-1
#¦Òt^0.714 = 0.026 +  0.091(|ut-1| - 0.371ut-1)^0.714 + 0.908(¦Òt-1)^0.714
#¦Åt-1 ¡« ¡« SkewT(0, 5.30, 0.94)

#With similar estimation of skewness k = 0.94 and ¦Í = 5.30. The power term ¦Ä = 0.714. Similar to previous
#model, Weighted Ljung-Box Test on Standardized/Squared Residuals, Weighted ARCH LM Tests and
#Adjusted Pearson Goodness-of-Fit Test did not return significant p-value that suggests the assumptions of
#model are satisfied.

#A significant ¦Ã = 0.371 > 0 indicates the leverage effect exist. Negative past values of at increase volatility
#more than positive past values of the same magnitude.

#Question2

#(a)
dat = read.table("HW5-Q4.txt",header=T)
s = dat[,1]; f=  dat[,2]
x = diff(s,1)
y = diff(f,1)

# Auto fit arima as last homework
fit_arimax = auto.arima(y,max.p=20, xreg = x, max.q = 20, max.d=3,ic="bic")
# get the predicted value
y_pred = fitted(fit_arimax)
# direct input y
spec = ugarchspec(variance.model=list(model="sGARCH",
                                      external.regressors = matrix(x^2),
                                      garchOrder=c(1,1)),
                  mean.model=list(armaOrder=c(0,0),
                                  external.regressors = matrix(y_pred^2)),
                  fixed.pars = list(mu = 0, mxreg1 = 1),
                  distribution.model = "norm")
garch = ugarchfit(spec = spec, data = y, solver.control = list(trace=0))
print(fit_arimax)
print(garch)

#Since ARIMAX and GARCH are separate models for mean/variance regression. We use the result of
#auto.arima() function which gives us p = 1,q= 1,d =0. Then the fitted problem becomes:
    
#yt = 0.656yt-1 - 0.555¦Åt-1 + 0.225xt
#ut-1 = ¦Òt-1*¦Åt-1
#¦Òt^2 = 0.0 + 0.933(¦Òt-1)^2 + 0.0xt^2
#¦Åt-1 ¡«  N(0, 1)

#Most parameters are significant exclude ¦Î and ¦Ø. ¦Î = 0 suggest the variance of y have no relation with
#predictor x.

#All 3 tests: Weighted Ljung-Box Test on Standardized/Squared Residuals, Weighted ARCH LM Tests gives us
#insignificant results which suggest the series have no auto-correlation and ARCH effec remaining in residuals.

#However, the Adjusted Pearson Goodness-of-Fit Test gives us significant p-values to reject the null hypothesis
#that residual follows normal distribution.

#(b)
spec = ugarchspec(variance.model=list(model="apARCH",
                                      external.regressors = matrix(x^2),
                                      garchOrder=c(1,1)),
                  mean.model=list(armaOrder=c(0,0),
                                  external.regressors = matrix(y_pred^2)),
                  fixed.pars = list(mu = 0, mxreg1 = 1),
                  distribution.model = "norm")
garch = ugarchfit(spec = spec, data = y, solver.control = list(trace=0))
print(fit_arimax)
print(garch)

#To answer the question, we perform an APACH to conduct the similar analysis on last problem. The output
#model is similar to the one in (a) with: ¦Ø = 0, ¦Á = 0.052, ¦Â = 0.91, ¦Î = 0 and ¦Ä = 2.5. Which is:

#yt = 0.656yt-1 - 0.555¦Åt-1 + 0.225xt
#ut-1 = ¦Òt-1*¦Åt-1
#¦Òt^2.5 = 0.0 + 0.052(|ut-1| - 0.046ut-1)^2.5 + 0.91(¦Òt-1)^2.5 + 0.0xt^2
#¦Åt-1 ¡«  SkewT(0, 5.30, 0.94)

#All 3 tests: Weighted Ljung-Box Test on Standardized/Squared Residuals, Weighted ARCH LM Tests gives
#us insignificant results. But the Adjusted Pearson Goodness-of-Fit Test gives us significant p-values to reject
#the null hypothesis that residual follows normal distribution.

#The leverage effect parameter ¦Ã = 0.046 > 0. It is not significant (p-value =0.08) which suggest a very weak
#contribution of negative shock to volatility.

#Question3

#(a)
# read data
dat=read.table("HW6-Q3.txt", header=TRUE, sep = ",")
VALE=xts(dat[,2], order.by=as.Date(dat[, 1]))
BHP=xts(dat[,3], order.by=as.Date(dat[, 1]))
names(VALE) = c("VALE"); names(BHP) = c("BHP")
p1 = log(BHP); p2 = log(VALE)
# ADF test
for (i in 1:3){
    print(sprintf('P1, order %d',i))
    adf.test(diff(p1,1,i))
    print(sprintf('P2, order %d',i))
    adf.test(diff(p2,1,i))
}
#By performe an Augmented-Dickey-Fuller (ADF) test for the two series. It is obviouse that the level 0 is not
#stationary for both p1 and p2. However, if we take the 1st differentiate, the ADF test returns the insignificant
#p value (DF¦Ã > 25 and p >= 0.99) for all three types of series.
#Therefore, the order of integration is 1 for both p1 and p2.

#(b)
#Step 1: OLS regression get relation
mdl = lm(p1 ~ p2)
summary(mdl)
# Step 2: ADF test
z = p1 - fitted(mdl) 
adf.test(z)

#The first step gives us relation: p1 = 1.82 + 0.72p2 + zt
#And second step gives us DF¦Ã > 6 and p >= 0.99. This suggest that the resulting zt series is stationary
#Finally we have the cointegrating relation: zt = p1 - 0.72p2 - 1.82

#(c)
#The mean value of zt is 0 (or ¦Á = 1, 82 if we define zt' = ¦Á+zt. 
#The standard deviation is 0.044 (the regression RSME)

#Question4

#(a)

#No. Because from Engle two step approaches. We can uniquely estimated the parmeter ¦Ç = ¦Á¦Â^T with OLS.
#However, for unique ¦Ç, the decomposition is not unique because cointegration vector ¦Â is not unique.
#To ensure the result is unqiue. We need to normalized the ¦Â to make sure one of the rows is 1.


#(b)
Stock_FX_Bond = read.csv("HW6-Q4.csv", header=T)
adjClose = Stock_FX_Bond[,seq(from=3, to=13, by=2)]

library(urca)
summary(ca.jo(adjClose))

#No.
#Because if all closing prices are stationary, the number of stationary linear combinations r should equals
#to the number of stocks d which gives us a significant p-value to reject the null hypothesis at r = d = 5.
#However, the result shows r is can not reject the NULL hypothesis when r ¡Ü 5. Therefore, there exist at
#least one closing price variable which is not stationary.

#(c)

#On the level of significant level p < 0.05, it accepts that r is lessthan or equal to 2 but rejects that r is
#less than or equal to 1. So the order r = 2. There exist 2 cointegrated relations. Only the eigenvector
#corresponding to the largest 2 eigenvalues are meaningful.

#(d)

#                     {-0.40  -1.02  -5.82  -1.15 -0.08 -0.03}   {1.00   1.00}
#     (GMt)           {-0.37  0.59  -1.54  0.44 -0.07 0.03}      {4.53  -0.96}   (GMt-1)
#delta(   )   =0.001 *{-0.61  -0.22  -1.72  -0.25 0.12 0.11}   * {10.15 -1.04} * (     ) + ¦Åt
#     (GMt)           {0.09  -3.89  -1.94  0.60  0.02  0.09}     {-6.28  0.88}   (Ft-1 )
#                     {-0.11  -0.58  0.38  -1.53  -0.17  0.21}   {3.49   0.60}
#                     {0.59  2.14  -1.58  -0.35  -0.02  0.14}    {-12.69 -0.87}

#(e)
#We can perform in statistical arbitrage on cointegration pairs. The error of cointegrated pairs is related with
#each other which means on would very likely to detect signals between the correlation of two stocks and
#create signal of buy and sell.

