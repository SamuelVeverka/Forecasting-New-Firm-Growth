# Forecasting New U.S. Firm Growth with VAR Models
Sam Veverka  
20 March 2017  



# Introduction

Entrepreneurship has been a popular topic of discussion in the United States for the past few decades. Much media attention has been directed at the perceived center of entrepreneurship, Silicon Valley. The attention makes sense. Stories of small, garage tech companies flourishing into multi-billion behemoths captures the imagination. Given the spotlight shown on Silicon Valley and its disrupters, one would assume that entrepreneurship, quantified in this post by the number of new establishments entering the economy, would be on the rise. The opposite is true. The creation of new businesses has been on the decline in the U.S. for thirty five years .

What drives new establishment growth, and hence entrepreneurship in the United States? Among the variables possibly explaining movement in new business growth, this post singles out two which I believe significant, consumer confidence and interest rates. Consumer confidence or sentiment indices track the attitudes of persons in the economy towards the current business and economic climate. If the index is low, persons are pessimistic about the economic future and theoretically less likely to start a business. Interest rates play a more obvious role in the creation of new business. If interest rates drop, loans become relatively less expensive for the borrower. Most entrepreneurs need substantial loans to start a business, so when interest rates are low, new business growth should be relatively high.

So if movements in new business growth can be explained by consumer confidence and interest rates, one should be able to forecast movements in business growth using consumer confidence and interest rates. This post, which was adapted from a class project, attempts to do that.

# Load Libraries


```r
#load packages
library(forecast)
library(vars)
library(tseries)
```

# Import Data and Convert to Time Series


There are not really good proxies for entrepreneurship outside of new business growth and for new business growth data sets are limited. The metric I use is the quarterly national establishment birth rate which is the number of newly created establishments as a percentage of total establishments. The rate of establishment birth is provided by the Bureau of Labor Statistics and has only been calculated since Q3 1992 to the present. Historical new business growth is the limiting factor in this model, so all data will be over the period of Q3 1992 to Q1 2016.

```r
#Establishment Birth - seasonally adjusted
est_birth_adj <- read.csv("Establishment_Birth_adj.csv",header=TRUE)
establishment_birth <- ts(data=est_birth_adj,start=c(1992,3),frequency=4) #ts function converts data to time series data, which is useful for VARs forecasting interprebility
```

Consumer confidence numbers are from the University of Michigan’s Consumer Sentiment Index. The index runs from 1952 to the current day and has a normalized value of 100 based on 1964 . The index is formed from data collected in telephone interviews meant to assess consumers’ attitudes on the future of the economy. The index is published monthly, but I have converted it to quarterly data by simple averaging

```r
#University of Michigan Consumer Sentiment index
CSI_1 <- read.csv("UMCSENT.csv",header=TRUE)
CSI <- ts(data=CSI_1,start=c(1992,3),frequency=4)
```


The 10-Year Treasury Constant Maturity Rate, provided by the Federal Reserve System, is used to measure the change in interest rates. The 10-Year Treasury Constant Maturity Rate is an index of the yields of all Treasury securities, normalized to the benchmark 10-year rate. Obviously, businesses will borrow at business loan rates, not the treasury rate, but movements in the treasury security rate mirror movements in business loan rates reasonably well.

```r
#10-Year Treasury Constant Maturity Rate
DGS10_1 <- read.csv("DGS10.csv",header=TRUE)
DGS10 <- ts(data=DGS10_1,start=c(1992,3),frequency=4)
```


# Methodology
VAR models will be used to recursively forecast new establishment growth. There are 3 different VAR specifications, two bivariate models and a trivariate model encompassing the bivariate models.  All models will have new establishment births as the dependent variable and lagged establishment births as an independent variable or variables. The bivariate models will be a combination of lagged establishment births with lagged consumer confidence and lagged interest rates. The trivariate model will include all three lagged values as independent variables.


```r
#Create 3 VAR system
var_1 <- ts(cbind(establishment_birth,CSI),start=c(1992,3),frequency=4) #Establishment Birth and consumer sentiment
var_2 <- ts(cbind(establishment_birth,DGS10),start=c(1992,3),frequency=4) #Establishment Birth and 10-Year Treasury Constant Maturity Rate
var_3 <- ts(cbind(establishment_birth,CSI, DGS10),start=c(1992,3),frequency=4) #stock returns, consumer sentiment and 10-Year Treasury Constant Maturity Rate
```

Once the model is specified, I will recursively forecast new establishment births. The period of 1992:Q3 – 2012:Q4 will be used for the sample, and the period of 2013:Q1 – 2016:Q1 will be forecasted with all three VAR models. I believe through 2012 is a good ending point for the sample space as it captures the Great Recession and most of the bounce back from it. The results will be compared amongst each other and against an AR(1) benchmark, which regresses new establishment births on lagged new establishment births. The forecasts will be compared with the appropriate nested and non-nested comparison tests.

# Specification Results


To find the correct specification, I start by checking to verify that the new establishment births series is covariance stationary via the Augmented Dickey Fuller test and the Phillips-Perron Unit Root Test.

```r
#Run Augmented Dickey-Fuller(ADF) and Phillips-Perron tests for unit root
pp.test(establishment_birth)
```

```
## Warning in pp.test(establishment_birth): p-value smaller than printed p-
## value
```

```
## 
## 	Phillips-Perron Unit Root Test
## 
## data:  establishment_birth
## Dickey-Fuller Z(alpha) = -36.588, Truncation lag parameter = 3,
## p-value = 0.01
## alternative hypothesis: stationary
```

```r
adf.test(establishment_birth)
```

```
## 
## 	Augmented Dickey-Fuller Test
## 
## data:  establishment_birth
## Dickey-Fuller = -2.2017, Lag order = 4, p-value = 0.4931
## alternative hypothesis: stationary
```
The two test produced conflicting results. The ADF had a p-value of .49, which implies non-stationarity, and the Phillips-Perron had a p-value of .01, which implies stationarity. Given these results, I did not first-difference establishment births, as change in the percentage of establishment births does not provide as much economic insight as change in the level set. 

It can be helpful to observe a plot of the time series which will make up the VAR models. If the series are plotted side-by-side, possible similarities or differences can become apparent before running any actual tests. In the plot provided below, the three series do move together. I would expect CSI and establishment births to move together, but not interest rates and establishment births.


```r
#Plot variables
layout(1)
plot(var_3,type="l",col="blue",main="Establishment Birth, CSI, and 10-Year Treasury")
```

![](http://i.imgur.com/7dvHgYL.png)<!-- -->

Following identifying the level of stationarity in the dependent variable, I can use model selection criteria to find the appropriate number of lagged independent variables to use in the three VAR system.


```r
# find optimal number of lags
info.crit1 <- VARselect(var_1,lag.max=4,type="const")
info.crit2 <- VARselect(var_2,lag.max=4,type="const")
info.crit3 <- VARselect(var_3,lag.max=4,type="const")

info.crit1
```

```
## $selection
## AIC(n)  HQ(n)  SC(n) FPE(n) 
##      1      1      1      1 
## 
## $criteria
##                 1          2          3          4
## AIC(n) -1.0899329 -1.0886688 -1.0877633 -1.0426955
## HQ(n)  -1.0231433 -0.9773528 -0.9319209 -0.8423267
## SC(n)  -0.9243817 -0.8127502 -0.7014772 -0.5460420
## FPE(n)  0.3362551  0.3367390  0.3371746  0.3529609
```

```r
info.crit2
```

```
## $selection
## AIC(n)  HQ(n)  SC(n) FPE(n) 
##      3      1      1      3 
## 
## $criteria
##                   1            2            3            4
## AIC(n) -6.104581000 -6.139326850 -6.162113532 -6.103101624
## HQ(n)  -6.037791412 -6.028010869 -6.006271159 -5.902732858
## SC(n)  -5.939029824 -5.863408223 -5.775827455 -5.606448095
## FPE(n)  0.002232723  0.002156853  0.002109078  0.002238825
```

```r
info.crit3
```

```
## $selection
## AIC(n)  HQ(n)  SC(n) FPE(n) 
##      1      1      1      1 
## 
## $criteria
##                  1           2           3           4
## AIC(n) -3.17458293 -3.14257570 -3.08715227 -3.02163496
## HQ(n)  -3.04100375 -2.90881214 -2.75320433 -2.58750264
## SC(n)  -2.84348058 -2.56314659 -2.25939639 -1.94555232
## FPE(n)  0.04181865  0.04321092  0.04575387  0.04900997
```

The BIC was uniformly lowest for all three VAR models at 1 lag. Given the number of lags, the VAR equations can be written as so:

VAR Model 1: est.birth_t =  α_t + β_1est.birth_t-_1 + φ_1CSI_t-_1 + ε_t

VAR Model 2: est.birth_t = α_t + β_1est.birth + γ_1DGS10_t-_1 + ε_t

VAR Model 3: est.birtht = α_t + β_1est_birth_t-_1 + φ_1CSI_t-_1 + γ_1DGS10_t-_1 + ε_t

Now that the VAR system is specified, we can estimate the VAR models and check for Granger causality. 

```r
# estimate VAR(1)
model1 <- VAR(var_1,p=1,type="const")
summary(model1)
model2 <- VAR(var_2,p=1,type="const")
summary(model2)
model3 <- VAR(var_3,p=1,type="const")
summary(model3)
model_ar =arima(establishment_birth ,order=c(1,0,0),method="ML")
summary(model_ar)
model_ar
```

Granger causality is determined below.

```r
#Granger Test the bivariate VARs
causality(model1,cause="establishment_birth")$Granger
```

```
## 
## 	Granger causality H0: establishment_birth do not Granger-cause
## 	CSI
## 
## data:  VAR object model1
## F-Test = 0.37178, df1 = 1, df2 = 182, p-value = 0.5428
```

```r
causality(model1,cause="CSI")$Granger
```

```
## 
## 	Granger causality H0: CSI do not Granger-cause
## 	establishment_birth
## 
## data:  VAR object model1
## F-Test = 7.1598, df1 = 1, df2 = 182, p-value = 0.008136
```

```r
causality(model2,cause="establishment_birth")$Granger
```

```
## 
## 	Granger causality H0: establishment_birth do not Granger-cause
## 	DGS10
## 
## data:  VAR object model2
## F-Test = 2.0857, df1 = 1, df2 = 182, p-value = 0.1504
```

```r
causality(model2,cause="DGS10")$Granger
```

```
## 
## 	Granger causality H0: DGS10 do not Granger-cause
## 	establishment_birth
## 
## data:  VAR object model2
## F-Test = 11.301, df1 = 1, df2 = 182, p-value = 0.000944
```

For the trivariate case, VAR Model 3, the coefficients on establishment birth, CSI, and the 10-Year Treasury are all significant at the 1% level of significance.The outcome of the Granger causality test as well as the VAR Model 3 coefficient’s significance are a favorable sign and perhaps indicate that the VAR models have marginal predictive power and may forecast better, or at least as well as a simple AR(1) model.

# Forecast Results
As mentioned earlier, recursive forecasting will be used. The period of 1992:Q3 – 2012:Q4 is the sample for the four models (including the AR(1) model), and the period of 2013:Q1 – 2016:Q1 will be forecasted. The sample is reasonably large, 95 observations, with 82 of those being used as the sample period. Since the sample is reasonably large, I forecasted 1, 2, 3 and 4-steps-ahead. 4-step-ahead is meaningful in this case as the data is quarterly so 4-step-ahead completes a full year.

The loop used to forecast is below:

```r
##Recursive forecasting from 2013:Q1-2016:Q1
#Comparing forecasts from the 3 VAR(1) MODELS and the AR(1) MODEL
#Initial sample estimation 1993:Q3-2012:Q4 (sample size=82)

n.end = 82 #Initial sample estimation
t = length(establishment_birth) #Full Sample size
n = t-n.end - 3 #Forecast sample

# set matrix for storage
pred <- matrix(rep(0,16*n),n,16)


# start loop
for(i in 1:n){
  x_var_1 = var_1[1:n.end+i-1,]
  x_var_2 = var_2[1:n.end+i-1,]
  x_var_3 = var_3[1:n.end+i-1,]
  x_ar = establishment_birth[1:n.end+i-1]
  
  model_var_1=VAR(x_var_1,p=1,type="const")
  for_var_1=predict(model_var_1,n.ahead=4,se.fit=FALSE)
  pred[i,1:4]=for_var_1$fcst$establishment_birth[1:4] 
  
  model_var_2 =VAR(x_var_2,p=1,type="const")
  for_var_2=predict(model_var_2,n.ahead=4,se.fit=FALSE)
  pred[i,5:8]=for_var_2$fcst$establishment_birth[1:4] 
  
  model_var_3 =VAR(x_var_3,p=1,type="const")
  for_var_3=predict(model_var_3,n.ahead=4,se.fit=FALSE)
  pred[i,9:12]=for_var_3$fcst$establishment_birth[1:4] 
  
  model_ar=arima(x_ar,order=c(1,0,0),method="ML")
  pred[i,13:16]=predict(model_ar,n.ahead=4,se.fit=FALSE)[1:4]
}
```


We now want to convert forecast results to time series objects.

```r
# set predictions as time series object
pred_var_1_ts=ts(data=pred[,1:4],start=c(2013,1),frequency=4)
pred_var_2_ts=ts(data=pred[,5:8],start=c(2013,1),frequency=4)
pred_var_3_ts=ts(data=pred[,9:12],start=c(2013,1),frequency=4)
pred_ar_ts=ts(data=pred[,13:16],start=c(2013,1),frequency=4)
act_ts=ts(data=est_birth_adj[83:92,1],start=c(2013,1),frequency=4)
dim(est_birth_adj)
```

```
## [1] 95  1
```


We can gauge the results by observing a plot of the forecast results.

```r
# plot actual JOBS GROWTH and 1-step ahead forecasts
plot(x = act_ts,col="black",ylim=c(2.5,3.7),ylab = "Births as a % of Total Establishments")
lines(pred_var_1_ts[,1],col="green")
lines(pred_var_2_ts[,1],col="red")
lines(pred_var_3_ts[,1],col="yellow")
lines(pred_ar_ts[,1],col="purple")
abline(h=0)
legend(x="topright",c("Estmt Births","VAR 1 Forecast","VAR 2 Forecast", "VAR 3 Forecast","AR(1) Forecast"), ncol = 2,
       col=c("black","green","red", "yellow","purple"),lty=1,lwd=.5)
title(main = "Actual Estmt Births and 1-Step-Ahead Forecasts")
```

![](http://i.imgur.com/fuw2i07.png)<!-- -->

```r
# plot actual JOBS GROWTH and 1-step ahead forecasts
plot(act_ts,col="black",ylim=c(2.5,3.7),ylab = "Births as a % of Total Establishments")
lines(pred_var_1_ts[,4],col="green")
lines(pred_var_2_ts[,4],col="red")
lines(pred_var_3_ts[,4],col="yellow")
lines(pred_ar_ts[,4],col="purple")
abline(h=0)
legend(x="topright",c("Estmt Births","VAR 1 Forecast","VAR 2 Forecast", "VAR 3 Forecast","AR(1) Forecast"), ncol = 2,
       col=c("black","green","red", "yellow","purple"),lty=1,lwd=.8)
title(main = "Actual Estmt Births and 4-Step-Ahead Forecasts")
```

![](http://i.imgur.com/IVDpCh2.png)<!-- -->

Plotted are the 1 and 4-steps-ahead forecast. The results are as expected. The 1-step-ahead chart shows the forecast reacting to what had happened 1 period prior. The 4-step-ahead forecast is smoother. Looking at the 4-Step-Ahead forecast, the VAR Model 3 Forecast appears to be superior, as graphically it travels right through the middle of the actual data’s peaks and valleys.

To compare the models forecasting ability, it is helpful to look at the generated root-mean-square error. Doing so will allow us to at least eye-ball whether the inclusion of CSI and/or interest rate data will help forecasting.

So calculate the prediction errors and then the root-mean-squared errors


```r
# prediction errors
# compute prediction errors
errors <- data.frame(matrix(NA,16,10))

for(i in 1:4) {
  
  errors[i,] <- establishment_birth[(n.end+i):(t-4+i)]-pred[,i]
  errors[4+i,] <- establishment_birth[(n.end+i):(t-4+i)]-pred[,4+i]
  errors[8+i,] <- establishment_birth[(n.end+i):(t-4+i)]-pred[,8+i]
  errors[12+i,] <- establishment_birth[(n.end+i):(t-4+i)]-pred[,12+i]
}


rmse_results <- data.frame(matrix(NA,4,4))
colnames(rmse_results) <- c("1-step-ahead","2-step-ahead","3-step-ahead","4-step-ahead")
rownames(rmse_results) <- c("var 1", "var 2", "var 3", "ar")

for(i in 1:4) {
  rmse_results[1,i] <- sqrt(mean(errors[i,]^2))
  rmse_results[2,i] <- sqrt(mean(errors[4+i,]^2))
  rmse_results[3,i] <- sqrt(mean(errors[8+i,]^2))
  rmse_results[4,i] <- sqrt(mean(errors[12+i,]^2))
}

#Print out RMSE results
rmse_results
```

```
##       1-step-ahead 2-step-ahead 3-step-ahead 4-step-ahead
## var 1    0.1241354   0.11012730   0.12940020    0.1947392
## var 2    0.1093360   0.09004612   0.11810764    0.1338015
## var 3    0.1028305   0.07095981   0.07993336    0.1356435
## ar       0.1212719   0.08589467   0.11516933    0.1713726
```

It appears that overall the RMSE for VAR Model 3, the model including lagged versions of all variables on the right side, is the superior model. VAR Model 3 has a lower RMSE than all other models at every step ahead sans 4-step-ahead, for which VAR Model 2 has a lower RMSE.

Observing the RMSE values is good first step in comparing forecast, but it is not sufficient to declare a model superior. To rigorously decide the best model, I employ the Clark-West(CW) and Diebold-Mariano-West(DMW) comparison tests.

First, I started by comparing the two bivariate models to the AR(1) model. VAR Model 1, which included the consumer sentiment index, did not do better than the AR(1) Model at any time frame. 



```r
###- Clark-West Test for nested forecast comparison (VAR model with CSI with AR)
#results for all 4
csi_ar_cw <- data.frame(matrix(0,1,4))
colnames(csi_ar_cw) <- c("1-step-ahead","2-step-ahead","3-step-ahead","4-step-ahead")
p=rep(1,n)

for(i in 1:4) {
  cw1 = (errors[12+i,]^2)-(errors[i,]^2)+((errors[12+i,])-(errors[i,]))^2
  reg.cw1 =lm(cw1[1,] ~ p -1)
  avar.cw1 =NeweyWest(reg.cw1,lag=3,prewhite=FALSE)
  cw.test1=reg.cw1$coef/sqrt(avar.cw1)
  
  csi_ar_cw[,i] <- (1-pnorm(cw.test1))/2
}
csi_ar_cw
```

```
##           p         p         p         p
## 1 0.1440116 0.2976437 0.2092247 0.3731449
```


VAR Model 2, which included 10-Year Treasury Index, was significantly better than the AR(1) Model for all step-ahead forecasts at the 10% level of significance, and better for the 1, 2, and 4-step ahead forecasts at the 5% level of significance.


```r
###- Clark-West Test for nested forecast comparison (VAR model with 10 year treasury with AR)

dgs10_ar_cw <- data.frame(matrix(0,1,4))
colnames(dgs10_ar_cw) <- c("1-step-ahead","2-step-ahead","3-step-ahead","4-step-ahead")
p=rep(1,n)

for(i in 1:4) {
  cw1 = (errors[12+i,]^2)-(errors[4+i,]^2)+((errors[12+i,])-(errors[4+i,]))^2
  reg.cw1 =lm(cw1[1,] ~ p -1)
  avar.cw1 =NeweyWest(reg.cw1,lag=3,prewhite=FALSE)
  cw.test1=reg.cw1$coef/sqrt(avar.cw1)
  
  dgs10_ar_cw[,i] <- (1-pnorm(cw.test1))/2
}
dgs10_ar_cw
```

```
##            p          p          p          p
## 1 0.03947797 0.04897645 0.08158432 0.01492487
```
Now, I compare the two bivariate models

```r
#####Now compare the 2-bivariate models - Use DMW 
dgs10_csi_dmw <- data.frame(matrix(0,1,4))
colnames(dgs10_csi_dmw) <- c("1-step-ahead","2-step-ahead","3-step-ahead","4-step-ahead")
p=rep(1,n)

for(i in 1:4) {
  dmw = (errors[4+i,]^2)-(errors[i,]^2)
  reg.dmw =lm(dmw[1,] ~ p -1)
  avar.dmw =NeweyWest(reg.dmw,lag=3,prewhite=FALSE)
  dmw.test1=reg.dmw$coef/sqrt(avar.dmw)
  
  dgs10_csi_dmw[,i] <- (1-pnorm(dmw.test1))/2
}


dgs10_csi_dmw
```

```
##           p         p         p         p
## 1 0.3924327 0.3670846 0.2889129 0.4378562
```

Interestingly, according to the Diebold-Mariano-West test results, the bivariate VAR models are not significantly different. One would think that given the bivariate to AR(1) comparison tests that VAR Model 2 would be significantly better than VAR Model 1. Perhaps the different function forms between the Diebold-Mariano-West and Clark-West tests can explain the discrepancy.

Finally, I compare the  trivariate model with the two bivariate models and the AR model.


```r
####Compare the trivariate model with the CSI model. Use the Clark-West
tri_csi_cw <- data.frame(matrix(0,1,4))
colnames(tri_csi_cw) <- c("1-step-ahead","2-step-ahead","3-step-ahead","4-step-ahead")
p=rep(1,n)

for(i in 1:4) {
  cw1 = (errors[i,]^2)-(errors[8+i,]^2)+((errors[i,])-(errors[8+i,]))^2
  reg.cw1 =lm(cw1[1,] ~ p -1)
  avar.cw1 =NeweyWest(reg.cw1,lag=3,prewhite=FALSE)
  cw.test1=reg.cw1$coef/sqrt(avar.cw1)
  
  tri_csi_cw[,i] <- (1-pnorm(cw.test1))/2
}
tri_csi_cw
```

```
##             p            p           p           p
## 1 0.004890627 0.0001094091 0.006218789 0.003660419
```


```r
####Compare the trivariate model with the 10-Year Treasury model. Use the Clark-West
tri_dgs10_cw<- data.frame(matrix(0,1,4))
colnames(tri_dgs10_cw) <- c("1-step-ahead","2-step-ahead","3-step-ahead","4-step-ahead")
p=rep(1,n)

for(i in 1:4) {
  cw1 = (errors[4+i,]^2)-(errors[8+i,]^2)+((errors[4+i,])-(errors[8+i,]))^2
  reg.cw1 =lm(cw1[1,] ~ p -1)
  avar.cw1 =NeweyWest(reg.cw1,lag=3,prewhite=FALSE)
  cw.test1=reg.cw1$coef/sqrt(avar.cw1)
  
  tri_dgs10_cw[,i] <- (1-pnorm(cw.test1))/2
}
tri_dgs10_cw
```

```
##            p         p          p          p
## 1 0.06036021 0.0399855 0.04280538 0.05633821
```


```r
####Compare the trivariate model with the AR model. Use the Clark-West
tri_ar_cw<- data.frame(matrix(0,1,4))
colnames(tri_ar_cw) <- c("1-step-ahead","2-step-ahead","3-step-ahead","4-step-ahead")
p=rep(1,n)

for(i in 1:4) {
  cw1 = (errors[12+i,]^2)-(errors[8+i,]^2)+((errors[12+i,])-(errors[8+i,]))^2
  reg.cw1 =lm(cw1[1,] ~ p -1)
  avar.cw1 =NeweyWest(reg.cw1,lag=3,prewhite=FALSE)
  cw.test1=reg.cw1$coef/sqrt(avar.cw1)
  
  tri_ar_cw[,i] <- (1-pnorm(cw.test1))/2
}
tri_ar_cw
```

```
##            p          p          p          p
## 1 0.01224723 0.01179233 0.02805407 0.01960627
```
The trivariate model (VAR Model 3) performs better than the VAR Model 1 at all steps ahead at the 5% level of significance. VAR Model 3 performs better than VAR Model 2 at the 2 and 3-steps-ahead at the 5% level of significance and 1 and 4-steps-aheads at the 5% level of significance. The trivariate model performs better than the AR(1) model at the 5% level of significance and almost at the 1% level of significance.

# Conclusion
Given the comparison test results, it appears that the VAR Model 3, the model including both the consumer sentiment index and the 10-Year Treasury Constant Maturity Rate, is best out of the VAR models tested in this post in forecasting future values of new establishment births. VAR Model 3 is also significantly better than a simple AR(1) model.

The results are intriguing, because as seen earlier when observing the time series, the establishment birth rate moved with the consumer sentiment index and the 10-Year Treasury rate pretty well. I would expect establishment birth rate to move with CSI, as when the economy is doing poorly one would expect consumers to have low confidence in the economy. If the economy is doing poorly, persons, who are also consumers with low confidence, are probably unlikely to start new businesses. The 10-Year Treasury moves with establishment birth rate as well instead of opposite of it, which still helps forecasting but does not support my hypothesis that low interest rates broadly would correspond to higher establishment birth rates. Apparently, the connection between interest rates and new business growth is not so clear cut. 





---
