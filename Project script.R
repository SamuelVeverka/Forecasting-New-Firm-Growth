setwd("C:\\Users\\Samuel\\Documents\\R\\Excel Practice Sheets\\411 Project") # Set my working directory


library(forecast)
library(vars)
library(tseries)

####Import data files and convert to time series

#Establishment Birth - seasonally adjusted
est_birth_adj <- read.csv("Establishment_Birth_adj.csv",header=TRUE)
establishment_birth <- ts(data=est_birth_adj,start=c(1992,3),frequency=4)
head(establishment_birth)
plot(establishment_birth)

#University of Michigan Consumer sentiment index
CSI_1 <- read.csv("UMCSENT.csv",header=TRUE)
CSI <- ts(data=CSI_1,start=c(1992,3),frequency=4)
head(CSI)
plot(CSI)

#10-Year Treasury Constant Maturity Rate
DGS10_1 <- read.csv("DGS10.csv",header=TRUE)
DGS10 <- ts(data=DGS10_1,start=c(1992,3),frequency=4)
head(DGS10)
plot(DGS10)

#Run Augmented Dickey-Fuller(ADF) and Phillips-Perron tests for unit root
pp.test(establishment_birth)
adf.test(establishment_birth)


#Create 3 VAR system
var_1 <- ts(cbind(establishment_birth,CSI),start=c(1992,3),frequency=4) #Establishment Birth and consumer sentiment
var_2 <- ts(cbind(establishment_birth,DGS10),start=c(1992,3),frequency=4) #Establishment Birth and 10-Year Treasury Constant Maturity Rate
var_3 <- ts(cbind(establishment_birth,CSI, DGS10),start=c(1992,3),frequency=4) #stock returns, consumer sentiment and 10-Year Treasury Constant Maturity Rate

var_1
#Plot variables
layout(1)
plot(var_3,type="l",col="blue",main="Establishment Birth, Consumer Sentiment Index, and 10-Year Treasury")

#Use model selection criteria to choose optimal number of lags
# get lag correlation summary



# find optimal number of lags
info.crit1 <- VARselect(var_1,lag.max=4,type="const")
info.crit2 <- VARselect(var_2,lag.max=4,type="const")
info.crit3 <- VARselect(var_3,lag.max=4,type="const")

info.crit1
info.crit2
info.crit3

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
#Granger Test the bivariate VARs
causality(model1,cause="establishment_birth")$Granger
causality(model1,cause="CSI")$Granger

causality(model2,cause="establishment_birth")$Granger
causality(model2,cause="DGS10")$Granger

##Recursive forecasting from 2013:Q1-2016:Q1
#Comparing forecasts from the 3 VAR(1) MODELS and the AR(1) MODEL
#Initial sample estimation 1993:Q3-2012:Q4 (sample size=82)

n.end = 82 #Initial sample estimation
t = length(establishment_birth) #Full Sample size
n = t-n.end - 3 #Forecast sample

# set matrix for storage
pred_var_1 <- matrix(rep(0,4*n),n,4)
pred_var_2 <- matrix(rep(0,4*n),n,4)
pred_var_3 <- matrix(rep(0,4*n),n,4)
pred_ar <- matrix(rep(0,4*n),n,4)

# start loop
for(i in 1:n){
  x_var_1 = var_1[1:n.end+i-1,]
  x_var_2 = var_2[1:n.end+i-1,]
  x_var_3 = var_3[1:n.end+i-1,]
  x_ar = establishment_birth[1:n.end+i-1]
  
  model_var_1=VAR(x_var_1,p=1,type="const")
  for_var_1=predict(model_var_1,n.ahead=4,se.fit=FALSE)
  pred_var_1[i,1:4]=for_var_1$fcst$establishment_birth[1:4] 
  
  model_var_2 =VAR(x_var_2,p=1,type="const")
  for_var_2=predict(model_var_2,n.ahead=4,se.fit=FALSE)
  pred_var_2[i,1:4]=for_var_2$fcst$establishment_birth[1:4] 
  
  model_var_3 =VAR(x_var_3,p=1,type="const")
  for_var_3=predict(model_var_3,n.ahead=4,se.fit=FALSE)
  pred_var_3[i,1:4]=for_var_3$fcst$establishment_birth[1:4] 
  
  model_ar=arima(x_ar,order=c(1,0,0),method="ML")
  pred_ar[i,1:4]=predict(model_ar,n.ahead=4,se.fit=FALSE)[1:4]
}

dim(pred_var_1_ts)

# set predictions as time series object
pred_var_1_ts=ts(data=pred_var_1,start=c(2013,1),frequency=4)
pred_var_2_ts=ts(data=pred_var_2,start=c(2013,1),frequency=4)
pred_var_3_ts=ts(data=pred_var_3,start=c(2013,1),frequency=4)
pred_ar_ts=ts(data=pred_ar,start=c(2013,1),frequency=4)
act_ts=ts(data=est_birth_adj[83:92,1],start=c(2013,1),frequency=4)
dim(est_birth_adj)
# plot actual JOBS GROWTH and 1-step ahead forecasts
plot(x = act_ts,col="black",ylim=c(2.5,3.7),ylab = "Births as a % of Total Establishments")
lines(pred_var_1_ts[,1],col="green")
lines(pred_var_2_ts[,1],col="red")
lines(pred_var_3_ts[,1],col="yellow")
lines(pred_ar_ts[,1],col="purple")
abline(h=0)
legend(x="topright",c("Establishment Births","VAR 1 Forecast","VAR 2 Forecast", "VAR 3 Forecast","AR(1) Forecast"),
       col=c("black","green","red", "yellow","purple"),lty=1,lwd=.5)
title(main = "Actual Establishment Births and 1-Step-Ahead Forecasts")

# plot actual JOBS GROWTH and 1-step ahead forecasts
plot(act_ts,col="black",ylim=c(2.5,3.7),ylab = "Births as a % of Total Establishments")
lines(pred_var_1_ts[,4],col="green")
lines(pred_var_2_ts[,4],col="red")
lines(pred_var_3_ts[,4],col="yellow")
lines(pred_ar_ts[,4],col="purple")
abline(h=0)
legend(x="topright",c("Establishment Births","VAR 1 Forecast","VAR 2 Forecast", "VAR 3 Forecast","AR(1) Forecast"),
       col=c("black","green","red", "yellow","purple"),lty=1,lwd=.8)
title(main = "Actual Establishment Births and 4-Step-Ahead Forecasts")

# prediction errors
# compute prediction errors
e1_var_1=establishment_birth[(n.end+1):(t-3)]-pred_var_1[,1]#1-step ahead forecast error
e2_var_1=establishment_birth[(n.end+2):(t-2)]-pred_var_1[,2]#2-step ahead forecast error
e3_var_1=establishment_birth[(n.end+3):(t-1)]-pred_var_1[,3]#3-step ahead forecast error
e4_var_1=establishment_birth[(n.end+4):t]-pred_var_1[,4]#4-step ahead forecast error

e1_var_2=establishment_birth[(n.end+1):(t-3)]-pred_var_2[,1]#1-step ahead forecast error
e2_var_2=establishment_birth[(n.end+2):(t-2)]-pred_var_2[,2]#2-step ahead forecast error
e3_var_2=establishment_birth[(n.end+3):(t-1)]-pred_var_2[,3]#3-step ahead forecast error
e4_var_2=establishment_birth[(n.end+4):t]-pred_var_2[,4]#4-step ahead forecast error

e1_var_3=establishment_birth[(n.end+1):(t-3)]-pred_var_3[,1]#1-step ahead forecast error
e2_var_3=establishment_birth[(n.end+2):(t-2)]-pred_var_3[,2]#2-step ahead forecast error
e3_var_3=establishment_birth[(n.end+3):(t-1)]-pred_var_3[,3]#3-step ahead forecast error
e4_var_3=establishment_birth[(n.end+4):t]-pred_var_3[,4]#4-step ahead forecast error

e1_ar=establishment_birth[(n.end+1):(t-3)]-pred_ar[,1]#1-step ahead forecast error
e2_ar=establishment_birth[(n.end+2):(t-2)]-pred_ar[,2]#2-step ahead forecast error
e3_ar=establishment_birth[(n.end+3):(t-1)]-pred_ar[,3]#3-step ahead forecast error
e4_ar=establishment_birth[(n.end+4):t]-pred_ar[,4]#4-step ahead forecast error

# compute root mean squared errors
rmse1_var_1=sqrt(mean(e1_var_1^2))
rmse2_var_1=sqrt(mean(e2_var_1^2))
rmse3_var_1=sqrt(mean(e3_var_1^2))
rmse4_var_1=sqrt(mean(e4_var_1^2))

rmse1_var_2=sqrt(mean(e1_var_2^2))
rmse2_var_2=sqrt(mean(e2_var_2^2))
rmse3_var_2=sqrt(mean(e3_var_2^2))
rmse4_var_2=sqrt(mean(e4_var_2^2))

rmse1_var_3=sqrt(mean(e1_var_3^2))
rmse2_var_3=sqrt(mean(e2_var_3^2))
rmse3_var_3=sqrt(mean(e3_var_3^2))
rmse4_var_3=sqrt(mean(e4_var_3^2))

rmse1_ar=sqrt(mean(e1_ar^2))
rmse2_ar=sqrt(mean(e2_ar^2))
rmse3_ar=sqrt(mean(e3_ar^2))
rmse4_ar=sqrt(mean(e4_ar^2))

#Print out RMSE results
rmse1_var_1; rmse2_var_1;rmse3_var_1;rmse4_var_1;
rmse1_var_2; rmse2_var_2;rmse3_var_2;rmse4_var_2;
rmse1_var_3; rmse2_var_3;rmse3_var_3;rmse4_var_3;
rmse1_ar; rmse2_ar;rmse3_ar;rmse4_ar;


###- Clark-West Test for nested forecast comparison (VAR model with CSI with AR)
#1-step ahead
c=rep(1,n)
cw1 = e1_ar^2-e1_var_1^2+(e1_ar-e1_var_1)^2
reg.cw1 =lm(cw1 ~ c-1)
avar.cw1 =NeweyWest(reg.cw1,lag=3,prewhite=FALSE)
cw.test1=reg.cw1$coef/sqrt(avar.cw1)
cw.test1

pnorm(cw.test1)
cwp1 <- (1-pnorm(cw.test1))/2

#2-step ahead
cw2 = e2_ar^2-e2_var_1^2+(e2_ar-e2_var_1)^2
reg.cw2 =lm(cw2~ c-1)
avar.cw2=NeweyWest(reg.cw2,lag=3,prewhite=FALSE)
cw.test2=reg.cw2$coef/sqrt(avar.cw2)
cw.test2

pnorm(cw.test2)
cwp2 <- (1-pnorm(cw.test2))/2

#3-step ahead
cw3 = e3_ar^2-e3_var_1^2+(e3_ar-e3_var_1)^2
reg.cw3 =lm(cw3~ c-1)
avar.cw3=NeweyWest(reg.cw3,lag=3,prewhite=FALSE)
cw.test3=reg.cw3$coef/sqrt(avar.cw3)
cw.test3

pnorm(cw.test3)
cwp3 <- (1-pnorm(cw.test3))/2

#4-step ahead
cw4 = e4_ar^2-e4_var_1^2+(e4_ar-e4_var_1)^2
reg.cw4 =lm(cw4~ c-1)
avar.cw4=NeweyWest(reg.cw4,lag=3,prewhite=FALSE)
cw.test4=reg.cw4$coef/sqrt(avar.cw4)
cw.test4

pnorm(cw.test4)
cwp4 <- (1-pnorm(cw.test4))/2

#results for all 4
cwp1
cwp2
cwp3
cwp4

###- Clark-West Test for nested forecast comparison (VAR model with 10 year treasury with AR)
#1-step ahead
c=rep(1,n)
cw1 = e1_ar^2-e1_var_2^2+(e1_ar-e1_var_2)^2
reg.cw1 =lm(cw1 ~ c-1)
avar.cw1 =NeweyWest(reg.cw1,lag=3,prewhite=FALSE)
cw.test1=reg.cw1$coef/sqrt(avar.cw1)
cw.test1

pnorm(cw.test1)
cwp1 <- (1-pnorm(cw.test1))/2

#2-step ahead
cw2 = e2_ar^2-e2_var_2^2+(e2_ar-e2_var_2)^2
reg.cw2 =lm(cw2~ c-1)
avar.cw2=NeweyWest(reg.cw2,lag=3,prewhite=FALSE)
cw.test2=reg.cw2$coef/sqrt(avar.cw2)
cw.test2

pnorm(cw.test2)
cwp2 <- (1-pnorm(cw.test2))/2

#3-step ahead
cw3 = e3_ar^2-e3_var_2^2+(e3_ar-e3_var_2)^2
reg.cw3 =lm(cw3~ c-1)
avar.cw3=NeweyWest(reg.cw3,lag=3,prewhite=FALSE)
cw.test3=reg.cw3$coef/sqrt(avar.cw3)
cw.test3

pnorm(cw.test3)
cwp3 <- (1-pnorm(cw.test3))/2

#4-step ahead
cw4 = e4_ar^2-e4_var_2^2+(e4_ar-e4_var_2)^2
reg.cw4 =lm(cw4~ c-1)
avar.cw4=NeweyWest(reg.cw4,lag=3,prewhite=FALSE)
cw.test4=reg.cw4$coef/sqrt(avar.cw4)
cw.test4

pnorm(cw.test4)
cwp4 <- (1-pnorm(cw.test4))/2

#results for all 4
cwp1
cwp2
cwp3
cwp4


#####Now compare the 2-bivariate models - Use DMW 
c=rep(1,n)

dmw1=e1_var_2^2-e1_var_1^2
dmw2=e2_var_2^2-e2_var_1^2
dmw3=e3_var_2^2-e3_var_1^2
dmw4=e4_var_2^2-e4_var_1^2

#1-step Ahead
reg1=lm(dmw1~c-1)
avar1=NeweyWest(reg1,lag=3,prewhite=FALSE)
dmw.q1=reg1$coef/sqrt(avar1)#DMW STATISTIC
dmw.q1
pnorm(dmw.q1)

p.value1=1-pnorm(dmw.q1)
pstr.value1=p.value1/2 
pstr.value1 

#2-step Ahead
reg2=lm(dmw2~c-1)
avar2=NeweyWest(reg2,lag=3,prewhite=FALSE)
dmw.q2=reg2$coef/sqrt(avar2)#DMW STATISTIC
dmw.q2
pnorm(dmw.q2)

p.value2=1-pnorm(dmw.q2)
pstr.value2=p.value2/2 
pstr.value2

#3-step Ahead
reg3=lm(dmw3~c-1)
avar3=NeweyWest(reg3,lag=3,prewhite=FALSE)
dmw.q3=reg3$coef/sqrt(avar3)#DMW STATISTIC
dmw.q3
pnorm(dmw.q3)

p.value3=1-pnorm(dmw.q3)
pstr.value3=p.value3/2 
pstr.value3 

#4-step Ahead
reg4=lm(dmw4~c-1)
avar4=NeweyWest(reg4,lag=3,prewhite=FALSE)
dmw.q4=reg4$coef/sqrt(avar4)#DMW STATISTIC
dmw.q4
pnorm(dmw.q4)

p.value4=1-pnorm(dmw.q4)
pstr.value4=p.value4/2 
pstr.value4

#P-values for all four
pstr.value1
pstr.value2
pstr.value3
pstr.value4

####Compare the trivariate model with the CSI model. Use the Clark-West
c=rep(1,n)
cw1 = e1_var_1^2-e1_var_3^2+(e1_var_1-e1_var_3)^2
reg.cw1 =lm(cw1 ~ c-1)
avar.cw1 =NeweyWest(reg.cw1,lag=3,prewhite=FALSE)
cw.test1=reg.cw1$coef/sqrt(avar.cw1)
cw.test1

pnorm(cw.test1)
cwp1 <- (1-pnorm(cw.test1))/2

#2-step ahead
cw2 = e2_var_1^2-e2_var_3^2+(e2_var_1-e2_var_3)^2
reg.cw2 =lm(cw2~ c-1)
avar.cw2=NeweyWest(reg.cw2,lag=3,prewhite=FALSE)
cw.test2=reg.cw2$coef/sqrt(avar.cw2)
cw.test2

pnorm(cw.test2)
cwp2 <- (1-pnorm(cw.test2))/2

#3-step ahead
cw3 = e3_var_1^2-e3_var_3^2+(e3_var_1-e3_var_3)^2
reg.cw3 =lm(cw3~ c-1)
avar.cw3=NeweyWest(reg.cw3,lag=3,prewhite=FALSE)
cw.test3=reg.cw3$coef/sqrt(avar.cw3)
cw.test3

pnorm(cw.test3)
cwp3 <- (1-pnorm(cw.test3))/2

#4-step ahead
cw4 = e4_var_1^2-e4_var_3^2+(e4_var_1-e4_var_3)^2
reg.cw4 =lm(cw4~ c-1)
avar.cw4=NeweyWest(reg.cw4,lag=3,prewhite=FALSE)
cw.test4=reg.cw4$coef/sqrt(avar.cw4)
cw.test4

pnorm(cw.test4)
cwp4 <- (1-pnorm(cw.test4))/2

#results for all 4
cwp1
cwp2
cwp3
cwp4

####Compare the trivariate model with the 10-Year Treasury model. Use the Clark-West
c=rep(1,n)
cw1 = e1_var_2^2-e1_var_3^2+(e1_var_2-e1_var_3)^2
reg.cw1 =lm(cw1 ~ c-1)
avar.cw1 =NeweyWest(reg.cw1,lag=3,prewhite=FALSE)
cw.test1=reg.cw1$coef/sqrt(avar.cw1)
cw.test1

pnorm(cw.test1)
cwp1 <- (1-pnorm(cw.test1))/2

#2-step ahead
cw2 = e2_var_2^2-e2_var_3^2+(e2_var_2-e2_var_3)^2
reg.cw2 =lm(cw2~ c-1)
avar.cw2=NeweyWest(reg.cw2,lag=3,prewhite=FALSE)
cw.test2=reg.cw2$coef/sqrt(avar.cw2)
cw.test2

pnorm(cw.test2)
cwp2 <- (1-pnorm(cw.test2))/2

#3-step ahead
cw3 = e3_var_2^2-e3_var_3^2+(e3_var_2-e3_var_3)^2
reg.cw3 =lm(cw3~ c-1)
avar.cw3=NeweyWest(reg.cw3,lag=3,prewhite=FALSE)
cw.test3=reg.cw3$coef/sqrt(avar.cw3)
cw.test3

pnorm(cw.test3)
cwp3 <- (1-pnorm(cw.test3))/2

#4-step ahead
cw4 = e4_var_2^2-e4_var_3^2+(e4_var_2-e4_var_3)^2
reg.cw4 =lm(cw4~ c-1)
avar.cw4=NeweyWest(reg.cw4,lag=3,prewhite=FALSE)
cw.test4=reg.cw4$coef/sqrt(avar.cw4)
cw.test4

pnorm(cw.test4)
cwp4 <- (1-pnorm(cw.test4))/2

#results for all 4
cwp1
cwp2
cwp3
cwp4

####Compare the trivariate model with the AR model. Use the Clark-West
c=rep(1,n)
cw1 = e1_ar^2-e1_var_3^2+(e1_ar-e1_var_3)^2
reg.cw1 =lm(cw1 ~ c-1)
avar.cw1 =NeweyWest(reg.cw1,lag=3,prewhite=FALSE)
cw.test1=reg.cw1$coef/sqrt(avar.cw1)
cw.test1

pnorm(cw.test1)
cwp1 <- (1-pnorm(cw.test1))/2

#2-step ahead
cw2 = e2_ar^2-e2_var_3^2+(e2_ar-e2_var_3)^2
reg.cw2 =lm(cw2~ c-1)
avar.cw2=NeweyWest(reg.cw2,lag=3,prewhite=FALSE)
cw.test2=reg.cw2$coef/sqrt(avar.cw2)
cw.test2

pnorm(cw.test2)
cwp2 <- (1-pnorm(cw.test2))/2

#3-step ahead
cw3 = e3_ar^2-e3_var_3^2+(e3_ar-e3_var_3)^2
reg.cw3 =lm(cw3~ c-1)
avar.cw3=NeweyWest(reg.cw3,lag=3,prewhite=FALSE)
cw.test3=reg.cw3$coef/sqrt(avar.cw3)
cw.test3

pnorm(cw.test3)
cwp3 <- (1-pnorm(cw.test3))/2

#4-step ahead
cw4 = e4_ar^2-e4_var_3^2+(e4_ar-e4_var_3)^2
reg.cw4 =lm(cw4~ c-1)
avar.cw4=NeweyWest(reg.cw4,lag=3,prewhite=FALSE)
cw.test4=reg.cw4$coef/sqrt(avar.cw4)
cw.test4

pnorm(cw.test4)
cwp4 <- (1-pnorm(cw.test4))/2

#results for all 4
cwp1
cwp2
cwp3
cwp4

