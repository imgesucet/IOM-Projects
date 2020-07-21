# Forcasting
# Author: Murtiza(Muertizha),,,,,
# Set up ------------------------------------------------------------------

rm(list = ls())
options(scipen=999)
library(forecast)
library(tidyr)
library(readxl)
library(tidyverse)
library(xlsx)
library(dplyr)
library(car)
library("tseries")
library(stats)
# Reading Data --------------------------------------------------------------------

dir()
mean_scenario <- readRDS("mean_scenario.rds")# mean scenario

# TS data
# 1. Total
tot <- readRDS("forecast.rds" ) %>% 
  filter(flow=="total")

# 2. Labour
flows_labour <- readRDS("forecast.rds" ) %>% 
  filter(flow=="labour")

# 3. High
flows_high <- readRDS("forecast.rds" ) %>% 
  filter(flow=="high")

# 4. irregular
flows_irregular <- readRDS("forecast.rds" ) %>% 
  filter(flow=="irregular")

# 5. asylum
flows_asylum <- readRDS("forecast.rds" ) %>% 
  filter(flow=="labour")

# 1. For each flow implement the analysis. 5 analysis
# 2. Separated script with note.

# Total -------------------------------------------------------------------

# First visualize and check the trend
p <- ggplot(tot, aes(x=as.character(year), y=val, group = 1)) +
  geom_line() +
  xlab("Year")
p
# Note: So it seems there is an increasing trend with changing variance.

# 1. Linear model

# Before we apply time series model, lets check with linear regression.
# First thing first, need to check the linear model assumption whether we can apply it

# 1) Endogenous of the residual term (dependency btw residuals and covariates)
# Here, I just check the covariance of the two terms.
cov(lm(val ~ year, data = tot)$residuals,tot$year)
# Result: it is very close to the zero, that means residuals are independent 
# from the covariate.

# 2) Autocorrelation of the residual.
# Here I introduced durbinwatson test. e(t) = rho*e(t-1) + v(t)
durbinWatsonTest(lm(val ~ year, data = tot))
# we cannot reject the H0 : residuals are not auto-correlated.

# So lets try linear model.

# 3) Modeling
lm_tot <- lm(val ~ year, data = tot)
summary(lm_tot)
par("mar")
par(mar=c(1,1,1,1))
par(mfrow =c(2,2))
plot(lm_tot)

# Here I added some notes based on the residual visualization. 
# Generally, we test the model performance by residual analysis.
# Explanation and information extracted from the plot written below

# plot 1 (residuals vs fitted)
# We can observe that it is slightly curved.
# plot 2 (Normal QQ plot)
# it seems it is not normally distributed (2 points are off-set among  10 points)
# plot 3 (Scale location)
# line seems not really horizontal and variance are different over time.
# plot 4 (Residual vs leverage)
# observation 1 and 8 has a significant influence on the model,
# that means if we remove those observations, the model would have a significant changes.
# However, I didt remove and try the new model since we only have 10 data points.
# Hence, based on all the facts above, I wont recommend this model.
# we can do some changes based on the linear model, however, it is not necessary
# in our case since we dont want much advaced model.



# Prediction
lm_tot_pre <- predict(lm_tot, data.frame(year=c(2018:2030))) 

# 2. ARIMA model 
# make it ts data-frame
tsdata <- tot %>% 
  dplyr::select(val) %>% 
  ts(start = 2008)

# So lets start our time series trip, here I explained step by step for 
# total immigration. and for others, just simply explained, if you have any questions,
# feel free to ask me.

# 1) test the serial correlation

lag_length = 9 # number of lag we use in the test.

# Test whether any of a group of autocorrelations of a time series are different from zero.
Box.test(tsdata, lag=lag_length, type="Ljung-Box")
# Result : Here when Lag = 2-9, we cannot reject the hypothesis assuming the data are independently distributed.
# However, keep in mind that we only can say that we dont have sufficient information to reject the
# null hypothesis, but we cannot say that alternative hypothesis is true.

# Tips: In general, what is important here is to keep in mind that p-value < 0.05 
# lets you reject of the null-hypothesis, but a p-value > 0.05 does not let you 
# confirm the null-hypothesis.

# When lag = 1, it seems the process has a group autocorrelations.
# (From this one we also can see that auto-regressive part of the ARIMA model might be 1)

# In second, Before apply time-series model, we need to have a stationary process, 
# otherwise no matter what model you apply, it wont work well.

# 2) Test the process whether Stationary

# From the visualization, I find the non-stationary pattern, for further step
# lets try a statistical test. Here I introduced unit-root test.
# unit root test (If a process have unit root that means it is not stationary)
adf.test(tsdata)
# we cannot reject the hypothesis that assume the process has unit root.
# Result: the process maybe non-stationary (The series has the unit root)
# After the visualization and the test result, 
# I consider the TS is not a stationary process.

# 3) Transformation (make it stationary)
# Below is what I implemented.

# visualization
par(mar=c(3,3,3,3))
par(mfrow =c(1,1))
plot(tsdata)
abline(reg=lm(tsdata~time(tsdata)))
# You also can see from here the process is not stationary
# As you can see from the plot, we have two issues
# a) variance changing over time (you also can use sqrt function, it depends on the curve)
# b) we have an increasing trend of immigration.


# a) So first removing the unequal variance by using log function

# b) Second, address the trend component by using First/second order difference.
# Because first order still does not give us a stationary process

# Lets implement the test again with first difference
adf.test(diff(log(tsdata), d = 1), alternative="stationary", k=0)
# Second difference
adf.test(diff(log(tsdata), d = 2), alternative="stationary", k=0)

# so second difference strongly make the process stationary. However, again it doesnt
# mean that with first difference the process in not stationary 
# (again tricky statistical test definition).

# So lets find out the ARIMA parameter with auto-correlation function
# ACF and PACF
acf(diff(log(tsdata), d = 1))
pacf(diff(log(tsdata), d = 1))

acf(diff(log(tsdata), d = 2))
pacf(diff(log(tsdata), d = 2))

# it suggest auto-regressive lag order 1. How to see ACF and PACF, please check
# below website, I think it explained most of the cases.
https://people.duke.edu/~rnau/411arim3.htm

# ARIMA(p,d,q)
arima_110 <- arima(log(tsdata), c(1,1,0), method = "ML")
summary(arima_110)

arima_010 <- arima(log(tsdata), c(0,1,0), method = "ML")
summary(arima_010)

par(mar=c(10,10,10,10))
arima_forcast110 <- forecast::forecast(arima_110, h = 13, lambda = 0, biasadj = TRUE)
autoplot(tsdata) + forecast::autolayer(arima_forcast110)
forecast::checkresiduals(arima_110) # residual check

par(mar=c(10,10,10,10))
arima_forcast010 <- forecast::forecast(arima_010, h = 13, lambda = 0, biasadj = TRUE)
autoplot(tsdata) + forecast::autolayer(arima_forcast010)
forecast::checkresiduals(arima_010) # residual check

# Based on the AIC information criteria, MASE and other facts, 
# In the end I prefer ARIMA(1,1,0)

# Here is the auto arima implementation.

fit <- forecast::auto.arima(tsdata,
                            trace=T, 
                            stepwise = T,
                            stationary=F,
                            approximation=FALSE,
                            seasonal = F,
                            lambda = 0)


# step 6
forecast::checkresiduals(fit) # Good

# Labour ---------------------------------------------------------------
# Most of the procedures will be same

# First visualize and check the trend
p2 <- ggplot(flows_labour, aes(x=as.character(year), y=val, group = 1)) +
  geom_line() +
  xlab("Year")
p2
# Note: So it seems there is an increasing trend with changing variance.

# 1. Linear model

# Before we apply time series model, lets check with linear regression.
# First thing first, need to check the linear model assumption whether we can apply it

# 1) Endogenous of the residual term (dependency btw residuals and covariates)
# Here, I just check the covariance of the two terms.
cov(lm(val ~ year, data = flows_labour)$residuals,flows_labour$year)
# Result: it is very close to the zero, that means residuals are independent 
# from the covariate.

# 2) Autocorrelation of the residual.
# Here I introduced durbinwatson test. e(t) = rho*e(t-1) + v(t)
durbinWatsonTest(lm(val ~ year, data = flows_labour))
# we reject the H0 : residuals are not auto-correlated.
# So test result strongly recommend that 
# LM assumption doesnt hold for this dataset
# Anyway lets try linear model for comparison purpose.

# 3) Modeling
lm_labour <- lm(val ~ year, data = flows_labour)
summary(lm_labour)
par("mar")
par(mar=c(1,1,1,1))
par(mfrow =c(2,2))
plot(lm_labour)

# Here I added some notes based on the residual visualization. 
# Generally, we test the model performance by residual analysis.
# Explanation and information extracted from the plot written below

# plot 1 (residuals vs fitted)
# We can observe that it is strongly curved, it shows the non-linearity.
# In addition, we can observe some unusual observations and variance of the error term are unequal.
# strongly violates the linear model assumptions.
# plot 2 (Normal QQ plot)
# it seems it is not normally distributed (right skewed)
# plot 3 (Scale location)
# line is far asway from the zero-line and variance are different over time.
# plot 4 (Residual vs leverage)
# observation 1 has a significant influence on the model,
# that means if we remove the observation, the model would have a significant changes.
# Hence, based on all the facts above, I strongly do not recommend this model.
# we can do some changes based on the linear model, however, it is not necessary
# in our case since we dont want much advanced model.



# Prediction
lm_labour_pre <- predict(lm_labour, data.frame(year=c(2019:2030))) 

# 2. ARIMA model 
# make it ts data-frame
tsdata_labour <- flows_labour %>% 
  dplyr::select(val) %>% 
  ts(start = 2008)


# 1) test the serial correlation

lag_length = 8 # number of lag we use in the test.

# Test whether any of a group of autocorrelations of a time series are different from zero.
Box.test(tsdata_labour, lag=lag_length, type="Ljung-Box")
# Result : Here for all the order of the lags, we reject the hypothesis assuming the data are independently distributed.
# So based on the information we have, we are sure that the process has a serieal correlation.
# this also shows that linear model is not appropriate.

# In second, Before apply time-series model, we need to have a stationary process, 
# otherwise no matter what model you apply, it wont work well.

# 2) Test the process whether Stationary

# From the visualization, I find the non-stationary pattern, for further step
# lets try a statistical test. Here I introduced unit-root test.
# unit root test (If a process have unit root that means it is not stationary)
adf.test(tsdata_labour)
# we cannot reject the hypothesis that assume the process has unit root.
# Result: the process is may be non-stationary (The series has the unit root)
# After the visualization and the test result, 
# I consider the TS is not a stationary process.

# 3) Transformation (make it stationary)
# Below is what I implemented.

# visualization
par(mar=c(3,3,3,3))
par(mfrow =c(1,1))
plot(tsdata_labour)
abline(reg=lm(tsdata_labour~time(tsdata_labour)))
# You also can see from here the process is not stationary
# As you can see from the plot, we have two issues
# a) variance changing over time (you also can use sqrt function, it depends on the curve)
# b) we have an increasing trend of immigration.


# a) So first removing the unequal variance by using log function

# b) Second, address the trend component by using First/second order difference.
# Because first order still does not give us a stationary process

# Lets implement the test again with first difference
adf.test(diff(log(tsdata_labour), d = 1), alternative="stationary", k=0)
# Second difference
adf.test(diff(log(tsdata_labour), d = 2), alternative="stationary", k=0)
# Again, test provides second difference is stationary but first difference might not be stationary.
# (again tricky statistical test definition).

# So lets find out the ARIMA parameter with auto-correlation function
# ACF and PACF
acf(diff(log(tsdata_labour), d = 1))
pacf(diff(log(tsdata_labour), d = 1))

acf(diff(log(tsdata_labour), d = 2))
pacf(diff(log(tsdata_labour), d = 2))

# it suggest auto-regressive lag order 0 or 1 , and moving average term order 1 or 2.

# Here you go
# ARIMA(p,d,q)
Labour_arima_011 <- arima(log(tsdata_labour), c(0,1,1), method = "ML")
summary(Labour_arima_011)

Labour_arima_012 <- arima(log(tsdata_labour), c(0,1,2), method = "ML")
summary(Labour_arima_012)

Labour_arima_111 <- arima(log(tsdata_labour), c(1,1,1), method = "ML")
summary(Labour_arima_111)

Labour_arima_112 <- arima(log(tsdata_labour), c(1,1,2), method = "ML")
summary(Labour_arima_112)

# I select two models from above, 011 and 012 respectivly, and plotted.

par(mar=c(10,10,10,10))
pre_Labour_arima_011 <- forecast::forecast(Labour_arima_011, h = 12, lambda = 0, biasadj = TRUE)
autoplot(tsdata_labour) + forecast::autolayer(pre_Labour_arima_011)
forecast::checkresiduals(Labour_arima_011) # residual check

par(mar=c(10,10,10,10))
pre_Labour_arima_012 <- forecast::forecast(Labour_arima_012, h = 12, lambda = 0, biasadj = TRUE)
autoplot(tsdata_labour) + forecast::autolayer(pre_Labour_arima_012)
forecast::checkresiduals(Labour_arima_012) # residual check

# Based on the AIC information criteria, MASE and other facts, 
# In the end I prefer ARIMA(0,1,2)



# High --------------------------------------------------------------------

# Most of the procedures will be same

# First visualize and check the trend
p3 <- ggplot(flows_high, aes(x=as.character(year), y=val, group = 1)) +
  geom_line() +
  xlab("Year")
p3
# Note: So it seems there is an increasing trend with changing variance.

# 1. Linear model

# Before we apply time series model, lets check with linear regression.
# First thing first, need to check the linear model assumption whether we can apply it

# 1) Endogenous of the residual term (dependency btw residuals and covariates)
# Here, I just check the covariance of the two terms.
cov(lm(val ~ year, data = flows_high)$residuals,flows_high$year)
# Result: it is very close to the zero, that means residuals are independent 
# from the covariate.

# 2) Autocorrelation of the residual.
# Here I introduced durbinwatson test. e(t) = rho*e(t-1) + v(t)
durbinWatsonTest(lm(val ~ year, data = flows_labour))
# we reject the H0 : residuals are not auto-correlated.
# So test result strongly recommend that 
# LM assumption doesnt hold for this dataset
# Anyway lets try linear model for comparison purpose.

# 3) Modeling
lm_high <- lm(val ~ year, data = flows_high)
summary(lm_high)
par("mar")
par(mar=c(1,1,1,1))
par(mfrow =c(2,2))
plot(lm_high)

# Here I added some notes based on the residual visualization. 
# Generally, we test the model performance by residual analysis.
# Explanation and information extracted from the plot written below

# plot 1 (residuals vs fitted)
# We can observe that it is strongly curved, it shows the non-linearity.
# In addition, we can observe some unusual observations and variance of the error term are unequal.
# strongly violates the linear model assumptions.
# plot 2 (Normal QQ plot)
# it seems it is normally distributed (except first observation)
# plot 3 (Scale location)
# line is far away from the zero-line and variance are different over time.
# plot 4 (Residual vs leverage)
# observation 1 has a significant influence on the model,(this one looks like the outlier)
# that means if we remove the observation, the model would have a significant changes.
# Hence, based on all the facts above, I strongly do not recommend this model.
# we can do some changes based on the linear model, however, it is not necessary
# in our case since we dont want much advanced model.



# Prediction
lm_high_pre <- predict(lm_labour, data.frame(year=c(2019:2030))) 

# 2. ARIMA model 
# make it ts data-frame
tsdata_high <- flows_high %>% 
  dplyr::select(val) %>% 
  ts(start = 2008)


# 1) test the serial correlation

lag_length = 10 # number of lag we use in the test.

# Test whether any of a group of autocorrelations of a time series are different from zero.
Box.test(tsdata_high, lag=lag_length, type="Ljung-Box")
# Result : Here for  order of 10 lags, we reject the hypothesis assuming the data are independently distributed.
# So based on the information we have, we are sure that the process has a serieal correlation.
# this also shows that linear model is not appropriate.

# In second, Before apply time-series model, we need to have a stationary process, 
# otherwise no matter what model you apply, it wont work well.

# 2) Test the process whether Stationary

# From the visualization, I find the non-stationary pattern, for further step
# lets try a statistical test. Here I introduced unit-root test.
# unit root test (If a process have unit root that means it is not stationary)
adf.test(tsdata_high)
# we cannot reject the hypothesis that assume the process has unit root.
# Result: the process is may be non-stationary (The series has the unit root)
# After the visualization and the test result, 
# I consider the TS is not a stationary process.

# 3) Transformation (make it stationary)
# Below is what I implemented.

# visualization
par(mar=c(3,3,3,3))
par(mfrow =c(1,1))
plot(tsdata_high)
abline(reg=lm(tsdata_high~time(tsdata_high)))
# You also can see from here the process is not stationary
# As you can see from the plot, we have two issues
# a) variance changing over time (you also can use sqrt function, it depends on the curve)
# b) we have an increasing trend of immigration.


# a) So first removing the unequal variance by using log function

# b) Second, address the trend component by using First/second order difference.
# Because first order still does not give us a stationary process

# Lets implement the test again with first difference
adf.test(diff(log(tsdata_high), d = 1), alternative="stationary", k=0)
# Here, first difference with log transformation already make the process stationary,
# so no need to try second difference.

# So lets find out the ARIMA parameter with auto-correlation function
# ACF and PACF
acf(diff(log(tsdata_high), d = 1))
pacf(diff(log(tsdata_high), d = 1))

# it suggest both AR and MV terms are zero. lets see



# Here you go, since we already know the difference is 1(when d=1, the process is stationary),
# so apply it directly
# ARIMA(p,d,q)
High_arima_010 <- arima(log(tsdata_high), c(0,1,0), method = "ML")
summary(High_arima_010)
# I also tried suspected models.
High_arima_011 <- arima(log(tsdata_high), c(0,1,1), method = "ML")
summary(High_arima_011)

High_arima_110 <- arima(log(tsdata_high), c(1,1,0), method = "ML")
summary(High_arima_110)

# Here, I select ARIMA(0,1,0)

par(mar=c(10,10,10,10))
pre_Labour_arima_010 <- forecast::forecast(High_arima_010, h = 12, lambda = 0, biasadj = TRUE)
autoplot(tsdata_high) + forecast::autolayer(pre_Labour_arima_010)
forecast::checkresiduals(High_arima_010) # residual check

# Based on the AIC information criteria, MASE and other facts, 
# In the end I prefer ARIMA(0,1,0)


# Irregular ---------------------------------------------------------------

# Most of the procedures will be same

# First visualize and check the trend
p4 <- ggplot(flows_irregular, aes(x=as.character(year), y=val, group = 1)) +
  geom_line() +
  xlab("Year")
p4
# Note: So it seems except a pick in 2015, the trend seems flat.
# also, variance are time independent if we exclude the year 2015.
# we have to test for sure.

# 1. Linear model

# Before we apply time series model, lets check with linear regression.
# First thing first, need to check the linear model assumption whether we can apply it

# 1) Endogenous of the residual term (dependency btw residuals and covariates)
# Here, I just check the covariance of the two terms.
cov(lm(val ~ year, data = flows_irregular)$residuals,flows_irregular$year)
# Result: it is very close to the zero, that means residuals are independent 
# from the covariate.

# 2) Autocorrelation of the residual.
# Here I introduced durbinwatson test. e(t) = rho*e(t-1) + v(t)
durbinWatsonTest(lm(val ~ year, data = flows_irregular))
# we cannot reject the H0 : residuals are not auto-correlated.
# so we only can say that residuals might independently distributed.

# 3) Modeling
lm_irregular <- lm(val ~ year, data = flows_irregular)
summary(lm_irregular)
par("mar")
par(mar=c(1,1,1,1))
par(mfrow =c(2,2))
plot(lm_irregular)

# Here I added some notes based on the residual visualization. 
# Generally, we test the model performance by residual analysis.
# Explanation and information extracted from the plot written below

# plot 1 (residuals vs fitted)
# We can observe that residuals are randomply distributed around zero (influential point in 2015).
# plot 2 (Normal QQ plot)
# it seems it is normally distributed (except the observation in 2015)
# plot 3 (Scale location)
# Looks ok, but we can check without the observation in 2015.
# plot 4 (Residual vs leverage)
# observation 7(in 2015) has a significant influence on the model,(this one looks like an outlier)
# that means if we remove the observation, the model would have a significant changes.
# Hence, based on all the facts above, I suggest that try two different model whether include
# the observation in 2015.

# removing the "outlier"
flows_irregular_nooutlier1 <-flows_irregular %>% 
  filter(year != 2015)
lm_irregular_nooutlier1 <- lm(val ~ year, data = flows_irregular_nooutlier1)
summary(lm_irregular_nooutlier1)
par("mar")
par(mar=c(1,1,1,1))
par(mfrow =c(2,2))
plot(lm_irregular_nooutlier1)

# 2nd outlier
flows_irregular_nooutlier2 <-flows_irregular %>% 
  filter(year != c(2015, 2016))
lm_irregular_nooutlier2 <- lm(val ~ year, data = flows_irregular_nooutlier2)
summary(lm_irregular_nooutlier2)
par("mar")
par(mar=c(1,1,1,1))
par(mfrow =c(2,2))
plot(lm_irregular_nooutlier2)


# Prediction
lm_high_pre <- predict(lm_irregular, data.frame(year=c(2019:2030))) 
lm_high_pre1 <- predict(lm_irregular_nooutlier1, data.frame(year=c(2019:2030))) 
lm_high_pre2 <- predict(lm_irregular_nooutlier2, data.frame(year=c(2019:2030))) # *

# As you can see, when we remove the outliers, lm model works much worst than without removing.
# it might because of the number of the data set.
# in regression analysis, many researchers say that there should be at least 10 observations per variable.
# in our case, without moving it , we only have 10, so I dont recommend removing the outlier in lm model.

# So here I introduce imputation method. Here I only show meadian approach,
# since in whole process we have three influntial points, 2014, 2015 and 2016 respectively.
# if we choose mean, it again effect by some other influential points.
# So I simply replace the value in 2015 with the meadian of the whole data(more robust)
median_irregular <- median(flows_irregular$val) %>% round()
# since we dont have decimal number of people, I round it.
flows_irregular_outlierT <- flows_irregular %>% 
  mutate(val = if_else(year %in% c(2014,2015,2016), median_irregular, val))
# keep in mind that outliers are not the "bad" values, it is just an influential values
# for our model. I consider those are the outliers because of the refugee crises in these year plus its pattern.
# It really depends on our own target/objective.
# we dont include/expect any other refugee crisis in the future and this is not our variables,
# so we simply can replace these values by median for the precise forcasting reason.
lm_irregular_nooutlier3 <- lm(val ~ year, data = flows_irregular_outlierT)
summary(lm_irregular_nooutlier3)
par("mar")
par(mar=c(1,1,1,1))
par(mfrow =c(2,2))
plot(lm_irregular_nooutlier3)
# As you can see from the residual plots , this is much more better than aforementioned models.




# 2. ARIMA model 
# make it ts data-frame
tsdata_irregular <- flows_irregular %>% 
  dplyr::select(val) %>% 
  ts(start = 2009)


# 1) test the serial correlation

lag_length = 9 # number of lag we use in the test.

# Test whether any of a group of autocorrelations of a time series are different from zero.
Box.test(tsdata_irregular, lag=lag_length, type="Ljung-Box")
# Result : Here for all the order of the lags, we cannot reject the hypothesis assuming the data are independently distributed.
# So based on the information we have, the process might/might not be independently ditributed.

# In second, Before apply time-series model, we need to have a stationary process, 
# otherwise no matter what model you apply, it wont work well.

# 2) Test the process whether Stationary

# From the visualization, I find the non-stationary pattern, for further step
# lets try a statistical test. Here I introduced unit-root test.
# unit root test (If a process have unit root that means it is not stationary)
adf.test(tsdata_irregular)
# we cannot reject the hypothesis that assume the process has unit root.
# Result: the process is may be non-stationary (The series has the unit root)
# After the visualization and the test result, 
# I consider the TS is not a stationary process.

# 3) Transformation (make it stationary)
# Below is what I implemented.

# visualization
par(mar=c(3,3,3,3))
par(mfrow =c(1,1))
plot(tsdata_irregular)
abline(reg=lm(tsdata_irregular~time(tsdata_irregular)))
# You also can see from here the process is not stationary
# As you can see from the plot, we have two issues
# a) variance changing over time (you also can use sqrt function, it depends on the curve)
# b) we have an increasing trend of immigration.


# a) So first removing the unequal variance by using log function

# b) Second, address the trend component by using First/second order difference.
# Because first order still does not give us a stationary process

# Lets implement the test again with first difference
adf.test(diff(log(tsdata_irregular), d = 1), alternative="stationary", k=0)
# Second difference
adf.test(diff(log(tsdata_irregular), d = 2), alternative="stationary", k=0)
# After second differnece, it is still giving me non-stationary process.
# I think its because of the the aforementioned extreme values.
# For the sake of consistency, I implement the same treatment as lm model here.

# make it ts process
tsdata_irregular_outlierT <- flows_irregular_outlierT %>% 
  dplyr::select(val) %>% 
  ts(start = 2009)

par(mar=c(3,3,3,3))
par(mfrow =c(1,1))
plot(tsdata_irregular_outlierT)
abline(reg=lm(tsdata_irregular_outlierT~time(tsdata_irregular_outlierT)))

adf.test(diff(tsdata_irregular_outlierT, d = 1), alternative="stationary", k=0)
# Here the analysis would be the same, but the only difference is
# I didnt use log transformation since I dont see obvious changing variance in the process.


# So lets find out the ARIMA parameter with auto-correlation function
# ACF and PACF
acf(diff(tsdata_irregular_outlierT, d = 1))
pacf(diff(tsdata_irregular_outlierT, d = 1))

# it suggest AR order of 0 and MV terms order of 1 or 0. lets see



# Here you go, since we already know the difference is 1(when d=1, the process is stationary),
# so apply it directly
# ARIMA(p,d,q)
irregular_arima_010 <- arima(tsdata_irregular_outlierT, c(0,1,0), method = "ML")
summary(irregular_arima_010)

irregular_arima_011 <- arima(tsdata_irregular_outlierT, c(0,1,1), method = "ML")
summary(irregular_arima_011) # This is better.


# Here, I select ARIMA(0,1,1)

par(mar=c(10,10,10,10))
pre_irregular_arima_011 <- forecast::forecast(irregular_arima_011, h = 12, biasadj =TRUE)
autoplot(tsdata_irregular_outlierT) + forecast::autolayer(pre_irregular_arima_011)
forecast::checkresiduals(pre_irregular_arima_011) # residual check

# Based on the AIC information criteria, MASE and other facts, 
# In the end I prefer ARIMA(0,1,1)



# Assylum -----------------------------------------------------------------

# Most of the procedures will be same

# First visualize and check the trend
p5 <- ggplot(flows_asylum, aes(x=as.character(year), y=val, group = 1)) +
  geom_line() +
  xlab("Year")
p5
# Note: So it seems there is an increasing trend with changing variance.

# As you can see, the process is very similar with total trend.
# 1. Linear model

# 1) Endogenous of the residual term (dependency btw residuals and covariates)
# Here, I just check the covariance of the two terms.
cov(lm(val ~ year, data = flows_asylum)$residuals,flows_asylum$year)
# Result: it is very close to the zero, that means residuals are independent 
# from the covariate.

# 2) Autocorrelation of the residual.
# Here I introduced durbinwatson test. e(t) = rho*e(t-1) + v(t)
durbinWatsonTest(lm(val ~ year, data = flows_asylum))
# we reject the H0 : residuals are not auto-correlated.
# Here the difference is residuals of lm is auto correlated.
# lm assumption doesnt hold.

# 3) Modeling
lm_assylum <- lm(val ~ year, data = flows_asylum)
summary(lm_assylum)
par("mar")
par(mar=c(1,1,1,1))
par(mfrow =c(2,2))
plot(lm_assylum)

# plot 1 (residuals vs fitted)
# We can observe that it is very much curved.
# plot 2 (Normal QQ plot)
# it seems it is not normally distributed (2 points are off-set among  10 points)
# plot 3 (Scale location)
# line seems not really horizontal and variance are different over time.
# plot 4 (Residual vs leverage)
# observation 1 has a significant influence on the model,


# Prediction
lm_assylum_pre <- predict(lm_assylum, data.frame(year=c(2019:2030))) 

# 2. ARIMA model 
# make it ts data-frame
tsdata_assylum <- flows_asylum %>% 
  dplyr::select(val) %>% 
  ts(start = 2008)
# 1) test the serial correlation

lag_length = 9 # number of lag we use in the test.

# Test whether any of a group of autocorrelations of a time series are different from zero.
Box.test(tsdata_assylum, lag=lag_length, type="Ljung-Box")
# Result : So data have seriel correlation


# 2) Test the process whether Stationary

# From the visualization, I find the non-stationary pattern, for further step
# lets try a statistical test. Here I introduced unit-root test.
# unit root test (If a process have unit root that means it is not stationary)
adf.test(tsdata_assylum)
# we cannot reject the hypothesis that assume the process has unit root.
# Result: the process maybe non-stationary (The series has the unit root)
# After the visualization and the test result, 
# I consider the TS is not a stationary process.

# 3) Transformation (make it stationary)
# Below is what I implemented.

# visualization
par(mar=c(3,3,3,3))
par(mfrow =c(1,1))
plot(tsdata_assylum)
abline(reg=lm(tsdata_assylum~time(tsdata_assylum)))
# You also can see from here the process is not stationary
# As you can see from the plot, we have two issues
# a) variance changing over time (you also can use sqrt function, it depends on the curve)
# b) we have an increasing trend of immigration.


# a) So first removing the unequal variance by using log function

# b) Second, address the trend component by using First/second order difference.
# Because first order still does not give us a stationary process

# Lets implement the test again with first difference
adf.test(diff(log(tsdata_assylum), d = 1), alternative="stationary", k=0)
# Second difference
adf.test(diff(log(tsdata_assylum), d = 2), alternative="stationary", k=0)
adf.test(log(tsdata_assylum), alternative="stationary", k=0)

# so second difference strongly make the process stationary. However, again it doesnt
# mean that with first difference or without difference the process in not stationary 
# (again tricky statistical test definition).

# So lets find out the ARIMA parameter with auto-correlation function
# ACF and PACF
acf(log(tsdata_assylum))
pacf(log(tsdata_assylum))
# p=1, q=0 (this plot looks more general)
acf(diff(log(tsdata_assylum), d = 1))
pacf(diff(log(tsdata_assylum), d = 1))
# p =1/0, q=0/1
acf(diff(log(tsdata_assylum), d = 2))
pacf(diff(log(tsdata_assylum), d = 2))
# p=0, q= 1
# 
# ARIMA(p,d,q)
assylum_arima_100 <- arima(log(tsdata_assylum), c(1,0,0), method = "ML")
summary(assylum_arima_100)# I prefer this one.

assylum_arima_110 <- arima(log(tsdata_assylum), c(1,1,0), method = "ML")
summary(assylum_arima_110)

assylum_arima_011 <- arima(log(tsdata_assylum), c(0,1,0), method = "ML")
summary(assylum_arima_011)

par(mar=c(10,10,10,10))
assylum_arima_100_pre <- forecast::forecast(assylum_arima_100, h = 13, lambda = 0, biasadj = TRUE)
autoplot(tsdata_assylum) + forecast::autolayer(assylum_arima_100_pre)
forecast::checkresiduals(assylum_arima_100) # residual check
# Based on the AIC information criteria, MASE and other facts, 
# In the end I prefer ARIMA(1,0,0)


