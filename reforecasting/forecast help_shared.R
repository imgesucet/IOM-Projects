
#################################################################################
#################################################################################

# Linear forecast

options(scipen=999)
library(forecast)

# latest package - latest edition of book uses ARIMA, not auto.arima
# https://fable.tidyverts.org/
library(fable)
library(tsibble)
library(feasts)
library(lubridate)
library(tidyr)
library(readxl)
library(tidyverse)
library(xlsx)
library(ggbeeswarm)
library(grid)
library(tidytext)
library(ISOcodes)
library(data.table)
library(sjmisc)
library(openxlsx)
library(ggrepel)
library(eurostat)
library(countrycode)
library(stringi)
library(extrafont)
library(ggpubr)
library(car)

#### data ####

# averages by scenarios - not necessary for forecast but to compare the estimations from experts
# scenario exercise data to compare with linear prediction

# saveRDS(data, file = "C:/Users/eduar/OneDrive - International Organization for Migration - IOM/horizon 2020/journal article/code/forecast help/mean_scenario.rds")

data <- readRDS("C:/Users/eduar/OneDrive - International Organization for Migration - IOM/horizon 2020/journal article/code/forecast help/mean_scenario.rds")

readRDS(file = "C:/Users/eduar/OneDrive - International Organization for Migration - IOM/horizon 2020/journal article/code/forecast help/forecast.rds" )

################
####  TOTAL ####
################

# cleaning data - remove an na that was used for the chart
tot <- flows_forecast %>% 
  filter(flow=="total") 

# linerar prediction
predict_ols <- lm(val ~ year, data = tot)

# out of sample prediction
tot_lin <- predict(predict_ols, data.frame(year=c(2018:2030))) 

# alternatiove method using time series following this book
# https://otexts.com/fpp2/regression.html

tot_ts <- ts(tot, start = 2008)

fit.consBest <- forecast::tslm(
  val ~ year,
  data=tot_ts)

summary(fit.consBest)

# same result as with lm()
# -161207531+(81066*2030)

# test is not below 0.05, so we may proceed to forecast
checkresiduals(fit.consBest)

# out of sample periods
newdata <- data.frame(
  year = c(2018:2030))

# object with forecasted linear including confidence levels for prediction intervals (80 and 95)
tot_forecast <- forecast::forecast(fit.consBest, newdata = newdata)

# scenario exercise data to compare with linear prediction
data_tot <- data %>% 
  dplyr::filter(variable=="total") 

# total migration plot
a <- ggplot(non_eu, aes(x=year,y=val)) +
  geom_line(size=1.5) +
  autolayer(tot_forecast,
                colour="lightblue",
                alpha = .5) +
  geom_point(data = data_tot, aes(x=year,y=val, color=scenario_name),size=3.5)+
  scale_color_manual(values = cols_new,aesthetics = c("color", "fill")) +
  labs(y="Migration inflows (millions)",
       x=NULL,
       title="linear",
       color="Scenario")+
  theme_bw()+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(nrow = 2))+
  coord_cartesian(
    ylim = c(800000,5000000))
  
# ARIMA
# books used
# 1 https://otexts.com/fpp3/non-seasonal-arima.html
# 2 https://nwfsc-timeseries.github.io/atsa-labs/sec-boxjenkins-forecast.html


https://otexts.com/fpp2/stationarity.html
# 3 components to have in mind:
# - Seasonal component refers to fluctuations in the data related to calendar cycles.
# - Trend component is the overall pattern of the series
# - Cycle component consists of decreasing or increasing patterns that are not seasonal. 

# more relvant for us
https://otexts.com/fpp2/stationarity.html

# A stationary time series is one whose properties do not depend on the time at which the series 
# is observed. Thus, time series with trends, or with seasonality, are not stationary - the trend 
# and seasonality will affect the value of the time series at different times. On the other hand, a 
# white noise series is stationary - it does not matter when you observe it, it should look much the 
# same at any point in time.

- STEPS -
https://otexts.com/fpp3/arima-r.html
# 8.7 - how does auto.arima() - ARIMA - work? 

# The auto.arima() function in R uses a variation of the Hyndman-Khandakar 
# algorithm (Hyndman & Khandakar, 2008), which combines unit root tests, 
# minimisation of the AICc and MLE to obtain an ARIMA model. The arguments 
# to auto.arima() provide for many variations on the algorithm. What is 
# described here is the default behaviour.

# see exact procedure here: https://otexts.com/fpp2/arima-r.html 

# Modelling procedure

# When fitting an ARIMA model to a set of (non-seasonal) time series data, 
# the following procedure provides a useful general approach.
# 
# 1. Plot the data and identify any unusual observations.
# 2. If necessary, transform the data (using a Box-Cox transformation) to stabilise 
# the variance. 
# 3. If the data are non-stationary, take first differences of the data 
# until the data are stationary. 
# 4. Examine the ACF/PACF: Is an ARIMA(p,d,0) or ARIMA(0,d,q) model appropriate?
# 5. Try your chosen model(s), and use the AICc to search for a better model.
# 6. Check the residuals from your chosen model by plotting the ACF of the residuals, 
# and doing a portmanteau test of the residuals. If they do not look like white noise, 
# try a modified model.
# 7. Once the residuals look like white noise, calculate forecasts.

# The Hyndman-Khandakar algorithm only takes care of steps 3-5. So even if you 
# use it, you will still need to take care of the other steps yourself.

# step 1

# There is nothing unusual about the time plot and there appears to be no need to 
# do any data adjustments.

# step 2
# There is no evidence of changing variance, so we will not do a Box-Cox transformation.

# step 3 
# The data are clearly stationary, as the series does not wanders up and down for 
# long periods. Consequently, we will not take a first difference of the data. 
# As lags do not cross the cut off, Arima 0 seems good candidate

tsdata <- tot %>% 
  dplyr::select(val) %>% 
  ts(start = 2008)

tsdata %>% ggtsdisplay(main="")

fit <- forecast::auto.arima(tsdata, 
                            trace=T, 
                            stepwise = T,
                            stationary=T,
                            approximation=FALSE,
                            seasonal = F)

# why forecasted values are constant?
# answer by developer of function auto.forecast
# https://stats.stackexchange.com/questions/135651/arima-predictions-constant

# There's nothing wrong. The upcoming expected value from an ARIMA(0,1,0) process is 
# equal to the last observed value

# https://stats.stackexchange.com/questions/439608/out-of-sample-rolling-window-forecast-with-arima0-0-0-with-non-zero-mean

# ARIMA 0,0,0 is a very simple model, is that bad? Apparently, simple models can outperform more complicated
# one. 
# https://stats.stackexchange.com/questions/124955/is-it-unusual-for-the-mean-to-outperform-arima

# interpretation of best fit ARIMA 0,0,0

# https://stats.stackexchange.com/questions/236566/how-to-interpret-arima0-0-0/236584

# Reply 1
# An ARIMA(0,0,0) model with zero mean is white noise, so it means that the errors are uncorrelated across time.
# This doesn't imply anything about the size of the errors, so no in general it is not an indication of good 
# or bad fit.

# step 6
forecast::checkresiduals(fit)

# The ACF plot of the residuals from the ARIMA(0,0,0) model shows that all 
# autocorrelations are within the threshold limits, indicating that the residuals 
# are behaving like white noise (good thing). A portmanteau test returns a p-value of
# 0.08, also suggesting that the residuals are white noise.


# step 7 forecast from chosen model
fit <- forecast::forecast(fit, h = 13)

autoplot(forecast::forecast(fit, h = 13))

#### plot arima _ total migration

b <- ggplot(non_eu, aes(x=year,y=val)) +
  geom_line(size=1.5) +
  geom_forecast(fit,
            colour="lightblue",
            alpha = .5) +
  geom_point(data = data_tot, aes(x=year,y=val, color=scenario_name),size=3.5)+
  scale_color_manual(values = cols_new,aesthetics = c("color", "fill")) +
  labs(y="Migration inflows (millions)",
       x=NULL,
       title="white noise 0,0,0",
       color="Scenario")+
  theme_bw()+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(nrow = 2))+
  coord_cartesian(
    ylim = c(800000,5000000))

# same result using the latest version of ARIMA

ari_tot <- tot %>% 
  as_tsibble(index = year) 

# same result 
fit <- ari_tot %>% 
  model(arima=ARIMA(val ~ PDQ(0,0,0), 
                    stepwise = T,
                    greedy =T))

report(fit)

fit %>% forecast(h=13) %>% autoplot(ari_tot)

# Random walk

fit <- forecast::auto.arima(tsdata, 
                            trace=T, 
                            stepwise = F,
                            stationary=F,
                            approximation=FALSE,
                            seasonal = F)
forecast::checkresiduals(fit)

fit <- forecast::forecast(fit, h = 13)

#### plot arima _ RANDOM WALK 

c <- ggplot(non_eu, aes(x=year,y=val)) +
  geom_line(size=1.5) +
  geom_forecast(fit,
                colour="lightblue",
                alpha = .5) +
  geom_point(data = data_tot, aes(x=year,y=val, color=scenario_name),size=3.5)+
  scale_color_manual(values = cols_new,aesthetics = c("color", "fill")) +
  labs(y="Migration inflows (millions)",
       x=NULL,
       title="0,1,0 random walk",
       color="Scenario")+
  theme_bw()+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(nrow = 2))+
  coord_cartesian(
    ylim = c(800000,5000000))

ggarrange(a, b,c, common.legend = TRUE, ncol = 3,legend = "bottom")


# important lesson from Disney - p. 24
# https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/467405/Migration_Forecasting_report.pdf

# This illustrates the importance of treating migration as a non-stationary process where 
# systemic 'shocks' are expected.

# Consider more the nonstationary forecasts


################
#### labour ####

# use 11 years. in report, we presented only 10 years to make it consistant across migration flows
# but for forecast, it makes sense to use one more data point. 

# Labor
mig_res_labor <- flows_forecast %>% 
  filter(flow=="labour")

# labour forecast

predict_ols <- lm(val ~ year, data = mig_res_labor)
total_lin <- predict(predict_ols, data.frame(year=c(2019:2030)))

lab_linear <- mig_res_labor %>% 
  dplyr::select(val, year)

lab_linear <- ts(lab_linear, start = 2008)

fit.consBest <- forecast::tslm(
  val ~ year,
  data=lab_linear)

summary(fit.consBest)

# test is not below 0.05, so we may proceed to forecast
checkresiduals(fit.consBest)

# out of sample periods
newdata <- data.frame(
  year = c(2019:2030))

# object with forecasted linear including confidence levels for prediction intervals (80 and 95)
lab_forecast <- forecast::forecast(fit.consBest, newdata = newdata)

# scenario exercise data to compare with linear prediction
lab_per <- data %>% filter(variable=="labour")

a <- ggplot(mig_res_labor, aes(x=year,y=val)) +
  geom_line(size=1.5) +
  autolayer(lab_forecast,
            colour="lightblue",
            alpha = .5) +
  geom_point(data = lab_per, aes(x=year,y=val, color=scenario_name),size=3.5)+
  scale_color_manual(values = cols_new,aesthetics = c("color", "fill")) +
  labs(y="Labour migration inflows (thousands)",x=NULL,
       title="Linear", 
       color="Scenario")+
  theme_bw()+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(nrow = 2))

# ARIMA

lab_arima  <- mig_res_labor %>% 
  ungroup() %>% 
  dplyr::select(val) %>% 
  ts(start = 2008)

fit <- forecast::auto.arima(lab_arima, 
                            trace=T, 
                            stepwise = F,
                            stationary=T,
                            approximation=FALSE,
                            seasonal = F)

# check result
forecast::checkresiduals(fit)

autoplot(forecast::forecast(fit, h = 12))
fit <- forecast::forecast(fit, h = 12)

# same result using the latest version of ARIMA

ari_tot <- mig_res_labor %>% 
  ungroup() %>% 
  dplyr::select(val,year) %>% 
  as_tsibble(index = year) 

# same result 
fit2 <- ari_tot %>% 
  model(arima=ARIMA(val ~ PDQ(0,0,0), 
                    stepwise = T,
                    greedy =T))

report(fit2)

fit2 %>% forecast(h=13) %>% autoplot(ari_tot)

#### plot arima _ total migration

b <- ggplot(mig_res_labor, aes(x=year,y=val)) +
  geom_line(size=1.5) +
  geom_forecast(fit,
            colour="lightblue",
            alpha = .5) +
  geom_point(data = lab_per, aes(x=year,y=val, color=scenario_name),size=3.5)+
  scale_color_manual(values = cols_new,aesthetics = c("color", "fill")) +
  labs(y="Labour migration inflows (thousands)",x=NULL,
       title="Arima 1,0,0", 
       color="Scenario")+
  theme_bw()+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(nrow = 2))

ggarrange(a, b, common.legend = TRUE, ncol = 2,legend = "bottom")

################################
#### High-skilled migration ####

mig_res_labor <- flows_forecast %>% filter(flow=="high") 

predict_ols <- lm(val ~ year, data = mig_res_labor)
hig_lin <- predict(predict_ols, data.frame(year=c(2019:2030)))

high_linear <- mig_res_labor %>% 
  dplyr::select(val, year)

high_linear <- ts(high_linear, start = 2008)

fit.consBest <- forecast::tslm(
  val ~ year,
  data=high_linear)

summary(fit.consBest)

# test is not below 0.05, so we may proceed to forecast
checkresiduals(fit.consBest)

# out of sample periods
newdata <- data.frame(
  year = c(2019:2030))

# object with forecasted linear including confidence levels for prediction intervals (80 and 95)
high_forecast <- forecast::forecast(fit.consBest, newdata = newdata)

# scenario exercise data to compare with linear prediction
high_per <- data %>% filter(variable=="high")

a <- ggplot(mig_res_labor, aes(x=year,y=val)) +
  geom_line(size=1.5) +
  autolayer(high_forecast,
            colour="lightblue",
            alpha = .5) +
  geom_point(data = high_per, aes(x=year,y=val, color=scenario_name),size=3.5)+
  scale_color_manual(values = cols_new,aesthetics = c("color", "fill")) +
  labs(y="High-skilled migration inflows (thousands)",
       x=NULL,
       title="Linear",
       color="Scenario")+
  theme_bw()+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(nrow = 2))

# ARIMA

lab_arima  <- mig_res_labor %>% 
  ungroup() %>% 
  dplyr::select(val) %>% 
  ts(start = 2008)

fit <- forecast::auto.arima(lab_arima, 
                            trace=T, 
                            stepwise = F,
                            stationary=T,
                            approximation=FALSE,
                            seasonal = F)

# check result
forecast::checkresiduals(fit)

autoplot(forecast::forecast(fit, h = 12))
fit <- forecast::forecast(fit, h = 12)

#### plot arima _ total migration

b <- ggplot(mig_res_labor, aes(x=year,y=val)) +
  geom_line(size=1.5) +
  geom_forecast(fit,
                colour="lightblue",
                alpha = .5) +
  geom_point(data = high_per, aes(x=year,y=val, color=scenario_name),size=3.5)+
  scale_color_manual(values = cols_new,aesthetics = c("color", "fill")) +
  labs(y="High-skilled migration inflows (thousands)",
       title="Arima 0,0,0", 
       color="Scenario")+
  theme_bw()+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(nrow = 2))


# DIFFERENT result using the latest version of ARIMA

ari_tot <- mig_res_labor %>% 
  ungroup() %>% 
  dplyr::select(val,year) %>% 
  as_tsibble(index = year) 

# same result 
fit2 <- ari_tot %>% 
  model(arima=ARIMA(val ~ PDQ(0,0,0), 
                    stepwise = T,
                    greedy =T))

report(fit2)

fit2 %>% forecast(h=13) %>% autoplot(ari_tot)

# random walk

fit <- forecast::auto.arima(lab_arima, 
                            trace=T, 
                            stepwise = F,
#                            stationary=T,
                            approximation=FALSE,
                            seasonal = F)

# check result
forecast::checkresiduals(fit)

autoplot(forecast::forecast(fit, h = 12))
fit <- forecast::forecast(fit, h = 12)


c <- ggplot(mig_res_labor, aes(x=year,y=val)) +
  geom_line(size=1.5) +
  geom_forecast(fit,
                colour="lightblue",
                alpha = .5) +
  geom_point(data = high_per, aes(x=year,y=val, color=scenario_name),size=3.5)+
  scale_color_manual(values = cols_new,aesthetics = c("color", "fill")) +
  labs(y="Labour migration inflows (thousands)",x=NULL,
       title="random wal, 0,1,0", 
       color="Scenario")+
  theme_bw()+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(nrow = 2))


ggarrange(a, b, c, common.legend = TRUE, ncol = 3,legend = "bottom")


################
#### asylum ####
mig_res_asylum <- flows_forecast %>% 
  filter(flow== "asylum") %>% 
  dplyr::select(-flow)

predict_ols <- lm(val ~ year, data = mig_res_asylum)
asy_lin <- predict(predict_ols, data.frame(year=c(2019:2030)))

# forecast

asylum_linear <- ts(mig_res_asylum, start = 2008)

fit.consBest <- forecast::tslm(
  val ~ year,
  data=asylum_linear)

summary(fit.consBest)

# test is not below 0.05, so we may proceed to forecast
checkresiduals(fit.consBest)

# out of sample periods
newdata <- data.frame(
  year = c(2019:2030))

# object with forecasted linear including confidence levels for prediction intervals (80 and 95)
asy_forecast <- forecast::forecast(fit.consBest, newdata = newdata)

# scenario exercise data to compare with linear prediction
asy_per <- data %>% filter(variable=="asylum")

a <- ggplot(mig_res_asylum, aes(x=year,y=val)) +
  geom_line(size=1.5) +
  autolayer(asy_forecast,
            colour="lightblue",
            alpha = .5) +
  geom_point(data = asy_per, aes(x=year,y=val, color=scenario_name),size=3.5)+
  scale_color_manual(values = cols_new,aesthetics = c("color", "fill")) +  
  labs(y="Asylum applications (thousands)",x=NULL,
       title="Past first time asylum applications and expected applications\nin 2030 on average by scenario, thousands",
       color="Scenario")+
  theme_bw()+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(nrow = 2))

# ARIMA

lab_arima <- mig_res_asylum %>% 
  dplyr::select(val) %>% 
  ts(start = 2008)

fit <- forecast::auto.arima(lab_arima, 
                            trace=T, 
                            stepwise = F,
                            stationary=T,
                            approximation=FALSE,
                            seasonal = F,
                            lambda = 0) # added for non negative results

# check result
forecast::checkresiduals(fit)

autoplot(forecast::forecast(fit, h = 12))
fit <- forecast::forecast(fit, h = 12)

#### plot arima _ total migration

# https://people.duke.edu/~rnau/411arim.htm#arima100

b <- ggplot(mig_res_asylum, aes(x=year,y=val)) +
  geom_line(size=1.5) +
  geom_forecast(fit,
                colour="lightblue",
                alpha = .5) +
  geom_point(data = asy_per, aes(x=year,y=val, color=scenario_name),size=3.5)+
  scale_color_manual(values = cols_new,aesthetics = c("color", "fill")) +
  labs(y="High-skilled migration inflows (thousands)",
       title="Arima 1,0,0-first order autoregressive", 
       color="Scenario")+
  theme_bw()+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(nrow = 2))


# DIFFERENT result using the latest version of ARIMA
# suggests 0,0,1
ari_tot <- mig_res_asylum %>% 
  as_tsibble(index = year) 

# same result 
fit2 <- ari_tot %>% 
  model(arima=ARIMA(val ~ PDQ(0,0,0), 
                    stepwise = T,
                    greedy =T))

report(fit2)

fit2 <- fit2 %>% forecast(h=13) %>% autoplot(ari_tot, title ="0,0,1-")

# interpretation
# https://www.quora.com/What-is-the-interpretation-of-an-auto-arima-0-0-1-result-in-r

# Interpretation:
# 
#   0,0,1
# 
# The PACF value is 0 i.e. p=0 ,this means that the lags in PACF graph are not significant and this is 
# interpreted as trend component of the time series is not playing a significant role.
# 
# d=0
# This means that the Time series provided is stationary and does not require differencing.
# q=1
# The ACF value is 1. Which means that the lag after which the ACF value becomes zero in the graph 
# is 1. (excluding the first lag, which is correlation of our target variable with itself). This 
# means that our time series has a randomness component and the ACF after the lag 2 becomes 0. 
# This is because the first spike in ACF plot is because of the correlation of the variable with 
# itself(as I have mentioned before).


# random walk

fit <- forecast::auto.arima(lab_arima, 
                            trace=T, 
                            stepwise = F,
                            #                            stationary=T,
                            approximation=FALSE,
                            seasonal = F,
                            lambda = 0)

# check result
forecast::checkresiduals(fit)

autoplot(forecast::forecast(fit, h = 12))
fit <- forecast::forecast(fit, h = 12)


c <- ggplot(mig_res_asylum, aes(x=year,y=val)) +
  geom_line(size=1.5) +
  geom_forecast(fit,
                colour="lightblue",
                alpha = .5) +
  geom_point(data = asy_per, aes(x=year,y=val, color=scenario_name),size=3.5)+
  scale_color_manual(values = cols_new,aesthetics = c("color", "fill")) +
  labs(y="Labour migration inflows (thousands)",x=NULL,
       title="random walk, 0,1,0", 
       color="Scenario")+
  theme_bw()+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(nrow = 2))


ggarrange(a, b, c, fit2, common.legend = TRUE, ncol = 4,legend = "bottom")



###################
#### irregular ####
total_ir <- flows_forecast %>% filter(flow=="irregular") %>% select(-flow)

predict_ols <- lm(val ~ year, data = total_ir)
irr_lin <- predict(predict_ols, data.frame(year=c(2019:2030)))

irregular_linear <- ts(total_ir, start = 2009)

fit.consBest <- forecast::tslm(
  val ~ year,
  data=irregular_linear)

summary(fit.consBest)

# test is not below 0.05, so we may proceed to forecast
checkresiduals(fit.consBest)

# out of sample periods
newdata <- data.frame(
  year = c(2019:2030))

# object with forecasted linear including confidence levels for prediction intervals (80 and 95)
irregular_forecast <- forecast::forecast(fit.consBest, newdata = newdata)

# scenario exercise data to compare with linear prediction
irr_per <- data %>% filter(variable=="irregular")

a <- ggplot(irregular, aes(x=year,y=val)) +
  geom_line(size=1.5) +
  autolayer(irregular_forecast,
            colour="lightblue",
            alpha = .5,
            show.legend =T) +
  geom_point(data = irr_per, aes(x=year,y=val, color=scenario_name),size=3.5)+
    scale_color_manual(values = cols_new,aesthetics = c("color")) +  
  labs(y="Irregular border crossings (thousands)",x=NULL,
       title="linear",
       color="Scenario", fill="interval")+
  theme_bw()+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(nrow = 2, order = 1))

# ARIMA

irregular_arima <-  irregular %>% dplyr::select(val) 
irregular_arima <- ts(irregular_arima, start = 2009)

fit <- forecast::auto.arima(irregular_arima, 
                            trace=T, 
                            stepwise = F,
                            stationary=T,
                            approximation=FALSE,
                            seasonal = F,
                            lambda = 0)

summary(fit)
forecast::checkresiduals(fit)

fit <- forecast::forecast(fit, h = 13)

b <- ggplot(irregular, aes(x=year,y=val)) +
  geom_line(size=1.5) +
  autolayer(fit,
            colour="lightblue",
            alpha = .5,
            show.legend =T) +
  geom_point(data = irr_per, aes(x=year,y=val, color=scenario_name),size=3.5)+
  scale_color_manual(values = cols_new,aesthetics = c("color")) +  
  labs(y=NULL,x=NULL,
       title="arima 0,0,0",
       color="Scenario", fill="interval")+
  theme_bw()+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(nrow = 2, order = 1))

# random walk

fit <- Arima(irregular_arima, order = c(0,1,0))
fit <- forecast::forecast(fit, h = 12)

c <- ggplot(irregular, aes(x=year,y=val)) +
  geom_line(size=1.5) +
  autolayer(fit,
            colour="lightblue",
            alpha = .5,
            show.legend =T) +
  geom_point(data = irr_per, aes(x=year,y=val, color=scenario_name),size=3.5)+
  scale_color_manual(values = cols_new,aesthetics = c("color")) +  
  labs(y=NULL,x=NULL,
       title="arima 0,1,0 - random walk",
       color="Scenario", fill="interval")+
  theme_bw()+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(nrow = 2, order = 1))

ggarrange(a, b, c, common.legend = TRUE, ncol = 3,legend = "bottom")




# new ARIMA function - 0,0,0

ari_irregular <- irregular %>% 
  as_tsibble(index = year) 

# avoid negative forecasted values https://stats.stackexchange.com/questions/217474/is-there-a-way-to-prevent-forecasting-negative-values-with-arima-or-add-constra
fit <- ari_irregular %>% 
  model(arima=ARIMA(val ~ PDQ(0,0,0), 
                    stepwise = T,
                    greedy =T))

report(fit)

fit %>% forecast(h=13) %>% autoplot(ari_irregular)

