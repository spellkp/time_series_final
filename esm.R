install.packages('forecast',dependencies = T)
install.packages('tseries')
install.packages(c('expsmooth','lmtest','zoo','seasonal','haven','fma'))
library(haven)
library(forecast)
library(fma)
library(tseries)
library(expsmooth)
library(lmtest)
library(zoo)
library(ggplot2)

#reading in data
pm2data <- read.csv('PM_2_5_Raleigh2.csv', header=TRUE)

#removing PM2.5 values that are zero or negative
library(dplyr)
pm2data <- pm2data %>% filter(Daily.Mean.PM2.5.Concentration != 0)
pm2data <- pm2data[pm2data$Daily.Mean.PM2.5.Concentration >= 0, ]


#aggregate into monthly data
library(tidyverse)
library(lubridate)
pm2data$Date = as.Date(pm2data$Date, '%m/%d/%Y')
monthly = pm2data %>% group_by(Month=floor_date(Date, "month")) %>%
  summarize(Monthly.Mean.PM2.5.Concentration=mean(Daily.Mean.PM2.5.Concentration))

#create TS object
ts_object <- ts(monthly$Monthly.Mean.PM2.5.Concentration, start = 2014, frequency =12)

#split data
training=subset(ts_object,end=length(ts_object)-6)
test=subset(ts_object,start=length(ts_object)-5)

#multiplicative HW ESM
HWES.PM.train <- hw(training, seasonal = "multiplicative", initial='optimal')
summary(HWES.PM.train)
test.results=forecast(HWES.PM.train,h=6)

error=test-test.results$mean
mult_MAE=mean(abs(error)) #0.93
mult_MAPE=mean(abs(error)/abs(test)) #0.097

#time plot (predicted vs observed values in test set)
df_test = data.frame(Time = tail(monthly$Month,6), Observed = test, Predicted = test.results$mean)
ggplot() + 
  geom_line(data = df_test, aes(x = Time, y = Observed), color = "black") +
  geom_line(data = df_test, aes(x = Time, y = Predicted), color = "red") +
  ggtitle("Observed vs Predicted PM 2.5 Concentration For Test Data") +
  ylab('PM 2.5 Concentration') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size=20), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=18))

#forecast plot 
autoplot(HWES.PM.train)+
  autolayer(fitted(HWES.PM.train),series="Fitted")+ylab("PM with HW Multiplicative ESM Forecast") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size=20), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=18),
        legend.position = 'none')