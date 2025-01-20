#download packages
#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("openair")
#install.packages("readxl")
#install.packages("fable")
#install.packages("tsibble")

#load the Library
library(tidyverse)
library(lubridate)
library(operair)
library(readxl)
library(fable)
library(tsibble)

#choose the file
aqi_data = read_excel(file.choose())

#structure of the data
str(aqi_data)

aqi_data_date = as.Date(aqi_data$date)

class(aqi_data_date)

summary(aqi_data_date)

head(aqi_data)

summary(aqi_data)

#visualize the plot
plot(aqi_data$date,aqi_data$AQI, main = "Data on Year VS AQI", xlab = "Year", ylab = "AQI Index", col="Blue", type = "l")

#calculate the monthly mean
aqi_data$month = floor_date(aqi_data$date, "month")
aqi_mean = aqi_data %>% group_by(month) %>% summarize(AQI = mean(AQI))

#change the Date format
aqi_monthly = aqi_mean %>% mutate(Date = yearmonth(as.character(month))) %>% as_tsibble(index = Date)

arima_model <- ARIMA(aqi_monthly$AQI)


#fitting the model
#aqi_model = aqi_monthly %>% model(ARIMA = ARIMA(AQI), ETS=ETS(AQI~season(c("A")))) %>% mutate(AVERAGE = (ARIMA+ETS)/2)
#Fitting the model
aqi_models<-aqi_monthly %>% model(ETS=ETS(AQI~ season(c("A"))))

#forecast data
forecast_aqi = aqi_models %>% forecast(bootstap=TRUE, times=100, h="3 years")

forecast_aqi

autoplot(forecast_aqi) + autolayer(aqi_monthly, series = "Forecasts") + theme_minimal()


