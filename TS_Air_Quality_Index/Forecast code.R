#Load the libraries
library(tidyverse)
library(lubridate)
library(openair)
library(readxl)
library(fable)
library(tsibble)

#Choose the file

aqi_data<-read_excel(file.choose())

str(aqi_data)

#Visualize the data
timePlot(aqi_data, pollutant = c("AQI"), avg.time = "month")

#Calculating monthly means for each year

aqi_data$month<- floor_date(aqi_data$date, "month")

aqi_mean<-aqi_data %>% group_by(month) %>% summarize(AQI = mean(AQI))

#Changing the date format and converting into tsibble
aqi_monthy <- aqi_mean%>% mutate(Date = yearmonth(as.character(month))) %>%
  as_tsibble(index = Date)

#Fitting the model
aqi_models<-aqi_monthy %>% model(ARIMA=ARIMA(AQI),
                                 ETS=ETS(AQI~ season(c("A"))))%>% 
                                  mutate(AVERAGE=(ARIMA+ETS)/2)

#Forecast data
forecast_aqi<- aqi_models %>%forecast(bootstap=TRUE,times=100,h="1 year")

#Plotting the Forecast data
autoplot(forecast_aqi) +autolayer(aqi_monthy, series="Forecasts")+theme_minimal()


