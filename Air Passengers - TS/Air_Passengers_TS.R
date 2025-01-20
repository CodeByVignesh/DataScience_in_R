#Loading Dataset
data("AirPassengers")

#type of the dataset
class(AirPassengers)


#starting of the dataset
start(AirPassengers)

#end of the dataset
end(AirPassengers)

#structrure of the dataset
str(AirPassengers)

View(AirPassengers)

summary(AirPassengers)

cycle(AirPassengers)


head(AirPassengers)

#plot the graph to see the Trend
#plot the abline to see the Mean Line over the trend
#Abline - Absolute Line
#lm = Linear Model
#Plot time based on the time and passengers count
plot(AirPassengers)
abline(
  reg = lm(AirPassengers~time(AirPassengers))
)

#plot the box plot to see,  how values are distributed
boxplot(AirPassengers~cycle(AirPassengers))

#Now the model is not stationary but has a trend
#so make the model as stationary
# mean, variance and co variance should be constant to
# make the values stationary

plot(log(AirPassengers))
abline(
  reg = lm(log(AirPassengers)~time(AirPassengers))
)

#now the variance has become constant

#to make the mean value constant -> take differentiation of
#the log value

plot(diff(log(AirPassengers)))
abline(
  reg = lm(diff(log(AirPassengers))~time(diff(log(AirPassengers))))
)

#now the mean value is constant and the values are stationary

#so we can start using the time series modeling

#ARIMA - Auto Regressive Integraed Moving Average

# AR I MA
# p  d q

#Arima model needs p,d,q values to get model predicted

#so we need to find the p,d,q values

#to find that values we need to use acf (Auto correlation function)

acf(diff(log(AirPassengers)))
# from this graph we identify the q value, here q=1, (last upward line before the first downward line)

#to find the value of the p, we use pacf function (Partial Auto correlation function)

pacf(diff(log(AirPassengers)))

# from this graph we identify the p value, here p=0, (last upward line before the first downward line)

#and differentiation value is 1, because we got the data stationary after doing the first differentitaion 
#itself, its the value until we get the data stationary

#training ARIMA model
fit = arima(log(AirPassengers), c(0,1,1), seasonal = list(order=c(0,1,1), period=12))

pred = predict(fit, n.ahead = 5*12)

head(pred)

#Now the predicted values are in log form
# to change the log into decimal form, we need to do -> e^Pred
# where e=2.718

pred1 = 2.718^pred$pred

pred1
# we got the prediction values

#we can plot the prediction values

ts.plot(AirPassengers, pred1, log="y", lty=c(1,3))

head(pred1)

pred1

#rounding the values

pred1 = round(pred1, digits = 0)
pred1
