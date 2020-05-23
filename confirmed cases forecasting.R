#CLEARING THE ENVIRONMENT
rm(list = ls())

#IMPORTING THE DATA
ts_data <- read.csv("C:\\Users\\Shekhar Lamba\\Documents\\Datasets\\COVID-19\\time_series_covid_19_confirmed.csv", header = F)
ts_date <- ts_data[ts_data$V2 == "Country/Region",]
ts_india <- ts_data[ts_data$V2 == "India",]
ts_india <- as.data.frame(rbind(ts_date, ts_india)) 
ts_india$V1 <- NULL
ts_india$V2 <- NULL
ts_india$V3 <- NULL
ts_india$V4 <- NULL
ts_india <- as.data.frame(t(ts_india)) 
row.names(ts_india) <- NULL
names(ts_india) <- c("Date", "Confirmed_Cases")
ts_india$Confirmed_Cases <- as.numeric(levels(ts_india$Confirmed_Cases))[ts_india$Confirmed_Cases]
str(ts_india)
ts_india$Date <- as.Date(ts_india$Date, format = "%m/%d/%y")
summary(ts_india)

#FITTING A PROPHET MODEL
library(prophet)
ts_pro <- ts_india
names(ts_pro) <- c('ds', 'y')
prophet_model <- prophet(ts_pro)

#FORECASTING BY PROPHET MODEL
future_dates <- make_future_dataframe(prophet_model, periods = 21)
prophet_forecast <- predict(prophet_model, future_dates)
plot(prophet_model, prophet_forecast)

#CHECKING ACCURACY OF PROPHET MODEL
predicted <- prophet_forecast$yhat[1:121]
actual <- prophet_model$history$y
plot(actual, predicted)
summary(lm(predicted ~ actual))

#EXPLANATORY ANALYSIS
library(fpp2)
set.seed(1234)
ts_confirmed <- ts(ts_india[,2], start = c(2020, as.numeric(format(ts_india[1,1], "%j"))), frequency = 366)
autoplot(ts_confirmed) + ggtitle("Showing the increase in the number of Confirmed Cases") +
  xlab("Days observed") + ylab("Number of Confirmed Cases")

#SEPARATING INTO TRAIN AND TEST
set.seed(1234)
training_data <- head(ts_confirmed, round(length(ts_confirmed) * 0.95))
h <- length(ts_confirmed) - length(training_data)
testing_data <- tail(ts_confirmed, h)
autoplot(training_data) + autolayer(testing_data) + ggtitle("Showing the separation of data into Train and Test") +
  xlab("Days observed") + ylab("Number of Confirmed Cases") + labs(color = "")

#REMOVING THE TREND IN THE DATA
ts_confirmed_diff <- diff(ts_confirmed, differences = 2)
autoplot(ts_confirmed_diff) + ggtitle("Showing the data after removing trend by taking the 2nd difference") +
  xlab("Days observed") + ylab("Number of Confirmed Cases")

#ARIMA MODEL FITTING
arima_model <- auto.arima(training_data, d = 2, stepwise = F, approximation = F, trace = T)
summary(arima_model) #Residual SD = 145.3479
checkresiduals(arima_model)

#FORECASTING TESTING DATA BY ARIMA MODEL
arima_forecast <- forecast(arima_model, h = length(testing_data))
options(scipen = 999)
plot(arima_forecast) 
lines(testing_data, col = "red")
legend("topleft", lty = 1, bty = "n", col=c("red","blue"), c("Actual Data","Forecasted Data"))

#CHECKING ACCURACY OF ARIMA MODEL
arima_mean <- arima_forecast$mean
plot(testing_data, col = "red")
lines(arima_mean, col = "darkblue")
legend("topleft", lty = 1, bty = "n", col=c("red","blue"), c("Actual Data","Forecasted Data"))
accuracy(arima_forecast, testing_data)

#FITTING AN ETS MODEL
ets_model <- ets(training_data)
summary(ets_model) #Residual SD = 230.699
checkresiduals(ets_model)

#FORECASTING TESTING DATA BY ETS MODEL
ets_forecast <- forecast(ets_model, h = length(testing_data))
options(scipen = 999)
plot(ets_forecast)
lines(testing_data, col = "red")
legend("topleft", lty = 1, bty = "n", col=c("red","blue"), c("Actual Data","Forecasted Data"))

#CHECKING ACCURACY OF ETS MODEL
ets_mean <- ets_forecast$mean
plot(testing_data, col = "red")
lines(ets_mean, col = "darkblue")
legend("topleft", lty = 1, bty = "n", col=c("red","blue"), c("Actual Data","Forecasted Data"))
accuracy(ets_forecast, testing_data)

#FORECASTING NEXT 15 DAYS BY FINAL MODEL
final_forecast <- forecast(ets_model, h = length(testing_data) + 15)
plot(final_forecast)
lines(testing_data, col = "red")
legend("topleft", lty = 1, bty = "n", col=c("red","blue"), c("Actual Data","Forecasted Data"))
final_forecast$upper
