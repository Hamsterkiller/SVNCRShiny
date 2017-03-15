setwd("C:/!zemskov/SVNCForecast/data_sources")
load("prepared.RData")

require(dplyr)
require(ggplot2)
require(TTR)
require(tsauxfunc)
rm(list = c("aggr_data", "data_for_agr", "init_data", 
                    "init_data_filtered", "init_data_prepared", "outs_vc"))

# creating list of DPGs with the siaze of it's history (in months)
rangeByHistorySize <- function(dataset, keyFactor) {
  #creating output df
  historyRanks <- data.frame(FactorLevel=as.character(), 
                             HistorySize=as.integer())
  # for each unique value of the factor var do...
  for (i in levels(keyFactor)) {
    #historyRow <- data.frame(FactorLevel = i, HistorySize = nrow(dataset[which(dataset$keyFactor == i), ]))
    historyRanks <- rbind(historyRanks, data.frame(FactorLevel = i, 
                      HistorySize = nrow(dataset[which(keyFactor == i), ])))
  }
  return (historyRanks[order(-historyRanks$HistorySize), ])
}

history_ranking <- rangeByHistorySize(init_data_final, init_data_final$TCODE)
save(history_ranking, file = "history_ranking.RData")

# Select TCODES with max historySize (for pre-modelling ...)
max_history_size <- history_ranking[which(history_ranking$HistorySize 
                      == max(history_ranking$HistorySize)), ]
# Selecting data with TCODE with max history_size and reorganizing it
max_h_selected <- merge(max_history_size, init_data_final, 
                        by.x = 'FactorLevel', by.y = 'TCODE', sort = TRUE) %>% 
    select(DATE, -starts_with("History"), everything()) %>% rename(TCODE = FactorLevel, 
    SVNC_EE = P_VC_AVG, SVNC_M = P_NC_AVG)
max_h_selected <- max_h_selected[order(max_h_selected$TCODE, max_h_selected$DATE), ]

# plotting to get a general grip on data
require(xts)


# FOR ELECTRICITY
for_plot_EE <- max_h_selected[which(max_h_selected$TCODE == 'PABAKSB1'), ] 
for_plot_EE <- select(for_plot_EE, DATE, SVNC_EE)
# creating time-series object
SVNC_EE_ts <- xts(for_plot_EE$SVNC_EE, order.by=as.Date(for_plot_EE$DATE))
SVNC_EE_ts <- ts(SVNC_EE_ts, frequency=12, start=c(2011,1))

# decomposing observed curve to trend, seasonal and random components
SVNC_EE_ts_components <- decompose(SVNC_EE_ts)
plot(SVNC_EE_ts_components)
# seasonal component (represents seasonal coefficient)
SVNC_EE_ts_components$seasonal
# So, from this we can see that seasonal influence on the SVNC_EE is approximately 10%
SVNC_EE_ts_components$random
# Random component has influence about 20%

# SVNC_EE without seasonal component
SVNC_EE_ts_adjusted <- SVNC_EE_ts - SVNC_EE_ts_components$seasonal
plot(SVNC_EE_ts_adjusted)

# make training dataset
training_ts_EE <- for_plot_EE[-62, ] 
training_ts_EE <- xts(training_ts_EE$SVNC_EE,   order.by=as.Date(training_ts_EE$DATE)) 
training_ts_EE <- ts(training_ts_EE, frequency=12, start=c(2011,1))
# log
training_ts_EE_log <- log(training_ts_EE)

# make testing dataset
testing_ts_EE <- for_plot_EE[62, ] 
testing_ts_EE <- xts(testing_ts_EE$SVNC_EE,   order.by=as.Date(testing_ts_EE$DATE)) 
testing_ts_EE <- ts(testing_ts_EE, frequency=12, start=c(2016,2))

# A we have time series with trend and seasonal component we can apply Holt-Winters model
HoltWinters_model_EE <- HoltWinters(training_ts_EE, seasonal = "additive")
# Output:
#Smoothing parameters:
##alpha: 0.6232921 - estimated SVNC_EE depends relatively more of t-1 SVNC_EE 
##beta : 0.007168496 - the slope of the trend changes slightly with time
##gamma: 1 - seasonal component is equal to t-12 component

HoltWinters_model_EE_log <- HoltWinters(training_ts_EE_log)
# Output:
#Smoothing parameters:
##alpha: 0.6076419 - estimated SVNC_EE depends relatively more of t-1 SVNC_EE as well
##beta : 0.01873716 - the slope of the trend changes more significantly in log_model
##gamma: 1 - seasonal component is equal to t-12 component
plot(HoltWinters_model_EE)
plot(HoltWinters_model_EE_log)

require(forecast)
plot(forecast.HoltWinters(HoltWinters_model_EE, h = 10))

# FOR CAPACITY
for_plot_M <- max_h_selected[which(max_h_selected$TCODE == 'PSARATEN'), ]
for_plot_M <- select(for_plot_M, DATE, SVNC_M)
# creating time-series object
SVNC_M_ts <- xts(for_plot_M$SVNC_M, order.by=as.Date(for_plot_M$DATE))
SVNC_M_ts <- ts(SVNC_M_ts, frequency=12, start=c(2011,1))

# decomposing observed curve to trend, seasonal and random components
SVNC_M_ts_components <- decompose(SVNC_M_ts)
plot(SVNC_M_ts_components)
# seasonal component (represents seasonal coefficient)
SVNC_M_ts_components$seasonal
# So, from this we can see that seasonal influence on the SVNC_EE is approximately 10%
SVNC_M_ts_components$random
# Random component has influence about 20%

# SVNC_EE without seasonal component
SVNC_M_ts_adjusted <- SVNC_M_ts - SVNC_M_ts_components$seasonal
plot(SVNC_M_ts_adjusted)

# make training dataset
training_ts_M <- for_plot_M[-62, ] 
training_ts_M <- xts(training_ts_M$SVNC_M,   order.by=as.Date(training_ts_M$DATE)) 
training_ts_M <- ts(training_ts_M, frequency=12, start=c(2011,1))
# log
training_ts_M_log <- log(training_ts_M)
testing_ts_M <- for_plot_M[62, ] 
testing_ts_M <- xts(testing_ts_M$SVNC_M,   order.by=as.Date(testing_ts_M$DATE)) 
testing_ts_M <- ts(testing_ts_M, frequency=12, start=c(2016,2))

# A we have time series with trend and seasonal component we can apply Holt-Winters model
HoltWinters_model_M <- HoltWinters(training_ts_M, seasonal = "additive")
# Output:
#Smoothing parameters:
##alpha: 0.6232921 - estimated SVNC_EE depends relatively more of t-1 SVNC_EE 
##beta : 0.007168496 - the slope of the trend changes slightly with time
##gamma: 1 - seasonal component is equal to t-12 component

HoltWinters_model_M_log <- HoltWinters(training_ts_M_log)
# Output:
#Smoothing parameters:
##alpha: 0.6076419 - estimated SVNC_EE depends relatively more of t-1 SVNC_EE as well
##beta : 0.01873716 - the slope of the trend changes more significantly in log_model
##gamma: 1 - seasonal component is equal to t-12 component
plot(HoltWinters_model_M)
plot(HoltWinters_model_M_log)
plot(forecast.HoltWinters(HoltWinters_model_M, h = 10))
plot(SVNC_EE_ts)
plot(SVNC_M_ts)

# Now we have Holt-Winters models for EE and M.
# Visually it's clear that these models does not provide satisfactive
# results in forecasting. For EE forecasting using H.-W. model is less precise
# because of the fact, that EE has greater variance then M.
SVNC_EE_norm <- (for_plot_EE$SVNC_EE - mean(for_plot_EE$SVNC_EE))/mean(for_plot_EE$SVNC_EE)
SVNC_M_norm <- (for_plot_M$SVNC_M - mean(for_plot_M$SVNC_M))/mean(for_plot_M$SVNC_M)

var(SVNC_EE_norm)
var(SVNC_M_norm)
sd(SVNC_EE_norm)
sd(SVNC_M_norm)

par(mfrow = c(2, 3))
plot(SVNC_EE_ts, main="СВНЦ ЭЭ для PABAKSB1", xlab="месяц")
plot(SVNC_M_ts, main="СВНЦ М для PABAKSB1", xlab="месяц")
plot(SVNC_EE_norm, type = "h", main = "СВНЦ на ЭЭ норм.", xlab="порядк. номер месяца")
plot(SVNC_M_norm, type = "h", main = "СВНЦ на М норм.", xlab="порядк. номер месяца")
plot(HoltWinters_model_EE, main="Оценка эксп. сглаж. с сезонностью для ЭЭ")
plot(HoltWinters_model_M, main="Оценка эксп. сглаж. с сезонностью для М")


# Autocorrelation dependence testing (Ljung-Box) (last 1.5 years)

forecastHoltWinters_EE <- forecast.HoltWinters(HoltWinters_model_EE)
forecastHoltWinters_M <- forecast.HoltWinters(HoltWinters_model_M)

par(mfrow = c(2, 1))
acf(forecastHoltWinters_EE$residuals, lag.max = 18, main="Correlogram of the H.-W. forecast errors for EE")
acf(forecastHoltWinters_M$residuals, lag.max = 18, main="Correlogram of the H.-W. forecast errors for M")

# Box-Ljung (H0 = there is a correlation between errors)
box_ljung_t_EE <- Box.test(forecastHoltWinters_EE$residuals, lag=20, type="Ljung-Box")
box_ljung_t_M <- Box.test(forecastHoltWinters_M$residuals, lag=20, type="Ljung-Box")
# Test Output for EE
## X-squared = 28.0426, df = 20, p-value = 0.1084
# Test Output for M
## X-squared = 18.7185, df = 20, p-value = 0.5402
# Conclusion: H0 can be rejected due to p-level. There is no significant correlation
# between errors.
# BUT! Residuals of the forecast ARE NOT normally distributed.
plot.ts(forecastHoltWinters_EE$residuals)
plot.ts(forecastHoltWinters_M$residuals)
hist(forecastHoltWinters_EE$residuals)
hist(forecastHoltWinters_M$residuals)
# That means, that we got biased estimates with Holt-Winters Model.
# And our model can and must still be improved.

# ARIMA Modelling
# As we have non-stationary processes, we can make them stationary in 
# mean by taking differences (e.g. taking "current - previous" instead "current")
# After each iteration of diff() we test resulting series for mean-stationarity.

SVNC_EE_ts_diff1 <- diff(SVNC_EE_ts, differences = 1)
plot(SVNC_EE_ts_diff1)
mean(SVNC_EE_ts_diff1)

SVNC_M_ts_diff1 <- diff(SVNC_M_ts, differences = 1)
plot(SVNC_M_ts_diff1)
mean(SVNC_M_ts_diff1)


# at first glance first and second defference series are stationary
# we have to check this before moving on

require(tseries)
# Augmented Dickey-Fuller Unit Root Test
adf.test(SVNC_EE_ts_diff1, k=0)
adf.test(SVNC_M_ts_diff1, k=0)
# Phillips-Perron Unit Root Test
PP.test(SVNC_EE_ts_diff1)
PP.test(SVNC_M_ts_diff1)
# Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test
kpss.test(SVNC_EE_ts_diff1, null = "Trend")
kpss.test(SVNC_M_ts_diff1, null = "Trend")

# From the test and plots we can conclude, that first difference is enough to make
# SVNC_EE and SVNC_M stationary.
# So, we can use ARIMA(p, 1, q) class of models further... OR NOT?? 
# KPSS-test shows non-stationarity

# Selecting ARIMA models
# Correlogramms of 1st differences
## EE
acf(SVNC_EE_ts)
pacf(SVNC_EE_ts)
## M
acf(SVNC_M_ts)
pacf(SVNC_M_ts)

SVNC_EE_arima_model <- arima(training_ts_EE, order = c(1, 1, 1), seasonal = c(1, 0, 0))
predict(SVNC_EE_arima_model, h = 1)
SVNC_M_arima_model <- arima(training_ts_M,  order = c(1, 1, 1) , seasonal = c(1, 0, 0))
forecast(SVNC_M_arima_model, h = 1)
plot(SVNC_EE_ts, col = "red")
lines(fitted(SVNC_EE_arima_model), col = "blue", main = "ARIMA оценка для ЭЭ")
par(mfrow = c(2, 1))
plot(SVNC_M_ts, col = "blue", main="Оценка ARIMA для М")
lines(fitted(SVNC_M_arima_model), col = "red")
plot(HoltWinters_model_M, main="Оценка эксп. сглаж. с сезонностью для М")
