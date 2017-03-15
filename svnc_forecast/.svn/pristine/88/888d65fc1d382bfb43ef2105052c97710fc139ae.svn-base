# loading initial data for analysis
load("prices_data_history_ge_40.RData")
library(randomForest)
# selecting data for testing different models
test_data <- prices_data[prices_data$TCODE == 'PABAKSB1', ]
View(test_data)

# selecting variables
pabaksb1 <- test_data[, c('TCODE', 'DATE', 'P_VC_AVG', 'P_NC_AVG')]
pabaksb1_ee <- pabaksb1[, c('DATE', 'P_VC_AVG')]
pabaksb1_n <- pabaksb1[, c('DATE', 'P_NC_AVG')]

# making ts-object
library(xts)
pabaksb1_ee <- xts(pabaksb1_ee$P_VC_AVG, order.by=pabaksb1_ee$DATE)
pabaksb1_ee <- ts(pabaksb1_ee, frequency=12, start=c(2011, 7))
pabaksb1_n <- xts(pabaksb1_n$P_NC_AVG, order.by=pabaksb1_n$DATE)
pabaksb1_n <- ts(pabaksb1_n, frequency=12, start=c(2011, 7))

# generating arima model with auto.arima
library(forecast)
arima_model <- auto.arima(pabaksb1_ee)
summary(arima_model)
arima_model_n <- auto.arima(pabaksb1_n)
summary(arima_model_n)

# custom models
arima_custom <- Arima(pabaksb1_ee, order = c(2, 1, 1), seasonal = c(1, 0, 0))
arima_custom1 <- Arima(pabaksb1_ee, order = c(1, 1, 1), seasonal = c(1, 0, 0))

# extracting residuals from the fitted model
resid_ee <- arima_model$residuals
resid_n <- arima_model_n$residuals
plot(pabaksb1_ee)
plot(pabaksb1_n)
plot(resid_ee)
plot(resid_n)
# Let's check the correlation of residuals (autocorrelation function and
# partial autocorrelation function)
acf(resid)
pacf(resid)
# histogram of the residuals also tells us that they aren't normally distributed
hist(resid)

# Conclusion: from the plot it's quite clear that residuals of the fitted arima model
# has some sort of 'explosions'. This is the fact that give us a reason to suspect
# that process has (G)ARCH properties.

# generating GARCH model
library(tseries)
require(rugarch)
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)))
summary(garch_model_ee)
AIC(garch_model_ee)
##garch_model_ee <- ugarchfit(spec = spec, data = pabaksb1_ee)
# GARCH has to have more then 100 jbservations to be effective

# Exploring EVWMA - model
setwd("C:/!zemskov/SVNCForecast/data_sources")
init_data <- read.csv("./SVNC_FACT_DETAILED.csv", header = TRUE, sep=";", 
                      dec=".", stringsAsFactors = FALSE)
selection <- c("TDATE", "PCODE", "PNAME", "TCODE", 
               "REGION_CODE", "VC_FACT_I_T", "NC_PIKE_FACT_I_T", 
               "NC_M_FACT_I_T", "P_VC_UNREG_AVG", "P_NC_UNREG_AVG")
data_ewma <- subset(x = init_data, select = selection)
pabaksb1_ewma <- data_ewma[data_ewma$TCODE == 'PABAKSB1', ]

# plotting price

volume <- seq(from = 0.5, to = 1.0, along.with = pabaksb1_ewma$P_VC_UNREG_AVG)
# generating resulting volume vector with the season factor
result <- numeric()
for (i in seq(1, length(volume))) {
  if ((length(volume) - i) %% 12 == 0) {
    result[i] <- 1.0
  } else {
    result[i] <- volume[i]
  }
}
ewma_model <- EVWMA(pabaksb1_ewma$P_VC_UNREG_AVG, volume = result, n = 24)

# Further considerations:
# 1. Check every TS for every TCODE for relative volativity (var(Xt) / Xt)
# 2. For those with high volatility - use Holt-winters  model, for the rest - ARIMA
# 2.1. Maybe for every TCODE use "autoarima"??