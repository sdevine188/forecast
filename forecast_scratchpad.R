library(tidyverse)
library(forecast)
library(fpp2)
library(tidymodels)


# forecast package
# https://otexts.com/fpp2/arima-r.html
# forecast: regression w arima errors (best): https://otexts.com/fpp2/regarima.html
# tidymodels forecast with resampling
# https://www.tidymodels.org/learn/models/time-series/
# https://stats.stackexchange.com/questions/192739/applying-an-arima-model-with-exogenous-variables-to-new-data-for-forecasting


# forecast change in consumption ####

# get data
uschange
uschange %>% str()
uschange %>% as_tibble()

# plot data
autoplot(uschange[,1:2], facets=TRUE) +
        xlab("Year") + ylab("") +
        ggtitle("Quarterly changes in US consumption
    and personal income")

# note that subsetting ts object by single var puts minor time increments as columns and major increments as rows
# but subsetting same ts object by multiple vars puts all time incremements as unique row combos
uschange[ ,1]
uschange[ ,1:2]

# plot single time series w ggtsdisplay()
uschange[ ,1] %>% ggtsdisplay()

# fit arima predicting consumption, with income as covariate
fit <- auto.arima(uschange[,"Consumption"], xreg = uschange[,"Income"])
fit

# can calculate yt predictions as:
# yt = .599(intercept) + .203(income) + arima-t
# arima-t = .692arimat-1 + et + .576et-1 + .198et-2

# check accuracy
accuracy(fit)

# check residuals
checkresiduals(fit)

# forecast for next 8 years
# assume that income covariate remains at it's average value
fcast <- forecast(fit, xreg=rep(mean(uschange[,2]),8))
fcast

# plot forecast
autoplot(fcast) + xlab("Year") + ylab("Percentage change")


#////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////


# forecast electricity demand w multiple covariates ####

# get data
elecdaily
elecdaily %>% str()
elecdaily %>% as_tibble()

# inspect
autoplot(elecdaily[,1:3], facets=TRUE)

# get covariates
xreg <- cbind(MaxTemp = elecdaily[, "Temperature"],
              MaxTempSq = elecdaily[, "Temperature"]^2,
              Workday = elecdaily[, "WorkDay"])
xreg %>% as_tibble()

# fit model
fit <- auto.arima(elecdaily[, "Demand"], xreg = xreg)
fit

# check accuracy
accuracy(fit)

# check residuals
checkresiduals(fit)

# forecast 14 days ahead
# assume scenario is for continued 26 degree maxtemp
fcast <- forecast(fit,
                  xreg = cbind(MaxTemp=rep(26,14), MaxTempSq=rep(26^2,14),
                               Workday=c(0,1,0,0,1,1,1,1,1,0,0,1,1,1)))
fcast

# inspect forecast
autoplot(fcast) + ylab("Electricity demand (GW)")


#//////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////


# use lagged variables as covariates ####

# get data
# advertising var is covariate to predict insurance quotes
insurance
insurance %>% str()
insurance %>% as_tibble()


# inspect
autoplot(insurance, facets=TRUE) +
        xlab("Year") + ylab("") +
        ggtitle("Insurance advertising and quotations")

# create lagged advertising variables to allow past ad spending to influence current insurance quotes
# Lagged predictors. Test 0, 1, 2 or 3 lags.
Advert <- cbind(
        AdLag0 = insurance[,"TV.advert"],
        AdLag1 = stats::lag(insurance[,"TV.advert"],-1),
        AdLag2 = stats::lag(insurance[,"TV.advert"],-2),
        AdLag3 = stats::lag(insurance[,"TV.advert"],-3)) %>%
        head(NROW(insurance))
Advert

# Restrict data so models use same fitting period
fit1 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1],
                   stationary=TRUE)
fit2 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:2],
                   stationary=TRUE)
fit3 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:3],
                   stationary=TRUE)
fit4 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:4],
                   stationary=TRUE)

# inspect
fit1
fit2
fit3
fit4

# check accuracy
accuracy(fit1)
accuracy(fit2)
accuracy(fit3)
accuracy(fit4)

# then check aic and choose model with smallest/best aic
c(fit1[["aicc"]],fit2[["aicc"]],fit3[["aicc"]],fit4[["aicc"]])

# since model with 1 lag is best, re-run that model using all the trainng data
fit <- auto.arima(insurance[,1], xreg=Advert[,1:2],
                  stationary=TRUE)
fit

# forecast, assuming future values for advertising (assume it equals 8 continuously in this case)
fc8 <- forecast(fit, h=20,
                xreg=cbind(AdLag0 = rep(8,20),
                           AdLag1 = c(Advert[40,1], rep(8,19))))
fc8

# inspect
autoplot(fc8) + ylab("Quotes") +
        ggtitle("Forecast quotes with future advertising set to 8")


#///////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////


# time series cross validation ####

# this takes a series of all one-step-ahead forecasts and returns the error for each prediction
# note the initial arg specifies how many initial obs to use for building the first model (these then aren't used for CV error)
# h arg is horizon, allowing comparing prediction horizons of 1:h
# window arg specifies width for a rolling model-training window
# https://otexts.com/fpp2/accuracy.html

goog200
goog200 %>% str()

e <- tsCV(goog200, forecastfunction = rwf, drift=TRUE, h=5, xreg = NULL, initial = 10, window = 12)
e
e %>% str()

# inspect time series cross-validation error
# as expected, the cv error is greater because the model residuals are from model trained on full dataset, not 
# out of sample forecasts of unseen data
sqrt(mean(e^2, na.rm=TRUE))

# compare to regular model residuals
sqrt(mean(residuals(rwf(goog200, drift=TRUE))^2, na.rm=TRUE))

