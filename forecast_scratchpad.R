library(forecast)
library(fpp2)
# library(fpp3)
library(tidymodels)
# library(vars)
library(urca)
library(sweep)
library(fable)
library(feasts)
library(tsibble)
library(tsibbledata)
library(tidyverse)



# fable package is the newer version of forecast package for the tidyverse (aka tidyverts)
# https://tidyverts.org/
# https://fable.tidyverts.org/
# https://otexts.com/fpp3/
# https://feasts.tidyverts.org/

# forecast package
# https://otexts.com/fpp2/arima-r.html
# forecast: regression w arima errors aka arimax (best): https://otexts.com/fpp2/regarima.html
# https://stats.stackexchange.com/questions/192739/applying-an-arima-model-with-exogenous-variables-to-new-data-for-forecasting
# tidymodels forecast with resampling
# https://www.tidymodels.org/learn/models/time-series/


#///////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////


# use tsibble ####

# tsibbles are tidyverse-friendly versions of ts objects, so you can use dplyer etc
y <- tsibble(Year = 2015:2019,
        Observation = c(123, 39, 78, 52, 110),
        index = Year)
y
y %>% mutate(test = 1) %>% select(Year, test)


#///////////////////////////////////////////////////////////////////////////////////////


# https://otexts.com/fpp3/arima-r.html

global_economy
global_economy %>% glimpse()
global_economy %>% count(Year)

# plot
global_economy |>
        filter(Code == "CAF") |>
        autoplot(Exports) +
        labs(title="Central African Republic exports",
             y="% of GDP")

# plot
global_economy |>
        filter(Code == "CAF") |>
        gg_tsdisplay(Exports, plot_type = "partial")

# plot
global_economy |>
        filter(Code == "CAF") |>
        gg_tsdisplay(difference(Exports), plot_type='partial')


#///////////////////////


# fit models
caf_fit <- global_economy |>
        filter(Code == "CAF") |>
        model(arima210 = ARIMA(Exports ~ pdq(2,1,0)),
              arima013 = ARIMA(Exports ~ pdq(0,1,3)),
              stepwise = ARIMA(Exports),
              search = ARIMA(Exports, stepwise=FALSE))

# inspect
caf_fit
caf_fit |> pivot_longer(!Country, names_to = "Model name",
                        values_to = "Orders")

# full search model has the lowest/best AICc
caf_fit %>% glance()
caf_fit %>% glance() |> arrange(AICc) |> select(.model:BIC)
caf_fit %>% select(search) %>% report()
caf_fit %>% accuracy()


#///////////////////////


# check residuals
# they look like white noise
caf_fit |>
        select(search) |>
        gg_tsresiduals()

# use ljung-box portmanteau test to confirm residuals are like white noise
# note that augment() conveniently joins original data and forecast var with the fitted values and residuals
# note that "innovation residuals" are residuals on a transformed scale (if there is a transformed scale)
# if there's not a transformed scale, the "innovation residuals" and plain-old "residuals" are the same

# the results are not significant (i.e., the p-values are relatively large). 
# Thus, we can conclude that the residuals are not distinguishable from a white noise series.
# note that in this tutorial, the add arg dof = 3, but in other tutorials this arg is omitted from ljung-box, 
# so it seems optional
augment(caf_fit)
augment(caf_fit) |>
        filter(.model=='search') |>
        features(.var = .innov, features = ljung_box, lag = 10)
        # features(.var = .innov, features = ljung_box, lag = 10, dof = 3)


#///////////////////////


# forecast
caf_fit |>
        forecast(h=5)

# plot
caf_fit |>
        forecast(h=5) |>
        filter(.model=='search') |>
        autoplot(global_economy)

#///////////////////////////////////////////////////////////////////////////////////////


# regarima (aka arimax) w fable ####
# https://otexts.com/fpp3/regarima.html
# https://otexts.com/fpp3/forecasting.html

# Figure 10.1 shows the quarterly changes in personal consumption expenditure and personal disposable 
# income from 1970 to 2019 Q2. We would like to forecast changes in expenditure based on changes in income. 
# A change in income does not necessarily translate to an instant change in consumption 
# (e.g., after the loss of a job, it may take a few months for expenses to be reduced to allow for 
# the new circumstances). However, we will ignore this complexity in this example and try to measure the 
# instantaneous effect of the average change of income on the average change of consumption expenditure.

# plot
us_change |>
        pivot_longer(c(Consumption, Income),
                     names_to = "var", values_to = "value") |>
        ggplot(aes(x = Quarter, y = value)) +
        geom_line() +
        facet_grid(vars(var), scales = "free_y") +
        labs(title = "US consumption and personal income",
             y = "Quarterly % change")


#///////////////////////


# fit model
fit <- us_change |>
        model(ARIMA(Consumption ~ Income))
fit
fit %>% report()
fit %>% glance()
fit %>% accuracy()


#///////////////////////


# inspect residuals
# the arima residuals should resemble white noise
bind_rows(
        `Regression residuals` =
                as_tibble(residuals(fit, type = "regression")),
        `ARIMA residuals` =
                as_tibble(residuals(fit, type = "innovation")),
        .id = "type"
) |>
        mutate(
                type = factor(type, levels=c(
                        "Regression residuals", "ARIMA residuals"))
        ) |>
        ggplot(aes(x = Quarter, y = .resid)) +
        geom_line() +
        facet_grid(vars(type))

fit |> gg_tsresiduals()

# test residuals
augment(fit) |>
        features(.innov, ljung_box, dof = 3, lag = 8)


#///////////////////////


# forecast

# To forecast using a regression model with ARIMA errors, we need to forecast the regression part of the model 
# and the ARIMA part of the model, and combine the results. As with ordinary regression models, in order 
# to obtain forecasts we first need to forecast the predictors. When the predictors are known into the future 
# (e.g., calendar-related variables such as time, day-of-week, etc.), this is straightforward. 
# But when the predictors are themselves unknown, we must either model them separately, 
# or use assumed future values for each predictor.

# We will calculate forecasts for the next eight quarters assuming that the 
# future percentage changes in personal disposable income will be equal to the 
# mean percentage change from the last forty years.

# The prediction intervals for this model are narrower than if we had fitted an ARIMA model without covariates, 
# because we are now able to explain some of the variation in the data using the income predictor.

# It is important to realise that the prediction intervals from regression models 
# (with or without ARIMA errors) do not take into account the uncertainty in the forecasts of the predictors. 
# So they should be interpreted as being conditional on the assumed (or estimated) future values of the predictor variables.

# get new_data for covariate
us_change_future <- new_data(us_change, 8) |>
        mutate(Income = mean(us_change$Income))

# forecast
forecast(fit, new_data = us_change_future) |>
        autoplot(us_change) +
        labs(y = "Percentage change")


#///////////////////////////////////////////////////////////////////////////////////////


# regarima w fable example: electricity demand ####

# get data
vic_elec_daily <- vic_elec |>
        filter(year(Time) == 2014) |>
        index_by(Date = date(Time)) |>
        summarise(
                Demand = sum(Demand) / 1e3,
                Temperature = max(Temperature),
                Holiday = any(Holiday)
        ) |>
        mutate(Day_Type = case_when(
                Holiday ~ "Holiday",
                wday(Date) %in% 2:6 ~ "Weekday",
                TRUE ~ "Weekend"
        ))
vic_elec_daily

# plot
# see correlation with temperature, which will be used as covariate
# also see relationship with weekday/weekend/holiday
vic_elec_daily |>
        ggplot(aes(x = Temperature, y = Demand, colour = Day_Type)) +
        geom_point() +
        labs(y = "Electricity demand (GW)",
             x = "Maximum daily temperature")

# plot
vic_elec_daily |>
        pivot_longer(c(Demand, Temperature)) |>
        ggplot(aes(x = Date, y = value)) +
        geom_line() +
        facet_grid(name ~ ., scales = "free_y") + ylab("")


#///////////////////////


# fit model
fit <- vic_elec_daily %>%
        mutate(temperature_squared = Temperature ^ 2,
               weekday_flag = case_when(Day_Type == "Weekday" ~ 1, TRUE ~ 0)) %>%
        model(ARIMA(Demand ~ Temperature + temperature_squared + weekday_flag))

# original version written untidily
# fit2 <- vic_elec_daily |>
#         model(ARIMA(Demand ~ Temperature + I(Temperature^2) +
#                             (Day_Type == "Weekday")))


fit
fit %>% glance()
fit %>% report() 
fit %>% accuracy()

# fit2
# fit2 %>% glance()
# fit2 %>% report()


#///////////////////////


# check residuals
fit |> gg_tsresiduals()

# test residuals

# There is clear heteroscedasticity in the residuals, with higher variance in January and February, 
# and lower variance in May. The model also has some significant autocorrelation in the residuals, 
# and the histogram of the residuals shows long tails. All of these issues with the residuals may 
# affect the coverage of the prediction intervals, but the point forecasts should still be ok.

augment(fit) |>
        features(.innov, ljung_box, dof = 6, lag = 14)


#///////////////////////


# forecast

# Using the estimated model we forecast 14 days ahead starting from Thursday 1 January 2015 
# (a non-work-day being a public holiday for New Years Day). In this case, we could obtain weather forecasts 
# from the weather bureau for the next 14 days. But for the sake of illustration, we will use scenario based 
# forecasting (as introduced in Section 7.6) where we set the temperature for the next 14 days to a constant 26 degrees.

# note that we need to use new_data() to append new data to the tsibble, since the model needs future period covariate values
# new_data will append n additional time periods to the original tsibble
vic_elec_daily %>% as_tibble() %>% arrange(desc(Date))

# get new_data
vic_elec_future <- new_data(vic_elec_daily, n = 14) |>
        mutate(
                Temperature = 26,
                Holiday = c(TRUE, rep(FALSE, 13)),
                Day_Type = case_when(
                        Holiday ~ "Holiday",
                        wday(Date) %in% 2:6 ~ "Weekday",
                        TRUE ~ "Weekend"
                )
        )
vic_elec_future 

# forecast
vic_elect_forecast <- forecast(object = fit, new_data = vic_elec_future)
vic_elect_forecast


# plot
vic_elect_forecast %>%
        autoplot(vic_elec_daily) +
        labs(title="Daily electricity demand: Victoria",
             y="GW")


#///////////////////////////////////////////////////////////////////////////////////////


# time series cross validation w fable ####

# Time series cross-validation accuracy
# https://otexts.com/fpp3/tscv.html
# The stretch_tsibble() function is used to create many training sets. 
# In this example, we start with a training set of length .init=3, and 
# increase the size of successive training sets by .step=1.

# get data
google_stock <- gafa_stock |>
        filter(Symbol == "GOOG", year(Date) >= 2015) |>
        mutate(day = row_number()) |>
        update_tsibble(index = day, regular = TRUE)
# Filter the year of interest
google_2015 <- google_stock |> filter(year(Date) == 2015)

# stretch_tsibble to get set of extended data series for use in ts CV
google_2015_tr <- google_2015 |>
        stretch_tsibble(.init = 3, .step = 1) |>
        relocate(Date, Symbol, .id)

# inspect
google_2015_tr %>% print(n = 20)

# TSCV accuracy
google_2015_tr |>
        model(RW(Close ~ drift())) |>
        forecast(h = 1) |>
        accuracy(google_2015)

# Training set accuracy
google_2015 |>
        model(RW(Close ~ drift())) |>
        accuracy()


#///////////////////////////////////////////////////////////////////////////////////


# vector autoregressions (VAR) w fable ####

# https://otexts.com/fpp3/VAR.html


# fit model
fit <- us_change |>
        model(
                aicc = VAR(vars(Consumption, Income)),
                bic = VAR(vars(Consumption, Income), ic = "bic")
        )
fit
glance(fit)


#////////////////


# check residuals
fit |>
        augment() |>
        ACF(.innov) |>
        autoplot()


#/////////////////


# plot
fit |>
        select(aicc) |>
        forecast() |>
        autoplot(us_change |> filter(year(Quarter) > 2010))


#//////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////


# arima w forecast ####

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
uschange[ ,1] %>% ggAcf()
uschange[ ,1] %>% diff() %>% ggAcf()
uschange[ ,1] %>% ggtsdisplay()
uschange[ ,1] %>% diff() %>% ggtsdisplay()

# can run kpss unit root test for stationarity
# note that if test_stat is bigger than 1pct critical value, the data is not stationary
uschange[ ,1] %>% ur.kpss() %>% summary()
uschange[ ,1] %>% diff() %>% ur.kpss() %>% summary()
goog %>% ur.kpss() %>% summary()
goog %>% diff() %>% ur.kpss() %>% summary()


# fit arima predicting consumption, with income as covariate
fit <- auto.arima(uschange[,"Consumption"], xreg = uschange[,"Income"])
fit

# can calculate yt predictions as:
# yt = .599(intercept) + .203(income) + arima-t
# arima-t = .692arimat-1 + et + .576et-1 + .198et-2

# sweep::sw_glance gets tidy model metrics
fit %>% sw_glance()

# sweep::sw_tidy gets tidy model parameters
fit %>% sw_tidy()

# check accuracy
# accuracy(fit)

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


# arimax w forecast, w multiple covariates ####

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


# arimax with forecast, lagged variables as covariates ####

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


# time series cross validation w forecast ####

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



#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////


# vector autoregression (VAR) ####

# vars allow each covariate to influence each other
# whereas arima covariates you have supply the full covariate vector of values, in VARs the "covariate" values are
# themselves forecast using the variable-of-interest. the forecasted-covariate values are then used to forecast the 
# variable-of-interest values

# https://otexts.com/fpp2/VAR.html

uschange[,1:2] %>% as_tibble()

# use VARselect() to find out best number of lags to use
# note heynmann recommends BIC as best metric (labeled here as SC for schwarz criterion)
# he says AIC tends toward suggesting too many lags
# eg the AIC here selects 5 lags, but BIC selects 1
VARselect(uschange[,1:2], lag.max=8, type="const")
VARselect(uschange[,1:2], lag.max=8, type="const")[["selection"]]


var1 <- VAR(uschange[,1:2], p=1, type="const")
# as with arima, test that residuals are uncorrelated using portmanteau test (eg no info left in residuals)
# note that here both models show some remaining correlation (eg information) (the p value < .10)
serial.test(var1, lags.pt=10, type="PT.asymptotic")

var2 <- VAR(uschange[,1:2], p=2, type="const")
serial.test(var2, lags.pt=10, type="PT.asymptotic")

# since residuals of models w lags of 1 and 2 both show some remaining correlation (eg info), the
# next step is to fit model with lag of 3
# the residuals for this model pass the test for serial correlation
var3 <- VAR(uschange[,1:2], p=3, type="const")
serial.test(var3, lags.pt=10, type="PT.asymptotic")

# forecast 
forecast(var3) %>%
        autoplot() + xlab("Year")


