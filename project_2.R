library(GGally)
library(janitor)
library(skimr)
library(sweep)
library(devEMF)
library(officer)
library(patchwork)
library(modelr)
library(tidyverse)
library(fable)
library(tsibble)
library(fabletools)
library(feasts)

options(scipen = 999)

# setwd
setwd("C:/Users/Stephen/Desktop/University of Wisconsin/classes/DS745/project_2")


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# read in ct real estate sales data ####

# https://catalog.data.gov/dataset/real-estate-sales-2001-2018

sales <- read_csv(file = "Real_Estate_Sales_2001-2021_GL.csv") %>% 
        clean_names() %>%
        mutate(sale_date = mdy(date_recorded),
               sale_month = month(sale_date),
               sale_year = year(sale_date),
               sale_year_month = str_c(sale_year, "-", str_pad(string = sale_month, width = 2, side = "left", pad = "0")))


#////////////////////////


# inspect
sales 
sales %>% glimpse()

# check vars/values
sales %>% count(list_year)
sales %>% count(property_type) %>% arrange(desc(n))
sales %>% count(residential_type) %>% arrange(desc(n))
sales %>% count(property_type, residential_type) %>% arrange(desc(n))


#////////////////////////////////////////////////////////////////////////////////////////////


# get home_sales
# note that sale_year is filtered to >= 2007 and <= 2021 due to incomplete/low quality data prior
# also sale_amount filtered to >= 10k to drop some outlier low sale values
home_sales <- sales %>% 
        mutate(home_type = case_when(residential_type %in% 
                                c("Two Family", "Three Family", "Four Family" ) ~ "Multi Family",
                                TRUE ~ residential_type)) %>%
        filter(!is.na(home_type),
               sale_year >= 2007 & sale_year <= 2021,
               sale_amount >= 10000)

# drop sales
remove(sales)


#////////////////////////////


# inspect
home_sales
home_sales %>% glimpse()

home_sales %>% count(home_type) %>% arrange(desc(n))
home_sales %>% count(residential_type, home_type) %>% arrange(desc(n))
home_sales %>% count(sale_year) %>% print(n = nrow(.))
home_sales %>% skim(sale_amount)
home_sales %>% filter(sale_amount < 10000)


#////////////////////////////////////////////////////////////////////////////////////////////


# update home_sales
home_sales <- home_sales %>% 
        group_by(sale_year, sale_month, sale_year_month, home_type) %>%
        summarize(sale_count = n(),
                  sale_sum = sum(sale_amount)) %>%
        ungroup() %>%
        mutate(home_type = str_replace_all(string = str_to_lower(string = home_type), pattern = " ", replace = "_")) %>%
        pivot_wider(names_from = home_type,
                    names_glue = "{.value}_{home_type}",
                values_from = c(sale_sum, sale_count)) %>%
        arrange(sale_year, sale_month)


#///////////////////


# inspect
home_sales
home_sales %>% glimpse()

home_sales %>% count(sale_year) 
home_sales %>% count(sale_year_month) %>% print(n = 20)


#////////////////////////////////////////////////////////////////////////////////////


# get home_sales_ts
home_sales_ts <- home_sales %>% mutate(sale_year_month = yearmonth(sale_year_month)) %>% as_tsibble(index = sale_year_month)


#/////////////////////


# inspect
home_sales_ts
home_sales_ts %>% glimpse()
home_sales_ts %>% as_tibble()
home_sales_ts %>% str()

# check correlations
home_sales %>% select(-c(sale_year, sale_month, sale_year_month)) %>% ggpairs()


#////////////////////////////////////////////////////////////////////////////////////


# plot sale_sum_single_family
home_sales_ts %>% gg_tsdisplay(y = sale_sum_single_family, plot_type = "partial")


#////////////////////////////////////////////////////////////////////////////////////


# plot sale_count_single_family
home_sales_ts %>% gg_tsdisplay(y = sale_count_single_family, plot_type = "partial")


#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////


# get arima model for sale_count_single_family ####

sale_count_single_family_arima <- home_sales_ts %>%
        filter(sale_year != 2021) %>%
        update_tsibble(index = sale_year_month) %>%
        model(ARIMA(formula = sale_count_single_family, stepwise = FALSE))

# inspect
sale_count_single_family_arima
sale_count_single_family_arima %>% glance()
sale_count_single_family_arima %>% report()
sale_count_single_family_arima %>% accuracy()
sale_count_single_family_arima %>% augment()

home_sales_ts %>% count(sale_year)


#///////////////////////


# check residuals
sale_count_single_family_arima %>% gg_tsresiduals()

# use ljung-box portmanteau test to confirm residuals are like white noise
# note that augment() conveniently joins original data and forecast var with the fitted values and residuals
# note that "innovation residuals" are residuals on a transformed scale (if there is a transformed scale)
# if there's not a transformed scale, the "innovation residuals" and plain-old "residuals" are the same

# the results are not significant (i.e., the p-values are relatively large). 
# Thus, we can conclude that the residuals are not distinguishable from a white noise series.
sale_count_single_family_arima %>% augment() %>% features(.var = .innov, features = ljung_box, lag = 10)


#///////////////////////


# forecast
sale_count_single_family_arima_forecast <- sale_count_single_family_arima %>% forecast(h = 12)

# get forecast accuracy
sale_count_single_family_arima_forecast
sale_count_single_family_arima_forecast %>% accuracy(home_sales_ts)

# check accuracy manually
home_sales_ts %>% 
        select(sale_year, sale_year_month, sale_count_single_family) %>%
        filter(sale_year == 2021) %>%
        as_tibble() %>%
        left_join(., sale_count_single_family_arima_forecast %>%
                          as_tibble() %>%
                        select(sale_year_month, .mean) %>%
                          rename(forecast_mean = .mean), 
                  by = "sale_year_month") %>%
        mutate(error = sale_count_single_family - forecast_mean,
               rmse = sqrt(mean(error ^ 2)),
               me = mean(error),
               mae = mean(abs(error)))


#////////////////////


# get cross validated accuracy

# intiialize cv_sets 
home_sales_ts_cv_sets <- home_sales_ts %>%
        stretch_tsibble(.init = 10 * 12, .step = 1) %>% 
        relocate(.id, .before = everything())

# inspect
home_sales_ts_cv_sets
home_sales_ts_cv_sets %>% print(n = 100)

# manually specify model as a check
sale_count_single_family_regarima %>% glance()
home_sales_ts %>% 
        filter(sale_year != 2021) %>%
        model(ARIMA(sale_count_single_family ~ pdq(p = 1, d = 1, q = 4) + PDQ(P = 0, D = 1, Q = 1, period = 12))) %>%
        glance()

# get cv_sets_forecasts
home_sales_ts %>% model(ARIMA(sale_count_single_family ~ pdq(p = 1, d = 1, q = 4) + PDQ(P = 0, D = 1, Q = 1, period = 12)))
home_sales_ts_cv_sets_arima_forecasts <- home_sales_ts_cv_sets %>%
        model(ARIMA(sale_count_single_family ~ pdq(p = 1, d = 1, q = 4) + PDQ(P = 0, D = 1, Q = 1, period = 12))) %>%
        forecast(h = 1)

# get cv_sets_forecasts accuracy
home_sales_ts_cv_sets_arima_forecasts
home_sales_ts_cv_sets_arima_forecasts %>%
        accuracy(home_sales_ts)
        
# compare training accuracy
sale_count_single_family_arima %>% accuracy()


#//////////////////////


# plot
sale_count_single_family_arima_forecast %>%
        autoplot(home_sales_ts)


#/////////////////////////////////////////////////////////////////////////////////////////////////



# get sale_count_single_family_arima_forecast_line_chart ####
sale_count_single_family_arima_forecast_chart <- sale_count_single_family_arima_forecast %>% 
        autoplot(home_sales_ts) +
        # scale_x_continuous(breaks = seq(from = 2007, to = 2022, by = 1)) +
        scale_y_continuous(labels = label_comma()) +
        labs(x = NULL, y = "Sales count", 
             title = "Forecast sales count of single family homes from ARIMA(1,1,4)(0,1,1)[12]") +
        coord_fixed(ratio = .75 / 1, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                plot.margin = unit(c(0, 15, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_blank(),
                # panel.border = element_blank(),
                panel.border = element_rect(color = "#DDDDDD", size = .5),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 13, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 13, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                # axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 13, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 13, color = "#333333",
                                            margin = margin(t = 0, r = 3, b = 0, l = 5)),
                # axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                plot.title = element_text(size = 15, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                # legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 9, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 9, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) 

# inspect
sale_count_single_family_arima_forecast_chart


#///////////////////


# resize to 190%

# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(sale_count_single_family_arima_forecast_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "sale_count_single_family_arima_forecast_chart.docx")

#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////


# get regarima model for sale_count_single_family ####

sale_count_single_family_regarima <- home_sales_ts %>%
        filter(sale_year != 2021) %>%
        update_tsibble(index = sale_year_month) %>%
        model(ARIMA(formula = sale_count_single_family ~ sale_sum_single_family,
                            # lag(sale_sum_single_family, n = 1) +
                            # lag(sale_sum_single_family, n = 2), 
                            # lag(sale_sum_single_family, n = 3)
                    stepwise = FALSE))

# inspect
sale_count_single_family_regarima
sale_count_single_family_regarima %>% glance()
sale_count_single_family_regarima %>% report()
sale_count_single_family_regarima %>% accuracy()
sale_count_single_family_regarima %>% augment()

home_sales_ts %>% count(sale_year)


#///////////////////////


# check residuals
sale_count_single_family_regarima %>% gg_tsresiduals()

# use ljung-box portmanteau test to confirm residuals are like white noise
# note that augment() conveniently joins original data and forecast var with the fitted values and residuals
# note that "innovation residuals" are residuals on a transformed scale (if there is a transformed scale)
# if there's not a transformed scale, the "innovation residuals" and plain-old "residuals" are the same

# the results are not significant (i.e., the p-values are relatively large). 
# Thus, we can conclude that the residuals are not distinguishable from a white noise series.
sale_count_single_family_regarima %>% augment() %>% features(.var = .innov, features = ljung_box, lag = 10)


#///////////////////////


# forecast

# get covariates
sale_count_single_family_regarima_covariates <- home_sales_ts %>% 
        filter(sale_year != 2021) %>%
        new_data(n = 12) %>%
        mutate(sale_sum_single_family = home_sales_ts %>% 
                       as_tibble() %>%
                       filter(sale_year %in% c(2020)) %>%
                       group_by(sale_month) %>% 
                       summarize(sale_sum_single_family_mean = mean(sale_sum_single_family)) %>% 
                       pull(sale_sum_single_family_mean))
sale_count_single_family_regarima_covariates

# forecast
sale_count_single_family_regarima_forecast <- sale_count_single_family_regarima %>% 
        forecast(h = 12, new_data = sale_count_single_family_regarima_covariates)

# get forecast accuracy
sale_count_single_family_regarima_forecast
sale_count_single_family_regarima_forecast %>% accuracy(home_sales_ts)

# check accuracy manually
home_sales_ts %>%
        select(sale_year, sale_year_month, sale_count_single_family) %>%
        filter(sale_year == 2021) %>%
        as_tibble() %>%
        left_join(., sale_count_single_family_regarima_forecast %>%
                          as_tibble() %>%
                          select(sale_year_month, .mean) %>%
                          rename(forecast_mean = .mean),
                  by = "sale_year_month") %>%
        mutate(error = sale_count_single_family - forecast_mean,
               rmse = sqrt(mean(error ^ 2)),
               me = mean(error),
               mae = mean(abs(error)))


#////////////////////


# get cross validated accuracy

# intiialize cv_sets
home_sales_ts_cv_sets <- home_sales_ts %>%
        stretch_tsibble(.init = 10 * 12, .step = 1) %>%
        relocate(.id, .before = everything())

# inspect
home_sales_ts_cv_sets
home_sales_ts_cv_sets %>% print(n = 100)

# manually specify model as a check
sale_count_single_family_regarima %>% glance()
home_sales_ts %>% 
        filter(sale_year != 2021) %>%
        model(ARIMA(sale_count_single_family ~ pdq(p = 5, d = 1, q = 0) + PDQ(P = 1, D = 0, Q = 0, period = 12) +
                            lag(sale_sum_single_family, n = 1))) %>%
        glance()

# get cv_sets_forecasts
# home_sales_ts_cv_sets_regarima_models <- home_sales_ts_cv_sets %>%
#         model(ARIMA(sale_count_single_family ~ pdq(p = 5, d = 1, q = 0) + PDQ(P = 1, D = 0, Q = 0, period = 12) +
#                             lag(sale_sum_single_family, n = 1)))
# 
# home_sales_ts_cv_sets_regarima_forecasts <- home_sales_ts_cv_sets_regarima_models %>%
#         forecast(h = 1, new_data = home_sales_ts_cv_sets %>% 
#                                         filter(.id > 1))

# get cv_sets_forecasts accuracy
home_sales_ts_cv_sets_regarima_forecasts
home_sales_ts_cv_sets_regarima_forecasts %>%
        accuracy(home_sales_ts)

# compare training accuracy
sale_count_single_family_regarima %>% accuracy()


#//////////////////////


# plot
sale_count_single_family_regarima_forecast %>%
        autoplot(home_sales_ts)


#/////////////////////////////////////////////////////////////////////////////////////////////////


# get sale_count_single_family_regarima_forecast_line_chart ####
sale_count_single_family_regarima_forecast_chart <- sale_count_single_family_regarima_forecast %>% 
        autoplot(home_sales_ts) +
        # scale_x_continuous(breaks = seq(from = 2007, to = 2022, by = 1)) +
        scale_y_continuous(labels = label_comma()) +
        labs(x = NULL, y = "Sales count", 
             title = "Forecast sales count of single family homes from\nregression on sales value of single family homes\nw/ ARIMA(5,1,0)(1,0,0)[12] errors") +
        coord_fixed(ratio = .75 / 1, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                plot.margin = unit(c(0, 15, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_blank(),
                # panel.border = element_blank(),
                panel.border = element_rect(color = "#DDDDDD", size = .5),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 13, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 13, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                # axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 13, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 13, color = "#333333",
                                            margin = margin(t = 0, r = 3, b = 0, l = 5)),
                # axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                plot.title = element_text(size = 15, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                # legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 9, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 9, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) 

# inspect
sale_count_single_family_regarima_forecast_chart


#///////////////////


# resize to 190%

# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(sale_count_single_family_regarima_forecast_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "sale_count_single_family_regarima_forecast_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


sale_count_single_family_regarima %>% augment()
sale_count_single_family_regarima_forecast


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# get VAR model for sale_count_single_family ####

# fit model
sale_count_single_family_var_models <- home_sales_ts %>%
        filter(sale_year != 2021) %>%
        model(aicc = VAR(vars(sale_sum_single_family, sale_count_single_family)),
                bic = VAR(vars(sale_sum_single_family, sale_count_single_family), ic = "bic"))

# inspect
sale_count_single_family_var_models
sale_count_single_family_var_models %>% glance()
sale_count_single_family_var_models %>% accuracy()
sale_count_single_family_var_models %>% augment()


#////////////////


# check residuals
sale_count_single_family_var_models %>%
        augment() %>%
        ACF(.innov) %>%
        autoplot()


#/////////////////


# plot
sale_count_single_family_var_models %>%
        select(aicc) %>%
        forecast(h = 12) %>%
        autoplot(home_sales_ts)


#/////////////////////////////////////////////////////////////////////////////////////////////////


# arima_and_regarima_forecast_error_bar_chart ####


arima_and_regarima_forecast_error_bar_chart <- sale_count_single_family_arima_forecast %>% 
        accuracy(home_sales_ts) %>% 
        mutate(origin = "ARIMA") %>%
        bind_rows(., 
                  sale_count_single_family_regarima_forecast %>% 
                          accuracy(home_sales_ts) %>%
                          mutate(origin = "REGARIMA")) %>%
        select(-c(.model, .type)) %>%
        pivot_longer(cols = -origin, names_to = "var", values_to = "values") %>%
        filter(var %in% c("RMSE", "MAE")) %>%
        mutate(color_bin = var) %>%
        ggplot(data = ., mapping = aes(x = fct_reorder(.f = origin, .x = var, .desc = TRUE), 
                               y = values, fill = color_bin)) + 
        geom_col(position = "dodge", width = .8) +
        scale_y_continuous(labels = label_number()) +
        scale_fill_manual(values = c("RMSE" = "#083D7F", "MAE" = "#8BBFD0")) +
        labs(x = NULL, y = "Value of error measure", 
             fill = "Error measure",
             title = "Comparison of forecast errors\nfor ARIMA and REGARIMA models\non out-of-sample 2021 data") +
        coord_fixed(ratio = .0015 / 1, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                plot.margin = unit(c(0, 15, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_blank(),
                # panel.border = element_blank(),
                panel.border = element_rect(color = "#DDDDDD", size = .5),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 13, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 13, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                # axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 13, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 13, color = "#333333",
                                            margin = margin(t = 0, r = 3, b = 0, l = 5)),
                # axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                plot.title = element_text(size = 15, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                # legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 9, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 9, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) 


# inspect
arima_and_regarima_forecast_error_bar_chart


#///////////////////


# resize to 190%

# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(arima_and_regarima_forecast_error_bar_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "arima_and_regarima_forecast_error_bar_chart.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////////


# arima_and_regarima_revenue_loss_bar_chart ####

# get avg sale price for a single_family home in 2021
single_family_home_avg_price_in_2021 <- home_sales_ts %>% 
        as_tibble() %>%
        filter(sale_year == 2021) %>%
        summarize(sale_sum_single_family_mean = mean(sale_sum_single_family),
                  sale_count_single_family_mean = mean(sale_count_single_family),
                  avg_sale_price = sale_sum_single_family_mean / sale_count_single_family_mean) %>%
        pull(avg_sale_price)
single_family_home_avg_price_in_2021 

arima_and_regarima_revenue_loss_bar_chart <- sale_count_single_family_arima_forecast %>% 
        accuracy(home_sales_ts) %>% 
        mutate(origin = "ARIMA") %>%
        bind_rows(., 
                  sale_count_single_family_regarima_forecast %>% 
                          accuracy(home_sales_ts) %>%
                          mutate(origin = "REGARIMA")) %>%
        select(-c(.model, .type)) %>%
        pivot_longer(cols = -origin, names_to = "var", values_to = "values") %>%
        filter(var == "ME") %>%
        mutate(unstaffed_homes = values / 3,
               revenue_loss = abs(unstaffed_homes * single_family_home_avg_price_in_2021 * .06),
                color_bin = origin) %>%
        ggplot(data = ., mapping = aes(x = fct_reorder(.f = origin, .x = var, .desc = TRUE), 
                                       y = revenue_loss, fill = color_bin)) + 
        geom_col(width = .8) +
        scale_y_continuous(labels = label_dollar()) +
        scale_fill_manual(values = c("ARIMA" = "#083D7F", "REGARIMA" = "#8BBFD0")) +
        labs(x = NULL, y = "Revenue loss", 
             fill = NULL,
             title = "Comparison of revenue loss from unstaffed houses\nbased on ARIMA and REGARIMA model forecasts\nfor out-of-sample 2021 data") +
        coord_fixed(ratio = .00000015 / 1, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                plot.margin = unit(c(0, 15, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_blank(),
                # panel.border = element_blank(),
                panel.border = element_rect(color = "#DDDDDD", size = .5),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 13, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 13, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                # axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 13, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 13, color = "#333333",
                                            margin = margin(t = 0, r = 3, b = 0, l = 5)),
                # axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                plot.title = element_text(size = 15, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                # legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 9, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 9, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) 


# inspect
arima_and_regarima_revenue_loss_bar_chart


#///////////////////


# resize to 190%

# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(arima_and_regarima_revenue_loss_bar_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "arima_and_regarima_revenue_loss_bar_chart.docx")
