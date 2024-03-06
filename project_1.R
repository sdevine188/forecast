library(forecast)
library(vars)
library(tidymodels)
library(GGally)
library(janitor)
library(skimr)
library(urca)
library(sweep)
library(devEMF)
library(officer)
library(patchwork)
library(modelr)
library(tidyverse)



# https://www.gartner.com/en/digital-markets/insights/what-is-customer-lifetime-value

options(scipen = 999)

# setwd
setwd("C:/Users/Stephen/Desktop/University of Wisconsin/classes/DS745/project_1")


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
               sale_year_month = str_c(sale_year, "_", str_pad(string = sale_month, width = 2, side = "left", pad = "0")))


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
home_sales_ts <- home_sales %>% dplyr::select(-sale_year_month) %>% ts(start = 2007, frequency = 12)


#/////////////////////


# inspect
home_sales_ts
home_sales_ts %>% as_tibble()
home_sales_ts %>% str()

cor(x = home_sales %>% pull(sale_sum_single_family), y = home_sales %>% pull(sale_sum_condo))
home_sales %>% select(-c(sale_year, sale_month, sale_year_month)) %>% ggpairs()


#////////////////////////////////////////////////////////////////////////////////////


# plot sale_sum_condo
home_sales_ts[ , "sale_sum_single_family"] %>% ggtsdisplay()


#////////////////////////////////////////////////////////////////////////////////////


# plot sale_sum_condo
home_sales_ts[ , "sale_sum_condo"] %>% ggtsdisplay()


#////////////////////////////////////////////////////////////////////////////////////


# plot sale_sum_multi_family
home_sales_ts[ , "sale_sum_multi_family"] %>% ggtsdisplay()


#////////////////////////////////////////////////////////////////////////////////////


# inspect sale_sum_single_family

# plot sale_sum_single_family
home_sales_ts[ , "sale_sum_single_family"] %>% ggtsdisplay()

Box.test(home_sales_ts %>% 
                 as_tibble() %>%
                 select(sale_sum_single_family) %>%
                 ts(start = 2007, frequency = 12), 
         lag = 10, type = "Ljung-Box")

# plot sale_sum_single_family diff
home_sales_ts[ , "sale_sum_single_family"] %>% diff() %>% ggtsdisplay()

Box.test(home_sales_ts %>% 
                 as_tibble() %>%
                 select(sale_sum_single_family) %>%
                 ts(start = 2007, frequency = 12) %>% diff(), 
         lag = 10, type = "Ljung-Box")


#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////


# acf_chart ####

acf_chart <- (home_sales_ts[ , "sale_sum_single_family"] %>% ggAcf() + 
                      labs(title = "Total\nsingle family\nsales ACF") +
                theme(plot.title = element_text(size = 15, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                                margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")))) +
        (home_sales_ts[ , "sale_sum_multi_family"] %>% ggAcf() + 
                 labs(title = "Total\nmulti family\nsales ACF") +
                 theme(plot.title = element_text(size = 15, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                                 margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")))) +
        (home_sales_ts[ , "sale_sum_condo"] %>% ggAcf() + 
                 labs(title = "Total\ncondo\nsales ACF") +
                 theme(plot.title = element_text(size = 15, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                                 margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")))) +
        coord_fixed(ratio = 100 / 1, clip = "off") 
        
# inspect
acf_chart 


#///////////////////


# resize to 190%

# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(acf_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "acf_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////


# get arima model for sale_sum_single_family ####

sale_sum_single_family_arima <- auto.arima(y = home_sales_ts[ , "sale_sum_single_family"])

# inspect
sale_sum_single_family_arima
sale_sum_single_family_arima %>% sw_tidy()
sale_sum_single_family_arima %>% sw_glance()
sale_sum_single_family_arima %>% checkresiduals()

# forecast
sale_sum_single_family_arima_forecast <- forecast(sale_sum_single_family_arima, h = 12)
sale_sum_single_family_arima_forecast

# plot
sale_sum_single_family_arima_forecast %>% autoplot()

sale_sum_single_family_arima_forecast_chart <- sale_sum_single_family_arima_forecast %>% autoplot() +
        scale_x_continuous(breaks = seq(from = 2007, to = 2022, by = 1)) +
        scale_y_continuous(labels = label_dollar()) +
        labs(x = "Year", y = "Forecast total sales", 
             title = "Forecast total single family sales from ARIMA(0,1,1)(2,1,0)[12]") +
        coord_fixed(ratio = .000000003 / 1, clip = "off") +
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
sale_sum_single_family_arima_forecast_chart


#///////////////////


# resize to 190%

# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(sale_sum_single_family_arima_forecast_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "sale_sum_single_family_arima_forecast_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////



# get arima model for sale_sum_multi_family ####

sale_sum_multi_family_arima <- auto.arima(y = home_sales_ts[ , "sale_sum_multi_family"])

# inspect
sale_sum_multi_family_arima
sale_sum_multi_family_arima %>% sw_tidy()
sale_sum_multi_family_arima %>% sw_glance()
sale_sum_multi_family_arima %>% checkresiduals()

# forecast
sale_sum_multi_family_arima_forecast <- forecast(sale_sum_multi_family_arima, h = 12)
sale_sum_multi_family_arima_forecast

# plot
sale_sum_multi_family_arima_forecast %>% autoplot()

sale_sum_multi_family_arima_forecast_chart <- sale_sum_multi_family_arima_forecast %>% autoplot() +
        scale_x_continuous(breaks = seq(from = 2007, to = 2022, by = 1)) +
        scale_y_continuous(labels = label_dollar()) +
        labs(x = "Year", y = "Forecast total sales", 
             title = "Forecast total multi family sales from ARIMA(0,1,1)(0,0,2)[12]") +
        coord_fixed(ratio = .00000003 / 1, clip = "off") +
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
sale_sum_multi_family_arima_forecast_chart


#///////////////////


# resize to 190%

# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(sale_sum_multi_family_arima_forecast_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "sale_sum_multi_family_arima_forecast_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# get arima model for sale_sum_condo ####

sale_sum_condo_arima <- auto.arima(y = home_sales_ts[ , "sale_sum_condo"])

# inspect
sale_sum_condo_arima
sale_sum_condo_arima %>% sw_tidy()
sale_sum_condo_arima %>% sw_glance()
sale_sum_condo_arima %>% checkresiduals()

# forecast
sale_sum_condo_arima_forecast <- forecast(sale_sum_condo_arima, h = 12)
sale_sum_condo_arima_forecast

# plot
sale_sum_condo_arima_forecast %>% autoplot()


sale_sum_condo_arima_forecast_chart <- sale_sum_condo_arima_forecast %>% autoplot() +
        scale_x_continuous(breaks = seq(from = 2007, to = 2022, by = 1)) +
        scale_y_continuous(labels = label_dollar()) +
        labs(x = "Year", y = "Forecast total sales", 
             title = "Forecast total condo sales from ARIMA(0,1,1)") +
        coord_fixed(ratio = .000000004 / 1, clip = "off") +
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
sale_sum_condo_arima_forecast_chart


#///////////////////


# resize to 190%

# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(sale_sum_condo_arima_forecast_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "sale_sum_condo_arima_forecast_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# get clv chart ####
clv_chart <- sale_sum_single_family_arima_forecast %>% 
        as_tibble() %>%
        summarize(forecast = sum(`Point Forecast`)) %>%
        mutate(origin = "single_family") %>%
        select(origin, forecast) %>%
        bind_rows(., sale_sum_multi_family_arima_forecast %>% 
                          as_tibble() %>%
                          summarize(forecast = sum(`Point Forecast`)) %>%
                          mutate(origin = "multi_family") %>%
                          select(origin, forecast)) %>%
        bind_rows(., sale_sum_condo_arima_forecast %>% 
                          as_tibble() %>%
                          summarize(forecast = sum(`Point Forecast`)) %>%
                          mutate(origin = "condo") %>%
                          select(origin, forecast)) %>%
        mutate(forecast_profit = forecast * .06 * .1,
               origin = case_when(origin == "single_family" ~ "Single family",
                                  origin == "multi_family" ~ "Multi family",
                                  origin == "condo" ~ "Condo"),
               color_bin = "current_color") %>%
        ggplot(data = ., mapping = aes(x = fct_reorder(.f = origin, .x = forecast_profit, .desc = TRUE), 
                                       y = forecast_profit, fill = color_bin)) + 
        scale_y_continuous(labels = label_dollar()) +
        scale_fill_manual(values = c("current_color" = "#2474B6"), guide = "none") +
        labs(x = "Real estate agent customer type", y = "Forecast CLV", 
             title = "Forecast Customer Lifetime Value for next 12 months,\nby real estate agent customer type") +
        geom_col() +
        coord_fixed(ratio = .000000015 / 1, clip = "off") +
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
clv_chart


#///////////////////


# resize to 190%

# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(clv_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "clv_chart.docx")



#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# get arimax model w covariates for single_family ####
sale_sum_single_family_arimax <- auto.arima(y = home_sales_ts[ , "sale_sum_single_family"], 
                   xreg = home_sales_ts[ , "sale_count_single_family"])

# inspect
sale_sum_single_family_arimax
sale_sum_single_family_arimax %>% sw_tidy()
sale_sum_single_family_arimax %>% sw_glance()

home_sales_ts[ , "sale_count_single_family"] %>% ggtsdisplay()

# forecast using monthly values from most recent year
sale_sum_single_family_arimax_forecast_1 <- forecast(sale_sum_single_family_arimax, 
        xreg = home_sales_ts %>% 
                           as_tibble() %>%  
                           arrange(desc(sale_year), desc(sale_month)) %>%
                           slice(1:12) %>%
                           arrange(sale_year, sale_month) %>%
                           pull(sale_count_single_family), 
        h = 12)
sale_sum_single_family_arimax_forecast_1

# plot
sale_sum_single_family_arimax_forecast_1 %>% autoplot()


#///////////////////


# forecast using average value from most recent year
sale_sum_single_family_arimax_forecast_2 <- forecast(sale_sum_single_family_arimax, 
                                                     xreg = rep(x = home_sales_ts %>% 
                                                             as_tibble() %>%  
                                                             summarize(mean = mean(sale_count_single_family)) %>%
                                                             pull(mean), 
                                                             times = 12), 
                                                     h = 12)
sale_sum_single_family_arimax_forecast_2

# plot
sale_sum_single_family_arimax_forecast_2 %>% autoplot()


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# get var model for single_family ####

# select optimal lag
sale_sum_single_family_var_select <- VARselect(y = home_sales_ts[ , c("sale_sum_single_family", "sale_count_single_family",
                                                                      "sale_sum_multi_family", "sale_count_multi_family",
                                                                      "sale_sum_condo", "sale_count_condo")], 
                                        lag.max = 8, type = "const")

# inspect
sale_sum_single_family_var_select
sale_sum_single_family_var_select[["selection"]]

# get var model
var1 <- VAR(y = home_sales_ts[ , c("sale_sum_single_family", "sale_count_single_family",
                                   "sale_sum_multi_family", "sale_count_multi_family",
                                   "sale_sum_condo", "sale_count_condo")], p = 1, type = "const")
var1
serial.test(var1, lags.pt = 10, type = "PT.asymptotic")

var2 <- VAR(y = home_sales_ts[ , c("sale_sum_single_family", "sale_count_single_family",
                                   "sale_sum_multi_family", "sale_count_multi_family",
                                   "sale_sum_condo", "sale_count_condo")], p = 2, type = "const")
var2
serial.test(var2, lags.pt = 10, type = "PT.asymptotic")

var3 <- VAR(y = home_sales_ts[ , c("sale_sum_single_family", "sale_count_single_family",
                                   "sale_sum_multi_family", "sale_count_multi_family",
                                   "sale_sum_condo", "sale_count_condo")], p = 3, type = "const")
var3
serial.test(var3, lags.pt = 10, type = "PT.asymptotic")

var4 <- VAR(y = home_sales_ts[ , c("sale_sum_single_family", "sale_count_single_family",
                                   "sale_sum_multi_family", "sale_count_multi_family",
                                   "sale_sum_condo", "sale_count_condo")], p = 4, type = "const")
var4
serial.test(var4, lags.pt = 10, type = "PT.asymptotic")

var5 <- VAR(y = home_sales_ts[ , c("sale_sum_single_family", "sale_count_single_family",
                                   "sale_sum_multi_family", "sale_count_multi_family",
                                   "sale_sum_condo", "sale_count_condo")], p = 5, type = "const")
var5
serial.test(var4, lags.pt = 10, type = "PT.asymptotic")

var7 <- VAR(y = home_sales_ts[ , c("sale_sum_single_family", "sale_count_single_family",
                                   "sale_sum_multi_family", "sale_count_multi_family",
                                   "sale_sum_condo", "sale_count_condo")], p = 7, type = "const")
var7
serial.test(var4, lags.pt = 10, type = "PT.asymptotic")


#/////////////////////////////


# get var model
sale_sum_single_family_var <- VAR(y = home_sales_ts[ , c("sale_sum_single_family", "sale_count_single_family",
                                                         "sale_sum_multi_family", "sale_count_multi_family",
                                                         "sale_sum_condo", "sale_count_condo")], 
                                  p = 3, type = "const")

# forecast 
sale_sum_single_family_var_forecast <- forecast(sale_sum_single_family_var, h = 12) 
sale_sum_single_family_var_forecast 

# plot 
sale_sum_single_family_var_forecast %>% autoplot()
