## Chapter 8

# load library
install.packages("dplyr")
install.packages("lubridate")
install.packages("forecast")
install.packages("MLmetrics")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("zoo")
install.packages("tseries")


library(dplyr)     # data wrangling
library(lubridate) # date manipulation
library(forecast)  # time series library
library(MLmetrics) # calculate error
library(ggplot2)   # Beautify the graph
library(tidyr)     # Tidy the data
library(zoo)       # Order index observations
library(tseries)   # adf.test

getSymbols(c("GOOG"), from = "2010-01-01", to = "2023-01-01")

goog_close <- Cl(GOOG)

goog_close <- data.frame(
  Date = index(goog_close),
  Close = coredata(goog_close)) %>%
  rename(Date = 1, Close = 2)


GOOG <- goog_close %>% 
  mutate(Date = ymd(Date)) %>% 
  complete(Date = seq.Date(min(Date), max(Date), by="day")) %>% 
  arrange()

GOOG %>% head()

GOOG %>% is.na() %>% colSums()


GOOG$Close %>% tail(200) %>% 
  plot(type="l",
       col = "blue",
       lwd = 2,
       xlab = "time (days)",
       ylab = 'Close',
       main = "Stock Market Close")



GOOG_new <- GOOG %>% 
  mutate(Close = na.fill(Close, "extend")) # Fill the missing value with "extend"

GOOG_new %>%   
  tail(200) %>% # get the last 200 data
  plot(type="l",
       col = "blue",
       lwd = 2,
       xlab = "time (days)",
       ylab = 'Close',
       main = "Stock Market Close") # Plot GOOG_new


GOOG_new %>% 
  plot(type="l",
       col = "blue",
       lwd = 2,
       xlab = "time (years)",
       ylab = 'Close',
       main = "Stock Market Close")



GOOG_new %>% tail() %>% 
  plot(type="l",
       col = "blue",
       lwd = 2,
       xlab = "time (days)",
       ylab = 'Close',
       main = "Stock Market Close")


class(GOOG_new)


GOOG_m <- GOOG_new %>% 
  group_by(month = lubridate::floor_date(Date, "month")) %>%
  summarize(Close = mean(Close))

GOOG_m %>% head()


GOOG_ts <- ts(
  data = GOOG_m$Close,
  start = c(2010,01,01),
  frequency = 12
)

class(GOOG_ts)


GOOG_w <- window(GOOG_ts, start = 2010) 

GOOG_w%>% autoplot()



# test
GOOG_test <- tail(GOOG_w, 24)

# train
GOOG_train <- head(GOOG_w, -length(GOOG_test))

GOOG_train %>% autoplot()

GOOG_dc <- decompose(GOOG_train, type = "multiplicative")
GOOG_dc %>% autoplot()

