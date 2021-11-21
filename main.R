library(tidyverse)  # for grammar
library(scales)     # for pretty breaks in figures
library(tidyquant)  # for data download
library(knitr)      # for html knitting
library(kableExtra) # for nicer tables

dax_raw <- tq_get("^GDAXI", get = "stock.prices",
                  from = "1988-07-01")

dax <- dax_raw %>%
  select(date, price = adjusted)

dax <- dax %>%
  arrange(date) %>%
  fill(price, .direction = "down")

summary(dax)

dax %>%
  ggplot(aes(x = date, y = price)) +
  geom_line() +
  labs(x = "", y = "Adjusted Price") +
  scale_x_date(expand = c(0, 0), breaks = "5 years") +
  scale_y_continuous(trans = log2_trans(),
    breaks = trans_breaks("log2", function(x) 2^x)) +
  theme_classic()

dax_nested <- dax %>%
  filter(date >= "1988-07-01" & date <= "1999-12-31") %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  nest()

get_seasonality <- function(data) {
  data %>%
    arrange(date) %>%
    mutate(trading_day = 1:n(), # use number of trading days as index
           ret = (log(price) - lag(log(price)))*100,
           ret = if_else(is.na(ret), 0, ret),
           cum_ret = 100 + cumsum(ret)) %>%
    select(trading_day, ret, cum_ret)
}

dax_seasonality <- dax_nested %>%
  mutate(seasonality = map(data, get_seasonality)) %>%
  select(year, seasonality) %>%
  unnest(cols = c(seasonality)) %>%
  ungroup()

dax_seasonality_summary <- dax_seasonality %>%
  group_by(trading_day) %>%
  summarize(mean = mean(cum_ret),
            q05 = quantile(cum_ret, 0.05),
            q95 = quantile(cum_ret, 0.95))

dax_seasonality_summary %>%
  ggplot(aes(x = trading_day)) +
  geom_line(aes(y = mean)) +
  labs(x = "Trading Days", y = "Cumulative Returns (in %)") +
  scale_x_continuous(expand = c(0, 0), breaks = pretty_breaks()) +
  scale_y_continuous(breaks = pretty_breaks()) +
  theme_classic()