## Default repo
library(tidyverse)  # for grammar
library(scales)     # for pretty breaks in figures
library(tidyquant)  # for data download
library(knitr)      # for html knitting
library(kableExtra) # for nicer tables

dax_raw <- tq_get("^GDAXI", get = "stock.prices",
                  from = "2009-03-01", to = "2018-01-01")

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
  filter(date >= "2009-03-01" & date <= "2018-01-01") %>%
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

dax_seasonality_summary %>%
  ggplot(aes(x = trading_day)) +
  geom_ribbon(aes(ymin = q05, ymax = q95), alpha = 0.25) +
  geom_line(aes(y = mean)) +
  labs(x = "Trading Days", y = "Cumulative Returns (in %)") +
  scale_x_continuous(expand = c(0, 0), breaks = pretty_breaks()) +
  scale_y_continuous(breaks = pretty_breaks()) +
  theme_classic()

# Trading strategy
dax_monthly <- dax %>%
  mutate(year = year(date),
         month = month(date)) %>%
  group_by(year, month) %>%
  slice(which.max(date)) %>%
  ungroup() %>%
  arrange(date) %>%
  mutate(ret = (log(price) - lag(log(price))) * 100) %>%
  na.omit()
nrow(dax_monthly)

#temp <- tempfile()
#download.file("https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Europe_3_Factors_CSV.zip",
#              temp)
#unzip(temp, "Europe_3_Factors.csv")
rf_raw <- read_csv("Europe_3_Factors.csv", skip = 3)

## Parsed with column specification:
## cols(
##   X1 = col_character(),
##   `Mkt-RF` = col_character(),
##   SMB = col_character(),
##   HML = col_character(),
##   RF = col_character()
## )

rf <- rf_raw %>%
  mutate(date = ymd(paste0(X1, "01")),
         year = year(date),
         month = month(date),
         rf = as.numeric(RF)) %>%
  filter(date <= "2021-09-01") %>%
  select(year, month, rf)

dax_monthly <- dax_monthly %>%
  left_join(rf, by = c("year", "month")) %>%
  mutate(excess_ret = ret - rf) %>%
  na.omit()
nrow(dax_monthly) # lose a few obs b/c ff data starts in july 1990

dax_monthly$month_factor <- factor(x = months(dax_monthly$date),
                                   levels = c("January", "February", "March",
                                              "April", "May", "June", "July",
                                              "August", "September", "October",
                                              "November", "December"))

dax_monthly %>%
  group_by(`Month` = month_factor) %>%
  summarize(Mean = mean(excess_ret),
            SD = sd(excess_ret),
            Q05 = quantile(excess_ret, 0.05),
            Q95 = quantile(excess_ret, 0.95),
            `t-Statistic` = sqrt(n()) * mean(excess_ret) / sd(excess_ret) ) %>%
  kable(digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

dax_monthly %>%
  ggplot(aes(x = month_factor, y = excess_ret)) +
  geom_boxplot() +
  labs(x = "", y = "Monthly Excess Return (in %)") +
  theme_classic()

#summary(lm(excess_ret ~ month_factor, data = dax_monthly), robust = TRUE)

halloween_months <- c(11, 12, 1, 2, 3, 4)
seasonality_months <- c(10, 11, 12, 1, 2, 3, 4, 5, 6)
dax_monthly <- dax_monthly %>%
  mutate(halloween = if_else(month %in% halloween_months, 1L, 0L),
         seasonality = if_else(month %in% seasonality_months, 1L, 0L))

# `Halloween’ effect:
summary(lm(excess_ret ~ halloween, data = dax_monthly), robust = TRUE)

# excess returns are higher during the months November-April
summary(lm(excess_ret ~ seasonality, data = dax_monthly), robust = TRUE)

# compare the returns of the three different strategies on an annual basis:compare the returns of the three different strategies on an annual basis:
dax_monthly <- dax_monthly %>%
  mutate(excess_ret_halloween = if_else(halloween == 1, ret, rf),
         excess_ret_halloween_short = if_else(halloween == 1, ret, -ret),
         excess_ret_seasonality = if_else(seasonality == 1, ret, rf),
         excess_ret_seasonality_short = if_else(seasonality == 1, ret, -ret))

dax_monthly %>%
  group_by(year) %>%
  summarize(`Buy and Hold` = sum(excess_ret),
            `Seasonality` = sum(excess_ret_seasonality),
            `Seasonality-Short` = sum(excess_ret_seasonality_short),
            `Halloween` = sum(excess_ret_halloween),
            `Halloween-Short` = sum(excess_ret_halloween_short)) %>%
  pivot_longer(-year, names_to = "strategy", values_to = "excess_ret") %>%
  ggplot(aes(x = year, group = strategy)) +
  geom_col(aes(y = excess_ret, fill = strategy), position = "dodge") +
  labs(x = "", y = "Annual Excess Return (in %)", fill = "Strategy") +
  scale_x_continuous(expand = c(0, 0), breaks = pretty_breaks()) +
  theme_classic()

# Plotting the overall cumulative excess return of the five strategies confirms these conjectures
dax_monthly %>%
  arrange(date) %>%
  mutate(`Buy and Hold` = 100 + cumsum(excess_ret),
         `Seasonality` = 100 + cumsum(excess_ret_seasonality),
         `Seasonality-Short` = 100 + cumsum(excess_ret_seasonality_short),
         `Halloween` = 100 + cumsum(excess_ret_halloween),
         `Halloween-Short` = 100 + cumsum(excess_ret_halloween_short)) %>%
  select(date, `Buy and Hold`, `Seasonality`, `Seasonality-Short`,
         `Halloween`, `Halloween-Short`) %>%
  pivot_longer(-date, names_to = "strategy", values_to = "cum_excess_ret") %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = cum_excess_ret, color = strategy)) +
  scale_x_date(expand = c(0, 0), breaks = pretty_breaks()) +
  scale_y_continuous(breaks = pretty_breaks()) +
  labs(x = "", y = "Cumulative Excess Return (in %)", color = "Strategy") +
  theme_classic()

# Which of these strategies might constitute a better investment opportunity?
sharpe_ratio <- function(x) {
  sqrt(12) *  mean(x) / sd(x)
}

dax_monthly %>%
  arrange(date) %>%
  summarize(`Buy and Hold` = sharpe_ratio(excess_ret),
            `Seasonality` = sharpe_ratio(excess_ret_seasonality),
            `Seasonality-Short` = sharpe_ratio(excess_ret_seasonality_short),
            `Halloween` = sharpe_ratio(excess_ret_halloween),
            `Halloween-Short` = sharpe_ratio(excess_ret_halloween_short))