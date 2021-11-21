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
  scale_y_continuous(breaks = pretty_breaks()) +
  theme_classic()