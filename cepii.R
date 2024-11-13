library(dplyr)
library(tidyr)
library("ggplot2")
library("plm")
library(psych)
library(dplyr)

gravity <- read.csv('/Users/eer/Downloads/Gravity_csv_V202211/gravity_V202211.csv')
dfg <- gravity %>%
  filter(
    year >= 1980,
    country_id_o != country_id_d,
    country_exists_o != 0,
    country_exists_d != 0
  )

dfg <- dfg %>%
  select(year, iso3_o, iso3_d, dist, gdp_o, gdp_d, gatt_o, gatt_d, 
         wto_o, wto_d, eu_o, eu_d, fta_wto, tradeflow_comtrade_o, tradeflow_comtrade_d)

dfg <- dfg %>%
  drop_na(gdp_o, gdp_d, tradeflow_comtrade_o, tradeflow_comtrade_d)

write.csv(dfg, "~/Downloads/gravity80.csv", row.names = FALSE)
