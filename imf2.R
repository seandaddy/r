library(IMFData); library(tidyverse); library(lubridate); library(anytime); library(plm); library(stargazer)

avaiableDB <- DataflowMethod()

databaseID <- "IFS"
databaseID1 <- "DOT"
startdate = "2018-01-01"
enddate = "2023-12-31"
checkquery = FALSE

## All country, 1) Quartely (Q), Real GDP in National Currency, NGDP_R_XDC, and short-term government interest rate FIGB_S_PA, and exchange rate (/USD) ENDA_XDC_USD_RATE
queryfilter <- list(CL_FREQ = "A", CL_AREA_IFS = "", CL_INDICATOR_IFS = c("NGDP_R_XDC"))
queryfilterx <- list(CL_FREQ = "Q", CL_AREA_DOT = "", CL_INDICATOR_DOT = c("TXG_FOB_USD"), CL_COUNTERPART_AREA_DOT=c("KR"))

# Now get the data from IMF using CompactDataMethod
data1 <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, checkquery, tidy=TRUE)
xdata1 <- CompactDataMethod(databaseID1, queryfilterx, startdate, enddate, checkquery, tidy=TRUE) 
data2 <- data1 %>% select(c("@TIME_PERIOD", "@OBS_VALUE", "@REF_AREA", "@INDICATOR"))
xdata2 <- xdata1 %>% select(c("@TIME_PERIOD", "@OBS_VALUE", "@REF_AREA", "@INDICATOR"))
# 
names(xdata2) <- c("date", "value", "area", "indicator")
data2 <- data2 %>% relocate(area, date, indicator, value) %>% arrange(area, date)
# data2 <- data2 %>% relocate(area, date, indicator, value) %>% arrange(area, date)
# str(data2)
xdata2 <- xdata2 %>% mutate(value=as.numeric(value))
xdata3 <- xdata2 %>% spread(indicator,value)
names(xdata3) <- c("date", "iso2c", "export")
# data2 <- data2 %>% mutate(date=as.integer(date), value=as.numeric(value))
# data3 <- data2 %>% spread(indicator,value)
# names(data3) <- c("iso2c", "date", "exrate", "gdp")

# if(!require(countrycode)) install.packages("countrycode")
# library(countrycode)
# data(codelist)
# country_set <- codelist
# country_set <- country_set %>%
#   select(country.name.en, iso2c, iso3c, imf, continent, region) %>% filter(!is.na(imf) & !is.na(iso2c))
xdata4 <- merge(xdata3, country_set)
#data4 <- merge(data3, country_set)

#Now, remove data with any missing observations
xdata4 <- xdata4[complete.cases(data4),]
xdata4 <- xdata4 %>% filter(export!=0)
xdata5 <- pdata.frame(xdata4, index=c("iso2c","date"))
# data4 <- data4[complete.cases(data4),]
# data4 <- data4 %>% filter(gdp!=0)
# data5 <- pdata.frame(data4, index=c("iso2c","date"))
# model1 <- plm(log(gdp)~log(exrate), data5, model="random")
# 
# data5 <- data5 %>% mutate(cont=as.factor(continent))
# model2 <- plm(log(gdp)~log(exrate)+I(cont),data5, model='random')
# stargazer(model1, model2, type='text')

ggplot(xdata5, aes(x=date, y=export, colour=iso2c, group=iso2c)) +
  geom_line() + geom_point(shape=21, fill="white") + 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_text(colour = "#668cff", size = 5),
        #=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none") #+
#geom_label(aes(group = Country)) +
#scale_y_continuous(labels = scales::comma) #+
# ylim(min(mydata$Export1),max(mydata$Export1))
