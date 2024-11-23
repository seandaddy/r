rm(list=ls())

library(comtradr)
library(tidyverse)
library(openxlsx)
library(readxl)
library(RColorBrewer)
library(lubridate)
library(ggsci)
library(scales)
library(ggpubr)

options(ggrepel.max.overlaps = Inf)

set_primary_comtrade_key('db5113b42480437e853380b166d51f8b')

# Country Code (ISO-3)
cname <- country_codes
length(unique(cname$iso_3))

#Remove country disappeared
cname <- cname %>% filter(is.na(exit_year)==TRUE)
length(unique(cname$iso_3))

#Find 2-digit HS Code: 2, 4, 6 digit
HScode <- ct_get_ref_table("HS")

#99 is undefined
HS2 <- HScode %>% filter(nchar(id)==2) %>% filter(id!='99') %>% select(c(id,text))
hs2names <- HS2$id

#Find 4-digit HS Code
HS4 <- HScode %>% filter(nchar(id)==4) %>% filter(id!="9999") %>% select(c(id,text,parent))
hs4names <- HS4$id

#Find 6-digit HS Code
HS6 <- HScode %>% filter(nchar(id)==6) %>% filter(id!="999999") %>% select(c(id,text,parent))
hs6names <- HS6$id

# Find HS4 under HS2 85 Electrical machinery and equipment and parts
HS4_85 <- HS4[str_detect(HS4$id, '^85'),] #start with 85
HS4_85_86 <- HS4[str_detect(HS4$id, '^85') | str_detect(HS4$id, '^86'),] #start with 85 or 86
HS4_852 <- HS4[str_detect(HS4$id, '85$'),] #end with 85
HS4_85all <- HS4[str_detect(HS4$id, '85'),] #all including 85

#UN comtrade free version extract only 12 period 
data1 <- ct_get_data(
  reporter = c('CHN'),
  partner = c('World'),
  commodity_classification = 'HS',
  frequency = 'A',
  commodity_code = hs2names,
  start_date = 2020,
  end_date = 2024,
  flow_direction = c('export','import') #without flow_direction, total trade volumn and include reimport is extracted
)

data2 <- ct_get_data(
  reporter = c('KOR'),
  partner = c('World'),
  commodity_classification = 'HS',
  frequency = 'A',
  commodity_code = hs2names,
  start_date = 2010,
  end_date = 2019,
  flow_direction = c('export','import')
)

data3 <- ct_get_data(
  reporter = c('KOR'),
  partner = c('World'),
  commodity_classification = 'HS',
  frequency = 'A',
  commodity_code = hs2names,
  start_date = 2000,
  end_date = 2009,
  flow_direction = c('export','import')
)

data4 <- ct_get_data(
  reporter = c('KOR'),
  partner = c('World'),
  commodity_classification = 'HS',
  frequency = 'A',
  commodity_code = hs2names,
  start_date = 1990,
  end_date = 1999,
  flow_direction = c('export','import')
)

mdata <- bind_rows(data1,data2,data3,data4)

mdata <- mdata %>% select(c('refYear','reporterISO','partnerISO','flowDesc','primaryValue','cmdCode'))

names(mdata) <- c('date','reporter','partner','type','value','HS2')

mdata$partner <- replace(mdata$partner, mdata$partner=='W00', 'World')

mdata <- spread(mdata,type,value)
mdata <- mdata %>% relocate('date','reporter','partner','HS2')
HS2names <- HScode %>% select(c('id','text')) %>% filter(nchar(id)==2) %>% filter(id!='99') %>% rename(HS2=id)
mdata2 <- merge(mdata,HS2names)

# Based on 2022 Korea Top 10 export, decresing order, unit change to 1e+9 USD
mdata3 <- mdata2 |>group_by(date) |> mutate(Export=round(Export/1e+9,1),Import=round(Import/1e+9,1)) |> ungroup()

# Total export and import calculation
exall <- mdata3 |> group_by(date) |> summarize(eall=sum(Export)) |>ungroup()
imall <- mdata3 |> group_by(date) |> summarize(iall=sum(Import)) |>ungroup()

# Top 10 Export/Import
chnex <- mdata3 |> arrange(date,-Export) |> group_by(date) |>mutate(rank=row_number()) |> filter(rank<=10) |>ungroup() |> select(HS2, date, Export, text, rank)

chnex10sum <- chnex |> group_by(date) |> summarize(chnex10sum=sum(Export)) |>ungroup()

chnim <- mdata3 |> arrange(date,-Import) |> group_by(date) |>mutate(rank=row_number()) |> filter(rank<=10) |>ungroup() |> select(HS2, date, Import, text, rank)

chnim10sum <- chnim |> group_by(date) |> summarize(chnim10sum=sum(Import)) |>ungroup()

chnextemp <- merge(exall,chnex10sum)
chnimtemp <- merge(imall,chnim10sum)

chnexother <- chnextemp |>mutate(Export=eall-chnex10sum) |> select(date, Export)
chnexother$HS2 <- 'Other'; chnexother$text <- 'Other'

chnimother <- chnimtemp |>mutate(Import=iall-chnim10sum) |> select(date, Import)
chnimother$HS2 <- 'Other'; chnimother$text <- 'Other'

mchnex <-bind_rows(chnex,chnexother) |> merge(exall)
mchnim <-bind_rows(chnim,chnimother) |> merge(imall)

mchnex <- mchnex |> mutate(share=round(100*Export/eall,1))
mchnim <- mchnim |> mutate(share=round(100*Import/iall,1))

dfnames <- list('Export'=mchnex, 'Import'=mchnim)
write.xlsx(dfnames, file = 'chn_comtrade.xlsx')

