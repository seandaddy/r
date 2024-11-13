library(ggplot2)
library(dplyr)
library(tidyverse)
library(patchwork) # To display 2 charts together

jhu_url <- paste("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
covdata <- read_csv(jhu_url)

korea_confirmed <- covdata %>% 
  rename(province = "Province/State",country_region = "Country/Region") %>% 
  pivot_longer(-c(province, country_region, Lat, Long), names_to = "Date", values_to = "cumulative_cases") %>% 
  mutate(Date = lubridate::mdy(Date) ) %>% #- days(1)
  filter(country_region == "Korea, South") %>% arrange(province, Date) %>% # group_by(province) %>% 
  mutate(incident_cases = c(0, diff(cumulative_cases))) %>% 
  ungroup() %>% select(-c(country_region, province, Lat, Long)) 
  # ungroup() %>% select(-c(country_region, Lat, Long, cumulative_cases)) %>% 
  # filter(str_detect(province, "Diamond Princess", negate = TRUE))

japan_confirmed <- covdata %>% rename(province = "Province/State", 
                                                country_region = "Country/Region") %>% pivot_longer(-c(province, 
                                                                                                       country_region, Lat, Long), names_to = "Date", values_to = "cumulative_cases") %>% 
  mutate(Date = lubridate::mdy(Date) ) %>% #- days(1)
  filter(country_region == "Japan") %>% arrange(province, Date) %>%  group_by(Date) %>% 
  summarize(cumulative_cases=sum(cumulative_cases)) %>%
  mutate(incident_cases = c(0, diff(cumulative_cases))) # %>% 
  #ungroup() %>% select(-c(Lat, Long))

hubei_confirmed <- covdata %>% rename(province = "Province/State", 
                                                country_region = "Country/Region") %>% pivot_longer(-c(province, 
                                                                                                       country_region, Lat, Long), names_to = "Date", values_to = "cumulative_cases") %>% 
  mutate(Date = lubridate::mdy(Date) ) %>% #- days(1)
  filter(province == "Hubei") %>% arrange(province, Date) %>%  group_by(Date) %>% 
  mutate(incident_cases = c(0, diff(cumulative_cases))) %>% 
  ungroup() %>% select(-c(province, country_region, Lat, Long))
  
cov1 <- left_join(korea_confirmed, japan_confirmed, by=c("Date"))
# cov1 <- left_join(cov1, hubei_confirmed, by=c("Date"))
coeff <- 126403210/51254387
# coeff1 <- 58500000/51254387
  ggplot(cov1, aes(x=Date)) +
  geom_line( aes(y=cov1$cumulative_cases.y), color="#00AFBB") + 
  geom_line( aes(y=cov1$cumulative_cases.x*coeff), color="#FC4E07") +
  scale_y_continuous(
    # Features of the first axis
    name = "Japan",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="Korea")
  ) +
  ggtitle("COVID-19 JPN vs KOR")  

#  ggplot(cov1, aes(x=Date)) +
#    geom_line( aes(y=cov1$cumulative_cases), color="#00AFBB") + 
#    geom_line( aes(y=cov1$cumulative_cases.x*coeff1), color="#FC4E07") +
#    scale_y_continuous(
#      name = "China",
#      sec.axis = sec_axis(~./coeff1, name="Korea")
#    ) +
#    ggtitle("COVID-19 Hubei vs KOR") 
  
  ggplot(cov1, aes(x=Date)) +
    geom_line( aes(y=cov1$incident_cases.y), color="#00AFBB") + 
    geom_line( aes(y=cov1$incident_cases.x*coeff), color="#FC4E07") +
    scale_y_continuous(
      # Features of the first axis
      name = "Japan",
      # Add a second axis and specify its features
      sec.axis = sec_axis(~./coeff, name="Korea")
    ) +
    ggtitle("COVID-19 JPN vs KOR change")
  
  ggplot(cov1, aes(x=Date)) +
    geom_line( aes(y=log(cov1$cumulative_cases.y)), color="#00AFBB") + 
    geom_line( aes(y=log(cov1$cumulative_cases.x*coeff)), color="#FC4E07") +
    scale_y_continuous(
      name = "Japan",
      # Add a second axis and specify its features
      sec.axis = sec_axis(~./log(coeff), name="Korea")
    ) +
    ggtitle("COVID-19 JPN vs KOR log scale")
