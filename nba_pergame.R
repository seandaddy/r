library(readxl)
library(tidyverse)  # for data wrangling
library(janitor)  # for data cleaning
library(rvest)      # for web scraping
library(plm)

mydata <- read_excel("~/Downloads/NBA Data With Salaries (1996 - 2017).xlsx")

mydata <- clean_names(mydata) %>%
  mutate_at(vars(-c(player,tm,pos)),as.numeric)

mydata$lnsalary <- log(mydata$salary,base=exp(1))
mydata$age2 <- (mydata$age)^2

nba_new <- mydata[,c("player","year","pos","tm","lnsalary","pts_g","age","age2","trb_g","ast_g","mp_g","tov_g","blk_g")] %>% 
  dplyr::filter(!pos=="") %>%
  mutate_at(vars(-c(player,tm,pos)),as.numeric) %>%
  mutate_at(vars(-c(player,tm,pos)), funs(replace(., is.na(.), 0)))%>%
  mutate_at(vars(c(lnsalary)), funs(replace(., is.infinite(.), 0)))%>%
  dplyr::filter(!lnsalary==0) %>%
  as_tibble() %>% 
  group_by(player,year) %>% 
  slice(1) %>% 
  ungroup()

nba_lag<-nba_new[with(nba_new, order(player,year)), ]  # sort by id and then by date
nba_lag$l_pts=c(NA,nba_new$pts_g[-length(nba_new$pts_g)]) # create a new var with data displaced by 1 unit
nba_lag$l_pts[nba_new$player != c(NA, nba_new$player[-length(nba_new$player)])] =NA # NA data with different current and lagged id.
nba_lag$l_trb=c(NA,nba_new$trb_g[-length(nba_new$trb_g)]) # create a new var with data displaced by 1 unit
nba_lag$l_trb[nba_new$player != c(NA, nba_new$player[-length(nba_new$player)])] =NA # NA data with different current and lagged id.
nba_lag$l_ast=c(NA,nba_new$ast_g[-length(nba_new$ast_g)]) # create a new var with data displaced by 1 unit
nba_lag$l_ast[nba_new$player != c(NA, nba_new$player[-length(nba_new$player)])] =NA # NA data with different current and lagged id.
nba_lag$l_tov=c(NA,nba_new$tov_g[-length(nba_new$tov_g)]) # create a new var with data displaced by 1 unit
nba_lag$l_tov[nba_new$player != c(NA, nba_new$player[-length(nba_new$player)])] =NA # NA data with different current and lagged id.
nba_lag$l_mp=c(NA,nba_new$mp_g[-length(nba_new$mp_g)]) # create a new var with data displaced by 1 unit
nba_lag$l_mp[nba_new$player != c(NA, nba_new$player[-length(nba_new$player)])] =NA # NA data with different current and lagged id.
nba_lag$l_blk=c(NA,nba_new$blk_g[-length(nba_new$blk_g)]) # create a new var with data displaced by 1 unit
nba_lag$l_blk[nba_new$player != c(NA, nba_new$player[-length(nba_new$player)])] =NA # NA data with different current and lagged id.

fixed <- plm(lnsalary ~ l_pts + age +age2+ l_trb + l_ast + l_blk , data=nba_lag, index=c("player", "year"), model="within")
fixed.time <- plm(lnsalary ~ l_pts + age +age2 + l_trb + l_ast + l_blk +  factor(year), data=nba_lag, index=c("player","year"), model="within")
summary(fixed)
summary(fixed.time)
  