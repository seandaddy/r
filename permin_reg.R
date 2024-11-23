library(tidyverse)  # for data wrangling
library(janitor)  # for data cleaning
library(rvest)      # for web scraping
library(plm)

mydata <- read.delim("~/Documents/R script/mydata.txt")
salary <- read.csv("~/Documents/R script/salary.txt", sep="")

nba_data <- full_join(player_stats,salary, by = c("player","year"))

nba_data$tm <- nba_data$tm.x
nba_data$lnsalary <- log(nba_data$salary,base=exp(1))
nba_data$age2 <- (nba_data$age)^2

nba_new <- nba_data[,c("player","year","pos","tm","lnsalary","pts","age","age2","trb","ast","mp","tov","blk")] %>% 
  dplyr::filter(!pos=="") %>%
  dplyr::filter(mp>300) %>%
  mutate_at(vars(-c(player,tm,pos)),as.numeric) %>%
  mutate_at(vars(-c(player,tm,pos)), funs(replace(., is.na(.), 0)))%>%
  as_tibble() %>% 
  group_by(player,year) %>% 
  slice(1) %>% 
  ungroup()
x <- pdata.frame(nba_new, index=c('player', 'year'))
x$l_pts = plm::lag(x$pts)
x$l_trb = plm::lag(x$trb)
x$l_ast = plm::lag(x$ast)
x$l_mp = plm::lag(x$mp)
x$l_tov = plm::lag(x$tov)
x$l_blk = plm::lag(x$blk)
x <- x %>%
  mutate_at(vars(c(lnsalary)), funs(replace(., is.infinite(.), 0)))%>%
  dplyr::filter(!lnsalary==0)

fixed <- plm(lnsalary ~ l_pts + age +age2+ l_trb + l_ast + l_blk +l_mp , data=x, index=c("player", "year"), model="within")
fixed.time <- plm(lnsalary ~ l_pts + age +age2 + l_trb + l_ast + l_blk +l_mp+  factor(year), data=x, index=c("player","year"), model="within")
fixed.team <- plm(lnsalary ~ l_pts + age +age2 + l_trb + l_ast + l_blk +l_mp+  factor(tm), data=x, index=c("player","year"), model="within")

summary(fixed)
summary(fixed.time)
summary(fixed.team)
