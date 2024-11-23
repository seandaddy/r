library(tidyverse)  # for data wrangling
library(janitor)  # for data cleaning
library(rvest)      # for web scraping
library(plm)

mydata <- read.csv("~/Documents/R script/merge.txt", sep="")
winrate <- read.delim("~/Documents/R script/winrate.txt")
mydata <- clean_names(mydata) %>%
  mutate_at(vars(c(salary,age)),as.numeric)

mydata$tm <- mydata$tm_y
mydata$tm <- gsub("NJN", "BRK", mydata$tm)
mydata$tm <- gsub("WAS", "WSB", mydata$tm)
mydata <- full_join(mydata,winrate, by=c("tm","year"))


mydata$lnsalary <- log(mydata$salary,base=exp(1))
mydata$age2 <- (mydata$age)^2

nba_new <- mydata[,c("player","year","tm","g","lnsalary","age","age2","pts","trb","ast","mp","tov","blk","pos","winr")] %>% 
  dplyr::filter(!pos=="") %>%
  dplyr::filter(g > 20) %>%
  dplyr::filter(mp>250)%>%
  group_by(player) %>%
  mutate_at(vars(-c(player,tm)),as.numeric) %>%
  mutate_at(vars(-c(player,tm)), funs(replace(., is.na(.), 0)))%>%
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
  x$l_winr = plm::lag(x$winr)
  
x <- x %>%
  mutate_at(vars(c(lnsalary)), funs(replace(., is.infinite(.), 0)))%>%
  dplyr::filter(!lnsalary==0)
  
fixed <- plm(lnsalary ~ l_pts + age +age2+ l_trb +l_mp +l_ast + l_blk +l_winr , data=x, index=c("player", "year"), model="within")
fixed.time <- plm(lnsalary ~ l_pts + age +age2 + l_trb+l_mp + l_ast + l_blk + l_winr +  factor(year), data=x, index=c("player","year"), model="within")
summary(fixed)

summary(fixed.time)
