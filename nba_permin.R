library(tidyverse)  # for data wrangling
library(janitor)  # for data cleaning
library(rvest)      # for web scraping

scrape_stats <- function(season){
  #scrape
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_",season,"_per_minute.html")
  stats_tot <- url %>% 
    read_html() %>% 
    html_table() %>% 
    .[[1]]
  
  #clean
  player_stats <- stats_tot %>% 
    remove_empty("cols") %>%
    clean_names() %>% 
    dplyr::filter(!player=="Player") %>%
    dplyr::filter(!tm=="TOT") %>%
    mutate_at(vars(-c(player,tm,pos)),as.numeric) %>% 
    mutate_at(vars(-c(player,tm,pos)), funs(replace(., is.na(.), 0))) %>%
    as_tibble() %>% 
    group_by(player) %>% 
    slice(1) %>% 
    ungroup() %>% 
    select(-rk)
  return(player_stats)
}

player_stats <- scrape_stats(1996)
player_stats$year <- rep(1996,nrow(player_stats))

for (i in 1997:2018){
player_new <- scrape_stats(i)
player_new$year <- rep(i,nrow(player_new))
player_stats <- rbind(player_stats,player_new)
}

