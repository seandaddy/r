library(janitor)  # for data cleaning
library(rvest)      # for web scraping
library(tidyverse)

scrape_stats <- function(season){
  #total stats
  #scrape
  return(player_stats)
}

url <- paste0("https://www.basketball-reference.com/leagues/NBA_2017_totals.html")
stats_tot <- url %>% 
  read_html() %>% 
  html_table() %>% 
  .[[1]]

#clean
player_stats_tot <- stats_tot %>% 
  remove_empty("cols") %>%
  clean_names() %>% 
  dplyr::filter(!player=="Player") %>%
  mutate_at(vars(-c(player,tm,pos)),as.numeric) %>% 
  mutate_at(vars(-c(player,tm,pos)), funs(replace(., is.na(.), 0))) %>% 
  as_tibble() %>% 
  group_by(player) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-rk)

#per minute
url <- paste0("https://www.basketball-reference.com/leagues/NBA_2017_per_minute.html")
stats_pm <- url %>% 
  read_html() %>% 
  html_table() %>% 
  .[[1]]

player_stats_pm <- stats_pm %>% 
  remove_empty("cols") %>%
  clean_names() %>% 
  dplyr::filter(!player=="Player") %>%
  mutate_at(vars(-c(player,tm,pos)),as.numeric) %>% 
  mutate_at(vars(-c(player,tm,pos)), funs(replace(., is.na(.), 0))) %>% 
  as_tibble() %>% 
  group_by(player) %>% 
  slice(1) %>% 
  ungroup() %>% 
  rename_at(vars(9:29),funs(paste0(.,"_pm"))) %>% 
  select(-rk)

#advanced
url <- paste0("https://www.basketball-reference.com/leagues/NBA_2017_advanced.html")
stats_adv <- url %>% 
  read_html() %>% 
  html_table() %>% 
  .[[1]]

player_stats_adv <- stats_adv %>% 
  remove_empty("cols") %>%
  clean_names() %>% 
  dplyr::filter(!player=="Player") %>%
  mutate_at(vars(-c(player,tm,pos)),as.numeric) %>% 
  mutate_at(vars(-c(player,tm,pos)), funs(replace(., is.na(.), 0))) %>% 
  as_tibble() %>% 
  group_by(player) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-rk)

player_stats <- full_join(player_stats_tot,player_stats_pm,
                            by = c("player", "pos", "age", "tm", "g", "gs", "mp")) %>% 
   full_join(player_stats_adv,
             by = c("player", "pos", "age", "tm", "g", "mp"))