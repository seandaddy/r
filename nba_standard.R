library(tidyverse)  # for data wrangling
library(janitor)  # for data cleaning
library(rvest)      # for web scraping
#library(corrplot)   # correlation plots

scrape_stats <- function(season){
  return(stats)
}
url <- "https://www.basketball-reference.com/leagues/NBA_2017_totals.html"
stats <- url %>% 
  read_html() %>% 
  html_table() %>% 
  .[[1]]

stats <- stats %>% 
  remove_empty("cols") %>%  #if any exist
  clean_names() %>%        # all column names to lower case and removing "%"
  dplyr::filter(!player=="Player") %>%  #delete headers in data frame
  mutate_at(vars(-c(player,tm,pos)),as.numeric) %>% #turn all stat cols to numeric
  mutate_at(vars(-c(player,tm,pos)), funs(replace(., is.na(.), 0))) %>% #turn NA to 0
  as_tibble() %>%
  group_by(player) %>% 
  slice(1) %>%
  ungroup() %>%
  select(-rk)