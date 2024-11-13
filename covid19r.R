library(COVID19)

covid_deaths < - covid19(verbose = FALSE) %>%
  ungroup() %>% 
  mutate(Week = week(date)) %>% 
  select(Country = id, Date = date, Week, Deaths = deaths, Population = population) %>% 
  filter(Date <  today() %>% add(days(-2))) %>% 
  mutate(Deaths_by_1Mpop = round(Deaths/Population*1e6))

get_top_countries_df < - function(covid_deaths, top_by, top_n, since){
  covid_deaths %>% 
    group_by(Date) %>% 
    top_n(100, Population) %>% 
    group_by(Country) %>% 
    filter(Date == max(Date)) %>% 
    ungroup() %>% 
    top_n(top_n, {{top_by}}) %>% 
    select(Country) %>% 
    inner_join(covid_deaths, ., by = "Country") %>% 
    filter(Date >= ymd(since))
}

ggplotly(
  covid_deaths %>% 
    get_top_countries_df(top_by = Deaths, top_n = 10, since = 20200301) %>% 
    ggplot(aes(Date, Deaths, col = Country)) + 
    geom_line(size = 1, show.legend = F) +
    labs(title = "Total deaths due to COVID-19", 
         caption = "Source: covid19datahub.io") + 
    theme_minimal() + 
    theme_custom() +
    scale_color_tableau() +
    NULL
) %>%
  layout(legend = list(orientation = "h", y = 0),
         annotations = list(
           x = 1, y = 1.05, text = "Source: covid19datahub.io",
           showarrow = F, xref = 'paper', yref = 'paper', font = list(size = 10)
         )
  )

ggplotly(
  covid_deaths %>% 
    get_top_countries_df(top_by = Deaths_by_1Mpop, top_n = 10, since = 20200301) %>% 
    select(-Deaths) %>% 
    rename(Deaths = Deaths_by_1Mpop) %>% 
    ggplot(aes(Date, Deaths, col = Country)) + 
    geom_line(size = 1, show.legend = F) +
    labs(title = "Total deaths per million people", 
         caption = "Source: covid19datahub.io") + 
    theme_minimal() + 
    theme_custom() + 
    scale_color_tableau() +
    NULL
) %>% 
  layout(legend = list(orientation = "h", y = 0),
         annotations = list(
           x = 1, y = 1.05, text = "Source: covid19datahub.io", 
           showarrow = F, xref = 'paper', yref = 'paper', font = list(size = 10)
         )
  )
