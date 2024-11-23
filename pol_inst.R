# Load packages
library(wbstats) # for downloading WB data
library(dplyr) # for selecting, renaming and mutating
library(janitor) # for rounding

# Store the list of indicators in an object
indicators <- c("uem" = "SL.UEM.1524.ZS", "mil" = "MS.MIL.XPND.ZS", "gini" = "SI.POV.GINI") 

# Download the data  
wbdf <- wb_data(indicators, mrv = 50) |> # download data for last 50 yrs
  select(!iso2c) |> # drop the iso2c code which we won't be using
  rename(year = date) # |> # rename date to year 
  # mutate(
  #   flfp = round_to_fraction(flfp, denominator = 100), # round to nearest 100th
  #   women_rep = round_to_fraction(women_rep, denominator = 100) 
  # )
# Load packages
library(vdemdata) # to download V-Dem data

# Download the data
democracy <- vdem |> # download the V-Dem dataset
  filter(year >= 1990)  |> # filter out years less than 1990
  select(                  # select (and rename) these variables
    country = country_name,     # the name before the = sign is the new name  
    vdem_ctry_id = country_id,  # the name after the = sign is the old name
    year, 
    pol_sta = e_wbgi_pve, 
    social_group = v2clsocgrp, 
    region = e_regionpol_6C
  ) |>
  mutate(
    region = case_match(region, # replace the values in region with country names
                        1 ~ "Eastern Europe", 
                        2 ~ "Latin America",  
                        3 ~ "Middle East",   
                        4 ~ "Africa", 
                        5 ~ "The West", 
                        6 ~ "Asia")
    # number on the left of the ~ is the V-Dem region code
    # we are changing the number to the country name on the right
    # of the equals sign
  )

# Load countrycode
library(countrycode)

# Create new iso3c variable
democracy <- democracy |>    
  mutate(iso3c = countrycode(sourcevar = vdem_ctry_id, # what we are converting
                             origin = "vdem",         # we are converting from vdem
                             destination = "wb"))  |> # and converting to the WB iso3c code 
  relocate(iso3c, .after = vdem_ctry_id) # move iso3c 

# Load readr
library(readr)

# Perform left join using common iso3c variable and year
pol_sta <- left_join(democracy, wbdf, by = c("iso3c", "year")) |> 
  rename(country = country.x) |> # rename country.x
  select(!country.y)             # crop country.y

# Save as .csv for future use
write_csv(pol_sta, "pol_sta.csv")

# group_by(), summarize() and arrange()
pol_summary <- pol_sta |> # save result as new object
  group_by(region)  |> # group dem_women data by region
  summarize(           # summarize following vars (by region)
    pol_sta = mean(pol_sta, na.rm = TRUE), # calculate mean, remove NAs
    social_group = mean(social_group, na.rm = TRUE), 
    mil = mean(mil, na.rm = TRUE), 
    gini = mean(gini, na.rm = TRUE),
    uem = mean(uem, na.rm = TRUE)
  ) |> 
  arrange(desc(pol_sta)) # arrange in descending order by polyarchy score

# Save as .csv for future use
write_csv(pol_summary, "pol_summary.csv")

# Load ggplot2
library(ggplot2)

# Create a scatter plot

pol_sta |> # use the pol_sta data
  ggplot(aes(x = pol_sta, y = mil)) + # plot pol_sta on x-axis and mil on y-axis
  geom_point() + # add points
  geom_smooth(method = "lm") + # add a linear regression line
  labs( # add labels
    x = "Political Stability", 
    y = "Military Expenditure (USD)", 
    title = "Political Stability vs Military Expenditure"
  )

# Save the plot as a .png file
ggsave("pol_sta_mil.png")

# Regression analysis
  lm(pol_sta ~ mil + social_group + gini + uem, data = pol_sta) |> # run a linear regression
  summary()# summarize the results
  broom::tidy() |> # tidy the results
  write_csv("regression_results.csv") # save as .csv file
  
pol_sta$me <- ifelse(pol_sta$region == "Middle East", 1, 0)

# Regression analysis
lm(pol_sta ~ mil + social_group + gini + uem + me, data = pol_sta) |> # run a linear regression
  summary()# summarize the results
broom::tidy() |> # tidy the results
  write_csv("regression_results2.csv") # save as .csv file
