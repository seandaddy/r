COVID-19 Cases
================

## COVID-19 confirmed cases in China vs. South Korea

The second vertical axis is aajusted by the population ratio between two
countries.

## Including Code

You can include R code in the document as follows:

``` r
library(ggplot2)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(patchwork) # To display 2 charts together

cov=read.csv("data/cov19.csv")
cov$date <- as.Date(cov$date, format = "%m/%d/%y")

# Value used to transform the data
coeff <- 1408526449/51254387

ggplot(cov, aes(x=date)) +
  geom_line( aes(y=CHN), color="#00AFBB") + 
  geom_line( aes(y=KOR*coeff), color="#FC4E07") +
  scale_y_continuous(
    # Features of the first axis
    name = "China",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="Korea")
  ) +
  ggtitle("COVID-19 CHN vs KOR")
```

![](covid_files/figure-gfm/covid-1.png)<!-- -->
