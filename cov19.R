library(ggplot2)
library(dplyr)
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