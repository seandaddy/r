library(tidyverse)
library(investr)

cov=read.csv("data/cov19.csv")
cov$date <- as.Date(cov$date, format = "%m/%d/%y")

#exponential trend model
exp.model <- lm(log(KOR)~date,data = cov) 
exp.model.df <- data.frame(x=cov$date,
                           y=exp(fitted(exp.model)))
ggplot(cov, aes(x = date, y = KOR)) + geom_point() +
  stat_smooth(method = 'lm', aes(colour = 'linear'), se = FALSE) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = 'quadratic'), se= FALSE) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,3), aes(colour = 'cubic'), se = FALSE)+
  stat_smooth(data=exp.model.df, method = 'loess',aes(x,y,colour = 'exponential'), se = FALSE) 

# model_exponential <- lm(data = cov,log(KOR)~date)
# model_cubic <- lm(data = cov, KOR~poly(date,3))