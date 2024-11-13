library(readxl)
library(ggplot2)
library(plm)
library(psych)
library(dplyr)
library(stargazer)
library(broom)

mydata <- read_excel("/Users/eer/Library/CloudStorage/Dropbox/Data Analysis/Gravity Model/gravity91.xlsx")

pdata <- pdata.frame(mydata, index=c("Country", "quarter"))
eq1 <- log(Export1) ~ log(GDP1) + log(Dist1) + Rta +cash1 + Strict1 + Cases1

# Pooled OLS estimator
pooling <- plm(eq1, data=pdata, model= "pooling")
summary(pooling)

# Random effect estimator
random <- plm(eq1, data=pdata, model= "random")
summary(random)


# Fixed effects or within estimator
fixed <- plm(eq1, data=pdata, model= "within")
summary(fixed)

#pdata1 <- pdata %>% mutate(cindex=Country)
# eq2 <- log(Export1) ~ log(GDP1) + log(Dist1)+ cash1 + Strict1 + Cases1 + as.factor(Country)
# fixed1 <- plm(eq2, data=pdata, model="between")
# summary(fixed1)

stargazer(pooling, random, fixed,
          omit = "cindex",
          dep.var.labels=c("Pooling", "Random", "Fixed"),
          covariate.labels=c("ln(GDP)","ln(Distance)","FTA", "Cash", "Strict",
                             "Cases"),
          type='latex')