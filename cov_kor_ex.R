library(readxl)
library(ggplot2)
library(plm)
library(psych)
library(dplyr)
library(stargazer)
library(broom)

mydata <- read_excel("/Users/drsyoh/Documents/python/data/gravity91.xlsx")

oecd_countries <- c(
  "Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia",
  "Costa Rica", "Czech Republic", "Denmark", "Estonia", "Finland",
  "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland",
  "Israel", "Italy", "Japan", "Korea", "Latvia", "Lithuania",
  "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway",
  "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain",
  "Sweden", "Switzerland", "Turkey", "United Kingdom", "United States"
)

# For OECD countries
oecd_data <- mydata %>% 
  filter(Country %in% oecd_countries)

# For non-OECD countries
non_oecd_data <- mydata %>% 
  filter(!Country %in% oecd_countries)

pdata <- pdata.frame(mydata, index=c("Country", "quarter"))
oecd_pdata <- pdata.frame(oecd_data, index=c("Country", "quarter"))
non_oecd_pdata <- pdata.frame(non_oecd_data, index=c("Country", "quarter"))

eq1 <- log(Export1) ~ log(GDP1) + log(Dist1) + Rta + Strict1 + Cases1

# Pooled OLS estimator
# pooling <- plm(eq1, data=pdata, model= "pooling")
# summary(pooling)

# Random effect estimator
random <- plm(eq1, data=pdata, model= "random")
oecd_random <- plm(eq1, data=oecd_pdata, model= "random")
non_oecd_random <- plm(eq1, data=non_oecd_pdata, model= "random")
summary(random)
summary(oecd_random)
summary(non_oecd_random)

get_components <- function(model) {
  id <- index(model)[[1]]
  res <- residuals(model)
  
  # Calculate overall variance
  sigma2_1 <- var(res)
  
  # Calculate within variance
  means <- tapply(res, id, mean)
  sigma2_u <- var(means)
  
  # Calculate between variance
  sigma2_e <- sigma2_1 - sigma2_u
  
  # Calculate rho
  rho <- sigma2_u / (sigma2_u + sigma2_e)
  
  return(list(
    sigma_u = sqrt(sigma2_u),
    sigma_e = sqrt(sigma2_e),
    rho = rho
  ))
}

# Get components for both models
comp1 <- get_components(random)
comp2 <- get_components(non_oecd_random)
comp3 <- get_components(oecd_random)

# Create additional lines for table
add.lines <- list(
  c("sigma_u", round(comp1$sigma_u, 3), round(comp2$sigma_u, 3), round(comp3$sigma_u, 3)),
  c("sigma_e", round(comp1$sigma_e, 3), round(comp2$sigma_e, 3), round(comp3$sigma_e, 3)),
  c("rho", round(comp1$rho, 3), round(comp2$rho, 3), round(comp3$rho, 3))
)

# Create LaTeX table
stargazer(random, non_oecd_random, oecd_random,
          omit = "cindex",
          dep.var.labels=c("Random", "OECD", "Non-OECD"),
          covariate.labels=c("ln(GDP)","ln(Distance)","FTA", "Stringency", "Case"),
          type = "latex",
          add.lines = add.lines,
          digits = 3)

# Fixed effects or within estimator
# fixed <- plm(eq1, data=pdata, model= "within")
# summary(fixed)

#pdata1 <- pdata %>% mutate(cindex=Country)
# eq2 <- log(Export1) ~ log(GDP1) + log(Dist1)+ cash1 + Strict1 + Cases1 + as.factor(Country)
# fixed1 <- plm(eq2, data=pdata, model="between")
# summary(fixed1)

