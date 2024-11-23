library("readxl")
library("ggplot2")
library("plm")
library(psych)
library(dplyr)

mydata <- read_excel("/Users/sangyong/Desktop/gravity91.xlsx")

#ggplot(mydata, aes(x=quarter, y=Export1)) + 
#  geom_line() +
#  geom_point(size=0.1)

# scatplot <- select(panel, Strict1, Cases1, cash1)
# pairs.panels(scatplot, 
#             method = "pearson", # correlation method
#             hist.col = "#00AFBB",
#             density = TRUE,  # show density plots
#             ellipses = TRUE # show correlation ellipses
# )

# options(scipen=999)  # turn-off scientific notation like 1e+48

# Scatterplot
gg <- ggplot(mydata, aes(x=Country, y=Export1)) + 
  geom_point(aes(col=Country)) + 
#  geom_smooth(method="loess", se=F) + 
#  xlim(c(0, 0.1)) + 
#  ylim(c(0, 5000000000)) + 
  theme(#axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        #axis.ticks.x=element_blank()
        legend.position = "none"
        ) +
  labs(#subtitle="Export", 
       y="Export", 
       x="Country", 
       title="Export", 
       caption = "Source: 123")
plot(gg)

gg1 <- ggplot(mydata, aes(x=quarter, y=Export1)) + 
  geom_point(aes(col=quarter)) + 
  stat_summary(
    fun = "mean",        #argument updated in new version.
    geom = "point",) + 
#  geom_smooth(method = lm, se = FALSE) + 
  #  xlim(c(0, 0.1)) + 
    ylim(c(0, 2500000)) + 
  labs(#subtitle="Export", 
    y="Export", 
    x="Quarter", 
    title="Export", 
    caption = "Source: 123")
plot(gg1)


# Plot the individuals
ggplot(mydata, aes(x=quarter, y=Export1, colour=Country, group=Country)) +
  geom_line() + geom_point(shape=21, fill="white") + 
  labs(#subtitle="Export",
    # y="Export",
    # x="Quarter",
    caption = "Source: UN Comtrade") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
    # axis.text.x=element_blank(),
    #axis.ticks.x=element_blank()
    legend.position = "none") +
  #geom_label(aes(group = Country)) +
  scale_y_continuous(labels = scales::comma) #+
  # ylim(min(mydata$Export1),max(mydata$Export1))

#plotmeans(Export1 ~ Country, data = mydata)
#plotmeans(Export1 ~ quarter, data = mydata)

Y <- cbind(log(mydata$Export1))
X <- cbind(log(mydata$GDP1), log(mydata$Dist1), mydata$Strict1, 
           mydata$Cases1, mydata$Rta) #, mydata$cash1)
pdata <- pdata.frame(mydata, index=c("Country", "quarter"))

summary(X)
summary(Y)

# Pooled OLS estimator
pooling <- plm(Y ~ X, data=pdata, model= "pooling")
summary(pooling)

# Between estimator
between <- plm(Y ~ X, data=pdata, model= "between")
summary(between)

# First differences estimator
# firstdiff <- plm(Y ~ X, data=pdata, model= "fd")
# summary(firstdiff)

# Fixed effects or within estimator
fixed <- plm(Y ~ X, data=pdata, model= "within")
summary(fixed)

# Random effects estimator
random <- plm(Y ~ X, data=pdata, model= "random")
summary(random)

# LM test for random effects versus OLS
plmtest(pooling)

# LM test for fixed effects versus OLS
pFtest(fixed, pooling)

# Hausman test for fixed versus random effects model
phtest(fixed, random)

# twofixed <- plm(Y ~ X | . - Strict1 +
#                  Cases1, data = pdata,
#                  model = "within")
# summary(twofixed)