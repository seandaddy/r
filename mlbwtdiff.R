library(multcomp) # Multiple Comparisons Package
library(dplyr)
mlbw = read.csv("mlbw.csv")
id <- rownames(mlbw)
mlbw <- cbind(id=id, mlbw)
# mlbw <- rename(mlbw,c("Pitchers=pit","Infielder=inf","Outfielder=ouf"))
mlbl = reshape(data=mlbw, idvar="id",
               varying = c("Pitchers","Infielder","Outfielder"),
               v.name=c("wt"),
               times=c("Pitchers","Infielder","Outfielder"),
               new.row.names = 1:1000,
               direction="long")
mlb<-na.omit(mlbl)
mlb <- mlb %>% rename(pos = time)
mlb$pos = as.factor(mlb$pos)

boxplot(wt~pos, data=mlb)

out=lm(wt~pos, data=mlb)
anova(out)
par(mfrow=c(2,2))
plot(out)
shapiro.test(resid(out))

dunnett = glht(out, linfct= mcp(pos = "Dunnett"))
summary(dunnett)

tukey = glht(out, linfct= mcp(pos = "Tukey"))
summary(tukey)


plot(dunnett)
plot(tukey)


