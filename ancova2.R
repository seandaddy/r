library(multcomp)
anorexia <- read.csv("data/anorexia.csv")
attach(anorexia)
Treat <- relevel(Treat, ref = "Cont") # Change Cont as the reference group.
# levels(Treat) = c("Count","CBT", "FT") also possible

out <- lm(Postwt ~ Prewt + Treat, data = anorexia)
anova(out)
summary(out)

dunn <- glht(out, linfct = mcp(Treat = "Dunnett"))
summary(dunn)
plot(dunn)
