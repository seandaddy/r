setwd("/Users/sangyongoh/Documents/r")
df = read.csv("StudentsPerformance.csv")
attach(df)

pairs(df[ , 6:8],
      col = "red",                                         # Change color
      pch = 18,                                            # Change shape of points
      labels = c("math score","reading score","writing score"),                  # Change labels of diagonal
      main = "Scores pairs plot")             # Add a main title
hist(math.score)
hist(reading.score)
hist(writing.score)
boxplot(math.score~lunch, main="Math", xlab="Lunch", ylab="Math Score")
boxplot(reading.score~lunch, main="Reading", xlab="Lunch", ylab="Reading Score")
boxplot(writing.score~lunch, main="Writing", xlab="Lunch", ylab="Writing Score")

result <- lm (math.score ~ gender + race.ethnicity + 
                parental.level.of.education + lunch + 
                test.preparation.course)
result
detach(df)

anorexia=read.csv("data/anorexia.csv")

with(anorexia[anorexia$Treat=="FT",], t.test(Postwt-Prewt))
with(anorexia[anorexia$Treat!="CBT",], t.test(Postwt-Prewt))
with(anorexia[anorexia$Treat=="Cont",], shapiro.test(Postwt-Prewt))

#simulation
library(MASS)
x=mvrnorm(n=100,mu=c(94,93), Sigma=matrix(c(10,6,6,10), ncol=2))
t.test(x[,2]-x[,1])
