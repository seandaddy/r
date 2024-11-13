setwd("/Users/sangyongoh/Documents/R script")
dental = read.csv("data/dental.csv")

boxplot(resp~treatment,data=dental,col='red')
boxplot(log(resp)~treatment,data=dental)

var.test(resp~treatment,data=dental) #variance equality test
var.test(log(resp)~treatment,data=dental) #variance equality test, log-normal
t.test(resp~treatment,data=dental) #Welch test
t.test(log(resp)~treatment,var.equal=TRUE,data=dental) #pooled variance test

regout=lm(log(resp)~treatment,data=dental)
shapiro.test(resid(regout))

x=rlnorm(100,2.5,.5)
qqnorm(x)
qqline(x)