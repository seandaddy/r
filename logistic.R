respire=read.csv("data/respire.csv")

# using tapply function
p=with(respire, tapply(count*outcome,treat,sum)/tapply(count,treat,sum))
odds=p/(1-p)
odds[2]/odds[1]

out=glm(outcome ~ treat, weights=count, family=binomial, data=respire)
summary(out)
exp(coef(out)["treattest"])
exp(confint(out,parm="treattest"))

# Reference: Radelet, M. L. Racial Characteristics and the Imposition of the Death Penalty. American Sociological Review, v46 n6 p918-27 Dec 1981
death=read.csv("data/death_penalty.csv")
def=xtabs(count ~defendant+death, data=death)
chisq.test(def)
vic=xtabs(count ~victim+death, data=death)
chisq.test(vic)

out1=glm(death~victim*defendant,weights=count,family=binomial,data=death)
summary(out1)
out2=glm(death~victim,weights=count,family=binomial,data=death)
summary(out2)
anova(out2,out1,test="Chisq")
exp(coef(out2)["victimWhite"])

library(MASS)
model=glm(low ~ lwt+factor(race)+smoke+ht+ui,data=birthwt, family=binomial)
summary(model)
