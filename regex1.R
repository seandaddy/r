setwd("/Users/sangyongoh/Documents/r")
cars = read.csv("data/cars.csv")
out1=lm(dist~speed,data=cars)
out2=lm(log(dist)~speed,data=cars)
out3=lm(sqrt(dist)~speed,data=cars)
out4=lm(sqrt(dist)~speed-1,data=cars)

plot(dist~speed, data=cars, col="blue")
abline(out1, col="red")

plot(log(dist)~speed, data=cars, col="blue")
abline(out2, col="red")

plot(sqrt(dist)~speed, data=cars, col="blue")
abline(out3, col="red")

plot(sqrt(dist)~speed-1, data=cars, col="blue")
abline(out4, col="red")
summary(out4)
par(mfrow=c(2,2))
plot(out4)

#simulation
dists = rnorm(n=nrow(cars),mean=fitted(out4),sd=summary(out4)$sigma)^2
plot(dists~cars$speed)

