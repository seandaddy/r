set.seed(1234)
height=rnorm(n=100000,mean=175,sd=5)

mean(height)
var(height)

m=NULL
v=NULL
for (i in 1:10000){
  x=sample(height,size=10)
  m[i]=mean(x)
  v[i]=var(x)
}

mean(m)
sd(m)
mean(v)
mean(v*9/10)