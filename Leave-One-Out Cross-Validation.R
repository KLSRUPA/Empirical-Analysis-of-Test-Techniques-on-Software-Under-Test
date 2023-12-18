library(rethinking)
d <- read.csv("http://torkar.se/data-suts.csv", sep = ";")
data<-precis(d)
head(d)
str(d)
dat_list <- list(
  fm = d$fmeasure,
  techid = with(d, match (tech , unique (tech))),
  sutid =with(d, match (sut , unique (sut)))
)

m1<-ulam(
  alist
  (
    fm ~ exponential( lambda ),
    lambda <- 1.0/mu,
    log(mu) <- a[techid] + b[sutid],
    a[techid] ~ normal(log(10),0.5),
    b[sutid]  ~ normal(log(10),0.5)
  ) , data=dat_list , chains=4 , cores=4 , iter=10000, log_lik = True )
precis (m1, 2)
fmeasure <- sim(m1, post = extract.prior(m1))
hist(fmeasure, main = "histogram", xlab = "fmeasure values",ylab="frequency")

m2<-ulam(
  alist
  (
    fm ~ dnorm( k ,gamma ),
    k <- a[techid] + b[sutid],
    a[techid] ~ dnorm(100,10),
    b[sutid] ~ dnorm(100,10),
    gamma ~ dexp(1)
  ) , data=dat_list , chains=4 , cores=4 , iter=10000 , log_lik = TRUE)

Precis (m2,2)

fmeasure <- sim(m2, post = extract.prior(m2))
hist(fmeasure , main ="histogram", xlab = "fmeasure values", ylab = "frequency")

compare(m1,m2 func = "LOO")

