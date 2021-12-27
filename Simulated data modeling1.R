library(rethinking)
a_bar <- 1.5
sigma <- 1.5
ntanks <- 60
Ni <- as.integer(rep(c(5,10,25,35), each = 15))
set.seed(102)
a_tank <- rnorm(ntanks, a_bar, sigma)
d <- data.frame(tank = 1:ntanks, Ni=Ni, a_true=a_tank)
set.seed(103)
d$Si <- rbinom(ntanks, prob = logistic(d$a_true), size = d$Ni)
head(d)

dat <- list(
  N=d$Ni,
  S=d$Si,
  tank = d$tank
)

modelOF <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a[tank],
    a[tank] ~ dnorm(0,1.5)
  ), data = dat, chains = 4, log_lik = T
)

modelMM <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a[tank],
    a[tank] ~ dnorm(a_bar, sigma),
    a_bar ~ dnorm(0, 1.5),
    sigma ~ dexp(1)
  ), data=dat, chains = 4, log_lik = T
)

post <- extract.samples(modelOF)
d$OF_exP <- apply(inv_logit(post$a),2,mean)
post <- extract.samples(modelMM)
d$MM_exP <- apply(inv_logit(post$a),2,mean)
d$a_Ptrue <- inv_logit(d$a_true)
head(d)


plot(d$a_Ptrue, pch = 16)
points(d$OF_exP, pch = 16, col = rangi2)
points(d$MM_exP, pch = 16, col = col.alpha('red',0.7))

OF_error <- sqrt(sum((d$a_Ptrue - d$OF_exP)^2)/nrow(d))
MM_error <- sqrt(sum((d$a_Ptrue - d$MM_exP)^2)/nrow(d))
compare(modelMM, modelOF)


# plot(OF_error, pch = 16)
# points(MM_error, pch = 16, col = rangi2)
# 
# for( i in 1:length(MM_error)){
#   lines(x=c(i,i), y = c(OF_error[i], MM_error[i]))
# }
# 



##Creating a new dataset to test this on
a_bar <- 1.5
sigma <- 1.5
ntanks = 16
set.seet(104)
a_true <- rnorm(ntanks, mean = a_bar, sd = sigma)
Nitest <- rep(c(5,10,25,35), each = 4)
dt <- data.frame(tanks = 1:ntanks, Ni = Nitest, a_true = a_true)
dt$Si <- rbinom(ntanks, prob = inv_logit(dt$a_true), size = dt$Ni)

dt$destOF <- apply(link(modelOF, data = data.frame(N = dt$Ni, tank = nrow(dt))),2,mean)
dt$destMM <- apply(link(modelMM, data = data.frame(N = dt$Ni, tank = nrow(dt))),2,mean)
dt$p_true <-  inv_logit(dt$a_true)
