library(rethinking)
a_bar <- 1.5
sigma <- 1.5
ntanks <- 600
Ni <- as.integer(rep(c(5,10,25,35), each = 15))
a_tank <- rnorm(ntanks, mean = a_bar, sd = sigma)
d <- data.frame(tank = 1:ntanks, Ni=Ni, true_a=a_tank)
d$Si <- rbinom(ntanks, prob = logistic(d$true_a), size = d$Ni)
head(d)
# logistic(1.5)*35


a_bar <- 1.5
sigma <- 1.5
ntanks <- 12
Ni <- as.integer(rep(c(5,10,25,35), each = 3))
a_tank <- rnorm(ntanks, mean = a_bar, sd = sigma)
d1 <- data.frame(tank = 1:ntanks, Ni=Ni, true_a=a_tank)
d1$Si <- rbinom(ntanks, prob = logistic(d1$true_a), size = d1$Ni)
d1


# d1 <- reedfrogs[samp,]
# d <- d[-samp,]
# d$tank <- 1:nrow(d)
# d1$tank <- 1:nrow(d1)
# d1 <- d1[order(d1$density),]
# d
# d1

dat <- list(
  S = d$Si,
  N= d$Ni,
  tank = d$tank
)

overfit <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a[tank],
    a[tank] ~ dnorm(0,1.5)
  ), data = dat, chains = 4, log_lik = T
)

par(mfrow = c(1,2))
##In Sample prediction
post <- extract.samples(overfit)
d$a_OF <- apply(post$a,2,mean)
d$p_true <- inv_logit(d$true_a)
d$p_OF <- inv_logit(d$a_OF)
OF_error <- abs(d$p_OF - d$p_true)
plot(OF_error)


# inSampOF <- sim(overfit, data = data.frame(N= d$Ni, tank = 1:nrow(d)))
# inSampOF <- apply(inSampOF,2,mean)
# plot(d$Si, col = rangi2, pch = 16)
# points(inSampOF, pch = 16, col=col.alpha('black',0.5))
# mtext('In Sample Predictions')
# abline(v=30.5)
# abline(v=60.5)
# abline(v=90.5)
# 
# text(4.25,34,'small tanks')
# text(14.25,34,'medium tanks')
# text(23,34,'large tanks')
# 
# lines(x= 1:8, y=rep(mean(d$surv[1:8]),8), col = rangi2, lwd = 2)
# lines(x= 1:8, y=rep(mean(inSampOF[1:8]),8), lwd = 2, lty=2)
# lines(x= 9:18, y=rep(mean(d$surv[9:18]),10), col = rangi2, lwd = 2)
# lines(x= 9:18, y=rep(mean(inSampOF[9:18]),10), lwd = 2, lty=2)
# lines(x= 19:27, y=rep(mean(d$surv[19:27]),9), col = rangi2, lwd = 2)
# lines(x= 19:27, y=rep(mean(inSampOF[19:27]),9), lwd = 2, lty=2)
# 
# ##Out of sample prediction
# outSampOF <- sim(overfit, data = data.frame(N= d1$Ni, tank = 1:nrow(d1)))
# outSampOF <- apply(outSampOF,2,mean)
# plot(d1$Si, col = rangi2, pch = 16, 
#      ylim = c(min(min(d1$Si), min(outSampOF)),max(max(d1$Si), max(outSampOF))))
# points(outSampOF, pch = 16)
# mtext('Out of Sample Predictions')
# abline(v=3.5)
# abline(v=6.5)
# abline(v=9.5)
# 
# text(3,34,'small tanks')
# text(7,34,'medium tanks')
# text(11,34,'large tanks')
# lines(x= 1:5, y=rep(mean(d1$surv[1:5]),5), col = rangi2, lwd = 2)
# lines(x= 1:5, y=rep(mean(outSampOF[1:5]),5), lwd = 2, lty=2)
# 
# lines(x= 6:8, y=rep(mean(d1$surv[6:8]),3), col = rangi2, lwd = 2)
# lines(x= 6:8, y=rep(mean(outSampOF[6:8]),3), lwd = 2, lty=2)
# 
# lines(x= 9:12, y=rep(mean(d1$surv[9:12]),4), col = rangi2, lwd = 2)
# lines(x= 9:12, y=rep(mean(outSampOF[9:12]),4), lwd = 2, lty=2)



fit <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a[tank],
    a[tank] ~ dnorm(a_bar,sigma),
    a_bar ~ dnorm(0,1.5),
    sigma ~ dexp(1)
  ), data = dat, chains = 4, log_lik = T
)

post <- extract.samples(fit)
d$a_partpool <- apply(post$a,2,mean)
d$p_F <- inv_logit(d$a_partpool)
F_error <- abs(d$p_true - d$p_F)
plot(F_error, pch = 16, col = rangi2)
points(OF_error, pch = 16, col = col.alpha('black', 0.7))

for(i in 1:length(F_error)){
  lines(x=c(i,i),y=c(F_error[i],OF_error[i]) )
}
abline(v=45.5)  

mean(OF_error)


##In Sample prediction
inSampF <- sim(fit, data = data.frame(N= d$Ni, tank = 1:nrow(d)))
inSampF <- apply(inSampF,2,mean)
plot(d$Si, col = rangi2, pch = 16)
points(inSampF, pch = 16, col=col.alpha('black',0.5))
mtext('In Sample Predictions')
abline(v=30.5)
abline(v=60.5)
abline(v=90.5)
text(4.25,34,'small tanks')
text(14.25,34,'medium tanks')
text(23,34,'large tanks')

lines(x= 1:9, y=rep(mean(d$surv[1:9]),9), col = rangi2, lwd = 2)
lines(x= 1:9, y=rep(mean(inSampF[1:9]),9), lwd = 2, lty=2)
lines(x= 10:18, y=rep(mean(d$surv[10:18]),9), col = rangi2, lwd = 2)
lines(x= 10:18, y=rep(mean(inSampF[10:18]),9), lwd = 2, lty=2)
lines(x= 19:27, y=rep(mean(d$surv[19:27]),9), col = rangi2, lwd = 2)
lines(x= 19:27, y=rep(mean(inSampF[19:27]),9), lwd = 2, lty=2)

##Out of sample prediction
outSampF <- sim(fit, data = data.frame(N= d1$density, tank = 1:nrow(d1)))
outSampF <- apply(outSampF,2,mean)
plot(d1$surv, col = rangi2, pch = 16, 
     ylim = c(min(min(d$surv), min(outSampF)),max(max(d$surv), max(outSampF))))
points(outSampF, pch = 16)
mtext('Out of Sample Predictions')
abline(v=5.5)
abline(v=8.5)
text(3,34,'small tanks')
text(7,34,'medium tanks')
text(11,34,'large tanks')
lines(x= 1:5, y=rep(mean(d1$surv[1:5]),5), col = rangi2, lwd = 2)
lines(x= 1:5, y=rep(mean(outSampF[1:5]),5), lwd = 2, lty=2)

lines(x= 6:8, y=rep(mean(d1$surv[6:8]),3), col = rangi2, lwd = 2)
lines(x= 6:8, y=rep(mean(outSampF[6:8]),3), lwd = 2, lty=2)

lines(x= 9:12, y=rep(mean(d1$surv[9:12]),4), col = rangi2, lwd = 2)
lines(x= 9:12, y=rep(mean(outSampF[9:12]),4), lwd = 2, lty=2)



underfit <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a,
    a ~ dnorm(0, 1.5)
  ), data = dat, chains = 4, log_lik = T
)

##In Sample prediction
inSampUF <- sim(underfit, data = data.frame(N= d$density, tank = 1:nrow(d)))
inSampUF <- apply(inSampUF,2,mean)
plot(d$surv, col = rangi2, pch = 16)
points(inSampUF, pch = 16, col=col.alpha('black',0.5))
mtext('In Sample Predictions')
abline(v=8.5)
abline(v=18.5)
text(4.25,34,'small tanks')
text(14.25,34,'medium tanks')
text(23,34,'large tanks')

lines(x= 1:8, y=rep(mean(d$surv[1:8]),8), col = rangi2, lwd = 2)
lines(x= 1:8, y=rep(mean(inSampUF[1:8]),8), lwd = 2, lty=2)
lines(x= 9:18, y=rep(mean(d$surv[10:18]),10), col = rangi2, lwd = 2)
lines(x= 9:18, y=rep(mean(inSampUF[10:18]),10), lwd = 2, lty=2)
lines(x= 19:27, y=rep(mean(d$surv[19:27]),9), col = rangi2, lwd = 2)
lines(x= 19:27, y=rep(mean(inSampUF[19:27]),9), lwd = 2, lty=2)

##Out of sample prediction
outSampUF <- sim(underfit, data = data.frame(N= d1$density, tank = 1:nrow(d1)))
outSampUF <- apply(outSampUF,2,mean)
plot(d1$surv, col = rangi2, pch = 16, 
     ylim = c(min(min(d$surv), min(outSampF)),max(max(d$surv), max(outSampF))))
points(outSampUF, pch = 16)
mtext('Out of Sample Predictions')
abline(v=5.5)
abline(v=8.5)
text(3,34,'small tanks')
text(7,34,'medium tanks')
text(11,34,'large tanks')
lines(x= 1:5, y=rep(mean(d1$surv[1:5]),5), col = rangi2, lwd = 2)
lines(x= 1:5, y=rep(mean(outSampUF[1:5]),5), lwd = 2, lty=2)

lines(x= 6:8, y=rep(mean(d1$surv[6:8]),3), col = rangi2, lwd = 2)
lines(x= 6:8, y=rep(mean(outSampUF[6:8]),3), lwd = 2, lty=2)

lines(x= 9:12, y=rep(mean(d1$surv[9:12]),4), col = rangi2, lwd = 2)
lines(x= 9:12, y=rep(mean(outSampUF[9:12]),4), lwd = 2, lty=2)

par(mfrow = c(1,3))
##OutofSample Comparison
outSampOF <- sim(overfit, data = data.frame(N= d1$density, tank = 1:nrow(d1)))
outSampOF <- apply(outSampOF,2,mean)
plot(d1$surv, col = rangi2, pch = 16, 
     ylim = c(min(min(d$surv), min(outSampOF)),max(max(d$surv), max(outSampOF))))
points(outSampOF, pch = 16)
mtext('Out of Sample Predictions')
abline(v=5.5)
abline(v=8.5)
text(3,34,'small tanks')
text(7,34,'medium tanks')
text(11,34,'large tanks')
lines(x= 1:5, y=rep(mean(d1$surv[1:5]),5), col = rangi2, lwd = 2)
lines(x= 1:5, y=rep(mean(outSampOF[1:5]),5), lwd = 2, lty=2)
lines(x= 6:8, y=rep(mean(d1$surv[6:8]),3), col = rangi2, lwd = 2)
lines(x= 6:8, y=rep(mean(outSampOF[6:8]),3), lwd = 2, lty=2)
lines(x= 9:12, y=rep(mean(d1$surv[9:12]),4), col = rangi2, lwd = 2)
lines(x= 9:12, y=rep(mean(outSampOF[9:12]),4), lwd = 2, lty=2)
outSampF <- sim(fit, data = data.frame(N= d1$density, tank = 1:nrow(d1)))
outSampF <- apply(outSampF,2,mean)
plot(d1$surv, col = rangi2, pch = 16, 
     ylim = c(min(min(d$surv), min(outSampF)),max(max(d$surv), max(outSampF))))
points(outSampF, pch = 16)
mtext('Out of Sample Predictions')
abline(v=5.5)
abline(v=8.5)
text(3,34,'small tanks')
text(7,34,'medium tanks')
text(11,34,'large tanks')
lines(x= 1:5, y=rep(mean(d1$surv[1:5]),5), col = rangi2, lwd = 2)
lines(x= 1:5, y=rep(mean(outSampF[1:5]),5), lwd = 2, lty=2)
lines(x= 6:8, y=rep(mean(d1$surv[6:8]),3), col = rangi2, lwd = 2)
lines(x= 6:8, y=rep(mean(outSampF[6:8]),3), lwd = 2, lty=2)
lines(x= 9:12, y=rep(mean(d1$surv[9:12]),4), col = rangi2, lwd = 2)
lines(x= 9:12, y=rep(mean(outSampF[9:12]),4), lwd = 2, lty=2)
outSampUF <- sim(underfit, data = data.frame(N= d1$density, tank = 1:nrow(d1)))
outSampUF <- apply(outSampUF,2,mean)
plot(d1$surv, col = rangi2, pch = 16, 
     ylim = c(min(min(d$surv), min(outSampF)),max(max(d$surv), max(outSampF))))
points(outSampUF, pch = 16)
mtext('Out of Sample Predictions')
abline(v=5.5)
abline(v=8.5)
text(3,34,'small tanks')
text(7,34,'medium tanks')
text(11,34,'large tanks')
lines(x= 1:5, y=rep(mean(d1$surv[1:5]),5), col = rangi2, lwd = 2)
lines(x= 1:5, y=rep(mean(outSampUF[1:5]),5), lwd = 2, lty=2)
lines(x= 6:8, y=rep(mean(d1$surv[6:8]),3), col = rangi2, lwd = 2)
lines(x= 6:8, y=rep(mean(outSampUF[6:8]),3), lwd = 2, lty=2)
lines(x= 9:12, y=rep(mean(d1$surv[9:12]),4), col = rangi2, lwd = 2)
lines(x= 9:12, y=rep(mean(outSampUF[9:12]),4), lwd = 2, lty=2)






mean(abs(d1$surv - outSampOF))
mean(abs(d1$surv - outSampF))
mean(abs(d1$surv - outSampUF))

mean(abs(d$surv - inSampOF))
mean(abs(d$surv - inSampF))
mean(abs(d$surv - inSampUF))

compare(overfit, underfit, fit)

par(mfrow = c(1,1))
plot(d1$surv - outSampOF, pch = 16, ylim = c(-30,30))
points(d1$surv - outSampF, pch = 16, col = rangi2)
points(d1$surv - outSampUF, pch = 16, col = 'red')

