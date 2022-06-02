library(rethinking)
library(dplyr)
winner <- c()
for(i in 1:1000){
sim <- data.frame(id = 1:12)
sim$start = c(0,0,0,0,0,0,0,0,0,0,0,1)
sim$win <- as.numeric(!sim$start)
for(i in 1:11){
  sim_copy <- sim
  flip <- runif(1)
  if(flip > 0.5){
  if(sim[sim$start==1,1]==max(sim$id)){
    sim$start <- 0
    sim$start[sim$id==min(sim$id)] <- 1
  }else {sim$start = coalesce(lag(sim$start),0)
  }
} else {
   if(sim[sim$start==1,1]==min(sim$id)){
    sim$start <- 0
    sim$start[sim$id==max(sim$id)] <- 1
   }else {sim$start = coalesce(lead(sim$start),0)
  }
}
  sim <- sim[!(sim_copy$win==0),]
  sim$win <- as.numeric(!sim$start) 
}
winner <- append(winner, sim[sim$win==0,1])}
par(mfrow= c(1,2))
simplehist(winner)


winner <- c()
flips <- c()
for(i in 1:1000){
sim <- data.frame(id = 1:12)
sim$start = c(0,0,0,0,0,0,0,0,0,0,0,1)
sim$win <- as.numeric(!sim$start)
while(sum(sim$win)>1){
flip <- runif(1)
flips <- append(flips, flip)
if(flip > 0.5){
  if(sim[sim$start==1,1]==max(sim$id)){
    sim$start <- 0
    sim$start[sim$id==min(sim$id)] <- 1
  }else {sim$start = coalesce(lag(sim$start),0)
  }
} else {
  if(sim[sim$start==1,1]==min(sim$id)){
    sim$start <- 0
    sim$start[sim$id==max(sim$id)] <- 1
  }else {sim$start = coalesce(lead(sim$start),0)
  }
  
}
sim$win[sim$start==1]<- 0
# print(sim)
}
winner <- append(winner, sim[sim$win==1,1])
# print(i)
}
# simplehist(winner)
# par(mfrow =c(1,2))
simplehist(winner)
dens(flips)
