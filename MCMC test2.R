require(osmar)
require(osmdata)
require(geosphere)
require(ggmap)
require(mvtnorm)
require(coda)
library(Rcpp)
#library(units)
#library(sf)



#------------------My locations--------------------------------------

#killiney hill (53.2577, -6.1185 ) - my location
lambda<--6.1185
phi <- 53.2577


#phoenix park (-6.337470,53.372231)
#Howth(-6.06528,53.38778)
#Clondalkin GAA Tower (53.3172553976,53.3172553976 -6.38350346598)

landmarks<-data.frame(lon=c(-6.337470,-6.06528, -6.38350),lat=c(53.372231, 53.38778 ,53.31725))
LM1<-c(landmarks[1,1],landmarks[1,2])
LM2<-c(landmarks[2,1],landmarks[2,2])
LM3<-c(landmarks[3,1],landmarks[3,2])

#------------------Calculating bearings and plotting--------------------------------------


alpha <- (360+atan((-6.3374704-(-6.12))/(53.372231-53.26))*(180/pi%%360)) #Phoenix Park bearing
beta <- (360+atan((-6.06528-(-6.12))/(53.38778-53.26))*(180/pi%%360)) #Howth bearing
gamma<- (360+atan((-6.38350-(-6.12))/(53.31725-53.26))*(180/pi%%360)) #Clondalkin GAA Tower bearing



d <- seq(0,0.6,0.0001) # Length of the line
line1 <- data.frame(lon=landmarks[1,1] + d*sin(alpha*pi/180+pi),
                    lat=landmarks[1,2] + d*cos(alpha*pi/180+pi))
line2 <- data.frame(lon=landmarks[2,1] + d*sin(beta*pi/180+pi),
                    lat=landmarks[2,2] + d*cos(beta*pi/180+pi))
line3 <- data.frame(lon=landmarks[3,1] + d*sin(gamma*pi/180+pi),
                    lat=landmarks[3,2] + d*cos(gamma*pi/180+pi))

# register_google(key='IzaSyD0FVlGWPXnitj79tPO9hIhcRS',write=TRUE)
# 
# map <- get_map(c(-6.12,53.26),zoom=101,maptype="watercolor") #Use coordinates from where you are

mapPlot <- ggplot() +
  geom_point(aes(x = lon, y = lat), size = 1, data = landmarks, alpha = .5) +
  geom_line(aes(x=lon,y=lat),data=line1) +
  geom_line(aes(x=lon,y=lat),data=line2) +
  geom_line(aes(x=lon,y=lat),data=line3)

mapPlot


#------------------Defining likelihood--------------------------------------


loglikelihood <- function(lambda, phi, sigma){
  
  bearing1 <- (360 +atan((???6.337470 - lambda) / (53.372231 - phi)) * (180 / pi)) %% 360
  bearing2 <- (360 +atan((???6.06528 - lambda) / (53.38778 - phi)) * (180 / pi)) %% 360
  bearing3 <- (360 +atan((???6.38350346598 - lambda) / (53.3172553976 - phi)) * (180 / pi)) %% 360
  
  lh <-   dnorm(alpha,bearing1,sd = sigma, log=T) *
    dnorm(beta,bearing2,sd = sigma, log=T) *
    dnorm(gamma,bearing3,sd = sigma, log=T)
  return(lh)
}

lh <- loglikelihood(lambda, phi,0.5)#testing

#How to design this prior? The conjudate prior for normal should be normal or do I use the proposed 
#Like in the project description, use U(-7,-6) for lambda, U(53,54) for phi, Exponential(20) for sigma
prior <-function(lambda, phi, sigma){
  dunif(lambda,-7,-6)* #should i use log here as well since I've used for likelihood?
    dunif(phi,53,54)*
    dexp(20) 
}

#Sample using intersection to make convergion easier
intersectBearings <- function(p1,b1,p2,b2) {
  x1 <- p1[1]
  x2 <- p1[1] + 0.1*sin(b1*pi/180)
  x3 <- p2[1]
  x4 <- p2[1] + 0.1*sin(b2*pi/180)
  y1 <- p1[2]
  y2 <- p1[2] + 0.1*cos(b1*pi/180)
  y3 <- p2[2]
  y4 <- p2[2] + 0.1*cos(b2*pi/180)
  x <- ((x1*y2-y1*x2)*(x3-x4)-(x1-x2)*(x3*y4-y3*x4))/((x1-x2)*(y3-y4)-(y1-y2)*(x3-x4))
  y <- ((x1*y2-y1*x2)*(y3-y4)-(y1-y2)*(x3*y4-y3*x4))/((x1-x2)*(y3-y4)-(y1-y2)*(x3-x4))
  return(as.numeric(c(x,y)))
}

#Sample from intersection 1
intersection <- intersectBearings(landmarks[1,],alpha,landmarks[2,],beta)

draws <- array(0,dim=c(4000,3,3))
draws[4000,1,] <- runif(3,intersection[1] - 0.01, intersection[1] + 0.01)
draws[4000,2,] <- runif(3,intersection[2] - 0.01, intersection[2] + 0.01)
draws[4000,3,] <- rexp(3,20)

#how to decide on this matrix?
prop.cov <- c(1e-8,1e-8,1e-4)*diag(3)



converged <- FALSE
while (!converged) {
  draws[1,,] <- draws[4000,,]
  accepted <- 1
  for (step in 2:4000) {
    for (chain in 1:3) {
      proposed <- rmvnorm(1,draws[step-1,,chain],prop.cov)
      r <- loglikelihood(sparrows,proposed)+
        logprior(proposed)-
        loglikelihood(sparrows,draws[step-1,,chain])-
        logprior(draws[step-1,,chain])
      alpha <- min(0,r)
      u <- runif(1)
      if (log(u) < alpha) {
        draws[step,,chain] <- proposed
      } else {
        draws[step,,chain] <- draws[step-1,,chain]
      }
    }
  }   
  print(sprintf("Acceptance rate: %f",accepted/120))
  chainlist <- mcmc.list(Chain1=mcmc(draws[,,1]),
                         Chain2=mcmc(draws[,,2]),
                         Chain3=mcmc(draws[,,3]))
  converged <- all((gelman.diag(chainlist)$psrf[,2])<1.2)
  plot(chainlist) # This plots current state of the chains
  Sys.sleep(0.1)
}

#Should it be done for the other 2 intersections?