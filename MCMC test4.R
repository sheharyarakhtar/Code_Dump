# My current locations actual:
lambda <- array( -6.1185 + rnorm(1000,0,0.05))
phi <- array(53.2577 + rnorm(1000,0,0.05))

# Three landmarks I can see are A,B,C and their actual location is given by:

landmarks<-data.frame(lon=c(-6.337470,-6.06528, -6.38350),lat=c(53.372231, 53.38778 ,53.31725))
LM1<-c(landmarks[1,1],landmarks[1,2])
LM2<-c(landmarks[2,1],landmarks[2,2])
LM3<-c(landmarks[3,1],landmarks[3,2])

# Bearings from landmarks to my location:
alpha <- (360+atan((-6.3374704-lambda)/(53.372231-phi))*(180/pi%%360)) #Phoenix Park bearing
beta <- (360+atan((-6.06528-lambda)/(53.38778-phi))*(180/pi%%360)) #Howth bearing
gamma<- (360+atan((-6.38350-lambda)/(53.31725-phi))*(180/pi%%360)) #Clondalkin GAA Tower bearing


#Plotting this on graph
d <- seq(0,0.6,length.out=1000)
line1 <- data.frame(lon=landmarks[1,1] + d*sin(alpha*pi/180+pi),
                    lat=landmarks[1,2] + d*cos(alpha*pi/180+pi))
line2 <- data.frame(lon=landmarks[2,1] + d*sin(beta*pi/180+pi),
                    lat=landmarks[2,2] + d*cos(beta*pi/180+pi))
line3 <- data.frame(lon=landmarks[3,1] + d*sin(gamma*pi/180+pi),
                    lat=landmarks[3,2] + d*cos(gamma*pi/180+pi))

ggplot() +
  geom_point(aes(x = lon, y = lat), size = 1, data = landmarks, alpha = .5) +
  geom_line(aes(x=lon,y=lat),data=line1) +
  geom_line(aes(x=lon,y=lat),data=line2) +
  geom_line(aes(x=lon,y=lat),data=line3)


## Defining loglikelihood

# Currently we are saying that our true location is a bit hazy, 
# because we have an error N(0,0.05) in our bearings alpha beta and gamma






