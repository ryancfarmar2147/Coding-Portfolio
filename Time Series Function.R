
randomwalk <- function(sigsq,time){
  x <- rep(0,time)
  w <- rnorm(time, sd=sqrt(sigsq))
  for( i in 2:time ){
    x[i] <- x[i-1] + w[i]
  }
  x
}

#Autoregressive process#
arsim <- function(phis, sigsq, time){
  p <- length(phis) #find the order of our AR
  noise <- rnorm(time+p, sd=sqrt(sigsq))
  # generate the white noise plus a few to get started
  x <- c(noise[1:p], rep(0,time))
  #put the initial noise terms in and set the rest to zero
  for (i in (p+1):(time+p)){
    x[i] <- phis %*% x[i-(1:p)] +noise[i]
  }
  x <- x[(p+1):(time+p)] #throw away those initial starting points
  x #return the time series
}

#Moving average data generation#
masim<-function(thetas, sigsq, t){
  q<-length(thetas)
  noise<-rnorm(t+q, sd=sqrt(sigsq))
  x<-c(noise[1:q],rep(0,t))
  for (i in (q+1):(t+q)){
    x[i]<-thetas %*% noise[i-(1:q)] +noise[i]
  }
  x<-x[(q+1):(t+q)]
  x
}

#Actual Model Formulation based on previous noise and data generation#
get_x <- function(A,phi,w,t,sigsq){
  x <- array(0,t)
  time <- c(seq(1,t))
  noise <-rnorm(t,0,sd=sigsq)
  for(i in 1:length(time)){
    x[i] <- A*cos((2*pi*w*time[i])+ phi) + noise[i]
  }
  x
}

 
