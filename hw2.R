#Question 1#
masim <- function(thetas, sigmaSq, time){
  q <- length(thetas)
  noise <- rnorm(time+q, sd=sqrt(sigmaSq))
  x <- c(noise[1:q], rep(0,time))
  
  for (i in (q+1):(time+q)){
    x[i] <- thetas %*% noise[i-(1:q)] +noise[i]
  }
  x <- x[(q+1):(time+q)] 
  x 
}

#Question 2#
maProcess10k <- masim(thetas = c(0.5,2),sigmaSq = 1,time = 10000)
acf(maProcess10k)
#Question 3 and 3a#
ibm <- scan("dailyibm.dat", skip=1)
plot.ts(ibm)
acf(ibm)
#The time series does not appear to be stationary because it looks like the mean is not constant (aka dependent on time).
#The ACF indicates that the observations are highly correlated with each other and does not 
#depend on the lag between them, providing further justification that the series is not stationary.

#Question 3b#
diffIBM <- diff(ibm)
plot.ts(diffIBM)
acf(diffIBM)
#Ignoring a few outliers, the time series looks stationary now that we've differenced the data. The mean looks
#to be constant at zero and the ACF indicates that the autocovariance/autocorrelation depend only on the lag.

#Question 3c#
logPricesIBM <- log(ibm)
plot.ts(logPricesIBM)
acf(logPricesIBM)
#After taking the log of each closing price, the data still does not look stationary at all. The ACF does not appear
#to depend on the lag between observations and the mean is not constant.

#Question 3d#
#Taking the log of the difference is a bad idea because some of the difference are negative and taking the log of that
#will produce null values in our data, which is not useful.
logReturnsIBM <-  diff(log(ibm)) 
plot.ts(logReturnsIBM)
acf(logReturnsIBM)
#Ignoring a few outliers, this transformation succeeds in creating stationary data.

#Question 3e#
#2 panics occurred.
numPanics <- as.numeric(sum(logReturnsIBM < -0.1))

#Question 3f#
difflogibm1 <- logReturnsIBM[1:500]
difflogibm2 <- logReturnsIBM[501:length(logReturnsIBM)]

plot.ts(difflogibm1)
plot.ts(difflogibm2)
acf(difflogibm1)
acf(difflogibm2)



#Question 3g
#The model form and Gaussian white noise assumptions are reasonable based on how we have transformed the data
#and the time series plots of the data. 
deltaEst <- mean(difflogibm2)
sigmaWEst <- var(difflogibm2)
#In this case, delta can be interpreted as the average log return. It's good that delta is positive because ir means
#money is being made on the stock market instead of lost.