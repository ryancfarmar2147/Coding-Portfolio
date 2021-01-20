
#########################################################################
#                                                                       #
#  Simulations                                                    #
#                                                                       #
#########################################################################

library(ggplot2)

set.seed(5)
rnorm(10, mean=2.5, sd=0.5)
#set the seed each time you open r and then don't touch it#
#############################
#  Monte Carlo simulations  #
#############################

#  Monte Carlo simulation is the process of using repeated sampling     #
#  to determine some behavior or characteristic. This type of           #
#  simulation can be used in many different ways.                       #
#                                                                       #
#  Many applications of Monte Carlo sampling will use the population    #
#  or an appropriate pseudo-population to generate an estimated         #
#  sampling distribution of a relevant statistic that can then be       #
#  explored. One example in this category is to demonstrate and         #
#  understand the Central Limit Theorem.                                #
#                                                                       #
#  Suppose that we have a population from which we plan to take a       #
#  random sample and use the Central Limit Theorem to test the mean.    #
#  The distribution of our population is unknown, but we think that it  #
#  is similar to the chi-squared distribution with four degrees of      #
#  freedom. One important question is how large of a random sample is   #
#  needed from the population for the Central Limit Theorem to hold.    #
#  This question can be answered using a Monte Carlo simulation that    #
#  estimates the sampling distribution of the mean at various sample    #
#  sizes for evaluation of approximate normality.                       #


#Monte Carlo assumes that you have the population or something really close to it#

## First plot the chi-squared population distribution

library(ggplot2)
Xdata2 <- data.frame(X=seq(from = 0,to = 20,by=1))
ggplot(Xdata2, aes(x=X)) + stat_function(fun=dchisq, args=list(df=4),color = 'blue')

## Determine the number of repeated samples to draw and parameter values
K <- 10000
a <- 4

## Draw 10,000 samples of size 5 from the population distribution
samps <- replicate(K, rchisq(5,a))
samps[,1:10]

## Determine the sample mean from each random sample
means5 <- apply(samps,2,mean)

## Create a QQ plot of the resulting sample means
mean_data <- data.frame(X=means5)
ggplot(mean_data, aes(sample=X)) + stat_qq() + stat_qq_line() + labs(title="n=5")

#  If we want to repeat this process with other sample sizes, we can    #
#  write a function instead of writing the same code several times.     #

## Repeat this process with sample sizes of 15, 30, 45, 60, and 75
sizes <- c(15,30,45,60,75)
samp.means <- lapply(sizes, function(size) replicate(K, mean(rchisq(size,a))))

qq.means <- function(x){
  mean_data <- data.frame(X=samp.means[[which(sizes==x)]])
  qqmean <-ggplot(mean_data, aes(sample=X)) + stat_qq() + stat_qq_line() + 
    labs(title=paste("n=",x,sep=""))
  print(qqmean)
}

lapply(sizes, qq.means)

#  Once we have determined which sample size yields a sampling          #
#  distribution that is sufficiently approximately normal, we can       #
#  verify the parameters of the sampling distribution. The population   #
#  mean and variance of chi-squared distributions are the df and 2*df,  #
#  respectively.                                                        #

norm.size <- 45
means <- samp.means[[which(sizes==norm.size)]]

## Estimate the mean of the sampling distribution (which is 4)
mean(means)

## Estimate the standard devation of the sampling distribution
true_sd <- sqrt(2*4/norm.size)
true_sd
sd(means)


#t-test example#
mu <- 16.8
sigma <- 5.4
alpha <- 0.05
K <- 10000
sizes <- c(9,31,44)


do.test <- function(x){
  samp <- rnorm(x, mean = mu, sd=sigma)
  ts <- ((mean(samp)-mu)/(sd(samp)/sqrt(x)))
  p.val <- pnorm(ts)
  p.val <= alpha
}

rep.test <- function(x){
  res <- replicate(K, do.test(x))
  sum(res)/K
}

sapply(sizes, rep.test)


###################
#  Bootstrapping  #
###################

#  Bootstrapping uses similar ideas as Monte Carlo simulations, but     #
#  instead of drawing samples from a given or pseudo- population,       #
#  repeated samples are taken from the available sampled data.          #
#  Bootstrapping uses with replacement sampling to create several       #
#  simulated samples that are the same size as the original sampled     #
#  data.                                                                #

#  Like with Monte Carlo simulations, there are many applications of    #
#  bootstrapping that evaluate the estimated sampling distribution      #
#  of the parameter of interest determined from the bootstrap samples.  #
#                                                                       #
#  It is important to know that there are assumptions that need to      #
#  hold for bootstrapping to work. One of these assumptions is that     #
#  the sampled data represent an independent, representative sample     #
#  from the population. Also, bootstrapping will not work as expected   #
#  if the underlying population has very heavy tails.                   #
#                                                                       #
#  Suppose that for budget purposes we are only able to draw a sample   #
#  of 7 from the unknown population distribution. In this case, the     #
#  sample size is too small to be able to reasonably use the Central    #
#  Limit Theorem. Thus, the sampling distribution is unknown. In this   #
#  case, bootstrapping can be used to estimate the variation in the     #
#  sampling distribution.                                               #

## Draw the sample of 7 from the unknown distribution
samp_data <- c(1.77, 10.48, 4.30, 0.96, 2.67, 2.76, 2.02)

## Plot the sample
samp_df <- data.frame(samp_data)
ggplot(samp_df, aes(x=samp_data)) + geom_histogram(binwidth=1.5,fill="white",color="black")

## Find the sample mean
samp_mean <- mean(samp_data)

## Determine the number of bootstrap samples
B<-10000

## Draw the bootstrap samples
boot_samp <- replicate(B, sample(samp_data, replace=T))
boot_samp[,1:5]

## Determine the sample mean from each bootstrap sample
boot_means <- apply(boot_samp,2,mean)

## Plot the sampling distribution
means_df <- data.frame(boot_means)
ggplot(means_df, aes(x=boot_means)) + geom_histogram(binwidth=1.5,fill="white",color="black")

## Estimate the mean and standard deviation of the sampling distribution
mean(boot_means)
sd(boot_means)

## Determine the 95% bootstrap confidence interval of the sample mean
boot_err <- boot_means - samp_mean
boot_err_sort <- sort(boot_err)
p2.5 <- B*0.025
p97.5 <- B*0.975
boot_ci <- samp_mean - boot_err_sort[c(p97.5,p2.5)]
boot_ci

#  Bootstrapping can also be used to estimate the variability of        #
#  parameters whose sampling distribution is difficult or impossible    #
#  to determine theoretically.                                          #

## Determine the median of the sampled data
samp_med <- median(samp_data)

## Determine the sample median from each bootstrap sample
boot_meds <- apply(boot_samp,2,median)

## Determine the 95% bootstrap confidence interval
boot_meds_err <- boot_meds - samp_med
boot_meds_err_sort <- sort(boot_meds_err)
boot_med_ci <- samp_med - boot_meds_err_sort[c(p97.5,p2.5)]
boot_med_ci

#  Note that the bootstrap confidence intervals are not symmetric       #
#  around the point estimate. In certain cases, the standard deviation  # 
#  of the bootstrapped estimates can be used in place of the standard   #
#  error in the usual normal confidence interval equation if you are    #
#  confident that approximate normality holds.                          #

#  Suppose that we would like to use the sample of 7 observations       #
#  from the unknown population to test if the mean of the unknown       #
#  population is greater than 3. In this case, bootstrapping can be     #
#  used to estimate the sampling distribution of the test statistic     #
#  under the null hypothesis.                                           #

## Determine the test statistic 
samp_mean 

## Determine the sampling distribution of the test statistic under the null hypothesis
mu0 <- 3
boot_means_null <- boot_means - mean(boot_means) + mu0

## Plot the null hypothesis sampling distribution
means_df <- data.frame(boot_means_null)
ggplot(means_df, aes(x=boot_means_null)) + geom_histogram(binwidth=1.5,fill="white",color="black")

## Determine the p-value
dif <- samp_mean - mu0
sum(boot_means_null >= samp_mean)/B

#########################################################################

rchisq(7,df=4)
chi_samp <- c(1.149138 ,5.243066,2.667651, 3.833417, 5.914348 ,2.060082, 3.319064)
chi_df <- data.frame(chi_samp)
samp_mean <- mean(chi_samp)

B <- 10000
bootstrap <- replicate(B, sample(chi_samp, replace = T))
bootstrap[,1:5]

boot_means <- apply(bootstrap, 2, mean)
means_df <- data.frame(boot_means)

boot_err <- boot_means - samp_mean
boot_err_sort <- sort(boot_err)
p2.5 <- B*0.025
p97.5 <- B*0.975
boot_ci <- samp_mean - boot_err_sort[c(p97.5,p2.5)]
boot_ci
boot_err_sort[B*0.025]
#########################################################################

samps <- lapply(1:100, function(x) rchisq(7,4))

do.boot <- function(x){
samp_data <- rchisq(7,df=4)
samp_mean2 <- mean(samp_data)
boot_samp <- replicate(B, sample(samp_data,replace=T))
boot_means2 <- apply(boot_samp, 2, mean)
means_df2 <- data.frame(boot_means2)
boot_err2 <- boot_means2 - samp_mean2
boot_err_sort2 <- sort(boot_err2)
p2.5 <- B*0.025
p97.5 <- B*0.975
boot_ci2 <- samp_mean2 - boot_err_sort2[c(p97.5,p2.5)]
boot_ci2[1] <= 4 & boot_ci2[2] >= 4
}

res <- sapply(samps,do.boot)
res
sum(res)/100
table(res)




########################################################################

Xdata2 <- data.frame(X=c(0,20))
ggplot(Xdata2, aes(x=X)) + stat_function(fun=dchisq, args=list(df=2))

## Determine the number of repeated samples to draw and parameter values
N <- 10000
a <- 2

## Draw 10,000 samples of size 9 from the population distribution
samps <- replicate(N, rchisq(44,a))
samps[,1:10]
means_mat <- apply(samps,2,mean)
means_df <- data.frame(X=means_mat)
ggplot(means_df, aes(sample=X)) + geom_histogram(aes(x=X),binwidth=0.05)



ex_test <- replicate(N,rchisq(9,df=2))
ex_test_means <- apply(ex_test,2,mean)
mean(ex_test_means)
sd(ex_test_means)




sort_df <- data.frame(sort_p95)
ggplot(sort_df, aes(x=sort_p95)) + geom_histogram(binwidth=1.5)

mc_samp <- sample(size = 13,data1_df$V1)
mc_samp2 <- sample(size = 13,data1_df$V2)
t_test <- t.test(mc_samp, mc_samp2, paired=TRUE, mu=0, alternative = "two.sided")
t_test
