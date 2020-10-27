#Question 1, setting up the matrices to get estimates
library(astsa)
gamma0 <- 1382.2
rho0 <- 1
rho1 <- 1114.4/gamma0
rho2  <- 591.73/gamma0
rho3 <- 96.216/gamma0
rho_hat <- (matrix(data = c(rho1,rho2),nrow = 2,ncol = 1))
autocorr <- c(rho0,rho1,
              rho1, rho0)
              

cap_R_hat <- (matrix(data = autocorr ,ncol = 2,nrow = 2))

inv_cap_R <- solve(cap_R_hat)
phis <- (inv_cap_R) %*% (rho_hat)
phi1Est <- phis[1]
phi2Est <- phis[2]
gamma0*(1-(t(phis)%*%rho_hat))
sigSquaredEst <- 289.1791

#Calculating variance for phis now
variances <- (sigSquaredEst/(100*gamma0)) * inv_cap_R
diag(variances)

phi1Interval <- c(phi1Est - 2*sqrt(diag(variances)[1]),  phi1Est + 2*sqrt(diag(variances)[1]))
phi2Interval <- c(phi2Est - 2*sqrt(diag(variances)[2]),  phi2Est + 2*sqrt(diag(variances)[2]))


sarima(bicoal,2,1,1)
