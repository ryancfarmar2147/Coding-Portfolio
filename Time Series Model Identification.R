library(astsa)
#Question 1
phi1 <- c(1,-8/3,-1)
theta1 <- c(1,7/6,1/3)
Mod(polyroot(phi1))
Mod(polyroot(theta1))
mod1HasRedundantParams <- FALSE
mod1IsCausal <- FALSE
mod1IsInvertible <- TRUE
modOneOrders <- c(2,2)
mod1PsiWeights <- NULL
ARMAtoAR(-phi1[-1],theta1[-1],lag.max = 5)
mod1PiWeights <- c(1,-3.833,3.139,-2.384,1.735)

#Question 2
phi2 <- c(1,-2/3)
theta2 <- c(1,5/2,1)
Mod(polyroot(phi2))
Mod(polyroot(theta2))
mod2HasRedundantParams <- FALSE
mod2IsCausal <- TRUE
mod2IsInvertible <- FALSE
modTwoOrders <- c(1,2)
ARMAtoMA(-phi2[-1], theta2[-1], lag.max=5)
mod2PsiWeights <- c(1,3.166667,3.111111,2.0740741,1.382716)
mod2PiWeights <- NULL

#Question 3
phi3 <- c(1,-9/4,-9/4)
theta3 <- c(1,-3,1/9,-1/3)
Mod(polyroot(phi3))
Mod(polyroot(theta3))
mod3HasRedundantParams <- TRUE
mod3IsCausal <- TRUE
mod3IsInvertible <- TRUE
modThreeOrders <- c(1,2)
ARMAtoMA(-phi3[-1],theta3[-1],lag.max = 5)
mod3PsiWeights <- c(1,-0.75,0.6736111,-0.5052083,0.3789063)
mod3PiWeights <- c(1,0.75,-0.111,-0.083,0.012)
