library(tidyverse)
set.seed(400)

## Your turn
r_noisy_normal <- function(n, epsilon) {
  z <- rbinom(n, 1, 1 - epsilon)
  z*rnorm(n, 0, 1) + (1 - z)*rnorm(n, 0, 10)
}


## ------------------------------------------------------------------------
# skewness statistic function
skew <- function(x) {
  xbar <- mean(x)
  num <- mean((x - xbar)^3)
  denom <- mean((x - xbar)^2)
  num/denom^1.5
}

# setup for MC
alpha <- .1
n <- 100
m <- 100
epsilon <- seq(0, 1, length.out = 200)

est_power <- function(eps, alpha, n, m) {
  var_sqrt_b1 <- 6*(n - 2)/((n + 1)*(n + 3)) # adjusted variance for skewness test
  crit_val <- qnorm(1 - alpha/2, 0, 1) #crit value for the test
  
  indic <- rep(NA, m) ## storage vector
  for(j in 1:m) {
    ## generate data
    x <- r_noisy_normal(n, eps)
    
    ## compute statistic
    t_star <- abs(skew(x) / sqrt(var_sqrt_b1))
    
    ## perform hypothesis test
    indic[j] <- (t_star > crit_val)
  }
  return(mean(indic))
}

empirical_pwr <- rep(NA, length(epsilon)) #storage

# estimate power for each value of epsilon
for(j in 1:length(epsilon)) {
  # perform MC to estimate empirical power
  ## Your turn
  empirical_pwr[j] <- est_power(epsilon[j], alpha, n, m)
}

## store empirical se 
empirical_se <- sqrt(empirical_pwr * (1 - empirical_pwr) / m)

## plot results -- 
## x axis = epsilon values
## y axis = empirical power
## use lines + add band of estimate +/- se

ggplot() +
  geom_hline(aes(yintercept = 0.8), lty = 2, colour = "red") +
  geom_ribbon(aes(x = epsilon, ymin = empirical_pwr - empirical_se, ymax = empirical_pwr + empirical_se), fill = "grey40", alpha = 0.5) +
  geom_line(aes(x = epsilon, y = empirical_pwr))

epsilon[empirical_pwr - empirical_se > .8]

## n = 10
empirical_pwr <- rep(NA, length(epsilon)) #storage

# estimate power for each value of epsilon
for(j in 1:length(epsilon)) {
  # perform MC to estimate empirical power
  ## Your turn
  empirical_pwr[j] <- est_power(epsilon[j], alpha, 10, m)
}

## store empirical se 
empirical_se <- sqrt(empirical_pwr * (1 - empirical_pwr) / m)

## plot results -- 
## x axis = epsilon values
## y axis = empirical power
## use lines + add band of estimate +/- se
ggplot() +
  geom_hline(aes(yintercept = 0.8), lty = 2, colour = "red") +
  geom_ribbon(aes(x = epsilon, ymin = empirical_pwr - empirical_se, ymax = empirical_pwr + empirical_se), fill = "grey40", alpha = 0.5) +
  geom_line(aes(x = epsilon, y = empirical_pwr))

epsilon[empirical_pwr - empirical_se > .8]
