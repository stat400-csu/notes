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
var_sqrt_b1 <- 6*(n - 2)/((n + 1)*(n + 3)) # adjusted variance for skewness test
crit_val <- qnorm(1 - alpha/2, 0, sqrt(var_sqrt_b1)) #crit value for the test
empirical_pwr <- rep(NA, length(epsilon)) #storage
empirical_se <- rep(NA, length(epsilon)) #storage

# estimate power for each value of epsilon
for(j in 1:length(epsilon)) {
  # perform MC to estimate empirical power
  tests <- rep(NA, m)
  for(i in 1:m) {
    x <- r_noisy_normal(n, epsilon[j])
    t_star <- skew(x)
    tests[i] <- abs(t_star) > crit_val
  }
  empirical_pwr[j] <- mean(tests)
  
  ## store empirical se 
  empirical_se[j] <- sqrt(empirical_pwr[j]*(1 - empirical_pwr[j])/n)
}

## plot results -- 
## x axis = epsilon values
## y axis = empirical power
## use lines + add band of estimate +/- se

data.frame(epsilon = epsilon, 
           power = empirical_pwr,
           se = empirical_se) %>%
  ggplot() +
  geom_ribbon(aes(x = epsilon, ymin = power - se, ymax = power + se), alpha = .5, fill = "red") +
  geom_line(aes(x = epsilon, y = power)) +
  geom_hline(aes(yintercept = 0.8))

epsilon[which(empirical_pwr - empirical_se >= 0.8)]

# n = 10
n <- 10
var_sqrt_b1 <- 6*(n - 2)/((n + 1)*(n + 3)) # adjusted variance for skewness test
crit_val <- qnorm(1 - alpha/2, 0, sqrt(var_sqrt_b1)) #crit value for the test
empirical_pwr_10 <- rep(NA, length(epsilon)) #storage
empirical_se_10 <- rep(NA, length(epsilon)) #storage

# estimate power for each value of epsilon
for(j in 1:length(epsilon)) {
  # perform MC to estimate empirical power
  tests <- rep(NA, m)
  for(i in 1:m) {
    x <- r_noisy_normal(n, epsilon[j])
    t_star <- skew(x)
    tests[i] <- abs(t_star) > crit_val
  }
  empirical_pwr_10[j] <- mean(tests)
  
  ## store empirical se 
  empirical_se_10[j] <- sqrt(empirical_pwr_10[j]*(1 - empirical_pwr_10[j])/n)
}

data.frame(epsilon = epsilon, 
           power = empirical_pwr,
           se = empirical_se,
           power_10 = empirical_pwr_10,
           se_10 = empirical_se_10) %>%
  ggplot() +
  geom_ribbon(aes(x = epsilon, ymin = power - se, ymax = power + se), alpha = .5, fill = "red") +
  geom_line(aes(x = epsilon, y = power)) +
  geom_ribbon(aes(x = epsilon, ymin = power_10 - se_10, ymax = power_10 + se_10), alpha = .5, fill = "blue") +
  geom_line(aes(x = epsilon, y = power_10)) +
  geom_hline(aes(yintercept = 0.8))
