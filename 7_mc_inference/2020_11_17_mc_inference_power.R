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

# estimate power for each value of epsilon
for(i in 1:length(epsilon)) {
  # perform MC to estimate empirical power
  y <- rep(NA, m)
  for(j in seq_len(m)) {
    x <- r_noisy_normal(n, epsilon[i])
    t_star <- skew(x)
    y[j] <- abs(t_star) > crit_val
  }
  empirical_pwr[i] <- mean(y)
}

## store empirical se 
empirical_se <- sqrt(empirical_pwr * (1 - empirical_pwr)/n)

## plot results -- 
## x axis = epsilon values
## y axis = empirical power
## use lines + add band of estimate +/- se
ggplot() +
  geom_ribbon(aes(x = epsilon, ymin = empirical_pwr - empirical_se, ymax = empirical_pwr + empirical_se), alpha = 0.5) +
  geom_line(aes(epsilon, empirical_pwr)) +
  geom_hline(aes(yintercept = 0.8), lty = 2)

range(epsilon[empirical_pwr - empirical_se > 0.8])

## n = 10
n <- 10
empirical_pwr_10 <- rep(NA, length(epsilon)) #storage

# estimate power for each value of epsilon
for(i in 1:length(epsilon)) {
  # perform MC to estimate empirical power
  y <- rep(NA, m)
  for(j in seq_len(m)) {
    x <- r_noisy_normal(n, epsilon[i])
    t_star <- skew(x)
    y[j] <- abs(t_star) > crit_val
  }
  empirical_pwr_10[i] <- mean(y)
}

## store empirical se 
empirical_se_10 <- sqrt(empirical_pwr_10 * (1 - empirical_pwr_10)/n)

ggplot() +
  geom_ribbon(aes(x = epsilon, ymin = empirical_pwr - empirical_se, ymax = empirical_pwr + empirical_se), alpha = 0.5) +
  geom_line(aes(epsilon, empirical_pwr)) +
  geom_ribbon(aes(x = epsilon, ymin = empirical_pwr_10 - empirical_se_10, ymax = empirical_pwr_10 + empirical_se_10), alpha = 0.5, fill = "blue") +
  geom_line(aes(epsilon, empirical_pwr_10)) +
  geom_hline(aes(yintercept = 0.8), lty = 2)

range(epsilon[empirical_pwr_10 - empirical_se_10 > 0.8])
