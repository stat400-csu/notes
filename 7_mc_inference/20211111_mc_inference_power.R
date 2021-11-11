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

emp_power_se_skew <- function(n, m, epsilon) {
  var_sqrt_b1 <- 6*(n - 2)/((n + 1)*(n + 3)) # adjusted variance for skewness test
  crit_val <- qnorm(1 - alpha/2, 0, sqrt(var_sqrt_b1)) #crit value for the test
  empirical_pwr <- rep(NA, length(epsilon)) #storage
  
  # estimate power for each value of epsilon
  for(j in 1:length(epsilon)) {
    # perform MC to estimate empirical power
    y <- rep(NA, m)
    for(i in seq_len(m)) {
      x <- r_noisy_normal(n, epsilon[j])
      T_star <- skew(x)
      y[i] <- abs(T_star) > crit_val
    }
    empirical_pwr[j] <- mean(y)
  }
  
  ## store empirical se 
  empirical_se <- sqrt(empirical_pwr * (1 - empirical_pwr)/m)
  
  emp_power_skew <- data.frame(epsilon = epsilon, 
                               power = empirical_pwr,
                               se = empirical_se)
  emp_power_skew
}
## plot results -- 
## x axis = epsilon values
## y axis = empirical power
## use lines + add band of estimate +/- se
emp_power_se_skew(100, m, epsilon) |>
  mutate(n = 100) |>
  bind_rows(emp_power_se_skew(10, m, epsilon) |> mutate(n = 10)) |>
  ggplot() +
  geom_hline(aes(yintercept = 0.8), lty = 2) +
  geom_ribbon(aes(x = epsilon, ymin = power - se, ymax = power + se), alpha = 0.5, fill = "blue") +
  geom_line(aes(epsilon, power)) +
  facet_wrap(.~n)

