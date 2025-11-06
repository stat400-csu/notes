# libraries and setup
library(tidyverse)
set.seed(400)

# Your turn
# 1. Coverage for CI for $\mu$ when $\sigma$ is known, $\left(\overline{x} - z_{1 - \frac{\alpha}{2}} \frac{\sigma}{\sqrt{n}}, \overline{x} + z_{1 - \frac{\alpha}{2}} \frac{\sigma}{\sqrt{n}}\right)$.
# a. Simulate $X_1, \dots, X_n \stackrel{iid}{\sim} N(0, 1)$. Compute the empirical coverage for a $95%$ confidence interval for $n = 5$ using $m = 1000$ MC samples.
# b.  Plot 100 confidence intervals using `geom_segment()` and add a line indicating the true value for $\mu = 0$. Color your intervals by if they contain $\mu$ or not.
# c. Repeat the Monte Carlo estimate of coverage 100 times. Plot the distribution of the results. This is the Monte Carlo estimate of the distribution of the coverage.

n <- 5
m <- 1000
alpha <- .05

ci <- matrix(nrow = m, ncol = 2)
y <- rep(NA, m)
for(j in seq_len(m)) {
  x <- rnorm(n)

  x_bar <- mean(x)
  ci[j,] <- x_bar + c(-1, 1) * qnorm(1 - alpha / 2) * 1 / sqrt(n)
  y[j] <- (0 > ci[j, 1]) & (0 < ci[j, 2])
}

## empirical coverage
mean(y)

## part b
data.frame(ci) |> 
  rename(lb = X1, ub = X2) |>
  mutate(iter = 1:n()) |>
  mutate(contains = y) |>
  filter(iter <= 100) |>
  ggplot() +
  geom_segment(aes(x = lb, xend = ub, y = iter, colour = contains))


#2. Repeat part 1 but without $\sigma$ known. Now you will plug in an estimage for $\sigma$ (using `sd()`) when you estimate the CI using the same formula that assumes $\sigma$ known. 
# What happens to the empirical coverage? What can we do to improve the coverage? Now increase $n$. What happens to coverage?
  

#3. Repeat 2a. when the data are distributed $\text{Unif}[-1, 1]$ and variance unknown. What happens to the coverage? What can we do to improve coverage in this case and why?

y <- rep(NA, m)
for(j in seq_len(m)) {
  x <- runif(n, -1, 1)
  
  x_bar <- mean(x)
  sigma_hat <- sd(x)
  ci[j,] <- x_bar + c(-1, 1) * qnorm(1 - alpha / 2) * sigma_hat / sqrt(n)
  y[j] <- (0 > ci[j, 1]) & (0 < ci[j, 2])
}

## empirical coverage
mean(y)

coverage_hat <- rep(NA, 100) 
for(i in seq_len(100)) {
  y <- rep(NA, m)
  for(j in seq_len(m)) {
    x <- runif(n, -1, 1)
    
    x_bar <- mean(x)
    sigma_hat <- sd(x)
    ci[j,] <- x_bar + c(-1, 1) * qnorm(1 - alpha / 2) * sigma_hat / sqrt(n)
    y[j] <- (0 > ci[j, 1]) & (0 < ci[j, 2])
  }
  
  ## empirical coverage
  coverage_hat[i] <- mean(y)
  
}

ggplot() +
  geom_histogram(aes(coverage_hat)) +
  geom_vline(aes(xintercept = .95), colour = "blue")

