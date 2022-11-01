# libraries and setup
library(tidyverse)
set.seed(400)

# Your turn
# 1. Coverage for CI for $\mu$ when $\sigma$ is known, $\left(\overline{x} - z_{1 - \frac{\alpha}{2}} \frac{\sigma}{\sqrt{n}}, \overline{x} + z_{1 - \frac{\alpha}{2}} \frac{\sigma}{\sqrt{n}}\right)$.
# a. Simulate $X_1, \dots, X_n \stackrel{iid}{\sim} N(0, 1)$. Compute the empirical coverage for a $95%$ confidence interval for $n = 5$ using $m = 1000$ MC samples.

coverage_sigma_known <- function(alpha, mu, sigma, n, m, return_intervals = FALSE) {
  ## generate data
  x <- matrix(rnorm(n * m, mu, sigma), nrow = m, ncol = n)
  
  ## compute intervals
  x_bar <- rowMeans(x)
  lower <- x_bar - qnorm(1 - alpha/2, 0, 1) * sigma/sqrt(n)
  upper <- x_bar + qnorm(1 - alpha/2, 0, 1) * sigma/sqrt(n)
  
  ## create indicator 
  y <- lower <= mu & upper >= mu
  
  ## estimate coverage
  emp_cov <- mean(y)
  
  ## return results
  if(return_intervals) {
    res <- list(coverage = emp_cov,
                intervals = cbind(lower, upper))
  } else {
    res <- emp_cov
  }
  return(res)
}

one_a <- coverage_sigma_known(.05, 0, 1, 5, 1000, TRUE)
one_a$coverage

# b.  Plot 100 confidence intervals using `geom_segment()` and add a line indicating the true value for $\mu = 0$. Color your intervals by if they contain $\mu$ or not.
data.frame(one_a$intervals) |>
  mutate(iter = 1:n()) |>
  filter(iter <= 100) |>
  mutate(contains_mu = lower <= 0 & upper >= 0) |>
  ggplot() +
  geom_vline(aes(xintercept = 0), lty = 2) +
  geom_segment(aes(x = lower, xend = upper, y = iter, yend = iter, colour = contains_mu))

# c. Repeat the Monte Carlo estimate of coverage 100 times. Plot the distribution of the results. This is the Monte Carlo estimate of the distribution of the coverage.
one_c <- rep(NA, 100)
for(i in seq_len(100)) {
  one_c[i] <- coverage_sigma_known(.05, 0, 1, 5, 1000, FALSE)
}
ggplot() +
  geom_histogram(aes(one_c))


#2. Repeat part 1 but without $\sigma$ known. Now you will plug in an estimage for $\sigma$ (using `sd()`) when you estimate the CI using the same formula that assumes $\sigma$ known. 
# What happens to the empirical coverage? What can we do to improve the coverage? Now increase $n$. What happens to coverage?

coverage_sigma_unknown <- function(alpha, mu, sigma, n, m, return_intervals = FALSE) {
  ## generate data
  x <- matrix(rnorm(n * m, mu, sigma), nrow = m, ncol = n)
  
  ## compute intervals
  x_bar <- rowMeans(x)
  sigma_hat <- sqrt(rowSums((x - x_bar)^2)/(n - 1))
  lower <- x_bar - qnorm(1 - alpha/2, 0, 1) * sigma_hat/sqrt(n)
  upper <- x_bar + qnorm(1 - alpha/2, 0, 1) * sigma_hat/sqrt(n)
  
  ## create indicator 
  y <- lower <= mu & upper >= mu
  
  ## estimate coverage
  emp_cov <- mean(y)
  
  ## return results
  if(return_intervals) {
    res <- list(coverage = emp_cov,
                intervals = cbind(lower, upper))
  } else {
    res <- emp_cov
  }
  return(res)
}
two_a <- coverage_sigma_unknown(.05, 0, 1, 5, 1000)

two_b <- data.frame(n = c(5, 10, 25, 40, 60, 100),
                    emp_cov = NA)
for(n in seq_len(nrow(two_b))) {
  two_b[n, "emp_cov"] <- coverage_sigma_unknown(.05, 0, 1, two_b[n, "n"], 1000)
}

two_b |>
  ggplot() +
  geom_point(aes(n, emp_cov)) +
  geom_line(aes(n, emp_cov))

#3. Repeat 2a. when the data are distributed $\text{Unif}[-1, 1]$ and variance unknown. What happens to the coverage? What can we do to improve coverage in this case and why?
coverage_uniform <- function(alpha, mu, n, m, return_intervals = FALSE) {
  ## generate data
  x <- matrix(runif(n * m, mu - 1, mu + 1), nrow = m, ncol = n)
  
  ## compute intervals
  x_bar <- rowMeans(x)
  sigma_hat <- sqrt(rowSums((x - x_bar)^2)/(n - 1))
  lower <- x_bar - qnorm(1 - alpha/2, 0, 1) * sigma_hat/sqrt(n)
  upper <- x_bar + qnorm(1 - alpha/2, 0, 1) * sigma_hat/sqrt(n)
  
  ## create indicator 
  y <- lower <= mu & upper >= mu
  
  ## estimate coverage
  emp_cov <- mean(y)
  
  ## return results
  if(return_intervals) {
    res <- list(coverage = emp_cov,
                intervals = cbind(lower, upper))
  } else {
    res <- emp_cov
  }
  return(res)
}
three_a <- coverage_uniform(.05, 0, 5, 1000)

three_b <- data.frame(n = c(5, 10, 25, 40, 60, 100, 1000),
                    emp_cov = NA)
for(n in seq_len(nrow(three_b))) {
  three_b[n, "emp_cov"] <- coverage_uniform(.05, 0, three_b[n, "n"], 1000)
}

three_b |>
  ggplot() +
  geom_point(aes(n, emp_cov)) +
  geom_line(aes(n, emp_cov))
