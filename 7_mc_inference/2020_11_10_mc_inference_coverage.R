# libraries and setup
library(tidyverse)
set.seed(400)

# Your turn
# 1. Coverage for CI for $\mu$ when $\sigma$ is known, $\left(\overline{x} - z_{1 - \frac{\alpha}{2}} \frac{\sigma}{\sqrt{n}}, \overline{x} + z_{1 - \frac{\alpha}{2}} \frac{\sigma}{\sqrt{n}}\right)$.
# a. Simulate $X_1, \dots, X_n \stackrel{iid}{\sim} N(0, 1)$. Compute the empirical coverage for a $95%$ confidence interval for $n = 5$ using $m = 1000$ MC samples.
# b.  Plot 100 confidence intervals using `geom_segment()` and add a line indicating the true value for $\mu = 0$. Color your intervals by if they contain $\mu$ or not.
# c. Repeat the Monte Carlo estimate of coverage 100 times. Plot the distribution of the results. This is the Monte Carlo estimate of the distribution of the coverage.

mu <- 0 # this is the theta we care about
sigma <- 1
alpha <- .05

n <- 5
m <- 1000

## take samples
x <- matrix(rnorm(n * m, mu, sigma), nrow = m, ncol = n)

## get CIs
x_bar <- rowSums(x)/n
ci <- cbind(x_bar + -qnorm(1 - alpha/2)*sigma/sqrt(n), x_bar + qnorm(1 - alpha/2)*sigma/sqrt(n))
y <- ci[,1] <= mu & ci[,2] >= mu # indicator of contained true theta

## empirical coverage
mean(y)

## plots of CIs
data.frame(ci)[1:100,] %>% 
  rename(lower = X1, upper = X2) %>%
  mutate(iter = 1:100) %>%
  ggplot() +
  geom_vline(aes(xintercept = mu), lty = 2) +
  geom_segment(aes(x = lower, y = iter, xend = upper, yend = iter, colour = y[1:100]))
  
get_coverage <- function(n, m, mu, sigma, alpha = .05) {
  ## take samples
  x <- matrix(rnorm(n * m, mu, sigma), nrow = m, ncol = n)
  
  ## get CIs
  x_bar <- rowSums(x)/n
  ci <- cbind(x_bar + -qnorm(1 - alpha/2)*sigma/sqrt(n), x_bar + qnorm(1 - alpha/2)*sigma/sqrt(n))
  y <- ci[,1] <= mu & ci[,2] >= mu # indicator of contained true theta
  
  ## empirical coverage
  mean(y)
}

coverage <- rep(NA, 100) 
for(i in seq_len(100)) {
  coverage[i] <- get_coverage(n, m, mu, sigma, alpha)
}

## plot distribution of coverage
ggplot() +
  geom_histogram(aes(coverage), binwidth = .002)


#2. Repeat part 1 but without $\sigma$ known. Now you will plug in an estimage for $\sigma$ (using `sd()`) when you estimate the CI using the same formula that assumes $\sigma$ known. 
# What happens to the empirical coverage? What can we do to improve the coverage? Now increase $n$. What happens to coverage?
  
get_coverage_no_sigma <- function(n, m, mu, sigma, alpha = .05) {
  y <- rep(NA, m)
  for(i in seq_len(m)) {
    ## take samples
    x <- rnorm(n, mu, sigma)
    
    ## get CI
    x_bar <- mean(x)
    sd_x <- sd(x)
    ci <- c(x_bar + -qnorm(1 - alpha/2)*sd_x/sqrt(n), x_bar + qnorm(1 - alpha/2)*sd_x/sqrt(n))
    y[i] <- ci[1] <= mu & ci[2] >= mu # indicator of contained true theta  
  }

    ## empirical coverage
  mean(y)
}

coverage_sigma_unknown <- rep(NA, 100)
for(i in 1:100){
  coverage_sigma_unknown[i] <- get_coverage_no_sigma(n, m, mu, sigma, alpha)
}
get_coverage_no_sigma(50, m, mu, sigma, alpha)


#3. Repeat 2a. when the data are distributed $\text{Unif}[-1, 1]$ and variance unknown. What happens to the coverage? What can we do to improve coverage in this case and why?

get_coverage_unif <- function(n, m, alpha = .05) {
  y <- rep(NA, m)
  for(i in seq_len(m)) {
    ## take samples
    x <- runif(n, -1, 1)
    
    ## get CI
    x_bar <- mean(x)
    sd_x <- sd(x)
    ci <- c(x_bar + -qnorm(1 - alpha/2)*sd_x/sqrt(n), x_bar + qnorm(1 - alpha/2)*sd_x/sqrt(n))
    y[i] <- ci[1] <= 0 & ci[2] >= 0 # indicator of contained true theta  
  }
  
  ## empirical coverage
  mean(y)
}

coverage_unif <- rep(NA, 100)
for(i in 1:100){
  coverage_unif[i] <- get_coverage_unif(n, m, alpha)
}

ggplot() +
  geom_histogram(aes(coverage_sigma_unknown), binwidth = .002)

ggplot() +
  geom_histogram(aes(coverage_unif), binwidth = .002)
