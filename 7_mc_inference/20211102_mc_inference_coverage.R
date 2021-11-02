# libraries and setup
library(tidyverse)
set.seed(400)

# Your turn
# 1. Coverage for CI for $\mu$ when $\sigma$ is known, $\left(\overline{x} - z_{1 - \frac{\alpha}{2}} \frac{\sigma}{\sqrt{n}}, \overline{x} + z_{1 - \frac{\alpha}{2}} \frac{\sigma}{\sqrt{n}}\right)$.
# a. Simulate $X_1, \dots, X_n \stackrel{iid}{\sim} N(0, 1)$. Compute the empirical coverage for a $95%$ confidence interval for $n = 5$ using $m = 1000$ MC samples.

## setup for MC
n <- 5
m <- 1000

## simulate data from assumed model
sample <- matrix(rnorm(n * m), nrow = m)

## calculate CIs and indicator variable
ci <- cbind(rowSums(sample)/n - qnorm(.975) * 1/sqrt(n), rowSums(sample)/n + qnorm(.975) * 1/sqrt(n))
y <- ci[,1] < 0 & ci[,2] > 0 # 0 is the true mean

## estimate coverage
mean(y)

# b.  Plot 100 confidence intervals using `geom_segment()` and add a line indicating the true value for $\mu = 0$. Color your intervals by if they contain $\mu$ or not.
data.frame(ci[1:100,]) |>
  mutate(iter = 1:100, contains_true = y[1:100]) |>
  ggplot() +
  geom_vline(aes(xintercept = 0)) +
  geom_segment(aes(x = X1, y = iter, xend = X2, yend = iter, colour = contains_true)) +
  geom_point(aes(x = X1, y = iter)) +
  geom_point(aes(x = X2, y = iter))

# c. Repeat the Monte Carlo estimate of coverage 100 times. Plot the distribution of the results. This is the Monte Carlo estimate of the distribution of the coverage.

## setup for MC
n <- 5
m <- 1000
rep_num <- 100

emp_coverage <- rep(NA, rep_num)
for(i in seq_len(rep_num)) {
  ## simulate data from assumed model
  sample <- matrix(rnorm(n * m), nrow = m)
  
  ## calculate CIs and indicator variable
  ci <- cbind(rowSums(sample)/n - qnorm(.975) * 1/sqrt(n), rowSums(sample)/n + qnorm(.975) * 1/sqrt(n))
  y <- ci[,1] < 0 & ci[,2] > 0 # 0 is the true mean
  
  ## estimate coverage
  emp_coverage[i] <- mean(y)
}

summary(emp_coverage)
ggplot() +
  geom_histogram(aes(emp_coverage), binwidth = .001)

#2. Repeat part 1 but without $\sigma$ known. Now you will plug in an estimage for $\sigma$ (using `sd()`) when you estimate the CI using the same formula that assumes $\sigma$ known. 
# What happens to the empirical coverage? What can we do to improve the coverage? Now increase $n$. What happens to coverage?

## setup for MC
n <- 5
m <- 1000

## simulate data from assumed model
sample <- matrix(rnorm(n * m), nrow = m)

## calculate CIs and indicator variable
sample_means <- rowMeans(sample)
sample_sd <- sqrt(1/(n - 1) * rowSums((sample - matrix(rep(sample_means, n), nrow = m))^2))

ci <- cbind(sample_means - qnorm(.975) * sample_sd/sqrt(n), sample_means + qnorm(.975) * sample_sd/sqrt(n))
y <- ci[,1] < 0 & ci[,2] > 0 # 0 is the true mean

## estimate coverage
mean(y)

## try bigger n
## setup for MC
n <- 500
m <- 1000

## simulate data from assumed model
sample <- matrix(rnorm(n * m), nrow = m)

## calculate CIs and indicator variable
sample_means <- rowMeans(sample)
sample_sd <- sqrt(1/(n - 1) * rowSums((sample - matrix(rep(sample_means, n), nrow = m))^2))

ci <- cbind(sample_means - qnorm(.975) * sample_sd/sqrt(n), sample_means + qnorm(.975) * sample_sd/sqrt(n))
y <- ci[,1] < 0 & ci[,2] > 0 # 0 is the true mean

## estimate coverage
mean(y)

## try with the t dsn
## setup for MC
n <- 5
m <- 1000

## simulate data from assumed model
sample <- matrix(rnorm(n * m), nrow = m)

## calculate CIs and indicator variable
sample_means <- rowMeans(sample)
sample_sd <- sqrt(1/(n - 1) * rowSums((sample - matrix(rep(sample_means, n), nrow = m))^2))

ci <- cbind(sample_means - qt(.975, df = n - 1) * sample_sd/sqrt(n), sample_means + qt(.975, df = n - 1) * sample_sd/sqrt(n))
y <- ci[,1] < 0 & ci[,2] > 0 # 0 is the true mean

## estimate coverage
mean(y)

#3. Repeat 2a. when the data are distributed $\text{Unif}[-1, 1]$ and variance unknown. What happens to the coverage? What can we do to improve coverage in this case and why?

## setup for MC
n <- 5
m <- 1000

## simulate data from assumed model
sample <- matrix(runif(n * m, -1, 1), nrow = m)

## calculate CIs and indicator variable
sample_means <- rowMeans(sample)
sample_sd <- sqrt(1/(n - 1) * rowSums((sample - matrix(rep(sample_means, n), nrow = m))^2))

ci <- cbind(sample_means - qnorm(.975) * sample_sd/sqrt(n), sample_means + qnorm(.975) * sample_sd/sqrt(n))
y <- ci[,1] < 0 & ci[,2] > 0 # 0 is the true mean

## estimate coverage
mean(y)

## try to get some asymptotic normality with CLT
## setup for MC
n <- 30
m <- 1000

## simulate data from assumed model
sample <- matrix(runif(n * m, -1, 1), nrow = m)

## calculate CIs and indicator variable
sample_means <- rowMeans(sample)
sample_sd <- sqrt(1/(n - 1) * rowSums((sample - matrix(rep(sample_means, n), nrow = m))^2))

ci <- cbind(sample_means - qnorm(.975) * sample_sd/sqrt(n), sample_means + qnorm(.975) * sample_sd/sqrt(n))
y <- ci[,1] < 0 & ci[,2] > 0 # 0 is the true mean

## estimate coverage
mean(y)
