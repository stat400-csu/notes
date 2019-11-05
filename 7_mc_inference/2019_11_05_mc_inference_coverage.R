# libraries and setup
library(tidyverse)
set.seed(400)

# Your turn
# 1. Coverage for CI for $\mu$ when $\sigma$ is known, $\left(\overline{x} - z_{1 - \frac{\alpha}{2}} \frac{\sigma}{\sqrt{n}}, \overline{x} + z_{1 - \frac{\alpha}{2}} \frac{\sigma}{\sqrt{n}}\right)$.
# a. Simulate $X_1, \dots, X_n \stackrel{iid}{\sim} N(0, 1)$. Compute the empirical coverage for a $95%$ confidence interval for $n = 5$ using $m = 1000$ MC samples.
# b.  Plot 100 confidence intervals using `geom_segment()` and add a line indicating the true value for $\mu = 0$. Color your intervals by if they contain $\mu$ or not.
# c. Repeat the Monte Carlo estimate of coverage 100 times. Plot the distribution of the results. This is the Monte Carlo estimate of the distribution of the coverage.

ci_sigma_known <- function(x, alpha, sigma) {
  c(mean(x) - qnorm(1 - alpha/2, 0, 1)*sigma/sqrt(length(x)), mean(x) + qnorm(1 - alpha/2, 0, 1)*sigma/sqrt(length(x)))
}

n <- 5
m <- 1000
samples <- matrix(rnorm(n*m, 0, 1), nrow = m, ncol = n)
cis <- matrix(NA, nrow = m, ncol = 2)
for(i in 1:m) {
  cis[i, ] <- ci_sigma_known(samples[i,], .05, 1)
}
emp_coverage <- mean(cis[,1] < 0 & cis[,2] > 0)
cis_df <- data.frame(cis)
names(cis_df) <- c("L", "U")
cis_df$iter <- 1:m

ggplot(head(cis_df, 100)) +
  geom_segment(aes(x = L, xend = U, y = iter, yend = iter)) +
  geom_vline(aes(xintercept = 0), colour = "red")


# c. repeat 100 times
emp_coverage_100 <- rep(NA, 100)

for(j in 1:100) {
  samples <- matrix(rnorm(n*m, 0, 1), nrow = m, ncol = n)
  cis <- matrix(NA, nrow = m, ncol = 2)
  for(i in 1:m) {
    cis[i, ] <- ci_sigma_known(samples[i,], .05, 1)
  }
  emp_coverage_100[j] <- mean(cis[,1] < 0 & cis[,2] > 0)
}

ggplot() +
  geom_histogram(aes(emp_coverage_100), binwidth = .002)

#2. Repeat part 1 but without $\sigma$ known. Now you will plug in an estimage for $\sigma$ (using `sd()`) when you estimate the CI using the same formula that assumes $\sigma$ known. 
# What happens to the empirical coverage? What can we do to improve the coverage? Now increase $n$. What happens to coverage?

n <- 100
m <- 1000
samples <- matrix(rnorm(n*m, 0, 1), nrow = m, ncol = n)
cis <- matrix(NA, nrow = m, ncol = 2)
for(i in 1:m) {
  cis[i, ] <- ci_sigma_known(samples[i,], .05, sd(samples[i,]))
}
emp_coverage <- mean(cis[,1] < 0 & cis[,2] > 0)
cis_df <- data.frame(cis)
names(cis_df) <- c("L", "U")
cis_df$iter <- 1:m

ggplot(head(cis_df, 100)) +
  geom_segment(aes(x = L, xend = U, y = iter, yend = iter)) +
  geom_vline(aes(xintercept = 0), colour = "red")


# c. repeat 100 times
emp_coverage_100 <- rep(NA, 100)

for(j in 1:100) {
  samples <- matrix(rnorm(n*m, 0, 1), nrow = m, ncol = n)
  cis <- matrix(NA, nrow = m, ncol = 2)
  for(i in 1:m) {
    cis[i, ] <- ci_sigma_known(samples[i,], .05, sd(samples[i,]))
  }
  emp_coverage_100[j] <- mean(cis[,1] < 0 & cis[,2] > 0)
}

ggplot() +
  geom_histogram(aes(emp_coverage_100), binwidth = .002)


#3. Repeat 2a. when the data are distributed $\text{Unif}[-1, 1]$ and variance unknown. What happens to the coverage? What can we do to improve coverage in this case and why?


n <- 100
m <- 1000
samples <- matrix(runif(n*m, -1, 1), nrow = m, ncol = n)
cis <- matrix(NA, nrow = m, ncol = 2)
for(i in 1:m) {
  cis[i, ] <- ci_sigma_known(samples[i,], .05, sd(samples[i,]))
}
emp_coverage <- mean(cis[,1] < 0 & cis[,2] > 0)
