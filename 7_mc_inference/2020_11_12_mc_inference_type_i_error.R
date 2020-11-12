## ---------------------------
library(tidyverse)

# compare a symmetric and skewed distribution
data.frame(x = seq(0, 1, length.out = 1000)) %>%
  mutate(skewed = dbeta(x, 6, 2),
         symmetric = dbeta(x, 5, 5)) %>%
  gather(type, dsn, -x) %>%
  ggplot() +
  geom_line(aes(x, dsn, colour = type, lty = type))

## write a skewness function based on a sample x
## x is a numeric vector
skew <- function(x) {
  x_bar <- mean(x)
  mean((x - x_bar)^3)/mean((x - x_bar)^2)^(3/2)
}

## check skewness of some samples
n <- 100
a1 <- rbeta(n, 6, 2)
a2 <- rbeta(n, 2, 6)

## two symmetric samples
b1 <- rnorm(n)
b2 <- rnorm(n)

## fill in the skewness values
ggplot() + geom_histogram(aes(a1)) + xlab("Beta(6, 2)") + ggtitle(paste("Skewness = ", skew(a1)))
ggplot() + geom_histogram(aes(a2)) + xlab("Beta(2, 6)") + ggtitle(paste("Skewness = ", skew(a2)))
ggplot() + geom_histogram(aes(b1)) + xlab("N(0, 1)") + ggtitle(paste("Skewness = ", skew(b1)))
ggplot() + geom_histogram(aes(b2)) + xlab("N(0, 1)") + ggtitle(paste("Skewness = ", skew(b2)))


## ------------------------------------------------------------------------
## Assess the P(Type I Error) for alpha = .05, n = 10, 20, 30, 50, 100, 500
empirical_type_i <- function(n, m, alpha, sampling_function) {
  indicator <- rep(NA, m) 
  for(i in seq_len(m)) {
    x <- sampling_function(n)
    T_star <- skew(x)/sqrt(6/n)
    indicator[i] <- abs(T_star) >= qnorm(1 - alpha/2)
  }
  mean(indicator)
}

sample_normal <- function(n) {
  rnorm(n)
}

sample_beta <- function(n) {
  rbeta(n, 20, 20)
}

empirical_type_i(10, 1000, .05, sample_normal)
empirical_type_i(20, 1000, .05, sample_normal)
empirical_type_i(30, 1000, .05, sample_normal)
empirical_type_i(50, 1000, .05, sample_normal)
empirical_type_i(100, 1000, .05, sample_normal)
empirical_type_i(500, 1000, .05, sample_normal)

empirical_type_i(10, 1000, .05, sample_beta)
empirical_type_i(20, 1000, .05, sample_beta)
empirical_type_i(30, 1000, .05, sample_beta)
empirical_type_i(50, 1000, .05, sample_beta)
empirical_type_i(100, 1000, .05, sample_beta)
empirical_type_i(2000, 1000, .05, sample_beta)



## Assess the Type I error rate of a skewness test using the finite sample correction variance.
empirical_type_i_finite <- function(n, m, alpha, sampling_function) {
  indicator <- rep(NA, m) 
  for(i in seq_len(m)) {
    x <- sampling_function(n)
    T_star <- skew(x)/sqrt((6*(n - 2))/((n + 1)*(n + 3)))
    indicator[i] <- abs(T_star) >= qnorm(1 - alpha/2)
  }
  mean(indicator)
}

empirical_type_i_finite(10, 1000, .05, sample_normal)
empirical_type_i_finite(20, 1000, .05, sample_normal)
empirical_type_i_finite(30, 1000, .05, sample_normal)
empirical_type_i_finite(50, 1000, .05, sample_normal)
empirical_type_i_finite(100, 1000, .05, sample_normal)
empirical_type_i_finite(500, 1000, .05, sample_normal)

empirical_type_i_finite(10, 1000, .05, sample_beta)
empirical_type_i_finite(20, 1000, .05, sample_beta)
empirical_type_i_finite(30, 1000, .05, sample_beta)
empirical_type_i_finite(50, 1000, .05, sample_beta)
empirical_type_i_finite(100, 1000, .05, sample_beta)
empirical_type_i_finite(2000, 1000, .05, sample_beta)

