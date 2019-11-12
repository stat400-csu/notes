## ---------------------------
library(tidyverse)
set.seed(400)

# compare a symmetric and skewed distribution
data.frame(x = seq(0, 1, length.out = 1000)) %>%
  mutate(skewed = dbeta(x, 6, 2),
         symmetric = dbeta(x, 5, 5)) %>%
  gather(type, dsn, -x) %>%
  ggplot() +
  geom_line(aes(x, dsn, colour = type, lty = type))

## write a skewness function based on a sample x
skew <- function(x) {
  x_bar <- mean(x)
  num <- mean((x - x_bar)^3)
  denom <- (mean((x - x_bar)^2))^1.5
  num/denom
}

## check skewness of some samples
n <- 100
a1 <- rbeta(n, 6, 2)
a2 <- rbeta(n, 2, 6)

## two symmetric samples
b1 <- rnorm(100)
b2 <- rnorm(100)

## fill in the skewness values
ggplot() + geom_histogram(aes(a1)) + xlab("Beta(6, 2)") + ggtitle(paste("Skewness = ", skew(a1)))
ggplot() + geom_histogram(aes(a2)) + xlab("Beta(2, 6)") + ggtitle(paste("Skewness = ", skew(a2)))
ggplot() + geom_histogram(aes(b1)) + xlab("N(0, 1)") + ggtitle(paste("Skewness = "), skew(b1))
ggplot() + geom_histogram(aes(b2)) + xlab("N(0, 1)") + ggtitle(paste("Skewness = "), skew(b2))


## ------------------------------------------------------------------------
## Assess the P(Type I Error) for alpha = .05, n = 10, 20, 30, 50, 100, 500
## we choose N(0, 1) as our symmetric distribution to fulfill H_0
empirical_alpha <- function(n, m, alpha, sample_function) {
  Ij <- rep(NA, m)
  for(j in 1:m) {
    x <- sample_function(n)
    t_star <- skew(x)
    Ij[j] <- as.numeric(abs(t_star) >= qnorm(1 - alpha/2, 0, sqrt(6/n)))
  }
  mean(Ij)
}

sample_norm <- function(n) {
  rnorm(n)
}

sample_beta <- function(n) {
  rbeta(n, 5, 5)
}

empirical_alpha(10, m = 1000, .05, sample_norm)
empirical_alpha(20, m = 1000, .05, sample_norm)
empirical_alpha(30, m = 1000, .05, sample_norm)
empirical_alpha(50, m = 1000, .05, sample_norm)
empirical_alpha(100, m = 1000, .05, sample_norm)
empirical_alpha(500, m = 1000, .05, sample_norm)

empirical_alpha(10, m = 1000, .05, sample_beta)
empirical_alpha(20, m = 1000, .05, sample_beta)
empirical_alpha(30, m = 1000, .05, sample_beta)
empirical_alpha(50, m = 1000, .05, sample_beta)
empirical_alpha(100, m = 1000, .05, sample_beta)
empirical_alpha(500, m = 1000, .05, sample_beta)

## Assess the Type I error rate of a skewness test using the finite sample correction variance.
empirical_alpha2 <- function(n, m, alpha, sample_function) {
  Ij <- rep(NA, m)
  for(j in 1:m) {
    x <- sample_function(n)
    t_star <- skew(x)
    Ij[j] <- as.numeric(abs(t_star) >= qnorm(1 - alpha/2, 0, sqrt(6*(n - 2)/((n + 1)*(n + 3)))))
  }
  mean(Ij)
}

empirical_alpha2(10, m = 1000, .05, sample_norm)
empirical_alpha2(20, m = 1000, .05, sample_norm)
empirical_alpha2(30, m = 1000, .05, sample_norm)
empirical_alpha2(50, m = 1000, .05, sample_norm)
empirical_alpha2(100, m = 1000, .05, sample_norm)
empirical_alpha2(500, m = 1000, .05, sample_norm)
