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
skew <- function(x) {
  x_bar <- mean(x)
  
  # sqrt(b_1) function
  mean((x - x_bar)^3)/(mean((x - x_bar)^2))^(3/2)
}

## check skewness of some samples
n <- 100
a1 <- rbeta(n, 6, 2)
a2 <- rbeta(n, 2, 6)

## two symmetric samples
b1 <- rnorm(100)
b2 <- rnorm(100)

## fill in the skewness values
ggplot() + geom_histogram(aes(a1)) + xlab("Beta(6, 2)") + ggtitle(paste("Skewness =", skew(a1)))
ggplot() + geom_histogram(aes(a2)) + xlab("Beta(2, 6)") + ggtitle(paste("Skewness =", skew(a2)))
ggplot() + geom_histogram(aes(b1)) + xlab("N(0, 1)") + ggtitle(paste("Skewness =", skew(b1)))
ggplot() + geom_histogram(aes(b2)) + xlab("N(0, 1)") + ggtitle(paste("Skewness =", skew(b2)))


## ------------------------------------------------------------------------
## Assess the P(Type I Error) for alpha = .05, n = 10, 20, 30, 50, 100, 500

## function to assess type I error rate for different parameters
est_alpha_skew <- function(n, r_null_dist, variance = 6/n, alpha = 0.05, m = 1000) {
  reject <- rep(NA, m)
  for(i in seq_len(m)) {
    sample <- r_null_dist(n)
    t_star <- abs(skew(sample)/sqrt(variance))
    reject[i] <- t_star > qnorm(1 - alpha/2)
  }
  mean(reject)
}

## example of a symetric distribution we could use in our function
r_beta55 <- function(n) rbeta(n, 5, 5)

## store results
res <- data.frame(n = c(10, 20, 30, 50, 100, 500))
for(i in seq_len(nrow(res))) {
  n <- res[i, "n"]
  res[i, "est_alpha"] = est_alpha_skew(n, rnorm, m = 10000)
  res[i, "est_alpha_better"] = est_alpha_skew(n, rnorm, variance = 6*(n - 2)/((n + 1)*(n + 3)), m = 10000)
}


## plot estimated type I error rates for 2 methods
res |>
  pivot_longer(-n, names_to = "method", values_to = "est_alpha") |>
  ggplot() +
  geom_hline(aes(yintercept = 0.05), lty = 2, colour = "red") +
  geom_line(aes(n, est_alpha, colour = method)) +
  geom_point(aes(n, est_alpha, colour = method)) +
  ylim(c(0, .1))

## Assess the Type I error rate of a skewness test using the finite sample correction variance.

## see above

