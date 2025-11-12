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
  
  mean((x - x_bar)^3) / (mean((x - x_bar)^2))^(3 / 2)
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
ggplot() + geom_histogram(aes(b1)) + xlab("N(0, 1)") + ggtitle(paste("Skewness = ", skew(b1)))
ggplot() + geom_histogram(aes(b2)) + xlab("N(0, 1)") + ggtitle(paste("Skewness = ", skew(b2)))


## ------------------------------------------------------------------------
## Assess the P(Type I Error) for alpha = .05, n = 10, 20, 30, 50, 100, 500

type_i_error_skewness <- function(alpha = 0.05, n, m = 1000) {
  reject <- rep(NA, m)
  for(j in seq_len(m)) {
    x <- rnorm(n)
    t_star <- skew(x)
    
    reject[j] <- abs(t_star / sqrt(6 / n)) > qnorm(alpha / 2, lower.tail = FALSE)
  }
  mean(reject)
}

n <- c(10, 20, 30, 50, 100, 500)
for(i in n) {
  print(paste0("Type i error for n = ", i, ": ", type_i_error_skewness(n = i), "\n"))
}



## Assess the Type I error rate of a skewness test using the finite sample correction variance.
type_i_error_skewness_corrected <- function(alpha = 0.05, n, m = 1000) {
  reject <- rep(NA, m)
  for(j in seq_len(m)) {
    x <- rnorm(n)
    t_star <- skew(x)
    
    reject[j] <- abs(t_star / sqrt(6 * (n - 2) / ((n + 1) * (n + 3)))) > qnorm(alpha / 2, lower.tail = FALSE)
  }
  mean(reject)
}

n <- c(10, 20, 30, 50, 100, 500)
for(i in n) {
  print(paste0("Type i error for n = ", i, ": ", type_i_error_skewness_corrected(n = i)))
}

