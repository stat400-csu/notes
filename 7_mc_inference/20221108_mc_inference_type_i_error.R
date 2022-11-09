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
  diff <- x - x_bar
  
  num <- mean(diff^3)
  denom <- (mean(diff^2))^(3/2)
  
  return(num/denom)
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
type_i_estimate_skew <- function(n, alpha, m) {
  indic <- rep(NA, m)
  for(j in 1:m) {
    x <- rnorm(n, 0, 1)
    t_star <- skew(x)
    indic[j] <- abs(t_star/sqrt(6/n)) > qnorm(1 - alpha/2, 0, 1)
  }
  return(mean(indic))
}

alpha <- .05
n <- c(10, 20, 30, 50, 100, 500)
m <- 5000

for(n_star in n) {
  alpha_hat <- type_i_estimate_skew(n_star, alpha, m)
  print(paste0("n = ", n_star, ": ", alpha_hat))
}


## Assess the Type I error rate of a skewness test using the finite sample correction variance.
type_i_estimate_skew_finite_n <- function(n, alpha, m) {
  indic <- rep(NA, m)
  var_finite <- 6*(n - 2)/((n + 1)*(n + 3))
  for(j in 1:m) {
    x <- rnorm(n, 0, 1)
    t_star <- skew(x)
    indic[j] <- abs(t_star/sqrt(var_finite)) > qnorm(1 - alpha/2, 0, 1)
  }
  return(mean(indic))
}

for(n_star in n) {
  alpha_hat <- type_i_estimate_skew_finite_n(n_star, alpha, m)
  print(paste0("n = ", n_star, ": ", alpha_hat))
}
