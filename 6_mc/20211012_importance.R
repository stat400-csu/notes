## ------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
set.seed(100)

f <- function(x) {
  dexp(x, 1)
}

g <- function(x) {
  1/(1 + x^2) * (x <= 1)
}

phi_a <- function(x) {
  dunif(x, 0, 1)
}

phi_b <- function(x) {
  exp(-x)/(1 - exp(-1)) * (x <= 1 & x >= 0)
}

x <- seq(0, 1, length.out = 1000)

ggplot() +
  geom_line(aes(x, g(x))) +
  geom_line(aes(x, f(x) * g(x)/phi_a(x)), colour = "blue") +
  geom_line(aes(x, f(x) * g(x)/phi_b(x)), colour = "red")


## option 1
m <- 5000
sample_1 <- rexp(m, 1)
theta_hat_1 <- mean(g(sample_1))

## option 2
sample_2 <- runif(m, 0, 1)
theta_hat_2 <- mean(g(sample_2) * f(sample_2)/phi_a(sample_2))

## option 3
u <- runif(m)
sample_3 <- -log(1 - u * (1 - exp(-1)))
theta_hat_3 <- mean(g(sample_3) * f(sample_3)/phi_b(sample_3))


theta_hat_1
theta_hat_2
theta_hat_3

h <- function(x) {
  f(x) * g(x)
}
integrate(h, lower = 0, upper = 1)
