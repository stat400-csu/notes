## ------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
set.seed(100)

## lets make comparison plot to see which will be the best
h <- function(x) {
  exp(-x)/(1 + x^2)
}

f <- function(x) {
  dexp(x, 1)
}

phi_a <- function(x) {
  dunif(x, 0, 1)
}

phi_b <- function(x) {
  exp(-x)/(1 - exp(-1)) * (x >= 0 & x <= 1)
}

x <- seq(0, 1, length.out = 500)

ggplot() +
  geom_line(aes(x, h(x)/f(x)), color = "black") +
  geom_line(aes(x, h(x)/phi_a(x)), color = "red") +
  geom_line(aes(x, h(x)/phi_b(x)), color = "blue")

## implement all 3 algorithms
## get estimate of theta hat & estimate var of theta hat
m <- 5000

## method 1
theta_1 <- function(m) {
  x <- rexp(m, 1)
  g <- 1/(1 + x^2) * (x <= 1)
  theta_hat <- mean(g)
  var_hat <- 1/m * mean((g - theta_hat)^2)
  c(theta_hat, var_hat)
}

theta_2 <- function(m) {
  x <- runif(m, 0, 1)
  g <- exp(-x)/(1 + x^2)
  theta_hat <- mean(g)
  var_hat <- 1/m * mean((g - theta_hat)^2)
  c(theta_hat, var_hat)
}

theta_3 <- function(m) {
  u <- runif(m, 0, 1)
  x <- -log(1 - u * (1 - exp(-1)))
  g <- (1 - exp(-1))/(1 + x^2)
  theta_hat <- mean(g)
  var_hat <- 1/m * mean((g - theta_hat)^2)
  c(theta_hat, var_hat)
}

theta_1(m)
theta_2(m)
theta_3(m)

integrate(h, 0, 1)
