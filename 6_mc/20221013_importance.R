## ------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
set.seed(100)

f <- function(x) {
  dexp(x, 1)
}

phi <- function(x) {
  dnorm(x, 0, 1)
}

tau <- function(x) {
  dt(x, 1)
}

g <- function(x) {
  1/(1 + x^2) * (x <= 1)
}

x <- seq(0, 1, length.out = 1000)

ggplot() +
  geom_line(aes(x, g(x)), colour = "black") +
  geom_line(aes(x, f(x) * g(x) / phi(x)), colour = "blue") +
  geom_line(aes(x, f(x) * g(x) / tau(x)), colour = "red")

m <- 1000
## method 1
x_star <- rexp(m, 1)
theta_hat_1 <- mean(g(x_star))
var_hat_1 <- 1/m * mean((g(x_star) - theta_hat_1)^2)

## method 2
y <- rnorm(m, 0, 1)
theta_hat_2 <- mean(g(y) * f(y) / phi(y))
var_hat_2 <- 1/m * mean((g(y) * f(y) / phi(y) - theta_hat_2)^2)

## method 3
z <- rt(m, 1)
theta_hat_3 <- mean(g(z) * f(z) / tau(z))
var_hat_3 <- 1/m * mean((g(z) * f(z) / tau(z) - theta_hat_3)^2)

data.frame(method = c("MC", "Importance a)", "Importance b)"),
           theta_hat = c(theta_hat_1, theta_hat_2, theta_hat_3),
           var_hat = c(var_hat_1, var_hat_2, var_hat_3))

## method 1 is the best one.
## if we wanted to improve, we could think of a way to flatten that ratio.
