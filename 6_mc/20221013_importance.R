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
