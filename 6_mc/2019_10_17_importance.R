## ------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
set.seed(100)

## write functions
g <- function(x) {
  1/(1 + x^2)*(x <= 1)
}

f <- function(x) {
  dexp(x, 1)
}

phi_a <- function(x) {
  dunif(x, 0, 1)
}

phi_b <- function(x) {
  exp(-x)/(1 - exp(-1))*(0 <= x & x <= 1)
}

## make plots
x <- seq(0, 1, length.out = 1000)

data.frame(x = x) %>%
  mutate(f = f(x),
         g = g(x),
         phi_a = phi_a(x),
         phi_b = phi_b(x)) -> function_df

function_df %>%
  gather(phi, value, phi_a, phi_b) %>%
  ggplot() +
  geom_line(aes(x, f), colour = "blue", lty = 1) +
  geom_line(aes(x, value), colour = "black", lty = 2) +
  facet_wrap(.~phi)

function_df %>%
  mutate(fg_phi_a = f*g/phi_a,
         fg_phi_b = f*g/phi_b) %>%
  select(x, g, fg_phi_a, fg_phi_b) %>%
  gather(funct, value, -x) %>%
  ggplot() +
  geom_line(aes(x, value, colour = funct, lty = funct))

## get estimators
## method 1
m <- 1000
x <- rexp(m, 1)
phi_hat_1 <- mean(g(x))
var_hat_phi_hat_1 <- mean((g(x) - phi_hat_1)^2)/m

## method 2a
x <- runif(m, 0, 1)
phi_hat_2a <- mean(g(x)*f(x)/phi_a(x))
var_hat_phi_hat_2a <- mean((g(x)*f(x)/phi_a(x) - phi_hat_2a)^2)/m

## method 2b
u <- runif(m, 0, 1)
x <- -log(1 - u*(1 - exp(-1)))
phi_hat_2b <- mean(g(x)*f(x)/phi_b(x))
var_hat_phi_hat_2b <- mean((g(x)*f(x)/phi_b(x) - phi_hat_2b)^2)/m


