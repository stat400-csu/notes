# libraries
library(tidyverse)

# set seed
set.seed(420)

# h integrand function
h <- function(x) {
  res <- log(x)/(1 + x^2)*(x <= pi & x >= 1)
  res[is.nan(res)] <- 0
  res
}

# take a look
x <- seq(1, pi, length.out = 1000)
ggplot() +
  geom_line(aes(x, h(x)))

# 1. estimate theta using MC
g <- function(x) {
  h(x) * (pi - 1)
}

m <- 10000
u <- runif(m, 1, pi)
theta_hat <- mean(g(u))
se_theta_hat <- sqrt(mean((g(u) - theta_hat)^2)/m)

integrate(h, 1, pi)

## 2. Can we do better?
## importance sampling with Normal(1,1)

f <- function(x) {
  dunif(x, 1, pi)
}

phi <- function(x) {
  dnorm(x, 1, 1)
}
## probably not a good idea
ggplot() +
  geom_line(aes(x, g(x))) +
  geom_line(aes(x, f(x) * g(x)/phi(x)), color = "blue")

## but let's do it anyways
y <- rnorm(m, 1, 1)
theta_hat_i <- mean(g(y)*f(y)/phi(y))

density_fn <- function(x) {
  h(x)/theta_hat * (1 <= x & x <= pi)
}

## check this is a density
## should be close to 1
integrate(density_fn, 1, pi)

## 3. Sample from the density
## How to do it?? Use accept-reject
## need to choose e(x) envelope
## let e = d * f(x) where f is the pdf of the uniform(1, pi)
ggplot() +
  geom_line(aes(x, density_fn(x))) +
  geom_ribbon(aes(x, ymin = density_fn(x), ymax = max(density_fn(x))))

max(density_fn(x))
## then e(x) = x * 1/(pi - 1) = max(density_fn(x))
## => d = (pi - 1) * max(density_fn(x))

## envelope function
e <- function(x) {
  ## get max
  optimize(density_fn, c(1, pi), maximum = TRUE)$objective
}

## get samples
n <- 10000
sample <- rep(NA, n)
accept <- 0
while(accept < n) {
  y <- runif(1, 1, pi)
  u <- runif(1)
  if(u < density_fn(y)/e(y)) {
    accept <- accept + 1
    sample[accept] <- y
  }
}

## plot results
ggplot() +
  geom_histogram(aes(sample, y = ..density..), binwidth = .05) +
  geom_line(aes(x, density_fn(x)), colour = "red")


