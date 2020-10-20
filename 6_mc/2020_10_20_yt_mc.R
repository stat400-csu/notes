## libraries
library(tidyverse)

## reproducibility
set.seed(445)

## h(x)
h <- function(x) {
  res <- rep(0, length(x))
  valid_idx <- 1 <= x & x <= pi
  res[valid_idx] <-  log(x[valid_idx])/(1 + x[valid_idx]^2)
  res
}

## visualize h(x)
z <- seq(1, pi, length.out = 1000)
ggplot() +
  geom_line(aes(z, h(z)))

## estimate theta
g <- function(x) {
  h(x)*(pi - 1)
}

m <- 10000
u <- runif(m, 1, pi)
theta_hat <- mean(g(u))
var_theta_hat <- 1/m * mean((g(u) - theta_hat)^2)

## compare to truth
integrate(h, 1, pi)

## How to reduce variance?
## importance sampling
## phi is N(1, 1)
x <- rnorm(m, 1, 1)
f <- function(x) dunif(x, 1, pi)
phi <- function(x) dnorm(x, 1, 1)
inner_sum <- g(x)*f(x)/phi(x)
theta_hat2 <- mean(inner_sum)
var_theta_hat2 <- 1/m * mean((inner_sum - theta_hat2)^2)

## visualize what's going on
data.frame(x = z) %>%
  mutate(g = g(x),
         gf_phi = g(x)*f(x)/phi(x)) %>%
  gather(funct, value, -x) %>%
  ggplot() +
  geom_line(aes(x, value, group = funct, colour = funct, lty = funct))

## importance sampling will not improve on the original MC integration

## How could we sample from this density function?
density <- function(x) {
  h(x)/theta_hat * (1 <= x & x <= pi)
}

## check this is a density
integrate(density, 1, pi) ## pretty close!

## sample using accept-reject 
## with e(x) = const * dunif(1, pi)
ggplot() +
  geom_line(aes(z, density(y))) +
  geom_ribbon(aes(z, ymin = density(z), ymax = max(density(z))))

## get max
max_density <- optimize(density, c(1, pi), maximum = TRUE)$objective
e <- function(x) {
  max_density * (1 <= x & x <= pi)
}

## get samples
n <- 1000
sample <- rep(NA, n)
accept <- 0
while(accept < n) {
  y <- runif(1, 1, pi)
  u <- runif(1)
  if(u < density(y)/e(y)) { ## accept reject ratio
    accept <- accept + 1 ## increment accept
    sample[accept] <- y ## accept sample
  }
}

## plot results
ggplot() +
  geom_histogram(aes(sample, y = ..density..), binwidth = .05) +
  geom_line(aes(z, density(z)), colour = "red")

## estimated efficiency of our sampler is 1/const
1/(max_density*(pi - 1))

