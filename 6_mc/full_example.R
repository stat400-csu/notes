## libraries
library(tidyverse)

## h(x)
h <- function(x) {
  log(x)/(1 + x^2)
}

## visualize the function
x <- seq(1, pi, length.out = 1000)
ggplot() +
  geom_line(aes(x, h(x)))

## estimate theta
g <- function(x) (pi - 1)*h(x)

m <- 10000
u <- runif(m, 1, pi)
theta_hat <- mean(g(u))
se_theta_hat <- sqrt(mean((g(u) - theta_hat)^2)/m)

## compare to truth
integrate(h, 1, pi)

## can we do better? 
## importance sampling
## phi ~ t(1)
f <- function(x) dunif(x, 1, pi)
phi <- function(x) dt(x, 1)

data.frame(x = x) %>%
  mutate(g = g(x),
         gf_phi = g(x)*f(x)/phi(x)) %>%
  gather(funct, value, -x) %>%
  ggplot() +
  geom_line(aes(x, value, group = funct, colour = funct, lty = funct))

## not really
density_function <- function(x) {
  h(x)/theta_hat*(1 <= x & x <= pi)
}

## check
integrate(density_function, 1, pi)


## how could we sample from this density function?
## accept-reject
## need to choose e(x) envelope
## let e(x) = c*f(x) where f(x) is the pdf of a Unif(1, pi)
ggplot() +
  geom_line(aes(x, density_function(x))) +
  geom_ribbon(aes(x, ymin = density_function(x), ymax = max(density_function(x))))

max(density_function(x))
## then e(x) = c*1/(pi - 1) = max(density_function(x))
## c = max(density_function(x))*(pi - 1)

## envelope function
e <- function(x) {
  ## get actual max
  max_fn <- optimize(density_function, c(1, pi), maximum = TRUE)$objective 
  max_fn * (1 <= x & x <= pi)
}

## get samples from dsn
n <- 10000
sample <- rep(NA, n)
accept <- 0
while(accept < n) {
  y <- runif(1, 1, pi)
  u <- runif(1)
  if(u < density_function(y)/e(y)) { ## accept-reject ratio
    accept <- accept + 1 ## increment accept
    sample[accept] <- y ## accept sample
  }
}

## plot results
ggplot() +
  geom_histogram(aes(sample, y = ..density..), binwidth = .05) +
  geom_line(aes(x, density_function((x))), colour = "red")




