## libraries
library(tidyverse)

## reproducible
set.seed(400)

## Accept-Reject
## how to choose the envelope
## f = normal
## g = t_2

## check for a bunch of c values
const <- seq(1, 20)

## define functions
f <- function(x) dnorm(x)
g <- function(x) dt(x, 2)

## grid of x for plotting
x <- seq(-100, 100, length.out = 1000)

## first examination
expand.grid(x = x, const = const) %>%
  mutate(f = f(x)) %>%
  mutate(g = g(x)) %>%
  mutate(e = const*g) %>%
  mutate(diff = e - f) %>%
  ggplot() +
  geom_point(aes(x, diff, colour = diff < 0)) +
  facet_grid(const ~ .)

## c = 2 looks ok, let's check more x values
const_final <- 2
data.frame(x = seq(-2, 2, length.out = 1000))%>%
  mutate(f = f(x)) %>%
  mutate(g = g(x)) %>%
  mutate(e = const_final*g) %>%
  mutate(diff = e - f) %>%
  ggplot() +
  geom_point(aes(x, diff, colour = diff < 0))

## discrete inverse transform example
x <- c(1, 3, 4)
p <- c(0.1, 0.2, 0.7)

## need cdf
cdf <- cumsum(p)

## get samples
n <- 1000
u <- runif(n)
sample <- rep(NA, n) 

## room for improvement with a vectorized version
for(i in seq_len(n)) {
  sample[i] <- x[min(which(u[i] <= cdf))]
}

## vectorized version
## the trick is if 3 of them are true, then the first is is minimum
## if 2 of them are true, then the second is the minimum
## if 1 of them is true, then the 3rd is the minimum
sample_vec <- x[4 - rowSums(u <= matrix(rep(cdf, each = n), nrow = n))]

## compare truth to estimate
ggplot() +
  geom_histogram(aes(sample))

table(sample)/n
table(sample_vec)/n
p
