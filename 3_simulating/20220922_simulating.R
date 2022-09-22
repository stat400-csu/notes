## ---- echo = FALSE-------------------------------------------------------
library(ggplot2)
library(knitr)


## ------------------------------------------------------------------------
set.seed(400) #reproducibility

rnorm(10) # 10 observations of a N(0,1) r.v.
rnorm(10, 0, 5) # 10 observations of a N(0,5^2) r.v.
rexp(10) # 10 observations from an Exp(1) r.v.


## Simulate a random sample of size $1000$ from the pdf $f_X(x) = 3x^2, 0 \le x \le 1$.
cdf_inv <- function(u) {
  u^(1/3)
}

u <- runif(1000)
x <- cdf_inv(u)

## check result
ggplot() +
  geom_histogram(aes(x, y = ..density..)) +
  geom_line(aes(x, 3*x^2), colour = "red")

## ------------------------------------------------------------------------
# write code for inverse transform example
# f_X(x) = 3x^2, 0 <= x \<= 1


## Generate 1000 samples from the following discrete distribution.


## ------------------------------------------------------------------------
x <- 1:3
p <- c(0.1, 0.2, 0.7)
n <- 1000

rdiscrete <- function(n, x, p) {
  ## n = number of draws
  ## x = support, assumed in order
  ## p = pmf probabilities
  
  cdf <- cumsum(p) # get cdf from pmf
  
  x_star <- rep(NA, n) # storage vector
  for(i in seq_len(n)) { # I want n draws
    u <- runif(1) # sample uniform(0,1)
    x_star[i] <- min(x[u <= cdf]) # find the x value such that u <= F(x)
  }
  return(x_star) # return sample
}

data.frame(x_star = rdiscrete(n, x, p)) |>
  group_by(x_star) |>
  summarise(prob = n()/n) |>
  ggplot() +
  geom_point(aes(x_star, prob), colour = "blue") +
  geom_jitter(aes(x, p), colour = "red", height = 0)

## instead
x_star2 <- sample(x, n, replace = TRUE, prob = p)
as.data.frame(table(x_star2)) |>
  mutate(est_prob = Freq/n) |>
  mutate(x_star2 = as.numeric(x_star2)) -> est_prob2

data.frame(x_star = rdiscrete(n, x, p)) |>
  group_by(x_star) |>
  summarise(prob = n()/n) |>
  ggplot() +
  geom_point(aes(x_star, prob), colour = "blue") +
  geom_jitter(aes(x, p), colour = "red", height = 0) +
  geom_jitter(aes(x_star2, est_prob), colour = "darkgreen", height = 0, data = est_prob2)



## -----------------------------------------------------------
kable(t(data.frame(x = x, f = p)))


## ------------------------------------------------------------------------
# write code to sample from discrete dsn
n <- 1000


## ------------------------------------------------------------
x <- seq(-5, 10, length.out = 100)
f <- .5*dnorm(x, 0, 1) + .25*dnorm(x, 3, 2) + .1*dnorm(x, -3, .5) + .15*dnorm(3, .25)
e <- dnorm(x, -5, 10)*7

ggplot() +
  geom_line(aes(x, f)) +
  geom_ribbon(aes(x, ymin = f, ymax = e)) +
  geom_segment(aes(x = x[41], xend = x[41], y = 0, yend = f[41]), colour = "blue") +
  geom_segment(aes(x = x[41], xend = x[41], y = f[41], yend = e[41]), colour = "red")



## We want to generate a random variable with pdf $f(x) = 60x^3(1-x^2)$, $0 \le x \le 1$. This is a Beta$(4, 3)$ distribution.


# pdf function, could use dbeta() instead
f <- function(x) {
    60*x^3*(1-x)^2
}

# plot pdf
x <- seq(0, 1, length.out = 100)
ggplot() +
  geom_line(aes(x, f(x)))


## -----------------------------------------------------------
envelope <- function(x) {
  ## create the envelope function
}

# Accept reject algorithm
n <- 1000 # number of samples wanted
accepted <- 0 # number of accepted samples
samples <- rep(NA, n) # store the samples here

while(accepted < n) {
  # sample y from g

  # sample u from uniform(0,1)
  u <- runif(1)

  if(u < f(y)/envelope(y)) {
    # accept
    accepted <- accepted + 1
    samples[accepted] <- y
  }
}


## ------------------------------------------------------------------------
ggplot() +
  geom_histogram(aes(sample, y = ..density..), bins = 50, ) +
  geom_line(aes(x, f(x)), colour = "red") +
  xlab("x") + ylab("f(x)")


## ---- message=FALSE------------------------------------------------------
library(tidyverse)

# function for squared r.v.s
squares <- function(x) x^2

sample_z <- function(n, p) {
  # store the samples
  samples <- data.frame(matrix(rnorm(n*p), nrow = n))

  samples %>% 
    mutate_all("squares") %>% # square the rvs
    rowSums() # sum over rows
}

# get samples
n <- 1000 # number of samples

# apply our function over different degrees of freedom
samples <- data.frame(chisq_2 = sample_z(n, 2),
                      chisq_5 = sample_z(n, 5),
                      chisq_10 = sample_z(n, 10),
                      chisq_100 = sample_z(n, 100))

# plot results
samples %>%
  gather(distribution, sample, everything()) %>% # make easier to plot w/ facets
  separate(distribution, into = c("dsn_name", "df")) %>% # get the df
  mutate(df = as.numeric(df)) %>% # make numeric
  mutate(pdf = dchisq(sample, df)) %>% # add density function values 
  ggplot() + # plot
  geom_histogram(aes(sample, y = ..density..)) + # samples
  geom_line(aes(sample, pdf), colour = "red") + # true pdf
  facet_wrap(~df, scales = "free")


## ------------------------------------------------------------------------
head(faithful)

faithful %>%
  gather(variable, value) %>%
  ggplot() +
  geom_histogram(aes(value), bins = 50) +
  facet_wrap(~variable, scales = "free")


## ------------------------------------------------------------------------
x <- seq(-5, 25, length.out = 100)

mixture <- function(x, means, sd) {
  # x is the vector of points to evaluate the function at
  # means is a vector, sd is a single number
  f <- rep(0, length(x))
  for(mean in means) { 
    f <- f + dnorm(x, mean, sd)/length(means) # why do I divide?
  }
  f
}

# look at mixtures of N(mu, 4) for different values of mu
data.frame(x, 
           f1 = mixture(x, c(5, 10, 15), 2), 
           f2 = mixture(x, c(5, 6, 7), 2),
           f3 = mixture(x, c(5, 10, 20), 2),
           f4 = mixture(x, c(1, 10, 20), 2)) %>%
  gather(mixture, value, -x) %>%
  ggplot() +
  geom_line(aes(x, value)) +
  facet_wrap(.~mixture, scales = "free_y")


## ------------------------------------------------------------------------
n <- 1000
u <- rbinom(n, 1, 0.5)

z <- u*rnorm(n) + (1 - u)*rnorm(n, 4, 1)

ggplot() +
  geom_histogram(aes(z), bins = 50)



## ----------------------------------------------------------
fish <- read_csv("https://stats.idre.ucla.edu/stat/data/fish.csv")


## ------------------------------------------------------------------------
# with zeroes
ggplot(fish) + geom_histogram(aes(count), binwidth = 1)

# without zeroes
fish %>%
  filter(count > 0) %>%
  ggplot() + 
  geom_histogram(aes(count), binwidth = 1)


## ------------------------------------------------------------------------
n <- 1000
lambda <- 5
pi <- 0.3

u <- rbinom(n, 1, pi)
zip <- u*0 + (1-u)*rpois(n, lambda)

# zero inflated model
ggplot() + geom_histogram(aes(zip), binwidth = 1)

# Poisson(5)
ggplot() + geom_histogram(aes(rpois(n, lambda)), binwidth = 1)


