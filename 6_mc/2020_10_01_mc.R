## ------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
set.seed(100)

# X ~ Unif(0, 1)
# 1.
m <- 10000
x <- runif(m, 0, 1)

# 2. compute theta hat
theta_hat <- mean(x)

# X ~ Unif(10, 20)
y <- runif(m, 10, 20)
theta_hat <- mean(y)
