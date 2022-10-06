## ------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
set.seed(100)

## unif(0,1)
m <- 10000
x <- runif(m, 0, 1)
mean(x)

## unif(10,20)
y <- runif(m, 10, 20)
mean(y)
