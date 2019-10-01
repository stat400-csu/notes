## ------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
set.seed(100)

# estimate EX for unif(0, 1) and unif(10, 20)
m <- 1000
x <- runif(m)
y <- runif(m, 10, 20)
mean(x) # expect this will be close to 0.5
mean(y) # expect this will be close to 15
