## ----------------------------
library(tidyverse)
set.seed(400)


## ---------------------------------------------------------
# observed data
x <- c(2, 2, 1, 1, 5, 4, 4, 3, 1, 2)

# create 10 bootstrap samples
x_star <- matrix(NA, nrow = length(x), ncol = 10)
for(i in 1:10) {
  x_star[, i] <- sample(x, length(x), replace = TRUE)
}
x_star

# compare mean of the same to the means of the bootstrap samples
mean(x)
colMeans(x_star)

ggplot() + 
  geom_histogram(aes(colMeans(x_star)), binwidth = .05) +
  geom_vline(aes(xintercept = mean(x)), lty = 2, colour = "red") +
  xlab("Sampling distribution of the mean via bootstrapping")


## -------------------------------------------------------
library(bootstrap)

head(law)

ggplot() +
  geom_point(aes(LSAT, GPA), data = law) +
  geom_point(aes(LSAT, GPA), data = law82, pch = 1)


## ------------------------------------------------------------------------
# sample correlation
theta_hat <- cor(law$LSAT, law$GPA)

# population correlation
theta <- cor(law82$LSAT, law82$GPA)


## ------------------------------------------------------------------------
# set up the bootstrap
B <- 1000
n <- nrow(law)
theta_hat_star <- numeric(B) # storage

for(b in seq_len(B)) {
  ## Your Turn: Do the bootstrap!
  ## get bs dataset
  idx_star <- sample(seq_len(n), n, replace = TRUE)
  x_star <- law[idx_star,]
  
  ## computing the statistic
  theta_hat_star[b] <- cor(x_star$LSAT, x_star$GPA)
}

ggplot() +
  geom_histogram(aes(theta_hat_star)) +
  geom_vline(aes(xintercept = theta), color = "red") +
  geom_vline(aes(xintercept = theta_hat), color = "blue", lty = 2)

sd(theta_hat_star)  

## bias
mean(theta_hat_star) - theta_hat
