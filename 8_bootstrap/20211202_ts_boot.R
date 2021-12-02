## -----------------------------
library(tidyverse)
set.seed(400)

## ------------------------------------------------------------------------
data(lynx)
plot(lynx)


## ------------------------------------------------------------------------
theta_hat <- mean(lynx)
theta_hat


## ------------------------------------------------------------------------
library(simpleboot)
B <- 10000

## Your turn: perform the independent bootstap
## what is the bootstrap estimate se?

boot.iid <- one.boot(lynx, "mean", B)

sd(boot.iid$t)
ggplot() + geom_histogram(aes(boot.iid$t))


## ------------------------------------------------------------------------
acf(lynx)


## ------------------------------------------------------------------------
# function to create non-overlapping blocks
nb <- function(x, b) {
  n <- length(x)
  l <- n %/% b
  
  blocks <- matrix(NA, nrow = b, ncol = l)
  for(i in 1:b) {
    blocks[i, ] <- x[((i - 1)*l + 1):(i*l)]
  }
  blocks
}

# Your turn: perform the NBB with b = 10 and l = 11
theta_hat_star_nbb <- rep(NA, B)
nb_blocks <- nb(lynx, 10)
for(i in 1:B) {
  # sample blocks
  idx <- sample(1:10, 10, replace = TRUE)
  y_star <- as.vector(t(nb_blocks[idx, ]))
  
  # get theta_hat^*
  theta_hat_star_nbb[i] <- mean(y_star)
}

# Plot your results to inspect the distribution
ggplot() +
  geom_histogram(aes(theta_hat_star_nbb))

# What is the estimated standard error of theta hat? The Bias?
sd(theta_hat_star_nbb)
mean(theta_hat_star_nbb) - theta_hat


## ------------------------------------------------------------------------
# function to create overlapping blocks
mb <- function(x, l) {
  n <- length(x)
  blocks <- matrix(NA, nrow = n - l + 1, ncol = l)
  for(i in 1:(n - l + 1)) {
    blocks[i, ] <- x[i:(i + l - 1)]
  }
  blocks
}

# Your turn: perform the MBB with l = 11
mb_blocks <- mb(lynx, 11)
theta_hat_star_mbb <- rep(NA, B)
for(i in 1:B) {
  # sample blocks
  idx <- sample(1:nrow(mb_blocks), 10, replace = TRUE)
  y_star <- as.vector(t(mb_blocks[idx, ]))
  
  # get theta_hat^*
  theta_hat_star_mbb[i] <- mean(y_star)
}

# Plot your results to inspect the distribution
ggplot() +
  geom_histogram(aes(theta_hat_star_mbb))

# What is the estimated standard error of theta hat? The Bias?
sd(theta_hat_star_mbb)
mean(theta_hat_star_mbb) - theta_hat

## ------------------------------------------------------------------------
# Your turn: Perform the mbb for multiple block sizes l = 1:15
# Create a plot of the se vs the block size. What do you notice?
mbb_se_bias <- function(x, l) {
  mb_blocks <- mb(x, l)
  theta_hat_star_mbb <- rep(NA, B)
  n <- length(x)
  for(i in 1:B) {
    # sample blocks
    idx <- sample(1:nrow(mb_blocks), floor(n/l), replace = TRUE)
    y_star <- as.vector(t(mb_blocks[idx, ]))
    
    # get theta_hat^*
    theta_hat_star_mbb[i] <- mean(y_star)
  }
  c(sd(theta_hat_star_mbb), mean(theta_hat_star_mbb) - mean(x))
}

l <- 1:15
mbb_se_l <- data.frame(se = numeric(length(l)), bias = numeric(length(l)))
for(i in l) {
  mbb_se_l[i,] <- mbb_se_bias(lynx, i)
}
ggplot() + geom_point(aes(l, mbb_se_l$se))
ggplot() + geom_point(aes(l, abs(mbb_se_l$bias)))

## bias indicates block length of 4 might be appropriate for this data
mb_blocks <- mb(lynx, 4)
theta_hat_star_mbb <- rep(NA, B)
for(i in 1:B) {
  # sample blocks
  idx <- sample(1:nrow(mb_blocks), floor(length(lynx)/4), replace = TRUE)
  y_star <- as.vector(t(mb_blocks[idx, ]))
  
  # get theta_hat^*
  theta_hat_star_mbb[i] <- mean(y_star)
}

# Plot your results to inspect the distribution
ggplot() +
  geom_histogram(aes(theta_hat_star_mbb))

# What is the estimated standard error of theta hat? The Bias?
sd(theta_hat_star_mbb)
mean(theta_hat_star_mbb) - theta_hat
