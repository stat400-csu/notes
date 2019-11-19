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
cor(law$LSAT, law$GPA)

# population correlation
cor(law82$LSAT, law82$GPA)


## ------------------------------------------------------------------------
# set up the bootstrap
B <- 1000
n <- nrow(law)
r <- numeric(B) # storage

set.seed(400)
for(b in 1:B) {
  ## Your Turn: Do the bootstrap!
  idx <- sample(1:n, n, replace = TRUE)
  sample <- law[idx, ]
  r[b] <- cor(sample$LSAT, sample$GPA)
}

# distribution
ggplot() +
  geom_histogram(aes(r)) +
  geom_vline(aes(xintercept = cor(law$LSAT, law$GPA)), colour = "blue") +
  geom_vline(aes(xintercept = cor(law82$LSAT, law82$GPA)), colour = "red", lty = 2)

# bias estimate
mean(r) - cor(law$LSAT, law$GPA)

# se
sd(r)

# another way
library(boot)

cor_func <- function(data, idx) {
  sample <- data[idx,]
  cor(sample[, 1], sample[, 2])
}

set.seed(400)
cor.boot <- boot(law, cor_func, B)
cor.boot
