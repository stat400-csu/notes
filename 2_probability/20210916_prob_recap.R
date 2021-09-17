library(ggplot2)

## covariance = 0 does not mean no relationship
## only means no *linear* relationship
x <- -5:5
y <- x^2

ggplot() +
  geom_point(aes(x, y)) +
  geom_line(aes(x, y))

cov(x, y)
