## correlation is a measure of linear relationship between two variables
## correlation of 0 does not mean "no relationship"
x <- -5:5
y <- x^2

library(ggplot2)

ggplot() +
  geom_point(aes(x, y))

cov(x, y)
