## ----eval = FALSE--------------------------------------------------------
## install.packages("ggplot2")


## ------------------------------------------------------------------------
library(ggplot2)


## ------------------------------------------------------------------------
anscombe


## ------------------------------------------------------------------------
head(diamonds)
ggplot(data = diamonds)


## ------------------------------------------------------------------------
ggplot(data = diamonds, mapping = aes(carat, price))


## ------------------------------------------------------------------------
ggplot(data = diamonds, mapping = aes(carat, price)) +
  geom_point()


## ------------------------------------------------------------------------
ggplot(data = diamonds, mapping = aes(carat, price)) +
  geom_point(aes(color = cut))


## ------------------------------------------------------------------------
ggplot(data = diamonds, mapping = aes(carat, price)) +
  geom_point(aes(color = cut)) +
  geom_smooth(aes(color = cut), method = "lm")


## ------------------------------------------------------------------------
ggplot(data = diamonds, mapping = aes(carat, price)) +
  geom_point(aes(color = cut)) +
  geom_smooth(aes(color = cut), method = "lm") +
  scale_y_sqrt()
  


## ------------------------------------------------------------------------
dim(mpg)
summary(mpg)
head(mpg)


## ------------------------------------------------------------------------
ggplot(data = mpg) +
  geom_point(mapping = aes(displ, hwy))


## ------------------------------------------------------------------------
ggplot(data = mpg) +
  geom_point(mapping = aes(displ, hwy, colour = class))


## ------------------------------------------------------------------------
ggplot(data = mpg) +
  geom_point(mapping = aes(displ, hwy), colour = "darkgreen", size = 2, alpha = 0.5, shape = 2)

## Your Turn --------------------------------------------------------------
# 1. Make a scatterplot of `cty` vs. `hwy` mpg using the `mpg` dataset.
# 2. Describe the relationship that you see.
# 3. Map color and shape to type of drive the car is (see `?mpg` for details on the variables.). Do you see any patterns?
# 4. Alter your plot fro part 3. to make all the points be larger.

ggplot(data = mpg) +
  geom_point(aes(cty, hwy, color = drv, shape = drv), size = 10)



## ------------------------------------------------------------------------
ggplot(data = mpg) +
  geom_histogram(mapping = aes(hwy), bins = 30) 
## boxplots will look very different sometimes with different binwidths

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(drv, hwy)) 
## boxplots allow us to see the distribution of a cts rv conditional on a discrete one
## we can also show the actual data at the same time
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(drv, hwy)) +
  geom_jitter(mapping = aes(drv, hwy), alpha = .5)

ggplot(data = mpg) +
  geom_bar(mapping = aes(drv)) 
## shows us the distribution of a categorical variable


## ------------------------------------------------------------------------
ggplot(data = diamonds, mapping = aes(carat, price)) +
  geom_point(aes(color = cut))


## ------------------------------------------------------------------------
ggplot(data = diamonds, mapping = aes(carat, price)) +
  geom_point(aes(color = cut)) +
  facet_wrap(. ~ cut)


## ------------------------------------------------------------------------
ggplot(data = diamonds, mapping = aes(carat, price)) +
  geom_point(aes(color = cut)) +
  facet_grid(color ~ cut)

## Your Turn --------------------------------------------------------------
# 1. Make a histogram of `hwy`, faceted by `drv`.
# 2. Make a scatterplot that incorporates color, shape, size, and facets.
# 3. BONUS - Color your histograms from 1. by `cyl`. Did this do what you thought it would? (Look at `fill` and `group` as options instead).

ggplot(data = mpg) +
  geom_histogram(aes(hwy, fill = as.character(cyl), group = as.character(cyl)), bins = 10, position = "dodge") +
  facet_wrap(.~drv)

