## ------------------------------------------------------------------------
# simple math
5*(10 - 4) + 44

# integer division
7 %/% 2

# modulo operator (Remainder)
7 %% 2

# powers
1.5^3


## ------------------------------------------------------------------------
# exponentiation
exp(1)

# logarithms
log(100)
log(100, base = 10)

# trigonometric functions
sin(pi/2)
cos(pi)
asin(1)


## ------------------------------------------------------------------------
# create some variables
x <- 5
class <- 400
hello <- "world"


## ------------------------------------------------------------------------
# functions of variables
log(x)
class^2


## ------------------------------------------------------------------------
# store a vector
y <- c(1, 2, 6, 10, 17)


## ------------------------------------------------------------------------
# elementwise function
y/2

## Your Turn --------------------------------------------------------------
# 1. Use the `rep()` function to construct the following vector: `1 1 2 2 3 3 4 4 5 5`
c(rep(1, 2), rep(2, 2), rep(3, 2), rep(4, 2), rep(5, 2))
rep(seq(1, 5), each = 2)

# 2. Use `rep()` to construct this vector: `1 2 3 4 5 1 2 3 4 5 1 2 3 4 5`
rep(seq(1, 5), times = 3)

## ------------------------------------------------------------------------
# sequences
a <- 1:5
a
b <- seq(1, 5, by = 0.5)
b


## ------------------------------------------------------------------------
a[3]


## ------------------------------------------------------------------------
# indexing multiple items
a[c(1, 3, 5)]
a[1:3]


## ------------------------------------------------------------------------
a[-3]


## ------------------------------------------------------------------------
# indexing by vectors of logicals
a[c(TRUE, TRUE, FALSE, FALSE, FALSE)]

# indexing by calculated logicals
a < 3
a[a < 3]

# Your Turn ---------------------------------------------------------------
# 1. Create a vector of 1300 values evenly spaced between 1 and 100.
x <- seq(1, 100, length.out = 1300)

# 2. How many of these values are greater than 91? (Hint: see `sum()` as a helpful functions.)
sum(x > 91)

## ------------------------------------------------------------------------
c(TRUE, TRUE, FALSE) | c(FALSE, TRUE, FALSE)
c(TRUE, TRUE, FALSE) & c(FALSE, TRUE, FALSE)


## ------------------------------------------------------------------------
head(x, 2)
tail(x, 2)


## ------------------------------------------------------------------------
a[1] <- 0
a[c(4, 5)] <- 100
a

## Your Turn --------------------------------------------------------------
# Using the vector you created of 1300 values evenly spaced between 1 and 100,
# 1. Modify the elements greater than 90 to equal 9999.
x[x > 90] <- 9999

# 2. View (not modify) the first 10 values in your vector.
head(x, 10)
x[1:10]

tail(x, 10)

## ------------------------------------------------------------------------
a
a[1] <- ":-("
a


## ------------------------------------------------------------------------
as.character(b)


## ------------------------------------------------------------------------
n <- length(b)
n
sum_b <- sum(b)
sum_b


## ------------------------------------------------------------------------
mean_b <- sum_b/n
sd_b <- sqrt(sum((b - mean_b)^2)/(n - 1))


## ------------------------------------------------------------------------
mean(b)
sd(b)
summary(b)
quantile(b, c(.25, .75))


## ------------------------------------------------------------------------
# look at top 6 rows
head(iris)

# structure of the object
str(iris)


## ------------------------------------------------------------------------
iris[1, ]
iris[, 1]
iris[1, 1]


## ------------------------------------------------------------------------
iris$Species
iris[, "Species"]


## ------------------------------------------------------------------------
# make a copy of iris
my_iris <- iris

# add a column
my_iris$sepal_len_square <- my_iris$Sepal.Length^2  
head(my_iris)


## ------------------------------------------------------------------------
my_iris[my_iris$sepal_len_square < 20, ]


## ------------------------------------------------------------------------
df <- data.frame(NUMS = 1:5, 
                 lets = letters[1:5],
                 cols = c("green", "gold", "gold", "gold", "green"))


## ------------------------------------------------------------------------
names(df)
names(df)[1] <- "nums"

df

## Your Turn --------------------------------------------------------------
# 1. Make a data frame with column 1: `1,2,3,4,5,6` and column 2: `a,b,a,b,a,b`
yt <- data.frame(nums = c(1, 2, 3, 4, 5, 6),
                 nums2 = 1:6,
                 nums3 = seq(1, 6),
                 lets = rep(c("a", "b"), 3),
                 lets2 = rep(letters[1:2], 3))


# 2. Select only rows with value "a" in column 2 using logical vector
yt[yt$lets == "a",]

# 3. `mtcars` is a built-in data set like `iris`: Extract the 4th row of the mtcars data.
mtcars[4, ]



## ------------------------------------------------------------------------
# we store a function in a named value
# function is itself a function to create functions!
# we specify the inputs that we can use inside the function
# we can specify default values, but it is not necessary
name <- function(input = FALSE) {
  # body code goes here
  
  # return output vaues
  return(input)
}


## ------------------------------------------------------------------------
my_mean <- function(x) {
  sum(x)/length(x)
}


## ------------------------------------------------------------------------
my_mean(1:15)
my_mean(c(1:15, NA))


## ----eval=FALSE----------------------------------------------------------
## if(condition) {
##   # Some code that runs if condition is TRUE
## } else {
##   # Some code that runs if condition is TRUE
## }


## ----eval=FALSE----------------------------------------------------------
## if(na.rm) x <- na.omit(x) # na.omit is a function that removes NA values


## ----eval=FALSE----------------------------------------------------------
## for(i in index values) {
##   # block of code
##   # can print values also
##   # code in here will most likely depend on i
## }


## ------------------------------------------------------------------------
for(i in 1:3) {
  print(i)
}


## ------------------------------------------------------------------------
for(species in unique(iris$Species)) {
  subset_iris <- iris[iris$Species == species,]
  avg <- mean(subset_iris$Sepal.Length)
  print(paste(species, avg))
}


## ------------------------------------------------------------------------
condition <- TRUE
while(condition) {
  # do stuff
  # don't forget to eventually set the condition to false
  # in the toy example below I check if the current seconds is divisible by 5
  time <- Sys.time()
  if(as.numeric(format(time, format = "%S")) %% 5 == 0) condition <- FALSE
}
print(time)


## ------------------------------------------------------------------------
# we can also use while loops to iterate
i <- 1
while (i <= 5) {
    print(i)
    i <- i + 1
}


## Your Turn --------------------------------------------------------------
# 1. Alter your `my_mean()` function to take a second argument (`na.rm`) with default value `FALSE` that removes `NA` values if `TRUE`.
my_mean <- function(x, na.rm = FALSE) {
  if(na.rm) x <- na.omit(x)
  sum(x)/length(x)
}

# 2. Add checks to your function to make sure the input data is either numeric or logical. If it is logical convert it to numeric.
# 3. The diamonds data set is included in the `ggplot2` package (not by default in `R`). It can be read into your environment with the following function. Loop over the columns of the diamonds data set and apply your mean function to all of the numeric columns (Hint: look at the `class()` function).

data("diamonds", package = "ggplot2")

