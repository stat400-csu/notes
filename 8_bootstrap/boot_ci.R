## -----------------------------
library(tidyverse)
library(knitr)
set.seed(400)


## -------------------------
library(resample) # package containing the data

data(Verizon)
head(Verizon)

Verizon %>%
  group_by(Group) %>%
  summarize(mean = mean(Time), sd = sd(Time), min = min(Time), max = max(Time)) %>%
  kable()

ggplot(Verizon) +
  geom_histogram(aes(Time)) +
  facet_wrap(.~Group, scales = "free")

ggplot(Verizon) +
  geom_boxplot(aes(Group, Time))


## ---------------------------------------------------------
library(boot) # package containing the bootstrap function

mean_func <- function(x, idx) {
  mean(x[idx])
}

ilec_times <- Verizon[Verizon$Group == "ILEC",]$Time
boot.ilec <- boot(ilec_times, mean_func, 2000)

plot(boot.ilec)


## ------------------------------------------------------------------------
boot.ci(boot.ilec, conf = .95, type = c("perc", "basic", "norm", "bca"))

## we can do some of these on our own
## normal
mean(boot.ilec$t) + c(-1, 1)*qnorm(.975)*sd(boot.ilec$t)

## normal is bias corrected
2*mean(ilec_times) - (mean(boot.ilec$t) - c(-1, 1)*qnorm(.975)*sd(boot.ilec$t))

## percentile
quantile(boot.ilec$t, c(.025, .975))

## basic
2*mean(ilec_times) - quantile(boot.ilec$t, c(.975, .025))



## ------------------------------------------------------------------------
mean_var_func <- function(x, idx) {
  c(mean(x[idx]), var(x[idx])/length(idx))
}

boot.ilec_2 <- boot(ilec_times, mean_var_func, 2000)
boot.ci(boot.ilec_2, conf = .95, type = "stud")


## --------
library(simpleboot)

clec_times <- Verizon[Verizon$Group == "CLEC",]$Time

diff_means.boot <- two.boot(ilec_times, clec_times, "mean", R = 2000)

ggplot() +
  geom_histogram(aes(diff_means.boot$t)) +
  xlab("mean(ilec) - mean(clec)")

qqnorm(diff_means.boot$t) 
qqline(diff_means.boot$t)


## ------------------------------------------------------------------------
# Your turn: estimate the bias and se of the sampling distribution


## ------------------------------------------------------------------------
# Your turn: get the chosen CI using boot.ci


## ------------------------
head(Puromycin)
dim(Puromycin)

ggplot(Puromycin) +
  geom_point(aes(conc, rate))

ggplot(Puromycin) +
  geom_point(aes(log(conc), (rate)))


## ---- fig.height = 2.5, fig.width = 3.25, fig.show="hold"----------------
m0 <- lm(rate ~ conc, data = Puromycin)
plot(m0)
summary(m0)
confint(m0)

m1 <- lm(rate ~ log(conc), data = Puromycin)
plot(m1)
summary(m1)
confint(m1)


## ------------------------------------------------------------------------
# Your turn
library(boot)

reg_func <- function(dat, idx) {
  # write a regression function that returns fitted beta
}

# use the boot function to get the bootstrap samples

# examing the bootstrap sampling distribution, make histograms

# get confidence intervals for beta_0 and beta_1 using boot.ci



## ------------------------------------------------------------------------
# Your turn
library(boot)

reg_func_2 <- function(dat, idx) {
  # write a regression function that returns fitted beta
  # from fitting a y that is created from the residuals
  
}

# use the boot function to get the bootstrap samples

# examing the bootstrap sampling distribution, make histograms

# get confidence intervals for beta_0 and beta_1 using boot.ci


