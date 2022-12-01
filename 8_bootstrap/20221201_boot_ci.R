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

## bias
mean(diff_means.boot$t - diff_means.boot$t0)

## se
sd(diff_means.boot$t)

## which confidence interval?
## BCa



## ------------------------------------------------------------------------
# Your turn: get the chosen CI using boot.ci
boot.ci(diff_means.boot, conf = .99, type = "all")


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
  model <- lm(rate ~ log(conc), data = dat[idx,])
  coef(model)
}

# use the boot function to get the bootstrap samples
boot_reg_paired <- boot(Puromycin, reg_func, 2000)

# examing the bootstrap sampling distribution, make histograms
plot(boot_reg_paired, index = 1)
plot(boot_reg_paired, index = 2)

colMeans(boot_reg_paired$t) - boot_reg_paired$t0

# get confidence intervals for beta_0 and beta_1 using boot.ci
boot.ci(boot_reg_paired, conf = 0.95, type = c("perc", "basic", "norm", "bca"), index = 1)
boot.ci(boot_reg_paired, conf = 0.95, type = c("perc", "basic", "norm", "bca"), index = 2)


## ------------------------------------------------------------------------
# Your turn
library(boot)

reg_func_2 <- function(dat, idx) {
  # write a regression function that returns fitted beta
  # from fitting a y that is created from the residuals
  m1 <- lm(rate ~ log(conc), dat)
  
  y_star <- m1$fitted.values + m1$residuals[idx]
  x <- m1$model[, -1]
  model_star <- lm(y_star ~ x)
  coef(model_star)
}

# use the boot function to get the bootstrap samples
boot_reg_resid <- boot(Puromycin, reg_func_2, 2000)

# examing the bootstrap sampling distribution, make histograms
plot(boot_reg_resid, index = 1)
plot(boot_reg_resid, index = 2)

colMeans(boot_reg_resid$t) - boot_reg_resid$t0

# get confidence intervals for beta_0 and beta_1 using boot.ci
boot.ci(boot_reg_resid, conf = 0.95, type = c("perc", "basic", "norm", "bca"), index = 1)
boot.ci(boot_reg_resid, conf = 0.95, type = c("perc", "basic", "norm", "bca"), index = 2)
