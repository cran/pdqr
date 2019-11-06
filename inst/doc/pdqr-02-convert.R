## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(pdqr)

set.seed(102)

## ----existing------------------------------------------------------------
d_fin <- new_d(1:4, "discrete")

meta_x_tbl(d_fin)

# This is equivalent to `new_p(1:4, "discrete")`
(p_fin <- as_p(d_fin))
meta_x_tbl(p_fin)

## ----honored-------------------------------------------------------------
# "Honored" distributions
as_d(dnorm)

  # Underlying distribution doesn't depend on class ("p", "d", "q", "r").
  # Following code has the same effect as `as_r(as_d(dnorm))`
as_r(rnorm)

  # Different picewise-linear approximation precision is achieved with different
  # `n_grid` argument value
as_d(dnorm, n_grid = 101)

  # Different extra arguments for input
as_d(dnorm, mean = 10, sd = 0.1)

  # Currently only five distributions result into "discrete" output of `as_*()`
as_d(dbinom, size = 10, prob = 0.3)
as_d(dgeom, prob = 0.3)
as_d(dhyper, m = 10, n = 10, k = 7)
as_d(dnbinom, size = 10, prob = 0.3)
as_d(dpois, lambda = 1)

# This isn't recognized as "honored", but output is very close to "honored"
as_d(function(x) {dnorm(x)})

## ----support-detection_demo----------------------------------------------
my_d <- function(x) {ifelse(x >= -1 & x <= 1, 0.75 * (1 - x^2), 0)}

  # With default support detection
as_d(my_d)

  # Providing custom, maybe only partially known, support
as_d(my_d, support = c(-1, NA))
as_d(my_d, support = c(NA, 1))
as_d(my_d, support = c(-1, 1))

## ----support-detection_performance---------------------------------------
(p_norm <- as_p(function(x) {pnorm(x)}))
(d_norm <- as_d(function(x) {dnorm(x)}))
(q_norm <- as_q(function(x) {qnorm(x)}))
(r_norm <- as_r(function(x) {rnorm(x)}))

plot(
  as_d(p_norm), col = "black",
  main = "Comparison of `as_*()` functions support detection"
)
lines(d_norm, col = "blue")
lines(as_d(q_norm), col = "red")
lines(as_d(r_norm), col = "green")

## ----support_detection_infinity------------------------------------------
x_grid <- seq(0, 0.06, by = 1e-5)

# "Honored" distribution
plot(
  as_d(dchisq, df = 1), col = "black",
  xlim = c(0, 0.05), ylim = c(0, 20),
  main = "Infinity imputation for Chi-squared distribution"
)
lines(x_grid, dchisq(x_grid, df = 1), col = "red")

# Custom function
plot(
  as_d(function(x) {-log(x)}, support = c(0, 1)), col = "black",
  xlim = c(0, 0.001), ylim = c(6, 12),
  main = "Infinity imputation for custom function"
)
lines(x_grid, -log(x_grid), col = "red")

## ----pdqr_approx_error_demo----------------------------------------------
approx_err <- pdqr_approx_error(as_d(dnorm, sd = 2), dnorm, sd = 2)
head(approx_err)
summary(approx_err)

## ----pdqr_approx_error_common--------------------------------------------
abserror_stat <- function(f, ref_f, ...) {
  approx_err <- pdqr_approx_error(f, ref_f, ...)
  
  c(
    median_abserror = median(approx_err[["abserror"]]),
    max_abserror = max(approx_err[["abserror"]])
  )
}

abserror_stat_fin <- function(f, ref_f, grid, ...) {
  abserror <- abs(f(grid) - ref_f(grid, ...))
  
  c(median_abserror = median(abserror), max_abserror = max(abserror))
}

# Normal
abserror_stat(as_d(dnorm), dnorm)

# Beta
abserror_stat(
  as_d(dbeta, shape1 = 10, shape2 = 20), dbeta, shape1 = 10, shape2 = 20
)

  # By default, `pdqr_approx_error()` removes infinity errors. As one can see,
  # when density goes to infinity, error can be quite big
abserror_stat(
  as_d(dbeta, shape1 = 0.1, shape2 = 0.2), dbeta, shape1 = 0.1, shape2 = 0.2
)

# Exponential
abserror_stat(as_d(dexp, rate = 10), dexp, rate = 10)

# Student
abserror_stat(as_d(dt, df = 5), dt, df = 5)

# Cauchy. Heavy tails also affect approximation error
abserror_stat(as_d(dcauchy), dcauchy)

# Poisson. Pdqr-function isn't exact because of tail trimming.
abserror_stat_fin(as_d(dpois, lambda = 10), dpois, grid = 0:30, lambda = 10)

# For some distributions functions are exact
# Uniform
abserror_stat(as_d(dunif), dunif)

# Binomial
abserror_stat_fin(
  as_d(dbinom, size = 10, prob = 0.1), dbinom, grid = 0:10,
  size = 10, prob = 0.1
)

