## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(pdqr)

set.seed(103)

## ----form_trans_random---------------------------------------------------
# Transformation function should be vectorized
trans_fun <- function(x, y) {sin(x * y)}
d_norm <- as_d(dnorm)
d_unif <- as_d(dunif)

# Output's support has values outside of [-1; 1] interval, which is impossible
# with `sin()` function. This is because of "extending property" of `density()`.
(d_transformed <- form_trans(list(d_norm, d_unif), trans = trans_fun))

# One way of dealing with this is to set `cut = 0` as argument for `density()`
# which is called in `new_*()`. The other way is to use `form_resupport()`.
(d_transformed_2 <- form_trans(
  list(d_norm, d_unif), trans = trans_fun,
  args_new = list(cut = 0)
))

plot(d_transformed, col = "black", main = "Transformations of distribution")
  # `d_transformed_2()` differs slightly because of randomness involved
lines(d_transformed_2, col = "blue")

## ----form_trans_bruteforce-----------------------------------------------
d_binom <- as_d(dbinom, size = 10, prob = 0.3)

(d_binom_transformed <- form_trans(
  list(d_binom, d_binom), trans = trans_fun, method = "bruteforce"
))
head(meta_x_tbl(d_binom_transformed))

# Compare "bruteforce" output to "random" output
d_binom_transformed_random <- form_trans(
  list(d_binom, d_binom), trans = trans_fun, method = "random"
)
head(meta_x_tbl(d_binom_transformed_random))

## ----form_resupport------------------------------------------------------
plot(
  d_transformed, col = "black",
  main = "Different methods of `form_resupport()`"
)

# Reflect density tails outside of desired support to be inside
lines(
  form_resupport(d_transformed, support = c(-1, 1), method = "reflect"),
  col = "blue"
)

# Remove those tails while renormalizing density
lines(
  form_resupport(d_transformed, support = c(-1, 1), method = "trim"),
  col = "red"
)

# Concentrate those tails on edges of desired support
lines(
  form_resupport(d_transformed, support = c(-1, 1), method = "winsor"),
  col = "green"
)

# Warp support linear to be desired support
lines(
  form_resupport(d_transformed, support = c(-1, 1), method = "linear"),
  col = "magenta"
)

## ----form_tails----------------------------------------------------------
plot(d_norm, col = "black", main = "Different methods of `form_tails()`")

# Remove tail(s) completely with `method = "trim"`. By default, tails from both
# sides are removed
lines(form_tails(d_norm, level = 0.05, method = "trim"), col = "blue")

# Concentrate probability on edge(s) with `method = "winsor"`
lines(
  form_tails(d_norm, level = 0.1, method = "winsor", direction = "right"),
  col = "red"
)

# Use `form_resupport()` and `as_q()` to remove different levels from both
# directions. Here 0.1 level tail from left is removed, and 0.05 level from
# right
new_supp <- as_q(d_norm)(c(0.1, 1-0.05))
form_resupport(d_norm, support = new_supp, method = "trim")

## ----form_recenter-form_respread-----------------------------------------
my_beta <- as_d(dbeta, shape1 = 1, shape2 = 3)

# Distribution is shifted to the right so as to have mean (default method of
# `summ_center()`) equal to 2
my_beta2 <- form_recenter(my_beta, to = 2)
summ_center(my_beta2)

# Distribution is stretched around its center so as to have range equal to 10
my_beta3 <- form_respread(my_beta2, to = 10, method = "range")
summ_spread(my_beta3, method = "range")
  # Center remains unchainged
summ_center(my_beta3)

## ----form_mix------------------------------------------------------------
# All inputs have the same type
dis_list <- list(
  as_d(dbinom, size = 10, prob = 0.3),
  as_d(dpois, lambda = 10)
)
plot(form_mix(dis_list), main = "Mixture of binomial and Poisson")
norm_list <- list(as_d(dnorm), as_d(dnorm, mean = 4))
plot(
  form_mix(norm_list, weights = c(0.3, 0.7)),
  main = "Mixture of normal distributions"
)

# Here all "discrete" pdqr-functions are converted to be "continuous" with
# values represented as dirac-like density "spikes"
mixed_both <- form_mix(c(norm_list, dis_list))
plot(mixed_both, main = 'Density of mixture of "discrete" and "continuous"')
plot(as_p(mixed_both), main = 'CDF of mixture of "discrete" and "continuous"')

## ----form_estimate-------------------------------------------------------
(unif_mean <- form_estimate(d_unif, stat = mean, sample_size = 20))
plot(
  unif_mean, main = "Distribution of mean statistic for 20 elements of uniform"
)

  # Approximated normal output
lines(as_d(dnorm, mean = 0.5, sd = 1/sqrt(12*20)), col = "red")

# Estimation of 75% quantile. Here once again one can see the "extending
# property" of `density()` function used in `new_*()`, because right edge of
# support is more than 1, which is impossible.
(unif_quan <- form_estimate(
  d_unif, stat = quantile, sample_size = 20, probs = 0.75
))
plot(
  unif_quan,
  main = "Distribution of 75% quantile statistic for 20 elements of uniform"
)

  # "Correct" distribution of 15th order statistic of the uniform distribution
  # with sample size 20
lines(as_d(dbeta, shape1 = 15, shape2 = 6), col = "red")

## ----base_Math-----------------------------------------------------------
# Exponent of uniform distribution
exp(d_unif)

## ----base_Ops------------------------------------------------------------
# Distribution of used in `form_trans()` section transformation function. Note
# the correct support [-1, 1] without effect of "extending property" of
# `density()`. Here the default method of `form_resupport()` is used.
sin(d_norm * d_unif)

# Comparing random variables results into boolean random variable represented
# by boolean pdqr-function.
# Here it means that random value of `d_norm` will be greater than random value
# of `d_unif` with probability around 0.316.
d_norm > d_unif

## ----base_Summary--------------------------------------------------------
# Distribution of maximum of three random variables
max(d_norm, d_norm, d_norm)

# Probability that all inequalities are true
summ_prob_true(all(d_norm > d_unif, d_norm > 2*d_unif))

