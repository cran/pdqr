## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(pdqr)

set.seed(104)

## ----setup---------------------------------------------------------------
my_beta <- as_d(dbeta, shape1 = 2, shape2 = 5)
my_norm <- as_d(dnorm, mean = 0.5)
my_beta_mix <- form_mix(list(my_beta, my_beta + 1))

## ----basic_center--------------------------------------------------------
# Usage of `summ_center()`
summ_center(my_beta, method = "mean")
summ_center(my_beta, method = "median")
summ_center(my_beta, method = "mode")

# Usage of wrappers
summ_mean(my_beta)
summ_median(my_beta)
summ_mode(my_beta)

# `summ_mode()` can compute local modes instead of default global
summ_mode(my_beta_mix, method = "local")

## ----basic_spread--------------------------------------------------------
# Usage of `summ_spread()`
summ_spread(my_beta, method = "sd")
summ_spread(my_beta, method = "var")
summ_spread(my_beta, method = "iqr")
summ_spread(my_beta, method = "mad")
summ_spread(my_beta, method = "range")

# Usage of wrappers
summ_sd(my_beta)
summ_var(my_beta)
summ_iqr(my_beta)
summ_mad(my_beta)
summ_range(my_beta)

## ----basic_moments-------------------------------------------------------
summ_moment(my_beta, order = 3)
summ_moment(my_beta, order = 3, central = TRUE)
summ_moment(my_beta, order = 3, standard = TRUE)
summ_moment(my_beta, order = 3, absolute = TRUE)

## ----basic_skewness-kurtosis---------------------------------------------
summ_skewness(my_beta)

# This by default computes excess kurtosis
summ_kurtosis(my_beta)

  # Use `excess = FALSE` to compute non-excess kurtotsis
summ_kurtosis(my_beta, excess = FALSE)

## ----basic_quantiles-----------------------------------------------------
summ_quantile(my_beta, probs = seq(0, 1, by = 0.25))

## ----basic_entropy-------------------------------------------------------
summ_entropy(my_beta)
summ_entropy(new_d(1:10, type = "discrete"))

## ----basic_entropy2------------------------------------------------------
summ_entropy2(my_beta, my_norm)
summ_entropy2(my_norm, my_beta)
summ_entropy2(my_norm, my_beta, clip = exp(-10))
summ_entropy2(my_beta, my_norm, method = "cross")

## ----regions_interval----------------------------------------------------
summ_interval(my_beta, level = 0.9, method = "minwidth")
summ_interval(my_beta, level = 0.9, method = "percentile")
summ_interval(my_beta, level = 0.9, method = "sigma")

## ----regions_hdr---------------------------------------------------------
# Unimodal distribution
summ_hdr(my_beta, level = 0.9)

# Multimodal distribution
summ_hdr(my_beta_mix, level = 0.9)

  # Compare this to single interval of minimum width
summ_interval(my_beta_mix, level = 0.9, method = "minwidth")

## ----regions_family------------------------------------------------------
beta_mix_hdr <- summ_hdr(my_beta_mix, level = 0.9)
beta_mix_interval <- summ_interval(my_beta_mix, level = 0.9)

# Test if points are inside region
region_is_in(beta_mix_hdr, x = seq(0, 2, by = 0.5))

# Compute total probability of a region
region_prob(beta_mix_hdr, f = my_beta_mix)

  # Pdqr-function doesn't need to be the same as used for computing region
region_prob(beta_mix_hdr, f = my_norm)

# Compute height of region: minimum value of d-function inside region
region_height(beta_mix_hdr, f = my_beta_mix)

# Compute width of region: sum of interval widths
region_width(beta_mix_hdr)

  # Compare widths with single interval
region_width(beta_mix_interval)

# Draw region on existing plot
plot(my_beta_mix, main = "90% highest density region")
region_draw(beta_mix_hdr)

## ----distance------------------------------------------------------------
# Kolmogorov-Smirnov distance
summ_distance(my_beta, my_norm, method = "KS")

# Total variation distance
summ_distance(my_beta, my_norm, method = "totvar")

# Probability of one distribution being bigger than other, normalized to [0;1]
summ_distance(my_beta, my_norm, method = "compare")

# Wassertein distance: "average path density point should travel while
# transforming from one into another"
summ_distance(my_beta, my_norm, method = "wass")

# Cramer distance: integral of squared difference of p-functions
summ_distance(my_beta, my_norm, method = "cramer")

# "Align" distance: path length for which one of distribution should be "moved"
# towards the other so that they become "aligned" (probability of one being
# greater than the other is 0.5)
summ_distance(my_beta, my_norm, method = "align")

# "Entropy" distance: `KL(f, g) + KL(g, f)`, where `KL()` is Kullback-Leibler
# divergence. Usually should be used for distributions with same support, but
# works even if they are different (with big numerical penalty).
summ_distance(my_beta, my_norm, method = "entropy")

## ----sep-class_separation------------------------------------------------
summ_separation(my_beta, my_norm, method = "KS")
summ_separation(my_beta, my_norm, method = "F1")

## ----sep-class_metrics---------------------------------------------------
# Many threshold values can be supplied
thres_vec <- seq(0, 1, by = 0.2)
summ_classmetric(f = my_beta, g = my_norm, threshold = thres_vec, method = "F1")

# In `summ_classmetric_df()` many methods can be supplied
summ_classmetric_df(
  f = my_beta, g = my_norm, threshold = thres_vec, method = c("GM", "F1", "MCC")
)

## ----sep-class_roc-------------------------------------------------------
my_roc <- summ_roc(my_beta, my_norm)
head(my_roc)
summ_rocauc(my_beta, my_norm)
roc_plot(my_roc)

## ----ordering------------------------------------------------------------
# Here the only clear "correct" ordering is that `a <= b`.
f_list <- list(a = my_beta, b = my_beta + 1, c = my_norm)

# Returns an integer vector representing a permutation which rearranges f_list
# in desired order
summ_order(f_list, method = "compare")

  # In this particular case of `f_list` all orderings agree with each other, but
  # generally this is not the case: for any pair of methods there is a case
  # when they disagree with each other
summ_order(f_list, method = "mean")
summ_order(f_list, method = "median")
summ_order(f_list, method = "mode")

  # Use `decreasing = TRUE` to sort decreasingly
summ_order(f_list, method = "compare", decreasing = TRUE)

# Sort list
summ_sort(f_list)
summ_sort(f_list, decreasing = TRUE)

# Rank elements: 1 indicates "the smallest", `length(f_list)` - "the biggest"
summ_rank(f_list)

## ----other_prob----------------------------------------------------------
summ_prob_true(my_beta >= my_norm)
summ_prob_false(my_beta >= 2*my_norm)

## ----other_pval----------------------------------------------------------
# By default two-sided p-value is computed
summ_pval(my_beta, obs = 0.7)
summ_pval(my_beta, obs = 0.7, method = "left")
summ_pval(my_beta, obs = 0.7, method = "right")

# Multiple values are adjusted with `p.adjust()` with "holm" method by default
obs_vec <- seq(0, 1, by = 0.1)
summ_pval(my_beta, obs = obs_vec)

  # Use `adjust = "none"` to not adjust
summ_pval(my_beta, obs = obs_vec, adjust = "none")

