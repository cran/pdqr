## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(pdqr)

set.seed(101)

## ----setup_data-frame-inputs---------------------------------------------
# For type "discrete"
dis_df <- data.frame(x = 1:4, prob = 4:1 / 10)
# For type "continuous"
con_df <- data.frame(x = 1:4, y = c(0, 1, 1, 1))

## ----p-fun_sample--------------------------------------------------------
# Treating input as discrete
p_mpg_dis <- new_p(mtcars$mpg, type = "discrete")
p_mpg_dis

# Treating input as continuous
p_mpg_con <- new_p(mtcars$mpg, type = "continuous")
p_mpg_con

# Outputs are actually vectorized functions
p_mpg_dis(15:20)
p_mpg_con(15:20)

# You can plot them directly using base `plot()` and `lines()`
plot(p_mpg_con, main = "P-functions from sample")
lines(p_mpg_dis, col = "blue")

## ----p-fun_data-frame----------------------------------------------------
p_df_dis <- new_p(dis_df, type = "discrete")
p_df_dis

p_df_con <- new_p(con_df, type = "continuous")
p_df_con

plot(p_df_con, main = "P-functions from data frame")
lines(p_df_dis, col = "blue")

## ----d-fun_sample--------------------------------------------------------
# Treating input as discrete
d_mpg_dis <- new_d(mtcars$mpg, type = "discrete")
d_mpg_dis

# Treating input as continuous
d_mpg_con <- new_d(mtcars$mpg, type = "continuous")
d_mpg_con

# Outputs are actually vectorized functions
d_mpg_dis(15:20)
d_mpg_con(15:20)

# You can plot them directly using base `plot()` and `lines()`
op <- par(mfrow = c(1, 2))
plot(d_mpg_con, main = '"continuous" d-function\nfrom sample')
plot(d_mpg_dis, main = '"discrete" d-function\nfrom sample', col = "blue")
par(op)

## ----d-fun_data-frame----------------------------------------------------
d_df_dis <- new_d(dis_df, type = "discrete")
d_df_dis

d_df_con <- new_d(con_df, type = "continuous")
d_df_con

op <- par(mfrow = c(1, 2))
plot(d_df_con, main = '"continuous" d-function\nfrom data frame')
plot(d_df_dis, main = '"discrete" d-function\nfrom data frame', col = "blue")
par(op)

## ----q-fun_sample--------------------------------------------------------
# Treating input as discrete
q_mpg_dis <- new_q(mtcars$mpg, type = "discrete")
q_mpg_dis

# Treating input as continuous
q_mpg_con <- new_q(mtcars$mpg, type = "continuous")
q_mpg_con

# Outputs are actually vectorized functions
q_mpg_dis(c(0.1, 0.3, 0.7, 1.5))
q_mpg_con(c(0.1, 0.3, 0.7, 1.5))

# You can plot them directly using base `plot()` and `lines()`
plot(q_mpg_con, main = "Q-functions from sample")
lines(q_mpg_dis, col = "blue")

## ----q-fun_data-frame----------------------------------------------------
q_df_dis <- new_q(dis_df, type = "discrete")
q_df_dis

q_df_con <- new_q(con_df, type = "continuous")
q_df_con

plot(q_df_con, main = "Q-functions from data frame")
lines(q_df_dis, col = "blue")

## ----r-fun_sample--------------------------------------------------------
# Treating input as discrete
r_mpg_dis <- new_r(mtcars$mpg, type = "discrete")
r_mpg_dis

# Treating input as continuous
r_mpg_con <- new_r(mtcars$mpg, type = "continuous")
r_mpg_con

# Outputs are actually functions
r_mpg_dis(5)
r_mpg_con(5)

# You can plot them directly using base `plot()` and `lines()`
op <- par(mfrow = c(1, 2))
plot(r_mpg_con, main = '"continuous" r-function\nfrom sample')
plot(r_mpg_dis, main = '"discrete" r-function\nfrom sample', col = "blue")
par(op)

## ----r-fun_data-frame----------------------------------------------------
r_df_dis <- new_r(dis_df, type = "discrete")
r_df_dis

r_df_con <- new_r(con_df, type = "continuous")
r_df_con

op <- par(mfrow = c(1, 2))
plot(r_df_con, main = '"continuous" r-function\nfrom data frame')
plot(r_df_dis, main = '"discrete" r-function\nfrom data frame', col = "blue")
par(op)

## ----dirac---------------------------------------------------------------
r_dirac <- new_r(3.14, type = "continuous")
r_dirac
r_dirac(4)

  # Outputs aren't exactly but approximately equal
dput(r_dirac(4))

## ----boolean-------------------------------------------------------------
new_d(data.frame(x = c(0, 1), prob = c(0.25, 0.75)), type = "discrete")

## ----density-args--------------------------------------------------------
plot(
  new_d(mtcars$mpg, "continuous"), lwd = 3,
  main = "Examples of `density()` options"
)

# Argument `adjust` of `density()` helps to define smoothing bandwidth
lines(new_d(mtcars$mpg, "continuous", adj = 0.3), col = "blue")

# Argument `n` defines number of points to be used in piecewise-linear
# approximation
lines(new_d(mtcars$mpg, "continuous", n = 5), col = "green")

# Argument `cut` defines the "extending" property of density estimation.
# Using `cut = 0` assumes that density can't go outside of input's range
lines(new_d(mtcars$mpg, "continuous", cut = 0), col = "magenta")

## ----meta_x_tbl----------------------------------------------------------
# Type "discrete"
d_dis <- new_d(1:4, type = "discrete")
meta_x_tbl(d_dis)
meta_class(d_dis)
meta_type(d_dis)
meta_support(d_dis)

# Type "continuous"
p_con <- new_p(1:4, type = "continuous")
head(meta_x_tbl(p_con))
meta_class(p_con)
meta_type(p_con)
meta_support(p_con)

# Dirac-like "continuous" function
r_dirac <- new_r(1, type = "continuous")
dput(meta_x_tbl(r_dirac))
dput(meta_support(r_dirac))

# `meta_all()` returns all metadata in a single list
meta_all(d_dis)

