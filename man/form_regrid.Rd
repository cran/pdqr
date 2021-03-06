% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/form_regrid.R
\name{form_regrid}
\alias{form_regrid}
\title{Change grid of pdqr-function}
\usage{
form_regrid(f, n_grid, method = "x")
}
\arguments{
\item{f}{A pdqr-function.}

\item{n_grid}{A desired number of grid elements in output.}

\item{method}{Regrid method. Should be one of "x" or "q".}
}
\value{
A pdqr-function with modified grid.
}
\description{
Modify grid of pdqr-function (rows of \link[=meta_x_tbl]{"x_tbl" metadata}) to
increase (upgrid) or decrease (downgrid) granularity using method of choice.
Upgridding might be useful in order to obtain more information during certain
type of transformations. Downgridding might be useful for decreasing amount
of used memory for storing pdqr-function without losing much information.
}
\details{
The goal here is to create pdqr-function which is reasonably similar
to \code{f} and has \code{n_grid} rows in "x_tbl" metadata.

General algorithm of regridding is as follows:
\itemize{
\item \strong{Compute reference grid}. For method "x" it is a sequence of equidistant
points between edges of \code{f}'s \link[=meta_support]{support}. For method "q" -
sequence of quantiles for equidistant probabilities from 0 to 1. Lengths of
reference grids for both methods are \code{n_grid}.
\item \strong{Adjust \code{f}'s grid to reference one}. This is done depending on \code{f}'s
\link[=meta_type]{type} and which kind or regridding is done (upgridding is the
case when \code{n_grid} is strictly more than number of rows in "x_tbl" metadata,
downgridding - when it is strictly less):
\itemize{
\item Type "discrete":
\itemize{
\item UPgridding "discrete" functions is not possible as it is assumed
that input "discrete" functions can't have any "x" values other then
present ones. In this case input is returned, the only case when
output doesn't have desired \code{n_grid} rows in "x_tbl" metadata.
\item DOWNgridding "discrete" functions is done by computing nearest
match of reference grid to \code{f}'s one and collapsing (by summing
probabilities) all "x" values from input to the nearest matched ones.
Here "computing nearest match" means that every element of reference
grid is one-one matched with subset of unique values from \code{f}'s "x"
elements. Matching is done in greedy iterative fashion in order to
minimize total distance between reference grid and matched subset.
\strong{Note} that this can result in not optimal (with not minimum total
distance) match and can take a while to compute in some cases.
}
\item Type "continuous":
\itemize{
\item UPgridding "continuous" functions is done by adding rows to "x_tbl"
metadata with "x" values equal to those elements of reference grid
which are the furthest away from input "x" grid as a set. Distance
from point to set is meant as minimum of distances between point and
all points of set. Values of "y" and "cumprob" columns are taken as
values of corresponding to \code{f} d- and p-functions.
\item DOWNgridding "continuous" functions is done by computing nearest
match of reference grid to \code{f}'s one (as for "discrete" type) and
removing all unmatched rows from "x_tbl" metadata.
}
}
}

Special cases of \code{n_grid}:
\itemize{
\item If \code{n_grid} is the same as number of rows in "x_tbl" metadata, then input
\code{f} is returned.
\item If \code{n_grid} is 1, appropriate \verb{new_*()} function is used with single
numeric input equal to distribution's median.
}
}
\examples{
# Type "discrete"
d_dis <- new_d(data.frame(x = 1:10, prob = 1:10 / 55), type = "discrete")

# Downgridding
meta_x_tbl(form_regrid(d_dis, n_grid = 4))
meta_x_tbl(form_regrid(d_dis, n_grid = 4, method = "q"))

# Upgridding for "discrete" type isn't possible. Input is returned
identical(d_dis, form_regrid(d_dis, n_grid = 100))

# Type "continuous"
# Downgridding
d_norm <- as_d(dnorm)
plot(d_norm)
lines(form_regrid(d_norm, n_grid = 10), col = "blue")
lines(form_regrid(d_norm, n_grid = 10, method = "q"), col = "green")

# Upgridding
d_con <- new_d(data.frame(x = 1:3, y = rep(0.5, 3)), type = "continuous")
meta_x_tbl(form_regrid(d_con, n_grid = 6))

# Pdqr-function with center at median is returned in case `n_grid` is 1
form_regrid(d_dis, n_grid = 1)
# Dirac-like function is returned
form_regrid(d_con, n_grid = 1)
}
\seealso{
\code{\link[=form_resupport]{form_resupport()}} for changing support of pdqr-function.

\code{\link[=form_retype]{form_retype()}} for changing type of pdqr-function.

Other form functions: 
\code{\link{form_estimate}()},
\code{\link{form_mix}()},
\code{\link{form_resupport}()},
\code{\link{form_retype}()},
\code{\link{form_smooth}()},
\code{\link{form_tails}()},
\code{\link{form_trans}()}
}
\concept{form functions}
