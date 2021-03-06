#  File src/library/stats/R/weighted.mean.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

weighted.mean <- function(x, w, ...) UseMethod("weighted.mean")

weighted.mean.default <- function(x, w, ..., na.rm = FALSE)
{
    if(missing(w))
        w <- rep.int(1, length(x))
    else if (length(w) != length(x))
        stop("'x' and 'w' must have the same length")
    if(is.integer(w)) w <- as.numeric(w) # avoid overflow in sum.
    if (na.rm) { i <- !is.na(x); w <- w[i]; x <- x[i] }
    sum(x*(w/sum(w)))
}

## see note for ?mean.Date
weighted.mean.Date <- function (x, w, ...)
    structure(weighted.mean(unclass(x), w, ...), class = "Date")

weighted.mean.POSIXct <- function (x, w, ...)
    structure(weighted.mean(unclass(x), w, ...), class = c("POSIXt", "POSIXct"),
              tzone = attr(x, "tzone"))

weighted.mean.POSIXlt <- function (x, w, ...)
    as.POSIXlt(weighted.mean(as.POSIXct(x), w, ...))

