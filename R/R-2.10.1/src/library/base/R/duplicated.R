#  File src/library/base/R/duplicated.R
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

duplicated <- function(x, incomparables = FALSE, ...) UseMethod("duplicated")

duplicated.default <- function(x, incomparables = FALSE, fromLast = FALSE, ...)
{
    if(is.na(fromLast <- as.logical(fromLast[1L])))
        stop("'fromLast' must be TRUE or FALSE")
    .Internal(duplicated(x, incomparables, fromLast))
}

duplicated.data.frame <- function(x, incomparables = FALSE, fromLast = FALSE, ...)
{
    if(!identical(incomparables, FALSE))
	.NotYetUsed("incomparables != FALSE")
    duplicated(do.call("paste", c(x, sep="\r")), fromLast = fromLast)
}

duplicated.matrix <- duplicated.array <-
    function(x, incomparables = FALSE, MARGIN = 1L, fromLast = FALSE, ...)
{
    if(!identical(incomparables, FALSE))
	.NotYetUsed("incomparables != FALSE")
    ndim <- length(dim(x))
    if (length(MARGIN) > ndim || any(MARGIN > ndim))
        stop("MARGIN = ", MARGIN, " is invalid for dim = ", dim(x))
    temp <- apply(x, MARGIN, function(x) paste(x, collapse = "\r"))
    res <- duplicated(as.vector(temp), fromLast = fromLast)
    dim(res) <- dim(temp)
    dimnames(res) <- dimnames(temp)
    res
}

anyDuplicated <- function(x, incomparables = FALSE, ...) UseMethod("anyDuplicated")

anyDuplicated.default <- function(x, incomparables = FALSE, fromLast = FALSE, ...)
{
    if(is.na(fromLast <- as.logical(fromLast[1L])))
        stop("'fromLast' must be TRUE or FALSE")
    .Internal(anyDuplicated(x, incomparables, fromLast))
}

anyDuplicated.data.frame <- function(x, incomparables = FALSE, fromLast = FALSE, ...)
{
    if(!identical(incomparables, FALSE))
	.NotYetUsed("incomparables != FALSE")
    anyDuplicated(do.call("paste", c(x, sep="\r")), fromLast = fromLast)
}

anyDuplicated.matrix <- anyDuplicated.array <-
    function(x, incomparables = FALSE, MARGIN = 1L, fromLast = FALSE, ...)
{
    if(!identical(incomparables, FALSE))
	.NotYetUsed("incomparables != FALSE")
    ndim <- length(dim(x))
    if (length(MARGIN) > ndim || any(MARGIN > ndim))
        stop("MARGIN = ", MARGIN, " is invalid for dim = ", dim(x))
    temp <- apply(x, MARGIN, function(x) paste(x, collapse = "\r"))
    anyDuplicated(as.vector(temp), fromLast = fromLast)
}

unique <- function(x, incomparables = FALSE, ...) UseMethod("unique")


## NB unique.default is used by factor to avoid unique.matrix,
## so it needs to handle some other cases.
unique.default <- function(x, incomparables = FALSE, fromLast = FALSE, ...)
{
    if(is.na(fromLast <- as.logical(fromLast[1L])))
        stop("'fromLast' must be TRUE or FALSE")
    z <- .Internal(unique(x, incomparables, fromLast))
    if(is.factor(x))
	factor(z, levels = seq_len(nlevels(x)), labels = levels(x),
               ordered = is.ordered(x))
    else if(inherits(x, "POSIXct"))
        structure(z, class = class(x), tzone = attr(x, "tzone"))
    else if(inherits(x, "Date"))
        structure(z, class = class(x))
    else z
}

unique.data.frame <- function(x, incomparables = FALSE, fromLast = FALSE, ...)
{
    if(!identical(incomparables, FALSE))
	.NotYetUsed("incomparables != FALSE")
    x[!duplicated(x, fromLast = fromLast),  , drop = FALSE]
}

unique.matrix <- unique.array <-
    function(x, incomparables = FALSE , MARGIN = 1, fromLast = FALSE, ...)
{
    if(!identical(incomparables, FALSE))
	.NotYetUsed("incomparables != FALSE")
    ndim <- length(dim(x))
    if (length(MARGIN) > 1L || any(MARGIN > ndim))
        stop("MARGIN = ", MARGIN, " is invalid for dim = ", dim(x))
    temp <- apply(x, MARGIN, function(x) paste(x, collapse = "\r"))
    args <- rep(alist(a=), ndim)
    names(args) <- NULL
    args[[MARGIN]] <- !duplicated(as.vector(temp), fromLast = fromLast)
    do.call("[", c(list(x), args, list(drop=FALSE)))
}
