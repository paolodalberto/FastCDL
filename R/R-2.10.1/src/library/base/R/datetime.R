#  File src/library/base/R/datetime.R
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

Sys.time <- function()
    structure(.Internal(Sys.time()), class = c("POSIXt", "POSIXct"))

Sys.timezone <- function() as.vector(Sys.getenv("TZ"))

as.POSIXlt <- function(x, tz = "", ...) UseMethod("as.POSIXlt")

as.POSIXlt.Date <- function(x, ...)
    return(.Internal(Date2POSIXlt(x)))

as.POSIXlt.date <- as.POSIXlt.dates <- function(x, ...)
    as.POSIXlt(as.POSIXct(x), ...)

as.POSIXlt.POSIXct <- function(x, tz = "", ...)
{
    tzone <- attr(x, "tzone")
    if((missing(tz) || is.null(tz)) && !is.null(tzone)) tz <- tzone[1L]
    .Internal(as.POSIXlt(x, tz))
}

as.POSIXlt.factor <- function(x, ...) as.POSIXlt(as.character(x))

as.POSIXlt.character <- function(x, tz = "", format, ...)
{
    x <- unclass(x) # precaution PR7826
    if(!missing(format)) {
        res <- strptime(x, format, tz=tz)
        if(nzchar(tz)) attr(res, "tzone") <- tz
        return(res)
    }
    xx <- x[1L]
    if(is.na(xx)) {
        j <- 1L
        while(is.na(xx) && (j <- j+1L) <= length(x))
            xx <- x[j]
        if(is.na(xx)) f <- "%Y-%m-%d" # all NAs
    }
    if(is.na(xx) ||
       !is.na(strptime(xx, f <- "%Y-%m-%d %H:%M:%OS", tz=tz)) ||
       !is.na(strptime(xx, f <- "%Y/%m/%d %H:%M:%OS", tz=tz)) ||
       !is.na(strptime(xx, f <- "%Y-%m-%d %H:%M", tz=tz)) ||
       !is.na(strptime(xx, f <- "%Y/%m/%d %H:%M", tz=tz)) ||
       !is.na(strptime(xx, f <- "%Y-%m-%d", tz=tz)) ||
       !is.na(strptime(xx, f <- "%Y/%m/%d", tz=tz)))
    {
        res <- strptime(x, f, tz=tz)
        if(nzchar(tz)) attr(res, "tzone") <- tz
        return(res)
    }
    stop("character string is not in a standard unambiguous format")
}

as.POSIXlt.numeric <- function(x, tz = "", origin, ...)
{
    if(missing(origin)) stop("'origin' must be supplied")
    as.POSIXlt(as.POSIXct(origin, tz = "UTC", ...) + x, tz = tz)
}

as.POSIXlt.default <- function(x, tz = "", ...)
{

    if(inherits(x, "POSIXlt")) return(x)
    if(is.logical(x) && all(is.na(x)))
        return(as.POSIXlt(as.POSIXct.default(x), tz=tz))
    stop(gettextf("do not know how to convert '%s' to class \"POSIXlt\"",
                  deparse(substitute(x))))
}

as.POSIXct <- function(x, tz = "", ...) UseMethod("as.POSIXct")

as.POSIXct.Date <- function(x, ...)
    structure(unclass(x)*86400, class=c("POSIXt", "POSIXct"))


## convert from package date
as.POSIXct.date <- function(x, ...)
{
    if(inherits(x, "date")) {
        x <- (x - 3653) * 86400 # origin 1960-01-01
        return(structure(x, class = c("POSIXt", "POSIXct")))
    } else stop(gettextf("'%s' is not a \"date\" object",
                         deparse(substitute(x)) ))
}

## convert from package chron
as.POSIXct.dates <- function(x, ...)
{
    if(inherits(x, "dates")) {
        z <- attr(x, "origin")
        x <- as.numeric(x) * 86400
        if(length(z) == 3L && is.numeric(z))
            x  <- x + as.numeric(ISOdate(z[3L], z[1L], z[2L], 0))
        return(structure(x, class = c("POSIXt", "POSIXct")))
    } else stop(gettextf("'%s' is not a \"dates\" object",
                         deparse(substitute(x)) ))
}

as.POSIXct.POSIXlt <- function(x, tz = "", ...)
{
    tzone <- attr(x, "tzone")
    if(missing(tz) && !is.null(tzone)) tz <- tzone[1L]
    structure(.Internal(as.POSIXct(x, tz)), class = c("POSIXt", "POSIXct"),
              tzone = tz)
}

as.POSIXct.numeric <- function(x, tz = "", origin, ...)
{
    if(missing(origin)) stop("'origin' must be supplied")
    as.POSIXct(origin, tz=tz, ...) + x
}

as.POSIXct.default <- function(x, tz = "", ...)
{
    if(inherits(x, "POSIXct")) return(x)
    if(is.character(x) || is.factor(x))
	return(as.POSIXct(as.POSIXlt(x, tz, ...), tz, ...))
    if(is.logical(x) && all(is.na(x)))
        return(structure(as.numeric(x), class = c("POSIXt", "POSIXct")))
    stop(gettextf("do not know how to convert '%s' to class \"POSIXlt\"",
                  deparse(substitute(x))))
}

as.double.POSIXlt <- function(x, ...) as.POSIXct(x)

format.POSIXlt <- function(x, format = "", usetz = FALSE, ...)
{
    if(!inherits(x, "POSIXlt")) stop("wrong class")
    if(format == "") {
        ## need list [ method here.
        times <- unlist(unclass(x)[1L:3L])
        secs <- x$sec; secs <- secs[!is.na(secs)]
        np <- getOption("digits.secs")
        if(is.null(np)) np <- 0L else np <- min(6L, np)
        if(np >= 1L)
            for (i in seq_len(np)- 1L)
                if(all( abs(secs - round(secs, i)) < 1e-6 )) {
                    np <- i
                    break
                }
        format <- if(all(times[!is.na(times)] == 0)) "%Y-%m-%d"
        else if(np == 0L) "%Y-%m-%d %H:%M:%S"
        else paste("%Y-%m-%d %H:%M:%OS", np, sep="")
    }
    .Internal(format.POSIXlt(x, format, usetz))
}

## prior to 2.9.0 the same as format.POSIXlt.
## now more or less the same as format.POSIXct but also works for Dates.
strftime <- function(x, format = "", tz = "", usetz = FALSE, ...)
    format(as.POSIXlt(x, tz = tz), format = format, usetz = usetz, ...)

strptime <- function(x, format, tz = "")
    .Internal(strptime(as.character(x), format, tz))


format.POSIXct <- function(x, format = "", tz = "", usetz = FALSE, ...)
{
    if(!inherits(x, "POSIXct")) stop("wrong class")
    if(missing(tz) && !is.null(tzone <- attr(x, "tzone"))) tz <- tzone
    structure(format.POSIXlt(as.POSIXlt(x, tz), format, usetz, ...),
              names=names(x))
}

print.POSIXct <- function(x, ...)
{
    print(format(x, usetz=TRUE, ...), ...)
    invisible(x)
}

print.POSIXlt <- function(x, ...)
{
    print(format(x, usetz=TRUE), ...)
    invisible(x)
}

summary.POSIXct <- function(object, digits=15, ...)
{
    x <- summary.default(unclass(object), digits=digits, ...)[1L:6L]# no NA's
    class(x) <- oldClass(object)
    attr(x, "tzone") <- attr(object, "tzone")
    x
}

summary.POSIXlt <- function(object, digits = 15, ...)
    summary(as.POSIXct(object), digits = digits, ...)


"+.POSIXt" <- function(e1, e2)
{
    ## need to drop "units" attribute here
    coerceTimeUnit <- function(x)
        as.vector(switch(attr(x,"units"),
                         secs = x, mins = 60*x, hours = 60*60*x,
                         days = 60*60*24*x, weeks = 60*60*24*7*x))

    if (nargs() == 1) return(e1)
    # only valid if one of e1 and e2 is a scalar.
    if(inherits(e1, "POSIXt") && inherits(e2, "POSIXt"))
        stop("binary '+' is not defined for \"POSIXt\" objects")
    if(inherits(e1, "POSIXlt")) e1 <- as.POSIXct(e1)
    if(inherits(e2, "POSIXlt")) e2 <- as.POSIXct(e2)
    if (inherits(e1, "difftime")) e1 <- coerceTimeUnit(e1)
    if (inherits(e2, "difftime")) e2 <- coerceTimeUnit(e2)
    structure(unclass(e1) + unclass(e2), class = c("POSIXt", "POSIXct"),
              tzone = check_tzones(e1, e2))
}

"-.POSIXt" <- function(e1, e2)
{
    coerceTimeUnit <- function(x)
        as.vector(switch(attr(x,"units"),
                         secs = x, mins = 60*x, hours = 60*60*x,
                         days = 60*60*24*x, weeks = 60*60*24*7*x))
    if(!inherits(e1, "POSIXt"))
        stop("Can only subtract from POSIXt objects")
    if (nargs() == 1) stop("unary '-' is not defined for \"POSIXt\" objects")
    if(inherits(e2, "POSIXt")) return(difftime(e1, e2))
    if (inherits(e2, "difftime")) e2 <- coerceTimeUnit(e2)
    if(!is.null(attr(e2, "class")))
        stop("can only subtract numbers from POSIXt objects")
    structure(unclass(as.POSIXct(e1)) - e2, class = c("POSIXt", "POSIXct"))
}

Ops.POSIXt <- function(e1, e2)
{
    if (nargs() == 1)
        stop(gettextf("unary '%s' not defined for \"POSIXt\" objects",
                      .Generic), domain = NA)
    boolean <- switch(.Generic, "<" = , ">" = , "==" = ,
                      "!=" = , "<=" = , ">=" = TRUE, FALSE)
    if (!boolean)
        stop(gettextf("'%s' not defined for \"POSIXt\" objects", .Generic),
             domain = NA)
    if(inherits(e1, "POSIXlt") || is.character(e1)) e1 <- as.POSIXct(e1)
    if(inherits(e2, "POSIXlt") || is.character(e1)) e2 <- as.POSIXct(e2)
    check_tzones(e1, e2)
    NextMethod(.Generic)
}

Math.POSIXt <- function (x, ...)
{
    stop(gettextf("'%s' not defined for \"POSIXt\" objects", .Generic),
         domain = NA)
}

check_tzones <- function(...)
{
    tzs <- unique(sapply(list(...), function(x) {
        y <- attr(x, "tzone")
        if(is.null(y)) "" else y
    }))
    tzs <- tzs[tzs != ""]
    if(length(tzs) > 1L)
        warning("'tzone' attributes are inconsistent")
    if(length(tzs)) tzs[1L] else NULL
}

Summary.POSIXct <- function (..., na.rm)
{
    ok <- switch(.Generic, max = , min = , range = TRUE, FALSE)
    if (!ok)
        stop(gettextf("'%s' not defined for \"POSIXt\" objects", .Generic),
             domain = NA)
    args <- list(...)
    tz <- do.call("check_tzones", args)
    val <- NextMethod(.Generic)
    class(val) <- oldClass(args[[1L]])
    attr(val, "tzone") <- tz
    val
}

Summary.POSIXlt <- function (..., na.rm)
{
    ok <- switch(.Generic, max = , min = , range = TRUE, FALSE)
    if (!ok)
        stop(gettextf("'%s' not defined for \"POSIXt\" objects", .Generic),
             domain = NA)
    args <- list(...)
    tz <- do.call("check_tzones", args)
    args <- lapply(args, as.POSIXct)
    val <- do.call(.Generic, c(args, na.rm = na.rm))
    as.POSIXlt(structure(val, class = c("POSIXt", "POSIXct"), tzone = tz))
}

"[.POSIXct" <-
function(x, ..., drop = TRUE)
{
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[")
    class(val) <- cl
    attr(val, "tzone") <- attr(x, "tzone")
    val
}

"[[.POSIXct" <-
function(x, ..., drop = TRUE)
{
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[[")
    class(val) <- cl
    attr(val, "tzone") <- attr(x, "tzone")
    val
}

"[<-.POSIXct" <-
function(x, ..., value) {
    if(!as.logical(length(value))) return(x)
    value <- as.POSIXct(value)
    cl <- oldClass(x)
    tz <- attr(x, "tzone")
    class(x) <- class(value) <- NULL
    x <- NextMethod(.Generic)
    class(x) <- cl
    attr(x, "tzone") <- tz
    x
}

as.character.POSIXt <- function(x, ...) format(x, ...)

as.data.frame.POSIXct <- as.data.frame.vector

is.na.POSIXlt <- function(x) is.na(as.POSIXct(x))

## <FIXME> check the argument validity
## This is documented to remove the timezone
c.POSIXct <- function(..., recursive=FALSE)
    structure(c(unlist(lapply(list(...), unclass))),
              class = c("POSIXt", "POSIXct"))

## we need conversion to POSIXct as POSIXlt objects can be in different tz.
c.POSIXlt <- function(..., recursive=FALSE)
    as.POSIXlt(do.call("c", lapply(list(...), as.POSIXct)))

## force absolute comparisons
all.equal.POSIXct <- function(target, current, ..., scale=1)
{
    check_tzones(target, current)
    NextMethod("all.equal")
}


ISOdatetime <- function(year, month, day, hour, min, sec, tz="")
{
    x <- paste(year, month, day, hour, min, sec, sep="-")
    as.POSIXct(strptime(x, "%Y-%m-%d-%H-%M-%OS", tz=tz), tz=tz)
}

ISOdate <- function(year, month, day, hour=12, min=0, sec=0, tz="GMT")
    ISOdatetime(year, month, day, hour, min, sec, tz)

as.matrix.POSIXlt <- function(x, ...)
{
    as.matrix(as.data.frame(unclass(x)), ...)
}

mean.POSIXct <- function (x, ...)
    structure(mean(unclass(x), ...), class = c("POSIXt", "POSIXct"),
              tzone = attr(x, "tzone"))

mean.POSIXlt <- function (x, ...)
    as.POSIXlt(mean(as.POSIXct(x), ...))

## ----- difftime -----

difftime <-
    function(time1, time2, tz = "",
             units = c("auto", "secs", "mins", "hours", "days", "weeks"))
{
    time1 <- as.POSIXct(time1, tz = tz)
    time2 <- as.POSIXct(time2, tz = tz)
    z <- unclass(time1) - unclass(time2)
    units <- match.arg(units)
    if(units == "auto") {
        if(all(is.na(z))) units <- "secs"
        else {
            zz <- min(abs(z),na.rm=TRUE)
            if(is.na(zz) || zz < 60) units <- "secs"
            else if(zz < 3600) units <- "mins"
            else if(zz < 86400) units <- "hours"
            else units <- "days"
        }
    }
    switch(units,
           "secs" = structure(z, units="secs", class="difftime"),
           "mins" = structure(z/60, units="mins", class="difftime"),
           "hours"= structure(z/3600, units="hours", class="difftime"),
           "days" = structure(z/86400, units="days", class="difftime"),
           "weeks" = structure(z/(7*86400), units="weeks", class="difftime")
           )
}

## "difftime" constructor
## Martin Maechler, Date: 16 Sep 2002
## Numeric input version Peter Dalgaard, December 2006
as.difftime <- function(tim, format="%X", units="auto")
{
    if (inherits(tim, "difftime")) return(tim)
    if (is.character(tim)){
        difftime(strptime(tim, format=format),
             strptime("0:0:0", format="%X"), units=units)
    } else {
        if (!is.numeric(tim)) stop("'tim' is not character or numeric")
	if (units=="auto") stop("need explicit units for numeric conversion")
        if (!(units %in% c("secs", "mins", "hours", "days", "weeks")))
	    stop("invalid units specified")
        structure(tim, units=units, class="difftime")
    }
}

### For now, these have only difftime methods, but you never know...
units <- function(x) UseMethod("units")

"units<-" <- function(x, value) UseMethod("units<-")

units.difftime <- function(x) attr(x, "units")

"units<-.difftime" <- function(x, value)
{
    from <- units(x)
    if (from == value) return(x)
    if (!(value %in% c("secs", "mins", "hours", "days", "weeks")))
        stop("invalid units specified")
    sc <- cumprod(c(secs=1, mins=60, hours=60, days=24, weeks=7))
    newx <- as.vector(x)*sc[from]/sc[value]
    structure(newx, units=value, class="difftime")
}

as.double.difftime <- function(x, units="auto", ...) {
    if (units != "auto")
        units(x) <- units
    as.double(as.vector(x))
}

as.data.frame.difftime <- as.data.frame.vector

format.difftime <- function(x,...) paste(format(unclass(x),...), units(x))



print.difftime <- function(x, digits = getOption("digits"), ...)
{
    if(is.array(x) || length(x) > 1L) {
        cat("Time differences in ", attr(x, "units"), "\n", sep="")
        y <- unclass(x); attr(y, "units") <- NULL
        print(y)
    }
    else
        cat("Time difference of ", format(unclass(x), digits=digits), " ",
            attr(x, "units"), "\n", sep="")

    invisible(x)
}

"[.difftime" <- function(x, ..., drop = TRUE)
{
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[")
    class(val) <- cl
    attr(val, "units") <- attr(x, "units")
    val
}

Ops.difftime <- function(e1, e2)
{
    coerceTimeUnit <- function(x)
    {
        switch(attr(x, "units"),
               secs = x, mins = 60*x, hours = 60*60*x,
               days = 60*60*24*x, weeks = 60*60*24*7*x)
    }
    if (nargs() == 1) {
        switch(.Generic, "+"= {}, "-" = {e1[] <- -unclass(e1)},
               stop(gettextf("unary '%s' not defined for \"difftime\" objects",
                             .Generic), domain = NA, call. = FALSE)
               )
        return(e1)
    }
    boolean <- switch(.Generic, "<" = , ">" = , "==" = ,
                      "!=" = , "<=" = , ">=" = TRUE, FALSE)
    if (boolean) {
        ## assume user knows what he/she is doing if not both difftime
        if(inherits(e1, "difftime") && inherits(e2, "difftime")) {
            e1 <- coerceTimeUnit(e1)
            e2 <- coerceTimeUnit(e2)
        }
        NextMethod(.Generic)
    } else if(.Generic == "+" || .Generic == "-") {
        if(inherits(e1, "difftime") && !inherits(e2, "difftime"))
            return(structure(NextMethod(.Generic),
                             units = attr(e1, "units"), class = "difftime"))
        if(!inherits(e1, "difftime") && inherits(e2, "difftime"))
            return(structure(NextMethod(.Generic),
                             units = attr(e2, "units"), class = "difftime"))
        u1 <- attr(e1, "units")
        if(attr(e2, "units") == u1) {
            structure(NextMethod(.Generic), units=u1, class="difftime")
        } else {
            e1 <- coerceTimeUnit(e1)
            e2 <- coerceTimeUnit(e2)
            structure(NextMethod(.Generic), units="secs", class="difftime")
        }
    } else {
        ## '*' is covered by a specific method
        stop(gettextf("'%s' not defined for \"difftime\" objects", .Generic),
             domain = NA)
    }
}

"*.difftime" <- function (e1, e2)
{
    ## need one scalar, one difftime.
    if(inherits(e1, "difftime") && inherits(e2, "difftime"))
        stop("both arguments of * cannot be \"difftime\" objects")
    if(inherits(e2, "difftime")) {tmp <- e1; e1 <- e2; e2 <- tmp}
    structure(e2 * unclass(e1), units = attr(e1, "units"),
              class = "difftime")
}

"/.difftime" <- function (e1, e2)
{
    ## need one scalar, one difftime.
    if(inherits(e2, "difftime"))
        stop("second argument of / cannot be a \"difftime\" object")
    structure(unclass(e1) / e2, units = attr(e1, "units"),
              class = "difftime")
}

## "Math": some methods *should* work; the other ones are meaningless :
Math.difftime <- function (x, ...)
{
    switch(.Generic,
           'abs'=, 'sign'=,
           'floor'=, 'ceiling'=, 'trunc'=,
           'round'=, 'signif'= {
               units <- attr(x, "units")
               structure(NextMethod(), units=units, class="difftime")
           },
           ### otherwise :
           stop(gettextf("'%s' not defined for \"difftime\" objects", .Generic),
                domain = NA))
}


mean.difftime <- function (x, ...)
    structure(mean(unclass(x), ...), units=attr(x, "units"), class="difftime")

Summary.difftime <- function (..., na.rm)
{
    ## FIXME: this should return in the smallest of the units of the inputs.
    coerceTimeUnit <- function(x)
    {
        as.vector(switch(attr(x,"units"),
                         secs = x, mins = 60*x, hours = 60*60*x,
                         days = 60*60*24*x, weeks = 60*60*24*7*x))
    }
    ok <- switch(.Generic, max = , min = , sum=, range = TRUE, FALSE)
    if (!ok)
        stop(gettextf("'%s' not defined for \"difftime\" objects", .Generic),
             domain = NA)
    x <- list(...)
    Nargs <- length(x)
    if(Nargs == 0) {
        structure(do.call(.Generic), units="secs", class="difftime")
    } else {
        units <- sapply(x, function(x) attr(x, "units"))
        if(all(units == units[1L])) {
            args <- c(lapply(x, as.vector), na.rm = na.rm)
        } else {
            args <- c(lapply(x, coerceTimeUnit), na.rm = na.rm)
            units <- "secs"
        }
        structure(do.call(.Generic, args), units=units[[1L]], class="difftime")
    }
}


## ----- convenience functions -----

seq.POSIXt <-
    function(from, to, by, length.out = NULL, along.with = NULL, ...)
{
    if (missing(from)) stop("'from' must be specified")
    if (!inherits(from, "POSIXt")) stop("'from' must be a POSIXt object")
    cfrom <- as.POSIXct(from)
    if(length(cfrom) != 1L) stop("'from' must be of length 1")
    tz <- attr(cfrom , "tzone")
    if (!missing(to)) {
        if (!inherits(to, "POSIXt")) stop("'to' must be a POSIXt object")
        if (length(as.POSIXct(to)) != 1) stop("'to' must be of length 1")
    }
    if (!missing(along.with)) {
        length.out <- length(along.with)
    }  else if (!is.null(length.out)) {
        if (length(length.out) != 1L) stop("'length.out' must be of length 1")
        length.out <- ceiling(length.out)
    }
    status <- c(!missing(to), !missing(by), !is.null(length.out))
    if(sum(status) != 2L)
        stop("exactly two of 'to', 'by' and 'length.out' / 'along.with' must be specified")
    if (missing(by)) {
        from <- unclass(cfrom)
        to <- unclass(as.POSIXct(to))
        ## Till (and incl.) 1.6.0 :
        ##- incr <- (to - from)/length.out
        ##- res <- seq.default(from, to, incr)
        res <- seq.int(from, to, length.out = length.out)
        return(structure(res, class = c("POSIXt", "POSIXct"), tzone=tz))
    }

    if (length(by) != 1L) stop("'by' must be of length 1")
    valid <- 0L
    if (inherits(by, "difftime")) {
        by <- switch(attr(by,"units"), secs = 1, mins = 60, hours = 3600,
                     days = 86400, weeks = 7*86400) * unclass(by)
    } else if(is.character(by)) {
        by2 <- strsplit(by, " ", fixed=TRUE)[[1L]]
        if(length(by2) > 2L || length(by2) < 1L)
            stop("invalid 'by' string")
        valid <- pmatch(by2[length(by2)],
                        c("secs", "mins", "hours", "days", "weeks",
                          "months", "years", "DSTdays"))
        if(is.na(valid)) stop("invalid string for 'by'")
        if(valid <= 5L) {
            by <- c(1, 60, 3600, 86400, 7*86400)[valid]
            if (length(by2) == 2L) by <- by * as.integer(by2[1L])
        } else
            by <- if(length(by2) == 2L) as.integer(by2[1L]) else 1
    } else if(!is.numeric(by)) stop("invalid mode for 'by'")
    if(is.na(by)) stop("'by' is NA")

    if(valid <= 5L) {
        from <- unclass(as.POSIXct(from))
        if(!is.null(length.out))
            res <- seq.int(from, by=by, length.out=length.out)
        else {
            to <- unclass(as.POSIXct(to))
            ## defeat test in seq.default
            res <- seq.int(0, to - from, by) + from
        }
        return(structure(res, class=c("POSIXt", "POSIXct"), tzone=tz))
    } else {  # months or years or DSTdays
        r1 <- as.POSIXlt(from)
        if(valid == 7L) {
            if(missing(to)) { # years
                yr <- seq.int(r1$year, by = by, length.out = length.out)
            } else {
                to <- as.POSIXlt(to)
                yr <- seq.int(r1$year, to$year, by)
            }
            r1$year <- yr
            r1$isdst <- -1L
            res <- as.POSIXct(r1)
        } else if(valid == 6L) { # months
            if(missing(to)) {
                mon <- seq.int(r1$mon, by = by, length.out = length.out)
            } else {
                to <- as.POSIXlt(to)
                mon <- seq.int(r1$mon, 12*(to$year - r1$year) + to$mon, by)
            }
            r1$mon <- mon
            r1$isdst <- -1
            res <- as.POSIXct(r1)
        } else if(valid == 8L) { # DSTdays
            if(!missing(to)) {
                ## We might have a short day, so need to over-estimate.
                length.out <- 2L + floor((unclass(as.POSIXct(to)) -
                                          unclass(as.POSIXct(from)))/86400)
            }
            r1$mday <- seq.int(r1$mday, by = by, length.out = length.out)
            r1$isdst <- -1L
            res <- as.POSIXct(r1)
            ## now correct if necessary.
            if(!missing(to)) res <- res[res <= as.POSIXct(to)]
        }
        return(res)
    }
}

cut.POSIXt <-
    function (x, breaks, labels = NULL, start.on.monday = TRUE,
              right = FALSE, ...)
{
    if(!inherits(x, "POSIXt")) stop("'x' must be a date-time object")
    x <- as.POSIXct(x)

    if (inherits(breaks, "POSIXt")) {
	breaks <- as.POSIXct(breaks)
    } else if(is.numeric(breaks) && length(breaks) == 1L) {
	## specified number of breaks
    } else if(is.character(breaks) && length(breaks) == 1L) {
        by2 <- strsplit(breaks, " ", fixed=TRUE)[[1L]]
        if(length(by2) > 2L || length(by2) < 1L)
            stop("invalid specification of 'breaks'")
	valid <-
	    pmatch(by2[length(by2)],
		   c("secs", "mins", "hours", "days", "weeks",
		     "months", "years", "DSTdays", "quarters"))
	if(is.na(valid)) stop("invalid specification of 'breaks'")
	start <- as.POSIXlt(min(x, na.rm=TRUE))
	incr <- 1
	if(valid > 1L) { start$sec <- 0L; incr <- 59.99 }
	if(valid > 2L) { start$min <- 0L; incr <- 3600 - 1 }
	if(valid > 3L) { start$hour <- 0L; incr <- 86400 - 1 }
	if(valid == 5L) {
	    start$mday <- start$mday - start$wday
	    if(start.on.monday)
		start$mday <- start$mday + ifelse(start$wday > 0L, 1L, -6L)
	    incr <- 7*86400
	}
    if(valid == 8L) incr <- 25*3600
    if(valid == 6L) {
        start$mday <- 1L
        end <- as.POSIXlt(max(x, na.rm = TRUE))
        step <- ifelse(length(by2) == 2L, as.integer(by2[1L]), 1L)
        end <- as.POSIXlt(end + (31 * step * 86400))
        end$mday <- 1L
        breaks <- seq(start, end, breaks)
    } else if(valid == 7L) {
        start$mon <- 0L
        start$mday <- 1L
        end <- as.POSIXlt(max(x, na.rm = TRUE))
        step <- ifelse(length(by2) == 2L, as.integer(by2[1L]), 1L)
        end <- as.POSIXlt(end + (366 * step* 86400))
        end$mon <- 0L
        end$mday <- 1L
        breaks <- seq(start, end, breaks)
    } else if(valid == 9L) {
        qtr <- rep(c(0L, 3L, 6L, 9L), each = 3L)
        start$mon <- qtr[start$mon + 1L]
        start$mday <- 1L
        end <- as.POSIXlt(max(x, na.rm = TRUE))
        step <- ifelse(length(by2) == 2L, as.integer(by2[1L]), 1L)
        end <- as.POSIXlt(end + (93 * step * 86400))
        end$mon <- qtr[end$mon + 1L]
        end$mday <- 1L
        breaks <- seq(start, end, paste(step * 3, "months"))
    } else {
        if (length(by2) == 2L) incr <- incr * as.integer(by2[1L])
        maxx <- max(x, na.rm = TRUE)
        breaks <- seq.int(start, maxx + incr, breaks)
        breaks <- breaks[seq_len(1+max(which(breaks <= maxx)))]
      }
    } else stop("invalid specification of 'breaks'")
    res <- cut(unclass(x), unclass(breaks), labels = labels, right = right, ...)
    if(is.null(labels)) levels(res) <- as.character(breaks[-length(breaks)])
    res
}

julian <- function(x, ...) UseMethod("julian")

julian.POSIXt <- function(x, origin = as.POSIXct("1970-01-01", tz="GMT"), ...)
{
    origin <- as.POSIXct(origin)
    if(length(origin) != 1L) stop("'origin' must be of length one")
    res <- difftime(as.POSIXct(x), origin, units = "days")
    structure(res, "origin" = origin)
}

weekdays <- function(x, abbreviate) UseMethod("weekdays")
weekdays.POSIXt <- function(x, abbreviate = FALSE)
{
    format(x, ifelse(abbreviate, "%a", "%A"))
}

months <- function(x, abbreviate) UseMethod("months")
months.POSIXt <- function(x, abbreviate = FALSE)
{
    format(x, ifelse(abbreviate, "%b", "%B"))
}

quarters <- function(x, abbreviate) UseMethod("quarters")
quarters.POSIXt <- function(x, ...)
{
    x <- (as.POSIXlt(x)$mon)%/%3
    paste("Q", x+1, sep = "")
}

trunc.POSIXt <- function(x, units=c("secs", "mins", "hours", "days"), ...)
{
    units <- match.arg(units)
    x <- as.POSIXlt(x)
    if(length(x$sec))
	switch(units,
	       "secs" = {x$sec <- trunc(x$sec)},
	       "mins" = {x$sec <- 0},
	       "hours"= {x$sec <- 0; x$min <- 0L},
	       "days" = {x$sec <- 0; x$min <- 0L; x$hour <- 0L; x$isdst <- -1L}
	       )
    x
}

round.POSIXt <- function(x, units=c("secs", "mins", "hours", "days"))
{
    ## this gets the default from the generic, as that has two args.
    if(is.numeric(units) && units == 0.0) units <-"secs"
    units <- match.arg(units)
    x <- as.POSIXct(x)
    x <- x + switch(units,
                    "secs" = 0.5, "mins" = 30, "hours"= 1800, "days" = 43200)
    trunc.POSIXt(x, units = units)
}

## ---- additions in 1.5.0 -----

"[.POSIXlt" <- function(x, ..., drop = TRUE)
{
    val <- lapply(x, "[", ..., drop = drop)
    attributes(val) <- attributes(x) # need to preserve timezones
    val
}

"[<-.POSIXlt" <- function(x, i, value)
{
    if(!as.logical(length(value))) return(x)
    value <- as.POSIXlt(value)
    cl <- oldClass(x)
    class(x) <- class(value) <- NULL
    for(n in names(x)) x[[n]][i] <- value[[n]]
    class(x) <- cl
    x
}

as.data.frame.POSIXlt <- function(x, row.names = NULL, optional = FALSE, ...)
{
    value <- as.data.frame.POSIXct(as.POSIXct(x), row.names, optional, ...)
    if (!optional)
        names(value) <- deparse(substitute(x))[[1L]]
    value
}

## ---- additions in 1.8.0 -----

rep.POSIXct <- function(x, ...)
{
    y <- NextMethod()
    structure(y, class=c("POSIXt", "POSIXct"), tzone = attr(x, "tzone"))
}

rep.POSIXlt <- function(x, ...)
{
    y <- lapply(x, rep, ...)
    attributes(y) <- attributes(x)
    y
}

diff.POSIXt <- function (x, lag = 1L, differences = 1L, ...)
{
    ismat <- is.matrix(x)
    r <- if(inherits(x, "POSIXlt")) as.POSIXct(x) else x
    xlen <- if (ismat) dim(x)[1L] else length(r)
    if (length(lag) > 1L || length(differences) > 1L || lag < 1L || differences < 1L)
        stop("'lag' and 'differences' must be integers >= 1")
    if (lag * differences >= xlen)
        return(structure(numeric(0L), class="difftime", units="secs"))
    i1 <- -seq_len(lag)
    if (ismat) for (i in seq_len(differences)) r <- r[i1, , drop = FALSE] -
            r[-nrow(r):-(nrow(r) - lag + 1), , drop = FALSE]
    else for (i in seq_len(differences))
        r <- r[i1] - r[-length(r):-(length(r) - lag + 1L)]
    r
}

## ---- additions in 2.2.0 -----

duplicated.POSIXlt <- function(x, incomparables = FALSE, ...)
{
    x <- as.POSIXct(x)
    NextMethod("duplicated", x)
}

unique.POSIXlt <- function(x, incomparables = FALSE, ...)
    x[!duplicated(x, incomparables, ...)]

## ---- additions in 2.4.0 -----

sort.POSIXlt <- function(x, decreasing = FALSE, na.last = NA, ...)
    x[order(as.POSIXct(x), na.last = na.last, decreasing = decreasing)]


## ---- additions in 2.6.0 -----

is.numeric.POSIXt <- function(x) FALSE

## ---- additions in 2.6.0 -----

split.POSIXct <-
function(x, f, drop = FALSE, ...)
{
    lapply(split.default(as.double(x), f, drop = drop), structure,
           class = c("POSIXt", "POSIXct"), tzone = attr(x, "tzone"))
}

xtfrm.POSIXct <- function(x) as.numeric(x)
xtfrm.POSIXlt <- function(x) as.double(x)  # has POSIXlt method
