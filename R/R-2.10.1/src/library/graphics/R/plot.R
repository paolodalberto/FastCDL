#  File src/library/graphics/R/plot.R
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

### xy.coords() is now in the imported 'grDevices' package

plot <- function (x, y, ...)
{
    if (is.function(x) && is.null(attr(x, "class"))) {
	if (missing(y)) y <- NULL
	hasylab <- function(...) !all(is.na(pmatch(names(list(...)), "ylab")))
	## not ok: hasylab <- function(ylab, ...) !missing(ylab)
	if (hasylab(...))
	    plot.function(x, y, ...)
	else
	    plot.function(x, y, ylab=paste(deparse(substitute(x)),"(x)"), ...)
    }
    else UseMethod("plot")
}

## xlim = NULL (instead of "missing", since it will be passed to plot.default):
plot.function <- function(x, y = 0, to = 1, from = y, xlim = NULL, ...)
{
    if(is.null(xlim)) {
	if(is.null(from)) from <- 0
    } else {
	if(is.null(from)) from <- xlim[1L]
	if(missing(to))	  to   <- xlim[2L]
    }
    curve(x, from, to, xlim = xlim, ...)
}

plot.default <-
    function(x, y = NULL, type = "p", xlim = NULL, ylim = NULL,
             log = "", main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
             ann = par("ann"), axes = TRUE, frame.plot = axes,
             panel.first = NULL, panel.last = NULL, asp = NA, ...)
{
    ## These col, bg, pch, cex can be vectors, so exclude them
    ## Also, axis and box accept some of these
    localAxis <- function(..., col, bg, pch, cex, lty, lwd) Axis(...)
    localBox <- function(..., col, bg, pch, cex, lty, lwd) box(...)
    localWindow <- function(..., col, bg, pch, cex, lty, lwd) plot.window(...)
    localTitle <- function(..., col, bg, pch, cex, lty, lwd) title(...)
    xlabel <- if (!missing(x)) deparse(substitute(x))
    ylabel <- if (!missing(y)) deparse(substitute(y))
    xy <- xy.coords(x, y, xlabel, ylabel, log)
    xlab <- if (is.null(xlab)) xy$xlab else xlab
    ylab <- if (is.null(ylab)) xy$ylab else ylab
    xlim <- if (is.null(xlim)) range(xy$x[is.finite(xy$x)]) else xlim
    ylim <- if (is.null(ylim)) range(xy$y[is.finite(xy$y)]) else ylim
    plot.new()
    localWindow(xlim, ylim, log, asp, ...)
    panel.first # eval() is wrong here {Ross I.}
    plot.xy(xy, type, ...)
    panel.last
    if (axes) {
	localAxis(if(is.null(y)) xy$x else x, side = 1, ...)
	localAxis(if(is.null(y))  x   else y, side = 2, ...)
    }
    if (frame.plot) localBox(...)
    if (ann) localTitle(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
    invisible()
}

plot.factor <- function(x, y, legend.text = NULL, ...)
{
    if (missing(y) || is.factor(y)) {
        dargs <- list(...)
        axisnames <- if (!is.null(dargs$axes))
            dargs$axes
        else if (!is.null(dargs$xaxt))
            dargs$xaxt != "n"
        else TRUE
    }
    if (missing(y)) {
        barplot(table(x), axisnames = axisnames, ...)
    }
    else if (is.factor(y)) {
        if(is.null(legend.text)) spineplot(x, y, ...) else {
	  args <- c(list(x = x, y = y), list(...))
	  args$yaxlabels <- legend.text
	  do.call("spineplot", args)
	}
    }
    else if (is.numeric(y))
        boxplot(y ~ x, ...)
    else NextMethod("plot")
}

## FIXME (ideas/wishes):
## o for 1-D tables:
##   - alternatively, and/or as default, type = "bar" ??!??
##   - if "h", make the default lwd depend on number of classes instead of lwd=2
plot.table <-
    function(x, type = "h", ylim = c(0, max(x)), lwd = 2,
             xlab = NULL, ylab = NULL, frame.plot = is.num, ...)
{
    xnam <- deparse(substitute(x))
    rnk <- length(dim(x))
    if(rnk == 0L)
	stop("invalid table 'x'")
    if(rnk == 1L) {
	dn <- dimnames(x)
	nx <- dn[[1L]]
	if(is.null(xlab)) xlab <- names(dn)
	if(is.null(xlab)) xlab <- ""
	if(is.null(ylab)) ylab <- xnam
	ow <- options(warn = -1)
	is.num <- !any(is.na(xx <- as.numeric(nx))); options(ow)
	x0 <- if(is.num) xx else seq.int(x)
	plot(x0, unclass(x), type = type,
	     ylim = ylim, xlab = xlab, ylab = ylab, frame.plot = frame.plot,
	     lwd = lwd, ..., xaxt = "n")
	xaxt <-
	    if(length(as <- list(...))) {
		if(!is.null(as$axes) && !as$axes) "n" else as$xaxt
	    }## else NULL
	axis(1, at = x0, labels = nx, xaxt = xaxt)
    } else {
	if(length(as <- list(...)) && !is.null(as$main)) # use 'main'
	    mosaicplot(x, xlab = xlab, ylab = ylab, ...)
	else # default main
	    mosaicplot(x, xlab = xlab, ylab = ylab, main = xnam, ...)
    }
}

plot.formula <-
function(formula, data = parent.frame(), ..., subset,
         ylab = varnames[response], ask = dev.interactive())
{
    enquote <- function(x) as.call(list(as.name("quote"), x))

    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame())))
	m$data <- as.data.frame(data)

    dots <- m$...
    dots <- lapply(dots, eval, data, parent.frame())
    ## need to avoid evaluation of expressions in do.call later.
    ## see PR#10525
    nmdots <- names(dots)
    if("main" %in% nmdots) dots[["main"]] <- enquote(dots[["main"]])
    if("sub" %in% nmdots) dots[["sub"]] <- enquote(dots[["sub"]])
    if("xlab" %in% nmdots) dots[["xlab"]] <- enquote(dots[["xlab"]])


    m$ylab <- m$... <- m$ask <- NULL
    subset.expr <- m$subset
    m$subset <- NULL
    ## FIXME: model.frame is in stats
    require(stats, quietly=TRUE)
    m[[1L]] <- as.name("model.frame")
    m <- as.call(c(as.list(m), list(na.action = NULL)))
    mf <- eval(m, parent.frame())
    if (!missing(subset)) {
	s <- eval(subset.expr, data, parent.frame())
	l <- nrow(mf)
	dosub <- function(x) if (length(x) == l) x[s] else x
	dots <- lapply(dots, dosub)
	mf <- mf[s,]
    }
    ## check for horizontal arg
    horizontal <- FALSE
    if("horizontal" %in% names(dots)) horizontal <- dots[["horizontal"]]
    response <- attr(attr(mf, "terms"), "response")
    if (response) {
	varnames <- names(mf)
	y <- mf[[response]]
	funname <- NULL
	xn <- varnames[-response]
        ## <FIXME> why is this trying to do method dispatch?
	if( is.object(y) ) {
	    found <- FALSE
	    for(j in class(y)) {
		funname <- paste("plot.",j,sep = "")
		if( exists(funname) ) {
		    found <- TRUE
		    break
		}
	    }
	    if( !found )
		funname <- NULL
	}
	if( is.null(funname) )
	    funname <- "plot"
	if (length(varnames) > 2L) {
            oask <- devAskNewPage(ask)
            on.exit(devAskNewPage(oask))
	}
        if(length(xn)) {
            if( !is.null(xlab<- dots[["xlab"]]) )
                dots <- dots[-match("xlab", names(dots))]
            for (i in xn) {
                xl <- if(is.null(xlab)) i else xlab
                yl <- ylab
                if(horizontal && is.factor(mf[[i]])) {yl <- xl; xl <- ylab}
                do.call(funname,
                        c(list(mf[[i]], y, ylab = yl, xlab = xl), dots))
               }
	} else do.call(funname, c(list(y, ylab = ylab), dots))
    }
    else do.call("plot.data.frame", c(list(mf), dots))
    invisible()
}

lines.formula <-
function(formula,  data = parent.frame(), ..., subset)
{
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame())))
	m$data <- as.data.frame(data)
    dots <- m$...
    dots <- lapply(dots, eval, data, parent.frame())
    m$... <- NULL
    m[[1L]] <- as.name("model.frame")
    m <- as.call(c(as.list(m), list(na.action = NULL)))
    mf <- eval(m, parent.frame())
    if (!missing(subset)) {
	s <- eval(m$subset, data, parent.frame())
        ## need the number of points before subsetting
	if(!missing(data)) {
            l <- nrow(data)
        } else {
            mtmp <- m
            mtmp$subset <- NULL
            l <- nrow(eval(mtmp, parent.frame()))
        }
	dosub <- function(x) if (length(x) == l) x[s] else x
	dots <- lapply(dots, dosub)
    }
    response <- attr(attr(mf, "terms"), "response")
    if (response) {
	varnames <- names(mf)
	y <- mf[[response]]
	if (length(varnames) > 2L)
	    stop("cannot handle more than one 'x' coordinate")
	xn <- varnames[-response]
	if (length(xn) == 0L)
	    do.call("lines", c(list(y), dots))
	else
	    do.call("lines", c(list(mf[[xn]], y), dots))
    }
    else
	stop("must have a response variable")
}

points.formula <-
function(formula, data = parent.frame(), ..., subset)
{
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame())))
	m$data <- as.data.frame(data)
    dots <- m$...
    dots <- lapply(dots, eval, data, parent.frame())
    m$... <- NULL
    m[[1L]] <- as.name("model.frame")
    m <- as.call(c(as.list(m), list(na.action = NULL)))
    mf <- eval(m, parent.frame())
    if (!missing(subset)) {
	s <- eval(m$subset, data, parent.frame())
        ## need the number of points before subsetting
	if(!missing(data)) {
            l <- nrow(data)
        } else {
            mtmp <- m
            mtmp$subset <- NULL
            l <- nrow(eval(mtmp, parent.frame()))
        }
	dosub <- function(x) if (length(x) == l) x[s] else x
	dots <- lapply(dots, dosub)
    }
    response <- attr(attr(mf, "terms"), "response")
    if (response) {
	varnames <- names(mf)
	y <- mf[[response]]
	if (length(varnames) > 2L)
	    stop("cannot handle more than one 'x' coordinate")
	xn <- varnames[-response]
	if (length(xn) == 0L)
	    do.call("points", c(list(y), dots))
	else
	    do.call("points", c(list(mf[[xn]], y), dots))
    }
    else
	stop("must have a response variable")
}

plot.xy <- function(xy, type, pch = par("pch"), lty = par("lty"),
                    col = par("col"), bg = NA, cex = 1, lwd = par("lwd"),
                    ...)
    .Internal(plot.xy(xy, type, pch, lty, col, bg, cex, lwd, ...))


plot.new <- function()
{
    .Internal(plot.new())
    for(fun in getHook("plot.new")) {
        if(is.character(fun)) fun <- get(fun)
        try(fun())
    }
    invisible()
}

frame <- plot.new

plot.window <- function(xlim, ylim, log = "", asp = NA, ...)
    .Internal(plot.window(xlim, ylim, log, asp, ...))

plot.data.frame <- function (x, ...)
{
    plot2 <- function(x, xlab=names(x)[1L], ylab=names(x)[2L], ...)
        plot(x[[1L]], x[[2L]], xlab=xlab, ylab=ylab, ...)

    if(!is.data.frame(x))
	stop("'plot.data.frame' applied to non data frame")
    if(ncol(x) == 1) {
        x1 <- x[[1L]]
        cl <- class(x1)
        if(cl %in% c("integer", "numeric"))  stripchart(x1, ...)
        else plot(x1, ...) # factor, ts, complex ...
    } else if(ncol(x) == 2) {
        plot2(x, ...)
    } else {
	pairs(data.matrix(x), ...)
    }
}

## unexported hook for testing
## .newplot.hook <- function()
## {
##     pp <- par(c("mfg","mfcol","oma","mar"))
##     if(all(pp$mfg[1L:2] == c(1, pp$mfcol[2L]))) {
## 	outer <- (oma4 <- pp$oma[4L]) > 0; mar4 <- pp$mar[4L]
## 	mtext(paste("help(", ..nameEx, ")"), side = 4,
##               line = if(outer)max(1, oma4 - 1) else min(1, mar4 - 1),
##               outer = outer, adj = 1, cex = .8, col = "orchid", las=3)
##     }
## }

.units <- c("device", "ndc", "", "", "", "", "nic", "nfc", "", "", "", "",
            "user", "inches", "", "", "npc")

grconvertX <- function(x, from = "user", to = "user")
{
    from <- pmatch(from, .units)
    to <- pmatch(to, .units)
    .Internal(grconvertX(as.double(x), from, to))
}

grconvertY <- function(y, from = "user", to = "user")
{
    from <- pmatch(from, .units)
    to <- pmatch(to, .units)
    .Internal(grconvertY(as.double(y), from, to))
}
