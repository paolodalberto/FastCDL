
## need some standard blues to plot ; output of brewer.pal(9, "Blues"):
blues9 <- c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6",
	    "#4292C6", "#2171B5", "#08519C", "#08306B")

.smoothScatterCalcDensity <- function(x, nbin, bandwidth, range.x)
{
    if (length(nbin) == 1)
	nbin <- c(nbin, nbin)
    if (!is.numeric(nbin) || length(nbin) != 2)
	stop("'nbin' must be numeric of length 1 or 2")

    if (missing(bandwidth)) { ## cheap
	bandwidth <- diff(apply(x, 2, quantile,
				probs = c(0.05, 0.95),
                                na.rm = TRUE, names=FALSE)) / 25
	bandwidth[bandwidth==0] <- 1
    }
    else {
	if(!is.numeric(bandwidth)) stop("'bandwidth' must be numeric")
	if(any(bandwidth <= 0)) stop("'bandwidth' must be positive")
    }
    ## create density map
    rv <- KernSmooth::bkde2D(x, bandwidth=bandwidth, gridsize=nbin,
			     range.x=range.x)
    rv$bandwidth <- bandwidth
    rv
}

densCols <- function(x, y=NULL, nbin=128, bandwidth,
		     colramp=colorRampPalette(blues9[-(1:3)]))
{
    ## similar as in plot.default
    xy <- xy.coords(x, y)

    ## deal with NA, etc
    select <- is.finite(xy$x) & is.finite(xy$y)
    x <- cbind(xy$x, xy$y)[select, ]

    ## create density map
    map <- .smoothScatterCalcDensity(x, nbin, bandwidth)

    ## bin  x- and y- values
    mkBreaks <- function(u) u - diff(range(u))/(length(u)-1)/2
    xbin <- cut(x[,1], mkBreaks(map$x1), labels=FALSE)
    ybin <- cut(x[,2], mkBreaks(map$x2), labels=FALSE)

    dens <- map$fhat[cbind(xbin, ybin)]
    dens[is.na(dens)]<- 0

    ## transform densities to colors
    colpal <- cut(dens, length(dens), labels=FALSE)
    cols   <- rep(NA_character_, length(select))
    cols[select] <- colramp(length(dens))[colpal]

    cols
}
