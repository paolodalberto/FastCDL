#  File src/library/stats/R/ar.mle.R
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

ar.mle <- function (x, aic = TRUE, order.max = NULL, na.action = na.fail,
                    demean = TRUE, series = NULL, ...)
{
    if(is.null(series)) series <- deparse(substitute(x))
    ists <- is.ts(x)
    if (!is.null(dim(x)))
        stop("MLE only implemented for univariate series")
    x <- na.action(as.ts(x))
    if(any(is.na(x))) stop("NAs in 'x'")
    if(!is.numeric(x))
        stop("'x' must be numeric")
    if(ists)  xtsp <- tsp(x)
    xfreq <- frequency(x)
    x <- as.vector(x)
    n.used <- length(x)
    order.max <- if (is.null(order.max))
        min(n.used-1, 12, floor(10 * log10(n.used)))
    else round(order.max)

    if (order.max < 0) stop ("'order.max' must be >= 0")
    else if (order.max >= n.used) stop("'order.max' must be < 'n.used'")
    if (aic) {
        coefs <- matrix(NA, order.max+1, order.max+1)
        var.pred <- numeric(order.max+1)
        xaic <- numeric(order.max+1)
        xm <- if(demean) mean(x) else 0
        coefs[1, 1] <- xm
        var0 <- sum((x-xm)^2)/n.used
        var.pred[1L] <- var0
        xaic[1L] <- n.used * log(var0) + 2 * demean + 2 + n.used + n.used * log(2 * pi)
        for(i in 1L:order.max) {
            fit <- arima0(x, order=c(i, 0, 0), include.mean=demean)
            coefs[i+1, 1L:(i+demean)] <- fit$coef[1L:(i+demean)]
            xaic[i+1] <- fit$aic
            var.pred[i+1] <- fit$sigma2
        }
        xaic <- xaic - min(xaic)
        names(xaic) <- 0:order.max
        order <- (0:order.max)[xaic == 0]
        ar <- coefs[order+1, 1L:order]
        x.mean <- coefs[order+1, order+1]
        var.pred <- var.pred[order+1]
    } else {
        order <- order.max
        fit <- arima0(x, order=c(order, 0, 0), include.mean=demean)
        coefs <- fit$coef
        if(demean) {
            ar <- coefs[-length(coefs)]
            x.mean <- coefs[length(coefs)]
        } else {
            ar <- coefs
            x.mean <- 0
        }
        var.pred <- fit$sigma2
        xaic <- structure(0, names=order)
    }
    if(order > 0)
        resid <- c(rep(NA, order), embed(x - x.mean, order+1) %*% c(1, -ar))
    else resid <- as.vector(x) - x.mean
    if(ists) {
        attr(resid, "tsp") <- xtsp
        attr(resid, "class") <- "ts"
    }
    res <- list(order = order, ar = ar, var.pred = var.pred,
                x.mean = x.mean, aic = xaic,
                n.used = n.used, order.max = order.max,
                partialacf=NULL, resid=resid, method = "MLE",
                series = series, frequency = xfreq, call = match.call())
    if(order > 0) {
        xacf <- acf(x, type = "covariance", lag.max = order, plot=FALSE)$acf
        res$asy.var.coef <- solve(toeplitz(drop(xacf)[seq_len(order)])) *
            var.pred/n.used
    }
    class(res) <- "ar"
    res
}
