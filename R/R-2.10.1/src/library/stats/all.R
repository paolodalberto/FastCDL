.packageName <- "stats"
#  File src/library/stats/R/AIC.R
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

#### Return the object's value of the Akaike Information Criterion
#### (or "An Inf.. Crit..")

AIC <- function(object, ..., k = 2) UseMethod("AIC")

## AIC for logLik objects
AIC.logLik <- function(object, ..., k = 2)
    -2 * c(object) + k * attr(object, "df")

AIC.default <- function(object, ..., k = 2)
{
    ## AIC for various fitted objects --- any for which there's a logLik() method:
    ll <- if("stats4" %in% loadedNamespaces()) stats4:::logLik else logLik
    if(length(list(...))) {# several objects: produce data.frame
	object <- list(object, ...)
	val <- lapply(object, ll)
	val <- as.data.frame(t(sapply(val,
				      function(el)
				      c(attr(el, "df"), AIC(el, k = k)))))
	names(val) <- c("df", "AIC")
        Call <- match.call()
        Call$k <- NULL
	row.names(val) <- as.character(Call[-1L])
	val
    } else AIC(ll(object), k = k)
}
#  File src/library/stats/R/ARMAtheory.R
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

ARMAacf <- function(ar = numeric(0), ma = numeric(0), lag.max = r,
                    pacf = FALSE)
{
    p <- length(ar)
    q <- length(ma)
    if(!p && !q) stop("empty model supplied")
    r <- max(p, q + 1)
    if(p > 0) {
        if(r > 1) {
            if(r > p) { ## pad with zeros so p >= q+1
                ar <- c(ar, rep(0, r - p))
                p <- r
            }
            A <- matrix(0, p + 1L, 2L * p + 1L)
            ind <- as.matrix(expand.grid(1L:(p + 1), 1L:(p+1)))[, 2L:1L]
            ind[, 2] <- ind[, 1L] + ind[, 2L] - 1L
            A[ind] <- c(1, -ar)
            A[,  1L:p] <- A[, 1L:p] + A[, (2L * p + 1L):(p + 2L)]
            rhs <- c(1, rep(0, p))
            if(q > 0) {
                psi <- c(1, ARMAtoMA(ar, ma, q))
                theta <- c(1, ma, rep(0, q+1L))
                for(k in 1 + 0:q) rhs[k] <- sum(psi * theta[k + 0:q])
            }
            ind <- (p+1):1
            Acf <- solve(A[ind, ind], rhs)
	    Acf <- Acf[-1L]/Acf[1L]
        } else Acf <- ar
        if(lag.max > p) {
            xx <- rep(0, lag.max - p)
            Acf <- c(Acf, filter(xx, ar, "recursive", init = rev(Acf)))
        }
        Acf <- c(1, Acf[1L:lag.max])
    } else if(q > 0) {
        x <- c(1, ma)
        Acf <- filter(c(x, rep(0, q)), rev(x), sides=1)[-(1L:q)]
        if(lag.max > q) Acf <- c(Acf, rep(0, lag.max - q))
        Acf <- Acf/Acf[1L]
    }
    names(Acf) <- 0:lag.max
    if(pacf) .C(R_uni_pacf, as.double(Acf), pacf = double(lag.max),
                as.integer(lag.max))$pacf
    else Acf
}

acf2AR <- function(acf)
{
    r <- as.double(drop(acf))
    order.max <- length(r) - 1
    if(order.max <= 0) stop("'acf' must be of length two or more")
    z <- .Fortran(R_eureka, as.integer(order.max), r, r,
                  coefs = double(order.max^2), vars = double(order.max),
                  double(order.max))
    nm <- paste("ar(",1L:order.max, ")", sep="")
    matrix(z$coefs, order.max, order.max, dimnames=list(nm, 1L:order.max))
}

ARMAtoMA <- function(ar = numeric(0), ma = numeric(0), lag.max)
    .Call(R_ARMAtoMA, as.double(ar), as.double(ma), as.integer(lag.max))
#  File src/library/stats/R/C.R
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

C <- function(object, contr, how.many, ...)
{
    if(!nlevels(object)) stop("object not interpretable as a factor")
    if(!missing(contr) && is.name(Xcontr <- substitute(contr)))
	contr <- switch(as.character(Xcontr),
			poly =	"contr.poly",
			helmert = "contr.helmert",
			sum = "contr.sum",
			treatment = "contr.treatment",
			SAS = "contr.SAS",
			contr
			)
    if(missing(contr)) {
	oc <- getOption("contrasts")
	contr <-
	    if(length(oc) < 2) # should not happen
		if(is.ordered(object)) contr.poly else contr.treatment
	    else oc[1 + is.ordered(object)]
    }
    if(missing(how.many) && !length(list(...)))
	contrasts(object) <- contr
    else {
	if(is.character(contr)) contr <- get(contr, mode = "function")
	if(is.function(contr)) contr <- contr(nlevels(object), ...)
	contrasts(object, how.many) <- contr
    }
    object
}
#  File src/library/stats/R/HoltWinters.R
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

HoltWinters <-
function (x,

          # smoothing parameters
          alpha    = NULL, # level
          beta     = NULL, # trend
          gamma    = NULL, # seasonal component
          seasonal = c("additive", "multiplicative"),
          start.periods = 2,

          # starting values
          l.start  = NULL, # level
          b.start  = NULL, # trend
          s.start  = NULL, # seasonal components vector of length `period'

          # starting values for optim
          optim.start = c(alpha = 0.3, beta = 0.1, gamma = 0.1),
          optim.control = list()
          )
{
    x <- as.ts(x)
    seasonal <- match.arg(seasonal)
    f <- frequency(x)

    if(!is.null(alpha) && (alpha == 0))
        stop ("cannot fit models without level ('alpha' must not be 0 or FALSE).")
    if(!all(is.null(c(alpha, beta, gamma))) &&
        any(c(alpha, beta, gamma) < 0 || c(alpha, beta, gamma) > 1))
        stop ("'alpha', 'beta' and 'gamma' must be within the unit interval.")
    if((is.null(gamma) || gamma > 0)) {
        if (seasonal == "multiplicative" && any(x <= 0))
            stop ("data must be strictly non-negative for multiplicative Holt-Winters.")
        if (start.periods < 2)
            stop ("need at least 2 periods to compute seasonal start values.")
    }

    ## initialization
    if(!is.null(gamma) && is.logical(gamma) && !gamma) {
        ## non-seasonal Holt-Winters
        expsmooth <- !is.null(beta) && is.logical(beta) && !beta
        if(is.null(l.start))
            l.start <- if(expsmooth) x[1L] else x[2L]
        if(is.null(b.start))
            if(is.null(beta) || !is.logical(beta) || beta)
                b.start <- x[2L] - x[1L]
        start.time <- 3 - expsmooth
        s.start    <- 0
    } else {
        ## seasonal Holt-Winters
        start.time <- f + 1
        wind       <- start.periods * f

        ## decompose series
        st <- decompose(ts(x[1L:wind], start = start(x), frequency = f),
                        seasonal)

        ## level & intercept
        dat <- na.omit(st$trend)
        m   <- lm(dat ~ seq_along(dat))

        if (is.null(l.start)) l.start <- as.vector(coef(m)[1L])
        if (is.null(b.start)) b.start <- as.vector(coef(m)[2L])
        if (is.null(s.start)) s.start <- st$figure
    }

    ## Call to filtering loop
    len <- length(x) - start.time + 1
    hw <- function(alpha, beta, gamma)
        .C("HoltWinters",
           as.double(x),
           as.integer(length(x)),
           as.double(alpha),
           as.double(beta),
           as.double(gamma),
           as.integer(start.time),
           as.integer(! + (seasonal == "multiplicative")),
           as.integer(f),
           as.integer(!is.logical(beta) || beta),
           as.integer(!is.logical(gamma) || gamma),

           a = as.double(l.start),
           b = as.double(b.start),
           s = as.double(s.start),

	   ## return values
           SSE = as.double(0L),
           level = double(len + 1),
           trend = double(len + 1),
           seasonal = double(len + f),

	   PACKAGE = "stats"
           )

    ## if alpha and/or beta and/or gamma are omitted, use optim to find the
    ## values minimizing the squared prediction error
    if (is.null(gamma)) {
        ## optimize gamma
        if (is.null(alpha)) {
            ## optimize alpha
            if (is.null(beta)) {
                ## optimize beta
                ## --> optimize alpha, beta, and gamma
                error <- function (p) hw(p[1L], p[2L], p[3L])$SSE
                sol   <- optim(optim.start, error, method = "L-BFGS-B",
                               lower = c(0, 0, 0), upper = c(1, 1, 1),
                               control = optim.control)
                alpha <- sol$par[1L]
                beta  <- sol$par[2L]
                gamma <- sol$par[3L]
            } else {
                ## !optimize beta
                ## --> optimize alpha and gamma
                error <- function (p) hw(p[1L], beta, p[2L])$SSE
                sol   <- optim(c(optim.start["alpha"], optim.start["gamma"]),
                               error, method = "L-BFGS-B",
                               lower = c(0, 0), upper = c(1, 1),
                               control = optim.control)
                alpha <- sol$par[1L]
                gamma <- sol$par[2L]
            }
        } else {
            ## !optimize alpha
            if (is.null(beta)) {
                ## optimize beta
                ## --> optimize beta and gamma
                error <- function (p) hw(alpha, p[1L], p[2L])$SSE
                sol   <- optim(c(optim.start["beta"], optim.start["gamma"]),
                               error, method = "L-BFGS-B",
                               lower = c(0, 0), upper = c(1, 1),
                               control = optim.control)
                beta  <- sol$par[1L]
                gamma <- sol$par[2L]
            } else {
                ## !optimize beta
                ## --> optimize gamma
                error <- function (p) hw(alpha, beta, p)$SSE
                gamma <- optimize(error, lower = 0, upper = 1)$minimum
            }
        }
    } else {
        ## !optimize gamma
        if (is.null(alpha)) {
            ## optimize alpha
            if (is.null(beta)) {
                ## optimize beta
                ## --> optimize alpha and beta
                error <- function (p) hw(p[1L], p[2L], gamma)$SSE
                sol   <- optim(c(optim.start["alpha"], optim.start["beta"]),
                               error, method = "L-BFGS-B",
                               lower = c(0, 0), upper = c(1, 1),
                               control = optim.control)
                alpha <- sol$par[1L]
                beta  <- sol$par[2L]
            } else {
                ## !optimize beta
                ## --> optimize alpha
                error <- function (p) hw(p, beta, gamma)$SSE
                alpha <- optimize(error, lower = 0, upper = 1)$minimum
            }
        } else {
            ## !optimize alpha
            if(is.null(beta)) {
                ## optimize beta
                ## --> optimize beta
                error <- function (p) hw(alpha, p, gamma)$SSE
                beta <- optimize(error, lower = 0, upper = 1)$minimum
            } ## else optimize nothing!
        }
    }

    ## get (final) results
    final.fit <- hw(alpha, beta, gamma)

    ## return fitted values and estimated coefficients along with parameters used
    fitted <- ts(cbind(xhat   = final.fit$level[-len-1],
                       level  = final.fit$level[-len-1],
                       trend  = if (!is.logical(beta) || beta)
                           final.fit$trend[-len-1],
                       season = if (!is.logical(gamma) || gamma)
                           final.fit$seasonal[1L:len]),
                 start = start(lag(x, k = 1 - start.time)),
                 frequency  = frequency(x)
                 )
    if (!is.logical(beta) || beta) fitted[,1] <- fitted[,1] + fitted[,"trend"]
    if (!is.logical(gamma) || gamma)
      fitted[,1] <- if (seasonal == "multiplicative")
        fitted[,1] * fitted[,"season"]
      else
        fitted[,1] + fitted[,"season"]
    structure(list(fitted    = fitted,
                   x         = x,
                   alpha     = alpha,
                   beta      = beta,
                   gamma     = gamma,
                   coefficients = c(a = final.fit$level[len + 1],
                                    b = if (!is.logical(beta) || beta) final.fit$trend[len + 1],
                                    s = if (!is.logical(gamma) || gamma) final.fit$seasonal[len + 1L:f]),
                   seasonal  = seasonal,
                   SSE       = final.fit$SSE,
                   call      = match.call()
                   ),
              class = "HoltWinters"
              )
}

## Predictions, optionally with prediction intervals
predict.HoltWinters <-
    function (object, n.ahead = 1, prediction.interval = FALSE,
              level = 0.95, ...)
{
    f <- frequency(object$x)

    vars <- function(h) {
        psi <- function(j)
            object$alpha * (1 + j * object$beta) +
                (j %% f == 0) * object$gamma * (1 - object$alpha)
        var(residuals(object)) * if (object$seasonal == "additive")
            sum(1, (h > 1) * sapply(1L:(h-1), function(j) crossprod(psi(j))))
        else {
            rel <- 1 + (h - 1) %% f
            sum(sapply(0:(h-1), function(j) crossprod (psi(j) * object$coefficients[2 + rel] / object$coefficients[2 + (rel - j) %% f])))
        }
    }

    ## compute predictions
    # level
    fit <- rep(as.vector(object$coefficients[1L]),n.ahead)
    # trend
    if (!is.logical(object$beta) || object$beta)
        fit <- fit + as.vector((1L:n.ahead)*object$coefficients[2L])
        # seasonal component
    if (!is.logical(object$gamma) || object$gamma)
        if (object$seasonal == "additive")
            fit <- fit + rep(object$coefficients[-(1L:(1+(!is.logical(object$beta) || object$beta)))],
                             length.out=length(fit))
        else
            fit <- fit * rep(object$coefficients[-(1L:(1+(!is.logical(object$beta) || object$beta)))],
                             length.out=length(fit))

    ## compute prediction intervals
    if (prediction.interval)
      int <- qnorm((1 + level) / 2) * sqrt(sapply(1L:n.ahead,vars))
    ts(
       cbind(fit = fit,
             upr = if(prediction.interval) fit + int,
             lwr = if(prediction.interval) fit - int
             ),
       start = end(lag(fitted(object)[,1], k = -1)),
       frequency  = frequency(fitted(object)[,1])
       )
}

residuals.HoltWinters <- function (object, ...) object$x - object$fitted[,1]

plot.HoltWinters <-
    function (x, predicted.values = NA, intervals = TRUE, separator = TRUE,
              col = 1, col.predicted = 2, col.intervals = 4, col.separator = 1,
              lty = 1, lty.predicted = 1, lty.intervals = 1, lty.separator = 3,
              ylab = "Observed / Fitted", main = "Holt-Winters filtering",
              ylim = NULL, ...)
{
    if (is.null(ylim))
      ylim <- range(na.omit(c(fitted(x)[,1], x$x, predicted.values)))

    preds <- length(predicted.values) > 1 || !is.na(predicted.values)

    ## plot fitted/predicted values
    plot(ts(c(fitted(x)[,1], if(preds) predicted.values[,1]),
            start = start(fitted(x)[,1]),
            frequency = frequency(fitted(x)[,1])),
         col = col.predicted,
         ylim = ylim,
         ylab = ylab, main = main,
         lty = lty.predicted,
         ...
         )

    ## plot prediction interval
    if(preds && intervals && ncol(predicted.values) > 1) {
        lines(predicted.values[,2], col = col.intervals, lty = lty.intervals)
        lines(predicted.values[,3], col = col.intervals, lty = lty.intervals)
    }

    ## plot observed values
    lines(x$x, col = col, lty = lty)

    ## plot separator
    if (separator && preds)
        abline (v = time(x$x)[length(x$x)], lty = lty.separator, col = col.separator)
}

## print function
print.HoltWinters <- function (x, ...)
{
    cat("Holt-Winters exponential smoothing",
        if (is.logical(x$beta) && !x$beta) "without" else "with", "trend and",
        if (is.logical(x$gamma) && !x$gamma) "without" else
        paste(if (is.logical(x$beta) && !x$beta) "with ", x$seasonal, sep=""),
        "seasonal component.\n")
    cat("\nCall:\n", deparse (x$call), "\n\n")
    cat("Smoothing parameters:\n")
    cat(" alpha: ", x$alpha, "\n")
    cat(" beta : ", x$beta, "\n")
    cat(" gamma: ", x$gamma, "\n\n")

    cat("Coefficients:\n")
    print(t(t(x$coefficients)))
    invisible(x)
}

# decompose additive/multiplicative series into trend/seasonal figures/noise
decompose <-
function (x, type = c("additive", "multiplicative"), filter = NULL)
{
    type <- match.arg(type)
    l <- length(x)
    f <- frequency(x)
    if (f <= 1 || length(na.omit(x)) < 2 * f)
        stop("time series has no or less than 2 periods")

    ## filter out seasonal components
    if (is.null(filter))
        filter <- if (!f %% 2)
            c(0.5, rep(1, f - 1), 0.5) / f
        else
            rep(1, f) / f
    trend <- filter(x, filter)

    ## compute seasonal components
    season <- if (type == "additive")
        x - trend
    else
        x / trend

    ## rearrange seasons at beginning/end
    season <- na.omit(c(as.numeric(window(season,
                                          start(x) + c(1, 0),
                                          end(x))),
                        as.numeric(window(season,
                                          start(x),
                                          start(x) + c(0, f))))
                      )

    ## average seasonal figures
    periods <- l%/%f
    index <- c(0, cumsum(rep(f, periods - 2)))
    figure <- numeric(f)
    for (i in 1L:f) figure[i] <- mean(season[index + i])

    ## normalize figure
    figure <- if (type == "additive")
        figure - mean(figure)
    else figure / mean(figure)

    seasonal <- ts(rep(figure, periods), start = start(x), frequency = f)

    ## return values
    structure(list(seasonal = seasonal,
                   trend = trend,
                   random = if (type == "additive")
                       x - seasonal - trend
                   else
                       x / seasonal / trend,
                   figure = figure,
                   type = type),
              class = "decomposed.ts")
}

plot.decomposed.ts <- function(x, ...)
{
    plot(cbind(
               observed = if (x$type == "additive")
                 x$random + x$trend + x$seasonal
               else
                 x$random * x$trend * x$seasonal,
               trend    = x$trend,
               seasonal = x$seasonal,
               random   = x$random
               ),
         main = paste("Decomposition of", x$type, "time series"),
         ...)
}

#  File src/library/stats/R/Kalman.R
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


KalmanLike <- function(y, mod, nit = 0, fast=TRUE)
{
    ## next call changes objects a, P, Pn if fast==TRUE: beware!
    x <- .Call("KalmanLike", y, mod$Z, mod$a, mod$P, mod$T, mod$V, mod$h,
               mod$Pn, as.integer(nit), FALSE, fast=fast, PACKAGE = "stats")
    names(x) <- c("ssq", "sumlog")
    s2 <- x[1L]/length(y)
    list(Lik = 0.5*(log(x[1L]/length(y)) + x[2L]/length(y)), s2 = s2)
}

KalmanRun <- function(y, mod, nit = 0, fast=TRUE)
{
    ## next call changes objects a, P, Pn if fast==TRUE: beware!
    z <- .Call("KalmanLike", y, mod$Z, mod$a, mod$P, mod$T, mod$V, mod$h,
               mod$Pn, as.integer(nit), TRUE, fast=fast, PACKAGE = "stats")
    names(z) <- c("values", "resid", "states")
    x <- z$values
    s2 <- x[1L]/length(y)
    z[[1L]] <- c(Lik = 0.5*(log(x[1L]/length(y)) + x[2L]/length(y)), s2 = s2)
    z
}

KalmanForecast <- function(n.ahead = 10, mod, fast=TRUE)
{
    a <- numeric(p <- length(mod$a))
    P <- matrix(0, p, p)
    a[] <- mod$a
    P[] <- mod$P
    ## next call changes objects a, P if fast==TRUE
    x <- .Call("KalmanFore", as.integer(n.ahead), mod$Z, a, P,
               mod$T, mod$V, mod$h, fast=fast, PACKAGE = "stats")
    names(x) <- c("pred", "var")
    x
}

KalmanSmooth <- function(y, mod, nit = 0)
{
    z <- .Call("KalmanSmooth", y, mod$Z, mod$a, mod$P, mod$T, mod$V, mod$h,
               mod$Pn, as.integer(nit), PACKAGE = "stats")
    names(z) <- c("smooth", "var")
    dn <- dim(z$smooth)
    dim(z$var) <- dn[c(1, 2, 2)]
    z
}
#  File src/library/stats/R/StructTS.R
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

StructTS <- function(x, type = c("level", "trend", "BSM"),
                     init = NULL, fixed = NULL, optim.control = NULL)
{
    KalmanLike2 <- function (y, mod, nit = 0)
    {
        x <- .Call(R_KalmanLike, y, mod$Z, mod$a, mod$P, mod$T, mod$V,
                   mod$h, mod$Pn, as.integer(nit), FALSE, fast=TRUE)
        0.5 * sum(x)/length(y)
    }
    makeLevel <- function(x)
    {
        T <- matrix(1., 1L, 1L)
        Z <- 1.
        xm <- if(is.na(x[1L])) mean(x, na.rm = TRUE) else x[1L]
        if(is.na(xm)) stop("the series is entirely NA")
        a <- xm
        P <- Pn <- matrix(0., 1L, 1L)
        h <- 1.0
        V <- diag(1)
        return(list(Z=Z, a=a, P=P, T=T, V=V, h=h, Pn=Pn))
    }
    makeTrend <- function(x)
    {
        T <- matrix(c(1.,0.,1.,1.), 2L, 2L)
        Z <- c(1., 0.)
        xm <- if(is.na(x[1L])) mean(x, na.rm = TRUE) else x[1L]
        if(is.na(xm)) stop("the series is entirely NA")
        a <- c(xm, 0)
        P <- Pn <- matrix(0., 2L, 2L)
        h <- 1.0
        V <- diag(2)
        return(list(Z=Z, a=a, P=P, T=T, V=V, h=h, Pn=Pn))
    }
    makeBSM <- function(x, nf)
    {
        if(nf <= 1L) stop("frequency must be a positive integer for BSM")
        T <- matrix(0., nf + 1L, nf + 1L)
        T[1L:2L, 1L:2L] <- c(1, 0, 1, 1)
        T[3L, ] <- c(0, 0, rep(-1, nf - 1L))
        ind <- 3:nf
        T[cbind(ind+1L, ind)] <- 1
        Z <- c(1., 0., 1., rep(0., nf - 2L))
        xm <- if(is.na(x[1L])) mean(x, na.rm = TRUE) else x[1L]
        if(is.na(xm)) stop("the series is entirely NA")
        a <- c(xm, rep(0, nf))
        P <- Pn <- matrix(0., nf+1L, nf+1L)
        h <- 1.
        V <- diag(c(1., 1., 1., rep(0., nf-2L)))
        return(list(Z=Z, a=a, P=P, T=T, V=V, h=h, Pn=Pn))
    }
    getLike <- function(par)
    {
        p <- cf
        p[mask] <- par
        if(all(p == 0)) return(1000)
        Z$V[cbind(1L:np, 1L:np)] <- p[-(np+1L)]*vx
        Z$h <- p[np+1]*vx
        Z$P[] <- 1e6*vx
        Z$a <- a0
        KalmanLike2(y, Z, -1.0)
    }

    series <- deparse(substitute(x))
    if(NCOL(x) > 1L)
        stop("only implemented for univariate time series")
    x <- as.ts(x)
    if(!is.numeric(x))
        stop("'x' must be numeric")
    storage.mode(x) <- "double"
    if(is.na(x[1L]))
        stop("the first value of the time series must not be missing")
    type <- if(missing(type)) if(frequency(x) > 1) "BSM" else "trend"
    else match.arg(type)
    dim(x) <- NULL
    xtsp <- tsp(x)
    nf <- frequency(x)
    Z <- switch(type,
                "level" = makeLevel(x),
                "trend" = makeTrend(x),
                "BSM" = makeBSM(x, nf)
                )
    a0 <- Z$a
    vx <- var(x, na.rm=TRUE)/100
    np <- switch(type, "level" = 1L, "trend" = 2L, "BSM" = 3L)
    if (is.null(fixed)) fixed <- rep(NA_real_, np+1L)
    mask <- is.na(fixed)
    if(!any(mask)) stop("all parameters were fixed")
    cf <- fixed/vx
    if(is.null(init)) init <- rep(1, np+1) else init <- init/vx

    y <- x
    res <- optim(init[mask], getLike, method = "L-BFGS-B",
                 lower = rep(0, np+1L), upper = rep(Inf, np+1L),
                 control = optim.control)
        if(res$convergence > 0)
            warning("possible convergence problem: optim gave code=",
                    res$convergence, " ", res$message)
    coef <- cf
    coef[mask] <- res$par
    Z$V[cbind(1L:np, 1L:np)] <- coef[1L:np]*vx
    Z$h <- coef[np+1]*vx
    Z$P[] <- 1e6*vx
    Z$a <- a0
    z <- KalmanRun(y, Z, -1)
    resid <- ts(z$resid)
    tsp(resid) <- xtsp
    Z0 <- Z; Z0$P[] <- 1e6*vx; Z0$a <- a0

    cn <- switch(type,
                 "level" = c("level"),
                 "trend" = c("level", "slope"),
                 "BSM" = c("level", "slope", "sea")
                 )
    states <- z$states
    if(type == "BSM") states <- states[, 1L:3L]
    dimnames(states) <- list(time(x), cn)
    states <- ts(states, start = xtsp[1L], frequency = nf)

    coef <- coef*vx
    names(coef) <- switch(type,
                          "level" = c("level", "epsilon"),
                          "trend" = c("level", "slope", "epsilon"),
                          "BSM" = c("level", "slope", "seas", "epsilon")
                          )
    loglik <- -length(y) * res$value + length(y) * log(2 * pi)
    res <- list(coef = coef, loglik = loglik, data = y,
                residuals = resid, fitted = states,
                call = match.call(), series = series,
                code = res$convergence, model = Z, model0 = Z0, xtsp = xtsp)
    class(res) <- "StructTS"
    res
}

print.StructTS <- function(x, digits = max(3, getOption("digits") - 3), ...)
{
    cat("\nCall:", deparse(x$call, width.cutoff = 75L), "", sep = "\n")
    cat("Variances:\n")
    print.default(x$coef, print.gap = 2L, digits=digits)
    invisible(x)
}

predict.StructTS <- function(object, n.ahead = 1, se.fit = TRUE, ...)
{
    xtsp <- object$xtsp
    z <- KalmanForecast(n.ahead, object$model)
    pred <- ts(z[[1L]], start = xtsp[2L] + 1/xtsp[3L], frequency = xtsp[3L])
    if (se.fit) {
        se <- ts(sqrt(z[[2L]]), start = xtsp[2L] + 1/xtsp[3L],
                 frequency = xtsp[3L])
        return(list(pred=pred, se=se))
    }
    else return(pred)
}

tsdiag.StructTS <- function(object, gof.lag = 10, ...)
{
    ## plot standardized residuals, acf of residuals, Ljung-Box p-values
    oldpar<- par(mfrow = c(3, 1))
    on.exit(par(oldpar))
    rs <- object$residuals
    stdres <- rs
    plot(stdres, type = "h", main = "Standardized Residuals", ylab = "")
    abline(h = 0.)
    acf(object$residuals, plot = TRUE, main = "ACF of Residuals",
        na.action = na.pass)
    nlag <- gof.lag
    pval <- numeric(nlag)
    for(i in 1L:nlag) pval[i] <- Box.test(rs, i, type = "Ljung-Box")$p.value
    plot(1L:nlag, pval, xlab = "lag", ylab = "p value", ylim = c(0,1),
         main = "p values for Ljung-Box statistic")
    abline(h = 0.05, lty = 2L, col = "blue")
}


tsSmooth <- function(object, ...) UseMethod("tsSmooth")

tsSmooth.StructTS <- function(object, ...)
{
    res <- KalmanSmooth(object$data, object$model0, -1)$smooth
    dn <- dim(fitted(object))
    res <- res[, 1L:dn[2L], drop = FALSE]
    dimnames(res) <- dimnames(fitted(object))
    ts(res, start = object$xtsp[1L], frequency = object$xtsp[3L])
}
#  File src/library/stats/R/TukeyHSD.R
#  Part of the R package, http://www.R-project.org
#  Copyright 2000-2001  Douglas M. Bates <bates@stat.wisc.edu>
#  Modified for base R 2002 B. D. Ripley
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

###
###               Tukey multiple comparisons for R
###

TukeyHSD <-
    function(x, which, ordered = FALSE, conf.level = 0.95, ...)
    UseMethod("TukeyHSD")

TukeyHSD.aov <-
    function(x, which = seq_along(tabs), ordered = FALSE,
             conf.level = 0.95, ...)
{
    mm <- model.tables(x, "means")
    if(is.null(mm$n))
        stop("no factors in the fitted model")
    tabs <- mm$tables[-1L]
    tabs <- tabs[which]
    ## mm$n need not be complete -- factors only -- so index by names
    nn <- mm$n[names(tabs)]
    nn_na <- is.na(nn)
    if(all(nn_na))
        stop("'which' specified no factors")
    if(any(nn_na)) {
        warning("'which' specified some non-factors which will be dropped")
        tabs <- tabs[!nn_na]
        nn <- nn[!nn_na]
    }
    out <- vector("list", length(tabs))
    names(out) <- names(tabs)
    MSE <- sum(resid(x)^2)/x$df.residual
    for (nm in names(tabs)) {
        tab <- tabs[[nm]]
        means <- as.vector(tab)
        nms <- if(length(d <- dim(tab)) > 1) {
            dn <- dimnames(tab)
            apply(do.call("expand.grid", dn), 1L, paste, collapse=":")
        } else names(tab)
        n <- nn[[nm]]
        ## expand n to the correct length if necessary
        if (length(n) < length(means)) n <- rep.int(n, length(means))
        if (as.logical(ordered)) {
            ord <- order(means)
            means <- means[ord]
            n <- n[ord]
            if (!is.null(nms)) nms <- nms[ord]
        }
        center <- outer(means, means, "-")
        keep <- lower.tri(center)
        center <- center[keep]
        width <- qtukey(conf.level, length(means), x$df.residual) *
            sqrt((MSE/2) * outer(1/n, 1/n, "+"))[keep]
        est <- center/(sqrt((MSE/2) * outer(1/n, 1/n, "+"))[keep])
        pvals <- ptukey(abs(est),length(means),x$df.residual,lower.tail=FALSE)
        dnames <- list(NULL, c("diff", "lwr", "upr","p adj"))
        if (!is.null(nms)) dnames[[1L]] <- outer(nms, nms, paste, sep = "-")[keep]
        out[[nm]] <- array(c(center, center - width, center + width,pvals),
                           c(length(width), 4), dnames)
    }
    class(out) <- c("multicomp", "TukeyHSD")
    attr(out, "orig.call") <- x$call
    attr(out, "conf.level") <- conf.level
    attr(out, "ordered") <- ordered
    out
}

print.TukeyHSD <- function(x, digits=getOption("digits"), ...)
{
    cat("  Tukey multiple comparisons of means\n")
    cat("    ", format(100*attr(x, "conf.level"), 2),
        "% family-wise confidence level\n", sep="")
    if (attr(x, "ordered"))
        cat("    factor levels have been ordered\n")
    cat("\nFit: ", deparse(attr(x, "orig.call"), 500), "\n\n", sep="")
    xx <- unclass(x)
    attr(xx, "orig.call") <- attr(xx, "conf.level") <- attr(xx, "ordered") <- NULL
    xx[] <- lapply(xx, function(z, digits)
               {z[, "p adj"] <- round(z[, "p adj"], digits); z},
                   digits=digits)
    print.default(xx, digits, ...)
    invisible(x)
}

plot.TukeyHSD <- function (x, ...)
{
    for (i in seq_along(x)) {
        xi <- x[[i]][, -4, drop=FALSE] # drop p-values
        yvals <- nrow(xi):1
        plot(c(xi[, "lwr"], xi[, "upr"]), rep.int(yvals, 2), type = "n",
             axes = FALSE, xlab = "", ylab = "", ...)
        axis(1, ...)
        axis(2, at = nrow(xi):1, labels = dimnames(xi)[[1L]],
             srt = 0, ...)
        abline(h = yvals, lty = 1, lwd = 0, col = "lightgray")
        abline(v = 0, lty = 2, lwd = 0, ...)
        segments(xi[, "lwr"], yvals, xi[, "upr"], yvals, ...)
        segments(as.vector(xi), rep.int(yvals - 0.1, 3), as.vector(xi),
                 rep.int(yvals + 0.1, 3), ...)
        title(main = paste(format(100 * attr(x, "conf.level"),
              2), "% family-wise confidence level\n", sep = ""),
              xlab = paste("Differences in mean levels of", names(x)[i]))
        box()
    }
}
#  File src/library/stats/R/acf.R
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

acf <-
    function (x, lag.max = NULL,
              type = c("correlation", "covariance", "partial"),
              plot = TRUE, na.action = na.fail, demean= TRUE, ...)
{
    type <- match.arg(type)
    if(type == "partial") {
        m <- match.call()
        m[[1L]] <- as.name("pacf")
        m$type <- NULL
        return(eval(m, parent.frame()))
    }
    series <- deparse(substitute(x))
    x <- na.action(as.ts(x))
    x.freq <- frequency(x)
    x <- as.matrix(x)
    if(!is.numeric(x))
        stop("'x' must be numeric")
    sampleT <- nrow(x)
    nser <- ncol(x)
    if (is.null(lag.max))
        lag.max <- floor(10 * (log10(sampleT) - log10(nser)))
    lag.max <- min(lag.max, sampleT - 1)
    if (lag.max < 0) stop("'lag.max' must be at least 0")
    if(demean)
	x <- sweep(x, 2, colMeans(x, na.rm = TRUE), check.margin=FALSE)
    lag <- matrix(1, nser, nser)
    lag[lower.tri(lag)] <- -1
    acf <- array(.C(R_acf,
                    as.double(x), as.integer(sampleT), as.integer(nser),
                    as.integer(lag.max), as.integer(type=="correlation"),
                    acf=double((lag.max+1L) * nser * nser), NAOK = TRUE
                    )$acf, c(lag.max + 1L, nser, nser))
    lag <- outer(0:lag.max, lag/x.freq)
    acf.out <- structure(.Data = list(acf = acf, type = type,
        n.used = sampleT, lag = lag, series = series, snames = colnames(x)),
        class = "acf")
    if (plot) {
        plot.acf(acf.out, ...)
        return(invisible(acf.out))
    } else return(acf.out)
}

pacf <- function(x, lag.max, plot, na.action, ...) UseMethod("pacf")

pacf.default <- function(x, lag.max = NULL, plot = TRUE,
                         na.action = na.fail, ...)
{
    series <- deparse(substitute(x))
    x <- drop(na.action(as.ts(x)))  # use univariate code for a single series
    if(!is.numeric(x)) stop("'x' must be numeric")
    x.freq <- frequency(x)
    sampleT <- NROW(x)
    if (is.null(lag.max))
        lag.max <- if(is.matrix(x)) floor(10 * (log10(sampleT) - log10(ncol(x))))
        else floor(10 * (log10(sampleT)))
    lag.max <- min(lag.max, sampleT - 1)
    if (lag.max < 1) stop("'lag.max' must be at least 1")

    if(is.matrix(x)) {
        if(any(is.na(x))) stop("NAs in 'x'")
        nser <- ncol(x)
        x <- sweep(x, 2, colMeans(x), check.margin=FALSE)
        lag <- matrix(1, nser, nser)
        lag[lower.tri(lag)] <- -1
        pacf <- ar.yw(x, order.max = lag.max)$partialacf
        lag <- outer(1L:lag.max, lag/x.freq)
        snames <- colnames(x)
    } else {
        x <- scale(x, TRUE, FALSE)
        acf <- drop(acf(x, lag.max = lag.max, plot = FALSE,
                        na.action = na.action)$acf)
        pacf <- array(.C(R_uni_pacf,
                         as.double(acf),
                         pacf = double(lag.max),
                         as.integer(lag.max))$pacf,
                      dim=c(lag.max,1L,1L))
        lag <- array((1L:lag.max)/x.freq, dim=c(lag.max,1L,1L))
        snames <- NULL
    }

    acf.out <- structure(.Data = list(acf = pacf, type = "partial",
                         n.used = sampleT, lag = lag, series = series,
                         snames = snames), class = "acf")
    if (plot) {
        plot.acf(acf.out, ...)
        invisible(acf.out)
    } else acf.out
}

plot.acf <-
    function (x, ci = 0.95, type = "h", xlab = "Lag", ylab = NULL,
              ylim = NULL, main = NULL, ci.col="blue",
              ci.type = c("white", "ma"),
              max.mfrow = 6,
              ask = Npgs > 1 && dev.interactive(),
              mar = if(nser > 2) c(3,2,2,0.8) else par("mar"),
              oma = if(nser > 2) c(1,1.2,1,1) else par("oma"),
              mgp = if(nser > 2) c(1.5,0.6,0) else par("mgp"),
              xpd = par("xpd"),
              cex.main = if(nser > 2) 1 else par("cex.main"),
              verbose = getOption("verbose"),
              ...)
{
    ci.type <- match.arg(ci.type)
    if((nser <- ncol(x$lag)) < 1) stop("x$lag must have at least 1 column")
    if (is.null(ylab))
        ylab <- switch(x$type,
                       correlation = "ACF",
                       covariance = "ACF (cov)",
                       partial = "Partial ACF")
    if (is.null(snames <- x$snames))
        snames <- paste("Series ", if (nser == 1) x$series else 1L:nser)

    with.ci <- ci > 0 && x$type != "covariance"
    with.ci.ma <- with.ci && ci.type == "ma" && x$type == "correlation"
    if(with.ci.ma && x$lag[1,1,1] != 0) {
        warning("can use ci.type=\"ma\" only if first lag is 0")
        with.ci.ma <- FALSE
    }
    clim0 <- if (with.ci) qnorm((1 + ci)/2)/sqrt(x$n.used) else c(0, 0)

    Npgs <- 1 ## we will do [ Npgs x Npgs ] pages !
    nr <- nser
    if(nser > 1) { ## at most m x m (m := max.mfrow)  panels per page
        sn.abbr <- if(nser > 2) abbreviate(snames) else snames

        if(nser > max.mfrow) {
            ##  We need more than one page: The plots are laid out
            ##  such that we can manually paste the paper pages and get a
            ##  nice square layout with diagonal !
            ## NB: The same applies to pairs() where we'd want several pages
            Npgs <- ceiling(nser / max.mfrow)
            nr <- ceiling(nser / Npgs)  # <= max.mfrow
        }
        opar <- par(mfrow = rep(nr, 2), mar = mar, oma = oma, mgp = mgp,
                    ask = ask, xpd = xpd, cex.main = cex.main)
        on.exit(par(opar))
        if(verbose) { # FIXME: message() can be suppressed but not str()
            message("par(*) : ", appendLF=FALSE, domain = NA)
            str(par("mfrow","cex", "cex.main","cex.axis","cex.lab","cex.sub"))
        }
    }

    if (is.null(ylim)) {
        ## Calculate a common scale
        ylim <- range(x$acf[, 1L:nser, 1L:nser], na.rm = TRUE)
        if (with.ci) ylim <- range(c(-clim0, clim0, ylim))
        if (with.ci.ma) {
	    for (i in 1L:nser) {
                clim <- clim0 * sqrt(cumsum(c(1, 2*x$acf[-1, i, i]^2)))
                ylim <- range(c(-clim, clim, ylim))
            }
        }
    }

    for (I in 1L:Npgs) for (J in 1L:Npgs) {
        ## Page [ I , J ] : Now do   nr x nr  'panels' on this page
        iind <- (I-1)*nr + 1L:nr
        jind <- (J-1)*nr + 1L:nr
        if(verbose)
            message("Page [",I,",",J,"]: i =",
                    paste(iind,collapse=","),"; j =",
                    paste(jind,collapse=","), domain = NA)
        for (i in iind) for (j in jind)
            if(max(i,j) > nser) {
                frame(); box(col = "light gray")
                ## the above is EXTREMELY UGLY; should have a version
                ## of frame() that really does advance a frame !!
            }
            else {
                clim <- if (with.ci.ma && i == j)
                    clim0 * sqrt(cumsum(c(1, 2*x$acf[-1, i, j]^2))) else clim0
                plot(x$lag[, i, j], x$acf[, i, j], type = type, xlab = xlab,
                     ylab = if(j==1) ylab else "", ylim = ylim, ...)
                abline(h = 0)
                if (with.ci && ci.type == "white")
                    abline(h = c(clim, -clim), col = ci.col, lty = 2)
                else if (with.ci.ma && i == j) {
                    clim <- clim[-length(clim)]
                    lines(x$lag[-1, i, j], clim, col = ci.col, lty = 2)
                    lines(x$lag[-1, i, j], -clim, col = ci.col, lty = 2)
                }
                title(if (!is.null(main)) main else
                      if (i == j) snames[i]
                      else paste(sn.abbr[i], "&", sn.abbr[j]),
                      line = if(nser > 2) 1 else 2)
            }
        if(Npgs > 1) {                  # label the page
            mtext(paste("[",I,",",J,"]"), side=1, line = -0.2, adj=1,
                  col = "dark gray", cex = 1, outer = TRUE)
        }
    }
    invisible()
}

ccf <- function(x, y, lag.max = NULL,
                type = c("correlation", "covariance"),
                plot = TRUE, na.action = na.fail, ...)
{
    type <- match.arg(type)
    if(is.matrix(x) || is.matrix(y))
        stop("univariate time series only")
    X <- na.action(ts.intersect(as.ts(x), as.ts(y)))
    colnames(X) <- c(deparse(substitute(x))[1L], deparse(substitute(y))[1L])
    acf.out <- acf(X, lag.max = lag.max, plot = FALSE, type = type)
    lag <- c(rev(acf.out$lag[-1,2,1]), acf.out$lag[,1,2])
    y   <- c(rev(acf.out$acf[-1,2,1]), acf.out$acf[,1,2])
    acf.out$acf <- array(y, dim=c(length(y),1L,1L))
    acf.out$lag <- array(lag, dim=c(length(y),1L,1L))
    acf.out$snames <- paste(acf.out$snames, collapse = " & ")
    if (plot) {
        plot(acf.out, ...)
        return(invisible(acf.out))
    } else return(acf.out)
}

"[.acf" <- function(x, i, j)
{
    if(missing(j)) j <- seq_len(ncol(x$lag))
    ii <- if(missing(i)) seq_len(nrow(x$lag))
    else match(i, x$lag[, 1, 1], nomatch = NA_integer_)
    x$acf <- x$acf[ii, j, j, drop = FALSE]
    x$lag <- x$lag[ii, j, j, drop = FALSE]
    x
}

print.acf <- function(x, digits=3, ...)
{
    type <- match(x$type, c("correlation", "covariance", "partial"))
    msg <- c("Autocorrelations", "Autocovariances", "Partial autocorrelations")
    cat("\n", msg[type]," of series ", sQuote(x$series), ", by lag\n\n",
        sep="")
    nser <- ncol(x$lag)
    if(type != 2) x$acf <- round(x$acf, digits)
    if(nser == 1) {
        acfs <- drop(x$acf)
        names(acfs) <- format(drop(x$lag), digits=3)
        print(acfs, digits = digits, ...)
    } else {
        acfs <- format(x$acf, ...)
        lags <- format(x$lag, digits=3)
        acfs <- array(paste(acfs, " (", lags, ")", sep=""), dim=dim(x$acf))
        dimnames(acfs)  <- list(rep("", nrow(x$lag)), x$snames, x$snames)
        print(acfs, quote=FALSE, ...)
    }
    invisible(x)
}
#  File src/library/stats/R/add.R
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

## version to return NA for df = 0, as R did before 2.7.0
safe_pchisq <- function(q, df, ...)
{
    df[df <= 0] <- NA
    pchisq(q=q, df=df, ...)
}
## and to avoid a warning
safe_pf <- function(q, df1, ...)
{
    df1[df1 <= 0] <- NA
    pf(q=q, df1=df1, ...)
}


add1 <- function(object, scope, ...) UseMethod("add1")

add1.default <- function(object, scope, scale = 0, test=c("none", "Chisq"),
			 k = 2, trace = FALSE, ...)
{
    if(missing(scope) || is.null(scope)) stop("no terms in scope")
    if(!is.character(scope))
	scope <- add.scope(object, update.formula(object, scope))
    if(!length(scope))
	stop("no terms in scope for adding to object")
#     newform <- update.formula(object,
#                               paste(". ~ . +", paste(scope, collapse="+")))
#     data <- model.frame(update(object, newform)) # remove NAs
#     object <- update(object, data = data)
    ns <- length(scope)
    ans <- matrix(nrow = ns + 1L, ncol = 2L,
                  dimnames = list(c("<none>", scope), c("df", "AIC")))
    ans[1L,  ] <- extractAIC(object, scale, k = k, ...)
    n0 <- length(object$residuals)
    env <- environment(formula(object))
    for(i in seq(ns)) {
	tt <- scope[i]
	if(trace > 1) {
	    cat("trying +", tt, "\n", sep='')
	    utils::flush.console()
	}
	nfit <- update(object, as.formula(paste("~ . +", tt)),
                       evaluate = FALSE)
	nfit <- eval(nfit, envir=env) # was  eval.parent(nfit)
	ans[i+1L, ] <- extractAIC(nfit, scale, k = k, ...)
        if(length(nfit$residuals) != n0)
            stop("number of rows in use has changed: remove missing values?")
    }
    dfs <- ans[, 1L] - ans[1L, 1L]
    dfs[1L] <- NA
    aod <- data.frame(Df = dfs, AIC = ans[, 2L])
    test <- match.arg(test)
    if(test == "Chisq") {
	dev <- ans[, 2L] - k*ans[, 1L]
	dev <- dev[1L] - dev; dev[1L] <- NA
	nas <- !is.na(dev)
	P <- dev
	P[nas] <- safe_pchisq(dev[nas], dfs[nas], lower.tail=FALSE)
	aod[, c("LRT", "Pr(Chi)")] <- list(dev, P)
    }
    head <- c("Single term additions", "\nModel:",
	      deparse(as.vector(formula(object))),
	      if(scale > 0) paste("\nscale: ", format(scale), "\n"))
    class(aod) <- c("anova", "data.frame")
    attr(aod, "heading") <- head
    aod
}

check_exact <- function(object)
{
    w <- object$weights
    if(is.null(w)) {
        mss <- sum(object$fitted.values^2)
        rss <- sum(object$residuals^2)
    } else {
        mss <- sum(w * object$fitted.values^2)
        rss <- sum(w * object$residuals^2)
    }
    if(rss < 1e-10*mss)
        warning("attempting model selection on an essentially perfect fit is nonsense", call. = FALSE)
}

add1.lm <- function(object, scope, scale = 0, test=c("none", "Chisq", "F"),
		    x = NULL, k = 2,...)
{
    Fstat <- function(table, RSS, rdf) {
	dev <- table$"Sum of Sq"
	df <- table$Df
	rms <- (RSS - dev)/(rdf - df)
	Fs <- (dev/df)/rms
	Fs[df < .Machine$double.eps] <- NA
	P <- Fs
	nnas <- !is.na(Fs)
	P[nnas] <- safe_pf(Fs[nnas], df[nnas], rdf - df[nnas], lower.tail=FALSE)
	list(Fs=Fs, P=P)
    }

    check_exact(object)
    if(missing(scope) || is.null(scope)) stop("no terms in scope")
    if(!is.character(scope))
	scope <- add.scope(object, update.formula(object, scope))
    if(!length(scope))
	stop("no terms in scope for adding to object")
    oTerms <- attr(object$terms, "term.labels")
    int <- attr(object$terms, "intercept")
    ns <- length(scope)
    y <- object$residuals + object$fitted.values
    ## predict(object) applies na.action where na.exclude results in too long
    dfs <- numeric(ns+1)
    RSS <- numeric(ns+1)
    names(dfs) <- names(RSS) <- c("<none>", scope)
    add.rhs <- paste(scope, collapse = "+")
    add.rhs <- eval(parse(text = paste("~ . +", add.rhs)))
    new.form <- update.formula(object, add.rhs)
    Terms <- terms(new.form)
    if(is.null(x)) {
	fc <- object$call
	fc$formula <- Terms
	## model.frame.lm looks at the terms part for the environment
	fob <- list(call = fc, terms = Terms)
	class(fob) <- oldClass(object)
	m <- model.frame(fob, xlev = object$xlevels)
	x <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
        offset <- model.offset(m)
        wt <- model.weights(m)
        oldn <- length(y)
        y <- model.response(m, "numeric")
        newn <- length(y)
        if(newn < oldn)
            warning(gettextf("using the %d/%d rows from a combined fit",
                             newn, oldn), domain = NA)
    } else {
        ## need to get offset and weights from somewhere
        wt <- object$weights
        offset <- object$offset
    }
    n <- nrow(x)
    Terms <- attr(Terms, "term.labels")
    asgn <- attr(x, "assign")
    ousex <- match(asgn, match(oTerms, Terms), 0L) > 0L
    if(int) ousex[1L] <- TRUE
    iswt <- !is.null(wt)
    X <- x[, ousex, drop = FALSE]
    z <- if(iswt) lm.wfit(X, y, wt, offset=offset)
    else lm.fit(X, y, offset=offset)
    dfs[1L] <- z$rank
    class(z) <- "lm" # needed as deviance.lm calls generic residuals()
    RSS[1L] <- deviance(z)
    ## workaround for PR#7842. terms.formula may have flipped interactions
    sTerms <- sapply(strsplit(Terms, ":", fixed=TRUE),
                     function(x) paste(sort(x), collapse=":"))
    for(tt in scope) {
        stt <- paste(sort(strsplit(tt, ":")[[1L]]), collapse=":")
	usex <- match(asgn, match(stt, sTerms), 0L) > 0L
	X <- x[, usex|ousex, drop = FALSE]
	z <- if(iswt) lm.wfit(X, y, wt, offset=offset)
        else lm.fit(X, y, offset=offset)
        class(z) <- "lm" # needed as deviance.lm calls generic residuals()
	dfs[tt] <- z$rank
	RSS[tt] <- deviance(z)
    }
    if(scale > 0) aic <- RSS/scale - n + k*dfs
    else aic <- n * log(RSS/n) + k*dfs
    dfs <- dfs - dfs[1L]
    dfs[1L] <- NA
    aod <- data.frame(Df = dfs, "Sum of Sq" = c(NA, RSS[1L] - RSS[-1L]),
		      RSS = RSS, AIC = aic,
                      row.names = names(dfs), check.names = FALSE)
    if(scale > 0) names(aod) <- c("Df", "Sum of Sq", "RSS", "Cp")
    test <- match.arg(test)
    if(test == "Chisq") {
        dev <- aod$"Sum of Sq"
        if(scale == 0) {
            dev <- n * log(RSS/n)
            dev <- dev[1L] - dev
            dev[1L] <- NA
        } else dev <- dev/scale
        df <- aod$Df
        nas <- !is.na(df)
        dev[nas] <- safe_pchisq(dev[nas], df[nas], lower.tail=FALSE)
        aod[, "Pr(Chi)"] <- dev
    } else if(test == "F") {
	rdf <- object$df.residual
	aod[, c("F value", "Pr(F)")] <- Fstat(aod, aod$RSS[1L], rdf)
    }
    head <- c("Single term additions", "\nModel:",
	      deparse(as.vector(formula(object))),
	      if(scale > 0) paste("\nscale: ", format(scale), "\n"))
    class(aod) <- c("anova", "data.frame")
    attr(aod, "heading") <- head
    aod
}

add1.glm <- function(object, scope, scale = 0, test=c("none", "Chisq", "F"),
		     x = NULL, k = 2, ...)
{
    Fstat <- function(table, rdf) {
	dev <- table$Deviance
	df <- table$Df
	diff <- pmax(0, (dev[1L] - dev)/df)
	Fs <- (diff/df)/(dev/(rdf-df))
	Fs[df < .Machine$double.eps] <- NA
	P <- Fs
	nnas <- !is.na(Fs)
	P[nnas] <- safe_pf(Fs[nnas], df[nnas], rdf - df[nnas], lower.tail=FALSE)
	list(Fs=Fs, P=P)
    }
    if(!is.character(scope))
	scope <- add.scope(object, update.formula(object, scope))
    if(!length(scope))
	stop("no terms in scope for adding to object")
    oTerms <- attr(object$terms, "term.labels")
    int <- attr(object$terms, "intercept")
    ns <- length(scope)
    dfs <- dev <- numeric(ns+1)
    names(dfs) <- names(dev) <- c("<none>", scope)
    add.rhs <- paste(scope, collapse = "+")
    add.rhs <- eval(parse(text = paste("~ . +", add.rhs)))
    new.form <- update.formula(object, add.rhs)
    Terms <- terms(new.form)
    y <- object$y
    if(is.null(x)) {
	fc <- object$call
	fc$formula <- Terms
	## model.frame.glm looks at the terms part for the environment
	fob <- list(call = fc, terms = Terms)
	class(fob) <- oldClass(object)
	m <- model.frame(fob, xlev = object$xlevels)
        offset <- model.offset(m)
        wt <- model.weights(m)
	x <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
        oldn <- length(y)
        y <- model.response(m)
        if(!is.factor(y)) storage.mode(y) <- "double"
        ## binomial case has adjusted y and weights
        if(NCOL(y) == 2) {
            n <- y[, 1] + y[, 2]
            y <- ifelse(n == 0, 0, y[, 1]/n)
            if(is.null(wt)) wt <- rep.int(1, length(y))
            wt <- wt * n
        }
        newn <- length(y)
        if(newn < oldn)
            warning(gettextf("using the %d/%d rows from a combined fit",
                             newn, oldn), domain = NA)
    } else {
        ## need to get offset and weights from somewhere
        wt <- object$prior.weights
        offset <- object$offset
    }
    n <- nrow(x)
    if(is.null(wt)) wt <- rep.int(1, n)
    Terms <- attr(Terms, "term.labels")
    asgn <- attr(x, "assign")
    ousex <- match(asgn, match(oTerms, Terms), 0L) > 0L
    if(int) ousex[1L] <- TRUE
    X <- x[, ousex, drop = FALSE]
    z <-  glm.fit(X, y, wt, offset=offset,
                  family=object$family, control=object$control)
    dfs[1L] <- z$rank
    dev[1L] <- z$deviance
    ## workaround for PR#7842. terms.formula may have flipped interactions
    sTerms <- sapply(strsplit(Terms, ":", fixed=TRUE),
                     function(x) paste(sort(x), collapse=":"))
    for(tt in scope) {
        stt <- paste(sort(strsplit(tt, ":")[[1L]]), collapse=":")
	usex <- match(asgn, match(stt, sTerms), 0L) > 0L
	X <- x[, usex|ousex, drop = FALSE]
	z <-  glm.fit(X, y, wt, offset=offset,
		      family=object$family, control=object$control)
	dfs[tt] <- z$rank
	dev[tt] <- z$deviance
    }
    if (scale == 0)
	dispersion <- summary(object, dispersion = NULL)$dispersion
    else dispersion <- scale
    fam <- object$family$family
    if(fam == "gaussian") {
	if(scale > 0) loglik <- dev/scale - n
	else loglik <- n * log(dev/n)
    } else loglik <- dev/dispersion
    aic <- loglik + k * dfs
    aic <- aic + (extractAIC(object, k = k)[2L] - aic[1L])
    dfs <- dfs - dfs[1L]
    dfs[1L] <- NA
    aod <- data.frame(Df = dfs, Deviance = dev, AIC = aic,
		      row.names = names(dfs), check.names = FALSE)
    if(all(is.na(aic))) aod <- aod[, -3]
    test <- match.arg(test)
    if(test == "Chisq") {
        dev <- pmax(0, loglik[1L] - loglik)
        dev[1L] <- NA
        LRT <- if(dispersion == 1) "LRT" else "scaled dev."
        aod[, LRT] <- dev
        nas <- !is.na(dev)
        dev[nas] <- safe_pchisq(dev[nas], aod$Df[nas], lower.tail=FALSE)
        aod[, "Pr(Chi)"] <- dev
    } else if(test == "F") {
        if(fam == "binomial" || fam == "poisson")
            warning(gettextf("F test assumes quasi%s family", fam),
                    domain = NA)
	rdf <- object$df.residual
	aod[, c("F value", "Pr(F)")] <- Fstat(aod, rdf)
    }
    head <- c("Single term additions", "\nModel:",
	      deparse(as.vector(formula(object))),
	      if(scale > 0) paste("\nscale: ", format(scale), "\n"))
    class(aod) <- c("anova", "data.frame")
    attr(aod, "heading") <- head
    aod
}

add1.mlm <- function(object, scope, ...)
    stop("no 'add1' method implemented for \"mlm\" models")

drop1 <- function(object, scope, ...) UseMethod("drop1")

drop1.default <- function(object, scope, scale = 0, test=c("none", "Chisq"),
			  k = 2, trace = FALSE, ...)
{
    tl <- attr(object$terms, "term.labels")
    if(missing(scope)) scope <- drop.scope(object)
    else {
	if(!is.character(scope))
	    scope <- attr(terms(update.formula(object, scope)), "term.labels")
	if(!all(match(scope, tl, 0L) > 0L))
	    stop("scope is not a subset of term labels")
    }
#    data <- model.frame(object) # remove NAs
#    object <- update(object, data = data)
    ns <- length(scope)
    ans <- matrix(nrow = ns + 1L, ncol = 2L,
                  dimnames =  list(c("<none>", scope), c("df", "AIC")))
    ans[1, ] <- extractAIC(object, scale, k = k, ...)
    n0 <- length(object$residuals)
    env <- environment(formula(object))
    for(i in seq(ns)) {
	tt <- scope[i]
	if(trace > 1) {
	    cat("trying -", tt, "\n", sep='')
	    utils::flush.console()
        }
        nfit <- update(object, as.formula(paste("~ . -", tt)),
                       evaluate = FALSE)
	nfit <- eval(nfit, envir=env) # was  eval.parent(nfit)
	ans[i+1, ] <- extractAIC(nfit, scale, k = k, ...)
        if(length(nfit$residuals) != n0)
            stop("number of rows in use has changed: remove missing values?")
    }
    dfs <- ans[1L , 1L] - ans[, 1L]
    dfs[1L] <- NA
    aod <- data.frame(Df = dfs, AIC = ans[,2])
    test <- match.arg(test)
    if(test == "Chisq") {
        dev <- ans[, 2L] - k*ans[, 1L]
        dev <- dev - dev[1L] ; dev[1L] <- NA
        nas <- !is.na(dev)
        P <- dev
        P[nas] <- safe_pchisq(dev[nas], dfs[nas], lower.tail = FALSE)
        aod[, c("LRT", "Pr(Chi)")] <- list(dev, P)
    }
    head <- c("Single term deletions", "\nModel:",
	      deparse(as.vector(formula(object))),
	      if(scale > 0) paste("\nscale: ", format(scale), "\n"))
    class(aod) <- c("anova", "data.frame")
    attr(aod, "heading") <- head
    aod
}

drop1.lm <- function(object, scope, scale = 0, all.cols = TRUE,
		     test=c("none", "Chisq", "F"), k = 2, ...)
{
    check_exact(object)
    x <- model.matrix(object)
    offset <- model.offset(model.frame(object))
    iswt <- !is.null(wt <- object$weights)
    n <- nrow(x)
    asgn <- attr(x, "assign")
    tl <- attr(object$terms, "term.labels")
    if(missing(scope)) scope <- drop.scope(object)
    else {
	if(!is.character(scope))
	    scope <- attr(terms(update.formula(object, scope)), "term.labels")
	if(!all(match(scope, tl, 0L) > 0L))
	    stop("scope is not a subset of term labels")
    }
    ndrop <- match(scope, tl)
    ns <- length(scope)
    rdf <- object$df.residual
    chisq <- deviance.lm(object)
    dfs <- numeric(ns)
    RSS <- numeric(ns)
    y <- object$residuals + object$fitted.values
    ## predict(object) applies na.action where na.exclude results in too long
    na.coef <- seq_along(object$coefficients)[!is.na(object$coefficients)]
    for(i in 1L:ns) {
	ii <- seq_along(asgn)[asgn == ndrop[i]]
	jj <- setdiff(if(all.cols) seq(ncol(x)) else na.coef, ii)
	z <- if(iswt) lm.wfit(x[, jj, drop = FALSE], y, wt, offset=offset)
	else lm.fit(x[, jj, drop = FALSE], y, offset=offset)
	dfs[i] <- z$rank
        oldClass(z) <- "lm" # needed as deviance.lm calls residuals.lm
	RSS[i] <- deviance(z)
    }
    scope <- c("<none>", scope)
    dfs <- c(object$rank, dfs)
    RSS <- c(chisq, RSS)
    if(scale > 0) aic <- RSS/scale - n + k*dfs
    else aic <- n * log(RSS/n) + k*dfs
    dfs <- dfs[1L] - dfs
    dfs[1L] <- NA
    aod <- data.frame(Df = dfs, "Sum of Sq" = c(NA, RSS[-1L] - RSS[1L]),
		      RSS = RSS, AIC = aic,
                      row.names = scope, check.names = FALSE)
    if(scale > 0) names(aod) <- c("Df", "Sum of Sq", "RSS", "Cp")
    test <- match.arg(test)
    if(test == "Chisq") {
        dev <- aod$"Sum of Sq"
        if(scale == 0) {
            dev <- n * log(RSS/n)
            dev <- dev - dev[1L]
            dev[1L] <- NA
        } else dev <- dev/scale
        df <- aod$Df
        nas <- !is.na(df)
        dev[nas] <- safe_pchisq(dev[nas], df[nas], lower.tail=FALSE)
        aod[, "Pr(Chi)"] <- dev
    } else if(test == "F") {
	dev <- aod$"Sum of Sq"
	dfs <- aod$Df
	rdf <- object$df.residual
	rms <- aod$RSS[1L]/rdf
	Fs <- (dev/dfs)/rms
	Fs[dfs < 1e-4] <- NA
	P <- Fs
	nas <- !is.na(Fs)
	P[nas] <- safe_pf(Fs[nas], dfs[nas], rdf, lower.tail=FALSE)
	aod[, c("F value", "Pr(F)")] <- list(Fs, P)
    }
    head <- c("Single term deletions", "\nModel:",
	      deparse(as.vector(formula(object))),
	      if(scale > 0) paste("\nscale: ", format(scale), "\n"))
    class(aod) <- c("anova", "data.frame")
    attr(aod, "heading") <- head
    aod
}

drop1.mlm <- function(object, scope, ...)
    stop("no 'drop1' method for \"mlm\" models")

drop1.glm <- function(object, scope, scale = 0, test=c("none", "Chisq", "F"),
		      k = 2, ...)
{
    x <- model.matrix(object)
#    iswt <- !is.null(wt <- object$weights)
    n <- nrow(x)
    asgn <- attr(x, "assign")
    tl <- attr(object$terms, "term.labels")
    if(missing(scope)) scope <- drop.scope(object)
    else {
	if(!is.character(scope))
	    scope <- attr(terms(update.formula(object, scope)), "term.labels")
	if(!all(match(scope, tl, 0L) > 0L))
	    stop("scope is not a subset of term labels")
    }
    ndrop <- match(scope, tl)
    ns <- length(scope)
    rdf <- object$df.residual
    chisq <- object$deviance
    dfs <- numeric(ns)
    dev <- numeric(ns)
    y <- object$y
    if(is.null(y)) {
        y <- model.response(model.frame(object))
        if(!is.factor(y)) storage.mode(y) <- "double"
    }
#    na.coef <- seq_along(object$coefficients)[!is.na(object$coefficients)]
    wt <- object$prior.weights
    if(is.null(wt)) wt <- rep.int(1, n)
    for(i in 1L:ns) {
	ii <- seq_along(asgn)[asgn == ndrop[i]]
	jj <- setdiff(seq(ncol(x)), ii)
	z <-  glm.fit(x[, jj, drop = FALSE], y, wt, offset=object$offset,
		      family=object$family, control=object$control)
	dfs[i] <- z$rank
	dev[i] <- z$deviance
    }
    scope <- c("<none>", scope)
    dfs <- c(object$rank, dfs)
    dev <- c(chisq, dev)
    dispersion <- if (is.null(scale) || scale == 0)
	summary(object, dispersion = NULL)$dispersion
    else scale
    fam <- object$family$family
    loglik <-
        if(fam == "gaussian") {
            if(scale > 0) dev/scale - n else n * log(dev/n)
        } else dev/dispersion
    aic <- loglik + k * dfs
    dfs <- dfs[1L] - dfs
    dfs[1L] <- NA
    aic <- aic + (extractAIC(object, k = k)[2L] - aic[1L])
    aod <- data.frame(Df = dfs, Deviance = dev, AIC = aic,
		      row.names = scope, check.names = FALSE)
    if(all(is.na(aic))) aod <- aod[, -3]
    test <- match.arg(test)
    if(test == "Chisq") {
        dev <- pmax(0, loglik - loglik[1L])
        dev[1L] <- NA
        nas <- !is.na(dev)
        LRT <- if(dispersion == 1) "LRT" else "scaled dev."
        aod[, LRT] <- dev
        dev[nas] <- safe_pchisq(dev[nas], aod$Df[nas], lower.tail=FALSE)
        aod[, "Pr(Chi)"] <- dev
    } else if(test == "F") {
        if(fam == "binomial" || fam == "poisson")
            warning(gettextf("F test assumes 'quasi%s' family", fam),
                    domain = NA)
	dev <- aod$Deviance
	rms <- dev[1L]/rdf
        dev <- pmax(0, dev - dev[1L])
	dfs <- aod$Df
	rdf <- object$df.residual
	Fs <- (dev/dfs)/rms
	Fs[dfs < 1e-4] <- NA
	P <- Fs
	nas <- !is.na(Fs)
	P[nas] <- safe_pf(Fs[nas], dfs[nas], rdf, lower.tail=FALSE)
	aod[, c("F value", "Pr(F)")] <- list(Fs, P)
    }
    head <- c("Single term deletions", "\nModel:",
	      deparse(as.vector(formula(object))),
	      if(!is.null(scale) && scale > 0)
	      paste("\nscale: ", format(scale), "\n"))
    class(aod) <- c("anova", "data.frame")
    attr(aod, "heading") <- head
    aod
}

add.scope <- function(terms1, terms2)
{
    terms1 <- terms(terms1)
    terms2 <- terms(terms2)
    factor.scope(attr(terms1, "factors"),
		 list(add = attr(terms2, "factors")))$add
}

drop.scope <- function(terms1, terms2)
{
    terms1 <- terms(terms1)
    f2 <- if(missing(terms2)) numeric(0L)
    else attr(terms(terms2), "factors")
    factor.scope(attr(terms1, "factors"), list(drop = f2))$drop
}

factor.scope <- function(factor, scope)
{
    drop <- scope$drop
    add <- scope$add

    if(length(factor) && !is.null(drop)) {# have base model
	nmdrop <- colnames(drop)
	facs <- factor
	if(length(drop)) {
	    nmfac <- colnames(factor)
            ## workaround as in PR#7842.
            ## terms.formula may have flipped interactions
            nmfac0 <- sapply(strsplit(nmfac, ":", fixed=TRUE),
                             function(x) paste(sort(x), collapse=":"))
            nmdrop0 <- sapply(strsplit(nmdrop, ":", fixed=TRUE),
                             function(x) paste(sort(x), collapse=":"))
	    where <- match(nmdrop0, nmfac0, 0L)
	    if(any(!where))
                stop(gettextf("lower scope has term(s) %s not included in model",
                              paste(sQuote(nmdrop[where==0]), collapse=", ")),
                     domain = NA)
	    facs <- factor[, -where, drop = FALSE]
	    nmdrop <- nmfac[-where]
	} else nmdrop <- colnames(factor)
	if(ncol(facs) > 1) {
            ## check no interactions will be left without margins.
	    keep <- rep.int(TRUE, ncol(facs))
	    f <- crossprod(facs > 0)
	    for(i in seq(keep)) keep[i] <- max(f[i, - i]) != f[i, i]
	    nmdrop <- nmdrop[keep]
	}
    } else nmdrop <- character(0L)

    if(!length(add)) nmadd <- character(0L)
    else {
	nmfac <- colnames(factor)
	nmadd <- colnames(add)
	if(!is.null(nmfac)) {
            ## workaround as in PR#7842.
            ## terms.formula may have flipped interactions
            nmfac0 <- sapply(strsplit(nmfac, ":", fixed=TRUE),
                             function(x) paste(sort(x), collapse=":"))
            nmadd0 <- sapply(strsplit(nmadd, ":", fixed=TRUE),
                             function(x) paste(sort(x), collapse=":"))
	    where <- match(nmfac0, nmadd0, 0L)
	    if(any(!where))
                stop(gettextf("upper scope does not include model term(s) %s",
                              paste(sQuote(nmfac[where==0L]), collapse=", ")),
                     domain = NA)
	    nmadd <- nmadd[-where]
	    add <- add[, -where, drop = FALSE]
	}
	if(ncol(add) > 1) {             # check marginality:
	    keep <- rep.int(TRUE, ncol(add))
	    f <- crossprod(add > 0)
	    for(i in seq(keep)) keep[-i] <- keep[-i] & (f[i, -i] < f[i, i])
	    nmadd <- nmadd[keep]
	}
    }
    list(drop = nmdrop, add = nmadd)
}

step <- function(object, scope, scale = 0,
		 direction = c("both", "backward", "forward"),
		 trace = 1, keep = NULL, steps = 1000, k = 2, ...)
{
#     fixFormulaObject <- function(object) {
# 	tt <- terms(object)
# 	tmp <- attr(tt, "term.labels")
# 	if (!attr(tt, "intercept"))
# 	    tmp <- c(tmp, "0")
# 	if (!length(tmp))
# 	    tmp <- "1"
#         tmp <- paste("~", paste(tmp, collapse = " + "))
#         form <- formula(object) # some formulae have no lhs
#         tmp <- if(length(form) > 2) paste(deparse(form[[2L]]), tmp)
#         ## must be as.character as deparse gives spurious ()
# 	if (length(offset <- attr(tt, "offset")))
# 	    tmp <- paste(tmp, as.character(attr(tt, "variables")[offset + 1]),
# 			 sep = " + ")
# 	form <- formula(tmp)
#         environment(form) <- environment(tt)
#         form
#     }
    mydeviance <- function(x, ...)
    {
        dev <- deviance(x)
        if(!is.null(dev)) dev else extractAIC(x, k=0)[2L]
    }

    cut.string <- function(string)
    {
	if(length(string) > 1)
	    string[-1L] <- paste("\n", string[-1L], sep = "")
	string
    }
    re.arrange <- function(keep)
    {
	namr <- names(k1 <- keep[[1L]])
	namc <- names(keep)
	nc <- length(keep)
	nr <- length(k1)
	array(unlist(keep, recursive = FALSE), c(nr, nc), list(namr, namc))
    }

    step.results <- function(models, fit, object, usingCp=FALSE)
    {
	change <- sapply(models, "[[", "change")
	rd <- sapply(models, "[[", "deviance")
        dd <- c(NA, abs(diff(rd)))
	rdf <- sapply(models, "[[", "df.resid")
	ddf <- c(NA, diff(rdf))
	AIC <- sapply(models, "[[", "AIC")
	heading <- c("Stepwise Model Path \nAnalysis of Deviance Table",
		     "\nInitial Model:", deparse(as.vector(formula(object))),
		     "\nFinal Model:", deparse(as.vector(formula(fit))),
		     "\n")
	aod <- data.frame(Step = I(change), Df = ddf, Deviance = dd,
                          "Resid. Df" = rdf, "Resid. Dev" = rd, AIC = AIC,
                          check.names = FALSE)
        if(usingCp) {
            cn <- colnames(aod)
            cn[cn == "AIC"] <- "Cp"
            colnames(aod) <- cn
        }
	attr(aod, "heading") <- heading
        ##stop gap attr(aod, "class") <- c("anova", "data.frame")
	fit$anova <- aod
	fit
    }

    Terms <- terms(object)
    object$call$formula <- object$formula <- Terms
    md <- missing(direction)
    direction <- match.arg(direction)
    backward <- direction == "both" | direction == "backward"
    forward  <- direction == "both" | direction == "forward"
    if(missing(scope)) {
	fdrop <- numeric(0L)
        fadd <- attr(Terms, "factors")
        if(md) forward <- FALSE
    }
    else {
	if(is.list(scope)) {
	    fdrop <- if(!is.null(fdrop <- scope$lower))
		attr(terms(update.formula(object, fdrop)), "factors")
	    else numeric(0L)
	    fadd <- if(!is.null(fadd <- scope$upper))
		attr(terms(update.formula(object, fadd)), "factors")
	}
        else {
	    fadd <- if(!is.null(fadd <- scope))
		attr(terms(update.formula(object, scope)), "factors")
	    fdrop <- numeric(0L)
	}
    }
    models <- vector("list", steps)
    if(!is.null(keep)) keep.list <- vector("list", steps)
    n <- length(object$residuals)
    fit <- object
    bAIC <- extractAIC(fit, scale, k = k, ...)
    edf <- bAIC[1L]
    bAIC <- bAIC[2L]
    if(is.na(bAIC))
        stop("AIC is not defined for this model, so 'step' cannot proceed")
    nm <- 1
    Terms <- fit$terms
    if(trace)
	cat("Start:  AIC=", format(round(bAIC, 2)), "\n",
	    cut.string(deparse(as.vector(formula(fit)))), "\n\n", sep='')

    models[[nm]] <- list(deviance = mydeviance(fit), df.resid = n - edf,
			 change = "", AIC = bAIC)
    if(!is.null(keep)) keep.list[[nm]] <- keep(fit, bAIC)
    usingCp <- FALSE
    while(steps > 0) {
	steps <- steps - 1
	AIC <- bAIC
	ffac <- attr(Terms, "factors")
	scope <- factor.scope(ffac, list(add = fadd, drop = fdrop))
	aod <- NULL
	change <- NULL
	if(backward && length(scope$drop)) {
	    aod <- drop1(fit, scope$drop, scale = scale,
                         trace = trace, k = k, ...)
	    rn <- row.names(aod)
	    row.names(aod) <- c(rn[1L], paste("-", rn[-1L], sep=" "))
            ## drop zero df terms first: one at time since they
            ## may mask each other
	    if(any(aod$Df == 0, na.rm=TRUE)) {
		zdf <- aod$Df == 0 & !is.na(aod$Df)
		change <- rev(rownames(aod)[zdf])[1L]
	    }
	}
	if(is.null(change)) {
	    if(forward && length(scope$add)) {
		aodf <- add1(fit, scope$add, scale = scale,
                             trace = trace, k = k, ...)
		rn <- row.names(aodf)
		row.names(aodf) <- c(rn[1L], paste("+", rn[-1L], sep=" "))
		aod <-
                    if(is.null(aod)) aodf
                    else rbind(aod, aodf[-1, , drop = FALSE])
	    }
	    attr(aod, "heading") <- NULL
	    ## need to remove any terms with zero df from consideration
	    nzdf <- if(!is.null(aod$Df))
		aod$Df != 0 | is.na(aod$Df)
	    aod <- aod[nzdf, ]
	    if(is.null(aod) || ncol(aod) == 0) break
	    nc <- match(c("Cp", "AIC"), names(aod))
	    nc <- nc[!is.na(nc)][1L]
	    o <- order(aod[, nc])
	    if(trace) print(aod[o, ])
	    if(o[1L] == 1) break
	    change <- rownames(aod)[o[1L]]
	}
	usingCp <- match("Cp", names(aod), 0L) > 0L
        ## may need to look for a `data' argument in parent
	fit <- update(fit, paste("~ .", change), evaluate = FALSE)
        fit <- eval.parent(fit)
        if(length(fit$residuals) != n)
            stop("number of rows in use has changed: remove missing values?")
        Terms <- terms(fit)
	bAIC <- extractAIC(fit, scale, k = k, ...)
	edf <- bAIC[1L]
	bAIC <- bAIC[2L]
	if(trace)
	    cat("\nStep:  AIC=", format(round(bAIC, 2)), "\n",
		cut.string(deparse(as.vector(formula(fit)))), "\n\n", sep='')
        ## add a tolerance as dropping 0-df terms might increase AIC slightly
	if(bAIC >= AIC + 1e-7) break
	nm <- nm + 1
	models[[nm]] <-
	    list(deviance = mydeviance(fit), df.resid = n - edf,
		 change = change, AIC = bAIC)
	if(!is.null(keep)) keep.list[[nm]] <- keep(fit, bAIC)
    }
    if(!is.null(keep)) fit$keep <- re.arrange(keep.list[seq(nm)])
    step.results(models = models[seq(nm)], fit, object, usingCp)
}

extractAIC <- function(fit, scale, k = 2, ...) UseMethod("extractAIC")

extractAIC.coxph <- function(fit, scale, k = 2, ...)
{
    ## seems that coxph sometimes gives one and sometimes gives two values
    ## for loglik
    edf <- length(fit$coef)
    loglik <- fit$loglik[length(fit$loglik)]
    c(edf, -2 * loglik + k * edf)
}

extractAIC.survreg <- function(fit, scale, k = 2, ...)
{
    edf <- sum(fit$df)
    c(edf, -2 * fit$loglik[2L] + k * edf)
}

extractAIC.glm <- function(fit, scale = 0, k = 2, ...)
{
    n <- length(fit$residuals)
    edf <- n  - fit$df.residual
    aic <- fit$aic
    c(edf, aic + (k-2) * edf)
}

extractAIC.lm <- function(fit, scale = 0, k = 2, ...)
{
    n <- length(fit$residuals)
    edf <- n  - fit$df.residual
    RSS <- deviance.lm(fit)
    dev <- if(scale > 0) RSS/scale - n else n * log(RSS/n)
    c(edf, dev + k * edf)
}
extractAIC.aov <- extractAIC.lm

extractAIC.negbin <- function(fit, scale, k = 2, ...)
{
    n <- length(fit$residuals)
    edf <- n - fit$df.residual
    c(edf, -fit$twologlik + k * edf)
}
#  File src/library/stats/R/addmargins.R
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

addmargins <-
    function(A, margin = seq_along(dim(A)), FUN = sum, quiet = FALSE)
{
### The workhorse for this margin-expansion is the function
### expand.one, which is defined and called at the bottom.
###
### All this initial stuff is just to check consistency of
### specifications, and form maximally sensible margin names
###
### BxC, August 2003
###	 Sept	2003: Single margins caused crash. Fixed.
### Duncan Murdoch, Feb 2004: Machinery to derive functionnames
###			      from unnamed lists
###-------------------------------------------------------------

    if(is.null(dim(A))) stop("'A' must be an array or table")
    ## How many dimensions of A, and how many sides do we touch?
    n.sid <- length(margin)

    ## Check if FUN was specified
    ##
    miss.FUN <- missing(FUN)

    ## Check if FUN has the same length as margin, and if not, stop or
    ## expand a single function specification to a list of the same
    ## length as the margins vector.
    if (length(FUN) == 1 && !is.list(FUN)) {
	fname <- if (!miss.FUN) deparse(substitute(FUN)) else "Sum"
	FUN <- list(FUN)
	names(FUN) <- fname
    }

    if (!miss.FUN) {
	## Recursive function to add names to unnamed list components
	add.names <- function(thelist)
	{
	    n <- names(thelist)
	    if (is.null(n)) n <- rep("", length(thelist))
	    for (i in seq_along(thelist)[-1L]) {
		if (!is.call(thelist[[i]])) {
		    if (n[i] == "") n[i] <- as.character(thelist[[i]])
		} else if (as.character(thelist[[i]][[1L]]) == "list")
		    thelist[[i]] <- add.names(thelist[[i]])
	    }
	    names(thelist) <- n
	    thelist
	}
	## this only makes sense if we were given an expression for FUN
	## which we can deparse.
	if(mode(substitute(FUN)) == "call")
	    FUN <- eval(add.names(substitute(FUN)))
	if (is.null(names(FUN))) names(FUN) <- rep("", length(FUN))
    }

    ## At this point FUN is a list with names wherever
    ## we could figure them out, empty strings otherwise

    if(length(FUN) != n.sid) {
	if(length(FUN) == 1)
	    FUN <- rep(FUN, n.sid)
	else
	    stop(gettextf("length of FUN, %d,\n does not match the length of the margins, %d",
			  length(FUN), n.sid), domain = NA)
    }

    ## If FUN is not given the default sum is put in the margin
    ## otherwise make a list to fill with names
    ##
    fnames <- vector("list", n.sid)

    ## Use the names from FUN and also possibly the names from
    ## sublists of FUN.	 Replace blanks with constructed names

    for(i in seq_along(FUN)) {
	fnames[[i]] <- names(FUN)[i]
	if (is.list(FUN[[i]])) {
	    topname <- fnames[[i]]
	    fnames[[i]] <- names(FUN[[i]])
	    blank <- fnames[[i]] == ""
	    fnames[[i]][blank] <- seq_along(blank)[blank]
	    if (topname == "") {
		fnames[[i]][blank] <-
		    paste("Margin ", margin[i], ".", fnames[[i]][blank],
			  sep = "")
	    } else {
		fnames[[i]] <- paste(topname, ".", fnames[[i]], sep = "")
	    }
	} else
	    if (fnames[[i]] == "") fnames[[i]] <- paste("Margin", margin[i])
    }

    ## So finally we have the relevant form of FUN and fnames to pass
    ## on to expand.one which expands over one factor at a time.

    expand.one <- function(A, margin, FUN, fnames)
    {
	## Function to expand a table with a set of margins over the
	## side <margin>, i.e. by a set of marginal tables classified by
	## all factors except <margin>.
	##
	## BxC, August 2003

	## Make sure that FUN is a list
	if(class(FUN) != "list") FUN <- list(FUN)

	## Useful constants
	d <- dim(A)
	n.dim <- length(d)   # number of dimensions in the table
	n.mar <- length(FUN) # number of margins to be added

	## Define the dimensions of the new table with the margins
	newdim <- d
	newdim[margin] <- newdim[margin] + n.mar
	newdimnames <- dimnames(A)
	newdimnames[[margin]] <- c(newdimnames[[margin]], fnames)

	## Number of elements in the expanded array
	n.new <- prod(newdim)

	## The positions in the vector-version of the new table
	## where the original table values goes, as a logical vector
	skip <- prod(d[1L:margin])
	runl <- skip / d[margin]
	apos <- rep(c(rep(TRUE, skip), rep(FALSE, n.mar*runl)),
		    n.new/(skip+n.mar*runl))

	## Define a vector to hold all the values of the new table
	values <- numeric(apos)

	## First fill in the body of the table
	values[apos] <- as.vector(A)

	## Then sucessively compute and fill in the required margins
	for(i in 1L:n.mar) {
	    mtab <- if(n.dim > 1) {
		apply(A, (1L:n.dim)[-margin], FUN[[i]])
	    } else FUN[[i]](A)
	    ## Vector the same length as the number of margins
	    select <- rep(FALSE, n.mar)
	    ## The position of the current margin
	    select[i] <- TRUE
	    ## Expand that to a vector the same length as the entire new matrix
	    mpos <- rep(c(rep(FALSE, skip), rep(select, each=runl)),
			prod(dim(A))/skip)
	    ## Fill the marginal table in there
	    values[mpos] <- as.vector(mtab)
	}

	## Then define the new table with contents and margins
	##
	new.A <- array(values, dim=newdim, dimnames=newdimnames)
	if(inherits(A, "table")) # result shall be table, too
	    class(new.A) <- c("table", class(new.A))
	new.A
    }

    ## Once defined, we can use the expand.one function repeatedly
    new.A <- A
    for(i in 1L:n.sid)
	new.A <- expand.one(A = new.A, margin = margin[i], FUN = FUN[[i]],
			    fnames = fnames[[i]])

    ## Done! Now print it.
    ##
    if(!quiet && !miss.FUN  && n.sid > 1) {
	cat("Margins computed over dimensions\nin the following order:\n")
	for(i in 1L:n.sid)
	    cat(paste(i), ": ", names(dimnames(A))[margin[i]], "\n", sep="")
    }
    new.A
}
#  File src/library/stats/R/aggregate.R
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

aggregate <- function(x, ...) UseMethod("aggregate")

aggregate.default <- function(x, ...)
{
    if(is.ts(x))
        aggregate.ts(as.ts(x), ...)
    else
        aggregate.data.frame(as.data.frame(x), ...)
}

aggregate.data.frame <- function(x, by, FUN, ...)
{
    if(!is.data.frame(x)) x <- as.data.frame(x)
    ## do this here to avoid masking by non-function (could happen)
    FUN <- match.fun(FUN)
    if(NROW(x) == 0) stop("no rows to aggregate")
    if(NCOL(x) == 0) {
        ## fake it
        x <- data.frame(x=rep(1, NROW(x)))
        return(aggregate.data.frame(x, by, function(x) 0L)[seq_along(by)])
    }
    if(!is.list(by))
        stop("'by' must be a list")
    if(is.null(names(by)))
        names(by) <- paste("Group", seq_along(by), sep = ".")
    else {
        nam <- names(by)
        ind <- which(!nzchar(nam))
        names(by)[ind] <- paste("Group", ind, sep = ".")
    }
    y <- lapply(x, tapply, by, FUN, ..., simplify = FALSE)
    ## not sapply as might be of length zero, and results will be of
    ## length zero for an empty group.
    lens <-  unlist(lapply(unlist(y, recursive = FALSE), length))
    if(any(lens > 1L)) stop("'FUN' must always return a scalar")
    z <- y[[1L]]
    d <- dim(z)
    w <- vector("list", length(d))
    for (i in seq_along(d)) {
        j <- rep.int(rep.int(seq_len(d[i]),
                             prod(d[seq_len(i - 1L)]) * rep.int(1L, d[i])),
                     prod(d[seq.int(from = i + 1L, length.out = length(d) - i)]))
        zz <- dimnames(z)[[i]][j]
        ## zz is character, so match to the levels created in tapply
        ## and not to as.character(by[[i]])
        w[[i]] <- by[[i]][match(zz, as.factor(by[[i]]))]
    }
    ## this gives w row names that may not be consecutive.
    w <- as.data.frame(w, stringsAsFactors = FALSE)[which(!unlist(lapply(z, is.null))), , drop = FALSE]
    y <- data.frame(w, lapply(y, unlist, use.names = FALSE),
                    stringsAsFactors = FALSE)
    names(y) <- c(names(by), names(x))
    row.names(y) <- NULL
    y
}

aggregate.ts <- function(x, nfrequency = 1, FUN = sum, ndeltat = 1,
                         ts.eps = getOption("ts.eps"), ...)
{
    x <- as.ts(x)
    ofrequency <- tsp(x)[3L]
    ## do this here to avoid masking by non-function (could happen)
    FUN <- match.fun(FUN)
    ## Set up the new frequency, and make sure it is an integer.
    if(missing(nfrequency))
        nfrequency <- 1 / ndeltat
    if((nfrequency > 1) &&
        (abs(nfrequency - round(nfrequency)) < ts.eps))
        nfrequency <- round(nfrequency)

    if(nfrequency == ofrequency)
        return(x)
    ratio <- ofrequency /nfrequency
    if(abs(ratio - round(ratio)) > ts.eps)
        stop(gettextf("cannot change frequency from %g to %g",
                      ofrequency, nfrequency), domain = NA)
    ## The desired result is obtained by applying FUN to blocks of
    ## length ofrequency/nfrequency, for each of the variables in x.
    ## We first get the new start and end right, and then break x into
    ## such blocks by reshaping it into an array and setting dim.
    len <- ofrequency %/% nfrequency
    mat <- is.matrix(x)
    if(mat) cn <- colnames(x)
#    nstart <- ceiling(tsp(x)[1L] * nfrequency) / nfrequency
#    x <- as.matrix(window(x, start = nstart))
    nstart <- tsp(x)[1L]
    # Can't use nstart <- start(x) as this causes problems if
    # you get a vector of length 2.
    x <- as.matrix(x)
    nend <- floor(nrow(x) / len) * len
    x <- apply(array(c(x[1 : nend, ]),
                     dim = c(len, nend / len, ncol(x))),
               MARGIN = c(2L, 3L), FUN = FUN, ...)
    if(!mat) x <- as.vector(x)
    else colnames(x) <- cn
    ts(x, start = nstart, frequency = nfrequency)
}
#  File src/library/stats/R/anova.R
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

## utility for anova.FOO(), FOO in "lmlist", "glm", "glmlist"
## depending on the ordering of the models this might get called with
## negative deviance and df changes.
stat.anova <- function(table, test=c("Chisq", "F", "Cp"), scale, df.scale, n)
{
    test <- match.arg(test)
    dev.col <- match("Deviance", colnames(table))
    if(is.na(dev.col)) dev.col <- match("Sum of Sq", colnames(table))
    switch(test,
	   "Chisq" = {
               dfs <- table[, "Df"]
               vals <- table[, dev.col]/scale * sign(dfs)
	       vals[dfs %in% 0] <- NA
               vals[!is.na(vals) & vals < 0] <- NA # rather than p = 0
	       cbind(table,
                     "P(>|Chi|)" = pchisq(vals, abs(dfs), lower.tail=FALSE)
                     )
	   },
	   "F" = {
               dfs <- table[, "Df"]
	       Fvalue <- (table[, dev.col]/dfs)/scale
	       Fvalue[dfs %in% 0] <- NA
               Fvalue[!is.na(Fvalue) & Fvalue < 0] <- NA # rather than p = 0
	       cbind(table,
                     F = Fvalue,
		     "Pr(>F)" = pf(Fvalue, abs(dfs), df.scale, lower.tail=FALSE)
                     )
	   },
	   "Cp" = {
	       cbind(table, Cp = table[,"Resid. Dev"] +
		     2*scale*(n - table[,"Resid. Df"]))
	   })
}

printCoefmat <-
    function(x, digits = max(3, getOption("digits") - 2),
	     signif.stars = getOption("show.signif.stars"),
             signif.legend = signif.stars,
	     dig.tst = max(1, min(5, digits - 1)),
	     cs.ind = 1:k, tst.ind = k+1, zap.ind = integer(0),
	     P.values = NULL,
	     has.Pvalue = nc >= 4 && substr(colnames(x)[nc], 1, 3) == "Pr(",
             eps.Pvalue = .Machine$double.eps,
	     na.print = "NA", ...)
{
    ## For printing ``coefficient matrices'' as they are in summary.xxx(.) where
    ## xxx in {lm, glm, aov, ..}. (Note: summary.aov(.) gives a class "anova").

    ## By Default
    ## Assume: x is a matrix-like numeric object.
    ## ------  with *last* column = P-values  --iff-- P.values (== TRUE)
    ##	  columns {cs.ind}= numbers, such as coefficients & std.err  [def.: 1L:k]
    ##	  columns {tst.ind}= test-statistics (as "z", "t", or "F")  [def.: k+1]

    if(is.null(d <- dim(x)) || length(d) != 2)
	stop("'x' must be coefficient matrix/data frame")
    nc <- d[2L]
    if(is.null(P.values)) {
        scp <- getOption("show.coef.Pvalues")
        if(!is.logical(scp) || is.na(scp)) {
            warning("option \"show.coef.Pvalues\" is invalid: assuming TRUE")
            scp <- TRUE
        }
	P.values <- has.Pvalue && scp
    }
    else if(P.values && !has.Pvalue)
	stop("'P.values' is TRUE, but 'has.Pvalue' is not")

    if(has.Pvalue && !P.values) {# P values are there, but not wanted
	d <- dim(xm <- data.matrix(x[,-nc , drop = FALSE]))
	nc <- nc - 1
	has.Pvalue <- FALSE
    } else xm <- data.matrix(x)

    k <- nc - has.Pvalue - (if(missing(tst.ind)) 1 else length(tst.ind))
    if(!missing(cs.ind) && length(cs.ind) > k) stop("wrong k / cs.ind")

    Cf <- array("", dim=d, dimnames = dimnames(xm))

    ok <- !(ina <- is.na(xm))
    ## zap before deciding any formats
    for (i in zap.ind) xm[, i] <- zapsmall(xm[, i], digits)
    if(length(cs.ind)) {
	acs <- abs(coef.se <- xm[, cs.ind, drop=FALSE])# = abs(coef. , stderr)
	if(any(ia <- is.finite(acs))) {
	    ## #{digits} BEFORE decimal point -- for min/max. value:
	    digmin <- 1 + if(length(acs <- acs[ia & acs != 0]))
		floor(log10(range(acs[acs != 0], finite = TRUE))) else 0
            Cf[,cs.ind] <- format(round(coef.se, max(1,digits-digmin)),
                                  digits=digits)
        }
    }
    if(length(tst.ind))
	Cf[, tst.ind]<- format(round(xm[, tst.ind], digits = dig.tst),
                               digits = digits)
    if(any(r.ind <- !((1L:nc) %in%
                      c(cs.ind, tst.ind, if(has.Pvalue) nc))))
	for(i in which(r.ind)) Cf[, i] <- format(xm[, i], digits=digits)
    okP <- if(has.Pvalue) ok[, -nc] else ok
    ## we need to find out where Cf is zero.  We can't use as.numeric
    ## directly as OutDec could have been set.
    ## x0 <- (xm[okP]==0) != (as.numeric(Cf[okP])==0)
    x1 <- Cf[okP]
    dec <- getOption("OutDec")
    if(dec != ".") x1 <- chartr(dec, ".", x1)
    x0 <- (xm[okP] == 0) != (as.numeric(x1) == 0)
    if(length(not.both.0 <- which(x0 & !is.na(x0)))) {
	## not.both.0==TRUE:  xm !=0, but Cf[] is: --> fix these:
	Cf[okP][not.both.0] <- format(xm[okP][not.both.0],
                                      digits = max(1, digits-1))
    }
    if(any(ina)) Cf[ina] <- na.print
    if(P.values) {
        if(!is.logical(signif.stars) || is.na(signif.stars)) {
            warning("option \"show.signif.stars\" is invalid: assuming TRUE")
            signif.stars <- TRUE
        }
	if(any(okP <- ok[,nc])) {
	pv <- as.vector(xm[, nc]) # drop names
	    Cf[okP, nc] <- format.pval(pv[okP],
                                       digits = dig.tst, eps = eps.Pvalue)
	    signif.stars <- signif.stars && any(pv[okP] < .1)
	    if(signif.stars) {
		Signif <- symnum(pv, corr = FALSE, na = FALSE,
				 cutpoints = c(0,  .001,.01,.05, .1, 1),
				 symbols   =  c("***","**","*","."," "))
		Cf <- cbind(Cf, format(Signif)) #format.ch: right=TRUE
	    }
	} else signif.stars <- FALSE
    } else signif.stars <- FALSE
    print.default(Cf, quote = FALSE, right = TRUE, na.print=na.print, ...)
    if(signif.stars && signif.legend)
        cat("---\nSignif. codes: ",attr(Signif,"legend"),"\n")
    invisible(x)
}

print.anova <- function(x, digits = max(getOption("digits") - 2, 3),
                        signif.stars = getOption("show.signif.stars"), ...)
{
    if (!is.null(heading <- attr(x, "heading")))
	cat(heading, sep = "\n")
    nc <- dim(x)[2L]
    if(is.null(cn <- colnames(x))) stop("'anova' object must have colnames")
    has.P <- grepl("^(P|Pr)\\(", cn[nc]) # P-value as last column
    zap.i <- 1L:(if(has.P) nc-1 else nc)
    i <- which(substr(cn,2,7) == " value")
    i <- c(i, which(!is.na(match(cn, c("F", "Cp", "Chisq")))))
    if(length(i))
	zap.i <- zap.i[!(zap.i %in% i)]
    tst.i <- i
    if(length(i <- grep("Df$", cn)))
	zap.i <- zap.i[!(zap.i %in% i)]

    printCoefmat(x, digits = digits, signif.stars = signif.stars,
                 has.Pvalue = has.P, P.values = has.P,
                 cs.ind = NULL, zap.ind = zap.i, tst.ind= tst.i,
                 na.print = "", # not yet in print.matrix:  print.gap = 2,
                 ...)
    invisible(x)
}

#  File src/library/stats/R/ansari.test.R
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

ansari.test <- function(x, ...) UseMethod("ansari.test")

ansari.test.default <-
function(x, y, alternative = c("two.sided", "less", "greater"),
         exact = NULL, conf.int = FALSE, conf.level = 0.95, ...)
{
    alternative <- match.arg(alternative)
    if(conf.int) {
        if(!((length(conf.level) == 1L)
             && is.finite(conf.level)
             && (conf.level > 0)
             && (conf.level < 1)))
            stop("'conf.level' must be a single number between 0 and 1")
    }
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))

    x <- x[complete.cases(x)]
    y <- y[complete.cases(y)]
    m <- length(x)
    if(m < 1)
        stop("not enough 'x' observations")
    n <- length(y)
    if(n < 1)
        stop("not enough 'y' observations")
    N <- m + n

    r <- rank(c(x, y))
    STATISTIC <- sum(pmin(r, N - r + 1)[seq_along(x)])
    TIES <- (length(r) != length(unique(r)))

    if(is.null(exact))
        exact <- ((m < 50) && (n < 50))

    if(exact && !TIES) {
        pansari <- function(q, m, n) {
            .C(R_pansari,
               as.integer(length(q)),
               p = as.double(q),
               as.integer(m),
               as.integer(n))$p
        }
        PVAL <-
            switch(alternative,
                   two.sided = {
                       if (STATISTIC > ((m + 1)^2 %/% 4
                                        + ((m * n) %/% 2) / 2))
                           p <- 1 - pansari(STATISTIC - 1, m, n)
                       else
                           p <- pansari(STATISTIC, m, n)
                       min(2 * p, 1)
                   },
                   less = 1 - pansari(STATISTIC - 1, m, n),
                   greater = pansari(STATISTIC, m, n))
        if (conf.int) {
            qansari <- function(p, m, n) {
                .C(R_qansari,
                   as.integer(length(p)),
                   q = as.double(p),
                   as.integer(m),
                   as.integer(n))$q
            }
            alpha <- 1 - conf.level
            x <- sort(x)
            y <- sort(y)
            ab <- function(sig) {
                rab <- rank(c(x/sig, y))
                sum(pmin(rab, N - rab + 1)[seq_along(x)])
            }
            ratio <- outer(x,y,"/")
            aratio <- ratio[ratio >= 0]
            sigma <- sort(aratio)

            cci <- function(alpha) {
              u <- absigma - qansari(alpha/2,  m, n)
              l <- absigma - qansari(1 - alpha/2, m, n)
              ## Check if the statistic exceeds both quantiles first.
              uci <- NULL
              lci <- NULL
              if(length(u[u >= 0]) == 0L || length(l[l > 0]) == 0L) {
                  warning("samples differ in location: cannot compute confidence set, returning NA")
                  return(c(NA, NA))
              }
              if (is.null(uci)) {
                  u[u < 0] <- NA
                  uci <- min(sigma[which(u == min(u, na.rm=TRUE))])
              }
              if (is.null(lci)) {
                  l[l <= 0] <- NA
                  lci <- max(sigma[which(l == min(l, na.rm=TRUE))])
              }
              ## The process of the statistics does not need to be
              ## monotone in sigma: check this and interchange quantiles.
              if (uci > lci) {
                  l <- absigma - qansari(alpha/2,  m, n)
                  u <- absigma - qansari(1 - alpha/2, m, n)
                  u[u < 0] <- NA
                  uci <- min(sigma[which(u == min(u, na.rm=TRUE))])
                  l[l <= 0] <- NA
                  lci <- max(sigma[which(l == min(l, na.rm=TRUE))])
               }
               c(uci, lci)
            }

            cint <- if(length(sigma) < 1L) {
                warning("cannot compute confidence set, returning NA")
                c(NA, NA)
            }
            else {
                ## Compute statistics directly: computing the steps is
                ## not faster.
                absigma <-
                    sapply(sigma + c(diff(sigma)/2,
                                     sigma[length(sigma)]*1.01), ab)
                switch(alternative, two.sided = {
                    cci(alpha)
                }, greater= {
                    c(cci(alpha*2)[1L], Inf)
                }, less= {
                    c(0, cci(alpha*2)[2L])
                })
            }
            attr(cint, "conf.level") <- conf.level
            u <- absigma - qansari(0.5, m, n)
            sgr <- sigma[u <= 0]
            if (length(sgr) == 0L) sgr <- NA
            else sgr <- max(sgr)
            sle <- sigma[u > 0]
            if (length(sle) == 0L) sle <- NA
            else sle <- min(sle)
            ESTIMATE <- mean(c(sle, sgr))
        }
    }
    else {
        EVEN <- ((N %% 2) == 0)
        normalize <- function(s, r, TIES, m=length(x), n=length(y)) {
            z <- if(EVEN)
                s - m * (N + 2)/4
            else
                s - m * (N + 1)^2/(4 * N)
            if (!TIES) {
                SIGMA <- if(EVEN)
                    sqrt((m * n * (N + 2) * (N - 2))/(48 * (N - 1)))
                else
                    sqrt((m * n * (N + 1) * (3 + N^2))/(48 * N^2))
            }
            else {
                r <- rle(sort(pmin(r, N - r + 1)))
                SIGMA <- if(EVEN)
                    sqrt(m * n
                         * (16 * sum(r$lengths * r$values^2) - N * (N + 2)^2)
                         / (16 * N * (N - 1)))
                else
                    sqrt(m * n
                         * (16 * N * sum(r$lengths * r$values^2) - (N + 1)^4)
                         / (16 * N^2 * (N - 1)))
            }
            z / SIGMA
        }
        p <- pnorm(normalize(STATISTIC, r, TIES))
        PVAL <- switch(alternative,
                       two.sided = 2 * min(p, 1 - p),
                       less = 1 - p,
                       greater = p)

        if(conf.int && !exact) {
            alpha <- 1 - conf.level
            ab2 <- function(sig, zq) {
                r <- rank(c(x / sig, y))
                s <- sum(pmin(r, N -r + 1)[seq_along(x)])
                TIES <- (length(r) != length(unique(r)))
                normalize(s, r, TIES, length(x), length(y)) - zq
            }
            ## Use uniroot here.
            ## Compute the range of sigma first.
            srangepos <- NULL
            srangeneg <- NULL
            if (length(x[x > 0]) && length(y[y > 0]))
                srangepos <-
                    c(min(x[x>0], na.rm=TRUE)/max(y[y>0], na.rm=TRUE),
                      max(x[x>0], na.rm=TRUE)/min(y[y>0], na.rm=TRUE))
            if (length(x[x <= 0]) && length(y[y < 0]))
                srangeneg <-
                    c(min(x[x<=0], na.rm=TRUE)/max(y[y<0], na.rm=TRUE),
                      max(x[x<=0], na.rm=TRUE)/min(y[y<0], na.rm=TRUE))
            if (any(is.infinite(c(srangepos, srangeneg)))) {
                warning("cannot compute asymptotic confidence set or estimator")
                conf.int <- FALSE
            } else {
                ccia <- function(alpha) {
                    ## Check if the statistic exceeds both quantiles
                    ## first.
                    statu <- ab2(srange[1L], zq=qnorm(alpha/2))
                    statl <- ab2(srange[2L], zq=qnorm(alpha/2, lower.tail=FALSE))
                    if (statu > 0 || statl < 0) {
                        warning("samples differ in location: cannot compute confidence set, returning NA")
                        return(c(NA, NA))
                    }
                    u <- uniroot(ab2, srange, tol=1e-4,
                                 zq=qnorm(alpha/2))$root
                    l <- uniroot(ab2, srange, tol=1e-4,
                                 zq=qnorm(alpha/2, lower.tail=FALSE))$root
                    ## The process of the statistics does not need to be
                    ## monotone: sort is ok here.
                    sort(c(u, l))
                }
                srange <- range(c(srangepos, srangeneg), na.rm=FALSE)
                cint <- switch(alternative, two.sided = {
                    ccia(alpha)
                }, greater= {
                    c(ccia(alpha*2)[1L], Inf)
                }, less= {
                    c(0, ccia(alpha*2)[2L])
                })
                attr(cint, "conf.level") <- conf.level
                ## Check if the statistic exceeds both quantiles first.
                statu <- ab2(srange[1L], zq=0)
                statl <- ab2(srange[2L], zq=0)
                if (statu > 0 || statl < 0) {
                    ESTIMATE <- NA
                    warning("cannot compute estimate, returning NA")
                } else
                    ESTIMATE <- uniroot(ab2, srange, tol=1e-4, zq=0)$root
            }
        }
        if(exact && TIES) {
            warning("cannot compute exact p-value with ties")
            if(conf.int)
                warning("cannot compute exact confidence intervals with ties")
        }
    }

    names(STATISTIC) <- "AB"
    RVAL <- list(statistic = STATISTIC,
                 p.value = PVAL,
                 null.value = c("ratio of scales" = 1),
                 alternative = alternative,
                 method = "Ansari-Bradley test",
                 data.name = DNAME)
    if(conf.int)
        RVAL <- c(RVAL,
                  list(conf.int = cint,
                       estimate = c("ratio of scales" = ESTIMATE)))
    class(RVAL) <- "htest"
    return(RVAL)
}

ansari.test.formula <-
function(formula, data, subset, na.action, ...)
{
    if(missing(formula)
       || (length(formula) != 3L)
       || (length(attr(terms(formula[-2L]), "term.labels")) != 1L))
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m[[1L]] <- as.name("model.frame")
    m$... <- NULL
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " by ")
    names(mf) <- NULL
    response <- attr(attr(mf, "terms"), "response")
    g <- factor(mf[[-response]])
    if(nlevels(g) != 2)
        stop("grouping factor must have exactly 2 levels")
    DATA <- split(mf[[response]], g)
    names(DATA) <- c("x", "y")
    y <- do.call("ansari.test", c(DATA, list(...)))
    y$data.name <- DNAME
    y
}
#  File src/library/stats/R/aov.R
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

aov <- function(formula, data = NULL, projections = FALSE, qr = TRUE,
                contrasts = NULL, ...)
{
    Terms <- if(missing(data)) terms(formula, "Error")
    else terms(formula, "Error", data = data)
    indError <- attr(Terms, "specials")$Error
    if(length(indError) > 1L)
        stop(sprintf(ngettext(length(indError),
                              "there are %d Error terms: only 1 is allowed",
                              "there are %d Error terms: only 1 is allowed"),
                     length(indError)), domain = NA)
    lmcall <- Call <- match.call()
    lmcall[[1L]] <- as.name("lm")
    lmcall$singular.ok <- TRUE
    if(projections) qr <- lmcall$qr <- TRUE
    lmcall$projections <- NULL
    if(is.null(indError)) {
        ## no Error term
        fit <- eval(lmcall, parent.frame())
        if(projections) fit$projections <- proj(fit)
        class(fit) <- if(inherits(fit, "mlm"))
            c("maov", "aov", oldClass(fit)) else c("aov", oldClass(fit))
        fit$call <- Call
        return(fit)
    } else {
        if(pmatch("weights", names(match.call()), 0L))
            stop("weights are not supported in a multistratum aov() fit")
        ##  Helmert contrasts can be helpful: do we want to force them?
        ##  this version does for the Error model.
        opcons <- options("contrasts")
        options(contrasts = c("contr.helmert", "contr.poly"))
        on.exit(options(opcons))
        allTerms <- Terms
        errorterm <-  attr(Terms, "variables")[[1 + indError]]
        eTerm <- deparse(errorterm[[2L]], width.cutoff = 500L, backtick = TRUE)
        intercept <- attr(Terms, "intercept")
        ecall <- lmcall
        ecall$formula <-
            as.formula(paste(deparse(formula[[2L]], width.cutoff = 500L,
                                     backtick = TRUE), "~", eTerm,
                             if(!intercept) "- 1"),
                       env=environment(formula))

        ecall$method <- "qr"
        ecall$qr <- TRUE
        ecall$contrasts <- NULL
        er.fit <- eval(ecall, parent.frame())
        options(opcons)
        nmstrata <- attr(terms(er.fit), "term.labels")
        ## remove backticks from simple labels for strata (only)
        nmstrata <- sub("^`(.*)`$", "\\1", nmstrata)
        nmstrata <- c("(Intercept)", nmstrata)
        qr.e <- er.fit$qr
        rank.e <- er.fit$rank
        if(rank.e < length(er.fit$coefficients))
            warning("Error() model is singular")
        qty <- er.fit$residuals
        maov <- is.matrix(qty)
        asgn.e <- er.fit$assign[qr.e$pivot[1L:rank.e]]
        ## we want this to label the rows of qtx, not cols of x.
        maxasgn <- length(nmstrata)-1L
        nobs <- NROW(qty)
        if(nobs > rank.e) {
            result <- vector("list", maxasgn + 2L)
            asgn.e[(rank.e+1):nobs] <- maxasgn + 1L
            nmstrata <- c(nmstrata, "Within")
        } else result <- vector("list", maxasgn + 1L)
        names(result) <- nmstrata
        lmcall$formula <- form <-
            update(formula, paste(". ~ .-", deparse(errorterm, width.cutoff = 500L, backtick = TRUE)))
        Terms <- terms(form)
        lmcall$method <- "model.frame"
        mf <- eval(lmcall, parent.frame())
        xvars <- as.character(attr(Terms, "variables"))[-1L]
        if ((yvar <- attr(Terms, "response")) > 0L)
            xvars <- xvars[-yvar]
        if (length(xvars)) {
            xlev <- lapply(mf[xvars], levels)
            xlev <- xlev[!sapply(xlev, is.null)]
        } else xlev <- NULL
        resp <- model.response(mf)
        qtx <- model.matrix(Terms, mf, contrasts)
        cons <- attr(qtx, "contrasts")
        dnx <- colnames(qtx)
        asgn.t <- attr(qtx, "assign")
        if(length(wts <- model.weights(mf))) {
            wts <- sqrt(wts)
            resp <- resp * wts
            qtx <- qtx * wts
        }
        qty <- as.matrix(qr.qty(qr.e, resp))
        if((nc <- ncol(qty)) > 1) {
            dny <- colnames(resp)
            if(is.null(dny)) dny <- paste("Y", 1L:nc, sep="")
            dimnames(qty) <- list(seq(nrow(qty)), dny)
        } else dimnames(qty) <- list(seq(nrow(qty)), NULL)
        qtx <- qr.qty(qr.e, qtx)
        dimnames(qtx) <- list(seq(nrow(qtx)) , dnx)
        for(i in seq_along(nmstrata)) {
            select <- asgn.e==(i-1)
            ni <- sum(select)
            if(!ni) next
            ## helpful to drop constant columns.
            xi <- qtx[select, , drop = FALSE]
            cols <- colSums(xi^2) > 1e-5
            if(any(cols)) {
                xi <- xi[, cols, drop = FALSE]
                attr(xi, "assign") <- asgn.t[cols]
                fiti <- lm.fit(xi, qty[select,,drop=FALSE])
                fiti$terms <- Terms
            } else {
                y <- qty[select,,drop=FALSE]
                fiti <- list(coefficients = numeric(0L), residuals = y,
                             fitted.values = 0 * y, weights = wts, rank = 0L,
                             df.residual = NROW(y))
            }
            if(projections) fiti$projections <- proj(fiti)
            class(fiti) <- c(if(maov) "maov", "aov", oldClass(er.fit))
            result[[i]] <- fiti
        }
        ## drop empty strata
        result <- result[!sapply(result, is.null)]
        class(result) <- c("aovlist", "listof")
        if(qr) attr(result, "error.qr") <- qr.e
        attr(result, "call") <- Call
        if(length(wts)) attr(result, "weights") <- wts
        attr(result, "terms") <- allTerms
        attr(result, "contrasts") <- cons
        attr(result, "xlevels") <- xlev
        result
    }
}

print.aov <-
function(x, intercept = FALSE, tol = .Machine$double.eps^0.5, ...)
{
    if(!is.null(cl <- x$call)) {
        cat("Call:\n   ")
        dput(cl, control=NULL)
    }
    asgn <- x$assign[x$qr$pivot[1L:x$rank]]
    effects <- x$effects
    if(!is.null(effects))
        effects <- as.matrix(effects)[seq_along(asgn),,drop=FALSE]
    rdf <- x$df.residual
    resid <- as.matrix(x$residuals)
    wt <- x$weights
    if(!is.null(wt)) resid <- resid * wt^0.5
    RSS <- colSums(resid^2)
    uasgn <- unique(asgn)
    nmeffect <- c("(Intercept)", attr(x$terms, "term.labels"))[1+uasgn]
    nterms <- length(uasgn)
    nresp <- NCOL(effects)
    df <- numeric(nterms)
    ss <- matrix(NA, nterms, nresp)
    if(nterms) {
        for(i in seq(nterms)) {
            ai <- asgn==uasgn[i]
            df[i] <- sum(ai)
            ef <- effects[ai,, drop=FALSE]
            ss[i,] <- if(sum(ai) > 1) colSums(ef^2) else ef^2
        }
        keep <- df > 0
        if(!intercept && uasgn[1L] == 0) keep[1L] <- FALSE
        nmeffect <- nmeffect[keep]
        df <- df[keep]
        ss <- ss[keep,,drop=FALSE]
        nterms <- length(df)
    }
    cat("\nTerms:\n")
    if(nterms == 0) {
        ## empty model
        if(rdf > 0) {
            ss <- RSS
            ssp <- sapply(ss, format)
            if(!is.matrix(ssp)) ssp <- t(ssp)
            tmp <- as.matrix(c(ssp, format(rdf)))
            if(length(ss) > 1L) {
                rn <- colnames(x$fitted.values)
                if(is.null(rn)) rn <- paste("resp", seq_along(ss))
            } else rn <- "Sum of Squares"
            dimnames(tmp) <- list(c(rn, "Deg. of Freedom"), "Residuals")
            print(tmp, quote = FALSE, right = TRUE)
            cat("\n")
            cat("Residual standard error:", sapply(sqrt(ss/rdf), format), "\n")
        } else
        print(matrix(0, 2L, 1L, dimnames=
                     list(c("Sum of Squares", "Deg. of Freedom"), "<empty>")))
    } else {
        if(rdf > 0L) {
            nterms <- nterms + 1L
            df <- c(df, rdf)
            ss <- rbind(ss, RSS)
            nmeffect <- c(nmeffect, "Residuals")
        }
        ssp <- apply(zapsmall(ss), 2L, format)
        tmp <- t(cbind(ssp, format(df)))
        if(ncol(effects) > 1L) {
            rn <- colnames(x$coeffficients)
            if(is.null(rn)) rn <- paste("resp", seq(ncol(effects)))
        } else rn <- "Sum of Squares"
        dimnames(tmp) <- list(c(rn, "Deg. of Freedom"), nmeffect)
        print(tmp, quote = FALSE, right = TRUE)
        rank <- x$rank
#        int <- attr(x$terms, "intercept")
#        nobs <- NROW(x$residuals) - !(is.null(int) || int == 0)
        cat("\n")
        if(rdf > 0) {
            rs <- sqrt(RSS/rdf)
            cat("Residual standard error:", sapply(rs, format), "\n")
        }
        coef <- as.matrix(x$coefficients)[,1]
        R <- x$qr$qr
        R <- R[1L:min(dim(R)), ,drop=FALSE]
        R[lower.tri(R)] <- 0
        if(rank < (nc <- length(coef))) {
            cat(paste(nc - rank, "out of", nc, "effects not estimable\n"))
            R <- R[, 1L:rank, drop = FALSE]
        }
        d2 <- sum(abs(diag(R)))
        diag(R) <- 0
        if(sum(abs(R))/d2 > tol)
            cat("Estimated effects may be unbalanced\n")
        else cat("Estimated effects are balanced\n")
        if(nzchar(mess <- naprint(x$na.action))) cat(mess, "\n", sep="")
    }
    invisible(x)
}

summary.aov <- function(object, intercept = FALSE, split,
                        expand.split = TRUE, keep.zero.df = TRUE, ...)
{
    splitInteractions <- function(split, factors, names, asgn, df.names)
    {
        ns <- names(split)
        for(i in unique(asgn)) {
            if(i == 0 || names[i+1] %in% ns) next
            f <- rownames(factors)[factors[, i] > 0]
            sp <- f %in% ns
            if(any(sp)) {              # some marginal terms are split
                if(sum(sp) > 1) {
                    old <- split[ f[sp] ]
                    nn <- f[sp]
                    names(nn) <- nn
                    marg <- lapply(nn, function(x)
                                   df.names[asgn == (match(x, names) - 1L)])
                    term.coefs <- strsplit(df.names[asgn == i], ":", fixed=TRUE)
                    ttc <- sapply(term.coefs, function(x) x[sp])
                    rownames(ttc) <- nn
                    splitnames <- apply(expand.grid(lapply(old, names)), 1L,
                                        function(x) paste(x, collapse="."))
                    names(splitnames) <- splitnames
                    tmp <- sapply(nn, function(i)
                                  names(old[[i]])[match(ttc[i, ], marg[[i]])] )
                    tmp <- apply(tmp, 1L, function(x) paste(x, collapse="."))
                    new <- lapply(splitnames, function(x) match(x, tmp))
                    split[[ names[i+1] ]] <-
                        new[sapply(new, function(x) length(x) > 0L)]
                } else {
                    old <- split[[ f[sp] ]]
                    marg.coefs <- df.names[asgn == (match(f[sp], names) - 1L)]
                    term.coefs <- strsplit(df.names[asgn == i], ":", fixed=TRUE)
                    ttc <- sapply(term.coefs, function(x) x[sp])
                    new <- lapply(old, function(x)
                                  seq_along(ttc)[ttc %in% marg.coefs[x]])
                    split[[ names[i+1] ]] <- new
                }
            }
        }
        split
    }

    asgn <- object$assign[object$qr$pivot[1L:object$rank]]
    uasgn <- unique(asgn)
    nterms <- length(uasgn)
    effects <- object$effects
    if(!is.null(effects))
        effects <- as.matrix(effects)[seq_along(asgn),,drop=FALSE]
    rdf <- object$df.residual
    nmeffect <- c("(Intercept)", attr(object$terms, "term.labels"))
    coef <- as.matrix(object$coefficients)
    resid <- as.matrix(object$residuals)
    wt <- object$weights
    if(!is.null(wt)) resid <- resid * wt^0.5
    nresp <- NCOL(resid)
    ans <- vector("list", nresp)
    if(nresp > 1) {
        names(ans) <- character(nresp)
        for (y in 1L:nresp) {
            cn <- colnames(resid)[y]
            if(is.null(cn) || cn == "") cn <- y
            names(ans)[y] <- paste(" Response", cn)
        }
    }

    if(!is.null(effects) && !missing(split)) {
        ns <- names(split)
        if(!is.null(Terms <- object$terms)) {
            if(!is.list(split))
                stop("the 'split' argument must be a list")
            if(!all(ns %in% nmeffect))
                stop("unknown name(s) in the 'split' list")
        }
        if(expand.split) {
            df.names <- names(coef(object))
            split <- splitInteractions(split, attr(Terms, "factors"),
                                       nmeffect, asgn, df.names)
            ns <- names(split)
        }
    }

    for (y in 1L:nresp) {
        if(is.null(effects)) {
            nterms <- 0
            df <- ss <- ms <- numeric(0L)
            nmrows <- character(0L)
        } else {
            df <- ss <- numeric(0L)
            nmrows <- character(0L)
            for(i in seq(nterms)) {
                ai <- (asgn == uasgn[i])
                df <- c(df, sum(ai))
                ss <- c(ss, sum(effects[ai, y]^2))
                nmi <- nmeffect[1 + uasgn[i]]
                nmrows <- c(nmrows, nmi)
                if(!missing(split) && !is.na(int <- match(nmi, ns))) {
                    df <- c(df, unlist(lapply(split[[int]], length)))
                    if(is.null(nms <- names(split[[int]])))
                        nms <- paste("C", seq_along(split[[int]]), sep = "")
                    ss <- c(ss, unlist(lapply(split[[int]],
                                              function(i, e)
                                              sum(e[i]^2), effects[ai, y])))
                    nmrows <- c(nmrows, paste("  ", nmi, ": ", nms, sep = ""))
                }
            }
        }
        if(rdf > 0) {
            df <- c(df, rdf)
            ss <- c(ss, sum(resid[, y]^2))
            nmrows <- c(nmrows,  "Residuals")
        }
        nt <- length(df)
        ms <- ifelse(df > 0L, ss/df, NA)
        x <- list(Df = df, "Sum Sq" = ss, "Mean Sq" = ms)
        if(rdf > 0) {
            TT <- ms/ms[nt]
            TP <- pf(TT, df, rdf, lower.tail = FALSE)
            TT[nt] <- TP[nt] <- NA
            x$"F value" <- TT
            x$"Pr(>F)" <- TP
            ## 'nterms' ~= 'Residuals' have no P-value
        }
        class(x) <- c("anova", "data.frame")
        attr(x, "row.names") <- format(nmrows)
        if(!keep.zero.df) x <- x[df > 0, ]
        pm <- pmatch("(Intercept)", row.names(x), 0L)
        if(!intercept && pm > 0) x <- x[-pm ,]
        ans[[y]] <- x
    }
    class(ans) <- c("summary.aov", "listof")
    attr(ans, "na.action") <- object$na.action
    ans
}

print.summary.aov <-
    function(x, digits = max(3, getOption("digits") - 3), symbolic.cor = FALSE,
             signif.stars= getOption("show.signif.stars"),	...)
{
    if (length(x) == 1L)  print(x[[1L]], ...)
    else NextMethod()
    if(nzchar(mess <- naprint(attr(x, "na.action")))) cat(mess, "\n", sep="")
    invisible(x)
}

coef.aov <- function(object, ...)
{
    z <- object$coefficients
    z[!is.na(z)]
}

alias <- function(object, ...) UseMethod("alias")

alias.formula <- function(object, data, ...)
{
    lm.obj <- if(missing(data)) aov(object) else aov(object, data)
    alias(lm.obj, ...)
}

alias.lm <- function(object, complete = TRUE, partial = FALSE,
                     partial.pattern = FALSE, ...)
{
    CompPatt <- function(x, ...) {
        x[abs(x) < 1e-6] <- 0
        MASS::fractions(x)
    }
    PartPatt <- function(x) {
        z <- zapsmall(x) != 0
        if(any(z)) {
            xx <- abs(signif(x[z], 2))
            ll <- length(unique(xx))
            if(ll > 10) xx <- cut(xx, 9) else if(ll == 1) x[] <- 1
            x[z] <- paste(ifelse(x[z] > 0, " ", "-"), xx, sep = "")
        }
        x[!z] <- ""
        collabs <- colnames(x)
        collabs <- if(length(collabs)) abbreviate(sub("\\.", "", collabs), 3)
        else 1L:ncol(x)
        colnames(x) <- collabs
        class(x) <- "mtable"
        x
    }
    Model <- object$terms
    attributes(Model) <- NULL
    value <- list(Model = Model)
    R <- object$qr$qr
    R <- R[1L:min(dim(R)), , drop=FALSE]
    R[lower.tri(R)] <- 0
    d <- dim(R)
    rank <- object$rank
    p <- d[2L]
    if(complete) {                      # full rank, no aliasing
        value$Complete <-
            if(is.null(p) || rank == p) NULL else {
                p1 <- 1L:rank
                X <- R[p1, p1]
                Y <-  R[p1, -p1, drop = FALSE]
                beta12 <- as.matrix(qr.coef(qr(X), Y))
                # dimnames(beta12) <- list(dn[p1], dn[ -p1])
                CompPatt(t(beta12))
            }
    }
    if(partial) {
        tmp <- summary.lm(object)$cov.unscaled
        ses <- sqrt(diag(tmp))
        beta11 <- tmp /outer(ses, ses)
        beta11[row(beta11) >= col(beta11)] <- 0
        beta11[abs(beta11) < 1e-6] <- 0
        if(all(beta11 == 0)) beta11 <- NULL
        else if(partial.pattern) beta11 <- PartPatt(beta11)
        value$Partial <- beta11
    }
    class(value) <- "listof"
    value
}

print.aovlist <- function(x, ...)
{
    cl <- attr(x, "call")
    if(!is.null(cl)) {
        cat("\nCall:\n")
        dput(cl)
    }
    if(!is.null(attr(x, "weights")))
        cat("Note: The results below are on the weighted scale\n")
    nx <- names(x)
    if(nx[1L] == "(Intercept)") {
        mn <- x[[1L]]$coefficients
        if(is.matrix(mn)) {
            cat("\nGrand Means:\n")
            print(format(mn[1,]), quote=FALSE)
        } else cat("\nGrand Mean:", format(mn[1L]), "\n")
        nx <- nx[-1L]
    }
    for(ii in seq_along(nx)) {
        i <- nx[ii]
        cat("\nStratum ", ii, ": ", i, "\n", sep = "")
        xi <- x[[i]]
        print(xi, ...)
    }
    invisible(x)
}

summary.aovlist <- function(object, ...)
{
    if(!is.null(attr(object, "weights")))
        cat("Note: The results below are on the weighted scale\n")
    dots <- list(...)
    strata <- names(object)
    if(strata[1L] == "(Intercept)") {
        strata <- strata[-1L]
        object <- object[-1L]
    }
    x <- vector(length = length(strata), mode = "list")
    names(x) <- paste("Error:", strata)
    for(i in seq_along(strata))
        x[[i]] <- do.call("summary", c(list(object = object[[i]]), dots))
    class(x) <- "summary.aovlist"
    x
}

print.summary.aovlist <- function(x, ...)
{
    nn <- names(x)
    for (i in nn) {
        cat("\n", i, "\n", sep="")
        print(x[[i]], ...)
    }
    invisible(x)
}

coef.listof <- function(object, ...)
{
    val <- vector("list", length(object))
    names(val) <- names(object)
    for(i in seq_along(object)) val[[i]] <- coef(object[[i]])
    class(val) <- "listof"
    val
}

se.contrast <- function(object, ...) UseMethod("se.contrast")

se.contrast.aov <-
    function(object, contrast.obj, coef = contr.helmert(ncol(contrast))[, 1],
             data = NULL, ...)
{
    contrast.weight.aov <- function(object, contrast)
    {
        asgn <- object$assign[object$qr$pivot[1L:object$rank]]
        uasgn <- unique(asgn)
        nterms <- length(uasgn)
        nmeffect <- c("(Intercept)",
                      attr(object$terms, "term.labels"))[1 + uasgn]
        effects <- as.matrix(qr.qty(object$qr, contrast))
        res <- matrix(0, nrow = nterms, ncol = ncol(effects),
                      dimnames = list(nmeffect, colnames(contrast)))
        for(i in seq(nterms)) {
            select <- (asgn == uasgn[i])
            res[i,] <- colSums(effects[seq_along(asgn)[select], , drop = FALSE]^2)
        }
        res
    }
    if(is.null(data)) contrast.obj <- eval(contrast.obj)
    else contrast.obj <- eval(substitute(contrast.obj), data, parent.frame())
    if(!is.matrix(contrast.obj)) { # so a list
        if(!missing(coef)) {
            if(sum(coef) != 0)
                stop("'coef' must define a contrast, i.e., sum to 0")
            if(length(coef) != length(contrast.obj))
                stop("'coef' must have same length as 'contrast.obj'")
        }
        contrast <-
            sapply(contrast.obj, function(x)
               {
                   if(!is.logical(x))
                           stop(gettextf("each element of '%s' must be logical",
                                         substitute(contrasts.list)),
                                domain = NA)
                   x/sum(x)
               })
        if(!length(contrast) || all(is.na(contrast)))
            stop("the contrast defined is empty (has no TRUE elements)")
        contrast <- contrast %*% coef
    } else {
        contrast <- contrast.obj
        if(any(abs(colSums(contrast)) > 1e-8))
            stop("columns of 'contrast.obj' must define a contrast (sum to zero)")
        if(length(colnames(contrast)) == 0L)
            colnames(contrast) <- paste("Contrast", seq(ncol(contrast)))
    }
    weights <- contrast.weight.aov(object, contrast)
    rdf <- object$df.residual
    if(rdf == 0) stop("no degrees of freedom for residuals")
    resid <- as.matrix(object$residuals)
    wt <- object$weights
    if(!is.null(wt)) resid <- resid * wt^0.5
    rse <- sum(resid^2)/rdf
    if(!is.matrix(contrast.obj)) sqrt(sum(weights) * rse)
    else sqrt(rse * colSums(weights))
}

se.contrast.aovlist <-
    function(object, contrast.obj, coef = contr.helmert(ncol(contrast))[, 1],
             data = NULL, ...)
{
    contrast.weight.aovlist <- function(object, contrast)
    {
        e.qr <- attr(object, "error.qr")
        if(!is.qr(e.qr))
            stop("'object' does not include an error 'qr' component")
        c.qr <- qr.qty(e.qr, contrast)
        e.assign <- attr(e.qr$qr, "assign")
        n.object <- length(object)
        e.assign <- c(e.assign,
                      rep.int(n.object - 1, nrow(c.qr) - length(e.assign)))
        res <- vector(length = n.object, mode = "list")
        names(res) <- names(object)
        for(j in seq_along(names(object))) {
            strata <- object[[j]]
            if(is.qr(strata$qr)) {
                scontrast <- c.qr[e.assign == (j - 1), , drop = FALSE]
                effects <- as.matrix(qr.qty(strata$qr, scontrast))
                asgn <- strata$assign[strata$qr$pivot[1L:strata$rank]]
                uasgn <- unique(asgn)
                nm <- c("(Intercept)", attr(strata$terms, "term.labels"))
                res.i <-
                    matrix(0, length(uasgn), ncol(effects),
                           dimnames = list(nm[1 + uasgn], colnames(contrast)))
                for(i in seq_along(uasgn)) {
                    select <- (asgn == uasgn[i])
                    res.i[i, ] <-
                        colSums(effects[seq_along(asgn)[select], , drop = FALSE]^2)
                }
                res[[j]] <- res.i
            }
        }
        res
    }
    SS <- function(aov.object)
    {
        rdf <- aov.object$df.residual
        if(is.null(rdf)) {
            nobs <- length(aov.object$residuals)
            rank <- aov.object$rank
            rdf <- nobs - rank
        }
        if(rdf == 0) return(NA)
        resid <- as.matrix(aov.object$residuals)
        wt <- aov.object$weights
        if(!is.null(wt)) resid <- resid * wt^0.5
        sum(resid^2)/rdf
    }
    if(is.null(attr(object, "error.qr"))) {
        message("Refitting model to allow projection")
        object <- update(object, qr = TRUE)
    }
    contrast.obj <-
        if(is.null(data)) eval(contrast.obj)
        else eval(substitute(contrast.obj), data, parent.frame())
    if(!is.matrix(contrast.obj)) {
        if(!missing(coef)) {
            if(sum(coef) != 0)
                stop("'coef' must define a contrast, i.e., sum to 0")
            if(length(coef) != length(contrast.obj))
                stop("'coef' must have same length as 'contrast.obj'")
        }
        contrast <-
            sapply(contrast.obj,
                   function(x) {
                       if(!is.logical(x))
                           stop(gettextf("each element of '%s' must be logical",
                                         substitute(contrasts.obj)),
                                domain = NA)
                       x/sum(x)
                   })
        if(!length(contrast) || all(is.na(contrast)))
            stop("the contrast defined is empty (has no TRUE elements)")
        contrast <- contrast %*% coef
    }
    else {
        contrast <- contrast.obj
        if(any(abs(colSums(contrast)) > 1e-8))
            stop("columns of 'contrast.obj' must define a contrast(sum to zero)")
        if(length(colnames(contrast)) == 0L)
            colnames(contrast) <- paste("Contrast", seq(ncol(contrast)))
    }
    weights <- contrast.weight.aovlist(object, contrast)
    weights <- weights[-match("(Intercept)", names(weights))]
    effic <- eff.aovlist(object)
    ## Need to identify the lowest stratum where each nonzero term appears
    eff.used <- apply(effic, 2L,
                      function(x, ind = seq_along(x)) {
                          temp <- (x > 0)
                          if(sum(temp) == 1) temp
                          else max(ind[temp]) == ind
                      })
    if(is.matrix(eff.used)) {
        strata.nms <- rownames(effic)[row(eff.used)[eff.used]]
        var.nms <- colnames(effic)[col(eff.used)[eff.used]]
    } else {
        strata.nms <- rownames(effic)
        var.nms <- colnames(effic)
    }
    rse.list <- sapply(object[unique(strata.nms)], SS)
    wgt <- matrix(0, nrow = length(var.nms), ncol = ncol(contrast),
                  dimnames = list(var.nms, colnames(contrast)))
    for(i in seq_along(var.nms))
        wgt[i, ] <- weights[[strata.nms[i]]][var.nms[i], , drop = FALSE]
    rse <- rse.list[strata.nms]
    eff <- effic[eff.used]
    drop(sqrt((rse/eff^2) %*% wgt))
}
#  File src/library/stats/R/approx.R
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

### approx() and approxfun() are *very similar* -- keep in sync!

approx <- function(x, y = NULL, xout, method = "linear", n = 50,
		   yleft, yright, rule = 1, f = 0, ties = mean)
{
    x <- xy.coords(x, y) # -> (x,y) numeric of same length
    y <- x$y
    x <- x$x
    nx <- length(x)
    method <- pmatch(method, c("linear", "constant"))
    if (is.na(method))
	stop("invalid interpolation method")
    stopifnot(is.numeric(rule), (lenR <- length(rule)) >= 1, lenR <= 2)
    if(lenR == 1) rule <- rule[c(1,1)]
    if(any(na <- is.na(x) | is.na(y))) {
	ok <- !na
	x <- x[ok]
	y <- y[ok]
	nx <- length(x)
    }
    if (!identical(ties, "ordered")) {
	if (length(ux <- unique(x)) < nx) {
	    if (missing(ties))
		warning("collapsing to unique 'x' values")
	    y <- as.vector(tapply(y,x,ties))# as.v: drop dim & dimn.
	    x <- sort(ux)
	    nx <- length(x)
	} else {
	    o <- order(x)
	    x <- x[o]
	    y <- y[o]
	}
    }
    if (nx <= 1) {
	if(method == 1)# linear
	    stop("need at least two non-NA values to interpolate")
	if(nx == 0) stop("zero non-NA points")
    }

    if (missing(yleft))
	yleft <- if (rule[1] == 1) NA else y[1L]
    if (missing(yright))
	yright <- if (rule[2] == 1) NA else y[length(y)]
    stopifnot(length(yleft) == 1, length(yright) == 1, length(f) == 1)
    if (missing(xout)) {
	if (n <= 0)
	    stop("'approx' requires n >= 1")
	xout <- seq.int(x[1L], x[nx], length.out = n)
    }
    y <- .C("R_approx", as.double(x), as.double(y), as.integer(nx),
	    xout = as.double(xout), as.integer(length(xout)),
	    as.integer(method), as.double(yleft), as.double(yright),
	    as.double(f), NAOK = TRUE, PACKAGE = "stats")$xout
    list(x = xout, y = y)
}

approxfun <- function(x, y = NULL, method = "linear",
		   yleft, yright, rule = 1, f = 0, ties = mean)
{
    x <- xy.coords(x, y)
    y <- x$y
    x <- x$x
    n <- length(x)
    method <- pmatch(method, c("linear", "constant"))
    if (is.na(method))
	stop("invalid interpolation method")
    stopifnot(is.numeric(rule), (lenR <- length(rule)) >= 1, lenR <= 2)
    if(lenR == 1) rule <- rule[c(1,1)]
    if(any(o <- is.na(x) | is.na(y))) {
	o <- !o
	x <- x[o]
	y <- y[o]
	n <- length(x)
    }
    if (!identical(ties, "ordered")) {
	if (length(ux <- unique(x)) < n) {
	    if (missing(ties))
		warning("collapsing to unique 'x' values")
	    y <- as.vector(tapply(y,x,ties))# as.v: drop dim & dimn.
	    x <- sort(ux)
	    n <- length(x)
	    rm(ux)
	} else {
	    o <- order(x)
	    x <- x[o]
	    y <- y[o]
	}
    }
    if (n <= 1) {
	if(method == 1)# linear
	    stop("need at least two non-NA values to interpolate")
	if(n == 0) stop("zero non-NA points")
    }
    if (missing(yleft))
	yleft <- if (rule[1] == 1) NA else y[1L]
    if (missing(yright))
	yright <- if (rule[2] == 1) NA else y[length(y)]
    force(f)
    stopifnot(length(yleft) == 1, length(yright) == 1, length(f) == 1)
    rm(o, rule, ties)

## Changed here:
## suggestion:
    # 1. Test input consistency once
    .C("R_approxtest",as.double(x), as.double(y), as.integer(n),
        as.integer(method), as.double(f), NAOK = TRUE,
        PACKAGE = "stats")

    # 2. Create and return function that does not test input validity...
    function(v) .C("R_approxfun", as.double(x), as.double(y), as.integer(n),
        xout = as.double(v), as.integer(length(v)), as.integer(method),
        as.double(yleft), as.double(yright), as.double(f), NAOK = TRUE,
        PACKAGE = "stats")$xout
}

### This is a `variant' of  approx( method = "constant" ) :
findInterval <- function(x, vec, rightmost.closed = FALSE, all.inside = FALSE)
{
    ## Purpose: gives back the indices of  x in vec;  vec[] sorted
    ## -------------------------------------------------------------------------
    ## Author: Martin Maechler, Date:  4 Jan 2002, 10:16

    if(any(is.na(vec)))
	stop("'vec' contains NAs")
    if(is.unsorted(vec))
	stop("'vec' must be sorted non-decreasingly")
    ## deal with NA's in x:
    if(has.na <- any(ix <- is.na(x)))
	x <- x[!ix]
    nx <- length(x)
    index <- integer(nx)
    .C("find_interv_vec",
       xt = as.double(vec), n = as.integer(length(vec)),
       x  = as.double(x),  nx = as.integer(nx),
       as.logical(rightmost.closed),
       as.logical(all.inside),
       index, DUP = FALSE, NAOK = TRUE, # NAOK: 'Inf' only
       PACKAGE = "base")
    if(has.na) {
	ii <- as.integer(ix)
	ii[ix] <- NA
	ii[!ix] <- index
	ii
    } else index
}
#  File src/library/stats/R/ar.R
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

## based on, especially multivariate case, code by Martyn Plummer
ar <-
    function (x, aic = TRUE, order.max = NULL,
              method=c("yule-walker","burg", "ols", "mle", "yw"),
              na.action = na.fail, series = deparse(substitute(x)), ...)
{
    res <- switch(match.arg(method),
        "yule-walker" = ar.yw(x, aic=aic, order.max=order.max,
                  na.action = na.action, series=series, ...),
	"burg" = ar.burg(x, aic=aic, order.max=order.max,
                              na.action = na.action, series=series, ...),
	"ols" = ar.ols(x, aic=aic, order.max=order.max,
                              na.action = na.action, series=series, ...),
 	"mle" = ar.mle(x, aic=aic, order.max=order.max,
                              na.action = na.action, series=series, ...),
        "yw" = ar.yw(x, aic=aic, order.max=order.max,
                  na.action = na.action, series=series, ...)
   )
    res$call <- match.call()
    res
}

ar.yw <- function(x, ...) UseMethod("ar.yw")

ar.yw.default <-
    function (x, aic = TRUE, order.max = NULL, na.action = na.fail,
              demean = TRUE, series = NULL, ...)
{
    if(is.null(series)) series <- deparse(substitute(x))
    ists <- is.ts(x)
    x <- na.action(as.ts(x))
    if(ists)  xtsp <- tsp(x)
    xfreq <- frequency(x)
    x <- as.matrix(x)
    if(!is.numeric(x))
        stop("'x' must be numeric")
    if(any(is.na(x))) stop("NAs in 'x'")
    nser <- ncol(x)
    if (demean) {
        xm <- colMeans(x)
        x <- sweep(x, 2, xm, check.margin=FALSE)
    } else xm <- rep(0, nser)
    n.used <- nrow(x)
    order.max <- if (is.null(order.max))
	min(n.used-1, floor(10 * log10(n.used))) else round(order.max)
    if (order.max < 1) stop("'order.max' must be >= 1")
    else if (order.max >= n.used) stop("'order.max' must be < 'n.used'")
    xacf <- acf(x, type = "covariance", lag.max = order.max, plot = FALSE,
                demean = demean)$acf
    if(nser > 1) {
        ## multivariate case
        snames <- colnames(x)
        A <- B <- array(0, dim = c(order.max + 1L, nser, nser))
        A[1, , ] <- B[1, , ] <- diag(nser)
        EA <- EB <- xacf[1, , , drop = TRUE]
        partialacf <- array(dim = c(order.max, nser, nser))
        xaic <- numeric(order.max + 1)
        solve.yw <- function(m) {
            # Solve Yule-Walker equations with Whittle's
            # generalization of the Levinson(-Durbin) algorithm
            betaA <- betaB <- 0
            for (i in 0:m) {
                betaA <- betaA + A[i + 1, , ] %*% xacf[m + 2 - i, , ]
                betaB <- betaB + B[i + 1, , ] %*% t(xacf[m + 2 - i, , ])
            }
            KA <- -t(qr.solve(t(EB), t(betaA)))
            KB <- -t(qr.solve(t(EA), t(betaB)))
            EB <<- (diag(nser) - KB %*% KA) %*% EB
            EA <<- (diag(nser) - KA %*% KB) %*% EA
            Aold <- A
            Bold <- B
            for (i in 1L:(m + 1)) {
                A[i + 1, , ] <<- Aold[i + 1, , ] + KA %*% Bold[m + 2 - i, , ]
                B[i + 1, , ] <<- Bold[i + 1, , ] + KB %*% Aold[m + 2 - i, , ]
            }
        }
        cal.aic <- function() { # omits mean params, that is constant adj
            det <- abs(prod(diag(qr(EA)$qr)))
            return(n.used * log(det) + 2 * m * nser * nser)
        }
        cal.resid <- function() {
            resid <- array(0, dim = c(n.used - order, nser))
            for (i in 0L:order) {
                resid <- resid + x[(order - i + 1L):(n.used - i),
                                   , drop = FALSE] %*% t(ar[i + 1L, , ])
            }
            return(rbind(matrix(NA, order, nser), resid))
        }
        order <- 0
        for (m in 0:order.max) {
            xaic[m + 1] <- cal.aic()
            if (!aic || xaic[m + 1] == min(xaic[1L:(m + 1)])) {
                ar <- A
                order <- m
                var.pred <- EA * n.used/(n.used - nser * (m + 1))
            }
            if (m < order.max) {
                solve.yw(m)
                partialacf[m + 1, , ] <- -A[m + 2, , ]
            }
        }
        xaic <- xaic - min(xaic)
        names(xaic) <- 0:order.max
        resid <- cal.resid()
        if(order > 0 ) {
            ar <- -ar[2:(order + 1), , , drop = FALSE]
            dimnames(ar) <- list(1L:order, snames, snames)
        } else ar <- array(0, dim=c(0L, nser, nser),
                           dimnames=list(NULL, snames, snames))
        dimnames(var.pred) <- list(snames, snames)
        dimnames(partialacf) <- list(1L:order.max, snames, snames)
        colnames(resid) <- colnames(x)
    } else {
        ## univariate case
        r <- as.double(drop(xacf))
        z <- .Fortran(R_eureka,
                      as.integer(order.max),
                      r, r,
                      coefs=double(order.max^2),
                      vars=double(order.max),
                      double(order.max))
        coefs <- matrix(z$coefs, order.max, order.max)
        partialacf <- array(diag(coefs), dim=c(order.max, 1L, 1L))
        var.pred <- c(r[1L], z$vars)
        xaic <- n.used * log(var.pred) + 2 * (0:order.max) + 2 * demean
        xaic <- xaic - min(xaic)
        names(xaic) <- 0:order.max
        order <- if (aic) (0:order.max)[xaic == 0] else order.max
        ar <- if (order > 0) coefs[order, 1L:order] else numeric(0L)
        var.pred <- var.pred[order+1]
        ## Splus compatibility fix
        var.pred <- var.pred * n.used/(n.used - (order + 1))
        if(order > 0)
            resid <- c(rep(NA, order), embed(x, order+1) %*% c(1, -ar))
        else resid <- as.vector(x)
        if(ists) {
            attr(resid, "tsp") <- xtsp
            attr(resid, "class") <- "ts"
        }
    }
    res <- list(order=order, ar=ar, var.pred=var.pred, x.mean = drop(xm),
                aic = xaic, n.used=n.used, order.max=order.max,
                partialacf=partialacf, resid=resid, method = "Yule-Walker",
                series=series, frequency=xfreq, call=match.call())
    if(nser == 1 && order > 0)
        res$asy.var.coef <-
            solve(toeplitz(drop(xacf)[seq_len(order)]))*var.pred/n.used
    class(res) <- "ar"
    res
}

print.ar <- function(x, digits = max(3, getOption("digits") - 3), ...)
{
    cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
    nser <- NCOL(x$var.pred)
    if(nser > 1) {
        if(!is.null(x$x.intercept))
            res <- x[c("ar", "x.intercept", "var.pred")]
        else res <- x[c("ar", "var.pred")]
        res$ar <- aperm(res$ar, c(2,3,1))
        print(res, digits=digits)
    } else {
        if(x$order > 0) {
            cat("Coefficients:\n")
            coef <- drop(round(x$ar, digits = digits))
            names(coef) <- seq_len(x$order)
            print.default(coef, print.gap = 2)
        }
        if(!is.null(xint <- x$x.intercept) && !is.na(xint))
            cat("\nIntercept: ", format(xint, digits = digits),
                " (", format(x$asy.se.coef$x.mean, digits = digits),
                ") ", "\n", sep="")
        cat("\nOrder selected", x$order, " sigma^2 estimated as ",
            format(x$var.pred, digits = digits),"\n")

    }
    invisible(x)
}

predict.ar <- function(object, newdata, n.ahead = 1, se.fit=TRUE, ...)
{
    if(missing(newdata)) {
        newdata <- eval.parent(parse(text=object$series))
        if (!is.null(nas <- object$call$na.action))
            newdata <- eval.parent(call(nas, newdata))
    }
    nser <- NCOL(newdata)
    ar <- object$ar
    p <- object$order
    st <- tsp(as.ts(newdata))[2L]
    dt <- deltat(newdata)
    xfreq <- frequency(newdata)
    tsp(newdata) <- NULL
    class(newdata) <- NULL
    if(NCOL(ar) != nser)
        stop("number of series in 'object' and 'newdata' do not match")
    n <- NROW(newdata)
    if(nser > 1) {
        if(is.null(object$x.intercept)) xint <- rep(0, nser)
        else xint <- object$x.intercept
        x <- rbind(sweep(newdata, 2, object$x.mean, check.margin=FALSE),
                   matrix(rep(0, nser), n.ahead, nser, byrow=TRUE))
        if(p > 0) {
            for(i in 1L:n.ahead) {
                x[n+i,] <- ar[1,,] %*% x[n+i-1,] + xint
                if(p > 1) for(j in 2:p)
                    x[n+i,] <- x[n+i,] + ar[j,,] %*% x[n+i-j,]
            }
            pred <- x[n+(1L:n.ahead), ]
        } else {
            pred <- matrix(xint, n.ahead, nser, byrow=TRUE)
        }
        pred <- pred + matrix(object$x.mean, n.ahead, nser, byrow=TRUE)
        colnames(pred) <- colnames(object$var.pred)
        if(se.fit) {
            warning("'se.fit' not yet implemented for multivariate models")
            se <- matrix(NA, n.ahead, nser)
        }
    } else {
        if(is.null(object$x.intercept)) xint <- 0
        else xint <- object$x.intercept
        x <- c(newdata - object$x.mean, rep(0, n.ahead))
        if(p > 0) {
            for(i in 1L:n.ahead) {
                x[n+i] <- sum(ar * x[n+i - (1L:p)]) + xint
            }
            pred <- x[n+(1L:n.ahead)]
            if(se.fit) {
                npsi <- n.ahead - 1
                psi <- .C(R_artoma,
                        as.integer(object$order), as.double(ar),
                        psi = double(npsi+object$order+1),
                        as.integer(npsi+object$order+1))$psi[1L:npsi]
                vars <- cumsum(c(1, psi^2))
                se <- sqrt(object$var.pred*vars)[1L:n.ahead]
            }
        } else {
            pred <- rep(xint, n.ahead)
            if (se.fit) se <- rep(sqrt(object$var.pred), n.ahead)
        }
        pred <- pred + rep(object$x.mean, n.ahead)
    }
    pred <- ts(pred, start = st + dt, frequency=xfreq)
    if(se.fit) se <- ts(se, start = st + dt, frequency=xfreq)
    if(se.fit) return(list(pred=pred, se=se)) else return(pred)
}

#  File src/library/stats/R/ar.burg.R
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

## ar.burg by B.D. Ripley based on R version by Martyn Plummer
ar.burg <- function(x, ...) UseMethod("ar.burg")
ar.burg.default <-
    function (x, aic = TRUE, order.max = NULL, na.action = na.fail,
                   demean = TRUE, series = NULL, var.method = 1, ...)
{
    if(is.null(series)) series <- deparse(substitute(x))
    if (!is.null(dim(x)))
        stop("Burg's algorithm only implemented for univariate series")
    if (ists <- is.ts(x)) xtsp <- tsp(x)
    x <- na.action(as.ts(x))
    if(any(is.na(x))) stop("NAs in 'x'")
    if (ists)  xtsp <- tsp(x)
    xfreq <- frequency(x)
    x <- as.vector(x)
    if (demean) {
        x.mean <- mean(x)
        x <- x - x.mean
    } else x.mean <- 0
    n.used <- length(x)
    order.max <- if (is.null(order.max))
	min(n.used-1L, floor(10 * log10(n.used)))
    else floor(order.max)
    if (order.max < 1L) stop("'order.max' must be >= 1")
    else if (order.max >= n.used) stop("'order.max' must be < 'n.used'")
    xaic <- numeric(order.max + 1)
    z <- .C(R_burg,
            as.integer(n.used),
            as.double(x),
            as.integer(order.max),
            coefs=double(order.max^2),
            var1=double(1+order.max),
            var2=double(1+order.max)
            )
    coefs <- matrix(z$coefs, order.max, order.max)
    partialacf <- array(diag(coefs), dim = c(order.max, 1L, 1L))
    var.pred <- if(var.method == 1L) z$var1 else z$var2
    xaic <- n.used * log(var.pred) + 2 * (0L:order.max) + 2 * demean
    xaic <- xaic - min(xaic)
    names(xaic) <- 0L:order.max
    order <- if (aic) (0L:order.max)[xaic == 0] else order.max
    ar <- if (order > 0L) coefs[order, 1L:order] else numeric(0L)
    var.pred <- var.pred[order+1L]
    if(order > 0L)
        resid <- c(rep(NA, order), embed(x, order+1) %*% c(1, -ar))
    else resid <- as.vector(x)
    if(ists) {
        attr(resid, "tsp") <- xtsp
        attr(resid, "class") <- "ts"
    }
    res <- list(order = order, ar = ar, var.pred = var.pred, x.mean = x.mean,
                aic = xaic, n.used = n.used, order.max = order.max,
                partialacf = partialacf, resid = resid,
                method = ifelse(var.method==1L,"Burg","Burg2"),
                series = series, frequency = xfreq, call = match.call())
    if(order > 0L) {
        xacf <- acf(x, type = "covariance", lag.max = order, plot=FALSE)$acf
        res$asy.var.coef <- solve(toeplitz(drop(xacf)[seq_len(order)]))*var.pred/n.used
    }
    class(res) <- "ar"
    return(res)
}

#  File src/library/stats/R/ar.burg.mts.R
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

"ar.burg.mts" <-
function (x, aic = TRUE, order.max = NULL, na.action = na.fail,
    demean = TRUE, series = NULL, var.method = 1, ...)
{
    if (is.null(series))
        series <- deparse(substitute(x))
    if (ists <- is.ts(x))
        xtsp <- tsp(x)
    x <- na.action(as.ts(x))
    if (any(is.na(x)))
        stop("NAs in 'x'")
    if (ists)
        xtsp <- tsp(x)
    xfreq <- frequency(x)
    x <- as.matrix(x)
    nser <- ncol(x)
    n.used <- nrow(x)
    if (demean) {
        x.mean <- colMeans(x)
        x <- sweep(x, 2, x.mean, check.margin=FALSE)
    }
    else x.mean <- rep(0, nser)
    order.max <- if (is.null(order.max))
        floor(10 * log10(n.used))
    else floor(order.max)
    xaic <- numeric(order.max + 1)
    z <- .C(R_multi_burg,
            as.integer(n.used),
            resid = as.double(x),
            as.integer(order.max),
            as.integer(nser),
            coefs = double((1 + order.max) * nser * nser),
            pacf = double((1 + order.max) * nser * nser),
            var = double((1 + order.max) * nser * nser),
            aic = double(1 + order.max),
            order = integer(1L),
            as.integer(aic),
            as.integer(var.method))
    partialacf <- aperm(array(z$pacf, dim = c(nser, nser, order.max +
        1)), c(3, 2, 1))[-1, , , drop = FALSE]
    var.pred <- aperm(array(z$var, dim = c(nser, nser, order.max +
        1)), c(3, 2, 1))
    xaic <- z$aic - min(z$aic)
    names(xaic) <- 0:order.max
    order <- z$order
    ar <- if (order > 0)
        -aperm(array(z$coefs, dim = c(nser, nser, order.max + 1)),
               c(3, 2, 1))[2:(order + 1), , , drop = FALSE]
    else array(dim = c(0, nser, nser))
    var.pred <- var.pred[order + 1, , , drop = TRUE]
    resid <- matrix(z$resid, nrow = n.used, ncol = nser)
    if (order > 0)
        resid[1L:order, ] <- NA
    if (ists) {
        attr(resid, "tsp") <- xtsp
        attr(resid, "class") <- "mts"
    }
    snames <- colnames(x)
    colnames(resid) <- snames
    dimnames(ar) <- list(seq_len(order), snames, snames)
    dimnames(var.pred) <- list(snames, snames)
    dimnames(partialacf) <- list(1L:order.max, snames, snames)
    res <- list(order = order, ar = ar, var.pred = var.pred,
        x.mean = x.mean, aic = xaic, n.used = n.used, order.max = order.max,
        partialacf = partialacf, resid = resid, method = ifelse(var.method ==
            1, "Burg", "Burg2"), series = series, frequency = xfreq,
        call = match.call())
    class(res) <- "ar"
    return(res)
}
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
#  File src/library/stats/R/ar.ols.R
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

## code by Adrian Trapletti
ar.ols <- function (x, aic = TRUE, order.max = NULL, na.action = na.fail,
                    demean = TRUE, intercept = demean, series = NULL, ...)
{
    if(is.null(series)) series <- deparse(substitute(x))
    rescale <- TRUE
    ists <- is.ts(x)
    x <- na.action(as.ts(x))
    xfreq <- frequency(x)
    if(any(is.na(x))) stop("NAs in 'x'")
    if(ists)  xtsp <- tsp(x)
    x <- as.matrix(x)
    if(!is.numeric(x))
        stop("'x' must be numeric")
    n.used <- nrow(x)
    nser <- ncol(x)
    if(rescale) {
        sc <- sqrt(drop(apply(x, 2L, var)))
        x <- x/rep(sc, rep(n.used, nser))
    } else sc <- rep(1, nser)

    order.max <- if (is.null(order.max))
	min(n.used-1, floor(10 * log10(n.used)))
    else round(order.max)
    if (order.max < 0) stop ("'order.max' must be >= 0")
    else if (order.max >= n.used) stop("'order.max' must be < 'n.used'")
    if (aic) order.min <- 0
    else order.min <- order.max
    A <- vector("list", order.max - order.min + 1)
    varE <- vector("list", order.max - order.min + 1)
    seA <- vector("list", order.max - order.min + 1)
    aic <- rep(Inf, order.max - order.min + 1)

    det <- function(x) { prod(diag(qr(x)$qr))*(-1)^(ncol(x)-1) }

    ## remove means for conditioning
    if(demean) {
        xm <- colMeans(x)
        x <- sweep(x, 2, xm, check.margin=FALSE)
    } else xm <- rep(0, nser)
    ## Fit models of increasing order

    for (m in order.min:order.max)
    {
        y <- embed(x, m+1)
        if(intercept) {
            if (m > 0) X <- cbind(rep(1,nrow(y)), y[, (nser+1):ncol(y)])
            else X <- as.matrix(rep(1, nrow(y)))
        } else {
            if (m > 0) X <- y[, (nser+1):ncol(y)]
            else X <- matrix(0, nrow(y), 0)
        }
        Y <- t(y[, 1L:nser])
        N <- ncol(Y)
        XX <- t(X)%*%X
        rank <- qr(XX)$rank
        if (rank != nrow(XX))
        {
            warning ("model order: ", m,

                     "singularities in the computation of the projection matrix",
                     "results are only valid up to model order", m - 1)
            break
        }
        P <- if(ncol(XX) > 0) solve(XX) else XX
        A[[m - order.min + 1]] <- Y %*% X %*% P
        YH <- A[[m - order.min + 1]] %*% t(X)
        E <- (Y - YH)
        varE[[m - order.min + 1]] <- E %*% t(E)/N
        varA <- P %x% (varE[[m - order.min + 1]])
        seA[[m - order.min+1]] <- if(ncol(varA) > 0) sqrt(diag(varA))
        else numeric(0L)
        aic[m - order.min+1] <-
            n.used*log(det(varE[[m-order.min+1]]))+2*nser*(nser*m+intercept)
    }

    m <- which(aic==min(aic)) + order.min - 1 # Determine best model

    ## Recalculate residuals of best model

    y <- embed(x, m+1)
    AA <- A[[m - order.min + 1]]
    if(intercept) {
        xint <- AA[, 1]
        ar <- AA[, -1]
        if (m > 0) X <- cbind(rep(1,nrow(y)), y[, (nser+1):ncol(y)])
        else X <- as.matrix(rep(1, nrow(y)))
    } else {
        if (m > 0) X <- y[, (nser+1):ncol(y)]
        else X <- matrix(0, nrow(y), 0)
        xint <- NULL
        ar <- AA
    }
    Y <- t(y[, 1L:nser, drop=FALSE])
    YH <- AA %*% t(X)
    E <- drop(rbind(matrix(NA, m, nser), t(Y - YH)))

    aic <- aic - min(aic)
    names(aic) <- order.min:order.max
    dim(ar) <- c(nser, nser, m)
    ar <- aperm(ar, c(3,1,2))
    ses <- seA[[m - order.min + 1]]
    if(intercept) {
        sem <- ses[1L:nser]
        ses <- ses[-(1L:nser)]
    } else sem <- rep(0, nser)
    dim(ses) <- c(nser, nser, m)
    ses <- aperm(ses, c(3,1,2))
    var.pred <- varE[[m - order.min + 1]]
    if(nser > 1) {
        snames <- colnames(x)
        dimnames(ses) <- dimnames(ar) <- list(seq_len(m), snames, snames)
        dimnames(var.pred) <- list(snames, snames)
        names(sem) <- colnames(E) <- snames
    }
    if(ists) {
        attr(E, "tsp") <- xtsp
        attr(E, "class") <- "ts"
    }
    if(rescale) {
        xm <- xm * sc
        if(!is.null(xint)) xint <- xint * sc
        aa <- outer(sc, 1/sc)
        if(nser > 1 && m > 0)
            for(i in 1L:m) ar[i,,] <- ar[i,,]*aa
        var.pred <- var.pred * outer(sc, sc)
        E <- E * rep(sc, rep(NROW(E), nser))
        sem <- sem*sc
        if(m > 0)
            for(i in 1L:m) ses[i,,] <- ses[i,,]*aa
    }
    res <- list(order = m, ar = ar, var.pred = var.pred,
                x.mean = xm, x.intercept = xint, aic = aic,
                n.used = n.used, order.max = order.max,
                partialacf = NULL, resid = E, method = "Unconstrained LS",
                series = series, frequency = xfreq, call = match.call(),
                asy.se.coef = list(x.mean = sem, ar=drop(ses)))
    class(res) <- "ar"
    res
}
#  File src/library/stats/R/ar.yw.mts.R
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

"ar.yw.mts" <-
function (x, aic = TRUE, order.max = NULL, na.action = na.fail,
    demean = TRUE, series = NULL, var.method = 1, ...)
{
    if (is.null(series))
        series <- deparse(substitute(x))
    if (ists <- is.ts(x))
        xtsp <- tsp(x)
    x <- na.action(as.ts(x))
    if (any(is.na(x)))
        stop("NAs in 'x'")
    if (ists)
        xtsp <- tsp(x)
    xfreq <- frequency(x)
    x <- as.matrix(x)
    nser <- ncol(x)
    n.used <- nrow(x)
    if (demean) {
        x.mean <- colMeans(x)
        x <- sweep(x, 2, x.mean, check.margin=FALSE)
    }
    else x.mean <- rep(0, nser)
    order.max <- if (is.null(order.max))
        floor(10 * log10(n.used))
    else floor(order.max)
    if (order.max < 1)
        stop("'order.max' must be >= 1")
    xacf <- acf(x, type = "cov", plot = FALSE, lag.max = order.max)$acf
    z <- .C(R_multi_yw,
            aperm(xacf, c(3, 2, 1)),
            as.integer(n.used),
            as.integer(order.max),
            as.integer(nser),
            coefs = double((1 + order.max) * nser * nser),
            pacf = double((1 + order.max) * nser * nser),
            var = double((1 + order.max) * nser * nser),
            aic = double(1 + order.max),
            order = integer(1L),
            as.integer(aic))
    partialacf <- aperm(array(z$pacf, dim = c(nser, nser, order.max +
        1)), c(3, 2, 1))[-1, , , drop = FALSE]
    var.pred <- aperm(array(z$var, dim = c(nser, nser, order.max +
        1)), c(3, 2, 1))
    xaic <- z$aic - min(z$aic)
    names(xaic) <- 0:order.max
    order <- z$order
    resid <- x
    if (order > 0) {
        ar <- -aperm(array(z$coefs, dim = c(nser, nser, order.max +
            1)), c(3, 2, 1))[2:(order + 1), , , drop = FALSE]
        for (i in 1L:order) resid[-(1L:order), ] <- resid[-(1L:order),
            ] - x[(order - i + 1):(n.used - i), ] %*% t(ar[i,
            , ])
        resid[1L:order, ] <- NA
    }
    else ar <- array(dim = c(0, nser, nser))
    var.pred <- var.pred[order + 1, , , drop = TRUE] * n.used/(n.used -
        nser * (demean + order))
    if (ists) {
        attr(resid, "tsp") <- xtsp
        attr(resid, "class") <- c("mts", "ts")
    }
    snames <- colnames(x)
    colnames(resid) <- snames
    dimnames(ar) <- list(seq_len(order), snames, snames)
    dimnames(var.pred) <- list(snames, snames)
    dimnames(partialacf) <- list(1L:order.max, snames, snames)
    res <- list(order = order, ar = ar, var.pred = var.pred,
        x.mean = x.mean, aic = xaic, n.used = n.used, order.max = order.max,
        partialacf = partialacf, resid = resid, method = "Yule-Walker",
        series = series, frequency = xfreq, call = match.call())
    class(res) <- "ar"
    return(res)
}
#  File src/library/stats/R/arima.R
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

arima <- function(x, order = c(0, 0, 0),
                  seasonal = list(order = c(0, 0, 0), period = NA),
                  xreg = NULL, include.mean = TRUE,
                  transform.pars = TRUE, fixed = NULL, init = NULL,
                  method = c("CSS-ML", "ML", "CSS"), n.cond,
                  optim.method = "BFGS",
                  optim.control = list(), kappa = 1e6)
{
    "%+%" <- function(a, b) .Call(R_TSconv, a, b)

    upARIMA <- function(mod, phi, theta)
    {
        p <- length(phi); q <- length(theta)
        mod$phi <- phi; mod$theta <- theta
        r <- max(p, q + 1L)
        if(p > 0) mod$T[1L:p, 1L] <- phi
        if(r > 1L)
            mod$Pn[1L:r, 1L:r] <- .Call(R_getQ0, phi, theta)
        else if (p > 0)
            mod$Pn[1L, 1L] <- 1/(1 - phi^2)
        else
            mod$Pn[1,1] <- 1
        # End change
        mod$a[] <- 0
        mod
    }

    arimaSS <- function(y, mod)
    {
        ## next call changes objects a, P, Pn so beware!
        .Call(R_ARIMA_Like, y, mod$phi, mod$theta, mod$Delta,
              mod$a, mod$P, mod$Pn, as.integer(0L), TRUE)
    }

    armafn <- function(p, trans)
    {
        par <- coef
        par[mask] <- p
        trarma <- .Call(R_ARIMA_transPars, par, arma, trans)
        Z <- upARIMA(mod, trarma[[1L]], trarma[[2L]])
        if(ncxreg > 0) x <- x - xreg %*% par[narma + (1L:ncxreg)]
        ## next call changes objects a, P, Pn so beware!
        res <- .Call(R_ARIMA_Like, x, Z$phi, Z$theta, Z$Delta,
                     Z$a, Z$P, Z$Pn, as.integer(0L), FALSE)
        s2 <- res[1L]/res[3L]
        0.5*(log(s2) + res[2L]/res[3L])
    }

    armaCSS <- function(p)
    {
        par <- as.double(fixed)
        par[mask] <- p
        trarma <- .Call(R_ARIMA_transPars, par, arma, FALSE)
        if(ncxreg > 0) x <- x - xreg %*% par[narma + (1L:ncxreg)]
        res <- .Call(R_ARIMA_CSS, x, arma, trarma[[1L]], trarma[[2L]],
                     as.integer(ncond), FALSE)
        0.5 * log(res)
    }

    arCheck <- function(ar)
    {
        p <- max(which(c(1, -ar) != 0)) - 1
        if(!p) return(TRUE)
        all(Mod(polyroot(c(1, -ar[1L:p]))) > 1)
    }

    maInvert <- function(ma)
    {
        ## polyroot can't cope with leading zero.
        q <- length(ma)
        q0 <- max(which(c(1,ma) != 0)) - 1L
        if(!q0) return(ma)
        roots <- polyroot(c(1, ma[1L:q0]))
        ind <- Mod(roots) < 1
        if(all(!ind)) return(ma)
        if(q0 == 1) return(c(1/ma[1L], rep(0, q - q0)))
        roots[ind] <- 1/roots[ind]
        x <- 1
        for (r in roots) x <- c(x, 0) - c(0, x)/r
        c(Re(x[-1L]), rep(0, q - q0))
    }

    series <- deparse(substitute(x))
    if(NCOL(x) > 1L)
        stop("only implemented for univariate time series")
    method <- match.arg(method)

    x <- as.ts(x)
    if(!is.numeric(x))
        stop("'x' must be numeric")
    storage.mode(x) <- "double"  # a precaution
    dim(x) <- NULL
    n <- length(x)

    if(!missing(order))
        if(!is.numeric(order) || length(order) != 3L || any(order < 0))
            stop("'order' must be a non-negative numeric vector of length 3")
    if(!missing(seasonal))
        if(is.list(seasonal)) {
            if(is.null(seasonal$order))
                stop("'seasonal' must be a list with component 'order'")
            if(!is.numeric(seasonal$order) || length(seasonal$order) != 3L
               || any(seasonal$order < 0L))
                stop("'seasonal$order' must be a non-negative numeric vector of length 3")
        } else if(is.numeric(order)) {
            if(length(order) == 3L) seasonal <- list(order=seasonal)
            else ("'seasonal' is of the wrong length")
        } else stop("'seasonal' must be a list with component 'order'")

    if (is.null(seasonal$period) || is.na(seasonal$period)
        ||seasonal$period == 0) seasonal$period <- frequency(x)
    arma <- as.integer(c(order[-2L], seasonal$order[-2L], seasonal$period,
                         order[2L], seasonal$order[2L]))
    narma <- sum(arma[1L:4L])

    xtsp <- tsp(x)
    tsp(x) <- NULL
    Delta <- 1.
    for(i in seq_len(order[2L])) Delta <- Delta %+% c(1., -1.)
    for(i in seq_len(seasonal$order[2L]))
        Delta <- Delta %+% c(1, rep(0, seasonal$period-1), -1)
    Delta <- - Delta[-1L]
    nd <- order[2L] + seasonal$order[2L]
    n.used <- sum(!is.na(x)) - length(Delta)
    if (is.null(xreg)) {
        ncxreg <- 0L
    } else {
        nmxreg <- deparse(substitute(xreg))
        if (NROW(xreg) != n) stop("lengths of 'x' and 'xreg' do not match")
        ncxreg <- NCOL(xreg)
        xreg <- as.matrix(xreg)
        storage.mode(xreg) <- "double"
    }
    class(xreg) <- NULL
    if (ncxreg > 0L && is.null(colnames(xreg)))
        colnames(xreg) <-
            if(ncxreg == 1L) nmxreg else paste(nmxreg, 1L:ncxreg, sep = "")
    if (include.mean && (nd == 0L)) {
        xreg <- cbind(intercept = rep(1, n), xreg = xreg)
        ncxreg <- ncxreg + 1L
    }
    if(method == "CSS-ML") {
        anyna <- any(is.na(x))
        if(ncxreg) anyna <- anyna || any(is.na(xreg))
        if(anyna) method <- "ML"
    }

    if (method == "CSS" || method == "CSS-ML") {
        ncond <- order[2L] + seasonal$order[2L] * seasonal$period
        ncond1 <- order[1L] + seasonal$period * seasonal$order[1L]
        ncond <- if (!missing(n.cond)) ncond + max(n.cond, ncond1)
        else ncond + ncond1
    } else ncond <- 0

    if (is.null(fixed)) fixed <- rep(NA_real_, narma + ncxreg)
    else if(length(fixed) != narma + ncxreg) stop("wrong length for 'fixed'")
    mask <- is.na(fixed)
##    if(!any(mask)) stop("all parameters were fixed")
    no.optim <- !any(mask)
    if(no.optim) transform.pars <- FALSE
    if(transform.pars) {
        ind <- arma[1L] + arma[2L] + seq_len(arma[3L])
        if (any(!mask[seq_len(arma[1L])]) || any(!mask[ind])) {
            warning("some AR parameters were fixed: setting transform.pars = FALSE")
            transform.pars <- FALSE
        }
    }
    init0 <- rep(0, narma)
    parscale <- rep(1, narma)
    if (ncxreg) {
        cn <- colnames(xreg)
        orig.xreg <- (ncxreg == 1L) || any(!mask[narma + 1L:ncxreg])
        if (!orig.xreg) {
            S <- svd(na.omit(xreg))
            xreg <- xreg %*% S$v
        }
        fit <- lm(x ~ xreg - 1, na.action = na.omit)
        n.used <- sum(!is.na(resid(fit))) - length(Delta)
        init0 <- c(init0, coef(fit))
        ses <- summary(fit)$coefficients[, 2L]
        parscale <- c(parscale, 10 * ses)
    }
    if (n.used <= 0) stop("too few non-missing observations")

    if(!is.null(init)) {
        if(length(init) != length(init0))
            stop("'init' is of the wrong length")
        if(any(ind <- is.na(init))) init[ind] <- init0[ind]
        if(method == "ML") {
            ## check stationarity
            if(arma[1L] > 0)
                if(!arCheck(init[1L:arma[1L]]))
                    stop("non-stationary AR part")
            if(arma[3L] > 0)
                if(!arCheck(init[sum(arma[1L:2L]) + 1L:arma[3L]]))
                    stop("non-stationary seasonal AR part")
            if(transform.pars)
                init <- .Call(R_ARIMA_Invtrans, as.double(init), arma)
        }
    } else init <- init0

    coef <- as.double(fixed)
    if(!("parscale" %in% names(optim.control)))
       optim.control$parscale <- parscale[mask]

    if(method == "CSS") {
        res <- if(no.optim)
            list(convergence=0L, par=numeric(0L), value=armaCSS(numeric(0L)))
        else
            optim(init[mask], armaCSS,  method = optim.method, hessian = TRUE,
                  control = optim.control)
        if(res$convergence > 0)
            warning("possible convergence problem: optim gave code=",
                          res$convergence)
        coef[mask] <- res$par
        ## set model for predictions
        trarma <- .Call(R_ARIMA_transPars, coef, arma, FALSE)
        mod <- makeARIMA(trarma[[1L]], trarma[[2L]], Delta, kappa)
        if(ncxreg > 0) x <- x - xreg %*% coef[narma + (1L:ncxreg)]
        arimaSS(x, mod)
        val <- .Call(R_ARIMA_CSS, x, arma, trarma[[1L]], trarma[[2L]],
                     as.integer(ncond), TRUE)
        sigma2 <- val[[1L]]
        var <- if(no.optim) numeric(0L) else solve(res$hessian * n.used)
    } else {
        if(method == "CSS-ML") {
            res <- if(no.optim)
                list(convergence=0L, par=numeric(0L), value=armaCSS(numeric(0L)))
            else
                optim(init[mask], armaCSS,  method = optim.method,
                      hessian = FALSE, control = optim.control)
            if(res$convergence == 0) init[mask] <- res$par
            ## check stationarity
            if(arma[1L] > 0)
                if(!arCheck(init[1L:arma[1L]]))
                    stop("non-stationary AR part from CSS")
            if(arma[3L] > 0)
                if(!arCheck(init[sum(arma[1L:2L]) + 1L:arma[3L]]))
                    stop("non-stationary seasonal AR part from CSS")
            ncond <- 0L
        }
        if(transform.pars) {
            init <- .Call(R_ARIMA_Invtrans, init, arma)
            ## enforce invertibility
            if(arma[2L] > 0) {
                ind <- arma[1L] + 1L:arma[2L]
                init[ind] <- maInvert(init[ind])
            }
            if(arma[4L] > 0) {
                ind <- sum(arma[1L:3L]) + 1L:arma[4L]
                init[ind] <- maInvert(init[ind])
            }
        }
        trarma <- .Call(R_ARIMA_transPars, init, arma, transform.pars)
        mod <- makeARIMA(trarma[[1L]], trarma[[2L]], Delta, kappa)
        res <- if(no.optim)
            list(convergence = 0, par = numeric(0L),
                 value = armafn(numeric(0L), as.logical(transform.pars)))
        else
            optim(init[mask], armafn,  method = optim.method,
                  hessian = TRUE, control = optim.control,
                  trans = as.logical(transform.pars))
        if(res$convergence > 0)
            warning("possible convergence problem: optim gave code=",
                    res$convergence)
        coef[mask] <- res$par
        if(transform.pars) {
            ## enforce invertibility
            if(arma[2L] > 0L) {
                ind <- arma[1L] + 1L:arma[2L]
                if(all(mask[ind]))
                    coef[ind] <- maInvert(coef[ind])
            }
            if(arma[4L] > 0L) {
                ind <- sum(arma[1L:3L]) + 1L:arma[4L]
                if(all(mask[ind]))
                    coef[ind] <- maInvert(coef[ind])
            }
            if(any(coef[mask] != res$par))  {  # need to re-fit
                oldcode <- res$convergence
                res <- optim(coef[mask], armafn, method = optim.method,
                             hessian = TRUE,
                             control = list(maxit = 0,
                             parscale = optim.control$parscale),
                             trans = TRUE)
                res$convergence <- oldcode
                coef[mask] <- res$par
            }
            ## do it this way to ensure hessian was computed inside
            ## stationarity region
            A <- .Call(R_ARIMA_Gradtrans, as.double(coef), arma)
            A <- A[mask, mask]
            var <- t(A) %*% solve(res$hessian * n.used) %*% A
            coef <- .Call(R_ARIMA_undoPars, coef, arma)
        } else var <- if(no.optim) numeric(0L) else solve(res$hessian * n.used)
        trarma <- .Call(R_ARIMA_transPars, coef, arma, FALSE)
        mod <- makeARIMA(trarma[[1L]], trarma[[2L]], Delta, kappa)
        val <- if(ncxreg > 0L)
            arimaSS(x - xreg %*% coef[narma + (1L:ncxreg)], mod)
        else arimaSS(x, mod)
        sigma2 <- val[[1L]][1L]/n.used
    }
    value <- 2 * n.used * res$value + n.used + n.used * log(2 * pi)
    aic <- if(method != "CSS") value + 2*sum(mask) + 2 else NA
    nm <- NULL
    if (arma[1L] > 0L) nm <- c(nm, paste("ar", 1L:arma[1L], sep = ""))
    if (arma[2L] > 0L) nm <- c(nm, paste("ma", 1L:arma[2L], sep = ""))
    if (arma[3L] > 0L) nm <- c(nm, paste("sar", 1L:arma[3L], sep = ""))
    if (arma[4L] > 0L) nm <- c(nm, paste("sma", 1L:arma[4L], sep = ""))
    if (ncxreg > 0L) {
        nm <- c(nm, cn)
        if(!orig.xreg) {
            ind <- narma + 1L:ncxreg
            coef[ind] <- S$v %*% coef[ind]
            A <- diag(narma + ncxreg)
            A[ind, ind] <- S$v
            A <- A[mask, mask]
            var <- A %*% var %*% t(A)
        }
    }
    names(coef) <- nm
    if(!no.optim) dimnames(var) <- list(nm[mask], nm[mask])
    resid <- val[[2L]]
    tsp(resid) <- xtsp
    class(resid) <- "ts"
    res <- list(coef = coef, sigma2 = sigma2, var.coef = var, mask = mask,
                loglik = -0.5 * value, aic = aic, arma = arma,
                residuals = resid, call = match.call(), series = series,
                code = res$convergence, n.cond = ncond, model = mod)
    class(res) <- "Arima"
    res
}


print.Arima <-
    function (x, digits = max(3, getOption("digits") - 3), se = TRUE, ...)
{
    cat("\nCall:", deparse(x$call, width.cutoff = 75L), "", sep = "\n")
    if (length(x$coef)) {
        cat("Coefficients:\n")
        coef <- round(x$coef, digits = digits)
        ## use NROW as if all coefs are fixed there are no var.coef's
        if (se && NROW(x$var.coef)) {
            ses <- rep(0, length(coef))
            ses[x$mask] <- round(sqrt(diag(x$var.coef)), digits = digits)
            coef <- matrix(coef, 1L, dimnames = list(NULL, names(coef)))
            coef <- rbind(coef, s.e. = ses)
        }
        print.default(coef, print.gap = 2)
    }
    cm <- x$call$method
    if(is.null(cm) || cm != "CSS")
        cat("\nsigma^2 estimated as ", format(x$sigma2, digits = digits),
            ":  log likelihood = ", format(round(x$loglik, 2L)),
            ",  aic = ", format(round(x$aic, 2L)), "\n", sep = "")
    else
        cat("\nsigma^2 estimated as ",
            format(x$sigma2, digits = digits),
            ":  part log likelihood = ", format(round(x$loglik,2)),
            "\n", sep="")
    invisible(x)
}


predict.Arima <-
    function (object, n.ahead = 1, newxreg = NULL, se.fit = TRUE, ...)
{
    myNCOL <- function(x) if (is.null(x)) 0 else NCOL(x)
    rsd <- object$residuals
    xr <- object$call$xreg
    xreg <- if (!is.null(xr)) eval.parent(xr) else NULL
    ncxreg <- myNCOL(xreg)
    if (myNCOL(newxreg) != ncxreg)
        stop("'xreg' and 'newxreg' have different numbers of columns")
    class(xreg) <- NULL
    xtsp <- tsp(rsd)
    n <- length(rsd)
    arma <- object$arma
    coefs <- object$coef
    narma <- sum(arma[1L:4L])
    if (length(coefs) > narma) {
        if (names(coefs)[narma + 1L] == "intercept") {
            xreg <- cbind(intercept = rep(1, n), xreg)
            newxreg <- cbind(intercept = rep(1, n.ahead), newxreg)
            ncxreg <- ncxreg + 1L
        }
        xm <- if(narma == 0) drop(as.matrix(newxreg) %*% coefs)
        else drop(as.matrix(newxreg) %*% coefs[-(1L:narma)])
    }
    else xm <- 0
    if (arma[2L] > 0L) {
        ma <- coefs[arma[1L] + 1L:arma[2L]]
        if (any(Mod(polyroot(c(1, ma))) < 1))
            warning("MA part of model is not invertible")
    }
    if (arma[4L] > 0L) {
        ma <- coefs[sum(arma[1L:3L]) + 1L:arma[4L]]
        if (any(Mod(polyroot(c(1, ma))) < 1))
            warning("seasonal MA part of model is not invertible")
    }
    z <- KalmanForecast(n.ahead, object$model)
    pred <- ts(z[[1L]] + xm, start = xtsp[2L] + deltat(rsd),
               frequency = xtsp[3L])
    if (se.fit) {
        se <- ts(sqrt(z[[2L]] * object$sigma2),
                 start = xtsp[2L] + deltat(rsd),
                 frequency = xtsp[3L])
        return(list(pred=pred, se=se))
    }
    else return(pred)
}

makeARIMA <- function(phi, theta, Delta, kappa = 1e6)
{
    p <- length(phi); q <- length(theta)
    r <- max(p, q + 1L); d <- length(Delta)
    rd <- r + d
    Z <- c(1., rep(0., r-1L), Delta)
    T <- matrix(0., rd, rd)
    if(p > 0) T[1L:p, 1L] <- phi
    if(r > 1L) {
        ind <- 2:r
        T[cbind(ind-1L, ind)]<- 1
    }
    if(d > 0L) {
        T[r+1L, ] <- Z
        if(d > 1L) {
            ind <- r + 2:d
            T[cbind(ind, ind-1)]<- 1
        }
    }
    if(q < r - 1L) theta <- c(theta, rep(0, r-1L-q))
    R <- c(1, theta, rep(0, d))
    V <- R %o% R
    h <- 0.
    a <- rep(0., rd)
    Pn <- P <- matrix(0., rd, rd)
    if(r > 1L) Pn[1L:r, 1L:r] <- .Call(R_getQ0, phi, theta)
    else Pn[1L, 1L] <- if(p > 0) 1/(1 - phi^2) else 1
    if(d > 0L) Pn[cbind(r+1L:d, r+1L:d)] <- kappa
    return(list(phi=phi, theta=theta, Delta=Delta, Z=Z, a=a, P=P, T=T, V=V,
                h=h, Pn=Pn))
}

coef.Arima <- function (object, ...) object$coef

vcov.Arima <- function (object, ...) object$var.coef

logLik.Arima <- function (object, ...) {
    res <- if(is.na(object$aic)) NA
    else structure(object$loglik, df=sum(object$mask) + 1)
    class(res) <- "logLik"
    res
}

## arima.sim() is in ./ts.R
#  File src/library/stats/R/arma0.R
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

arima0 <- function(x, order = c(0, 0, 0),
                   seasonal = list(order = c(0, 0, 0), period = NA),
                   xreg = NULL, include.mean = TRUE, delta = 0.01,
                   transform.pars = TRUE, fixed = NULL, init = NULL,
                   method = c("ML", "CSS"), n.cond,
                   optim.control = list())
{
    arma0f <- function(p)
    {
        par <- as.double(fixed)
        par[mask] <- p
        .Call(R_arma0fa, G, par)
    }

    arCheck <- function(ar)
    {
        p <- max(which(c(1, -ar) != 0)) - 1
        if(!p) return(TRUE)
        all(Mod(polyroot(c(1, -ar[1L:p]))) > 1)
    }

    maInvert <- function(ma)
    {
        ## polyroot can't cope with leading zero.
        q <- length(ma)
        q0 <- max(which(c(1,ma) != 0)) - 1
        if(!q0) return(ma)
        roots <- polyroot(c(1, ma[1L:q0]))
        ind <- Mod(roots) < 1
        if(all(!ind)) return(ma)
        warning("converting non-invertible initial MA values")
        if(q0 == 1) return(c(1/ma[1L], rep(0, q-q0)))
        roots[ind] <- 1/roots[ind]
        x <- 1
        for(r in roots) x <- c(x, 0) - c(0, x)/r
        c(Re(x[-1L]), rep(0, q-q0))
    }

    series <- deparse(substitute(x))
    if(NCOL(x) > 1)
        stop("only implemented for univariate time series")
    method <- match.arg(method)
    x <- as.ts(x)
    if(!is.numeric(x))
        stop("'x' must be numeric")
    dim(x) <- NULL
    n <- length(x)

    if(!missing(order))
        if(!is.numeric(order) || length(order) != 3 || any(order < 0))
            stop("'order' must be a non-negative numeric vector of length 3")
    if(!missing(seasonal))
        if(is.list(seasonal)) {
            if(is.null(seasonal$order))
                stop("'seasonal' must be a list with component 'order'")
            if(!is.numeric(seasonal$order) || length(seasonal$order) != 3
               || any(seasonal$order < 0))
                stop("'seasonal$order' must be a non-negative numeric vector of length 3")
        } else if(is.numeric(order)) {
            if(length(order) == 3) seasonal <- list(order=seasonal)
            else ("'seasonal' is of the wrong length")
        } else stop("'seasonal' must be a list with component 'order'")

    if(is.null(seasonal$period) || is.na(seasonal$period)
       || seasonal$period == 0) seasonal$period <- frequency(x)
    arma <- c(order[-2], seasonal$order[-2], seasonal$period,
              order[2L], seasonal$order[2L])
    narma <- sum(arma[1L:4L])
    if(d <- order[2L]) x <- diff(x, 1, d)
    if(d <- seasonal$order[2L]) x <- diff(x, seasonal$period, d)
    xtsp <- tsp(x)
    tsp(x) <- NULL
    nd <- order[2L] + seasonal$order[2L]
    n.used <- length(x)
    ncond <- n - n.used
    if(method == "CSS") {
        ncond1 <- order[1L] + seasonal$period * seasonal$order[1L]
        ncond <- if(!missing(n.cond)) ncond + max(n.cond, ncond1)
        else ncond + ncond1
    }
    if(is.null(xreg)) {
        ncxreg <- 0
    } else {
        if(NROW(xreg) != n) stop("lengths of 'x' and 'xreg' do not match")
        ncxreg <- NCOL(xreg)
    }
    class(xreg) <- NULL
    if(include.mean && (nd == 0)) {
        if(is.matrix(xreg) && is.null(colnames(xreg)))
            colnames(xreg) <- paste("xreg", 1L:ncxreg, sep = "")
        xreg <- cbind(intercept = rep(1, n), xreg = xreg)
        ncxreg <- ncxreg + 1
    }

    if (is.null(fixed)) fixed <- rep(NA_real_, narma + ncxreg)
    else if(length(fixed) != narma + ncxreg) stop("wrong length for 'fixed'")
    mask <- is.na(fixed)
    if(!any(mask)) stop("all parameters were fixed")
    if(transform.pars && any(!mask[1L:narma])) {
        warning("some ARMA parameters were fixed: setting transform.pars = FALSE")
        transform.pars <- FALSE
    }

    if(ncxreg) {
        if(d <- order[2L]) xreg <- diff(xreg, 1, d)
        if(d <- seasonal$order[2L]) xreg <- diff(xreg, seasonal$period, d)
        xreg <- as.matrix(xreg)
        if(qr(na.omit(xreg))$rank < ncol(xreg)) stop("'xreg' is collinear")
        if(is.null(cn <- colnames(xreg)))
            cn <- paste("xreg", 1L:ncxreg, sep = "")
    }
    if(any(is.na(x)) || (ncxreg && any(is.na(xreg))))
        ## only exact recursions handle NAs
        if(method == "ML" && delta >= 0) {
            warning("NAs present: setting 'delta' to -1")
            delta <- -1
        }

    init0 <- rep(0, narma)
    parscale <- rep(1, narma)
    if (ncxreg) {
        orig.xreg <- (ncxreg == 1) || any(!mask[narma + 1L:ncxreg])
        if(!orig.xreg) {
            S <- svd(na.omit(xreg))
            xreg <- xreg %*% S$v
        }
        fit <- lm(x ~ xreg - 1, na.action = na.omit)
        init0 <- c(init0, coef(fit))
        ses <- summary(fit)$coefficients[,2]
        parscale <- c(parscale, ses)
    }

    storage.mode(x) <- storage.mode(xreg) <- "double"
    if(method == "CSS") transform.pars <- 0
    G <- .Call(R_setup_starma, as.integer(arma), x, n.used, xreg,
               ncxreg, delta, transform.pars > 0,
               ncond - (n - n.used))
    on.exit(.Call(R_free_starma, G))

    if(!is.null(init)) {
        if(length(init) != length(init0))
            stop("'init' is of the wrong length")
        if(any(ind <- is.na(init))) init[ind] <- init0[ind]
        if(transform.pars) {
            if(any(!mask[1L:narma]))
                warning("transformed ARMA parameters were fixed")
            ## check stationarity
            if(arma[1L] > 0)
                if(!arCheck(init[1L:arma[1L]]))
                    stop("non-stationary AR part")
            if(arma[3L] > 0)
                if(!arCheck(init[sum(arma[1L:2]) + 1L:arma[3L]]))
                    stop("non-stationary seasonal AR part")
            ## enforce invertibility
            if(arma[2L] > 0) {
                ind <- arma[1L] + 1L:arma[2L]
                init[ind] <- maInvert(init[ind])
            }
            if(arma[4L] > 0) {
                ind <- sum(arma[1L:3]) + 1L:arma[4L]
                init[ind] <- maInvert(init[ind])
            }
            init <- .Call(R_Invtrans, G, as.double(init))
        }
    } else init <- init0


    .Call(R_Starma_method, G, method == "CSS")
    if(!("parscale" %in% names(optim.control)))
       optim.control$parscale <- parscale[mask]
    res <- optim(init[mask], arma0f, method = "BFGS",
                 hessian = TRUE, control = optim.control)
    if((code <- res$convergence) > 0)
        warning("possible convergence problem: optim gave code=", code)
    coef <- res$par

    if(transform.pars) {
        cf <- fixed
        cf[mask] <- coef
        ## do it this way to ensure hessian was computed inside
        ## stationarity region
        A <- .Call(R_Gradtrans, G, as.double(cf))[mask, mask]
        var <- t(A) %*% solve(res$hessian*length(x)) %*% A
        coef <- .Call(R_Dotrans, G, as.double(cf))[mask]
        .Call(R_set_trans, G, 0)
    } else var <- solve(res$hessian*length(x))
    arma0f(coef)  # reset pars
    sigma2 <- .Call(R_get_s2, G)
    resid <- .Call(R_get_resid, G)
    tsp(resid) <- xtsp
    class(resid) <- "ts"
    n.used <- sum(!is.na(resid))
    nm <- NULL
    if(arma[1L] > 0) nm <- c(nm, paste("ar", 1L:arma[1L], sep = ""))
    if(arma[2L] > 0) nm <- c(nm, paste("ma", 1L:arma[2L], sep = ""))
    if(arma[3L] > 0) nm <- c(nm, paste("sar", 1L:arma[3L], sep = ""))
    if(arma[4L] > 0) nm <- c(nm, paste("sma", 1L:arma[4L], sep = ""))
    fixed[mask] <- coef
    if(ncxreg > 0) {
        nm <- c(nm, cn)
        if(!orig.xreg) {
            ind <- narma + 1L:ncxreg
            fixed[ind] <- S$v %*% fixed[ind]
            A <- diag(narma + ncxreg)
            A[ind, ind] <- S$v
            A <- A[mask, mask]
            var <- A %*% var %*% t(A)
        }
    }
    names(fixed) <- nm
    names(arma) <- c("ar", "ma", "sar", "sma", "period", "diff", "sdiff")
    dimnames(var) <- list(nm[mask], nm[mask])
    value <- 2 * n.used * res$value + n.used + n.used*log(2*pi)
    aic <- if(method != "CSS") value + 2*length(coef) + 2 else NA
    res <- list(coef = fixed, sigma2 = sigma2, var.coef = var, mask = mask,
                loglik = -0.5*value, aic = aic, arma = arma,
                residuals = resid,
                call = match.call(), series = series,
                code = code, n.cond = ncond)
    class(res) <- "arima0"
    res
}

print.arima0 <- function(x, digits = max(3, getOption("digits") - 3),
                         se = TRUE, ...)
{
    cat("\nCall:", deparse(x$call, width.cutoff = 75L), "", sep = "\n")
    cat("Coefficients:\n")
    coef <- round(x$coef, digits = digits)
    if (se && nrow(x$var.coef)) {
        ses <- rep(0, length(coef))
        ses[x$mask] <- round(sqrt(diag(x$var.coef)), digits = digits)
        coef <- matrix(coef, 1, dimnames = list(NULL, names(coef)))
        coef <- rbind(coef, s.e. = ses)
    }
    print.default(coef, print.gap = 2)
    cm <- x$call$method
    if(is.null(cm) || cm != "CSS")
        cat("\nsigma^2 estimated as ",
            format(x$sigma2, digits = digits),
            ":  log likelihood = ", format(round(x$loglik,2)),
            ",  aic = ", format(round(x$aic,2)),
            "\n", sep="")
    else
        cat("\nsigma^2 estimated as ",
            format(x$sigma2, digits = digits),
            ":  part log likelihood = ", format(round(x$loglik,2)),
            "\n", sep="")
    invisible(x)
}

predict.arima0 <-
    function(object, n.ahead = 1, newxreg = NULL, se.fit=TRUE, ...)
{
    myNCOL <- function(x) if(is.null(x)) 0 else NCOL(x)
    data <- eval.parent(parse(text = object$series))
    xr <- object$call$xreg
    xreg <- if(!is.null(xr)) eval.parent(xr) else NULL
    ncxreg <- myNCOL(xreg)
    if(myNCOL(newxreg) != ncxreg)
        stop("'xreg' and 'newxreg' have different numbers of columns")
    class(xreg) <- NULL
    xtsp <- tsp(object$residuals)
    n <- length(data)
    arma <- object$arma
    coefs <- object$coef
    narma <- sum(arma[1L:4L])
    if(length(coefs) > narma) {
        if(names(coefs)[narma+1] == "intercept") {
            xreg <- cbind(intercept = rep(1, n), xreg)
            newxreg <- cbind(intercept = rep(1, n.ahead), newxreg)
            ncxreg <- ncxreg+1
        }
        data <- data - as.matrix(xreg) %*% coefs[-(1L:narma)]
        xm <- drop(as.matrix(newxreg) %*% coefs[-(1L:narma)])
    } else xm <- 0
    ## check invertibility of MA part(s)
    if(arma[2L] > 0) {
        ma <- coefs[arma[1L] + 1L:arma[2L]]
        if(any(Mod(polyroot(c(1, ma))) < 1))
            warning("MA part of model is not invertible")
    }
    if(arma[4L] > 0) {
        ma <- coefs[sum(arma[1L:3L]) + 1L:arma[4L]]
        if(any(Mod(polyroot(c(1, ma))) < 1))
            warning("seasonal MA part of model is not invertible")
    }
    storage.mode(data) <- "double"
    G <- .Call(R_setup_starma, as.integer(arma), data, n, rep(0., n),
               0., -1., 0., 0.)
    on.exit(.Call(R_free_starma, G))
    .Call(R_Starma_method, G, TRUE)
    .Call(R_arma0fa, G, as.double(coefs))
    z <- .Call(R_arma0_kfore, G, arma[6], arma[7], n.ahead)
    pred <- ts(z[[1L]] + xm, start = xtsp[2L] + deltat(data),
               frequency = xtsp[3L])
    if(se.fit) {
        se <- ts(sqrt(z[[2L]]),
                 start = xtsp[2L] + deltat(data), frequency = xtsp[3L])
        return(list(pred=pred, se=se))
    } else return(pred)
}

arima0.diag <- function(...) .Defunct()

tsdiag.Arima <- tsdiag.arima0 <- function(object, gof.lag = 10, ...)
{
    ## plot standardized residuals, acf of residuals, Ljung-Box p-values
    oldpar<- par(mfrow = c(3, 1))
    on.exit(par(oldpar))
    rs <- object$residuals
    stdres <- rs/sqrt(object$sigma2)
    plot(stdres, type = "h", main = "Standardized Residuals", ylab = "")
    abline(h = 0)
    acf(object$residuals, plot = TRUE, main = "ACF of Residuals",
        na.action = na.pass)
    nlag <- gof.lag
    pval <- numeric(nlag)
    for(i in 1L:nlag) pval[i] <- Box.test(rs, i, type="Ljung-Box")$p.value
    plot(1L:nlag, pval, xlab = "lag", ylab = "p value", ylim = c(0,1),
         main = "p values for Ljung-Box statistic")
    abline(h = 0.05, lty = 2, col = "blue")
}

tsdiag <- function(object, gof.lag, ...) UseMethod("tsdiag")
#  File src/library/stats/R/ave.R
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

ave <- function (x, ..., FUN = mean)
{
    n <- length(list(...))
    if (n) {
	g <- interaction(...)
	split(x,g) <- lapply(split(x, g), FUN)
    } else
        x[] <- FUN(x)
    x
}
#  File src/library/stats/R/bandwidths.R
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

### copyright (C) 1994-2001 W. N. Venables and B. D. Ripley
### This version distributed under GPL (version 2 or later)


#====           bandwidth selection rules              ====

bw.nrd0 <- function (x)
{
    if(length(x) < 2) stop("need at least 2 data points")
    hi <- sd(x)
    if(!(lo <- min(hi, IQR(x)/1.34)))# qnorm(.75) - qnorm(.25) = 1.34898
        (lo <- hi) || (lo <- abs(x[1L])) || (lo <- 1.)
    0.9 * lo * length(x)^(-0.2)
}

bw.nrd <- function (x)
{
    if(length(x) < 2) stop("need at least 2 data points")
    r <- quantile(x, c(0.25, 0.75))
    h <- (r[2L] - r[1L])/1.34
    1.06 * min(sqrt(var(x)), h) * length(x)^(-1/5)
}

bw.SJ <- function(x, nb = 1000, lower = 0.1*hmax, upper = hmax,
                  method = c("ste", "dpi"), tol = 0.1*lower)
{
    if((n <- length(x)) < 2) stop("need at least 2 data points")
    if(!is.numeric(x)) stop("invalid 'x'")
    storage.mode(x) <- "double"
    method <- match.arg(method)

    fSD <- function(h, x, alph2, c1, n, d)
        (c1/SDh(x, alph2 * h^(5/7), n, d))^(1/5) - h
    SDh <- function(x, h, n, d)
        .C(R_band_phi4_bin,
           as.integer(n),
           as.integer(length(x)),
           as.double(d),
           x,
           as.double(h),
           u = double(1))$u
    TDh <- function(x, h, n, d)
        .C(R_band_phi6_bin,
           as.integer(n),
           as.integer(length(x)),
           as.double(d),
           x,
           as.double(h),
           u = double(1))$u

    Z <- .C(R_band_den_bin,
            as.integer(n),
            as.integer(nb),
            d = double(1),
            x,
            cnt = integer(nb))
    d <- Z$d; cnt <- as.integer(Z$cnt)
    scale <- min(sd(x), IQR(x)/1.349)
    a <- 1.24 * scale * n^(-1/7)
    b <- 1.23 * scale * n^(-1/9)
    c1 <- 1/(2*sqrt(pi)*n)
    TD  <- -TDh(cnt, b, n, d)
    if(!is.finite(TD) || TD <= 0)
        stop("sample is too sparse to find TD")
    if(method == "dpi")
        res <- (c1/SDh(cnt, (2.394/(n * TD))^(1/7), n, d))^(1/5)
    else {
        if(bnd.Miss <- missing(lower) || missing(upper)) {
            ## only used for  lower & upper  defaults :
            hmax <- 1.144 * scale * n^(-1/5)
            ##              ^^^^^ used to be  sd(x)  which maybe way off
        }
        alph2 <- 1.357*(SDh(cnt, a, n, d)/TD)^(1/7)
        if(!is.finite(alph2))
            stop("sample is too sparse to find alph2")
        itry <- 1
	while (fSD(lower, cnt, alph2, c1, n, d) *
	       fSD(upper, cnt, alph2, c1, n, d) > 0) {
	    if(itry > 99 || !bnd.Miss) # 1.2 ^ 99 = 69'014'979 .. enough
		stop("no solution in the specified range of bandwidths")
	    if(itry %% 2)
		upper <- upper * 1.2
	    else lower <- lower / 1.2
	    if(getOption("verbose"))
		message(gettextf("increasing bw.SJ() search interval (%d) to [%.4g,%.4g]",
                        itry, lower, upper), sep='', domain = NA)
	    itry <- itry + 1
	}
        res <- uniroot(fSD, c(lower, upper), tol=tol,
                       x=cnt, alph2=alph2, c1=c1, n=n, d=d)$root
    }
    res
}


bw.ucv <- function(x, nb = 1000, lower = 0.1*hmax, upper = hmax,
                   tol = 0.1*lower)
{
    if((n <- length(x)) < 2) stop("need at least 2 data points")
    if(!is.numeric(x)) stop("invalid 'x'")

    fucv <- function(h, x, n, d)
        .C(R_band_ucv_bin,
           as.integer(n),
           as.integer(length(x)),
           as.double(d),
           x,
           as.double(h),
           u = double(1))$u

    hmax <- 1.144 * sqrt(var(x)) * n^(-1/5)
    storage.mode(x) <- "double"
    Z <- .C(R_band_den_bin,
            as.integer(n),
            as.integer(nb),
            d = double(1),
            x,
            cnt = integer(nb))
    d <- Z$d; cnt <- as.integer(Z$cnt)
    h <- optimize(fucv, c(lower, upper), tol=tol,
                  x=cnt, n=n, d=d)$minimum
    if(h < lower+tol | h > upper-tol)
        warning("minimum occurred at one end of the range")
    h
}

bw.bcv <- function(x, nb = 1000, lower = 0.1*hmax, upper = hmax,
                   tol = 0.1*lower)
{
    if((n <- length(x)) < 2) stop("need at least 2 data points")
    if(!is.numeric(x)) stop("invalid 'x'")

    fbcv <- function(h, x, n, d)
        .C(R_band_bcv_bin,
           as.integer(n),
           as.integer(length(x)),
           as.double(d),
           x,
           as.double(h),
           u = double(1))$u

    hmax <- 1.144 * sqrt(var(x)) * n^(-1/5)
    storage.mode(x) <- "double"
    Z <- .C(R_band_den_bin,
            as.integer(n),
            as.integer(nb),
            d = double(1),
            x,
            cnt = integer(nb))
    d <- Z$d; cnt <- as.integer(Z$cnt)
    h <- optimize(fbcv, c(lower, upper), tol=tol,
                  x=cnt, n=n, d=d)$minimum
    if(h < lower+tol | h > upper-tol)
        warning("minimum occurred at one end of the range")
    h
}
#  File src/library/stats/R/bartlett.test.R
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

bartlett.test <- function(x, ...) UseMethod("bartlett.test")

bartlett.test.default <-
function(x, g, ...)
{
    LM <- FALSE
    if (is.list(x)) {
        if (length(x) < 2)
            stop("'x' must be a list with at least 2 elements")
        DNAME <- deparse(substitute(x))
        if (all(sapply(x, function(obj) inherits(obj, "lm"))))
            LM <- TRUE
        else
            x <- lapply(x, function(x) x <- x[is.finite(x)])
        k <- length(x)
    }
    else {
        if (length(x) != length(g))
            stop("'x' and 'g' must have the same length")
        DNAME <- paste(deparse(substitute(x)), "and",
                       deparse(substitute(g)))
        OK <- complete.cases(x, g)
        x <- x[OK]
        g <- factor(g[OK])
        k <- nlevels(g)
        if (k < 2)
            stop("all observations are in the same group")
        x <- split(x, g)
    }

    if (LM) {
        n <- sapply(x, function(obj) obj$df.resid)
        v <- sapply(x, function(obj) sum(obj$residuals^2))
    } else {
        n <- sapply(x, "length") - 1
        if (any(n <= 0))
            stop("there must be at least 2 observations in each group")
        v <- sapply(x, "var")
    }

    n.total <- sum(n)
    v.total <- sum(n * v) / n.total
    STATISTIC <- ((n.total * log(v.total) - sum(n * log(v))) /
                  (1 + (sum(1 / n) - 1 / n.total) / (3 * (k - 1))))
    PARAMETER <- k - 1
    PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
    names(STATISTIC) <- "Bartlett's K-squared"
    names(PARAMETER) <- "df"

    RVAL <- list(statistic = STATISTIC,
                 parameter = PARAMETER,
                 p.value = PVAL,
                 data.name = DNAME,
                 method = "Bartlett test of homogeneity of variances")
    class(RVAL) <- "htest"
    return(RVAL)
}

bartlett.test.formula <-
function(formula, data, subset, na.action, ...)
{
    if(missing(formula) || (length(formula) != 3))
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m[[1L]] <- as.name("model.frame")
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " by ")
    names(mf) <- NULL
    y <- do.call("bartlett.test", as.list(mf))
    y$data.name <- DNAME
    y
}
#  File src/library/stats/R/binom.test.R
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

binom.test <-
function(x, n, p = 0.5, alternative = c("two.sided", "less", "greater"),
         conf.level = 0.95)
{
    DNAME <- deparse(substitute(x))
    xr <- round(x)

    if(any(is.na(x) | (x < 0)) || max(abs(x-xr)) > 1e-7)
        stop("'x' must be nonnegative and integer")
    x <- xr
    if(length(x) == 2) {
        ## x gives successes and failures
        n <- sum(x)
        x <- x[1L]
    }
    else if(length(x) == 1) {
        ## x gives successes, n gives trials
        nr <- round(n)
        if((length(n) > 1) || is.na(n) || (n < 1) || abs(n-nr) > 1e-7
           || (x > nr))
            stop("'n' must be a positive integer >= 'x'")
        DNAME <- paste(DNAME, "and", deparse(substitute(n)))
        n <- nr
    }
    else
        stop("incorrect length of 'x'")

    if(!missing(p) && (length(p) > 1 || is.na(p) || p < 0 || p > 1))
        stop ("'p' must be a single number between 0 and 1")
    alternative <- match.arg(alternative)

    if(!((length(conf.level) == 1) && is.finite(conf.level) &&
         (conf.level > 0) && (conf.level < 1)))
        stop("'conf.level' must be a single number between 0 and 1")

    PVAL <- switch(alternative,
                   less = pbinom(x, n, p),
                   greater = pbinom(x - 1, n, p, lower.tail = FALSE),
                   two.sided = {
                       if(p == 0)
                           (x == 0)
                       else if(p == 1)
                           (x == n)
                       else {
                           ## Do
                           ##   d <- dbinom(0 : n, n, p)
                           ##   sum(d[d <= dbinom(x, n, p)])
                           ## a bit more efficiently ...
                           ## Note that we need a little fuzz.
                           relErr <- 1 + 1e-7
                           d <- dbinom(x, n, p)
			   ## This is tricky: need to be sure
			   ## only to sum values in opposite tail
			   ## and not count x twice.
			   ## For the binomial dist., the mode will
			   ## equal the mean if it is an integer.
			   m <- n * p
			   if (x == m)
			   	1
                           else if (x < m) {
                               i <- seq.int(from = ceiling(m), to = n)
                               y <- sum(dbinom(i, n, p) <= d * relErr)
                               pbinom(x, n, p) +
                                   pbinom(n - y, n, p, lower.tail = FALSE)
                           } else {
                               i <- seq.int(from = 0, to = floor(m))
                               y <- sum(dbinom(i, n, p) <= d * relErr)
                               pbinom(y - 1, n, p) +
                                   pbinom(x - 1, n, p, lower.tail = FALSE)
                           }
                       }
                   })
    ## Determine p s.t. Prob(B(n,p) >= x) = alpha.
    ## Use that for x > 0,
    ##   Prob(B(n,p) >= x) = pbeta(p, x, n - x + 1).
    p.L <- function(x, alpha) {
        if(x == 0)                      # No solution
            0
        else
            qbeta(alpha, x, n - x + 1)
    }
    ## Determine p s.t. Prob(B(n,p) <= x) = alpha.
    ## Use that for x < n,
    ##   Prob(B(n,p) <= x) = 1 - pbeta(p, x + 1, n - x).
    p.U <- function(x, alpha) {
        if(x == n)                      # No solution
            1
        else
            qbeta(1 - alpha, x + 1, n - x)
    }
    CINT <- switch(alternative,
                   less = c(0, p.U(x, 1 - conf.level)),
                   greater = c(p.L(x, 1 - conf.level), 1),
                   two.sided = {
                       alpha <- (1 - conf.level) / 2
                       c(p.L(x, alpha), p.U(x, alpha))
                   })
    attr(CINT, "conf.level") <- conf.level

    ESTIMATE <- x / n

    names(x) <- "number of successes"	# or simply "x" ??
    names(n) <- "number of trials"	# or simply "n" ??
    names(ESTIMATE) <-
    names(p) <- "probability of success"# or simply "p" ??

    structure(list(statistic = x,
                   parameter = n,
                   p.value = PVAL,
                   conf.int = CINT,
                   estimate = ESTIMATE,
                   null.value = p,
                   alternative = alternative,
                   method = "Exact binomial test",
                   data.name = DNAME),
              class = "htest")
}
#  File src/library/stats/R/biplot.R
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

biplot <- function(x, ...) UseMethod("biplot")

biplot.default <-
    function(x, y, var.axes = TRUE, col, cex = rep(par("cex"), 2),
	     xlabs = NULL, ylabs = NULL, expand=1, xlim = NULL, ylim = NULL,
	     arrow.len = 0.1,
             main = NULL, sub = NULL, xlab = NULL, ylab = NULL, ...)
{
    n <- nrow(x)
    p <- nrow(y)
    if(missing(xlabs)) {
	xlabs <- dimnames(x)[[1L]]
	if(is.null(xlabs)) xlabs <- 1L:n
    }
    xlabs <- as.character(xlabs)
    dimnames(x) <- list(xlabs, dimnames(x)[[2L]])
    if(missing(ylabs)) {
	ylabs <- dimnames(y)[[1L]]
	if(is.null(ylabs)) ylabs <- paste("Var", 1L:p)
    }
    ylabs <- as.character(ylabs)
    dimnames(y) <- list(ylabs, dimnames(y)[[2L]])

    if(length(cex) == 1L) cex <- c(cex, cex)
    if(missing(col)) {
	col <- par("col")
	if (!is.numeric(col)) col <- match(col, palette(), nomatch=1L)
	col <- c(col, col + 1L)
    }
    else if(length(col) == 1L) col <- c(col, col)

    unsigned.range <- function(x)
        c(-abs(min(x, na.rm=TRUE)), abs(max(x, na.rm=TRUE)))
    rangx1 <- unsigned.range(x[, 1L])
    rangx2 <- unsigned.range(x[, 2L])
    rangy1 <- unsigned.range(y[, 1L])
    rangy2 <- unsigned.range(y[, 2L])

    if(missing(xlim) && missing(ylim))
	xlim <- ylim <- rangx1 <- rangx2 <- range(rangx1, rangx2)
    else if(missing(xlim)) xlim <- rangx1
    else if(missing(ylim)) ylim <- rangx2
    ratio <- max(rangy1/rangx1, rangy2/rangx2)/expand
    on.exit(par(op))
    op <- par(pty = "s")
    if(!is.null(main))
        op <- c(op, par(mar = par("mar")+c(0,0,1,0)))
    plot(x, type = "n", xlim = xlim, ylim = ylim, col = col[1L],
         xlab = xlab, ylab = ylab, sub = sub, main = main, ...)
    text(x, xlabs, cex = cex[1L], col = col[1L], ...)
    par(new = TRUE)
    plot(y, axes = FALSE, type = "n", xlim = xlim*ratio, ylim = ylim*ratio,
	 xlab = "", ylab = "", col = col[1L], ...)
    axis(3, col = col[2L], ...)
    axis(4, col = col[2L], ...)
    box(col = col[1L])
    text(y, labels=ylabs, cex = cex[2L], col = col[2L], ...)
    if(var.axes)
	arrows(0, 0, y[,1L] * 0.8, y[,2L] * 0.8, col = col[2L], length=arrow.len)
    invisible()
}

biplot.princomp <- function(x, choices = 1L:2L, scale = 1, pc.biplot=FALSE, ...)
{
    if(length(choices) != 2) stop("length of choices must be 2")
    if(!length(scores <- x$scores))
	stop(gettextf("object '%s' has no scores", deparse(substitute(x))),
             domain = NA)
    lam <- x$sdev[choices]
    if(is.null(n <- x$n.obs)) n <- 1
    lam <- lam * sqrt(n)
    if(scale < 0 || scale > 1) warning("'scale' is outside [0, 1]")
    if(scale != 0) lam <- lam^scale else lam <- 1
    if(pc.biplot) lam <- lam / sqrt(n)
    biplot.default(t(t(scores[, choices]) / lam),
		   t(t(x$loadings[, choices]) * lam), ...)
    invisible()
}

biplot.prcomp <- function(x, choices = 1L:2L, scale = 1, pc.biplot=FALSE, ...)
{
    if(length(choices) != 2) stop("length of choices must be 2")
    if(!length(scores <- x$x))
	stop(gettextf("object '%s' has no scores", deparse(substitute(x))),
             domain = NA)
    if(is.complex(scores))
        stop("biplots are not defined for complex PCA")
    lam <- x$sdev[choices]
    n <- NROW(scores)
    lam <- lam * sqrt(n)
    if(scale < 0 || scale > 1) warning("'scale' is outside [0, 1]")
    if(scale != 0) lam <- lam^scale else lam <- 1
    if(pc.biplot) lam <- lam / sqrt(n)
    biplot.default(t(t(scores[, choices]) / lam),
		   t(t(x$rotation[, choices]) * lam), ...)
    invisible()
}
#  File src/library/stats/R/birthday.R
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


qbirthday <- function(prob = 0.5, classes = 365, coincident = 2)
{
    k <- coincident
    c <- classes
    p <- prob
    if (p <= 0) return(1)
    if (p >= 1) return(c*(k-1)+1)
    if ((k-1)*log(c) > 8 ||  1 - p < 1e-7) {
        lnN <- ((k-1)*log(c) + lgamma(k+1) + log(-log1p(-p)))/k
        N <- exp(lnN)
    } else {
        N <- (c^(k-1) * gamma(k+1) * log(1/(1-p)))^(1/k)
    }
    round(N)
}

pbirthday <- function(n, classes = 365, coincident = 2)
{
    k <- coincident
    c <- classes
    if (k < 2) return(1)
    if (k > n) return(0)
    if (n > c*(k-1)) return(1)
    eps <- 1e-14
    if (qbirthday(1-eps, c, k) <= n)
	return(1-eps)
    f1 <- function(p) qbirthday(p,c,k) - n
    upper <- min(1, exp(k * log(n) - (k-1) * log(c)), na.rm = TRUE)
    nmin <- uniroot(f1, lower = 0, upper = upper, tol = eps)
    if(nmin$root == 0 && f1(.Machine$double.xmin) < 0) {
	## try again -- on log scale --
	f2 <- function(ln.p) qbirthday(exp(ln.p), c, k) - n
	nmin <- uniroot(f2, lower= floor(log(.Machine$double.xmin)),
			upper = -2, tol = eps)
	exp(nmin$root)
    }
    else
	nmin$root
}

#  File src/library/stats/R/cancor.R
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

## Seber pages 506-507, after a Golub original

cancor <- function(x, y, xcenter=TRUE, ycenter=TRUE)
{
    x <- as.matrix(x)
    y <- as.matrix(y)
    if((nr <- nrow(x)) != nrow(y)) stop("unequal number of rows in 'cancor'")
    ncx <- ncol(x)
    ncy <- ncol(y)
    if(!nr || !ncx || !ncy) stop("dimension 0 in 'x' or 'y'")
    if(is.logical(xcenter)) {
	if(xcenter) {
	    xcenter <- colMeans(x,)
	    x <- x - rep(xcenter, rep.int(nr, ncx))
	}
	else xcenter <- rep.int(0, ncx)
    }
    else {
	xcenter <- rep(xcenter, length.out = ncx)
	x <- x - rep(xcenter, rep.int(nr, ncx))
    }
    if(is.logical(ycenter)) {
	if(ycenter) {
	    ycenter <- colMeans(y)
	    y <- y - rep(ycenter, rep.int(nr, ncy))
	}
	else ycenter <- rep.int(0, ncy)
    }
    else {
	ycenter <- rep(ycenter, length.out = ncy)
	y <- y - rep(ycenter, rep.int(nr,ncy))
    }
    qx <- qr(x)
    qy <- qr(y)
    dx <- qx$rank;	if(!dx) stop("'x' has rank 0")
    dy <- qy$rank;	if(!dy) stop("'y' has rank 0")
    ## compute svd(Qx'Qy)
    z <- svd(qr.qty(qx, qr.qy(qy, diag(1, nr, dy)))[1L:dx,, drop = FALSE],
             dx, dy)
    xcoef <- backsolve((qx$qr)[1L:dx, 1L:dx, drop = FALSE], z$u)
    rownames(xcoef) <- colnames(x)[qx$pivot][1L:dx]
    ycoef <-  backsolve((qy$qr)[1L:dy, 1L:dy, drop = FALSE], z$v)
    rownames(ycoef) <- colnames(y)[qy$pivot][1L:dy]
    list(cor = z$d, xcoef = xcoef, ycoef = ycoef, xcenter = xcenter,
	 ycenter = ycenter)
}
#  File src/library/stats/R/chisq.test.R
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

chisq.test <- function(x, y = NULL, correct = TRUE,
		       p = rep(1/length(x), length(x)),
		       rescale.p = FALSE, simulate.p.value = FALSE, B = 2000)
{
    DNAME <- deparse(substitute(x))
    if (is.data.frame(x))
	x <- as.matrix(x)
    if (is.matrix(x)) {
	if (min(dim(x)) == 1)
	    x <- as.vector(x)
    }
    if (!is.matrix(x) && !is.null(y)) {
	if (length(x) != length(y))
	    stop("'x' and 'y' must have the same length")
        DNAME2 <- deparse(substitute(y))
        ## omit names on dims if too long (and 1 line might already too long)
        xname <- if(length(DNAME) > 1L || nchar(DNAME, "w") > 30) "" else DNAME
        yname <- if(length(DNAME2) > 1L || nchar(DNAME2, "w") > 30) "" else DNAME2
	OK <- complete.cases(x, y)
	x <- factor(x[OK])
	y <- factor(y[OK])
	if ((nlevels(x) < 2) || (nlevels(y) < 2))
	    stop("'x' and 'y' must have at least 2 levels")
	## Could also call table() with 'deparse.level = 2', but we need
	## to deparse ourselves for DNAME anyway ...
	x <- table(x, y)
	names(dimnames(x)) <- c(xname, yname)
        ## unclear what to do here: might abbreviating be preferable?
	DNAME <- paste(paste(DNAME, collapse = "\n"),
                       "and",
                       paste(DNAME2, collapse = "\n"))
    }

    if (any(x < 0) || any(is.na(x)))
	stop("all entries of 'x' must be nonnegative and finite")
    if ((n <- sum(x)) == 0)
	stop("at least one entry of 'x' must be positive")

    if(simulate.p.value) {
	setMETH <- function() # you shalt not cut_n_paste
	    METHOD <<- paste(METHOD,
			     "with simulated p-value\n\t (based on", B,
			     "replicates)")
        almost.1 <- 1 - 64 * .Machine$double.eps
    }
    if (is.matrix(x)) {
	METHOD <- "Pearson's Chi-squared test"
	nr <- nrow(x)
	nc <- ncol(x)
	sr <- rowSums(x)
	sc <- colSums(x)
	E <- outer(sr, sc, "*") / n
	dimnames(E) <- dimnames(x)
	if (simulate.p.value && all(sr > 0) && all(sc > 0)) {
	    setMETH()
	    tmp <- .C(R_chisqsim,
		      as.integer(nr),
		      as.integer(nc),
		      as.integer(sr),
		      as.integer(sc),
		      as.integer(n),
		      as.integer(B),
		      as.double(E),
		      integer(nr * nc),
		      double(n + 1),
		      integer(nc),
		      results = double(B))
	    ## Sorting before summing may look strange, but seems to be
	    ## a sensible way to deal with rounding issues (PR#3486):
	    STATISTIC <- sum(sort((x - E) ^ 2 / E, decreasing = TRUE))
	    PARAMETER <- NA
	    ## use correct significance level for a Monte Carlo test
	    PVAL <- (1 + sum(tmp$results >= almost.1 * STATISTIC)) / (B + 1)
	}
	else {
	    if (simulate.p.value)
		warning("cannot compute simulated p-value with zero marginals")
	    if (correct && nrow(x) == 2 && ncol(x) == 2) {
		YATES <- 0.5
		METHOD <- paste(METHOD, "with Yates' continuity correction")
	    }
	    else
		YATES <- 0
	    STATISTIC <- sum((abs(x - E) - YATES)^2 / E)
	    PARAMETER <- (nr - 1) * (nc - 1)
	    PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
	}
    }
    else {
	if (length(x) == 1)
	    stop("'x' must at least have 2 elements")
	if (length(x) != length(p))
	    stop("'x' and 'p' must have the same number of elements")
	if(any(p < 0)) stop("probabilities must be non-negative.")
	if(abs(sum(p)-1) > sqrt(.Machine$double.eps)) {
	    if(rescale.p) p <- p/sum(p)
	    else stop("probabilities must sum to 1.")
	}
	METHOD <- "Chi-squared test for given probabilities"
	E <- n * p
	names(E) <- names(x)
	STATISTIC <- sum((x - E) ^ 2 / E)
	if(simulate.p.value) {
	    setMETH()
	    nx <- length(x)
	    sm <- matrix(sample.int(nx, B*n, TRUE, prob = p),nrow = n)
	    ss <- apply(sm, 2L, function(x,E,k) {
		sum((table(factor(x, levels=1L:k)) - E)^2 / E)
	    }, E = E, k = nx)
	    PARAMETER <- NA
	    PVAL <- (1 + sum(ss >= almost.1 * STATISTIC))/(B + 1)
	} else {
	    PARAMETER <- length(x) - 1
	    PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
	}
    }

    names(STATISTIC) <- "X-squared"
    names(PARAMETER) <- "df"
    if (any(E < 5) && is.finite(PARAMETER))
	warning("Chi-squared approximation may be incorrect")

    structure(list(statistic = STATISTIC,
		   parameter = PARAMETER,
		   p.value = PVAL,
		   method = METHOD,
		   data.name = DNAME,
		   observed = x,
		   expected = E,
		   residuals = (x - E) / sqrt(E)),
	      class = "htest")
}
#  File src/library/stats/R/cmdscale.R
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

cmdscale <- function (d, k = 2, eig = FALSE, add = FALSE, x.ret = FALSE)
{
    if (any(is.na(d)))
	stop("NA values not allowed in 'd'")
    if (is.null(n <- attr(d, "Size"))) {
        if(add) d <- as.matrix(d)
	x <- as.matrix(d^2)
	if ((n <- nrow(x)) != ncol(x))
	    stop("distances must be result of 'dist' or a square matrix")
        rn <- rownames(x)
    } else {
	x <- matrix(0, n, n)
        if(add) d0 <- x
	x[row(x) > col(x)] <- d^2
	x <- x + t(x)
        if(add) {
            d0[row(x) > col(x)] <- d
            d <- d0 + t(d0)
        }
        rn <- attr(d, "Labels")
    }
    if((k <- as.integer(k)) > n - 1 || k < 1)
        stop("'k' must be in {1, 2, ..  n - 1}")
    storage.mode(x) <- "double"
    ## doubly center x in-place
    .C(R_dblcen, x, as.integer(n), DUP = FALSE)

    if(add) { ## solve the additive constant problem
        ## it is c* = largest eigenvalue of 2 x 2 (n x n) block matrix Z:
        i2 <- n + (i <- 1L:n)
        Z <- matrix(0, 2L*n, 2L*n)
        Z[cbind(i2,i)] <- -1
        Z[ i, i2] <- -x
        Z[i2, i2] <- .C(R_dblcen, x = 2*d, as.integer(n))$x
        e <- eigen(Z, symmetric = FALSE, only.values = TRUE)$values
        add.c <- max(Re(e))
        ## and construct a new x[,] matrix:
	x <- matrix(double(n*n), n, n)
        non.diag <- row(d) != col(d)
        x[non.diag] <- (d[non.diag] + add.c)^2
    }
    e <- eigen(-x/2, symmetric = TRUE)
    ev <- e$values[1L:k]
    if(any(ev < 0))
        warning(gettextf("some of the first %d eigenvalues are < 0", k),
                domain = NA)
    points <- e$vectors[, 1L:k, drop = FALSE] %*% diag(sqrt(ev), k)
    dimnames(points) <- list(rn, NULL)
    if (eig || x.ret || add) {
        evalus <- e$values[-n]
        list(points = points, eig = if(eig) ev, x = if(x.ret) x,
             ac = if(add) add.c else 0,
             GOF = sum(ev)/c(sum(abs(evalus)), sum(evalus[evalus > 0])))
    } else points
}
#  File src/library/stats/R/complete.cases.R
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

complete.cases <- function(...) .Internal(complete.cases(...))
#  File src/library/stats/R/confint.R
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

confint <- function(object, parm, level = 0.95, ...) UseMethod("confint")

format.perc <- function(probs, digits)
    ## Not yet exported, maybe useful in other contexts:
    ## quantile.default() sometimes uses a version of it
    paste(format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits),
	  "%")

confint.lm <- function(object, parm, level = 0.95, ...)
{
    cf <- coef(object)
    pnames <- names(cf)
    if(missing(parm)) parm <- pnames
    else if(is.numeric(parm)) parm <- pnames[parm]
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    fac <- qt(a, object$df.residual) # difference from default method
    pct <- format.perc(a, 3)
    ci <- array(NA, dim = c(length(parm), 2L),
		dimnames = list(parm, pct))
    ses <- sqrt(diag(vcov(object)))[parm] # gives NA for aliased parms
    ci[] <- cf[parm] + ses %o% fac
    ci
}


confint.glm <- function(object, parm, level = 0.95, ...)
    try(MASS:::confint.glm(object, parm, level, ...))

confint.nls <- function(object, parm, level = 0.95, ...)
    try(MASS:::confint.nls(object, parm, level, ...))

confint.default <- function (object, parm, level = 0.95, ...)
{
    cf <- coef(object)
    pnames <- names(cf)
    if(missing(parm)) parm <- pnames
    else if(is.numeric(parm)) parm <- pnames[parm]
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    pct <- format.perc(a, 3)
    fac <- qnorm(a)
    ci <- array(NA, dim = c(length(parm), 2L),
		dimnames = list(parm, pct))
    ses <- sqrt(diag(vcov(object)))[parm]
    ci[] <- cf[parm] + ses %o% fac
    ci
}
#  File src/library/stats/R/constrOptim.R
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


constrOptim<-function(theta,f,grad,ui,ci,mu=0.0001,control=list(),
                  method=if(is.null(grad)) "Nelder-Mead" else "BFGS",
                  outer.iterations=100,outer.eps=0.00001,...){

    if (!is.null(control$fnscale) && control$fnscale<0)
      mu<- -mu ##maximizing
  
    R<-function(theta,theta.old,...){
        ui.theta<-ui%*%theta
        gi<- ui.theta-ci
        if (any(gi<0)) return(NaN)
        gi.old<-ui%*%theta.old-ci
        bar<-sum( gi.old*log(gi)-ui.theta)
        if (!is.finite(bar)) bar<- -Inf
        f(theta,...)-mu*bar
    }
 
    dR<-function(theta,theta.old,...){
        ui.theta<-ui%*%theta
        gi<-drop(ui.theta-ci)
        gi.old<-drop(ui%*%theta.old-ci)
        dbar<-colSums( ui*gi.old/gi-ui)
        grad(theta,...)-mu*dbar
    }

    if (any(ui%*%theta-ci<=0))
        stop("initial value not feasible")
    obj<-f(theta,...)
    r<-R(theta,theta,...)
    for(i in 1L:outer.iterations){
        obj.old<-obj
        r.old<-r
        theta.old<-theta
        fun<-function(theta,...){ R(theta,theta.old,...)}
        
        gradient<-function(theta,...) { dR(theta,theta.old,...)}
        a<-optim(theta.old, fun, gradient, control=control,method=method,...)
        r<-a$value
        if (is.finite(r) && is.finite(r.old) && abs(r-r.old)/(outer.eps+abs(r-r.old))<outer.eps)
            break
        theta<-a$par
        obj<-f(theta,...)
        if (obj>obj.old) break
    }
    if (i==outer.iterations){
        a$convergence<-7
        a$message<-"Barrier algorithm ran out of iterations and did not converge"
    }
    if (mu>0 && obj>obj.old){
        a$convergence<-11
        a$message<-paste("Objective function increased at outer iteration",i)
    }
    if (mu<0 && obj<obj.old){
        a$convergence<-11
        a$message<-paste("Objective function decreased at outer iteration",i)
    }

        
    a$outer.iterations<-i
    a$barrier.value<-a$value
    a$value<-f(a$par,...)
    a$barrier.value<-a$barrier.value-a$value
    a
    
}
#  File src/library/stats/R/contr.poly.R
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

contr.poly <- function (n, scores = 1:n, contrasts = TRUE, sparse = FALSE)
{
## sparse.model.matrix() may call this one with sparse=TRUE anyway ..
##     if(sparse)
## 	stop("orthogonal polynomial contrasts cannot be sparse")
    make.poly <- function(n, scores)
    {
	y <- scores - mean(scores)
	X <- outer(y, seq_len(n) - 1, "^")
	QR <- qr(X)
	z <- QR$qr
	z <- z *(row(z) == col(z))
	raw <- qr.qy(QR, z)
	Z <- sweep(raw, 2L, apply(raw, 2L, function(x) sqrt(sum(x^2))), "/",
		   check.margin=FALSE)
	colnames(Z) <- paste("^", 1L:n - 1L, sep="")
	Z
    }

    if (is.numeric(n) && length(n) == 1) levs <- seq_len(n)
    else {
	levs <- n
	n <- length(levs)
    }
    if (n < 2)
        stop(gettextf("contrasts not defined for %d degrees of freedom",
                      n - 1), domain = NA)
    if (n > 95)
        stop(gettextf("orthogonal polynomials cannot be represented accurately enough for %d degrees of freedom", n-1), domain = NA)
    if (length(scores) != n)
        stop("'scores' argument is of the wrong length")
    if (!is.numeric(scores) || anyDuplicated(scores))
        stop("'scores' must all be different numbers")
    contr <- make.poly(n, scores)
    if(sparse) contr <- .asSparse(contr)
    if (contrasts) {
	dn <- colnames(contr)
	dn[2:min(4,n)] <- c(".L", ".Q", ".C")[1:min(3, n-1)]
	colnames(contr) <- dn
	contr[, -1, drop = FALSE]
    }
    else {
	contr[, 1] <- 1
	contr
    }
}

poly <- function(x, ..., degree = 1, coefs = NULL, raw = FALSE)
{
    dots <- list(...)
    if(nd <- length(dots)) {
        if(nd == 1 && length(dots[[1L]]) == 1) # unnamed degree
            degree <- dots[[1L]]
        else return(polym(x, ..., degree = degree, raw = raw))
    }
    if(is.matrix(x)) {
        m <- unclass(as.data.frame(cbind(x, ...)))
        return(do.call("polym", c(m, degree = degree, raw = raw)))
    }
    if(degree < 1)
        stop("'degree' must be at least 1")
    if(any(is.na(x))) stop("missing values are not allowed in 'poly'")
    n <- degree + 1
    if(raw) {
        if(degree >= length(unique(x)))
            stop("'degree' must be less than number of unique points")
        Z <- outer(x, 1L:degree, "^")
        colnames(Z) <- 1L:degree
        attr(Z, "degree") <- 1L:degree
        class(Z) <- c("poly", "matrix")
        return(Z)
    }
    if(is.null(coefs)) { # fitting
        if(degree >= length(unique(x)))
            stop("'degree' must be less than number of unique points")
        xbar <- mean(x)
        x <- x - xbar
        X <- outer(x, seq_len(n) - 1, "^")
        QR <- qr(X)
        if(QR$rank < degree)
            stop("'degree' must be less than number of unique points")
        z <- QR$qr
        z <- z * (row(z) == col(z))
        raw <- qr.qy(QR, z)
        norm2 <- colSums(raw^2)
        alpha <- (colSums(x*raw^2)/norm2 + xbar)[1L:degree]
        Z <- raw / rep(sqrt(norm2), each = length(x))
        colnames(Z) <- 1L:n - 1L
        Z <- Z[, -1, drop = FALSE]
        attr(Z, "degree") <- 1L:degree
        attr(Z, "coefs") <- list(alpha = alpha, norm2 = c(1, norm2))
        class(Z) <- c("poly", "matrix")
    } else {            # prediction
        alpha <- coefs$alpha; norm2 <- coefs$norm2
        Z <- matrix(, length(x), n)
        Z[, 1] <- 1
        Z[, 2] <- x - alpha[1L]
        if(degree > 1)
            for(i in 2:degree)
                Z[, i+1] <- (x - alpha[i]) * Z[, i]  -
                    (norm2[i+1] / norm2[i]) * Z[, i-1]
        Z <- Z / rep(sqrt(norm2[-1L]), each = length(x))
        colnames(Z) <- 0:degree
        Z <- Z[, -1, drop = FALSE]
        ## we may want to use the prediction to clone another prediction
        attr(Z, "degree") <- 1L:degree
        attr(Z, "coefs") <- list(alpha = alpha, norm2 = norm2)
        class(Z) <- c("poly", "matrix")
    }
    Z
}

predict.poly <- function(object, newdata, ...)
{
    if(missing(newdata)) return(object)
    if(is.null(attr(object, "coefs")))
       poly(newdata, degree = max(attr(object, "degree")), raw = TRUE)
    else
       poly(newdata, degree = max(attr(object, "degree")),
            coefs = attr(object, "coefs"))
}

makepredictcall.poly  <- function(var, call)
{
    if(as.character(call)[1L] != "poly") return(call)
    call$coefs <- attr(var, "coefs")
    call
}

polym <- function(..., degree = 1, raw = FALSE)
{
    dots <- list(...)
    nd <- length(dots)
    if(nd == 0) stop("must supply one or more vectors")
    if(nd == 1) return(poly(dots[[1L]], degree, raw = raw))
    n <- sapply(dots, length)
    if(any(n != n[1L]))
        stop("arguments must have the same length")
    z <- do.call("expand.grid", rep.int(list(0:degree), nd))
    s <- rowSums(z)
    ind <- (s > 0) & (s <= degree)
    z <- z[ind, ]; s <- s[ind]
    res <- cbind(1, poly(dots[[1L]], degree, raw = raw))[, 1 + z[, 1]]
    for(i in 2:nd)
        res <- res * cbind(1, poly(dots[[i]], degree, raw = raw))[, 1 + z[, i]]
    colnames(res) <- apply(z, 1L, function(x) paste(x, collapse = "."))
    attr(res, "degree") <- as.vector(s)
    res
}
#  File src/library/stats/R/contrast.R
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


## This is also called from C : do_model_matrix() { ../../../main/model.c }:
contrasts <- function (x, contrasts = TRUE, sparse = FALSE)
{
    if (is.logical(x)) x <- factor(x, levels=c(FALSE, TRUE))
    if (!is.factor(x))
	stop("contrasts apply only to factors")
    if(!contrasts)
        return(.Diag(levels(x), sparse=sparse))

    ctr <- attr(x, "contrasts")
    if ((NL <- is.null(ctr)) || is.character(ctr)) {
	if(NL) ctr <- getOption("contrasts")[[if (is.ordered(x)) 2L else 1L]]
	ctrfn <- get(ctr, mode="function", envir=parent.frame())
	if(useSparse <- isTRUE(sparse)) {
	    if(!(useSparse <- any("sparse" == names(formals(ctrfn)))))
		warning(sprintf(
		"contrast function '%s' does not support 'sparse = TRUE'",
				ctr), domain = NA)
	}
        ctr <- if(useSparse)
            ctrfn(levels(x), contrasts = contrasts, sparse = sparse)
        else ctrfn(levels(x), contrasts = contrasts)
    }
    ctr
}

`contrasts<-` <- function(x, how.many, value)
{
    if (is.logical(x)) x <- factor(x, levels=c(FALSE, TRUE))
    if(!is.factor(x))
	stop("contrasts apply only to factors")
    if(nlevels(x) < 2L)
        stop("contrasts can be applied only to factors with 2 or more levels")
    if(is.function(value)) value <- value(nlevels(x))
    if((is.n <- is.numeric(value)) || is(value, "Matrix")) {
	## also work for "sparseMatrix"
	if(is.n) value <- as.matrix(value)
	nlevs <- nlevels(x)
	if(nrow(value) != nlevs)
	    stop("wrong number of contrast matrix rows")
	n1 <- if(missing(how.many)) nlevs - 1L else how.many
	nc <- ncol(value)
	rownames(value) <- levels(x)
	if(nc < n1) {
	    if(!is.n) value <- as.matrix(value) ## for now use traditional qr():
	    cm <- qr(cbind(1,value))
	    if(cm$rank != nc+1) stop("singular contrast matrix")
	    cm <- qr.qy(cm, diag(nlevs))[, 2L:nlevs]
	    cm[,1L:nc] <- value
	    dimnames(cm) <- list(levels(x),NULL)
	    if(!is.null(nmcol <- dimnames(value)[[2L]]))
		dimnames(cm)[[2L]] <- c(nmcol, rep.int("", n1-nc))
	} else cm <- value[, 1L:n1, drop=FALSE]
    }
    else if(is.character(value)) cm <- value
    else if(is.null(value)) cm <- NULL
    else stop("numeric contrasts or contrast name expected")
    attr(x, "contrasts") <- cm
    x
}


## a fast version of diag(n, .) / sparse-Diagonal() + dimnames
.Diag <- function(nms, sparse) {
    ## no error checking here
    n <- as.integer(length(nms))
    d <- c(n,n)
    dn <- list(nms, nms)
    if(sparse) {
	if(is.null(tryCatch(loadNamespace("Matrix"), error= function(e)NULL)))
	    stop("contr*(.., sparse=TRUE) needs package \"Matrix\" correctly installed")
	new("ddiMatrix", diag = "U", Dim = d, Dimnames = dn)
    } else
	array(c(rep.int(c(1, numeric(n)), n-1L), 1), d, dn)
}

.asSparse <- function(m) {
    ## ensure helpful error message when Matrix is missing:
    if(is.null(tryCatch(loadNamespace("Matrix"), error= function(e)NULL)))
	stop("contr*(.., sparse=TRUE) needs package \"Matrix\" correctly installed")
    as(m, "sparseMatrix")
}

## contr.poly() is in ./contr.poly.R

contr.helmert <-
    function (n, contrasts = TRUE, sparse = FALSE)
{
    if (length(n) <= 1L) {
	if(is.numeric(n) && length(n) == 1L && n > 1L) levels <- seq_len(n)
	else stop("not enough degrees of freedom to define contrasts")
    } else levels <- n
    levels <- as.character(levels)

    if (contrasts) {
        n <- length(levels)
	cont <- array(-1, c(n, n-1L), list(levels, NULL))
	cont[col(cont) <= row(cont) - 2L] <- 0
	cont[col(cont) == row(cont) - 1L] <- seq_len(n-1L)
        colnames(cont) <- NULL
        if(sparse) .asSparse(cont) else cont
    }
    else
        .Diag(levels, sparse=sparse)
}

contr.treatment <-
    function(n, base = 1, contrasts = TRUE, sparse = FALSE)
{
    if(is.numeric(n) && length(n) == 1L) {
	if(n > 1L) levels <- as.character(seq_len(n))
	else stop("not enough degrees of freedom to define contrasts")
    } else {
	levels <- as.character(n)
	n <- length(n)
    }

    contr <- .Diag(levels, sparse=sparse)
    if(contrasts) {
	if(n < 2L)
	    stop(gettextf("contrasts not defined for %d degrees of freedom",
                          n - 1L), domain = NA)
	if (base < 1L | base > n)
	    stop("baseline group number out of range")
	contr <- contr[, -base, drop = FALSE]
    }
    contr
}

contr.sum <- function (n, contrasts = TRUE, sparse = FALSE)
{
    if (length(n) <= 1L) {
	if (is.numeric(n) && length(n) == 1L && n > 1L)
	    levels <- seq_len(n)
	else stop("not enough degrees of freedom to define contrasts")
    } else levels <- n

    levels <- as.character(levels)
    cont <- .Diag(levels, sparse=sparse)
    if (contrasts) {
        cont <- cont[, -length(levels), drop = FALSE]
        cont[length(levels), ] <- -1
        colnames(cont) <- NULL
    }
    cont
}

contr.SAS <- function(n, contrasts = TRUE, sparse = FALSE)
{
    contr.treatment(n,
                    base = if (is.numeric(n) && length(n) == 1) n else length(n),
                    contrasts=contrasts, sparse=sparse)
}
#  File src/library/stats/R/cor.R
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

#### cor() , cov() and var() : Based on the same C code

cor <-
function(x, y=NULL, use="everything", method = c("pearson", "kendall", "spearman"))
{
    na.method <-
	pmatch(use, c("all.obs", "complete.obs", "pairwise.complete.obs",
		      "everything", "na.or.complete"))
    method <- match.arg(method)
    if(is.data.frame(y)) y <- as.matrix(y) else stopifnot(is.atomic(y))
    if(is.data.frame(x)) x <- as.matrix(x)
    else {
	stopifnot(is.atomic(x))
	if(!is.matrix(x)) {
	    if(is.null(y)) stop("supply both 'x' and 'y' or a matrix-like 'x'")
	    x <- as.vector(x)
	}
    }
    if(method == "pearson")
        .Internal(cor(x, y, na.method, FALSE))
    else if (na.method != 3L) {
	## Rank transform
	Rank <- function(u) {
            ## take care not to drop dims on a 0-row matrix
            if(length(u) == 0L) u else
            if(is.matrix(u)) apply(u, 2L, rank, na.last="keep")
            else rank(u, na.last="keep")
        }

	x <- Rank(x)
	if(!is.null(y)) y <- Rank(y)
        .Internal(cor(x, y, na.method, method == "kendall"))
    }
    else { # rank correlations and pairwise complete; the hard case
         ## Based on contribution from Shigenobu Aoki.
         ## matrix
         if (is.null(y)) {
             ncy <- ncx <- ncol(x)
             if(ncx == 0) stop("'x' is empty")
             r <- matrix(0, nrow = ncx, ncol = ncy)
             ## 2.6.0 assumed the diagonal was 1, but not so for all NAs,
             ## nor single non-NA pairs.
             for (i in seq_len(ncx)) {
                 for (j in seq_len(i)) {
                     x2 <- x[,i]
                     y2 <- x[,j]
                     ok <- complete.cases(x2, y2)
                     x2 <- rank(x2[ok])
                     y2 <- rank(y2[ok])
                     ## we've removed all NAs
                     r[i, j] <- if(any(ok)) .Internal(cor(x2, y2, 1L, method == "kendall")) else NA
                 }
             }
             r <- r + t(r) - diag(diag(r))
	     rownames(r) <- colnames(x)
	     colnames(r) <- colnames(x)
             r
         }
         ## vector/matrix x vector/matrix
         else {
             if(length(x) == 0 || length(y) == 0)
                 stop("both 'x' and 'y' must be non-empty")
             matrix_result <- is.matrix(x) || is.matrix(y)
	     if (!is.matrix(x)) x <- matrix(x, ncol=1L)
	     if (!is.matrix(y)) y <- matrix(y, ncol=1L)
             ncx <- ncol(x)
             ncy <- ncol(y)
             r <- matrix(0, nrow = ncx, ncol = ncy)
             for (i in seq_len(ncx)) {
                 for (j in seq_len(ncy)) {
                     x2 <- x[,i]
                     y2 <- y[,j]
                     ok <- complete.cases(x2, y2)
                     x2 <- rank(x2[ok])
                     y2 <- rank(y2[ok])
                     r[i, j] <- if(any(ok)) .Internal(cor(x2, y2, 1L, method == "kendall")) else NA
                 }
             }
	     rownames(r) <- colnames(x)
	     colnames(r) <- colnames(y)
             if(matrix_result) r else drop(r)
         }
     }
}

cov <-
function(x, y=NULL, use="everything", method = c("pearson", "kendall", "spearman"))
{
    na.method <-
	pmatch(use, c("all.obs", "complete.obs", "pairwise.complete.obs",
		      "everything", "na.or.complete"))
    method <- match.arg(method)
    if(is.data.frame(y)) y <- as.matrix(y) else stopifnot(is.atomic(y))
    if(is.data.frame(x)) x <- as.matrix(x)
    else {
	stopifnot(is.atomic(x))
	if(!is.matrix(x)) {
	    if(is.null(y)) stop("supply both 'x' and 'y' or a matrix-like 'x'")
	    x <- as.vector(x)
	}
    }
    if(method == "pearson")
	.Internal(cov(x, y, na.method, method == "kendall"))
    else if (na.method != 3L) {
	## Rank transform
	Rank <- function(u) {
	    if(length(u) == 0) u else
	    if(is.matrix(u)) apply(u, 2L, rank, na.last="keep")
	    else rank(u, na.last="keep")
	}

	x <- Rank(x)
	if(!is.null(y)) y <- Rank(y)
	.Internal(cov(x, y, na.method, method == "kendall"))
    }
    else
	stop("cannot handle 'pairwise.complete.obs'")
}

var <- function(x, y = NULL, na.rm = FALSE, use) {
    if(missing(use))
	use <- if(na.rm) "na.or.complete" else "everything"
    na.method <-
	pmatch(use, c("all.obs", "complete.obs", "pairwise.complete.obs",
		      "everything", "na.or.complete"))
    if (is.data.frame(x)) x <- as.matrix(x) else stopifnot(is.atomic(x))
    if (is.data.frame(y)) y <- as.matrix(y) else stopifnot(is.atomic(y))
    .Internal(cov(x, y, na.method, FALSE))
}

cov2cor <- function(V)
{
    ## Purpose: Covariance matrix |--> Correlation matrix -- efficiently
    ## ----------------------------------------------------------------------
    ## Arguments: V: a covariance matrix (i.e. symmetric and positive definite)
    ## ----------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 12 Jun 2003, 11L:50
    p <- (d <- dim(V))[1L]
    if(!is.numeric(V) || length(d) != 2L || p != d[2L])
	stop("'V' is not a square numeric matrix")
    Is <- sqrt(1/diag(V)) # diag( 1/sigma_i )
    if(any(!is.finite(Is)))
	warning("diag(.) had 0 or NA entries; non-finite result is doubtful")
    r <- V # keep dimnames
    r[] <- Is * V * rep(Is, each = p)
    ##	== D %*% V %*% D  where D = diag(Is)
    r[cbind(1L:p,1L:p)] <- 1 # exact in diagonal
    r
}
#  File src/library/stats/R/cor.test.R
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

cor.test <- function(x, ...) UseMethod("cor.test")

cor.test.default <-
function(x, y, alternative = c("two.sided", "less", "greater"),
         method = c("pearson", "kendall", "spearman"), exact = NULL,
         conf.level = 0.95, continuity = FALSE, ...)
{
    alternative <- match.arg(alternative)
    method <- match.arg(method)
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))

    if(length(x) != length(y))
	stop("'x' and 'y' must have the same length")
    OK <- complete.cases(x, y)
    x <- x[OK]
    y <- y[OK]
    n <- length(x)

    PVAL <- NULL
    NVAL <- 0
    conf.int <- FALSE

    if(method == "pearson") {
	if(n < 3)
	    stop("not enough finite observations")
	method <- "Pearson's product-moment correlation"
	names(NVAL) <- "correlation"
	r <- cor(x, y)
        df <- n - 2
	ESTIMATE <- c(cor = r)
	PARAMETER <- c(df = df)
	STATISTIC <- c(t = sqrt(df) * r / sqrt(1 - r^2))
	p <- pt(STATISTIC, df)
        if(n > 3) { ## confidence int.
            if(!missing(conf.level) &&
               (length(conf.level) != 1 || !is.finite(conf.level) ||
                conf.level < 0 || conf.level > 1))
                stop("'conf.level' must be a single number between 0 and 1")
            conf.int <- TRUE
            z <- atanh(r)
            sigma <- 1 / sqrt(n - 3)
            cint <-
                switch(alternative,
                       less = c(-Inf, z + sigma * qnorm(conf.level)),
                       greater = c(z - sigma * qnorm(conf.level), Inf),
                       two.sided = z +
                       c(-1, 1) * sigma * qnorm((1 + conf.level) / 2))
            cint <- tanh(cint)
            attr(cint, "conf.level") <- conf.level
        }
    }
    else {
	if(n < 2)
	    stop("not enough finite observations")
	PARAMETER <- NULL
	TIES <- (min(length(unique(x)), length(unique(y))) < n)
	if(method == "kendall") {
	    method <- "Kendall's rank correlation tau"
	    names(NVAL) <- "tau"
	    r <- cor(x,y, method = "kendall")
            ESTIMATE <- c(tau = r)

            if(!is.finite(ESTIMATE)) {  # all x or all y the same
                ESTIMATE[] <- NA
                STATISTIC <- c(T = NA)
                PVAL <- NA
            }
            else {
                if(is.null(exact))
                    exact <- (n < 50)
                if(exact && !TIES) {
                    q <- round((r + 1) * n * (n - 1) / 4)
                    pkendall <- function(q, n) {
                        .C("pkendall",
                           length(q),
                           p = as.double(q),
                           as.integer(n),
                           PACKAGE = "stats")$p
                    }
                    PVAL <-
                        switch(alternative,
                               "two.sided" = {
                                   if(q > n * (n - 1) / 4)
                                       p <- 1 - pkendall(q - 1, n)
                                   else
                                       p <- pkendall(q, n)
                                   min(2 * p, 1)
                               },
                               "greater" = 1 - pkendall(q - 1, n),
                               "less" = pkendall(q, n))
                    STATISTIC <- c(T = q)
                } else {
                    xties <- table(x[duplicated(x)]) + 1
                    yties <- table(y[duplicated(y)]) + 1
                    T0 <- n * (n - 1)/2
                    T1 <- sum(xties * (xties - 1))/2
                    T2 <- sum(yties * (yties - 1))/2
                    S <- r * sqrt((T0 - T1) * (T0 - T2))
                    v0 <- n * (n - 1) * (2 * n + 5)
                    vt <- sum(xties * (xties - 1) * (2 * xties + 5))
                    vu <- sum(yties * (yties - 1) * (2 * yties + 5))
                    v1 <- sum(xties * (xties - 1)) * sum(yties * (yties - 1))
                    v2 <- sum(xties * (xties - 1) * (xties - 2)) *
                        sum(yties * (yties - 1) * (yties - 2))

                    var_S <- (v0 - vt - vu) / 18 +
                        v1 / (2 * n * (n - 1)) +
                            v2 / (9 * n * (n - 1) * (n - 2))

                    if (continuity) S <- sign(S) * (abs(S) - 1)
                    STATISTIC <- c(z = S / sqrt(var_S))
                    p <- pnorm(STATISTIC)
                    if(exact && TIES)
                        warning("Cannot compute exact p-value with ties")
                }
            }
	} else {
	    method <- "Spearman's rank correlation rho"
            if (is.null(exact))
                exact <- TRUE
	    names(NVAL) <- "rho"
	    r <- cor(rank(x), rank(y))
	    ESTIMATE <- c(rho = r)
            if(!is.finite(ESTIMATE)) {  # all x or all y the same
                ESTIMATE[] <- NA
                STATISTIC <- c(S = NA)
                PVAL <- NA
            }
            else {
                ## Use the test statistic S = sum(rank(x) - rank(y))^2
                ## and AS 89 for obtaining better p-values than via the
                ## simple normal approximation.
                ## In the case of no ties, S = (1-rho) * (n^3-n)/6.
                pspearman <- function(q, n, lower.tail = TRUE) {
                    if(n <= 1290 && exact) # n*(n^2 - 1) does not overflow
                        .C("prho",
                           as.integer(n),
                           as.double(round(q) + 2*lower.tail),
                           p = double(1L),
                           integer(1L),
                           as.logical(lower.tail),
                           PACKAGE = "stats")$p
		    else { # for large n: asymptotic t_{n-2}
                        den <- (n*(n^2-1))/6 # careful for overflow
                        ## Kendall et all (1939) p. 260
                        if (continuity) den <- den + 1
			r <- 1 - q / den
			pt(r / sqrt((1 - r^2)/(n-2)), df = n-2,
			   lower.tail= !lower.tail)
		    }
                }
                q <- (n^3 - n) * (1 - r) / 6
                STATISTIC <- c(S = q)
                if(TIES && exact){
                    exact <- FALSE
                    warning("Cannot compute exact p-values with ties")
                }
                PVAL <-
                    switch(alternative,
                           "two.sided" = {
                               p <- if(q > (n^3 - n) / 6)
                                   pspearman(q, n, lower.tail = FALSE)
                               else
				   pspearman(q, n, lower.tail = TRUE)
			       min(2 * p, 1)
			   },
			   "greater" = pspearman(q, n, lower.tail = TRUE),
			   "less" = pspearman(q, n, lower.tail = FALSE))
            }
        }
    }

    if(is.null(PVAL)) # for "pearson" only, currently
	PVAL <- switch(alternative,
		       "less" = p,
		       "greater" = 1 - p,
		       "two.sided" = 2 * min(p, 1 - p))

    RVAL <- list(statistic = STATISTIC,
                 parameter = PARAMETER,
                 p.value = as.numeric(PVAL),
                 estimate = ESTIMATE,
                 null.value = NVAL,
                 alternative = alternative,
                 method = method,
                 data.name = DNAME)
    if(conf.int)
        RVAL <- c(RVAL, list(conf.int = cint))
    class(RVAL) <- "htest"
    RVAL
}

cor.test.formula <-
function(formula, data, subset, na.action, ...)
{
    if(missing(formula)
       || !inherits(formula, "formula")
       || length(formula) != 2)
        stop("'formula' missing or invalid")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m[[1L]] <- as.name("model.frame")
    m$... <- NULL
    mf <- eval(m, environment(formula))
    if(length(mf) != 2)
        stop("invalid formula")
    DNAME <- paste(names(mf), collapse = " and ")
    names(mf) <- c("x", "y")
    y <- do.call("cor.test", c(mf, list(...)))
    y$data.name <- DNAME
    y
}
#  File src/library/stats/R/cov.wt.R
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

cov.wt <- function(x, wt = rep(1/nrow(x), nrow(x)), cor = FALSE, center = TRUE,
                   method = c("unbiased", "ML"))
{
    if (is.data.frame(x))
	x <- as.matrix(x)
    else if (!is.matrix(x))
	stop("'x' must be a matrix or a data frame")
    if (!all(is.finite(x)))
	stop("'x' must contain finite values only")
    n <- nrow(x)
    if (with.wt <- !missing(wt)) {
	if (length(wt) != n)
	    stop("length of 'wt' must equal the number of rows in 'x'")
	if (any(wt < 0) || (s <- sum(wt)) == 0)
	    stop("weights must be non-negative and not all zero")
	wt <- wt / s
    }
    if (is.logical(center)) {
	center <- if (center) colSums(wt * x) else 0
    } else {
	if (length(center) != ncol(x))
	    stop("length of 'center' must equal the number of columns in 'x'")
    }
    x <- sqrt(wt) * sweep(x, 2, center, check.margin=FALSE)
    cov <-
        switch(match.arg(method),
               "unbiased" = crossprod(x) / (1 - sum(wt^2)),
               "ML" = crossprod(x))
    y <- list(cov = cov, center = center, n.obs = n)
    if (with.wt) y$wt <- wt
    if (cor) { ## as cov2cor():
        Is <- 1 / sqrt(diag(cov))
        R <- cov
        R[] <- Is * cov * rep(Is, each = nrow(cov))
	y$cor <- R
    }
    y
}
#  File src/library/stats/R/cpgram.R
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

## from MASS library: (C) 1994-9 W. N. Venables and B. D. Ripley

cpgram <- function(ts, taper=0.1,
		   main=paste("Series: ", deparse(substitute(ts))), ci.col="blue")
{
    main
    if(NCOL(ts) > 1)
	stop("only implemented for univariate time series")
    x <- as.vector(ts)
    x <- x[!is.na(x)]
    x <- spec.taper(scale(x, TRUE, FALSE), p=taper)
    y <- Mod(fft(x))^2/length(x)
    y[1L] <- 0
    n <- length(x)
    x <- (0:(n/2))*frequency(ts)/n
    if(length(x)%%2==0) {
	n <- length(x)-1
	y <- y[1L:n]
	x <- x[1L:n]
    } else y <- y[seq_along(x)]
    xm <- frequency(ts)/2
    mp <- length(x)-1
    crit <- 1.358/(sqrt(mp)+0.12+0.11/sqrt(mp))
    oldpty <- par(pty ="s")
    on.exit(par(oldpty))
    plot(x, cumsum(y)/sum(y), type="s", xlim=c(0, xm),
	 ylim=c(0, 1), xaxs="i", yaxs="i", xlab="frequency",
	 ylab="")
    lines(c(0, xm*(1-crit)), c(crit, 1), col = ci.col, lty = 2)
    lines(c(xm*crit, xm), c(0, 1-crit), col = ci.col, lty = 2)
    title(main = main)
    invisible()
}
#  File src/library/stats/R/cutree.R
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

cutree <- function(tree, k=NULL, h=NULL)
{
    if(is.null(n1 <- nrow(tree$merge)) || n1 < 1)
        stop("invalid 'tree' (merge component)")
    n <- n1 + 1
    if(is.null(k) && is.null(h))
        stop("either 'k' or 'h' must be specified")
    if(is.null(k)) {
        if(is.unsorted(tree$height))
            stop("the 'height' component of 'tree' is not sorted\n(increasingly); consider applying as.hclust() first")
        ## h |--> k
        k <- integer(length(h))
        ## S+6 help(cutree) says k(h) = k(h+), but does k(h-) [continuity]
        ## h < min() should give k = n;
        k <- n+1 - apply(outer(c(tree$height,Inf), h, ">"), 2, which.max)
        if(getOption("verbose")) message("cutree(): k(h) = ", k, domain = NA)
    }
    else {
        k <- as.integer(k)
        if(min(k) < 1 || max(k) > n)
            stop(gettextf("elements of 'k' must be between 1 and %d", n),
                 domain = NA)
    }

    ans <- .Call("R_cutree", tree$merge, k, PACKAGE = "stats")

    if(length(k) == 1) {
        ans <- as.vector(ans)
        names(ans) <- tree$labels
    }
    else{
        colnames(ans) <- if(!is.null(h)) h else k
        rownames(ans) <- tree$labels
    }
    return(ans)
}
#  File src/library/stats/R/dendrogram.R
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

as.dendrogram <- function(object, ...) UseMethod("as.dendrogram")

as.dendrogram.dendrogram <- function(object, ...) object

as.dendrogram.hclust <- function (object, hang = -1, ...)
## hang = 0.1  is default for plot.hclust
{
    stopifnot(length(object$order) > 0L)
    if (is.null(object$labels))
	object$labels <- seq_along(object$order)
    z <- list()
    nMerge <- length(oHgt <- object$height)
    if (nMerge != nrow(object$merge))
	stop("'merge' and 'height' do not fit!")
    hMax <- oHgt[nMerge]
    one <- 1L;	   two <- 2L # integer!
    for (k in 1L:nMerge) {
	x <- object$merge[k, ]# no sort() anymore!
	if (any(neg <- x < 0))
	    h0 <- if (hang < 0) 0 else max(0, oHgt[k] - hang * hMax)
	if (all(neg)) {			# two leaves
	    zk <- as.list(-x)
	    attr(zk, "members") <- two
	    attr(zk, "midpoint") <- 0.5 # mean( c(0,1) )
	    objlabels <- object$labels[-x]
	    attr(zk[[1L]], "label") <- objlabels[1L]
	    attr(zk[[2L]], "label") <- objlabels[2L]
	    attr(zk[[1L]], "members") <- attr(zk[[2L]], "members") <- one
	    attr(zk[[1L]], "height") <- attr(zk[[2L]], "height") <- h0
	    attr(zk[[1L]], "leaf") <- attr(zk[[2L]], "leaf") <- TRUE
	}
	else if (any(neg)) {		# one leaf, one node
	    X <- as.character(x)
	    ## Originally had "x <- sort(..) above => leaf always left, x[1L];
	    ## don't want to assume this
	    isL <- x[1L] < 0 ## is leaf left?
	    zk <-
		if(isL) list(-x[1L], z[[X[2L]]])
		else	list(z[[X[1L]]], -x[2L])
	    attr(zk, "members") <- attr(z[[X[1 + isL]]], "members") + one
	    attr(zk, "midpoint") <-
                (.memberDend(zk[[1L]]) + attr(z[[X[1 + isL]]], "midpoint"))/2
	    attr(zk[[2 - isL]], "members") <- one
	    attr(zk[[2 - isL]], "height") <- h0
	    attr(zk[[2 - isL]], "label") <- object$labels[-x[2 - isL]]
	    attr(zk[[2 - isL]], "leaf") <- TRUE
	}
	else {				# two nodes
	    x <- as.character(x)
	    zk <- list(z[[x[1L]]], z[[x[2L]]])
	    attr(zk, "members") <- attr(z[[x[1L]]], "members") +
		attr(z[[x[2L]]], "members")
	    attr(zk, "midpoint") <- (attr(z[[x[1L]]], "members") +
				     attr(z[[x[1L]]], "midpoint") +
				     attr(z[[x[2L]]], "midpoint"))/2
	}
	attr(zk, "height") <- oHgt[k]
	z[[k <- as.character(k)]] <- zk
    }
    z <- z[[k]]
    class(z) <- "dendrogram"
    z
}

### MM: 'FIXME'	 (2002-05-14):
###	 =====
## We currently (mis)use a node's "members" attribute for two things:
## 1) #{sub nodes}
## 2) information about horizontal layout of the given node
## Because of that, cut.dend..() cannot correctly set "members" as it should!

## ==> start using "x.member" and the following function :

.memberDend <- function(x) {
    r <- attr(x,"x.member")
    if(is.null(r)) {
	r <- attr(x,"members")
	if(is.null(r)) r <- 1L
    }
    r
}

.midDend <- function(x)
    if(is.null(mp <- attr(x, "midpoint"))) 0 else mp

midcache.dendrogram <- function (x, type = "hclust")
{
    ## Recompute "midpoint" attributes of a dendrogram, e.g. after reorder().

    type <- match.arg(type) ## currently only "hclust"
    if( !inherits(x, "dendrogram") )
	stop("we require a dendrogram")
    setmid <- function(d, type) {
	if(is.leaf(d))# no "midpoint"
	    return(d)
	k <- length(d)
	if(k < 1)
	    stop("dendrogram node with non-positive #{branches}")
	r <- d # incl. attributes!
	midS <- 0
	for(j in 1L:k) {
	    r[[j]] <- unclass(setmid(d[[j]], type))
	    midS <- midS + .midDend(r[[j]])
	}
	if(type == "hclust" && k != 2)
	    warning("midcache() of non-binary dendrograms only partly implemented")
	## compatible to as.dendrogram.hclust() {MM: doubtful if k > 2}
	attr(r, "midpoint") <- (.memberDend(d[[1L]]) + midS) / 2
	r
    }
    setmid(x, type=type)
}


### Define a very concise print() method for dendrograms:
##  Martin Maechler, 15 May 2002
print.dendrogram <- function(x, digits = getOption("digits"), ...)
{
    cat("'dendrogram' ")
    if(is.leaf(x))
	cat("leaf '", format(attr(x, "label"), digits = digits),"'", sep='')
    else
	cat("with", length(x), "branches and",
	    attr(x,"members"), "members total")

    cat(", at height", format(attr(x,"height"), digits = digits), "\n")
    invisible(x)
}

str.dendrogram <-
function (object, max.level = NA, digits.d = 3, give.attr = FALSE,
          wid = getOption("width"), nest.lev = 0, indent.str = "",
          stem = "--", ...)
{
## TO DO: when object is part of a larger structure which is str()ed
##    with default max.level= NA, it should not be str()ed to all levels,
##   but only to e.g. level 2
## Implement via smarter default for 'max.level' (?)

    pasteLis <- function(lis, dropNam, sep = " = ") {
	## drop uninteresting "attributes" here
	lis <- lis[!(names(lis) %in% dropNam)]
	fl <- sapply(lis, format, digits=digits.d)
	paste(paste(names(fl), fl, sep=sep), collapse = ", ")
    }

    istr <- sub(" $", "`", indent.str)
    cat(istr, stem, sep="")

    at <- attributes(object)
    memb <- at[["members"]]
    hgt	 <- at[["height"]]
    if(!is.leaf(object)) {
	le <- length(object)
	if(give.attr) {
	    if(nzchar(at <- pasteLis(at, c("class", "height", "members"))))
		at <- paste(",", at)
	}
	cat("[dendrogram w/ ", le, " branches and ", memb, " members at h = ",
            format(hgt, digits=digits.d), if(give.attr) at,
            "]", if(!is.na(max.level) && nest.lev == max.level)" ..", "\n", sep="")
	if (is.na(max.level) || nest.lev < max.level) {
	    for(i in 1L:le) {
		##cat(indent.str, nam.ob[i], ":", sep="")
		str(object[[i]], nest.lev = nest.lev + 1,
		    indent.str= paste(indent.str, if(i < le) " |" else "  "),
		    max.level=max.level, digits.d=digits.d,
		    give.attr= give.attr, wid=wid)
	    }
	}
    } else { ## leaf
	cat("leaf",
	    if(is.character(at$label)) paste("",at$label,"",sep='"') else
	    format(object, digits=digits.d),"")
	any.at <- hgt != 0
	if(any.at) cat("(h=",format(hgt, digits=digits.d))
	if(memb != 1) #MM: when can this happen?
	    cat(if(any.at)", " else {any.at <- TRUE; "("}, "memb= ",memb,sep="")
	at <- pasteLis(at, c("class", "height", "members", "leaf", "label"))
	if(any.at || nzchar(at)) cat(if(!any.at)"(", at, ")")
	cat("\n")
    }
    invisible()
}


## The ``generic'' method for "[["  (identical to e.g., "[[.POSIXct"):
## --> subbranches are dendrograms as well!
"[[.dendrogram" <- function(x, ..., drop = TRUE)
{
    cl <- class(x)
    class(x) <- NULL
    val <- NextMethod("[[")
    class(val) <- cl
    val
}


## FIXME: need larger par("mar")[1L] or [4L] for longish labels !
## {probably don't change, just print a warning ..}
plot.dendrogram <-
    function (x, type = c("rectangle", "triangle"), center = FALSE,
	      edge.root = is.leaf(x) || !is.null(attr(x, "edgetext")),
	      nodePar = NULL, edgePar = list(),
	      leaflab = c("perpendicular", "textlike", "none"), dLeaf = NULL,
	      xlab = "", ylab = "", xaxt="n", yaxt="s",
	      horiz = FALSE, frame.plot = FALSE, xlim, ylim, ...)
{
    type <- match.arg(type)
    leaflab <- match.arg(leaflab)
    hgt <- attr(x, "height")
    if (edge.root && is.logical(edge.root))
	edge.root <- 0.0625 * if(is.leaf(x)) 1 else hgt
    mem.x <- .memberDend(x)
    yTop <- hgt + edge.root
    if(center) { x1 <- 0.5 ; x2 <- mem.x + 0.5 }
    else       { x1 <- 1   ; x2 <- mem.x }
    xl. <- c(x1 - 1/2, x2 + 1/2)
    yl. <- c(0, yTop)
    if (horiz) {## swap and reverse direction on `x':
	tmp <- xl.; xl. <- rev(yl.); yl. <- tmp
	tmp <- xaxt; xaxt <- yaxt; yaxt <- tmp
    }
    if(missing(xlim) || is.null(xlim)) xlim <- xl.
    if(missing(ylim) || is.null(ylim)) ylim <- yl.
    plot(0, xlim = xlim, ylim = ylim, type = "n", xlab = xlab, ylab = ylab,
	 xaxt = xaxt, yaxt = yaxt, frame.plot = frame.plot, ...)
    if(is.null(dLeaf))
        dLeaf <- .75*(if(horiz) strwidth("w") else strheight("x"))

    if (edge.root) {
### FIXME: the first edge + edgetext is drawn here, all others in plotNode()
### -----  maybe use trick with adding a single parent node to the top ?
	x0 <- plotNodeLimit(x1, x2, x, center)$x
	if (horiz)
	    segments(hgt, x0, yTop, x0)
	else segments(x0, hgt, x0, yTop)
	if (!is.null(et <- attr(x, "edgetext"))) {
	    my <- mean(hgt, yTop)
	    if (horiz)
		text(my, x0, et)
	    else text(x0, my, et)
	}
    }
    plotNode(x1, x2, x, type = type, center = center, leaflab = leaflab,
             dLeaf = dLeaf, nodePar = nodePar, edgePar = edgePar, horiz = horiz)
}

### the work horse: plot node (if pch) and lines to all children
plotNode <-
    function(x1, x2, subtree, type, center, leaflab, dLeaf,
	     nodePar, edgePar, horiz = FALSE)
{
    inner <- !is.leaf(subtree) && x1 != x2
    yTop <- attr(subtree, "height")
    bx <- plotNodeLimit(x1, x2, subtree, center)
    xTop <- bx$x

    ## handle node specific parameters in "nodePar":
    hasP <- !is.null(nPar <- attr(subtree, "nodePar"))
    if(!hasP) nPar <- nodePar

    if(getOption("verbose")) {
	cat(if(inner)"inner node" else "leaf", ":")
	if(!is.null(nPar)) { cat(" with node pars\n"); str(nPar) }
	cat(if(inner)paste(" height", formatC(yTop),"; "),
	    "(x1,x2)= (",formatC(x1,width=4),",",formatC(x2,width=4),")",
	    "--> xTop=", formatC(xTop, width=8),"\n", sep="")
    }

    Xtract <- function(nam, L, default, indx)
	rep(if(nam %in% names(L)) L[[nam]] else default,
	    length.out = indx)[indx]
    asTxt <- function(x) # to allow 'plotmath' labels:
	if(is.character(x) || is.expression(x) || is.null(x)) x else as.character(x)

    i <- if(inner || hasP) 1 else 2 # only 1 node specific par

    if(!is.null(nPar)) { ## draw this node
	pch <- Xtract("pch", nPar, default = 1L:2,	 i)
	cex <- Xtract("cex", nPar, default = c(1,1),	 i)
	col <- Xtract("col", nPar, default = par("col"), i)
	bg <- Xtract("bg", nPar, default = par("bg"), i)
	points(if (horiz) cbind(yTop, xTop) else cbind(xTop, yTop),
	       pch = pch, bg = bg, col = col, cex = cex)
    }

    if(leaflab == "textlike")
        p.col <- Xtract("p.col", nPar, default = "white", i)
    lab.col <- Xtract("lab.col", nPar, default = par("col"), i)
    lab.cex <- Xtract("lab.cex", nPar, default = c(1,1), i)
    lab.font <- Xtract("lab.font", nPar, default = par("font"), i)
    if (is.leaf(subtree)) {
	## label leaf
	if (leaflab == "perpendicular") { # somewhat like plot.hclust
	    if(horiz) {
                X <- yTop + dLeaf * lab.cex
                Y <- xTop; srt <- 0; adj <- c(0, 0.5)
	    }
	    else {
                Y <- yTop - dLeaf * lab.cex
                X <- xTop; srt <- 90; adj <- 1
	    }
            nodeText <- asTxt(attr(subtree,"label"))
	    text(X, Y, nodeText, xpd = TRUE, srt = srt, adj = adj,
                 cex = lab.cex, col = lab.col, font = lab.font)
	}
    }
    else if (inner) {
	segmentsHV <- function(x0, y0, x1, y1) {
	    if (horiz)
		segments(y0, x0, y1, x1, col = col, lty = lty, lwd = lwd)
	    else segments(x0, y0, x1, y1, col = col, lty = lty, lwd = lwd)
	}
	for (k in seq_along(subtree)) {
	    child <- subtree[[k]]
	    ## draw lines to the children and draw them recursively
	    yBot <- attr(child, "height")
	    if (getOption("verbose")) cat("ch.", k, "@ h=", yBot, "; ")
	    if (is.null(yBot))
		yBot <- 0
	    xBot <-
		if (center) mean(bx$limit[k:(k + 1)])
		else bx$limit[k] + .midDend(child)

	    hasE <- !is.null(ePar <- attr(child, "edgePar"))
	    if (!hasE)
		ePar <- edgePar
	    i <- if (!is.leaf(child) || hasE) 1 else 2
	    ## define line attributes for segmentsHV():
	    col <- Xtract("col", ePar, default = par("col"), i)
	    lty <- Xtract("lty", ePar, default = par("lty"), i)
	    lwd <- Xtract("lwd", ePar, default = par("lwd"), i)
	    if (type == "triangle") {
		segmentsHV(xTop, yTop, xBot, yBot)
	    }
	    else { # rectangle
		segmentsHV(xTop,yTop, xBot,yTop)# h
		segmentsHV(xBot,yTop, xBot,yBot)# v
	    }
	    vln <- NULL
	    if (is.leaf(child) && leaflab == "textlike") {
		nodeText <- asTxt(attr(child,"label"))
		if(getOption("verbose"))
		    cat('-- with "label"',format(nodeText))
		hln <- 0.6 * strwidth(nodeText, cex = lab.cex)/2
		vln <- 1.5 * strheight(nodeText, cex = lab.cex)/2
		rect(xBot - hln, yBot,
		     xBot + hln, yBot + 2 * vln, col = p.col)
		text(xBot, yBot + vln, nodeText, xpd = TRUE,
                     cex = lab.cex, col = lab.col, font = lab.font)
	    }
	    if (!is.null(attr(child, "edgetext"))) {
		edgeText <- asTxt(attr(child, "edgetext"))
		if(getOption("verbose"))
		    cat('-- with "edgetext"',format(edgeText))
		if (!is.null(vln)) {
		    mx <-
			if(type == "triangle")
			    (xTop+ xBot+ ((xTop - xBot)/(yTop - yBot)) * vln)/2
			else xBot
		    my <- (yTop + yBot + 2 * vln)/2
		}
		else {
		    mx <- if(type == "triangle") (xTop + xBot)/2 else xBot
		    my <- (yTop + yBot)/2
		}
		## Both for "triangle" and "rectangle" : Diamond + Text

                p.col <- Xtract("p.col", ePar, default = "white", i)
                p.border <- Xtract("p.border", ePar, default = par("fg"), i)
                ## edge label pars: defaults from the segments pars
                p.lwd <- Xtract("p.lwd", ePar, default = lwd, i)
                p.lty <- Xtract("p.lty", ePar, default = lty, i)
                t.col <- Xtract("t.col", ePar, default = col, i)
                t.cex <- Xtract("t.cex", ePar, default =  1,  i)
                t.font<- Xtract("t.font",ePar, default= par("font"), i)

		vlm <- strheight(c(edgeText,"h"), cex = t.cex)/2
		hlm <- strwidth (c(edgeText,"m"), cex = t.cex)/2
		hl3 <- c(hlm[1L], hlm[1L] + hlm[2L], hlm[1L])
                if(horiz) {
                    polygon(my+ c(-hl3, hl3), mx + sum(vlm)*c(-1L:1L, 1L:-1L),
                            col = p.col, border= p.border,
                            lty = p.lty, lwd = p.lwd)
                    text(my, mx, edgeText, cex = t.cex, col = t.col,
                         font = t.font)
                } else {
                    polygon(mx+ c(-hl3, hl3), my + sum(vlm)*c(-1L:1L, 1L:-1L),
                            col = p.col, border= p.border,
                            lty = p.lty, lwd = p.lwd)
                    text(mx, my, edgeText, cex = t.cex, col = t.col,
                         font = t.font)
                }
	    }
	    plotNode(bx$limit[k], bx$limit[k + 1], subtree = child,
		     type, center, leaflab, dLeaf, nodePar, edgePar, horiz)
	}
    }
    invisible()
}

plotNodeLimit <- function(x1, x2, subtree, center)
{
    ## get the left borders limit[k] of all children k=1..K, and
    ## the handle point `x' for the edge connecting to the parent.
    inner <- !is.leaf(subtree) && x1 != x2
    if(inner) {
	K <- length(subtree)
	mTop <- .memberDend(subtree)
	limit <- integer(K)
	xx1 <- x1
	for(k in 1L:K) {
	    m <- .memberDend(subtree[[k]])
	    ##if(is.null(m)) m <- 1
	    xx1 <- xx1 + (if(center) (x2-x1) * m/mTop else m)
	    limit[k] <- xx1
	}
	limit <- c(x1, limit)
    } else { ## leaf
	limit <- c(x1, x2)
    }
    mid <- attr(subtree, "midpoint")
    center <- center || (inner && !is.numeric(mid))
    x <- if(center) mean(c(x1,x2)) else x1 + (if(inner) mid else 0)
    list(x = x, limit = limit)
}

cut.dendrogram <- function(x, h, ...)
{
    LOWER <- list()
    X <- 1

    assignNodes <- function(subtree, h) {
	if(!is.leaf(subtree)) {
	    if(!(K <- length(subtree)))
		stop("non-leaf subtree of length 0")
	    new.mem <- 0L
	    for(k in 1L:K) {
                sub <- subtree[[k]]
		if(attr(sub, "height") <= h) {
		    ## cut it, i.e. save to LOWER[] and make a leaf
		    at <- attributes(sub)
		    at$leaf <- TRUE
                    at$class <- NULL# drop it from leaf
		    at$x.member <- at$members
		    new.mem <- new.mem + (at$members <- 1L)
		    at$label <- paste("Branch", X)
		    subtree[[k]] <- X #paste("Branch", X)
		    attributes(subtree[[k]]) <- at
		    class(sub) <- "dendrogram"
		    LOWER[[X]] <<- sub
		    X <<- X+1
		}
		else { ## don't cut up here, possibly its children:
		    subtree[[k]] <- assignNodes(sub, h)
		    new.mem <- new.mem + attr(subtree[[k]], "members")
		}
	    }
	    ## re-count members:
	    attr(subtree,"x.member") <- attr(subtree,"members")
	    attr(subtree,"members") <- new.mem
	}
	subtree
    }# assignNodes()

    list(upper = assignNodes(x, h), lower = LOWER)
}## cut.dendrogram()

is.leaf <- function(object) (is.logical(L <- attr(object, "leaf"))) && L

## *Not* a method (yet):
order.dendrogram <- function(x) {
    if( !inherits(x, "dendrogram") )
	stop("'order.dendrogram' requires a dendrogram")
    unlist(x)
}

##RG's first version -- for posterity
# order.dendrogram <- function(x) {
#    if( !inherits(x, "dendrogram") )
#	stop("order.dendrogram requires a dendrogram")
#    ord <- function(x) {
#      if( is.leaf(x) ) return(x[1L])
#      return(c(ord(x[[1L]]), ord(x[[2L]])))
#    }
#   return(ord(x))
# }

reorder <- function(x, ...) UseMethod("reorder")

reorder.dendrogram <- function(x, wts, agglo.FUN = sum, ...)
{
    if( !inherits(x, "dendrogram") )
	stop("we require a dendrogram")
    agglo.FUN <- match.fun(agglo.FUN)
    oV <- function(x, wts) {
	if(is.leaf(x)) {
	    attr(x, "value") <- wts[x[1L]]
	    return(x)
	}
        k <- length(x)
        if(k == 0L) stop("invalid (length 0) node in dendrogram")
        vals <- numeric(k)
        for(j in 1L:k) {
            ## insert/compute 'wts' recursively down the branches:
            b <- oV(x[[j]], wts)
            x[[j]] <- b
            vals[j] <- attr(b, "value")
        }
        iOrd <- sort.list(vals)
	attr(x, "value") <- agglo.FUN(vals[iOrd])
        x[] <- x[iOrd]
        x
    }
    midcache.dendrogram( oV(x, wts) )
}

rev.dendrogram <- function(x) {
    if(is.leaf(x))
	return(x)

    k <- length(x)
    if(k < 1)
	stop("dendrogram non-leaf node with non-positive #{branches}")
    r <- x # incl. attributes!
    for(j in 1L:k) ## recurse
	r[[j]] <- rev(x[[k+1-j]])
    midcache.dendrogram( r )
}

## This is a cheap
labels.dendrogram <- function(object, ...)
    unlist(dendrapply(object, function(n) attr(n,"label")))


dendrapply <- function(X, FUN, ...)
{
    ## Purpose: "dendrogram" apply {to each node}
    ## ----------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 26 Jun 2004, 22:43
    FUN <- match.fun(FUN)
    if( !inherits(X, "dendrogram") ) stop("'X' is not a dendrogram")

    ## Node apply recursively:
    Napply <- function(d) {
	r <- FUN(d, ...)
	if(!is.leaf(d)) {
            if(!is.list(r)) r <- as.list(r) # fixing unsafe FUN()s
            for(j in seq_along(d))
                r[[j]] <- Napply(d[[j]])
        }
	r
    }
    Napply(X)
}


## original Andy Liaw; modified RG, MM :
heatmap <-
function (x, Rowv=NULL, Colv=if(symm)"Rowv" else NULL,
	  distfun = dist, hclustfun = hclust,
          reorderfun = function(d,w) reorder(d,w),
          add.expr, symm = FALSE, revC = identical(Colv, "Rowv"),
	  scale = c("row", "column", "none"), na.rm=TRUE,
	  margins = c(5, 5), ColSideColors, RowSideColors,
	  cexRow = 0.2 + 1/log10(nr), cexCol = 0.2 + 1/log10(nc),
	  labRow = NULL, labCol = NULL, main = NULL, xlab = NULL, ylab = NULL,
	  keep.dendro = FALSE,
	  verbose = getOption("verbose"), ...)
{
    scale <- if(symm && missing(scale)) "none" else match.arg(scale)
    if(length(di <- dim(x)) != 2 || !is.numeric(x))
	stop("'x' must be a numeric matrix")
    nr <- di[1L]
    nc <- di[2L]
    if(nr <= 1 || nc <= 1)
	stop("'x' must have at least 2 rows and 2 columns")
    if(!is.numeric(margins) || length(margins) != 2L)
	stop("'margins' must be a numeric vector of length 2")

    doRdend <- !identical(Rowv,NA)
    doCdend <- !identical(Colv,NA)
    if(!doRdend && identical(Colv, "Rowv")) doCdend <- FALSE
    ## by default order by row/col means
    if(is.null(Rowv)) Rowv <- rowMeans(x, na.rm = na.rm)
    if(is.null(Colv)) Colv <- colMeans(x, na.rm = na.rm)

    ## get the dendrograms and reordering indices

    if(doRdend) {
	if(inherits(Rowv, "dendrogram"))
	    ddr <- Rowv
	else {
	    hcr <- hclustfun(distfun(x))
	    ddr <- as.dendrogram(hcr)
	    if(!is.logical(Rowv) || Rowv)
		ddr <- reorderfun(ddr, Rowv)
	}
	if(nr != length(rowInd <- order.dendrogram(ddr)))
	    stop("row dendrogram ordering gave index of wrong length")
    }
    else rowInd <- 1L:nr

    if(doCdend) {
	if(inherits(Colv, "dendrogram"))
	    ddc <- Colv
	else if(identical(Colv, "Rowv")) {
	    if(nr != nc)
		stop('Colv = "Rowv" but nrow(x) != ncol(x)')
	    ddc <- ddr
	}
	else {
	    hcc <- hclustfun(distfun(if(symm)x else t(x)))
	    ddc <- as.dendrogram(hcc)
	    if(!is.logical(Colv) || Colv)
		ddc <- reorderfun(ddc, Colv)
	}
	if(nc != length(colInd <- order.dendrogram(ddc)))
	    stop("column dendrogram ordering gave index of wrong length")
    }
    else colInd <- 1L:nc

    ## reorder x
    x <- x[rowInd, colInd]

    labRow <-
	if(is.null(labRow))
	    if(is.null(rownames(x))) (1L:nr)[rowInd] else rownames(x)
	else labRow[rowInd]
    labCol <-
	if(is.null(labCol))
	    if(is.null(colnames(x))) (1L:nc)[colInd] else colnames(x)
	else labCol[colInd]

    if(scale == "row") {
	x <- sweep(x, 1L, rowMeans(x, na.rm = na.rm), check.margin=FALSE)
	sx <- apply(x, 1L, sd, na.rm = na.rm)
	x <- sweep(x, 1L, sx, "/", check.margin=FALSE)
    }
    else if(scale == "column") {
	x <- sweep(x, 2L, colMeans(x, na.rm = na.rm), check.margin=FALSE)
	sx <- apply(x, 2L, sd, na.rm = na.rm)
	x <- sweep(x, 2L, sx, "/", check.margin=FALSE)
    }

    ## Calculate the plot layout
    lmat <- rbind(c(NA, 3), 2:1)
    lwid <- c(if(doRdend) 1 else 0.05, 4)
    lhei <- c((if(doCdend) 1 else 0.05) + if(!is.null(main)) 0.2 else 0, 4)
    if(!missing(ColSideColors)) { ## add middle row to layout
	if(!is.character(ColSideColors) || length(ColSideColors) != nc)
	    stop("'ColSideColors' must be a character vector of length ncol(x)")
	lmat <- rbind(lmat[1,]+1, c(NA,1), lmat[2,]+1)
	lhei <- c(lhei[1L], 0.2, lhei[2L])
    }
    if(!missing(RowSideColors)) { ## add middle column to layout
	if(!is.character(RowSideColors) || length(RowSideColors) != nr)
	    stop("'RowSideColors' must be a character vector of length nrow(x)")
	lmat <- cbind(lmat[,1]+1, c(rep(NA, nrow(lmat)-1), 1), lmat[,2]+1)
	lwid <- c(lwid[1L], 0.2, lwid[2L])
    }
    lmat[is.na(lmat)] <- 0
    if(verbose) {
	cat("layout: widths = ", lwid, ", heights = ", lhei,"; lmat=\n")
	print(lmat)
    }

    ## Graphics `output' -----------------------

    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    layout(lmat, widths = lwid, heights = lhei, respect = TRUE)
    ## draw the side bars
    if(!missing(RowSideColors)) {
	par(mar = c(margins[1L],0, 0,0.5))
	image(rbind(1L:nr), col = RowSideColors[rowInd], axes = FALSE)
    }
    if(!missing(ColSideColors)) {
	par(mar = c(0.5,0, 0,margins[2L]))
	image(cbind(1L:nc), col = ColSideColors[colInd], axes = FALSE)
    }
    ## draw the main carpet
    par(mar = c(margins[1L], 0, 0, margins[2L]))
    if(!symm || scale != "none")
	x <- t(x)
    if(revC) { # x columns reversed
	iy <- nr:1
        if(doRdend) ddr <- rev(ddr)
	x <- x[,iy]
    } else iy <- 1L:nr

    image(1L:nc, 1L:nr, x, xlim = 0.5+ c(0, nc), ylim = 0.5+ c(0, nr),
	  axes = FALSE, xlab = "", ylab = "", ...)
    axis(1, 1L:nc, labels= labCol, las= 2, line= -0.5, tick= 0, cex.axis= cexCol)
    if(!is.null(xlab)) mtext(xlab, side = 1, line = margins[1L] - 1.25)
    axis(4, iy, labels= labRow, las= 2, line= -0.5, tick= 0, cex.axis= cexRow)
    if(!is.null(ylab)) mtext(ylab, side = 4, line = margins[2L] - 1.25)

    if (!missing(add.expr))
	eval(substitute(add.expr))

    ## the two dendrograms :
    par(mar = c(margins[1L], 0, 0, 0))
    if(doRdend)
	plot(ddr, horiz = TRUE, axes = FALSE, yaxs = "i", leaflab = "none")
    else frame()

    par(mar = c(0, 0, if(!is.null(main)) 1 else 0, margins[2L]))
    if(doCdend)
	plot(ddc,		axes = FALSE, xaxs = "i", leaflab = "none")
    else if(!is.null(main)) frame()

    ## title
    if(!is.null(main)) {
        par(xpd = NA)# {we have room on the left}
        title(main, cex.main = 1.5*op[["cex.main"]])
    }

    invisible(list(rowInd = rowInd, colInd = colInd,
		   Rowv = if(keep.dendro && doRdend) ddr,
		   Colv = if(keep.dendro && doCdend) ddc ))
}
#  File src/library/stats/R/density.R
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

density <- function(x, ...) UseMethod("density")

density.default <-
    function(x, bw = "nrd0", adjust = 1,
	     kernel = c("gaussian", "epanechnikov", "rectangular",
	     "triangular", "biweight", "cosine", "optcosine"),
	     weights = NULL, window = kernel, width,
	     give.Rkern = FALSE,
	     n = 512, from, to, cut = 3, na.rm = FALSE, ...)
{
    if(length(list(...)))
	warning("non-matched further arguments are disregarded")
    if(!missing(window) && missing(kernel))
	kernel <- window
    kernel <- match.arg(kernel)
    if(give.Rkern)
        ##-- sigma(K) * R(K), the scale invariant canonical bandwidth:
        return(switch(kernel,
                      gaussian = 1/(2*sqrt(pi)),
                      rectangular = sqrt(3)/6,
                      triangular  = sqrt(6)/9,
                      epanechnikov= 3/(5*sqrt(5)),
                      biweight    = 5*sqrt(7)/49,
                      cosine      = 3/4*sqrt(1/3 - 2/pi^2),
                      optcosine   = sqrt(1-8/pi^2)*pi^2/16
                      ))

    if (!is.numeric(x))
        stop("argument 'x' must be numeric")
    name <- deparse(substitute(x))
    x <- as.vector(x)
    x.na <- is.na(x)
    if (any(x.na)) {
        if (na.rm) x <- x[!x.na]
        else stop("'x' contains missing values")
    }
    N <- nx <- length(x)
    x.finite <- is.finite(x)
    if(any(!x.finite)) {
        x <- x[x.finite]
        nx <- length(x) # == sum(x.finite)
    }

    ## Handle 'weights'
    if(is.null(weights))  {
        weights <- rep.int(1/nx, nx)
        totMass <- nx/N
    }
    else {
        if(length(weights) != N)
            stop("'x' and 'weights' have unequal length")
        if(!all(is.finite(weights)))
            stop("'weights' must all be finite")
        if(any(weights < 0))
            stop("'weights' must not be negative")
        wsum <- sum(weights)
        if(any(!x.finite)) {
            weights <- weights[x.finite]
            totMass <- sum(weights) / wsum
        } else totMass <- 1

        ## No error, since user may have wanted "sub-density"
        if (!isTRUE(all.equal(1, wsum)))
            warning("sum(weights) != 1  -- will not get true density")
    }

    n.user <- n
    n <- max(n, 512)
    if (n > 512) n <- 2^ceiling(log2(n)) #- to be fast with FFT

    if (missing(bw) && !missing(width)) {
        if(is.numeric(width)) {
            ## S has width equal to the length of the support of the kernel
            ## except for the gaussian where it is 4 * sd.
            ## R has bw a multiple of the sd.
            fac <- switch(kernel,
                          gaussian = 4,
                          rectangular = 2*sqrt(3),
                          triangular = 2 * sqrt(6),
                          epanechnikov = 2 * sqrt(5),
                          biweight = 2 * sqrt(7),
                          cosine = 2/sqrt(1/3 - 2/pi^2),
                          optcosine = 2/sqrt(1-8/pi^2)
                          )
            bw <- width / fac
        }
        if(is.character(width)) bw <- width
    }
    if (is.character(bw)) {
        if(nx < 2)
            stop("need at least 2 points to select a bandwidth automatically")
        bw <- switch(tolower(bw),
                     nrd0 = bw.nrd0(x),
                     nrd = bw.nrd(x),
                     ucv = bw.ucv(x),
                     bcv = bw.bcv(x),
                     sj = , "sj-ste" = bw.SJ(x, method="ste"),
                     "sj-dpi" = bw.SJ(x, method="dpi"),
                     stop("unknown bandwidth rule"))
    }
    if (!is.finite(bw)) stop("non-finite 'bw'")
    bw <- adjust * bw
    if (bw <= 0) stop("'bw' is not positive.")

    if (missing(from))
        from <- min(x) - cut * bw
    if (missing(to))
	to   <- max(x) + cut * bw
    if (!is.finite(from)) stop("non-finite 'from'")
    if (!is.finite(to)) stop("non-finite 'to'")
    lo <- from - 4 * bw
    up <- to + 4 * bw
    y <- .C("massdist",
	    x = as.double(x),
            xmass = as.double(weights),
	    nx = nx,
	    xlo = as.double(lo),
	    xhi = as.double(up),
	    y = double(2 * n),
	    ny = as.integer(n),
            PACKAGE = "stats" )$y * totMass
    kords <- seq.int(0, 2*(up-lo), length.out = 2L * n)
    kords[(n + 2):(2 * n)] <- -kords[n:2]
    kords <- switch(kernel,
		    gaussian = dnorm(kords, sd = bw),
                    ## In the following, a := bw / sigma(K0), where
                    ##	K0() is the unscaled kernel below
		    rectangular = {
                        a <- bw*sqrt(3)
                        ifelse(abs(kords) < a, .5/a, 0) },
		    triangular = {
                        a <- bw*sqrt(6) ; ax <- abs(kords)
                        ifelse(ax < a, (1 - ax/a)/a, 0) },
		    epanechnikov = {
                        a <- bw*sqrt(5) ; ax <- abs(kords)
                        ifelse(ax < a, 3/4*(1 - (ax/a)^2)/a, 0) },
		    biweight = { ## aka quartic
                        a <- bw*sqrt(7) ; ax <- abs(kords)
                        ifelse(ax < a, 15/16*(1 - (ax/a)^2)^2/a, 0) },
		    cosine = {
                        a <- bw/sqrt(1/3 - 2/pi^2)
                        ifelse(abs(kords) < a, (1+cos(pi*kords/a))/(2*a),0)},
		    optcosine = {
                        a <- bw/sqrt(1-8/pi^2)
                        ifelse(abs(kords) < a, pi/4*cos(pi*kords/(2*a))/a, 0)}
                    )
    kords <- fft( fft(y)* Conj(fft(kords)), inverse=TRUE)
    kords <- pmax.int(0, Re(kords)[1L:n]/length(y))
    xords <- seq.int(lo, up, length.out = n)
#    keep <- (xords >= from) & (xords <= to)
    x <- seq.int(from, to, length.out = n.user)
    structure(list(x = x, y = approx(xords, kords, x)$y, bw = bw, n = N,
		   call=match.call(), data.name=name, has.na = FALSE),
	      class="density")
}

plot.density <- function(x, main=NULL, xlab=NULL, ylab="Density", type="l",
			 zero.line = TRUE, ...)
{
    if(is.null(xlab))
	xlab <- paste("N =", x$n, "  Bandwidth =", formatC(x$bw))
    if(is.null(main)) main <- deparse(x$call)
    plot.default(x, main=main, xlab=xlab, ylab=ylab, type=type, ...)
    if(zero.line) abline(h=0, lwd=0.1, col = "gray")
    invisible(NULL)
}

print.density <- function(x, digits=NULL, ...)
{
    cat("\nCall:\n\t",deparse(x$call),
	"\n\nData: ",x$data.name," (",x$n," obs.);",
	"\tBandwidth 'bw' = ",formatC(x$bw,digits=digits), "\n\n",sep="")
    print(summary(as.data.frame(x[c("x","y")])), digits=digits, ...)
    invisible(x)
}
#  File src/library/stats/R/deriv.R
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

D <- function(expr, name) .Internal(D(expr, name))

deriv <- function(expr, ...) UseMethod("deriv")

deriv.formula <- function(expr, namevec, function.arg=NULL, tag=".expr",
                          hessian = FALSE, ...)
{
    if((le <- length(expr)) > 1)
	.Internal(deriv.default(expr[[le]], namevec, function.arg, tag, hessian))
    else stop("invalid formula in deriv")
}

deriv.default <- function(expr, namevec, function.arg=NULL, tag=".expr",
                          hessian = FALSE, ...)
    .Internal(deriv.default(expr, namevec, function.arg, tag, hessian))

deriv3 <- function(expr, ...) UseMethod("deriv3")

deriv3.formula <- function(expr, namevec, function.arg=NULL, tag=".expr",
                          hessian = TRUE, ...)
{
    if((le <- length(expr)) > 1)
	.Internal(deriv.default(expr[[le]], namevec, function.arg, tag, hessian))
    else stop("invalid formula in deriv")
}

deriv3.default <- function(expr, namevec, function.arg=NULL, tag=".expr",
                          hessian = TRUE, ...)
    .Internal(deriv.default(expr, namevec, function.arg, tag, hessian))

#  File src/library/stats/R/diffinv.R
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

## Copyright (C) 1997-1999  Adrian Trapletti
## Cppyright (C) 2003-2007  R Core Development Team
## This version distributed under GPL (version 2 or later)

diffinv <- function (x, ...) { UseMethod("diffinv") }

## the workhorse of diffinv.default:
diffinv.vector <- function (x, lag = 1, differences = 1, xi, ...)
{
    if (!is.vector(x)) stop ("'x' is not a vector")
    if (lag < 1 || differences < 1) stop ("bad value for 'lag' or 'differences'")
    if(missing(xi)) xi <- rep(0., lag*differences)
    if (length(xi) != lag*differences) stop ("'xi' has not the right length")
    if (differences == 1) {
        x <- as.double(x)
        xi <- as.double(xi)
        n <- length(x)
        y <- c(xi[1L:lag], double(n))
        .C("R_intgrt_vec",
           x, y=y, as.integer(lag), n, PACKAGE="stats")$y
    }
    else
        diffinv.vector(diffinv.vector(x, lag, differences-1,
                                      diff(xi, lag=lag, differences=1)),
                       lag, 1, xi[1L:lag])
}

diffinv.default <- function (x, lag = 1, differences = 1, xi, ...)
{
    if (is.matrix(x)) {
        n <- nrow(x)
        m <- ncol(x)
        y <- matrix(0, nrow = n+lag*differences, ncol = m)
        if(m >= 1) {
            if(missing(xi)) xi <- matrix(0.0, lag*differences, m)
            if(NROW(xi) != lag*differences || NCOL(xi) != m)
                stop("incorrect dimensions for 'xi'")
            for (i in 1L:m)
                y[,i] <- diffinv.vector(as.vector(x[,i]), lag, differences,
                                        as.vector(xi[,i]))
        }
    }
    else if (is.vector(x))
        y <- diffinv.vector(x, lag, differences, xi)
    else
        stop ("'x' is not a vector or matrix")
    y
}

diffinv.ts <- function (x, lag = 1, differences = 1, xi, ...)
{
    y <- diffinv.default(if(is.ts(x) && is.null(dim(x))) as.vector(x) else
                         as.matrix(x), lag, differences, xi)
    ts(y, frequency = frequency(x), end = end(x))
}

toeplitz <- function (x)
{
    if (!is.vector(x)) stop ("'x' is not a vector")
    n <- length (x)
    A <- matrix (0, n, n)
    matrix (x[abs(col(A) - row(A)) + 1], n, n)
}





#  File src/library/stats/R/dist.R
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

dist <- function(x, method="euclidean", diag=FALSE, upper=FALSE, p=2)
{
    ## account for possible spellings of euclid?an
    if(!is.na(pmatch(method, "euclidian")))
	method <- "euclidean"

    METHODS <- c("euclidean", "maximum",
		 "manhattan", "canberra", "binary", "minkowski")
    method <- pmatch(method, METHODS)
    if(is.na(method))
	stop("invalid distance method")
    if(method == -1)
	stop("ambiguous distance method")

    N <- nrow(x <- as.matrix(x))
    d <- .C("R_distance",
	    x = as.double(x),
	    nr= N,
	    nc= ncol(x),
	    d = double(N*(N - 1)/2),
	    diag  = as.integer(FALSE),
	    method= as.integer(method),
	    p = as.double(p),
	    DUP = FALSE, NAOK=TRUE, PACKAGE="stats")$d
    attr(d, "Size") <- N
    attr(d, "Labels") <- dimnames(x)[[1L]]
    attr(d, "Diag") <- diag
    attr(d, "Upper") <- upper
    attr(d, "method") <- METHODS[method]
    if(method == 6) attr(d, "p") <- p
    attr(d, "call") <- match.call()
    class(d) <- "dist"
    return(d)
}

format.dist <- function(x, ...) format(as.vector(x), ...)

as.matrix.dist <- function(x, ...)
{
    size <- attr(x, "Size")
    df <- matrix(0, size, size)
    df[row(df) > col(df)] <- x
    df <- df + t(df)
    labels <- attr(x, "Labels")
    dimnames(df) <-
	if(is.null(labels)) list(1L:size,1L:size) else list(labels,labels)
    df
}


as.dist <- function(m, diag = FALSE, upper = FALSE)
    UseMethod("as.dist")

as.dist.default <- function(m, diag = FALSE, upper = FALSE)
{
    if (inherits(m,"dist"))
	ans <- m
    else { ## matrix |-> dist
	m <- as.matrix(m)
        if(!is.numeric(m)) # coerce w/o losing attributes
            storage.mode(m) <- "numeric"
        p <- nrow(m)
        if(ncol(m) != p) warning("non-square matrix")
	ans <- m[row(m) > col(m)]
	attributes(ans) <- NULL
	if(!is.null(rownames(m)))
	    attr(ans,"Labels") <- rownames(m)
	else if(!is.null(colnames(m)))
	    attr(ans,"Labels") <- colnames(m)
	attr(ans,"Size") <- p
	attr(ans, "call") <- match.call()
	class(ans) <- "dist"
    }
    if(is.null(attr(ans,"Diag")) || !missing(diag))
	attr(ans,"Diag") <- diag
    if(is.null(attr(ans,"Upper")) || !missing(upper))
	attr(ans,"Upper") <- upper
    ans
}


print.dist <-
    function(x, diag = NULL, upper = NULL,
	     digits = getOption("digits"), justify = "none", right = TRUE, ...)
{
    if(length(x)) {
	if(is.null(diag))
	    diag <-	 if(is.null(a <- attr(x, "Diag"))) FALSE else a
	if(is.null(upper))
	    upper <- if(is.null(a <- attr(x,"Upper"))) FALSE else a

	m <- as.matrix(x)
	cf <- format(m, digits = digits, justify = justify)
	if(!upper)
	    cf[row(cf) < col(cf)] <- ""
	if(!diag)
	    cf[row(cf) == col(cf)] <- ""

	## Better: use an improved prettyNum() function -> ../../base/R/format.R
	##-	if(any((i <- m == floor(m))))
	##-	    cf[i] <- sub("0+$", "", cf[i])
	print(if(diag || upper) cf else cf[-1, -attr(x, "Size"), drop = FALSE],
	      quote = FALSE, right = right, ...)
    } else {
	cat(data.class(x),"(0)\n", sep='')
    }
    invisible(x)
}

labels.dist <- function (object, ...) attr(object,"Labels")
#  File src/library/stats/R/distn.R
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

dexp <- function(x, rate=1, log = FALSE) .Internal(dexp(x, 1/rate, log))
pexp <- function(q, rate=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(pexp(q, 1/rate, lower.tail, log.p))
qexp <- function(p, rate=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(qexp(p, 1/rate, lower.tail, log.p))
rexp <- function(n, rate=1) .Internal(rexp(n, 1/rate))

dunif <- function(x, min=0, max=1, log = FALSE)
    .Internal(dunif(x, min, max, log))
punif <- function(q, min=0, max=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(punif(q, min, max, lower.tail, log.p))
qunif <- function(p, min=0, max=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(qunif(p, min, max, lower.tail, log.p))
runif <- function(n, min=0, max=1) .Internal(runif(n, min, max))

dnorm <- function(x, mean=0, sd=1, log=FALSE) .Internal(dnorm(x, mean, sd, log))
pnorm <- function(q, mean=0, sd=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(pnorm(q, mean, sd, lower.tail, log.p))
qnorm <- function(p, mean=0, sd=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(qnorm(p, mean, sd, lower.tail, log.p))
rnorm <- function(n, mean=0, sd=1) .Internal(rnorm(n, mean, sd))

dcauchy <- function(x, location=0, scale=1, log = FALSE)
    .Internal(dcauchy(x, location, scale, log))
pcauchy <-
    function(q, location=0, scale=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(pcauchy(q, location, scale, lower.tail, log.p))
qcauchy <-
    function(p, location=0, scale=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(qcauchy(p, location, scale, lower.tail, log.p))
rcauchy <-
    function(n, location=0, scale=1) .Internal(rcauchy(n, location, scale))

dgamma <- function(x, shape, rate = 1, scale = 1/rate, log = FALSE)
    .Internal(dgamma(x, shape, scale, log))
pgamma <- function(q, shape, rate = 1, scale = 1/rate,
                   lower.tail = TRUE, log.p = FALSE)
    .Internal(pgamma(q, shape, scale, lower.tail, log.p))

qgamma <- function(p, shape, rate = 1, scale = 1/rate,
                   lower.tail = TRUE, log.p = FALSE)
    .Internal(qgamma(p, shape, scale, lower.tail, log.p))
rgamma <- function(n, shape, rate = 1, scale = 1/rate)
    .Internal(rgamma(n, shape, scale))

dlnorm <- function(x, meanlog=0, sdlog=1, log=FALSE)
    .Internal(dlnorm(x, meanlog, sdlog, log))
plnorm <- function(q, meanlog=0, sdlog=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(plnorm(q, meanlog, sdlog, lower.tail, log.p))
qlnorm <- function(p, meanlog=0, sdlog=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(qlnorm(p, meanlog, sdlog, lower.tail, log.p))
rlnorm <- function(n, meanlog=0, sdlog=1) .Internal(rlnorm(n, meanlog, sdlog))

dlogis <- function(x, location=0, scale=1, log = FALSE)
    .Internal(dlogis(x, location, scale, log))
plogis <- function(q, location=0, scale=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(plogis(q, location, scale, lower.tail, log.p))
qlogis <- function(p, location=0, scale=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(qlogis(p, location, scale, lower.tail, log.p))
rlogis <- function(n, location=0, scale=1) .Internal(rlogis(n, location, scale))

dweibull <- function(x, shape, scale=1, log = FALSE)
    .Internal(dweibull(x, shape, scale, log))
pweibull <- function(q, shape, scale=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(pweibull(q, shape, scale, lower.tail, log.p))
qweibull <- function(p, shape, scale=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(qweibull(p, shape, scale, lower.tail, log.p))
rweibull <- function(n, shape, scale=1) .Internal(rweibull(n, shape, scale))

dbeta <- function(x, shape1, shape2, ncp=0, log = FALSE) {
    if(missing(ncp)) .Internal(dbeta(x, shape1, shape2, log))
    else .Internal(dnbeta(x, shape1, shape2, ncp, log))
}
pbeta <- function(q, shape1, shape2, ncp=0, lower.tail = TRUE, log.p = FALSE) {
    if(missing(ncp)) .Internal(pbeta(q, shape1, shape2, lower.tail, log.p))
    else .Internal(pnbeta(q, shape1, shape2, ncp, lower.tail, log.p))
}
qbeta <- function(p, shape1, shape2, ncp=0, lower.tail = TRUE, log.p = FALSE) {
    if(missing(ncp)) .Internal(qbeta(p, shape1, shape2, lower.tail, log.p))
    else .Internal(qnbeta(p, shape1, shape2, ncp, lower.tail, log.p))
}
rbeta <- function(n, shape1, shape2, ncp = 0) {
    if(ncp == 0) .Internal(rbeta(n, shape1, shape2))
    else {
        X <- rchisq(n, 2*shape1, ncp =ncp)
        X/(X + rchisq(n, 2*shape2))
    }
}

dbinom <- function(x, size, prob, log = FALSE)
    .Internal(dbinom(x, size, prob, log))
pbinom <- function(q, size, prob, lower.tail = TRUE, log.p = FALSE)
    .Internal(pbinom(q, size, prob, lower.tail, log.p))
qbinom <- function(p, size, prob, lower.tail = TRUE, log.p = FALSE)
    .Internal(qbinom(p, size, prob, lower.tail, log.p))
rbinom <- function(n, size, prob) .Internal(rbinom(n, size, prob))

## Multivariate: that's why there's no C interface (yet) for d...():
dmultinom <- function(x, size=NULL, prob, log = FALSE)
{
    K <- length(prob)
    if(length(x) != K) stop("x[] and prob[] must be equal length vectors.")
    if(any(prob < 0) || (s <- sum(prob)) == 0)
	stop("probabilities cannot be negative nor all 0.")
    prob <- prob / s

    x <- as.integer(x + 0.5)
    if(any(x < 0)) stop("'x' must be non-negative")
    N <- sum(x)
    if(is.null(size)) size <- N
    else if (size != N) stop("size != sum(x), i.e. one is wrong")

    i0 <- prob == 0
    if(any(i0)) {
	if(any(x[i0] != 0))
            ##  prob[j] ==0 and x[j] > 0 ==>  "impossible" => P = 0
	    return(if(log)-Inf else 0)
	## otherwise : 'all is fine': prob[j]= 0 = x[j] ==> drop j and continue
	if(all(i0)) return(if(log)0 else 1)
	## else
	x <- x[!i0]
	prob <- prob[!i0]
    }
    r <- lgamma(size+1) + sum(x*log(prob) - lgamma(x+1))
    if(log) r else exp(r)
}
rmultinom <- function(n, size, prob) .Internal(rmultinom(n, size, prob))

dchisq <- function(x, df, ncp=0, log = FALSE) {
    if(missing(ncp)) .Internal(dchisq(x, df, log))
    else .Internal(dnchisq(x, df, ncp, log))
}
pchisq <- function(q, df, ncp=0, lower.tail = TRUE, log.p = FALSE) {
    if(missing(ncp)) .Internal(pchisq(q, df, lower.tail, log.p))
    else .Internal(pnchisq(q, df, ncp, lower.tail, log.p))
}
qchisq <- function(p, df, ncp=0, lower.tail = TRUE, log.p = FALSE) {
    if(missing(ncp)) .Internal(qchisq(p, df, lower.tail, log.p))
    else .Internal(qnchisq(p, df, ncp, lower.tail, log.p))
}
rchisq <- function(n, df, ncp=0) {
    if(missing(ncp)) .Internal(rchisq(n, df))
    else .Internal(rnchisq(n, df, ncp))
}

df <- function(x, df1, df2, ncp, log = FALSE) {
    if(missing(ncp)) .Internal(df(x, df1, df2, log))
    else .Internal(dnf(x, df1, df2, ncp, log))
}
pf <- function(q, df1, df2, ncp, lower.tail = TRUE, log.p = FALSE) {
    if(missing(ncp)) .Internal(pf(q, df1, df2, lower.tail, log.p))
    else .Internal(pnf(q, df1, df2, ncp, lower.tail, log.p))
}
qf <- function(p, df1, df2, ncp, lower.tail = TRUE, log.p = FALSE) {
    if(missing(ncp)) .Internal(qf(p, df1, df2, lower.tail, log.p))
    else .Internal(qnf(p, df1, df2, ncp, lower.tail, log.p))
}
rf <- function(n, df1, df2, ncp)
{
    if(missing(ncp)) .Internal(rf(n, df1, df2))
    else (rchisq(n, df1, ncp=ncp)/df1)/(rchisq(n, df2)/df2)
}

dgeom <- function(x, prob, log = FALSE) .Internal(dgeom(x, prob, log))
pgeom <- function(q, prob, lower.tail = TRUE, log.p = FALSE)
    .Internal(pgeom(q, prob, lower.tail, log.p))
qgeom <- function(p, prob, lower.tail = TRUE, log.p = FALSE)
    .Internal(qgeom(p, prob, lower.tail, log.p))
rgeom <- function(n, prob) .Internal(rgeom(n, prob))

dhyper <- function(x, m, n, k, log = FALSE) .Internal(dhyper(x, m, n, k, log))
phyper <- function(q, m, n, k, lower.tail = TRUE, log.p = FALSE)
    .Internal(phyper(q, m, n, k, lower.tail, log.p))
qhyper <- function(p, m, n, k, lower.tail = TRUE, log.p = FALSE)
    .Internal(qhyper(p, m, n, k, lower.tail, log.p))
rhyper <- function(nn, m, n, k) .Internal(rhyper(nn, m, n, k))

dnbinom <- function(x, size, prob, mu, log = FALSE)
{
    if (!missing(mu)) {
	if (!missing(prob)) stop("'prob' and 'mu' both specified")
	.Internal(dnbinom_mu(x, size, mu, log))
    }
    else
	.Internal(dnbinom (x, size, prob, log))
}
pnbinom <- function(q, size, prob, mu, lower.tail = TRUE, log.p = FALSE)
{
    if (!missing(mu)) {
	if (!missing(prob)) stop("'prob' and 'mu' both specified")
	.Internal(pnbinom_mu(q, size, mu, lower.tail, log.p))
    }
    else
	.Internal(pnbinom (q, size, prob, lower.tail, log.p))
}
qnbinom <- function(p, size, prob, mu, lower.tail = TRUE, log.p = FALSE)
{
    if (!missing(mu)) {
	if (!missing(prob)) stop("'prob' and 'mu' both specified")
### FIXME: implement qnbinom_mu(...)
	prob <- size/(size + mu)
    }
    .Internal(qnbinom(p, size, prob, lower.tail, log.p))
}
rnbinom <- function(n, size, prob, mu)
{
    if (!missing(mu)) {
        if (!missing(prob)) stop("'prob' and 'mu' both specified")
        .Internal(rnbinom_mu(n, size, mu))
    }
    else
        .Internal(rnbinom(n, size, prob))
}

dpois <- function(x, lambda, log = FALSE) .Internal(dpois(x, lambda, log))
ppois <- function(q, lambda, lower.tail = TRUE, log.p = FALSE)
    .Internal(ppois(q, lambda, lower.tail, log.p))
qpois <- function(p, lambda, lower.tail = TRUE, log.p = FALSE)
    .Internal(qpois(p, lambda, lower.tail, log.p))
rpois <- function(n, lambda) .Internal(rpois(n, lambda))

dt <- function(x, df, ncp, log = FALSE) {
    if(missing(ncp)) .Internal(dt(x, df, log))
    else .Internal(dnt(x, df, ncp, log))
}
pt <- function(q, df, ncp, lower.tail = TRUE, log.p = FALSE) {
    if(missing(ncp)) .Internal(pt(q, df, lower.tail, log.p))
    else .Internal(pnt(q, df, ncp, lower.tail, log.p))
}
qt <- function(p, df, ncp, lower.tail = TRUE, log.p = FALSE) {
    if(missing(ncp)) .Internal(qt(p, df, lower.tail, log.p))
    else .Internal(qnt(p, df, ncp, lower.tail, log.p))
}
rt <- function(n, df, ncp) {
    if(missing(ncp)) .Internal(rt(n, df))
    else rnorm(n, ncp)/sqrt(rchisq(n, df)/df)
}

ptukey <- function(q, nmeans, df, nranges=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(ptukey(q, nranges, nmeans, df, lower.tail, log.p))
qtukey <- function(p, nmeans, df, nranges=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(qtukey(p, nranges, nmeans, df, lower.tail, log.p))

dwilcox <- function(x, m, n, log = FALSE)
{
    on.exit(.C("wilcox_free", PACKAGE = "base"))
    .Internal(dwilcox(x, m, n, log))
}
pwilcox <- function(q, m, n, lower.tail = TRUE, log.p = FALSE)
{
    on.exit(.C("wilcox_free", PACKAGE = "base"))
    .Internal(pwilcox(q, m, n, lower.tail, log.p))
}
qwilcox <- function(p, m, n, lower.tail = TRUE, log.p = FALSE)
{
    on.exit(.C("wilcox_free", PACKAGE = "base"))
    .Internal(qwilcox(p, m, n, lower.tail, log.p))
}
rwilcox <- function(nn, m, n) .Internal(rwilcox(nn, m, n))

dsignrank <- function(x, n, log = FALSE)
{
    on.exit(.C("signrank_free", PACKAGE = "base"))
    .Internal(dsignrank(x, n, log))
}
psignrank <- function(q, n, lower.tail = TRUE, log.p = FALSE)
{
    on.exit(.C("signrank_free", PACKAGE = "base"))
    .Internal(psignrank(q, n, lower.tail, log.p))
}
qsignrank <- function(p, n, lower.tail = TRUE, log.p = FALSE)
{
    on.exit(.C("signrank_free", PACKAGE = "base"))
    .Internal(qsignrank(p, n, lower.tail, log.p))
}
rsignrank <- function(nn, n) .Internal(rsignrank(nn, n))
#  File src/library/stats/R/dummy.coef.R
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

dummy.coef <- function(object, ...) UseMethod("dummy.coef")

dummy.coef.lm <- function(object, use.na=FALSE, ...)
{
    Terms <- terms(object)
    tl <- attr(Terms, "term.labels")
    int <- attr(Terms, "intercept")
    facs <- attr(Terms, "factors")[-1, , drop=FALSE]
    Terms <- delete.response(Terms)
    vars <- all.vars(Terms)
    xl <- object$xlevels
    if(!length(xl)) {			# no factors in model
	return(as.list(coef(object)))
    }
    nxl <- rep.int(1, length(vars))
    names(nxl) <- vars
    tmp <- unlist(lapply(xl, length))
    nxl[names(tmp)] <- tmp
    lterms <- apply(facs, 2L, function(x) prod(nxl[x > 0]))
    nl <- sum(lterms)
    args <- vector("list", length(vars))
    names(args) <- vars
    for(i in vars)
	args[[i]] <- if(nxl[[i]] == 1) rep.int(1, nl)
	else factor(rep.int(xl[[i]][1L], nl), levels = xl[[i]])
    dummy <- do.call("data.frame", args)
    pos <- 0
    rn <- rep.int(tl, lterms)
    rnn <- rep.int("", nl)
    for(j in tl) {
	i <- vars[facs[, j] > 0]
	ifac <- i[nxl[i] > 1]
	if(length(ifac) == 0) {		# quantitative factor
	    rnn[pos+1] <- j
	} else if(length(ifac) == 1) {	# main effect
	    dummy[ pos+1L:lterms[j], ifac ] <- xl[[ifac]]
	    rnn[ pos+1L:lterms[j] ] <- as.character(xl[[ifac]])
	} else {			# interaction
	    tmp <- expand.grid(xl[ifac])
	    dummy[ pos+1L:lterms[j], ifac ] <- tmp
	    rnn[ pos+1L:lterms[j] ] <-
		apply(as.matrix(tmp), 1L, function(x) paste(x, collapse=":"))
	}
	pos <- pos + lterms[j]
    }
    ## some terms like poly(x,1) will give problems here, so allow
    ## NaNs and set to NA afterwards.
    mf <- model.frame(Terms, dummy, na.action=function(x)x, xlev=xl)
    mm <- model.matrix(Terms, mf, object$contrasts, xl)
    if(any(is.na(mm))) {
        warning("some terms will have NAs due to the limits of the method")
        mm[is.na(mm)] <- NA
    }
    coef <- object$coefficients
    if(!use.na) coef[is.na(coef)] <- 0
    asgn <- attr(mm,"assign")
    res <- vector("list", length(tl))
    names(res) <- tl
    for(j in seq_along(tl)) {
	keep <- asgn == j
	ans <- drop(mm[rn == tl[j], keep, drop=FALSE] %*% coef[keep])
	names(ans) <- rnn[rn == tl[j]]
	res[[j]] <- ans
    }
    if(int > 0) {
	res <- c(list(coef[int]), res)
	names(res)[1L] <- "(Intercept)"
    }
    class(res) <- "dummy_coef"
    res
}

dummy.coef.aovlist <- function(object, use.na = FALSE, ...)
{
    Terms <- terms(object, specials="Error")
    err <- attr(Terms,"specials")$Error - 1
    tl <- attr(Terms, "term.labels")[-err]
    int <- attr(Terms, "intercept")
    facs <- attr(Terms, "factors")[-c(1,1+err), -err, drop=FALSE]
    vars <- rownames(facs)
    xl <- attr(object, "xlevels")
    if(!length(xl)) {			# no factors in model
	return(as.list(coef(object)))
    }
    nxl <- rep.int(1, length(vars))
    names(nxl) <- vars
    tmp <- unlist(lapply(xl, length))
    nxl[names(tmp)] <- tmp
    lterms <- apply(facs, 2L, function(x) prod(nxl[x > 0]))
    nl <- sum(lterms)
    args <- vector("list", length(vars))
    names(args) <- vars
    for(i in vars)
	args[[i]] <- if(nxl[[i]] == 1) rep.int(1, nl)
	else factor(rep.int(xl[[i]][1L], nl), levels = xl[[i]])
    dummy <- do.call("data.frame", args)
    pos <- 0
    rn <- rep.int(tl, lterms)
    rnn <- rep.int("", nl)
    for(j in tl) {
	i <- vars[facs[, j] > 0]
	ifac <- i[nxl[i] > 1]
	if(length(ifac) == 0) {		# quantitative factor
	    rnn[pos + 1] <- j
	} else if(length(ifac) == 1) {	# main effect
	    dummy[ pos+1L:lterms[j], ifac ] <- xl[[ifac]]
	    rnn[ pos+1L:lterms[j] ] <- as.character(xl[[ifac]])
	} else {			# interaction
	    tmp <- expand.grid(xl[ifac])
	    dummy[ pos+1L:lterms[j], ifac ] <- tmp
	    rnn[ pos+1L:lterms[j] ] <-
		apply(as.matrix(tmp), 1L, function(x) paste(x, collapse=":"))
	}
	pos <- pos + lterms[j]
    }
    form <- paste("~", paste(tl, collapse = " + "))
    if (!int) form <- paste(form, "- 1")
    mm <- model.matrix(terms(formula(form)), dummy,
		       attr(object, "contrasts"), xl)
    res <- vector("list", length(object))
    names(res) <- names(object)
    tl <- c("(Intercept)", tl)
    allasgn <- attr(mm, "assign")
    for(i in names(object)) {
	coef <- object[[i]]$coefficients
	if(!use.na) coef[is.na(coef)] <- 0
	asgn <- object[[i]]$assign
	uasgn <- unique(asgn)
	tll <- tl[1 + uasgn]
	mod <- vector("list", length(tll))
	names(mod) <- tll
	for(j in uasgn) {
	    if(j == 0) {
		ans <- structure(coef[asgn == j], names="(Intercept)")
	    } else {
		ans <- drop(mm[rn == tl[1+j], allasgn == j, drop=FALSE] %*%
			    coef[asgn == j])
		names(ans) <- rnn[rn == tl[1+j]]
	    }
	    mod[[tl[1+j]]] <- ans
	}
	res[[i]] <- mod
    }
    class(res) <- "dummy_coef_list"
    res
}

print.dummy_coef <- function(x, ..., title)
{
    terms <- names(x)
    n <- length(x)
    nm <- max(sapply(x, length))
    ans <- matrix("", 2L*n, nm)
    rn <- rep.int("", 2L*n)
    line <- 0
    for (j in seq(n)) {
	this <- x[[j]]
	n1 <- length(this)
	if(n1 > 1) {
	    line <- line + 2
	    ans[line-1, 1L:n1] <- names(this)
	    ans[line, 1L:n1] <- format(this, ...)
	    rn[line-1] <- paste(terms[j], ":   ", sep="")
	} else {
	    line <- line + 1
	    ans[line, 1L:n1] <- format(this, ...)
	    rn[line] <- paste(terms[j], ":   ", sep="")
	}
    }
    rownames(ans) <- rn
    colnames(ans) <- rep.int("", nm)
    cat(if(missing(title)) "Full coefficients are" else title, "\n")
    print(ans[1L:line, , drop=FALSE], quote=FALSE, right=TRUE)
    invisible(x)
}

print.dummy_coef_list <- function(x, ...)
{
    for(strata in names(x))
	print.dummy_coef(x[[strata]], ..., title=paste("\n     Error:", strata))
    invisible(x)
}
#  File src/library/stats/R/ecdf.R
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

#### Empirical Cumulative Distribution Functions :  "ecdf"
##--  inherit from  "stepfun"

## Constructor
ecdf <- function (x)
{
    x <- sort(x) # drops NAs
    n <- length(x)
    if(n < 1) stop("'x' must have 1 or more non-missing values")
    vals <- unique(x)
    rval <- approxfun(vals, cumsum(tabulate(match(x, vals)))/n,
		      method = "constant", yleft = 0, yright = 1, f = 0,
                      ties = "ordered")
    class(rval) <- c("ecdf", "stepfun", class(rval))
    attr(rval, "call") <- sys.call()
    rval
}

print.ecdf <- function (x, digits= getOption("digits") - 2, ...)
{
    numform <- function(x) paste(formatC(x, digits=digits), collapse=", ")
    cat("Empirical CDF \nCall: ")
    print(attr(x, "call"), ...)
    n <- length(xx <- eval(expression(x), envir = environment(x)))
    i1 <- 1L:min(3L,n)
    i2 <- if(n >= 4L) max(4L,n-1L):n else integer(0L)
    cat(" x[1:",n,"] = ", numform(xx[i1]),
	if(n>3L) ", ", if(n>5L) " ..., ", numform(xx[i2]), "\n", sep = "")
    invisible(x)
}

summary.ecdf <- function(object, ...)
{
    header <- paste("Empirical CDF:	 ",
                    eval(expression(n), envir = environment(object)),
                    "unique values with summary\n")
    structure(summary(knots(object), ...),
              header = header, class = "summary.ecdf")
}

print.summary.ecdf <- function(x, ...)
{
    cat(attr(x, "header"))
    y <- unclass(x); attr(y, "header") <- NULL
    print(y, ...)
    invisible(x)
}

## add  conf.int = 0.95
## and  conf.type = c("none", "KS")
## (these argument names are compatible to Kaplan-Meier survfit() !)
## and use ./KS-confint.R 's  code !!!

plot.ecdf <- function(x, ..., ylab="Fn(x)", verticals = FALSE,
		      col.01line = "gray70", pch = 19)
{
    plot.stepfun(x, ..., ylab = ylab, verticals = verticals, pch = pch)
    abline(h = c(0,1), col = col.01line, lty = 2)
}
#  File src/library/stats/R/embed.R
#  Part of the R package, http://www.R-project.org
#
# Copyright (C) 1997-1999  Adrian Trapletti
#
# Rewritten to use R indexing (C) 1999, 2006 R Core Development Team
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Library General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, a copy is available at
# http://www.r-project.org/Licenses/

embed <- function (x, dimension = 1)
{
    if (is.matrix(x)) {
        n <- nrow(x)
        m <- ncol(x)
        if ((dimension < 1) | (dimension > n))
            stop ("wrong embedding dimension")
        y <- matrix(0.0, n - dimension + 1L, dimension * m)
        for (i in seq_len(m))
            y[, seq.int(i, by = m, length.out = dimension)] <-
                Recall (as.vector(x[,i]), dimension)
        return (y)
    } else if (is.vector(x) || is.ts(x)) {
        n <- length (x)
        if ((dimension < 1) | (dimension > n))
            stop ("wrong embedding dimension")
        m <- n - dimension + 1L
        data <- x[1L:m + rep.int(dimension:1L, rep.int(m, dimension)) - 1L]
        dim(data) <- c(m, dimension)
        return(data)
    } else
        stop ("'x' is not a vector or matrix")
}
#  File src/library/stats/R/expand.model.frame.R
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

expand.model.frame <- function(model, extras,
                               envir=environment(formula(model)),
                               na.expand=FALSE)
{
    ## don't use model$call$formula -- it might be a variable name
    f <- formula(model)
    data <- eval(model$call$data, envir)

    # new formula (there must be a better way...)
    ff <- foo ~ bar + baz
    if (is.call(extras))
        gg <- extras
    else
        gg <- parse(text=paste("~", paste(extras, collapse="+")))[[1L]]
    ff[[2L]] <- f[[2L]]
    ff[[3L]][[2L]] <- f[[3L]]
    ff[[3L]][[3L]] <- gg[[2L]]

    if (!na.expand){
        naa <- model$call$na.action
        subset <- model$call$subset
        rval <- eval(call("model.frame",ff, data = data, subset = subset, 
                      na.action = naa),envir )
    } else {
        subset <- model$call$subset
        rval <- eval(call("model.frame",ff, data = data, subset = subset, 
                          na.action = I), envir)
        oldmf <- model.frame(model)
        keep <- match(rownames(oldmf), rownames(rval))
        rval <- rval[keep, ]
        class(rval) <- "data.frame" # drop "AsIs"
    }

    return(rval)
}
#  File src/library/stats/R/factanal.R
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

## Hmm, MM thinks diag(.) needs checking { diag(vec) when length(vec)==1 !}
## However, MM does not understand that factor analysis
##   is a *multi*variate technique!
factanal <-
    function (x, factors, data = NULL, covmat = NULL, n.obs = NA,
              subset, na.action, start = NULL,
              scores = c("none", "regression", "Bartlett"),
              rotation = "varimax",
              control = NULL, ...)
{
    sortLoadings <- function(Lambda)
    {
        cn <- colnames(Lambda)
        Phi <- attr(Lambda, "covariance")
        ssq <- apply(Lambda, 2L, function(x) -sum(x^2))
        Lambda <- Lambda[, order(ssq), drop = FALSE]
        colnames(Lambda) <- cn
        neg <- colSums(Lambda) < 0
        Lambda[, neg] <- -Lambda[, neg]
        if(!is.null(Phi)) {
            unit <- ifelse(neg, -1, 1)
            attr(Lambda, "covariance") <-
                unit %*% Phi[order(ssq), order(ssq)] %*% unit
        }
        Lambda
    }
    cl <- match.call()
    na.act <- NULL
    if (is.list(covmat)) {
        if (any(is.na(match(c("cov", "n.obs"), names(covmat)))))
            stop("'covmat' is not a valid covariance list")
        cv <- covmat$cov
        n.obs <- covmat$n.obs
        have.x <- FALSE
    }
    else if (is.matrix(covmat)) {
        cv <- covmat
        have.x <- FALSE
    }
    else if (is.null(covmat)) {
        if(missing(x)) stop("neither 'x' nor 'covmat' supplied")
        have.x <- TRUE
        if(inherits(x, "formula")) {
            ## this is not a `standard' model-fitting function,
            ## so no need to consider contrasts or levels
            mt <- terms(x, data = data)
            if(attr(mt, "response") > 0)
                stop("response not allowed in formula")
            attr(mt, "intercept") <- 0
            mf <- match.call(expand.dots = FALSE)
            names(mf)[names(mf) == "x"] <- "formula"
            mf$factors <- mf$covmat <- mf$scores <- mf$start <-
                mf$rotation <- mf$control <- mf$... <- NULL
            mf[[1L]] <- as.name("model.frame")
            mf <- eval.parent(mf)
            na.act <- attr(mf, "na.action")
            if (.check_vars_numeric(mf))
                stop("factor analysis applies only to numerical variables")
            z <- model.matrix(mt, mf)
        } else {
            z <- as.matrix(x)
            if(!is.numeric(z))
                stop("factor analysis applies only to numerical variables")
            if(!missing(subset)) z <- z[subset, , drop = FALSE]
        }
        covmat <- cov.wt(z)
        cv <- covmat$cov
        n.obs <- covmat$n.obs
    }
    else stop("'covmat' is of unknown type")
    scores <- match.arg(scores)
    if(scores != "none" && !have.x)
        stop("requested scores without an 'x' matrix")
    p <- ncol(cv)
    if(p < 3) stop("factor analysis requires at least three variables")
    dof <- 0.5 * ((p - factors)^2 - p - factors)
    if(dof < 0)
        stop(gettextf("%d factors is too many for %d variables", factors, p),
             domain = NA)
    sds <- sqrt(diag(cv))
    cv <- cv/(sds %o% sds)

    cn <- list(nstart = 1, trace = FALSE, lower = 0.005)
    cn[names(control)] <- control
    more <- list(...)[c("nstart", "trace", "lower", "opt", "rotate")]
    if(length(more)) cn[names(more)] <- more

    if(is.null(start)) {
        start <- (1 - 0.5*factors/p)/diag(solve(cv))
        if((ns <- cn$nstart) > 1)
            start <- cbind(start, matrix(runif(ns-1), p, ns-1, byrow=TRUE))
    }
    start <- as.matrix(start)
    if(nrow(start) != p)
        stop(gettextf("'start' must have %d rows", p), domain = NA)
    nc <- ncol(start)
    if(nc < 1) stop("no starting values supplied")

    best <- Inf
    for (i in 1L:nc) {
        nfit <- factanal.fit.mle(cv, factors, start[, i],
                                 max(cn$lower, 0), cn$opt)
        if(cn$trace)
            cat("start", i, "value:", format(nfit$criteria[1L]),
                "uniqs:", format(as.vector(round(nfit$uniquenesses, 4))), "\n")
        if(nfit$converged && nfit$criteria[1L] < best) {
            fit <- nfit
            best <- fit$criteria[1L]
        }
    }
    if(best == Inf) stop("unable to optimize from these starting value(s)")
    load <- fit$loadings
    if(rotation != "none") {
        rot <- do.call(rotation, c(list(load), cn$rotate))
        load <- if(is.list(rot)) rot$loadings else rot
    }
    fit$loadings <- sortLoadings(load)
    class(fit$loadings) <- "loadings"
    fit$na.action <- na.act # not used currently
    if(have.x && scores != "none") {
        Lambda <- fit$loadings
        zz <- scale(z, TRUE, TRUE)
        switch(scores,
               regression = {
                   sc <- zz %*% solve(cv, Lambda)
                   if(!is.null(Phi <- attr(Lambda, "covariance")))
                       sc <- sc %*% Phi
               },
               Bartlett = {
                   d <- 1/fit$uniquenesses
                   tmp <- t(Lambda * d)
                   sc <- t(solve(tmp %*% Lambda, tmp %*% t(zz)))
               })
        rownames(sc) <- rownames(z)
        colnames(sc) <- colnames(Lambda)
        if(!is.null(na.act)) sc <- napredict(na.act, sc)
        fit$scores <- sc
    }
    if(!is.na(n.obs) && dof > 0) {
        fit$STATISTIC <- (n.obs - 1 - (2 * p + 5)/6 -
                     (2 * factors)/3) * fit$criteria["objective"]
        fit$PVAL <- pchisq(fit$STATISTIC, dof, lower.tail = FALSE)
    }
    fit$n.obs <- n.obs
    fit$call <- cl
    fit
}

factanal.fit.mle <-
    function(cmat, factors, start=NULL, lower = 0.005, control = NULL, ...)
{
    FAout <- function(Psi, S, q)
    {
        sc <- diag(1/sqrt(Psi))
        Sstar <- sc %*% S %*% sc
        E <- eigen(Sstar, symmetric = TRUE)
        L <- E$vectors[, 1L:q, drop = FALSE]
        load <- L %*% diag(sqrt(pmax(E$values[1L:q] - 1, 0)), q)
        diag(sqrt(Psi)) %*% load
    }
    FAfn <- function(Psi, S, q)
    {
        sc <- diag(1/sqrt(Psi))
        Sstar <- sc %*% S %*% sc
        E <- eigen(Sstar, symmetric = TRUE, only.values = TRUE)
        e <- E$values[-(1L:q)]
        e <- sum(log(e) - e) - q + nrow(S)
##        print(round(c(Psi, -e), 5))  # for tracing
        -e
    }
    FAgr <- function(Psi, S, q)
    {
        sc <- diag(1/sqrt(Psi))
        Sstar <- sc %*% S %*% sc
        E <- eigen(Sstar, symmetric = TRUE)
        L <- E$vectors[, 1L:q, drop = FALSE]
        load <- L %*% diag(sqrt(pmax(E$values[1L:q] - 1, 0)), q)
        load <- diag(sqrt(Psi)) %*% load
        g <- load %*% t(load) + diag(Psi) - S
        diag(g)/Psi^2
    }
    p <- ncol(cmat)
    if(is.null(start))
        start <- (1 - 0.5*factors/p)/diag(solve(cmat))
    res <- optim(start, FAfn, FAgr, method = "L-BFGS-B",
                 lower = lower, upper = 1,
                 control = c(list(fnscale=1,
                 parscale = rep(0.01, length(start))), control),
                 q = factors, S = cmat)
    Lambda <- FAout(res$par, cmat, factors)
    dimnames(Lambda) <- list(dimnames(cmat)[[1L]],
                             paste("Factor", 1L:factors, sep = ""))
    p <- ncol(cmat)
    dof <- 0.5 * ((p - factors)^2 - p - factors)
    un <- res$par
    names(un) <- colnames(cmat)
    class(Lambda) <- "loadings"
    ans <- list(converged = res$convergence == 0,
                loadings = Lambda, uniquenesses = un,
                correlation = cmat,
                criteria = c(objective = res$value, counts = res$counts),
                factors = factors, dof = dof, method = "mle")
    class(ans) <- "factanal"
    ans
}

print.loadings <- function(x, digits = 3, cutoff = 0.1, sort = FALSE, ...)
{
    Lambda <- unclass(x)
    p <- nrow(Lambda)
    factors <- ncol(Lambda)
    if (sort) {
        mx <- max.col(abs(Lambda))
        ind <- cbind(1L:p, mx)
        mx[abs(Lambda[ind]) < 0.5] <- factors + 1
        Lambda <- Lambda[order(mx, 1L:p),]
    }
    cat("\nLoadings:\n")
    fx <- format(round(Lambda, digits))
    names(fx) <- NULL
    nc <- nchar(fx[1L], type="c")
    fx[abs(Lambda) < cutoff] <- paste(rep(" ", nc), collapse = "")
    print(fx, quote = FALSE, ...)
    vx <- colSums(x^2)
    varex <- rbind("SS loadings" = vx)
    if(is.null(attr(x, "covariance"))) {
        varex <- rbind(varex, "Proportion Var" = vx/p)
        if(factors > 1)
            varex <- rbind(varex, "Cumulative Var" = cumsum(vx/p))
    }
    cat("\n")
    print(round(varex, digits))
    invisible(x)
}

print.factanal <- function(x, digits = 3, ...)
{
    cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
    cat("Uniquenesses:\n")
    print(round(x$uniquenesses, digits), ...)
    print(x$loadings, digits = digits, ...)
    if(!is.null(x$STATISTIC)) {
        factors <- x$factors
        cat("\nTest of the hypothesis that", factors, if(factors == 1)
            "factor is" else "factors are", "sufficient.\n")
        cat("The chi square statistic is", round(x$STATISTIC, 2), "on", x$dof,
            if(x$dof == 1) "degree" else "degrees",
            "of freedom.\nThe p-value is", signif(x$PVAL, 3), "\n")
    } else {
        cat(paste("\nThe degrees of freedom for the model is",
                  x$dof, "and the fit was", round(x$criteria["objective"], 4),
                  "\n"))
    }
    invisible(x)
}

varimax <- function(x, normalize = TRUE, eps = 1e-5)
{
    nc <- ncol(x)
    if(nc < 2) return(x)
    if(normalize) {
        sc <- sqrt(drop(apply(x, 1L, function(x) sum(x^2))))
        x <- x/sc
    }
    p <- nrow(x)
    TT <- diag(nc)
    d <- 0
    for(i in 1L:1000) {
        z <- x %*% TT
        B  <- t(x) %*% (z^3 - z %*% diag(drop(rep(1, p) %*% z^2))/p)
        sB <- La.svd(B)
        TT <- sB$u %*% sB$vt
        dpast <- d
        d <- sum(sB$d)
        if(d < dpast * (1 + eps)) break
    }
    z <- x %*% TT
    if(normalize) z <- z * sc
    dimnames(z) <- dimnames(x)
    class(z) <- "loadings"
    list(loadings = z, rotmat = TT)
}

promax <- function(x, m = 4)
{
    if(ncol(x) < 2) return(x)
    dn <- dimnames(x)
    xx <- varimax(x)
    x <- xx$loadings
    Q <- x * abs(x)^(m-1)
    U <- lm.fit(x, Q)$coefficients
    d <- diag(solve(t(U) %*% U))
    U <- U %*% diag(sqrt(d))
    dimnames(U) <- NULL
    z <- x %*% U
    U <- xx$rotmat %*% U
    dimnames(z) <- dn
    class(z) <- "loadings"
    list(loadings = z, rotmat = U)
}
#  File src/library/stats/R/family.R
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

family <- function(object, ...) UseMethod("family")

print.family <- function(x, ...)
{
    cat("\nFamily:", x$family, "\n")
    cat("Link function:", x$link, "\n\n")
    invisible(x)
}

power <- function(lambda = 1)
{
    if(!is.numeric(lambda) || is.na(lambda))
        stop("invalid argument 'lambda'")
    if(lambda <= 0) return(make.link("log"))
    if(lambda == 1) return(make.link("identity"))
    linkfun <- function(mu) mu^lambda
    linkinv <- function(eta)
        pmax(eta^(1/lambda), .Machine$double.eps)
    mu.eta <- function(eta)
        pmax((1/lambda) * eta^(1/lambda - 1), .Machine$double.eps)
    valideta <- function(eta) all(eta>0)
    link <- paste("mu^", round(lambda, 3), sep="")
    structure(list(linkfun = linkfun, linkinv = linkinv,
                   mu.eta = mu.eta, valideta = valideta, name = link),
              class="link-glm")
}

## Written by Simon Davies Dec 1995
## Modified by Thomas Lumley 26 Apr 97
## added valideta(eta) function..
make.link <- function (link)
{
    switch(link,
           "logit" = {
               linkfun <- function(mu)
                   .Call("logit_link", mu, PACKAGE="stats")
               linkinv <- function(eta)
                   .Call("logit_linkinv", eta, PACKAGE="stats")
               mu.eta <- function(eta)
                   .Call("logit_mu_eta", eta, PACKAGE="stats")
               valideta <- function(eta) TRUE
           },
           "probit" = {
               linkfun <- function(mu) qnorm(mu)
               linkinv <- function(eta) {
                   thresh <- - qnorm(.Machine$double.eps)
                   eta <- pmin(pmax(eta, -thresh), thresh)
                   pnorm(eta)
               }
               mu.eta <- function(eta)
                   pmax(dnorm(eta),.Machine$double.eps)
               valideta <- function(eta) TRUE
           },
           "cauchit" = {
               linkfun <- function(mu) qcauchy(mu)
               linkinv <- function(eta) {
                   thresh <- -qcauchy(.Machine$double.eps)
                   eta <- pmin(pmax(eta, -thresh), thresh)
                   pcauchy(eta)
               }
               mu.eta <- function(eta)
                   pmax(dcauchy(eta), .Machine$double.eps)
               valideta <- function(eta) TRUE
           },
           "cloglog" = {
               linkfun <- function(mu) log(-log(1 - mu))
               linkinv <- function(eta)
                   pmax(pmin(-expm1(-exp(eta)), 1 - .Machine$double.eps),
                        .Machine$double.eps)
               mu.eta <- function(eta) {
                   eta <- pmin(eta, 700)
                   pmax(exp(eta) * exp(-exp(eta)), .Machine$double.eps)
               }
               valideta <- function(eta) TRUE
           },
           "identity" = {
               linkfun <- function(mu) mu
               linkinv <- function(eta) eta
               mu.eta <- function(eta) rep.int(1, length(eta))
               valideta <- function(eta) TRUE
           },
           "log" = {
               linkfun <- function(mu) log(mu)
               linkinv <- function(eta)
                   pmax(exp(eta), .Machine$double.eps)
               mu.eta <- function(eta)
                   pmax(exp(eta), .Machine$double.eps)
               valideta <- function(eta) TRUE
           },
           "sqrt" = {
               linkfun <- function(mu) sqrt(mu)
               linkinv <- function(eta) eta^2
               mu.eta <- function(eta) 2 * eta
               valideta <- function(eta) all(eta>0)
           },
           "1/mu^2" = {
               linkfun <- function(mu) 1/mu^2
               linkinv <- function(eta) 1/sqrt(eta)
               mu.eta <- function(eta) -1/(2 * eta^1.5)
               valideta <- function(eta) all(eta>0)
           },
           "inverse" = {
               linkfun <- function(mu) 1/mu
               linkinv <- function(eta) 1/eta
               mu.eta <- function(eta) -1/(eta^2)
               valideta <- function(eta) all(eta!=0)
           },
           ## else :
           stop(sQuote(link), " link not recognised")
           )# end switch(.)
    structure(list(linkfun = linkfun, linkinv = linkinv,
                   mu.eta = mu.eta, valideta = valideta, name = link),
              class="link-glm")
}

poisson <- function (link = "log")
{
    linktemp <- substitute(link)
    ## idea is that we can specify a character string or a name for
    ## the standard links, and if the name is not one of those, look
    ## for an object of the appropiate class.
    if (!is.character(linktemp)) {
	linktemp <- deparse(linktemp)
        ## the idea here seems to be that we can have a character variable
        ## 'link' naming the link (undocumented).  Deprecate in 2.4.0.
	if (linktemp == "link") {
            warning("use of poisson(link=link) is deprecated\n", domain = NA)
	    linktemp <- eval(link)
            if(!is.character(linktemp) || length(linktemp) != 1)
                stop("'link' is invalid", domain=NA)
        }
    }
    okLinks <- c("log", "identity", "sqrt")
    if (linktemp %in% okLinks)
	stats <- make.link(linktemp)
    else if (is.character(link)) {
        stats <- make.link(link)
        linktemp <- link
    } else {
        ## what else shall we allow?  At least objects of class link-glm.
        if(inherits(link, "link-glm")) {
            stats <- link
            if(!is.null(stats$name)) linktemp <- stats$name
        } else {
            stop(gettextf('link "%s" not available for poisson family; available links are %s',
			  linktemp, paste(sQuote(okLinks), collapse =", ")),
		 domain = NA)
        }
    }
    variance <- function(mu) mu
    validmu <- function(mu) all(mu>0)
    dev.resids <- function(y, mu, wt)
        2 * wt * (y * log(ifelse(y == 0, 1, y/mu)) - (y - mu))
    aic <- function(y, n, mu, wt, dev) -2*sum(dpois(y, mu, log=TRUE)*wt)
    initialize <- expression({
	if (any(y < 0))
	    stop("negative values not allowed for the Poisson family")
	n <- rep.int(1, nobs)
	mustart <- y + 0.1
    })
    simfun <- function(object, nsim) {
        ## A Poisson GLM has dispersion fixed at 1, so prior weights
        ## do not have a simple unambiguous interpretation:
        ## they might be frequency weights or indicate averages.
        wts <- object$prior.weights
        if (any(wts != 1)) warning("ignoring prior weights")
        ftd <- fitted(object)
        rpois(nsim*length(ftd), ftd)
    }
    structure(list(family = "poisson",
		   link = linktemp,
		   linkfun = stats$linkfun,
		   linkinv = stats$linkinv,
		   variance = variance,
		   dev.resids = dev.resids,
		   aic = aic,
		   mu.eta = stats$mu.eta,
		   initialize = initialize,
		   validmu = validmu,
		   valideta = stats$valideta,
                   simulate = simfun),
	      class = "family")
}

quasipoisson <- function (link = "log")
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) {
	linktemp <- deparse(linktemp)
        ## the idea here seems to be that we can have a character variable
        ## 'link' naming the link.
	if (linktemp == "link") {
            warning("use of quasipoisson(link=link) is deprecated\n",
                    domain = NA)
	    linktemp <- eval(link)
            if(!is.character(linktemp) || length(linktemp) != 1)
                stop("'link' is invalid", domain=NA)
        }
    }

    okLinks <- c("log", "identity", "sqrt")
    if (linktemp %in% okLinks)
        stats <- make.link(linktemp)
    else if (is.character(link)) {
        stats <- make.link(link)
        linktemp <- link
    } else {
        ## what else shall we allow?  At least objects of class link-glm.
        if(inherits(link, "link-glm")) {
            stats <- link
            if(!is.null(stats$name)) linktemp <- stats$name
        } else {
	    stop(gettextf('link "%s" not available for quasipoisson family; available links are %s',
			  linktemp, paste(sQuote(okLinks), collapse =", ")),
		 domain = NA)
        }
    }
    variance <- function(mu) mu
    validmu <- function(mu) all(mu>0)
    dev.resids <- function(y, mu, wt)
	2 * wt * (y * log(ifelse(y == 0, 1, y/mu)) - (y - mu))
    aic <- function(y, n, mu, wt, dev) NA
    initialize <- expression({
	if (any(y < 0))
	    stop("negative values not allowed for the quasiPoisson family")
	n <- rep.int(1, nobs)
	mustart <- y + 0.1
    })
    structure(list(family = "quasipoisson",
		   link = linktemp,
		   linkfun = stats$linkfun,
		   linkinv = stats$linkinv,
		   variance = variance,
		   dev.resids = dev.resids,
		   aic = aic,
		   mu.eta = stats$mu.eta,
		   initialize = initialize,
		   validmu = validmu,
		   valideta = stats$valideta),
	      class = "family")
}

gaussian <- function (link = "identity")
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) {
	linktemp <- deparse(linktemp)
        ## the idea here seems to be that we can have a character variable
        ## 'link' naming the link.
	if (linktemp == "link") {
            warning("use of gaussian(link=link) is deprecated\n", domain = NA)
	    linktemp <- eval(link)
            if(!is.character(linktemp) || length(linktemp) != 1)
                stop("'link' is invalid", domain=NA)
        }
    }
    okLinks <- c("inverse", "log", "identity")
    if (linktemp %in% okLinks)
	stats <- make.link(linktemp)
    else if (is.character(link)) {
        stats <- make.link(link)
        linktemp <- link
    } else {
        ## what else shall we allow?  At least objects of class link-glm.
        if(inherits(link, "link-glm")) {
            stats <- link
            if(!is.null(stats$name)) linktemp <- stats$name
        } else {
	    stop(gettextf('link "%s" not available for gaussian family; available links are %s',
			  linktemp, paste(sQuote(okLinks), collapse =", ")),
		 domain = NA)
        }
    }
    structure(list(family = "gaussian",
		   link = linktemp,
		   linkfun = stats$linkfun,
		   linkinv = stats$linkinv,
		   variance = function(mu) rep.int(1, length(mu)),
		   dev.resids = function(y, mu, wt) wt * ((y - mu)^2),
		   aic =	function(y, n, mu, wt, dev) {
                       nobs <- length(y)
                       nobs*(log(dev/nobs*2*pi)+1)+2 - sum(log(wt))
                   },
		   mu.eta = stats$mu.eta,
		   initialize = expression({
		       n <- rep.int(1, nobs)
                       if(is.null(etastart) && is.null(start) &&
                          is.null(mustart) &&
                          ((family$link == "inverse" && any(y == 0)) ||
                          (family$link == "log" && any(y <= 0))))
                           stop("cannot find valid starting values: please specify some")

		       mustart <- y }),
		   validmu = function(mu) TRUE,
		   valideta = stats$valideta
		   ),
	      class = "family")
}

binomial <- function (link = "logit")
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) {
	linktemp <- deparse(linktemp)
        ## the idea here seems to be that we can have a character variable
        ## 'link' naming the link.
	if (linktemp == "link") {
            warning("use of binomial(link=link) is deprecated\n", domain = NA)
	    linktemp <- eval(link)
            if(!is.character(linktemp) || length(linktemp) != 1)
                stop("'link' is invalid", domain=NA)
        }
    }
    okLinks <- c("logit", "probit", "cloglog", "cauchit", "log")
    if (linktemp %in% okLinks)
        stats <- make.link(linktemp)
    else if (is.character(link)) {
        stats <- make.link(link)
        linktemp <- link
    } else {
        ## what else shall we allow?  At least objects of class link-glm.
        if(inherits(link, "link-glm")) {
            stats <- link
            if(!is.null(stats$name)) linktemp <- stats$name
        } else {
	    stop(gettextf('link "%s" not available for binomial family; available links are %s',
			  linktemp, paste(sQuote(okLinks), collapse =", ")),
	     domain = NA)
        }
    }
    variance <- function(mu) mu * (1 - mu)
    validmu <- function(mu) all(mu>0) && all(mu<1)
    dev.resids <- function(y, mu, wt)
        .Call("binomial_dev_resids", y, mu, wt, PACKAGE="stats")
    aic <- function(y, n, mu, wt, dev) {
        m <- if(any(n > 1)) n else wt
	-2*sum(ifelse(m > 0, (wt/m), 0)*
               dbinom(round(m*y), round(m), mu, log=TRUE))
    }
    initialize <- expression({
	if (NCOL(y) == 1) {
	    ## allow factors as responses
	    ## added BDR 29/5/98
	    if (is.factor(y)) y <- y != levels(y)[1L]
	    n <- rep.int(1, nobs)
            ## anything, e.g. NA/NaN, for cases with zero weight is OK.
            y[weights == 0] <- 0
	    if (any(y < 0 | y > 1))
		stop("y values must be 0 <= y <= 1")
            mustart <- (weights * y + 0.5)/(weights + 1)
            m <- weights * y
            if(any(abs(m - round(m)) > 1e-3))
                warning("non-integer #successes in a binomial glm!")
	}
	else if (NCOL(y) == 2) {
            if(any(abs(y - round(y)) > 1e-3))
                warning("non-integer counts in a binomial glm!")
	    n <- y[, 1] + y[, 2]
	    y <- ifelse(n == 0, 0, y[, 1]/n)
	    weights <- weights * n
            mustart <- (n * y + 0.5)/(n + 1)
	}
	else stop("for the binomial family, y must be a vector of 0 and 1\'s\n",
                  "or a 2 column matrix where col 1 is no. successes and col 2 is no. failures")
    })
    simfun <- function(object, nsim) {
        ftd <- fitted(object)
        n <- length(ftd)
        ntot <- n*nsim
        wts <- object$prior.weights
        if (any(wts %% 1 != 0))
            stop("cannot simulate from non-integer prior.weights")
        ## Try to fathom out if the original data were
        ## proportions, a factor or a two-column matrix
        if (!is.null(m <- object$model)) {
            y <- model.response(m)
            if(is.factor(y)) {
                ## ignote weights
                yy <- factor(1+rbinom(ntot, size = 1, prob = ftd),
                             labels = levels(y))
                split(yy, rep(seq_len(nsim), each = n))
            } else if(is.matrix(y) && ncol(y) == 2) {
                yy <- vector("list", nsim)
                for (i in seq_len(nsim)) {
                    Y <- rbinom(n, size = wts, prob = ftd)
                    YY <- cbind(Y, wts - Y)
                    colnames(YY) <- colnames(y)
                    yy[[i]] <- YY
                }
                yy
            } else
            rbinom(ntot, size = wts, prob = ftd)/wts
        } else rbinom(ntot, size = wts, prob = ftd)/wts
    }
    structure(list(family = "binomial",
		   link = linktemp,
		   linkfun = stats$linkfun,
		   linkinv = stats$linkinv,
		   variance = variance,
		   dev.resids = dev.resids,
		   aic = aic,
		   mu.eta = stats$mu.eta,
		   initialize = initialize,
		   validmu = validmu,
		   valideta = stats$valideta,
                   simulate = simfun),
	      class = "family")
}

quasibinomial <- function (link = "logit")
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) {
	linktemp <- deparse(linktemp)
        ## the idea here seems to be that we can have a character variable
        ## 'link' naming the link.
	if (linktemp == "link") {
            warning("use of quasibinomial(link=link) is deprecated\n", domain = NA)
	    linktemp <- eval(link)
            if(!is.character(linktemp) || length(linktemp) != 1)
                stop("'link' is invalid", domain=NA)
        }
    }

    okLinks <- c("logit", "probit", "cloglog", "cauchit", "log")
    if (linktemp %in% okLinks)
        stats <- make.link(linktemp)
    else if (is.character(link)) {
        stats <- make.link(link)
        linktemp <- link
    } else {
        ## what else shall we allow?  At least objects of class link-glm.
        if(inherits(link, "link-glm")) {
            stats <- link
            if(!is.null(stats$name)) linktemp <- stats$name
        } else {
	    stop(gettextf('link "%s" not available for quasibinomial family; available links are %s',
			  linktemp, paste(sQuote(okLinks), collapse =", ")),
	     domain = NA)
        }
    }
    variance <- function(mu) mu * (1 - mu)
    validmu <- function(mu) all(mu>0) && all(mu<1)
    dev.resids <- function(y, mu, wt)
	2 * wt * (y * log(ifelse(y == 0, 1, y/mu)) +
		  (1 - y) * log(ifelse(y == 1, 1, (1 - y)/(1 - mu))))
    aic <- function(y, n, mu, wt, dev) NA
    initialize <- expression({
	if (NCOL(y) == 1) {
	    if (is.factor(y)) y <- y != levels(y)[1L]
	    n <- rep.int(1, nobs)
	    if (any(y < 0 | y > 1))
		stop("y values must be 0 <= y <= 1")
            mustart <- (weights * y + 0.5)/(weights + 1)
	}
	else if (NCOL(y) == 2) {
	    n <- y[, 1] + y[, 2]
	    y <- ifelse(n == 0, 0, y[, 1]/n)
	    weights <- weights * n
            mustart <- (n * y + 0.5)/(n + 1)
	}
	else stop("for the quasibinomial family, y must be a vector of 0 and 1\'s\n",
                  "or a 2 column matrix where col 1 is no. successes and col 2 is no. failures")
    })
    structure(list(family = "quasibinomial",
		   link = linktemp,
		   linkfun = stats$linkfun,
		   linkinv = stats$linkinv,
		   variance = variance,
		   dev.resids = dev.resids,
		   aic = aic,
		   mu.eta = stats$mu.eta,
		   initialize = initialize,
		   validmu = validmu,
		   valideta = stats$valideta),
	      class = "family")
}

Gamma <- function (link = "inverse")
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) {
	linktemp <- deparse(linktemp)
        ## the idea here seems to be that we can have a character variable
        ## 'link' naming the link.
	if (linktemp == "link") {
            warning("use of Gamma(link=link) is deprecated\n", domain = NA)
	    linktemp <- eval(link)
            if(!is.character(linktemp) || length(linktemp) != 1)
                stop("'link' is invalid", domain=NA)
        }
    }
    okLinks <- c("inverse", "log", "identity")
    if (linktemp %in% okLinks)
	stats <- make.link(linktemp)
    else if(is.character(link)) stats <- make.link(link)
    else {
        ## what else shall we allow?  At least objects of class link-glm.
        if(inherits(link, "link-glm")) {
            stats <- link
            if(!is.null(stats$name)) linktemp <- stats$name
        } else {
	    stop(gettextf('link "%s" not available for gamma family; available links are %s',
			  linktemp, paste(sQuote(okLinks), collapse =", ")),
	     domain = NA)
        }
    }
    variance <- function(mu) mu^2
    validmu <- function(mu) all(mu>0)
    dev.resids <- function(y, mu, wt)
	-2 * wt * (log(ifelse(y == 0, 1, y/mu)) - (y - mu)/mu)
    aic <- function(y, n, mu, wt, dev){
	n <- sum(wt)
	disp <- dev/n
	-2*sum(dgamma(y, 1/disp, scale=mu*disp, log=TRUE)*wt) + 2
    }
    initialize <- expression({
	if (any(y <= 0))
	    stop("non-positive values not allowed for the gamma family")
	n <- rep.int(1, nobs)
	mustart <- y
    })
    simfun <- function(object, nsim) {
        wts <- object$prior.weights
        if (any(wts != 1)) message("using weights as shape parameters")
        ftd <- fitted(object)
        shape <- MASS::gamma.shape(object)$alpha * wts
        rgamma(nsim*length(ftd), shape = shape, rate = shape/ftd)
    }
    structure(list(family = "Gamma",
		   link = linktemp,
		   linkfun = stats$linkfun,
		   linkinv = stats$linkinv,
		   variance = variance,
		   dev.resids = dev.resids,
		   aic = aic,
		   mu.eta = stats$mu.eta,
		   initialize = initialize,
		   validmu = validmu,
		   valideta = stats$valideta,
                   simulate = simfun),
	      class = "family")
}

inverse.gaussian <- function(link = "1/mu^2")
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) {
	linktemp <- deparse(linktemp)
        ## the idea here seems to be that we can have a character variable
        ## 'link' naming the link.
	if (linktemp == "link") {
            warning("use of inverse.gaussian(link=link) is deprecated\n",
                    domain = NA)
	    linktemp <- eval(link)
            if(!is.character(linktemp) || length(linktemp) != 1)
                stop("'link' is invalid", domain=NA)
        }
    }
    okLinks <- c("inverse", "log", "identity", "1/mu^2")
    if (linktemp %in% okLinks)
	stats <- make.link(linktemp)
    else if (is.character(link)) {
        stats <- make.link(link)
        linktemp <- link
    } else {
        ## what else shall we allow?  At least objects of class link-glm.
        if(inherits(link, "link-glm")) {
            stats <- link
            if(!is.null(stats$name)) linktemp <- stats$name
        } else {
	    stop(gettextf('link "%s" not available for inverse.gaussian family; available links are %s',
			  linktemp, paste(sQuote(okLinks), collapse =", ")),
                 domain = NA)
        }
    }
    variance <- function(mu) mu^3
    dev.resids <- function(y, mu, wt)  wt*((y - mu)^2)/(y*mu^2)
    aic <- function(y, n, mu, wt, dev)
	sum(wt)*(log(dev/sum(wt)*2*pi)+1)+3*sum(log(y)*wt)+2
    initialize <- expression({
	if(any(y <= 0))
	    stop("positive values only allowed for the inverse.gaussian family")
	n <- rep.int(1, nobs)
	mustart <- y
    })
    validmu <- function(mu) TRUE
    simfun <- function(object, nsim) {
        if(is.null(tryCatch(loadNamespace("SuppDists"),
                            error = function(e) NULL)))
            stop("Need CRAN package 'SuppDists' for 'inverse.gaussian' family")
        wts <- object$prior.weights
        if (any(wts != 1)) message("using weights as inverse variances")
        ftd <- fitted(object)
        SuppDists::rinvGauss(nsim * length(ftd), nu = ftd,
                             lambda = wts/summary(object)$dispersion)
    }

    structure(list(family = "inverse.gaussian",
		   link = linktemp,
		   linkfun = stats$linkfun,
		   linkinv = stats$linkinv,
		   variance = variance,
		   dev.resids = dev.resids,
		   aic = aic,
		   mu.eta = stats$mu.eta,
		   initialize = initialize,
		   validmu = validmu,
		   valideta = stats$valideta,
                   simulate = simfun),
	      class = "family")
}

quasi <- function (link = "identity", variance = "constant")
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) linktemp <- deparse(linktemp)
    if (linktemp %in% c("logit", "probit", "cloglog", "identity",
                        "inverse", "log", "1/mu^2", "sqrt"))
        stats <- make.link(linktemp)
    else if (is.character(link)) {
        stats <- make.link(link)
        linktemp <- link
    } else {
        stats <- link
        linktemp <- if(!is.null(stats$name)) stats$name else deparse(linktemp)
    }
    vtemp <- substitute(variance)
    if (!is.character(vtemp)) vtemp <- deparse(vtemp)
    variance_nm <- vtemp
    switch(vtemp,
           "constant" = {
               varfun <- function(mu) rep.int(1, length(mu))
               dev.resids <- function(y, mu, wt) wt * ((y - mu)^2)
                   validmu <- function(mu) TRUE
               initialize <- expression({n <- rep.int(1, nobs); mustart <- y})
           },
           "mu(1-mu)" = {
               varfun <- function(mu) mu * (1 - mu)
               validmu <- function(mu) all(mu>0) && all(mu<1)
               dev.resids <- function(y, mu, wt)
                   2 * wt * (y * log(ifelse(y == 0, 1, y/mu)) +
                             (1 - y) * log(ifelse(y == 1, 1, (1 - y)/(1 - mu))))
               initialize <- expression({n <- rep.int(1, nobs)
                                         mustart <- pmax(0.001, pmin(0.999, y))})
           },
           "mu" = {
               varfun <- function(mu) mu
               validmu <- function(mu) all(mu>0)
               dev.resids <- function(y, mu, wt)
                   2 * wt * (y * log(ifelse(y == 0, 1, y/mu)) - (y - mu))
               ## 0.1 fudge here matches poisson: S has 1/6.
               initialize <- expression({n <- rep.int(1, nobs)
                                         mustart <- y + 0.1 * (y == 0)})
           },
           "mu^2" = {
               varfun <- function(mu) mu^2
               validmu <- function(mu) all(mu>0)
               dev.resids <- function(y, mu, wt)
		   pmax(-2 * wt * (log(ifelse(y == 0, 1, y)/mu) - (y - mu)/mu), 0)
               initialize <- expression({n <- rep.int(1, nobs)
                                         mustart <- y + 0.1 * (y == 0)})
           },
           "mu^3" = {
               varfun <- function(mu) mu^3
               validmu <- function(mu) all(mu>0)
               dev.resids <- function(y, mu, wt)
                   wt * ((y - mu)^2)/(y * mu^2)
               initialize <- expression({n <- rep.int(1, nobs)
                                         mustart <- y + 0.1 * (y == 0)})
           },
           variance_nm <- NA
           )# end switch(.)

    if(is.na(variance_nm)) {
        if(is.character(variance))
            stop(gettextf('\'variance\' "%s" is invalid: possible values are "mu(1-mu)", "mu", "mu^2", "mu^3" and "constant"', variance_nm), domain = NA)
        ## so we really meant the object.
        varfun <- variance$varfun
        validmu <- variance$validmu
        dev.resids <- variance$dev.resids
        initialize <- variance$initialize
        variance_nm <- variance$name
    }
    aic <- function(y, n, mu, wt, dev) NA
    structure(list(family = "quasi",
		   link = linktemp,
		   linkfun = stats$linkfun,
		   linkinv = stats$linkinv,
		   variance = varfun,
		   dev.resids = dev.resids,
		   aic = aic,
		   mu.eta = stats$mu.eta,
		   initialize = initialize,
		   validmu = validmu,
		   valideta = stats$valideta,
                   ## character form of the var fun is needed for gee
                   varfun = variance_nm),
	      class = "family")
}
#  File src/library/stats/R/fft.R
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

fft <- function(z, inverse=FALSE)
    .Internal(fft(z, inverse))

mvfft <- function(z, inverse=FALSE)
    .Internal(mvfft(z, inverse))

nextn <- function(n, factors=c(2,3,5))
    .Internal(nextn(n, factors))

convolve <- function(x, y, conj=TRUE, type=c("circular","open","filter")) {
    type <- match.arg(type)
    n <- length(x)
    ny <- length(y)
    Real <- is.numeric(x) && is.numeric(y)
    ## switch(type, circular = ..., )
    if(type == "circular") {
        if(ny != n)
            stop("length mismatch in convolution")
    }
    else { ## "open" or "filter": Pad with zeros
        n1 <- ny - 1
        x <- c(rep.int(0, n1), x)
        n <- length(y <- c(y, rep.int(0, n - 1)))# n = nx+ny-1
    }
    x <- fft(fft(x)* (if(conj)Conj(fft(y)) else fft(y)), inverse=TRUE)
    if(type == "filter")
        (if(Real) Re(x) else x)[-c(1L:n1, (n-n1+1L):n)]/n
    else
        (if(Real) Re(x) else x)/n
}

#  File src/library/stats/R/filter.R
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

filter <- function(x, filter, method = c("convolution", "recursive"),
                   sides = 2, circular = FALSE, init=NULL)
{
    method <- match.arg(method)
    x <- as.ts(x)
    xtsp <- tsp(x)
    x <- as.matrix(x)
    n <- nrow(x)
    nser <- ncol(x)
    nfilt <- length(filter)
    if(any(is.na(filter))) stop("missing values in 'filter'")
    y <- matrix(NA, n, nser)
    if(method == "convolution") {
        if(nfilt > n) stop("'filter' is longer than time series")
        if(sides != 1 && sides != 2)
            stop("argument 'sides' must be 1 or 2")
        for (i in 1L:nser)
            y[, i] <- .C("filter1",
                         as.double(x[,i]),
                         as.integer(n),
                         as.double(filter),
                         as.integer(nfilt),
                         as.integer(sides),
                         as.integer(circular),
                         out=double(n), NAOK=TRUE,
                         PACKAGE = "stats")$out
    } else {
        if(missing(init)) {
            init <- matrix(0, nfilt, nser)
        } else {
            ni <- NROW(init)
            if(ni != nfilt)
                stop("length of 'init' must equal length of 'filter'")
            if(NCOL(init) != 1 && NCOL(init) != nser)
                stop(gettextf("'init'; must have 1 or %d cols", nser),
                     domain = NA)
            if(!is.matrix(init)) init <- matrix(init, nfilt, nser)
        }
        for (i in 1L:nser)
            y[, i] <- .C("filter2",
                         as.double(x[,i]),
                         as.integer(n),
                         as.double(filter),
                         as.integer(nfilt),
                         out=as.double(c(rev(init[, i]), double(n))),
                         NAOK=TRUE,
                         PACKAGE = "stats")$out[-(1L:nfilt)]
    }
    y <- drop(y)
    tsp(y) <- xtsp
    class(y) <- if(nser > 1) c("mts", "ts") else "ts"
    y
}

#  File src/library/stats/R/fisher.test.R
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

fisher.test <-
function(x, y = NULL, workspace = 200000, hybrid = FALSE,
         control = list(), or = 1, alternative = "two.sided",
         conf.int = TRUE, conf.level = 0.95,
         simulate.p.value = FALSE, B = 2000)
{
    DNAME <- deparse(substitute(x))
    METHOD <- "Fisher's Exact Test for Count Data"
    if(is.data.frame(x))
        x <- as.matrix(x)
    if(is.matrix(x)) {
        if(any(dim(x) < 2))
            stop("'x' must have at least 2 rows and columns")
        if(!is.numeric(x) || any(x < 0) || any(is.na(x)))
            stop("all entries of 'x' must be nonnegative and finite")
        if(!is.integer(x)) {
            xo <- x
            x <- round(x)
            if(any(x > .Machine$integer.max))
                stop("'x' has entries too large to be integer")
            if(!identical(TRUE, (ax <- all.equal(xo, x))))
                warning("'x' has been rounded to integer: ", ax)
            storage.mode(x) <- "integer"
        }
    }
    else {
        if(is.null(y))
            stop("if 'x' is not a matrix, 'y' must be given")
        if(length(x) != length(y))
            stop("'x' and 'y' must have the same length")
        DNAME <- paste(DNAME, "and", deparse(substitute(y)))
        OK <- complete.cases(x, y)
        x <- factor(x[OK])
        y <- factor(y[OK])
        if((nlevels(x) < 2) || (nlevels(y) < 2))
            stop("'x' and 'y' must have at least 2 levels")
        x <- table(x, y)
    }
    ## x is integer
    con <- list(mult = 30)
    con[names(control)] <- control
    if((mult <- as.integer(con$mult)) < 2)
        stop("'mult' must be integer >= 2, typically = 30")

    nr <- nrow(x)
    nc <- ncol(x)

    if((nr == 2) && (nc == 2)) {
        alternative <- char.expand(alternative,
                                   c("two.sided", "less", "greater"))
        if(length(alternative) > 1 || is.na(alternative))
            stop("alternative must be \"two.sided\", \"less\" or \"greater\"")
        if(!((length(conf.level) == 1) && is.finite(conf.level) &&
             (conf.level > 0) && (conf.level < 1)))
            stop("'conf.level' must be a single number between 0 and 1")
        if(!missing(or) && (length(or) > 1 || is.na(or) || or < 0))
            stop("'or' must be a single number between 0 and Inf")
    }

    PVAL <- NULL
    if(nr != 2  ||  nc != 2) {
        if(simulate.p.value) {
            ## we drop all-zero rows and columns
            sr <- rowSums(x)
            sc <- colSums(x)
            x <- x[sr > 0, sc > 0, drop = FALSE]
            nr <- nrow(x)
            nc <- ncol(x)
            if(nr <= 1)
                stop("need 2 or more non-zero row marginals")
            if(nc <= 1)
                stop("need 2 or more non-zero column marginals")
            METHOD <- paste(METHOD, "with simulated p-value\n\t (based on", B,
			     "replicates)")
            sr <- rowSums(x)
            sc <- colSums(x)
            n <- sum(sc)
            STATISTIC <- -sum(lfactorial(x))
	    tmp <- .C("fisher_sim",
		      as.integer(nr),
		      as.integer(nc),
		      as.integer(sr),
		      as.integer(sc),
		      as.integer(n),
		      as.integer(B),
		      integer(nr * nc),
		      double(n + 1),
		      integer(nc),
		      results = double(B),
		      PACKAGE = "stats")$results
	    ## use correct significance level for a Monte Carlo test
            almost.1 <- 1 + 64 * .Machine$double.eps
            ## PR#10558: STATISTIC is negative
	    PVAL <- (1 + sum(tmp <= STATISTIC/almost.1)) / (B + 1)
        } else if(hybrid) {
            PVAL <- .C("fexact",
                       nr,
                       nc,
                       x,
                       nr,
                       ## Cochran condition for asym.chisq. decision:
                       as.double(5), #  expect
                       as.double(80),#  percnt
                       as.double(1), #  emin
                       double(1),    #  prt
                       p = double(1),
                       as.integer(workspace),
                       mult = as.integer(mult),
                       PACKAGE = "stats")$p
        } else
            PVAL <- .C("fexact",
                       nr,
                       nc,
                       x,
                       nr,
                       as.double(-1),#  expect < 0 : exact
                       as.double(100),
                       as.double(0L),
                       double(1),#   prt
                       p = double(1),
                       as.integer(workspace),
                       mult = as.integer(mult),
                       PACKAGE = "stats")$p
        RVAL <- list(p.value = PVAL)
    }

    if((nr == 2) && (nc == 2)) {## conf.int and more only in  2 x 2 case
        if(hybrid) warning("'hybrid' is ignored for a 2 x 2 table")
        m <- sum(x[, 1])
        n <- sum(x[, 2])
        k <- sum(x[1, ])
        x <- x[1, 1]
        lo <- max(0, k - n)
        hi <- min(k, m)
        NVAL <- or
        names(NVAL) <- "odds ratio"

        ## Note that in general the conditional distribution of x given
        ## the marginals is a non-central hypergeometric distribution H
        ## with non-centrality parameter ncp, the odds ratio.
        support <- lo : hi
        ## Density of the *central* hypergeometric distribution on its
        ## support: store for once as this is needed quite a bit.
        logdc <- dhyper(support, m, n, k, log = TRUE)
        dnhyper <- function(ncp) {
            ## Does not work for boundary values for ncp (0, Inf) but it
            ## does not need to.
            d <- logdc + log(ncp) * support
            d <- exp(d - max(d))        # beware of overflow
            d / sum(d)
        }
        mnhyper <- function(ncp) {
            if(ncp == 0)
                return(lo)
            if(ncp == Inf)
                return(hi)
            sum(support * dnhyper(ncp))
        }
        pnhyper <- function(q, ncp = 1, upper.tail = FALSE) {
            if(ncp == 1) {
                if(upper.tail)
                    return(phyper(x - 1, m, n, k, lower.tail = FALSE))
                else
                    return(phyper(x, m, n, k))
            }
            if(ncp == 0) {
                if(upper.tail)
                    return(as.numeric(q <= lo))
                else
                    return(as.numeric(q >= lo))
            }
            if(ncp == Inf) {
                if(upper.tail)
                    return(as.numeric(q <= hi))
                else
                    return(as.numeric(q >= hi))
            }
            d <- dnhyper(ncp)
            if(upper.tail)
                sum(d[support >= q])
            else
                sum(d[support <= q])
        }

        ## Determine the p-value (if still necessary).
        if(is.null(PVAL)) {
            PVAL <-
                switch(alternative,
                       less = pnhyper(x, or),
                       greater = pnhyper(x, or, upper.tail = TRUE),
                       two.sided = {
                           if(or == 0)
                               as.numeric(x == lo)
                           else if(or == Inf)
                               as.numeric(x == hi)
                           else {
                               ## Note that we need a little fuzz.
                               relErr <- 1 + 10 ^ (-7)
                               d <- dnhyper(or)
                               sum(d[d <= d[x - lo + 1] * relErr])
                           }
                       })
            RVAL <- list(p.value = PVAL)
        }

        ## Determine the MLE for ncp by solving E(X) = x, where the
        ## expectation is with respect to H.
        mle <- function(x) {
            if(x == lo)
                return(0)
            if(x == hi)
                return(Inf)
            mu <- mnhyper(1)
            if(mu > x)
                uniroot(function(t) mnhyper(t) - x, c(0, 1))$root
            else if(mu < x)
                1 / uniroot(function(t) mnhyper(1/t) - x,
                            c(.Machine$double.eps, 1))$root
            else
                1
        }
        ESTIMATE <- mle(x)
        names(ESTIMATE) <- "odds ratio"

        if(conf.int) {
            ## Determine confidence intervals for the odds ratio.
            ncp.U <- function(x, alpha) {
                if(x == hi)
                    return(Inf)
                p <- pnhyper(x, 1)
                if(p < alpha)
                    uniroot(function(t) pnhyper(x, t) - alpha,
                            c(0, 1))$root
                else if(p > alpha)
                    1 / uniroot(function(t) pnhyper(x, 1/t) - alpha,
                                c(.Machine$double.eps, 1))$root
                else
                    1
            }
            ncp.L <- function(x, alpha) {
                if(x == lo)
                    return(0)

                p <- pnhyper(x, 1, upper.tail = TRUE)
                if(p > alpha)
                    uniroot(function(t)
                            pnhyper(x, t, upper.tail = TRUE) - alpha,
                            c(0, 1))$root
                else if(p < alpha)
                    1 / uniroot(function(t)
                                pnhyper(x, 1/t, upper.tail = TRUE) - alpha,
                                c(.Machine$double.eps, 1))$root
                else
                    1
            }
            CINT <- switch(alternative,
                           less = c(0, ncp.U(x, 1 - conf.level)),
                           greater = c(ncp.L(x, 1 - conf.level), Inf),
                           two.sided = {
                               alpha <- (1 - conf.level) / 2
                               c(ncp.L(x, alpha), ncp.U(x, alpha))
                           })
            attr(CINT, "conf.level") <- conf.level
        }
        RVAL <- c(RVAL,
                  list(conf.int = if(conf.int) CINT,
                       estimate = ESTIMATE,
                       null.value = NVAL))
    } ## end (2 x 2)

    RVAL <- c(RVAL,
              alternative = alternative,
              method = METHOD,
              data.name = DNAME)
    attr(RVAL, "class") <- "htest"
    return(RVAL)
}
#  File src/library/stats/R/fivenum.R
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

fivenum <- function(x, na.rm=TRUE)
{
    xna <- is.na(x)
    if(na.rm) x <- x[!xna]
    else if(any(xna)) return(rep.int(NA,5))
    x <- sort(x)
    n <- length(x)
    if(n == 0) rep.int(NA,5)
    else {
        n4 <- floor((n+3)/2) / 2
	d <- c(1, n4, (n+1)/2, n + 1 - n4, n)
	0.5*(x[floor(d)] + x[ceiling(d)])
    }
}
#  File src/library/stats/R/fligner.test.R
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

fligner.test <- function(x, ...) UseMethod("fligner.test")

fligner.test.default <-
function(x, g, ...)
{
    ## FIXME: This is the same code as in kruskal.test(), and could also
    ## rewrite bartlett.test() accordingly ...
    if (is.list(x)) {
        if (length(x) < 2)
            stop("'x' must be a list with at least 2 elements")
        DNAME <- deparse(substitute(x))
        x <- lapply(x, function(u) u <- u[complete.cases(u)])
        k <- length(x)
        l <- sapply(x, "length")
        if (any(l == 0))
            stop("all groups must contain data")
        g <- factor(rep(1 : k, l))
        x <- unlist(x)
    }
    else {
        if (length(x) != length(g))
            stop("'x' and 'g' must have the same length")
        DNAME <- paste(deparse(substitute(x)), "and",
                       deparse(substitute(g)))
        OK <- complete.cases(x, g)
        x <- x[OK]
        g <- g[OK]
        if (!all(is.finite(g)))
            stop("all group levels must be finite")
        g <- factor(g)
        k <- nlevels(g)
        if (k < 2)
            stop("all observations are in the same group")
    }
    n <- length(x)
    if (n < 2)
        stop("not enough observations")
    ## FIXME: now the specific part begins.

    ## Careful. This assumes that g is a factor:
    x <- x - tapply(x,g,median)[g]

    a <- qnorm((1 + rank(abs(x)) / (n + 1)) / 2)
    STATISTIC <- sum(tapply(a, g, "sum")^2 / tapply(a, g, "length"))
    STATISTIC <- (STATISTIC - n * mean(a)^2) / var(a)
    PARAMETER <- k - 1
    PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
    names(STATISTIC) <- "Fligner-Killeen:med chi-squared"
    names(PARAMETER) <- "df"
    METHOD <- "Fligner-Killeen test of homogeneity of variances"

    RVAL <- list(statistic = STATISTIC,
                 parameter = PARAMETER,
                 p.value = PVAL,
                 method = METHOD,
                 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}

fligner.test.formula <-
function(formula, data, subset, na.action, ...)
{
    if(missing(formula) || (length(formula) != 3))
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m[[1L]] <- as.name("model.frame")
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " by ")
    names(mf) <- NULL
    y <- do.call("fligner.test", as.list(mf))
    y$data.name <- DNAME
    y
}
#  File src/library/stats/R/friedman.test.R
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

friedman.test <- function(y, ...) UseMethod("friedman.test")

friedman.test.default <-
function(y, groups, blocks, ...)
{
    DNAME <- deparse(substitute(y))
    if (is.matrix(y)) {
        groups <- factor(c(col(y)))
        blocks <- factor(c(row(y)))
    }
    else {
        if (any(is.na(groups)) || any(is.na(blocks)))
            stop("NA's are not allowed in groups or blocks")
        if (any(diff(c(length(y), length(groups), length(blocks))) != 0L))
            stop("y, groups and blocks must have the same length")
        DNAME <- paste(DNAME, ", ", deparse(substitute(groups)),
                       " and ", deparse(substitute(blocks)), sep = "")
        if (any(table(groups, blocks) != 1))
            stop("not an unreplicated complete block design")
        groups <- factor(groups)
        blocks <- factor(blocks)
        ## Need to ensure consistent order of observations within
        ## blocks.
        o <- order(groups, blocks)
        y <- y[o]
        groups <- groups[o]
        blocks <- blocks[o]
    }

    k <- nlevels(groups)
    y <- matrix(unlist(split(y, blocks)), ncol = k, byrow = TRUE)
    y <- y[complete.cases(y), ]
    n <- nrow(y)
    r <- t(apply(y, 1L, rank))
    TIES <- tapply(r, row(r), table)
    STATISTIC <- ((12 * sum((colSums(r) - n * (k + 1) / 2)^2)) /
                  (n * k * (k + 1)
                   - (sum(unlist(lapply(TIES, function (u) {u^3 - u}))) /
                      (k - 1))))
    PARAMETER <- k - 1
    PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
    names(STATISTIC) <- "Friedman chi-squared"
    names(PARAMETER) <- "df"

    structure(list(statistic = STATISTIC,
                   parameter = PARAMETER,
                   p.value = PVAL,
                   method = "Friedman rank sum test",
                   data.name = DNAME),
              class = "htest")
}

friedman.test.formula <-
function(formula, data, subset, na.action, ...)
{
    if(missing(formula))
        stop("formula missing")
    ## <FIXME>
    ## Maybe put this into an internal rewriteTwoWayFormula() when
    ## adding support for strata()
    if((length(formula) != 3)
       || (length(formula[[3L]]) != 3)
       || (formula[[3L]][[1L]] != as.name("|"))
       || (length(formula[[3L]][[2L]]) != 1)
       || (length(formula[[3L]][[3L]]) != 1))
        stop("incorrect specification for 'formula'")
    formula[[3L]][[1L]] <- as.name("+")
    ## </FIXME>
    m <- match.call(expand.dots = FALSE)
    m$formula <- formula
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m[[1L]] <- as.name("model.frame")
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " and ")
    names(mf) <- NULL
    y <- do.call("friedman.test", as.list(mf))
    y$data.name <- DNAME
    y
}
#  File src/library/stats/R/ftable.R
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

ftable <- function(x, ...) UseMethod("ftable")

ftable.default <- function(..., exclude = c(NA, NaN),
                           row.vars = NULL, col.vars = NULL) {
    args <- list(...)
    if (length(args) == 0)
        stop("nothing to tabulate")
    x <- args[[1L]]
    if(is.list(x))
        x <- table(x, exclude = exclude)
    else if(inherits(x, "ftable")) {
        x <- as.table(x)
    }
    else if(!(is.array(x) && (length(dim(x)) > 1L))) {
        x <- table(..., exclude = exclude)
    }
    dn <- dimnames(x)
    dx <- dim(x)
    n <- length(dx)
    if(!is.null(row.vars)) {
        if(is.character(row.vars)) {
            i <- pmatch(row.vars, names(dn))
            if(any(is.na(i)))
                stop("incorrect specification for 'row.vars'")
            row.vars <- i
        } else if(any((row.vars < 1) | (row.vars > n)))
            stop("incorrect specification for 'row.vars'")
    }
    if(!is.null(col.vars)) {
        if(is.character(col.vars)) {
            i <- pmatch(col.vars, names(dn))
            if(any(is.na(i)))
             stop("incorrect specification for 'col.vars'")
            col.vars <- i
        } else if(any((col.vars < 1) | (col.vars > n)))
            stop("incorrect specification for 'col.vars'")
    }
    i <- 1 : n
    if(!is.null(row.vars) && !is.null(col.vars)) {
        all.vars <- sort(c(row.vars, col.vars))
        if (length(all.vars) < n) {
            x <- apply(x, all.vars, sum)
            row.vars <- match(row.vars, all.vars)
            col.vars <- match(col.vars, all.vars)
            dn <- dn[all.vars]
            dx <- dx[all.vars]
        }
    }
    else if(!is.null(row.vars))
        col.vars <- i[-row.vars]
    else if(!is.null(col.vars))
        row.vars <- i[-col.vars]
    else {
        row.vars <- 1 : (n-1)
        col.vars <- n
    }

    y <- aperm(x, c(rev(row.vars), rev(col.vars)))
    dim(y) <- c(prod(dx[row.vars]), prod(dx[col.vars]))
    attr(y, "row.vars") <- dn[row.vars]
    attr(y, "col.vars") <- dn[col.vars]
    class(y) <- "ftable"
    y
}

ftable.formula <- function(formula, data = NULL, subset, na.action, ...)
{
    if(missing(formula) || !inherits(formula, "formula"))
        stop("'formula' missing or incorrect")
    if(length(formula) != 3)
        stop("'formula' must have both left and right hand sides")
    ## need to cope with '.' in formula
    tt <- if(is.data.frame(data)) terms(formula, data=data)
    else terms(formula, allowDotAsName=TRUE)
    if(any(attr(tt, "order") > 1))
        stop("interactions are not allowed")
    ## here we do NOT want '.' expanded
    rvars <- attr(terms(formula[-2], allowDotAsName=TRUE), "term.labels")
    cvars <- attr(terms(formula[-3], allowDotAsName=TRUE), "term.labels")
    rhs.has.dot <- any(rvars == ".")
    lhs.has.dot <- any(cvars == ".")
    if(lhs.has.dot && rhs.has.dot)
        stop("'formula' has '.' in both left and right hand side")
    m <- match.call(expand.dots = FALSE)
    edata <- eval(m$data, parent.frame())
    if(inherits(edata, "ftable")
       || inherits(edata, "table")
       || length(dim(edata)) > 2) {
        if(inherits(edata, "ftable")) {
            data <- as.table(data)
        }
        varnames <- names(dimnames(data))
        if(rhs.has.dot)
            rvars <- NULL
        else {
            i <- pmatch(rvars, varnames)
            if(any(is.na(i)))
                stop("incorrect variable names in rhs of formula")
            rvars <- i
        }
        if(lhs.has.dot)
            cvars <- NULL
        else {
            i <- pmatch(cvars, varnames)
            if(any(is.na(i)))
                stop("incorrect variable names in lhs of formula")
            cvars <- i
        }
        ftable(data, row.vars = rvars, col.vars = cvars)
    }
    else {
        if(is.matrix(edata))
            m$data <- as.data.frame(data)
        m$... <- NULL
        if(!is.null(data) && is.environment(data)) {
            varnames <- names(data)
            if(rhs.has.dot)
                rvars <- seq_along(varnames)[-cvars]
            if(lhs.has.dot)
                cvars <- seq_along(varnames)[-rvars]
        }
        else {
            if(lhs.has.dot || rhs.has.dot)
                stop("cannot use dots in formula with given data")
        }
        m$formula <- as.formula(paste("~",
                                   paste(c(rvars, cvars),
                                         collapse = "+")),
                                env = environment(formula))
        m[[1L]] <- as.name("model.frame")
        mf <- eval(m, parent.frame())
        ftable(mf, row.vars = rvars, col.vars = cvars, ...)
    }
}

as.table.ftable <- function(x, ...)
{
    if(!inherits(x, "ftable"))
        stop("'x' must be an \"ftable\" object")
    xrv <- rev(attr(x, "row.vars"))
    xcv <- rev(attr(x, "col.vars"))
    x <- array(data = c(x),
               dim = c(sapply(xrv, length),
                       sapply(xcv, length)),
               dimnames = c(xrv, xcv))
    nrv <- length(xrv)
    ncv <- length(xcv)
    x <- aperm(x, c(seq.int(from = nrv, to = 1),
                    seq.int(from = nrv + ncv, to = nrv + 1)))
    class(x) <- "table"
    x
}

format.ftable <- function(x, quote = TRUE, digits = getOption("digits"), ...)
{
    if(!inherits(x, "ftable"))
        stop("'x' must be an \"ftable\" object")
    charQuote <- function(s)
        if(quote) paste("\"", s, "\"", sep = "") else s
    makeLabels <- function(lst) {
        lens <- sapply(lst, length)
        cplensU <- c(1, cumprod(lens))
        cplensD <- rev(c(1, cumprod(rev(lens))))
        y <- NULL
        for (i in rev(seq_along(lst))) {
            ind <- 1 + seq.int(from = 0, to = lens[i] - 1) * cplensD[i + 1]
            tmp <- character(length = cplensD[i])
            tmp[ind] <- charQuote(lst[[i]])
            y <- cbind(rep(tmp, times = cplensU[i]), y)
        }
        y
    }
    makeNames <- function(x) {
        nmx <- names(x)
        if(is.null(nmx))
            nmx <- rep("", length.out = length(x))
        nmx
    }

    xrv <- attr(x, "row.vars")
    xcv <- attr(x, "col.vars")
    LABS <- cbind(rbind(matrix("", nrow = length(xcv), ncol = length(xrv)),
                        charQuote(makeNames(xrv)),
                        makeLabels(xrv)),
                  c(charQuote(makeNames(xcv)),
                    rep("", times = nrow(x) + 1)))
    DATA <- rbind(if(length(xcv)) t(makeLabels(xcv)),
                  rep("", times = ncol(x)),
                  format(unclass(x), digits = digits))
    cbind(apply(LABS, 2L, format, justify = "left"),
	  apply(DATA, 2L, format, justify = "right"))
}

write.ftable <- function(x, file = "", quote = TRUE, append = FALSE,
			 digits = getOption("digits"))
{
    r <- format.ftable(x, quote = quote, digits = digits)
    cat(t(r), file = file, append = append,
	sep = c(rep(" ", ncol(r) - 1), "\n"))
    invisible(x)
}

print.ftable <- function(x, digits = getOption("digits"), ...)
    write.ftable(x, quote = FALSE, digits = digits)

read.ftable <- function(file, sep = "", quote = "\"", row.var.names,
                        col.vars, skip = 0)
{
    if(is.character(file)) {
        file <- file(file, "r")
        on.exit(close(file))
    }
    if(!inherits(file, "connection"))
        stop("'file' must be a character string or connection")
    if(!isSeekable(file)) {
        ## We really need something seekable, see below.  If it is not,
        ## the best we can do is write everything to a tempfile.
        tmpf <- tempfile()
        cat(readLines(file), file = tmpf, sep="\n")
        file <- file(tmpf, "r")
        on.exit({close(file);unlink(tmpf)}, add=TRUE)
    }

    z <- utils::count.fields(file, sep, quote, skip)
    n.row.vars <- z[max(which(z == max(z)))] - z[length(z)] + 1

    seek(file, where = 0)
    if(skip > 0) readLines(file, skip)
    lines <- readLines(file)
    seek(file, where = 0)
    if(skip > 0) readLines(file, skip)

    i <- which(z == n.row.vars)
    ## For an ftable, we have
    ##                     cv.1.nm cv.1.l1 .........
    ##                     cv.2.nm cv.2.l1 .........
    ## rv.1.nm ... rv.k.nm
    ## rv.1.l1 ... rv.k.l1         ...     ...
    ##
    ## so there is exactly one line which does not start with a space
    ## and has n.row.vars fields (and it cannot be the first one).
    j <- i[grep("^[^[:space:]]", lines[i])]
    if((length(j) == 1) && (j > 1)) {
        ## An ftable: we can figure things out ourselves.
        n.col.vars <- j - 1
        col.vars <- vector("list", length = n.col.vars)
        n <- c(1, z[1 : n.col.vars] - 1)
        for(k in seq.int(from = 1, to = n.col.vars)) {
            s <- scan(file, what = "", sep = sep, quote = quote,
                      nlines = 1, quiet = TRUE)
            col.vars[[k]] <- s[-1L]
            names(col.vars)[k] <- s[1L]
        }
        row.vars <- vector("list", length = n.row.vars)
        names(row.vars) <- scan(file, what = "", sep = sep, quote =
                                quote, nlines = 1, quiet = TRUE)
        z <- z[-(1 : (n.col.vars + 1))]
    }
    else {
        ## This is not really an ftable.
        if((z[1L] == 1) && z[2L] == max(z)) {
            ## Case A.  File looks like
            ##
            ##                                cvar.nam
            ## rvar.1.nam   ... rvar.k.nam    cvar.lev.1 ... cvar.lev.l
            ## rvar.1.lev.1 ... rvar.k.lev.1  ...        ... ...
            ##
            n.col.vars <- 1
            col.vars <- vector("list", length = n.col.vars)
            s <- scan(file, what = "", sep = sep, quote = quote,
                      nlines = 2, quiet = TRUE)
            names(col.vars) <- s[1L]
            s <- s[-1L]
            row.vars <- vector("list", length = n.row.vars)
            i <- 1 : n.row.vars
            names(row.vars) <- s[i]
            col.vars[[1L]] <- s[-i]
            z <- z[-(1 : 2)]
        }
        else {
            ## Case B.
            ## We cannot determine the names and levels of the column
            ## variables, and also not the names of the row variables.
            if(missing(row.var.names)) {
                ## 'row.var.names' should be a character vector (or
                ## factor) with the names of the row variables.
                stop("'row.var.names' missing")
            }
            n.row.vars <- length(row.var.names)
            row.vars <- vector("list", length = n.row.vars)
            names(row.vars) <- as.character(row.var.names)
            if(missing(col.vars) || !is.list(col.vars)) {
                ## 'col.vars' should be a list.
                stop("'col.vars' missing or incorrect")
            }
            col.vars <- lapply(col.vars, as.character)
            n.col.vars <- length(col.vars)
            if(is.null(names(col.vars)))
                names(col.vars) <-
                    paste("Factor", seq_along(col.vars), sep = ".")
            else {
                nam <- names(col.vars)
                ind <- which(!nzchar(nam))
                names(col.vars)[ind] <-
                    paste("Factor", ind, sep = ".")
            }
        }
    }

    p <- 1
    n <- integer(n.row.vars)
    for(k in seq.int(from = 1, to = n.row.vars)) {
        n[k] <- sum(z >= max(z) - k + 1) / p
        p <- p * n[k]
    }
    is.row.lab <- rep(rep(c(TRUE, FALSE), length(z)),
                      c(rbind(z - min(z) + 1, min(z) - 1)))
    s <- scan(file, what = "", sep = sep, quote = quote, quiet = TRUE)
    values <- as.numeric(s[!is.row.lab])
    tmp <- s[is.row.lab]
    len <- length(tmp)
    for(k in seq.int(from = 1, to = n.row.vars)) {
        i <- seq.int(from = 1, to = len, by = len / n[k])
        row.vars[[k]] <- unique(tmp[i])
        tmp <- tmp[seq.int(from = 2, to = len / n[k])]
        len <- length(tmp)
    }
    values <- matrix(values,
                     nrow = prod(sapply(row.vars, length)),
                     ncol = prod(sapply(col.vars, length)),
                     byrow = TRUE)
    structure(values,
              row.vars = row.vars,
              col.vars = col.vars,
              class = "ftable")
}

as.data.frame.ftable <- function(x, row.names = NULL, optional = FALSE, ...)
    as.data.frame(as.table(x), row.names, optional)
#  File src/library/stats/R/glm.R
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

### This function fits a generalized linear model via
### iteratively reweighted least squares for any family.
### Written by Simon Davies, Dec 1995
### glm.fit modified by Thomas Lumley, Apr 1997, and then others..

glm <- function(formula, family = gaussian, data, weights,
		subset, na.action, start = NULL,
		etastart, mustart, offset,
		control = glm.control(...), model = TRUE,
                method = "glm.fit", x = FALSE, y = TRUE,
                contrasts = NULL, ...)
{
    call <- match.call()
    ## family
    if(is.character(family))
        family <- get(family, mode = "function", envir = parent.frame())
    if(is.function(family)) family <- family()
    if(is.null(family$family)) {
	print(family)
	stop("'family' not recognized")
    }

    ## extract x, y, etc from the model formula and frame
    if(missing(data)) data <- environment(formula)
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "weights", "na.action",
                 "etastart", "mustart", "offset"), names(mf), 0L)
    mf <- mf[c(1, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
    if(identical(method, "model.frame")) return(mf)
    ## we use 'glm.fit' for the sake of error messages.
    if(identical(method, "glm.fit")) {
        ## OK
    } else if(is.function(method)) {
        glm.fit <- method
    } else if(is.character(method)) {
        if(exists(method)) glm.fit <- get(method)
        else stop(gettextf("invalid 'method': %s", method), domain = NA)
    } else stop("invalid 'method'")
    mt <- attr(mf, "terms") # allow model.frame to have updated it

    Y <- model.response(mf, "any") # e.g. factors are allowed
    ## avoid problems with 1D arrays, but keep names
    if(length(dim(Y)) == 1L) {
        nm <- rownames(Y)
        dim(Y) <- NULL
        if(!is.null(nm)) names(Y) <- nm
    }
    ## null model support
    X <- if (!is.empty.model(mt)) model.matrix(mt, mf, contrasts) else matrix(,NROW(Y), 0L)
    ## avoid any problems with 1D or nx1 arrays by as.vector.
    weights <- as.vector(model.weights(mf))
    if(!is.null(weights) && !is.numeric(weights))
        stop("'weights' must be a numeric vector")
    ## check weights and offset
    if( !is.null(weights) && any(weights < 0) )
	stop("negative weights not allowed")

    offset <- as.vector(model.offset(mf))
    if(!is.null(offset)) {
        if(length(offset) != NROW(Y))
            stop(gettextf("number of offsets is %d should equal %d (number of observations)", length(offset), NROW(Y)), domain = NA)
    }
    ## these allow starting values to be expressed in terms of other vars.
    mustart <- model.extract(mf, "mustart")
    etastart <- model.extract(mf, "etastart")

    ## fit model via iterative reweighted least squares
    fit <- glm.fit(x = X, y = Y, weights = weights, start = start,
                   etastart = etastart, mustart = mustart,
                   offset = offset, family = family, control = control,
                   intercept = attr(mt, "intercept") > 0)

    ## This calculated the null deviance from the intercept-only model
    ## if there is one, otherwise from the offset-only model.
    ## We need to recalculate by a proper fit if there is intercept and
    ## offset.
    ##
    ## The glm.fit calculation could be wrong if the link depends on the
    ## observations, so we allow the null deviance to be forced to be
    ## re-calculated by setting an offset (provided there is an intercept).
    ## Prior to 2.4.0 this was only done for non-zero offsets.
    if(length(offset) && attr(mt, "intercept") > 0L) {
	fit$null.deviance <-
	    glm.fit(x = X[,"(Intercept)",drop=FALSE], y = Y, weights = weights,
                    offset = offset, family = family,
                    control = control, intercept = TRUE)$deviance
    }
    if(model) fit$model <- mf
    fit$na.action <- attr(mf, "na.action")
    if(x) fit$x <- X
    if(!y) fit$y <- NULL
    fit <- c(fit, list(call = call, formula = formula,
		       terms = mt, data = data,
		       offset = offset, control = control, method = method,
		       contrasts = attr(X, "contrasts"),
                       xlevels = .getXlevels(mt, mf)))
    class(fit) <- c("glm", "lm")
    fit
}


glm.control <- function(epsilon = 1e-8, maxit = 25, trace = FALSE)
{
    if(!is.numeric(epsilon) || epsilon <= 0)
	stop("value of 'epsilon' must be > 0")
    if(!is.numeric(maxit) || maxit <= 0)
	stop("maximum number of iterations must be > 0")
    list(epsilon = epsilon, maxit = maxit, trace = trace)
}

## Modified by Thomas Lumley 26 Apr 97
## Added boundary checks and step halving
## Modified detection of fitted 0/1 in binomial
## Updated by KH as suggested by BDR on 1998/06/16

glm.fit <-
    function (x, y, weights = rep(1, nobs), start = NULL,
	      etastart = NULL, mustart = NULL, offset = rep(0, nobs),
	      family = gaussian(), control = glm.control(), intercept = TRUE)
{
    x <- as.matrix(x)
    xnames <- dimnames(x)[[2L]]
    ynames <- if(is.matrix(y)) rownames(y) else names(y)
    conv <- FALSE
    nobs <- NROW(y)
    nvars <- ncol(x)
    EMPTY <- nvars == 0
    ## define weights and offset if needed
    if (is.null(weights))
	weights <- rep.int(1, nobs)
    if (is.null(offset))
	offset <- rep.int(0, nobs)

    ## get family functions:
    variance <- family$variance
    linkinv  <- family$linkinv
    if (!is.function(variance) || !is.function(linkinv) )
	stop("'family' argument seems not to be a valid family object")
    dev.resids <- family$dev.resids
    aic <- family$aic
    mu.eta <- family$mu.eta
    unless.null <- function(x, if.null) if(is.null(x)) if.null else x
    valideta <- unless.null(family$valideta, function(eta) TRUE)
    validmu  <- unless.null(family$validmu,  function(mu) TRUE)
    if(is.null(mustart)) {
        ## calculates mustart and may change y and weights and set n (!)
        eval(family$initialize)
    } else {
        mukeep <- mustart
        eval(family$initialize)
        mustart <- mukeep
    }
    if(EMPTY) {
        eta <- rep.int(0, nobs) + offset
        if (!valideta(eta))
            stop("invalid linear predictor values in empty model")
        mu <- linkinv(eta)
        ## calculate initial deviance and coefficient
        if (!validmu(mu))
            stop("invalid fitted means in empty model")
        dev <- sum(dev.resids(y, mu, weights))
        w <- ((weights * mu.eta(eta)^2)/variance(mu))^0.5
        residuals <- (y - mu)/mu.eta(eta)
        good <- rep(TRUE, length(residuals))
        boundary <- conv <- TRUE
        coef <- numeric(0L)
        iter <- 0L
    } else {
        coefold <- NULL
        eta <-
            if(!is.null(etastart)) etastart
            else if(!is.null(start))
                if (length(start) != nvars)
                    stop(gettextf("length of 'start' should equal %d and correspond to initial coefs for %s", nvars, paste(deparse(xnames), collapse=", ")),
                         domain = NA)
                else {
                    coefold <- start
                    offset + as.vector(if (NCOL(x) == 1) x * start else x %*% start)
                }
            else family$linkfun(mustart)
        mu <- linkinv(eta)
        if (!(validmu(mu) && valideta(eta)))
            stop("cannot find valid starting values: please specify some")
        ## calculate initial deviance and coefficient
        devold <- sum(dev.resids(y, mu, weights))
        boundary <- conv <- FALSE

        ##------------- THE Iteratively Reweighting L.S. iteration -----------
        for (iter in 1L:control$maxit) {
            good <- weights > 0
            varmu <- variance(mu)[good]
            if (any(is.na(varmu)))
                stop("NAs in V(mu)")
            if (any(varmu == 0))
                stop("0s in V(mu)")
            mu.eta.val <- mu.eta(eta)
            if (any(is.na(mu.eta.val[good])))
                stop("NAs in d(mu)/d(eta)")
            ## drop observations for which w will be zero
            good <- (weights > 0) & (mu.eta.val != 0)

            if (all(!good)) {
                conv <- FALSE
                warning("no observations informative at iteration ", iter)
                break
            }
            z <- (eta - offset)[good] + (y - mu)[good]/mu.eta.val[good]
            w <- sqrt((weights[good] * mu.eta.val[good]^2)/variance(mu)[good])
            ngoodobs <- as.integer(nobs - sum(!good))
            ## call Fortran code
            fit <- .Fortran("dqrls",
                            qr = x[good, ] * w, n = ngoodobs,
                            p = nvars, y = w * z, ny = 1L,
                            tol = min(1e-7, control$epsilon/1000),
                            coefficients = double(nvars),
                            residuals = double(ngoodobs),
                            effects = double(ngoodobs),
                            rank = integer(1L),
                            pivot = 1L:nvars, qraux = double(nvars),
                            work = double(2 * nvars),
                            PACKAGE = "base")
            if (any(!is.finite(fit$coefficients))) {
                conv <- FALSE
                warning("non-finite coefficients at iteration ", iter)
                break
            }
            ## stop if not enough parameters
            if (nobs < fit$rank)
                stop(gettextf("X matrix has rank %d, but only %d observations",
                              fit$rank, nobs), domain = NA)
            ## calculate updated values of eta and mu with the new coef:
            start[fit$pivot] <- fit$coefficients
            eta <- drop(x %*% start)
            mu <- linkinv(eta <- eta + offset)
            dev <- sum(dev.resids(y, mu, weights))
            if (control$trace)
                cat("Deviance =", dev, "Iterations -", iter, "\n")
            ## check for divergence
            boundary <- FALSE
            if (!is.finite(dev)) {
                if(is.null(coefold))
                    stop("no valid set of coefficients has been found: please supply starting values", call. = FALSE)
                warning("step size truncated due to divergence", call. = FALSE)
                ii <- 1
                while (!is.finite(dev)) {
                    if (ii > control$maxit)
                        stop("inner loop 1; cannot correct step size")
                    ii <- ii + 1
                    start <- (start + coefold)/2
                    eta <- drop(x %*% start)
                    mu <- linkinv(eta <- eta + offset)
                    dev <- sum(dev.resids(y, mu, weights))
                }
                boundary <- TRUE
                if (control$trace)
                    cat("Step halved: new deviance =", dev, "\n")
            }
            ## check for fitted values outside domain.
            if (!(valideta(eta) && validmu(mu))) {
                if(is.null(coefold))
                    stop("no valid set of coefficients has been found: please supply starting values", call. = FALSE)
                warning("step size truncated: out of bounds", call. = FALSE)
                ii <- 1
                while (!(valideta(eta) && validmu(mu))) {
                    if (ii > control$maxit)
                        stop("inner loop 2; cannot correct step size")
                    ii <- ii + 1
                    start <- (start + coefold)/2
                    eta <- drop(x %*% start)
                    mu <- linkinv(eta <- eta + offset)
                }
                boundary <- TRUE
                dev <- sum(dev.resids(y, mu, weights))
                if (control$trace)
                    cat("Step halved: new deviance =", dev, "\n")
            }
            ## check for convergence
            if (abs(dev - devold)/(0.1 + abs(dev)) < control$epsilon) {
                conv <- TRUE
                coef <- start
                break
            } else {
                devold <- dev
                coef <- coefold <- start
            }
        } ##-------------- end IRLS iteration -------------------------------

        if (!conv) warning("algorithm did not converge")
        if (boundary) warning("algorithm stopped at boundary value")
        eps <- 10*.Machine$double.eps
        if (family$family == "binomial") {
            if (any(mu > 1 - eps) || any(mu < eps))
                warning("fitted probabilities numerically 0 or 1 occurred")
        }
        if (family$family == "poisson") {
            if (any(mu < eps))
                warning("fitted rates numerically 0 occurred")
        }
        ## If X matrix was not full rank then columns were pivoted,
        ## hence we need to re-label the names ...
        ## Original code changed as suggested by BDR---give NA rather
        ## than 0 for non-estimable parameters
        if (fit$rank < nvars) coef[fit$pivot][seq.int(fit$rank+1, nvars)] <- NA
        xxnames <- xnames[fit$pivot]
        ## update by accurate calculation, including 0-weight cases.
        residuals <-  (y - mu)/mu.eta(eta)
##        residuals <- rep.int(NA, nobs)
##        residuals[good] <- z - (eta - offset)[good] # z does not have offset in.
        fit$qr <- as.matrix(fit$qr)
        nr <- min(sum(good), nvars)
        if (nr < nvars) {
            Rmat <- diag(nvars)
            Rmat[1L:nr, 1L:nvars] <- fit$qr[1L:nr, 1L:nvars]
        }
        else Rmat <- fit$qr[1L:nvars, 1L:nvars]
        Rmat <- as.matrix(Rmat)
        Rmat[row(Rmat) > col(Rmat)] <- 0
        names(coef) <- xnames
        colnames(fit$qr) <- xxnames
        dimnames(Rmat) <- list(xxnames, xxnames)
    }
    names(residuals) <- ynames
    names(mu) <- ynames
    names(eta) <- ynames
    # for compatibility with lm, which has a full-length weights vector
    wt <- rep.int(0, nobs)
    wt[good] <- w^2
    names(wt) <- ynames
    names(weights) <- ynames
    names(y) <- ynames
    if(!EMPTY)
        names(fit$effects) <-
            c(xxnames[seq_len(fit$rank)], rep.int("", sum(good) - fit$rank))
    ## calculate null deviance -- corrected in glm() if offset and intercept
    wtdmu <-
	if (intercept) sum(weights * y)/sum(weights) else linkinv(offset)
    nulldev <- sum(dev.resids(y, wtdmu, weights))
    ## calculate df
    n.ok <- nobs - sum(weights==0)
    nulldf <- n.ok - as.integer(intercept)
    rank <- if(EMPTY) 0 else fit$rank
    resdf  <- n.ok - rank
    ## calculate AIC
    aic.model <-
	aic(y, n, mu, weights, dev) + 2*rank
	##     ^^ is only initialize()d for "binomial" [yuck!]
    list(coefficients = coef, residuals = residuals, fitted.values = mu,
	 effects = if(!EMPTY) fit$effects, R = if(!EMPTY) Rmat, rank = rank,
	 qr = if(!EMPTY) structure(fit[c("qr", "rank", "qraux", "pivot", "tol")], class="qr"),
         family = family,
	 linear.predictors = eta, deviance = dev, aic = aic.model,
	 null.deviance = nulldev, iter = iter, weights = wt,
	 prior.weights = weights, df.residual = resdf, df.null = nulldf,
	 y = y, converged = conv, boundary = boundary)
}


print.glm <- function(x, digits= max(3, getOption("digits") - 3), ...)
{
    cat("\nCall: ", deparse(x$call), "\n\n")
    if(length(coef(x))) {
        cat("Coefficients")
        if(is.character(co <- x$contrasts))
            cat("  [contrasts: ",
                apply(cbind(names(co),co), 1L, paste, collapse="="), "]")
        cat(":\n")
        print.default(format(x$coefficients, digits=digits),
                      print.gap = 2, quote = FALSE)
    } else cat("No coefficients\n\n")
    cat("\nDegrees of Freedom:", x$df.null, "Total (i.e. Null); ",
        x$df.residual, "Residual\n")
    if(nzchar(mess <- naprint(x$na.action))) cat("  (",mess, ")\n", sep="")
    cat("Null Deviance:	   ",	format(signif(x$null.deviance, digits)),
	"\nResidual Deviance:", format(signif(x$deviance, digits)),
	"\tAIC:", format(signif(x$aic, digits)), "\n")
    invisible(x)
}


anova.glm <- function(object, ..., dispersion=NULL, test=NULL)
{
    ## check for multiple objects
    dotargs <- list(...)
    named <- if (is.null(names(dotargs)))
	rep(FALSE, length(dotargs)) else (names(dotargs) != "")
    if(any(named))
	warning("the following arguments to 'anova.glm' are invalid and dropped: ",
		paste(deparse(dotargs[named]), collapse=", "))
    dotargs <- dotargs[!named]
    is.glm <- unlist(lapply(dotargs,function(x) inherits(x,"glm")))
    dotargs <- dotargs[is.glm]
    if (length(dotargs))
	return(anova.glmlist(c(list(object), dotargs),
			     dispersion = dispersion, test=test))

    ## extract variables from model

    varlist <- attr(object$terms, "variables")
    ## must avoid partial matching here.
    x <-
	if (n <- match("x", names(object), 0L))
	    object[[n]]
	else model.matrix(object)
    varseq <- attr(x, "assign")
    nvars <- max(0, varseq)
    resdev <- resdf <- NULL

    ## if there is more than one explanatory variable then
    ## recall glm.fit to fit variables sequentially

    if(nvars > 1) {
	method <- object$method
	if(!is.function(method))
	    method <- get(method, mode = "function", envir=parent.frame())
        ## allow for 'y = FALSE' in the call (PR#13098)
        y <- object$y
        if(is.null(y)) { ## code from residuals.glm
            mu.eta <- object$family$mu.eta
            eta <- object$linear.predictors
            y <-   object$fitted.values + object$residuals * mu.eta(eta)
        }
	for(i in 1L:(nvars-1)) {
	    ## explanatory variables up to i are kept in the model
	    ## use method from glm to find residual deviance
	    ## and df for each sequential fit
	    fit <- method(x=x[, varseq <= i, drop = FALSE],
			  y=y,
			  weights=object$prior.weights,
			  start	 =object$start,
			  offset =object$offset,
			  family =object$family,
			  control=object$control)
	    resdev <- c(resdev, fit$deviance)
	    resdf <- c(resdf, fit$df.residual)
	}
    }

    ## add values from null and full model

    resdf <- c(object$df.null, resdf, object$df.residual)
    resdev <- c(object$null.deviance, resdev, object$deviance)

    ## construct table and title

    table <- data.frame(c(NA, -diff(resdf)),
			c(NA, pmax(0, -diff(resdev))), resdf, resdev)
    tl <- attr(object$terms, "term.labels")
    if (length(tl) == 0L) table <- table[1,,drop=FALSE] # kludge for null model
    dimnames(table) <- list(c("NULL", tl),
			    c("Df", "Deviance", "Resid. Df", "Resid. Dev"))
    title <- paste("Analysis of Deviance Table", "\n\nModel: ",
		   object$family$family, ", link: ", object$family$link,
		   "\n\nResponse: ", as.character(varlist[-1L])[1L],
		   "\n\nTerms added sequentially (first to last)\n\n", sep="")

    ## calculate test statistics if needed

    df.dispersion <- Inf
    if(is.null(dispersion)) {
	dispersion <- summary(object, dispersion=dispersion)$dispersion
	df.dispersion <- if (dispersion == 1) Inf else object$df.residual
    }
    if(!is.null(test)) {
        if(test == "F" && df.dispersion == Inf) {
            fam <- object$family$family
            if(fam == "binomial" || fam == "poisson")
                warning(gettextf("using F test with a %s family is inappropriate",
                                 fam),
                        domain = NA)
            else
                warning("using F test with a fixed dispersion is inappropriate")
        }
	table <- stat.anova(table=table, test=test, scale=dispersion,
			    df.scale=df.dispersion, n=NROW(x))
    }
    structure(table, heading = title, class= c("anova", "data.frame"))
}


anova.glmlist <- function(object, ..., dispersion=NULL, test=NULL)
{

    ## find responses for all models and remove
    ## any models with a different response

    responses <- as.character(lapply(object, function(x) {
	deparse(formula(x)[[2L]])} ))
    sameresp <- responses==responses[1L]
    if(!all(sameresp)) {
	object <- object[sameresp]
	warning("models with response ", deparse(responses[!sameresp]),
		" removed because response differs from model 1")
    }

    ns <- sapply(object, function(x) length(x$residuals))
    if(any(ns != ns[1L]))
	stop("models were not all fitted to the same size of dataset")

    ## calculate the number of models

    nmodels <- length(object)
    if(nmodels==1)
	return(anova.glm(object[[1L]], dispersion=dispersion, test=test))

    ## extract statistics

    resdf  <- as.numeric(lapply(object, function(x) x$df.residual))
    resdev <- as.numeric(lapply(object, function(x) x$deviance))

    ## construct table and title

    table <- data.frame(resdf, resdev, c(NA, -diff(resdf)),
			c(NA, -diff(resdev)) )
    variables <- lapply(object, function(x)
			paste(deparse(formula(x)), collapse="\n") )
    dimnames(table) <- list(1L:nmodels, c("Resid. Df", "Resid. Dev", "Df",
					 "Deviance"))
    title <- "Analysis of Deviance Table\n"
    topnote <- paste("Model ", format(1L:nmodels),": ",
		     variables, sep="", collapse="\n")

    ## calculate test statistic if needed

    if(!is.null(test)) {
	bigmodel <- object[[order(resdf)[1L]]]
	dispersion <- summary(bigmodel, dispersion=dispersion)$dispersion
	df.dispersion <- if (dispersion == 1) Inf else min(resdf)
        if(test == "F" && df.dispersion == Inf) {
            fam <- bigmodel$family$family
            if(fam == "binomial" || fam == "poisson")
                warning(gettextf("using F test with a '%s' family is inappropriate",
                                 fam),
                        domain = NA, call. = FALSE)
            else
                warning("using F test with a fixed dispersion is inappropriate")
        }
	table <- stat.anova(table = table, test = test,
			    scale = dispersion, df.scale = df.dispersion,
			    n = length(bigmodel$residuals))
    }
    structure(table, heading = c(title, topnote),
	      class = c("anova", "data.frame"))
}


summary.glm <- function(object, dispersion = NULL,
			correlation = FALSE, symbolic.cor = FALSE, ...)
{
    est.disp <- FALSE
    df.r <- object$df.residual
    if(is.null(dispersion))	# calculate dispersion if needed
	dispersion <-
	    if(object$family$family %in% c("poisson", "binomial"))  1
	    else if(df.r > 0) {
                est.disp <- TRUE
		if(any(object$weights==0))
		    warning("observations with zero weight not used for calculating dispersion")
		sum((object$weights*object$residuals^2)[object$weights > 0])/ df.r
	    } else {
                est.disp <- TRUE
                NaN
            }

    ## calculate scaled and unscaled covariance matrix

    aliased <- is.na(coef(object))  # used in print method
    p <- object$rank
    if (p > 0) {
        p1 <- 1L:p
        Qr <- object$qr
        ## WATCHIT! doesn't this rely on pivoting not permuting 1L:p? -- that's quaranteed
        coef.p <- object$coefficients[Qr$pivot[p1]]
        covmat.unscaled <- chol2inv(Qr$qr[p1,p1,drop=FALSE])
        dimnames(covmat.unscaled) <- list(names(coef.p),names(coef.p))
        covmat <- dispersion*covmat.unscaled
        var.cf <- diag(covmat)

        ## calculate coef table

        s.err <- sqrt(var.cf)
        tvalue <- coef.p/s.err

        dn <- c("Estimate", "Std. Error")
        if(!est.disp) { # known dispersion
            pvalue <- 2*pnorm(-abs(tvalue))
            coef.table <- cbind(coef.p, s.err, tvalue, pvalue)
            dimnames(coef.table) <- list(names(coef.p),
                                         c(dn, "z value","Pr(>|z|)"))
        } else if(df.r > 0) {
            pvalue <- 2*pt(-abs(tvalue), df.r)
            coef.table <- cbind(coef.p, s.err, tvalue, pvalue)
            dimnames(coef.table) <- list(names(coef.p),
                                         c(dn, "t value","Pr(>|t|)"))
        } else { # df.r == 0
            coef.table <- cbind(coef.p, NaN, NaN, NaN)
            dimnames(coef.table) <- list(names(coef.p),
                                         c(dn, "t value","Pr(>|t|)"))
        }
        df.f <- NCOL(Qr$qr)
    } else {
        coef.table <- matrix(, 0L, 4L)
        dimnames(coef.table) <-
            list(NULL, c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
        covmat.unscaled <- covmat <- matrix(, 0L, 0L)
        df.f <- length(aliased)
    }
    ## return answer

    ## these need not all exist, e.g. na.action.
    keep <- match(c("call","terms","family","deviance", "aic",
		      "contrasts", "df.residual","null.deviance","df.null",
                      "iter", "na.action"), names(object), 0L)
    ans <- c(object[keep],
	     list(deviance.resid = residuals(object, type = "deviance"),
		  coefficients = coef.table,
                  aliased = aliased,
		  dispersion = dispersion,
		  df = c(object$rank, df.r, df.f),
		  cov.unscaled = covmat.unscaled,
		  cov.scaled = covmat))

    if(correlation && p > 0) {
	dd <- sqrt(diag(covmat.unscaled))
	ans$correlation <-
	    covmat.unscaled/outer(dd,dd)
	ans$symbolic.cor <- symbolic.cor
    }
    class(ans) <- "summary.glm"
    return(ans)
}

print.summary.glm <-
    function (x, digits = max(3, getOption("digits") - 3),
	      symbolic.cor = x$symbolic.cor,
	      signif.stars = getOption("show.signif.stars"), ...)
{
    cat("\nCall:\n")
    cat(paste(deparse(x$call), sep="\n", collapse="\n"), "\n\n", sep="")
    cat("Deviance Residuals: \n")
    if(x$df.residual > 5) {
	x$deviance.resid <- quantile(x$deviance.resid,na.rm=TRUE)
	names(x$deviance.resid) <- c("Min", "1Q", "Median", "3Q", "Max")
    }
    print.default(x$deviance.resid, digits=digits, na.print = "",
                  print.gap = 2)

    if(length(x$aliased) == 0L) {
        cat("\nNo Coefficients\n")
    } else {
        ## df component added in 1.8.0
        ## partial matching problem here.
        df <- if ("df" %in% names(x)) x[["df"]] else NULL
        if (!is.null(df) && (nsingular <- df[3L] - df[1L]))
            cat("\nCoefficients: (", nsingular,
                " not defined because of singularities)\n", sep = "")
        else cat("\nCoefficients:\n")
        coefs <- x$coefficients
        if(!is.null(aliased <- x$aliased) && any(aliased)) {
            cn <- names(aliased)
            coefs <- matrix(NA, length(aliased), 4L,
                            dimnames=list(cn, colnames(coefs)))
            coefs[!aliased, ] <- x$coefficients
        }
        printCoefmat(coefs, digits=digits, signif.stars=signif.stars,
                     na.print="NA", ...)
    }
    ##
    cat("\n(Dispersion parameter for ", x$family$family,
	" family taken to be ", format(x$dispersion), ")\n\n",
	apply(cbind(paste(format(c("Null","Residual"), justify="right"),
                          "deviance:"),
		    format(unlist(x[c("null.deviance","deviance")]),
			   digits= max(5, digits+1)), " on",
		    format(unlist(x[c("df.null","df.residual")])),
		    " degrees of freedom\n"),
	      1L, paste, collapse=" "), sep="")
    if(nzchar(mess <- naprint(x$na.action))) cat("  (",mess, ")\n", sep="")
    cat("AIC: ", format(x$aic, digits= max(4, digits+1)),"\n\n",
	"Number of Fisher Scoring iterations: ", x$iter,
	"\n", sep="")

    correl <- x$correlation
    if(!is.null(correl)) {
# looks most sensible not to give NAs for undefined coefficients
#         if(!is.null(aliased) && any(aliased)) {
#             nc <- length(aliased)
#             correl <- matrix(NA, nc, nc, dimnames = list(cn, cn))
#             correl[!aliased, !aliased] <- x$correl
#         }
	p <- NCOL(correl)
	if(p > 1) {
	    cat("\nCorrelation of Coefficients:\n")
	    if(is.logical(symbolic.cor) && symbolic.cor) {# NULL < 1.7.0 objects
		print(symnum(correl, abbr.colnames = NULL))
	    } else {
		correl <- format(round(correl, 2), nsmall = 2, digits = digits)
		correl[!lower.tri(correl)] <- ""
		print(correl[-1, -p, drop=FALSE], quote = FALSE)
	    }
	}
    }
    cat("\n")
    invisible(x)
}


## GLM Methods for Generic Functions :

## needed to avoid deviance.lm
deviance.glm <- function(object, ...) object$deviance
effects.glm <- function(object, ...) object$effects
family.glm <- function(object, ...) object$family

residuals.glm <-
    function(object,
	     type = c("deviance", "pearson", "working", "response", "partial"),
	     ...)
{
    type <- match.arg(type)
    y <- object$y
    r <- object$residuals
    mu	<- object$fitted.values
    wts <- object$prior.weights
    switch(type,
           deviance=,pearson=,response=
           if(is.null(y)) {
               mu.eta <- object$family$mu.eta
               eta <- object$linear.predictors
               y <-  mu + r * mu.eta(eta)
           })
    res <- switch(type,
		  deviance = if(object$df.residual > 0) {
		      d.res <- sqrt(pmax((object$family$dev.resids)(y, mu, wts), 0))
		      ifelse(y > mu, d.res, -d.res)
		  } else rep.int(0, length(mu)),
		  pearson = (y-mu)*sqrt(wts)/sqrt(object$family$variance(mu)),
		  working = r,
		  response = y - mu,
		  partial = r
		  )
    if(!is.null(object$na.action))
        res <- naresid(object$na.action, res)
    if (type == "partial") ## need to avoid doing naresid() twice.
        res <- res+predict(object, type="terms")
    res
}

## For influence.glm() ... --> ./lm.influence.R

## KH on 1998/06/22: update.default() is now used ...

model.frame.glm <- function (formula, ...)
{
    dots <- list(...)
    nargs <- dots[match(c("data", "na.action", "subset"), names(dots), 0L)]
    if (length(nargs) || is.null(formula$model)) {
	fcall <- formula$call
	fcall$method <- "model.frame"
	fcall[[1L]] <- as.name("glm")
        fcall[names(nargs)] <- nargs
#	env <- environment(fcall$formula)  # always NULL
        env <- environment(formula$terms)
	if (is.null(env)) env <- parent.frame()
	eval(fcall, env)
    }
    else formula$model
}

weights.glm <- function(object, type = c("prior", "working"), ...)
{
    type <- match.arg(type)
    res <- if(type == "prior") object$prior.weights else object$weights
    if(is.null(object$na.action)) res
    else naresid(object$na.action, res)
}

formula.glm <- function(x, ...)
{
    form <- x$formula
    if( !is.null(form) ) {
        form <- formula(x$terms) # has . expanded
        environment(form) <- environment(x$formula)
        form
    } else formula(x$terms)
}
#  File src/library/stats/R/hclust.R
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

## Hierarchical clustering, on raw input data; we will use Euclidean
## distance.  A range of criteria are supported; also there is a
## storage-economic option.
##
## We use the general routine, `hc', which caters for 7 criteria,
## using a half dissimilarity matrix; (BTW, this uses the very efficient
## nearest neighbor chain algorithm, which makes this algorithm of
## O(n^2) computational time, and differentiates it from the less
## efficient -- i.e. O(n^3) -- implementations in all commercial
## statistical packages -- as far as I am aware -- except Clustan.)
##
## Clustering Methods:
##
## 1. Ward's minimum variance or error sum of squares method.
## 2. single linkage or nearest neighbor method.
## 3. complete linkage or diameter.
## 4. average linkage, group average, or UPGMA method.
## 5. McQuitty's or WPGMA method.
## 6. median, Gower's or WPGMC method.
## 7. centroid or UPGMC method (7).
##
## Original author: F. Murtagh, May 1992
## R Modifications: Ross Ihaka, Dec 1996
##		    Friedrich Leisch, Apr 1998, Jun 2000

hclust <- function(d, method="complete", members=NULL)
{
    METHODS <- c("ward", "single",
                 "complete", "average", "mcquitty",
                 "median", "centroid")
    method <-  pmatch(method, METHODS)
    if(is.na(method))
	stop("invalid clustering method")
    if(method == -1)
	stop("ambiguous clustering method")

    n <- as.integer(attr(d, "Size"))
    if(is.null(n))
	stop("invalid dissimilarities")
    if(n < 2)
        stop("must have n >= 2 objects to cluster")
    len <- as.integer(n*(n-1)/2)
    if(length(d) != len)
        (if (length(d) < len) stop else warning
         )("dissimilarities of improper length")

    if(is.null(members))
        members <- rep(1, n)
    else if(length(members) != n)
        stop("invalid length of members")

    hcl <- .Fortran("hclust",
		    n = n,
		    len = len,
		    method = as.integer(method),
		    ia = integer(n),
		    ib = integer(n),
		    crit = double(n),
		    members = as.double(members),
		    nn = integer(n),
		    disnn = double(n),
		    flag = logical(n),
		    diss = as.double(d), PACKAGE="stats")

    ## 2nd step: interpret the information that we now have
    ## as merge, height, and order lists.

    hcass <- .Fortran("hcass2",
		      n = as.integer(n),
		      ia = as.integer(hcl$ia),
		      ib = as.integer(hcl$ib),
  		      order = integer(n),
		      iia = integer(n),
		      iib = integer(n), PACKAGE="stats")

    tree <- list(merge = cbind(hcass$iia[1L:(n-1)], hcass$iib[1L:(n-1)]),
		 height= hcl$crit[1L:(n-1)],
		 order = hcass$order,
		 labels=attr(d, "Labels"),
                 method=METHODS[method],
                 call = match.call(),
                 dist.method = attr(d, "method"))
    class(tree) <- "hclust"
    tree
}

plot.hclust <-
    function (x, labels = NULL, hang = 0.1,
              axes = TRUE, frame.plot = FALSE, ann = TRUE,
              main = "Cluster Dendrogram",
              sub = NULL, xlab = NULL, ylab = "Height", ...)
{
    merge <- x$merge
    if (!is.matrix(merge) || ncol(merge) != 2)
	stop("invalid dendrogram")
    ## merge should be integer but might not be after dump/restore.
    if (any(as.integer(merge) != merge))
        stop("'merge' component in dendrogram must be integer")
    storage.mode(merge) <- "integer"
    n <- nrow(merge)
    height <- as.double(x$height)
    labels <-
	if(missing(labels) || is.null(labels)) {
	    if (is.null(x$labels))
		paste(1L:(n+1))
	    else
		as.character(x$labels)
	} else {
	    if(is.logical(labels) && !labels)# FALSE
		character(n+1)
	    else
		as.character(labels)
	}

    plot.new()
    .Internal(dend.window(n, merge, height,                 hang, labels, ...))
    .Internal(dend       (n, merge, height, order(x$order), hang, labels, ...))
    if(axes)
        axis(2, at=pretty(range(height)))
    if (frame.plot)
        box(...)
    if (ann) {
        if(!is.null(cl <- x$call) && is.null(sub))
            sub <- paste(deparse(cl[[1L]])," (*, \"", x$method,"\")",sep="")
        if(is.null(xlab) && !is.null(cl))
            xlab <- deparse(cl[[2L]])
        title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
    }
    invisible()
}

## For S ``compatibility'': was in cluster just as
## plclust <- plot.hclust ## .Alias
plclust <- function(tree, hang = 0.1, unit = FALSE, level = FALSE, hmin = 0,
                    square = TRUE, labels = NULL, plot. = TRUE,
                    axes = TRUE, frame.plot = FALSE, ann = TRUE,
                    main = "", sub = NULL, xlab = NULL, ylab = "Height")
{
    if(!missing(level) && level)	.NotYetUsed("level", error = FALSE)
    if(!missing(hmin) && hmin != 0)	.NotYetUsed("hmin",  error = FALSE)
    if(!missing(square) && !square)	.NotYetUsed("square",error = FALSE)
    if(!missing(plot.) && !plot.)	.NotYetUsed("plot.", error = TRUE)
    if(!missing(hmin)) tree$height <- pmax(tree$height, hmin)
    if(unit) tree$height <- rank(tree$height)
    plot.hclust(x = tree, labels = labels, hang = hang,
                axes = axes, frame.plot = frame.plot, ann = ann,
                main = main, sub = sub, xlab = xlab, ylab = ylab)
}


as.hclust <- function(x, ...) UseMethod("as.hclust")
## need *.default for idempotency:
as.hclust.default <- function(x, ...) {
    if(inherits(x, "hclust")) x
    else
	stop(gettext("argument 'x' cannot be coerced to class \"hclust\""),
             if(!is.null(oldClass(x)))
             gettextf("\n Consider providing an as.hclust.%s() method",
                      oldClass(x)[1L]), domain = NA)
}

as.hclust.twins <- function(x, ...)
{
    r <- list(merge = x$merge,
	      height = sort(x$height),
	      order = x$order,
	      labels = if(!is.null(lb <- x$order.lab)) {
                  lb[sort.list(x$order)] } else rownames(x$data),# may be NULL
	      call = if(!is.null(cl <- x$call)) cl else match.call(),
	      method = if(!is.null(mt <- x$method)) mt else NA,
	      dist.method = attr(x$diss, "Metric"))
    class(r) <- "hclust"
    r
}

print.hclust <- function(x, ...)
{
    if(!is.null(x$call))
        cat("\nCall:\n",deparse(x$call),"\n\n",sep="")
    if(!is.null(x$method))
        cat("Cluster method   :", x$method, "\n")
    if(!is.null(x$dist.method))
        cat("Distance         :", x$dist.method, "\n")
    cat("Number of objects:", length(x$height)+1, "\n")
    cat("\n")
    invisible(x)
}

cophenetic <-
function(x)
    UseMethod("cophenetic")
cophenetic.default <-
function(x)
{
    x <- as.hclust(x)
    nobs <- length(x$order)
    ilist <- vector("list", length = nobs)
    out <- matrix(0, nrow = nobs, ncol = nobs)
    for(i in 1 : (nobs - 1)) {
        inds <- x$merge[i,]
        ids1 <- if(inds[1L] < 0) -inds[1L] else ilist[[inds[1L]]]
        ids2 <- if(inds[2L] < 0) -inds[2L] else ilist[[inds[2L]]]
        ilist[[i]] <- c(ids1, ids2)
        out[cbind(rep.int(ids1, rep.int(length(ids2), length(ids1))),
                  rep.int(ids2, length(ids1)))] <- x$height[i]
    }
    rownames(out) <- x$labels
    as.dist(out + t(out))
}
cophenetic.dendrogram <-
function(x)
{
    ## Obtain cophenetic distances from a dendrogram by recursively
    ## doing the following:
    ## * if not a leaf, then for all children call ourselves, create
    ##   a block diagonal matrix from this, and fill the rest with the
    ##   current height (as everything in different children is joined
    ##   at the current split) ...
    ## * if a leaf, height and result are 0.
    ## Actually, we need to return something of class "dist", so things
    ## are a bit more complicated, and we might be able to make this
    ## more efficient by avoiding matrices ...
    if(is.leaf(x)) {
        ## If there is no label, we cannot recover the (names of the)
        ## objects the distances are for, and hence abort.
        if(is.null(label <- attr(x, "label")))
            stop("need dendrograms where all leaves have labels")
        return(as.dist(matrix(0, dimnames = list(label, label))))
    }
    children <- vector("list", length(x))
    for(i in seq_along(x))
        children[[i]] <- Recall(x[[i]])
    lens <- sapply(children, attr, "Size")
    m <- matrix(attr(x, "height"), sum(lens), sum(lens))
    ## This seems a bit slower:
    ##    inds <- split(seq(length = sum(lens)),
    ##                  rep.int(seq_along(lens), lens))
    ##    for(i in seq_along(inds))
    ##         m[inds[[i]], inds[[i]]] <- as.matrix(children[[i]])
    hi <- cumsum(lens)
    lo <- c(0, hi[-length(hi)]) + 1
    for(i in seq_along(x))
        m[lo[i] : hi[i], lo[i] : hi[i]] <- as.matrix(children[[i]])
    rownames(m) <- colnames(m) <-
        unlist(sapply(children, attr, "Labels"))
    as.dist(m)
}
#  File src/library/stats/R/htest.R
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

print.htest <- function(x, digits = 4, quote = TRUE, prefix = "", ...)
{
    cat("\n")
    cat(strwrap(x$method, prefix = "\t"), sep="\n")
    cat("\n")
    cat("data: ", x$data.name, "\n")
    out <- character()
    if(!is.null(x$statistic))
	out <- c(out, paste(names(x$statistic), "=",
			    format(round(x$statistic, 4))))
    if(!is.null(x$parameter))
	out <- c(out, paste(names(x$parameter), "=",
			    format(round(x$parameter, 3))))
    if(!is.null(x$p.value)) {
	fp <- format.pval(x$p.value, digits = digits)
	out <- c(out, paste("p-value",
			    if(substr(fp, 1L, 1L) == "<") fp else paste("=",fp)))
    }
    cat(strwrap(paste(out, collapse = ", ")), sep="\n")
    if(!is.null(x$alternative)) {
	cat("alternative hypothesis: ")
	if(!is.null(x$null.value)) {
	    if(length(x$null.value) == 1) {
		alt.char <-
		    switch(x$alternative,
			   two.sided = "not equal to",
			   less = "less than",
			   greater = "greater than")
		cat("true", names(x$null.value), "is", alt.char,
		    x$null.value, "\n")
	    }
	    else {
		cat(x$alternative, "\nnull values:\n")
		print(x$null.value, ...)
	    }
	}
	else cat(x$alternative, "\n")
    }
    if(!is.null(x$conf.int)) {
	cat(format(100 * attr(x$conf.int, "conf.level")),
	    "percent confidence interval:\n",
	    format(c(x$conf.int[1L], x$conf.int[2L])), "\n")
    }
    if(!is.null(x$estimate)) {
	cat("sample estimates:\n")
	print(x$estimate, ...)
    }
    cat("\n")
    invisible(x)
}
#  File src/library/stats/R/identify.hclust.R
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

rect.hclust <- function(tree, k=NULL, which=NULL,
                        x=NULL, h=NULL, border=2, cluster=NULL)
{
    if(length(h)>1 | length(k)>1)
        stop("'k' and 'h' must be a scalar")

    if(!is.null(h)){
        if(!is.null(k))
            stop("specify exactly one of 'k' and 'h'")
        k <- min(which(rev(tree$height)<h))
        k <- max(k, 2)
    }
    else
        if(is.null(k))
            stop("specify exactly one of 'k' and 'h'")

    if(k < 2 | k > length(tree$height))
        stop(gettextf("k must be between 2 and %d", length(tree$height)),
             domain = NA)

    if(is.null(cluster))
        cluster <- cutree(tree, k=k)
    ## cutree returns classes sorted by data, we need classes
    ## as occurring in the tree (from left to right)
    clustab <- table(cluster)[unique(cluster[tree$order])]
    m <- c(0, cumsum(clustab))

    if(!is.null(x)){
        if(!is.null(which))
            stop("specify exactly one of 'which' and 'x'")
        which <- x
        for(n in seq_along(x))
            which[n] <- max(which(m<x[n]))
    }
    else
        if(is.null(which))
            which <- 1L:k

    if(any(which>k))
        stop(gettextf("all elements of 'which' must be between 1 and %d", k),
             domain = NA)

    border <- rep(border, length.out = length(which))

    retval <- list()
    for(n in seq_along(which)) {
        rect(m[which[n]]+0.66, par("usr")[3L],
             m[which[n]+1]+0.33, mean(rev(tree$height)[(k-1):k]),
             border = border[n])
        retval[[n]] <- which(cluster==as.integer(names(clustab)[which[n]]))
    }
    invisible(retval)
}

identify.hclust <- function(x, FUN = NULL, N = 20, MAXCLUSTER = 20,
                            DEV.FUN = NULL, ...)
{
    cluster <- cutree(x, k = 2:MAXCLUSTER)

    retval <- list()
    oldk <- NULL
    oldx <- NULL
    DEV.x <- grDevices::dev.cur()

    for(n in 1L:N){

        grDevices::dev.set(DEV.x)
        X <- locator(1)
        if(is.null(X))
            break

        k <- min(which(rev(x$height) < X$y), MAXCLUSTER)
        k <- max(k, 2)
        if(!is.null(oldx)){
            rect.hclust(x, k = oldk, x = oldx, cluster = cluster[, oldk-1],
                        border = "grey")
        }
        retval[[n]] <- unlist(rect.hclust(x, k = k, x = X$x,
                                          cluster = cluster[, k-1],
                                          border = "red"))
        if(!is.null(FUN)){
            if(!is.null(DEV.FUN)){
                grDevices::dev.set(DEV.FUN)
            }
            retval[[n]] <- FUN(retval[[n]], ...)
        }

        oldx <- X$x
        oldk <- k
    }
    grDevices::dev.set(DEV.x)
    invisible(retval)
}
#  File src/library/stats/R/integrate.R
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

integrate<- function(f, lower, upper, ..., subdivisions=100,
		     rel.tol = .Machine$double.eps^.25,
		     abs.tol = rel.tol, stop.on.error = TRUE,
		     keep.xy = FALSE, aux = NULL)
{
    f <- match.fun(f)
    ff <- function(x) f(x, ...)
    limit <- as.integer(subdivisions)
    if (limit < 1 || (abs.tol <= 0 &&
	rel.tol < max(50*.Machine$double.eps, 0.5e-28)))
	stop("invalid parameter values")
    if(is.finite(lower) && is.finite(upper)) {
	wk <- .External("call_dqags",
			ff, rho = environment(),
			as.double(lower), as.double(upper),
			as.double(abs.tol), as.double(rel.tol),
			limit = limit,
			PACKAGE = "base")
    } else { # indefinite integral
	if(is.na(lower) || is.na(upper)) stop("a limit is missing")
	if (is.finite(lower)) {
	    inf <- 1
	    bound <- lower
	} else if (is.finite(upper)) {
	    inf <- -1
	    bound <- upper
	} else {
	    inf <- 2
	    bound <- 0.0
	}
	wk <- .External("call_dqagi",
			ff, rho = environment(),
			as.double(bound), as.integer(inf),
			as.double(abs.tol), as.double(rel.tol),
			limit = limit,
			PACKAGE = "base")
    }
    res <- wk[c("value", "abs.error", "subdivisions")]
    res$message <-
	switch(wk$ierr + 1,
	       "OK",
	       "maximum number of subdivisions reached",
	       "roundoff error was detected",
	       "extremely bad integrand behaviour",
	       "roundoff error is detected in the extrapolation table",
	       "the integral is probably divergent",
	       "the input is invalid")
    if(wk$ierr == 6 || (wk$ierr > 0 && stop.on.error)) stop(res$message)
    res$call <- match.call()
    class(res) <- "integrate"
    res
}

print.integrate <- function (x, digits=getOption("digits"), ...)
{
    if(x$message == "OK") cat(format(x$value, digits=digits),
       " with absolute error < ", format(x$abs.error, digits=2),
       "\n", sep = "")
    else cat("failed with message ", sQuote(x$message), "\n", sep = "")
    invisible(x)
}
#  File src/library/stats/R/interaction.plot.R
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

interaction.plot <-
    function(x.factor, trace.factor, response, fun=mean,
             type = c("l", "p", "b"), legend = TRUE,
             trace.label=deparse(substitute(trace.factor)), fixed=FALSE,
             xlab = deparse(substitute(x.factor)), ylab = ylabel,
             ylim = range(cells, na.rm=TRUE),
             lty = nc:1, col = 1, pch = c(1L:9, 0, letters),
             xpd = NULL, leg.bg = par("bg"), leg.bty = "n",
             xtick = FALSE, xaxt = par("xaxt"), axes = TRUE, ...)
{
    ylabel <- paste(deparse(substitute(fun)), "of ",
                    deparse(substitute(response)))
    type <- match.arg(type)
    cells <- tapply(response, list(x.factor, trace.factor), fun)
    nr <- nrow(cells); nc <- ncol(cells)
    xvals <- 1L:nr
    ## See if the x.factor labels are a sensible scale
    if(is.ordered(x.factor)) {
        wn <- getOption("warn")
        options(warn=-1)
        xnm <- as.numeric(levels(x.factor))
        options(warn=wn)
        if(!any(is.na(xnm))) xvals <- xnm
    }
    xlabs <- rownames(cells)
    ylabs <- colnames(cells)
    nch <- max(sapply(ylabs, nchar, type="width"))
    if(is.null(xlabs)) xlabs <- as.character(xvals)
    if(is.null(ylabs)) ylabs <- as.character(1L:nc)
    xlim <- range(xvals)
    xleg <- xlim[2L] + 0.05 * diff(xlim)
    xlim <- xlim + c(-0.2/nr, if(legend) 0.2 + 0.02*nch else 0.2/nr) * diff(xlim)
    matplot(xvals, cells, ..., type = type, xlim = xlim, ylim = ylim,
            xlab = xlab, ylab = ylab, axes = axes, xaxt = "n",
            col = col, lty = lty, pch = pch)
    if(axes && xaxt != "n") {
	## swallow ... arguments intended for matplot():
	axisInt <- function(x, main, sub, lwd, bg, log, asp, ...)
	    axis(1, x, ...)
	mgp. <- par("mgp") ; if(!xtick) mgp.[2L] <- 0
	axisInt(1, at = xvals, labels = xlabs, tick = xtick, mgp = mgp.,
		xaxt = xaxt, ...)
    }
    if(legend) {
        yrng <- diff(ylim)
        yleg <- ylim[2L] - 0.1 * yrng
        if(!is.null(xpd) || { xpd. <- par("xpd")
                              !is.na(xpd.) && !xpd. && (xpd <- TRUE)}) {
            op <- par(xpd = xpd)
            on.exit(par(op))
        }
        text(xleg, ylim[2L] - 0.05 * yrng, paste("  ", trace.label), adj = 0)
        if(!fixed) {
            ## sort them on the value at the last level of x.factor
            ord <- sort.list(cells[nr,  ], decreasing = TRUE)
            ylabs <- ylabs[ord]
            lty <- lty[1 + (ord - 1) %% length(lty)]
            col <- col[1 + (ord - 1) %% length(col)]
            pch <- pch[ord]
        }

        legend(xleg, yleg, legend = ylabs, col = col,
               pch = if(type %in% c("p","b")) pch,# NULL works
               lty = if(type %in% c("l","b")) lty,# NULL works
               bty = leg.bty, bg = leg.bg)
    }
    invisible()
}
#  File src/library/stats/R/isoreg.R
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

### Isotonic Regression --- original code is simplification of MASS' Shepard():
##
isoreg <- function(x, y=NULL)
{
    xy <- xy.coords(x,y)
    x <- xy$x
    if(any(is.na(x)) || any(is.na(xy$y)))
	stop("missing values not allowed")
    isOrd <- ((!is.null(xy$xlab) && xy$xlab == "Index")
              || !is.unsorted(x, strictly = TRUE))
    if(!isOrd) {
	y <- xy$y
	ord <- order(x, -y) ## 'increasing in x, decreasing in y'
	y <- y[ord]
    }
    z <- .Call("R_isoreg", if(isOrd)xy$y else y, PACKAGE = "stats")
    structure(c(xy[c("x","y")], z[c("yf","yc","iKnots")],
                list(isOrd = isOrd, ord = if(!isOrd) ord,
                     call = match.call())),
	      class = "isoreg")
}

fitted.isoreg <- function(object, ...)
{
    if(object$isOrd) object$yf
    else object$yf[order(object$ord)]
}

residuals.isoreg <- function(object, ...) object$y - fitted(object)

print.isoreg <- function(x, digits = getOption("digits"), ...)
{
  cat("Isotonic regression from ", deparse(x$call), ",\n", sep="")
  cat("  with", length(x$iKnots), "knots / breaks at obs.nr.", x$iKnots, ";\n")
  if(x$isOrd) cat("  initially ordered 'x'\n")
  else { cat("  (x,y) ordering:"); str(x$ord) }
  cat("  and further components ")
  str(x[1L:4], digits.d = 3 + max(0,digits - 7))
  invisible(x)
}

lines.isoreg <- function(x, col = "red", lwd = 1.5,
			 do.points = FALSE, cex = 1.5, pch = 13, ...)
{
    xx <- if(x$isOrd) x$x else x$x[x$ord]
    lines (xx, x$yf, col = col, lwd = lwd, type = "S")
    if(do.points)
	points(xx[x$iKnots], x$yf[x$iKnots], col = col, cex = cex, pch = pch)
    invisible()
}

plot.isoreg <-
    function(x, plot.type = c("single", "row.wise", "col.wise"),
	     main = paste("Isotonic regression", deparse(x$call)),
	     main2= "Cumulative Data and Convex Minorant",
	     xlab = "x0", ylab = "x$y",
	     par.fit = list(col = "red", cex = 1.5, pch = 13, lwd = 1.5),
	     mar = if(both) .1 + c(3.5,2.5,1,1) else par("mar"),
	     mgp = if(both) c(1.6, 0.7, 0) else par("mgp"),
	     grid = length(x$x) < 12,
	     ...)
{
    plot.type <- match.arg(plot.type)
    both <- plot.type != "single"
    if(both) {
	col.wise <- plot.type == "col.wise"
	if(!is.null(main)) main.wid <- 2
	op <- par(mfcol = if(col.wise) 1L:2 else 2:1,
		  oma = c(0,0, main.wid, 0), mar = mar, mgp = mgp)
    } else
	op <- par(mar = mar, mgp = mgp)

    on.exit(par(op))

    xx <- if(x$isOrd) x$x else x$x[x$ord]
    x0 <- c(xx[1L] - mean(diff(xx)), xx)# 1 pt left
    cy <- x$yc # = cumsum(c(0, x$y[ordered]))
    cf <- cumsum(c(0, x$yf))

    ##Dbg i <- abs(cy - cf) < 1e-10 * abs(cy + cf)## cy == cf
    ##Dbg if(!identical(which(i[-1L]), x$iKnots))
    ##Dbg    warning("x$iKnots differs from which(i[-1L]) ..")

    ## Plot of "Data" + Fit
    plot(x0, c(NA, if(x$isOrd) x$y else x$y[x$ord]), ...,
	 xlab = xlab, ylab = ylab, main = if(!both) main)
    lines (xx, x$yf, col = par.fit$col, lwd = par.fit$lwd, type = "S")
    points(xx[x$iKnots], x$yf[x$iKnots], col = par.fit$col,
           cex = par.fit$cex, pch = par.fit$pch)
    if(grid) grid()
    if(both) { ## Cumulative Plot
	plot (x0, cy, type = "n", xlab = xlab,
	      ylab = paste("cumsum(", ylab, ")", sep=""), ylim = range(cy, cf),
              ...)
        i <- 1L + x$iKnots
        lines(x0, cf, col = par.fit$col, lwd = par.fit$lwd)
        points(x0[i], cy[i], col = par.fit$col, cex = par.fit$cex,
               pch = par.fit$pch)
	if(grid) {
	    Agrid <- formals("grid")
	    abline(v = x0[i], col = Agrid$col, lty = Agrid$lty,
                   xpd = !col.wise)
	}
	points(x0[-1L], cy[-1L])# over draw
	if(!is.null(main2))
	    mtext(main2, cex = par("cex.main"),
		  col = par("col.main"), font = par("font.main"))
	if(!is.null(main))
	    mtext(main, side = 3, outer = TRUE, cex = par("cex.main"),
		  col = par("col.main"), font = par("font.main"))
    }
    invisible()
}
#  File src/library/stats/R/kernel.R
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

## Copyright (C) 1997-1999  Adrian Trapletti
## Copyright (C) 1999-2006  The R Development Core Team
## This version distributed under LGPL (version 2 or later)


kernel <- function (coef, m = length(coef)+1, r, name="unknown")
{
    mkName <- function(name, args)
        paste(name,"(", paste(args, collapse=","), ")", sep="")

    modified.daniell.kernel <- function (m)
    {
        if(length(m) == 1L)
            k <- kernel(c(rep(1, m), 0.5)/(2*m), m)
        else {
            k <- Recall(m[1L])
            for(i in 2L:length(m)) k <- kernapply(k,  Recall(m[i]))
        }
        attr(k,"name") <- mkName("mDaniell", m)
        k
    }

    daniell.kernel <- function (m)
    {
        if(length(m) == 1L)
            k <- kernel(rep(1/(2*m+1),m+1), m)
        else {
            k <- Recall(m[1L])
            for(i in 2L:length(m)) k <- kernapply(k,  Recall(m[i]))
        }
        attr(k,"name") <- mkName("Daniell", m)
        k
    }

    fejer.kernel <- function (m, r)
    {
        if (r < 1L) stop ("'r' is less than 1")
        if (m < 1L) stop ("'m' is less than 1")
        n <- 2L*m+1L
        wn <- double(m+1L)
        wj <- 2*pi*(1L:m)/n
        wn[2L:(m+1L)] <- sin(r*wj/2)^2 / sin(wj/2)^2 / r
        wn[1L] <- r
        wn <- wn / (wn[1L] + 2*sum(wn[2L:(m+1L)]))
        kernel(wn, m, name = mkName("Fejer", c(m,r)))
    }

    dirichlet.kernel <- function (m, r)
    {
        if (r < 0) stop ("'r' is less than 0")
        if (m < 1) stop ("'m' is less than 1")
        n <- 2L*m+1L
        wn <- double(m+1L)
        wj <- 2*pi*(1L:m)/n
        wn[2L:(m+1)] <- sin((r+0.5)*wj) / sin(wj/2)
        wn[1L] <- 2*r+1
        wn <- wn / (wn[1L] + 2*sum(wn[2L:(m+1L)]))
        kernel(wn, m, name = mkName("Dirichlet", c(m,r)))
    }

    if(!missing(m))
	if(!is.numeric(m) || length(m) < 1L || m != round(m) || any(m < 0L))
	    stop("'m' must be numeric with non-negative integers")

    if(is.character(coef)) {
        switch(coef,
               daniell = daniell.kernel(m),
               dirichlet = dirichlet.kernel(m, r),
               fejer = fejer.kernel(m, r),
               modified.daniell = modified.daniell.kernel(m),
               stop("unknown named kernel"))
    } else {
        if (!is.vector(coef))
            stop ("'coef' must be a vector")
        if ((length(coef) != m+1) | (length(coef) <= 0))
            stop ("'coef' does not have the correct length")
        kernel <- list (coef=coef, m=m)
        attr(kernel, "name") <- name
        class(kernel) <- "tskernel"
        sk <- sum(kernel[-m:m]) # via '[.kernel' !
        if (abs(sk - 1) > getOption("ts.eps"))
            stop ("coefficients do not add to 1")
        kernel
    }
}

print.tskernel <- function (x, digits = max(3,getOption("digits")-3), ...)
{
    m <- x$m
    y <- x[i <- -m:m]
    cat(attr(x, "name"), "\n")
    cat(paste("coef[", format(i), "] = ", format(y, digits = digits), sep = ""),
        sep = "\n")
    invisible(x)
}

plot.tskernel <-
    function(x, type = "h", xlab = "k", ylab = "W[k]", main=attr(x,"name"), ...)
{
    i <- -x$m:x$m
    plot(i, x[i], type = type, xlab = xlab, ylab = ylab, main = main, ...)
}

df.kernel <- function (k)
{
    2/sum(k[-k$m:k$m]^2)
}

bandwidth.kernel <- function (k)
{
    i <- -k$m:k$m
    sqrt(sum((1/12 + i^2) * k[i]))
}


"[.tskernel" <- function (k, i)
{
    m1 <- k$m + 1L
    y <- k$coef[c(m1:2L, 1L:m1)]
    y[i+m1]
}

is.tskernel <- function (k)
{
    inherits(k, "tskernel")
}

kernapply <- function (x, ...)
{
    UseMethod("kernapply")
}

kernapply.vector <- function (x, k, circular = FALSE, ...)
{
    if (!is.vector(x)) stop ("'x' is not a vector")
    if (!is.tskernel(k)) stop ("'k' is not a kernel")
    m <- k$m
    if (length(x) <= 2L*m)
        stop ("'x' is shorter than kernel 'k'")
    if (m == 0L)
        return (x)
    else
    {
        n <- length(x)
        w <- c(k[0L:m], rep(0,n-2L*m-1L), k[-m:-1L])
        y <- fft(fft(x)*fft(w), inverse = TRUE)/n
        if (is.numeric(x)) y <- Re(y)
        if (circular)
            return (y)
        else
            return (y[(1L+m):(n-m)])
    }
}

kernapply.default <- function (x, k, circular = FALSE, ...)
{
    if (is.vector(x))
        return (kernapply.vector(x, k, circular=circular))
    else if (is.matrix(x))
        return (apply(x, MARGIN=2, FUN=kernapply, k, circular=circular))
    else
        stop ("'kernapply' is not available for object 'x'")
}

kernapply.ts <- function (x, k, circular = FALSE, ...)
{
    if (!is.matrix(x))
        y <- kernapply.vector(as.vector(x), k, circular=circular)
    else
        y <- apply(x, MARGIN=2L, FUN=kernapply, k, circular=circular)
    ts (y, end=end(x), frequency=frequency(x))
}

kernapply.tskernel <- function (x, k, ...)
{
    if (!is.tskernel(x))
        stop ("'x' is not a kernel")
    if (!is.tskernel(k))
        stop ("'k' is not a kernel")
    n <- k$m
    xx <- c(rep(0,n), x[-x$m:x$m], rep(0,n))
    coef <- kernapply(xx, k, circular = TRUE)
    m <- length(coef)%/%2L
    kernel(coef[(m+1L):length(coef)],m,
           paste("Composite(", attr(x, "name"), ",",
                 attr(k, "name"), ")", sep=""))
}
#  File src/library/stats/R/kmeans.R
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

kmeans <-
function(x, centers, iter.max = 10, nstart = 1,
         algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
{
    do_one <- function(nmeth) {
        Z <-
            switch(nmeth,
                   { # 1
                       Z <- .Fortran("kmns", as.double(x), as.integer(m),
                                as.integer(ncol(x)),
                                centers = as.double(centers),
                                as.integer(k), c1 = integer(m), integer(m),
                                nc = integer(k), double(k), double(k), integer(k),
                                double(m), integer(k), integer(k),
                                as.integer(iter.max), wss = double(k),
                                ifault = as.integer(0L), PACKAGE="stats")
                       switch(Z$ifault,
                              stop("empty cluster: try a better set of initial centers",
                                   call.=FALSE),
                              warning(gettextf("did not converge in %d iterations",
                                               iter.max), call.=FALSE, domain =NA),
                              stop("number of cluster centres must lie between 1 and nrow(x)",
                                   call.=FALSE)
                              )
                       Z
                   },
                   { # 2
                       Z <- .C("kmeans_Lloyd", as.double(x), as.integer(m),
                               as.integer(ncol(x)),
                               centers = as.double(centers), as.integer(k),
                               c1 = integer(m), iter = as.integer(iter.max),
                               nc = integer(k), wss = double(k),
                               PACKAGE="stats")
                       if(Z$iter > iter.max)
                           warning("did not converge in ",
                                  iter.max, " iterations", call.=FALSE)
                       if(any(Z$nc == 0))
                           warning("empty cluster: try a better set of initial centers", call.=FALSE)
                       Z
                   },
                   { # 3
                       Z <- .C("kmeans_MacQueen", as.double(x), as.integer(m),
                               as.integer(ncol(x)),
                               centers = as.double(centers), as.integer(k),
                               c1 = integer(m), iter = as.integer(iter.max),
                               nc = integer(k), wss = double(k),
                               PACKAGE="stats")
                       if(Z$iter > iter.max)
                           warning("did not converge in ",
                                   iter.max, " iterations", call.=FALSE)
                       if(any(Z$nc == 0))
                           warning("empty cluster: try a better set of initial centers", call.=FALSE)
                       Z
                    })
        Z
    }
    x <- as.matrix(x)
    m <- nrow(x)
    if(missing(centers))
	stop("'centers' must be a number or a matrix")
    algorithm <- match.arg(algorithm)
    nmeth <- switch(algorithm,
                    "Hartigan-Wong" = 1,
                    "Lloyd" = 2, "Forgy" = 2,
                    "MacQueen" = 3)
    if(length(centers) == 1) {
	k <- centers
        ## we need to avoid duplicates here
        if(nstart == 1)
            centers <- x[sample.int(m, k), , drop = FALSE]
        if(nstart >= 2 || any(duplicated(centers))) {
            cn <- unique(x)
            mm <- nrow(cn)
            if(mm < k)
                stop("more cluster centers than distinct data points.")
            centers <- cn[sample.int(mm, k), , drop=FALSE]
        }
    } else {
	centers <- as.matrix(centers)
        if(any(duplicated(centers)))
            stop("initial centers are not distinct")
        cn <- NULL
	k <- nrow(centers)
        if(m < k)
            stop("more cluster centers than data points")
    }
    if(iter.max < 1) stop("'iter.max' must be positive")
    if(ncol(x) != ncol(centers))
	stop("must have same number of columns in 'x' and 'centers'")
    Z <- do_one(nmeth)
    if(nstart >= 2 && !is.null(cn)) {
        best <- sum(Z$wss)
        for(i in 2:nstart) {
            centers <- cn[sample.int(mm, k), , drop=FALSE]
            ZZ <- do_one(nmeth)
            if((z <- sum(ZZ$wss)) < best) {
                Z <- ZZ
                best <- z
            }
        }
    }
    centers <- matrix(Z$centers, k)
    dimnames(centers) <- list(1L:k, dimnames(x)[[2L]])
    cluster <- Z$c1
    if(!is.null(rn <- rownames(x)))
        names(cluster) <- rn
    out <- list(cluster = cluster, centers = centers, withinss = Z$wss,
                size = Z$nc)
    class(out) <- "kmeans"
    out
}

## modelled on print methods in the cluster package
print.kmeans <- function(x, ...)
{
    cat("K-means clustering with ", length(x$size), " clusters of sizes ",
        paste(x$size, collapse=", "), "\n", sep="")
    cat("\nCluster means:\n")
    print(x$centers, ...)
    cat("\nClustering vector:\n")
    print(x$cluster, ...)
    cat("\nWithin cluster sum of squares by cluster:\n")
    print(x$withinss, ...)
    cat("\nAvailable components:\n")
    print(names(x))
    invisible(x)
}
#  File src/library/stats/R/kruskal.test.R
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

kruskal.test <- function(x, ...) UseMethod("kruskal.test")

kruskal.test.default <-
function(x, g, ...)
{
    if (is.list(x)) {
        if (length(x) < 2)
            stop("'x' must be a list with at least 2 elements")
        DNAME <- deparse(substitute(x))
        x <- lapply(x, function(u) u <- u[complete.cases(u)])
        k <- length(x)
        l <- sapply(x, "length")
        if (any(l == 0))
            stop("all groups must contain data")
        g <- factor(rep(1 : k, l))
        x <- unlist(x)
    }
    else {
        if (length(x) != length(g))
            stop("'x' and 'g' must have the same length")
        DNAME <- paste(deparse(substitute(x)), "and",
                       deparse(substitute(g)))
        OK <- complete.cases(x, g)
        x <- x[OK]
        g <- g[OK]
        if (!all(is.finite(g)))
            stop("all group levels must be finite")
        g <- factor(g)
        k <- nlevels(g)
        if (k < 2)
            stop("all observations are in the same group")
    }

    n <- length(x)
    if (n < 2)
        stop("not enough observations")
    r <- rank(x)
    TIES <- table(x)
    STATISTIC <- sum(tapply(r, g, "sum")^2 / tapply(r, g, "length"))
    STATISTIC <- ((12 * STATISTIC / (n * (n + 1)) - 3 * (n + 1)) /
                  (1 - sum(TIES^3 - TIES) / (n^3 - n)))
    PARAMETER <- k - 1
    PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
    names(STATISTIC) <- "Kruskal-Wallis chi-squared"
    names(PARAMETER) <- "df"

    RVAL <- list(statistic = STATISTIC,
                 parameter = PARAMETER,
                 p.value = PVAL,
                 method = "Kruskal-Wallis rank sum test",
                 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}

kruskal.test.formula <-
function(formula, data, subset, na.action, ...)
{
    if(missing(formula) || (length(formula) != 3))
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m[[1L]] <- as.name("model.frame")
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " by ")
    names(mf) <- NULL
    y <- do.call("kruskal.test", as.list(mf))
    y$data.name <- DNAME
    y
}
#  File src/library/stats/R/ks.test.R
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

ks.test <-
function(x, y, ..., alternative = c("two.sided", "less", "greater"),
         exact = NULL)
{

    pkolmogorov1x <- function(x, n) {
        ## Probability function for the one-sided one-sample Kolmogorov
        ## statistics, based on the formula of Birnbaum & Tingey (1951).
        if(x <= 0) return(0)
        if(x >= 1) return(1)
        j <- seq.int(from = 0, to = floor(n * (1 - x)))
        1 - x * sum(exp(lchoose(n, j)
                        + (n - j) * log(1 - x - j / n)
                        + (j - 1) * log(x + j / n)))
    }

    alternative <- match.arg(alternative)
    DNAME <- deparse(substitute(x))
    x <- x[!is.na(x)]
    n <- length(x)
    if(n < 1L)
        stop("not enough 'x' data")
    PVAL <- NULL

    if(is.numeric(y)) {
        DNAME <- paste(DNAME, "and", deparse(substitute(y)))
        y <- y[!is.na(y)]
        n.x <- as.double(n)             # to avoid integer overflow
        n.y <- length(y)
        if(n.y < 1L)
            stop("not enough 'y' data")
        if(is.null(exact))
            exact <- (n.x * n.y < 10000)
        METHOD <- "Two-sample Kolmogorov-Smirnov test"
        TIES <- FALSE
        n <- n.x * n.y / (n.x + n.y)
        w <- c(x, y)
        z <- cumsum(ifelse(order(w) <= n.x, 1 / n.x, - 1 / n.y))
        if(length(unique(w)) < (n.x + n.y)) {
            warning("cannot compute correct p-values with ties")
            z <- z[c(which(diff(sort(w)) != 0), n.x + n.y)]
            TIES <- TRUE
        }
        STATISTIC <- switch(alternative,
                            "two.sided" = max(abs(z)),
                            "greater" = max(z),
                            "less" = - min(z))
        nm_alternative <- switch(alternative,
                                 "two.sided" = "two-sided",
                                 "less" = "the CDF of x lies below that of y",
                                 "greater" = "the CDF of x lies above that of y")
        if(exact && (alternative == "two.sided") && !TIES)
            PVAL <- 1 - .C("psmirnov2x",
                           p = as.double(STATISTIC),
                           as.integer(n.x),
                           as.integer(n.y),
                           PACKAGE = "stats")$p
    }
    else {
        if(is.character(y))
            y <- get(y, mode="function")
        if(mode(y) != "function")
            stop("'y' must be numeric or a string naming a valid function")
        if(is.null(exact))
            exact <- (n < 100)
        METHOD <- "One-sample Kolmogorov-Smirnov test"
        TIES <- FALSE
        if(length(unique(x)) < n) {
            warning("cannot compute correct p-values with ties")
            TIES <- TRUE
        }
        x <- y(sort(x), ...) - (0 : (n-1)) / n
        STATISTIC <- switch(alternative,
                            "two.sided" = max(c(x, 1/n - x)),
                            "greater" = max(1/n - x),
                            "less" = max(x))
        if(exact && !TIES) {
            PVAL <- if(alternative == "two.sided")
                1 - .C("pkolmogorov2x",
                           p = as.double(STATISTIC),
                           as.integer(n),
                           PACKAGE = "stats")$p
            else
                1 - pkolmogorov1x(STATISTIC, n)
        }
        nm_alternative <- switch(alternative,
                                 "two.sided" = "two-sided",
                                 "less" = "the CDF of x lies below the null hypothesis",
                                 "greater" = "the CDF of x lies above the null hypothesis")

    }

    names(STATISTIC) <- switch(alternative,
                               "two.sided" = "D",
                               "greater" = "D^+",
                               "less" = "D^-")

    pkstwo <- function(x, tol = 1e-6) {
        ## Compute \sum_{-\infty}^\infty (-1)^k e^{-2k^2x^2}
        ## Not really needed at this generality for computing a single
        ## asymptotic p-value as below.
        if(is.numeric(x))
            x <- as.vector(x)
        else
            stop("argument 'x' must be numeric")
        p <- rep(0, length(x))
        p[is.na(x)] <- NA
        IND <- which(!is.na(x) & (x > 0))
        if(length(IND)) {
            p[IND] <- .C("pkstwo",
                         as.integer(length(x[IND])),
                         p = as.double(x[IND]),
                         as.double(tol),
                         PACKAGE = "stats")$p
        }
        return(p)
    }

    if(is.null(PVAL)) {
        ## <FIXME>
        ## Currently, p-values for the two-sided two-sample case are
        ## exact if n.x * n.y < 10000 (unless controlled explicitly).
        ## In all other cases, the asymptotic distribution is used
        ## directly.  But: let m and n be the min and max of the sample
        ## sizes, respectively.  Then, according to Kim and Jennrich
        ## (1973), if m < n / 10, we should use the
        ## * Kolmogorov approximation with c.c. -1/(2*n) if 1 < m < 80;
        ## * Smirnov approximation with c.c. 1/(2*sqrt(n)) if m >= 80.
        PVAL <- ifelse(alternative == "two.sided",
                       1 - pkstwo(sqrt(n) * STATISTIC),
                       exp(- 2 * n * STATISTIC^2))
        ## </FIXME>
    }

    RVAL <- list(statistic = STATISTIC,
                 p.value = PVAL,
                 alternative = nm_alternative,
                 method = METHOD,
                 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}
#  File src/library/stats/R/ksmooth.R
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

ksmooth <-
  function(x, y, kernel=c("box", "normal"), bandwidth=0.5, range.x=range(x),
	   n.points=max(100, length(x)), x.points)
{
    ## box is [-0.5, 0.5]. normal is sd = 1.4826/4
    if(missing(y))
	stop("y must be supplied.\nFor density estimation use density()")
    kernel <- match.arg(kernel)
    krn <- switch(kernel, "box" = 1, "normal" = 2)
    x.points <-
	if(missing(x.points))
	    seq.int(range.x[1L], range.x[2L], length.out = n.points)
	else {
	    n.points <- length(x.points)
	    sort(x.points)
	}
    ord <- order(x)
    z <- .C("BDRksmooth",
	    as.double(x[ord]),
	    as.double(y[ord]),
	    as.integer(length(x)),
	    xp=as.double(x.points),
	    yp=double(n.points),
	    as.integer(n.points),
	    as.integer(krn),
	    as.double(bandwidth),
	    PACKAGE="stats"
	    )
    list(x=z$xp, y=z$yp)
}

#  File src/library/stats/R/lag.R
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

lag <- function(x, ...) UseMethod("lag")

lag.default <- function(x, k = 1, ...)
{
    if(k != round(k)) {
        k <- round(k)
        warning("'k' is not an integer")
    }
    x <- hasTsp(x)
    p <- tsp(x)
    tsp(x) <- p - (k/p[3L]) * c(1, 1, 0)
    x
}
#  File src/library/stats/R/lag.plot.R
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

## Function exists in S-plus

## Differences:
## 1) R has `type = "p"' argument
##    Idea: use "b" for n <= 10, else "p" as default, allow "text" / "labels" !
## 2) R uses `main', not `head' {consistency!}
## 3) R has `oma' and `...' args
## 4) R has  ask = par("ask") where S-plus has  ask = FALSE,
## ....

lag.plot <- function(x, lags = 1, layout = NULL, set.lags = 1L:lags,
                     main = NULL, asp = 1,
                     diag = TRUE, diag.col = "gray", type = "p", oma = NULL,
                     ask = NULL, do.lines = (n <= 150), labels = do.lines, ...)
{
    lAxis <- function(side , ..., mgp, xpd, panel, Mgp)
        if(missing(Mgp)) axis(side, ..., xpd = NA)
        else axis(side, ..., xpd = NA, mgp = Mgp)

    xnam <- deparse(substitute(x))
    is.mat <- !is.null(ncol(x))
    nser <- ncol(x <- as.ts(as.matrix(x)))
    n <- nrow(x)

    if(missing(lags) && !missing(set.lags))
        lags <- length(set.lags <- as.integer(set.lags))
    tot.lags <- nser * lags

    if(is.null(ask)) {
        if (.Device == "null device") dev.new()
        ask <-
            if(is.null(layout)) par("ask") ## FALSE, since will have big layout
            else (dev.interactive() && prod(layout) < tot.lags)
    }
    if(is.null(layout))
        layout <-
            if(prod(pmf <- par("mfrow")) >= tot.lags) pmf
            else grDevices::n2mfrow(tot.lags)

    ## Plotting
    ## avoid resetting mfrow and using outer margins for just one plot
    mlayout <- any(layout > 1)
    if(mlayout) {
        dots <- list(...)
        cex.main <- dots$cex.main
        if(is.null(cex.main)) cex.main <- par("cex.main")
        if(is.null(oma)) {
            oma <- rep(2, 4)
            if (!is.null(main)) oma[3L] <- oma[3L] + 3*cex.main
        }
        opar <- par(mfrow = layout,
                    mar = c(1.1, 1.1, 0.5, 0.5) + is.mat*c(0, 0.5, 0, 0.5),
                    oma = oma, ask = ask)
        on.exit(par(opar))
    }
    nR <- layout[1L]
    nC <- layout[2L]

    ii <- jj <- 0 ## current row and column in the layout
    for(i in 1L:nser) {
        X <-  x[,i]
        xl <- range(X)
        nam <- if(is.mat) dimnames(x)[[2L]][i] else xnam
        newX <- is.mat

        for (ll in set.lags) {
            jj <- 1 + jj %% nC
            if(jj == 1) #  new row
                ii <- 1 + ii %% nR
            ##  plot.ts(x,y) *does* a lag plot -> text, ...
            if(mlayout) {
                plot(lag(X, ll), X, xlim = xl, ylim = xl, asp = asp,
                     xlab = paste("lag", ll), ylab = nam,
                     mgp = if(mlayout) c(0,0,0),
                     axes = FALSE, type = type,
                     xy.lines = do.lines, xy.labels = labels,
                     col.lab = if(newX) "red",
                     font.lab = if(newX) 2,
                     ...)
                box(...) # pass bty along
                if (jj ==  1 && ii %% 2 == 1 && !newX)
                    lAxis(2, ...)
                if (ii ==  1 && jj %% 2 == 1)
                    lAxis(3, ...)

                do.4 <- (ii %% 2 == 0 && (jj == nC ||
                               ## very last one:
                               (i == nser && ll == set.lags[lags])))
                if (do.4) lAxis(4, ...)
                if (jj %% 2 == 0 && ii == nR) lAxis(1, ...)

                if(newX) {
                    newX <- FALSE
                    if(!do.4) lAxis(4, Mgp = c(0,.6,0), ...)
                }
            } else  {
                plot(lag(X, ll), X, xlim = xl, ylim = xl, asp = asp,
                     xlab = paste("lag", ll), ylab = nam,
                     type = type,
                     xy.lines = do.lines, xy.labels = labels,
                     main = main, ...)
            }
            if(diag) abline(c(0,1), lty = 2, col = diag.col)

            if (mlayout && !is.null(main)) {
                font.main <- dots$font.main
                if(is.null(font.main)) font.main <- par("font.main")
                if ((jj == nC && ii == nR)  || ll == set.lags[lags])
                    mtext(main, 3, 3, outer = TRUE, at = 0.5,
                          cex = cex.main, font = font.main)
            }
        }
    }
    invisible(NULL)
}
#  File src/library/stats/R/lm.R
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


lm <- function (formula, data, subset, weights, na.action,
		method = "qr", model = TRUE, x = FALSE, y = FALSE,
		qr = TRUE, singular.ok = TRUE, contrasts = NULL,
		offset, ...)
{
    ret.x <- x
    ret.y <- y
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "weights", "na.action", "offset"),
	       names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
    if (method == "model.frame")
	return(mf)
    else if (method != "qr")
	warning(gettextf("method = '%s' is not supported. Using 'qr'", method),
                domain = NA)
    mt <- attr(mf, "terms") # allow model.frame to update it
    y <- model.response(mf, "numeric")
    ## avoid any problems with 1D or nx1 arrays by as.vector.
    w <- as.vector(model.weights(mf))
    if(!is.null(w) && !is.numeric(w))
        stop("'weights' must be a numeric vector")
    offset <- as.vector(model.offset(mf))
    if(!is.null(offset)) {
        if(length(offset) != NROW(y))
            stop(gettextf("number of offsets is %d, should equal %d (number of observations)",
                          length(offset), NROW(y)), domain = NA)
    }

    if (is.empty.model(mt)) {
	x <- NULL
	z <- list(coefficients = if (is.matrix(y))
                    matrix(,0,3) else numeric(0L), residuals = y,
		  fitted.values = 0 * y, weights = w, rank = 0L,
		  df.residual = if (is.matrix(y)) nrow(y) else length(y))
        if(!is.null(offset)) {
            z$fitted.values <- offset
            z$residuals <- y - offset
        }
    }
    else {
	x <- model.matrix(mt, mf, contrasts)
	z <- if(is.null(w)) lm.fit(x, y, offset = offset,
                                   singular.ok=singular.ok, ...)
	else lm.wfit(x, y, w, offset = offset, singular.ok=singular.ok, ...)
    }
    class(z) <- c(if(is.matrix(y)) "mlm", "lm")
    z$na.action <- attr(mf, "na.action")
    z$offset <- offset
    z$contrasts <- attr(x, "contrasts")
    z$xlevels <- .getXlevels(mt, mf)
    z$call <- cl
    z$terms <- mt
    if (model)
	z$model <- mf
    if (ret.x)
	z$x <- x
    if (ret.y)
	z$y <- y
    if (!qr) z$qr <- NULL
    z
}

## lm.fit() and lm.wfit() have *MUCH* in common  [say ``code re-use !'']
lm.fit <- function (x, y, offset = NULL, method = "qr", tol = 1e-07,
                    singular.ok = TRUE, ...)
{
    if (is.null(n <- nrow(x))) stop("'x' must be a matrix")
    if(n == 0L) stop("0 (non-NA) cases")
    p <- ncol(x)
    if (p == 0L) {
        ## oops, null model
        return(list(coefficients = numeric(0L), residuals = y,
                    fitted.values = 0 * y, rank = 0,
                    df.residual = length(y)))
    }
    ny <- NCOL(y)
    ## treat one-col matrix as vector
    if(is.matrix(y) && ny == 1)
        y <- drop(y)
    if(!is.null(offset))
        y <- y - offset
    if (NROW(y) != n)
	stop("incompatible dimensions")
    if(method != "qr")
	warning(gettextf("method = '%s' is not supported. Using 'qr'", method),
                domain = NA)
    if(length(list(...)))
	warning("extra arguments ", paste(names(list(...)), sep=", "),
                " are just disregarded.")
    storage.mode(x) <- "double"
    storage.mode(y) <- "double"
    z <- .Fortran("dqrls",
		  qr = x, n = n, p = p,
		  y = y, ny = ny,
		  tol = as.double(tol),
		  coefficients = mat.or.vec(p, ny),
		  residuals = y, effects = y, rank = integer(1L),
		  pivot = 1L:p, qraux = double(p), work = double(2*p),
                  PACKAGE="base")
    if(!singular.ok && z$rank < p) stop("singular fit encountered")
    coef <- z$coefficients
    pivot <- z$pivot
    ## careful here: the rank might be 0
    r1 <- seq_len(z$rank)
    dn <- colnames(x); if(is.null(dn)) dn <- paste("x", 1L:p, sep="")
    nmeffects <- c(dn[pivot[r1]], rep.int("", n - z$rank))
    r2 <- if(z$rank < p) (z$rank+1L):p else integer(0L)
    if (is.matrix(y)) {
	coef[r2, ] <- NA
	coef[pivot, ] <- coef
	dimnames(coef) <- list(dn, colnames(y))
	dimnames(z$effects) <- list(nmeffects, colnames(y))
    } else {
	coef[r2] <- NA
	coef[pivot] <- coef
	names(coef) <- dn
	names(z$effects) <- nmeffects
    }
    z$coefficients <- coef
    r1 <- y - z$residuals ; if(!is.null(offset)) r1 <- r1 + offset
    qr <- z[c("qr", "qraux", "pivot", "tol", "rank")]
    colnames(qr$qr) <- colnames(x)[qr$pivot]
    c(z[c("coefficients", "residuals", "effects", "rank")],
      list(fitted.values = r1, assign = attr(x, "assign"),
	   qr = structure(qr, class="qr"),
	   df.residual = n - z$rank))
}

lm.wfit <- function (x, y, w, offset = NULL, method = "qr", tol = 1e-7,
                     singular.ok = TRUE, ...)
{
    if(is.null(n <- nrow(x))) stop("'x' must be a matrix")
    if(n == 0) stop("0 (non-NA) cases")
    ny <- NCOL(y)
    ## treat one-col matrix as vector
    if(is.matrix(y) && ny == 1L)
        y <- drop(y)
    if(!is.null(offset))
        y <- y - offset
    if (NROW(y) != n | length(w) != n)
	stop("incompatible dimensions")
    if (any(w < 0 | is.na(w)))
	stop("missing or negative weights not allowed")
    if(method != "qr")
	warning(gettextf("method = '%s' is not supported. Using 'qr'", method),
                domain = NA)
    if(length(list(...)))
	warning("extra arguments ", paste(names(list(...)), sep=", "),
                " are just disregarded.")
    x.asgn <- attr(x, "assign")# save
    zero.weights <- any(w == 0)
    if (zero.weights) {
	save.r <- y
	save.f <- y
	save.w <- w
	ok <- w != 0
	nok <- !ok
	w <- w[ok]
	x0 <- x[!ok, , drop = FALSE]
	x <- x[ok,  , drop = FALSE]
	n <- nrow(x)
	y0 <- if (ny > 1L) y[!ok, , drop = FALSE] else y[!ok]
	y  <- if (ny > 1L) y[ ok, , drop = FALSE] else y[ok]
    }
    p <- ncol(x)
    if (p == 0) {
        ## oops, null model
        return(list(coefficients = numeric(0L), residuals = y,
                    fitted.values = 0 * y, weights = w, rank = 0L,
                    df.residual = length(y)))
    }
    storage.mode(y) <- "double"
    wts <- sqrt(w)
    z <- .Fortran("dqrls",
		  qr = x * wts, n = n, p = p,
		  y  = y * wts, ny = ny,
		  tol = as.double(tol),
		  coefficients = mat.or.vec(p, ny), residuals = y,
		  effects = mat.or.vec(n, ny),
		  rank = integer(1L), pivot = 1L:p, qraux = double(p),
		  work = double(2 * p),
                  PACKAGE="base")
    if(!singular.ok && z$rank < p) stop("singular fit encountered")
    coef <- z$coefficients
    pivot <- z$pivot
    r1 <- seq_len(z$rank)
    dn <- colnames(x); if(is.null(dn)) dn <- paste("x", 1L:p, sep="")
    nmeffects <- c(dn[pivot[r1]], rep.int("", n - z$rank))
    r2 <- if(z$rank < p) (z$rank+1L):p else integer(0L)
    if (is.matrix(y)) {
	coef[r2, ] <- NA
	coef[pivot, ] <- coef
	dimnames(coef) <- list(dn, colnames(y))
	dimnames(z$effects) <- list(nmeffects,colnames(y))
    } else {
	coef[r2] <- NA
	coef[pivot] <- coef
	names(coef) <- dn
	names(z$effects) <- nmeffects
    }
    z$coefficients <- coef
    z$residuals <- z$residuals/wts
    z$fitted.values <- y - z$residuals
    z$weights <- w
    if (zero.weights) {
	coef[is.na(coef)] <- 0
	f0 <- x0 %*% coef
	if (ny > 1) {
	    save.r[ok, ] <- z$residuals
	    save.r[nok, ] <- y0 - f0
	    save.f[ok, ] <- z$fitted.values
	    save.f[nok, ] <- f0
	}
	else {
	    save.r[ok] <- z$residuals
	    save.r[nok] <- y0 - f0
	    save.f[ok] <- z$fitted.values
	    save.f[nok] <- f0
	}
	z$residuals <- save.r
	z$fitted.values <- save.f
	z$weights <- save.w
    }
    if(!is.null(offset))
        z$fitted.values <- z$fitted.values + offset
    qr <- z[c("qr", "qraux", "pivot", "tol", "rank")]
    colnames(qr$qr) <- colnames(x)[qr$pivot]
    c(z[c("coefficients", "residuals", "fitted.values", "effects",
	  "weights", "rank")],
      list(assign = x.asgn,
	   qr = structure(qr, class="qr"),
	   df.residual = n - z$rank))
}

print.lm <- function(x, digits = max(3, getOption("digits") - 3), ...)
{
    cat("\nCall:\n",deparse(x$call),"\n\n",sep="")
    if(length(coef(x))) {
        cat("Coefficients:\n")
        print.default(format(coef(x), digits=digits),
                      print.gap = 2, quote = FALSE)
    } else cat("No coefficients\n")
    cat("\n")
    invisible(x)
}

summary.lm <- function (object, correlation = FALSE, symbolic.cor = FALSE, ...)
{
    z <- object
    p <- z$rank
    if (p == 0) {
        r <- z$residuals
        n <- length(r)
        w <- z$weights
        if (is.null(w)) {
            rss <- sum(r^2)
        } else {
            rss <- sum(w * r^2)
            r <- sqrt(w) * r
        }
        resvar <- rss/(n - p)
        ans <- z[c("call", "terms")]
        class(ans) <- "summary.lm"
        ans$aliased <- is.na(coef(object))  # used in print method
        ans$residuals <- r
        ans$df <- c(0L, n, length(ans$aliased))
        ans$coefficients <- matrix(NA, 0L, 4L)
        dimnames(ans$coefficients)<-
            list(NULL, c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
        ans$sigma <- sqrt(resvar)
        ans$r.squared <- ans$adj.r.squared <- 0
        return(ans)
    }
    Qr <- object$qr
    if (is.null(z$terms) || is.null(Qr))
	stop("invalid \'lm\' object:  no 'terms' nor 'qr' component")
    n <- NROW(Qr$qr)
    rdf <- n - p
    if(is.na(z$df.residual) || rdf != z$df.residual)
        warning("residual degrees of freedom in object suggest this is not an \"lm\" fit")
    p1 <- 1L:p
    ## do not want missing values substituted here
    r <- z$residuals
    f <- z$fitted.values
    w <- z$weights
    if (is.null(w)) {
        mss <- if (attr(z$terms, "intercept"))
            sum((f - mean(f))^2) else sum(f^2)
        rss <- sum(r^2)
    } else {
        mss <- if (attr(z$terms, "intercept")) {
            m <- sum(w * f /sum(w))
            sum(w * (f - m)^2)
        } else sum(w * f^2)
        rss <- sum(w * r^2)
        r <- sqrt(w) * r
    }
    resvar <- rss/rdf
    R <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
    se <- sqrt(diag(R) * resvar)
    est <- z$coefficients[Qr$pivot[p1]]
    tval <- est/se
    ans <- z[c("call", "terms")]
    ans$residuals <- r
    ans$coefficients <-
	cbind(est, se, tval, 2*pt(abs(tval), rdf, lower.tail = FALSE))
    dimnames(ans$coefficients)<-
	list(names(z$coefficients)[Qr$pivot[p1]],
	     c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
    ans$aliased <- is.na(coef(object))  # used in print method
    ans$sigma <- sqrt(resvar)
    ans$df <- c(p, rdf, NCOL(Qr$qr))
    if (p != attr(z$terms, "intercept")) {
	df.int <- if (attr(z$terms, "intercept")) 1L else 0L
	ans$r.squared <- mss/(mss + rss)
	ans$adj.r.squared <- 1 - (1 - ans$r.squared) * ((n - df.int)/rdf)
	ans$fstatistic <- c(value = (mss/(p - df.int))/resvar,
			    numdf = p - df.int, dendf = rdf)
    } else ans$r.squared <- ans$adj.r.squared <- 0
    ans$cov.unscaled <- R
    dimnames(ans$cov.unscaled) <- dimnames(ans$coefficients)[c(1,1)]
    if (correlation) {
	ans$correlation <- (R * resvar)/outer(se, se)
	dimnames(ans$correlation) <- dimnames(ans$cov.unscaled)
        ans$symbolic.cor <- symbolic.cor
    }
    if(!is.null(z$na.action)) ans$na.action <- z$na.action
    class(ans) <- "summary.lm"
    ans
}

print.summary.lm <-
    function (x, digits = max(3, getOption("digits") - 3),
              symbolic.cor = x$symbolic.cor,
	      signif.stars= getOption("show.signif.stars"),	...)
{
    cat("\nCall:\n")#S: ' ' instead of '\n'
    cat(paste(deparse(x$call), sep="\n", collapse = "\n"), "\n\n", sep="")
    resid <- x$residuals
    df <- x$df
    rdf <- df[2L]
    cat(if(!is.null(x$w) && diff(range(x$w))) "Weighted ",
        "Residuals:\n", sep="")
    if (rdf > 5L) {
	nam <- c("Min", "1Q", "Median", "3Q", "Max")
	rq <- if (length(dim(resid)) == 2)
	    structure(apply(t(resid), 1L, quantile),
		      dimnames = list(nam, dimnames(resid)[[2L]]))
	else  structure(quantile(resid), names = nam)
	print(rq, digits = digits, ...)
    }
    else if (rdf > 0L) {
	print(resid, digits = digits, ...)
    } else { # rdf == 0 : perfect fit!
	cat("ALL", df[1L], "residuals are 0: no residual degrees of freedom!\n")
    }
    if (length(x$aliased) == 0L) {
        cat("\nNo Coefficients\n")
    } else {
        if (nsingular <- df[3L] - df[1L])
            cat("\nCoefficients: (", nsingular,
                " not defined because of singularities)\n", sep = "")
        else cat("\nCoefficients:\n")
        coefs <- x$coefficients
        if(!is.null(aliased <- x$aliased) && any(aliased)) {
            cn <- names(aliased)
            coefs <- matrix(NA, length(aliased), 4, dimnames=list(cn, colnames(coefs)))
            coefs[!aliased, ] <- x$coefficients
        }

        printCoefmat(coefs, digits=digits, signif.stars=signif.stars, na.print="NA", ...)
    }
    ##
    cat("\nResidual standard error:",
	format(signif(x$sigma, digits)), "on", rdf, "degrees of freedom\n")
    if(nzchar(mess <- naprint(x$na.action))) cat("  (",mess, ")\n", sep="")
    if (!is.null(x$fstatistic)) {
	cat("Multiple R-squared:", formatC(x$r.squared, digits=digits))
	cat(",\tAdjusted R-squared:",formatC(x$adj.r.squared,digits=digits),
	    "\nF-statistic:", formatC(x$fstatistic[1L], digits=digits),
	    "on", x$fstatistic[2L], "and",
	    x$fstatistic[3L], "DF,  p-value:",
	    format.pval(pf(x$fstatistic[1L], x$fstatistic[2L],
                           x$fstatistic[3L], lower.tail = FALSE), digits=digits),
	    "\n")
    }
    correl <- x$correlation
    if (!is.null(correl)) {
	p <- NCOL(correl)
	if (p > 1L) {
	    cat("\nCorrelation of Coefficients:\n")
	    if(is.logical(symbolic.cor) && symbolic.cor) {# NULL < 1.7.0 objects
		print(symnum(correl, abbr.colnames = NULL))
	    } else {
                correl <- format(round(correl, 2), nsmall = 2, digits = digits)
                correl[!lower.tri(correl)] <- ""
                print(correl[-1, -p, drop=FALSE], quote = FALSE)
            }
	}
    }
    cat("\n")#- not in S
    invisible(x)
}

residuals.lm <-
    function(object,
             type = c("working","response", "deviance","pearson", "partial"),
             ...)
{
    type <- match.arg(type)
    r <- object$residuals
    res <- switch(type,
                  working =, response = r,
                  deviance=, pearson =
                  if(is.null(object$weights)) r else r * sqrt(object$weights),
                  partial = r
           )
    res <- naresid(object$na.action, res)
    if (type=="partial") ## predict already does naresid
      res <- res + predict(object,type="terms")
    res
}

## The lm method includes objects of class "glm"
simulate.lm <- function(object, nsim = 1, seed = NULL, ...)
{
    if(!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
        runif(1)                     # initialize the RNG if necessary
    if(is.null(seed))
        RNGstate <- get(".Random.seed", envir = .GlobalEnv)
    else {
        R.seed <- get(".Random.seed", envir = .GlobalEnv)
	set.seed(seed)
        RNGstate <- structure(seed, kind = as.list(RNGkind()))
        on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
    }
    ftd <- fitted(object)             # == napredict(*, object$fitted)
    nm <- names(ftd)
    n <- length(ftd)
    ntot <- n * nsim
    fam <- if(inherits(object, "glm")) object$family$family else "gaussian"
    val <- switch(fam,
                  "gaussian" = {
                      vars <- deviance(object)/ df.residual(object)
                      if (!is.null(object$weights)) vars <- vars/object$weights
                      ftd + rnorm(ntot, sd = sqrt(vars))
                  },
                  if(!is.null(object$family$simulate))
                      object$family$simulate(object, nsim)
                  else stop("family '", fam, "' not implemented"))

    if(!is.list(val)) {
        dim(val) <- c(n, nsim)
        val <- as.data.frame(val)
    } else
        class(val) <- "data.frame"
    names(val) <- paste("sim", seq_len(nsim), sep="_")
    if (!is.null(nm)) row.names(val) <- nm
    attr(val, "seed") <- RNGstate
    val
}

#fitted.lm <- function(object, ...)
#    napredict(object$na.action, object$fitted.values)

# coef.lm <- function(object, ...) object$coefficients

## need this for results of lm.fit() in drop1():
weights.default <- function(object, ...)
    naresid(object$na.action, object$weights)


deviance.lm <- function(object, ...)
    sum(weighted.residuals(object)^2, na.rm=TRUE)

formula.lm <- function(x, ...)
{
    form <- x$formula
    if( !is.null(form) ) {
        form <- formula(x$terms) # has . expanded
        environment(form) <- environment(x$formula)
        form
    } else formula(x$terms)
}

family.lm <- function(object, ...) { gaussian() }

model.frame.lm <- function(formula, ...)
{
    dots <- list(...)
    nargs <- dots[match(c("data", "na.action", "subset"), names(dots), 0)]
    if (length(nargs) || is.null(formula$model)) {
        fcall <- formula$call
        fcall$method <- "model.frame"
        fcall[[1L]] <- as.name("lm")
        fcall[names(nargs)] <- nargs
#	env <- environment(fcall$formula)  # always NULL
        env <- environment(formula$terms)
	if (is.null(env)) env <- parent.frame()
        eval(fcall, env, parent.frame())
    }
    else formula$model
}

variable.names.lm <- function(object, full = FALSE, ...)
{
    if(full)	dimnames(object$qr$qr)[[2L]]
    else if(object$rank) dimnames(object$qr$qr)[[2L]][seq_len(object$rank)]
    else character(0L)
}

case.names.lm <- function(object, full = FALSE, ...)
{
    w <- weights(object)
    dn <- names(residuals(object))
    if(full || is.null(w)) dn else dn[w!=0]
}

anova.lm <- function(object, ...)
{
    if(length(list(object, ...)) > 1L)
	return(anova.lmlist(object, ...))
    w <- object$weights
    ssr <- sum(if(is.null(w)) object$residuals^2 else w*object$residuals^2)
    mss <- sum(if(is.null(w)) object$fitted.values^2 else w*object$fitted.values^2)
    if(ssr < 1e-10*mss)
        warning("ANOVA F-tests on an essentially perfect fit are unreliable")
    dfr <- df.residual(object)
    p <- object$rank
    if(p > 0L) {
        p1 <- 1L:p
        comp <- object$effects[p1]
        asgn <- object$assign[object$qr$pivot][p1]
        nmeffects <- c("(Intercept)", attr(object$terms, "term.labels"))
        tlabels <- nmeffects[1 + unique(asgn)]
        ss <- c(unlist(lapply(split(comp^2,asgn), sum)), ssr)
        df <- c(unlist(lapply(split(asgn,  asgn), length)), dfr)
    } else {
        ss <- ssr
        df <- dfr
        tlabels <- character(0L)
    }
    ms <- ss/df
    f <- ms/(ssr/dfr)
    P <- pf(f, df, dfr, lower.tail = FALSE)
    table <- data.frame(df, ss, ms, f, P)
    table[length(P), 4:5] <- NA
    dimnames(table) <- list(c(tlabels, "Residuals"),
                            c("Df","Sum Sq", "Mean Sq", "F value", "Pr(>F)"))
    if(attr(object$terms,"intercept")) table <- table[-1, ]
    structure(table, heading = c("Analysis of Variance Table\n",
		     paste("Response:", deparse(formula(object)[[2L]]))),
	      class= c("anova", "data.frame"))# was "tabular"
}

anova.lmlist <- function (object, ..., scale = 0, test = "F")
{
    objects <- list(object, ...)
    responses <- as.character(lapply(objects,
				     function(x) deparse(x$terms[[2L]])))
    sameresp <- responses == responses[1L]
    if (!all(sameresp)) {
	objects <- objects[sameresp]
	warning("models with response ",
                deparse(responses[!sameresp]),
                " removed because response differs from ", "model 1")
    }

    ns <- sapply(objects, function(x) length(x$residuals))
    if(any(ns != ns[1L]))
        stop("models were not all fitted to the same size of dataset")

    ## calculate the number of models
    nmodels <- length(objects)
    if (nmodels == 1)
	return(anova.lm(object))

    ## extract statistics

    resdf  <- as.numeric(lapply(objects, df.residual))
    resdev <- as.numeric(lapply(objects, deviance))

    ## construct table and title

    table <- data.frame(resdf, resdev, c(NA, -diff(resdf)),
                        c(NA, -diff(resdev)) )
    variables <- lapply(objects, function(x)
                        paste(deparse(formula(x)), collapse="\n") )
    dimnames(table) <- list(1L:nmodels,
                            c("Res.Df", "RSS", "Df", "Sum of Sq"))

    title <- "Analysis of Variance Table\n"
    topnote <- paste("Model ", format(1L:nmodels),": ",
		     variables, sep="", collapse="\n")

    ## calculate test statistic if needed

    if(!is.null(test)) {
	bigmodel <- order(resdf)[1L]
        scale <- if(scale > 0) scale else resdev[bigmodel]/resdf[bigmodel]
	table <- stat.anova(table = table, test = test,
			    scale = scale,
                            df.scale = resdf[bigmodel],
			    n = length(objects[bigmodel$residuals]))
    }
    structure(table, heading = c(title, topnote),
              class = c("anova", "data.frame"))
}

## code originally from John Maindonald 26Jul2000
predict.lm <-
    function(object, newdata, se.fit = FALSE, scale = NULL, df = Inf,
	     interval = c("none", "confidence", "prediction"),
	     level = .95,  type = c("response", "terms"),
	     terms = NULL, na.action = na.pass, pred.var = res.var/weights,
             weights = 1, ...)
{
    tt <- terms(object)
    if(missing(newdata) || is.null(newdata)) {
	mm <- X <- model.matrix(object)
	mmDone <- TRUE
	offset <- object$offset
    }
    else {
        Terms <- delete.response(tt)
        m <- model.frame(Terms, newdata, na.action = na.action,
                         xlev = object$xlevels)
        if(!is.null(cl <- attr(Terms, "dataClasses"))) .checkMFClasses(cl, m)
        X <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
        offset <- rep(0, nrow(X))
        if (!is.null(off.num <- attr(tt, "offset")))
            for(i in off.num)
                offset <- offset + eval(attr(tt, "variables")[[i+1]], newdata)
	if (!is.null(object$call$offset))
	    offset <- offset + eval(object$call$offset, newdata)
	mmDone <- FALSE
    }
    n <- length(object$residuals) # NROW(object$qr$qr)
    p <- object$rank
    p1 <- seq_len(p)
    piv <- object$qr$pivot[p1]
    if(p < ncol(X) && !(missing(newdata) || is.null(newdata)))
	warning("prediction from a rank-deficient fit may be misleading")
### NB: Q[p1,] %*% X[,piv] = R[p1,p1]
    beta <- object$coefficients
    predictor <- drop(X[, piv, drop = FALSE] %*% beta[piv])
    if (!is.null(offset))
	predictor <- predictor + offset

    interval <- match.arg(interval)
    if (interval == "prediction") {
        if (missing(newdata))
            warning("Predictions on current data refer to _future_ responses\n")
        if (missing(newdata) && missing(weights))
        {
            w <-  weights.default(object)
            if (!is.null(w)) {
                weights <- w
                warning("Assuming prediction variance inversely proportional to weights used for fitting\n")
            }
        }
        if (!missing(newdata) && missing(weights) && !is.null(object$weights) && missing(pred.var))
            warning("Assuming constant prediction variance even though model fit is weighted\n")
        if (inherits(weights, "formula")){
            if (length(weights) != 2L)
                stop("'weights' as formula should be one-sided")
            d <- if(missing(newdata) || is.null(newdata))
                model.frame(object)
            else
                newdata
            weights <- eval(weights[[2L]], d, environment(weights))
        }
    }

    type <- match.arg(type)
    if(se.fit || interval != "none") {
	res.var <-
	    if (is.null(scale)) {
		r <- object$residuals
		w <- object$weights
		rss <- sum(if(is.null(w)) r^2 else r^2 * w)
		df <- n - p
		rss/df
	    } else scale^2
	if(type != "terms") {
            if(p > 0) {
                XRinv <-
                    if(missing(newdata) && is.null(w))
                        qr.Q(object$qr)[, p1, drop = FALSE]
                    else
                        X[, piv] %*% qr.solve(qr.R(object$qr)[p1, p1])
#	NB:
#	 qr.Q(object$qr)[, p1, drop = FALSE] / sqrt(w)
#	looks faster than the above, but it's slower, and doesn't handle zero
#	weights properly
#
                ip <- drop(XRinv^2 %*% rep(res.var, p))
            } else ip <- rep(0, n)
	}
    }

    if (type == "terms") { ## type == "terms" ------------

	if(!mmDone) { mm <- model.matrix(object); mmDone <- TRUE }
	## asgn <- attrassign(mm, tt) :
	aa <- attr(mm, "assign")
	ll <- attr(tt, "term.labels")
	hasintercept <- attr(tt, "intercept") > 0L
	if (hasintercept)
	    ll <- c("(Intercept)", ll)
	aaa <- factor(aa, labels = ll)
	asgn <- split(order(aa), aaa)
	if (hasintercept) {
	    asgn$"(Intercept)" <- NULL
	    if(!mmDone) { mm <- model.matrix(object); mmDone <- TRUE }
	    avx <- colMeans(mm)
	    termsconst <- sum(avx[piv] * beta[piv])
	}
	nterms <- length(asgn)
        if(nterms > 0) {
            predictor <- matrix(ncol = nterms, nrow = NROW(X))
            dimnames(predictor) <- list(rownames(X), names(asgn))

            if (se.fit || interval != "none") {
                ip <- matrix(ncol = nterms, nrow = NROW(X))
                dimnames(ip) <- list(rownames(X), names(asgn))
                Rinv <- qr.solve(qr.R(object$qr)[p1, p1])
            }
            if(hasintercept)
                X <- sweep(X, 2L, avx, check.margin=FALSE)
            unpiv <- rep.int(0L, NCOL(X))
            unpiv[piv] <- p1
            ## Predicted values will be set to 0 for any term that
            ## corresponds to columns of the X-matrix that are
            ## completely aliased with earlier columns.
            for (i in seq.int(1L, nterms, length.out = nterms)) {
                iipiv <- asgn[[i]]      # Columns of X, ith term
                ii <- unpiv[iipiv]      # Corresponding rows of Rinv
                iipiv[ii == 0L] <- 0L
                predictor[, i] <-
                    if(any(iipiv > 0L)) X[, iipiv, drop = FALSE] %*% beta[iipiv]
                    else 0
                if (se.fit || interval != "none")
                    ip[, i] <-
                        if(any(iipiv > 0L))
                            as.matrix(X[, iipiv, drop = FALSE] %*%
                                      Rinv[ii, , drop = FALSE])^2 %*% rep.int(res.var, p)
                        else 0
            }
            if (!is.null(terms)) {
                predictor <- predictor[, terms, drop = FALSE]
                if (se.fit)
                    ip <- ip[, terms, drop = FALSE]
            }
        } else { # no terms
            predictor <- ip <- matrix(0, n, 0L)
        }
	attr(predictor, 'constant') <- if (hasintercept) termsconst else 0
    }

### Now construct elements of the list that will be returned

    if(interval != "none") {
	tfrac <- qt((1 - level)/2, df)
	hwid <- tfrac * switch(interval,
			       confidence = sqrt(ip),
			       prediction = sqrt(ip+pred.var)
			       )
	if(type != "terms") {
	    predictor <- cbind(predictor, predictor + hwid %o% c(1, -1))
	    colnames(predictor) <- c("fit", "lwr", "upr")
	}
	else {
	    lwr <- predictor + hwid
	    upr <- predictor - hwid
	}
    }
    if(se.fit || interval != "none") se <- sqrt(ip)
    if(missing(newdata) && !is.null(na.act <- object$na.action)) {
	predictor <- napredict(na.act, predictor)
	if(se.fit) se <- napredict(na.act, se)
    }
    if(type == "terms" && interval != "none") {
	if(missing(newdata) && !is.null(na.act)) {
	    lwr <- napredict(na.act, lwr)
	    upr <- napredict(na.act, upr)
	}
	list(fit = predictor, se.fit = se, lwr = lwr, upr = upr,
	     df = df, residual.scale = sqrt(res.var))
    } else if (se.fit)
	list(fit = predictor, se.fit = se,
	     df = df, residual.scale = sqrt(res.var))
    else predictor
}

effects.lm <- function(object, set.sign = FALSE, ...)
{
    eff <- object$effects
    if(is.null(eff)) stop("'object' has no 'effects' component")
    if(set.sign) {
	dd <- coef(object)
	if(is.matrix(eff)) {
	    r <- 1L:dim(dd)[1L]
	    eff[r,  ] <- sign(dd) * abs(eff[r,	])
	} else {
	    r <- seq_along(dd)
	    eff[r] <- sign(dd) * abs(eff[r])
	}
    }
    structure(eff, assign = object$assign, class = "coef")
}

## plot.lm --> now in ./plot.lm.R

model.matrix.lm <- function(object, ...)
{
    if(n_match <- match("x", names(object), 0L)) object[[n_match]]
    else {
        data <- model.frame(object, xlev = object$xlevels, ...)
        NextMethod("model.matrix", data = data,
                   contrasts.arg = object$contrasts)
    }
}

##---> SEE ./mlm.R  for more methods, etc. !!
predict.mlm <-
    function(object, newdata, se.fit = FALSE, na.action = na.pass, ...)
{
    if(missing(newdata)) return(object$fitted.values)
    if(se.fit)
	stop("the 'se.fit' argument is not yet implemented for \"mlm\" objects")
    if(missing(newdata)) {
        X <- model.matrix(object)
        offset <- object$offset
    }
    else {
        tt <- terms(object)
        Terms <- delete.response(tt)
        m <- model.frame(Terms, newdata, na.action = na.action,
                         xlev = object$xlevels)
        if(!is.null(cl <- attr(Terms, "dataClasses"))) .checkMFClasses(cl, m)
        X <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
	offset <- if (!is.null(off.num <- attr(tt, "offset")))
	    eval(attr(tt, "variables")[[off.num+1]], newdata)
	else if (!is.null(object$offset))
	    eval(object$call$offset, newdata)
    }
    piv <- object$qr$pivot[seq(object$rank)]
    pred <- X[, piv, drop = FALSE] %*% object$coefficients[piv,]
    if ( !is.null(offset) ) pred <- pred + offset
    if(inherits(object, "mlm")) pred else pred[, 1L]
}

## from base/R/labels.R
labels.lm <- function(object, ...)
{
    tl <- attr(object$terms, "term.labels")
    asgn <- object$assign[object$qr$pivot[1L:object$rank]]
    tl[unique(asgn)]
}
#  File src/library/stats/R/lm.influence.R
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

### "lm"  *and*	 "glm"	 leave-one-out influence measures

## this is mainly for back-compatibility (from "lsfit" time) -- use hatvalues()!
hat <- function(x, intercept = TRUE)
{
    if(is.qr(x)) n <- nrow(x$qr)
    else {
	if(intercept) x <- cbind(1, x)
	n <- nrow(x)
	x <- qr(x)
    }
    rowSums(qr.qy(x, diag(1, nrow = n, ncol = x$rank))^2)
}

## see PR#7961
weighted.residuals <- function(obj, drop0 = TRUE)
{
    w <- weights(obj)
    r <- residuals(obj, type="deviance")
    if(drop0 && !is.null(w)) r[w != 0] else r
}

lm.influence <- function (model, do.coef = TRUE)
{
    wt.res <- weighted.residuals(model)
    e <- na.omit(wt.res)

    if (model$rank == 0) {
        n <- length(wt.res) # drops 0 wt, may drop NAs
        sigma <- sqrt(deviance(model)/df.residual(model))
        res <- list(hat = rep(0, n), coefficients = matrix(0, n, 0),
                    sigma = rep(sigma, n), wt.res = e)
    } else {
        ## if we have a point with hat = 1, the corresponding e should be
        ## exactly zero.  Protect against returning Inf by forcing this
        e[abs(e) < 100 * .Machine$double.eps * median(abs(e))] <- 0
        n <- as.integer(nrow(model$qr$qr))
        k <- as.integer(model$qr$rank)
        ## in na.exclude case, omit NAs; also drop 0-weight cases
        if(NROW(e) != n)
            stop("non-NA residual length does not match cases used in fitting")
        do.coef <- as.logical(do.coef)
        res <- .Fortran("lminfl",
                        model$qr$qr,
                        n,
                        n,
                        k,
                        as.integer(do.coef),
                        model$qr$qraux,
                        wt.res = e,
                        hat = double(n),
                        coefficients= if(do.coef) matrix(0, n, k) else double(0L),
                        sigma = double(n),
                        tol = 10 * .Machine$double.eps,
                        DUP = FALSE, PACKAGE="stats"
                        )[c("hat", "coefficients", "sigma","wt.res")]
        if(!is.null(model$na.action)) {
            hat <- naresid(model$na.action, res$hat)
            hat[is.na(hat)] <- 0       # omitted cases have 0 leverage
            res$hat <- hat
            if(do.coef) {
                coefficients <- naresid(model$na.action, res$coefficients)
                coefficients[is.na(coefficients)] <- 0 # omitted cases have 0 change
                res$coefficients <- coefficients
            }
            sigma <- naresid(model$na.action, res$sigma)
            sigma[is.na(sigma)] <- sqrt(deviance(model)/df.residual(model))
            res$sigma <- sigma
        }
    }
    res$wt.res <- naresid(model$na.action, res$wt.res)
    res$hat[res$hat > 1 - 10*.Machine$double.eps] <- 1 # force 1
    names(res$hat) <- names(res$sigma) <- names(res$wt.res)
    if(!do.coef) ## drop it
	res$coefficients <- NULL
    else {
        rownames(res$coefficients) <- names(res$wt.res)
        colnames(res$coefficients) <- names(coef(model))[!is.na(coef(model))]
    }
    res
}

## The following is adapted from John Fox's  "car" :
influence <- function(model, ...) UseMethod("influence")
influence.lm  <- function(model, do.coef = TRUE, ...)
    lm.influence(model, do.coef = do.coef, ...)
influence.glm <- function(model, do.coef = TRUE, ...) {
    res <- lm.influence(model, do.coef = do.coef, ...)
    pRes <- na.omit(residuals(model, type = "pearson"))[model$prior.weights != 0]
    pRes <- naresid(model$na.action, pRes)
    names(res)[names(res) == "wt.res"] <- "dev.res"
    c(res, list(pear.res = pRes))
}

hatvalues <- function(model, ...) UseMethod("hatvalues")
hatvalues.lm <- function(model, infl = lm.influence(model, do.coef=FALSE), ...)
{
    hat <- infl$hat
    names(hat) <- names(infl$wt.res)
    hat
}

rstandard <- function(model, ...) UseMethod("rstandard")
rstandard.lm <- function(model, infl = lm.influence(model, do.coef=FALSE),
                         sd = sqrt(deviance(model)/df.residual(model)), ...)
{
    res <- infl$wt.res / (sd * sqrt(1 - infl$hat))
    res[is.infinite(res)] <- NaN
    res
}

## FIXME ! -- make sure we are following "the literature":
rstandard.glm <- function(model, infl = lm.influence(model, do.coef=FALSE), ...)
{
    res <- infl$wt.res # = "dev.res"  really
    res <- res / sqrt(summary(model)$dispersion * (1 - infl$hat))
    res[is.infinite(res)] <- NaN
    res
}

rstudent <- function(model, ...) UseMethod("rstudent")
rstudent.lm <- function(model, infl = lm.influence(model, do.coef=FALSE),
			res = infl$wt.res, ...)
{
    res <- res / (infl$sigma * sqrt(1 - infl$hat))
    res[is.infinite(res)] <- NaN
    res
}

rstudent.glm <- function(model, infl = influence(model, do.coef=FALSE), ...)
{
    r <- infl$dev.res
    r <- sign(r) * sqrt(r^2 + (infl$hat * infl$pear.res^2)/(1 - infl$hat))
    r[is.infinite(r)] <- NaN
    r
    if (any(family(model)$family == c("binomial", "poisson")))
	r else r/infl$sigma
}

### FIXME for glm (see above) ?!?
dffits <- function(model, infl = lm.influence(model, do.coef=FALSE),
		   res = weighted.residuals(model))
{
    res <- res * sqrt(infl$hat)/(infl$sigma*(1-infl$hat))
    res[is.infinite(res)] <- NaN
    res
}


dfbeta <- function(model, ...) UseMethod("dfbeta")

dfbeta.lm <- function(model, infl = lm.influence(model, do.coef=TRUE), ...)
{
    ## for lm & glm
    b <- infl$coefficients
    dimnames(b) <- list(names(infl$wt.res), variable.names(model))
    b
}

dfbetas <- function(model, ...) UseMethod("dfbetas")

dfbetas.lm <- function (model, infl = lm.influence(model, do.coef=TRUE), ...)
{
    ## for lm & glm
    xxi <- chol2inv(model$qr$qr, model$qr$rank)
    dfbeta(model, infl) / outer(infl$sigma, sqrt(diag(xxi)))
}

covratio <- function(model, infl = lm.influence(model, do.coef=FALSE),
		     res = weighted.residuals(model))
{
    n <- nrow(model$qr$qr)
    p <- model$rank
    omh <- 1-infl$hat
    e.star <- res/(infl$sigma*sqrt(omh))
    e.star[is.infinite(e.star)] <- NaN
    1/(omh*(((n - p - 1)+e.star^2)/(n - p))^p)
}

cooks.distance <- function(model, ...) UseMethod("cooks.distance")

## Used in plot.lm(); allow passing of known parts; `infl' used only via `hat'
cooks.distance.lm <-
function(model, infl = lm.influence(model, do.coef=FALSE),
	 res = weighted.residuals(model),
	 sd = sqrt(deviance(model)/df.residual(model)),
	 hat = infl$hat, ...)
{
    p <- model$rank
    res <- ((res/(sd * (1 - hat)))^2 * hat)/p
    res[is.infinite(res)] <- NaN
    res
}

cooks.distance.glm <-
function(model, infl = influence(model, do.coef=FALSE),
	 res = infl$pear.res,
	 dispersion = summary(model)$dispersion, hat = infl$hat, ...)
{
    p <- model$rank
    res <- (res/(1-hat))^2 * hat/(dispersion* p)
    res[is.infinite(res)] <- NaN
    res
}

influence.measures <- function(model)
{
    is.influential <- function(infmat, n)
    {
	## Argument is result of using influence.measures
	## Returns a matrix  of logicals structured like the argument
	k <- ncol(infmat) - 4
	if(n <= k)
	    stop("too few cases, n < k")
	absmat <- abs(infmat)
	result <- cbind(absmat[, 1L:k] > 1, # |dfbetas| > 1
			absmat[, k + 1] > 3 * sqrt(k/(n - k)), # |dffit| > ..
			abs(1 - infmat[, k + 2]) > (3*k)/(n - k),# |1-cov.r| >..
			pf(infmat[, k + 3], k, n - k) > 0.5,# "P[cook.d..]" > .5
			infmat[, k + 4] > (3 * k)/n) # hat > 3k/n
	dimnames(result) <- dimnames(infmat)
	result
    }
    infl <- influence(model) # generic -> lm, glm, [...] methods
    p <- model$rank
    e <- weighted.residuals(model)
    s <- sqrt(sum(e^2, na.rm=TRUE)/df.residual(model))
    xxi <- chol2inv(model$qr$qr, model$qr$rank)
    si <- infl$sigma
    h <- infl$hat
    dfbetas <- infl$coefficients / outer(infl$sigma, sqrt(diag(xxi)))
    vn <- variable.names(model); vn[vn == "(Intercept)"] <- "1_"
    colnames(dfbetas) <- paste("dfb",abbreviate(vn),sep=".")
    ## Compatible to dffits():
    dffits <- e*sqrt(h)/(si*(1-h))
    if(any(ii <- is.infinite(dffits))) dffits[ii] <- NaN
    cov.ratio <- (si/s)^(2 * p)/(1 - h)
    cooks.d <-
        if(inherits(model, "glm"))
            (infl$pear.res/(1-h))^2 * h/(summary(model)$dispersion * p)
        else # lm
            ((e/(s * (1 - h)))^2 * h)/p
#    dn <- dimnames(model$qr$qr)
    infmat <- cbind(dfbetas, dffit = dffits, cov.r = cov.ratio,
		    cook.d = cooks.d, hat=h)
    infmat[is.infinite(infmat)] <- NaN
    is.inf <- is.influential(infmat, sum(h > 0))
    ans <- list(infmat = infmat, is.inf = is.inf, call = model$call)
    class(ans) <- "infl"
    ans
}

print.infl <- function(x, digits = max(3, getOption("digits") - 4), ...)
{
    ## `x' : as the result of  influence.measures(.)
    cat("Influence measures of\n\t", deparse(x$call),":\n\n")
    is.star <- apply(x$is.inf, 1L, any, na.rm = TRUE)
    print(data.frame(x$infmat,
		     inf = ifelse(is.star, "*", " ")),
	  digits = digits, ...)
    invisible(x)
}

summary.infl <- function(object, digits = max(2, getOption("digits") - 5), ...)
{
    ## object must be as the result of	influence.measures(.)
    is.inf <- object$is.inf
    ## will have NaN values from any hat=1 rows.
    is.inf[is.na(is.inf)] <- FALSE
     is.star <- apply(is.inf, 1L, any)
    is.inf <- is.inf[is.star,]
    cat("Potentially influential observations of\n\t",
	deparse(object$call),":\n")
    if(any(is.star)) {
	imat <- object $ infmat[is.star,, drop = FALSE]
	if(is.null(rownam <- dimnames(object $ infmat)[[1L]]))
	    rownam <- format(seq(is.star))
	dimnames(imat)[[1L]] <- rownam[is.star]
	chmat <- format(round(imat, digits = digits))
	cat("\n")
	print(array(paste(chmat,c("","_*")[1+is.inf], sep=''),
		    dimnames = dimnames(imat), dim=dim(imat)),
	      quote = FALSE)
	invisible(imat)
    } else {
	cat("NONE\n")
	numeric(0L)
    }
}
#  File src/library/stats/R/loess.R
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

loess <-
function(formula, data, weights, subset, na.action, model = FALSE,
	 span = 0.75, enp.target, degree = 2, parametric = FALSE,
	 drop.square = FALSE, normalize = TRUE,
	 family = c("gaussian", "symmetric"),
	 method = c("loess", "model.frame"),
	 control = loess.control(...), ...)
{
    family <- match.arg(family)
    method <- match.arg(method)
    mf <- match.call(expand.dots=FALSE)
    mf$model <- mf$span <- mf$enp.target <- mf$degree <-
	mf$parametric <- mf$drop.square <- mf$normalize <- mf$family <-
	    mf$method <- mf$control <- mf$... <- NULL
    mf[[1L]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
    if (match.arg(method) == "model.frame") return(mf)
    mt <- attr(mf, "terms")
    y <- model.response(mf, "numeric")
    w <- model.weights(mf)
    if(is.null(w)) w <- rep(1, length(y))
    nmx <- as.character(attr(mt, "variables"))[-(1L:2)]
    x <- mf[, nmx, drop=FALSE]
    if(any(sapply(x, is.factor))) stop("predictors must all be numeric")
    x <- as.matrix(x)
    D <- ncol(x)
    nmx <- colnames(x)
    names(nmx) <- nmx
    drop.square <- match(nmx, nmx[drop.square], 0L) > 0L
    parametric <- match(nmx, nmx[parametric], 0L) > 0L
    if(!match(degree, 0L:2L, 0L)) stop("'degree' must be 0, 1 or 2")
    iterations <- if(family == "gaussian") 1 else control$iterations
    if(!missing(enp.target))
	if(!missing(span))
	    warning("both 'span' and 'enp.target' specified: 'span' will be used")
	else {				# White book p.321
	    tau <- switch(degree+1, 1, D+1, (D+1)*(D+2)/2) - sum(drop.square)
	    span <- 1.2 * tau/enp.target
	}
    ## Let's add sanity checks on control
    if(!is.list(control) || !is.character(control$surface) ||
       !is.character(control$statistics) || !is.character(control$trace.hat) ||
       !is.numeric(control$cell) || !is.numeric(iterations))
        stop("invalid 'control' argument")
    fit <- simpleLoess(y, x, w, span, degree, parametric, drop.square,
		       normalize, control$statistics, control$surface,
		       control$cell, iterations, control$trace.hat)
    fit$call <- match.call()
    fit$terms <- mt
    fit$xnames <- nmx
    fit$x <- x
    fit$y <- y
    fit$weights <- w
    if(model) fit$model <- mf
    fit$na.action <- attr(mf, "na.action")
    fit
}

loess.control <-
  function(surface = c("interpolate", "direct"),
	   statistics = c("approximate", "exact"),
	   trace.hat = c("exact", "approximate"),
	   cell = 0.2, iterations = 4, ...)
{
    list(surface=match.arg(surface),
	 statistics=match.arg(statistics),
	 trace.hat=match.arg(trace.hat),
	 cell=cell, iterations=iterations)
}


simpleLoess <-
  function(y, x, weights, span = 0.75, degree = 2, parametric = FALSE,
	   drop.square = FALSE, normalize = TRUE,
	   statistics = "approximate", surface = "interpolate",
	   cell = 0.2, iterations = 1, trace.hat = "exact")
{
    ## loess_ translated to R.

    D <- NCOL(x)
    if(D > 4) stop("only 1-4 predictors are allowed")
    N <- NROW(x)
    if(!N || !D)	stop("invalid 'x'")
    if(!length(y))	stop("invalid 'y'")
    x <- as.matrix(x)
    max.kd <-  max(N, 200)
    robust <- rep(1, N)
    divisor<- rep(1, D)
    if(normalize && D > 1L) {
	trim <- ceiling(0.1 * N)
	divisor <-
	    sqrt(apply(apply(x, 2L, sort)[seq(trim+1, N-trim), , drop = FALSE],
		       2L, var))
	x <- x/rep(divisor, rep(N, D))
    }
    sum.drop.sqr <- sum(drop.square)
    sum.parametric <- sum(parametric)
    nonparametric <- sum(!parametric)
    order.parametric <- order(parametric)
    x <- x[, order.parametric]
    order.drop.sqr <- (2 - drop.square)[order.parametric]
    if(degree==1 && sum.drop.sqr)
	stop("specified the square of a factor predictor to be dropped when degree = 1")
    if(D == 1 && sum.drop.sqr)
	stop("specified the square of a predictor to be dropped with only one numeric predictor")
    if(sum.parametric == D) stop("specified parametric for all predictors")

    if(iterations)
    for(j in 1L:iterations) {
	robust <- weights * robust
	if(j > 1) statistics <- "none"
	else if(surface == "interpolate" && statistics == "approximate")
	    statistics <- if(trace.hat == "exact") "1.approx"
            else "2.approx" # trace.hat == "approximate"
	surf.stat <- paste(surface, statistics, sep="/")
	z <- .C(R_loess_raw, # ../src/loessc.c
		as.double(y),
		as.double(x),
		as.double(weights),
		as.double(robust),
		as.integer(D),
		as.integer(N),
		as.double(span),
		as.integer(degree),
		as.integer(nonparametric),
		as.integer(order.drop.sqr),
		as.integer(sum.drop.sqr),
		as.double(span*cell),
		as.character(surf.stat),
		fitted.values = double(N),
		parameter = integer(7),
		a = integer(max.kd),
		xi = double(max.kd),
		vert = double(2*D),
		vval = double((D+1)*max.kd),
		diagonal = double(N),
		trL = double(1),
		delta1 = double(1),
		delta2 = double(1),
		as.integer(surf.stat == "interpolate/exact"))
	if(j==1) {
	    trace.hat.out <- z$trL
	    one.delta <- z$delta1
	    two.delta <- z$delta2
	}
	fitted.residuals <- y - z$fitted.values
	if(j < iterations)
	    robust <- .Fortran(R_lowesw,
			       as.double(fitted.residuals),
			       as.integer(N),
			       robust = double(N),
			       integer(N))$robust
    }
    if(surface == "interpolate")
    {
	pars <- z$parameter
	names(pars) <- c("d", "n", "vc", "nc", "nv", "liv", "lv")
	enough <- (D + 1) * pars["nv"]
	fit.kd <- list(parameter=pars, a=z$a[1L:pars[4L]], xi=z$xi[1L:pars[4L]],
		       vert=z$vert, vval=z$vval[1L:enough])
    }
    if(iterations > 1) {
	pseudovalues <- .Fortran(R_lowesp,
				 as.integer(N),
				 as.double(y),
				 as.double(z$fitted.values),
				 as.double(weights),
				 as.double(robust),
				 integer(N),
				 pseudovalues = double(N))$pseudovalues
	zz <- .C(R_loess_raw,
		as.double(pseudovalues),
		as.double(x),
		as.double(weights),
		as.double(weights),
		as.integer(D),
		as.integer(N),
		as.double(span),
		as.integer(degree),
		as.integer(nonparametric),
		as.integer(order.drop.sqr),
		as.integer(sum.drop.sqr),
		as.double(span*cell),
		as.character(surf.stat),
		temp = double(N),
		parameter = integer(7),
		a = integer(max.kd),
		xi = double(max.kd),
		vert = double(2*D),
		vval = double((D+1)*max.kd),
		diagonal = double(N),
		trL = double(1),
		delta1 = double(1),
		delta2 = double(1),
		as.integer(0L))
	pseudo.resid <- pseudovalues - zz$temp
    }
    sum.squares <- if(iterations <= 1) sum(weights * fitted.residuals^2)
    else sum(weights * pseudo.resid^2)
    enp <- one.delta + 2*trace.hat.out - N
    s <- sqrt(sum.squares/one.delta)
    pars <- list(robust=robust, span=span, degree=degree, normalize=normalize,
		 parametric=parametric, drop.square=drop.square,
		 surface=surface, cell=cell, family=
		 if(iterations <= 1) "gaussian" else "symmetric",
		 iterations=iterations)
    fit <- list(n=N, fitted=z$fitted.values, residuals=fitted.residuals,
		enp=enp, s=s, one.delta=one.delta, two.delta=two.delta,
		trace.hat=trace.hat.out, divisor=divisor)
    fit$pars <- pars
    if(surface == "interpolate") fit$kd <- fit.kd
    class(fit) <- "loess"
    fit
}

predict.loess <- function(object, newdata = NULL, se = FALSE, ...)
{
    if(!inherits(object, "loess"))
	stop("first argument must be a \"loess\" object")
    if(is.null(newdata) && !se)
	return(fitted(object))

    newx <- if(is.null(newdata)) object$x
    else if(is.data.frame(newdata))
        as.matrix(model.frame(delete.response(terms(object)), newdata))
    else as.matrix(newdata)
    res <- predLoess(object$y, object$x, newx, object$s, object$weights,
		     object$pars$robust, object$pars$span, object$pars$degree,
		     object$pars$normalize, object$pars$parametric,
		     object$pars$drop.square, object$pars$surface,
		     object$pars$cell, object$pars$family,
		     object$kd, object$divisor, se=se)
    if(!is.null(out.attrs <- attr(newdata, "out.attrs"))) { # expand.grid used
        if(se) {
            res$fit <- array(res$fit, out.attrs$dim, out.attrs$dimnames)
            res$se.fit <- array(res$se.fit, out.attrs$dim, out.attrs$dimnames)
        } else res <- array(res, out.attrs$dim, out.attrs$dimnames)
    }
    if(se)
	res$df <- object$one.delta^2/object$two.delta
    res
}

predLoess <-
  function(y, x, newx, s, weights, robust, span, degree,
	   normalize, parametric, drop.square, surface, cell, family,
	   kd, divisor, se=FALSE)
{
    ## translation of pred_
    D <- NCOL(x); N <- NROW(x); M <- NROW(newx)
    x <- as.matrix(x); newx <- as.matrix(newx)
    newx <- newx/rep(divisor, rep(M, D))
    x <- x/rep(divisor, rep(N, D))
    sum.drop.sqr <- sum(drop.square)
    nonparametric <- sum(!parametric)
    order.parametric <- order(parametric)
    x <- x[, order.parametric, drop=FALSE]
    x.evaluate <- newx[, order.parametric, drop=FALSE]
    order.drop.sqr <- (2 - drop.square)[order.parametric]
    if(surface == "direct") {
	if(se) {
	    z <- .C(R_loess_dfitse,
		    as.double(y),
		    as.double(x),
		    as.double(x.evaluate),
		    as.double(weights*robust),
		    as.double(robust),
		    as.integer(family =="gaussian"),
		    as.double(span),
		    as.integer(degree),
		    as.integer(nonparametric),
		    as.integer(order.drop.sqr),
		    as.integer(sum.drop.sqr),
		    as.integer(D),
		    as.integer(N),
		    as.integer(M),
		    fit = double(M),
		    L = double(N*M))[c("fit", "L")]
	    fit <- z$fit
	    se.fit <- (matrix(z$L^2, M, N)/rep(weights, rep(M,N))) %*% rep(1,N)
	    se.fit <- drop(s * sqrt(se.fit))
	} else {
	    fit <- .C(R_loess_dfit,
		      as.double(y),
		      as.double(x),
		      as.double(x.evaluate),
		      as.double(weights*robust),
		      as.double(span),
		      as.integer(degree),
		      as.integer(nonparametric),
		      as.integer(order.drop.sqr),
		      as.integer(sum.drop.sqr),
		      as.integer(D),
		      as.integer(N),
		      as.integer(M),
		      fit = double(M))$fit
	}
    }
    else { ## interpolate
	## need to eliminate points outside original range - not in pred_
	inside <- matrix(FALSE, M, ncol = D)
	ranges <- apply(x, 2L, range)
	inside <- (x.evaluate <= rep(ranges[2,], rep(M, D))) &
	(x.evaluate >= rep(ranges[1,], rep(M, D)))
	inside <- inside %*% rep(1, D) == D
	M1 <- sum(inside)
	fit <- rep(NA_real_, M)
	if(any(inside))
	    fit[inside] <- .C(R_loess_ifit,
			      as.integer(kd$parameter),
			      as.integer(kd$a), as.double(kd$xi),
			      as.double(kd$vert), as.double(kd$vval),
			      as.integer(M1),
			      as.double(x.evaluate[inside, ]),
			      fit = double(M1))$fit
	if(se) {
	    se.fit <- rep(NA_real_, M)
	    if(any(inside)) {
		L <- .C(R_loess_ise,
			as.double(y),
			as.double(x),
			as.double(x.evaluate[inside, ]),
			as.double(weights),
			as.double(span),
			as.integer(degree),
			as.integer(nonparametric),
			as.integer(order.drop.sqr),
			as.integer(sum.drop.sqr),
			as.double(span*cell),
			as.integer(D),
			as.integer(N),
			as.integer(M1),
			double(M1),
			L = double(N*M1)
			)$L
		tmp <- (matrix(L^2, M1, N)/rep(weights, rep(M1,N))) %*% rep(1,N)
		se.fit[inside] <- drop(s * sqrt(tmp))
	    }
	}
    }
    if(se) list(fit = fit, se.fit = drop(se.fit), residual.scale = s) else fit
}

pointwise <- function(results, coverage)
{
    fit <- results$fit
    lim <- qt((1 - coverage)/2, results$df, lower.tail = FALSE) * results$se.fit
    list(fit = fit, lower = fit - lim, upper = fit + lim)
}

print.loess <- function(x, digits=max(3, getOption("digits")-3), ...)
{
    if(!is.null(cl <- x$call)) {
	cat("Call:\n")
	dput(cl, control=NULL)
    }
    cat("\nNumber of Observations:", x$n, "\n")
    cat("Equivalent Number of Parameters:", format(round(x$enp, 2)), "\n")
    cat("Residual",
	if(x$pars$family == "gaussian")"Standard Error:" else "Scale Estimate:",
	format(signif(x$s, digits)), "\n")
    invisible(x)
}

summary.loess <- function(object, ...)
{
    class(object) <- "summary.loess"
    object
}

print.summary.loess <- function(x, digits=max(3, getOption("digits")-3), ...)
{
    if(!is.null(cl <- x$call)) {
	cat("Call:\n")
	dput(cl, control=NULL)
    }
    cat("\nNumber of Observations:", x$n, "\n")
    cat("Equivalent Number of Parameters:", format(round(x$enp, 2)), "\n")
    if(x$pars$family == "gaussian")
	cat("Residual Standard Error:", format(signif(x$s, digits)), "\n")
    else cat("Residual Scale Estimate:", format(signif(x$s, digits)), "\n")
    cat("Trace of smoother matrix:", format(round(x$trace.hat, 2)), "\n")
    cat("\nControl settings:\n")
    cat("  normalize: ", x$pars$normalize, "\n")
    cat("  span	    : ", format(x$pars$span), "\n")
    cat("  degree   : ", x$pars$degree, "\n")
    cat("  family   : ", x$pars$family)
    if(x$pars$family != "gaussian")
	cat("	    iterations =", x$pars$iterations)
    cat("\n  surface  : ", x$pars$surface)
    if(x$pars$surface == "interpolate")
	cat("	  cell =", format(x$pars$cell))
    cat("\n")
    invisible(x)
}

scatter.smooth <-
    function(x, y = NULL, span = 2/3, degree = 1,
	     family = c("symmetric", "gaussian"),
	     xlab = NULL, ylab = NULL,
	     ylim = range(y, prediction$y, na.rm = TRUE),
             evaluation = 50, ...)
{
    xlabel <- if (!missing(x)) deparse(substitute(x))
    ylabel <- if (!missing(y)) deparse(substitute(y))
    xy <- xy.coords(x, y, xlabel, ylabel)
    x <- xy$x
    y <- xy$y
    xlab <- if (is.null(xlab)) xy$xlab else xlab
    ylab <- if (is.null(ylab)) xy$ylab else ylab
    prediction <- loess.smooth(x, y, span, degree, family, evaluation)
    plot(x, y, ylim = ylim, xlab = xlab, ylab = ylab, ...)
    lines(prediction)
    invisible()
}

loess.smooth <-
  function(x, y, span = 2/3, degree = 1, family = c("symmetric", "gaussian"),
	   evaluation = 50, ...)
{
    notna <- !(is.na(x) | is.na(y))
    new.x <- seq.int(min(x[notna]), max(x[notna]), length.out = evaluation)

    control <- loess.control(...)
    ##	x <- matrix(x, ncol = 1L)
    ##	n <- length(y)
    x <- x[notna]; y <- y[notna]
    w <- rep(1, length(y))
    family <- match.arg(family)
    iterations <- if(family == "gaussian") 1 else control$iterations
    fit <- simpleLoess(y, x, w, span, degree, FALSE, FALSE,
		       normalize=FALSE, "none", "interpolate",
		       control$cell, iterations, control$trace.hat)
    kd <- fit$kd
    z <- .C(R_loess_ifit,
	    as.integer(kd$parameter),
	    as.integer(kd$a), as.double(kd$xi),
	    as.double(kd$vert), as.double(kd$vval),
	    as.integer(evaluation),
	    as.double(new.x),
	    fit = double(evaluation))$fit
    list(x = new.x, y = z)
}

## panel.smooth is currently defined in ../../graphics/R/coplot.R :
## panel.smooth <-
##   function(x, y, span = 2/3, degree = 1, family = c("symmetric", "gaussian"),
##	   zero.line = FALSE, evaluation = 50, ...)
## {
##   points(x, y, ...)
##   lines(loess.smooth(x, y, span, degree, family, evaluation), ...)
##   if(zero.line) abline(h = 0, ...)
## }

anova.loess <- function(object, ...)
{
    objects <- list(object, ...)
    responses <- as.character(lapply(objects,
				     function(x) as.character(x$terms[[2L]])))
    sameresp <- responses == responses[1L]
    ## calculate the number of models
    if (!all(sameresp)) {
	objects <- objects[sameresp]
	warning("models with response ",
                sQuote(deparse(responses[!sameresp])),
                "removed because response differs from model 1")
    }
    nmodels <- length(objects)
    if(nmodels <= 1) stop("no models to compare")
    models <- as.character(lapply(objects, function(x) x$call))
    descr <- paste("Model ", format(1L:nmodels), ": ", models,
		   sep = "", collapse = "\n")
    ## extract statistics
    delta1 <- sapply(objects, function(x) x$one.delta)
    delta2 <- sapply(objects, function(x) x$two.delta)
    s <- sapply(objects, function(x) x$s)
    enp <- sapply(objects, function(x) x$enp)
    rss <- s^2*delta1
    max.enp <- order(enp)[nmodels]
    d1diff <- abs(diff(delta1))
    dfnum <- c(d1diff^2/abs(diff(delta2)))
    dfden <- (delta1^2/delta2)[max.enp]
    Fvalue <- c(NA, (abs(diff(rss))/d1diff)/s[max.enp]^2)
    pr <- pf(Fvalue, dfnum, dfden, lower.tail = FALSE)
    ans <- data.frame(ENP = round(enp,2), RSS = rss, "F-value" = Fvalue,
		      "Pr(>F)" = pr, check.names = FALSE)
    attr(ans, "heading") <-
	paste(descr, "\n\n", "Analysis of Variance:   denominator df ",
	      format(round(dfden,2)), "\n", sep = "")
    class(ans) <- c("anova", "data.frame")
    ans
}
#  File src/library/stats/R/logLik.R
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

logLik <- function(object, ...) UseMethod("logLik")

print.logLik <- function(x, digits = getOption("digits"), ...)
{
    cat("'log Lik.' ", paste(format(c(x), digits=digits), collapse=", "),
        " (df=",format(attr(x,"df")),")\n",sep="")
    invisible(x)
}

str.logLik <- function(object, digits = max(2, getOption("digits") - 3),
                       vec.len = getOption("str")$vec.len, ...)
{
    cl <- oldClass(object)
    len <- length(co <- c(object))
    cutl <- len > vec.len
    cat("Class", if (length(cl) > 1) "es",
	" '", paste(cl, collapse = "', '"), "' : ",
	paste(format(co[seq_len(min(len,vec.len))], digits=digits),
	      collapse = ", "), if(cutl)", ...",
	" (df=",format(attr(object,"df")),")\n",sep="")
}

## rather silly (but potentially used in pkg nlme):
as.data.frame.logLik <- function (x, ...)
    as.data.frame(c(x), ...)

## >> logLik.nls() in ../../nls/R/nls.R

## from package:nlme

## log-likelihood for glm objects
logLik.glm <- function(object, ...)
{
    if(length(list(...)))
        warning("extra arguments discarded")
    fam <- family(object)$family
    p <- object$rank
    ## allow for estimated dispersion
    if(fam %in% c("gaussian", "Gamma", "inverse.gaussian")) p <- p + 1
    val <- p - object$aic / 2
    attr(val, "df") <- p
    class(val) <- "logLik"
    val
}

## log-likelihood for lm objects
logLik.lm <- function(object, REML = FALSE, ...)
{
    res <- object$residuals # not resid(object) because of NA methods
    p <- object$rank
    N <- length(res)
    if(is.null(w <- object$weights)) {
        w <- rep.int(1, N)
    } else {
        ## this is OK as both resids and weights are for the cases used
        excl <- w == 0			# eliminating zero weights
        if (any(excl)) {
            res <- res[!excl]
            N <- length(res)
            w <- w[!excl]
        }
    }
    N0 <- N
    if(REML) N <- N - p
    val <- .5* (sum(log(w)) - N * (log(2 * pi) + 1 - log(N) +
                                   log(sum(w*res^2))))
    if(REML) val <- val - sum(log(abs(diag(object$qr$qr)[1L:p])))
    attr(val, "nall") <- N0
    attr(val, "nobs") <- N
    attr(val, "df") <- p + 1
    class(val) <- "logLik"
    val
}

#  File src/library/stats/R/loglin.R
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

loglin <- function(table, margin, start = rep(1, length(table)), fit =
                   FALSE, eps = 0.1, iter = 20, param = FALSE, print =
                   TRUE) {
    rfit <- fit

    dtab <- dim(table)
    nvar <- length(dtab)

    ncon <- length(margin)
    conf <- matrix(0, nrow = nvar, ncol = ncon)
    nmar <- 0
    varnames <- names(dimnames(table))
    for (k in seq_along(margin)) {
        tmp <- margin[[k]]
        if (is.character(tmp)) {
            ## Rewrite margin names to numbers
            tmp <- match(tmp, varnames)
            margin[[k]] <- tmp
        }
        conf[seq_along(tmp), k] <- tmp
        nmar <- nmar + prod(dtab[tmp])
    }

    ntab <- length(table)
    if (length(start) != ntab ) stop("'start' and 'table' must be same length")

    storage.mode(conf) <- "integer"
    ## NOTE: We make no use of the arguments locmar, nmar, marg, nu, and
    ## u.  It might make sense to eliminate them and simplify the underlying C
    ## code accordingly.
    z <- .C("loglin",
            as.integer(nvar),
            as.integer(dtab),
            as.integer(ncon),
            conf,
            as.integer(ntab),
            as.double(table),
            fit = as.double(start),
            locmar = integer(ncon),
            as.integer(nmar),
            marginals = double(nmar),
            as.integer(ntab),
            u = double(ntab),
            as.double(eps),
            as.integer(iter),
            dev = double(iter),
            nlast = integer(1L),
            ifault = integer(1L),
            PACKAGE = "stats")
    switch(z$ifault,
           stop("this should not happen"),
           stop("this should not happen"),
           warning("algorithm did not converge"),
           stop("incorrect specification of 'table' or 'start'")
           )

    if (print)
        cat(z$nlast, "iterations: deviation", z$dev[z$nlast], "\n")

    fit <- z$fit
    attributes(fit) <- attributes(table)

    ## Pearson chi-sq test statistic
    observed <- as.vector(table[start > 0])
    expected <- as.vector(fit[start > 0])
    pearson <- sum((observed - expected)^2 / expected)

    ## Likelihood Ratio Test statistic
    observed <- as.vector(table[table * fit > 0])
    expected <- as.vector(fit[table * fit > 0])
    lrt <- 2 * sum(observed * log(observed / expected))

    ## Compute degrees of freedom.
    ## Use a dyadic-style representation for the (possible) subsets B.
    ## Let u_i(B) = 1 if i is contained in B and 0 otherwise.  Then B
    ## <-> u(B) = (u_1(B),...,u_N(B)) <-> \sum_{i=1}^N u_i(B) 2^{i-1}.
    ## See also the code for 'dyadic' below which computes the u_i(B).
    subsets <- function(x) {
        y <- list(vector(mode(x), length = 0))
        for (i in seq_along(x)) {
            y <- c(y, lapply(y, "c", x[i]))
        }
        y[-1L]
    }
    df <- rep.int(0, 2^nvar)
    for (k in seq_along(margin)) {
        terms <- subsets(margin[[k]])
        for (j in seq_along(terms))
            df[sum(2 ^ (terms[[j]] - 1))] <- prod(dtab[terms[[j]]] - 1)
    }

    ## Rewrite margin numbers to names if possible
    if (!is.null(varnames) && all(nzchar(varnames))) {
        for (k in seq_along(margin))
            margin[[k]] <- varnames[margin[[k]]]
    } else {
        varnames <- as.character(1 : ntab)
    }

    y <- list(lrt = lrt,
              pearson = pearson,
              df = ntab - sum(df) - 1,
              margin = margin)

    if (rfit)
        y$fit <- fit

    if (param) {
        fit <- log(fit)
        terms <- seq_along(df)[df > 0]

        parlen <- length(terms) + 1
        parval <- list(parlen)
        parnam <- character(parlen)

        parval[[1L]] <- mean(fit)
        parnam[1L] <- "(Intercept)"
        fit <- fit - parval[[1L]]

        ## Get the u_i(B) in the rows of 'dyadic', see above.
        dyadic <- NULL
        while(any(terms > 0)) {
            dyadic <- cbind(dyadic, terms %% 2)
            terms <- terms %/% 2
        }
        dyadic <- dyadic[order(rowSums(dyadic)), ]

        for (i in 2 : parlen) {
            vars <- which(dyadic[i - 1, ] > 0)
            parval[[i]] <- apply(fit, vars, mean)
            parnam[i] <- paste(varnames[vars], collapse = ".")
            fit <- sweep(fit, vars, parval[[i]], check.margin=FALSE)
        }

        names(parval) <- parnam
        y$param <- parval
    }

    return(y)
}
#  File src/library/stats/R/lowess.R
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

lowess <- function(x, y=NULL, f=2/3, iter=3, delta=.01*diff(range(xy$x[o]))) {
    xy <- xy.coords(x,y)
    n <- as.integer(length(xy$x))
    if(n == 0) stop("'x' is empty")
    o <- order(xy$x)
    .C("lowess",
       x=as.double(xy$x[o]),
       as.double(xy$y[o]),
       n,
       as.double(f),
       as.integer(iter),
       as.double(delta),
       y=double(n),
       double(n),
       double(n), PACKAGE="stats")[c("x","y")]
}
#  File src/library/stats/R/lsfit.R
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

lsfit <- function(x, y, wt=NULL, intercept=TRUE, tolerance=1e-07, yname=NULL)
{
    ## find names of x variables (design matrix)

    x <- as.matrix(x)
    y <- as.matrix(y)
    xnames <- colnames(x)
    if( is.null(xnames) ) {
	if(ncol(x)==1) xnames <- "X"
	else xnames <- paste("X", 1L:ncol(x), sep="")
    }
    if( intercept ) {
	x <- cbind(1, x)
	xnames <- c("Intercept", xnames)
    }

    ## find names of y variables (responses)

    if(is.null(yname) && ncol(y) > 1) yname <- paste("Y", 1L:ncol(y), sep="")

    ## remove missing values

    good <- complete.cases(x, y, wt)
    dimy <- dim(as.matrix(y))
    if( any(!good) ) {
	warning(gettextf("%d missing values deleted", sum(!good)), domain = NA)
	x <- as.matrix(x)[good, ]
	y <- as.matrix(y)[good, ]
	wt <- wt[good]
    }

    ## check for compatible lengths

    nrx <- NROW(x)
    ncx <- NCOL(x)
    nry <- NROW(y)
    ncy <- NCOL(y)
    nwts <- length(wt)
    if(nry != nrx)
        stop(gettextf("'X' matrix has %d responses, 'Y' has %d responses",
                      nrx, nry), domain = NA)
    if(nry < ncx)
        stop(gettextf("%d responses, but only %d variables", nry, ncx),
             domain = NA)

    ## check weights if necessary

    if( !is.null(wt) ) {
	if(any(wt < 0)) stop("negative weights not allowed")
	if(nwts != nry)
            stop(gettextf("number of weights = %d should equal %d (number of responses)", nwts, nry), domain = NA)
	wtmult <- wt^0.5
	if( any(wt==0) ) {
	    xzero <- as.matrix(x)[wt==0, ]
	    yzero <- as.matrix(y)[wt==0, ]
	}
	x <- x*wtmult
	y <- y*wtmult
	invmult <- 1/ifelse(wt==0, 1, wtmult)
    }

    ## call linpack

    storage.mode(x) <- "double"
    storage.mode(y) <- "double"
    z <- .Fortran("dqrls",
		  qr=x,
		  n=nrx,
		  p=ncx,
		  y=y,
		  ny=ncy,
		  tol=tolerance,
		  coefficients=mat.or.vec(ncx, ncy),
		  residuals=mat.or.vec(nrx, ncy),
		  effects=mat.or.vec(nrx, ncy),
		  rank=integer(1L),
		  pivot=as.integer(1L:ncx),
		  qraux=double(ncx),
		  work=double(2*ncx),
                  PACKAGE="base")

    ## dimension and name output from linpack

    resids <- array(NA, dim=dimy)
    dim(z$residuals) <- c(nry, ncy)
    if(!is.null(wt)) {
	if(any(wt==0)) {
	    if(ncx==1) fitted.zeros <- xzero * z$coefficients
	    else fitted.zeros <- xzero %*% z$coefficients
	    z$residuals[wt==0, ] <- yzero - fitted.zeros
	}
	z$residuals <- z$residuals*invmult
    }
    resids[good, ] <- z$residuals
    if(dimy[2L] == 1 && is.null(yname)) {
	resids <- as.vector(resids)
	names(z$coefficients) <- xnames
    }
    else {
	colnames(resids) <- yname
	colnames(z$effects) <- yname
	dim(z$coefficients) <- c(ncx, ncy)
	dimnames(z$coefficients) <- list(xnames, yname)
    }
    z$qr <- as.matrix(z$qr)
    colnames(z$qr) <- xnames
    output <- list(coefficients=z$coefficients, residuals=resids)

    ## if X matrix was collinear, then the columns would have been
    ## pivoted hence xnames need to be corrected

    if( z$rank != ncx ) {
	xnames <- xnames[z$pivot]
	dimnames(z$qr) <- list(NULL, xnames)
	warning("'X' matrix was collinear")
    }

    ## return weights if necessary

    if (!is.null(wt) ) {
	weights <- rep.int(NA, dimy[1L])
	weights[good] <- wt
	output <- c(output, list(wt=weights))
    }

    ## return rest of output

    rqr <- list(qt=z$effects, qr=z$qr, qraux=z$qraux, rank=z$rank,
		pivot=z$pivot, tol=z$tol)
    oldClass(rqr) <- "qr"
    output <- c(output, list(intercept=intercept, qr=rqr))
    return(output)
}

ls.diag <- function(ls.out)
{
    resids <- as.matrix(ls.out$residuals)
    d0 <- dim(resids)
    xnames <- colnames(ls.out$qr$qr)
    yname <- colnames(resids)

    ## remove any missing values

    good <- complete.cases(resids, ls.out$wt)
    if( any(!good) ) {
	warning("missing observations deleted")
	resids <- resids[good, , drop = FALSE]
    }

    ## adjust residuals if needed

    if( !is.null(ls.out$wt) ) {
	if( any(ls.out$wt[good] == 0) )
	    warning("observations with 0 weight not used in calculating standard deviation")
	resids <- resids * ls.out$wt[good]^0.5
    }

    ## initialize

    p <- ls.out$qr$rank
    n <- nrow(resids)
    hatdiag <- rep.int(NA, n)
    stats <- array(NA, dim = d0)
    colnames(stats) <- yname
    stdres <- studres <- dfits <- Cooks <- stats

    ## calculate hat matrix diagonals

    q <- qr.qy(ls.out$qr, rbind(diag(p), matrix(0, nrow=n-p, ncol=p)))
    hatdiag[good] <- rowSums(as.matrix(q^2))

    ## calculate diagnostics

    stddev <- (colSums(as.matrix(resids^2))/(n - p))^0.5
    stddevmat <- matrix(stddev, nrow=sum(good), ncol=ncol(resids), byrow=TRUE)
    stdres[good, ] <- resids/((1-hatdiag[good])^0.5 * stddevmat)
    studres[good, ] <- (stdres[good, ]*stddevmat)/
        (((n-p)*stddevmat^2 - resids^2/(1-hatdiag[good]))/(n-p-1))^0.5
    dfits[good, ] <- (hatdiag[good]/(1-hatdiag[good]))^0.5 * studres[good, ]
    Cooks[good, ] <- ((stdres[good, ]^2 * hatdiag[good])/p)/(1-hatdiag[good])
    if(ncol(resids)==1 && is.null(yname)) {
	stdres <- as.vector(stdres)
	Cooks <- as.vector(Cooks)
	studres <- as.vector(studres)
	dfits <- as.vector(dfits)
    }

    ## calculate unscaled covariance matrix

    qr <- as.matrix(ls.out$qr$qr[1L:p, 1L:p])
    qr[row(qr)>col(qr)] <- 0
    qrinv <- solve(qr)
    covmat.unscaled <- qrinv%*%t(qrinv)
    dimnames(covmat.unscaled) <- list(xnames, xnames)

    ## calculate scaled covariance matrix

    covmat.scaled <- sum(stddev^2) * covmat.unscaled

    ## calculate correlation matrix

    cormat <- covmat.scaled/
	(outer(diag(covmat.scaled), diag(covmat.scaled))^0.5)

    ## calculate standard error

    stderr <- outer(diag(covmat.unscaled)^0.5, stddev)
    dimnames(stderr) <- list(xnames, yname)

    return(list(std.dev=stddev, hat=hatdiag, std.res=stdres,
		stud.res=studres, cooks=Cooks, dfits=dfits,
		correlation=cormat, std.err=stderr,
		cov.scaled=covmat.scaled, cov.unscaled=covmat.unscaled))
}

ls.print <- function(ls.out, digits=4, print.it=TRUE)
{
    ## calculate residuals to be used

    resids <- as.matrix(ls.out$residuals)
    if( !is.null(ls.out$wt) ) {
	if(any(ls.out$wt == 0))
	    warning("observations with 0 weights not used")
	resids <- resids * ls.out$wt^0.5
    }
    n <- apply(resids, 2L, length)-colSums(is.na(resids))
    lsqr <- ls.out$qr
    p <- lsqr$rank

    ## calculate total sum sq and df

    if(ls.out$intercept) {
	if(is.matrix(lsqr$qt))
	    totss <- colSums(lsqr$qt[-1, ]^2)
	else totss <- sum(lsqr$qt[-1L]^2)
	degfree <- p - 1
    } else {
	totss <- colSums(as.matrix(lsqr$qt^2))
	degfree <- p
    }

    ## calculate residual sum sq and regression sum sq

    resss <- colSums(resids^2, na.rm=TRUE)
    resse <- (resss/(n-p))^.5
    regss <- totss - resss
    rsquared <- regss/totss
    fstat <- (regss/degfree)/(resss/(n-p))
    pvalue <- pf(fstat, degfree, (n-p), lower.tail = FALSE)

    ## construct summary

    Ynames <- colnames(resids)
    summary <- cbind(format(round(resse, digits)),
		     format(round(rsquared, digits)),
		     format(round(fstat, digits)),
		     format(degfree),
		     format(n-p),
		     format(round(pvalue, digits)))
    dimnames(summary) <- list(Ynames,
			      c("Mean Sum Sq", "R Squared",
				"F-value", "Df 1", "Df 2", "Pr(>F)"))
    mat <- as.matrix(lsqr$qr[1L:p, 1L:p])
    mat[row(mat)>col(mat)] <- 0
    qrinv <- solve(mat)

    ## construct coef table

    m.y <- ncol(resids)
    coef.table <- as.list(1L:m.y)
    if(m.y==1) coef <- matrix(ls.out$coefficients, ncol=1)
    else coef <- ls.out$coefficients
    for(i in 1L:m.y) {
	covmat <- (resss[i]/(n[i]-p)) * (qrinv%*%t(qrinv))
	se <- diag(covmat)^.5
	coef.table[[i]] <- cbind(coef[, i], se, coef[, i]/se,
				 2*pt(abs(coef[, i]/se), n[i]-p,
                                      lower.tail = FALSE))
	dimnames(coef.table[[i]]) <-
	    list(colnames(lsqr$qr),
		 c("Estimate", "Std.Err", "t-value", "Pr(>|t|)"))

	##-- print results --

	if(print.it) {
	    if(m.y>1)
		cat("Response:", Ynames[i], "\n\n")
	    cat(paste("Residual Standard Error=",
                      format(round(resse[i], digits)), "\nR-Square=",
                      format(round(rsquared[i], digits)), "\nF-statistic (df=",
		      format(degfree), ", ", format(n[i]-p), ")=",
		      format(round(fstat[i], digits)), "\np-value=",
		      format(round(pvalue[i], digits)), "\n\n", sep=""))
	    print(round(coef.table[[i]], digits))
	    cat("\n\n")
	}
    }
    names(coef.table) <- Ynames

    invisible(list(summary=summary, coef.table=coef.table))
}
#  File src/library/stats/R/mad.R
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

mad <- function(x, center = median(x), constant = 1.4826,
                na.rm = FALSE, low = FALSE, high = FALSE)
{
    if(na.rm)
	x <- x[!is.na(x)]
    n <- length(x)
    constant *
        if((low || high) && n%%2 == 0) {
            if(low && high) stop("'low' and 'high' cannot be both TRUE")
            n2 <- n %/% 2 + as.integer(high)
            sort(abs(x - center), partial = n2)[n2]
        }
        else median(abs(x - center))
}

#  File src/library/stats/R/mahalanobis.R
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

mahalanobis <- function(x, center, cov, inverted=FALSE, ...)
{
    x <- if(is.vector(x)) matrix(x, ncol=length(x)) else as.matrix(x)
    ## save speed in customary case:
    ## if(any(center != 0))
    x <- sweep(x, 2, center)# = (x - center)

    ## The following would be considerably faster for  small nrow(x) and
    ## slower otherwise; probably always faster if the two t(.) weren't needed:
    ##
    ##	retval <- apply(x * if(inverted) x%*%cov
    ##	                    else    t(solve(cov,t(x), tol=tol.inv)),
    ##			1, sum)
    if(!inverted)
	cov <- solve(cov, ...)
    retval <- rowSums((x%*%cov) * x)
    names(retval) <- rownames(x)
    retval
}
#  File src/library/stats/R/manova.R
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

manova <- function(...)
{
    Call <- fcall <- match.call()
    fcall[[1L]] <- as.name("aov")
    result <- eval(fcall, parent.frame())
    if(inherits(result, "aovlist")) {
        for(i in seq_along(result)) {
            if(!inherits(result[[i]], "maov")) stop("need multiple response")
            class(result[[i]]) <- c("manova", oldClass(result[[i]]))
        }
        attr(result, "call") <- Call
    } else {
        if(!inherits(result, "maov")) stop("need multiple response")
        class(result) <- c("manova", oldClass(result))
        result$call <- Call
    }
    result
}

summary.manova <-
    function(object,
             test = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"),
             intercept = FALSE, tol = 1e-7, ...)
{
    if(!inherits(object, "maov"))
        stop("object must be of class \"manova\" or \"maov\"")
    test <- match.arg(test)

    asgn <- object$assign[object$qr$pivot[1L:object$rank]]
    uasgn <- unique(asgn)
    nterms <- length(uasgn)
    effects <- object$effects
    if (!is.null(effects))
        effects <- as.matrix(effects)[seq_along(asgn), , drop = FALSE]
    rdf <- object$df.residual
    nmeffect <- c("(Intercept)", attr(object$terms, "term.labels"))
    resid <- as.matrix(object$residuals)
    wt <- object$weights
    if (!is.null(wt)) resid <- resid * wt^0.5
    nresp <- NCOL(resid)
    if(nresp <= 1) stop("need multiple responses")

    if (is.null(effects)) {
        df <- nterms <- 0
        ss <- list(0)
        nmrows <- character(0L)
    } else {
        df <- numeric(nterms)
        ss <- list(nterms)
        nmrows <- character(nterms)
        for (i in seq(nterms)) {
            ai <- (asgn == uasgn[i])
            nmrows[i] <- nmeffect[1 + uasgn[i]]
            df[i] <- sum(ai)
            ss[[i]] <- crossprod(effects[ai, , drop=FALSE])
        }
    }
    pm <- pmatch("(Intercept)", nmrows, 0L)
    if (!intercept && pm > 0) {
        nterms <- nterms - 1
        df <- df[-pm]
        nmrows <- nmrows[-pm]
        ss <- ss[-pm]
    }
    names(ss) <- nmrows

    nt <- nterms
    if (rdf > 0) {
        nt <- nterms + 1
        df[nt] <- rdf
        ss[[nt]] <- crossprod(resid)
        names(ss)[nt] <- nmrows[nt] <- "Residuals"
        ok <- df[-nt] > 0
        eigs <- array(NA, c(nterms, nresp))
        dimnames(eigs) <- list(nmrows[-nt], NULL)
        stats <- matrix(NA, nt, 5)
        dimnames(stats) <-  list(nmrows,
                                 c(test, "approx F", "num Df", "den Df",
                                   "Pr(>F)"))
        sc <- sqrt(diag(ss[[nt]]))
        ## Let us try to distnguish bad scaling and near-perfect fit
        sss <- sc^2
        for(i in seq_len(nterms)[ok]) sss <- sss +  diag(ss[[i]])
        sc[sc < sqrt(sss)*1e-6] <- 1
        D <- diag(1/sc)
        rss.qr <- qr(D %*% ss[[nt]] %*% D, tol=tol)
        if(rss.qr$rank < ncol(resid))
            stop(gettextf("residuals have rank %d < %d",
                          rss.qr$rank, ncol(resid)), domain = NA)
        if(!is.null(rss.qr))
            for(i in seq_len(nterms)[ok]) {
                A1 <- qr.coef(rss.qr, D %*% ss[[i]] %*% D)
                eigs[i, ] <- Re(eigen(A1, symmetric = FALSE)$values)
                stats[i, 1L:4] <-
                    switch(test,
                           "Pillai" = Pillai(eigs[i,  ], df[i], df[nt]),
                           "Wilks" = Wilks(eigs[i,  ], df[i], df[nt]),
                           "Hotelling-Lawley" = HL(eigs[i,  ], df[i], df[nt]),
                           "Roy" = Roy(eigs[i,  ], df[i], df[nt]))
                ok <- stats[, 2] >= 0 & stats[, 3] > 0 & stats[, 4] > 0
                ok <- !is.na(ok) & ok
                stats[ok, 5] <- pf(stats[ok, 2], stats[ok, 3], stats[ok, 4],
                                   lower.tail = FALSE)

            }
        x <- list(row.names = nmrows, SS = ss,
                  Eigenvalues = eigs, stats = cbind(Df=df, stats=stats))
    } else x <- list(row.names = nmrows, SS = ss, Df = df)
    class(x) <- "summary.manova"
    x
}

print.summary.manova <- function(x, digits = getOption("digits"), ...)
{
    if(length(stats <- x$stats)) {
        print.anova(stats)
    } else {
        cat("No error degrees of freedom\n\n")
        print(data.frame(Df = x$Df, row.names = x$row.names))
    }
    invisible(x)
}
#  File src/library/stats/R/mantelhaen.test.R
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

mantelhaen.test <-
function(x, y = NULL, z = NULL,
         alternative = c("two.sided", "less", "greater"),
         correct = TRUE, exact = FALSE, conf.level = 0.95)
{
    DNAME <- deparse(substitute(x))
    if(is.array(x)) {
        if(length(dim(x)) == 3) {
            if(any(is.na(x)))
                stop("NAs are not allowed")
            if(any(dim(x) < 2))
                stop("each dimension in table must be >= 2")
        }
        else
            stop("'x' must be a 3-dimensional array")
    }
    else {
        if(is.null(y))
            stop("if 'x' is not an array, 'y' must be given")
        if(is.null(z))
            stop("if 'x' is not an array, 'z' must be given")
        if(any(diff(c(length(x), length(y), length(z))) != 0L ))
            stop("'x', 'y', and 'z' must have the same length")
        DNAME <- paste(DNAME, "and", deparse(substitute(y)), "and",
                       deparse(substitute(z)))
        OK <- complete.cases(x, y, z)
        x <- factor(x[OK])
        y <- factor(y[OK])
        if((nlevels(x) < 2) || (nlevels(y) < 2))
            stop("'x' and 'y' must have at least 2 levels")
        else
            x <- table(x, y, z[OK])
    }

    if(any(apply(x, 3L, sum) < 2))
        stop("sample size in each stratum must be > 1")

    I <- dim(x)[1L]
    J <- dim(x)[2L]
    K <- dim(x)[3L]

    if((I == 2) && (J == 2)) {
        ## 2 x 2 x K case
        alternative <- match.arg(alternative)
        if(!missing(conf.level) &&
           (length(conf.level) != 1 || !is.finite(conf.level) ||
            conf.level < 0 || conf.level > 1))
            stop("'conf.level' must be a single number between 0 and 1")

        NVAL <- 1
        names(NVAL) <- "common odds ratio"

        if(!exact) {
            ## Classical Mantel-Haenszel 2 x 2 x K test
            s.x <- apply(x, c(1L, 3L), sum)
            s.y <- apply(x, c(2L, 3L), sum)
            n <- apply(x, 3L, sum)
            DELTA <- abs(sum(x[1, 1, ] - s.x[1, ] * s.y[1, ] / n))
            YATES <- ifelse(correct && (DELTA >= .5), .5, 0)
            STATISTIC <- ((DELTA - YATES)^2 /
                          sum(apply(rbind(s.x, s.y), 2L, prod)
                              / (n^2 * (n - 1))))
            PARAMETER <- 1
            if (alternative == "two.sided")
                PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
            else {
                z <- sign(DELTA) * sqrt(STATISTIC)
                PVAL <- pnorm(z, lower.tail = (alternative == "less"))
            }


            names(STATISTIC) <- "Mantel-Haenszel X-squared"
            names(PARAMETER) <- "df"
            METHOD <- paste("Mantel-Haenszel chi-squared test",
                            ifelse(YATES, "with", "without"),
                            "continuity correction")
            s.diag <- sum(x[1, 1, ] * x[2, 2, ] / n)
            s.offd <- sum(x[1, 2, ] * x[2, 1, ] / n)
            ## Mantel-Haenszel (1959) estimate of the common odds ratio.
            ESTIMATE <- s.diag / s.offd
            ## Robins et al. (1986) estimate of the standard deviation
            ## of the log of the Mantel-Haenszel estimator.
            sd <-
                sqrt(  sum((x[1,1,] + x[2,2,]) * x[1,1,] * x[2,2,]
                           / n^2)
                     / (2 * s.diag^2)
                     + sum((  (x[1,1,] + x[2,2,]) * x[1,2,] * x[2,1,]
                            + (x[1,2,] + x[2,1,]) * x[1,1,] * x[2,2,])
                           / n^2)
                     / (2 * s.diag * s.offd)
                     + sum((x[1,2,] + x[2,1,]) * x[1,2,] * x[2,1,]
                           / n^2)
                     / (2 * s.offd^2))
            CINT <-
                switch(alternative,
                       less = c(0, ESTIMATE * exp(qnorm(conf.level) * sd)),
                       greater = c(ESTIMATE * exp(qnorm(conf.level,
                                   lower.tail = FALSE) * sd), Inf),
                       two.sided = {
                           ESTIMATE * exp(c(1, -1) *
                                          qnorm((1 - conf.level) / 2) * sd)
                       })
            RVAL <- list(statistic = STATISTIC,
                         parameter = PARAMETER,
                         p.value = PVAL)
        }
        else {
            ## Exact inference for the 2 x 2 x k case can be carried out
            ## conditional on the strata margins, similar to the case
            ## for Fisher's exact test (k = 1).  Again, the distribution
            ## of S (in our case, sum(x[2, 1, ]) to be consistent with
            ## the notation in Mehta et al. (1985), is of the form
            ##    P(S = s) \propto d(s) * or^s,   lo <= s <= hi
            ## where or is the common odds ratio in the k tables (and
            ## d(.) is a product hypergeometric distribution).

            METHOD <- paste("Exact conditional test of independence",
                            "in 2 x 2 x k tables")
            m <- apply(x, c(2L, 3L), sum)[1, ]
            n <- apply(x, c(2L, 3L), sum)[2, ]
            t <- apply(x, c(1L, 3L), sum)[1, ]
            s <- sum(x[1, 1, ])
            lo <- sum(pmax(0, t - n))
            hi <- sum(pmin(m, t))

            support <- lo : hi
            ## Density of the *central* product hypergeometric
            ## distribution on its support: store for once as this is
            ## needed quite a bit.
            dc <- .C(R_d2x2xk,
                     as.integer(K),
                     as.double(m),
                     as.double(n),
                     as.double(t),
                     d = double(hi - lo + 1))$d
            logdc <- log(dc)

            dn2x2xk <- function(ncp) {
                ## Does not work for boundary values for ncp (0, Inf)
                ## but it does not need to.
                if(ncp == 1) return(dc)
                d <- logdc + log(ncp) * support
                d <- exp(d - max(d))    # beware of overflow
                d / sum(d)
            }
            mn2x2xk <- function(ncp) {
                if(ncp == 0)
                    return(lo)
                if(ncp == Inf)
                    return(hi)
                sum(support * dn2x2xk(ncp))
            }
            pn2x2xk <- function(q, ncp = 1, upper.tail = FALSE) {
                if(ncp == 0) {
                    if(upper.tail)
                        return(as.numeric(q <= lo))
                    else
                        return(as.numeric(q >= lo))
                }
                if(ncp == Inf) {
                    if(upper.tail)
                        return(as.numeric(q <= hi))
                    else
                        return(as.numeric(q >= hi))
                }
                d <- dn2x2xk(ncp)
                if(upper.tail)
                    sum(d[support >= q])
                else
                    sum(d[support <= q])
            }

            ## Determine the p-value.
            PVAL <-
                switch(alternative,
                       less = pn2x2xk(s, 1),
                       greater = pn2x2xk(s, 1, upper.tail = TRUE),
                       two.sided = {
                           ## Note that we need a little fuzz.
                           relErr <- 1 + 10 ^ (-7)
                           d <- dc      # same as dn2x2xk(1)
                           sum(d[d <= d[s - lo + 1] * relErr])
                       })

            ## Determine the MLE for ncp by solving E(S) = s, where the
            ## expectation is with respect to the above distribution.
            mle <- function(x) {
                if(x == lo)
                    return(0)
                if(x == hi)
                    return(Inf)
                mu <- mn2x2xk(1)
                if(mu > x)
                    uniroot(function(t) mn2x2xk(t) - x,
                            c(0, 1))$root
                else if(mu < x)
                    1 / uniroot(function(t) mn2x2xk(1/t) - x,
                                c(.Machine$double.eps, 1))$root
                else
                    1
            }
            ESTIMATE <- mle(s)

            ## Determine confidence intervals for the odds ratio.
            ncp.U <- function(x, alpha) {
                if(x == hi)
                    return(Inf)
                p <- pn2x2xk(x, 1)
                if(p < alpha)
                    uniroot(function(t) pn2x2xk(x, t) - alpha,
                            c(0, 1))$root
                else if(p > alpha)
                    1 / uniroot(function(t) pn2x2xk(x, 1/t) - alpha,
                                c(.Machine$double.eps, 1))$root
                else
                    1
            }
            ncp.L <- function(x, alpha) {
                if(x == lo)
                    return(0)
                p <- pn2x2xk(x, 1, upper.tail = TRUE)
                if(p > alpha)
                    uniroot(function(t)
                            pn2x2xk(x, t, upper.tail = TRUE) - alpha,
                            c(0, 1))$root
            else if (p < alpha)
                1 / uniroot(function(t)
                            pn2x2xk(x, 1/t, upper.tail = TRUE) - alpha,
                            c(.Machine$double.eps, 1))$root
            else
                1
            }
            CINT <- switch(alternative,
                           less = c(0, ncp.U(s, 1 - conf.level)),
                           greater = c(ncp.L(s, 1 - conf.level), Inf),
                           two.sided = {
                               alpha <- (1 - conf.level) / 2
                               c(ncp.L(s, alpha), ncp.U(s, alpha))
                           })

            STATISTIC <- s
            names(STATISTIC) <- "S"

            RVAL <- list(statistic = STATISTIC,
                         p.value = PVAL)
        }

        names(ESTIMATE) <- names(NVAL)
        attr(CINT, "conf.level") <- conf.level
        RVAL <- c(RVAL,
                  list(conf.int = CINT,
                       estimate = ESTIMATE,
                       null.value = NVAL,
                       alternative = alternative))

    }
    else {
        ## Generalized Cochran-Mantel-Haenszel I x J x K test
        ## Agresti (1990), pages 234--235.
        ## Agresti (2002), pages 295ff.
        ## Note that n in the reference is in column-major order.
        ## (Thanks to Torsten Hothorn for spotting this.)
        df <- (I - 1) * (J - 1)
        n <- m <- double(length = df)
        V <- matrix(0, nrow = df, ncol = df)
        for (k in 1 : K) {
            f <- x[ , , k]              # frequencies in stratum k
            ntot <- sum(f)              # n_{..k}
            rowsums <- apply(f, 1L, sum)[-I]
                                        # n_{i.k}, i = 1 to I-1
            colsums <- apply(f, 2L, sum)[-J]
                                        # n_{.jk}, j = 1 to J-1
            n <- n + c(f[-I, -J])
            m <- m + c(outer(rowsums, colsums, "*")) / ntot
            V <- V + (kronecker(diag(ntot * colsums, nrow = J - 1)
                                - outer(colsums, colsums),
                                diag(ntot * rowsums, nrow = I - 1)
                                - outer(rowsums, rowsums))
                      / (ntot^2 * (ntot - 1)))
        }
        n <- n - m
        STATISTIC <- c(crossprod(n, qr.solve(V, n)))
        PARAMETER <- df
        PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
        names(STATISTIC) <- "Cochran-Mantel-Haenszel M^2"
        names(PARAMETER) <- "df"
        METHOD <- "Cochran-Mantel-Haenszel test"
        RVAL <- list(statistic = STATISTIC,
                     parameter = PARAMETER,
                     p.value = PVAL)
    }

    RVAL <- c(RVAL,
              list(method = METHOD,
                   data.name = DNAME))
    class(RVAL) <- "htest"
    return(RVAL)
}
#  File src/library/stats/R/mcnemar.test.R
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

mcnemar.test <- function(x, y = NULL, correct = TRUE)
{
    if (is.matrix(x)) {
        r <- nrow(x)
        if ((r < 2) || (ncol (x) != r))
            stop("'x' must be square with at least two rows and columns")
        if (any(x < 0) || any(is.na(x)))
            stop("all entries of 'x' must be nonnegative and finite")
        DNAME <- deparse(substitute(x))
    }
    else {
        if (is.null(y))
            stop("if 'x' is not a matrix, 'y' must be given")
        if (length(x) != length(y))
            stop("'x' and 'y' must have the same length")
        DNAME <- paste(deparse(substitute(x)), "and",
                       deparse(substitute(y)))
        OK <- complete.cases(x, y)
        x <- factor(x[OK])
        y <- factor(y[OK])
        r <- nlevels(x)
        if ((r < 2) || (nlevels(y) != r))
            stop("'x' and 'y' must have the same number of levels (minimum 2)")
        x <- table(x, y)
    }

    PARAMETER <- r * (r-1) / 2
    METHOD <- "McNemar's Chi-squared test"

    if (correct && (r == 2) && any(x - t(x) != 0)) {
        y <- (abs(x - t(x)) - 1)
        METHOD <- paste(METHOD, "with continuity correction")
    }
    else
        y <- x - t(x)
    x <- x + t(x)

    STATISTIC <- sum(y[upper.tri(x)]^2 / x[upper.tri(x)])
    PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
    names(STATISTIC) <- "McNemar's chi-squared"
    names(PARAMETER) <- "df"

    RVAL <- list(statistic = STATISTIC,
                 parameter = PARAMETER,
                 p.value = PVAL,
                 method = METHOD,
                 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}
#  File src/library/stats/R/median.R
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

median <- function(x, na.rm=FALSE) UseMethod("median")

median.default <- function(x, na.rm = FALSE)
{
    if(is.factor(x)) stop("need numeric data")
    ## all other objects only need sort() & mean() to be working
    if(length(names(x))) names(x) <- NULL # for e.g., c(x = NA_real_)
    if(na.rm) x <- x[!is.na(x)] else if(any(is.na(x))) return(x[FALSE][NA])
    n <- length(x)
    if (n == 0L) return(x[FALSE][NA])
    half <- (n + 1L) %/% 2L
    if(n %% 2L == 1L) sort(x, partial = half)[half]
    else mean(sort(x, partial = half + 0L:1L)[half + 0L:1L])
}
#  File src/library/stats/R/medpolish.R
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

medpolish <-
    function (x, eps=0.01, maxiter=10, trace.iter = TRUE, na.rm = FALSE)
{
    z <- as.matrix(x)
    nr <- nrow(z)
    nc <- ncol(z)
    t <- 0
    r <- numeric(nr)
    c <- numeric(nc)
    oldsum <- 0
    for(iter in 1L:maxiter) {
	rdelta <- apply(z, 1L, median, na.rm = na.rm)
	z <- z - matrix(rdelta, nrow=nr, ncol=nc)
	r <- r + rdelta
	delta <- median(c, na.rm = na.rm)
	c <- c - delta
	t <- t + delta
	cdelta <- apply(z, 2L, median, na.rm = na.rm)
	z <- z - matrix(cdelta, nrow=nr, ncol=nc, byrow=TRUE)
	c <- c + cdelta
	delta <- median(r, na.rm = na.rm)
	r <- r - delta
	t <- t + delta
	newsum <- sum(abs(z), na.rm = na.rm)
	converged <- newsum==0 || abs(newsum-oldsum) < eps*newsum
	if(converged) break
	oldsum <- newsum
	if(trace.iter) cat(iter,":", newsum,"\n")
    }
    if(converged) { if(trace.iter) cat("Final:", newsum,"\n")
    } else
    warning(gettextf("medpolish() did not converge in %d iterations", maxiter),
            domain = NA)
    names(r) <- rownames(z)
    names(c) <- colnames(z)
    ans <- list(overall=t, row=r, col=c, residuals=z,
		name=deparse(substitute(x)))
    class(ans) <- "medpolish"
    ans
}

print.medpolish <- function(x, digits=getOption("digits"), ...)
{
    cat("\nMedian Polish Results (Dataset: \"", x$name, "\")\n", sep="")
    cat("\nOverall:", x$overall, "\n\nRow Effects:\n")
    print(x$row, digits=digits, ...)
    cat("\nColumn Effects:\n")
    print(x$col, digits=digits, ...)
    cat("\nResiduals:\n")
    print(x$residuals, digits=max(2, digits-2), ...)
    cat("\n")
    invisible(x)
}

plot.medpolish <- function(x, main="Tukey Additivity Plot", ...) {
    plot(outer(x$row,x$col)/x$overall, x$residuals,
	 main=main, xlab="Diagnostic Comparison Values",
	 ylab="Residuals", ...)
    abline(h=0, v=0, lty="dotted")
}
#  File src/library/stats/R/mlm.R
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

## mlm := multivariate lm()
summary.mlm <- function(object, ...)
{
    coef <- coef(object)
    ny <- ncol(coef)
    effects <- object$effects
    resid <- object$residuals
    fitted <- object$fitted.values
    ynames <- colnames(coef)
    if(is.null(ynames)) {
	lhs <- object$terms[[2L]]
	if(mode(lhs) == "call" && lhs[[1L]] == "cbind")
	    ynames <- as.character(lhs)[-1L]
	else ynames <- paste("Y", seq_len(ny), sep = "")
    }
    ## we need to ensure that _all_ responses are named
    ind <- ynames == ""
    if(any(ind)) ynames[ind] <-  paste("Y", seq_len(ny), sep = "")[ind]

    value <- vector("list", ny)
    names(value) <- paste("Response", ynames)
    cl <- oldClass(object)
    class(object) <- cl[match("mlm", cl):length(cl)][-1L]
    for(i in seq(ny)) {
	object$coefficients <- coef[, i]
        ## if there is one coef, above drops names
        names(object$coefficients) <- rownames(coef)
	object$residuals <- resid[, i]
	object$fitted.values <- fitted[, i]
	object$effects <- effects[, i]
	object$call$formula[[2L]] <- object$terms[[2L]] <- as.name(ynames[i])
	value[[i]] <- summary(object, ...)
    }
    class(value) <- "listof"
    value
}


### SSD(object) returns object of class "SSD":
###           $SSD  matrix of sums of squares  & products
###           $df   degrees of freedom.
### estVar(object)returns the estimated covariance matrix
SSD <- function(object, ...) UseMethod("SSD")
estVar <- function(object, ...) UseMethod("estVar")

SSD.mlm <- function(object, ...){
    ## It's not all that hard to incorporate weights, but will
    ## anyone use them?
    if (!is.null(object$weights))
        stop("'mlm' objects with weights are not supported")
    structure(list(SSD=crossprod(residuals(object)),
                   call=object$call,
                   df=object$df.residual), class="SSD")
}
estVar.SSD <- function(object, ...)
    object$SSD/object$df

estVar.mlm <- function(object, ...)
    estVar(SSD(object))


### Convenience functions:
###  Tr: is the trace operator
###  proj: the projection operator possibly generalized to matrices.
###  Rg: matrix rank
###  Thin.row, Thin.col: thin matrix to full (row/column) rank

Tr <- function(matrix) sum(diag(matrix))
proj.matrix <- function(X, orth=FALSE){
    X <- Thin.col(X)
    P <- if (ncol(X) == 0)
        matrix(0,nrow(X),nrow(X))
    else
        ## Brute force. There must be a better way...
        X %*% solve(crossprod(X),t(X))
    if (orth) diag(nrow=nrow(X)) - P else P
}

## qr() will miss the cases where a row has all near-zeros,
## sensibly in some ways, annoying in others...

Rank  <- function(X, tol=1e-7)
    qr(zapsmall(X, digits=-log10(tol)+5),
       tol=tol, LAPACK=FALSE)$rank

Thin.row <- function(X, tol=1e-7) {
    X <- zapsmall(X, digits=-log10(tol)+5)
    QR <- qr(t(X), tol=tol, LAPACK=FALSE)
    X[QR$pivot[seq_len(QR$rank)],,drop=FALSE]
}

Thin.col <- function(X, tol=1e-7) {
    X <- zapsmall(X, digits=-log10(tol)+5)
    QR <- qr(X, tol=tol, LAPACK=FALSE)
    X[,QR$pivot[seq_len(QR$rank)],drop=FALSE]
}


mauchly.test <- function(object, ...)
	 UseMethod("mauchly.test", object)

mauchly.test.mlm <- function(object, ...)
 	mauchly.test(SSD(object), ...)


mauchly.test.SSD <- function(object, Sigma=diag(nrow=p),
                          T = Thin.row(proj(M)-proj(X)),
                          M = diag(nrow=p),
                          X = ~0,
                          idata=data.frame(index=seq_len(p)),...)
{
    p <- ncol(object$SSD)

    Xmis <- missing(X)
    Mmis <- missing(M)
    if (missing(T)){
        orig.X <- X
        orig.M <- M
        if (class(M) == "formula") M <- model.matrix(M, idata)
        if (class(X) == "formula") X <- model.matrix(X, idata)
        if (Rank(cbind(M,X)) != Rank(M))
            stop("X does not define a subspace of M")
    }
    Psi <- T %*% Sigma %*% t(T)
    B <- T %*% object$SSD %*% t(T)
    pp <- nrow(T)
    U <- solve(Psi,B)
    n <- object$df
    logW <- log(det(U)) - pp * log(Tr(U/pp))
    ## Asymptotic mumbojumbo (from TWA)....
    rho <- 1 - (2*pp^2 + pp + 2)/(6*pp*n)
    w2 <- (pp+2)*(pp-1)*(pp-2)*(2*pp^3+6*pp^2+3*p +
                                2)/(288*(n*pp*rho)^2)

    z <- -n * rho * logW
    f <- pp * (pp + 1)/2 - 1

    Pr1 <- pchisq(z, f, lower.tail=FALSE)
    Pr2 <- pchisq(z, f+4, lower.tail=FALSE)
    pval <- Pr1 + w2 * (Pr2 - Pr1)
    transformnote <- if (!missing(T))
        c("\nContrast matrix", apply(format(T), 1L, paste, collapse=" "))
    else
        c(
          if (!Xmis)
          c("\nContrasts orthogonal to",
            if (is.matrix(orig.X))  apply(format(X), 2L, paste, collapse=" ")
            else deparse(formula(orig.X)),"",
            if (!Mmis)
            c("\nContrasts spanned by",
              if (is.matrix(orig.M))  apply(format(M), 2L, paste, collapse=" ")
              else deparse(formula(orig.M)),""
              )
            )
          )

    retval <- list(statistic=c(W=exp(logW)),p.value=pval,
                   method=c("Mauchly's test of sphericity", transformnote),
                   data.name=paste("SSD matrix from",
                   deparse(object$call), collapse=" "))
    class(retval) <- "htest"
    retval
}

sphericity <- function(object, Sigma=diag(nrow=p),
                          T = Thin.row(proj(M)-proj(X)),
                          M = diag(nrow=p),
                          X = ~0,
                          idata=data.frame(index=seq_len(p)))
{
    p <- ncol(object$SSD)

    if (missing(T)){
        if (class(M) == "formula") M <- model.matrix(M, idata)
        if (class(X) == "formula") X <- model.matrix(X, idata)
        if (Rank(cbind(M,X)) != Rank(M))
            stop("X does not define a subspace of M")
    }
    Psi <- T %*% Sigma %*% t(T)
    B <- T %*% object$SSD %*% t(T)
    pp <- nrow(T)
    U <- solve(Psi,B)
    sigma <- Tr(U)/pp/object$df
    lambda <- Re(eigen(U)$values)
    GG.eps <- sum(lambda)^2/sum(lambda^2)/pp
    n <- object$df
    HF.eps <- ((n + 1) * pp * GG.eps - 2) / (pp * (n - pp * GG.eps))
    return(list(GG.eps=GG.eps,HF.eps=HF.eps,sigma=sigma))
}

anova.mlm <-
    function(object, ...,
             test = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy", "Spherical"),
             Sigma = diag(nrow = p),
             T = Thin.row(proj(M) - proj(X)),
             M = diag(nrow = p),
             X = ~0,
             idata = data.frame(index = seq_len(p)), tol = 1e-7)
{
    if(length(list(object, ...)) > 1){
        cl <- match.call()
        cl[[1L]] <- anova.mlmlist
        return(eval.parent(cl))
    } else {
        p <- ncol(SSD(object)$SSD)
        Xmis <- missing(X)
        Mmis <- missing(M)
        if (missing(T)){
            orig.M <- M # keep for printing
            orig.X <- X
            if (class(M) == "formula") M <- model.matrix(M, idata)
            if (class(X) == "formula") X <- model.matrix(X, idata)
            if (Rank(cbind(M,X)) != Rank(M))
                stop("X does not define a subspace of M")
        }
        title <- "Analysis of Variance Table\n"
        transformnote <- if (!missing(T))
            c("\nContrast matrix", apply(format(T), 1L, paste, collapse=" "))
        else
            c(
              if (!Xmis)
              c("\nContrasts orthogonal to",
                if (is.matrix(orig.X))
                apply(format(X), 2L, paste, collapse=" ")
                else deparse(formula(orig.X)),"",
                if (!Mmis)
                c("\nContrasts spanned by",
                  if (is.matrix(orig.M))
                  apply(format(M), 2L, paste, collapse=" ")
                  else deparse(formula(orig.M)),""
                  )
                )
              )
        epsnote <- NULL

        ssd <- SSD(object)
        rk <- object$rank
        pp <- nrow(T)
        if(rk > 0) {
            p1 <- 1L:rk
            comp <- object$effects[p1, , drop=FALSE]
            asgn <- object$assign[object$qr$pivot][p1]
            nmeffects <- c("(Intercept)", attr(object$terms, "term.labels"))
            tlabels <- nmeffects[1 + unique(asgn)]
	    ix <- split(seq_len(nrow(comp)), asgn)
            ss <- lapply(ix, function(i) crossprod(comp[i,,drop=FALSE]))
# This was broken. Something similar might work if we implement
#  split.matrix a la split.data.frame
#            ss <- lapply(split(comp,asgn), function(x) crossprod(t(x)))
            df <- sapply(split(asgn,  asgn), length)
        } else {
#            ss <- ssr
#            df <- dfr
#            tlabels <- character(0L)
        }
        test <- match.arg(test)
        nmodels <- length(ss)
        if(test == "Spherical"){
            df.res <- ssd$df
            sph <- sphericity(ssd, T=T, Sigma=Sigma)
            epsnote <- c(paste(format(c("Greenhouse-Geisser epsilon:",
                                        "Huynh-Feldt epsilon:")),
                               format(c(sph$GG.eps, sph$HF.eps),digits=4)),
                         "")

            Psi <- T %*% Sigma %*% t(T)
            stats <- matrix(NA, nmodels+1, 6L)
            colnames(stats) <- c("F", "num Df", "den Df",
                                 "Pr(>F)", "G-G Pr", "H-F Pr")
            for(i in 1L:nmodels) {
                s2 <- Tr(solve(Psi,T %*% ss[[i]] %*% t(T)))/pp/df[i]
                Fval <- s2/sph$sigma
                stats[i,1L:3L] <- abs(c(Fval, df[i]*pp, df.res*pp))
            }
            stats[,4] <- pf(stats[,1L], stats[,2L], stats[,3L], lower.tail=FALSE)
            stats[,5] <- pf(stats[,1L],
                            stats[,2L]*sph$GG.eps, stats[,3L]*sph$GG.eps,
                            lower.tail=FALSE)
            stats[,6] <- pf(stats[,1L],
                            stats[,2L]*min(1,sph$HF.eps),
                            stats[,3L]*min(1,sph$HF.eps),
                            lower.tail=FALSE)
        } else {

            ## Try to distinguish bad scaling and near-perfect fit
            ## Notice that we must transform by T before scaling
            sc <- sqrt(diag(T %*% ssd$SSD %*% t(T)))
            D <- sqrt(sc^2 + rowSums(as.matrix(sapply(ss, function(X)
                                            diag(T %*% X %*% t(T))))))
            sc <- ifelse(sc/D < 1e-6, 1, 1/sc)
            scm <- tcrossprod(sc)

            df.res <- ssd$df

            rss.qr <- qr((T %*% ssd$SSD  %*% t(T)) * scm, tol=tol)
            if(rss.qr$rank < pp)
                stop("residuals have rank ", rss.qr$rank," < ", pp)
            eigs <- array(NA, c(nmodels, pp))
            stats <- matrix(NA, nmodels+1L, 5L)
            colnames(stats) <- c(test, "approx F", "num Df", "den Df",
                                       "Pr(>F)")

            for(i in 1L:nmodels) {
                eigs[i, ] <- Re(eigen(qr.coef(rss.qr,
                                              (T %*% ss[[i]] %*% t(T)) * scm),
                                      symmetric = FALSE)$values)
                stats[i, 1L:4L] <-
                    switch(test,
                           "Pillai" = Pillai(eigs[i,  ],
                           df[i], df.res),
                           "Wilks" = Wilks(eigs[i,  ],
                           df[i], df.res),
                           "Hotelling-Lawley" = HL(eigs[i,  ],
                           df[i], df.res),
                           "Roy" = Roy(eigs[i,  ],
                           df[i], df.res))
                ok <- stats[, 2L] >= 0 & stats[, 3L] > 0 & stats[, 4L] > 0
                ok <- !is.na(ok) & ok
                stats[ok, 5L] <- pf(stats[ok, 2], stats[ok, 3L], stats[ok, 4L],
                                   lower.tail = FALSE)
            }

        }
        table <- data.frame(Df=c(df,ssd$df), stats, check.names=FALSE)
        row.names(table) <- c(tlabels, "Residuals")
#        if(attr(object$terms,"intercept")) table <- table[-1, ]
        structure(table, heading = c(title, transformnote, epsnote),
                  class = c("anova", "data.frame"))

#        f <- ms/(ssr/dfr)
#        P <- pf(f, df, dfr, lower.tail = FALSE)
#        table <- data.frame(df, ss, ms, f, P)
#        table[length(P), 4:5] <- NA
#        dimnames(table) <- list(c(tlabels, "Residuals"),
#                                c("Df","Sum Sq", "Mean Sq", "F value", "Pr(>F)"))
#        if(attr(object$terms,"intercept")) table <- table[-1, ]
#        structure(table, heading = c("Analysis of Variance Table\n",
#                         paste("Response:", deparse(formula(object)[[2L]]))),
#                  class= c("anova", "data.frame"))# was "tabular"
    }
}

Pillai <- function(eig, q, df.res)
{
    test <- sum(eig/(1 + eig))
    p <- length(eig)
    s <- min(p, q)
    n <- 0.5 * (df.res - p - 1)
    m <- 0.5 * (abs(p - q) - 1)
    tmp1 <- 2 * m + s + 1
    tmp2 <- 2 * n + s + 1
    c(test, (tmp2/tmp1 * test)/(s - test), s*tmp1, s*tmp2)
}

Wilks <- function(eig, q, df.res)
{
    test <- prod(1/(1 + eig))
    p <- length(eig)
    tmp1 <- df.res - 0.5 * (p - q + 1)
    tmp2 <- (p * q - 2)/4
    tmp3 <- p^2 + q^2 - 5
    tmp3 <-  if(tmp3 > 0) sqrt(((p*q)^2 - 4)/tmp3) else 1
    c(test, ((test^(-1/tmp3) - 1) * (tmp1 * tmp3 - 2 * tmp2))/p/q,
      p * q, tmp1 * tmp3 - 2 * tmp2)
}

HL <- function(eig, q, df.res)
{
    test <- sum(eig)
    p <- length(eig)
    m <- 0.5 * (abs(p - q) - 1)
    n <- 0.5 * (df.res - p - 1)
    s <- min(p, q)
    tmp1 <- 2 * m + s + 1
    tmp2 <- 2 * (s * n + 1)
    c(test, (tmp2 * test)/s/s/tmp1, s * tmp1, tmp2)
}

Roy <- function(eig, q, df.res)
{
    p <- length(eig)
    test <- max(eig)
    tmp1 <- max(p, q)
    tmp2 <- df.res - tmp1 + q
    c(test, (tmp2 * test)/tmp1, tmp1, tmp2)
}

anova.mlmlist <- function (object, ...,
                           test=c("Pillai", "Wilks",
                           "Hotelling-Lawley", "Roy","Spherical"),
                           Sigma=diag(nrow=p),
                           T = Thin.row(proj(M)-proj(X)),
                           M = diag(nrow=p),
                           X = ~0,
                           idata=data.frame(index=seq_len(p)), tol = 1e-7)
{
    objects <- list(object, ...)
    p <- ncol(SSD(object)$SSD)
    Xmis <- missing(X)
    Mmis <- missing(M)
    if (missing(T)){
        orig.M <- M # keep for printing
        orig.X <- X
        if (class(M) == "formula") M <- model.matrix(M, idata)
        if (class(X) == "formula") X <- model.matrix(X, idata)
        if (Rank(cbind(M,X)) != Rank(M))
            stop("X does not define a subspace of M")
    }
    pp <- nrow(T)
    responses <- as.character(lapply(objects,
				     function(x) deparse(x$terms[[2L]])))
    sameresp <- responses == responses[1L]
    if (!all(sameresp)) {
	objects <- objects[sameresp]
	warning("models with response ",
                deparse(responses[!sameresp]),
                " removed because response differs from ", "model 1")
    }

    ns <- sapply(objects, function(x) length(x$residuals))
    if(any(ns != ns[1L]))
        stop("models were not all fitted to the same size of dataset")

    ## calculate the number of models
    nmodels <- length(objects)
    if (nmodels == 1)
	return(anova.mlm(object))

    ## extract statistics

    resdf  <- as.numeric(lapply(objects, df.residual))
    df <- c(NA,diff(resdf))
    resssd <- lapply(objects, SSD)
    deltassd <- mapply(function(x,y) y$SSD - x$SSD,
                       resssd[-nmodels], resssd[-1L], SIMPLIFY=FALSE)
    resdet <- sapply(resssd,
                     function(x) det(T %*% (x$SSD/x$df) %*% t(T))^(1/pp))


    ## construct table and title

    table <- data.frame(resdf, df, resdet)
    variables <- lapply(objects, function(x)
                        paste(deparse(formula(x)), collapse="\n") )
    dimnames(table) <- list(1L:nmodels,
                            c("Res.Df", "Df", "Gen.var."))

    title <- "Analysis of Variance Table\n"
    topnote <- paste("Model ", format(1L:nmodels),": ",
		     variables, sep="", collapse="\n")
    transformnote <- if (!missing(T))
        c("\nContrast matrix", apply(format(T), 1L, paste, collapse=" "))
    else
        c(
          if (!Xmis)
          c("\nContrasts orthogonal to",
            if (is.matrix(orig.X))  apply(format(X), 2L, paste, collapse=" ")
            else deparse(formula(orig.X)),"",
            if (!Mmis)
            c("\nContrasts spanned by",
              if (is.matrix(orig.M))  apply(format(M), 2L, paste, collapse=" ")
              else deparse(formula(orig.M)),""
              )
            )
          )
    epsnote <- NULL

    ## calculate test statistic

    test <- match.arg(test)
    if(test == "Spherical"){
	bigmodel <- order(resdf)[1L]
        df.res <- resdf[bigmodel]
        sph <- sphericity(resssd[[bigmodel]],T=T,Sigma=Sigma)
        epsnote <- c(paste(format(c("Greenhouse-Geisser epsilon:",
                           "Huynh-Feldt epsilon:")),
                         format(c(sph$GG.eps, sph$HF.eps),digits=4)),
                     "")

        Psi <- T %*% Sigma %*% t(T)
        stats <- matrix(NA, nmodels, 6L)
        dimnames(stats) <-  list(1L:nmodels,
                                 c("F", "num Df", "den Df",
                                   "Pr(>F)", "G-G Pr", "H-F Pr"))
        for(i in 2:nmodels) {
            s2 <- Tr(solve(Psi,T %*% deltassd[[i-1]] %*% t(T)))/pp/df[i]
            Fval <- s2/sph$sigma
            stats[i,1L:3] <- abs(c(Fval, df[i]*pp, df.res*pp))
        }
        stats[,4] <- pf(stats[,1], stats[,2], stats[,3], lower.tail=FALSE)
        stats[,5] <- pf(stats[,1],
                        stats[,2]*sph$GG.eps, stats[,3]*sph$GG.eps,
                        lower.tail=FALSE)
        stats[,6] <- pf(stats[,1],
                        stats[,2]*min(1,sph$HF.eps),
                        stats[,3]*min(1,sph$HF.eps),
                        lower.tail=FALSE)
        table <- cbind(table, stats)
    }
    else if(!is.null(test)) {
	bigmodel <- order(resdf)[1L]
        df.res <- resdf[bigmodel]

        ## Try to distinguish bad scaling and near-perfect fit
        ## Notice that we must transform by T before scaling

        sc <- sqrt(diag(T %*% resssd[[bigmodel]]$SSD %*% t(T)))
        D <- sqrt(sc^2+apply(abs(sapply(deltassd,
                                        function(X) diag((T %*% X %*% t(T))))),
                             1,max))
        sc <- ifelse(sc/D < 1e-6, 1, 1/sc)
        scm <- tcrossprod(sc)



        rss.qr <- qr((T %*% resssd[[bigmodel]]$SSD %*% t(T)) * scm, tol=tol)
        if(rss.qr$rank < pp)
            stop("residuals have rank ", rss.qr$rank," < ", pp)
        eigs <- array(NA, c(nmodels, pp))
        stats <- matrix(NA, nmodels, 5L)
        dimnames(stats) <-  list(1L:nmodels,
                                 c(test, "approx F", "num Df", "den Df",
                                   "Pr(>F)"))

        for(i in 2:nmodels) {
            sg <- (df[i] > 0) -  (df[i] < 0)
            eigs[i, ] <- Re(eigen(qr.coef(rss.qr,
                                          sg * (T %*% deltassd[[i-1]] %*%
                                          t(T)) * scm),
                                  symmetric = FALSE)$values)
            stats[i, 1L:4] <-
                switch(test,
                       "Pillai" = Pillai(eigs[i,  ],
                       sg * df[i], resdf[bigmodel]),
                       "Wilks" = Wilks(eigs[i,  ],
                       sg * df[i], resdf[bigmodel]),
                       "Hotelling-Lawley" = HL(eigs[i,  ],
                       sg * df[i], resdf[bigmodel]),
                       "Roy" = Roy(eigs[i,  ],
                       sg * df[i], resdf[bigmodel]))
            ok <- stats[, 2] >= 0 & stats[, 3] > 0 & stats[, 4] > 0
            ok <- !is.na(ok) & ok
            stats[ok, 5] <- pf(stats[ok, 2], stats[ok, 3], stats[ok, 4],
                               lower.tail = FALSE)

        }
        table <- cbind(table,stats)

    }
    structure(table, heading = c(title, topnote, transformnote, epsnote),
              class = c("anova", "data.frame"))
}


deviance.mlm <- function(object, ...)
{
    res <-
	if(is.null(w <- object$weights)) object$residuals^2
	else w * object$residuals^2
    drop(rep.int(1, nrow(res)) %*% res)
}

plot.mlm <- function (x, ...) .NotYetImplemented()
#  File src/library/stats/R/model.tables.R
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

model.tables <- function(x, ...) UseMethod("model.tables")

model.tables.aov <- function(x, type = "effects", se = FALSE, cterms, ...)
{
    if(inherits(x, "maov"))
	stop("'model.tables' is not implemented for multiple responses")
    type <- match.arg(type, c("effects", "means", "residuals"))
    if(type == "residuals")
	stop(gettextf("type '%s' is not implemented yet", type), domain = NA)
    prjs <- proj(x, unweighted.scale = TRUE)
    if(is.null(x$call)) stop("this fit does not inherit from \"lm\"")
    mf <- model.frame(x)
    factors <- attr(prjs, "factors")
    dn.proj <- as.list(names(factors))
    m.factors <- factors
    names(m.factors) <- names(dn.proj) <- names(factors)
    t.factor <- attr(prjs, "t.factor")
    vars <- colnames(t.factor)
    which <- match(vars, names(dn.proj))
    which <- which[!is.na(which)]
    dn.proj <- dn.proj[which]
    m.factors <- m.factors[which]
    ## with cterms, can specify subset of tables by name
    if(!missing(cterms)) {
	if(any(is.na(match(cterms, names(factors)))))
	    stop("'cterms' argument must match terms in model object")
	dn.proj <- dn.proj[cterms]
	m.factors <- m.factors[cterms]
    }
    if(type == "means") {
	dn.proj <-
	    lapply(dn.proj,
		   function(x, mat, vn)
		   c("(Intercept)",
		     vn[(t(mat) %*% (as.logical(mat[, x]) - 1)) == 0]),
		   t.factor, vars)
    }
    tables <- make.tables.aovproj(dn.proj, m.factors, prjs, mf)

    ## This was reordering some interaction terms, e.g. N + V:N
    ##n <- replications(paste("~", paste(names(tables), collapse = "+")),
    ##		      data = mf)
    n <- NULL
    for(xx in names(tables)) n <- c(n, replications(paste("~", xx), data=mf))
    if(se)
	if(is.list(n)) {
	    message("Design is unbalanced - use se.contrast() for se's")
	    se <- FALSE
	} else se.tables <- se.aov(x, n, type = type)
    if(type == "means") {
	gmtable <- mean(prjs[,"(Intercept)"])
	class(gmtable) <- "mtable"
	tables <- c("Grand mean" = gmtable, tables)
    }
    result <- list(tables = tables, n = n)
    if(se) result$se <- se.tables
    attr(result, "type") <- type
    class(result) <- c("tables_aov", "list.of")
    result
}

se.aov <- function(object, n, type = "means")
{
    ## for balanced designs only
    rdf <- object$df.residual
    rse <- sqrt(sum(object$residuals^2)/rdf)
    if(type == "effects") result <- rse/sqrt(n)
    if(type == "means")
	result <-
	    lapply(n,
		   function(x, d) {
		       nn <- unique(x)
		       nn <- nn[!is.na(nn)]
		       mat <- outer(nn, nn, function(x, y) 1/x + 1/y)
		       dimnames(mat) <- list(paste(nn), paste(nn))
		       d * sqrt(mat)
		   }, d=rse)
    attr(result, "type") <- type
    class(result) <- "mtable"
    result
}


model.tables.aovlist <- function(x, type = "effects", se = FALSE, ...)
{
    type <- match.arg(type, c("effects", "means", "residuals"))
    if(type == "residuals")
	stop(gettextf("type '%s' is not implemented yet", type), domain = NA)
    prjs <- proj(x, unweighted.scale = TRUE)
    mf <- model.frame.aovlist(x)
    factors <- lapply(prjs, attr, "factors")
    dn.proj <- unlist(lapply(factors, names), recursive = FALSE)
    m.factors <- unlist(factors, recursive = FALSE)
    dn.strata <- rep.int(names(factors), unlist(lapply(factors, length)))
    names(dn.strata) <- names(m.factors) <- names(dn.proj) <- unlist(dn.proj)
    t.factor <- attr(prjs, "t.factor")
    efficiency <- FALSE
    if(type == "effects" || type == "means") {
	if(anyDuplicated(names(dn.proj)[names(dn.proj) != "Residuals"])) {
	    efficiency <- eff.aovlist(x)
	    ## Elect to use the effects from the lowest stratum:
	    ##	usually expect this to be highest efficiency
	    eff.used <- apply(efficiency, 2L,
			      function(x, ind = seq_len(x)) {
				  temp <- (x > 0)
				  if(sum(temp) == 1) temp
				  else max(ind[temp]) == ind
			      })
	}
    }
    if(any(efficiency)) {
        if(is.list(eff.used))
            stop("design is unbalanced so cannot proceed")
	which <- match(outer(rownames(efficiency),
			     colnames(efficiency), paste)[eff.used],
		       paste(dn.strata, dn.proj))
	efficiency <- efficiency[eff.used]
    } else  which <- match(colnames(t.factor), names(dn.proj))
    which <- which[!is.na(which)]
    dn.proj <- dn.proj[which]
    dn.strata <- dn.strata[which]
    m.factors <- m.factors[which]
    if(type == "means")	 {
	t.factor <- t.factor[, names(dn.proj), drop = FALSE]
	dn.proj <-
	    lapply(dn.proj,
		   function(x, mat, vn)
		   vn[(t(mat) %*% (as.logical(mat[, x]) - 1)) == 0],
		   t.factor, colnames(t.factor))
    }
    tables <-
	if(any(efficiency)) {
	    names(efficiency) <- names(dn.proj)
	    make.tables.aovprojlist(dn.proj, dn.strata, m.factors, prjs, mf,
				    efficiency)
	}
	else make.tables.aovprojlist(dn.proj, dn.strata, m.factors, prjs, mf)
    if(type == "means") {
	gmtable <- mean(prjs[["(Intercept)"]])
	class(gmtable) <- "mtable"
	tables <- lapply(tables, "+", gmtable)
	tables <- c("Grand mean" = gmtable, tables)
    }
    n <- replications(attr(x, "call"), data = mf)
    if(se)
	if(type == "effects"  && is.list(n)) {
	    message("Standard error information not returned as design is unbalanced. \nStandard errors can be obtained through 'se.contrast'.")
	    se <- FALSE
	} else if(type != "effects") {
	    warning(gettextf("SEs for type '%s' are not yet implemented",
                             type), domain = NA)
	    se <- FALSE
	} else {
	    se.tables <- se.aovlist(x, dn.proj, dn.strata, factors, mf,
				    efficiency, n, type = type)
	}
    result <- list(tables = tables, n = n)
    if(se) result$se <- se.tables
    attr(result, "type") <- type
    class(result) <- c("tables_aov", "list.of")
    result
}

se.aovlist <- function(object, dn.proj, dn.strata, factors, mf, efficiency, n,
		       type = "diff.means", ...)
{
    if(type != "effects")
	stop(gettextf("SEs for type '%s' are not yet implemented", type),
             domain = NA)
    RSS <- sapply(object, function(x) sum(x$residuals^2)/x$df.residual)
    res <- vector(length = length(n), mode = "list")
    names(res) <- names(n)
    for(i in names(n)) {
	sse <- RSS[[dn.strata[dn.proj[[i]]]]]
	if(any(efficiency))
	    sse <- sse/efficiency[i]
	res[[i]] <- as.vector(sqrt(sse/n[i]))
	class(res[[i]]) <- "mtable"
    }
    attr(res, "type") <- type
    res
}


make.tables.aovproj <-
    function(proj.cols, mf.cols, prjs, mf, fun = "mean", prt = FALSE, ...)
{
    tables <- vector(mode = "list", length = length(proj.cols))
    names(tables) <- names(proj.cols)
    for(i in seq_along(tables)) {
	terms <- proj.cols[[i]]
        terms <- terms[terms %in% colnames(prjs)]
	data <-
	    if(length(terms) == 1L) prjs[, terms]
	    else prjs[, terms] %*% as.matrix(rep.int(1, length(terms)))
	tables[[i]] <- tapply(data, mf[mf.cols[[i]]],
                              get(fun, mode="function"))
	class(tables[[i]]) <- "mtable"
	if(prt) print(tables[i], ..., quote = FALSE)
    }
    tables
}


make.tables.aovprojlist <-
    function(proj.cols, strata.cols, model.cols, projections, model, eff,
	     fun = "mean", prt = FALSE, ...)
{
    tables <- vector(mode = "list", length = length(proj.cols))
    names(tables) <- names(proj.cols)
    if(!missing(eff)) {
	for(i in seq_along(tables)) {
	    terms <- proj.cols[[i]]
	    if(all(is.na(eff.i <- match(terms, names(eff)))))
		eff.i <- rep.int(1, length(terms))
	    if(length(terms) == 1L)
		data <- projections[[strata.cols[i]]][, terms]/ eff[eff.i]
	    else {
		if(length(strata <- unique(strata.cols[terms])) == 1)
		    data <- projections[[strata]][, terms] %*%
			as.matrix(1/eff[eff.i])
		else {
		    mat <- NULL
		    for(j in strata) {
			mat <- cbind(mat, projections[[j]][, terms[!is.na(match(terms,
										names(strata.cols)[strata.cols == j]))]])
		    }
		    data <- mat %*% as.matrix(1/eff[eff.i])
		}
	    }
	    tables[[i]] <- tapply(data, model[model.cols[[i]]],
                                  get(fun, mode="function"))
	    attr(tables[[i]], "strata") <- strata.cols[i]
	    class(tables[[i]]) <- "mtable"
	    if(prt) print(tables[i], ..., quote = FALSE)
	}
    } else for(i in seq_along(tables)) {
	terms <- proj.cols[[i]]
	if(length(terms) == 1L) data <- projections[[strata.cols[i]]][, terms]
	else {
	    if(length(strata <- unique(strata.cols[terms])) == 1L)
		data <- projections[[strata]][, terms] %*%
		    as.matrix(rep.int(1, length(terms)))
	    else {
		mat <- NULL
		for(j in strata) {
		    mat <- cbind(mat, projections[[j]][, terms[!is.na(match(terms,
									    names(strata.cols)[strata.cols == j]))]])
		}
		data <- mat %*% as.matrix(rep.int(1, length(terms)))
	    }
	}
	tables[[i]] <- tapply(data, model[model.cols[[i]]], get(fun))
	attr(tables[[i]], "strata") <- strata.cols[i]
	class(tables[[i]]) <- "mtable"
	if(prt) print(tables[i], ..., quote = FALSE)
    }
    tables
}

replications <- function(formula, data = NULL, na.action)
{
    if(missing(data) && inherits(formula, "data.frame")) {
	data <- formula
	formula <-  ~ .
    }
    if(!inherits(formula, "terms")) {
	formula <- as.formula(formula)
	if(length(formula) < 3L) {
	    f <- y ~ x
	    f[[3L]] <- formula[[2L]]
	    formula <- f
	}
	formula <- terms(formula, data = data)
    }
    if(missing(na.action))
        if(!is.null(tj <- attr(data, "na.action")) && is.function(tj))
            na.action <- tj
        else {
            naa <- getOption("na.action")
            if(!is.null(naa)) na.action <- match.fun(naa)
            else  na.action <- na.fail
        }
    f <- attr(formula, "factors")
    o <- attr(formula, "order")
    labels <- attr(formula, "term.labels")
    vars <- as.character(attr(formula, "variables"))[-1L]
    if(is.null(data)) {
	v <- c(as.name("data.frame"), attr(formula, "variables"))
	data <- eval(as.call(v), parent.frame())
    }
    if(!is.function(na.action)) stop("na.action must be a function")
    data <- na.action(data)
    class(data) <- NULL
    n <- length(o)
    z <- vector("list", n)
    names(z) <- labels
    dummy <- numeric(.row_names_info(data, 2L))
    notfactor <- !sapply(data, function(x) inherits(x, "factor"))
    balance <- TRUE
    for(i in seq_len(n)) {
	l <- labels[i]
	if(o[i] < 1 || substring(l, 1L, 5L) == "Error") { z[[l]] <- NULL; next }
	select <- vars[f[, i] > 0]
	if(any(nn <- notfactor[select])) {
	    warning("non-factors ignored: ",
                    paste(names(nn), collapse = ", "))
	    next
	}
	if(length(select))
	    tble <- tapply(dummy, unclass(data[select]), length)
	nrep <- unique(as.vector(tble))
	if(length(nrep) > 1L) {
	    balance <- FALSE
	    tble[is.na(tble)] <- 0
	    z[[l]] <- tble
	} else z[[l]] <- as.vector(nrep)
    }
    if(balance) unlist(z) else z
}

print.tables_aov <- function(x, digits = 4, ...)
{
    tables.aov <- x$tables
    n.aov <- x$n
    se.aov <- if(se <- !is.na(match("se", names(x)))) x$se
    type <- attr(x, "type")
    switch(type,
	   effects = cat("Tables of effects\n"),
	   means = cat("Tables of means\n"),
	   residuals = if(length(tables.aov) > 1L) cat(
	   "Table of residuals from each stratum\n"))
    if(!is.na(ii <- match("Grand mean", names(tables.aov)))) {
	cat("Grand mean\n")
	gmtable <- tables.aov[[ii]]
	print.mtable(gmtable, digits = digits, ...)
    }
    for(i in names(tables.aov)) {
	if(i == "Grand mean") next
	table <- tables.aov[[i]]
	cat("\n", i, "\n")
	if(!is.list(n.aov))
	    print.mtable(table, digits = digits, ...)
	else {
	    n <- n.aov[[i]]
	    if(length(dim(table)) < 2L) {
		table <- rbind(table, n)
		rownames(table) <- c("", "rep")
		print(table, digits = digits, ...)
	    } else {
		ctable <- array(c(table, n), dim = c(dim(table), 2))
		dim.t <- dim(ctable)
		d <- length(dim.t)
		ctable <- aperm(ctable, c(1, d, 2:(d - 1)))
		dim(ctable) <- c(dim.t[1L] * dim.t[d], dim.t[-c(1, d)])
		dimnames(ctable) <-
		    c(list(format(c(rownames(table), rep.int("rep", dim.t[1L])))),
                      dimnames(table)[-1L])
		ctable <- eval(parse(text = paste(
				     "ctable[as.numeric(t(matrix(seq(nrow(ctable)),ncol=2)))", paste(rep.int(", ", d - 2), collapse = " "), "]")))
		names(dimnames(ctable)) <- names(dimnames(table))
		class(ctable) <- "mtable"
		print.mtable(ctable, digits = digits, ...)
	    }
	}
    }
    if(se) {
	if(type == "residuals") rn <- "df" else rn <- "replic."
	switch(attr(se.aov, "type"),
	       effects = cat("\nStandard errors of effects\n"),
	       means = cat("\nStandard errors for differences of means\n"),
	       residuals = cat("\nStandard errors of residuals\n"))
	if(length(unlist(se.aov)) == length(se.aov)) {
	    ## the simplest case: single replication, unique se
					# kludge for NA's
	    n.aov <- n.aov[!is.na(n.aov)]
	    se.aov <- unlist(se.aov)
	    cn <- names(se.aov)
	    se.aov <- rbind(format(se.aov, digits = digits), format(n.aov))
	    dimnames(se.aov) <- list(c(" ", rn), cn)
	    print(se.aov, quote=FALSE, right=TRUE, ...)
	} else for(i in names(se.aov)) {
	    se <- se.aov[[i]]
	    if(length(se) == 1L) { ## single se
		se <- rbind(se, n.aov[i])
		dimnames(se) <- list(c(i, rn), "")
		print(se, digits = digits, ...)
	    } else {		## different se
		dimnames(se)[[1L]] <- ""
		cat("\n", i, "\n")
		cat("When comparing means with same levels of:\n")
		print(se, digits, ...)
		cat("replic.", n.aov[i], "\n")
	    }
	}
    }
    invisible(x)
}

eff.aovlist <- function(aovlist)
{
    Terms <- terms(aovlist)
    if(names(aovlist)[[1L]] == "(Intercept)") aovlist <- aovlist[-1L]
    pure.error.strata <- sapply(aovlist, function(x) is.null(x$qr))
    aovlist <- aovlist[!pure.error.strata]
    s.labs <- names(aovlist)
    ## find which terms are in which strata
    s.terms <-
        lapply(aovlist, function(x) {
            asgn <- x$assign[x$qr$pivot[1L:x$rank]]
            attr(terms(x), "term.labels")[asgn]
        })
    t.labs <- attr(Terms, "term.labels")
    t.labs <- t.labs[t.labs %in% unlist(s.terms)]
    eff <- matrix(0, ncol = length(t.labs), nrow = length(s.labs),
		  dimnames = list(s.labs, t.labs))
    for(i in names(s.terms)) eff[i, s.terms[[i]] ] <- 1
    cs <- colSums(eff)
    ## if all terms are in just one stratum we are done
    if(all(cs <= 1)) return(eff[, cs > 0, drop = FALSE])

    nm <- t.labs[ cs > 1]
    pl <-
	lapply(aovlist, function(x)
	   {
	       asgn <- x$assign[x$qr$pivot[1L:x$rank]]
	       sp <- split(seq_along(asgn), attr(terms(x), "term.labels")[asgn])
               sp <- sp[names(sp) %in% nm]
	       sapply(sp, function(x, y) {
                   y <- y[x, x, drop = FALSE]
                   res <- sum(diag(y)^2)
                   if(nrow(y) > 1 && sum(y^2) > 1.01 * res)
                       stop("eff.aovlist: non-orthogonal contrasts would give an incorrect answer")
                   res
               }, y=x$qr$qr)
	   })
    for(i in names(pl)) eff[i, names(pl[[i]]) ] <- pl[[i]]
    cs <- colSums(eff)
    eff <- eff/rep(cs, each = nrow(eff))
    eff[, cs != 0, drop = FALSE]
}


model.frame.aovlist <- function(formula, data = NULL, ...)
{
    ## formula is an aovlist object
    call <- match.call()
    oc <- attr(formula, "call")
    Terms <- attr(formula, "terms")
    rm(formula)
    indError <- attr(Terms, "specials")$Error
    errorterm <-  attr(Terms, "variables")[[1 + indError]]
    form <- update.formula(Terms,
                           paste(". ~ .-", deparse(errorterm, width.cutoff=500L,
                                                   backtick = TRUE),
                                 "+", deparse(errorterm[[2L]], width.cutoff=500L,
                                              backtick = TRUE)))
    nargs <- as.list(call)
    oargs <- as.list(oc)
    nargs <- nargs[match(c("data", "na.action", "subset"), names(nargs), 0)]
    args <- oargs[match(c("data", "na.action", "subset"), names(oargs), 0)]
    args[names(nargs)] <- nargs
    args$formula <- form
    env <- environment(Terms)
    if (is.null(env)) env <- parent.frame()
    fcall <- c(list(as.name("model.frame")), args)
#    do.call("model.frame", args)
    eval(as.call(fcall), env)
}

print.mtable <-
    function(x, ..., digits = getOption("digits"), quote = FALSE, right = FALSE)
{
    xxx <- x
    xx <- attr(x, "Notes")
#    nn <- names(dimnames(x))
    a.ind <- match(names(a <- attributes(x)), c("dim", "dimnames", "names"))
    a <- a[!is.na(a.ind)]
    class(x) <- attributes(x) <- NULL
    attributes(x) <- a
#    if(length(nn) > 1L)
#	cat(paste("Dim ",paste(seq(length(nn)), "=", nn, collapse= ", "),"\n"))
    if(length(x) == 1 && is.null(names(x)) && is.null(dimnames(x)))
	names(x) <- rep("", length(x))
    if(length(dim(x)) && is.numeric(x)) {
	xna <- is.na(x)
	x <- format(zapsmall(x, digits))
	x[xna] <- "  "
    }
    print(x, quote = quote, right = right, ...)
    if(length(xx)) {
	cat("\nNotes:\n")
	print(xx)
    }
    invisible(xxx)
}


#  File src/library/stats/R/models.R
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

formula <- function(x, ...) UseMethod("formula")
formula.default <- function (x, env = parent.frame(), ...)
{
    notAtomic <- !is.atomic(x)
    notnull <- function(z) notAtomic && !is.null(z)

    if (notnull(x$formula)) eval(x$formula)
    else if (notnull(x$terms)) {z <- x$terms; oldClass(z) <- "formula"; z}
    else if (notnull(x$call$formula))	eval(x$call$formula)
    else if (!is.null(attr(x, "formula"))) attr(x, "formula")
    else {
        form <- switch(mode(x),
                       NULL = structure(NULL, class = "formula"),
                       character = formula(eval(parse(text = x)[[1L]])),
                       call = eval(x), stop("invalid formula"))
        environment(form) <- env
        form
    }
}
formula.formula <- function(x, ...) x
formula.terms <- function(x, ...) {
    env<- environment(x)
    attributes(x) <- list(class="formula")
    if (!is.null(env))
    	environment(x) <- env
    else
    	environment(x) <- globalenv()
    x
}

formula.data.frame <- function (x, ...)
{
    nm <- sapply(names(x), as.name)
    if (length(nm) > 1L) {
        rhs <- nm[-1L]
        lhs <- nm[1L]
    } else if (length(nm) == 1L) {
        rhs <- nm[1L]
        lhs <- NULL
    } else stop("cannot create a formula from a zero-column data frame")
    ff <- parse(text = paste(lhs, paste(rhs, collapse = "+"), sep = "~"))
    ff <- eval(ff)
    environment(ff) <- parent.frame()
    ff
}

formula.character <- function(x, env = parent.frame(), ...)
{
    ff <- formula(eval(parse(text=x)[[1L]]))
    environment(ff) <- env
    ff
}

print.formula <- function(x, ...) {
    attr(x, ".Environment") <- NULL
    print.default(unclass(x), ...)
    invisible(x)
}

"[.formula" <- function(x,i) {
    ans <- NextMethod("[")
    ## as.character gives a vector.
    if(as.character(ans[[1L]])[1L] == "~") {
	class(ans) <- "formula"
        environment(ans)<-environment(x)
    }
    ans
}

as.formula <- function(object, env = parent.frame())
{
    if(inherits(object, "formula"))
        object
    else {
        rval<-formula(object,env=baseenv())
        if (identical(environment(rval), baseenv()) || !missing(env))
            environment(rval)<-env
        rval
    }
}

terms <- function(x, ...) UseMethod("terms")
terms.default <- function(x, ...) {
    v <- x$terms
    if(is.null(v))
        stop("no terms component")
    v
}

terms.terms <- function(x, ...) x
print.terms <- function(x, ...) {
    print.default(unclass(x))
    invisible(x)
}

## moved from base/R/labels.R
labels.terms <- function(object, ...) attr(object, "term.labels")

### do this `by hand' as previous approach was vulnerable to re-ordering.
delete.response <- function (termobj)
{
    a <- attributes(termobj)
    y <- a$response
    if(!is.null(y) && y) {
        termobj[[2L]] <- NULL
        a$response <- 0
        a$variables <- a$variables[-(1+y)]
        a$predvars <- a$predvars[-(1+y)]
        if(length(a$factors))
            a$factors <- a$factors[-y, , drop = FALSE]
        if(length(a$offset))
            a$offset <- ifelse(a$offset > y, a$offset-1, a$offset)
        if(length(a$specials))
            for(i in seq_along(a$specials)) {
                b <- a$specials[[i]]
                a$specials[[i]] <- ifelse(b > y, b-1, b)
            }
        attributes(termobj) <- a
    }
    termobj
}

reformulate <- function (termlabels, response=NULL)
{
    if(!is.character(termlabels) || !length(termlabels))
        stop("'termlabels' must be a character vector of length at least one")
    has.resp <- !is.null(response)
    termtext <- paste(if(has.resp) "response", "~",
		      paste(termlabels, collapse = "+"),
		      collapse = "")
    rval <- eval(parse(text = termtext)[[1L]])
    if(has.resp) rval[[2L]] <-
        if(is.character(response)) as.symbol(response) else response
    ## response can be a symbol or call as  Surv(ftime, case)
    environment(rval) <- parent.frame()
    rval
}

drop.terms <- function(termobj, dropx = NULL, keep.response = FALSE)
{
    if (is.null(dropx))
	termobj
    else {
        if(!inherits(termobj, "terms"))
            stop("'termobj' must be a object of class \"terms\"")
	newformula <- reformulate(attr(termobj, "term.labels")[-dropx],
				  if (keep.response) termobj[[2L]] else NULL)
        environment(newformula) <- environment(termobj)
	terms(newformula, specials=names(attr(termobj, "specials")))
    }
}


"[.terms" <-function (termobj, i)
{
    resp <- if (attr(termobj, "response")) termobj[[2L]] else NULL
    newformula <- attr(termobj, "term.labels")[i]
    if (length(newformula) == 0L) newformula <- "1"
    newformula <- reformulate(newformula, resp)
    environment(newformula)<-environment(termobj)
    terms(newformula, specials = names(attr(termobj, "specials")))
}


terms.formula <- function(x, specials = NULL, abb = NULL, data = NULL,
			  neg.out = TRUE, keep.order = FALSE,
                          simplify = FALSE, ..., allowDotAsName = FALSE)
{
    fixFormulaObject <- function(object) {
        Terms <- terms(object)
	tmp <- attr(Terms, "term.labels")
        ## fix up terms involving | : PR#8462
        ind <- grep("|", tmp, fixed = TRUE)
        if(length(ind)) tmp[ind] <- paste("(", tmp[ind], ")")
        ## need to add back any offsets
        if(length(ind <- attr(Terms, "offset"))) {
            ## can't look at rownames of factors, as not there y ~ offset(x)
            tmp2 <- as.character(attr(Terms, "variables"))[-1L]
            tmp <- c(tmp, tmp2[ind])
        }
	rhs <- if(length(tmp)) paste(tmp, collapse = " + ") else "1"
	if(!attr(terms(object), "intercept")) rhs <- paste(rhs, "- 1")
        if(length(form <- formula(object)) > 2L) {
            res <- formula(paste("lhs ~", rhs))
            res[[2L]] <- form[[2L]]
            res
        } else formula(paste("~", rhs))
    }

    if (!is.null(data) && !is.environment(data) && !is.data.frame(data))
	data <- as.data.frame(data, optional=TRUE)
    terms <- .Internal(terms.formula(x, specials, data, keep.order,
                                     allowDotAsName))
    if (simplify) {
        a <- attributes(terms)
        terms <- fixFormulaObject(terms)
        attributes(terms) <- a
    }
    environment(terms) <- environment(x)
    if(!inherits(terms, "formula"))
        class(terms) <- c(oldClass(terms), "formula")
    terms
}

coef <- function(object, ...) UseMethod("coef")
coef.default <- function(object, ...) object$coefficients
coefficients <- coef

residuals <- function(object, ...) UseMethod("residuals")
residuals.default <- function(object, ...)
    naresid(object$na.action, object$residuals)
resid <- residuals

deviance <- function(object, ...) UseMethod("deviance")
deviance.default <- function(object, ...) object$deviance

fitted <- function(object, ...) UseMethod("fitted")
## we really do need partial matching here
fitted.default <- function(object, ...)
{
    xx <- if("fitted.values" %in% names(object))
        object$fitted.values else object$fitted
    napredict(object$na.action, xx)
}
fitted.values <- fitted

anova <- function(object, ...)UseMethod("anova")

effects <- function(object, ...)UseMethod("effects")

weights <- function(object, ...)UseMethod("weights")

df.residual <- function(object, ...)UseMethod("df.residual")
df.residual.default <- function(object, ...) object$df.residual

variable.names <- function(object, ...) UseMethod("variable.names")
variable.names.default <- function(object, ...) colnames(object)

case.names <- function(object, ...) UseMethod("case.names")
case.names.default <- function(object, ...) rownames(object)

simulate <- function(object, nsim = 1, seed = NULL, ...) UseMethod("simulate")

offset <- function(object) object
## ?

.checkMFClasses <- function(cl, m, ordNotOK = FALSE)
{
    ## when called from predict.nls, vars not match.
    new <- sapply(m, .MFclass)
    new <- new[names(new) %in% names(cl)]
     if(length(new) == 0L) return()
    old <- cl[names(new)]
    if(!ordNotOK) {
        old[old == "ordered"] <- "factor"
        new[new == "ordered"] <- "factor"
    }
    ## ordered is OK as a substitute for factor, but not v.v.
    new[new == "ordered" && old == "factor"] <- "factor"
    if(!identical(old, new)) {
        wrong <- old != new
        if(sum(wrong) == 1)
            stop(gettextf(
    "variable '%s' was fitted with type \"%s\" but type \"%s\" was supplied",
                          names(old)[wrong], old[wrong], new[wrong]),
                 call. = FALSE, domain = NA)
        else
            stop(gettextf(
    "variables %s were specified with different types from the fit",
                 paste(sQuote(names(old)[wrong]), collapse=", ")),
                 call. = FALSE, domain = NA)
    }
}

.MFclass <- function(x)
{
    ## the idea is to identify the relevant classes that model.matrix
    ## will handle differently
    ## logical, factor, ordered vs numeric, and other for future proofing
    if(is.logical(x)) return("logical")
    if(is.ordered(x)) return("ordered")
    if(is.factor(x))  return("factor")
    if(is.matrix(x) && is.numeric(x))
        return(paste("nmatrix", ncol(x), sep="."))
    ## this is unclear.  Prior to 2.6.0 we assumed numeric with attributes
    ## meant something, but at least for now model.matrix does not
    ## treat it differently.
##    if(is.vector(x) && is.numeric(x)) return("numeric")
    if(is.numeric(x)) return("numeric")
    return("other")
}

model.frame <- function(formula, ...) UseMethod("model.frame")
model.frame.default <-
    function(formula, data = NULL, subset = NULL, na.action = na.fail,
	     drop.unused.levels = FALSE, xlev = NULL,...)
{
    ## first off, establish if we were passed a data frame 'newdata'
    ## and note the number of rows.
    possible_newdata <-
        !missing(data) && is.data.frame(data) &&
        identical(deparse(substitute(data)), "newdata") &&
        (nr <- nrow(data)) > 0

    ## were we passed just a fitted model object?
    ## the fit might have a saved model object
    if(!missing(formula) && nargs() == 1 && is.list(formula)
       && !is.null(m <- formula$model)) return(m)
    ## if not use the saved call (if there is one).
    if(!missing(formula) && nargs() == 1 && is.list(formula)
       && all(c("terms", "call") %in% names(formula))) {
        fcall <- formula$call
        m <- match(c("formula", "data", "subset", "weights", "na.action"),
                   names(fcall), 0)
        fcall <- fcall[c(1, m)]
        fcall[[1L]] <- as.name("model.frame")
        env <- environment(formula$terms)
	if (is.null(env)) env <- parent.frame()
        return(eval(fcall, env, parent.frame()))
    }
    if(missing(formula)) {
	if(!missing(data) && inherits(data, "data.frame") &&
	   length(attr(data, "terms")))
	    return(data)
	formula <- as.formula(data)
    }
    else if(missing(data) && inherits(formula, "data.frame")) {
	if(length(attr(formula, "terms")))
	    return(formula)
	data <- formula
	formula <- as.formula(data)
    }
    formula <- as.formula(formula)
    if(missing(na.action)) {
	if(!is.null(naa <- attr(data, "na.action")) & mode(naa)!="numeric")
	    na.action <- naa
	else if(!is.null(naa <- getOption("na.action")))
	    na.action <- naa
    }
    if(missing(data))
	data <- environment(formula)
    else if (!is.data.frame(data) && !is.environment(data)
             && !is.null(attr(data, "class")))
        data <- as.data.frame(data)
    else if (is.array(data))
        stop("'data' must be a data.frame, not a matrix or an array")
    if(!inherits(formula, "terms"))
	formula <- terms(formula, data = data)
    env <- environment(formula)
    rownames <- .row_names_info(data, 0L) #attr(data, "row.names")
    vars <- attr(formula, "variables")
    predvars <- attr(formula, "predvars")
    if(is.null(predvars)) predvars <- vars
    varnames <- sapply(vars, deparse, width.cutoff=500)[-1L]
    variables <- eval(predvars, data, env)
    if(is.null(rownames) && (resp <- attr(formula, "response")) > 0L) {
        ## see if we can get rownames from the response
        lhs <- variables[[resp]]
        rownames <- if(is.matrix(lhs)) rownames(lhs) else names(lhs)
    }
    if(possible_newdata && length(variables)) {
        ## need to do this before subsetting and na.action
        nr2 <- max(sapply(variables, NROW))
        if(nr2 != nr)
            warning(gettextf(
                    "'newdata' had %d rows but variable(s) found have %d rows",
                             nr, nr2), call.=FALSE)
    }
    if(is.null(attr(formula, "predvars"))) {
        for (i in seq_along(varnames))
            predvars[[i+1]] <- makepredictcall(variables[[i]], vars[[i+1]])
        attr(formula, "predvars") <- predvars
    }
    extras <- substitute(list(...))
    extranames <- names(extras[-1L])
    extras <- eval(extras, data, env)
    subset <- eval(substitute(subset), data, env)
    data <- .Internal(model.frame(formula, rownames, variables, varnames,
				  extras, extranames, subset, na.action))
    ## fix up the levels
    if(length(xlev)) {
	for(nm in names(xlev))
	    if(!is.null(xl <- xlev[[nm]])) {
		xi <- data[[nm]]
                if(is.character(xi)) {
                    xi <- as.factor(xi)
		    warning(gettextf("character variable '%s' changed to a factor", nm),
                            domain = NA)
                }
		if(!is.factor(xi) || is.null(nxl <- levels(xi)))
		    warning(gettextf("variable '%s' is not a factor", nm),
                            domain = NA)
		else {
		    xi <- xi[, drop = TRUE] # drop unused levels
                    nxl <- levels(xi)
		    if(any(m <- is.na(match(nxl, xl))))
			stop(gettextf("factor '%s' has new level(s) %s",
                                      nm, paste(nxl[m], collapse=", ")),
                             domain = NA)
		    data[[nm]] <- factor(xi, levels=xl)
		}
	    }
    } else if(drop.unused.levels) {
	for(nm in names(data)) {
	    x <- data[[nm]]
	    if(is.factor(x) &&
	       length(unique(x)) < length(levels(x)))
		data[[nm]] <- data[[nm]][, drop = TRUE]
	}
    }
    attr(formula, "dataClasses") <- sapply(data, .MFclass)
    attr(data, "terms") <- formula
    data
}

## we don't assume weights are numeric or a vector, leaving this to the
## calling application
model.weights <- function(x) x$"(weights)"

## we do check that offsets are numeric.
model.offset <- function(x) {
    offsets <- attr(attr(x, "terms"),"offset")
    if(length(offsets)) {
	ans <- x$"(offset)"
        if (is.null(ans)) ans <- 0
	for(i in offsets) ans <- ans+x[[i]]
	ans
    }
    else ans <- x$"(offset)"
    if(!is.null(ans) && !is.numeric(ans)) stop("'offset' must be numeric")
    ans
}

model.matrix <- function(object, ...) UseMethod("model.matrix")

model.matrix.default <- function(object, data = environment(object),
				 contrasts.arg = NULL, xlev = NULL, ...)
{
    t <- if(missing(data)) terms(object) else terms(object, data=data)
    if (is.null(attr(data, "terms")))
	data <- model.frame(object, data, xlev=xlev)
    else {
	reorder <- match(sapply(attr(t,"variables"),deparse,
                                width.cutoff=500)[-1L],
                         names(data))
	if (any(is.na(reorder)))
	    stop("model frame and formula mismatch in model.matrix()")
	if(!identical(reorder, seq_len(ncol(data))))
	    data <- data[,reorder, drop=FALSE]
    }
    int <- attr(t, "response")
    if(length(data)) {      # otherwise no rhs terms, so skip all this
        contr.funs <- as.character(getOption("contrasts"))
        namD <- names(data)
        ## turn any character columns into factors
        for(i in namD)
            if(is.character(data[[i]])) {
                data[[i]] <- factor(data[[i]])
                warning(gettextf("variable '%s' converted to a factor", i),
                        domain = NA)
            }
        isF <- sapply(data, function(x) is.factor(x) || is.logical(x) )
        isF[int] <- FALSE
        isOF <- sapply(data, is.ordered)
        for(nn in namD[isF])            # drop response
            if(is.null(attr(data[[nn]], "contrasts")))
                contrasts(data[[nn]]) <- contr.funs[1 + isOF[nn]]
        ## it might be safer to have numerical contrasts:
        ##	  get(contr.funs[1 + isOF[nn]])(nlevels(data[[nn]]))
        if (!is.null(contrasts.arg) && is.list(contrasts.arg)) {
            if (is.null(namC <- names(contrasts.arg)))
                stop("invalid 'contrasts.arg' argument")
            for (nn in namC) {
                if (is.na(ni <- match(nn, namD)))
                    warning(gettextf("variable '%s' is absent, its contrast will be ignored", nn),
                            domain = NA)
                else {
                    ca <- contrasts.arg[[nn]]
                    if(is.matrix(ca)) contrasts(data[[ni]], ncol(ca)) <- ca
                    else contrasts(data[[ni]]) <- contrasts.arg[[nn]]
                }
            }
        }
    } else {               # internal model.matrix needs some variable
        isF <-  FALSE
        data <- list(x=rep(0, nrow(data)))
    }
    ans <- .Internal(model.matrix(t, data))
    cons <- if(any(isF))
	lapply(data[isF], function(x) attr(x,  "contrasts"))
    else NULL
    attr(ans, "contrasts") <- cons
    ans
}

model.response <- function (data, type = "any")
{
    if (attr(attr(data, "terms"), "response")) {
	if (is.list(data) | is.data.frame(data)) {
	    v <- data[[1L]]
	    if (type == "numeric" && is.factor(v)) {
		warning('using type="numeric" with a factor response will be ignored')
	    } else if (type == "numeric" | type == "double")
		storage.mode(v) <- "double"
	    else if (type != "any") stop("invalid response type")
	    if (is.matrix(v) && ncol(v) == 1L) dim(v) <- NULL
	    rows <- attr(data, "row.names")
	    if (nrows <- length(rows)) {
		if (length(v) == nrows) names(v) <- rows
		else if (length(dd <- dim(v)) == 2L)
		    if (dd[1L] == nrows && !length((dn <- dimnames(v))[[1L]]))
			dimnames(v) <- list(rows, dn[[2L]])
	    }
	    return(v)
	} else stop("invalid 'data' argument")
    } else return(NULL)
}

model.extract <- function (frame, component)
{
    component <- as.character(substitute(component))
    rval <- switch(component,
		   response = model.response(frame),
		   offset = model.offset(frame),
                   frame[[paste("(", component, ")", sep = "")]]
                   )
    if(!is.null(rval)){
	if (length(rval) == nrow(frame))
	    names(rval) <- attr(frame, "row.names")
	else if (is.matrix(rval) && nrow(rval) == nrow(frame)) {
	    t1 <- dimnames(rval)
	    dimnames(rval) <- list(attr(frame, "row.names"), t1[[2L]])
	}
    }
    return(rval)
}

preplot <- function(object, ...) UseMethod("preplot")
update <- function(object, ...) UseMethod("update")

is.empty.model <- function (x)
{
    tt <- terms(x)
    (length(attr(tt, "factors")) == 0L) & (attr(tt, "intercept") == 0L)
}

makepredictcall <- function(var, call) UseMethod("makepredictcall")

makepredictcall.default  <- function(var, call)
{
    if(as.character(call)[1L] != "scale") return(call)
    if(!is.null(z <- attr(var, "scaled:center"))) call$center <- z
    if(!is.null(z <- attr(var, "scaled:scale"))) call$scale <- z
    call
}

.getXlevels <- function(Terms, m)
{
    xvars <- sapply(attr(Terms, "variables"), deparse, width.cutoff=500)[-1L]
    if((yvar <- attr(Terms, "response")) > 0) xvars <- xvars[-yvar]
    if(length(xvars)) {
        xlev <- lapply(m[xvars], function(x) if(is.factor(x)) levels(x) else NULL)
        xlev[!sapply(xlev, is.null)]
    } else NULL
}

get_all_vars <- function(formula, data = NULL, ...)
{
    if(missing(formula)) {
	if(!missing(data) && inherits(data, "data.frame") &&
	   length(attr(data, "terms")) )
	    return(data)
	formula <- as.formula(data)
    }
    else if(missing(data) && inherits(formula, "data.frame")) {
	if(length(attr(formula, "terms")))
	    return(formula)
	data <- formula
	formula <- as.formula(data)
    }
    formula <- as.formula(formula)
    if(missing(data))
	data <- environment(formula)
    else if (!is.data.frame(data) && !is.environment(data)
             && !is.null(attr(data, "class")))
        data <- as.data.frame(data)
    else if (is.array(data))
        stop("'data' must be a data.frame, not a matrix or an array")
    if(!inherits(formula, "terms"))
	formula <- terms(formula, data = data)
    env <- environment(formula)
    rownames <- .row_names_info(data, 0L) #attr(data, "row.names")
    varnames <- all.vars(formula)
    inp <- parse(text=paste("list(", paste(varnames, collapse=","), ")"))
    variables <- eval(inp, data, env)
    if(is.null(rownames) && (resp <- attr(formula, "response")) > 0) {
        ## see if we can get rownames from the response
        lhs <- variables[[resp]]
        rownames <- if(is.matrix(lhs)) rownames(lhs) else names(lhs)
    }
    extras <- substitute(list(...))
    extranames <- names(extras[-1L])
    extras <- eval(extras, data, env)
    x <- as.data.frame(c(variables, extras), optional=TRUE)
    names(x) <- c(varnames, extranames)
    attr(x, "row.names") <- rownames # might be short form
    x
}
#  File src/library/stats/R/monthplot.R
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

monthplot <- function(x, ...) UseMethod("monthplot")

monthplot.StructTS <-
    function (x, labels = NULL, ylab = choice, choice = "sea", ...)
    monthplot(fitted(x)[, choice], labels = labels, ylab = ylab, ...)

monthplot.stl <-
    function (x, labels = NULL, ylab = choice, choice = "seasonal", ...)
    monthplot(x$time.series[, choice], labels = labels, ylab = ylab, ...)

monthplot.ts <-
    function (x, labels = NULL, times = time(x), phase = cycle(x),
              ylab = deparse(substitute(x)), ...)
{
    if (is.null(labels) & !missing(phase))
        return(monthplot.default(x, times = times, phase = phase,
                                 ylab = ylab, ...))
    if (is.null(labels)) {
        if (missing(phase)) {
            f <- frequency(x)
            if (f == 4) labels <- paste("Q", 1L:4L, sep = "")
            else if (f == 12)
                labels <- c("J", "F", "M", "A", "M", "J", "J",
                  "A", "S", "O", "N", "D")
            else labels <- 1L:f
        }
    }
    monthplot.default(x, labels = labels, times = times, phase = phase,
                      ylab = ylab, ...)
}

monthplot.default <-
    function (x, labels = 1L:12L,
              ylab = deparse(substitute(x)),
              times = seq_along(x),
              phase = (times - 1L)%%length(labels) + 1L, base = mean,
              axes = TRUE, type = c("l", "h"), box = TRUE, add = FALSE, ...)
{
    dots <- list(...); nmdots <- names(dots)
    type <- match.arg(type)
    if (is.null(labels) || (missing(labels) && !missing(phase))) {
        labels <- unique(phase)
        phase <- match(phase, labels)
    }
    f <- length(labels)
    if (!is.null(base))
        means <- tapply(x, phase, base)
    if (!add) {
        Call <- match.call()
        Call[[1L]] <- as.name("plot")
        Call$x <- NA
        Call$y <- NA
        Call$axes <- FALSE
        Call$xlim <- if("xlim" %in% nmdots) dots$xlim else c(0.55, f + 0.45)
        Call$ylim <- if("ylim" %in% nmdots) dots$ylim else range(x, na.rm = TRUE)
        Call$xlab <- if("xlab" %in% nmdots) dots$xlab else ""
        if(box) Call$frame.plot <- TRUE
        Call$labels <- Call$times <- Call$phase <- Call$base <-
            Call$type <- Call$box <- Call$add <- NULL
        eval(Call)
        if (axes) {
            axis(1, at = 1L:f, labels = labels, ...)
            axis(2, ...)
        }
        if (!is.null(base))
            segments(1L:f - 0.45, means, 1L:f + 0.45, means)
    }
    y <- as.numeric(times)
    scale <- 1 / diff(range(y, na.rm = TRUE)) * 0.9
    for (i in 1L:f) {
        sub <- phase == i
        if (type != "h")
            lines((y[sub] - min(y)) * scale - 0.45 + i, x[sub],
                  type = type, ...)
        else segments((y[sub] - min(y)) * scale - 0.45 + i, means[i],
                      (y[sub] - min(y)) * scale - 0.45 + i, x[sub], ...)
    }
    invisible()
}
#  File src/library/stats/R/mood.test.R
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

mood.test <- function(x, ...) UseMethod("mood.test")

mood.test.default <-
function(x, y, alternative = c("two.sided", "less", "greater"), ...)
{
    alternative <- match.arg(alternative)
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))

    x <- x[is.finite(x)]
    y <- y[is.finite(y)]
    m <- length(x)
    n <- length(y)
    if ((N <- m + n) < 3L)
        stop("not enough observations")
    E <- m * (N ^ 2 - 1) / 12
    ## avoid possible integer overflow
    v <- (1/180) * m * n * (N + 1) * (N + 2) * (N - 2)
    z <- c(x, y)
    if(!anyDuplicated(z)) {
        ## Proceed as per Conover (1971).
        r <- rank(z)
        T <- sum((r[seq_along(x)] - (N + 1L) / 2) ^ 2)
    }
    else {
        ## Proceed as per Mielke (1967).
        u <- sort(unique(z))
        a <- tabulate(match(x, u), length(u))
        t <- tabulate(match(z, u), length(u))
        p <- cumsum((seq_along(z) - (N + 1L) / 2) ^ 2)
        v <- v - (m * n) / (180 * N * (N - 1L)) *
            sum(t * (t ^ 2 - 1) * (t ^ 2 - 4 + 15 * (N - t) ^ 2))
        T <- sum(a * diff(c(0, p[cumsum(t)])) / t)
    }
    z <- (T - E) / sqrt(v)
    p <- pnorm(z)
    PVAL <- switch(alternative,
                   "less" = p,
                   "greater" = 1 - p,
                   "two.sided" = 2 * min(p, 1 - p))

    structure(list(statistic = structure(z, names = "Z"),
                   p.value = PVAL,
                   alternative = alternative,
                   method = "Mood two-sample test of scale",
                   data.name = DNAME),
              class = "htest")
}

mood.test.formula <-
function(formula, data, subset, na.action, ...)
{
    if(missing(formula)
       || (length(formula) != 3L)
       || (length(attr(terms(formula[-2]), "term.labels")) != 1L))
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m[[1L]] <- as.name("model.frame")
    m$... <- NULL
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " by ")
    names(mf) <- NULL
    response <- attr(attr(mf, "terms"), "response")
    g <- factor(mf[[-response]])
    if(nlevels(g) != 2L)
        stop("grouping factor must have exactly 2 levels")
    DATA <- split(mf[[response]], g)
    names(DATA) <- c("x", "y")
    y <- do.call("mood.test", c(DATA, list(...)))
    y$data.name <- DNAME
    y
}
#  File src/library/stats/R/na.ts.R
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

na.contiguous <- function(object, ...) UseMethod("na.contiguous")

na.contiguous.default <- function(object, ...)
{
    tm <- time(object)
    xfreq <- frequency(object)
    ## use (first) maximal contiguous length of non-NAs
    if(is.matrix(object))
        good <- apply(!is.na(object), 1L, all)
    else  good <- !is.na(object)
    if(!sum(good)) stop("all times contain an NA")
    tt <- cumsum(!good)
    ln <- sapply(0:max(tt), function(i) sum(tt==i))
    seg <- (seq_along(ln)[ln==max(ln)])[1L] - 1
    keep <- (tt == seg)
    st <- min(which(keep))
    if(!good[st]) st <- st + 1
    en <- max(which(keep))
    omit <- integer(0L)
    n <- NROW(object)
    if(st > 1) omit <- c(omit, 1L:(st-1))
    if(en < n) omit <- c(omit, (en+1):n)
    cl <- class(object)
    if(length(omit)) {
        object <- if(is.matrix(object)) object[st:en,] else object[st:en]
        attr(omit, "class") <- "omit"
        attr(object, "na.action") <- omit
        tsp(object) <- c(tm[st], tm[en], xfreq)
        if(!is.null(cl)) class(object) <- cl
    }
    object
}
#  File src/library/stats/R/nafns.R
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

na.pass <- function(object, ...) object
na.action <- function(object, ...) UseMethod("na.action")
na.action.default <- function(object, ...)
{
    if(is.list(object) && "na.action" %in% names(object)) object[["lm"]]
    else attr(object, "na.action")
}

na.fail <- function(object, ...) UseMethod("na.fail")
na.fail.default <- function(object, ...)
{
    ok <- complete.cases(object)
    if(all(ok)) object else stop("missing values in object")
}

na.omit <- function(object, ...) UseMethod("na.omit")

na.omit.default <- function(object, ...)
{
    ## only handle vectors and matrices
    if (!is.atomic(object)) return(object)
    d <- dim(object)
    if (length(d) > 2) return(object)
    omit <- seq_along(object)[is.na(object)]
    if (length(omit) == 0) return(object)
    if (length(d)){
        omit <- unique(((omit-1) %% d[1L]) + 1L)
        nm <- rownames(object)
        object <- object[-omit, , drop=FALSE]
    } else {
        nm <- names(object)
        object <- object[-omit]
    }
    if (any(omit > 0L)) {
	names(omit) <- nm[omit]
	attr(omit, "class") <- "omit"
	attr(object, "na.action") <- omit
    }
    object
}

na.omit.data.frame <- function(object, ...)
{
    ## Assuming a data.frame like object
    n <- length(object)
    omit <- FALSE
    vars <- seq_len(n)
    for(j in vars) {
	x <- object[[j]]
	if(!is.atomic(x)) next
	## variables are assumed to be either some sort of matrix, numeric,...
	x <- is.na(x)
	d <- dim(x)
	if(is.null(d) || length(d) != 2)
	    omit <- omit | x
	else # matrix
	    for(ii in 1L:d[2L])
		omit <- omit | x[, ii]
    }
    xx <- object[!omit, , drop = FALSE]
    if (any(omit > 0L)) {
	temp <- seq(omit)[omit]
	names(temp) <- attr(object, "row.names")[omit]
	attr(temp, "class") <- "omit"
	attr(xx, "na.action") <- temp
    }
    xx
}

na.exclude <- function(object, ...) UseMethod("na.exclude")

na.exclude.default <- function(object, ...)
{
    ## only handle vectors and matrices
    if (!is.atomic(object)) return(object)
    d <- dim(object)
    if (length(d) > 2) return(object)
    omit <- seq_along(object)[is.na(object)]
    if (length(omit) == 0) return(object)
    if (length(d)){
        omit <- unique(((omit-1) %% d[1L]) + 1L)
        nm <- rownames(object)
        object <- object[-omit, , drop=FALSE]
    } else {
        nm <- names(object)
        object <- object[-omit]
    }
    if (any(omit > 0L)) {
	names(omit) <- nm[omit]
	attr(omit, "class") <- "exclude"
	attr(object, "na.action") <- omit
    }
    object
}

na.exclude.data.frame <- function(object, ...)
{
    ## Assuming a data.frame like object
    n <- length(object)
    omit <- FALSE
    vars <- seq_len(n)
    for(j in vars) {
	x <- object[[j]]
	if(!is.atomic(x)) next
	## variables are assumed to be either some sort of matrix, numeric,...
	x <- is.na(x)
	d <- dim(x)
	if(is.null(d) || length(d) != 2)
	    omit <- omit | x
	else # matrix
	    for(ii in 1L:d[2L])
		omit <- omit | x[, ii]
    }
    xx <- object[!omit, , drop = FALSE]
    if (any(omit > 0L)) {
	temp <- seq(omit)[omit]
	names(temp) <- attr(object, "row.names")[omit]
	attr(temp, "class") <- "exclude"
	attr(xx, "na.action") <- temp
    }
    xx
}

naresid <- function(omit, x, ...) UseMethod("naresid")
naresid.default <- function(omit, x, ...) x

## naresid.exclude (same as napredict...) *reconstruct* original size values:
naresid.exclude <- function(omit, x, ...)
{
    if (length(omit) == 0 || !is.numeric(omit))
	stop("invalid argument 'omit'")
    if(length(x) == 0)## << FIXME? -- reconstructing all NA object
        return(x)

    if (is.matrix(x)) {
	n <- nrow(x)
	keep <- rep.int(NA, n+length(omit))
	keep[-omit] <- 1L:n
	x <- x[keep, , drop=FALSE]
	temp <- rownames(x)
	if (length(temp)) {
	    temp[omit] <- names(omit)
	    rownames(x) <- temp
        }
    } else {# vector *or* data.frame !
	n <- length(x)
	keep <- rep.int(NA, n+length(omit))
	keep[-omit] <- 1L:n
	x <- x[keep]
	temp <- names(x)
	if (length(temp)) {
	    temp[omit] <- names(omit)
	    names(x) <- temp
        }
    }
    x
}

naprint <- function(x, ...) UseMethod("naprint")
naprint.default <- function(x, ...) return("")
naprint.exclude <- naprint.omit <- function(x, ...)
    sprintf(ngettext(n <- length(x), "%d observation deleted due to missingness",
                     "%d observations deleted due to missingness"),
            n)

napredict <- function(omit, x, ...) UseMethod("napredict")
napredict.default <- function(omit, x, ...) x
napredict.exclude <- function(omit, x, ...) naresid.exclude(omit, x)
#  File src/library/stats/R/nlm.R
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

nlm <- function(f, p, ..., hessian=FALSE, typsize=rep(1,length(p)),
		fscale=1, print.level=0, ndigit=12, gradtol=1e-6,
		stepmax=max(1000 * sqrt(sum((p/typsize)^2)), 1000),
		steptol=1e-6, iterlim=100, check.analyticals=TRUE)
{

    print.level <- as.integer(print.level)
    if(print.level < 0 || print.level > 2)
	stop("'print.level' must be in {0,1,2}")
    ## msg is collection of bits, i.e., sum of 2^k (k = 0,..,4):
    msg <- (1 + c(8,0,16))[1+print.level]
    if(!check.analyticals) msg <- msg + (2 + 4)
    .Internal(nlm(function(x) f(x, ...), p, hessian, typsize, fscale,
                  msg, ndigit, gradtol, stepmax, steptol, iterlim))
}

optimize <- function(f, interval, ...,
		     lower=min(interval), upper=max(interval),
		     maximum=FALSE, tol=.Machine$double.eps^0.25)
{
    if(maximum) {
	val <- .Internal(fmin(function(arg) -f(arg, ...), lower, upper, tol))
	list(maximum = val, objective= f(val, ...))
    } else {
	val <- .Internal(fmin(function(arg) f(arg, ...), lower, upper, tol))
	list(minimum = val, objective= f(val, ...))
    }
}

##nice to the English (or rather the Scots)
optimise <- optimize

uniroot <- function(f, interval, ...,
                    lower = min(interval), upper = max(interval),
                    f.lower = f(lower, ...), f.upper = f(upper, ...),
		    tol = .Machine$double.eps^0.25, maxiter = 1000)
{
    if(!missing(interval) && length(interval) != 2)
        stop("'interval' must be a vector of length 2")
    if(!is.numeric(lower) || !is.numeric(upper) || lower >= upper)
        stop("lower < upper  is not fulfilled")
    if(is.na(f.lower)) stop("f.lower = f(lower) is NA")
    if(is.na(f.upper)) stop("f.upper = f(upper) is NA")
    if(f.lower * f.upper > 0)
	stop("f() values at end points not of opposite sign")
    val <- .Internal(zeroin2(function(arg) f(arg, ...),
                             lower, upper, f.lower, f.upper,
			     tol, as.integer(maxiter)))
    iter <- as.integer(val[2L])
    if(iter < 0) {
	warning("_NOT_ converged in ", maxiter, " iterations")
        iter <- maxiter
    }
    list(root = val[1L], f.root = f(val[1L], ...),
         iter = iter, estim.prec = val[3L])
}
#  File src/library/stats/R/nlminb.R
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

nlminb <-
    function(start, objective, gradient = NULL, hessian = NULL, ...,
             scale = 1, control = list(), lower =  - Inf, upper = Inf)
{
    ## Establish the working vectors and check and set options
    par <- as.double(start)
    names(par) <- names(start)
    n <- length(par)
    iv <- integer(78 + 3 * n)
    v <- double(130 + (n * (n + 27)) / 2)
    .Call(R_port_ivset, 2, iv, v)
    if (length(control)) {
        nms <- names(control)
        if (!is.list(control) || is.null(nms))
            stop("control argument must be a named list")
        cpos <- c(eval.max = 17, iter.max = 18, trace = 19, abs.tol = 31,
                  rel.tol = 32, x.tol = 33, step.min = 34, step.max = 35,
                  scale.init = 38, sing.tol = 37, diff.g = 42)
        pos <- pmatch(nms, names(cpos))
        if (any(nap <- is.na(pos))) {
            warning(paste("unrecognized control element(s) named `",
                          paste(nms[nap], collapse = ", "),
                          "' ignored", sep = ""))
            pos <- pos[!nap]
            control <- control[!nap]
        }
        ivpars <- pos < 4
        if (any(ivpars))
            iv[cpos[pos[ivpars]]] <- as.integer(unlist(control[ivpars]))
        if (any(!ivpars))
            v[cpos[pos[!ivpars]]] <- as.double(unlist(control[!ivpars]))
    }

    ## Establish the objective function and its environment
    obj <- quote(objective(.par, ...))
    rho <- new.env(parent = environment())
    assign(".par", par, envir = rho)

    ## Create values of other arguments if needed
    grad <- hess <- low <- upp <- NULL
    if (!is.null(gradient)) {
        grad <- quote(gradient(.par, ...))
        if (!is.null(hessian)) {
            if (is.logical(hessian))
                stop("Logical `hessian' argument not allowed.  See documentation.")
            hess <- quote(hessian(.par, ...))
        }
    }
    if (any(lower != -Inf) || any(upper != Inf)) {
        low <- rep(as.double(lower), length.out = length(par))
        upp <- rep(as.double(upper), length.out = length(par))
    } else low <- upp <- numeric(0L)

    ## Do the optimization
    .Call(R_port_nlminb, obj, grad, hess, rho, low, upp,
          d = rep(as.double(scale), length.out = length(par)), iv, v)

    ans <- list(par = get(".par", envir = rho))
    ans$objective <- v[10L]
    ans$convergence <- as.integer(if (iv[1] %in% 3L:6L) 0L else 1L)

    ans$message <- if (19 <= iv[1] && iv[1] <= 43) {
	if(any(B <- iv[1] == cpos))
	    sprintf("'control' component '%s' = %g, is out of range",
		    names(cpos)[B], v[iv[1]])
	else
	    sprintf("V[IV[1]] = V[%d] = %g is out of range (see PORT docu.)",
		    iv[1], v[iv[1]])
    }
    else
	switch(as.character(iv[1]),
               "3" = "X-convergence (3)",
               "4" = "relative convergence (4)",
               "5" = "both X-convergence and relative convergence (5)",
               "6" = "absolute function convergence (6)",

               "7" = "singular convergence (7)",
               "8" = "false convergence (8)",
               "9" = "function evaluation limit reached without convergence (9)",
               "10" = "iteration limit reached without convergence (9)",
               "14" = "storage has been allocated (?) (14)",

               "15" = "LIV too small (15)",
               "16" = "LV too small (16)",
               "63" = "fn cannot be computed at initial par (63)",
               "65" = "gr cannot be computed at initial par (65)")
    if (is.null(ans$message))
        ans$message <-
            paste("See PORT documentation.  Code (", iv[1], ")", sep = "")
    ans$iterations <- iv[31]
    ans$evaluations <- c("function" = iv[6], gradient = iv[30])
    ans
}
#  File src/library/stats/R/nls-profile.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright 1999-1999 Saikat DebRoy <saikat$stat.wisc.edu>,
#                      Douglas M. Bates <bates$stat.wisc.edu>,
#  Copyright 2005-6   The R Development Core Team
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

###
### Profiling nonlinear least squares for R
###

profiler <- function(fitted, ...) UseMethod("profiler")

profiler.nls <- function(fitted, ...)
{
    fittedModel <- fitted$m
    algorithm <- fitted$call$algorithm
    ctrl <- fitted$call$control
    trace <- fitted$call$trace
    defaultPars <- fittedPars <- fittedModel$getPars()
    lower <- fitted$call$lower
    lower <- rep(if(!is.null(lower)) as.double(lower) else Inf,
                 length.out = length(defaultPars))
    upper <- fitted$call$upper
    upper <- rep(if(!is.null(upper)) as.double(upper) else Inf,
                 length.out = length(defaultPars))
    defaultVary <- rep.int(TRUE, length(defaultPars))
    S.hat <- deviance(fitted) # need to allow for weights
    s2.hat <- summary(fitted)$sigma^2
    thisEnv <- environment()
    on.exit(remove(fitted))
    prof <- list(getFittedPars = function() fittedPars,
                 getFittedModel = function() fittedModel,
                 setDefault = function(varying, params)
             {
                 if(missing(params) && missing(varying)) {
                     fittedModel$setVarying()
                     fittedModel$setPars(fittedPars)
                     assign("defaultPars", fittedPars, envir = thisEnv)
                     assign("defaultVary", rep.int(TRUE, length(defaultPars)),
                            envir = thisEnv)
                 } else {
                     if(!missing(params)) {
                         if(length(params) != length(fittedPars))
                             stop("'params' has wrong length")
                         assign("defaultPars", params, envir = thisEnv)
                     }
                     if(!missing(varying)) {
                         if(is.numeric(varying)) {
                             if(!all(varying %in% seq_along(fittedPars)))
                                 stop("'varying' must be in seq_along(pars)")
                             varying <- !((seq_along(fittedPars)) %in% varying)
                         } else if(is.logical(varying)) {
                             if(length(varying) != length(fittedPars))
                                 stop("'varying' has wrong length")
                         } else if(is.character(varying)) {
                             if(!all(varying %in% names(fittedPars)))
                                 stop("'varying' must be in seq_along(pars)")
                             varying <- !(names(fittedPars) %in% varying)
                         } else stop("'varying' must be logical, integer or character")
                         assign("defaultVary", varying, envir = thisEnv)
                     }
                 }
             },
                 getProfile = function(...)
             {
                 args <- list(...)
                 if(length(args) == 0) {
                     vary <- defaultVary
                     startPars <- defaultPars
                 } else if(length(args) == 2 && is.logical(args[[1L]])) {
                     vary <- args[[1L]]
                     params <- unlist(args[[2L]])
                     startPars <- defaultPars
                     startPars[!vary] <- params
                 } else {
                     if(length(args) == 1 && is.list(args[[1L]])) {
                         params <- unlist(args[[1L]])
                     } else if(all(sapply(args, is.numeric))) {
                         params <- unlist(args)
                     } else stop("invalid argument to 'getProfile'")
                     if(!all(names(params) %in% names(fittedPars)))
                         stop("cannot recognize parameter name")
                     startPars <- defaultPars
                     vary <- !(names(fittedPars) %in% names(params))
                     startPars[!vary] <- params
                 }
                 fittedModel$setVarying()
                 fittedModel$setPars(startPars)
                 fittedModel$setVarying(vary)
                 fittedModel$setPars(startPars[vary])
                 ## change fittedModel into profiledModel
		 if(algorithm != "port") {
		     if(sum(vary)) .Call(R_nls_iter, fittedModel, ctrl, trace)
		     dev <- fittedModel$deviance()
		 } else {
		     iv <- nls_port_fit(fittedModel, startPars[vary],
					lower[vary], upper[vary], ctrl, trace)
		     dev <- if(!iv[1L] %in% 3:6)
			NA_real_
		     else
			fittedModel$deviance()
		 }
		 profiledModel <- fittedModel
                 fstat <- (dev - S.hat)/s2.hat
                 fittedModel$setVarying()
                 ans <- list(fstat = fstat,
                             parameters = profiledModel$getAllPars(),
                             varying = vary)
                 fittedModel$setPars(defaultPars)
                 ans
             })
    class(prof) <- c("profiler.nls", "profiler")
    prof
}

profile.nls <-
  function(fitted, which = 1L:npar, maxpts = 100, alphamax = 0.01,
           delta.t = cutoff/5, ...)
{
    f.summary <- summary(fitted)
    std.err <- f.summary$coefficients[, "Std. Error"]
    nobs <- length(resid(fitted))
    prof <- profiler(fitted)
    pars <- prof$getFittedPars()
    npar <- length(pars)  # less in a partially linear model
    lower <- fitted$call$lower
    lower <- rep(if(!is.null(lower)) as.double(lower) else -Inf, length.out = npar)
    upper <- fitted$call$upper
    upper <- rep(if(!is.null(upper)) as.double(upper) else Inf, length.out = npar)
    if(is.character(which)) which <- match(which, names(pars), 0)
    which <- which[which >= 1 & which <= npar]
    ## was 'npar' - length(which) would have made more sense
    cutoff <- sqrt(qf(1 - alphamax, 1L, nobs - npar))
    out <- vector("list", npar)
    on.exit(prof$setDefault())     # in case there is an abnormal exit
    for(par in which) {
        pars <- prof$getFittedPars() # reset to fitted model's values
        prof$setDefault(varying = par)
        sgn <- -1
        count <- 1
        varying <- rep.int(TRUE, npar)
        varying[par] <- FALSE
        tau <- double(2 * maxpts)
        par.vals <- array(0, c(2L * maxpts, npar), list(NULL, names(pars)))
        tau[1L] <- 0
        par.vals[1,  ] <- pars
        base <- pars[par]
        profile.par.inc <- delta.t * std.err[par]
        pars[par] <- base - profile.par.inc
        pars[par] <- pmin(upper[par], pmax(lower[par], pars[par]))
        while(count <= maxpts) {
            if(is.na(pars[par]) || isTRUE(all.equal(pars, par.vals[1, ])) ||
               pars[par] < lower[par] || pars[par] > upper[par] ||
               abs(pars[par] - base)/std.err[par] > 10 * cutoff) break
            prof$setDefault(params = pars)
            ans <- prof$getProfile()
            if(is.na(ans$fstat) || ans$fstat < 0) break
            newtau <- sgn*sqrt(ans$fstat)
            if(abs(newtau - tau[count]) < 0.1) break
            count <- count + 1
            tau[count] <- newtau
            par.vals[count, ] <- pars <- ans$parameters[1L:npar]
            if(abs(tau[count]) > cutoff) break
            pars <- pars + ((pars - par.vals[count - 1,  ]) * delta.t)/
                abs(tau[count] - tau[count - 1])
            pars[-par] <- pmin(upper[-par], pmax(lower[-par], pars[-par]))
        }
        ind <- seq_len(count)
        tau[ind] <- tau[rev(ind)]
        par.vals[ind,  ] <- par.vals[rev(ind),  ]
        sgn <- 1
        newmax <- count + maxpts
        pars <- par.vals[count,  ]
        pars[par] <- base + profile.par.inc
        pars[par] <- pmin(upper[par], pmax(lower[par], pars[par]))
        while(count <= newmax) {
            if(is.na(pars[par]) || isTRUE(all.equal(pars, par.vals[1, ])) ||
               pars[par] < lower[par] || pars[par] > upper[par] ||
               abs(pars[par] - base)/std.err[par] > 10 * cutoff) break
            prof$setDefault(params = pars)
            ans <- prof$getProfile()
            if(is.na(ans$fstat)|| ans$fstat < 0) break
            newtau <- sgn*sqrt(ans$fstat)
            if(abs(newtau - tau[count]) < 0.1) break
            count <- count + 1
            tau[count] <- newtau
            par.vals[count, ] <- pars <- ans$parameters[1L:npar]
            if(abs(tau[count]) > cutoff) break
            pars <- pars + ((pars - par.vals[count - 1,  ]) * delta.t)/
                abs(tau[count] - tau[count - 1])
            pars[-par] <- pmin(upper[-par], pmax(lower[-par], pars[-par]))
        }
        ind <- seq_len(count)
        out[[par]] <- structure(list(tau = tau[ind], par.vals =
                                     par.vals[ind,  , drop=FALSE]),
                                class = "data.frame",
                                row.names = as.character(ind),
                                parameters = list(par = par,
                                std.err = std.err[par]))
        prof$setDefault()
    }
    names(out)[which] <- names(coef(fitted))[which]
    out <- out[which]
    attr(out, "original.fit") <- fitted
    attr(out, "summary") <- f.summary
    class(out) <- c("profile.nls", "profile")
    out
}

plot.profile.nls <-
    function(x, levels, conf = c(99, 95, 90, 80, 50)/100, absVal = TRUE, ...)
{
    obj <- x
    dfres <- attr(obj, "summary")$df[2L]
    if(missing(levels))
        levels <- sqrt(qf(pmax(0, pmin(1, conf)), 1, dfres))
    if(any(levels <= 0)) {
        levels <- levels[levels > 0]
        warning("levels truncated to positive values only")
    }
    mlev <- max(levels) * 1.05
    nm <- names(obj)
    opar <- par(mar = c(5, 4, 1, 1) + 0.1)
    if (absVal) {
        for (i in nm) {
            sp <- splines::interpSpline(obj[[i]]$par.vals[,i], obj[[i]]$tau)
            bsp <- splines::backSpline(sp)
            xlim <- predict(bsp, c(-mlev, mlev))$y
            if (is.na(xlim[1L])) xlim[1L] <- min(x[[i]]$par.vals[, i])
            if (is.na(xlim[2L])) xlim[2L] <- max(x[[i]]$par.vals[, i])
            plot(abs(tau) ~ par.vals[, i], data = obj[[i]], xlab = i,
                 ylim = c(0, mlev), xlim = xlim, ylab = expression(abs(tau)),
                 type = "n")
            avals <- rbind(as.data.frame(predict(sp)),
                           data.frame(x = obj[[i]]$par.vals[, i],
                                      y = obj[[i]]$tau))
            avals$y <- abs(avals$y)
            lines(avals[ order(avals$x), ], col = 4)
            abline(v = predict(bsp, 0)$y , col = 3, lty = 2)
            for(lev in levels) {
                pred <- predict(bsp, c(-lev, lev))$y
                lines(pred, rep.int(lev, 2), type = "h", col = 6, lty = 2)
                lines(pred, rep.int(lev, 2), type = "l", col = 6, lty = 2)
            }
        }
    } else {
        for (i in nm) {
            sp <- splines::interpSpline(obj[[i]]$par.vals[,i], obj[[i]]$tau)
            bsp <- splines::backSpline(sp)
            xlim <- predict(bsp, c(-mlev, mlev))$y
            if (is.na(xlim[1L])) xlim[1L] <- min(x[[i]]$par.vals[, i])
            if (is.na(xlim[2L])) xlim[2L] <- max(x[[i]]$par.vals[, i])
            plot(tau ~ par.vals[, i], data = obj[[i]], xlab = i,
                 ylim = c(-mlev, mlev), xlim = xlim, ylab = expression(tau),
                 type = "n")
            lines(predict(sp), col = 4)
            abline(h = 0, col = 3, lty = 2)
            for(lev in  levels) {
                pred <- predict(bsp, c(-lev, lev))$y
                lines(pred, c(-lev, lev), type = "h", col = 6, lty = 2)
            }
        }
    }
    par(opar)
}
#  File src/library/stats/R/nls.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright 1999-1999 Saikat DebRoy <saikat$stat.wisc.edu>,
#                      Douglas M. Bates <bates$stat.wisc.edu>,
#                      Jose C. Pinheiro <jcp$research.bell-labs.com>
#  Copyright 2005-7    The R Development Core Team
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

###
###            Nonlinear least squares for R
###

numericDeriv <- function(expr, theta, rho = parent.frame(), dir=1.0)
{
    dir <- rep(dir, length.out = length(theta))
    val <- .Call(R_numeric_deriv, expr, theta, rho, dir)
    valDim <- dim(val)
    if (!is.null(valDim)) {
        if (valDim[length(valDim)] == 1)
            valDim <- valDim[-length(valDim)]
        if(length(valDim) > 1L)
            dim(attr(val, "gradient")) <- c(valDim,
                                            dim(attr(val, "gradient"))[-1L])
    }
    val
}

nlsModel.plinear <- function(form, data, start, wts)
{
    thisEnv <- environment()
    env <- new.env(parent=environment(form))
    for(i in names(data)) assign(i, data[[i]], envir = env)
    ind <- as.list(start)
    p2 <- 0
    for(i in names(ind)) {
        temp <- start[[i]]
        storage.mode(temp) <- "double"
        assign(i, temp, envir = env)
        ind[[i]] <- p2 + seq_along(start[[i]])
        p2 <- p2 + length(start[[i]])
    }
    lhs <- eval(form[[2L]], envir = env)
    storage.mode(lhs) <- "double"
    rhs <- eval(form[[3L]], envir = env)
    storage.mode(rhs) <- "double"
    .swts <- if(!missing(wts) && length(wts))
        sqrt(wts) else rep(1, length.out=NROW(rhs))
    assign(".swts", .swts, envir = env)
    p1 <- if(is.matrix(rhs)) ncol(rhs) else 1
    p <- p1 + p2
    n <- length(lhs)
    fac <- (n -  p)/p
    cc <- QR.B <- NA
    useParams <- rep(TRUE, p2)
    if(is.null(attr(rhs, "gradient"))) {
        getRHS.noVarying <- function()
            numericDeriv(form[[3L]], names(ind), env)
        getRHS <- getRHS.noVarying
        rhs <- getRHS()
    } else {
        getRHS.noVarying <- function() eval(form[[3L]], envir = env)
        getRHS <- getRHS.noVarying
    }
    dimGrad <- dim(attr(rhs, "gradient"))
    marg <- length(dimGrad)
    if(marg > 0) {
        gradSetArgs <- vector("list", marg+1)
        for(i in 2:marg)
            gradSetArgs[[i]] <- rep(TRUE, dimGrad[i-1])
        useParams <- rep(TRUE, dimGrad[marg])
    } else {
        gradSetArgs <- vector("list", 2)
        useParams <- rep(TRUE, length(attr(rhs, "gradient")))
    }
    gradSetArgs[[1L]] <- (~attr(ans, "gradient"))[[2L]]
    gradCall <-
        switch(length(gradSetArgs) - 1L,
               call("[", gradSetArgs[[1L]], gradSetArgs[[2L]]),
               call("[", gradSetArgs[[1L]], gradSetArgs[[2L]], gradSetArgs[[2L]]),
               call("[", gradSetArgs[[1L]], gradSetArgs[[2L]], gradSetArgs[[2L]],
                    gradSetArgs[[3L]]),
               call("[", gradSetArgs[[1L]], gradSetArgs[[2L]], gradSetArgs[[2L]],
                    gradSetArgs[[3L]], gradSetArgs[[4L]]))
    getRHS.varying <- function()
    {
        ans <- getRHS.noVarying()
        attr(ans, "gradient") <- eval(gradCall)
        ans
    }
    QR.rhs <- qr(.swts * rhs)
    lin <- qr.coef(QR.rhs, .swts * lhs)
    resid <- qr.resid(QR.rhs, .swts * lhs)
    topzero <- double(p1)
    dev <- sum(resid^2)
    if(marg <= 1) {
        ddot <- function(A, b) A %*% b
        dtdot <- function(A, b) t(A) %*% b
    } else if(marg == 2) {
        if(p1 == 1) {
            ddot <- function(A, b) as.matrix(A*b)
            dtdot <- function(A, b) t(b) %*% A
        } else if(p2 == 1) {
            ddot <- function(A, b) A %*% b
            dtdot <- function(A, b) t(A) %*% b
        }
    } else {
        ddot <- function(A, b) apply(A, MARGIN = 3L, FUN="%*%", b)
        dtdot <- function(A, b) apply(A, MARGIN = c(2L,3L), FUN = "%*%", b)
    }

    getPars.noVarying <- function()
        unlist(setNames(lapply(names(ind), get, envir = env), names(ind)))
    getPars.varying <- function()
        unlist(setNames(lapply(names(ind), get, envir = env),
                        names(ind)))[useParams]
    getPars <- getPars.noVarying

    internalPars <- getPars()
    setPars.noVarying <- function(newPars)
    {
        assign("internalPars", newPars, envir = thisEnv)
        for(i in names(ind)) {
            assign(i, unname(newPars[ ind[[i]] ]), envir = env )
        }
    }
    setPars.varying <- function(newPars)
    {
        internalPars[useParams] <- newPars
        for(i in names(ind))
            assign(i, unname(internalPars[ ind[[i]] ]), envir = env)
    }
    setPars <- setPars.noVarying
    getPred <-
        if(is.matrix(rhs)) function(X) as.numeric(X %*% lin)
        else function(X) X * lin

    m <-
        list(resid = function() resid,
             fitted = function() getPred(rhs),
             formula = function() form,
             deviance = function() dev,
             lhs = function() lhs,
             gradient = function() attr(rhs, "gradient"),
             conv = function() {
                 assign("cc", c(topzero, qr.qty(QR.rhs, .swts * lhs)[ -(1L:p1)]),
                        envir = thisEnv)
                 rr <- qr.qy(QR.rhs, cc)
                 B <- qr.qty(QR.rhs, .swts * ddot(attr(rhs, "gradient"), lin))
                 B[1L:p1, ] <- dtdot(.swts * attr(rhs, "gradient"), rr)
                 R <- t( qr.R(QR.rhs)[1L:p1, ] )
                 if(p1 == 1) B[1, ] <- B[1, ]/R
                 else B[1L:p1, ] <- forwardsolve(R, B[1L:p1, ])
                 assign("QR.B", qr(B), envir = thisEnv)
                 rr <- qr.qty(QR.B, cc)
                 sqrt( fac*sum(rr[1L:p1]^2) / sum(rr[-(1L:p1)]^2) )
             },
             incr = function() qr.solve(QR.B, cc),
             setVarying = function(vary = rep(TRUE, length(useParams))) {
                 assign("useParams", if(is.character(vary)) {
                     temp <- logical(length(useParams))
                     temp[unlist(ind[vary])] <- TRUE
                     temp
                 } else if(is.logical(vary) && length(vary) != length(useParams))
                        stop("setVarying : 'vary' length must match length of parameters")
                 else {
                     vary
                 }, envir = thisEnv)
                 gradCall[[length(gradCall)]] <<- useParams
                 if(all(useParams)) {
                     assign("setPars", setPars.noVarying, envir = thisEnv)
                     assign("getPars", getPars.noVarying, envir = thisEnv)
                     assign("getRHS", getRHS.noVarying, envir = thisEnv)
                 } else {
                     assign("setPars", setPars.varying, envir = thisEnv)
                     assign("getPars", getPars.varying, envir = thisEnv)
                     assign("getRHS", getRHS.varying, envir = thisEnv)
                 }
             },
             setPars = function(newPars) {
                 setPars(newPars)
                 assign("QR.rhs",
                        qr(.swts * assign("rhs", getRHS(), envir = thisEnv)),
                        envir = thisEnv)
                 assign("resid", qr.resid(QR.rhs, .swts * lhs),
                        envir = thisEnv)
                 assign("dev", sum(resid^2), envir = thisEnv )
                 if(QR.rhs$rank < p1) {
                     return(1)
                 } else {
                     assign("lin", qr.coef(QR.rhs, .swts * lhs),
                            envir = thisEnv)
                     return(0)
                 }
             },
             getPars = function() getPars(),
             getAllPars = function() c( getPars(), c( .lin = lin ) ),
             getEnv = function() env,
             trace = function() cat(format(dev),":",
             format(c(getPars(), lin)), "\n" ),
             Rmat = function()
             qr.R(qr(.swts * cbind(ddot(attr(rhs, "gradient"), lin), rhs))),
             predict = function(newdata = list(), qr = FALSE)
             getPred(eval(form[[3L]], as.list(newdata), env))
             )
    class(m) <- c("nlsModel.plinear", "nlsModel")
    m$conv()
    on.exit( remove( data, i, m, marg, n, p, start, temp, gradSetArgs) )
    m
}

nlsModel <- function(form, data, start, wts, upper=NULL)
{
    thisEnv <- environment()
    env <- new.env(parent = environment(form))
    for(i in names(data)) assign(i, data[[i]], envir = env)
    ind <- as.list(start)
    parLength <- 0
    for(i in names(ind) ) {
        temp <- start[[i]]
        storage.mode(temp) <- "double"
        assign(i, temp, envir = env)
        ind[[i]] <- parLength + seq_along(start[[i]])
        parLength <- parLength + length(start[[i]])
    }
    getPars.noVarying <- function()
        unlist(setNames(lapply(names(ind), get, envir = env), names(ind)))
    getPars <- getPars.noVarying
    internalPars <- getPars()

    if(!is.null(upper)) upper <- rep(upper, length.out = parLength)
    useParams <- rep(TRUE, parLength)
    lhs <- eval(form[[2L]], envir = env)
    rhs <- eval(form[[3L]], envir = env)
    .swts <- if(!missing(wts) && length(wts))
        sqrt(wts) else rep(1, length.out=length(rhs))
    assign(".swts", .swts, envir = env)
    resid <- .swts * (lhs - rhs)
    dev <- sum(resid^2)
    if(is.null(attr(rhs, "gradient"))) {
        getRHS.noVarying <- function() {
            if(is.null(upper))
                numericDeriv(form[[3L]], names(ind), env)
            else
                numericDeriv(form[[3L]], names(ind), env,
                             ifelse(internalPars < upper, 1, -1))
        }
        getRHS <- getRHS.noVarying
        rhs <- getRHS()
    } else {
        getRHS.noVarying <- function() eval(form[[3L]], envir = env)
        getRHS <- getRHS.noVarying
    }
    dimGrad <- dim(attr(rhs, "gradient"))
    marg <- length(dimGrad)
    if(marg > 0L) {
        gradSetArgs <- vector("list", marg+1)
        for(i in 2L:marg)
            gradSetArgs[[i]] <- rep(TRUE, dimGrad[i-1])
        useParams <- rep(TRUE, dimGrad[marg])
    } else {
        gradSetArgs <- vector("list", 2)
        useParams <- rep(TRUE, length(attr(rhs, "gradient")))
    }
    npar <- length(useParams)
    gradSetArgs[[1L]] <- (~attr(ans, "gradient"))[[2L]]
    gradCall <-
        switch(length(gradSetArgs) - 1L,
               call("[", gradSetArgs[[1L]], gradSetArgs[[2L]], drop = FALSE),
               call("[", gradSetArgs[[1L]], gradSetArgs[[2L]], gradSetArgs[[2L]],
                    drop = FALSE),
               call("[", gradSetArgs[[1L]], gradSetArgs[[2L]], gradSetArgs[[2L]],
                    gradSetArgs[[3L]], drop = FALSE),
               call("[", gradSetArgs[[1L]], gradSetArgs[[2L]], gradSetArgs[[2L]],
                    gradSetArgs[[3L]], gradSetArgs[[4L]]), drop = FALSE)
    getRHS.varying <- function()
    {
        ans <- getRHS.noVarying()
        attr(ans, "gradient") <- eval(gradCall)
        ans
    }
    QR <- qr(.swts * attr(rhs, "gradient"))
    qrDim <- min(dim(QR$qr))
    if(QR$rank < qrDim)
        stop("singular gradient matrix at initial parameter estimates")

    getPars.varying <- function()
        unlist(setNames(lapply(names(ind), get, envir = env),
                        names(ind)))[useParams]
    setPars.noVarying <- function(newPars)
    {
        assign("internalPars", newPars, envir = thisEnv)
        for(i in names(ind))
            assign(i, unname(newPars[ ind[[i]] ]), envir = env)
    }
    setPars.varying <- function(newPars)
    {
        internalPars[useParams] <- newPars
        for(i in names(ind))
            assign(i, unname(internalPars[ ind[[i]] ]), envir = env)
    }
    setPars <- setPars.noVarying

    on.exit(remove(i, data, parLength, start, temp, m))
    ## must use weighted resid for use with "port" algorithm.
    m <-
	list(resid = function() resid,
	     fitted = function() rhs,
	     formula = function() form,
	     deviance = function() dev,
	     lhs = function() lhs,
	     gradient = function() .swts * attr(rhs, "gradient"),
	     conv = function() {
		 if(npar == 0) return(0)
		 rr <- qr.qty(QR, resid) # rotated residual vector
		 sqrt( sum(rr[1L:npar]^2) / sum(rr[-(1L:npar)]^2))
	     },
	     incr = function() qr.coef(QR, resid),
	     setVarying = function(vary = rep(TRUE, length(useParams))) {
		 assign("useParams",
			if(is.character(vary)) {
			    temp <- logical(length(useParams))
			    temp[unlist(ind[vary])] <- TRUE
			    temp
			} else if(is.logical(vary) &&
				  length(vary) != length(useParams))
			stop("setVarying : 'vary' length must match length of parameters")
			else {
			    vary
			}, envir = thisEnv)
		 gradCall[[length(gradCall) - 1L]] <<- useParams
		 if(all(useParams)) {
		     assign("setPars", setPars.noVarying, envir = thisEnv)
		     assign("getPars", getPars.noVarying, envir = thisEnv)
		     assign("getRHS", getRHS.noVarying, envir = thisEnv)
		     assign("npar", length(useParams), envir = thisEnv)
		 } else {
		     assign("setPars", setPars.varying, envir = thisEnv)
		     assign("getPars", getPars.varying, envir = thisEnv)
		     assign("getRHS", getRHS.varying, envir = thisEnv)
                     ## FIXME this is which(useParams)
		     assign("npar", length(seq_along(useParams)[useParams]),
			    envir = thisEnv)
		 }
	     },
	     setPars = function(newPars) {
		 setPars(newPars)
		 assign("resid", .swts *
			(lhs - assign("rhs", getRHS(), envir = thisEnv)),
			envir = thisEnv)
		 assign("dev", sum(resid^2), envir = thisEnv)
		 assign("QR", qr(.swts * attr(rhs, "gradient")),
			envir = thisEnv )
		 return(QR$rank < min(dim(QR$qr))) # to catch the singular gradient matrix
	     },
	     getPars = function() getPars(),
	     getAllPars = function() getPars(),
	     getEnv = function() env,
	     trace = function() cat(format(dev),": ", format(getPars()), "\n"),
	     Rmat = function() qr.R(QR),
	     predict = function(newdata = list(), qr = FALSE)
	     eval(form[[3L]], as.list(newdata), env)
	     )

    class(m) <- "nlsModel"
    m
}

nls.control <- function(maxiter = 50, tol = 0.00001, minFactor = 1/1024,
			printEval = FALSE, warnOnly = FALSE)
    list(maxiter = maxiter, tol = tol, minFactor = minFactor,
	 printEval = printEval, warnOnly = warnOnly)

nls_port_fit <- function(m, start, lower, upper, control, trace)
{
    ## Establish the working vectors and check and set options
    p <- length(par <- as.double(unlist(start)))
    iv <- integer(4L*p + 82L)
    v <- double(105L + (p * (2L * p + 20L)))
    .Call(R_port_ivset, 1, iv, v)
    if (length(control)) {
	if (!is.list(control) || is.null(nms <- names(control)))
	    stop("control argument must be a named list")
	## remove those components that do not apply here
	for(noN in intersect(nms, c("tol", "minFactor", "warnOnly", "printEval")))
	    control[[noN]] <- NULL
	nms <- names(control)
	cpos <- c(eval.max = 17, maxiter = 18, trace = 19, abs.tol = 31,
		  rel.tol = 32, x.tol = 33, step.min = 34, step.max = 35,
		  scale.init = 38, sing.tol = 37, diff.g = 42)
	pos <- pmatch(nms, names(cpos))
        if (any(nap <- is.na(pos))) {
            warning(paste("unrecognized control element(s) named `",
                          paste(nms[nap], collapse = ", "),
                          "' ignored", sep = ""))
            pos <- pos[!nap]
            control <- control[!nap]
        }
        ivpars <- pos < 4
        if (any(ivpars))
            iv[cpos[pos[ivpars]]] <- as.integer(unlist(control[ivpars]))
        if (any(!ivpars))
            v[cpos[pos[!ivpars]]] <- as.double(unlist(control[!ivpars]))
    }
    if (trace)
        iv[19] <- 1L
    scale <- 1
    low <- upp <- NULL
    if (any(lower != -Inf) || any(upper != Inf)) {
        low <- rep(as.double(lower), length.out = length(par))
        upp <- rep(as.double(upper), length.out = length(par))
        if(any(unlist(start) < low) ||any( unlist(start) > upp)) {
            iv[1L] <- 300
            return(iv)
        }
    }
    if(p > 0) {
        ## driver routine port_nlsb() in ../src/port.c -- modifies m & iv
        .Call(R_port_nlsb, m,
              d = rep(as.double(scale), length.out = length(par)),
              df = m$gradient(), iv, v, low, upp)
    } else iv[1L] <- 6
    iv
}

nls <-
  function (formula, data = parent.frame(), start, control = nls.control(),
            algorithm = c("default", "plinear", "port"), trace = FALSE,
            subset, weights, na.action, model = FALSE,
            lower = -Inf, upper = Inf, ...)
{
    ## canonicalize the arguments
    formula <- as.formula(formula)
    algorithm <- match.arg(algorithm)

    if(!is.list(data) && !is.environment(data))
        stop("'data' must be a list or an environment")

    mf <- match.call()                  # for creating the model frame
    varNames <- all.vars(formula) # parameter and variable names from formula
    ## adjust a one-sided model formula by using 0 as the response
    if (length(formula) == 2L) {
        formula[[3L]] <- formula[[2L]]
        formula[[2L]] <- 0
    }
    ## for prediction we will need to know those which are in RHS
    form2 <- formula; form2[[2L]] <- 0
    varNamesRHS <- all.vars(form2)
    mWeights <- missing(weights)

    ## get names of the parameters from the starting values or selfStart model
    pnames <-
	if (missing(start)) {
	    if(!is.null(attr(data, "parameters"))) {
		names(attr(data, "parameters"))
	    } else { ## try selfStart - like object
		cll <- formula[[length(formula)]]
		func <- get(as.character(cll[[1L]]))
		if(!is.null(pn <- attr(func, "pnames")))
		    as.character(as.list(match.call(func, call = cll))[-1L][pn])
	    }
	} else
	    names(start)

    env <- environment(formula)
    if (is.null(env)) env <- parent.frame()

    ## Heuristics for determining which names in formula represent actual
    ## variables :

    ## If it is a parameter it is not a variable (nothing to guess here :-)
    if(length(pnames))
        varNames <- varNames[is.na(match(varNames, pnames))]

    ## This aux.function needs to be as complicated because
    ## exists(var, data) does not work (with lists or dataframes):
    lenVar <- function(var) tryCatch(length(eval(as.name(var), data, env)),
				     error = function(e) -1)

    if(length(varNames)) {
        n <- sapply(varNames, lenVar)
        if(any(not.there <- n == -1)) {
            nnn <- names(n[not.there])
            if(missing(start)) {
                if(algorithm == "plinear")
                    ## TODO: only specify values for the non-lin. parameters
                    stop("No starting values specified")
                ## Provide some starting values instead of erroring out later;
                ## '1' seems slightly better than 0 (which is often invalid):
                warning("No starting values specified for some parameters.\n",
                        "Intializing ", paste(sQuote(nnn), collapse=", "),
                        " to '1.'.\n",
                        "Consider specifying 'start' or using a selfStart model")
                start <- as.list(rep(1., length(nnn)))
                names(start) <- nnn
                varNames <- varNames[i <- is.na(match(varNames, nnn))]
                n <- n[i]
            }
            else                        # has 'start' but forgot some
                stop("parameters without starting value in 'data': ",
                     paste(nnn, collapse=", "))
        }
    }
    else { ## length(varNames) == 0
	if(length(pnames) && any((np <- sapply(pnames, lenVar)) == -1)) {
            ## Can fit a model with pnames even if no varNames
	    message("fitting parameters ",
		    paste(sQuote(pnames[np == -1]), collapse=", "),
		    " without any variables")
            n <- integer(0L)
        }
	else
	    stop("no parameters to fit")
    }

    ## If its length is a multiple of the response or LHS of the formula,
    ## then it is probably a variable.
    ## This may fail (e.g. when LHS contains parameters):
    respLength <- length(eval(formula[[2L]], data, env))

    if(length(n) > 0) {
	varIndex <- n %% respLength == 0
	if(is.list(data) && diff(range(n[names(n) %in% names(data)])) > 0) {
	    ## 'data' is a list that can not be coerced to a data.frame
	    mf <- data
            if(!missing(subset))
                warning("argument 'subset' will be ignored")
            if(!missing(na.action))
                warning("argument 'na.action' will be ignored")
	    if(missing(start))
		start <- getInitial(formula, mf)
	    startEnv <- new.env(parent = environment(formula))
	    for (i in names(start))
		assign(i, start[[i]], envir = startEnv)
	    rhs <- eval(formula[[3L]], data, startEnv)
	    n <- NROW(rhs)
            ## mimic what model.frame.default does
            wts <- if (mWeights) rep(1, n) else
                eval(substitute(weights), data, environment(formula))
	}
        else {
            mf$formula <-  # replace by one-sided linear model formula
                as.formula(paste("~", paste(varNames[varIndex], collapse = "+")),
                           env = environment(formula))
            mf$start <- mf$control <- mf$algorithm <- mf$trace <- mf$model <- NULL
            mf$lower <- mf$upper <- NULL
            mf[[1L]] <- as.name("model.frame")
            mf <- eval.parent(mf)
            n <- nrow(mf)
            mf <- as.list(mf)
            wts <- if (!mWeights) model.weights(mf) else rep(1, n)
        }
        if (any(wts < 0 | is.na(wts)))
            stop("missing or negative weights not allowed")
    }
    else {
        ## length(n) == 0 : Some problems might have no official varNames
        ##                  but still parameters to fit
        varIndex <- logical(0L)
        mf <- list(0)
        wts <- numeric(0L)
    }

    ## set up iteration
    if (missing(start)) start <- getInitial(formula, mf)
    for(var in varNames[!varIndex])
        mf[[var]] <- eval(as.name(var), data, env)
    varNamesRHS <- varNamesRHS[ varNamesRHS %in% varNames[varIndex] ]

    m <- switch(algorithm,
		plinear = nlsModel.plinear(formula, mf, start, wts),
		port    = nlsModel        (formula, mf, start, wts, upper),
                ## Default:
		nlsModel(formula, mf, start, wts))

    ctrl <- nls.control()
    if(!missing(control)) {
	control <- as.list(control)
	ctrl[names(control)] <- control
    }
                                        # Iterate
    if (algorithm != "port") {
	if (!missing(lower) || !missing(upper))
	    warning('Upper or lower bounds ignored unless algorithm = "port"')
        convInfo <- .Call(R_nls_iter, m, ctrl, trace)
	nls.out <- list(m = m, convInfo = convInfo,
			data = substitute(data), call = match.call())
    }
    else { ## "port" i.e., PORT algorithm
	iv <- nls_port_fit(m, start, lower, upper, control, trace)
	nls.out <- list(m = m, data = substitute(data), call = match.call())
        ## FIXME: this is really a logical for  *NON*convergence:
	nls.out$convergence <- as.integer(if (iv[1L] %in% 3:6) 0 else 1)
	nls.out$message <-
	    switch(as.character(iv[1L]),
		   "3" = "X-convergence (3)",
		   "4" = "relative convergence (4)",
		   "5" = "both X-convergence and relative convergence (5)",
		   "6" = "absolute function convergence (6)",

		   "7" = "singular convergence (7)",
		   "8" = "false convergence (8)",
		   "9" = "function evaluation limit reached without convergence (9)",
		   "10" = "iteration limit reached without convergence (9)",
		   "14" = "storage has been allocated (?) (14)",

		   "15" = "LIV too small (15)",
		   "16" = "LV too small (16)",
		   "63" = "fn cannot be computed at initial par (63)",
		   "65" = "gr cannot be computed at initial par (65)",
		   "300" = "initial par violates constraints")
	if (is.null(nls.out$message))
	    nls.out$message <-
		paste("See PORT documentation.	Code (", iv[1L], ")", sep = "")
	if (nls.out$convergence) {
            msg <- paste("Convergence failure:", nls.out$message)
            if(ctrl$warnOnly) {
                warning(msg)
            } else stop(msg)
        }

	## we need these (evaluated) for profiling
	nls.out$call$lower <- lower
	nls.out$call$upper <- upper
    }

    ## we need these (evaluated) for profiling
    nls.out$call$algorithm <- algorithm
    nls.out$call$control <- ctrl
    nls.out$call$trace <- trace

    nls.out$na.action <- attr(mf, "na.action")
    nls.out$dataClasses <-
        attr(attr(mf, "terms"), "dataClasses")[varNamesRHS]
    if(model)
	nls.out$model <- mf
    if(!mWeights)
	nls.out$weights <- wts
    nls.out$control <- control
    class(nls.out) <- "nls"
    nls.out
}

coef.nls <- function(object, ...) object$m$getAllPars()

summary.nls <-
    function (object, correlation = FALSE, symbolic.cor = FALSE, ...)
{
    r <- as.vector(object$m$resid()) # These are weighted residuals.
    w <- object$weights
    n <- if (!is.null(w)) sum(w > 0) else length(r)
    param <- coef(object)
    pnames <- names(param)
    p <- length(param)
    rdf <- n - p
    resvar <- if(rdf <= 0) NaN else deviance(object)/rdf
    XtXinv <- chol2inv(object$m$Rmat())
    dimnames(XtXinv) <- list(pnames, pnames)
    se <- sqrt(diag(XtXinv) * resvar)
    tval <- param/se
    param <- cbind(param, se, tval, 2 * pt(abs(tval), rdf, lower.tail = FALSE))
    dimnames(param) <-
        list(pnames, c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
    ans <- list(formula = formula(object), residuals = r, sigma = sqrt(resvar),
                df = c(p, rdf), cov.unscaled = XtXinv,
                call = object$call,
                convInfo = object$convInfo,
                control = object$control,
                na.action = object$na.action,
                coefficients = param,
                parameters = param)# never documented, for back-compatibility
    if(correlation && rdf > 0) {
        ans$correlation <- (XtXinv * resvar)/outer(se, se)
        ans$symbolic.cor <- symbolic.cor
    }
    if(identical(object$call$algorithm, "port"))
	ans$message <- object$message
    class(ans) <- "summary.nls"
    ans
}

.p.nls.convInfo <- function(x, digits)
{
    if(identical(x$call$algorithm, "port"))
	cat("\nAlgorithm \"port\", convergence message:",
	    x$message, "\n")
    else
	with(x$convInfo, {
	    cat("\nNumber of iterations",
		if(isConv) "to convergence:" else "till stop:", finIter,
		"\nAchieved convergence tolerance:",
                format(finTol, digits=digits),"\n")
	    if(!isConv)
		cat("Reason stopped:", stopMessage, "\n")
	})
    invisible()
}

print.nls <- function(x, digits = max(3, getOption("digits") - 3), ...)
{
    cat("Nonlinear regression model\n")
    cat("  model: ", deparse(formula(x)), "\n")
    cat("   data: ", deparse(x$data), "\n")
    print(x$m$getAllPars(), digits = digits, ...)
    cat(" ", if(!is.null(x$weights) && diff(range(x$weights))) "weighted ",
	"residual sum-of-squares: ", format(x$m$deviance(), digits = digits),
	"\n", sep = '')
    .p.nls.convInfo(x, digits = digits)
    invisible(x)
}

print.summary.nls <-
  function (x, digits = max(3, getOption("digits") - 3),
            symbolic.cor = x$symbolic.cor,
            signif.stars = getOption("show.signif.stars"), ...)
{
    cat("\nFormula: ")
    cat(paste(deparse(x$formula), sep = "\n", collapse = "\n"), "\n", sep = "")
    df <- x$df
    rdf <- df[2L]
    cat("\nParameters:\n")
    printCoefmat(x$coefficients, digits = digits, signif.stars = signif.stars,
                 ...)
    cat("\nResidual standard error:",
        format(signif(x$sigma, digits)), "on", rdf, "degrees of freedom\n")
    correl <- x$correlation
    if (!is.null(correl)) {
        p <- NCOL(correl)
        if (p > 1) {
            cat("\nCorrelation of Parameter Estimates:\n")
	    if(is.logical(symbolic.cor) && symbolic.cor) {
		print(symnum(correl, abbr.colnames = NULL))
            } else {
                correl <- format(round(correl, 2), nsmall = 2, digits = digits)
                correl[!lower.tri(correl)] <- ""
                print(correl[-1, -p, drop=FALSE], quote = FALSE)
            }
        }
    }

    .p.nls.convInfo(x, digits = digits)

    if(nzchar(mess <- naprint(x$na.action))) cat("  (", mess, ")\n", sep="")
    cat("\n")
    invisible(x)
}

weights.nls <- function(object, ...) object$weights

predict.nls <-
  function(object, newdata, se.fit = FALSE, scale = NULL, df = Inf,
           interval = c("none", "confidence", "prediction"), level = 0.95,
           ...)
{
    if (missing(newdata)) return(as.vector(fitted(object)))
    if(!is.null(cl <- object$dataClasses)) .checkMFClasses(cl, newdata)
    object$m$predict(newdata)
}

fitted.nls <- function(object, ...)
{
    val <- as.vector(object$m$fitted())
    if(!is.null(object$na.action)) val <- napredict(object$na.action, val)
    lab <- "Fitted values"
    if (!is.null(aux <- attr(object, "units")$y)) lab <- paste(lab, aux)
    attr(val, "label") <- lab
    val
}

formula.nls <- function(x, ...) x$m$formula()

residuals.nls <- function(object, type = c("response", "pearson"), ...)
{
    type <- match.arg(type)
    if (type == "pearson") {
        val <- as.vector(object$m$resid())
        std <- sqrt(sum(val^2)/(length(val) - length(coef(object))))
        val <- val/std
        if(!is.null(object$na.action)) val <- naresid(object$na.action, val)
        attr(val, "label") <- "Standardized residuals"
    } else {
        val <- as.vector(object$m$lhs() - object$m$fitted())
        if(!is.null(object$na.action))
            val <- naresid(object$na.action, val)
        lab <- "Residuals"
        if (!is.null(aux <- attr(object, "units")$y)) lab <- paste(lab, aux)
        attr(val, "label") <- lab
    }
    val
}

logLik.nls <- function(object, REML = FALSE, ...)
{
    if (REML)
        stop("cannot calculate REML log-likelihood for \"nls\" objects")
    res <- object$m$resid()
    N <- length(res)
    if(is.null(w <- object$weights)) w <- rep(1, N)
    val <-  -N * (log(2 * pi) + 1 - log(N) - sum(log(w)) + log(sum(w*res^2)))/2
    ## the formula here corresponds to estimating sigma^2.
    attr(val, "df") <- 1L + length(coef(object))
    attr(val, "nobs") <- attr(val, "nall") <- N
    class(val) <- "logLik"
    val
}

df.residual.nls <- function(object, ...) {
    w <- object$weights
    n <- if(!is.null(w)) sum(w != 0) else length(resid(object))
    n - length(coef(object))
}

deviance.nls <- function(object, ...) object$m$deviance()

vcov.nls <- function(object, ...)
{
    sm <- summary(object)
    sm$cov.unscaled * sm$sigma^2
}


anova.nls <- function(object, ...)
{
    if(length(list(object, ...)) > 1) return(anovalist.nls(object, ...))
    stop("anova is only defined for sequences of \"nls\" objects")
}

anovalist.nls <- function (object, ..., test = NULL)
{
    objects <- list(object, ...)
    responses <- as.character(lapply(objects,
				     function(x) formula(x)[[2L]]))
    sameresp <- responses == responses[1L]
    if (!all(sameresp)) {
	objects <- objects[sameresp]
	warning("models with response ",
                deparse(responses[!sameresp]),
                " removed because response differs from model 1")
    }
    ## calculate the number of models
    nmodels <- length(objects)
    if (nmodels == 1L)
        stop("'anova' is only defined for sequences of \"nls\" objects")

    models <- as.character(lapply(objects, function(x) formula(x)))

    ## extract statistics
    df.r <- unlist(lapply(objects, df.residual))
    ss.r <- unlist(lapply(objects, deviance))
    df <- c(NA, -diff(df.r))
    ss <- c(NA, -diff(ss.r))
    ms <- ss/df
    f <- p <- rep(NA_real_, nmodels)
    for(i in 2:nmodels) {
	if(df[i] > 0) {
	    f[i] <- ms[i]/(ss.r[i]/df.r[i])
	    p[i] <- pf(f[i], df[i], df.r[i], lower.tail = FALSE)
	}
	else if(df[i] < 0) {
	    f[i] <- ms[i]/(ss.r[i-1]/df.r[i-1])
	    p[i] <- pf(f[i], -df[i], df.r[i-1], lower.tail = FALSE)
	}
	else {                          # df[i] == 0
            ss[i] <- 0
	}
    }
    table <- data.frame(df.r,ss.r,df,ss,f,p)
    dimnames(table) <- list(1L:nmodels, c("Res.Df", "Res.Sum Sq", "Df",
					 "Sum Sq", "F value", "Pr(>F)"))
    ## construct table and title
    title <- "Analysis of Variance Table\n"
    topnote <- paste("Model ", format(1L:nmodels),": ",
		     models, sep="", collapse="\n")

    ## calculate test statistic if needed
    structure(table, heading = c(title, topnote),
	      class = c("anova", "data.frame")) # was "tabular"
}
#  File src/library/stats/R/nlsFunc.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright 1997,1999 Jose C. Pinheiro <jcp$research.bell-labs.com>,
#                      Douglas M. Bates <bates$stat.wisc.edu>
#            1999-1999 Saikat DebRoy <saikat$stat.wisc.edu>
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

###
###            Utility functions used with nls
###

###
### asOneSidedFormula is extracted from the NLME-3.1 library for S
###

asOneSidedFormula <-
  ## Converts an expression or a name or a character string
  ## to a one-sided formula
  function(object)
{
    if ((mode(object) == "call") && (object[[1L]] == "~")) {
        object <- eval(object)
    }
    if (inherits(object, "formula")) {
        if (length(object) != 2) {
            stop(gettextf("formula '%s' must be of the form '~expr'",
                          deparse(as.vector(object))), domain = NA)
        }
        return(object)
    }
    do.call("~",
            list(switch(mode(object),
                        name = ,
                        numeric = ,
                        call = object,
                        character = as.name(object),
                        expression = object[[1L]],
                        stop(gettextf("'%s' cannot be of mode '%s'",
                                      substitute(object), mode(object)),
                             domain = NA)
                        ))
            )
}

setNames <- function( object, nm )
{
    names( object ) <- nm
    object
}
#  File src/library/stats/R/oneway.test.R
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

oneway.test <-
function(formula, data, subset, na.action, var.equal = FALSE)
{
    if(missing(formula) || (length(formula) != 3))
        stop("'formula' missing or incorrect")
    dp <- as.character(formula)
    if(length(dp) != 3)
        stop("a two-sided formula is required")
    DNAME <- paste(dp[[2L]], "and", dp[[3L]])
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m$var.equal <- NULL
    m[[1L]] <- as.name("model.frame")
    mf <- eval(m, parent.frame())
    response <- attr(attr(mf, "terms"), "response")
    y <- mf[[response]]
    if(length(mf[-response]) > 1)
        g <- factor(do.call("interaction", mf[-response]))
    else
        g <- factor(mf[[-response]])
    k <- nlevels(g)
    if(k < 2)
        stop("not enough groups")
    n.i <- tapply(y, g, length)
    if(any(n.i < 2))
        stop("not enough observations")
    m.i <- tapply(y, g, mean)
    v.i <- tapply(y, g, var)
    w.i <- n.i / v.i
    sum.w.i <- sum(w.i)
    tmp <- sum((1 - w.i / sum.w.i)^2 / (n.i - 1)) / (k^2 - 1)
    METHOD <- "One-way analysis of means"
    if(var.equal) {
        n <- sum(n.i)
        STATISTIC <- ((sum(n.i * (m.i - mean(y))^2) / (k - 1)) /
                      (sum((n.i - 1) * v.i) / (n - k)))
        PARAMETER <- c(k - 1, n - k)
        PVAL <- pf(STATISTIC, k - 1, n - k, lower.tail = FALSE)
    }
    else {
        ## STATISTIC <- sum(w.i * (m.i - mean(y))^2) /
        ##    ((k - 1) * (1 + 2 * (k - 2) * tmp))
        m <- sum(w.i * m.i) / sum.w.i
        STATISTIC <- sum(w.i * (m.i - m)^2) /
            ((k - 1) * (1 + 2 * (k - 2) * tmp))
        PARAMETER <- c(k - 1, 1 / (3 * tmp))
        PVAL <- pf(STATISTIC, k - 1, 1 / (3 * tmp), lower.tail = FALSE)
        METHOD <- paste(METHOD, "(not assuming equal variances)")
    }
    names(STATISTIC) <- "F"
    names(PARAMETER) <- c("num df", "denom df")
    RVAL <- list(statistic = STATISTIC,
                 parameter = PARAMETER,
                 p.value = PVAL,
                 method = METHOD,
                 data.name = DNAME)
    class(RVAL) <- "htest"
    RVAL
}
#  File src/library/stats/R/optim.R
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

optim <- function(par, fn, gr = NULL, ...,
                  method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN"),
                  lower = -Inf, upper = Inf,
                  control = list(), hessian = FALSE)
{
    fn1 <- function(par) fn(par,...)
    gr1 <- if (!is.null(gr)) function(par) gr(par,...)
    method <- match.arg(method)
    if((length(lower) > 1L || length(upper) > 1L ||
       lower[1L] != -Inf || upper[1L] != Inf)
       && method != "L-BFGS-B") {
        warning("bounds can only be used with method L-BFGS-B")
        method <- "L-BFGS-B"
    }
    ## Defaults :
    con <- list(trace = 0, fnscale = 1, parscale = rep.int(1, length(par)),
                ndeps = rep.int(1e-3, length(par)),
                maxit = 100L, abstol = -Inf, reltol=sqrt(.Machine$double.eps),
                alpha = 1.0, beta = 0.5, gamma = 2.0,
                REPORT = 10,
                type = 1,
                lmm = 5, factr = 1e7, pgtol = 0,
                tmax = 10, temp = 10.0)
    nmsC <- names(con)
    if (method == "Nelder-Mead") con$maxit <- 500
    if (method == "SANN") {
	con$maxit <- 10000
	con$REPORT <- 100
    }
    con[(namc <- names(control))] <- control
    if(length(noNms <- namc[!namc %in% nmsC]))
        warning("unknown names in control: ", paste(noNms,collapse=", "))
    if(con$trace < 0)
        warning("read the documentation for 'trace' more carefully")
    else if (method == "SANN" && con$trace && as.integer(con$REPORT) == 0)
	stop("'trace != 0' needs 'REPORT >= 1'")
    if (method == "L-BFGS-B" &&
        any(!is.na(match(c("reltol","abstol"), namc))))
        warning("method L-BFGS-B uses 'factr' (and 'pgtol') instead of 'reltol' and 'abstol'")
    npar <- length(par)
    if(npar == 1 && method == "Nelder-Mead")
        warning("one-diml optimization by Nelder-Mead is unreliable: use optimize")
    lower <- as.double(rep(lower, , npar))
    upper <- as.double(rep(upper, , npar))
    res <- .Internal(optim(par, fn1, gr1, method, con, lower, upper))
    names(res) <- c("par", "value", "counts", "convergence", "message")
    nm <- names(par)
    if(!is.null(nm)) names(res$par) <- nm
    names(res$counts) <- c("function", "gradient")
    if (hessian) {
        hess <- .Internal(optimhess(res$par, fn1, gr1, con))
        hess <- 0.5*(hess + t(hess))
        if(!is.null(nm)) dimnames(hess) <- list(nm, nm)
        res$hessian <- hess
    }
    res
}
#  File src/library/stats/R/p.adjust.R
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

p.adjust.methods <-
    c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")

p.adjust <-
    function(p, method = p.adjust.methods, n = length(p))
{
    ## Methods 'Hommel', 'BH', 'BY' and speed improvements contributed by
    ## Gordon Smyth <smyth@wehi.edu.au>.
    method <- match.arg(method)
    if(method == "fdr") method <- "BH" # back compatibility

    p0 <- p
    if(all(nna <- !is.na(p))) nna <- TRUE
    p <- as.vector(p[nna])
 ## n <- length(p) ## -- maybe deprecate `n' argument ?
    stopifnot(n >= length(p))
    if (n <= 1) return(p0)
    if (n == 2 && method == "hommel") method <- "hochberg"

    p0[nna] <-
      switch(method,
             bonferroni = pmin(1, n * p),
             holm = {
               i <- 1L:n
               o <- order(p)
               ro <- order(o)
               pmin(1, cummax( (n - i + 1) * p[o] ))[ro]
             },
             hommel = { ## needs n-1 >= 2 in for() below
               i <- 1L:n
               o <- order(p)
               p <- p[o]
               ro <- order(o)
               q <- pa <- rep.int( min(n*p/(1L:n)), n)
               for (j in (n-1):2) {
                 q1 <- min(j*p[(n-j+2):n]/(2:j))
                 q[1L:(n-j+1)] <- pmin( j*p[1L:(n-j+1)], q1)
                 q[(n-j+2):n] <- q[n-j+1]
                 pa <- pmax(pa,q)
               }
               pmax(pa,p)[ro]
             },
             hochberg = {
               i <- n:1
               o <- order(p, decreasing = TRUE)
               ro <- order(o)
               pmin(1, cummin( (n - i + 1) * p[o] ))[ro]
             },
             BH = {
               i <- n:1
               o <- order(p, decreasing = TRUE)
               ro <- order(o)
               pmin(1, cummin( n / i * p[o] ))[ro]
             },
             BY = {
               i <- n:1
               o <- order(p, decreasing = TRUE)
               ro <- order(o)
               q <- sum(1/(1L:n))
               pmin(1, cummin(q * n / i * p[o]))[ro]
             },
             none = p)
    p0
}
#  File src/library/stats/R/pairwise.R
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

pairwise.t.test <-
function(x, g, p.adjust.method = p.adjust.methods, pool.sd = !paired,
         paired = FALSE, alternative = c("two.sided", "less", "greater"), ...)
{
    if (paired & pool.sd)
        stop("Pooling of SD is incompatible with paired tests")
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(g)))
    g <- factor(g)
    p.adjust.method <- match.arg(p.adjust.method)
    alternative <- match.arg(alternative)
    if (pool.sd)
    {
        METHOD <- "t tests with pooled SD"
        xbar <- tapply(x, g, mean, na.rm = TRUE)
        s <- tapply(x, g, sd, na.rm = TRUE)
        n <- tapply(!is.na(x), g, sum)
        degf <- n - 1
        total.degf <- sum(degf)
        pooled.sd <- sqrt(sum(s^2 * degf)/total.degf)
        compare.levels <- function(i, j) {
            dif <- xbar[i] - xbar[j]
            se.dif <- pooled.sd * sqrt(1/n[i] + 1/n[j])
            t.val <- dif/se.dif
            if (alternative == "two.sided")
                2 * pt(-abs(t.val), total.degf)
            else
                pt(t.val, total.degf,
                   lower.tail=(alternative == "less"))
        }
    } else {
        METHOD <- if (paired) "paired t tests"
        else "t tests with non-pooled SD"
        compare.levels <- function(i, j) {
            xi <- x[as.integer(g) == i]
            xj <- x[as.integer(g) == j]
            t.test(xi, xj, paired=paired,
                   alternative=alternative, ...)$p.value
        }
    }
    PVAL <- pairwise.table(compare.levels, levels(g), p.adjust.method)
    ans <- list(method = METHOD, data.name = DNAME,
                p.value = PVAL, p.adjust.method=p.adjust.method)
    class(ans) <- "pairwise.htest"
    ans
}


pairwise.wilcox.test <-
function(x, g, p.adjust.method = p.adjust.methods, paired=FALSE, ...)
{
    p.adjust.method <- match.arg(p.adjust.method)
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(g)))
    g <- factor(g)
    METHOD <- if (paired) "Wilcoxon signed rank test"
        else "Wilcoxon rank sum test"
    compare.levels <- function(i, j) {
        xi <- x[as.integer(g) == i]
        xj <- x[as.integer(g) == j]
        wilcox.test(xi, xj, paired=paired, ...)$p.value
    }
    PVAL <- pairwise.table(compare.levels, levels(g), p.adjust.method)
    ans <- list(method = METHOD, data.name = DNAME,
                p.value = PVAL, p.adjust.method=p.adjust.method)
    class(ans) <- "pairwise.htest"
    ans
}

pairwise.prop.test <-
function (x, n, p.adjust.method = p.adjust.methods, ...)
{
    p.adjust.method <- match.arg(p.adjust.method)
    METHOD <- "Pairwise comparison of proportions"
    DNAME <- deparse(substitute(x))
    if (is.matrix(x)) {
        if (ncol(x) != 2)
            stop("'x' must have 2 columns")
        n <- rowSums(x)
        x <- x[, 1]
    }
    else {
        DNAME <- paste(DNAME, "out of", deparse(substitute(n)))
        if (length(x) != length(n))
            stop("'x' and 'n' must have the same length")
    }
    OK <- complete.cases(x, n)
    x <- x[OK]
    n <- n[OK]
    if (length(x) < 2)
        stop("too few groups")
    compare.levels <- function(i, j) {
        prop.test(x[c(i,j)], n[c(i,j)], ...)$p.value
    }
    level.names <- names(x)
    if (is.null(level.names)) level.names <- seq_along(x)
    PVAL <- pairwise.table(compare.levels, level.names, p.adjust.method)
    ans <- list(method = METHOD, data.name = DNAME,
                p.value = PVAL, p.adjust.method=p.adjust.method)
    class(ans) <- "pairwise.htest"
    ans
}

pairwise.table <-
function(compare.levels, level.names, p.adjust.method)
{
    ix <- seq_along(level.names)
    names(ix) <- level.names
    pp <- outer(ix[-1L], ix[-length(ix)],function(ivec, jvec)
          sapply(seq_along(ivec), function(k) {
              i<-ivec[k]
              j<-jvec[k]
              if (i > j) compare.levels(i, j) else NA
          }))
    pp[lower.tri(pp, TRUE)] <- p.adjust(pp[lower.tri(pp, TRUE)],
                                        p.adjust.method)
    pp
}

print.pairwise.htest <-
function(x, ...)
{
    cat("\n\tPairwise comparisons using", x$method, "\n\n")
    cat("data: ", x$data.name, "\n\n")
    pp <- format.pval(x$p.value, 2, na.form="-")
    attributes(pp) <- attributes(x$p.value)
    print(pp, quote=FALSE)
    cat("\nP value adjustment method:", x$p.adjust.method, "\n")
    invisible(x)
}
#  File src/library/stats/R/plot.lm.R
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

plot.lm <-
function (x, which = c(1L:3L,5L), ## was which = 1L:4L,
	  caption = list("Residuals vs Fitted", "Normal Q-Q",
	  "Scale-Location", "Cook's distance",
	  "Residuals vs Leverage",
	  expression("Cook's dist vs Leverage  " * h[ii] / (1 - h[ii]))),
	  panel = if(add.smooth) panel.smooth else points,
	  sub.caption = NULL, main = "",
	  ask = prod(par("mfcol")) < length(which) && dev.interactive(), ...,
	  id.n = 3, labels.id = names(residuals(x)), cex.id = 0.75,
	  qqline = TRUE, cook.levels = c(0.5, 1.0),
	  add.smooth = getOption("add.smooth"),
	  label.pos = c(4,2), cex.caption = 1)
{
    dropInf <- function(x, h) {
	if(any(isInf <- h >= 1.0)) {
	    warning("Not plotting observations with leverage one:\n  ",
		    paste(which(isInf), collapse=", "),
                    call.=FALSE)
	    x[isInf] <- NaN
	}
	x
    }

    if (!inherits(x, "lm"))
	stop("use only with \"lm\" objects")
    if(!is.numeric(which) || any(which < 1) || any(which > 6))
	stop("'which' must be in 1:6")
    isGlm <- inherits(x, "glm")
    show <- rep(FALSE, 6)
    show[which] <- TRUE
    r <- residuals(x)
    yh <- predict(x) # != fitted() for glm
    w <- weights(x)
    if(!is.null(w)) { # drop obs with zero wt: PR#6640
	wind <- w != 0
	r <- r[wind]
	yh <- yh[wind]
	w <- w[wind]
	labels.id <- labels.id[wind]
    }
    n <- length(r)
    if (any(show[2L:6L])) {
	s <- if (inherits(x, "rlm")) x$s
        else if(isGlm) sqrt(summary(x)$dispersion)
        else sqrt(deviance(x)/df.residual(x))
	hii <- lm.influence(x, do.coef = FALSE)$hat
	if (any(show[4L:6L])) {
	    cook <- if (isGlm) cooks.distance(x)
            else cooks.distance(x, sd = s, res = r)
	}
    }
    if (any(show[2L:3L])) {
	ylab23 <- if(isGlm) "Std. deviance resid." else "Standardized residuals"
	r.w <- if (is.null(w)) r else sqrt(w) * r
        ## NB: rs is already NaN if r=0, hii=1
	rs <- dropInf( r.w/(s * sqrt(1 - hii)), hii )
    }

    if (any(show[5L:6L])) { # using 'leverages'
        r.hat <- range(hii, na.rm = TRUE) # though should never have NA
        isConst.hat <- all(r.hat == 0) ||
            diff(r.hat) < 1e-10 * mean(hii, na.rm = TRUE)
    }
    if (any(show[c(1L, 3L)]))
	l.fit <- if (isGlm) "Predicted values" else "Fitted values"
    if (is.null(id.n))
	id.n <- 0
    else {
	id.n <- as.integer(id.n)
	if(id.n < 0L || id.n > n)
	    stop(gettextf("'id.n' must be in {1,..,%d}", n), domain = NA)
    }
    if(id.n > 0L) { ## label the largest residuals
	if(is.null(labels.id))
	    labels.id <- paste(1L:n)
	iid <- 1L:id.n
	show.r <- sort.list(abs(r), decreasing = TRUE)[iid]
	if(any(show[2L:3L]))
	    show.rs <- sort.list(abs(rs), decreasing = TRUE)[iid]
	text.id <- function(x, y, ind, adj.x = TRUE) {
	    labpos <-
                if(adj.x) label.pos[1+as.numeric(x > mean(range(x)))] else 3
	    text(x, y, labels.id[ind], cex = cex.id, xpd = TRUE,
		 pos = labpos, offset = 0.25)
	}
    }
    getCaption <- function(k) # allow caption = "" , plotmath etc
        if(length(caption) < k) NA_character_ else as.graphicsAnnot(caption[[k]])

    if(is.null(sub.caption)) { ## construct a default:
	cal <- x$call
	if (!is.na(m.f <- match("formula", names(cal)))) {
	    cal <- cal[c(1, m.f)]
	    names(cal)[2L] <- "" # drop	" formula = "
	}
	cc <- deparse(cal, 80) # (80, 75) are ``parameters''
	nc <- nchar(cc[1L], "c")
	abbr <- length(cc) > 1 || nc > 75
	sub.caption <-
	    if(abbr) paste(substr(cc[1L], 1L, min(75L, nc)), "...") else cc[1L]
    }
    one.fig <- prod(par("mfcol")) == 1
    if (ask) {
	oask <- devAskNewPage(TRUE)
	on.exit(devAskNewPage(oask))
    }
    ##---------- Do the individual plots : ----------
    if (show[1L]) {
	ylim <- range(r, na.rm=TRUE)
	if(id.n > 0)
	    ylim <- extendrange(r= ylim, f = 0.08)
	plot(yh, r, xlab = l.fit, ylab = "Residuals", main = main,
	     ylim = ylim, type = "n", ...)
	panel(yh, r, ...)
	if (one.fig)
	    title(sub = sub.caption, ...)
	mtext(getCaption(1), 3, 0.25, cex = cex.caption)
	if(id.n > 0) {
	    y.id <- r[show.r]
	    y.id[y.id < 0] <- y.id[y.id < 0] - strheight(" ")/3
	    text.id(yh[show.r], y.id, show.r)
	}
	abline(h = 0, lty = 3, col = "gray")
    }
    if (show[2L]) { ## Normal
	ylim <- range(rs, na.rm=TRUE)
	ylim[2L] <- ylim[2L] + diff(ylim) * 0.075
	qq <- qqnorm(rs, main = main, ylab = ylab23, ylim = ylim, ...)
	if (qqline) qqline(rs, lty = 3, col = "gray50")
	if (one.fig)
	    title(sub = sub.caption, ...)
	mtext(getCaption(2), 3, 0.25, cex = cex.caption)
	if(id.n > 0)
	    text.id(qq$x[show.rs], qq$y[show.rs], show.rs)
    }
    if (show[3L]) {
	sqrtabsr <- sqrt(abs(rs))
	ylim <- c(0, max(sqrtabsr, na.rm=TRUE))
	yl <- as.expression(substitute(sqrt(abs(YL)), list(YL=as.name(ylab23))))
	yhn0 <- if(is.null(w)) yh else yh[w!=0]
	plot(yhn0, sqrtabsr, xlab = l.fit, ylab = yl, main = main,
	     ylim = ylim, type = "n", ...)
	panel(yhn0, sqrtabsr, ...)
	if (one.fig)
	    title(sub = sub.caption, ...)
	mtext(getCaption(3), 3, 0.25, cex = cex.caption)
	if(id.n > 0)
	    text.id(yhn0[show.rs], sqrtabsr[show.rs], show.rs)
    }
    if (show[4L]) {
	if(id.n > 0) {
	    show.r <- order(-cook)[iid]# index of largest 'id.n' ones
	    ymx <- cook[show.r[1L]] * 1.075
	} else ymx <- max(cook, na.rm = TRUE)
	plot(cook, type = "h", ylim = c(0, ymx), main = main,
	     xlab = "Obs. number", ylab = "Cook's distance", ...)
	if (one.fig)
	    title(sub = sub.caption, ...)
	mtext(getCaption(4), 3, 0.25, cex = cex.caption)
	if(id.n > 0)
	    text.id(show.r, cook[show.r], show.r, adj.x=FALSE)
    }
    if (show[5L]) {
        ylab5 <- if (isGlm) "Std. Pearson resid." else "Standardized residuals"
        r.w <- residuals(x, "pearson")
        if(!is.null(w)) r.w <- r.w[wind] # drop 0-weight cases
 	rsp <- dropInf( r.w/(s * sqrt(1 - hii)), hii )
	ylim <- range(rsp, na.rm = TRUE)
	if (id.n > 0) {
	    ylim <- extendrange(r= ylim, f = 0.08)
	    show.rsp <- order(-cook)[iid]
	}
        do.plot <- TRUE
        if(isConst.hat) { ## leverages are all the same
	    if(missing(caption)) # set different default
		caption[[5]] <- "Constant Leverage:\n Residuals vs Factor Levels"
            ## plot against factor-level combinations instead
            aterms <- attributes(terms(x))
            ## classes w/o response
            dcl <- aterms$dataClasses[ -aterms$response ]
            facvars <- names(dcl)[dcl %in% c("factor", "ordered")]
            mf <- model.frame(x)[facvars]# better than x$model
            if(ncol(mf) > 0) {
                ## now re-order the factor levels *along* factor-effects
                ## using a "robust" method {not requiring dummy.coef}:
                effM <- mf
                for(j in seq_len(ncol(mf)))
                    effM[, j] <- sapply(split(yh, mf[, j]), mean)[mf[, j]]
                ord <- do.call(order, effM)
                dm <- data.matrix(mf)[ord, , drop = FALSE]
                ## #{levels} for each of the factors:
                nf <- length(nlev <- unlist(unname(lapply(x$xlevels, length))))
                ff <- if(nf == 1) 1 else rev(cumprod(c(1, nlev[nf:2])))
                facval <- ((dm-1) %*% ff)
                ## now reorder to the same order as the residuals
                facval[ord] <- facval
                xx <- facval # for use in do.plot section.

                plot(facval, rsp, xlim = c(-1/2, sum((nlev-1) * ff) + 1/2),
                     ylim = ylim, xaxt = "n",
                     main = main, xlab = "Factor Level Combinations",
                     ylab = ylab5, type = "n", ...)
                axis(1, at = ff[1L]*(1L:nlev[1L] - 1/2) - 1/2,
                     labels= x$xlevels[[1L]][order(sapply(split(yh,mf[,1]), mean))])
                mtext(paste(facvars[1L],":"), side = 1, line = 0.25, adj=-.05)
                abline(v = ff[1L]*(0:nlev[1L]) - 1/2, col="gray", lty="F4")
                panel(facval, rsp, ...)
                abline(h = 0, lty = 3, col = "gray")
            }
	    else { # no factors
		message("hat values (leverages) are all = ",
                        format(mean(r.hat)),
			"\n and there are no factor predictors; no plot no. 5")
                frame()
                do.plot <- FALSE
            }
        }
        else { ## Residual vs Leverage
            xx <- hii
            ## omit hatvalues of 1.
            xx[xx >= 1] <- NA

            plot(xx, rsp, xlim = c(0, max(xx, na.rm = TRUE)), ylim = ylim,
                 main = main, xlab = "Leverage", ylab = ylab5, type = "n",
                 ...)
            panel(xx, rsp, ...)
            abline(h = 0, v = 0, lty = 3, col = "gray")
            if (one.fig)
                title(sub = sub.caption, ...)
            if(length(cook.levels)) {
                p <- length(coef(x))
                usr <- par("usr")
                hh <- seq.int(min(r.hat[1L], r.hat[2L]/100), usr[2L],
                              length.out = 101)
                for(crit in cook.levels) {
                    cl.h <- sqrt(crit*p*(1-hh)/hh)
                    lines(hh, cl.h, lty = 2, col = 2)
                    lines(hh,-cl.h, lty = 2, col = 2)
                }
                legend("bottomleft", legend = "Cook's distance",
                       lty = 2, col = 2, bty = "n")
                xmax <- min(0.99, usr[2L])
                ymult <- sqrt(p*(1-xmax)/xmax)
                aty <- c(-sqrt(rev(cook.levels))*ymult,
                         sqrt(cook.levels)*ymult)
                axis(4, at = aty,
                     labels = paste(c(rev(cook.levels), cook.levels)),
                     mgp = c(.25,.25,0), las = 2, tck = 0,
                     cex.axis = cex.id, col.axis = 2)
            }
        } # if(const h_ii) .. else ..
	if (do.plot) {
	    mtext(getCaption(5), 3, 0.25, cex = cex.caption)
	    if (id.n > 0) {
		y.id <- rsp[show.rsp]
		y.id[y.id < 0] <- y.id[y.id < 0] - strheight(" ")/3
		text.id(xx[show.rsp], y.id, show.rsp)
	    }
	}
    }
    if (show[6L]) {
	g <- dropInf( hii/(1-hii), hii )
	ymx <- max(cook, na.rm = TRUE)*1.025
	plot(g, cook, xlim = c(0, max(g, na.rm=TRUE)), ylim = c(0, ymx),
	     main = main, ylab = "Cook's distance",
             xlab = expression("Leverage  " * h[ii]),
	     xaxt = "n", type = "n", ...)
	panel(g, cook, ...)
        ## Label axis with h_ii values
	athat <- pretty(hii)
	axis(1, at = athat/(1-athat), labels = paste(athat))
	if (one.fig)
	    title(sub = sub.caption, ...)
	p <- length(coef(x))
	bval <- pretty(sqrt(p*cook/g), 5)

	usr <- par("usr")
	xmax <- usr[2L]
	ymax <- usr[4L]
	for(i in seq_along(bval)) {
	    bi2 <- bval[i]^2
	    if(ymax > bi2*xmax) {
		xi <- xmax + strwidth(" ")/3
		yi <- bi2*xi
		abline(0, bi2, lty = 2)
		text(xi, yi, paste(bval[i]), adj = 0, xpd = TRUE)
	    } else {
		yi <- ymax - 1.5*strheight(" ")
		xi <- yi/bi2
		lines(c(0, xi), c(0, yi), lty = 2)
		text(xi, ymax-0.8*strheight(" "), paste(bval[i]),
		     adj = 0.5, xpd = TRUE)
	    }
	}

	## axis(4, at=p*cook.levels, labels=paste(c(rev(cook.levels), cook.levels)),
	##	mgp=c(.25,.25,0), las=2, tck=0, cex.axis=cex.id)
	mtext(getCaption(6), 3, 0.25, cex = cex.caption)
	if (id.n > 0) {
	    show.r <- order(-cook)[iid]
            text.id(g[show.r], cook[show.r], show.r)
        }
    }

    if (!one.fig && par("oma")[3L] >= 1)
	mtext(sub.caption, outer = TRUE, cex = 1.25)
    invisible()
}
poisson.test <- function(x, T = 1, r = 1, alternative =
                         c("two.sided", "less", "greater"),
                         conf.level = 0.95)
{

    DNAME <- deparse(substitute(x))
    DNAME <- paste(DNAME, "time base:", deparse(substitute(T)))
    if ((l <- length(x)) != length(T))
        if (length(T) == 1)
            T <- rep(T, l)
        else
            stop("'x' and 'T' have incompatible length")
    xr <- round(x)

    if(any(!is.finite(x) | (x < 0)) || max(abs(x-xr)) > 1e-7)
        stop("'x' must be finite, nonnegative, and integer")
    x <- xr

    if(any(is.na(T) | (T < 0)))
        stop("'T' must be nonnegative")


    if ((k <- length(x)) < 1)
        stop("not enough data")

    if (k > 2)
        stop("The case k > 2 is unimplemented")

    if(!missing(r) && (length(r) > 1 || is.na(r) || r < 0 ))
        stop ("'r' must be a single positive number")
    alternative <- match.arg(alternative)


    if (k == 2) {

        RVAL <- binom.test(x, sum(x), r * T[1]/(r * T[1] + T[2]),
                           alternative=alternative)

        RVAL$data.name <- DNAME
        RVAL$statistic <- x[1]
        RVAL$parameter <- sum(x) * r * T[1]/sum(T * c(1, r))
        names(RVAL$statistic) <- c("count1")
        names(RVAL$parameter) <- c("expected count1")
        RVAL$estimate <- (x[1]/T[1])/(x[2]/T[2])
        names(RVAL$estimate) <- "rate ratio"
        pp <- RVAL$conf.int
        RVAL$conf.int <- pp/(1 - pp)*T[2]/T[1]
        names(r) <- "rate ratio"
        RVAL$null.value <- r

        RVAL$method <- "Comparison of Poisson rates"
        return (RVAL)
    } else {
        m <- r * T
        PVAL <- switch(alternative,
                       less = ppois(x, m),
                       greater = ppois(x - 1, m, lower.tail = FALSE),
                       two.sided = {
                           if(m == 0)
                               (x == 0)
                           else {
                               ## Do
                               ##   d <- dpois(0 : inf, r * T)
                               ##   sum(d[d <= dpois(x, r * T)])
                               ## a bit more efficiently ...
                               ## Note that we need a little fuzz.
                               relErr <- 1 + 1e-7
                               d <- dpois(x, r * T)
                               ## This is tricky: need to be sure
                               ## only to sum values in opposite tail
                               ## and not count x twice.

                               ## For the Poisson dist., the mode will
                               ## equal the mean if it is an integer.
                               if (x == m)
                                   1
                               else if (x < m) {
                                   ## Slightly trickier than in the binomial
                                   ## because we cannot use infinite-length i
                                   N <- ceiling(2 * m - x)
                                   while (dpois(N, m) > d)
                                       N <- 2 * N
                                   i <- seq.int(from = ceiling(m), to = N)
                                   y <- sum(dpois(i, m) <= d * relErr)
                                   ppois(x, m) +
                                       ppois(N - y, m, lower.tail = FALSE)
                               } else {
                                   i <- seq.int(from = 0, to = floor(m))
                                   y <- sum(dpois(i, m) <= d * relErr)
                                   ppois(y - 1, m) +
                                       ppois(x - 1, m, lower.tail = FALSE)
                               }
                           }
                       })
        ## Determine m s.t. Prob(Pois(m) >= x) = alpha.
        ## Use that for x > 0,
        ##   Prob(Pois >= x) = pgamma(m, x).
        p.L <- function(x, alpha) {
            if(x == 0)                      # No solution
                0
            else
                qgamma(alpha, x)
        }
        ## Determine p s.t. Prob(B(n,p) <= x) = alpha.
        ## Use that for x < n,
        ##   Prob(Pois(m) <= x) = 1 - pgamma(m, x + 1).

        p.U <- function(x, alpha)
            qgamma(1 - alpha, x + 1)

        CINT <- switch(alternative,
                       less = c(0, p.U(x, 1 - conf.level)),
                       greater = c(p.L(x, 1 - conf.level), Inf),
                       two.sided = {
                           alpha <- (1 - conf.level) / 2
                           c(p.L(x, alpha), p.U(x, alpha))
                       }) / T
        attr(CINT, "conf.level") <- conf.level

        ESTIMATE <- x / T

        names(x) <- "number of events"	# or simply "x" ??
        names(T) <- "time base"	# or simply "n" ??
        names(ESTIMATE) <-
            names(r) <- "event rate" # or simply "p" ??

        structure(list(statistic = x,
                       parameter = T,
                       p.value = PVAL,
                       conf.int = CINT,
                       estimate = ESTIMATE,
                       null.value = r,
                       alternative = alternative,
                       method = "Exact Poisson test",
                       data.name = DNAME),
                  class = "htest")

    }
}


### test cases:

## SMR, Welsh Nickel workers
## poisson.test(137, 24.19893)

## eba1977, compare Fredericia to other three cities for ages 55-59
## poisson.test(c(11,6+8+7),c(800, 1083+1050+878))
#  File src/library/stats/R/power.R
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

power.t.test <-
    function(n=NULL, delta=NULL, sd=1, sig.level=0.05, power=NULL,
	     type=c("two.sample", "one.sample", "paired"),
	     alternative=c("two.sided", "one.sided"), strict=FALSE)
{
    if ( sum(sapply(list(n, delta, sd, power, sig.level), is.null)) != 1 )
	stop("exactly one of 'n', 'delta', 'sd', 'power', and 'sig.level' must be NULL")
    if(!is.null(sig.level) && !is.numeric(sig.level) ||
       any(0 > sig.level | sig.level > 1))
	stop("'sig.level' must be numeric in [0, 1]")

    type <- match.arg(type)
    alternative <- match.arg(alternative)

    tsample <- switch(type, one.sample = 1, two.sample = 2, paired = 1)
    tside <- switch(alternative, one.sided = 1, two.sided = 2)
    if (tside == 2 && !is.null(delta)) delta <- abs(delta)

    p.body <- quote({nu <- (n - 1) * tsample
		     pt(qt(sig.level/tside, nu, lower.tail = FALSE),
			nu, ncp = sqrt(n/tsample) * delta/sd, lower.tail = FALSE)})
    if (strict & tside == 2) # count rejections in opposite tail
      p.body <- quote({
	  nu <- (n - 1) * tsample
	  qu<-qt(sig.level/tside, nu, lower.tail = FALSE)
	  pt(qu, nu, ncp = sqrt(n/tsample) * delta/sd, lower.tail = FALSE) +
	    pt(-qu, nu, ncp = sqrt(n/tsample) * delta/sd, lower.tail = TRUE)
      })

    if (is.null(power))
	power <- eval(p.body)
    else if (is.null(n))
	n <- uniroot(function(n) eval(p.body) - power,
		     c(2,1e7))$root
    else if (is.null(sd))
	sd <- uniroot(function(sd) eval(p.body) - power,
		      delta * c(1e-7,1e+7))$root
    else if (is.null(delta))
	delta <- uniroot(function(delta) eval(p.body) - power,
		      sd * c(1e-7,1e+7))$root
    else if (is.null(sig.level))
	sig.level <- uniroot(function(sig.level) eval(p.body) - power,
		      c(1e-10,1-1e-10))$root
    else # Shouldn't happen
	stop("internal error")
    NOTE <- switch(type,
		   paired = "n is number of *pairs*, sd is std.dev. of *differences* within pairs",
		   two.sample = "n is number in *each* group", NULL)

    METHOD <- paste(switch(type,
			   one.sample = "One-sample",
			   two.sample = "Two-sample",
			   paired = "Paired"),
		    "t test power calculation")

    structure(list(n=n, delta=delta, sd=sd,
		   sig.level=sig.level, power=power,
		   alternative=alternative, note=NOTE, method=METHOD),
	      class="power.htest")
}

power.prop.test <-
    function(n=NULL, p1=NULL, p2=NULL, sig.level=0.05, power=NULL,
	     alternative=c("two.sided", "one.sided"), strict=FALSE)
{
    if ( sum(sapply(list(n, p1, p2, power, sig.level), is.null)) != 1 )
	stop("exactly one of 'n', 'p1', 'p2', 'power', and 'sig.level' must be NULL")
    if(!is.null(sig.level) && !is.numeric(sig.level) ||
       any(0 > sig.level | sig.level > 1))
	stop("'sig.level' must be numeric in [0, 1]")

    alternative <- match.arg(alternative)

    tside <- switch(alternative, one.sided = 1, two.sided = 2)

    p.body <- quote(pnorm(((sqrt(n) * abs(p1 - p2)
			    - (qnorm(sig.level/tside, lower.tail = FALSE)
			       * sqrt((p1 + p2) * (1 - (p1 + p2)/2))))
			   / sqrt(p1 * (1 - p1) + p2 * (1 - p2)))))

    if (strict & tside == 2) # count rejections in opposite tail
      p.body <- quote({
	  qu <- qnorm(sig.level/tside, lower.tail = FALSE)
	  d <- abs(p1 - p2)
	  q1 <- 1 - p1
	  q2 <- 1 - p2
	  pbar <- (p1 + p2)/2
	  qbar <- 1 - pbar
	  v1 <- p1 * q1
	  v2 <- p2 * q2
	  vbar <- pbar * qbar
	  pnorm((sqrt(n)*d - qu * sqrt(2 * vbar) )
		/ sqrt(v1 + v2)) +
	  pnorm((sqrt(n)*d + qu * sqrt(2 * vbar) )
		/ sqrt(v1 + v2), lower.tail=FALSE)
      })
    if (is.null(power))
	power <- eval(p.body)
    else if (is.null(n))
	n <- uniroot(function(n) eval(p.body) - power,
		     c(1,1e7))$root
    else if (is.null(p1))
	p1 <- uniroot(function(p1) eval(p.body) - power,
		      c(0,p2))$root
    else if (is.null(p2))
	p2 <- uniroot(function(p2) eval(p.body) - power,
		      c(p1,1))$root
    else if (is.null(sig.level))
	sig.level <- uniroot(function(sig.level) eval(p.body) - power,
		      c(1e-10,1-1e-10))$root
    else # Shouldn't happen
	stop("internal error")

    NOTE <- "n is number in *each* group"

    METHOD <-  "Two-sample comparison of proportions power calculation"

    structure(list(n=n, p1=p1, p2=p2,
		   sig.level=sig.level, power=power,
		   alternative=alternative, note=NOTE, method=METHOD),
	      class="power.htest")
}

print.power.htest <-
function(x, ...)
{
    cat("\n    ", x$method, "\n\n")
    note <- x$note
    x[c("method","note")] <- NULL
    cat(paste(format(names(x), width= 15, justify = "right"),
	      format(x), sep= " = "), sep= "\n")
    if(!is.null(note))
	cat("\n", "NOTE:", note, "\n\n")
    else
	cat("\n")
    invisible(x)
}
#  File src/library/stats/R/power.anova.test.R
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

power.anova.test <-
function (groups = NULL, n = NULL, between.var = NULL, within.var = NULL,
	  sig.level = 0.05, power = NULL)
{
    ## Check parameters
    if (sum(sapply(list(groups, n, between.var, within.var, power, sig.level),
		   is.null)) != 1)
	stop("exactly one of 'groups', 'n', 'between.var', 'within.var', 'power', and 'sig.level' must be NULL")
    if (!is.null(groups) && groups < 2)
      stop("number of groups must be at least 2")
    if (!is.null(n) && n < 2)
      stop("number of observations in each group must be at least 2")
    if(!is.null(sig.level) && !is.numeric(sig.level) ||
       any(0 > sig.level | sig.level > 1))
	stop("'sig.level' must be numeric in [0, 1]")

    p.body <- quote({
	lambda <- (groups-1)*n*(between.var/within.var)
	pf(qf(sig.level, groups-1, (n-1)*groups, lower.tail = FALSE),
	   groups-1, (n-1)*groups, lambda, lower.tail = FALSE)
    })

    if (is.null(power))
	power <- eval(p.body)
    else if (is.null(groups))
	groups <- uniroot(function(groups) eval(p.body) - power,
			  c(2, 1e+02))$root
    else if (is.null(n))
	n <- uniroot(function(n) eval(p.body) - power, c(2, 1e+05))$root
    else if (is.null(within.var))
	within.var <- uniroot(function(within.var) eval(p.body) - power,
			      between.var * c(1e-07, 1e+07))$root
    else if (is.null(between.var))
	between.var <- uniroot(function(between.var) eval(p.body) - power,
			       within.var * c(1e-07, 1e+07))$root
    else if (is.null(sig.level))
	sig.level <- uniroot(function(sig.level) eval(p.body) - power,
			     c(1e-10, 1 - 1e-10))$root
    else stop("internal error")
    NOTE <- "n is number in each group"
    METHOD <- "Balanced one-way analysis of variance power calculation"
    structure(list(groups = groups, n = n, between.var = between.var,
		   within.var = within.var, sig.level = sig.level,
		   power = power, note = NOTE, method = METHOD),
	      class = "power.htest")
}
#  File src/library/stats/R/ppoints.R
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

ppoints <- function (n, a = ifelse(n <= 10, 3/8, 1/2))
{
    if(length(n) > 1) n <- length(n)
    if(n > 0)
	(1L:n - a)/(n + 1-2*a)
    else numeric(0L)
}
#  File src/library/stats/R/ppr.R
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

## file stats/R/ppr.R
## copyright (C) 1998 B. D. Ripley
## Copyright (C) 2000-3 The R Development Core Team
## This version distributed under GPL (version 2 or later)

ppr <- function(x, ...) UseMethod("ppr")

ppr.formula <-
function(formula, data, weights, subset,
	 na.action, contrasts = NULL, ..., model = FALSE)
{
    call <- match.call()
    m <- match.call(expand.dots = FALSE)
    m$contrasts <- m$... <- NULL
    m[[1L]] <- as.name("model.frame")
    m <- eval(m, parent.frame())
    Terms <- attr(m, "terms")
    attr(Terms, "intercept") <- 0L
    X <- model.matrix(Terms, m, contrasts)
    Y <- model.response(m)
    w <- model.weights(m)
    if(length(w) == 0) w <- rep(1, nrow(X))
    fit <- ppr.default(X, Y, w, ...)
    fit$na.action <- attr(m, "na.action")
    fit$terms <- Terms
    ## fix up call to refer to the generic, but leave arg name as `formula'
    call[[1L]] <- as.name("ppr")
    fit$call <- call
    fit$contrasts <- attr(X, "contrasts")
    fit$xlevels <- .getXlevels(Terms, m)
    if(model) fit$model <- m
    structure(fit, class=c("ppr.form", "ppr"))
}

ppr.default <-
function(x, y, weights=rep(1,n), ww=rep(1,q), nterms, max.terms=nterms,
	 optlevel=2, sm.method=c("supsmu", "spline", "gcvspline"),
	 bass=0, span=0, df=5, gcvpen=1, ...)
{
    call <- match.call()
    call[[1L]] <- as.name("ppr")
    sm.method <- match.arg(sm.method)
    ism <- switch(sm.method, supsmu=0, spline=1, gcvspline=2)
    if(missing(nterms)) stop("'nterms' is missing with no default")
    mu <- nterms; ml <- max.terms
    x <- as.matrix(x)
    y <- as.matrix(y)
    if(!is.numeric(x) || !is.numeric(y))
        stop("'ppr' applies only to numerical variables")
    n <- nrow(x)
    if(nrow(y) != n) stop("mismatched 'x' and 'y'")
    p <- ncol(x)
    q <- ncol(y)
    if(!is.null(dimnames(x))) xnames <- dimnames(x)[[2L]]
    else xnames <- paste("X", 1L:p, sep="")
    if(!is.null(dimnames(y))) ynames <- dimnames(y)[[2L]]
    else ynames <- paste("Y", 1L:q, sep="")
    msmod <- ml*(p+q+2*n)+q+7+ml+1	# for asr
    nsp <- n*(q+15)+q+3*p
    ndp <- p*(p+1)/2+6*p
    .Fortran(R_setppr,
	     as.double(span), as.double(bass), as.integer(optlevel),
	     as.integer(ism), as.double(df), as.double(gcvpen)
	     )
    Z <- .Fortran(R_smart,
		  as.integer(ml), as.integer(mu),
		  as.integer(p), as.integer(q), as.integer(n),
		  as.double(weights),
		  as.double(t(x)),
		  as.double(t(y)),
		  as.double(ww),
		  smod=double(msmod), as.integer(msmod),
		  double(nsp), as.integer(nsp),
		  double(ndp), as.integer(ndp),
		  edf=double(ml)
		  )
    smod <- Z$smod
    ys <- smod[q+6]
    tnames <- paste("term", 1L:mu)
    alpha <- matrix(smod[q+6L + 1L:(p*mu)],p, mu,
		    dimnames=list(xnames, tnames))
    beta <- matrix(smod[q+6L+p*ml + 1L:(q*mu)], q, mu,
		   dimnames=list(ynames, tnames))
    fitted <- drop(matrix(.Fortran(R_pppred,
				   as.integer(nrow(x)),
				   as.double(x),
				   as.double(smod),
				   y = double(nrow(x)*q),
				   double(2*smod[4L]))$y,
			  ncol=q, dimnames=dimnames(y)))
    jt <- q + 7 + ml*(p+q+2*n)
    gof <- smod[jt] * n * ys^2
    gofn <- smod[jt+1L:ml] * n * ys^2
    ## retain only terms for the size of model finally fitted
    jf <- q+6+ml*(p+q)
    smod <- smod[c(1L:(q+6+p*mu), q+6+p*ml + 1L:(q*mu),
		   jf + 1L:(mu*n), jf+ml*n + 1L:(mu*n))]
    smod[1L] <- mu
    structure(list(call=call, mu=mu, ml=ml, p=p, q=q,
		   gof=gof, gofn=gofn,
		   df=df, edf=Z$edf[1L:mu],
		   xnames=xnames, ynames=ynames,
		   alpha=drop(alpha), beta=ys*drop(beta),
		   yb=smod[5+1L:q], ys=ys,
		   fitted.values=fitted, residuals=drop(y-fitted),
		   smod=smod),
	      class="ppr")
}

print.ppr <- function(x, ...)
{
    if(!is.null(cl <- x$call)) {
	cat("Call:\n")
	dput(cl, control=NULL)
    }
    mu <- x$mu; ml <- x$ml
    cat("\nGoodness of fit:\n")
    gof <- x$gofn; names(gof) <- paste(1L:ml, "terms")
    print(format(gof[mu:ml], ...), quote=FALSE)
    invisible(x)
}

summary.ppr <- function(object, ...)
{
    class(object) <- "summary.ppr"
    object
}

print.summary.ppr <- function(x, ...)
{
    print.ppr(x, ...)
    mu <- x$mu
    cat("\nProjection direction vectors:\n")
    print(format(x$alpha, ...), quote=FALSE)
    cat("\nCoefficients of ridge terms:\n")
    print(format(x$beta, ...), quote=FALSE)
    if(any(x$edf >0)) {
	cat("\nEquivalent df for ridge terms:\n")
	edf <- x$edf; names(edf) <- paste("term", 1L:mu)
	print(round(edf,2), ...)
    }
    invisible(x)
}

plot.ppr <- function(x, ask, type="o", ...)
{
    ppr.funs <- function(obj)
    {
	## cols for each term
	p <- obj$p; q <- obj$q
	sm <- obj$smod
	n <- sm[4L]; mu <- sm[5]; m <- sm[1L]
	jf <- q+6+m*(p+q)
	jt <- jf+m*n
	f <- matrix(sm[jf+1L:(mu*n)],n, mu)
	t <- matrix(sm[jt+1L:(mu*n)],n, mu)
	list(x=t, y=f)
    }
    obj <- ppr.funs(x)
    if(!missing(ask)) {
        oask <- devAskNewPage(ask)
        on.exit(devAskNewPage(oask))
    }
    for(i in 1L:x$mu) {
	ord <- order(obj$x[ ,i])
	plot(obj$x[ord, i], obj$y[ord, i], type = type,
	     xlab = paste("term", i), ylab = "", ...)
    }
    invisible()
}

predict.ppr <- function(object, newdata, ...)
{
    if(missing(newdata)) return(fitted(object))
    if(!is.null(object$terms)) {
        newdata <- as.data.frame(newdata)
        rn <- row.names(newdata)
# work hard to predict NA for rows with missing data
        Terms <- delete.response(object$terms)
        m <- model.frame(Terms, newdata, na.action = na.omit,
                         xlev = object$xlevels)
        if(!is.null(cl <- attr(Terms, "dataClasses"))) .checkMFClasses(cl, m)
        keep <- match(row.names(m), rn)
        x <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
    } else {
        x <- as.matrix(newdata)
        keep <- 1L:nrow(x)
        rn <- dimnames(x)[[1L]]
    }
    if(ncol(x) != object$p) stop("wrong number of columns in 'x'")
    res <- matrix(NA, length(keep), object$q,
                  dimnames = list(rn, object$ynames))
    res[keep, ] <- matrix(.Fortran(R_pppred,
                                   as.integer(nrow(x)),
                                   as.double(x),
                                   as.double(object$smod),
                                   y = double(nrow(x)*object$q),
                                   double(2*object$smod[4L])
                                   )$y, ncol=object$q)
    drop(res)
}
#  File src/library/stats/R/prcomp.R
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

prcomp <- function (x, ...) UseMethod("prcomp")

prcomp.default <-
    function(x, retx = TRUE, center = TRUE, scale. = FALSE, tol = NULL, ...)
{
    x <- as.matrix(x)
    x <- scale(x, center = center, scale = scale.)
    cen <- attr(x, "scaled:center")
    sc <- attr(x, "scaled:scale")
    if(any(sc == 0))
        stop("cannot rescale a constant/zero column to unit variance")
    s <- svd(x, nu = 0)
    s$d <- s$d / sqrt(max(1, nrow(x) - 1))
    if (!is.null(tol)) {
        ## we get rank at least one even for a 0 matrix.
        rank <- sum(s$d > (s$d[1L]*tol))
        if (rank < ncol(x)) {
            s$v <- s$v[, 1L:rank, drop = FALSE]
            s$d <- s$d[1L:rank]
        }
    }
    dimnames(s$v) <-
        list(colnames(x), paste("PC", seq_len(ncol(s$v)), sep = ""))
    r <- list(sdev = s$d, rotation = s$v,
              center = if(is.null(cen)) FALSE else cen,
              scale = if(is.null(sc)) FALSE else sc)
    if (retx) r$x <- x %*% s$v
    class(r) <- "prcomp"
    r
}

prcomp.formula <- function (formula, data = NULL, subset, na.action, ...)
{
    mt <- terms(formula, data = data)
    if (attr(mt, "response") > 0L)
        stop("response not allowed in formula")
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    mf$... <- NULL
    mf[[1L]] <- as.name("model.frame")
    mf <- eval.parent(mf)
    ## this is not a `standard' model-fitting function,
    ## so no need to consider contrasts or levels
    if (.check_vars_numeric(mf))
        stop("PCA applies only to numerical variables")
    na.act <- attr(mf, "na.action")
    mt <- attr(mf, "terms")
    attr(mt, "intercept") <- 0L
    x <- model.matrix(mt, mf)
    res <- prcomp.default(x, ...)
    ## fix up call to refer to the generic, but leave arg name as `formula'
    cl[[1L]] <- as.name("prcomp")
    res$call <- cl
    if (!is.null(na.act)) {
        res$na.action <- na.act
        if (!is.null(sc <- res$x))
            res$x <- napredict(na.act, sc)
    }
    res
}

plot.prcomp <- function(x, main = deparse(substitute(x)), ...)
    screeplot.default(x, main = main, ...)

print.prcomp <- function(x, print.x = FALSE, ...) {
    cat("Standard deviations:\n")
    print(x$sdev, ...)
    cat("\nRotation:\n")
    print(x$rotation, ...)
    if (print.x && length(x$x)) {
        cat("\nRotated variables:\n")
        print(x$x, ...)
    }
    invisible(x)
}

summary.prcomp <- function(object, ...)
{
    vars <- object$sdev^2
    vars <- vars/sum(vars)
    importance <- rbind("Standard deviation" = object$sdev,
                        "Proportion of Variance" = round(vars, 5),
                        "Cumulative Proportion" = round(cumsum(vars), 5))
    colnames(importance) <- colnames(object$rotation)
    object$importance <- importance
    class(object) <- "summary.prcomp"
    object
}

print.summary.prcomp <-
function(x, digits = min(3, getOption("digits")-3), ...)
{
    cat("Importance of components:\n")
    print(x$importance, digits = digits)
    invisible(x)
}

predict.prcomp <- function(object, newdata, ...)
{
    if (missing(newdata)) {
        if(!is.null(object$x)) return(object$x)
        else stop("no scores are available: refit with 'retx=TRUE'")
    }
    if(length(dim(newdata)) != 2L)
        stop("'newdata' must be a matrix or data frame")
    nm <- rownames(object$rotation)
    if(!is.null(nm)) {
        if(!all(nm %in% colnames(newdata)))
            stop("'newdata' does not have named columns matching one or more of the original columns")
        newdata <- newdata[, nm, drop = FALSE]
    } else {
        if(NCOL(newdata) != NROW(object$rotation) )
            stop("'newdata' does not have the correct number of columns")
    }
    ## next line does as.matrix
    scale(newdata, object$center, object$scale) %*% object$rotation
}

.check_vars_numeric <- function(mf)
{
    ## we need to test just the columns which are actually used.
    mt <- attr(mf, "terms")
    mterms <- attr(mt, "factors")
    mterms <- rownames(mterms)[apply(mterms, 1L, function(x) any(x > 0L))]
    any(sapply(mterms, function(x) is.factor(mf[,x]) || !is.numeric(mf[,x])))
}
#  File src/library/stats/R/predict.R
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

predict <- function(object,...) UseMethod("predict")

## This is not used anywhere anymore, is it ?
## It would only work with objects very much like  "lm", would it?
if(FALSE)
predict.default <- function (object, ...) {
    namelist <- list(...)
    names(namelist) <- substitute(list(...))[-1L]
    m <- length(namelist)
    X <- as.matrix(namelist[[1L]])
    if (m > 1)
	for (i in (2:m)) X <- cbind(X, namelist[[i]])
    if (object$intercept)
	X <- cbind(rep(1, NROW(X)), X)
    k <- NCOL(X)
    n <- NROW(X)
    if (length(object$coefficients) != k)
	stop("wrong number of predictors")
    predictor <- X %*% object$coefficients
    ip <- numeric(n)
    names(ip) <- paste("P", 1L:n, sep = "")
    for (i in 1L:n)
	ip[i] <- sum(X[i, ] * (object$covmat %*% X[i, ]))
    stderr1 <- sqrt(ip)
    stderr2 <- sqrt(object$rms^2 + ip)
    tt <- qt(0.975, object$df)
    predictor + tt * cbind(Predicted=0,
                           "Conf lower"=-stderr1, "Conf upper"=stderr1,
                           "Pred lower"=-stderr2, "Pred upper"=stderr2)
}
#  File src/library/stats/R/predict.glm.R
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

predict.glm <-
  function(object, newdata = NULL, type = c("link", "response", "terms"),
           se.fit = FALSE, dispersion = NULL, terms=NULL,
           na.action = na.pass, ...)
{
    ## 1998/06/23 KH:  predict.lm() now merged with the version in lm.R

    type <- match.arg(type)
    na.act <- object$na.action
    object$na.action <- NULL # kill this for predict.lm calls
    if (!se.fit) {
	## No standard errors
	if(missing(newdata)) {
	    pred <- switch(type,
			   link = object$linear.predictors,
			   response = object$fitted.values,
                           terms = predict.lm(object,  se.fit=se.fit,
                               scale = 1, type="terms", terms=terms)
                           )
            if(!is.null(na.act)) pred <- napredict(na.act, pred)
	} else {
	    pred <- predict.lm(object, newdata, se.fit, scale = 1,
                               type = ifelse(type=="link", "response", type),
                               terms = terms, na.action = na.action)
	    switch(type,
		   response = {pred <- family(object)$linkinv(pred)},
		   link =, terms= )
          }
    } else {
	## summary.survreg has no ... argument.
	if(inherits(object, "survreg")) dispersion <- 1.
	if(is.null(dispersion) || dispersion == 0)
	    dispersion <- summary(object, dispersion=dispersion)$dispersion
	residual.scale <- as.vector(sqrt(dispersion))
	pred <- predict.lm(object, newdata, se.fit, scale = residual.scale,
                           type = ifelse(type=="link", "response", type),
                           terms = terms, na.action = na.action)
	fit <- pred$fit
	se.fit <- pred$se.fit
	switch(type,
	       response = {
		   se.fit <- se.fit * abs(family(object)$mu.eta(fit))
		   fit <- family(object)$linkinv(fit)
	       },
	       link =, terms=)
        if( missing(newdata) && !is.null(na.act) ) {
            fit <- napredict(na.act, fit)
            se.fit <- napredict(na.act, se.fit)
        }
	pred <- list(fit=fit, se.fit=se.fit, residual.scale=residual.scale)
    }
    pred
}
#  File src/library/stats/R/princomp-add.R
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

predict.princomp <- function(object, newdata, ...)
{
    if (missing(newdata)) return(object$scores)
    if(length(dim(newdata)) != 2)
        stop("'newdata' must be a matrix or data frame")
    p <- NCOL(object$loadings)
    nm <- rownames(object$loadings)
    if(!is.null(nm)) {
        if(!all(nm %in% colnames(newdata)))
            stop("'newdata' does not have named columns matching one or more of the original columns")
        newdata <- newdata[, nm]
    } else {
        if(NCOL(newdata) != p)
            stop("'newdata' does not have the correct number of columns")
    }
    ## next line does as.matrix
    scale(newdata, object$center, object$scale) %*% object$loadings
}

summary.princomp <- function(object, loadings = FALSE, cutoff = 0.1, ...)
{
    object$cutoff <- cutoff
    object$print.loadings <- loadings
    class(object) <- "summary.princomp"
    object
}

print.summary.princomp <-
    function(x, digits = 3, loadings = x$print.loadings, cutoff = x$cutoff,
             ...)
{
    vars <- x$sdev^2
    vars <- vars/sum(vars)
    cat("Importance of components:\n")
    print(rbind("Standard deviation" = x$sdev,
                "Proportion of Variance" = vars,
                "Cumulative Proportion" = cumsum(vars)))
    if(loadings) {
        cat("\nLoadings:\n")
        cx <- format(round(x$loadings, digits = digits))
        cx[abs(x$loadings) < cutoff] <-
            paste(rep(" ", nchar(cx[1,1], type="w")), collapse="")
        print(cx, quote = FALSE, ...)
    }
    invisible(x)
}

plot.princomp <- function(x, main = deparse(substitute(x)), ...)
  screeplot.default(x, main = main, ...)

screeplot <- function(x, ...) UseMethod("screeplot")

screeplot.default <-
function(x, npcs = min(10, length(x$sdev)),
         type = c("barplot", "lines"),
         main = deparse(substitute(x)), ...)
{
    main
    type <- match.arg(type)
    pcs <- x$sdev^2
    xp <- seq_len(npcs)
    if(type=="barplot")
        barplot(pcs[xp], names.arg = names(pcs[xp]), main = main,
                ylab = "Variances", ...)
    else {
        plot(xp, pcs[xp], type = "b", axes = FALSE, main = main,
             xlab = "", ylab = "Variances", ...)
        axis(2)
        axis(1, at = xp, labels = names(pcs[xp]))
    }
    invisible()
}

loadings <- function(x) x$loadings
#  File src/library/stats/R/princomp.R
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

princomp <- function(x, ...) UseMethod("princomp")

## use formula to allow update() to be used.
princomp.formula <- function(formula, data = NULL, subset, na.action, ...)
{
    mt <- terms(formula, data = data)
    if(attr(mt, "response") > 0) stop("response not allowed in formula")
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    mf$... <- NULL
    mf[[1L]] <- as.name("model.frame")
    mf <- eval.parent(mf)
    ## this is not a `standard' model-fitting function,
    ## so no need to consider contrasts or levels
    if (.check_vars_numeric(mf))
         stop("PCA applies only to numerical variables")
    na.act <- attr(mf, "na.action")
    mt <- attr(mf, "terms") # allow model.frame to update it
    attr(mt, "intercept") <- 0
    x <- model.matrix(mt, mf)
    res <- princomp.default(x, ...)
    ## fix up call to refer to the generic, but leave arg name as `formula'
    cl[[1L]] <- as.name("princomp")
    res$call <- cl
    if(!is.null(na.act)) {
        res$na.action <- na.act # not currently used
        if(!is.null(sc <- res$scores))
            res$scores <- napredict(na.act, sc)
    }
    res
}

princomp.default <-
    function(x, cor = FALSE, scores = TRUE, covmat = NULL,
             subset = rep(TRUE, nrow(as.matrix(x))), ...)
{
    cl <- match.call()
    cl[[1L]] <- as.name("princomp")
    if(!missing(x) && !missing(covmat))
        warning("both 'x' and 'covmat' were supplied: 'x' will be ignored")
    z <- if(!missing(x)) as.matrix(x)[subset, , drop = FALSE]
    if (is.list(covmat)) {
        if(any(is.na(match(c("cov", "n.obs"), names(covmat)))))
            stop("'covmat' is not a valid covariance list")
        cv <- covmat$cov
        n.obs <- covmat$n.obs
        cen <- covmat$center
    } else if(is.matrix(covmat)) {
        cv <- covmat
        n.obs <- NA
        cen <- NULL
    } else if(is.null(covmat)){
        dn <- dim(z)
        if(dn[1L] < dn[2L])
            stop("'princomp' can only be used with more units than variables")
        covmat <- cov.wt(z)             # returns list, cov() does not
        n.obs <- covmat$n.obs
        cv <- covmat$cov * (1 - 1/n.obs)# for S-PLUS compatibility
        cen <- covmat$center
    } else stop("'covmat' is of unknown type")
    if(!is.numeric(cv)) stop("PCA applies only to numerical variables")
    if (cor) {
        sds <- sqrt(diag(cv))
        if(any(sds == 0))
            stop("cannot use cor=TRUE with a constant variable")
        cv <- cv/(sds %o% sds)
    }
    edc <- eigen(cv, symmetric = TRUE)
    ev <- edc$values
    if (any(neg <- ev < 0)) { # S-PLUS sets all := 0
        ## 9 * : on Solaris found case where 5.59 was needed (MM)
        if (any(ev[neg] < - 9 * .Machine$double.eps * ev[1L]))
            stop("covariance matrix is not non-negative definite")
        else
            ev[neg] <- 0
    }
    cn <- paste("Comp.", 1L:ncol(cv), sep = "")
    names(ev) <- cn
    dimnames(edc$vectors) <- if(missing(x))
        list(dimnames(cv)[[2L]], cn) else list(dimnames(x)[[2L]], cn)
    sdev <- sqrt(ev)
    sc <- if (cor) sds else rep(1, ncol(cv))
    names(sc) <- colnames(cv)
    scr <- if (scores && !missing(x) && !is.null(cen))
        scale(z, center = cen, scale = sc) %*% edc$vectors
    if (is.null(cen)) cen <- rep(NA_real_, nrow(cv))
    edc <- list(sdev = sdev,
                loadings = structure(edc$vectors, class="loadings"),
                center = cen, scale = sc, n.obs = n.obs,
                scores = scr, call = cl)
    ## The Splus function also return list elements factor.sdev,
    ## correlations and coef, but these are not documented in the help.
    ## coef seems to equal load.  The Splus function also returns list
    ## element terms which is not supported here.
    class(edc) <- "princomp"
    edc
}

print.princomp <- function(x, ...)
{
    cat("Call:\n"); dput(x$call, control=NULL)
    cat("\nStandard deviations:\n")
    print(x$sdev, ...)
    cat("\n", length(x$scale), " variables and ", x$n.obs,
        "observations.\n")
    invisible(x)
}
#  File src/library/stats/R/profile.R
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

profile <- function(fitted, ...) UseMethod("profile")
#  File src/library/stats/R/proj.R
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

proj <- function(object, ...) UseMethod("proj")

proj.default <- function(object, onedf = TRUE, ...)
{
    if(!is.qr(object$qr))
	stop("argument does not include a 'qr' component")
    if(is.null(object$effects))
	stop("argument does not include an 'effects' component")
    RB <- c(object$effects[seq(object$rank)],
	    rep.int(0, nrow(object$qr$qr) - object$rank))
    prj <- as.matrix(qr.Q(object$qr, Dvec = RB))
    DN <- dimnames(object$qr$qr)
    dimnames(prj) <- list(DN[[1L]], DN[[2L]][seq(ncol(prj))])
    prj
}

proj.lm <- function(object, onedf = FALSE, unweighted.scale = FALSE, ...)
{
    if(inherits(object, "mlm"))
	stop("'proj' is not implemented for \"mlm\" fits")
    rank <- object$rank
    if(rank > 0) {
	prj <- proj.default(object, onedf = TRUE)[, 1L:rank, drop = FALSE]
	if(onedf) {
	    df <- rep.int(1, rank)
	    result <- prj
	} else {
	    asgn <- object$assign[object$qr$pivot[1L:object$rank]]
	    uasgn <- unique(asgn)
	    nmeffect <- c("(Intercept)",
			  attr(object$terms, "term.labels"))[1 + uasgn]
	    nterms <- length(uasgn)
	    df <- vector("numeric", nterms)
	    result <- matrix(0, length(object$residuals), nterms)
	    dimnames(result) <- list(rownames(object$fitted.values), nmeffect)
	    for(i in seq_along(uasgn)) {
		select <- (asgn == uasgn[i])
		df[i] <- sum(select)
		result[, i] <- prj[, select, drop = FALSE] %*% rep.int(1, df[i])
	    }
	}
    } else {
	result <- NULL
	df <- NULL
    }
    if(!is.null(wt <- object$weights) && unweighted.scale)
	result <- result/sqrt(wt)
    use.wt <- !is.null(wt) && !unweighted.scale
    if(object$df.residual > 0) {
	if(!is.matrix(result)) {
	    if(use.wt) result <- object$residuals * sqrt(wt)
	    else result <- object$residuals
	    result <- matrix(result, length(result), 1L, dimnames
			     = list(names(result), "Residuals"))
	} else {
	    dn <- dimnames(result)
	    d <- dim(result)
	    result <- c(result, if(use.wt) object$residuals * sqrt(wt)
			else object$residuals)
	    dim(result) <- d + c(0, 1)
	    dn[[1L]] <- names(object$residuals)
	    names(result) <- NULL
	    dn[[2L]] <- c(dn[[2L]], "Residuals")
	    dimnames(result) <- dn
	}
	df <- c(df, object$df.residual)
    }
    names(df) <- colnames(result)
    attr(result, "df") <- df
    attr(result, "formula") <- object$call$formula
    attr(result, "onedf") <- onedf
    if(!is.null(wt)) attr(result, "unweighted.scale") <- unweighted.scale
    result
}

proj.aov <- function(object, onedf = FALSE, unweighted.scale = FALSE, ...)
{
    if(inherits(object, "maov"))
	stop("'proj' is not implemented for multiple responses")
    factors.aov <- function(pnames, tfactor)
    {
	if(!is.na(int <- match("(Intercept)", pnames)))
	    pnames <- pnames[ - int]
	tnames <- lapply(colnames(tfactor), function(x, mat)
			 rownames(mat)[mat[, x] > 0], tfactor)
	names(tnames) <- colnames(tfactor)
	if(!is.na(match("Residuals", pnames))) {
	    enames <- c(rownames(tfactor)
			[as.logical(tfactor %*% rep.int(1, ncol(tfactor)))],
			"Within")
	    tnames <- append(tnames, list(Residuals = enames))
	}
	result <- tnames[match(pnames, names(tnames))]
	if(!is.na(int)) result <- c("(Intercept)" = "(Intercept)", result)
	## should reorder result, but probably OK
	result
    }
    projections <- NextMethod("proj")
    t.factor <- attr(terms(object), "factors")
    attr(projections, "factors") <-
	factors.aov(colnames(projections), t.factor)
    attr(projections, "call") <- object$call
    attr(projections, "t.factor") <- t.factor
    class(projections) <- "aovproj"
    projections
}


proj.aovlist <- function(object, onedf = FALSE, unweighted.scale = FALSE, ...)
{
    attr.xdim <- function(x)
    {
	## all attributes except names, dim and dimnames
	atrf <- attributes(x)
	atrf[is.na(match(names(atrf), c("names", "dim", "dimnames")))]
    }
    "attr.assign<-" <- function(x, value)
    {
	## assign to x all attributes in attr.x
	##    attributes(x)[names(value)] <- value not allowed in R
	for(nm in names(value)) attr(x, nm) <- value[nm]
	x
    }
    factors.aovlist <- function(pnames, tfactor,
				strata = FALSE, efactor = FALSE)
    {
	if(!is.na(int <- match("(Intercept)", pnames))) pnames <- pnames[-int]
	tnames <- apply(tfactor, 2L, function(x, nms)
			nms[as.logical(x)], rownames(tfactor))
	if(!missing(efactor)) {
	    enames <- NULL
	    if(!is.na(err <- match(strata, colnames(efactor))))
		enames <- (rownames(efactor))[as.logical(efactor[, err])]
	    else if(strata == "Within")
		enames <- c(rownames(efactor)
			    [as.logical(efactor %*% rep.int(1, ncol(efactor)))],
			    "Within")
	    if(!is.null(enames))
		tnames <- append(tnames, list(Residuals = enames))
	}
	result <- tnames[match(pnames, names(tnames))]
	if(!is.na(int))
	    result <- c("(Intercept)" = "(Intercept)", result)
	##should reorder result, but probably OK
	result
    }
    if(unweighted.scale && is.null(attr(object, "weights")))
	unweighted.scale <- FALSE
    err.qr <- attr(object, "error.qr")
    Terms <- terms(object, "Error")
    t.factor <- attr(Terms, "factors")
    i <- attr(Terms, "specials")$Error
    t <- attr(Terms, "variables")[[1 + i]]
    error <- Terms
    error[[3L]] <- t[[2L]]
    e.factor <- attr(terms(formula(error)), "factors")
    n <- nrow(err.qr$qr)
    n.object <- length(object)
    result <- vector("list", n.object)
    names(result) <- names(object)
    D1 <- seq_len(NROW(err.qr$qr))
    if(unweighted.scale) wt <- attr(object, "weights")
    for(i in names(object)) {
	prj <- proj.lm(object[[i]], onedf = onedf)
	if(unweighted.scale) prj <- prj/sqrt(wt)
	result.i <- matrix(0, n, ncol(prj), dimnames = list(D1, colnames(prj)))
	select <- rownames(object[[i]]$qr$qr)
	if(is.null(select)) select <- rownames(object[[i]]$residuals)
	result.i[select,  ] <- prj
	result[[i]] <- as.matrix(qr.qy(err.qr, result.i))
	attr.assign(result[[i]]) <- attr.xdim(prj)
	D2i <- colnames(prj)
	dimnames(result[[i]]) <- list(D1, D2i)
	attr(result[[i]], "factors") <-
	    factors.aovlist(D2i, t.factor, strata = i, efactor = e.factor)
    }
    attr(result, "call") <- attr(object, "call")
    attr(result, "e.factor") <- e.factor
    attr(result, "t.factor") <- t.factor
    class(result) <- c("aovprojlist", "listof")
    result
}

terms.aovlist <- function(x, ...)
{
    x <- attr(x, "terms")
    terms(x, ...)
}

## wish of PR#13505
as.data.frame.aovproj <- function(x, ...) as.data.frame(unclass(x), ...)
#  File src/library/stats/R/prop.test.R
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

prop.test <-
function(x, n, p = NULL, alternative = c("two.sided", "less", "greater"),
         conf.level = 0.95, correct = TRUE)
{
    DNAME <- deparse(substitute(x))

    if (is.matrix(x)) {
	if (ncol(x) != 2)
	    stop("'x' must have 2 columns")
	l <- nrow(x)
	n <- rowSums(x)
	x <- x[, 1]
    }
    else {
	DNAME <- paste(DNAME, "out of", deparse(substitute(n)))
	if ((l <- length(x)) != length(n))
	    stop("'x' and 'n' must have the same length")
    }

    OK <- complete.cases(x, n)
    x <- x[OK]
    n <- n[OK]
    if ((k <- length(x)) < 1)
	stop("not enough data")
    if (any(n <= 0))
	stop("elements of 'n' must be positive")
    if (any(x < 0))
	stop("elements of 'x' must be nonnegative")
    if (any(x > n))
	stop("elements of 'x' must not be greater than those of 'n'")

    if (is.null(p) && (k == 1))
	p <- .5
    if (!is.null(p)) {
	DNAME <- paste(DNAME, ", null ",
		       ifelse(k == 1, "probability ", "probabilities "),
		       deparse(substitute(p)), sep = "")
	if (length(p) != l)
	    stop("'p' must have the same length as 'x' and 'n'")
	p <- p[OK]
	if (any((p <= 0) | (p >= 1)))
	    stop("elements of 'p' must be in (0,1)")
    }

    alternative <- match.arg(alternative)
    if (k > 2 || (k == 2) && !is.null(p))
	alternative <- "two.sided"

    if ((length(conf.level) != 1) || is.na(conf.level) ||
	(conf.level <= 0) || (conf.level >= 1))
	stop("'conf.level' must be a single number between 0 and 1")

    correct <- as.logical(correct)

    ESTIMATE <- x/n
    names(ESTIMATE) <- if (k == 1) "p" else paste("prop", 1L:l)[OK]
    NVAL <- p
    CINT <- NULL
    YATES <- ifelse(correct && (k <= 2), .5, 0)

    if (k == 1) {
	z <- ifelse(alternative == "two.sided",
		    qnorm((1 + conf.level) / 2),
		    qnorm(conf.level))
	YATES <- min(YATES, abs(x - n * p))
        z22n <- z^2 / (2 * n)
	p.c <- ESTIMATE + YATES / n
	p.u <- if(p.c >= 1) 1 else (p.c + z22n
                  + z * sqrt(p.c * (1 - p.c) / n + z22n / (2 * n))) / (1+2*z22n)
	p.c <- ESTIMATE - YATES / n
	p.l <- if(p.c <= 0) 0 else (p.c + z22n
                  - z * sqrt(p.c * (1 - p.c) / n + z22n / (2 * n))) / (1+2*z22n)
	CINT <- switch(alternative,
		       "two.sided" = c(max(p.l, 0), min(p.u, 1)),
		       "greater" = c(max(p.l, 0), 1),
		       "less" = c(0, min(p.u, 1)))
    }
    else if ((k == 2) & is.null(p)) {
	DELTA <- ESTIMATE[1L] - ESTIMATE[2L]
	YATES <- min(YATES, abs(DELTA) / sum(1/n))
	WIDTH <- (switch(alternative,
			 "two.sided" = qnorm((1 + conf.level) / 2),
			 qnorm(conf.level))
		  * sqrt(sum(ESTIMATE * (1 - ESTIMATE) / n))
		  + YATES * sum(1/n))
	CINT <- switch(alternative,
		       "two.sided" = c(max(DELTA - WIDTH, -1),
		       min(DELTA + WIDTH, 1)),
		       "greater" = c(max(DELTA - WIDTH, -1), 1),
		       "less" = c(-1, min(DELTA + WIDTH, 1)))
    }
    if (!is.null(CINT))
	attr(CINT, "conf.level") <- conf.level

    METHOD <- paste(ifelse(k == 1,
			   "1-sample proportions test",
			   paste(k, "-sample test for ",
				 ifelse(is.null(p), "equality of", "given"),
				 " proportions", sep = "")),
		    ifelse(YATES, "with", "without"),
		    "continuity correction")

    if (is.null(p)) {
	p <- sum(x)/sum(n)
	PARAMETER <- k - 1
    }
    else {
	PARAMETER <- k
	names(NVAL) <- names(ESTIMATE)
    }
    names(PARAMETER) <- "df"

    x <- cbind(x, n - x)
    E <- cbind(n * p, n * (1 - p))
    if (any(E < 5))
	warning("Chi-squared approximation may be incorrect")
    STATISTIC <- sum((abs(x - E) - YATES)^2 / E)
    names(STATISTIC) <- "X-squared"

    if (alternative == "two.sided")
	PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
    else {
	if (k == 1)
	    z <- sign(ESTIMATE - p) * sqrt(STATISTIC)
	else
	    z <- sign(DELTA) * sqrt(STATISTIC)
	PVAL <- pnorm(z, lower.tail = (alternative == "less"))
    }

    RVAL <- list(statistic = STATISTIC,
		 parameter = PARAMETER,
		 p.value = as.numeric(PVAL),
		 estimate = ESTIMATE,
		 null.value = NVAL,
		 conf.int = CINT,
		 alternative = alternative,
		 method = METHOD,
		 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}
#  File src/library/stats/R/prop.trend.test.R
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

prop.trend.test <- function (x, n, score = seq_along(x))
{
    method <- "Chi-squared Test for Trend in Proportions"
    dname <- paste(deparse(substitute(x)), "out of", deparse(substitute(n)))
    dname <- paste(dname, ",\n using scores:", paste(score, collapse = " "))

    ## Tabular input have caused grief, get rid of
    ## dim() attributes:
    x <- as.vector(x)
    n <- as.vector(n)
    score <- as.vector(score)

    freq <- x/n
    p <- sum(x)/sum(n)
    w <- n/p/(1 - p)
    a <- anova(lm(freq ~ score, weights = w))
    chisq <- a["score", "Sum Sq"]
    names(chisq) <- "X-squared"
    df <- c(df = 1)
    pval <- pchisq(chisq, 1, lower.tail = FALSE)
    rval <- list(statistic = chisq, parameter = df,
                 p.value = as.numeric(pval),
                 method = method, data.name = dname)
    class(rval) <- "htest"
    return(rval)
}
#  File src/library/stats/R/qqnorm.R
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

qqnorm <- function(y, ...) UseMethod("qqnorm")

qqnorm.default <-
    function(y, ylim, main="Normal Q-Q Plot",
	     xlab="Theoretical Quantiles", ylab="Sample Quantiles",
	     plot.it=TRUE, datax = FALSE, ...)
{
    if(has.na <- any(ina <- is.na(y))) { ## keep NA's in proper places
        yN <- y
        y <- y[!ina]
    }
    if(0 == (n <- length(y)))
        stop("y is empty or has only NAs")
    if (plot.it && missing(ylim))
        ylim <- range(y)
    x <- qnorm(ppoints(n))[order(order(y))]
    if(has.na) {
        y <- x; x <- yN; x[!ina] <- y
        y <- yN
    }
    if(plot.it)
        if (datax)
            plot(y, x, main= main, xlab= ylab, ylab=xlab, xlim = ylim, ...)
        else
            plot(x, y, main= main, xlab= xlab, ylab= ylab, ylim= ylim, ...)
    invisible(if(datax) list(x = y, y = x) else list(x = x, y = y))
}

## Splus also has qqnorm.aov(), qqnorm.aovlist(), qqnorm.maov() ...

qqline <- function(y, datax = FALSE, ...)
{
    y <- quantile(y[!is.na(y)],c(0.25, 0.75))
    x <- qnorm(c(0.25, 0.75))
    if (datax) {
        slope <- diff(x)/diff(y)
        int <- x[1L] - slope*y[1L]
    } else {
        slope <- diff(y)/diff(x)
        int <- y[1L]-slope*x[1L]
    }
    abline(int, slope, ...)
}
#  File src/library/stats/R/qqplot.R
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

qqplot <- function(x, y, plot.it = TRUE, xlab = deparse(substitute(x)),
		   ylab = deparse(substitute(y)), ...)
{
    sx<-sort(x)
    sy<-sort(y)
    lenx<-length(sx)
    leny<-length(sy)
    if( leny < lenx )
	sx<-approx(1L:lenx, sx, n=leny)$y
    if( leny > lenx )
	sy<-approx(1L:leny, sy, n=lenx)$y
    if(plot.it)
	plot(sx, sy, xlab = xlab, ylab = ylab, ...)
    invisible(list(x = sx, y = sy))
}
#  File src/library/stats/R/quade.test.R
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

quade.test <- function(y, ...) UseMethod("quade.test")

quade.test.default <-
function(y, groups, blocks, ...)
{
    DNAME <- deparse(substitute(y))
    if(is.matrix(y)) {
        groups <- factor(c(col(y)))
        blocks <- factor(c(row(y)))
    }
    else {
        if(any(is.na(groups)) || any(is.na(blocks)))
            stop("NA's are not allowed in 'groups' or 'blocks'")
        if(any(diff(c(length(y), length(groups), length(blocks))) != 0L))
            stop("'y', 'groups' and 'blocks' must have the same length")
        DNAME <- paste(DNAME, ", ",
                       deparse(substitute(groups)), " and ",
                       deparse(substitute(blocks)), sep = "")
        if(any(table(groups, blocks) != 1))
            stop("not an unreplicated complete block design")
        groups <- factor(groups)
        blocks <- factor(blocks)
    }
    k <- nlevels(groups)
    b <- nlevels(blocks)
    y <- matrix(unlist(split(y, blocks)), ncol = k, byrow = TRUE)
    y <- y[complete.cases(y), ]
#    n <- nrow(y)
    r <- t(apply(y, 1L, rank))
    q <- rank(apply(y, 1, function(u) max(u) - min(u)))
    s <- q * (r - (k+1)/2)
    ## S is a matrix of ranks within blocks (minus the average rank)
    ## multiplied by the ranked ranges of the blocks
    A <- sum(s^2)
    B <- sum(colSums(s)^2) / b
    if(A == B) {
        ## Treat zero denominator case as suggested by Conover (1999),
        ## p.374.
        STATISTIC <- NaN
        PARAMETER <- c(NA, NA)
        PVAL <- (gamma(k+1))^(1-b)
    } else {
        STATISTIC <- (b - 1) * B / (A - B)
        ## The same as 2-way ANOVA on the scores S.
        PARAMETER <- c(k - 1, (b-1) * (k-1))
        PVAL <- pf(STATISTIC, PARAMETER[1L], PARAMETER[2L], lower.tail = FALSE)
    }
    names(STATISTIC) <- "Quade F"
    names(PARAMETER) <- c("num df", "denom df")

    structure(list(statistic = STATISTIC,
                   parameter = PARAMETER,
                   p.value = PVAL,
                   method = "Quade test",
                   data.name = DNAME),
              class = "htest")
}

quade.test.formula <-
function(formula, data, subset, na.action, ...)
{
    if(missing(formula))
        stop("'formula' missing")
    ## <FIXME>
    ## Maybe put this into an internal rewriteTwoWayFormula() when
    ## adding support for strata()
    if((length(formula) != 3)
       || (length(formula[[3L]]) != 3)
       || (formula[[3L]][[1L]] != as.name("|"))
       || (length(formula[[3L]][[2L]]) != 1)
       || (length(formula[[3L]][[3L]]) != 1))
        stop("incorrect specification for 'formula'")
    formula[[3L]][[1L]] <- as.name("+")
    ## </FIXME>
    m <- match.call(expand.dots = FALSE)
    m$formula <- formula
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m[[1L]] <- as.name("model.frame")
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " and ")
    names(mf) <- NULL
    y <- do.call("quade.test", as.list(mf))
    y$data.name <- DNAME
    y
}
#  File src/library/stats/R/quantile.R
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

quantile <- function(x, ...) UseMethod("quantile")

quantile.default <-
    function(x, probs = seq(0, 1, 0.25), na.rm = FALSE, names = TRUE,
             type = 7, ...)
{
    if(is.factor(x)) stop("need numeric data")
    if (na.rm)
	x <- x[!is.na(x)]
    else if (any(is.na(x)))
	stop("missing values and NaN's not allowed if 'na.rm' is FALSE")
    eps <- 100*.Machine$double.eps
    if (any((p.ok <- !is.na(probs)) & (probs < -eps | probs > 1+eps)))
	stop("'probs' outside [0,1]")
    n <- length(x)
    if(na.p <- any(!p.ok)) { # set aside NA & NaN
        o.pr <- probs
        probs <- probs[p.ok]
        probs <- pmax(0, pmin(1, probs)) # allow for slight overshoot
    }
    np <- length(probs)
    if (n > 0 && np > 0) {
        if(type == 7) { # be completely back-compatible
            index <- 1 + (n - 1) * probs
            lo <- floor(index)
            hi <- ceiling(index)
            x <- sort(x, partial = unique(c(lo, hi)))
            i <- index > lo
            qs <- x[lo]
            i <- seq_along(i)[i & !is.na(i)]
            h <- (index - lo)[i]
##          qs[i] <- qs[i] + .minus(x[hi[i]], x[lo[i]]) * (index[i] - lo[i])
            qs[i] <- ifelse(h == 0, qs[i], (1 - h) * qs[i] + h * x[hi[i]])
        } else {
            if (type <= 3) {
                ## Types 1, 2 and 3 are discontinuous sample qs.
                nppm <- if (type == 3) n * probs - .5 # n * probs + m; m = -0.5
                else n * probs          # m = 0
                j <- floor(nppm)
                switch(type,
                       h <- ifelse(nppm > j, 1, 0), # type 1
                       h <- ifelse(nppm > j, 1, 0.5), # type 2
                       h <- ifelse((nppm == j) &
                                   ((j %% 2) == 0), 0, 1)) # type 3
            } else {
                ## Types 4 through 9 are continuous sample qs.
                switch(type - 3,
                   {a <- 0; b <- 1},    # type 4
                       a <- b <- 0.5,   # type 5
                       a <- b <- 0,     # type 6
                       a <- b <- 1,     # type 7
                       a <- b <- 1 / 3, # type 8
                       a <- b <- 3 / 8) # type 9
                ## need to watch for rounding errors here
                fuzz <- 4 * .Machine$double.eps
                nppm <- a + probs * (n + 1 - a - b) # n*probs + m
                j <- floor(nppm + fuzz) # m = a + probs*(1 - a - b)
                h <- nppm - j
                h <- ifelse(abs(h) < fuzz, 0, h)
            }
            x <- sort(x, partial =
                      unique(c(1, j[j>0 & j<=n], (j+1)[j>0 & j<n], n))
                      )
            x <- c(x[1L], x[1L], x, x[n], x[n])
            ## h can be zero or one (types 1 to 3), and infinities matter
####        qs <- (1 - h) * x[j + 2] + h * x[j + 3]
            qs <- ifelse(h == 0, x[j+2],
                         ifelse(h == 1, x[j+3], (1-h)*x[j+2] + h*x[j+3]))
        }
    } else {
	qs <- rep(NA_real_, np)
    }
    if(names && np > 0) {
	dig <- max(2, getOption("digits"))
	names(qs) <- paste(## formatC is slow for long probs
			   if(np < 100)
			   formatC(100*probs, format="fg", width = 1, digits=dig)
			   else format(100 * probs, trim=TRUE, digits=dig),
			   "%", sep = "")
    }
    if(na.p) { # do this more elegantly (?!)
        o.pr[p.ok] <- qs
        names(o.pr) <- rep("", length(o.pr)) # suppress <NA> names
        names(o.pr)[p.ok] <- names(qs)
        o.pr
    } else qs
}

IQR <- function (x, na.rm = FALSE)
    diff(quantile(as.numeric(x), c(0.25, 0.75), na.rm = na.rm, names = FALSE))
#  File src/library/stats/R/r2dtable.R
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

r2dtable <- function(n, r, c)
{
    if(length(n) == 0 || (n < 0) || is.na(n))
	stop("invalid argument 'n'")
    if((length(r) <= 1) || any(r < 0) || any(is.na(r)))
	stop("invalid argument 'r'")
    if((length(c) <= 1) || any(c < 0) || any(is.na(c)))
	stop("invalid argument 'c'")
    if(sum(r) != sum(c))
	stop("arguments 'r' and 'c' must have the same sums")
    .Call("R_r2dtable",
	  as.integer(n), as.integer(r), as.integer(c),
          PACKAGE = "base")
}
#  File src/library/stats/R/relevel.R
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

relevel <- function(x, ref, ...) UseMethod("relevel")

relevel.default <- function(x, ref, ...)
    stop("'relevel' only for factors")

relevel.ordered <- function(x, ref, ...)
    stop("'relevel' only for factors")

relevel.factor <- function(x, ref, ...)
{
    lev <- levels(x)
    if(is.character(ref))
        ref <- match(ref, lev)
    if(is.na(ref))
        stop("'ref' must be an existing level")
    nlev <- length(lev)
    if(ref < 1 || ref > nlev)
        stop(gettextf("ref = %d must be in 1L:%d", ref, nlev), domain = NA)
    factor(x, levels = lev[c(ref, seq_along(lev)[-ref])])
}
#  File src/library/stats/R/reorder.factor.R
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

reorder.default <- function(x, X, FUN = mean, ..., order = is.ordered(x))
{
    scores <- tapply(X, x, FUN, ...)
    ans <- (if (order) ordered else factor)(x, levels = names(sort(scores)))
    attr(ans, "scores") <- scores
    ans
}

#  File src/library/stats/R/reshape.R
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

reshape <-
    function(data, varying= NULL, v.names= NULL, timevar = "time", idvar = "id",
             ids = 1L:NROW(data), times = seq_along(varying[[1L]]),
             drop = NULL, direction, new.row.names = NULL,
             sep = ".",
             split = if (sep==""){
                 list(regexp="[A-Za-z][0-9]",include=TRUE)
             } else {
                 list(regexp=sep, include= FALSE, fixed=TRUE)}
             )
{

    ix2names <- function(ix)
        if (is.character(ix)) ix else names(data)[ix]

    guess <- function(nms,re = split$regexp,drop = !split$include,
                      fixed=if(is.null(split$fixed)) FALSE else split$fixed)
    {
        if (drop)
            nn <- do.call("rbind",strsplit(nms, re, fixed=fixed))
        else
            nn <- cbind(substr(nms, 1L, regexpr(re,nms)),
                        substr(nms, regexpr(re,nms)+1L, 10000L))

        if (ncol(nn) != 2)
            stop("Failed to guess time-varying variables from their names")


        vn <- unique(nn[,1])
        v.names <- split(nms, factor(nn[,1], levels=vn))
        times <- unique(nn[,2])
        attr(v.names,"v.names") <- vn
        tt <- tryCatch({as.numeric(times)}, warning=function(w) times)
        attr(v.names,"times") <- tt
        v.names
    }

    reshapeLong <-
        function(data,varying,v.names = NULL,timevar,idvar,
                 ids = 1L:NROW(data), times,drop = NULL,new.row.names = NULL) {

        ll <- unlist(lapply(varying,length))
        if (any(ll != ll[1L])) stop("'varying' arguments must be the same length")
        if (ll[1L] != length(times)) stop("'times' is wrong length")

        if (!is.null(drop)) {
            if (is.character(drop))
                drop <- names(data) %in% drop
            data <- data[,if (is.logical(drop)) !drop else -drop, drop = FALSE]
        }

        ## store information for back-transformation.
        undoInfo <- list(varying = varying,v.names = v.names,
                                         idvar = idvar,timevar = timevar)

        ## multiple id variables
        if (length(idvar)>1){
               repeat({
                   tempidname<-basename(tempfile("tempID"))
                   if (!(tempidname %in% names(data))) break
               })
               data[,tempidname]<-interaction(data[,idvar],drop=TRUE)
               idvar<-tempidname
               drop.idvar<-TRUE
           } else drop.idvar<-FALSE


        d <- data
        all.varying <- unlist(varying)
        d <- d[,!(names(data) %in% all.varying),drop = FALSE]
        d[,timevar] <- times[1L]

        if (is.null(v.names))
            v.names <- unlist(lapply(varying,function(x) x[1L]))

        for(i in seq_along(v.names))
            d[, v.names[i]] <- data[, varying[[i]][1L] ]

        if (!(idvar %in% names(data)))
            d[,idvar] <- ids

        rval <- d

        if (length(times) == 1) {
            if (drop.idvar)
                rval[,idvar]<-NULL
            return(rval)
        }
        if (is.null(new.row.names))
            row.names(rval) <- paste(d[,idvar],times[1L],sep = ".")
        else
            row.names(rval) <- new.row.names[1L:NROW(rval)]

        for(i in 2:length(times)) {
            d[,timevar] <- times[i]
            for(j in seq_along(v.names))
                d[ ,v.names[j]] <- data[ ,varying[[j]][i]]

            if (is.null(new.row.names))
                row.names(d) <- paste(d[,idvar],times[i],sep = ".")
            else
                row.names(d) <- new.row.names[NROW(rval)+1L:NROW(d)]
            rval <- rbind(rval,d)  ##inefficient. So sue me.
        }

        ## if we created a temporary id variable, drop it
        if (drop.idvar)
            rval[,idvar]<-NULL

        attr(rval,"reshapeLong") <- undoInfo
        return(rval)
    } ## re..Long()

    reshapeWide <- function(data,timevar,idvar,varying = NULL,v.names = NULL,
                            drop = NULL,new.row.names = NULL) {

        if (!is.null(drop)) {
            if (is.character(drop))
                drop <- names(data) %in% drop
            data <- data[,if (is.logical(drop)) !drop else -drop, drop = FALSE]
        }

        undoInfo <- list(v.names = v.names,  timevar = timevar,idvar = idvar)

        orig.idvar<-idvar
        if (length(idvar)>1){
            repeat({
                tempidname<-basename(tempfile("tempID"))
                if (!(tempidname %in% names(data))) break
            })
            data[,tempidname]<-interaction(data[,idvar],drop=TRUE)
            idvar<-tempidname
            drop.idvar<-TRUE
        } else drop.idvar<-FALSE

        ## times <- sort(unique(data[,timevar]))
        ## varying and times must have the same order
        times <- unique(data[,timevar])
        if (any(is.na(times)))
            warning("there are records with missing times, which will be dropped.")
        undoInfo$times<-times

        if (is.null(v.names))
            v.names <- names(data)[!(names(data) %in% c(timevar,idvar,orig.idvar))]

        if (is.null(varying))
            varying <- outer(v.names,times,paste,sep = ".")
        if (is.list(varying))
            varying <- do.call("rbind",varying)

        undoInfo$varying<-varying


        CHECK <- TRUE
        if (CHECK) {
            keep <- !(names(data) %in% c(timevar,v.names,idvar,orig.idvar))
            if(any(keep)) {
                rval <- data[keep]
                tmp <- data[,idvar]
                really.constant <-
                    unlist(lapply(rval,
                                  function(a) all(tapply(a, as.vector(tmp),
                                                         function(b) length(unique(b)) == 1))))
                if (!all(really.constant))
                    warning(gettextf("some constant variables (%s) are really varying",
                                     paste(names(rval)[!really.constant],collapse = ",")), domain = NA)
            }
        }


        rval <- data[!duplicated(data[,idvar]),
                     !(names(data) %in% c(timevar,v.names)), drop = FALSE]

        for(i in seq_along(times)) {
            thistime <- data[data[,timevar] %in% times[i],]
            rval[,varying[,i]] <- thistime[match(rval[,idvar],thistime[,idvar]),
                                           v.names]
        }

        if (!is.null(new.row.names))
            row.names(rval) <- new.row.names

        ## temporary id variable to be dropped.
        if (drop.idvar) rval[,idvar]<-NULL

        ## information for back-transformation
        attr(rval,"reshapeWide") <- undoInfo

        rval
    } ## re..Wide()

    ## Begin reshape()

    if (missing(direction)){
        undo <- c("wide","long")[c("reshapeLong","reshapeWide")%in% names(attributes(data))]
        if (length(undo)==1) direction<-undo
    }
    direction <- match.arg(direction, c("wide", "long"))


    switch(direction,
           "wide" =
       {
           back <- attr(data,"reshapeLong")
           if (missing(timevar) && missing(idvar) && !is.null(back)) {
               reshapeWide(data, idvar = back$idvar, timevar = back$timevar,
                           varying = back$varying, v.names = back$v.names,
                           new.row.names = new.row.names)
           } else {
               reshapeWide(data, idvar = idvar, timevar = timevar,
                           varying = varying, v.names = v.names, drop = drop,
                           new.row.names = new.row.names)
           }

       },
           "long" =
       {
           if (missing(varying)) {
               back <- attr(data,"reshapeWide")
               if (is.null(back))
                   stop("no 'reshapeWide' attribute, must specify 'varying'")
               varying <- back$varying
               idvar <- back$idvar
               timevar <- back$timevar
               v.names <- back$v.names
               times <- back$times
           }

           if (is.matrix(varying))
               varying <- split(varying,row(varying))
           if (is.null(varying))
               stop("'varying' must be nonempty list or vector")
           if(is.atomic(varying)){
               varying <- ix2names(varying) # normalize
               if (missing(v.names))
                   varying <- guess(varying)
               else {
                   if (length(varying) %% length(v.names))
                       stop("length of v.names does not evenly divide length of varying")
                   ntimes <- length(varying) %/% length(v.names)
                   if (missing(times))
                       times <- seq_len(ntimes)
                   else if (length(times) != ntimes)
                       stop("length of varying must be the product of length of v.names and length of times")
                   varying <- split(varying, rep(v.names, ntimes))
                   attr(varying, "v.names") <- v.names
                   attr(varying, "times") <- times
               }
           }
           else varying <- lapply(varying, ix2names)

           ## This must happen after guess()
           if (missing(v.names) && !is.null(attr(varying,"v.names"))) {
               v.names <- attr(varying,"v.names")
               times <- attr(varying,"times")
           }


           reshapeLong(data, idvar = idvar, timevar = timevar,
                       varying = varying, v.names = v.names, drop = drop,
                       times = times, ids = ids, new.row.names = new.row.names)
       })
}
#  File src/library/stats/R/runmed.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995 Berwin A. Turlach <berwin@alphasun.anu.edu.au>
#  Ported to R, added interface to Stuetzle's code and further enhanced
#  by Martin Maechler,
#  Copyright (C) 1996--2002 Martin Maechler <maechler@stat.math.ethz.ch>
#  Copyright (C) 2003       The R Foundation
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


runmed <- function(x, k, endrule = c("median","keep","constant"),
                   algorithm = NULL, print.level = 0)
{
    n <- length(x)
    k <- as.integer(k)
    if(k < 0) stop("'k' must be positive")
    if(k%%2 == 0)
        warning("'k' must be odd!  Changing 'k' to ",
                k <- as.integer(1+ 2*(k %/% 2)))
    if(n == 0) {
	x <- double(); attr(x, "k") <- k
	return(x)
    }
    if (k > n)
        warning("'k' is bigger than 'n'!  Changing 'k' to ",
                k <- as.integer(1+ 2*((n - 1)%/% 2)))
    algorithm <-
        if(missing(algorithm)) { ## use efficient default
            ## This is too primitive, MM knows better :
            if(k < 20 || n < 300) "Stuetzle" else "Turlach"
        }
        else {
            match.arg(algorithm, c("Stuetzle", "Turlach"))
        }
    endrule <- match.arg(endrule)# including error.check
    iend <- switch(endrule,
                   ## "median" will be treated at the end
                   "median" =, "keep" = 0,
                   "constant" = 1)
    if(print.level)
        cat("runmed(*, endrule=", endrule,", algorithm=",algorithm,
            ", iend=",iend,")\n")
    res <- switch(algorithm,
                  Turlach = {
                      .C(R_Trunmed,
                         n,
                         k,
                         as.double(x),
                         rmed = double(n),	# median[] -> result
                         tmp1 = integer(k+1),	# outlist[]
                         tmp2 = integer(2*k +1),# nrlist []
                         tmp3 = double (2*k +1),# window []
                         as.integer(iend),
                         as.integer(print.level),
                         DUP = FALSE)$ rmed
                  },
                  Stuetzle = {
                      .C(R_Srunmed,
                         as.double(x),
                         smo = double(n),
                         n,
                         k,
                         as.integer(iend),
                         debug = (print.level > 0),
                         DUP = FALSE)$ smo
                  })
    if(endrule == "median")
        res <- smoothEnds(res, k = k)

    ## list(rmed=res$rmed, k=k)
    ## Setting attribute has the advantage that the result immediately plots
    attr(res,"k") <- k
    res
}

### All the following is from MM:

smoothEnds <- function(y, k = 3)
{
    ## Purpose: Smooth end values---typically after runmed()
    ##-- (C) COPYRIGHT 1994,  Martin Maechler <maechler@stat.math.ethz.ch>

    med3 <- function(a,b,c)
    {
        ## med3(a,b,c) == median(a,b,c)
        m <- b
        if (a < b) {
            if (c < b) m <- if (a >= c) a  else  c
        } else {## a >= b
            if (c > b) m <- if (a <= c) a  else  c
        }
        m
    }

    med.odd <- function(x, n = length(x))
    {
        ##  == median(x[1L:n]) IFF n is odd, slightly more efficient
        half <- (n + 1) %/% 2
        sort(x, partial = half)[half]
    }

    k <- as.integer(k)
    if (k < 0 || k%%2 == 0)
        stop("bandwidth 'k' must be >= 1 and odd!")
    k <- k %/% 2
    if (k < 1) return(y)
    ## else: k >= 1L: do something
    n <- length(y)
    sm <- y
    if (k >= 2) {
        sm [2L]  <- med3(y[1L],y [2L], y [3L])
        sm[n-1] <- med3(y[n],y[n-1],y[n-2])

        ## Here, could use Stuetzle's strategy for MUCH BIGGER EFFICIENCY
        ##	(when k>=3 , k >> 1):
        ## Starting with the uttermost 3 points,
        ## always 'adding'  2 new ones, and determine the new median recursively
        ##
        if (k >= 3) {
            for (i in 3:k) {
                j <- 2*i - 1
                sm  [i]   <- med.odd( y [1L:j] ,     j) #- left border
                sm[n-i+1] <- med.odd( y[(n+1-j):n], j) #- right border
            }
        }
    }

    ##--- For the very first and last pt.:  Use Tukey's end-point rule: ---
    ## Ysm[1L]:= Median(Ysm[2L],X1,Z_0), where Z_0 is extrapol. from Ysm[2L],Ysm[3L]
    sm[1L] <- med3(y[1L], sm [2L] , 3*sm [2L]  - 2*sm [3L])
    sm[n] <- med3(y[n], sm[n-1], 3*sm[n-1] - 2*sm[n-2])
    return(sm)
}
#  File src/library/stats/R/sd.R
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

sd <- function(x, na.rm=FALSE) {
    if (is.matrix(x))
	apply(x, 2, sd, na.rm=na.rm)
    else if (is.vector(x))
	sqrt(var(x, na.rm=na.rm))
    else if (is.data.frame(x))
	sapply(x, sd, na.rm=na.rm)
    else 
	sqrt(var(as.vector(x), na.rm=na.rm))
}
#  File src/library/stats/R/selfStart.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright 1997,1999 Jose C. Pinheiro <jcp$research.bell-labs.com>,
#                      Douglas M. Bates <bates$stat.wisc.edu>
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

###
###            self-starting nonlinear regression models
###

####* Constructors

selfStart <-
    function(model, initial, parameters, template) UseMethod("selfStart")

selfStart.default <-
  function(model, initial, parameters, template)
{
    value <- structure(as.function(model), initial = as.function(initial),
                       pnames = if(!missing(parameters))parameters)
    class(value) <- "selfStart"
    value
}

selfStart.formula <-
    function(model, initial, parameters, template = NULL)
{
    if (is.null(template)) {		# create a template if not given
        nm <- all.vars(model)
        if (any(msng <- is.na(match(parameters, nm)))) {
            stop(sprintf(ngettext(sum(msng),
                       "parameter %s does not occur in the model formula",
                       "parameters %s do not occur in the model formula"),
                         paste(sQuote(parameters[msng]), collapse=", ")),
                 domain = NA)
        }
        template <- function() {}
        argNams <- c( nm[ is.na( match(nm, parameters) ) ], parameters )
        args <- rep( alist( a = , b = 3 )[-2], length( argNams ) )
        names(args) <- argNams
        formals(template) <- args
    }
    value <- structure(deriv(model, parameters, template),
                       initial = as.function(initial),
                       pnames = parameters)
    class(value) <- "selfStart"
    value
}

###*# Methods


##*## Generics and methods specific to selfStart models

getInitial <-
    ## Create initial values for object from data
    function(object, data, ...) UseMethod("getInitial")

getInitial.formula <-
    function(object, data, ...)
{
    if(!is.null(attr(data, "parameters"))) {
        return(attr(data, "parameters"))
    }
    #obj <- object                       # kluge to create a copy inside this
    #object[[1L]] <- as.name("~")	 # function. match.call() is misbehaving
    switch (length(object),
            stop("argument 'object' has an impossible length"),
        {				# one-sided formula
	    func <- get(as.character(object[[2L]][[1L]]))
	    getInitial(func, data,
		       mCall = as.list(match.call(func, call = object[[2L]])),
                       ...)
        },
        {				# two-sided formula
	    func <- get(as.character(object[[3L]][[1L]]))
	    getInitial(func, data,
		       mCall = as.list(match.call(func, call = object[[3L]])),
		       LHS = object[[2L]], ...)
        })
}

getInitial.selfStart <-
    function(object, data, mCall, LHS = NULL, ...)
{
    (attr(object, "initial"))(mCall = mCall, data = data, LHS = LHS)
}

getInitial.default <-
    function(object, data, mCall, LHS = NULL, ...)
{
    if (is.function(object) && !is.null(attr(object, "initial"))) {
        stop("old-style self-starting model functions\n",
             "are no longer supported.\n",
             "New selfStart functions are available.\n",
             "Use\n",
             "  SSfpl instead of fpl,\n",
             "  SSfol instead of first.order.log,\n",
             "  SSbiexp instead of biexp,\n",
             "  SSlogis instead of logistic.\n",
             "If writing your own selfStart model, see\n",
             "  \"help(selfStart)\"\n",
             "for the new form of the \"initial\" attribute.", domain = NA)
    }
    stop(gettextf("no 'getInitial' method found for \"%s\" objects",
                  data.class(object)), domain = NA)
}

sortedXyData <-
    ## Constructor of the sortedXyData class
    function(x, y, data) UseMethod("sortedXyData")

sortedXyData.default <-
    function(x, y, data)
{
    ## works for x and y either numeric or language elements
    ## that can be evaluated in data
    #data <- as.data.frame(data)
    if (is.language(x) || ((length(x) == 1) && is.character(x))) {
        x <- eval(asOneSidedFormula(x)[[2L]], data)
    }
    x <- as.numeric(x)
    if (is.language(y) || ((length(y) == 1) && is.character(y))) {
        y <- eval(asOneSidedFormula(y)[[2L]], data)
    }
    y <- as.numeric(y)
    y.avg <- tapply(y, x, mean, na.rm = TRUE)
    xvals <- as.numeric(chartr(getOption("OutDec"), ".", names(y.avg)))
    ord <- order(xvals)
    value <- na.omit(data.frame(x = xvals[ord], y = as.vector(y.avg[ord])))
    class(value) <- c("sortedXyData", "data.frame")
    value
}

NLSstClosestX <-
    ## find the x value in the xy frame whose y value is closest to yval
    function(xy, yval) UseMethod("NLSstClosestX")

NLSstClosestX.sortedXyData <-
    ## find the x value in the xy frame whose y value is closest to yval
    ## uses linear interpolation in case the desired x falls between
    ## two data points in the xy frame
    function(xy, yval)
{
    deviations <- xy$y - yval
    if (any(deviations <= 0)) {
        dev1 <- max(deviations[deviations <= 0])
        lim1 <- xy$x[match(dev1, deviations)]
        if (all(deviations <= 0)) {
            return(lim1)
        }
    }
    if (any(deviations >= 0)) {
        dev2 <- min(deviations[deviations >= 0])
        lim2 <- xy$x[match(dev2, deviations)]
        if (all(deviations >= 0)) {
            return(lim2)
        }
    }
    dev1 <- abs(dev1)
    dev2 <- abs(dev2)
    lim1 + (lim2 - lim1) * dev1/(dev1 + dev2)
}

NLSstRtAsymptote <-
    ## Find a reasonable value for the right asymptote.
    function(xy) UseMethod("NLSstRtAsymptote")

NLSstRtAsymptote.sortedXyData <-
    function(xy)
{
    ## Is the last response value closest to the minimum or to
    ## the maximum?
    in.range <- range(xy$y)
    last.dif <- abs(in.range - xy$y[nrow(xy)])
    ## Estimate the asymptote as the largest (smallest) response
    ## value plus (minus) 1/8 of the range.
    if(match(min(last.dif), last.dif) == 2L) {
        return(in.range[2L] + diff(in.range)/8)
    }
    in.range[1L] - diff(in.range)/8
}

NLSstLfAsymptote <-
    ## Find a reasonable value for the left asymptote.
    function(xy) UseMethod("NLSstLfAsymptote")

NLSstLfAsymptote.sortedXyData <-
    function(xy)
{
    ## Is the first response value closest to the minimum or to
    ## the maximum?
    in.range <- range(xy$y)
    first.dif <- abs(in.range - xy$y[1L])
    ## Estimate the asymptote as the largest (smallest) response
    ## value plus (minus) 1/8 of the range.
    if(match(min(first.dif), first.dif) == 2L) {
        return(in.range[2L] + diff(in.range)/8)
    }
    in.range[1L] - diff(in.range)/8
}

NLSstAsymptotic <-
    ## fit the asymptotic regression model in the form
    ## b0 + b1*exp(-exp(lrc) * x)
    function(xy) UseMethod("NLSstAsymptotic")

NLSstAsymptotic.sortedXyData <-
    function(xy)
{
    xy$rt <- NLSstRtAsymptote(xy)
    ## Initial estimate of log(rate constant) from a linear regression
    value <- coef(nls(y ~ cbind(1, 1 - exp(-exp(lrc) * x)),
                      data = xy,
                      start = list(lrc =
                      as.vector(log(-coef(lm(log(abs(y - rt)) ~ x,
                                             data = xy))[2L]))),
                      algorithm = "plinear"))[c(2, 3, 1)]
    names(value) <- c("b0", "b1", "lrc")
    value
}

### Local variables:
### mode: S
### End:

#  File src/library/stats/R/shapiro.test.R
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

shapiro.test <- function(x) {
    DNAME <- deparse(substitute(x))
    x <- sort(x[complete.cases(x)])
    stopifnot(is.numeric(x))
    n <- length(x) # *is* integer
    if(n < 3 || n > 5000)
	stop("sample size must be between 3 and 5000")
    rng <- x[n] - x[1L]
    if(rng == 0)
	stop("all 'x' values are identical")
    if(rng < 1e-10)
	x <- x/rng # rescale to avoid ifault=6
    n2 <- n %/% 2L # integer, too
    ## C Code: Use the first n1 observations as uncensored
    sw <- .C(R_swilk,
	     init = FALSE,
	     as.single(x),
	     n,#      integer
	     n1 = n,#   "
	     n2,#       "
	     a = single(n2),
	     w	= double(1),
	     pw = double(1),
	     ifault = integer(1L))
    if (sw$ifault && sw$ifault != 7)# 7 *does* happen (Intel Linux)
	stop(gettextf("ifault=%d. This should not happen", sw$ifault),
             domain = NA)
    RVAL <- list(statistic = c(W = sw$w),
		 p.value = sw$pw,
		 method = "Shapiro-Wilk normality test",
		 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}
#  File src/library/stats/R/smooth.R
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

## do.ends = TRUE  is compatible with older behavior in R
## --------------  but *NOT*  with Colin Goodalls "smoother" "spl()"

smooth <- function(x, kind = c("3RS3R", "3RSS", "3RSR", "3R", "3", "S"),
                   twiceit = FALSE,
                   endrule = "Tukey", do.ends = FALSE)
{
    if(!is.numeric(x))
	stop("attempt to smooth non-numeric values")
    if(any(is.na(x)))
	stop("attempt to smooth NA values")
    rules <- c("copy","Tukey")#- exact order matters!
    if(is.na(iend <- pmatch(endrule, rules)))
        stop("wrong endrule")
    n <- length(x)
    kind <- match.arg(kind)
    if(substr(kind ,1L, 3L) == "3RS" && !do.ends)
        iend <- -iend
    else if(kind == "S")
        iend <- as.logical(do.ends)
    ## same number and type of arguments for all:
    smo <- .C(paste("Rsm", kind, sep="_"),
              as.double(x),
              y = double(n),
              n, iend,
              iter = integer(1L),
              DUP=FALSE,
	      PACKAGE = "stats")[c("y","iter")]

    if(any(kind == c("R", "S"))) { # `iter' really was `changed'
        smo$iter <- as.logical(smo$iter)
        names(smo)[names(smo) == "iter"] <- "changed"
    }

    if(twiceit) {
        ## c2 <- match.call() and re-call with twiceit = FALSE
        r <- smooth(x - smo$y, kind = kind, twiceit = FALSE,
                    endrule = endrule, do.ends = do.ends)
        smo$y <- smo$y + r
        if(!is.null(smo$iter))
            smo$iter <- smo$iter + attr(r,"iter")
        if(!is.null(smo$changed))
            smo$changed <- smo$changed || attr(r,"changed")
    }
    if(is.ts(x))
	smo$y <- ts(smo$y, start=start(x), frequency=frequency(x))

    structure(smo$y, kind = kind, twiced = twiceit,
              iter = smo$iter, changed = smo$changed,
              endrule = if(substr(kind, 1L, 1L) == "3") rules[iend],
              call = match.call(),
              class = c("tukeysmooth",if(is.ts(x)) "ts"))
}

print.tukeysmooth <- function(x, ...) {
    cat(attr(x,"kind"), "Tukey smoother resulting from ",
	deparse(attr(x, "call")),"\n")
    if(attr(x,"twiced"))		cat(" __twiced__ ")
    if(!is.null(it <- attr(x,"iter")))		cat(" used", it, "iterations\n")
    if(!is.null(ch <- attr(x,"changed")))	cat(if(!ch)"NOT", "changed\n")
    if(length(oldClass(x)) > 1)
	NextMethod()
    else {
	y <- x
	attributes(y) <- NULL
	print(y, ...)
	invisible(x)
    }
}

summary.tukeysmooth <- function(object, ...) {
    cat(attr(object,"kind"), "Tukey smoother resulting from\n",
	deparse(attr(object, "call")),";  n =", length(object),"\n")
    if(attr(object,"twiced"))		cat(" __twiced__ ")
    if(!is.null(it <- attr(object,"iter")))	cat(" used", it, "iterations\n")
    if(!is.null(ch <- attr(object,"changed")))	cat(if(!ch)" NOT", "changed\n")
    if(length(oldClass(object)) > 1)
	NextMethod()
    else {
	y <- object
	attributes(y) <- NULL
	summary(y, ...)
    }
}


#  File src/library/stats/R/smspline.R
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

smooth.spline <-
    function(x, y = NULL, w = NULL, df, spar = NULL, cv = FALSE,
             all.knots = FALSE, nknots = NULL, keep.data = TRUE,
             df.offset = 0, penalty = 1, control.spar = list())
{
    sknotl <- function(x, nk = NULL)
    {
        ## if (!all.knots)
	## return reasonable sized knot sequence for INcreasing x[]:
	n.kn <- function(n) {
	    ## Number of inner knots
	    if(n < 50L) n
	    else trunc({
		a1 <- log( 50, 2)
		a2 <- log(100, 2)
		a3 <- log(140, 2)
		a4 <- log(200, 2)
		if	(n < 200L) 2^(a1+(a2-a1)*(n-50)/150)
		else if (n < 800L) 2^(a2+(a3-a2)*(n-200)/600)
		else if (n < 3200L)2^(a3+(a4-a3)*(n-800)/2400)
		else  200 + (n-3200)^0.2
	    })
	}
        n <- length(x)
        if(is.null(nk)) nk <- n.kn(n)
        else if(!is.numeric(nk)) stop("'nknots' must be numeric <= n")
        else if(nk > n)
            stop("cannot use more inner knots than unique 'x' values")
	c(rep(x[1L], 3L), x[seq.int(1, n, length.out= nk)], rep(x[n], 3L))
    }
    contr.sp <- list(low = -1.5,## low = 0.      was default till R 1.3.x
                     high = 1.5,
                     tol = 1e-4,## tol = 0.001   was default till R 1.3.x
                     eps = 2e-8,## eps = 0.00244 was default till R 1.3.x
                     maxit = 500, trace = getOption("verbose"))
    contr.sp[(names(control.spar))] <- control.spar
    if(!all(sapply(contr.sp[1:4], is.numeric)) ||
       contr.sp$tol < 0 || contr.sp$eps <= 0 || contr.sp$maxit <= 0)
        stop("invalid 'control.spar'")

    xy <- xy.coords(x, y)
    y <- xy$y
    x <- xy$x
    n <- length(x)
    w <-
	if(is.null(w)) rep(1., n)
	else {
	    if(n != length(w)) stop("lengths of 'x' and 'w' must match")
	    if(any(w < 0.)) stop("all weights should be non-negative")
	    if(all(w == 0.)) stop("some weights should be positive")
	    (w * sum(w > 0.))/sum(w)
	}# now sum(w) == #{obs. with weight > 0} == sum(w > 0)

    ## Replace y[] for same x[] (to 6 digits precision) by their mean :
    x <- signif(x, 6L)
    ux <- unique(sort(x))
    ox <- match(x, ux)
    tmp <- matrix(unlist(tapply(seq_along(y), ox,
				function(i,y,w)
				c(sum(w[i]), sum(w[i]*y[i]),sum(w[i]*y[i]^2)),
				y = y, w = w)),
		  ncol = 3, byrow=TRUE)
    wbar <- tmp[, 1L]
    ybar <- tmp[, 2L]/ifelse(wbar > 0., wbar, 1.)
    yssw <- sum(tmp[, 3L] - wbar*ybar^2) # will be added to RSS for GCV
    nx <- length(ux)
    if(nx <= 3L) stop("need at least four unique 'x' values")
    if(cv && nx < n)
        warning("crossvalidation with non-unique 'x' values seems doubtful")
    r.ux <- ux[nx] - ux[1L]
    xbar <- (ux - ux[1L])/r.ux           # scaled to [0,1]
    if(all.knots) {
	knot <- c(rep(xbar[1L], 3L), xbar, rep(xbar[nx], 3L))
	nk <- nx + 2L
    } else {
	knot <- sknotl(xbar, nknots)
	nk <- length(knot) - 4L
    }

    ## ispar != 1 : compute spar (later)
    ispar <-
        if(is.null(spar) || missing(spar)) { ## || spar == 0
            if(contr.sp$trace) -1L else 0L
        } else 1L
    spar <- if(ispar == 1L) as.double(spar) else double(1)
    ## was <- if(missing(spar)) 0 else if(spar < 1.01e-15) 0 else  1
    ## icrit {../src/sslvrg.f}:
    ##		(0 = no crit,  1 = GCV ,  2 = ord.CV , 3 = df-matching)
    icrit <- if(cv) 2L else  1L
    dofoff <- df.offset
    if(!missing(df)) {
	if(df > 1 && df <= nx) {
	    icrit <- 3L
	    dofoff <- df
	} else warning("you must supply 1 < df <= n,  n = #{unique x} = ", nx)
    }
    iparms <- as.integer(c(icrit,ispar, contr.sp$maxit))
    names(iparms) <- c("icrit", "ispar", "iter")

    ## This uses DUP = FALSE which is dangerous since it does change
    ## its argument w.  We don't assume that as.double will
    ## always duplicate, although it does in R 2.3.1.
    fit <- .Fortran(R_qsbart,		# code in ../src/qsbart.f
		    as.double(penalty),
		    as.double(dofoff),
		    x = as.double(xbar),
		    y = as.double(ybar),
		    w = as.double(wbar),
		    ssw = as.double(yssw),
		    as.integer(nx),
		    as.double(knot),
		    as.integer(nk),
		    coef = double(nk),
		    ty = double(nx),
		    lev = double(nx),
		    crit = double(1),
		    iparms = iparms,
		    spar = spar,
		    parms = unlist(contr.sp[1:4]),
		    isetup = as.integer(0),
		    scrtch = double(17L * nk + 1L),
		    ld4  = as.integer(4),
		    ldnk = as.integer(1),
		    ier = integer(1),
		    DUP = FALSE
		    )[c("coef","ty","lev","spar","parms","crit","iparms","ier")]
    ## now we have clobbered wbar, recompute it.
    wbar <- tmp[, 1]

    lev <- fit$lev
    df <- sum(lev)
    if(is.na(df))
	stop("NA lev[]; probably smoothing parameter 'spar' way too large!")
    if(fit$ier > 0L ) {
        sml <- fit$spar < 0.5
	wtxt <- paste("smoothing parameter value too",
                      if(sml) "small" else "large")
        if(sml) {
            ## used to give warning too and mean() as below, but that's rubbish
            stop(wtxt)
        } else {
            fit$ty <- rep(mean(y), nx) ## would be df = 1
            df <- 1
            warning(wtxt,"\nsetting df = 1  __use with care!__")
        }
    }
    cv.crit <-
	if(cv) {
	    ww <- wbar
	    ww[!(ww > 0)] <- 1
	    weighted.mean(((y - fit$ty[ox])/(1 - (lev[ox] * w)/ww[ox]))^2, w)
	} else weighted.mean((y - fit$ty[ox])^2, w)/
	    (1 - (df.offset + penalty * df)/n)^2
    pen.crit <- sum(wbar * (ybar - fit$ty)^2)
    fit.object <- list(knot = knot, nk = nk, min = ux[1], range = r.ux,
		       coef = fit$coef)
    class(fit.object) <- "smooth.spline.fit"
    ## parms :  c(low = , high = , tol = , eps = )
    object <- list(x = ux, y = fit$ty, w = wbar, yin = ybar,
                   data = if(keep.data) list(x = x, y = y, w = w),
		   lev = lev, cv.crit = cv.crit, pen.crit = pen.crit,
                   crit = fit$crit,
                   df = df, spar = fit$spar,
                   lambda = unname(fit$parms["low"]),
                   iparms = fit$iparms, # c(icrit= , ispar= , iter= )
                   fit = fit.object, call = match.call())
    class(object) <- "smooth.spline"
    object
}

fitted.smooth.spline <- function(object, ...) {
    if(!is.list(dat <- object$data))
        stop("need result of smooth.spline(*, keep.data=TRUE)")
    ## note that object$x == unique(sort(object$data$x))
    object$y[match(dat$x, object$x)]
}

residuals.smooth.spline <-
    function (object, type = c("working", "response", "deviance",
                      "pearson", "partial"), ...)
{
    type <- match.arg(type)
    if(!is.list(dat <- object$data))
        stop("need result of smooth.spline(*, keep.data=TRUE)")
    r <- dat$y - object$y[match(dat$x, object$x)]
    ## this rest is `as' residuals.lm() :
    res <- switch(type,
                  working = ,
                  response = r,
                  deviance = ,
                  pearson = if (is.null(dat$w)) r else r * sqrt(dat$w),
                  partial = r)
    res <- naresid(object$na.action, res)
    if (type == "partial")
        stop('type = "partial" is not yet implemented')
        ## res <- res + predict(object, type = "terms")
    res
}


print.smooth.spline <- function(x, digits = getOption("digits"), ...)
{
    if(!is.null(cl <- x$call)) {
	cat("Call:\n")
	dput(cl, control=NULL)
    }
    ip <- x$iparms
    cv <- cl$cv
    if(is.null(cv)) cv <- FALSE else if(is.name(cv)) cv <- eval(cv)
    cat("\nSmoothing Parameter  spar=", format(x$spar, digits=digits),
        " lambda=", format(x$lambda, digits=digits),
        if(ip["ispar"] != 1L) paste("(", ip["iter"], " iterations)", sep=""),
        "\n")
    cat("Equivalent Degrees of Freedom (Df):", format(x$df,digits=digits),"\n")
    cat("Penalized Criterion:", format(x$pen.crit, digits=digits), "\n")
    cat(if(cv) "PRESS:" else "GCV:", format(x$cv.crit, digits=digits), "\n")
    invisible(x)
}

predict.smooth.spline <- function(object, x, deriv = 0, ...)
{
    if(missing(x)) {
        if(deriv == 0)
            return(object[c("x", "y")])
        else x <- object$x
    }
    fit <- object$fit
    if(is.null(fit)) stop("not a valid \"smooth.spline\" object")
    else predict(fit, x, deriv, ...)
}

predict.smooth.spline.fit <- function(object, x, deriv = 0, ...)
{
    if(missing(x))
	x <- seq.int(from = object$min, to = object$min + object$range,
                     length.out = length(object$coef) - 4L)
    xs <- (x - object$min)/object$range # x scaled to [0,1]
    extrap.left <- xs < 0
    extrap.right <- xs > 1
    interp <- !(extrap <- extrap.left | extrap.right)
    n <- sum(interp) # number of xs in [0,1]
    y <- xs
    if(any(interp))
	y[interp] <- .Fortran(R_bvalus,
			      n	  = as.integer(n),
			      knot= as.double(object$knot),
			      coef= as.double(object$coef),
			      nk  = as.integer(object$nk),
			      x	  = as.double(xs[interp]),
			      s	  = double(n),
			      order= as.integer(deriv),
			      DUP = FALSE)$s
    if(any(extrap)) {
	xrange <- c(object$min, object$min + object$range)
	if(deriv == 0) {
	    end.object <- Recall(object, xrange)$y
	    end.slopes <- Recall(object, xrange, 1)$y * object$range
	    if(any(extrap.left))
		y[extrap.left] <- end.object[1L] +
		    end.slopes[1L] * (xs[extrap.left] - 0)
	    if(any(extrap.right))
		y[extrap.right] <- end.object[2] +
		    end.slopes[2L] * (xs[extrap.right] - 1)
	} else if(deriv == 1) {
	    end.slopes <- Recall(object, xrange, 1)$y * object$range
	    y[extrap.left] <- end.slopes[1L]
	    y[extrap.right] <- end.slopes[2L]
	}
	else y[extrap] <- 0
    }
    if(deriv > 0)
	y <- y/(object$range^deriv)
    list(x = x, y = y)
}

supsmu <-
  function(x, y, wt = rep(1, n), span = "cv", periodic = FALSE, bass = 0)
{
    if(span == "cv") span <- 0
    n <- length(y)
    if(!n || !is.numeric(y)) stop("'y' must be numeric vector")
    if(length(x) != n) stop("number of observations in 'x' and 'y' must match.")
    if(length(wt) != n)
	stop("number of weights must match number of observations.")
    if(span < 0 || span > 1) stop("'span' must be between 0 and 1.")
    if(periodic) {
	iper <- 2L
	xrange <- range(x)
	if(xrange[1L] < 0 || xrange[2L] > 1)
	    stop("'x' must be between 0 and 1 for periodic smooth")
    } else iper <- 1L
    okay <- is.finite(x + y + wt)
    ord <- order(x[okay], y[okay])
    ord <- cumsum(!okay)[okay][ord] + ord
    xo <- x[ord]
    leno <- length(ord)
    if(leno == 0L)
        stop("no finite observations")
    if(diff <- n - leno)
	warning(diff, " observation(s) with NAs, NaNs and/or Infs deleted")
    .Fortran(R_setsmu)
    smo <- .Fortran(R_supsmu,
		    as.integer(leno),
		    as.double(xo),
		    as.double(y[ord]),
		    as.double(wt[ord]),
		    as.integer(iper),
		    as.double(span),
		    as.double(bass),
		    smo=double(leno),
		    double(n*7L), double(1L))$smo
    ## eliminate duplicate xsort values and corresponding smoothed values
    dupx <- duplicated(xo)
    list(x = xo[!dupx], y = smo[!dupx])
}
#  File src/library/stats/R/spectrum.R
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

## based on code by Martyn Plummer, plus kernel code by Adrian Trapletti
spectrum<- function (x, ..., method = c("pgram", "ar"))
{
    switch(match.arg(method),
	   pgram = spec.pgram(x, ...),
	   ar	 = spec.ar(x, ...)
	   )
}

## spec.taper based on code by Kurt Hornik
spec.taper <- function (x, p = 0.1)
{
    if (any(p < 0) || any(p > 0.5))
        stop("'p' must be between 0 and 0.5")
    a <- attributes(x)
    x <- as.matrix(x)
    nc <- ncol(x)
    if (length(p) == 1)
        p <- rep(p, nc)
    else if (length(p) != nc)
        stop("length of 'p' must be 1 or equal the number of columns of 'x'")
    nr <- nrow(x)
    for (i in 1L:nc) {
        m <- floor(nr * p[i])
        if(m == 0) next
        w <- 0.5 * (1 - cos(pi * seq.int(1, 2 * m - 1, by = 2)/(2 * m)))
        x[, i] <- c(w, rep(1, nr - 2 * m), rev(w)) * x[, i]
    }
    attributes(x) <- a
    x
}

spec.ar <- function(x, n.freq, order = NULL, plot = TRUE,
                    na.action = na.fail, method = "yule-walker", ...)
{
    ## can be called with a ts or a result of an AR fit.
    if(!is.list(x)) {
        series <- deparse(substitute(x))
        x <- na.action(as.ts(x))
        xfreq <- frequency(x)
        nser <- NCOL(x)
        x <- ar(x, is.null(order), order, na.action=na.action, method=method)
    } else {
        cn <- match(c("ar", "var.pred", "order"), names(x))
        if(any(is.na(cn)))
            stop("'x' must be a time series or an ar() fit")
        series <- x$series
        xfreq <- x$frequency
        if(is.array(x$ar)) nser <- dim(x$ar)[2L] else nser <- 1
    }
    order <- x$order
    if(missing(n.freq)) n.freq <- 500
    freq <- seq.int(0, 0.5, length.out = n.freq)
    if (nser == 1) {
        coh <- phase <- NULL
        if(order >= 1) {
            cs <- outer(freq, 1L:order, function(x, y) cos(2*pi*x*y)) %*% x$ar
            sn <- outer(freq, 1L:order, function(x, y) sin(2*pi*x*y)) %*% x$ar
            spec <- x$var.pred/(xfreq*((1 - cs)^2 + sn^2))
        } else
            spec <- rep(x$var.pred/(xfreq), length(freq))
    } else .NotYetImplemented()
    spg.out <- list(freq = freq*xfreq, spec = spec, coh = coh, phase = phase,
                    n.used = nrow(x), series = series,
                    method = paste("AR (", order, ") spectrum ", sep="")
                    )
    class(spg.out) <- "spec"
    if(plot) {
	plot(spg.out, ci = 0, ...)
        return(invisible(spg.out))
    } else return(spg.out)
}

spec.pgram <-
    function (x, spans = NULL, kernel = NULL, taper = 0.1,
              pad = 0, fast = TRUE,
              demean = FALSE, detrend = TRUE,
              plot = TRUE, na.action = na.fail, ...)
{
    ## Estimate spectral density from (smoothed) periodogram.
    series <- deparse(substitute(x))
    x <- na.action(as.ts(x))
    xfreq <- frequency(x)
    x <- as.matrix(x)
    N <- N0 <- nrow(x)
    nser <- ncol(x)
    if(!is.null(spans)) # allow user to mistake order of args
        kernel <- {
            if(is.tskernel(spans)) spans else
            kernel("modified.daniell", spans %/% 2)
        }
    if(!is.null(kernel) && !is.tskernel(kernel))
        stop("must specify 'spans' or a valid kernel")
    if (detrend) {
        t <- 1L:N - (N + 1)/2
        sumt2 <- N * (N^2 - 1)/12
        for (i in 1L:ncol(x))
            x[, i] <- x[, i] - mean(x[, i]) - sum(x[, i] * t) * t/sumt2
    }
    else if (demean) {
	x <- sweep(x, 2, colMeans(x), check.margin=FALSE)
    }
    ## apply taper:
    x <- spec.taper(x, taper)
    ## to correct for tapering: Bloomfield (1976, p. 194)
    ## Total taper is taper*2
    u2 <- (1 - (5/8)*taper*2)
    u4 <- (1 - (93/128)*taper*2)
    if (pad > 0) {
        x <- rbind(x, matrix(0, nrow = N * pad, ncol = ncol(x)))
        N <- nrow(x)
    }
    NewN <- if(fast) nextn(N) else N
    x <- rbind(x, matrix(0, nrow = (NewN - N), ncol = ncol(x)))
    N <- nrow(x)
    Nspec <- floor(N/2)
    freq <- seq.int(from = xfreq/N, by = xfreq/N, length.out = Nspec)
    xfft <- mvfft(x)
    pgram <- array(NA, dim = c(N, ncol(x), ncol(x)))
    for (i in 1L:ncol(x)) {
        for (j in 1L:ncol(x)) { # N0 = #{non-0-padded}
            pgram[, i, j] <- xfft[, i] * Conj(xfft[, j])/(N0*xfreq)
            ## value at zero is invalid as mean has been removed, so interpolate:
            pgram[1, i, j] <- 0.5*(pgram[2, i, j] + pgram[N, i, j])
        }
    }
    if(!is.null(kernel)) {
	for (i in 1L:ncol(x)) for (j in 1L:ncol(x))
	    pgram[, i, j] <- kernapply(pgram[, i, j], kernel, circular = TRUE)
	df <- df.kernel(kernel)
	bandwidth <- bandwidth.kernel(kernel)
    } else { # raw periodogram
	df <- 2
	bandwidth <- sqrt(1/12)
    }
    df <- df/(u4/u2^2)
    df <- df  * (N0 / N) ## << since R 1.9.0
    bandwidth <- bandwidth * xfreq/N
    pgram <- pgram[2:(Nspec+1),,, drop=FALSE]
    spec <- matrix(NA, nrow = Nspec, ncol = nser)
    for (i in 1L:nser) spec[, i] <- Re(pgram[1L:Nspec, i, i])
    if (nser == 1) {
        coh <- phase <- NULL
    } else {
        coh <- phase <- matrix(NA, nrow = Nspec, ncol = nser * (nser - 1)/2)
        for (i in 1L:(nser - 1)) {
            for (j in (i + 1):nser) {
                coh[, i + (j - 1) * (j - 2)/2] <-
                    Mod(pgram[, i, j])^2/(spec[, i] * spec[, j])
                phase[, i + (j - 1) * (j - 2)/2] <- Arg(pgram[, i, j])
            }
        }
    }
    ## correct for tapering
    for (i in 1L:nser) spec[, i] <- spec[, i]/u2
    spec <- drop(spec)
    spg.out <-
        list(freq = freq, spec = spec, coh = coh, phase = phase,
             kernel = kernel, df = df,
             bandwidth = bandwidth, n.used = N, orig.n = N0,# "n.orig" = "n..."
             series = series, snames = colnames(x),
             method = ifelse(!is.null(kernel), "Smoothed Periodogram",
                             "Raw Periodogram"),
             taper = taper, pad = pad, detrend = detrend, demean = demean)
    class(spg.out) <- "spec"
    if(plot) {
	plot(spg.out, ...)
        return(invisible(spg.out))
    } else return(spg.out)
}

plot.spec <-
    function (x, add = FALSE, ci = 0.95, log = c("yes", "dB", "no"),
              xlab = "frequency", ylab = NULL,
              type = "l", ci.col = "blue", ci.lty = 3,
              main = NULL, sub = NULL,
              plot.type = c("marginal", "coherency", "phase"), ...)
{
    spec.ci <- function (spec.obj, coverage = 0.95)
    {
        ## A utility function for plot.spec which calculates the confidence
        ## interval (centred around zero). We use a conditional argument to
        ## ensure that the ci always contains zero.

        if (coverage < 0 || coverage >= 1)
            stop("coverage probability out of range [0,1)")
        tail <- (1 - coverage)
        df <- spec.obj$df
        upper.quantile <- 1 - tail * pchisq(df, df, lower.tail = FALSE)
        lower.quantile <- tail * pchisq(df, df)
        1/(qchisq(c(upper.quantile, lower.quantile), df)/df)
    }

    plot.type <- match.arg(plot.type)
    log <- match.arg(log)
    m <- match.call()
    if(plot.type == "coherency") {
        m[[1L]] <- as.name("plot.spec.coherency")
        m$plot.type <- m$log <- m$add <- NULL
        return(eval(m, parent.frame()))
    }
    if(plot.type == "phase") {
        m[[1L]] <- as.name("plot.spec.phase")
        m$plot.type <- m$log <- m$add <- NULL
        return(eval(m, parent.frame()))
    }
    if(is.null(ylab))
        ylab <- if(log == "dB") "spectrum (dB)" else "spectrum"
    if(is.logical(log))
        log <- if(log) "yes" else "no"
    if(missing(log) && getOption("ts.S.compat")) log <- "dB"
    log <- match.arg(log)
    ylog <- ""
    if(log=="dB") x$spec <- 10 * log10(x$spec)
    if(log=="yes") ylog <- "y"
    if(add) {
        matplot(x$freq, x$spec, type = type, add=TRUE, ...)
    } else {
        matplot(x$freq, x$spec, xlab = xlab, ylab = ylab, type = type,
                log = ylog, ...)
        if (ci <= 0 || !is.numeric(x$df) || log == "no") {
            ## No confidence limits
            ci.text <- ""
        } else {
            ## The position of the error bar has no meaning: only the width
            ## and height. It is positioned in the top right hand corner.
            ##
            conf.lim <- spec.ci(x, coverage = ci)
            if(log=="dB") {
                conf.lim <- 10*log10(conf.lim)
                conf.y <- max(x$spec) - conf.lim[2L]
                conf.x <- max(x$freq) - x$bandwidth
                lines(rep(conf.x, 2), conf.y + conf.lim, col=ci.col)
                lines(conf.x + c(-0.5, 0.5) * x$bandwidth, rep(conf.y, 2),
                      col=ci.col)
                ci.text <- paste(", ", round(100*ci, 2),  "% C.I. is (",
                                 paste(format(conf.lim, digits = 3),
                                       collapse = ","), ")dB", sep="")
            } else {
                ci.text <- ""
                conf.y <- max(x$spec) / conf.lim[2L]
                conf.x <- max(x$freq) - x$bandwidth
                lines(rep(conf.x, 2), conf.y * conf.lim, col=ci.col)
                lines(conf.x + c(-0.5, 0.5) * x$bandwidth, rep(conf.y, 2),
                      col=ci.col)
            }
        }
        if (is.null(main))
            main <- paste(if(!is.null(x$series)) paste("Series:", x$series)
                          else "from specified model",
                          x$method, sep = "\n")
        if (is.null(sub) && is.numeric(x$bandwidth))
             sub <- paste("bandwidth = ", format(x$bandwidth, digits = 3),
                          ci.text, sep="")
        title(main = main, sub = sub)
    }
    invisible(x)
}

## based on code in Venables & Ripley
plot.spec.coherency <-
    function(x, ci = 0.95,
             xlab = "frequency", ylab = "squared coherency", ylim=c(0,1),
             type = "l", main = NULL, ci.col="blue",  ci.lty = 3, ...)
{
    nser <- NCOL(x$spec)
    ## Formulae from Bloomfield (1976, p.225)
    gg <- 2/x$df
    se <- sqrt(gg/2)
    z <- -qnorm((1-ci)/2)
    if (is.null(main))
        main <- paste(paste("Series:", x$series),
                      "Squared Coherency", sep = " --  ")
    if(nser == 2) {
        plot(x$freq, x$coh, type=type, xlab=xlab, ylab=ylab, ylim=ylim, ...)
        coh <- pmin(0.99999, sqrt(x$coh))
        lines(x$freq, (tanh(atanh(coh) + z*se))^2, lty=ci.lty, col=ci.col)
        lines(x$freq, (pmax(0, tanh(atanh(coh) - z*se)))^2,
              lty=ci.lty, col=ci.col)
        title(main)
    } else {
        opar <- par(mfrow = c(nser-1, nser-1), mar = c(1.5, 1.5, 0.5, 0.5),
                    oma = c(4, 4, 6, 4))
        on.exit(par(opar))
        plot.new()
        for (j in 2:nser) for (i in 1L:(j-1)) {
            par(mfg=c(j-1,i, nser-1, nser-1))
            ind <- i + (j - 1) * (j - 2)/2
            plot(x$freq, x$coh[, ind], type=type, ylim=ylim, axes=FALSE,
                 xlab="", ylab="", ...)
            coh <- pmin(0.99999, sqrt(x$coh[, ind]))
            lines(x$freq, (tanh(atanh(coh) + z*se))^2, lty=ci.lty, col=ci.col)
            lines(x$freq, (pmax(0, tanh(atanh(coh) - z*se)))^2,
                  lty=ci.lty, col=ci.col)
            box()
            if (i == 1) {
                axis(2, xpd = NA)
                title(ylab=x$snames[j], xpd = NA)
            }
            if (j == nser) {
                axis(1, xpd = NA)
                title(xlab=x$snames[i], xpd = NA)
            }
            mtext(main, 3, 3, TRUE, 0.5,
                  cex = par("cex.main"), font = par("font.main"))
        }
    }
    invisible()
}

plot.spec.phase <-
    function(x, ci = 0.95,
             xlab = "frequency", ylab = "phase", ylim=c(-pi, pi),
             type = "l", main = NULL, ci.col = "blue", ci.lty = 3, ...)
{
    nser <- NCOL(x$spec)
    ## Formulae from Bloomfield (1976, p.225)
    gg <- 2/x$df
    if (is.null(main))
        main <- paste(paste("Series:", x$series),
                      "Phase spectrum", sep = "  -- ")
    if(nser == 2) {
        plot(x$freq, x$phase, type=type, xlab=xlab, ylab=ylab, ylim=ylim, ...)
        coh <- sqrt(x$coh)
        cl <- asin( pmin( 0.9999, qt(ci, 2/gg-2)*
                         sqrt(gg*(coh^{-2} - 1)/(2*(1-gg)) ) ) )
        lines(x$freq, x$phase + cl, lty=ci.lty, col=ci.col)
        lines(x$freq, x$phase - cl, lty=ci.lty, col=ci.col)
        title(main)
    } else {
        opar <- par(mfrow = c(nser-1, nser-1), mar = c(1.5, 1.5, 0.5, 0.5),
                    oma = c(4, 4, 6, 4))
        on.exit(par(opar))
        plot.new()
        for (j in 2:nser) for (i in 1L:(j-1)) {
            par(mfg=c(j-1,i, nser-1, nser-1))
            ind <- i + (j - 1) * (j - 2)/2
            plot(x$freq, x$phase[, ind], type=type, ylim=ylim, axes=FALSE,
                 xlab="", ylab="", ...)
            coh <- sqrt(x$coh[, ind])
            cl <- asin( pmin( 0.9999, qt(ci, 2/gg-2)*
                             sqrt(gg*(coh^{-2} - 1)/(2*(1-gg)) ) ) )
            lines(x$freq, x$phase[, ind] + cl, lty=ci.lty, col=ci.col)
            lines(x$freq, x$phase[, ind] - cl, lty=ci.lty, col=ci.col)
            box()
            if (i == 1) {
                axis(2, xpd = NA)
                title(ylab=x$snames[j], xpd = NA)
            }
            if (j == nser) {
                axis(1, xpd = NA)
                title(xlab=x$snames[i], xpd = NA)
            }
            mtext(main, 3, 3, TRUE, 0.5,
                  cex = par("cex.main"), font = par("font.main"))
        }
    }
    invisible()
}
#  File src/library/stats/R/spline.R
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

#### 'spline' and 'splinefun' are very similar --- keep in sync!
####               --------- has more
####  also consider ``compatibility'' with  'approx' and 'approxfun'

spline <-
    function(x, y=NULL, n=3*length(x), method="fmm", xmin=min(x), xmax=max(x),
             xout, ties = mean)
{
    x <- xy.coords(x, y)
    y <- x$y
    x <- x$x
    nx <- length(x)
    method <- pmatch(method, c("periodic", "natural", "fmm"))
    if(is.na(method))
	stop("invalid interpolation method")
    if(any(o <- is.na(x) | is.na(y))) {
	o <- !o
	x <- x[o]
	y <- y[o]
	nx <- length(x)
    }
    if (!identical(ties, "ordered")) {
	if (length(ux <- unique(x)) < nx) {
	    if (missing(ties))
		warning("collapsing to unique 'x' values")
	    y <- as.vector(tapply(y,x,ties))# as.v: drop dim & dimn.
	    x <- sort(ux)
	    nx <- length(x)
	} else {
	    o <- order(x)
	    x <- x[o]
	    y <- y[o]
	}
        rm(ux)
    }
    if(nx == 0) stop("zero non-NA points")
    if(method == 1 && y[1L] != y[nx]) { # periodic
        warning("spline: first and last y values differ - using y[1] for both")
        y[nx] <- y[1L]
    }
    z <- .C("spline_coef",
	    method=as.integer(method),
	    n=as.integer(nx),
	    x=x,
	    y=y,
	    b=double(nx),
	    c=double(nx),
	    d=double(nx),
	    e=double(if(method == 1) nx else 0),
	    PACKAGE="stats")
    if(missing(xout))
        xout <- seq.int(xmin, xmax, length.out=n)
    else n <- length(xout)
    if (n <= 0)
        stop("'spline' requires n >= 1")
    .C("spline_eval",
       z$method,
       nu=as.integer(n),
       x =as.double(xout),
       y =double(n),
       z$n,
       z$x,
       z$y,
       z$b,
       z$c,
       z$d,
       PACKAGE="stats")[c("x","y")]
}
#  File src/library/stats/R/splinefun.R
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

#### 'spline' and 'splinefun' are very similar --- keep in sync!
####  also consider ``compatibility'' with  'approx' and 'approxfun'

splinefun <- function(x, y=NULL,
                      method = c("fmm", "periodic", "natural", "monoH.FC"),
                      ties = mean)
{
    x <- xy.coords(x, y)
    y <- x$y
    x <- x$x
    nx <- length(x)
    if(any(o <- is.na(x) | is.na(y))) {
	o <- !o
	x <- x[o]
	y <- y[o]
	nx <- length(x)
    }
    if (!identical(ties, "ordered")) {
	if (length(ux <- unique(x)) < nx) {
	    if (missing(ties))
		warning("collapsing to unique 'x' values")
	    y <- as.vector(tapply(y,x,ties))# as.v: drop dim & dimn.
	    x <- sort(ux)
	    nx <- length(x)
	} else {
	    o <- order(x)
	    x <- x[o]
	    y <- y[o]
	}
        rm(ux)
    }
    if(nx == 0) stop("zero non-NA points")
    method <- match.arg(method)
    if(method == "periodic" && y[1L] != y[nx]) {
	warning("spline: first and last y values differ - using y[1L] for both")
	y[nx] <- y[1L]
    }
    if(method == "monoH.FC") {
        n1 <- nx - 1L
        ## - - - "Data preprocessing" - - -

        dy <- y[-1L] - y[-nx]            # = diff(y)
        i0 <- dy == 0                   # or |dy| < eps ?? fixme ??
        dx <- x[-1L] - x[-nx]            # = diff(x)
        Sx <- dy / dx # 2. \Delta_k = (y_{k+1} - y_k)/(x_{k+1} - x_k), k=1L:n1
        m <- c(Sx[1L], (Sx[-1L] + Sx[-n1])/2, Sx[n1]) ## 1.
        if(any(i0)) {
            ## m0[k] := i0[k] or i0[k-1]
            m0 <- c(i0,FALSE) | c(FALSE,i0)
            m[m0] <- 0
        }
        if(any(ip <- !i0)) {
            alpha <- m[-nx][ip] / Sx[ip]
            beta  <- m[-1L][ip] / Sx[ip]
            a2b3 <- 2*alpha + beta - 3
            ab23 <- alpha + 2*beta - 3
            if(any(ok <- (a2b3 > 0 & ab23 > 0)) &&
               any(ok <- ok & (alpha * (a2b3 + ab23) < a2b3^2))) {
                ## correcting sum(ok) slopes m[] for monotonicity
                tau <- 3 / sqrt(alpha[ok]^2 + beta[ok]^2)
                m[-nx][ip][ok] <- tau * alpha[ok] * Sx[ip][ok]
                m[-1L][ip][ok] <- tau *  beta[ok] * Sx[ip][ok]
            }
        }
        ## Hermite spline with (x,y,m) :
        return(splinefunH0(x = x, y = y, m = m, dx = dx))
    }
    ## else
    iMeth <- match(method, c("periodic", "natural", "fmm", "monoH.FC"))
    z <- .C("spline_coef",
	    method=as.integer(iMeth),
	    n=as.integer(nx),
	    x=x,
	    y=y,
	    b=double(nx),
	    c=double(nx),
	    d=double(nx),
	    e=double(if(iMeth == 1) nx else 0),
	    PACKAGE="stats")
    rm(x,y,nx,o,method,iMeth)
    z$e <- NULL
    function(x, deriv = 0) {
	deriv <- as.integer(deriv)
	if (deriv < 0 || deriv > 3)
	    stop("'deriv' must be between 0 and 3")
	if (deriv > 0) {
	    ## For deriv >= 2, using approx() should be faster, but doing it correctly
	    ## for all three methods is not worth the programmer's time...
	    z0 <- double(z$n)
	    z[c("y", "b", "c")] <-
		switch(deriv,
		       list(y=	 z$b, b = 2*z$c, c = 3*z$d), # deriv = 1
		       list(y= 2*z$c, b = 6*z$d, c =	z0), # deriv = 2
		       list(y= 6*z$d, b =    z0, c =	z0)) # deriv = 3
	    z[["d"]] <- z0
	}
        ## yout[j] := y[i] + dx*(b[i] + dx*(c[i] + dx* d_i))
        ##           where dx := (u[j]-x[i]); i such that x[i] <= u[j] <= x[i+1},
        ##                u[j]:= xout[j] (unless sometimes for periodic spl.)
        ##           and  d_i := d[i] unless for natural splines at left
	res <- .C("spline_eval",
                  z$method,
                  as.integer(length(x)),
                  x=as.double(x),
                  y=double(length(x)),
                  z$n,
                  z$x,
                  z$y,
                  z$b,
                  z$c,
                  z$d,
                  PACKAGE="stats")$y

        ## deal with points to the left of first knot if natural
        ## splines are used  (Bug PR#13132)
        if( deriv > 0 && z$method==2 && any(ind <- x<=z$x[1L]) )
          res[ind] <- ifelse(deriv == 1, z$y[1L], 0)

        res
    }
}

## hidden : The exported user function is splinefunH()
splinefunH0 <- function(x, y, m, dx = x[-1L] - x[-length(x)])
{
    function(u, deriv=0, extrapol = c("linear","cubic"))
    {
	extrapol <- match.arg(extrapol)
	deriv <- as.integer(deriv)
	if (deriv < 0 || deriv > 2)
	    stop("'deriv' must be between 0 and 2")
	i <- findInterval(u, x, all.inside = (extrapol == "cubic"))
	if(deriv == 0)
	    interp <- function(u, i) {
		h <- dx[i]
		t <- (u - x[i]) / h
		## Compute the 4 Hermite (cubic) polynomials h00, h01,h10, h11
		t1 <- t-1
		h01 <- t*t*(3 - 2*t)
		h00 <- 1 - h01
		tt1 <- t*t1
		h10 <- tt1 * t1
		h11 <- tt1 * t
		y[i]  * h00 + h*m[i]  * h10 +
		y[i+1]* h01 + h*m[i+1]* h11
	    }
	else if(deriv == 1)
	    interp <- function(u, i) {
		h <- dx[i]
		t <- (u - x[i]) / h
		## 1st derivative of Hermite polynomials h00, h01,h10, h11
		t1 <- t-1
		h01 <- -6*t*t1 # h00 = - h01
		h10 <- (3*t - 1) * t1
		h11 <- (3*t - 2) * t
		(y[i+1] - y[i])/h * h01 + m[i] * h10 + m[i+1]* h11
	    }
	else ## deriv == 2
	    interp <- function(u, i) {
		h <- dx[i]
		t <- (u - x[i]) / h
		## 2nd derivative of Hermite polynomials h00, h01,h10, h11
		h01 <- 6*(1-2*t) # h00 = - h01
		h10 <- 2*(3*t - 2)
		h11 <- 2*(3*t - 1)
		((y[i+1] - y[i])/h * h01 + m[i] * h10 + m[i+1]* h11) / h
	    }


	if(extrapol == "linear" &&
	   any(iXtra <- (iL <- (i == 0)) | (iR <- (i == (n <- length(x)))))) {
	    ##	do linear extrapolation
	    r <- u
	    if(any(iL)) r[iL] <- if(deriv == 0) y[1L] + m[1L]*(u[iL] - x[1L]) else
				  if(deriv == 1) m[1L] else 0
	    if(any(iR)) r[iR] <- if(deriv == 0) y[n] + m[n]*(u[iR] - x[n]) else
				  if(deriv == 1) m[n] else 0
	    ## For internal values, compute "as normal":
	    ini <- !iXtra
	    r[ini] <- interp(u[ini], i[ini])
	    r
	}
        else { ## use cubic Hermite polynomials, even for extrapolation
            interp(u, i)
        }

    }
}

splinefunH <- function(x, y, m)
{
    ## Purpose: "Cubic Hermite Spline"
    ## ----------------------------------------------------------------------
    ## Arguments: (x,y): points;  m: slope at points,  all of equal length
    ## ----------------------------------------------------------------------
    ## Author: Martin Maechler, Date:  9 Jan 2008
    n <- length(x)
    stopifnot(is.numeric(x), is.numeric(y), is.numeric(m),
              length(y) == n, length(m) == n)
    if(is.unsorted(x)) {
        i <- sort.list(x)
        x <- x[i]
        y <- y[i]
        m <- m[i]
    }
    dx <- x[-1L] - x[-n]
    if(any(is.na(dx)) || any(dx == 0))
        stop("'x' must be *strictly* increasing (non - NA)")
    splinefunH0(x, y, m, dx=dx)
}
#  File src/library/stats/R/stats-defunct.R
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

## <entry>
## Deprecated in 1.4.0
## Defunct in 1.5.0
reshapeWide <- function(x, i, j, val, jnames = levels(j)) .Defunct("reshape")
reshapeLong <- function(x,jvars,  ilev = row.names(x),
                        jlev = names(x)[jvars], iname = "reshape.i",
                        jname = "reshape.j", vname = "reshape.v")
    .Defunct("reshape")
## </entry>

## <entry>
## Deprecated in 1.8.0
## Defunct in 1.9.0
print.coefmat <- function(x, digits=max(3, getOption("digits") - 2),
              signif.stars = getOption("show.signif.stars"),
              dig.tst = max(1, min(5, digits - 1)),
              cs.ind, tst.ind, zap.ind = integer(0L),
              P.values = NULL,
              has.Pvalue,
              eps.Pvalue = .Machine$double.eps,
              na.print = "", ...) .Defunct()
anovalist.lm <- function (object, ..., test = NULL) .Defunct()
lm.fit.null <- function(x, y, method = "qr", tol = 1e-07, ...)
    .Defunct("lm.fit")
lm.wfit.null <- function(x, y, w, method = "qr", tol = 1e-07, ...)
    .Defunct("lm.wfit")
glm.fit.null <- function(x, y, weights , start = NULL,
             etastart = NULL, mustart = NULL, offset,
             family = gaussian(), control = glm.control(),
             intercept = FALSE)
    .Defunct("glm.fit")
## </entry>

## <entry>
## Deprecated in 2.2.1
## Defunct in 2.4.0
mauchley.test <- function(...) .Defunct("mauchly.test")
## </entry>
#  File src/library/stats/R/stats-deprecated.R
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

## deprecated in 2.2.1
## mauchley.test <- function(object, Sigma=diag(nrow=p),
##                           T = Thin.row(proj(M)-proj(X)),
##                           M = diag(nrow=p),
##                           X = ~0,
##                           idata=data.frame(index=seq(length=p)),...)
## {
## 	.Deprecated("mauchly.test")
## 	UseMethod("mauchly.test")
## }

## <entry>
## Deprecated in 2.10.0
clearNames <- function( object )
{
    .Deprecated("unname")
    names( object ) <- NULL
    object
}
## </entry>
#  File src/library/stats/R/stepfun.R
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

## Constructor for  Step Functions:

## Given x[1L] .. x[n] ;	 y[0] .. y[n]  (one value more !)
## For 'cadlag' functions :  f(t) = y[i]  iff  t in  ( x[i], x[i+1] ],
##     where  x[0] := - Inf
##
## 'General case':  f(x[i]) = z[i]  with arbitrary z[]
## -- but we would have to modify 'approxfun' or not be able to use it..
## --->> forget about general case
stepfun <-
    function(x, y, f = as.numeric(right), ties = "ordered", right = FALSE)
{
    if(is.unsorted(x)) stop("stepfun: 'x' must be ordered increasingly")
    n <- length(x)
    if(n < 1) stop("'x' must have length >= 1")
    n1 <- n + 1L
    if(length(y) != n1) stop("'y' must be one longer than 'x'")
    rval <- approxfun(x, y[- if(right)n1 else 1], method = "constant",
		      yleft = y[1L], yright = y[n1], f = f, ties = ties)
    class(rval) <- c("stepfun", class(rval))
    attr(rval, "call") <- sys.call()
    rval
}

is.stepfun <- function(x) is.function(x) && inherits(x, "stepfun")

as.stepfun <- function(x, ...) UseMethod("as.stepfun")
as.stepfun.default <- function(x, ...)
{
    if(is.stepfun(x)) x
    else stop("no 'as.stepfun' method available for 'x'")
}

## Quite obvious  that I will want to have  knots.spline(..)  etc......
knots         <- function(Fn, ...) UseMethod("knots")
knots.stepfun <- function(Fn, ...) eval(expression(x), envir=environment(Fn))


print.stepfun <- function (x, digits = getOption("digits") - 2, ...)
{
    numform <- function(x) paste(formatC(x, digits = digits), collapse=", ")
    i1 <- function(n) 1L:min(3, n)
    i2 <- function(n) if(n >= 4) max(4, n-1):n else integer(0L)
    cat("Step function\nCall: ")
    print(attr(x, "call"), ...)
    env <- environment(x)
    n <- length(xx <- eval(expression(x), envir = env))
    cat(" x[1:", n, "] = ", numform(xx[i1(n)]),
	if(n > 3) ", ", if(n > 5) " ..., ", numform(xx[i2(n)]), "\n", sep = "")
    y <- eval(expression(c(yleft, y)), envir = env)
    cat(n+1, " plateau levels = ", numform(y[i1(n+1)]),
	if(n+1 > 3) ", ", if(n+1 > 5) " ..., ", numform(y[i2(n+1)]), "\n",
	sep = "")
    invisible(x)
}

summary.stepfun <- function(object, ...)
{
    n <- eval(expression(n), envir = environment(object))
    if(!is.integer(n) || n < 1) stop("not a valid step function")
    ## n <- n-1
    cat("Step function with continuity 'f'=",
	format(eval(expression(f), envir = environment(object))),
	", ", n, if(n <= 6) "knots at\n" else "knots with summary\n")
    summ <- if(n>6) summary else function(x) x
    print(summ(knots(object)))
    cat(if(n>6) "\n" else "  ", "and	", n+1,
        " plateau levels (y) ", if(n <= 6) "at\n" else "with summary\n",
        sep="")
    print(summ(eval(expression(c(yleft,y)), envir = environment(object))))
    invisible()
}

## Purpose: plot method for  stepfun (step function) objects
## --------------------------------------------------------------------
## Arguments: for numeric 'x', do empirical CDF;	  ==> `` ?plot.step ''
## --------------------------------------------------------------------
## Author: Martin Maechler <maechler@stat.math.ethz.ch>
##	      1990, U.Washington, Seattle; improved, Dec.1993
##	      Ported to R :  Sept.1997.
plot.stepfun <-
    function(x, xval, xlim, ylim = range(c(y,Fn.kn)),
	     xlab = "x", ylab = "f(x)", main = NULL,
	     add = FALSE, verticals = TRUE, do.points = (n < 1000),
	     pch = par("pch"), col = par("col"),
             col.points = col, cex.points = par("cex"),
	     col.hor = col, col.vert = col,
	     lty = par("lty"), lwd = par("lwd"),
	     ...)
{
    if(!is.stepfun(x)) { #- make it work when called explicitly with data
	if(is.numeric(x)) {
	    sarg <- substitute(x)
	    x <- ecdf(x)
	    attr(x,"call") <- call("ecdf", sarg)
	} else stop("'plot.stepfun' called with wrong type of argument 'x'")
    }
    if(missing(main))
	main <- {
	    cl <- attr(x,"call")
	    deparse(if(!is.null(cl))cl else sys.call())
	}

    knF <- knots(x)
    xval <- if (missing(xval)) knF else sort(xval)
    if (missing(xlim)) {
        rx <- range(xval)
        dr <-
            if(length(xval) > 1)
                max(0.08 * diff(rx), median(diff(xval)))
            else
                abs(xval)/16
        xlim <- rx +  dr * c(-1,1)

    } else dr <- diff(xlim)

    knF <- knF[xlim[1L]-dr <= knF & knF <= xlim[2L]+dr]

    ## Careful for heights of horizontals -- these depend on f
    ti <- c(xlim[1L]-dr, knF, xlim[2L]+dr)
    ti.l <- ti[-length(ti)]
    ti.r <- ti[-1L]
    y <- x(0.5*(ti.l + ti.r))
    n <- length(y)
    Fn.kn <- x(knF)

    ##------------------------ Plotting ----------------------------

    ## horizontal segments
    if (add)
	segments(ti.l, y, ti.r, y, col=col.hor, lty=lty, lwd=lwd, ...)
    else {
        if(missing(ylim)) ylim <- range(c(y,Fn.kn))
	plot(0,0, type="n", xlim=xlim, ylim=ylim,
	     xlab=xlab, ylab=ylab, main= main, ...)
	segments(ti.l, y, ti.r, y, col=col.hor, lty=lty, lwd=lwd)
    }
    if(do.points)
	points(knF, Fn.kn, pch=pch, col=col.points, cex=cex.points)

    if(verticals)
	segments(knF, y[-n], knF, y[-1L], col=col.vert, lty=lty, lwd=lwd)
    invisible(list(t = ti, y = y))
}

lines.stepfun <- function(x, ...) plot(x, add = TRUE, ...)

as.stepfun.isoreg <- function(x, ...)
{
    sf <- stepfun(x = (if(x$isOrd) x$x else x$x[x$ord])[x$iKnots],
                        y = c(x$yf[x$iKnots], x$yf[length(x$yf)]),
                  right = TRUE)
    attr(sf, "call") <- x$call
    sf
}
#  File src/library/stats/R/stl.R
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

stl <- function(x, s.window,
		s.degree = 0,
		t.window = NULL, t.degree = 1,
		l.window = nextodd(period), l.degree = t.degree,
		s.jump = ceiling(s.window/10),
		t.jump = ceiling(t.window/10),
		l.jump = ceiling(l.window/10),
		robust = FALSE,
		inner = if(robust)  1 else 2,
		outer = if(robust) 15 else 0,
		na.action = na.fail)
{
    nextodd <- function(x){
	x <- round(x)
	if(x%%2==0) x <- x+1
	as.integer(x)
    }
    deg.check <- function(deg) {
	degname <- deparse(substitute(deg))
	deg <- as.integer(deg)
	if(deg < 0 || deg > 1) stop(degname, " must be 0 or 1")
	deg
    }
    x <- na.action(as.ts(x))
    if(is.matrix(x)) stop("only univariate series are allowed")
    n <- length(x)
    period <- frequency(x)
    if(period < 2 || n <= 2 * period)
	stop("series is not periodic or has less than two periods")
    periodic <- FALSE
    if(is.character(s.window)) {
	if(is.na(pmatch(s.window, "periodic")))
	    stop("unknown string value for s.window")
	else {
	    periodic <- TRUE
	    s.window <- 10 * n + 1
	    s.degree <- 0
	}
    }
    s.degree <- deg.check(s.degree)
    t.degree <- deg.check(t.degree)
    l.degree <- deg.check(l.degree)
    if(is.null(t.window))
	t.window <- nextodd(ceiling( 1.5 * period / (1- 1.5/s.window)))
    z <- .Fortran(R_stl,
		  as.double(x),
		  as.integer(n),
		  as.integer(period),
		  as.integer(s.window),
		  as.integer(t.window),
		  as.integer(l.window),
		  s.degree, t.degree, l.degree,
		  nsjump = as.integer(s.jump),
		  ntjump = as.integer(t.jump),
		  nljump = as.integer(l.jump),
		  ni = as.integer(inner),
		  no = as.integer(outer),
		  weights = double(n),
		  seasonal = double(n),
		  trend = double(n),
		  double((n+2*period)*5))
    if(periodic) {
	## make seasonal part exactly periodic
	which.cycle <- cycle(x)
	z$seasonal <- tapply(z$seasonal, which.cycle, mean)[which.cycle]
    }
    remainder <- as.vector(x) - z$seasonal - z$trend
    y <- cbind(seasonal=z$seasonal, trend=z$trend, remainder=remainder)
    res <- list(time.series = ts(y, start=start(x), frequency = period),
		weights=z$weights, call=match.call(),
		win = c(s = s.window, t = t.window, l = l.window),
		deg = c(s = s.degree, t = t.degree, l = l.degree),
		jump= c(s = s.jump,   t = t.jump,   l = l.jump),
		inner = z$ni, outer = z$no)
    class(res) <- "stl"
    res
}

print.stl <- function(x, ...)
{
    cat(" Call:\n ")
    dput(x$call, control=NULL)
    cat("\nComponents\n")
    print(x$time.series, ...)
    invisible(x)
}

summary.stl <- function(object, digits = getOption("digits"), ...)
{
    cat(" Call:\n ")
    dput(object$call, control=NULL)
    cat("\n Time.series components:\n")
    print(summary(object$time.series, digits = digits, ...))
    cat(" IQR:\n")
    iqr <- apply(cbind(STL = object$time.series,
                       data = object$time.series %*% rep(1,3)),
		 2L, IQR)
    print(rbind(format(iqr, digits = max(2, digits - 3)),
		"   %"= format(round(100 * iqr / iqr["data"], 1))),
	  quote = FALSE)
    cat("\n Weights:")
    if(all(object$weights == 1)) cat(" all == 1\n")
    else { cat("\n"); print(summary(object$weights, digits = digits, ...)) }
    cat("\n Other components: ")
    str(object[-(1L:3)], give.attr = FALSE)
    invisible(object)
}

plot.stl <- function(x, labels = colnames(X),
		     set.pars = list(mar = c(0, 6, 0, 6), oma = c(6, 0, 4, 0),
				     tck = -0.01, mfrow = c(nplot, 1)),
		     main = NULL, range.bars = TRUE, ...,
                     col.range = "light gray")
{
    sers <- x$time.series
    ncomp <- ncol(sers)
    data <- drop(sers %*% rep(1, ncomp))
    X <- cbind(data, sers)
    colnames(X) <- c("data", colnames(sers))
    nplot <- ncomp + 1
    if(range.bars)
	mx <- min(apply(rx <- apply(X,2, range), 2, diff))
    if(length(set.pars)) {
	oldpar <- do.call("par", as.list(names(set.pars)))
	on.exit(par(oldpar))
	do.call("par", set.pars)
    }
    for(i in 1L:nplot) {
	plot(X[, i], type = if(i < nplot) "l" else "h",
	     xlab = "", ylab = "", axes = FALSE, ...)
	if(range.bars) {
	    dx <- 1/64 * diff(ux <- par("usr")[1L:2])
	    y <- mean(rx[,i])
	    rect(ux[2L] - dx, y + mx/2, ux[2L] - 0.4*dx, y - mx/2,
		 col = col.range, xpd = TRUE)
	}
	if(i == 1 && !is.null(main))
	    title(main, line = 2, outer = par("oma")[3L] > 0)
	if(i == nplot) abline(h=0)
	box()
	right <- i %% 2 == 0
	axis(2, labels = !right)
	axis(4, labels = right)
	axis(1, labels = i == nplot)
	mtext(labels[i], side = 2, 3)
    }
    mtext("time", side = 1, line = 3)
    invisible()
}
#  File src/library/stats/R/symnum.R
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

symnum <- function(x, cutpoints = c(  .3,  .6,	 .8,  .9, .95),
		   symbols = if(numeric.x) c(" ", ".", ",", "+", "*", "B")
		   else c(".", "|"),
		   legend = length(symbols) >= 3,
		   na = "?", eps = 1e-5, numeric.x = is.numeric(x),
		   corr = missing(cutpoints) && numeric.x,
		   show.max = if(corr) "1", show.min = NULL,
		   abbr.colnames = has.colnames,
		   lower.triangular = corr && is.numeric(x) && is.matrix(x),
		   diag.lower.tri = corr && !is.null(show.max))
{
    ## Martin Maechler, 21 Jan 1994; Dedicated to Benjamin Schaad, born that day

    ##--------------- Argument checking -----------------------------
    if(length(x) == 0L)
	return(noquote(if(is.null(d <- dim(x)))character(0L) else array("", dim=d)))
    has.na <- any(nax <- is.na(x))
    if(numeric.x) {
	force(corr) # missingness..
	cutpoints <- sort(cutpoints)
	if(corr) cutpoints <- c(0, cutpoints, 1)
	if(anyDuplicated(cutpoints) ||
	   (corr && (any(cutpoints > 1) || any(cutpoints < 0)) ))
	    stop(if(corr) gettext("'cutpoints' must be unique in 0 < cuts < 1, but are = ")
                 else gettext("'cutpoints' must be unique, but are = "),
                 paste(format(cutpoints), collapse="|"), domain = NA)
	nc <- length(cutpoints)
	minc <- cutpoints[1L]
	maxc <- cutpoints[nc]
	range.msg <- if(corr) gettext("'x' must be between -1 and 1")
        else gettextf("'x' must be between %s and %s",
                      format(minc), format(maxc))
	if(corr) x <- abs(x)
	else
	    if(any(x < minc - eps, na.rm=TRUE)) stop(range.msg, domain = NA)
	if (   any(x > maxc + eps, na.rm=TRUE)) stop(range.msg, domain = NA)

	ns <- length(symbols)
	symbols <- as.character(symbols)
	if(anyDuplicated(symbols))
	    stop("'symbols' must be unique, but are = ",
                 paste(symbols, collapse="|"), domain = NA)
	if(nc != ns+1)
            if(corr)
                stop("number of 'cutpoints' must be one less than number of symbols")
            else
                stop("number of 'cutpoints' must be one more than number of symbols")

	iS <- cut(x, breaks=cutpoints, include.lowest=TRUE, labels= FALSE)
	if(any(ii <- is.na(iS))) {
	    ##-- can get 0, if x[i]== minc  --- only case ?
	    iS[which(ii)[!is.na(x[ii]) & (abs(x[ii] - minc) < eps)]] <- 1#-> symbol[1L]
	}
    }
##     else if(!is.logical(x))
## 	stop("'x' must be numeric or logical")
    else  { ## assume logical x : no need for cut(points)
	if(!missing(symbols) && length(symbols) != 2L)
	    stop("must have 2 'symbols' for logical 'x' argument")
	iS <- x + 1 # F = 1,  T = 2
    }
    if(has.na) {
	ans <- character(length(iS))
	if((has.na <- is.character(na)))
	    ans[nax] <- na
	ans[!nax] <- symbols[iS[!nax]]
    } else ans <- symbols[iS]
    if(numeric.x) {
	if(!is.null(show.max)) ans[x >= maxc - eps] <-
	    if(is.character(show.max)) show.max else format(maxc, dig=1)
	if(!is.null(show.min)) ans[x <= minc + eps] <-
	    if(is.character(show.min)) show.min else format(minc, dig=1)
    }
    if(lower.triangular && is.matrix(x))
	ans[!lower.tri(x, diag = diag.lower.tri)] <- ""
    attributes(ans) <- attributes(x)
    if(is.array(ans)&& (rank <- length(dim(x))) >= 2L) { # `fix' column names
	has.colnames <- !is.null(dimnames(ans))
	if(!has.colnames) {
	    dimnames(ans) <- vector("list",rank)
	} else {
	    has.colnames <- length(dimnames(ans)[[2L]]) > 0L
	}
	if((is.logical(abbr.colnames) || is.numeric(abbr.colnames))
	   && abbr.colnames) {
	    dimnames(ans)[[2L]] <-
		abbreviate(dimnames(ans)[[2L]], minlength= abbr.colnames)
	    ## dropped further abbrev. depending on getOption("width")
	}
	else if(is.null(abbr.colnames) || is.null(dimnames(ans)[[2L]]))
	    dimnames(ans)[[2L]] <- rep("", dim(ans)[2L])
	else if(!is.logical(abbr.colnames)) stop("invalid 'abbr.colnames'")
    }
    if(legend) {
	legend <- c(rbind(sapply(cutpoints,format),
			  c(sQuote(symbols),"")),
		    if(has.na) paste("	    ## NA:", sQuote(na)))
	attr(ans,"legend") <- paste(legend[-2*(ns+1)], collapse=" ")
    }
    noquote(ans)
}
#  File src/library/stats/R/t.test.R
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

t.test <- function(x, ...) UseMethod("t.test")

t.test.default <-
function(x, y = NULL, alternative = c("two.sided", "less", "greater"),
         mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95,
         ...)
{
    alternative <- match.arg(alternative)

    if(!missing(mu) && (length(mu) != 1 || is.na(mu)))
        stop("'mu' must be a single number")
    if(!missing(conf.level) &&
       (length(conf.level) != 1 || !is.finite(conf.level) ||
        conf.level < 0 || conf.level > 1))
        stop("'conf.level' must be a single number between 0 and 1")
    if( !is.null(y) ) {
	dname <- paste(deparse(substitute(x)),"and",
		       deparse(substitute(y)))
	if(paired)
	    xok <- yok <- complete.cases(x,y)
	else {
	    yok <- !is.na(y)
	    xok <- !is.na(x)
	}
	y <- y[yok]
    }
    else {
	dname <- deparse(substitute(x))
	if( paired ) stop("'y' is missing for paired test")
	xok <- !is.na(x)
	yok <- NULL
    }
    x <- x[xok]
    if( paired ) {
	x <- x-y
	y <- NULL
    }
    nx <- length(x)
    mx <- mean(x)
    vx <- var(x)
    estimate <- mx
    if(is.null(y)) {
        if(nx < 2) stop("not enough 'x' observations")
	df <- nx-1
	stderr <- sqrt(vx/nx)
        if(stderr < 10 *.Machine$double.eps * abs(mx))
            stop("data are essentially constant")
	tstat <- (mx-mu)/stderr
	method <- ifelse(paired,"Paired t-test","One Sample t-test")
	names(estimate) <- ifelse(paired,"mean of the differences","mean of x")
    } else {
	ny <- length(y)
        if(nx < 1 || (!var.equal && nx < 2))
            stop("not enough 'x' observations")
	if(ny < 1 || (!var.equal && ny < 2))
            stop("not enough 'y' observations")
        if(var.equal && nx+ny < 3) stop("not enough observations")
	my <- mean(y)
	vy <- var(y)
	method <- paste(if(!var.equal)"Welch", "Two Sample t-test")
	estimate <- c(mx,my)
	names(estimate) <- c("mean of x","mean of y")
	if(var.equal) {
	    df <- nx+ny-2
            v <- 0
            if(nx > 1) v <- v + (nx-1)*vx
            if(ny > 1) v <- v + (ny-1)*vy
	    v <- v/df
	    stderr <- sqrt(v*(1/nx+1/ny))
	} else {
	    stderrx <- sqrt(vx/nx)
	    stderry <- sqrt(vy/ny)
	    stderr <- sqrt(stderrx^2 + stderry^2)
	    df <- stderr^4/(stderrx^4/(nx-1) + stderry^4/(ny-1))
	}
        if(stderr < 10 *.Machine$double.eps * max(abs(mx), abs(my)))
            stop("data are essentially constant")
        tstat <- (mx - my - mu)/stderr
    }
    if (alternative == "less") {
	pval <- pt(tstat, df)
	cint <- c(-Inf, tstat + qt(conf.level, df) )
    }
    else if (alternative == "greater") {
	pval <- pt(tstat, df, lower.tail = FALSE)
	cint <- c(tstat - qt(conf.level, df), Inf)
    }
    else {
	pval <- 2 * pt(-abs(tstat), df)
	alpha <- 1 - conf.level
        cint <- qt(1 - alpha/2, df)
	cint <- tstat + c(-cint, cint)
    }
    cint <- mu + cint * stderr
    names(tstat) <- "t"
    names(df) <- "df"
    names(mu) <- if(paired || !is.null(y)) "difference in means" else "mean"
    attr(cint,"conf.level") <- conf.level
    rval <- list(statistic = tstat, parameter = df, p.value = pval,
	       conf.int=cint, estimate=estimate, null.value = mu,
	       alternative=alternative,
	       method=method, data.name=dname)
    class(rval) <- "htest"
    return(rval)
}

t.test.formula <-
function(formula, data, subset, na.action, ...)
{
    if(missing(formula)
       || (length(formula) != 3)
       || (length(attr(terms(formula[-2]), "term.labels")) != 1))
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m[[1L]] <- as.name("model.frame")
    m$... <- NULL
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " by ")
    names(mf) <- NULL
    response <- attr(attr(mf, "terms"), "response")
    g <- factor(mf[[-response]])
    if(nlevels(g) != 2)
        stop("grouping factor must have exactly 2 levels")
    DATA <- split(mf[[response]], g)
    names(DATA) <- c("x", "y")
    y <- do.call("t.test", c(DATA, list(...)))
    y$data.name <- DNAME
    if(length(y$estimate) == 2)
        names(y$estimate) <- paste("mean in group", levels(g))
    y
}
#  File src/library/stats/R/termplot.R
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

termplot <- function(model, data = NULL,envir = environment(formula(model)),
                     partial.resid = FALSE,
		     rug = FALSE, terms = NULL, se = FALSE,
                     xlabs = NULL, ylabs = NULL,
                     main = NULL, col.term = 2, lwd.term = 1.5,
                     col.se = "orange", lty.se = 2, lwd.se = 1,
                     col.res= "gray", cex = 1, pch = par("pch"),
                     col.smth = "darkred",lty.smth = 2,span.smth = 2/3,
                     ask = dev.interactive() && nb.fig < n.tms,
                     use.factor.levels = TRUE, smooth = NULL,
                     ylim = "common", ...)
{
    which.terms <- terms
    terms <- ## need if(), since predict.coxph() has non-NULL default terms :
	if (is.null(terms))
	    predict(model, type = "terms", se.fit = se)
	else
	    predict(model, type = "terms", se.fit = se, terms = terms)
    n.tms <- ncol(tms <- as.matrix(if(se) terms$fit else terms))
##     if(inherits(model, "gam")) {
##         m.nms <- names(model$model)
##         for(j in seq_along(model$smooth)) {
##             sj <- model$smooth[[j]]
##             names(model$model)[m.nms == sj[["term"]]] <- sj[["label"]]
##         }
##     }
    mf <- model.frame(model)
    if (is.null(data))
        data <- eval(model$call$data, envir)
    if (is.null(data))
        data <- mf
    use.rows <- if (NROW(tms) < NROW(data))
        match(rownames(tms), rownames(data)) ## else NULL
    nmt <- colnames(tms)
    cn <- parse(text = nmt)
    ## Defaults:
    if (!is.null(smooth))
      smooth <- match.fun(smooth)
    if (is.null(ylabs))
	ylabs <- paste("Partial for",nmt)
    if (is.null(main))
        main <- ""
    else if(is.logical(main))
        main <- if(main) deparse(model$call, 500) else ""
    else if(!is.character(main))
        stop("'main' must be TRUE, FALSE, NULL or character (vector).")
    main <- rep(main, length.out = n.tms) # recycling
    pf <- envir
    carrier <- function(term) { # used for non-factor ones
	if (length(term) > 1)
	    carrier(term[[2L]])
	else
	    eval(term, data, enclos = pf)
    }
    carrier.name <- function(term){
      	if (length(term) > 1)
	    carrier.name(term[[2L]])
	else
	    as.character(term)
    }
    if (is.null(xlabs))
        xlabs <- unlist(lapply(cn,carrier.name))

    if (partial.resid || !is.null(smooth)){
	pres <- residuals(model, "partial")
        if (!is.null(which.terms)) pres <- pres[, which.terms, drop = FALSE]
      }
    is.fac <- sapply(nmt, function(i) is.factor(mf[, i]))

    se.lines <- function(x, iy, i, ff = 2) {
        tt <- ff * terms$se.fit[iy, i]
        lines(x, tms[iy, i] + tt, lty = lty.se, lwd = lwd.se, col = col.se)
        lines(x, tms[iy, i] - tt, lty = lty.se, lwd = lwd.se, col = col.se)
    }

    nb.fig <- prod(par("mfcol"))
    if (ask) {
	oask <- devAskNewPage(TRUE)
	on.exit(devAskNewPage(oask))
    }

    ylims <- ylim
    if(identical(ylims, "common")) {
        ylims <- if(!se) range(tms, na.rm = TRUE)
        else range(tms + 1.05*2*terms$se.fit,
                   tms - 1.05*2*terms$se.fit,
                   na.rm = TRUE)
        if (partial.resid) ylims <- range(ylims, pres, na.rm = TRUE)
        if (rug) ylims[1L] <- ylims[1L] - 0.07*diff(ylims)
    }

    ##---------- Do the individual plots : ----------

    for (i in 1L:n.tms) {
        if(identical(ylim, "free")) {
            ylims <- range(tms[, i], na.rm = TRUE)
            if (se)
                ylims <- range(ylims,
                               tms[, i] + 1.05*2*terms$se.fit[, i],
                               tms[, i] - 1.05*2*terms$se.fit[, i],
                               na.rm = TRUE)
            if (partial.resid)
                ylims <- range(ylims, pres[, i], na.rm = TRUE)
            if (rug)
                ylims[1L] <- ylims[1L] - 0.07*diff(ylims)
        }
	if (is.fac[i]) {
	    ff <- mf[,nmt[i]]
            if (!is.null(model$na.action))
              ff <- naresid(model$na.action, ff)
	    ll <- levels(ff)
	    xlims <- range(seq_along(ll)) + c(-.5, .5)
            xx <- as.numeric(ff) ##need if rug or partial
	    if(rug) {
		xlims[1L] <- xlims[1L] - 0.07*diff(xlims)
		xlims[2L] <- xlims[2L] + 0.03*diff(xlims)
	    }
	    plot(1, 0, type = "n", xlab = xlabs[i], ylab = ylabs[i],
                 xlim = xlims, ylim = ylims, main = main[i], xaxt="n", ...)
            if (use.factor.levels)
                axis(1, at = seq_along(ll), labels = ll, ...)
            else
                axis(1)
	    for(j in seq_along(ll)) {
		ww <- which(ff==ll[j])[c(1,1)]
		jf <- j + c(-.4, .4)
		lines(jf,tms[ww, i], col = col.term, lwd = lwd.term, ...)
		if(se) se.lines(jf, iy = ww, i = i)
	    }
	}
	else { ## continuous carrier
	    xx <- carrier(cn[[i]])
            if (!is.null(use.rows)) xx <- xx[use.rows]
	    xlims <- range(xx, na.rm = TRUE)
	    if(rug)
		xlims[1L] <- xlims[1L] - 0.07*diff(xlims)
	    oo <- order(xx)
	    plot(xx[oo], tms[oo, i], type = "l",
                 xlab = xlabs[i], ylab = ylabs[i],
		 xlim = xlims, ylim = ylims, main = main[i],
                 col = col.term, lwd = lwd.term, ...)
            if(se) se.lines(xx[oo], iy = oo, i = i)
	}
	if (partial.resid){
          if (!is.fac[i] && !is.null(smooth)){
            smooth(xx,pres[, i], lty = lty.smth,
                   cex = cex, pch = pch, col = col.res,
                   col.smooth = col.smth, span = span.smth)
          }
          else
              points(xx, pres[, i], cex = cex, pch = pch, col = col.res)
        }
	if (rug) {
            n <- length(xx)
            ## Fixme: Isn't this a kludge for segments() ?
	    lines(rep.int(jitter(xx), rep.int(3, n)),
                  rep.int(ylims[1L] + c(0, 0.05, NA)*diff(ylims), n))
	    if (partial.resid)
		lines(rep.int(xlims[1L] + c(0, 0.05, NA)*diff(xlims), n),
                      rep.int(pres[, i], rep.int(3, n)))
	}
    }
    invisible(n.tms)
}
#  File src/library/stats/R/ts-tests.R
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

Box.test <- function (x, lag = 1, type=c("Box-Pierce", "Ljung-Box"), fitdf=0)
{
    if (NCOL(x) > 1)
        stop ("x is not a vector or univariate time series")
    DNAME <- deparse(substitute(x))
    type <- match.arg(type)
    cor <- acf (x, lag.max = lag, plot = FALSE, na.action = na.pass)
    n <- sum(!is.na(x))
    PARAMETER <- lag-fitdf
    obs <- cor$acf[2:(lag+1)]
    if (type=="Box-Pierce")
    {
        METHOD <- "Box-Pierce test"
        STATISTIC <- n*sum(obs^2)
        PVAL <- 1-pchisq(STATISTIC, lag-fitdf)
    }
    else
    {
        METHOD <- "Box-Ljung test"
        STATISTIC <- n*(n+2)*sum(1/seq.int(n-1, n-lag)*obs^2)
        PVAL <- 1-pchisq(STATISTIC, lag-fitdf)
    }
    names(STATISTIC) <- "X-squared"
    names(PARAMETER) <- "df"
    structure(list(statistic = STATISTIC,
                   parameter = PARAMETER,
                   p.value = PVAL,
                   method = METHOD,
                   data.name = DNAME),
              class = "htest")
}

PP.test <- function (x, lshort = TRUE)
{
    if (NCOL(x) > 1)
        stop ("x is not a vector or univariate time series")
    DNAME <- deparse(substitute(x))
    z <- embed (x, 2)
    yt <- z[,1]
    yt1 <- z[,2]
    n <- length (yt)
    tt <- (1L:n)-n/2
    res <- lm (yt~1+tt+yt1)
    if (res$rank < 3)
        stop ("singularities in regression")
    res.sum <- summary (res)
    tstat <- (res.sum$coefficients[3,1]-1)/res.sum$coefficients[3,2]
    u <- residuals (res)
    ssqru <- sum(u^2)/n
    if (lshort)
        l <- trunc(4*(n/100)^0.25)
    else
        l <- trunc(12*(n/100)^0.25)
    ssqrtl <- .C ("R_pp_sum", as.vector(u,mode="double"), as.integer(n),
                  as.integer(l), trm=as.double(ssqru), PACKAGE="stats")
    ssqrtl <- ssqrtl$trm
    n2 <- n^2
    trm1 <- n2*(n2-1)*sum(yt1^2)/12
    trm2 <- n*sum(yt1*(1L:n))^2
    trm3 <- n*(n+1)*sum(yt1*(1L:n))*sum(yt1)
    trm4 <- (n*(n+1)*(2*n+1)*sum(yt1)^2)/6
    Dx <- trm1-trm2+trm3-trm4
    STAT <- sqrt(ssqru)/sqrt(ssqrtl)*tstat-(n^3)/(4*sqrt(3)*sqrt(Dx)*sqrt(ssqrtl))*(ssqrtl-ssqru)
    table <- cbind(c(4.38,4.15,4.04,3.99,3.98,3.96),
                   c(3.95,3.80,3.73,3.69,3.68,3.66),
                   c(3.60,3.50,3.45,3.43,3.42,3.41),
                   c(3.24,3.18,3.15,3.13,3.13,3.12),
                   c(1.14,1.19,1.22,1.23,1.24,1.25),
                   c(0.80,0.87,0.90,0.92,0.93,0.94),
                   c(0.50,0.58,0.62,0.64,0.65,0.66),
                   c(0.15,0.24,0.28,0.31,0.32,0.33))
    table <- -table
    tablen <- dim(table)[2L]
    tableT <- c(25,50,100,250,500,100000)
    tablep <- c(0.01,0.025,0.05,0.10,0.90,0.95,0.975,0.99)
    tableipl <- numeric(tablen)
    for (i in (1L:tablen))
        tableipl[i] <- approx (tableT,table[,i],n,rule=2)$y
    PVAL <- approx (tableipl,tablep,STAT,rule=2)$y
    PARAMETER <- l
    METHOD <- "Phillips-Perron Unit Root Test"
    names(STAT) <- "Dickey-Fuller"
    names(PARAMETER) <- "Truncation lag parameter"
    structure(list(statistic = STAT, parameter = PARAMETER,
                   p.value = PVAL, method = METHOD, data.name = DNAME),
              class = "htest")
}
#  File src/library/stats/R/ts.R
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

start	  <- function(x, ...) UseMethod("start")
end	  <- function(x, ...) UseMethod("end")
frequency <- function(x, ...) UseMethod("frequency")
time	  <- function(x, ...) UseMethod("time")
window	  <- function(x, ...) UseMethod("window")
cycle     <- function(x, ...) UseMethod("cycle")
deltat    <- function(x, ...) UseMethod("deltat")

ts <- function(data = NA, start = 1, end = numeric(0), frequency = 1,
	       deltat = 1, ts.eps  =  getOption("ts.eps"),
               class = if(nseries > 1) c("mts", "ts") else "ts",
               names = if(!is.null(dimnames(data))) colnames(data)
               else paste("Series", seq(nseries))
               )
{
    if(is.data.frame(data)) data <- data.matrix(data)
#   if(!is.numeric(data)) stop("'data'  must be a numeric vector or matrix")
    if(is.matrix(data)) {
	nseries <- ncol(data)
	ndata <- nrow(data)
        dimnames(data) <- list(NULL, names)
    } else {
	nseries <- 1
	ndata <- length(data)
    }
    if(ndata == 0) stop("'ts' object must have one or more observations")

    if(missing(frequency)) frequency <- 1/deltat
    else if(missing(deltat)) deltat <- 1/frequency

    if(frequency > 1 && abs(frequency - round(frequency)) < ts.eps)
	frequency <- round(frequency)

    if(length(start) > 1L) {
## strange: this never checked for < 1!  commented for 1.7.0
##	if(start[2L] > frequency) stop("invalid start")
	start <- start[1L] + (start[2L] - 1)/frequency
    }
    if(length(end) > 1L) {
##	if(end[2L] > frequency) stop("invalid end")
	end <- end[1L] + (end[2L] - 1)/frequency
    }
    if(missing(end))
	end <- start + (ndata - 1)/frequency
    else if(missing(start))
	start <- end - (ndata - 1)/frequency

    if(start > end) stop("'start' cannot be after 'end'")
    nobs <- floor((end - start) * frequency + 1.01)

    if(nobs != ndata)
	data <-
	    if(NCOL(data) == 1) {
		if(ndata < nobs) rep(data, length.out = nobs)
		else if(ndata > nobs) data[1L:nobs]
	    } else {
		if(ndata < nobs) data[rep(1L:ndata, length.out = nobs), ]
		else if(ndata > nobs) data[1L:nobs, ]
	    }
    ## FIXME: The following "attr<-"() calls C tspgets() which uses a
    ##  	fixed equivalent of ts.eps := 1e-5
    attr(data, "tsp") <- c(start, end, frequency) #-- order is fixed
    if(!is.null(class) && class != "none") attr(data, "class") <- class
    ## if you alter the return structure, you also need to alter
    ## newBasic in methods/R/RClassUtils.R.  So please don't.
    data
}

tsp <- function(x) attr(x, "tsp")

"tsp<-" <- function(x, value)
{
    cl <- oldClass(x)
    attr(x, "tsp") <- value # does error-checking internally
    if (inherits(x, "ts") && is.null(value))
        class(x) <- if(!identical(cl,"ts")) cl["ts" != cl]
    else if (inherits(x, "mts") && is.null(value))
        class(x) <- if(!identical(cl,"mts")) cl["mts" != cl]
    x
}

hasTsp <- function(x)
{
    if(is.null(attr(x, "tsp")))
        attr(x, "tsp") <- c(1, NROW(x), 1)
    x
}

is.ts <- function(x) inherits(x, "ts") && length(x)

as.ts <- function(x, ...) UseMethod("as.ts")

as.ts.default <- function(x, ...)
{
    if (is.ts(x)) x
    else if(!is.null(xtsp <- tsp(x))) ts(x, xtsp[1L], xtsp[2L], xtsp[3L])
    else ts(x)
}

.cbind.ts <- function(sers, nmsers, dframe = FALSE, union = TRUE)
{
    nulls <- sapply(sers, is.null)
    sers <- sers[!nulls]
    nser <- length(sers)
    if(nser == 0L) return(NULL)
    if(nser == 1L)
        if(dframe) return(as.data.frame(sers[[1L]])) else return(sers[[1L]])
    tsser <-  sapply(sers, function(x) length(tsp(x)) > 0L)
    if(!any(tsser))
        stop("no time series supplied")
    sers <- lapply(sers, as.ts)
    nsers <- sapply(sers, NCOL)
    tsps <- sapply(sers[tsser], tsp)
    freq <- mean(tsps[3,])
    if(max(abs(tsps[3,] - freq)) > getOption("ts.eps")) {
        stop("not all series have the same frequency")
    }
    if(union) {
        st <- min(tsps[1,])
        en <- max(tsps[2,])
    } else {
        st <- max(tsps[1,])
        en <- min(tsps[2,])
        if(st > en) {
            warning("non-intersecting series")
            return(NULL)
        }
    }
    p <- c(st, en, freq)
    n <- round(freq * (en - st) + 1)
    if(any(!tsser)) {
        ln <- sapply(sers[!tsser], NROW)
        if(any(ln != 1 && ln != n))
            stop("non-time series not of the correct length")
        for(i in (1L:nser)[!tsser]) {
            sers[[i]] <- ts(sers[[i]], start=st, end=en, frequency=freq)
        }
        tsps <- sapply(sers, tsp)
    }
    if(dframe) {
        x <- vector("list", nser)
        names(x) <- nmsers
    } else {
        ns <- sum(nsers)
        x <- matrix(, n, ns)
        cs <- c(0, cumsum(nsers))
        nm <- character(ns)
        for(i in 1L:nser)
            if(nsers[i] > 1) {
                cn <- colnames(sers[[i]])
                if(is.null(cn)) cn <- 1L:nsers[i]
                nm[(1+cs[i]):cs[i+1]] <- paste(nmsers[i], cn, sep=".")
            } else nm[cs[i+1]] <- nmsers[i]
        dimnames(x) <- list(NULL, nm)
    }
    for(i in 1L:nser) {
        if(union) {
            xx <-
                if(nsers[i] > 1)
                    rbind(matrix(NA, round(freq * (tsps[1,i] - st)), nsers[i]),
                          sers[[i]],
                          matrix(NA, round(freq * (en - tsps[2,i])), nsers[i]))
                else
                    c(rep.int(NA, round(freq * (tsps[1,i] - st))), sers[[i]],
                      rep.int(NA, round(freq * (en - tsps[2,i]))))
        } else {
            xx <- window(sers[[i]], st, en)
        }
        if(dframe) x[[i]] <- structure(xx, tsp=p, class="ts")
        else x[, (1+cs[i]):cs[i+1]] <- xx
    }
    if(dframe) as.data.frame(x)
    else ts(x, start=st, frequency=freq)
}

.makeNamesTs <- function(...)
{
    l <- as.list(substitute(list(...)))[-1L]
    nm <- names(l)
    fixup <- if(is.null(nm)) seq_along(l) else nm == ""
    ## <NOTE>
    dep <- sapply(l[fixup], function(x) deparse(x)[1L])
    ## We could add support for 'deparse.level' here by creating dep
    ## as in list.names() inside table().  But there is a catch: we
    ## need deparse.level = 2 to get the 'usual' deparsing when the
    ## method is invoked by the generic ...
    ## </NOTE>
    if(is.null(nm)) return(dep)
    if(any(fixup)) nm[fixup] <- dep
    nm
}

Ops.ts <- function(e1, e2)
{
    if(missing(e2)) {
        ## univariate operator
        NextMethod(.Generic)
    } else if(any(!nzchar(.Method))) {
        ## one operand is not a ts
        NextMethod(.Generic)
    } else {
        nc1 <- NCOL(e1)
        nc2 <- NCOL(e2)
        ## use ts.intersect to align e1 and e2
        e12 <- .cbind.ts(list(e1, e2),
                         c(deparse(substitute(e1))[1L],
                           deparse(substitute(e2))[1L]),
                         union = FALSE)
        e1 <- if(is.matrix(e1)) e12[, 1L:nc1, drop = FALSE] else e12[, 1]
        e2 <- if(is.matrix(e2)) e12[, nc1 + (1L:nc2), drop = FALSE]
        else e12[, nc1 + 1]
        NextMethod(.Generic)
    }
}

cbind.ts <- function(..., deparse.level = 1) {
    if(deparse.level != 1) .NotYetUsed("deparse.level != 1")
    .cbind.ts(list(...), .makeNamesTs(...), dframe = FALSE, union = TRUE)
}

ts.union <- function(..., dframe = FALSE)
    .cbind.ts(list(...), .makeNamesTs(...), dframe = dframe, union = TRUE)

ts.intersect <- function(..., dframe = FALSE)
    .cbind.ts(list(...), .makeNamesTs(...), dframe = dframe, union = FALSE)

diff.ts <- function (x, lag = 1, differences = 1, ...)
{
    if (lag < 1 | differences < 1)
        stop("bad value for 'lag' or 'differences'")
    if (lag * differences >= NROW(x)) return(x[0])
    ## <FIXME>
    ## lag() and its default method are defined in package ts, so we
    ## need to provide our own implementation.
    tsLag <- function(x, k = 1) {
        p <- tsp(x)
        tsp(x) <- p - (k/p[3L]) * c(1, 1, 0)
        x
    }
    r <- x
    for (i in 1L:differences) {
        r <- r - tsLag(r, -lag)
    }
    xtsp <- attr(x, "tsp")
    if(is.matrix(x)) colnames(r) <- colnames(x)
    ts(r, end = xtsp[2L], frequency = xtsp[3L])
}

na.omit.ts <- function(object, ...)
{
    tm <- time(object)
    xfreq <- frequency(object)
    ## drop initial and final NAs
    if(is.matrix(object))
        good <- which(apply(!is.na(object), 1L, all))
    else  good <- which(!is.na(object))
    if(!length(good)) stop("all times contain an NA")
    omit <- integer(0L)
    n <- NROW(object)
    st <- min(good)
    if(st > 1) omit <- c(omit, 1L:(st-1))
    en <- max(good)
    if(en < n) omit <- c(omit, (en+1):n)
    cl <- attr(object, "class")
    if(length(omit)) {
        object <- if(is.matrix(object)) object[st:en,] else object[st:en]
        attr(omit, "class") <- "omit"
        attr(object, "na.action") <- omit
        tsp(object) <- c(tm[st], tm[en], xfreq)
        if(!is.null(cl)) class(object) <- cl
    }
    if(any(is.na(object))) stop("time series contains internal NAs")
    object
}

is.mts <- function (x) inherits(x, "mts")

start.default <- function(x, ...)
{
    ts.eps <- getOption("ts.eps")
    tsp <- attr(hasTsp(x), "tsp")
    is <- tsp[1L]*tsp[3L]
    if(abs(tsp[3L] - round(tsp[3L])) < ts.eps &&
       abs(is - round(is)) < ts.eps) {
	is <- floor(tsp[1L]+ts.eps)
	fs <- floor(tsp[3L]*(tsp[1L] - is)+0.001)
	c(is,fs+1)
    }
    else tsp[1L]
}

end.default <- function(x, ...)
{
    ts.eps <- getOption("ts.eps")
    tsp <- attr(hasTsp(x), "tsp")
    is <- tsp[2L]*tsp[3L]
    if(abs(tsp[3L] - round(tsp[3L])) < ts.eps &&
       abs(is - round(is)) < ts.eps) {
	is <- floor(tsp[2L]+ts.eps)
	fs <- floor(tsp[3L]*(tsp[2L] - is)+0.001)
	c(is, fs+1)
    }
    else tsp[2L]
}

frequency.default <- function(x, ...)
    if(!is.null(xtsp <- attr(x, "tsp"))) xtsp[3L] else 1

deltat.default <- function(x, ...)
    if(!is.null(xtsp <- attr(x, "tsp"))) 1/xtsp[3L] else 1

time.default <- function (x, offset = 0, ...)
{
    n <- if(is.matrix(x)) nrow(x) else length(x)
    xtsp <- attr(hasTsp(x), "tsp")
    y <- seq.int(xtsp[1L], xtsp[2L], length.out = n) + offset/xtsp[3L]
    tsp(y) <- xtsp
    y
}

time.ts <- function (x, ...) as.ts(time.default(x, ...))

cycle.default <- function(x, ...)
{
    p <- tsp(hasTsp(x))
    m <- round((p[1L] %% 1) * p[3L])
    x <- (1L:NROW(x) + m - 1) %% p[3L] + 1
    tsp(x) <- p
    x
}

cycle.ts <- function (x, ...) as.ts(cycle.default(x, ...))

print.ts <- function(x, calendar, ...)
{
    x.orig <- x
    x <- as.ts(x)
    fr.x <- frequency(x)
    if(missing(calendar))
	calendar <- any(fr.x == c(4,12)) && length(start(x)) == 2L
    ## sanity check
    Tsp <- tsp(x)
    if(is.null(Tsp)) {
        warning("series is corrupt, with no 'tsp' attribute")
        print(unclass(x))
        return(invisible(x))
    }
    nn <- 1 + round((Tsp[2L] - Tsp[1L]) * Tsp[3L])
    if(NROW(x) != nn) {
        warning(gettextf("series is corrupt: length %d with 'tsp' implying %d",
                         NROW(x), nn), domain=NA, call.=FALSE)
        calendar <- FALSE
    }
    if(!calendar)
        header <- function(x) {
            if((fr.x <- frequency(x))!= 1)
                cat("Time Series:\nStart =", deparse(start(x)),
                    "\nEnd =", deparse(end(x)),
                    "\nFrequency =", deparse(fr.x), "\n")
            else
                cat("Time Series:\nStart =", format(tsp(x)[1L]),
                    "\nEnd =", format(tsp(x)[2L]),
                    "\nFrequency =", deparse(fr.x), "\n")
        }
    if(NCOL(x) == 1) { # could be 1-col matrix
        if(calendar) {
            if(fr.x > 1) {
                dn2 <-
                    if(fr.x == 12) month.abb
                    else if(fr.x == 4) {
                        c("Qtr1", "Qtr2", "Qtr3", "Qtr4")
                    } else paste("p", 1L:fr.x, sep = "")
                if(NROW(x) <= fr.x && start(x)[1L] == end(x)[1L]) {
                    ## not more than one period
                    dn1 <- start(x)[1L]
                    dn2 <- dn2[1 + (start(x)[2L] - 2 + seq_along(x))%%fr.x]
                    x <- matrix(format(x, ...), nrow = 1L , byrow = TRUE,
                                dimnames = list(dn1, dn2))
                } else { # more than one period
                    start.pad <- start(x)[2L] - 1
                    end.pad <- fr.x - end(x)[2L]
                    dn1 <- start(x)[1L]:end(x)[1L]
                    x <- matrix(c(rep.int("", start.pad), format(x, ...),
                                  rep.int("", end.pad)), ncol =  fr.x,
                                byrow = TRUE, dimnames = list(dn1, dn2))
                }
            } else { ## fr.x == 1
                tx <- time(x)
                attributes(x) <- NULL
                names(x) <- tx
            }
        } else { ##-- no 'calendar' --
            header(x)
            attr(x, "class") <- attr(x, "tsp") <- attr(x, "na.action") <- NULL
        }
    } else { # multi-column matrix
	if(calendar && fr.x > 1) {
	    tm <- time(x)
	    t2 <- 1 + round(fr.x*((tm+0.001) %%1))
	    p1 <- format(floor(zapsmall(tm)))# yr
	    rownames(x) <-
		if(fr.x == 12)
		    paste(month.abb[t2], p1, sep=" ")
		else
		    paste(p1, if(fr.x == 4) c("Q1", "Q2", "Q3", "Q4")[t2]
			      else format(t2),
			  sep=" ")
        } else {
            if(!calendar) header(x)
            rownames(x) <- format(time(x))
        }
        attr(x, "class") <- attr(x, "tsp") <- attr(x, "na.action") <- NULL
    }
    NextMethod("print", x, quote = FALSE, right = TRUE, ...)
    invisible(x.orig)
}

plot.ts <-
    function (x, y = NULL, plot.type = c("multiple", "single"),
	      xy.labels, xy.lines, panel = lines, nc, yax.flip = FALSE,
	      mar.multi = c(0, 5.1, 0, if(yax.flip) 5.1 else 2.1),
	      oma.multi = c(6, 0, 5, 0), axes = TRUE, ...)
{
    plotts <-
	function (x, y = NULL, plot.type = c("multiple", "single"),
		  xy.labels, xy.lines, panel = lines, nc,
		  xlabel, ylabel, type = "l", xlim = NULL, ylim = NULL,
		  xlab = "Time", ylab, log = "", col = par("col"), bg = NA,
		  pch = par("pch"), cex = par("cex"),
		  lty = par("lty"), lwd = par("lwd"),
		  axes = TRUE, frame.plot = axes, ann = par("ann"),
                  cex.lab = par("cex.lab"), col.lab = par("col.lab"),
                  font.lab = par("font.lab"), cex.axis = par("cex.axis"),
                  col.axis = par("col.axis"), font.axis = par("font.axis"),
		  main = NULL, ...)
    {
	plot.type <- match.arg(plot.type)
	nser <- NCOL(x)

	if(plot.type == "multiple" && nser > 1) {
	    addmain <- function(main, cex.main=par("cex.main"),
				font.main=par("font.main"),
				col.main=par("col.main"), ...)
		## pass 'cex.main' etc	via "..." from main function
		mtext(main, side=3, line=3,
		      cex=cex.main, font=font.main, col=col.main, ...)
	    panel <- match.fun(panel)
	    nser <- NCOL(x)
	    if(nser > 10) stop("cannot plot more than 10 series as \"multiple\"")
	    if(is.null(main)) main <- xlabel
	    nm <- colnames(x)
	    if(is.null(nm)) nm <- paste("Series", 1L:nser)
	    if(missing(nc)) nc <- if(nser > 4) 2 else 1
	    nr <- ceiling(nser/nc)

	    oldpar <- par(mar = mar.multi, oma = oma.multi, mfcol = c(nr, nc))
	    on.exit(par(oldpar))
	    for(i in 1L:nser) {
		plot.default(x[, i], axes = FALSE, xlab="", ylab="",
		     log = log, col = col, bg = bg, pch = pch, ann = ann,
		     type = "n", ...)
		panel(x[, i], col = col, bg = bg, pch = pch, type=type, ...)
		if(frame.plot) box(...)
		y.side <- if (i %% 2 || !yax.flip) 2 else 4
		do.xax <- i %% nr == 0 || i == nser
		if(axes) {
		    axis(y.side, xpd = NA, cex.axis = cex.axis,
                         col.axis = col.axis, font.axis = font.axis)
		    if(do.xax)
			axis(1, xpd = NA, cex.axis = cex.axis,
                             col.axis = col.axis, font.axis = font.axis)
		}
		if(ann) {
		    mtext(nm[i], y.side, line=3, cex=cex.lab, col=col.lab,
                          font=font.lab, ...)
		    if(do.xax)
			mtext(xlab, side=1, line=3, cex=cex.lab, col=col.lab,
                          font=font.lab, ...)
		}
	    }
	    if(ann && !is.null(main)) {
		par(mfcol=c(1,1))
		addmain(main, ...)
	    }
	    return(invisible())
	}
	## end of multiple plot section

	x <- as.ts(x)
	if(!is.null(y)) {
	    ## want ("scatter") plot of y ~ x
	    y <- hasTsp(y)
	    if(NCOL(x) > 1 || NCOL(y) > 1)
		stop("scatter plots only for univariate time series")
	    if (is.ts(x) && is.ts(y)) {
		xy <- ts.intersect(x, y)
		xy <- xy.coords(xy[,1], xy[,2], xlabel, ylabel, log)
	    } else
	    xy <- xy.coords(x, y, xlabel, ylabel, log)
	    xlab <- if (missing(xlab)) xy$xlab else xlab
	    ylab <- if (missing(ylab)) xy$ylab else ylab
	    xlim <- if (is.null(xlim)) range(xy$x[is.finite(xy$x)]) else xlim
	    ylim <- if (is.null(ylim)) range(xy$y[is.finite(xy$y)]) else ylim
	    n <- length(xy $ x)		  #-> default for xy.l(ines|abels)
	    if(missing(xy.labels)) xy.labels <- (n <= 150)
	    if(!is.logical(xy.labels)) {
		if(!is.character(xy.labels))
		    stop("'xy.labels' must be logical or character")
		do.lab <- TRUE
	    } else do.lab <- xy.labels

	    ptype <-
		if(do.lab) "n" else if(missing(type)) "p" else type
	    plot.default(xy, type = ptype,
			 xlab = xlab, ylab = ylab,
			 xlim = xlim, ylim = ylim, log = log, col = col, bg = bg,
			 pch = pch, axes = axes, frame.plot = frame.plot,
			 ann = ann, main = main, ...)
	    if(missing(xy.lines)) xy.lines <- do.lab
	    if(do.lab)
		text(xy, labels =
		     if(is.character(xy.labels)) xy.labels
		     else if(all(tsp(x) == tsp(y))) formatC(time(x), width = 1)
		     else seq_along(xy$x),
		     col = col, cex = cex)
	    if(xy.lines)
		lines(xy, col = col, lty = lty, lwd = lwd,
		      type = if(do.lab) "c" else "l")
	    return(invisible())
	}
	## Else : no y, only x

	if(missing(ylab)) {
	    ylab <- colnames(x)
	    if(length(ylab) != 1L)
		ylab <- xlabel
	}
	## using xy.coords() mainly for the log treatment
	if(is.matrix(x)) {
	    k <- ncol(x)
	    tx <- time(x)
	    xy <- xy.coords(x = matrix(rep.int(tx, k), ncol = k),
			    y = x, log=log)
	    xy$x <- tx
	}
	else xy <- xy.coords(x, NULL, log=log)
	if(is.null(xlim)) xlim <- range(xy$x)
	if(is.null(ylim)) ylim <- range(xy$y[is.finite(xy$y)])
	plot.new()
	plot.window(xlim, ylim, log, ...)
	if(is.matrix(x)) {
	    for(i in seq_len(k))
		lines.default(xy$x, x[,i],
			      col = col[(i-1L) %% length(col) + 1L],
			      lty = lty[(i-1L) %% length(lty) + 1L],
			      lwd = lwd[(i-1L) %% length(lwd) + 1L],
			      bg  = bg [(i-1L) %% length(bg) + 1L],
			      pch = pch[(i-1L) %% length(pch) + 1L],
			      type = type)
	}
	else {
	    lines.default(xy$x, x, col = col[1L], bg = bg, lty = lty[1L],
			  lwd = lwd[1L], pch = pch[1L], type = type)
	}
	if (ann)
	    title(main = main, xlab = xlab, ylab = ylab, ...)
	if (axes) {
	    axis(1, ...)
	    axis(2, ...)
	}
	if (frame.plot) box(...)
    }
    xlabel <- if (!missing(x)) deparse(substitute(x))# else NULL
    ylabel <- if (!missing(y)) deparse(substitute(y))
    plotts(x = x, y = y, plot.type = plot.type,
	   xy.labels = xy.labels, xy.lines = xy.lines,
	   panel = panel, nc = nc, xlabel = xlabel, ylabel = ylabel,
           axes = axes, ...)
}

lines.ts <- function(x, ...)
    lines.default(time(as.ts(x)), x, ...)


window.default <- function(x, start = NULL, end = NULL,
                           frequency = NULL, deltat = NULL,
                           extend = FALSE, ...)
{
    x <- hasTsp(x)
    xtsp <- tsp(x)
    xfreq <- xtsp[3L]
    xtime <- time(x)
    ts.eps <- getOption("ts.eps")

    if(!is.null(frequency) && !is.null(deltat) &&
       abs(frequency*deltat - 1) > ts.eps)
        stop("'frequency' and 'deltat' are both supplied and are inconsistent")
    if (is.null(frequency) && is.null(deltat)) yfreq <- xfreq
    else if (is.null(deltat)) yfreq <- frequency
    else if (is.null(frequency)) yfreq <- 1/deltat
    thin <- round(xfreq/yfreq)
    if (yfreq > 0 && abs(xfreq/yfreq -thin) < ts.eps) {
        yfreq <- xfreq/thin
    } else {
        thin <- 1
        yfreq <- xfreq
        warning("Frequency not changed")
    }
    start <- if(is.null(start))
	xtsp[1L]
    else switch(length(start),
		start,
		start[1L] + (start[2L] - 1)/xfreq,
		stop("bad value for 'start'"))
    if(start < xtsp[1L]-ts.eps/xfreq && !extend) {
	start <- xtsp[1L]
	warning("'start' value not changed")
    }

    end <- if(is.null(end))
	xtsp[2L]
    else switch(length(end),
		end,
		end[1L] + (end[2L] - 1)/xfreq,
		stop("bad value for 'end'"))
    if(end > xtsp[2L]+ts.eps/xfreq && !extend) {
	end <- xtsp[2L]
	warning("'end' value not changed")
    }

    if(start > end)
	stop("'start' cannot be after 'end'")

    if(!extend) {
        if(all(abs(start - xtime) > ts.eps/xfreq))
            start <- xtime[(xtime > start) & ((start + 1/xfreq) > xtime)]

        if(all(abs(end - xtime) > ts.eps/xfreq))
            end <- xtime[(xtime < end) & ((end - 1/xfreq) < xtime)]

        i <- seq.int(trunc((start - xtsp[1L]) * xfreq + 1.5),
                     trunc((end - xtsp[1L]) * xfreq + 1.5), by = thin)
        y <- if(is.matrix(x)) x[i, , drop = FALSE] else x[i]
        ystart <- xtime[i[1L]]
        yend <- xtime[i[length(i)]]
        attr(y, "tsp") <- c(ystart, yend, yfreq)
    } else {
        ## first adjust start and end to the time base
        ## try to ensure that they are exactly n/xfreq
        stoff <- ceiling((start - xtsp[1L]) * xfreq - ts.eps)
        ystart <- (round(xtsp[1L]*xfreq) + stoff)/xfreq
        enoff <- floor((end - xtsp[2L]) * xfreq + ts.eps)
        yend <- (round(xtsp[2L]*xfreq) + enoff)/xfreq
        nold <- round(xfreq*(xtsp[2L] - xtsp[1L])) + 1
        ## both start and end could be outside time base
        ## and indeed the new ad old ranges might not intersect.
        i <- if(start > xtsp[2L]+ts.eps/xfreq || end < xtsp[1L] - ts.eps/xfreq)
            rep(nold+1, floor(1+(end-start)*xfreq + ts.eps))
        else {
            i0 <- 1+max(0, stoff); i1 <- nold + min(0, enoff)
            c(rep.int(nold+1, max(0, -stoff)),
              if(i0 <= i1) i0:i1,
              rep.int(nold+1, max(0, enoff)))
        }
        y <- if(is.matrix(x)) rbind(x, NA)[i, , drop = FALSE] else c(x, NA)[i]
        attr(y, "tsp") <- c(ystart, yend, xfreq)
        if(yfreq != xfreq) y <- Recall(y, frequency = yfreq)
    }
    y
}

window.ts <- function (x, ...) as.ts(window.default(x, ...))

"window<-" <- function(x, ..., value) UseMethod("window<-")

"window<-.ts" <- function(x, start, end, frequency, deltat, ..., value)
{
    xtsp <- tsp(x)
    m <- match.call(expand.dots = FALSE)
    m$value <- NULL
    m$extend <- TRUE
    m[[1L]] <- as.name("window")
    xx <- eval.parent(m)
    xxtsp <- tsp(xx)
    start <- xxtsp[1L]; end <- xxtsp[2L]
    if(start > end) stop("'start' > 'end'")
    if (start < xtsp[1L] || end > xtsp[2L]) {
        warning("extending time series when replacing values", call. = FALSE)
        x <- window(x, min(start, xtsp[1L]), max(end, xtsp[2L]), extend = TRUE)
    }
    xfreq <- xtsp[3L]
    xtimes <- round(xfreq*time(x))
    xxtimes <- round(xfreq * time(xx))

    ind <- match(xxtimes, xtimes)
    if(any(is.na(ind))) stop("times to be replaced do not match")

    len <- length(ind)
    val_len <- length(value)
    if(!val_len) stop("no replacement values supplied")
    if(val_len > len) stop("too many replacement values supplied")
    if(val_len > 1L && (len %% val_len))
        stop("number of values supplied is not a sub-multiple of the number of values to be replaced")
    if(NCOL(x) == 1L) x[ind] <- value else x[ind, ] <- value
    x
}

"[.ts" <- function (x, i, j, drop = TRUE) {
    y <- NextMethod("[")
    if (missing(i))
	ts(y, start = start(x), frequency = frequency(x))
#     else {
#         if(is.matrix(i)) return(y)
# 	n <- if (is.matrix(x)) nrow(x) else length(x)
# 	li <- length(ind <- (1L:n)[i])
#         if(li == 0) return(numeric(0))
#         if(li == 1) {
#             tsp(y) <- c(start(x), start(x), frequency(x))
#             class(y) <- class(x)
#             return(y)
#         }
# 	if (length(unique(ind[-1L] - ind[-li])) != 1) {
# 	    warning("Not returning a time series object")
# 	} else {
# 	    xtsp <- tsp(x)
# 	    xtimes <- seq(from = xtsp[1L], to = xtsp[2L], by = 1 / xtsp[3L])
# 	    ytsp <- xtimes[range(ind)]
# 	    tsp(y) <- c(ytsp, (li - 1) / (ytsp[2L] - ytsp[1L]))
#             class(y) <- class(x)
# 	}
# 	y
#     }
    else y
}

"[<-.ts" <- function (x, i, j, value) {
    y <- NextMethod("[<-")
    if (NROW(y) != NROW(x)) stop("only replacement of elements is allowed")
    y
}

t.ts <- function(x) {
    cl <- oldClass(x)
    other <- !(cl %in% c("ts","mts"))
    class(x) <- if(any(other)) cl[other]
    attr(x, "tsp") <- NULL
    t(x)
}

ts.plot <- function(..., gpars = list())
{
    dots <- list(...)
    pars <- c("xlab", "ylab", "xlim", "ylim", "col", "lty", "lwd",
              "type", "main", "sub", "log")
    m <- names(dots) %in% pars
    if(length(m)) {
        gpars <- c(gpars, dots[m])
        dots <- dots[!m]
    }
    sers <- do.call("ts.union", dots)
    if(is.null(gpars$ylab))
        gpars$ylab <- if(NCOL(sers) > 1) "" else deparse(substitute(...))
    do.call("plot.ts", c(list(sers, plot.type = "single"), gpars))
}

arima.sim <- function(model, n, rand.gen = rnorm,
                      innov = rand.gen(n, ...), n.start = NA,
                      start.innov = rand.gen(n.start, ...), ...)
{
    if(!is.list(model)) stop("'model' must be list")
    p <- length(model$ar)
    if(p) {
        minroots <- min(Mod(polyroot(c(1, -model$ar))))
        if(minroots <= 1) stop("'ar' part of model is not stationary")
    }
    q <- length(model$ma)
    if(is.na(n.start)) n.start <- p + q +
        ifelse(p > 0, ceiling(6/log(minroots)), 0)
    if(n.start < p + q) stop("burn-in 'n.start' must be as long as 'ar + ma'")
    d <- 0
    if(!is.null(ord <- model$order)) {
        if(length(ord) != 3L) stop("'model$order' must be of length 3")
        if(p != ord[1L]) stop("inconsistent specification of 'ar' order")
        if(q != ord[3L]) stop("inconsistent specification of 'ma' order")
        d <- ord[2L]
        if(d != round(d) || d < 0)
            stop("number of differences must be a positive integer")
    }
    if(!missing(start.innov) && length(start.innov) < n.start)
        stop(gettextf("'start.innov' is too short: need %d points", n.start),
             domain = NA)
    x <- ts(c(start.innov[1L:n.start], innov[1L:n]), start = 1 - n.start)
    if(length(model$ma)) x <- filter(x, c(1, model$ma), sides = 1)
    if(length(model$ar)) x <- filter(x, model$ar, method = "recursive")
    if(n.start > 0) x <- x[-(1L:n.start)]
    if(d > 0) x <- diffinv(x, differences = d)
    as.ts(x)
}
#  File src/library/stats/R/tukeyline.R
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

line <- function(x, y=NULL)
{
    xy <- xy.coords(x, y)
    ok <- complete.cases(xy$x,xy$y)
    n <- length(ok)
    if(n <= 1) stop("insufficient observations")
    z <- .C("tukeyline",
	    as.double(xy$x[ok]),
	    as.double(xy$y[ok]),
	    double(n),
	    double(n),
	    n,
	    double(2),
	    DUP=FALSE, PACKAGE="stats")
    value <- list(call=sys.call(), coefficients = z[[6]],
                  residuals = z[[3L]], fitted.values = z[[4L]])
    class(value) <- "tukeyline"
    value
}
#coef.tukeyline <- coef.lm
residuals.tukeyline <- residuals.lm
# fitted.tukeyline <- fitted.lm
print.tukeyline <- print.lm
#  File src/library/stats/R/update.R
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

update.default <-
    function (object, formula., ..., evaluate = TRUE)
{
    call <- object$call
    if (is.null(call))
	stop("need an object with call component")
    extras <- match.call(expand.dots = FALSE)$...
    if (!missing(formula.))
	call$formula <- update.formula(formula(object), formula.)
    if(length(extras)) {
	existing <- !is.na(match(names(extras), names(call)))
	## do these individually to allow NULL to remove entries.
	for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
	if(any(!existing)) {
	    call <- c(as.list(call), extras[!existing])
	    call <- as.call(call)
	}
    }
    if(evaluate) eval(call, parent.frame())
    else call
}

update.formula <- function (old, new, ...) {
    tmp <- .Internal(update.formula(as.formula(old), as.formula(new)))
    out <- formula(terms.formula(tmp, simplify = TRUE))
    return(out)
}
#  File src/library/stats/R/var.test.R
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

var.test <- function(x, ...) UseMethod("var.test")

var.test.default <-
function(x, y, ratio = 1,
         alternative = c("two.sided", "less", "greater"),
         conf.level = 0.95, ...)
{
    if (!((length(ratio) == 1L) && is.finite(ratio) && (ratio > 0)))
        stop("'ratio' must be a single positive number")

    alternative <- match.arg(alternative)

    if (!((length(conf.level) == 1L) && is.finite(conf.level) &&
          (conf.level > 0) && (conf.level < 1)))
        stop("'conf.level' must be a single number between 0 and 1")

    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))

    if (inherits(x, "lm") && inherits(y, "lm")) {
        DF.x <- x$df.residual
        DF.y <- y$df.residual
        V.x <- sum(x$residuals^2) / DF.x
        V.y <- sum(y$residuals^2) / DF.y
    } else {
        x <- x[is.finite(x)]
        DF.x <- length(x) - 1L
        if (DF.x < 1L)
            stop("not enough 'x' observations")
        y <- y[is.finite(y)]
        DF.y <- length(y) - 1L
        if (DF.y < 1L)
            stop("not enough 'y' observations")
        V.x <- var(x)
        V.y <- var(y)
    }
    ESTIMATE <- V.x / V.y
    STATISTIC <- ESTIMATE / ratio
    PARAMETER <- c(DF.x, DF.y)

    PVAL <- pf(STATISTIC, DF.x, DF.y)
    if (alternative == "two.sided") {
        PVAL <- 2 * min(PVAL, 1 - PVAL)
        BETA <- (1 - conf.level) / 2
        CINT <- c(ESTIMATE / qf(1 - BETA, DF.x, DF.y),
                  ESTIMATE / qf(BETA, DF.x, DF.y))
    }
    else if (alternative == "greater") {
        PVAL <- 1 - PVAL
        CINT <- c(ESTIMATE / qf(conf.level, DF.x, DF.y), Inf)
    }
    else
        CINT <- c(0, ESTIMATE / qf(1 - conf.level, DF.x, DF.y))
    names(STATISTIC) <- "F"
    names(PARAMETER) <- c("num df", "denom df")
    names(ESTIMATE) <- names(ratio) <- "ratio of variances"
    attr(CINT, "conf.level") <- conf.level
    RVAL <- list(statistic = STATISTIC,
                 parameter = PARAMETER,
                 p.value = PVAL,
                 conf.int = CINT,
                 estimate = ESTIMATE,
                 null.value = ratio,
                 alternative = alternative,
                 method = "F test to compare two variances",
                 data.name = DNAME)
    attr(RVAL, "class") <- "htest"
    return(RVAL)
}

var.test.formula <-
function(formula, data, subset, na.action, ...)
{
    if(missing(formula)
       || (length(formula) != 3L)
       || (length(attr(terms(formula[-2L]), "term.labels")) != 1L))
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m[[1L]] <- as.name("model.frame")
    m$... <- NULL
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " by ")
    names(mf) <- NULL
    response <- attr(attr(mf, "terms"), "response")
    g <- factor(mf[[-response]])
    if(nlevels(g) != 2)
        stop("grouping factor must have exactly 2 levels")
    DATA <- split(mf[[response]], g)
    names(DATA) <- c("x", "y")
    y <- do.call("var.test", c(DATA, list(...)))
    y$data.name <- DNAME
    y
}
#  File src/library/stats/R/vcov.R
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

vcov <- function(object, ...) UseMethod("vcov")

## The next three have to call the method, as classes which
## inherit from "glm" need not have summary methods which
## inherit from "summary.glm".
vcov.glm <- function(object, ...)
{
    so <- summary.glm(object, corr=FALSE, ...)
    so$dispersion * so$cov.unscaled
}

vcov.lm <- function(object, ...)
{
    so <- summary.lm(object, corr=FALSE)
    so$sigma^2 * so$cov.unscaled
}

vcov.mlm <- function(object, ...)
{
    so <- summary.mlm(object, corr=FALSE)[[1L]]
    kronecker(estVar(object), so$cov.unscaled, make.dimnames = TRUE)
}

## gls and lme methods moved to nlme in 2.6.0

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

#  File src/library/stats/R/wilcox.test.R
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

wilcox.test <- function(x, ...) UseMethod("wilcox.test")

wilcox.test.default <-
function(x, y = NULL, alternative = c("two.sided", "less", "greater"),
         mu = 0, paired = FALSE, exact = NULL, correct = TRUE,
         conf.int = FALSE, conf.level = 0.95, ...)
{
    alternative <- match.arg(alternative)
    if(!missing(mu) && ((length(mu) > 1) || !is.finite(mu)))
        stop("'mu' must be a single number")
    if(conf.int) {
        if(!((length(conf.level) == 1)
             && is.finite(conf.level)
             && (conf.level > 0)
             && (conf.level < 1)))
            stop("'conf.level' must be a single number between 0 and 1")
    }

    if(!is.numeric(x)) stop("'x' must be numeric")
    if(!is.null(y)) {
        if(!is.numeric(y)) stop("'y' must be numeric")
        DNAME <- paste(deparse(substitute(x)), "and",
                       deparse(substitute(y)))
        if(paired) {
            if(length(x) != length(y))
                stop("'x' and 'y' must have the same length")
            OK <- complete.cases(x, y)
            x <- x[OK] - y[OK]
            y <- NULL
        }
        else {
            x <- x[is.finite(x)]
            y <- y[is.finite(y)]
        }
    } else {
        DNAME <- deparse(substitute(x))
        if(paired)
            stop("'y' is missing for paired test")
        x <- x[is.finite(x)]
    }

    if(length(x) < 1)
        stop("not enough (finite) 'x' observations")
    CORRECTION <- 0
    if(is.null(y)) {
        METHOD <- "Wilcoxon signed rank test"
        x <- x - mu
        ZEROES <- any(x == 0)
        if(ZEROES)
            x <- x[x != 0]
        n <- as.double(length(x))
        if(is.null(exact))
            exact <- (n < 50)
        r <- rank(abs(x))
        STATISTIC <- sum(r[x > 0])
        names(STATISTIC) <- "V"
        TIES <- length(r) != length(unique(r))

        if(exact && !TIES && !ZEROES) {
            PVAL <-
                switch(alternative,
                       "two.sided" = {
                           p <- if(STATISTIC > (n * (n + 1) / 4))
                                psignrank(STATISTIC - 1, n, lower.tail = FALSE)
                           else psignrank(STATISTIC, n)
                           min(2 * p, 1)
                       },
                       "greater" = psignrank(STATISTIC - 1, n, lower.tail = FALSE),
                       "less" = psignrank(STATISTIC, n))
            if(conf.int) {
                ## Exact confidence interval for the median in the
                ## one-sample case.  When used with paired values this
                ## gives a confidence interval for mean(x) - mean(y).
                x <- x + mu             # we want a conf.int for the median
                alpha <- 1 - conf.level
                diffs <- outer(x, x, "+")
                diffs <- sort(diffs[!lower.tri(diffs)]) / 2
                cint <-
                    switch(alternative,
                           "two.sided" = {
                               qu <- qsignrank(alpha / 2, n)
                               if(qu == 0) qu <- 1
                               ql <- n*(n+1)/2 - qu
                               uci <- diffs[qu]
                               lci <- diffs[ql+1]
                               achieved.alpha<-2*psignrank(trunc(qu)-1,n)
                               if (achieved.alpha-alpha > (alpha)/2){
                                 warning("Requested conf.level not achievable")
                                 conf.level<-1-signif(achieved.alpha,2)
                               }
                               c(uci, lci)
                           },
                           "greater"= {
                               qu <- qsignrank(alpha, n)
                               if(qu == 0) qu <- 1
                               uci <- diffs[qu]
                               achieved.alpha<-psignrank(trunc(qu)-1,n)
                               if (achieved.alpha-alpha > (alpha)/2){
                                 warning("Requested conf.level not achievable")
                                 conf.level<-1-signif(achieved.alpha,2)
                               }
                               c(uci, +Inf)
                           },
                           "less"= {
                               qu <- qsignrank(alpha, n)
                               if(qu == 0) qu <- 1
                               ql <- n*(n+1)/2 - qu
                               lci <- diffs[ql+1]
                               achieved.alpha<-psignrank(trunc(qu)-1,n)
                               if (achieved.alpha-alpha > (alpha)/2){
                                 warning("Requested conf.level not achievable")
                                 conf.level<-1-signif(achieved.alpha,2)
                               }
                               c(-Inf, lci)
                           })
                attr(cint, "conf.level") <- conf.level
                ESTIMATE <- median(diffs)
                names(ESTIMATE) <- "(pseudo)median"

            }
        } else { ## not exact, maybe ties or zeroes
            NTIES <- table(r)
            z <- STATISTIC - n * (n + 1)/4
            SIGMA <- sqrt(n * (n + 1) * (2 * n + 1) / 24
                          - sum(NTIES^3 - NTIES) / 48)
            if(correct) {
                CORRECTION <-
                    switch(alternative,
                           "two.sided" = sign(z) * 0.5,
                           "greater" = 0.5,
                           "less" = -0.5)
                METHOD <- paste(METHOD, "with continuity correction")
            }
	    z <- (z - CORRECTION) / SIGMA
	    PVAL <- switch(alternative,
			   "less" = pnorm(z),
			   "greater" = pnorm(z, lower.tail=FALSE),
			   "two.sided" = 2 * min(pnorm(z),
						 pnorm(z, lower.tail=FALSE)))
            if(conf.int) {
                ## Asymptotic confidence interval for the median in the
                ## one-sample case.  When used with paired values this
                ## gives a confidence interval for mean(x) - mean(y).
                ## Algorithm not published, thus better documented here.
                x <- x + mu
                alpha <- 1 - conf.level
                ## These are sample based limits for the median
                ## [They don't work if alpha is too high]
                mumin <- min(x)
                mumax <- max(x)
                ## wdiff(d, zq) returns the absolute difference between
                ## the asymptotic Wilcoxon statistic of x - mu - d and
                ## the quantile zq.
                wdiff <- function(d, zq) {
                    xd <- x - d
                    xd <- xd[xd != 0]
                    nx <- length(xd)
                    dr <- rank(abs(xd))
		    zd <- sum(dr[xd > 0]) - nx * (nx + 1)/4
		    NTIES.CI <- table(dr)
		    SIGMA.CI <- sqrt(nx * (nx + 1) * (2 * nx + 1) / 24
				     - sum(NTIES.CI^3 - NTIES.CI) / 48)
		    CORRECTION.CI <-
			if(correct) {
                            switch(alternative,
                                   "two.sided" = sign(zd) * 0.5,
                                   "greater" = 0.5,
                                   "less" = -0.5)
			} else 0
		    (zd - CORRECTION.CI) / SIGMA.CI - zq
                }
                ## Here we optimize the function wdiff in d over the set
                ## c(mumin, mumax).
                ## This returns a value from c(mumin, mumax) for which
                ## the asymptotic Wilcoxon statistic is equal to the
                ## quantile zq.  This means that the statistic is not
                ## within the critical region, and that implies that d
                ## is a confidence limit for the median.
                ##
                ## As in the exact case, interchange quantiles.
                cint <- switch(alternative, "two.sided" = {
                  repeat({
                    mindiff<-wdiff(mumin,zq=qnorm(alpha/2, lower.tail=FALSE))
                    maxdiff<-wdiff(mumax,zq=qnorm(alpha/2))
                    if(mindiff<0 || maxdiff>0){
                      alpha<-alpha*2
                    } else break
                    })
                  if(1-conf.level < alpha*0.75){
                    conf.level<-1-alpha
                    warning("Requested conf.level not achievable")
                  }
                  l <- uniroot(wdiff, c(mumin, mumax), tol=1e-4,
                               zq=qnorm(alpha/2, lower.tail=FALSE))$root
                  u <- uniroot(wdiff, c(mumin, mumax), tol=1e-4,
                               zq=qnorm(alpha/2))$root
                  c(l, u)
                }, "greater"= {
                  repeat({
                    mindiff<-wdiff(mumin,zq=qnorm(alpha, lower.tail=FALSE))
                    if(mindiff<0){
                      alpha<-alpha*2
                    } else break
                    })
                  if(1-conf.level < alpha*0.75){
                    conf.level<-1-alpha
                    warning("Requested conf.level not achievable")
                  }
                  l <- uniroot(wdiff, c(mumin, mumax), tol=1e-4,
                               zq=qnorm(alpha, lower.tail=FALSE))$root
                    c(l, +Inf)
                }, "less"= {
                  repeat({
                    maxdiff<-wdiff(mumax,zq=qnorm(alpha))
                    if(maxdiff>0){
                      alpha<-alpha*2
                    } else break
                    })
                  if(1-conf.level < alpha*0.75){
                    conf.level<-1-alpha
                    warning("Requested conf.level not achievable")
                  }
                  u <- uniroot(wdiff, c(mumin, mumax), tol=1e-4,
                                  zq=qnorm(alpha))$root
                  c(-Inf, u)
                })
                attr(cint, "conf.level") <- conf.level
		correct <- FALSE # no continuity correction for estimate
                ESTIMATE <- uniroot(wdiff, c(mumin, mumax), tol=1e-4,
                                    zq=0)$root
		names(ESTIMATE) <- "(pseudo)median"

            }

            if(exact && TIES) {
                warning("cannot compute exact p-value with ties")
                if(conf.int)
                    warning("cannot compute exact confidence interval with ties")
            }
            if(exact && ZEROES) {
                warning("cannot compute exact p-value with zeroes")
                if(conf.int)
                    warning("cannot compute exact confidence interval with zeroes")
            }

	}
    }
    else { ##-------------------------- 2-sample case ---------------------------
        if(length(y) < 1)
            stop("not enough 'y' observations")
        METHOD <- "Wilcoxon rank sum test"
        r <- rank(c(x - mu, y))
        n.x <- as.double(length(x))
        n.y <- as.double(length(y))
        if(is.null(exact))
            exact <- (n.x < 50) && (n.y < 50)
        STATISTIC <- sum(r[seq_along(x)]) - n.x * (n.x + 1) / 2
        names(STATISTIC) <- "W"
        TIES <- (length(r) != length(unique(r)))
        if(exact && !TIES) {
            PVAL <-
                switch(alternative,
                       "two.sided" = {
                           p <- if(STATISTIC > (n.x * n.y / 2))
                               pwilcox(STATISTIC - 1, n.x, n.y,
                                       lower.tail = FALSE)
                           else
                               pwilcox(STATISTIC, n.x, n.y)
                           min(2 * p, 1)
                       },
                       "greater" = {
                           pwilcox(STATISTIC - 1, n.x, n.y,
                                   lower.tail = FALSE)
                       },
                       "less" = pwilcox(STATISTIC, n.x, n.y))
            if(conf.int) {
                ## Exact confidence interval for the location parameter
                ## mean(x) - mean(y) in the two-sample case (cf. the
                ## one-sample case).
                alpha <- 1 - conf.level
                diffs <- sort(outer(x, y, "-"))
                cint <-
                    switch(alternative,
                           "two.sided" = {
                               qu <- qwilcox(alpha/2, n.x, n.y)
                               if(qu == 0) qu <- 1
                               ql <- n.x*n.y - qu
                               uci <- diffs[qu]
                               lci <- diffs[ql + 1]
                               achieved.alpha<-2*pwilcox(trunc(qu)-1,n.x,n.y)
                               if (achieved.alpha-alpha > alpha/2){
                                 warning("Requested conf.level not achievable")
                                 conf.level<-1-achieved.alpha
                               }
                               c(uci, lci)
                           },
                           "greater"= {
                               qu <- qwilcox(alpha, n.x, n.y)
                               if(qu == 0) qu <- 1
                               uci <- diffs[qu]
                               achieved.alpha<-2*pwilcox(trunc(qu)-1,n.x,n.y)
                               if (achieved.alpha-alpha > alpha/2){
                                 warning("Requested conf.level not achievable")
                                 conf.level<-1-achieved.alpha
                               }
                               c(uci, +Inf)
                           },
                           "less"= {
                               qu <- qwilcox(alpha, n.x, n.y)
                               if(qu == 0) qu <- 1
                               ql <- n.x*n.y - qu
                               lci <- diffs[ql + 1]
                               achieved.alpha<-2*pwilcox(trunc(qu)-1,n.x,n.y)
                               if (achieved.alpha-alpha > alpha/2){
                                 warning("Requested conf.level not achievable")
                                 conf.level<-1-achieved.alpha
                               }
                               c(-Inf, lci)
                           })
                attr(cint, "conf.level") <- conf.level
                ESTIMATE <- median(diffs)
                names(ESTIMATE) <- "difference in location"
            }
        }
        else {
            NTIES <- table(r)
            z <- STATISTIC - n.x * n.y / 2
            SIGMA <- sqrt((n.x * n.y / 12) *
                          ((n.x + n.y + 1)
                           - sum(NTIES^3 - NTIES)
                           / ((n.x + n.y) * (n.x + n.y - 1))))
            if(correct) {
                CORRECTION <- switch(alternative,
                                     "two.sided" = sign(z) * 0.5,
                                     "greater" = 0.5,
                                     "less" = -0.5)
                METHOD <- paste(METHOD, "with continuity correction")
            }
	    z <- (z - CORRECTION) / SIGMA
	    PVAL <- switch(alternative,
			   "less" = pnorm(z),
			   "greater" = pnorm(z, lower.tail=FALSE),
			   "two.sided" = 2 * min(pnorm(z),
						 pnorm(z, lower.tail=FALSE)))
            if(conf.int) {
                ## Asymptotic confidence interval for the location
                ## parameter mean(x) - mean(y) in the two-sample case
                ## (cf. one-sample case).
                ##
                ## Algorithm not published, for a documentation see the
                ## one-sample case.
                alpha <- 1 - conf.level
                mumin <- min(x) - max(y)
                mumax <- max(x) - min(y)
                wdiff <- function(d, zq) {
                    dr <- rank(c(x - d, y))
                    NTIES.CI <- table(dr)
                    dz <- (sum(dr[seq_along(x)])
                           - n.x * (n.x + 1) / 2 - n.x * n.y / 2)
		    CORRECTION.CI <-
			if(correct) {
                            switch(alternative,
                                   "two.sided" = sign(dz) * 0.5,
                                   "greater" = 0.5,
                                   "less" = -0.5)
			} else 0
                    SIGMA.CI <- sqrt((n.x * n.y / 12) *
                                     ((n.x + n.y + 1)
                                      - sum(NTIES.CI^3 - NTIES.CI)
                                      / ((n.x + n.y) * (n.x + n.y - 1))))
                    (dz - CORRECTION.CI) / SIGMA.CI - zq
                }
                root <- function(zq) {
                    ## in extreme cases we need to return endpoints,
                    ## e.g.  wilcox.test(1, 2:60, conf.int=TRUE)
                    f.lower <- wdiff(mumin, zq)
                    if(f.lower <= 0) return(mumin)
                    f.upper <- wdiff(mumax, zq)
                    if(f.upper >= 0) return(mumax)
                    uniroot(wdiff, c(mumin, mumax),
                            f.lower = f.lower, f.upper = f.upper,
                            tol = 1e-4, zq = zq)$root
                }
                cint <- switch(alternative, "two.sided" = {
                    l <- root(zq=qnorm(alpha/2, lower.tail=FALSE))
                    u <- root(zq=qnorm(alpha/2))
                    c(l, u)
                }, "greater"= {
                    l <- root(zq=qnorm(alpha, lower.tail=FALSE))
                    c(l, +Inf)
                }, "less"= {
                    u <- root(zq=qnorm(alpha))
                    c(-Inf, u)
                })
                attr(cint, "conf.level") <- conf.level
		correct <- FALSE # no continuity correction for estimate
                ESTIMATE <- uniroot(wdiff, c(mumin, mumax), tol=1e-4,
                                    zq=0)$root
                names(ESTIMATE) <- "difference in location"
            }

            if(exact && TIES) {
                warning("cannot compute exact p-value with ties")
                if(conf.int)
                    warning("cannot compute exact confidence intervals with ties")
            }
        }
    }

    names(mu) <- if(paired || !is.null(y)) "location shift" else "location"
    RVAL <- list(statistic = STATISTIC,
                 parameter = NULL,
                 p.value = as.numeric(PVAL),
                 null.value = mu,
                 alternative = alternative,
                 method = METHOD,
                 data.name = DNAME)
    if(conf.int)
        RVAL <- c(RVAL,
                  list(conf.int = cint,
                       estimate = ESTIMATE))
    class(RVAL) <- "htest"
    return(RVAL)
}

wilcox.test.formula <-
function(formula, data, subset, na.action, ...)
{
    if(missing(formula)
       || (length(formula) != 3)
       || (length(attr(terms(formula[-2]), "term.labels")) != 1))
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m[[1L]] <- as.name("model.frame")
    m$... <- NULL
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " by ")
    names(mf) <- NULL
    response <- attr(attr(mf, "terms"), "response")
    g <- factor(mf[[-response]])
    if(nlevels(g) != 2)
        stop("grouping factor must have exactly 2 levels")
    DATA <- split(mf[[response]], g)
    names(DATA) <- c("x", "y")
    y <- do.call("wilcox.test", c(DATA, list(...)))
    y$data.name <- DNAME
    y
}
#  File src/library/stats/R/xtabs.R
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

xtabs <- function(formula = ~., data = parent.frame(), subset, sparse = FALSE,
		  na.action, exclude = c(NA, NaN), drop.unused.levels = FALSE)
{
    if (missing(formula) && missing(data))
	stop("must supply either 'formula' or 'data'")
    if(!missing(formula)){
	## We need to coerce the formula argument now, but model.frame
	## will coerce the original version later.
	formula <- as.formula(formula)
	if (!inherits(formula, "formula"))
	    stop("'formula' missing or incorrect")
    }
    if (any(attr(terms(formula, data = data), "order") > 1))
	stop("interactions are not allowed")
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame())))
	m$data <- as.data.frame(data)
    m$... <- m$exclude <- m$drop.unused.levels <- m$sparse <- NULL
    m[[1]] <- as.name("model.frame")
    mf <- eval(m, parent.frame())
    if(length(formula) == 2) {
	by <- mf
	y <- NULL
    }
    else {
	i <- attr(attr(mf, "terms"), "response")
	by <- mf[-i]
	y <- mf[[i]]
    }
    by <- lapply(by, function(u) {
	if(!is.factor(u)) u <- factor(u, exclude = exclude)
	u[ , drop = drop.unused.levels]
    })
    if(!sparse) { ## identical to stats::xtabs
	x <-
	    if(is.null(y))
		do.call("table", by)
	    else if(NCOL(y) == 1)
		tapply(y, by, sum)
	    else {
		z <- lapply(as.data.frame(y), tapply, by, sum)
		array(unlist(z),
		      dim = c(dim(z[[1]]), length(z)),
		      dimnames = c(dimnames(z[[1]]), list(names(z))))
	    }
	x[is.na(x)] <- 0
	class(x) <- c("xtabs", "table")
	attr(x, "call") <- match.call()
	x

    } else { ## sparse
	if (length(by) != 2)
	    stop("xtabs(*, sparse=TRUE) applies only to two-way tables")
        ## loadNamespace(.) is very quick, once it *is* loaded:
	if(is.null(tryCatch(loadNamespace("Matrix"), error= function(e)NULL)))
            stop("xtabs(*, sparse=TRUE) needs package \"Matrix\" correctly installed")
	rows <- by[[1]]
	cols <- by[[2]]
	rl <- levels(rows)
	cl <- levels(cols)
	if (is.null(y))
	    y <- rep.int(1, length(rows))
	as(new("dgTMatrix",
	       i = as.integer(rows) - 1L,
	       j = as.integer(cols) - 1L,
	       x = as.double(y),
	       Dim = c(length(rl), length(cl)),
	       Dimnames = list(rl, cl)), "CsparseMatrix")
    }
}

print.xtabs <- function(x, ...)
{
    ox <- x
    attr(x, "call") <- NULL
    print.table(x, ...)
    invisible(ox)
}
#  File src/library/stats/R/zzModels.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright 1997, 1999 Jose C. Pinheiro <jcp$research.bell-labs.com>,
#                       Douglas M. Bates <bates$stat.wisc.edu>
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

##*## SSasymp - asymptotic regression model

SSasymp <- # selfStart(~ Asym + (R0 - Asym) * exp(-exp(lrc) * input),
    selfStart(function(input, Asym, R0, lrc)
          {
              .expr1 <- R0 - Asym
              .expr2 <- exp(lrc)
              .expr5 <- exp((( - .expr2) * input))
              .value <- Asym + (.expr1 * .expr5)
              .actualArgs <- as.list(match.call()[c("Asym", "R0", "lrc")])
              if(all(unlist(lapply(.actualArgs, is.name))))
              {
                  .grad <- array(0, c(length(.value), 3L),
                                 list(NULL, c("Asym", "R0", "lrc")))
                  .grad[, "Asym"] <- 1 - .expr5
                  .grad[, "R0"] <- .expr5
                  .grad[, "lrc"] <-  -(.expr1*(.expr5*(.expr2*input)))
                  dimnames(.grad) <- list(NULL, .actualArgs)
                  attr(.value, "gradient") <- .grad
              }
              .value
          },
              function(mCall, data, LHS)
          {
              xy <- sortedXyData(mCall[["input"]], LHS, data)
              if (nrow(xy) < 3) {
                  stop("too few distinct input values to fit an asymptotic regression model")
              }
              if(nrow(xy) > 3) {
                  xy$ydiff <- abs(xy$y - NLSstRtAsymptote(xy))
                  xy <- data.frame(xy)
                  lrc <- log( - coef(lm(log(ydiff) ~ x, data = xy))[2L])
                  names(lrc) <- NULL
                  ## This gives an estimate of the log (rate constant).  Use that
                  ## with a partially linear nls algorithm
                  pars <- coef(nls(y ~ cbind(1 - exp( - exp(lrc) * x),
                                             exp(- exp(lrc) * x)),
                                   data = xy,
                                   start = list(lrc = lrc),
                                   algorithm = "plinear"))
              }
              else {
                  ydiff <- diff(xy$y)
                  if(prod(ydiff) <= 0) {
                      stop("cannot fit an asymptotic regression model to these data")
                  }
                  avg.resp <- xy$y
                  frac <- (avg.resp[3L] - avg.resp[1L])/(avg.resp[2L] - avg.resp[1L])
                  xunique <- unique(xy$x)
                  xdiff <- diff(xunique)
                  if(xdiff[1L] == xdiff[2L]) {	# equal spacing - can use a shortcut
                      expmRd <- frac - 1
                      rc <-  - log(expmRd)/xdiff[1L]
                      lrc <- log(rc)
                      expmRx1 <- exp( - rc * xunique[1L])
                      bma <- ydiff[1L]/(expmRx1 * (expmRd - 1))
                      Asym <- avg.resp[1L] - bma * expmRx1
                      pars <- c(lrc = lrc, Asym = Asym, R0 = bma + Asym)
                  }
                  else {
                      stop("too few observations to fit an asymptotic regression model")
                  }
              }
              names(pars) <- NULL
              val <- list(pars[2L], pars[3L], pars[1L])
              names(val) <- mCall[c("Asym", "R0", "lrc")]
              val
          },
              parameters = c("Asym", "R0", "lrc"))

##*## SSasympOff - alternate formulation of asymptotic regression model
##*## with an offset

SSasympOff <- # selfStart(~ Asym *( 1 - exp(-exp(lrc) * (input - c0) ) ),
    selfStart(
              function(input, Asym, lrc, c0)
          {
              .expr1 <- exp(lrc)
              .expr3 <- input - c0
              .expr5 <- exp((( - .expr1) * .expr3))
              .expr6 <- 1 - .expr5
              .value <- Asym * .expr6
              .actualArgs <- as.list(match.call()[c("Asym", "lrc", "c0")])
              if(all(unlist(lapply(.actualArgs, is.name))))
              {
                  .grad <- array(0, c(length(.value), 3L), list(NULL, c("Asym", "lrc", "c0")))
                  .grad[, "Asym"] <- .expr6
                  .grad[, "lrc"] <- Asym * (.expr5 * (.expr1 * .expr3))
                  .grad[, "c0"] <-  - (Asym * (.expr5 * .expr1))
                  dimnames(.grad) <- list(NULL, .actualArgs)
                  attr(.value, "gradient") <- .grad
              }
              .value
          },
              function(mCall, data, LHS)
          {
              xy <- sortedXyData(mCall[["input"]], LHS, data)
              if (nrow(xy) < 4) {
                  stop("too few distinct input values to fit the 'asympOff' model")
              }
              xy$ydiff <- abs(xy$y - NLSstRtAsymptote(xy))
              xy <- data.frame(xy)
              lrc <- log( - coef(lm(log(ydiff) ~ x, data = xy))[2L]) # log( rate constant)
              pars <- as.vector(coef(nls(y ~ cbind(1, exp(- exp(lrc) * x)),
                                         data = xy, algorithm = "plinear",
                                         start = list(lrc = lrc))))
              val <- list(pars[2L], pars[1L], exp(-pars[1L]) * log(-pars[3L]/pars[2L]))
              names(val) <- mCall[c("Asym", "lrc", "c0")]
              val
          }, parameters = c("Asym", "lrc", "c0"))

##*## SSasympOrig - exponential curve through the origin to an asymptote

SSasympOrig <- # selfStart(~ Asym * (1 - exp(-exp(lrc) * input)),
    selfStart(
              function(input, Asym, lrc)
          {
              .expr1 <- exp(lrc)
              .expr4 <- exp((( - .expr1) * input))
              .expr5 <- 1 - .expr4
              .value <- Asym * .expr5
              .actualArgs <- as.list(match.call()[c("Asym", "lrc")])
              if(all(unlist(lapply(.actualArgs, is.name))))
              {
                  .grad <- array(0, c(length(.value), 2L), list(NULL, c("Asym", "lrc")))
                  .grad[, "Asym"] <- .expr5
                  .grad[, "lrc"] <- Asym * (.expr4 * (.expr1 * input))
                  dimnames(.grad) <- list(NULL, .actualArgs)
                  attr(.value, "gradient") <- .grad
              }
              .value
          },
              function(mCall, data, LHS)
          {
              xy <- sortedXyData(mCall[["input"]], LHS, data)
              if (nrow(xy) < 3) {
                  stop("too few distinct input values to fit the 'asympOrig' model")
              }
              ## get a preliminary estimate for A
              A0 <- NLSstRtAsymptote(xy)
              ## get a least squares estimate for log of the rate constant
              lrc <- log(abs(mean(log(1 - xy$y/A0)/xy$x, na.rm = TRUE)))
              ## use the partially linear form to converge quickly
              xy <- data.frame(xy)
              pars <- as.vector(coef(nls(y ~ 1 - exp(-exp(lrc)*x),
                                         data = xy,
                                         start = list(lrc = lrc),
                                         algorithm = "plinear")))
              value <- c(pars[2L], pars[1L])
              names(value) <- mCall[c("Asym", "lrc")]
              value
          }, parameters = c("Asym", "lrc"))

##*## SSbiexp - linear combination of two exponentials

SSbiexp <-
                                        #  selfStart(~ A1 * exp(-exp(lrc1)*input) + A2 * exp(-exp(lrc2) * input),
    selfStart(
              function(input, A1, lrc1, A2, lrc2)
          {
              .expr1 <- exp(lrc1)
              .expr4 <- exp((( - .expr1) * input))
              .expr6 <- exp(lrc2)
              .expr9 <- exp((( - .expr6) * input))
              .value <- (A1 * .expr4) + (A2 * .expr9)
              .actualArgs <- as.list(match.call()[c("A1", "lrc1", "A2", "lrc2")])
              if(all(unlist(lapply(.actualArgs, is.name))))
              {
                  .grad <- array(0, c(length(.value), 4L),
                                 list(NULL, c("A1", "lrc1", "A2", "lrc2")))
                  .grad[, "A1"] <- .expr4
                  .grad[, "lrc1"] <-  - (A1 * (.expr4 * (.expr1 * input)))
                  .grad[, "A2"] <- .expr9
                  .grad[, "lrc2"] <-  - (A2 * (.expr9 * (.expr6 * input)))
                  dimnames(.grad) <- list(NULL, .actualArgs)
                  attr(.value, "gradient") <- .grad
              }
              .value
          },
              function(mCall, data, LHS)
          {
              xy <- data.frame(sortedXyData(mCall[["input"]], LHS, data))
              if (nrow(xy) < 5) {
                  stop("too few distinct input values to fit a biexponential")
              }
              ndistinct <- nrow(xy)
              nlast <- max(3, round(ndistinct/2))		# take at least half the data
              dlast <- xy[(ndistinct + 1 - nlast):ndistinct, ]
              pars2 <- coef(lm(log(y) ~ x, data = dlast))
              lrc2 <- log(abs(pars2[2L]))		# log of the slope
              xy[["res"]] <- xy[["y"]] - exp(pars2[1L]) * exp(-exp(lrc2)*xy[["x"]])
              dfirst <- xy[1L:(ndistinct - nlast), ]
              pars1 <- coef(lm(log(abs(res)) ~ x, data = dfirst))
              lrc1 <- log(abs(pars1[2L]))
              pars <- coef(nls(y ~ cbind(exp(-exp(lrc1)*x), exp(-exp(lrc2)*x)),
                               data = xy,
                               start = list(lrc1 = lrc1, lrc2 = lrc2),
                               algorithm = "plinear"))
              value <- c(pars[3L], pars[1L], pars[4L], pars[2L])
              names(value) <- mCall[c("A1", "lrc1", "A2", "lrc2")]
              value
          }, parameters = c("A1", "lrc1", "A2", "lrc2"))

##*## SSfol - first order compartment model with the log of the rates
##*##         and the clearence
SSfol <-
                                        #  selfStart(~Dose * exp(lKe + lKa - lCl) * (exp(-exp(lKe) * input) -
                                        # exp(-exp(lKa) * input))/(exp(lKa) - exp(lKe)),
    selfStart(
              function(Dose, input, lKe, lKa, lCl)
          {
              .expr4 <- Dose * (exp(((lKe + lKa) - lCl)))
              .expr5 <- exp(lKe)
              .expr8 <- exp((( - .expr5) * input))
              .expr9 <- exp(lKa)
              .expr12 <- exp((( - .expr9) * input))
              .expr14 <- .expr4 * (.expr8 - .expr12)
              .expr15 <- .expr9 - .expr5
              .expr16 <- .expr14/.expr15
              .expr23 <- .expr15^2
              .value <- .expr16
              .actualArgs <- as.list(match.call()[c("lKe", "lKa", "lCl")])
              if(all(unlist(lapply(.actualArgs, is.name))))
              {
                  .grad <- array(0, c(length(.value), 3L), list(NULL, c("lKe", "lKa", "lCl")))
                  .grad[, "lKe"] <- ((.expr14 - (.expr4 * (.expr8 * (.expr5 * input))))/
                                     .expr15) + ((.expr14 * .expr5)/.expr23)
                  .grad[, "lKa"] <- ((.expr14 + (.expr4 * (.expr12 * (.expr9 * input))))/
                                     .expr15) - ((.expr14 * .expr9)/.expr23)
                  .grad[, "lCl"] <-  - .expr16
                  dimnames(.grad) <- list(NULL, .actualArgs)
                  attr(.value, "gradient") <- .grad
              }
              .value
          },
              function(mCall, data, LHS)
          {
              data <- data.frame(data)
              resp <- eval(LHS, data)
              input <- eval(mCall[["input"]], data)
              Dose <- eval(mCall[["Dose"]], data)
              n <- length(resp)
              if(length(input) != n) {
                  stop("must have length of response = length of second argument to 'SSfol'")
              }
              if(n < 4) {
                  stop("must have at least 4 observations to fit an 'SSfol' model")
              }
              rmaxind <- order(resp)[n]

              lresp <- log(resp)
              if(rmaxind == n) {
                  lKe <- -2.5
              } else {
                  lKe <- log((lresp[rmaxind] - lresp[n])/(input[n] - input[rmaxind]))
              }
              cond.lin <- nls(resp ~ (exp(-input * exp(lKe))-exp(-input * exp(lKa))) * Dose,
                              data = list(resp = resp, input = input, Dose = Dose, lKe = lKe),
                              start = list(lKa = lKe + 1),
                              algorithm = "plinear")
              pars <- coef(cond.lin)
              names(pars) <- NULL
              cond.lin <- nls(resp ~ (Dose * (exp(-input*exp(lKe))-
                                              exp(-input*exp(lKa))))/(exp(lKa) - exp(lKe)),
                              data = data.frame(list(resp = resp, input = input,
                              Dose = Dose)),
                              start = list(lKa = pars[1L],lKe = lKe),
                              algorithm = "plinear")
              pars <- coef(cond.lin)
              names(pars) <- NULL
              lKa <- pars[1L]
              lKe <- pars[2L]
              Ka <- exp(lKa)
              Ke <- exp(lKe)
              value <- list(lKe, lKa, log((Ke * Ka)/(pars[3L])))
              names(value) <- as.character(mCall)[4:6]
              value
          }, parameters = c("lKe", "lKa", "lCl"))

##*## SSfpl - four parameter logistic model

SSfpl <- # selfStart(~ A + (B - A)/(1 + exp((xmid - input)/scal)),
    selfStart(
              function(input, A, B, xmid, scal)
          {
              .expr1 <- B - A
              .expr2 <- xmid - input
              .expr4 <- exp((.expr2/scal))
              .expr5 <- 1 + .expr4
              .expr8 <- 1/.expr5
              .expr13 <- .expr5^2
              .value <- A + (.expr1/.expr5)
              .actualArgs <- as.list(match.call()[c("A", "B", "xmid", "scal")])
              if(all(unlist(lapply(.actualArgs, is.name))))
              {
                  .grad <- array(0, c(length(.value), 4L),
                                 list(NULL, c("A", "B", "xmid", "scal")))
                  .grad[, "A"] <- 1 - .expr8
                  .grad[, "B"] <- .expr8
                  .grad[, "xmid"] <-  - ((.expr1 * (.expr4 * (1/ scal)))/.expr13)
                  .grad[, "scal"] <- (.expr1 * (.expr4 * (.expr2/(scal^2))))/.expr13
                  dimnames(.grad) <- list(NULL, .actualArgs)
                  attr(.value, "gradient") <- .grad
              }
              .value
          },
              function(mCall, data, LHS)
          {
              xy <- sortedXyData(mCall[["input"]], LHS, data)
              if (nrow(xy) < 5) {
                  stop("too few distinct input values to fit a four-parameter logistic")
              }
              ## convert the response to a proportion (i.e. contained in (0,1))
              rng <- range(xy$y); drng <- diff(rng)
              xy$prop <- (xy$y - rng[1L] + 0.05 * drng)/(1.1 * drng)
              ## inverse regression of the x values on the proportion
              ir <- as.vector(coef(lm(x ~ I(log(prop/(1-prop))), data = xy)))
              pars <- as.vector(coef(nls(y ~ cbind(1, 1/(1 + exp((xmid - x)/
                                                                 exp(lscal)))),
                                         data = xy,
                                         start = list(xmid = ir[1L],
                                                      lscal = log(abs(ir[2L]))),
                                         algorithm = "plinear")))
              value <- c(pars[3L], pars[3L] + pars[4L], pars[1L], exp(pars[2L]))
              names(value) <- mCall[c("A", "B", "xmid", "scal")]
              value
          }, parameters = c("A", "B", "xmid", "scal"))

##*## SSlogis - logistic model for nonlinear regression

SSlogis <- # selfStart(~ Asym/(1 + exp((xmid - input)/scal)),
    selfStart(
              function(input, Asym, xmid, scal)
          {
              .expr1 <- xmid - input
              .expr3 <- exp((.expr1/scal))
              .expr4 <- 1 + .expr3
              .expr10 <- .expr4^2
              .value <- Asym/.expr4
              .actualArgs <- as.list(match.call()[c("Asym", "xmid", "scal")])
              if(all(unlist(lapply(.actualArgs, is.name))))
              {
                  .grad <- array(0, c(length(.value), 3L), list(NULL, c("Asym", "xmid", "scal")))
                  .grad[, "Asym"] <- 1/.expr4
                  .grad[, "xmid"] <-  - ((Asym * (.expr3 * (1/scal)))/.expr10)
                  .grad[, "scal"] <- (Asym * (.expr3 * (.expr1/(scal^2))))/.expr10
                  dimnames(.grad) <- list(NULL, .actualArgs)
                  attr(.value, "gradient") <- .grad
              }
              .value
          },
              function(mCall, data, LHS)
          {
              xy <- data.frame(sortedXyData(mCall[["input"]], LHS, data))
              if(nrow(xy) < 4) {
                  stop("too few distinct input values to fit a logistic model")
              }
              z <- xy[["y"]]
              if (min(z) <= 0) { z <- z - 1.05 * min(z) } # avoid zeroes
              z <- z/(1.05 * max(z))		# scale to within unit height
              xy[["z"]] <- log(z/(1 - z))		# logit transformation
              aux <- coef(lm(x ~ z, xy))
              pars <- as.vector(coef(nls(y ~ 1/(1 + exp((xmid - x)/scal)),
                                         data = xy,
                                         start = list(xmid = aux[1L], scal = aux[2L]),
                                         algorithm = "plinear")))
              value <- c(pars[3L], pars[1L], pars[2L])
              names(value) <- mCall[c("Asym", "xmid", "scal")]
              value
          }, parameters = c("Asym", "xmid", "scal"))

##*## SSmicmen - Michaelis-Menten model for enzyme kinetics.

SSmicmen <- # selfStart(~ Vm * input/(K + input),
    selfStart(
              function(input, Vm, K)
          {
              .expr1 <- Vm * input
              .expr2 <- K + input
              .value <- .expr1/.expr2
              .actualArgs <- as.list(match.call()[c("Vm", "K")])
              if(all(unlist(lapply(.actualArgs, is.name))))
              {
                  .grad <- array(0, c(length(.value), 2L), list(NULL, c("Vm", "K")))
                  .grad[, "Vm"] <- input/.expr2
                  .grad[, "K"] <-  - (.expr1/(.expr2^2))
                  dimnames(.grad) <- list(NULL, .actualArgs)
                  attr(.value, "gradient") <- .grad
              }
              .value
          },
              function(mCall, data, LHS)
          {
              xy <- data.frame(sortedXyData(mCall[["input"]], LHS, data))
              if (nrow(xy) < 3) {
                  stop("too few distinct input values to fit a Michaelis-Menten model")
              }
              ## take the inverse transformation
              pars <- as.vector(coef(lm(1/y ~ I(1/x), data = xy)))
              ## use the partially linear form to converge quickly
              pars <- as.vector(coef(nls(y ~ x/(K + x),
                                         data = xy,
                                         start = list(K = abs(pars[2L]/pars[1L])),
                                         algorithm = "plinear")))
              value <- c(pars[2L], pars[1L])
              names(value) <- mCall[c("Vm", "K")]
              value
          }, parameters = c("Vm", "K"))

SSgompertz <- #    selfStart( ~ Asym * exp(-b2*b3^x),
    ## Gompertz model for growth curve data
    selfStart(function(x, Asym, b2, b3)
          {
              .expr2 <- b3^x
              .expr4 <- exp(-b2 * .expr2)
              .value <- Asym * .expr4
              .actualArgs <- as.list(match.call()[c("Asym", "b2", "b3")])
              if(all(unlist(lapply(.actualArgs, is.name))))
              {
                  .grad <- array(0, c(length(.value), 3L),
                                 list(NULL, c("Asym", "b2", "b3")))
                  .grad[, "Asym"] <- .expr4
                  .grad[, "b2"] <- -Asym * (.expr4 * .expr2)
                  .grad[, "b3"] <- -Asym * (.expr4 * (b2 * (b3^(x - 1) * x)))
                  dimnames(.grad) <- list(NULL, .actualArgs)
                  attr(.value, "gradient") <- .grad
              }
              .value
          },
              function(mCall, data, LHS)
          {
              xy <- sortedXyData(mCall[["x"]], LHS, data)
              if (nrow(xy) < 4) {
                  stop("too few distinct input values to fit the Gompertz model")
              }
              xyL <- xy
              xyL$y <- log(abs(xyL$y))
              pars <- NLSstAsymptotic(xyL)
              pars <- coef(nls(y ~ exp(-b2*b3^x),
                               data = xy,
                               algorithm = "plinear",
                               start = c(b2 = pars[["b1"]],
                               b3 = exp(-exp(pars[["lrc"]])))))
              val <- pars[c(3,1,2)]
              names(val) <- mCall[c("Asym", "b2", "b3")]
              val
          },
              c("Asym", "b2", "b3"))

SSweibull <- # selfStart( ~ Asym - Drop * exp(-exp(lrc)*x^pwr),
    ## Weibull model for growth curve data
    selfStart( function(x, Asym, Drop, lrc, pwr)
          {
              .expr1 <- exp(lrc)
              .expr3 <- x^pwr
              .expr5 <- exp(-.expr1 * .expr3)
              .value <- Asym - Drop * .expr5
              .actualArgs <- as.list(match.call()[c("Asym", "Drop", "lrc", "pwr")])
              if(all(unlist(lapply(.actualArgs, is.name))))
              {
                  .grad <- array(0, c(length(.value), 4L),
                                 list(NULL, c("Asym", "Drop", "lrc", "pwr")))
                  .grad[, "Asym"] <- 1
                  .grad[, "Drop"] <- -.expr5
                  .grad[, "lrc"] <- Drop * (.expr5 * (.expr1 * .expr3))
                  .grad[, "pwr"] <- Drop * (.expr5 * (.expr1 * (.expr3 * log(x))))
                  dimnames(.grad) <- list(NULL, .actualArgs)
                  attr(.value, "gradient") <- .grad
              }
              .value
          },
              function(mCall, data, LHS)
          {
              xy <- sortedXyData(mCall[["x"]], LHS, data)
              if (nrow(xy) < 5) {
                  stop("too few distinct input values to fit the Weibull growth model")
              }
              if (any(xy[["x"]] < 0)) {
                  stop("all 'x' values must be non-negative to fit the Weibull growth model")
              }
              Rasym <- NLSstRtAsymptote(xy)
              Lasym <- NLSstLfAsymptote(xy)
              pars <- coef(lm(log(-log((Rasym - y)/(Rasym - Lasym))) ~ log(x),
                             data = xy, subset = x > 0))
              val <- coef(nls(y ~ cbind(1, -exp(-exp(lrc)*x^pwr)),
                               data = xy,
                               algorithm = "plinear",
                               start = c(lrc = pars[[1L]], pwr = pars[[2L]])))[
                                                         c(3,4,1,2)]
              names(val) <- mCall[c("Asym", "Drop", "lrc", "pwr")]
              val
          },
              c("Asym", "Drop", "lrc", "pwr"))


### Local variables:
### mode: S
### End:
#  File src/library/stats/R/zzz.R
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

.noGenerics <- TRUE

.onLoad <- function(libname, pkgname)
{
    op <- options()
    op.stats <-
        list(contrasts = c(unordered="contr.treatment",
             ordered="contr.poly"),
             na.action = "na.omit",
             show.coef.Pvalues = TRUE,
             show.signif.stars = TRUE,
             ts.eps = 1e-5,
             ts.S.compat = FALSE)
    toset <- !(names(op.stats) %in% names(op))
    if(any(toset)) options(op.stats[toset])
}

.onUnload <- function(libpath)
    library.dynam.unload("stats", libpath)
