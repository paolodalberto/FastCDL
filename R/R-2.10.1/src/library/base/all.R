#  File src/library/base/R/Bessel.R
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

besselI <- function(x, nu, expon.scaled = FALSE)
{
    ## Oddly, this is passed as a double to fit Math3 semantics
    .Internal(besselI(x,nu, 1 + as.logical(expon.scaled)))
}
besselK <- function(x, nu, expon.scaled = FALSE)
{
    .Internal(besselK(x,nu, 1 + as.logical(expon.scaled)))
}
besselJ <- function(x, nu) .Internal(besselJ(x,nu))
besselY <- function(x, nu) .Internal(besselY(x,nu))
#  File src/library/base/R/Defunct.R
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

.Defunct <- function(new, package=NULL, msg) {
    if (missing(msg)) {
	msg <- gettextf("'%s' is defunct.\n",
			as.character(sys.call(sys.parent())[[1L]]))
	if(!missing(new))
	    msg <- c(msg, gettextf("Use '%s' instead.\n", new))
	msg <- c(msg,
		 if(!is.null(package))
		 gettextf("See help(\"Defunct\") and help(\"%s-defunct\").",
			  package)
		 else gettext("See help(\"Defunct\")"))
    }
    else msg <- as.character(msg)

    stop(paste(msg, collapse=""), call. = FALSE, domain = NA)
}

Version <- function() .Defunct("R.Version")
provide <- function(package) .Defunct()

## <entry>
## Deprecated in 1.2.0
## Defunct in 1.3.0
getenv <- function(...) .Defunct("Sys.getenv")
## </entry>

## <entry>
## Deprecated in 1.2.3
## Defunct in 1.3.0
## Removed in 1.4.0: conflicts with lattice
## dotplot <- function(...) .Defunct()
## stripplot <- function(...) .Defunct()
## </entry>

## <entry>
## Deprecated in 1.3.0
## Defunct in 1.4.0
read.table.url <- function(url, method, ...) .Defunct("read.table(url())")
scan.url <- function(url, file = tempfile(), method, ...)
    .Defunct("scan(url())")
source.url <- function(url, file = tempfile(), method, ...)
    .Defunct("source(url())")
httpclient <- function(url, port=80, error.is.fatal=TRUE, check.MIME.type=TRUE,
                       file=tempfile(), drop.ctrl.z=TRUE)
    .Defunct()
parse.dcf <- function(text = NULL, file = "", fields = NULL,
                      versionfix = FALSE) .Defunct("read.dcf")
## </entry>

## <entry>
## Deprecated in 1.4.0
## Defunct in 1.5.0
.Alias <- function(expr) .Defunct()
## </entry>

## <entry>
## Deprecated in 1.6.0
## Defunct in 1.7.0
machine <- function() .Defunct()
Machine <- function() .Defunct(".Machine")
Platform <- function() .Defunct(".Platform")
restart <- function() .Defunct("try")
## </entry>

## <entry>
## Deprecated in 1.7.0
## Defunct in 1.8.0
printNoClass <- function(x, digits = NULL, quote = TRUE, na.print = NULL,
                         print.gap = NULL, right = FALSE, ...)
    .Defunct()
## </entry>

## <entry>
## Deprecated in 1.8.0
## Defunct in 1.9.0
codes <- function(x, ...) .Defunct()
codes.factor <- function(x, ...) .Defunct("unclass")
codes.ordered <- function(x, ...) .Defunct("unclass")
"codes<-" <- function(x, ..., value) .Defunct()
# removed in 2.9.1, as it caused confusion for an S4 class union of this name.
#print.atomic <- function(x, quote = TRUE, ...) .Defunct("print.default")
## </entry>

## <entry>
## Deprecated in 1.9.0
## Defunct in 2.0.0
La.eigen <- function(x, symmetric, only.values = FALSE,
                     method = c("dsyevr", "dsyev")) .Defunct("eigen")
tetragamma <- function(x) .Defunct("psigamma")
pentagamma <- function(x) .Defunct("psigamma")
package.description <- function(pkg, lib.loc = NULL, fields = NULL)
    .Defunct("packageDescription")
## </entry>

## <entry>
## Deprecated in 2.1.0
## Defunct in 2.2.0
delay <- function(x, env=.GlobalEnv) .Defunct("delayedAssign")
loadURL <- function (url, envir = parent.frame(), quiet = TRUE, ...)
    .Defunct("load(url())")
## </entry>

## Defunct in 2.3.0
write.table0 <-
function (x, file = "", append = FALSE, quote = TRUE, sep = " ",
          eol = "\n", na = "NA", dec = ".", row.names = TRUE,
          col.names = TRUE, qmethod = c("escape", "double"))
    .Defunct("write.table")
format.char <- function(x, width = NULL, flag = "-")
    .Defunct("format.default")
## </entry>

## <entry>
## Deprecated in 2.3.0
## Defunct in 2.4.0
La.chol <- function(x) .Defunct("chol")
La.chol2inv <- function(x, size = ncol(x)) .Defunct("chol2inv")
## </entry>

## <entry>
## Deprecated in 2.4.0
## Defunct in 2.5.0
symbol.C <- function(name)
{
    warning("'symbol.C' is not needed: please remove it", immediate.=TRUE)
    name
}
symbol.For <- function(name)
{
    warning("'symbol.For' is not needed: please remove it", immediate.=TRUE)
    name
}
## </entry>

## <entry>
## Deprecated in 1999
## Defunct in 2.5.0
unix <- function(call, intern = FALSE) .Defunct("system")
## </entry>


## <entry>
## Deprecated in 2.7.0
## Defunct in 2.8.0
gammaCody <- function(x) .Defunct("gamma")
## </entry>

## <entry>
## Deprecated inter alia in 2.8.1
## Defunct in 2.9.0
manglePackageName <- function (pkgName, pkgVersion) .Defunct()
## </entry>
#  File src/library/base/R/Deprecated.R
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

###----- NOTE:	../man/base-deprecated.Rd   must be synchronized with this file!
###		-------------------------
.Deprecated <- function(new, package = NULL, msg) {
    if( missing(msg) ) {
       msg <- gettextf("'%s' is deprecated.\n",
                       as.character(sys.call(sys.parent()))[1L] )
      if(!missing(new))
	msg <- c(msg, gettextf("Use '%s' instead.\n", new))
      msg <- c(msg,
	     if(!is.null(package))
	     gettextf("See help(\"Deprecated\") and help(\"%s-deprecated\").",
		      package)
	     else gettext("See help(\"Deprecated\")"))
    }
    else msg = as.character(msg)
    warning(paste(msg, collapse=""), call. = FALSE, domain = NA)
}

## consider keeping one (commented) entry here, for easier additions

## <entry>
## Deprecated in 2.5.0
Sys.putenv <- function(...) {
    .Deprecated("Sys.setenv")
    Sys.setenv(...)
}
## </entry>
#  File src/library/base/R/LAPACK.R
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

La.svd <- function(x, nu = min(n, p), nv = min(n, p))
{
    if(!is.numeric(x) && !is.complex(x))
	stop("argument to 'La.svd' must be numeric or complex")
    if (any(!is.finite(x))) stop("infinite or missing values in 'x'")
    x <- as.matrix(x)
    if (is.numeric(x)) storage.mode(x) <- "double"
    n <- nrow(x)
    p <- ncol(x)
    if(!n || !p) stop("0 extent dimensions")

    if(is.complex(x)) {
        if(nu == 0L) {
            jobu <- "N"
            u <- matrix(0+0i, 1L, 1L)  # dim is checked
        }
        else if(nu == n) {
            jobu <- ifelse(n > p, "A", "S")
            u <- matrix(0+0i, n, n)
        }
        else if(nu == p) {
            jobu <- ifelse(n > p, "S", "A")
            u <- matrix(0+0i, n, p)
        }
        else
            stop("'nu' must be 0, nrow(x) or ncol(x)")

        if (nv == 0L) {
            jobv <- "N"
            v <- matrix(0+0i, 1L, 1L) # dim is checked
        }
        else if (nv == n) {
            jobv <- ifelse(n > p, "A", "S")
            v <- matrix(0+0i, min(n, p), p)
        }
        else if (nv == p) {
            jobv <- ifelse(n > p, "S", "A")
            v <- matrix(0+0i, p, p)
        }
        else
            stop("'nv' must be 0, nrow(x) or ncol(x)")
        res <- .Call("La_svd_cmplx", jobu, jobv, x, double(min(n, p)),
                     u, v, PACKAGE = "base")
        return(res[c("d", if(nu) "u", if(nv) "vt")])
    } else {
        if(nu || nv) {
            np <- min(n, p)
            if(nu <= np && nv <= np) {
                jobu <- "S"
                u <- matrix(0, n, np)
                v <- matrix(0, np, p)
            } else {
                jobu <- "A"
                u <- matrix(0, n, n)
                v <- matrix(0, p, p)
            }
        } else {
            jobu <- "N"
            # these dimensions _are_ checked, but unused
            u <- matrix(0, 1L, 1L)
            v <- matrix(0, 1L, 1L)
        }
        jobv <- ""
        res <- .Call("La_svd", jobu, jobv, x, double(min(n,p)), u, v,
                     "dgsedd", PACKAGE = "base")
        res <- res[c("d", if(nu) "u", if(nv) "vt")]
        if(nu) res$u <- res$u[, 1L:min(n, nu), drop = FALSE]
        if(nv) res$vt <- res$vt[1L:min(p, nv), , drop = FALSE]
        return(res)
    }
    ## not reached
}
#  File src/library/base/R/New-Internal.R
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

geterrmessage <- function() .Internal(geterrmessage())

try <- function(expr, silent = FALSE) {
    tryCatch(expr, error = function(e) {
        call <- conditionCall(e)
        if (! is.null(call)) {
            ## Patch up the call to produce nicer result for testing as
            ## try(stop(...)).  This will need adjusting if the
            ## implementation of tryCatch changes.
            ## Use identical() since call[[1L]] can be non-atomic.
            if (identical(call[[1L]], quote(doTryCatch)))
                call <- sys.call(-4L)
            dcall <- deparse(call)[1L]
            prefix <- paste("Error in", dcall, ": ")
            LONG <- 75L # to match value in errors.c
            msg <- conditionMessage(e)
            sm <- strsplit(msg, "\n")[[1L]]
            w <- 14L + nchar(dcall, type="w") + nchar(sm[1L], type="w")
            ## this could be NA if any of this is invalid in a MBCS
            if(is.na(w))
                w <-  14L + nchar(dcall, type="b") + nchar(sm[1L], type="b")
            if (w > LONG)
                prefix <- paste(prefix, "\n  ", sep = "")
        }
        else prefix <- "Error : "
        msg <- paste(prefix, conditionMessage(e), "\n", sep="")
        ## Store the error message for legacy uses of try() with
        ## geterrmessage().
        .Internal(seterrmessage(msg[1L]))
        if (! silent && identical(getOption("show.error.messages"), TRUE)) {
            cat(msg, file = stderr())
            .Internal(printDeferredWarnings())
        }
        invisible(structure(msg, class = "try-error"))
    })
}

comment <- function(x).Internal(comment(x))
"comment<-" <- function(x,value).Internal("comment<-"(x, value))

logb <- function(x, base=exp(1)) if(missing(base)) log(x) else log(x, base)

atan2 <- function(y, x).Internal(atan2(y, x))

beta <- function(a, b).Internal( beta(a, b))
lbeta <- function(a, b).Internal(lbeta(a, b))

psigamma <- function(x, deriv = 0) .Internal(psigamma(x, deriv))

factorial <- function(x) gamma(x + 1)
lfactorial <- function(x) lgamma(x + 1)

choose <- function(n,k).Internal(choose(n,k))
lchoose <- function(n,k).Internal(lchoose(n,k))

##-- 2nd part --
R.Version <- function().Internal(Version())

commandArgs <- function(trailingOnly = FALSE) {
    args <- .Internal(commandArgs())
    if(trailingOnly) {
        m <- match("--args", args, 0L)
        if(m) args[-seq_len(m)] else character(0L)
    } else args
}

args <- function(name).Internal(args(name))

cbind <- function(..., deparse.level = 1)
    .Internal(cbind(deparse.level, ...))

rbind <- function(..., deparse.level = 1)
    .Internal(rbind(deparse.level, ...))

## for methods:::bind_activation
.__H__.cbind <- cbind
.__H__.rbind <- rbind


# convert deparsing options to bitmapped integer

.deparseOpts <- function(control) {
    opts <- pmatch(as.character(control),
                   ## the exact order of these is determined by the integer codes in
                   ## ../../../include/Defn.h
                   c("all",
                     "keepInteger", "quoteExpressions", "showAttributes",
                     "useSource", "warnIncomplete", "delayPromises",
                     "keepNA", "S_compatible"))
    if (any(is.na(opts)))
        stop(sprintf(ngettext(as.integer(sum(is.na(opts))),
                              "deparse option %s is not recognized",
                              "deparse options %s are not recognized"),
                     paste(sQuote(control[is.na(opts)]), collapse=", ")),
             call. = FALSE, domain = NA)
    if (any(opts == 1L))
        opts <- unique(c(opts[opts != 1L], 2L,3L,4L,5L,6L,8L)) # not (7,9)
    return(sum(2^(opts-2)))
}

deparse <-
    function(expr, width.cutoff = 60L,
	     backtick = mode(expr) %in% c("call", "expression", "(", "function"),
	     control = c("keepInteger", "showAttributes", "keepNA"),
             nlines = -1L)
    .Internal(deparse(expr, width.cutoff, backtick,
                      .deparseOpts(control), nlines))

do.call <- function(what, args, quote = FALSE, envir = parent.frame())
{
    if (!is.list(args))
	stop("second argument must be a list")
    if (quote) {
	enquote <- function(x) as.call(list(as.name("quote"), x))
	args <- lapply(args, enquote)
    }
    .Internal(do.call(what, args, envir))
}

drop <- function(x).Internal(drop(x))

format.info <- function(x, digits = NULL, nsmall = 0)
    .Internal(format.info(x, digits, nsmall))

gc <- function(verbose = getOption("verbose"),	reset=FALSE)
{
    res <- .Internal(gc(verbose, reset))
    res <- matrix(res, 2L, 7L,
		  dimnames = list(c("Ncells","Vcells"),
		  c("used", "(Mb)", "gc trigger", "(Mb)",
		    "limit (Mb)", "max used", "(Mb)")))
    if(all(is.na(res[, 5L]))) res[, -5L] else res
}
gcinfo <- function(verbose) .Internal(gcinfo(verbose))
gctorture <- function(on=TRUE) invisible(.Internal(gctorture(on)))

is.unsorted <- function(x, na.rm = FALSE, strictly = FALSE)
{
    if(is.null(x)) return(FALSE)
    if(!na.rm && any(is.na(x)))## "FIXME" is.na(<large>) is "too slow"
	return(NA)
    ## else
    if(na.rm && any(ii <- is.na(x)))
	x <- x[!ii]
    .Internal(is.unsorted(x, strictly))
}

mem.limits <- function(nsize=NA, vsize=NA)
    structure(.Internal(mem.limits(as.integer(nsize), as.integer(vsize))),
              names=c("nsize", "vsize"))

nchar <- function(x, type = "chars", allowNA = FALSE)
    .Internal(nchar(x, type, allowNA))

polyroot <- function(z).Internal(polyroot(z))

readline <- function(prompt="").Internal(readline(prompt))
search <- function().Internal(search())
searchpaths <- function()
{
    s <- search()
    paths <-
        lapply(seq_along(s), function(i) attr(as.environment(i), "path"))
    paths[[length(s)]] <- system.file()
    m <- grep("^package:", s)
    if(length(m)) paths[-m] <- as.list(s[-m])
    unlist(paths)
}

sprintf <- function(fmt, ...) .Internal(sprintf(fmt, ...))

##-- DANGER ! ---   substitute(list(...))  inside functions !!!
##substitute <- function(expr, env=baseenv()).Internal(substitute(expr, env))

t.default <- function(x).Internal(t.default(x))
typeof <- function(x).Internal(typeof(x))


memory.profile <- function() .Internal(memory.profile())

capabilities <- function(what = NULL)
{
    z  <- .Internal(capabilities())
    if(!is.null(what))
        z <- z[match(what, names(z), 0L)]
    if(.Platform$OS.type == "windows") return(z)
    ## Now we need to deal with any NA entries if X11 is unknown.
    nas <- names(z[is.na(z)])
    if(any(nas %in% c("X11", "jpeg", "png", "tiff"))) {
        ## This might throw an X11 error
         z[nas] <- tryCatch(.Internal(capabilitiesX11()),
                            error = function(e) FALSE)
    }
    z
}

inherits <- function(x, what, which = FALSE)
	.Internal(inherits(x, what, which))

NextMethod <- function(generic=NULL, object=NULL, ...)
    .Internal(NextMethod(generic, object,...))

data.class <- function(x) {
    if (length(cl <- oldClass(x)))
	cl[1L]
    else {
	l <- length(dim(x))
        if (l == 2L) "matrix" else if(l) "array" else mode(x)
    }
}

encodeString <- function(x, width = 0, quote = "", na.encode = TRUE,
                         justify = c("left", "right", "centre", "none"))
{
    at <- attributes(x)
    x <- as.character(x) # we want e.g. NULL to work
    attributes(x) <- at  # preserve names, dim etc
    oldClass(x) <- NULL  # but not class
    justify <- match(match.arg(justify),
                     c("left", "right", "centre", "none")) - 1L
    .Internal(encodeString(x, width, quote, justify, na.encode))
}

l10n_info <- function() .Internal(l10n_info())

iconv <- function(x, from = "", to = "", sub = NA, mark = TRUE)
{
    if(!is.character(x)) x <- as.character(x)
    .Internal(iconv(x, from, to, as.character(sub), mark))
}

iconvlist <- function()
{
    int <- .Internal(iconv(NULL, "", "", "", TRUE))
    if(length(int)) return(sort.int(int))
    icfile <- system.file("iconvlist", package="utils")
    if(!nchar(icfile, type="bytes"))
        stop("'iconvlist' is not available on this system")
    ext <- readLines(icfile)
    if(!length(ext)) stop("'iconvlist' is not available on this system")
    ## glibc has lines ending //, some versions with a header and some without.
    ## libiconv has lines with multiple entries separated by spaces
    cnt <- grep("//$", ext)
    if(length(cnt)/length(ext) > 0.5) {
        ext <- grep("//$", ext, value = TRUE)
        ext <- sub("//$", "", ext)
    }
    sort.int(unlist(strsplit(ext, "[[:space:]]")))
}

Cstack_info <- function() .Internal(Cstack_info())

reg.finalizer <- function(e, f, onexit = FALSE)
    .Internal(reg.finalizer(e, f, onexit))

Encoding <- function(x) .Internal(Encoding(x))
`Encoding<-` <- function(x, value) .Internal(setEncoding(x, value))

setTimeLimit <- function(cpu = Inf, elapsed = Inf, transient = FALSE)
    .Internal(setTimeLimit(cpu, elapsed, transient))
setSessionTimeLimit <- function(cpu = Inf, elapsed = Inf)
    .Internal(setSessionTimeLimit(cpu, elapsed))

icuSetCollate <- function(...) .Internal(icuSetCollate(...))

## base has no S4 generics
.noGenerics <- TRUE
#  File src/library/base/R/RNG.R
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

## Random Number Generator

## The available kinds are in
## ../../../include/Random.h  and ../../../main/RNG.c [RNG_Table]
##
RNGkind <- function(kind = NULL, normal.kind = NULL)
{
    kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
               "Mersenne-Twister", "Knuth-TAOCP", "user-supplied",
               "Knuth-TAOCP-2002", "default")
    n.kinds <- c("Buggy Kinderman-Ramage", "Ahrens-Dieter", "Box-Muller",
                 "user-supplied", "Inversion", "Kinderman-Ramage",
		 "default")
    do.set <- length(kind) > 0L
    if(do.set) {
	if(!is.character(kind) || length(kind) > 1L)
	    stop("'kind' must be a character string of length 1 (RNG to be used).")
	if(is.na(i.knd <- pmatch(kind, kinds) - 1L))
	    stop(gettextf("'%s' is not a valid abbreviation of an RNG", kind),
                 domain = NA)
        if(i.knd == length(kinds) - 1L) i.knd <- -1L
    } else i.knd <- NULL

    if(!is.null(normal.kind)) {
	if(!is.character(normal.kind) || length(normal.kind) != 1L)
	    stop("'normal.kind' must be a character string of length 1")
        normal.kind <- pmatch(normal.kind, n.kinds) - 1L
        if(is.na(normal.kind))
	    stop(gettextf("'%s' is not a valid choice", normal.kind),
                 domain = NA)
	if (normal.kind == 0L)
            warning("Buggy version of Kinderman-Ramage generator used")
         if(normal.kind == length(n.kinds) - 1L) normal.kind <- -1L
    }
    r <- 1L + .Internal(RNGkind(i.knd, normal.kind))
    r <- c(kinds[r[1L]], n.kinds[r[2L]])
    if(do.set || !is.null(normal.kind)) invisible(r) else r
}

set.seed <- function(seed, kind = NULL, normal.kind = NULL)
{
    kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
               "Mersenne-Twister", "Knuth-TAOCP", "user-supplied",
               "Knuth-TAOCP-2002", "default")
    n.kinds <- c("Buggy Kinderman-Ramage", "Ahrens-Dieter", "Box-Muller",
                 "user-supplied", "Inversion", "Kinderman-Ramage",
		 "default")
    if(length(kind) ) {
	if(!is.character(kind) || length(kind) > 1L)
	    stop("'kind' must be a character string of length 1 (RNG to be used).")
	if(is.na(i.knd <- pmatch(kind, kinds) - 1L))
	    stop(gettextf("'%s' is not a valid abbreviation of an RNG", kind),
                 domain = NA)
        if(i.knd == length(kinds) - 1L) i.knd <- -1L
    } else i.knd <- NULL

    if(!is.null(normal.kind)) {
	if(!is.character(normal.kind) || length(normal.kind) != 1L)
	    stop("'normal.kind' must be a character string of length 1")
        normal.kind <- pmatch(normal.kind, n.kinds) - 1L
        if(is.na(normal.kind))
	    stop(gettextf("'%s' is not a valid choice", normal.kind),
                 domain = NA)
	if (normal.kind == 0L)
            stop("Buggy version of Kinderman-Ramage generator is not allowed")
         if(normal.kind == length(n.kinds) - 1L) normal.kind <- -1L
    }
    invisible(.Internal(set.seed(seed, i.knd, normal.kind)))
}

# Compatibility function to set RNGkind as in a given R version

RNGversion <- function(vstr)
{
    vnum <- as.numeric(strsplit(vstr,".", fixed=TRUE)[[1L]])
    if (length(vnum) < 2L)
	stop("malformed version string")
    if (vnum[1L] == 0 && vnum[2L] < 99)
        RNGkind("Wichmann-Hill", "Buggy Kinderman-Ramage")
    else if (vnum[1L] == 0 || vnum[1L] == 1 && vnum[2L] <= 6)
	RNGkind("Marsaglia-Multicarry", "Buggy Kinderman-Ramage")
    else
	RNGkind("Mersenne-Twister", "Inversion")
}
#  File src/library/base/R/Scripts.R
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

.Script <-
function(interpreter, script, args, ...)
{
    if(.Platform$OS.type == "windows") {
        cmd <- paste(file.path(R.home("bin"), "Rcmd"),
                     file.path("..", "share", interpreter, script),
                     args)
        system(cmd, invisible = TRUE)
    }
    else
        system(paste(shQuote(file.path(R.home("bin"), "Rcmd")),
                     interpreter,
                     shQuote(file.path(R.home("share"),
                                       interpreter, script)),
                     args),
               ...)
}
#  File src/library/base/R/files.R
#  Part of the R package, http://www.R-project.org
#  Copyright (C) 2007 R Development Core Team
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

.TAOCP1997init <- function(seed)
{
    KK <- 100L; LL <- 37L; MM <- as.integer(2^30)
    KKK <- KK + KK - 1L; KKL <- KK - LL
    ss <- seed - (seed %% 2L) + 2L
    X <- integer(KKK)
    for(j in 1L:KK) {
        X[j] <- ss
        ss <- ss+ss
        if(ss >= MM) ss <- ss - MM + 2L
    }
    X[2L] <- X[2L] + 1L
    ss <- seed
    T <- 69L
    while(T > 0) {
        for(j in KK:2L) X[j + j - 1L] <- X[j]
        for(j in seq(KKK, KKL + 1L, -2L))
            X[KKK - j + 2L] <- X[j] - (X[j] %% 2L)
        for(j in KKK:(KK+1L))
            if(X[j] %% 2L == 1L) {
                X[j - KKL] <- (X[j - KKL] - X[j]) %% MM
                X[j - KK] <- (X[j - KK] - X[j]) %% MM
            }
        if(ss %% 2L == 1L) {
            for(j in KK:1L) X[j + 1L] <- X[j]
            X[1L] <- X[KK + 1L]
            if(X[KK + 1L] %% 2L == 1L)
                X[LL + 1L] <- (X[LL + 1L] - X[KK + 1L]) %% MM
        }
        if(ss) ss <- ss %/% 2L else T <- T - 1L
    }
    rs <- c(X[(LL+1L):KK], X[1L:LL])
    invisible(rs)
}
#  File src/library/base/R/all.equal.R
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

all.equal <- function(target, current, ...) UseMethod("all.equal")

all.equal.default <-
    function(target, current, ...)
{
    ## Really a dispatcher given mode() of args :
    ## use data.class as unlike class it does not give "Integer"
    if(is.language(target) || is.function(target) || is.environment(target))
	return(all.equal.language(target, current, ...))
    if(is.recursive(target))
	return(all.equal.list(target, current, ...))
    msg <- switch (mode(target),
                   integer = ,
                   complex = ,
                   numeric = all.equal.numeric(target, current, ...),
                   character = all.equal.character(target, current, ...),
                   logical = ,
                   raw = all.equal.raw(target, current, ...),
		   ## assumes that slots are implemented as attributes :
		   S4 = attr.all.equal(target, current, ...),
                   if(data.class(target) != data.class(current)) {
                       paste("target is ", data.class(target), ", current is ",
                             data.class(current), sep = "")
                   } else NULL)
    if(is.null(msg)) TRUE else msg
}

all.equal.numeric <-
    function(target, current, tolerance = .Machine$double.eps ^ .5,
             scale = NULL, check.attributes = TRUE, ...)
{
    msg <- if(check.attributes) attr.all.equal(target, current, ...)
    if(data.class(target) != data.class(current)) {
	msg <- c(msg, paste("target is ", data.class(target), ", current is ",
			    data.class(current), sep = ""))
	return(msg)
    }

    lt <- length(target)
    lc <- length(current)
    cplx <- is.complex(target)
    if(lt != lc) {
	## *replace* the 'Lengths' msg[] from attr.all.equal():
	if(!is.null(msg)) msg <- msg[- grep("\\bLengths\\b", msg)]
	msg <- c(msg, paste(if(cplx)"Complex" else "Numeric",
			    ": lengths (", lt, ", ", lc, ") differ", sep = ""))
	return(msg)
    }
    target <- as.vector(target)
    current <- as.vector(current)
    out <- is.na(target)
    if(any(out != is.na(current))) {
	msg <- c(msg, paste("'is.NA' value mismatch:", sum(is.na(current)),
			    "in current", sum(out), "in target"))
	return(msg)
    }
    out <- out | target == current
    if(all(out)) { if (is.null(msg)) return(TRUE) else return(msg) }

    target <- target[!out]
    current <- current[!out]
    if(is.integer(target) && is.integer(current)) target <- as.double(target)
    xy <- mean((if(cplx)Mod else abs)(target - current))
    what <-
	if(is.null(scale)) {
	    xn <- mean(abs(target))
	    if(is.finite(xn) && xn > tolerance) {
		xy <- xy/xn
		"relative"
	    } else "absolute"
	} else {
	    xy <- xy/scale
	    "scaled"
	}

    if (cplx) what <- paste(what, "Mod") # PR#10575
    if(is.na(xy) || xy > tolerance)
        msg <- c(msg, paste("Mean", what, "difference:", format(xy)))

    if(is.null(msg)) TRUE else msg
}

all.equal.character <-
    function(target, current, check.attributes = TRUE, ...)
{
    msg <-  if(check.attributes) attr.all.equal(target, current, ...)
    if(data.class(target) != data.class(current)) {
	msg <- c(msg, paste("target is ", data.class(target), ", current is ",
			    data.class(current), sep = ""))
	return(msg)
    }
    lt <- length(target)
    lc <- length(current)
    if(lt != lc) {
	if(!is.null(msg)) msg <- msg[- grep("\\bLengths\\b", msg)]
	msg <- c(msg, paste("Lengths (", lt, ", ", lc,
		     ") differ (string compare on first ", ll <- min(lt, lc),
		     ")", sep = ""))
	ll <- seq_len(ll)
	target <- target[ll]
	current <- current[ll]
    }
    nas <- is.na(target); nasc <- is.na(current)
    if (any(nas != nasc)) {
	msg <- c(msg, paste("'is.NA' value mismatch:", sum(nasc),
                            "in current", sum(nas), "in target"))
	return(msg)
    }
    ne <- !nas & (target != current)
    if(!any(ne) && is.null(msg)) TRUE
    else if(sum(ne) == 1L) c(msg, paste("1 string mismatch"))
    else if(sum(ne) > 1L) c(msg, paste(sum(ne), "string mismatches"))
    else msg
}

all.equal.factor <- function(target, current, check.attributes = TRUE, ...)
{
    if(!inherits(current, "factor"))
	return("'current' is not a factor")
    msg <-  if(check.attributes) attr.all.equal(target, current, ...)
    class(target) <- class(current) <- NULL
    nax <- is.na(target)
    nay <- is.na(current)
    n <- sum(nax != nay)
    if(n > 1L)
	msg <- c(msg, paste(n, "NA mismatches"))
    else if (n == 1L)
        msg <- c(msg, paste("1, NA mismatch"))
    else {
	target <- levels(target)[target[!nax]]
	current <- levels(current)[current[!nay]]
        n <- all.equal(target, current, check.attributes = check.attributes,
                       ...)
	if(is.character(n)) msg <- c(msg, n)
    }
    if(is.null(msg)) TRUE else msg
}

all.equal.formula <- function(target, current, ...)
{
    if(length(target) != length(current))
	return(paste("target, current differ in having response: ",
		     length(target) == 3L, ", ",
                     length(current) == 3L, sep=""))
    if(all(deparse(target) != deparse(current)))
	"formulas differ in contents"
    else TRUE
}

all.equal.language <- function(target, current, ...)
{
    mt <- mode(target)
    mc <- mode(current)
    if(mt == "expression" && mc == "expression")
	return(all.equal.list(target, current, ...))
    ttxt <- paste(deparse(target), collapse = "\n")
    ctxt <- paste(deparse(current), collapse = "\n")
    msg <- c(if(mt != mc)
	     paste("Modes of target, current: ", mt, ", ", mc, sep = ""),
	     if(ttxt != ctxt) {
		 if(pmatch(ttxt, ctxt, 0L))
		     "target is a subset of current"
		 else if(pmatch(ctxt, ttxt, 0L))
		     "current is a subset of target"
		 else "target, current do not match when deparsed"
	     })
    if(is.null(msg)) TRUE else msg
}

all.equal.list <- function(target, current, check.attributes = TRUE, ...)
{
    msg <- if(check.attributes) attr.all.equal(target, current, ...)
##    nt <- names(target)
##    nc <- names(current)
    iseq <-
	## <FIXME>
	## Commenting this eliminates PR#674, and assumes that lists are
	## regarded as generic vectors, so that they are equal iff they
	## have identical names attributes and all components are equal.
	## if(length(nt) && length(nc)) {
	##     if(any(not.in <- (c.in.t <- match(nc, nt, 0L)) == 0L))
	##	msg <- c(msg, paste("Components not in target:",
	##			    paste(nc[not.in], collapse = ", ")))
	##     if(any(not.in <- match(nt, nc, 0L) == 0L))
	##	msg <- c(msg, paste("Components not in current:",
	##			    paste(nt[not.in], collapse = ", ")))
	##     nt[c.in.t]
	## } else
	## </FIXME>
	if(length(target) == length(current)) {
	    seq_along(target)
	} else {
            if(!is.null(msg)) msg <- msg[- grep("\\bLengths\\b", msg)]
	    nc <- min(length(target), length(current))
	    msg <- c(msg, paste("Length mismatch: comparison on first",
				nc, "components"))
	    seq_len(nc)
	}
    for(i in iseq) {
	mi <- all.equal(target[[i]], current[[i]], check.attributes = check.attributes, ...)
	if(is.character(mi))
	    msg <- c(msg, paste("Component ", i, ": ", mi, sep=""))
    }
    if(is.null(msg)) TRUE else msg
}

all.equal.raw <-
    function(target, current, check.attributes = TRUE, ...)
{
    msg <-  if(check.attributes) attr.all.equal(target, current, ...)
    if(data.class(target) != data.class(current)) {
	msg <- c(msg, paste("target is ", data.class(target), ", current is ",
			    data.class(current), sep = ""))
	return(msg)
    }
    lt <- length(target)
    lc <- length(current)
    if(lt != lc) {
	if(!is.null(msg)) msg <- msg[- grep("\\bLengths\\b", msg)]
	msg <- c(msg, paste("Lengths (", lt, ", ", lc,
		     ") differ (comparison on first ", ll <- min(lt, lc),
		     " components)", sep = ""))
	ll <- seq_len(ll)
	target <- target[ll]
	current <- current[ll]
    }
    # raws do not have NAs, but logicals do
    nas <- is.na(target); nasc <- is.na(current)
    if (any(nas != nasc)) {
	msg <- c(msg, paste("'is.NA' value mismatch:", sum(nasc),
                            "in current", sum(nas), "in target"))
	return(msg)
    }
    ne <- !nas & (target != current)
    if(!any(ne) && is.null(msg)) TRUE
    else if(sum(ne) == 1L) c(msg, paste("1 element mismatch"))
    else if(sum(ne) > 1L) c(msg, paste(sum(ne), "element mismatches"))
    else msg
}


attr.all.equal <- function(target, current,
                           check.attributes = TRUE, check.names = TRUE, ...)
{
    ##--- "all.equal(.)" for attributes ---
    ##---  Auxiliary in all.equal(.) methods --- return NULL or character()
    msg <- NULL
    if(mode(target) != mode(current))
	msg <- paste("Modes: ", mode(target), ", ", mode(current), sep = "")
    if(length(target) != length(current))
	msg <- c(msg, paste("Lengths: ", length(target), ", ",
			    length(current), sep = ""))
    ax <- attributes(target)
    ay <- attributes(current)
    if(check.names) {
        nx <- names(target)
        ny <- names(current)
        if((lx <- length(nx)) | (ly <- length(ny))) {
            ## names() treated now; hence NOT with attributes()
            ax$names <- ay$names <- NULL
            if(lx && ly) {
                if(is.character(m <- all.equal.character(nx, ny, check.attributes = check.attributes)))
                    msg <- c(msg, paste("Names:", m))
            } else if(lx)
                msg <- c(msg, "names for target but not for current")
            else msg <- c(msg, "names for current but not for target")
        }
    }
    if(check.attributes && (length(ax) || length(ay))) {# some (more) attributes
	## order by names before comparison:
	nx <- names(ax)
	ny <- names(ay)
	if(length(nx)) ax <- ax[order(nx)]
	if(length(ny)) ay <- ay[order(ny)]
	tt <- all.equal(ax, ay, check.attributes = check.attributes, ...)
	if(is.character(tt)) msg <- c(msg, paste("Attributes: <", tt, ">"))
    }
    msg # NULL or character
}
#  File src/library/base/R/allnames.R
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

all.names <- function(expr, functions = TRUE, max.names = -1L, unique = FALSE)
    .Internal(all.names(expr, functions, max.names, unique))

all.vars <- function(expr, functions = FALSE, max.names = -1L, unique = TRUE)
    .Internal(all.names(expr, functions, max.names, unique))
#  File src/library/base/R/aperm.R
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

aperm <- function(a, perm, resize=TRUE)
{
    if (missing(perm))
	perm <- integer(0L) # will reverse the order
    .Internal(aperm(a, perm, resize))
}
#  File src/library/base/R/append.R
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

append <- function (x, values, after = length(x))
{
    lengx <- length(x)
    if (!after) c(values, x)
    else if (after >= lengx) c(x, values)
    else c(x[1L:after], values, x[(after + 1L):lengx])
}
#  File src/library/base/R/apply.R
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

apply <- function(X, MARGIN, FUN, ...)
{
    FUN <- match.fun(FUN)

    ## Ensure that X is an array object
    d <- dim(X)
    dl <- length(d)
    if(dl == 0L)
	stop("dim(X) must have a positive length")
    ds <- 1L:dl
    if(length(oldClass(X)))
	X <- if(dl == 2) as.matrix(X) else as.array(X)
    ## now recompute things as coercion can change dims
    ## (e.g. when a data frame contains a matrix).
    d <- dim(X)
    dn <- dimnames(X)

    ## Extract the margins and associated dimnames

    s.call <- ds[-MARGIN]
    s.ans  <- ds[MARGIN]
    d.call <- d[-MARGIN]
    d.ans  <- d[MARGIN]
    dn.call<- dn[-MARGIN]
    dn.ans <- dn[MARGIN]
    ## dimnames(X) <- NULL

    ## do the calls

    d2 <- prod(d.ans)
    if(d2 == 0L) {
        ## arrays with some 0 extents: return ``empty result'' trying
        ## to use proper mode and dimension:
        ## The following is still a bit `hackish': use non-empty X
        newX <- array(vector(typeof(X), 1L), dim = c(prod(d.call), 1L))
        ans <- FUN(if(length(d.call) < 2L) newX[,1] else
                   array(newX[, 1L], d.call, dn.call), ...)
        return(if(is.null(ans)) ans else if(length(d.ans) < 2L) ans[1L][-1L]
               else array(ans, d.ans, dn.ans))
    }
    ## else
    newX <- aperm(X, c(s.call, s.ans))
    dim(newX) <- c(prod(d.call), d2)
    ans <- vector("list", d2)
    if(length(d.call) < 2L) {# vector
        if (length(dn.call)) dimnames(newX) <- c(dn.call, list(NULL))
        for(i in 1L:d2) {
            tmp <- FUN(newX[,i], ...)
            if(!is.null(tmp)) ans[[i]] <- tmp
        }
    } else
       for(i in 1L:d2) {
           tmp <- FUN(array(newX[,i], d.call, dn.call), ...)
           if(!is.null(tmp)) ans[[i]] <- tmp
        }

    ## answer dims and dimnames

    ans.list <- is.recursive(ans[[1L]])
    l.ans <- length(ans[[1L]])

    ans.names <- names(ans[[1L]])
    if(!ans.list)
	ans.list <- any(unlist(lapply(ans, length)) != l.ans)
    if(!ans.list && length(ans.names)) {
        all.same <- sapply(ans, function(x) identical(names(x), ans.names))
        if (!all(all.same)) ans.names <- NULL
    }
    len.a <- if(ans.list) d2 else length(ans <- unlist(ans, recursive = FALSE))
    if(length(MARGIN) == 1L && len.a == d2) {
	names(ans) <- if(length(dn.ans[[1L]])) dn.ans[[1L]] # else NULL
	return(ans)
    }
    if(len.a == d2)
	return(array(ans, d.ans, dn.ans))
    if(len.a && len.a %% d2 == 0L) {
        if(is.null(dn.ans)) dn.ans <- vector(mode="list", length(d.ans))
        dn.ans <- c(list(ans.names), dn.ans)
	return(array(ans, c(len.a %/% d2, d.ans),
                     if(!all(sapply(dn.ans, is.null))) dn.ans))
    }
    return(ans)
}
#  File src/library/base/R/array.R
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

array <-
function(data = NA, dim = length(data), dimnames = NULL)
{
    data <- as.vector(data)
    dim <- as.integer(dim)
    vl <- prod(dim)
    if(length(data) != vl) {
        if(vl > .Machine$integer.max)
            stop("'dim' specifies too large an array")
        data <- rep(data, length.out=vl)
    }
    if(length(dim))
	dim(data) <- dim
    if(is.list(dimnames) && length(dimnames))
	dimnames(data) <- dimnames
    data
}

slice.index <-
function(x, MARGIN)
{
    d <- dim(x)
    if(is.null(d))
        d <- length(x)
    n <- length(d)

    if((length(MARGIN) > 1L) || (MARGIN < 1L) || (MARGIN > n))
        stop("incorrect value for 'MARGIN'")

    if(any(d == 0L)) return(array(integer(0L), d))

    y <- rep.int(rep.int(1L:d[MARGIN],
			 prod(d[seq_len(MARGIN - 1L)]) * rep.int(1L, d[MARGIN])),
		 prod(d[seq.int(from = MARGIN + 1L, length.out = n - MARGIN)]))
    dim(y) <- d
    y
}
#  File src/library/base/R/as.R
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

as.single <- function(x,...) UseMethod("as.single")
as.single.default <- function(x,...)
    structure(.Internal(as.vector(x,"double")), Csingle=TRUE)

# as.character is now internal.  The default method remains here to
# preserve the semantics that for a call with an object argument
# dispatching is done first on as.character and then on as.vector.
as.character.default <- function(x,...) .Internal(as.vector(x, "character"))

as.expression <- function(x,...) UseMethod("as.expression")
as.expression.default <- function(x,...) .Internal(as.vector(x, "expression"))

as.list <- function(x,...) UseMethod("as.list")
as.list.default <- function (x, ...)
    if (typeof(x) == "list") x else .Internal(as.vector(x, "list"))

as.list.function <- function (x, ...) c(formals(x), list(body(x)))

## FIXME:  Really the above  as.vector(x, "list")  should work for data.frames!
as.list.data.frame <- function(x,...) {
    x <- unclass(x)
    attr(x,"row.names") <- NULL
    x
}

as.list.environment <- function(x, all.names=FALSE, ...)
    .Internal(env2list(x, all.names))

##as.vector dispatches internally so no need for a generic
as.vector <- function(x, mode = "any") .Internal(as.vector(x, mode))

as.matrix <- function(x, ...) UseMethod("as.matrix")
as.matrix.default <- function(x, ...) {
    if (is.matrix(x)) x
    else
	array(x, c(length(x), 1L),
	      if(!is.null(names(x))) list(names(x), NULL) else NULL)
}
as.null <- function(x,...) UseMethod("as.null")
as.null.default <- function(x,...) NULL

as.function <- function(x,...) UseMethod("as.function")
as.function.default <- function (x, envir = parent.frame(), ...)
    if (is.function(x)) x else .Internal(as.function.default(x, envir))

as.array <- function(x, ...) UseMethod("as.array")
as.array.default <- function(x, ...)
{
    if(is.array(x)) return(x)
    n <- names(x)
    dim(x) <- length(x)
    if(length(n)) dimnames(x) <- list(n)
    return(x)
}

as.symbol <- function(x) .Internal(as.vector(x, "symbol"))
as.name <- as.symbol
## would work too: as.name <- function(x) .Internal(as.vector(x, "name"))

## as.call <- function(x) stop("type call cannot be assigned")
as.qr <- function(x) stop("you cannot be serious")
#  File src/library/base/R/assign.R
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

assign <-
    function (x, value, pos = -1, envir = as.environment(pos),
              inherits = FALSE, immediate = TRUE)
    .Internal(assign(x, value, envir, inherits))
#  File src/library/base/R/attach.R
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

attach <- function(what, pos = 2, name = deparse(substitute(what)),
                   warn.conflicts = TRUE)
{
    checkConflicts <- function(env)
    {
        dont.mind <- c("last.dump", "last.warning", ".Last.value",
                       ".Random.seed", ".First.lib", ".Last.lib",
                       ".packageName", ".noGenerics", ".required",
                       ".no_S3_generics")
        sp <- search()
        for (i in seq_along(sp)) {
            if (identical(env, as.environment(i))) {
                db.pos <- i
                break
            }
        }
        ob <- objects(db.pos, all.names = TRUE)
        if(.isMethodsDispatchOn()) {
            ## <FIXME>: this is wrong-headed: see library().
            these <- objects(db.pos, all.names = TRUE)
            these <- these[substr(these, 1L, 6L) == ".__M__"]
            gen <- gsub(".__M__(.*):([^:]+)", "\\1", these)
            from <- gsub(".__M__(.*):([^:]+)", "\\2", these)
            gen <- gen[from != ".GlobalEnv"]
            ob <- ob[!(ob %in% gen)]
        }
        ipos <- seq_along(sp)[-c(db.pos, match(c("Autoloads", "CheckExEnv"), sp, 0L))]
        for (i in ipos) {
            obj.same <- match(objects(i, all.names = TRUE), ob, nomatch = 0L)
            if (any(obj.same > 0)) {
                same <- ob[obj.same]
                same <- same[!(same %in% dont.mind)]
                Classobjs <- grep("^\\.__", same)
                if(length(Classobjs)) same <- same[-Classobjs]
                ## report only objects which are both functions or
                ## both non-functions.
                is_fn1 <- sapply(same, function(x)
                                 exists(x, where = i, mode = "function",
                                        inherits = FALSE))
                is_fn2 <- sapply(same, function(x)
                                 exists(x, where = db.pos, mode = "function",
                                        inherits = FALSE))
                same <- same[is_fn1 == is_fn2]
                if(length(same)) {
                    cat("\n\tThe following object(s) are masked",
                        if (i < db.pos) "_by_" else "from", sp[i],
                        if (sum(sp == sp[i]) > 1L) paste("( position", i, ")"),
                        ":\n\n\t", same, "\n\n")
                }
            }
        }
    }

    if(pos == 1) {
        warning("*** 'pos=1' is not possible; setting 'pos=2' for now.\n",
                "*** Note that 'pos=1' will give an error in the future")
        pos <- 2
    }
    if (is.character(what) && (length(what) == 1L)){
        if (!file.exists(what))
            stop(gettextf("file '%s' not found", what), domain = NA)
        name <- paste("file:", what, sep="")
        value <- .Internal(attach(NULL, pos, name))
        load(what, envir=as.environment(pos))
    }
    else
        value <- .Internal(attach(what, pos, name))
    if(warn.conflicts &&
       !exists(".conflicts.OK", envir = value, inherits = FALSE)) {
        checkConflicts(value)
    }
    if( length(objects(envir = value, all.names = TRUE) )
       && .isMethodsDispatchOn())
      methods:::cacheMetaData(value, TRUE)
    invisible(value)
}

detach <- function(name, pos=2, unload=FALSE, character.only = FALSE)
{
    if(!missing(name)) {
	if(!character.only)
	    name <- substitute(name)# when a name..
	pos <-
	    if(is.numeric(name))
                name
	    else {
                if (!is.character(name))
                    name <- deparse(name)
                match(name, search())
            }
	if(is.na(pos))
	    stop("invalid name")
    }
    env <- as.environment(pos)
    packageName <- search()[[pos]]

    ## we need treat packages differently from other objects.
    isPkg <- grepl("^package:", packageName)
    if (!isPkg) {
        .Internal(detach(pos))
        return(invisible())
    }

    pkgname <- sub("^package:", "", packageName)
    libpath <- attr(env, "path")
    hook <- getHook(packageEvent(pkgname, "detach")) # might be list()
    for(fun in rev(hook)) try(fun(pkgname, libpath))
    if(exists(".Last.lib", mode = "function", where = pos, inherits=FALSE)) {
        .Last.lib <- get(".Last.lib",  mode = "function", pos = pos,
                         inherits=FALSE)
        if(!is.null(libpath)) try(.Last.lib(libpath))
    }
    .Internal(detach(pos))

    ## Check for detaching a package require()d by another package (not
    ## by .GlobalEnv because detach() can't currently fix up the
    ## .required there)
    for(pkg in search()[-1L]) {
        ## How can a namespace environment get on the search list?
        ## (base fails isNamespace).
        if(!isNamespace(as.environment(pkg)) &&
           exists(".required", pkg, inherits = FALSE) &&
           pkgname %in% get(".required", pkg, inherits = FALSE))
            warning(packageName, " is required by ", pkg, " (still attached)")
    }

    if(pkgname %in% loadedNamespaces()) {
        ## the lazyload DB is flushed when the name space is unloaded
        if(unload) {
            tryCatch(unloadNamespace(pkgname),
                     error = function(e)
                     warning(pkgname, " namespace cannot be unloaded\n",
                             conditionMessage(e), call. = FALSE))
        }
    } else
        .Call("R_lazyLoadDBflush",
              paste(libpath, "/R/", pkgname, ".rdb", sep=""),
              PACKAGE="base")
    invisible()
}

ls <- objects <-
    function (name, pos = -1, envir = as.environment(pos), all.names = FALSE,
              pattern)
{
    if (!missing(name)) {
        nameValue <- try(name)
        if(identical(class(nameValue), "try-error")) {
            name <- substitute(name)
            if (!is.character(name))
                name <- deparse(name)
            pos <- name
        }
        else
            pos <- nameValue
    }
    all.names <- .Internal(ls(envir, all.names))
    if (!missing(pattern)) {
        if ((ll <- length(grep("[", pattern, fixed=TRUE))) &&
            ll != length(grep("]", pattern, fixed=TRUE))) {
            if (pattern == "[") {
                pattern <- "\\["
                warning("replaced regular expression pattern '[' by  '\\\\['")
            }
            else if (length(grep("[^\\\\]\\[<-", pattern))) {
                pattern <- sub("\\[<-", "\\\\\\[<-", pattern)
                warning("replaced '[<-' by '\\\\[<-' in regular expression pattern")
            }
        }
        grep(pattern, all.names, value = TRUE)
    }
    else all.names
}
#  File src/library/base/R/attr.R
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

"mostattributes<-" <- function(obj, value) {
    if(length(value)) {
	if(!is.list(value)) stop("RHS must be list")
	if(h.nam <- !is.na(inam <- match("names", names(value)))) {
	    n1 <- value[[inam]];	value <- value[-inam] }
	if(h.dim <- !is.na(idin <- match("dim", names(value)))) {
	    d1 <- value[[idin]];	value <- value[-idin] }
	if(h.dmn <- !is.na(idmn <- match("dimnames", names(value)))) {
	    dn1 <- value[[idmn]];	value <- value[-idmn] }
	attributes(obj) <- value
        dm <- dim(obj)
	if(h.nam && is.null(dm) && length(obj) == length(n1))
	    names(obj) <- n1
	if(h.dim && length(obj) == prod(d1))
	    dim(obj) <- dm <- d1
	if(h.dmn && !is.null(dm)) {
            ddn <- sapply(dn1, length)
            if( all((dm == ddn)[ddn > 0]) ) dimnames(obj) <- dn1
        }
    }
    obj
}
#  File src/library/base/R/autoload.R
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

autoload <- function(name, package, reset=FALSE, ...)
{
    if (!reset && exists(name, envir = .GlobalEnv, inherits = FALSE))
	stop("an object with that name already exists")
    m <- match.call()
    m[[1L]] <- as.name("list")
    newcall <- eval(m, parent.frame())
    newcall <- as.call(c(as.name("autoloader"), newcall))
    newcall$reset <- NULL
    if (is.na(match(package, .Autoloaded)))
	assign(".Autoloaded", c(package, .Autoloaded), envir =.AutoloadEnv)
    do.call("delayedAssign", list(name, newcall, .GlobalEnv, .AutoloadEnv))
    ## no longer return the result, which is a promise
    invisible()
}

autoloader <- function (name, package, ...)
{
    name <- paste(name, "", sep = "")
    rm(list = name, envir = .AutoloadEnv, inherits = FALSE)
    m <- match.call()
    m$name <- NULL
    m[[1L]] <- as.name("library")
    ## load the package
    eval(m, .GlobalEnv)
    ## reset the autoloader
    autoload(name, package, reset = TRUE, ...)
    ## reevaluate the object
    where <- match(paste("package", package, sep = ":"), search())
    if (exists(name, where = where, inherits = FALSE))
	eval(as.name(name), as.environment(where))
    else
	stop(gettextf("autoloader did not find '%s' in '%s'", name, package),
             domain = NA)
}
#  File src/library/base/R/backquote.R
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



bquote<-function(expr, where=parent.frame())
{
    unquote<-function(e)
        if (length(e) <= 1L) e
        else if (e[[1L]] == as.name(".")) eval(e[[2L]], where)
        else as.call(lapply(e,unquote))

    unquote(substitute(expr))
}

#  File src/library/base/R/backsolve.R
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

forwardsolve <- function(l, x, k=ncol(l), upper.tri = FALSE, transpose = FALSE)
    backsolve(l,x, k=k, upper.tri= upper.tri, transpose= transpose)

backsolve <- function(r, x, k=ncol(r), upper.tri = TRUE, transpose = FALSE)
{
    r <- as.matrix(r) # nr  x  k
    storage.mode(r) <- "double"
    x.mat <- is.matrix(x)
    if(!x.mat) x <- as.matrix(x) # k  x	nb
    storage.mode(x) <- "double"
    k <- as.integer(k)
    if(k <= 0 || nrow(x) < k) stop("invalid argument values in 'backsolve'")
    nb <- ncol(x)
    upper.tri <- as.logical(upper.tri)
    transpose <- as.logical(transpose)
    job <- as.integer(upper.tri + 10L*transpose)
    z <- .C("bakslv",
	    t  = r, ldt= nrow(r), n  = k,
	    b  = x, ldb= k,	  nb = nb,
	    x  = matrix(0, k, nb),
	    job = job,
	    info = integer(1L),
	    DUP = FALSE, PACKAGE = "base")[c("x","info")]
    if(z$info)
	stop(gettextf("singular matrix in 'backsolve'. First zero in diagonal [%d]", z$info), domain = NA)
    if(x.mat) z$x else drop(z$x)
}
#  File src/library/base/R/bindenv.R
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

lockEnvironment <- function(env, bindings = FALSE)
    .Internal(lockEnvironment(env, bindings))

environmentIsLocked <- function(env)
    .Internal(environmentIsLocked(env))

lockBinding <- function(sym, env) {
    if (is.character(sym)) sym <- as.name(sym)
    .Internal(lockBinding(sym, env))
}

bindingIsLocked <- function(sym, env) {
    if (is.character(sym)) sym <- as.name(sym)
    .Internal(bindingIsLocked(sym, env))
}

makeActiveBinding <- function(sym, fun, env) {
    if (is.character(sym)) sym <- as.name(sym)
    .Internal(makeActiveBinding(sym, fun, env))
}

bindingIsActive <- function(sym, env) {
    if (is.character(sym)) sym <- as.name(sym)
    .Internal(bindingIsActive(sym, env))
}

unlockBinding <- function(sym, env) {
    if (is.character(sym)) sym <- as.name(sym)
    .Internal(unlockBinding(sym, env))
}
#  File src/library/base/R/builtins.R
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

builtins <- function(internal=FALSE)
    .Internal(builtins(internal))
#  File src/library/base/R/by.R
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

by <- function(data, INDICES, FUN, ..., simplify = TRUE) UseMethod("by")

## prior to 2.7.0 this promoted vectors to data frames, but
## the data frame method dropped to a single column.
by.default <- function(data, INDICES, FUN, ..., simplify = TRUE)
{
    dd <- as.data.frame(data)
    if(length(dim(data)))
        by(dd, INDICES, FUN, ..., simplify = simplify)
    else {
        if(!is.list(INDICES)) {        # record the names for print.by
            IND <- vector("list", 1L)
            IND[[1L]] <- INDICES
            names(IND) <- deparse(substitute(INDICES))[1L]
        } else IND <- INDICES
        FUNx <- function(x) FUN(dd[x,], ...)
        nd <- nrow(dd)
        ans <- eval(substitute(tapply(1L:nd, IND, FUNx, simplify = simplify)),
                    dd)
        attr(ans, "call") <- match.call()
        class(ans) <- "by"
        ans
    }
}

by.data.frame <- function(data, INDICES, FUN, ..., simplify = TRUE)
{
    if(!is.list(INDICES)) { # record the names for print.by
        IND <- vector("list", 1L)
        IND[[1L]] <- INDICES
        names(IND) <- deparse(substitute(INDICES))[1L]
    } else IND <- INDICES
    FUNx <- function(x) FUN(data[x,, drop=FALSE], ...) # (PR#10506)
    nd <- nrow(data)
    ans <- eval(substitute(tapply(1L:nd, IND, FUNx, simplify = simplify)), data)
    attr(ans, "call") <- match.call()
    class(ans) <- "by"
    ans
}

print.by <- function(x, ..., vsep)
{
    d <- dim(x)
    dn <- dimnames(x)
    dnn <- names(dn)
    if(missing(vsep))
        vsep <- paste(rep("-", 0.75*getOption("width")), collapse = "")
    lapply(seq_along(x), function(i, x, vsep, ...) {
        if(i != 1L && !is.null(vsep)) cat(vsep, "\n")
        ii <- i - 1L
        for(j in seq_along(dn)) {
            iii <- ii %% d[j] + 1L; ii <- ii %/% d[j]
            cat(dnn[j], ": ", dn[[j]][iii], "\n", sep = "")
        }
        print(x[[i]], ...)
    } , x, vsep, ...)
    invisible(x)
}
#  File src/library/base/R/callCC.R
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

callCC <- function(fun) {
    value <- NULL
    delayedAssign("throw", return(value))
    fun(function(v) { value <<- v; throw })
}
#  File src/library/base/R/cat.R
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

cat <- function(..., file = "", sep = " ", fill = FALSE,
                labels = NULL, append = FALSE)
{
    if(is.character(file))
        if(file == "") file <- stdout()
        else if(substring(file, 1L, 1L) == "|") {
            file <- pipe(substring(file, 2L), "w")
            on.exit(close(file))
        } else {
            file <- file(file, ifelse(append, "a", "w"))
            on.exit(close(file))
        }
    .Internal(cat(list(...), file, sep, fill, labels, append))
}
#  File src/library/base/R/character.R
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

strsplit <-
    function(x, split, extended = TRUE, fixed = FALSE, perl = FALSE,
             useBytes = FALSE)
    .Internal(strsplit(x, as.character(split), as.logical(extended),
                       as.logical(fixed), as.logical(perl),
                       as.logical(useBytes)))

substr <- function(x, start, stop)
{
    if(!is.character(x)) x <- as.character(x)
    .Internal(substr(x, as.integer(start), as.integer(stop)))
}

substring <- function(text, first, last=1000000L)
{
    if(!is.character(text)) text <- as.character(text)
    n <- max(lt <- length(text), length(first), length(last))
    if(lt && lt < n) text <- rep(text, length.out = n)
    .Internal(substr(text, as.integer(first), as.integer(last)))
}

`substr<-` <- function(x, start, stop, value)
    .Internal(`substr<-`(x, as.integer(start), as.integer(stop), value))

`substring<-` <- function(text, first, last=1000000L, value)
    .Internal(`substr<-`(text, as.integer(first), as.integer(last), value))

abbreviate <-
    function(names.arg, minlength = 4, use.classes = TRUE, dot = FALSE,
             strict = FALSE, method = c("left.kept", "both.sides"))
{
    ## we just ignore use.classes
    if(minlength <= 0)
	return(rep.int("", length(names.arg)))
    ## need to remove leading/trailing spaces before we check for dups
    ## This is inefficient but easier than modifying do_abbrev (=> FIXME !)
    names.arg <- sub("^ +", "", sub(" +$", "", as.character(names.arg)))
    dups <- duplicated(names.arg)
    old <- names.arg
    if(any(dups))
	names.arg <- names.arg[!dups]
    x <- names.arg
    if(strict) {
	x[] <- .Internal(abbreviate(x, minlength, use.classes))
    } else {
	method <- match.arg(method)
	if(method == "both.sides")
	    ## string reversion: FIXME reverse .Internal(abbreviate(.))
	    chRev <- function(x)
		sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")
	dup2 <- rep.int(TRUE, length(names.arg))
	these <- names.arg
	repeat {
	    ans <- .Internal(abbreviate(these, minlength, use.classes))
	    ## NB: fulfills   max(nchar(ans)) <= minlength
	    x[dup2] <- ans
	    if(!any(dup2 <- duplicated(x))) break
	    if(method == "both.sides") { ## abbreviate the dupl. ones from the other side:
		x[dup2] <- chRev(.Internal(abbreviate(chRev(names.arg[dup2]),
						      minlength, use.classes)))
		if(!any(dup2 <- duplicated(x))) break
	    }
	    minlength <- minlength+1
	    dup2 <- dup2 | match(x, x[dup2], 0L)
	    these <- names.arg[dup2]
	}
    }
    if(any(dups))
	x <- x[match(old,names.arg)]
    if(dot) {			    # add "." where we did abbreviate:
	chgd <- x != old
	x[chgd] <- paste(x[chgd],".",sep = "")
    }
    names(x) <- old
    x
}

make.names <- function(names, unique = FALSE, allow_ = TRUE)
{
    names <- .Internal(make.names(as.character(names), allow_))
    if(unique) names <- make.unique(names)
    names
}

make.unique <- function (names, sep = ".") .Internal(make.unique(names, sep))

chartr <- function(old, new, x)
{
    if(!is.character(x)) x <- as.character(x)
    .Internal(chartr(old, new, x))
}
tolower <- function(x) {
    if(!is.character(x)) x <- as.character(x)
    .Internal(tolower(x))
}
toupper <- function(x) {
    if(!is.character(x)) x <- as.character(x)
    .Internal(toupper(x))
}

casefold <- function(x, upper = FALSE)
    if(upper) toupper(x) else tolower(x)

sQuote <- function(x) {
    before <- after <- "'"
    q <- getOption("useFancyQuotes")
    if(!is.null(q)) {
        if(identical(q, TRUE)) {
            li <- l10n_info()
            if(li$"UTF-8") q <- "UTF-8"
            if(!is.null(li$codepage) && li$codepage > 0L) {
                ## we can't just use iconv, as that seems to think
                ## it is in latin1 in CP1252
                if(li$codepage >= 1250L && li$codepage <= 1258L
                   || li$codepage == 874L) {
                    before <- "\x91"; after <- "\x92"
                } else {
                    z <- iconv(c("\xe2\x80\x98", "\xe2\x80\x99"), "UTF-8", "")
                    before <- z[1L]; after <- z[2L]
                }
            }
        }
        if(identical(q, "TeX")) {
            before <- "`"; after <- "'"
        }
        if(identical(q, "UTF-8")) {
            before <- "\xe2\x80\x98"; after <- "\xe2\x80\x99"
        }
        if(is.character(q) && length(q) >= 4L) {
            before <- q[1L]; after <- q[2L]
        }
        ## we do not want these strings marked as in the encoding
        ## R was built under
        Encoding(before) <- Encoding(after) <- "unknown"
    }
    paste(before, x, after, sep = "")
}

dQuote <- function(x) {
    before <- after <- "\""
    q <- getOption("useFancyQuotes")
    if(!is.null(q)) {
        if(identical(q, TRUE)) {
            li <- l10n_info()
            if(li$"UTF-8") q <- "UTF-8"
            if(!is.null(li$codepage) && li$codepage > 0L) {
                if(li$codepage >= 1250L && li$codepage <= 1258L
                    || li$codepage == 874L) {
                    before <- "\x93"; after <- "\x94"
                } else {
                    z <- iconv(c("\xe2\x80\x9c", "\xe2\x80\x9d"), "UTF-8", "")
                    before <- z[1L]; after <- z[2L]
                }
            }
        }
        if(identical(q, "TeX")) {
            before <- "``"; after <- "''"
        }
        if(identical(q, "UTF-8")) {
            before <- "\xe2\x80\x9c"; after <- "\xe2\x80\x9d"
        }
        if(is.character(q) && length(q) >= 4L) {
            before <- q[3L]; after <- q[4L]
        }
        Encoding(before) <- Encoding(after) <- "unknown"
    }
    paste(before, x, after, sep = "")
}
#  File src/library/base/R/chol.R
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

chol <- function(x, ...) UseMethod("chol")

chol.default <- function(x, pivot = FALSE, LINPACK = pivot, ...)
{
    if (is.complex(x))
        stop("complex matrices not permitted at present")
    else if(!is.numeric(x))
	stop("non-numeric argument to 'chol'")

    if(is.matrix(x)) {
	if(nrow(x) != ncol(x))
	    stop("non-square matrix in 'chol'")
	n <- nrow(x)
    } else {
	if(length(x) != 1L)
	    stop("non-matrix argument to 'chol'")
	n <- 1L
    }
    if(!pivot && !LINPACK)
        return(.Call("La_chol", as.matrix(x), PACKAGE = "base"))

    if(!is.double(x)) storage.mode(x) <- "double"

    if(pivot) { ## code could be used in the other case too
        xx <- x
        xx[lower.tri(xx)] <- 0
        z <- .Fortran("dchdc",
                      x = xx,
                      n,
                      n,
                      double(n),
                      piv = integer(n),
                      as.integer(pivot),
                      rank = integer(1L),
                      DUP = FALSE, PACKAGE = "base")
        if (z$rank < n)
            if(!pivot) stop("matrix not positive definite")
            else warning("matrix not positive definite")
        robj <- z$x
        if (pivot) {
            attr(robj, "pivot") <- z$piv
            attr(robj, "rank") <- z$rank
            if(!is.null(cn <- colnames(x)))
                colnames(robj) <- cn[z$piv]
        }
        robj
    } else {
        z <- .Fortran("chol",
                      x = x,
                      n,
                      n,
                      v = matrix(0, nrow=n, ncol=n),
                      info = integer(1L),
                      DUP = FALSE, PACKAGE = "base")
        if(z$info)
            stop("non-positive definite matrix in 'chol'")
        z$v
    }
}

chol2inv <- function(x, size=NCOL(x), LINPACK=FALSE)
{
    if(!is.numeric(x))
	stop("non-numeric argument to 'chol2inv'")
    if(!LINPACK) return(.Call("La_chol2inv", x, size, PACKAGE = "base"))

    if(is.matrix(x)) {
	nr <- nrow(x)
	nc <- ncol(x)
    }
    else {
	nr <- length(x)
	nc <- 1L
    }
    size <- as.integer(size)
    if(size <= 0L || size > nr || size > nc)
	stop("invalid 'size' argument in 'chol2inv'")
    if(!is.double(x)) storage.mode(x) <- "double"
    z <- .Fortran("ch2inv",
		  x=x,
		  nr,
		  size,
		  v=matrix(0, nrow=size, ncol=size),
		  info=integer(1L),
		  DUP=FALSE, PACKAGE="base")
    if(z$info)
	stop("singular matrix in 'chol2inv'")
    z$v
}
#  File src/library/base/R/colSums.R
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


## NB: we now have  implicitGeneric() on these,
##     in ../../methods/R/makeBasicFunsList.R

colSums <- function(x, na.rm = FALSE, dims = 1L)
{
    if(is.data.frame(x)) x <- as.matrix(x)
    if(!is.array(x) || length(dn <- dim(x)) < 2L)
        stop("'x' must be an array of at least two dimensions")
    if(dims < 1L || dims > length(dn) - 1L)
        stop("invalid 'dims'")
    n <- prod(dn[1L:dims])
    dn <- dn[-(1L:dims)]
    z <- if(is.complex(x))
        .Internal(colSums(Re(x), n, prod(dn), na.rm)) +
            1i * .Internal(colSums(Im(x), n, prod(dn), na.rm))
    else .Internal(colSums(x, n, prod(dn), na.rm))
    if(length(dn) > 1L) {
        dim(z) <- dn
        dimnames(z) <- dimnames(x)[-(1L:dims)]
    } else names(z) <- dimnames(x)[[dims+1]]
    z
}

colMeans <- function(x, na.rm = FALSE, dims = 1L)
{
    if(is.data.frame(x)) x <- as.matrix(x)
    if(!is.array(x) || length(dn <- dim(x)) < 2L)
        stop("'x' must be an array of at least two dimensions")
    if(dims < 1L || dims > length(dn) - 1L)
        stop("invalid 'dims'")
    n <- prod(dn[1L:dims])
    dn <- dn[-(1L:dims)]
    z <- if(is.complex(x))
        .Internal(colMeans(Re(x), n, prod(dn), na.rm)) +
            1i * .Internal(colMeans(Im(x), n, prod(dn), na.rm))
    else .Internal(colMeans(x, n, prod(dn), na.rm))
    if(length(dn) > 1L) {
        dim(z) <- dn
        dimnames(z) <- dimnames(x)[-(1L:dims)]
    } else names(z) <- dimnames(x)[[dims+1]]
    z
}

rowSums <- function(x, na.rm = FALSE, dims = 1L)
{
    if(is.data.frame(x)) x <- as.matrix(x)
    if(!is.array(x) || length(dn <- dim(x)) < 2L)
        stop("'x' must be an array of at least two dimensions")
    if(dims < 1L || dims > length(dn) - 1L)
        stop("invalid 'dims'")
    p <- prod(dn[-(1L:dims)])
    dn <- dn[1L:dims]
    z <- if(is.complex(x))
        .Internal(rowSums(Re(x), prod(dn), p, na.rm)) +
            1i * .Internal(rowSums(Im(x), prod(dn), p, na.rm))
    else .Internal(rowSums(x, prod(dn), p, na.rm))
    if(length(dn) > 1L) {
        dim(z) <- dn
        dimnames(z) <- dimnames(x)[1L:dims]
    } else  names(z) <- dimnames(x)[[1L]]
    z
}

rowMeans <- function(x, na.rm = FALSE, dims = 1L)
{
    if(is.data.frame(x)) x <- as.matrix(x)
    if(!is.array(x) || length(dn <- dim(x)) < 2L)
        stop("'x' must be an array of at least two dimensions")
    if(dims < 1L || dims > length(dn) - 1L)
        stop("invalid 'dims'")
    p <- prod(dn[-(1L:dims)])
    dn <- dn[1L:dims]
    z <- if(is.complex(x))
        .Internal(rowMeans(Re(x), prod(dn), p, na.rm)) +
            1i * .Internal(rowMeans(Im(x), prod(dn), p, na.rm))
    else .Internal(rowMeans(x, prod(dn), p, na.rm))
    if(length(dn) > 1L) {
        dim(z) <- dn
        dimnames(z) <- dimnames(x)[1L:dims]
    } else  names(z) <- dimnames(x)[[1L]]
    z
}
#  File src/library/base/R/conditions.R
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

##
## Handling Conditions
##

## CARE:  try() in ./New-Internal.R  depends on *internal* coding of tryCatch()!
## ----   If you change this, be sure to adapt  try().
tryCatch <- function(expr, ..., finally) {
    tryCatchList <- function(expr, names, parentenv, handlers) {
	nh <- length(names)
	if (nh > 1L)
	    tryCatchOne(tryCatchList(expr, names[-nh], parentenv,
                                     handlers[-nh]),
			names[nh], parentenv, handlers[[nh]])
	else if (nh == 1L)
	    tryCatchOne(expr, names, parentenv, handlers[[1L]])
	else expr
    }
    tryCatchOne <- function(expr, name, parentenv, handler) {
	doTryCatch <- function(expr, name, parentenv, handler) {
	    .Internal(.addCondHands(name, list(handler), parentenv,
				    environment(), FALSE))
	    expr
	}
	value <- doTryCatch(return(expr), name, parentenv, handler)
	# The return in the call above will exit withOneRestart unless
	# the handler is invoked; we only get to this point if the handler
	# is invoked.  If we get here then the handler will have been
	# popped off the internal handler stack.
	if (is.null(value[[1L]])) {
	    # a simple error; message is stored internally
	    # and call is in result; this defers all allocs until
	    # after the jump
	    msg <- .Internal(geterrmessage())
	    call <- value[[2L]]
	    cond <- simpleError(msg, call)
	}
	else cond <- value[[1L]]
	value[[3L]](cond)
    }
    if (! missing(finally))
        on.exit(finally)
    handlers <- list(...)
    classes <- names(handlers)
    parentenv <- parent.frame()
    if (length(classes) != length(handlers))
        stop("bad handler specification")
    tryCatchList(expr, classes, parentenv, handlers)
}

withCallingHandlers <- function(expr, ...) {
    handlers <- list(...)
    classes <- names(handlers)
    parentenv <- parent.frame()
    if (length(classes) != length(handlers))
        stop("bad handler specification")
    .Internal(.addCondHands(classes, handlers, parentenv, NULL, TRUE))
    expr
}

suppressWarnings <- function(expr) {
    withCallingHandlers(expr,
                        warning=function(w)
                            invokeRestart("muffleWarning"))
}


##
## Conditions and Condition Signaling
##

simpleCondition <- function(message, call = NULL) {
    class <- c("simpleCondition", "condition")
    structure(list(message=as.character(message), call = call), class=class)
}

simpleError <- function(message, call = NULL) {
    class <- c("simpleError", "error", "condition")
    structure(list(message=as.character(message), call = call), class=class)
}

simpleWarning <- function(message, call = NULL) {
    class <- c("simpleWarning", "warning", "condition")
    structure(list(message=as.character(message), call = call), class=class)
}

conditionMessage <- function(c) UseMethod("conditionMessage")
conditionCall <- function(c) UseMethod("conditionCall")

conditionMessage.condition <- function(c) c$message
conditionCall.condition <- function(c) c$call

print.condition <- function(x, ...) {
    msg <- conditionMessage(x)
    call <- conditionCall(x)
    cl <- class(x)[1L]
    if (! is.null(call))
        cat("<", cl, " in ", deparse(call), ": ", msg, ">\n", sep="")
    else
        cat("<", cl, ": ", msg, ">\n", sep="")
    invisible(x)
}

as.character.condition <- function(x, ...) {
    msg <- conditionMessage(x)
    call <- conditionCall(x)
    cl <- class(x)[1L]
    if (! is.null(call))
        paste(cl, " in ", deparse(call)[1L], ": ", msg, "\n", sep="")
    else
        paste(cl, ": ", msg, "\n", sep="")
}

as.character.error <- function(x, ...) {
    msg <- conditionMessage(x)
    call <- conditionCall(x)
    if (! is.null(call))
        paste("Error in ", deparse(call)[1L], ": ", msg, "\n", sep="")
    else
        paste("Error: ", msg, "\n", sep="")
}

signalCondition <- function(cond) {
    if (! inherits(cond, "condition"))
        cond <- simpleCondition(cond)
    msg <- conditionMessage(cond)
    call <- conditionCall(cond)
    .Internal(.signalCondition(cond, msg, call))
}


##
##  Restarts
##

restartDescription <- function(r) r$description
restartFormals <- function(r) formals(r$handler)

print.restart <- function(x, ...) {
    cat(paste("<restart:", x[[1L]], ">\n"))
    invisible(x)
}

isRestart <- function(x) inherits(x, "restart")

findRestart <- function(name, cond = NULL) {
    i <- 1L
    repeat {
        r <- .Internal(.getRestart(i))
        if (is.null(r))
            return(NULL)
        else if (name == r[[1L]] &&
                 (is.null(cond) || is.null(r$test) || r$test(cond)))
            return(r)
        else i <- i + 1L
    }
}

computeRestarts <- function(cond = NULL) {
    val <- NULL
    i <- 1L
    repeat {
        r <- .Internal(.getRestart(i))
        if (is.null(r))
            return(val)
        else if (is.null(cond) || is.null(r$test) || r$test(cond))
            val <- c(val, list(r))
        i <- i + 1L
    }
}

invokeRestart <- function(r, ...) {
    if (! isRestart(r)) {
        res <- findRestart(r)
        if (is.null(res))
            stop(gettextf("no 'restart' '%s' found", as.character(r)),
                 domain = NA)
        r <- res
    }
    .Internal(.invokeRestart(r, list(...)))
}

invokeRestartInteractively <- function(r) {
    if (! interactive())
        stop("not an interactive session")
    if (! isRestart(r)) {
        res <- findRestart(r)
        if (is.null(res))
            stop(gettextf("no 'restart' '%s' found", as.character(r)),
                 domain = NA)
        r <- res
    }
    if (is.null(r$interactive)) {
        pars <- names(restartFormals(r))
        args <- NULL
        if (length(pars)) {
            cat("Enter values for restart arguments:\n\n")
            for (p in pars) {
            if (p == "...") {
		    prompt <- "... (a list): "
		    args <- c(args, eval(parse(prompt = prompt)))
		}
		else {
		    prompt <- paste(p, ": ", sep="")
		    args <- c(args, list(eval(parse(prompt = prompt))))
		}
	    }
	}
    }
    else args <- r$interactive()
    .Internal(.invokeRestart(r, args))
}

withRestarts <- function(expr, ...) {
    docall <- function(fun, args) {
	enquote <- function(x) as.call(list(as.name("quote"), x))
	if ((is.character(fun) && length(fun) == 1L) || is.name(fun))
	    fun <- get(as.character(fun), envir = parent.frame(),
                       mode = "function")
	do.call("fun", lapply(args, enquote))
    }
    makeRestart <- function(name = "",
			   handler = function(...) NULL,
			   description = "",
			   test = function(c) TRUE,
			   interactive = NULL) {
	structure(list(name = name, exit = NULL, handler = handler,
		       description = description, test = test,
		       interactive = interactive),
		  class = "restart")
    }
    makeRestartList <- function(...) {
        specs <- list(...)
        names <- names(specs)
        restarts <- vector("list", length(specs))
        for (i in seq_along(specs)) {
            spec <- specs[[i]]
            name <- names[i]
            if (is.function(spec))
                restarts[[i]] <- makeRestart(handler = spec)
            else if (is.character(spec))
                restarts[[i]] <- makeRestart(description = spec)
            else if (is.list(spec))
                restarts[[i]] <- docall("makeRestart", spec)
            else
               stop("not a valid restart specification")
            restarts[[i]]$name <- name
        }
        restarts
    }
    withOneRestart <- function(expr, restart) {
	doWithOneRestart <- function(expr, restart) {
	    restart$exit <- environment()
	    .Internal(.addRestart(restart))
	    expr
	}
	restartArgs <- doWithOneRestart(return(expr), restart)
	# The return in the call above will exit withOneRestart unless
	# the restart is invoked; we only get to this point if the restart
	# is invoked.  If we get here then the restart will have been
	# popped off the internal restart stack.
	docall(restart$handler, restartArgs)
    }
    withRestartList <- function(expr, restarts) {
	nr <- length(restarts)
	if (nr > 1L)
	    withOneRestart(withRestartList(expr, restarts[-nr]),
                           restarts[[nr]])
	else if (nr == 1L)
	    withOneRestart(expr, restarts[[1L]])
	else expr
    }
    restarts <- makeRestartList(...)
    if (length(restarts) == 0L)
        expr
    else if (length(restarts) == 1L)
        withOneRestart(expr, restarts[[1L]])
    else withRestartList(expr, restarts)
}


##
## Callbacks
##

.signalSimpleWarning <- function(msg, call)
    withRestarts({
           .Internal(.signalCondition(simpleWarning(msg, call), msg, call))
           .Internal(.dfltWarn(msg, call))
        }, muffleWarning = function() NULL)

.handleSimpleError <- function(h, msg, call)
    h(simpleError(msg, call))
#  File src/library/base/R/conflicts.R
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

conflicts <- function(where = search(), detail = FALSE)
{
    if(length(where) < 1L) stop("argument 'where' of length 0")
    z <- vector(length(where), mode="list")
    names(z) <- where
    for(i in seq_along(where)) z[[i]] <- objects(pos = where[i])
    all <- unlist(z, use.names=FALSE)
    dups <- duplicated(all)
    dups <- all[dups]
    if(detail) {
	for(i in where) z[[i]] <- z[[i]][match(dups, z[[i]], 0L)]
	z[sapply(z, function(x) length(x) == 0L)] <- NULL
	z
    } else dups
}
#  File src/library/base/R/connections.R
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

stdin <- function() .Internal(stdin())
stdout <- function() .Internal(stdout())
stderr <- function() .Internal(stderr())

readLines <- function(con = stdin(), n = -1L, ok = TRUE, warn = TRUE,
                      encoding = "unknown")
{
    if(is.character(con)) {
        con <- file(con, "r")
        on.exit(close(con))
    }
    .Internal(readLines(con, n, ok, warn, encoding))
}


writeLines <- function(text, con = stdout(), sep = "\n", useBytes = FALSE)
{
    if(is.character(con)) {
        con <- file(con, "w")
        on.exit(close(con))
    }
    invisible(.Internal(writeLines(text, con, sep, useBytes)))
}

open <- function(con, ...)
    UseMethod("open")

open.connection <- function(con, open = "r", blocking = TRUE, ...)
{
    invisible(.Internal(open(con, open, blocking)))
}

isOpen <- function(con, rw = "")
{
    rw <- pmatch(rw, c("read", "write"), 0L)
    .Internal(isOpen(con, rw))
}

isIncomplete <- function(con)
    .Internal(isIncomplete(con))

isSeekable <- function(con)
    .Internal(isSeekable(con))

close <- function(con, ...)
    UseMethod("close")

close.connection <- function (con, type = "rw", ...)
    invisible(.Internal(close(con, type)))

flush <- function(con) UseMethod("flush")

flush.connection <- function (con)
    invisible(.Internal(flush(con)))

file <- function(description = "", open = "", blocking = TRUE,
                 encoding = getOption("encoding"))
    .Internal(file(description, open, blocking, encoding))

pipe <- function(description, open = "", encoding = getOption("encoding"))
    .Internal(pipe(description, open, encoding))

fifo <- function(description, open = "", blocking = FALSE,
                 encoding = getOption("encoding"))
    .Internal(fifo(description, open, blocking, encoding))

url <- function(description, open = "", blocking = TRUE,
                encoding = getOption("encoding"))
    .Internal(url(description, open, blocking, encoding))

gzfile <- function(description, open = "",
                   encoding = getOption("encoding"), compression = 6)
    .Internal(gzfile(description, open, encoding, compression))

unz <- function(description, filename, open = "",
                encoding = getOption("encoding"))
    .Internal(unz(paste(description, filename, sep=":"), open, encoding))

bzfile <- function(description, open = "", encoding = getOption("encoding"),
                   compression = 9)
    .Internal(bzfile(description, open, encoding, compression))

xzfile <- function(description, open = "", encoding = getOption("encoding"),
                   compression = 6)
    .Internal(xzfile(description, open, encoding, compression))

socketConnection <- function(host = "localhost", port, server = FALSE,
                             blocking = FALSE, open = "a+",
                             encoding = getOption("encoding"))
    .Internal(socketConnection(host, port, server, blocking, open, encoding))

rawConnection <- function(object, open = "r") {
    .Internal(rawConnection(deparse(substitute(object)), object, open))
}

rawConnectionValue <- function(con) .Internal(rawConnectionValue(con))

textConnection <- function(object, open = "r", local = FALSE,
                           encoding = c("", "bytes", "UTF-8"))
{
    env <- if (local) parent.frame() else .GlobalEnv
    type <- match(match.arg(encoding), c("", "bytes", "UTF-8"))
    .Internal(textConnection(deparse(substitute(object)), object, open,
                             env, type))
}

textConnectionValue <- function(con) .Internal(textConnectionValue(con))

seek <- function(con, ...)
    UseMethod("seek")

seek.connection <- function(con, where = NA, origin = "start", rw = "", ...)
{
    origin <- pmatch(origin, c("start", "current", "end"))
    rw <- pmatch(rw, c("read", "write"), 0L)
    if(is.na(origin))
        stop("'origin' must be one of 'start', 'current' or 'end'")
    .Internal(seek(con, as.double(where), origin, rw))
}

truncate <- function(con, ...)
    UseMethod("truncate")

truncate.connection <- function(con, ...)
{
    if(!isOpen(con)) stop("can only truncate an open connection")
    .Internal(truncate(con))
}

pushBack <- function(data, connection, newLine = TRUE)
    invisible(.Internal(pushBack(data, connection, newLine)))

pushBackLength <- function(connection)
    .Internal(pushBackLength(connection))

print.connection <- function(x, ...)
{
    print(unlist(summary(x)))
    invisible(x)
}

summary.connection <- function(object, ...)
    .Internal(summary.connection(object))

showConnections <- function(all = FALSE)
{
    set <- getAllConnections()
    if(!all) set <- set[set > 2L]
    ans <- matrix("", length(set), 7L)
    for(i in seq_along(set)) ans[i, ] <- unlist(summary.connection(set[i]))
    rownames(ans) <- set
    colnames(ans) <- c("description", "class", "mode", "text", "isopen",
                       "can read", "can write")
    if(!all) ans[ans[, 5L] == "opened", , drop = FALSE]
    else ans[, , drop = FALSE]
}

getAllConnections <- function()
    .Internal(getAllConnections())

getConnection <- function(what) .Internal(getConnection(what))

closeAllConnections <- function()
{
    # first re-divert any diversion of stderr.
    i <- sink.number(type = "message")
    if(i > 0L) sink(stderr(), type = "message")
    # now unwind the sink diversion stack.
    n <- sink.number()
    if(n > 0L) for(i in seq_len(n)) sink()
    # get all the open connections.
    set <- getAllConnections()
    set <- set[set > 2L]
    # and close all user connections.
    for(i in seq_along(set)) close(getConnection(set[i]))
    invisible()
}

readBin <- function(con, what, n = 1L, size = NA_integer_, signed = TRUE,
                    endian = .Platform$endian)
{
    if(is.character(con)) {
        con <- file(con, "rb")
        on.exit(close(con))
    }
    swap <- endian != .Platform$endian
    if(!is.character(what) || is.na(what) ||
       length(what) != 1L || ## hence length(what) == 1:
       !any(what == c("numeric", "double", "integer", "int", "logical",
	    "complex", "character", "raw")))
	what <- typeof(what)
    .Internal(readBin(con, what, n, size, signed, swap))
}

writeBin <-
    function(object, con, size = NA_integer_, endian = .Platform$endian,
             useBytes = FALSE)
{
    swap <- endian != .Platform$endian
    if(!is.vector(object) || mode(object) == "list")
        stop("can only write vector objects")
    if(is.character(con)) {
        con <- file(con, "wb")
        on.exit(close(con))
    }
    .Internal(writeBin(object, con, size, swap, useBytes))
}

readChar <- function(con, nchars, useBytes = FALSE)
{
    if(is.character(con)) {
        con <- file(con, "rb")
        on.exit(close(con))
    }
    .Internal(readChar(con, as.integer(nchars), useBytes))
}

writeChar <- function(object, con, nchars = nchar(object, type="chars"),
                      eos = "", useBytes = FALSE)
{
    if(!is.character(object))
        stop("can only write character objects")
    if(is.character(con)) {
        con <- file(con, "wb")
        on.exit(close(con))
    }
    .Internal(writeChar(object, con, as.integer(nchars), eos, useBytes))
}

gzcon <- function(con, level = 6, allowNonCompressed = TRUE)
    .Internal(gzcon(con, level, allowNonCompressed))

socketSelect <- function(socklist, write = FALSE, timeout = NULL) {
    if (is.null(timeout))
        timeout <- -1
    else if (timeout < 0)
        stop("supplied timeout must be NULL or a non-negative number")
    if (length(write) < length(socklist))
        write <- rep(write, length.out = length(socklist))
    .Internal(sockSelect(socklist, write, timeout))
}

memCompress <-
    function(from, type = c("gzip", "bzip2", "xz", "none"))
{
    if(is.character(from))
        from <- charToRaw(paste(from, collapse = "\n"))
    else if(!is.raw(from)) stop("'from' must be raw or character")
    type <- match(match.arg(type), c("none", "gzip", "bzip2", "xz"))
    .Internal(memCompress(from, type))
}

memDecompress <-
    function(from,
             type = c("unknown", "gzip", "bzip2", "xz", "none"),
             asChar = FALSE)
{
    type <- match(match.arg(type),
                  c("none", "gzip", "bzip2", "xz", "unknown"))
    ans <- .Internal(memDecompress(from, type))
    if(asChar) rawToChar(ans) else ans
}
#  File src/library/base/R/constants.R
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

pi <- 4*atan(1)

letters <- c("a","b","c","d","e","f","g","h","i","j","k","l", "m",
	     "n","o","p","q","r","s","t","u","v","w","x","y","z")

LETTERS <- c("A","B","C","D","E","F","G","H","I","J","K","L", "M",
	     "N","O","P","Q","R","S","T","U","V","W","X","Y","Z")

month.name <-
    c("January", "February", "March", "April", "May", "June",
      "July", "August", "September", "October", "November", "December")

month.abb <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
	       "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
#  File src/library/base/R/contributors.R
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

contributors <- function()
{
    outFile <- tempfile()
    outConn <- file(outFile, open = "w")
    writeLines(paste("R is a project which is attempting to provide a ",
                     "modern piece of\nstatistical software for the ",
                     "GNU suite of software.\n\n",
                     "The current R is the result of a collaborative ",
                     "effort with\ncontributions from all over the ",
                     "world.\n\n",
                     sep = ""), outConn)
    writeLines(readLines(file.path(R.home("doc"), "AUTHORS")), outConn)
    writeLines("", outConn)
    writeLines(readLines(file.path(R.home("doc"), "THANKS")), outConn)
    close(outConn)
    file.show(outFile, delete.file = TRUE)
}
#  File src/library/base/R/converters.R
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

getNumCConverters <- function()
    .Internal(getNumRtoCConverters())


getCConverterDescriptions <- function()
 .Internal(getRtoCConverterDescriptions())


getCConverterStatus <- function()
{
    v <- .Internal(getRtoCConverterStatus())
    names(v) <- getCConverterDescriptions()
    v
}


setCConverterStatus <- function(id, status)
  .Internal(setToCConverterActiveStatus(id, as.logical(status)))


removeCConverter <- function(id)
  .Internal(removeToCConverterActiveStatus(id))

#  File src/library/base/R/cut.R
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

cut <- function(x, ...) UseMethod("cut")

cut.default <-
    function (x, breaks, labels = NULL, include.lowest = FALSE,
              right = TRUE, dig.lab = 3, ordered_result = FALSE, ...)
{
    if (!is.numeric(x)) stop("'x' must be numeric")
    if (length(breaks) == 1L) {
	if (is.na(breaks) | breaks < 2L)
	    stop("invalid number of intervals")
	nb <- as.integer(breaks + 1)# one more than #{intervals}
	dx <- diff(rx <- range(x,na.rm=TRUE))
	if(dx == 0) dx <- abs(rx[1L])
	breaks <- seq.int(rx[1L] - dx/1000,
                          rx[2L] + dx/1000, length.out = nb)
    } else nb <- length(breaks <- sort.int(as.double(breaks)))
    if (anyDuplicated(breaks)) stop("'breaks' are not unique")
    codes.only <- FALSE
    if (is.null(labels)) {#- try to construct nice ones ..
	for(dig in dig.lab:max(12, dig.lab)) {
	    ch.br <- formatC(breaks, digits=dig, width=1)
	    if(ok <- all(ch.br[-1L] != ch.br[-nb])) break
	}
	labels <-
	    if(ok) paste(if(right)"(" else "[",
			 ch.br[-nb], ",", ch.br[-1L],
			 if(right)"]" else ")", sep='')
	    else paste("Range", seq_len(nb - 1L), sep="_")
        if (ok && include.lowest) {
            if (right)
                substr(labels[1L], 1L, 1L) <- "[" # was "("
            else
                substring(labels[nb-1L],
                          nchar(labels[nb-1L], "c")) <- "]" # was ")"
        }
    } else if (is.logical(labels) && !labels)
        codes.only <- TRUE
    else if (length(labels) != nb - 1L)
        stop("labels/breaks length conflict")
    code <- .C("bincode",
	       x =     	as.double(x),
	       n =	as.integer(length(x)),
	       breaks =	as.double(breaks),
               as.integer(nb),
	       code= 	integer(length(x)),
               right=	as.logical(right),
	       include= as.logical(include.lowest), naok = TRUE,
	       NAOK= TRUE, DUP = FALSE, PACKAGE = "base") $code
    ## NB this relies on passing NAOK in that position!
    if(codes.only) code
    else factor(code, seq_along(labels), labels, ordered = ordered_result)
}
#  File src/library/base/R/data.matrix.R
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

data.matrix <- function(frame, rownames.force = NA)
{
    if(!is.data.frame(frame)) return(as.matrix(frame))

    d <- dim(frame)
    rn <- if(rownames.force %in% FALSE) NULL
    else if(rownames.force %in% TRUE) row.names(frame)
    else {if(.row_names_info(frame) <= 0L) NULL else row.names(frame)}

    for(i in seq_len(d[2L])) {
        xi <- frame[[i]]
        ## at present is.numeric suffices, but let's be cautious
        if(is.integer(xi) || is.numeric(xi)) next
        if(is.logical(xi) || is.factor(xi)) {
            frame[[i]] <- as.integer(xi)
            next
        }
        frame[[i]] <- if(isS4(xi)) methods::as(xi, "numeric") else as.numeric(xi)
    }

    ## it makes sense to find the type needed first.
    intOK <- all(unlist(lapply(frame, is.integer)))
    x <- matrix(if(intOK) NA_integer_ else NA_real_,
                nrow = d[1L], ncol = d[2L],
		dimnames = list(rn, names(frame)) )
    for(i in seq_len(d[2L])) x[, i] <- frame[[i]]
    x
}
#  File src/library/base/R/dataframe.R
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

## As from R 2.4.0, row.names can be either character or integer.
## row.names() will always return character.
## attr(, "row.names") will return either character or integer.
##
## Do not assume that the internal representation is either, since
## 1L:n is stored as the integer vector c(NA, n) to save space (and
## the C-level code to get/set the attribute makes the appropriate
## translations.
##
## As from 2.5.0 c(NA, n > 0) indicates deliberately assigned row names,
## and c(NA, n < 0) automatic row names.

.row_names_info <- function(x, type = 1L)
    .Call("R_shortRowNames", x, type, PACKAGE = "base")

row.names <- function(x) UseMethod("row.names")
row.names.data.frame <- function(x) as.character(attr(x, "row.names"))
row.names.default <- function(x) if(!is.null(dim(x))) rownames(x)# else NULL

.set_row_names <- function(n)
    if(n > 0) c(NA_integer_, -n) else integer(0L)

"row.names<-" <- function(x, value) UseMethod("row.names<-")

"row.names<-.data.frame" <- function(x, value)
{
    if (!is.data.frame(x)) x <- as.data.frame(x)
    n <- .row_names_info(x, 2L)
    if(is.null(value)) { # set automatic row.names
        attr(x, "row.names") <- .set_row_names(n)
        return(x)
    }
    ## do this here, as e.g. POSIXlt changes length when coerced.
    if( is.object(value) || !is.integer(value) )
        value <- as.character(value)
    if(n == 0L) {
        ## we have to be careful here.  This could be a
        ## 0-row data frame or an invalid one being constructed.
        if(!is.null(attr(x, "row.names")) && length(value) > 0L)
           stop("invalid 'row.names' length")
    }
    else if (length(value) != n)
	stop("invalid 'row.names' length")
    if (anyDuplicated(value)) {
        nonuniq <- sort(unique(value[duplicated(value)]))
        warning(ngettext(length(nonuniq),
                         sprintf("non-unique value when setting 'row.names': %s",
                                 sQuote(nonuniq[1L])),
                         sprintf("non-unique values when setting 'row.names': %s",
                                 paste(sQuote(nonuniq), collapse = ", "))),
                domain = NA, call. = FALSE)
	stop("duplicate 'row.names' are not allowed")
    }
    if (any(is.na(value)))
	stop("missing values in 'row.names' are not allowed")
    attr(x, "row.names") <- value
    x
}

"row.names<-.default" <- function(x, value) "rownames<-"(x, value)

is.na.data.frame <- function (x)
{
    y <- do.call("cbind", lapply(x, "is.na")) # gives a matrix
    if(.row_names_info(x) > 0L) rownames(y) <- row.names(x)
    y
}

is.data.frame <- function(x) inherits(x, "data.frame")

I <- function(x) { structure(x, class = unique(c("AsIs", oldClass(x)))) }

print.AsIs <- function (x, ...)
{
    cl <- oldClass(x)
    oldClass(x) <- cl[cl != "AsIs"]
    NextMethod("print")
    invisible(x)
}


t.data.frame <- function(x)
{
    x <- as.matrix(x)
    NextMethod("t")
}

dim.data.frame <- function(x) c(.row_names_info(x, 2L), length(x))

dimnames.data.frame <- function(x) list(row.names(x), names(x))

"dimnames<-.data.frame" <- function(x, value)
{
    d <- dim(x)
    if(!is.list(value) || length(value) != 2L)
	stop("invalid 'dimnames' given for data frame")
    ## do the coercion first, as might change length
    value[[1L]] <- as.character(value[[1L]])
    value[[2L]] <- as.character(value[[2L]])
    if(d[[1L]] != length(value[[1L]]) || d[[2L]] != length(value[[2L]]))
	stop("invalid 'dimnames' given for data frame")
    row.names(x) <- value[[1L]] # checks validity
    names(x) <- value[[2L]]
    x
}

as.data.frame <- function(x, row.names = NULL, optional = FALSE, ...)
{
    if(is.null(x))			# can't assign class to NULL
	return(as.data.frame(list()))
    UseMethod("as.data.frame")
}

as.data.frame.default <- function(x, ...)
    stop(gettextf("cannot coerce class %s into a data.frame",
                  deparse(class(x))),
         domain = NA)

###  Here are methods ensuring that the arguments to "data.frame"
###  are in a form suitable for combining into a data frame.

as.data.frame.data.frame <- function(x, row.names = NULL, ...)
{
    cl <- oldClass(x)
    i <- match("data.frame", cl)
    if(i > 1L)
	class(x) <- cl[ - (1L:(i-1L))]
    if(!is.null(row.names)){
        nr <- .row_names_info(x, 2L)
	if(length(row.names) == nr)
	    attr(x, "row.names") <- row.names
	else stop(gettextf("invalid 'row.names', length %d for a data frame with %d rows",
                           length(row.names), nr), domain = NA)
    }
    x
}

## prior to 1.8.0 this coerced names - PR#3280
as.data.frame.list <-
    function(x, row.names = NULL, optional = FALSE, ...,
             stringsAsFactors = default.stringsAsFactors())
{
    ## need to protect names in x.
    cn <- names(x)
    m <- match(c("row.names", "check.rows", "check.names", "stringsAsFactors"),
               cn, 0L)
    if(any(m)) {
        cn[m] <- paste("..adfl.", cn[m], sep="")
        names(x) <- cn
    }
    x <- eval(as.call(c(expression(data.frame), x, check.names = !optional,
                        stringsAsFactors = stringsAsFactors)))
    if(any(m)) names(x) <- sub("^\\.\\.adfl\\.", "", names(x))
    if(!is.null(row.names)) {
	# row.names <- as.character(row.names)
	if(length(row.names) != dim(x)[[1L]])
            stop(gettextf("supplied %d row names for %d rows",
                          length(row.names), dim(x)[[1L]]), domain = NA)
	attr(x, "row.names") <- row.names
    }
    x
}

as.data.frame.vector <- function(x, row.names = NULL, optional = FALSE, ...,
                                 nm = paste(deparse(substitute(x),
                                 width.cutoff = 500L), collapse=" ")  )
{
    force(nm)
    nrows <- length(x)
    if(is.null(row.names)) {
	if (nrows == 0L)
	    row.names <- character(0L)
	else if(length(row.names <- names(x)) == nrows &&
		!anyDuplicated(row.names)) {}
	else row.names <- .set_row_names(nrows)
    }
    if(!is.null(names(x))) names(x) <- NULL # remove names as from 2.0.0
    value <- list(x)
    if(!optional) names(value) <- nm
    attr(value, "row.names") <- row.names
    class(value) <- "data.frame"
    value
}

as.data.frame.ts <- function(x, ...)
{
    if(is.matrix(x))
	as.data.frame.matrix(x, ...)
    else
	as.data.frame.vector(x, ...)
}

as.data.frame.raw  <- as.data.frame.vector
as.data.frame.factor  <- as.data.frame.vector
as.data.frame.ordered <- as.data.frame.vector
as.data.frame.integer <- as.data.frame.vector
as.data.frame.numeric <- as.data.frame.vector
as.data.frame.complex <- as.data.frame.vector

default.stringsAsFactors <- function()
{
    val <- getOption("stringsAsFactors")
    if(is.null(val)) val <- TRUE
    if(!is.logical(val) || is.na(val) || length(val) != 1L)
        stop("options('stringsAsFactors') not set to TRUE or FALSE")
    val
}


as.data.frame.character <-
    function(x, ..., stringsAsFactors = default.stringsAsFactors()){
        nm <- deparse(substitute(x), width.cutoff=500L)
        if(stringsAsFactors) x <- factor(x)
        as.data.frame.vector(x, ..., nm=nm)
    }

as.data.frame.logical <- as.data.frame.vector

as.data.frame.matrix <- function(x, row.names = NULL, optional = FALSE, ...,
                                 stringsAsFactors = default.stringsAsFactors())
{
    d <- dim(x)
    nrows <- d[1L]; ir <- seq_len(nrows)
    ncols <- d[2L]; ic <- seq_len(ncols)
    dn <- dimnames(x)
    ## surely it cannot be right to override the supplied row.names?
    ## changed in 1.8.0
    if(is.null(row.names)) row.names <- dn[[1L]]
    collabs <- dn[[2L]]
    if(any(empty <- !nzchar(collabs)))
	collabs[empty] <- paste("V", ic, sep = "")[empty]
    value <- vector("list", ncols)
    if(mode(x) == "character" && stringsAsFactors) {
	for(i in ic)
	    value[[i]] <- as.factor(x[,i])
    } else {
	for(i in ic)
	    value[[i]] <- as.vector(x[,i])
    }
    if(length(row.names) != nrows)
	row.names <- .set_row_names(nrows)
    if(length(collabs) == ncols)
	names(value) <- collabs
    else if(!optional)
	names(value) <- paste("V", ic, sep="")
    attr(value, "row.names") <- row.names
    class(value) <- "data.frame"
    value
}

as.data.frame.model.matrix <-
    function(x, row.names = NULL, optional = FALSE, ...)
{
    d <- dim(x)
    nrows <- d[1L]
    dn <- dimnames(x)
    row.names <- dn[[1L]]
    value <- list(x)
    if(!is.null(row.names)) {
	row.names <- as.character(row.names)
	if(length(row.names) != nrows)
            stop(gettextf("supplied %d row names for %d rows",
                          length(row.names), nrows), domain = NA)
    }
    else row.names <- .set_row_names(nrows)
    if(!optional) names(value) <- deparse(substitute(x))[[1L]]
    attr(value, "row.names") <- row.names
    class(value) <- "data.frame"
    value
}

as.data.frame.array <- function(x, row.names = NULL, optional = FALSE, ...)
{
    d <- dim(x)
    if(length(d) == 1L) { ## same as as.data.frame.vector, but deparsed here
        value <- as.data.frame.vector(drop(x), row.names, optional, ...)
        if(!optional) names(value) <- deparse(substitute(x))[[1L]]
        value
    } else if (length(d) == 2L) {
        as.data.frame.matrix(x, row.names, optional, ...)
    } else {
        dn <- dimnames(x)
        dim(x) <- c(d[1L], prod(d[-1L]))
        if(!is.null(dn)) {
            if(length(dn[[1L]])) rownames(x) <- dn[[1L]]
            for(i in 2L:length(d))
                if(is.null(dn[[i]])) dn[[i]] <- seq_len(d[i])
            colnames(x) <- interaction(expand.grid(dn[-1L]))
        }
        as.data.frame.matrix(x, row.names, optional, ...)
    }
}

## will always have a class here
"[.AsIs" <- function(x, i, ...) structure(NextMethod("["), class = class(x))

as.data.frame.AsIs <- function(x, row.names = NULL, optional = FALSE, ...)
{
    ## why not remove class and NextMethod here?
    if(length(dim(x)) == 2L)
	as.data.frame.model.matrix(x, row.names, optional)
    else { # as.data.frame.vector without removing names
        nrows <- length(x)
        nm <- paste(deparse(substitute(x), width.cutoff=500L), collapse=" ")
        if(is.null(row.names)) {
            if (nrows == 0L)
                row.names <- character(0L)
            else if(length(row.names <- names(x)) == nrows &&
                    !anyDuplicated(row.names)) {}
            else row.names <- .set_row_names(nrows)
        }
        value <- list(x)
        if(!optional) names(value) <- nm
        attr(value, "row.names") <- row.names
        class(value) <- "data.frame"
        value
    }

}

###  This is the real "data.frame".
###  It does everything by calling the methods presented above.

data.frame <-
    function(..., row.names = NULL, check.rows = FALSE, check.names = TRUE,
             stringsAsFactors = default.stringsAsFactors())
{
    data.row.names <-
	if(check.rows && is.null(row.names))
	    function(current, new, i) {
		if(is.character(current)) new <- as.character(new)
		if(is.character(new)) current <- as.character(current)
		if(anyDuplicated(new))
		    return(current)
		if(is.null(current))
		    return(new)
		if(all(current == new) || all(current == ""))
		    return(new)
		stop(gettextf("mismatch of row names in arguments of 'data.frame\', item %d", i), domain = NA)
	    }
	else function(current, new, i) {
	    if(is.null(current)) {
		if(anyDuplicated(new)) {
		    warning("some row.names duplicated: ",
                            paste(which(duplicated(new)), collapse=","),
                            " --> row.names NOT used")
		    current
		} else new
	    } else current
	}
    object <- as.list(substitute(list(...)))[-1L]
    mrn <- is.null(row.names) # missing or NULL
    x <- list(...)
    n <- length(x)
    if(n < 1L) {
        if(!mrn) {
            if(is.object(row.names) || !is.integer(row.names))
                row.names <- as.character(row.names)
            if(any(is.na(row.names)))
                stop("row names contain missing values")
            if(anyDuplicated(row.names))
                stop("duplicate row.names: ",
                     paste(unique(row.names[duplicated(row.names)]),
                           collapse = ", "))
        } else row.names <- integer(0L)
	return(structure(list(), names = character(0L),
                         row.names = row.names,
			 class = "data.frame"))
    }
    vnames <- names(x)
    if(length(vnames) != n)
	vnames <- character(n)
    no.vn <- !nzchar(vnames)
    vlist <- vnames <- as.list(vnames)
    nrows <- ncols <- integer(n)
    for(i in seq_len(n)) {
        ## do it this way until all as.data.frame methods have been updated
	xi <- if(is.character(x[[i]]) || is.list(x[[i]]))
                 as.data.frame(x[[i]], optional = TRUE,
                               stringsAsFactors = stringsAsFactors)
        else as.data.frame(x[[i]], optional = TRUE)

        nrows[i] <- .row_names_info(xi) # signed for now
	ncols[i] <- length(xi)
	namesi <- names(xi)
	if(ncols[i] > 1L) {
	    if(length(namesi) == 0L) namesi <- seq_len(ncols[i])
	    if(no.vn[i]) vnames[[i]] <- namesi
	    else vnames[[i]] <- paste(vnames[[i]], namesi, sep=".")
	}
	else {
            if(length(namesi)) vnames[[i]] <- namesi
            else if (no.vn[[i]]) {
                tmpname <- deparse(object[[i]])[1L]
                if( substr(tmpname, 1L, 2L) == "I(" ) {
                    ntmpn <- nchar(tmpname, "c")
                    if(substr(tmpname, ntmpn, ntmpn) == ")")
                        tmpname <- substr(tmpname, 3L, ntmpn - 1L)
                }
                vnames[[i]] <- tmpname
            }
        } # end of ncols[i] <= 1
	if(missing(row.names) && nrows[i] > 0L) {
            rowsi <- attr(xi, "row.names")
            ## Avoid all-blank names
            nc <- nchar(rowsi, allowNA = FALSE)
            nc <- nc[!is.na(nc)]
            if(length(nc) && any(nc))
                row.names <- data.row.names(row.names, rowsi, i)
        }
        nrows[i] <- abs(nrows[i])
	vlist[[i]] <- xi
    }
    nr <- max(nrows)
    for(i in seq_len(n)[nrows < nr]) {
	xi <- vlist[[i]]
	if(nrows[i] > 0L && (nr %% nrows[i] == 0L)) {
            ## make some attempt to recycle column i
            xi <- unclass(xi) # avoid data-frame methods
            fixed <- TRUE
            for(j in seq_along(xi)) {
                xi1 <- xi[[j]]
                if(is.vector(xi1) || is.factor(xi1))
                    xi[[j]] <- rep(xi1, length.out = nr)
                else if(is.character(xi1) && class(xi1) == "AsIs")
                    xi[[j]] <- structure(rep(xi1, length.out = nr),
                                         class = class(xi1))
                else if(inherits(xi1, "Date") || inherits(xi1, "POSIXct"))
                    xi[[j]] <-rep(xi1, length.out = nr)
                else {
                    fixed <- FALSE
                    break
                }
            }
            if (fixed) {
                vlist[[i]] <- xi
                next
            }
        }
	stop("arguments imply differing number of rows: ",
             paste(unique(nrows), collapse = ", "))
    }
    value <- unlist(vlist, recursive=FALSE, use.names=FALSE)
    ## unlist() drops i-th component if it has 0 columns
    vnames <- unlist(vnames[ncols > 0L])
    noname <- !nzchar(vnames)
    if(any(noname))
	vnames[noname] <- paste("Var", seq_along(vnames), sep = ".")[noname]
    if(check.names)
	vnames <- make.names(vnames, unique=TRUE)
    names(value) <- vnames
    if(!mrn) { # non-null row.names arg was supplied
        if(length(row.names) == 1L && nr != 1L) {  # one of the variables
            if(is.character(row.names))
                row.names <- match(row.names, vnames, 0L)
            if(length(row.names) != 1L ||
               row.names < 1L || row.names > length(vnames))
                stop("row.names should specify one of the variables")
            i <- row.names
            row.names <- value[[i]]
            value <- value[ - i]
        } else if ( !is.null(row.names) && length(row.names) != nr )
            stop("row names supplied are of the wrong length")
    } else if( !is.null(row.names) && length(row.names) != nr ) {
        warning("row names were found from a short variable and have been discarded")
        row.names <- NULL
    }
    if(is.null(row.names))
        row.names <- .set_row_names(nr) #seq_len(nr)
    else {
        if(is.object(row.names) || !is.integer(row.names))
            row.names <- as.character(row.names)
        if(any(is.na(row.names)))
            stop("row names contain missing values")
        if(anyDuplicated(row.names))
            stop("duplicate row.names: ",
                 paste(unique(row.names[duplicated(row.names)]),
                       collapse = ", "))
    }
    attr(value, "row.names") <- row.names
    attr(value, "class") <- "data.frame"
    value
}


###  Subsetting and mutation methods
###  These are a little less general than S

"[.data.frame" <-
    function(x, i, j, drop = if(missing(i)) TRUE else length(cols) == 1)
{
    mdrop <- missing(drop)
    Narg <- nargs() - !mdrop  # number of arg from x,i,j that were specified
    has.j <- !missing(j)
    if(!all(names(sys.call()) %in% c("", "drop")))
        warning("named arguments other than 'drop' are discouraged")

    if(Narg < 3L) {  # list-like indexing or matrix indexing
        if(!mdrop) warning("drop argument will be ignored")
	if(missing(i)) return(x)
	if(is.matrix(i))
	    return(as.matrix(x)[i])  # desperate measures
        ## zero-column data frames prior to 2.4.0 had no names.
        nm <- names(x); if(is.null(nm)) nm <- character(0L)
        ## if we have NA names, character indexing should always fail
        ## (for positive index length)
        if(!is.character(i) && any(is.na(nm))) { # less efficient version
            names(nm) <- names(x) <- seq_along(x)
            y <- NextMethod("[")
            cols <- names(y)
            if(any(is.na(cols))) stop("undefined columns selected")
            cols <- names(y) <- nm[cols]
        } else {
            y <- NextMethod("[")
            cols <- names(y)
            if(!is.null(cols) && any(is.na(cols)))
                stop("undefined columns selected")
        }
        ## added in 1.8.0
        if(anyDuplicated(cols)) names(y) <- make.unique(cols)
        ## since we have not touched the rows, copy over the raw row.names
	return(structure(y, class = oldClass(x),
                         row.names = .row_names_info(x, 0L)))
    }

    if(missing(i)) { # df[, j] or df[ , ]
        ## not quite the same as the 1/2-arg case, as 'drop' is used.
        if(missing(j) && drop && length(x) == 1L) return(.subset2(x, 1L))
        nm <- names(x); if(is.null(nm)) nm <- character(0L)
        if(!missing(j) && !is.character(j) && any(is.na(nm))) {
            ## less efficient version
            names(nm) <- names(x) <- seq_along(x)
            y <- if(missing(j)) x else .subset(x, j)
            cols <- names(y)
            if(any(is.na(cols))) stop("undefined columns selected")
            cols <- names(y) <- nm[cols]
        } else {
            y <- if(missing(j)) x else .subset(x, j)
            cols <- names(y)
            if(any(is.na(cols))) stop("undefined columns selected")
        }
        if(drop && length(y) == 1L) return(.subset2(y, 1L))
        if(anyDuplicated(cols)) names(y) <- make.unique(cols)
        nrow <- .row_names_info(x, 2L)
        if(drop && !mdrop && nrow == 1L)
            return(structure(y, class = NULL, row.names = NULL))
        else
            return(structure(y, class = oldClass(x),
                             row.names = .row_names_info(x, 0L)))
    }

    ### df[i, j] or df[i , ]
    ## rewritten for R 2.5.0 to avoid duplicating x.
    xx <- x
    cols <- names(xx)  # needed for computation of 'drop' arg
    ## make a shallow copy
    x <- vector("list", length(x))
    ## attributes(x) <- attributes(xx) expands row names
    x <- .Call("R_copyDFattr", xx, x, PACKAGE="base")
    oldClass(x) <- attr(x, "row.names") <- NULL

    if(!missing(j)) { # df[i, j]
        nm <- names(x); if(is.null(nm)) nm <- character(0L)
        if(!is.character(j) && any(is.na(nm)))
            names(nm) <- names(x) <- seq_along(x)
        x <- x[j]
        cols <- names(x)  # needed for 'drop'
        if(drop && length(x) == 1L) {
            ## for consistency with [, <length-1>]
            if(is.character(i)) {
                rows <- attr(xx, "row.names")
                i <- pmatch(i, rows, duplicates.ok = TRUE)
            }
            ## need to figure which col was selected:
            ## cannot use .subset2 directly as that may
            ## use recursive selection for a logical index.
            xj <- .subset2(.subset(xx, j), 1L)
            return(if(length(dim(xj)) != 2L) xj[i] else xj[i, , drop = FALSE])
        }
        if(any(is.na(cols))) stop("undefined columns selected")
        ## fix up names if we altered them.
        if(!is.null(names(nm))) cols <- names(x) <- nm[cols]
        ## sxx <- match(cols, names(xx)) fails with duplicate names
        nxx <- structure(seq_along(xx), names=names(xx))
        sxx <- match(nxx[j], seq_along(xx))
    } else sxx <- seq_along(x)

    rows <- NULL # placeholder: only create row names when needed
                 # as this can be expensive.
    if(is.character(i)) {
        rows <- attr(xx, "row.names")
        i <- pmatch(i, rows, duplicates.ok = TRUE)
    }
    for(j in seq_along(x)) {
        xj <- xx[[ sxx[j] ]]
        ## had drop = drop prior to 1.8.0
        x[[j]] <- if(length(dim(xj)) != 2L) xj[i] else xj[i, , drop = FALSE]
    }

    if(drop) {
	n <- length(x)
	if(n == 1L) return(x[[1L]]) # drops attributes
	if(n > 1L) {
	    xj <- x[[1L]]
	    nrow <- if(length(dim(xj)) == 2L) dim(xj)[1L] else length(xj)
            ## for consistency with S: don't drop (to a list)
            ## if only one row, unless explicitly asked for
            drop <- !mdrop && nrow == 1L
	} else drop <- FALSE ## for n == 0
    }

    if(!drop) { # not else as previous section might reset drop
        ## row names might have NAs.
        if(is.null(rows)) rows <- attr(xx, "row.names")
        rows <- rows[i]
	if((ina <- any(is.na(rows))) | (dup <- anyDuplicated(rows))) {
	    ## both will coerce integer 'rows' to character:
	    if (!dup && is.character(rows)) dup <- "NA" %in% rows
	    if(ina)
		rows[is.na(rows)] <- "NA"
	    if(dup)
		rows <- make.unique(as.character(rows))
	}
        ## new in 1.8.0  -- might have duplicate columns
	if(has.j && anyDuplicated(nm <- names(x)))
            names(x) <- make.unique(nm)
        if(is.null(rows)) rows <- attr(xx, "row.names")[i]
	attr(x, "row.names") <- rows
	oldClass(x) <- oldClass(xx)
    }
    x
}

"[[.data.frame" <- function(x, ..., exact=TRUE)
{
    ## use in-line functions to refer to the 1st and 2nd ... arguments
    ## explicitly. Also will check for wrong number or empty args
    na <- nargs() - !missing(exact)
    if(!all(names(sys.call()) %in% c("", "exact")))
        warning("named arguments other than 'exact' are discouraged")

    if(na < 3L)
	(function(x, i, exact)
	  if(is.matrix(i)) as.matrix(x)[[i]]
 	  else .subset2(x, i, exact=exact))(x, ..., exact=exact)
    else {
        col <- .subset2(x, ..2, exact=exact)
        i <- if(is.character(..1))
            pmatch(..1, row.names(x), duplicates.ok = TRUE)
        else ..1
        .subset2(col, i, exact=exact)
    }
}

"[<-.data.frame" <- function(x, i, j, value)
{
    if(!all(names(sys.call()) %in% c("", "value")))
        warning("named arguments are discouraged")

    nA <- nargs() # 'value' is never missing, so 3 or 4.
    if(nA == 4L) { ## df[,] or df[i,] or df[, j] or df[i,j]
	has.i <- !missing(i)
	has.j <- !missing(j)
    }
    else if(nA == 3L) {
        ## this collects both df[] and df[ind]
        if(is.atomic(value)) names(value) <- NULL
        if(missing(i) && missing(j)) { # case df[]
            i <- j <- NULL
            has.i <- has.j <- FALSE
            ## added in 1.8.0
            if(is.null(value)) return(x[logical(0L)])
        } else { # case df[ind]
            ## really ambiguous, but follow common use as if list
            ## except for a full-sized logical matrix
            if(is.logical(i) && is.matrix(i) && all(dim(i) == dim(x))) {
                nreplace <- sum(i, na.rm=TRUE)
                if(!nreplace) return(x) # nothing to replace
                ## allow replication of length(value) > 1 in 1.8.0
                N <- length(value)
                if(N > 1L && N < nreplace && (nreplace %% N) == 0L)
                    value <- rep(value, length.out = nreplace)
                if(N > 1L && (length(value) != nreplace))
                    stop("rhs is the wrong length for indexing by a logical matrix")
                n <- 0L
                nv <- nrow(x)
                for(v in seq_len(dim(i)[2L])) {
                    thisvar <- i[, v, drop = TRUE]
                    nv <- sum(thisvar, na.rm = TRUE)
                    if(nv) {
                        if(is.matrix(x[[v]]))
                            x[[v]][thisvar, ] <- if(N > 1L) value[n+seq_len(nv)] else value
                        else
                            x[[v]][thisvar] <- if(N > 1L) value[n+seq_len(nv)] else value
                    }
                    n <- n+nv
                }
                return(x)
            }  # end of logical matrix
            if(is.matrix(i))
                stop("only logical matrix subscripts are allowed in replacement")
            j <- i
            i <- NULL
            has.i <- FALSE
            has.j <- TRUE
        }
    }
    else {
	stop("need 0, 1, or 2 subscripts")
    }
    ## no columns specified
    if(has.j && length(j) == 0L) return(x)

    cl <- oldClass(x)
    ## delete class: S3 idiom to avoid any special methods for [[, etc
    class(x) <- NULL
    new.cols <- NULL
    nvars <- length(x)
    nrows <- .row_names_info(x, 2L)
    if(has.i) { # df[i, ] or df[i, j]
        rows <- NULL  # indicator that it is not yet set
        if(any(is.na(i)))
            stop("missing values are not allowed in subscripted assignments of data frames")
	if(char.i <- is.character(i)) {
            rows <- attr(x, "row.names")
	    ii <- match(i, rows)
	    nextra <- sum(new.rows <- is.na(ii))
	    if(nextra > 0L) {
		ii[new.rows] <- seq.int(from = nrows + 1L, length.out = nextra)
		new.rows <- i[new.rows]
	    }
	    i <- ii
	}
	if(all(i >= 0L) && (nn <- max(i)) > nrows) {
	    ## expand
            if(is.null(rows)) rows <- attr(x, "row.names")
	    if(!char.i) {
		nrr <- (nrows + 1L):nn
		if(inherits(value, "data.frame") &&
		   (dim(value)[1L]) >= length(nrr)) {
		    new.rows <- attr(value, "row.names")[seq_along(nrr)]
		    repl <- duplicated(new.rows) | match(new.rows, rows, 0L)
		    if(any(repl)) new.rows[repl] <- nrr[repl]
		}
		else new.rows <- nrr
	    }
	    x <- xpdrows.data.frame(x, rows, new.rows)
	    rows <- attr(x, "row.names")
	    nrows <- length(rows)
	}
	iseq <- seq_len(nrows)[i]
	if(any(is.na(iseq))) stop("non-existent rows not allowed")
    }
    else iseq <- NULL

    if(has.j) {
        if(any(is.na(j)))
            stop("missing values are not allowed in subscripted assignments of data frames")
	if(is.character(j)) {
            if("" %in% j) stop("column name \"\" cannot match any column")
	    jj <- match(j, names(x))
	    nnew <- sum(is.na(jj))
	    if(nnew > 0L) {
		n <- is.na(jj)
		jj[n] <- nvars + seq_len(nnew)
		new.cols <- j[n]
	    }
	    jseq <- jj
	}
	else if(is.logical(j) || min(j) < 0L)
	    jseq <- seq_along(x)[j]
	else {
	    jseq <- j
	    if(max(jseq) > nvars) {
		new.cols <- paste("V", seq.int(from = nvars + 1L, to = max(jseq)),
                                  sep = "")
		if(length(new.cols)  != sum(jseq > nvars))
		    stop("new columns would leave holes after existing columns")
                ## try to use the names of a list `value'
                if(is.list(value) && !is.null(vnm <- names(value))) {
                    p <- length(jseq)
                    if(length(vnm) < p) vnm <- rep(vnm, length.out = p)
                    new.cols <- vnm[jseq > nvars]
                }
	    }
	}
    }
    else jseq <- seq_along(x)

    ## addition in 1.8.0
    if(anyDuplicated(jseq))
        stop("duplicate subscripts for columns")
    n <- length(iseq)
    if(n == 0L) n <- nrows
    p <- length(jseq)
    m <- length(value)
    if(!is.list(value)) {
        if(p == 1L) {
            N <- NROW(value)
            if(N > n)
                stop(gettextf("replacement has %d rows, data has %d", N, n),
                     domain = NA)
            if(N < n && N > 0L)
                if(n %% N == 0L && length(dim(value)) <= 1L)
                    value <- rep(value, length.out = n)
                else
                    stop(gettextf("replacement has %d rows, data has %d", N, n),
                         domain = NA)
            names(value) <- NULL
            value <- list(value)
         } else {
            if(m < n*p && (m == 0L || (n*p) %% m))
                stop(gettextf("replacement has %d items, need %d", m, n*p),
                     domain = NA)
            value <- matrix(value, n, p)  ## will recycle
            value <- split(value, col(value))
        }
	dimv <- c(n, p)
    } else { # a list
        ## careful, as.data.frame turns things into factors.
	## value <- as.data.frame(value)
        value <- unclass(value) # to avoid data frame indexing
        lens <- sapply(value, NROW)
        for(k in seq_along(lens)) {
            N <- lens[k]
            if(n != N && length(dim(value[[k]])) == 2L)
                stop(gettextf("replacement element %d is a matrix/data frame of %d rows, need %d", k, N, n),
                     domain = NA)
            if(N > 0L && N < n && n %% N)
                stop(gettextf("replacement element %d has %d rows, need %d",
                              k, N, n), domain = NA)
            ## these fixing-ups will not work for matrices
            if(N > 0L && N < n) value[[k]] <- rep(value[[k]], length.out = n)
            if(N > n) {
                warning(gettextf("replacement element %d has %d rows to replace %d rows",
                                 k, N, n), domain = NA)
                value[[k]] <- value[[k]][seq_len(n)]
            }
        }
	dimv <- c(n, length(value))
    }
    nrowv <- dimv[1L]
    if(nrowv < n && nrowv > 0L) {
	if(n %% nrowv == 0L)
	    value <- value[rep(seq_len(nrowv), length.out = n),,drop = FALSE]
	else stop(gettextf("%d rows in value to replace %d rows", nrowv, n),
                  domain = NA)
    }
    else if(nrowv > n)
	warning(gettextf("replacement data has %d rows to replace %d rows",
                         nrowv, n), domain = NA)
    ncolv <- dimv[2L]
    jvseq <- seq_len(p)
    if(ncolv < p) jvseq <- rep(seq_len(ncolv), length.out = p)
    else if(ncolv > p) {
	warning(gettextf("provided %d variables to replace %d variables",
                         ncolv, p), domain = NA)
        new.cols <- new.cols[seq_len(p)]
    }
    if(length(new.cols)) {
        ## extend and name now, as assignment of NULL may delete cols later.
        nm <- names(x)
        rows <- .row_names_info(x, 0L)
        a <- attributes(x); a["names"] <- NULL
        x <- c(x, vector("list", length(new.cols)))
        attributes(x) <- a
        names(x) <- c(nm, new.cols)
        attr(x, "row.names") <- rows
    }
    if(has.i)
	for(jjj in seq_len(p)) {
	    jj <- jseq[jjj]
	    vjj <- value[[ jvseq[[jjj]] ]]
            if(jj <= nvars) {
                ## if a column exists, preserve its attributes
                if(length(dim(x[[jj]])) != 2L) x[[jj]][iseq] <- vjj
                else x[[jj]][iseq, ] <- vjj
            } else {
                ## try to make a new column match in length: may be an error
                x[[jj]] <- vjj[FALSE]
                if(length(dim(vjj)) == 2L) {
                    length(x[[j]]) <- nrows * ncol(vjj)
                    dim(x[[j]]) <- c(nrows, ncol(vjj))
                    x[[jj]][iseq, ] <- vjj
                } else {
                    length(x[[j]]) <- nrows
                    x[[jj]][iseq] <- vjj
                }
            }
	}
    else if(p > 0L) for(jjj in p:1L) { # we might delete columns with NULL
	jj <- jseq[jjj]
        v <- value[[ jvseq[[jjj]] ]]
        ## This is consistent with the have.i case rather than with
        ## [[<- and $<- (which throw an error).  But both are plausible.
        if (nrows > 0L && !length(v)) length(v) <- nrows
	x[[jj]] <- v
        if(!is.null(v) && is.atomic(x[[jj]])) names(x[[jj]]) <- NULL
    }
    if(length(new.cols) > 0L) {
        new.cols <- names(x) # we might delete columns with NULL
        ## added in 1.8.0
        if(anyDuplicated(new.cols)) names(x) <- make.unique(new.cols)
    }
    class(x) <- cl
    x
}

"[[<-.data.frame"<- function(x, i, j, value)
{
    if(!all(names(sys.call()) %in% c("", "value")))
        warning("named arguments are discouraged")

    cl <- oldClass(x)
    ## delete class: Version 3 idiom
    ## to avoid any special methods for [[<-
    class(x) <- NULL
    nrows <- .row_names_info(x, 2L)
    if(is.atomic(value)) names(value) <- NULL
    if(nargs() < 4L) {
	## really ambiguous, but follow common use as if list
        nc <- length(x)
	if(!is.null(value)) {
            N <- NROW(value)
            if(N > nrows)
                stop(gettextf("replacement has %d rows, data has %d", N, nrows),
                     domain = NA)
            if(N < nrows)
                if(N > 0L && (nrows %% N == 0L) && length(dim(value)) <= 1L)
                    value <- rep(value, length.out = nrows)
                else
                    stop(gettextf("replacement has %d rows, data has %d",
                                  N, nrows), domain = NA)
	}
	x[[i]] <- value
        ## added in 1.8.0 -- make sure there is a name
        if(length(x) > nc) {
            nc <- length(x)
            if(names(x)[nc] == "") names(x)[nc] <- paste("V", nc, sep="")
            names(x) <- make.unique(names(x))
        }
	class(x) <- cl
	return(x)
    }
    if(missing(i) || missing(j))
	stop("only valid calls are x[[j]] <- value or x[[i,j]] <- value")
    rows <- attr(x, "row.names")
    nvars <- length(x)
    if(n <- is.character(i)) {
	ii <- match(i, rows)
	n <- sum(new.rows <- is.na(ii))
	if(n > 0L) {
	    ii[new.rows] <- seq.int(from = nrows + 1L, length.out = n)
	    new.rows <- i[new.rows]
	}
	i <- ii
    }
    if(all(i >= 0L) && (nn <- max(i)) > nrows) {
	## expand
	if(n == 0L) {
	    nrr <- (nrows + 1L):nn
	    if(inherits(value, "data.frame") &&
	       (dim(value)[1L]) >= length(nrr)) {
		new.rows <- attr(value, "row.names")[seq_len(nrr)]
		repl <- duplicated(new.rows) | match(new.rows, rows, 0L)
		if(any(repl)) new.rows[repl] <- nrr[repl]
	    }
	    else new.rows <- nrr
	}
	x <- xpdrows.data.frame(x, rows, new.rows)
	rows <- attr(x, "row.names")
	nrows <- length(rows)
    }

    ## FIXME: this is wasteful and probably unnecessary
    iseq <- seq_len(nrows)[i]
    if(any(is.na(iseq)))
	stop("non-existent rows not allowed")

    if(is.character(j)) {
        if("" %in% j) stop("column name \"\" cannot match any column")
	jseq <- match(j, names(x))
	if(any(is.na(jseq)))
	    stop("replacing element in non-existent column: ", j[is.na(jseq)])
    }
    else if(is.logical(j) || min(j) < 0L)
	jseq <- seq_along(x)[j]
    else {
	jseq <- j
	if(max(jseq) > nvars)
	    stop("replacing element in non-existent column: ",
                 jseq[jseq>nvars])
    }
    if(length(iseq) > 1L || length(jseq) > 1L)
	stop("only a single element should be replaced")
    x[[jseq]][[iseq]] <- value
    class(x) <- cl
    x
}

## added in 1.8.0
"$<-.data.frame"<- function(x, name, value)
{
    cl <- oldClass(x)
    ## delete class: Version 3 idiom
    ## to avoid any special methods for [[<-
    ## This forces a copy, but we are going to need one anyway
    ## and NAMED=1 prevents any further copying.
    class(x) <- NULL
    nrows <- .row_names_info(x, 2L)
    if(!is.null(value)) {
        N <- NROW(value)
        if(N > nrows)
            stop(gettextf("replacement has %d rows, data has %d", N, nrows),
                 domain = NA)
        if (N < nrows)
            if (N > 0L && (nrows %% N == 0L) && length(dim(value)) <= 1L)
                value <- rep(value, length.out = nrows)
            else
                stop(gettextf("replacement has %d rows, data has %d", N, nrows),
                     domain = NA)
        if(is.atomic(value)) names(value) <- NULL
    }
    x[[name]] <- value
    class(x) <- cl
    return(x)
}

xpdrows.data.frame <- function(x, old.rows, new.rows)
{
    nc <- length(x)
    nro <- length(old.rows)
    nrn <- length(new.rows)
    nr <- nro + nrn
    for (i in seq_len(nc)) {
	y <- x[[i]]
	dy <- dim(y)
	cy <- oldClass(y)
	class(y) <- NULL
	if (length(dy) == 2L) {
	    dny <- dimnames(y)
	    if (length(dny[[1L]]) > 0L)
		dny[[1L]] <- c(dny[[1L]], new.rows)
	    z <- array(y[1L], dim = c(nr, nc), dimnames = dny)
	    z[seq_len(nro), ] <- y
	    class(z) <- cy
	    x[[i]] <- z
	}
	else {
	    ay <- attributes(y)
	    if (length(names(y)) > 0L)
		ay$names <- c(ay$names, new.rows)
	    length(y) <- nr
	    attributes(y) <- ay
	    class(y) <- cy
	    x[[i]] <- y
	}
    }
    attr(x, "row.names") <- c(old.rows, new.rows)
    x
}


### Here are the methods for rbind and cbind.

cbind.data.frame <- function(..., deparse.level = 1)
    data.frame(..., check.names = FALSE)

rbind.data.frame <- function(..., deparse.level = 1)
{
    match.names <- function(clabs, nmi)
    {
	if(identical(clabs, nmi)) NULL
	else if(length(nmi) == length(clabs) && all(nmi %in% clabs)) {
            ## we need 1-1 matches here
	    m <- pmatch(nmi, clabs, 0L)
            if(any(m == 0L))
                stop("names do not match previous names")
            m
	} else stop("names do not match previous names")
    }
    Make.row.names <- function(nmi, ri, ni, nrow)
    {
	if(nzchar(nmi)) {
            if(ni == 0L) character(0L)  # PR8506
	    else if(ni > 1L) paste(nmi, ri, sep = ".")
	    else nmi
	}
	else if(nrow > 0L && identical(ri, seq_len(ni)))
	    as.integer(seq.int(from = nrow + 1L, length.out = ni))
	else ri
    }
    allargs <- list(...)
    allargs <- allargs[sapply(allargs, length) > 0L]
    if(length(allargs)) {
    ## drop any zero-row data frames, as they may not have proper column
    ## types (e.g. NULL).
        nr <- sapply(allargs, function(x)
                     if(is.data.frame(x)) .row_names_info(x, 2L)
                     else if(is.list(x)) length(x[[1L]]) # mismatched lists are checked later
                     else length(x))
        if(any(nr > 0L)) allargs <- allargs[nr > 0L]
        else return(allargs[[1L]]) # pretty arbitrary
    }
    n <- length(allargs)
    if(n == 0L)
	return(structure(list(),
			 class = "data.frame",
			 row.names = integer()))
    nms <- names(allargs)
    if(is.null(nms))
	nms <- character(length(allargs))
    cl <- NULL
    perm <- rows <- rlabs <- vector("list", n)
    nrow <- 0L
    value <- clabs <- NULL
    all.levs <- list()
    for(i in seq_len(n)) {
	## check the arguments, develop row and column labels
	xi <- allargs[[i]]
	nmi <- nms[i]
        ## coerce matrix to data frame
        if(is.matrix(xi)) allargs[[i]] <- xi <- as.data.frame(xi)
	if(inherits(xi, "data.frame")) {
	    if(is.null(cl))
		cl <- oldClass(xi)
	    ri <- attr(xi, "row.names")
	    ni <- length(ri)
	    if(is.null(clabs))
		clabs <- names(xi)
	    else {
                if(length(xi) != length(clabs))
                    stop("numbers of columns of arguments do not match")
		pi <- match.names(clabs, names(xi))
		if( !is.null(pi) ) perm[[i]] <- pi
	    }
	    rows[[i]] <- seq.int(from = nrow + 1L, length.out = ni)
	    rlabs[[i]] <- Make.row.names(nmi, ri, ni, nrow)
	    nrow <- nrow + ni
	    if(is.null(value)) {
		value <- unclass(xi)
		nvar <- length(value)
		all.levs <- vector("list", nvar)
		has.dim <- logical(nvar)
                facCol <- logical(nvar)
                ordCol <- logical(nvar)
		for(j in seq_len(nvar)) {
		    xj <- value[[j]]
		    if( !is.null(levels(xj)) ) {
			all.levs[[j]] <- levels(xj)
                        facCol[j] <- TRUE # turn categories into factors
                    } else facCol[j] <- is.factor(xj)
                    ordCol[j] <- is.ordered(xj)
		    has.dim[j] <- length(dim(xj)) == 2L
		}
	    }
	    else for(j in seq_len(nvar)) {
                xij <- xi[[j]]
                if(is.null(pi) || is.na(jj <- pi[[j]])) jj <- j
                if(facCol[jj]) {
                    if(length(lij <- levels(xij))) {
                        all.levs[[jj]] <- unique(c(all.levs[[jj]], lij))
                        ordCol[jj] <- ordCol[jj] & is.ordered(xij)
                    } else if(is.character(xij))
                        all.levs[[jj]] <- unique(c(all.levs[[jj]], xij))
                }
            }
	}
	else if(is.list(xi)) {
	    ni <- range(sapply(xi, length))
	    if(ni[1L] == ni[2L])
		ni <- ni[1L]
	    else stop("invalid list argument: all variables should have the same length")
	    rows[[i]] <- ri <-
                as.integer(seq.int(from = nrow + 1L, length.out = ni))
	    nrow <- nrow + ni
	    rlabs[[i]] <- Make.row.names(nmi, ri, ni, nrow)
	    if(length(nmi <- names(xi)) > 0L) {
		if(is.null(clabs))
		    clabs <- nmi
		else {
                    if(length(xi) != length(clabs))
                        stop("numbers of columns of arguments do not match")
		    pi <- match.names(clabs, nmi)
		    if( !is.null(pi) ) perm[[i]] <- pi
		}
	    }
	}
	else if(length(xi)) {
	    rows[[i]] <- nrow <- nrow + 1L
	    rlabs[[i]] <- if(nzchar(nmi)) nmi else as.integer(nrow)
	}
    }
    nvar <- length(clabs)
    if(nvar == 0L)
	nvar <- max(sapply(allargs, length))	# only vector args
    if(nvar == 0L)
	return(structure(list(), class = "data.frame",
			 row.names = integer()))
    pseq <- seq_len(nvar)
    if(is.null(value)) { # this happens if there has been no data frame
	value <- list()
	value[pseq] <- list(logical(nrow)) # OK for coercion except to raw.
        all.levs <- vector("list", nvar)
        has.dim <- logical(nvar)
        facCol <- logical(nvar)
        ordCol <- logical(nvar)
    }
    names(value) <- clabs
    for(j in pseq)
	if(length(lij <- all.levs[[j]]))
            value[[j]] <-
                factor(as.vector(value[[j]]), lij, ordered = ordCol[j])
    if(any(has.dim)) {
	rmax <- max(unlist(rows))
	for(i in pseq[has.dim])
	    if(!inherits(xi <- value[[i]], "data.frame")) {
		dn <- dimnames(xi)
		rn <- dn[[1L]]
		if(length(rn) > 0L) length(rn) <- rmax
		pi <- dim(xi)[2L]
		length(xi) <- rmax * pi
		value[[i]] <- array(xi, c(rmax, pi), list(rn, dn[[2L]]))
	    }
    }
    for(i in seq_len(n)) {
	xi <- unclass(allargs[[i]])
	if(!is.list(xi))
	    if(length(xi) != nvar)
		xi <- rep(xi, length.out = nvar)
	ri <- rows[[i]]
	pi <- perm[[i]]
	if(is.null(pi)) pi <- pseq
	for(j in pseq) {
	    jj <- pi[j]
            xij <- xi[[j]]
	    if(has.dim[jj]) {
		value[[jj]][ri,	 ] <- xij
                ## copy rownames
                rownames(value[[jj]])[ri] <- rownames(xij)
	    } else {
                ## coerce factors to vectors, in case lhs is character or
                ## level set has changed
                value[[jj]][ri] <- if(is.factor(xij)) as.vector(xij) else xij
                ## copy names if any
                if(!is.null(nm <- names(xij))) names(value[[jj]])[ri] <- nm
            }
	}
    }
    rlabs <- unlist(rlabs)
    if(anyDuplicated(rlabs))
        rlabs <- make.unique(as.character(unlist(rlabs)), sep = "")
    if(is.null(cl)) {
	as.data.frame(value, row.names = rlabs)
    } else {
	class(value) <- cl
	attr(value, "row.names") <- rlabs
	value
    }
}


### coercion and print methods

print.data.frame <-
    function(x, ..., digits = NULL, quote = FALSE, right = TRUE,
	     row.names = TRUE)
{
    n <- length(row.names(x))
    if(length(x) == 0L) {
        cat(gettextf("data frame with 0 columns and %d rows\n", n))
    } else if(n == 0L) {
        ## FIXME: header format is inconsistent here
	print.default(names(x), quote = FALSE)
	cat(gettext("<0 rows> (or 0-length row.names)\n"))
    } else {
	## format.<*>() : avoiding picking up e.g. format.AsIs
	m <- as.matrix(format.data.frame(x, digits=digits, na.encode=FALSE))
	if(!isTRUE(row.names))
	    dimnames(m)[[1L]] <- if(identical(row.names,FALSE))
		rep.int("", n) else row.names
	print(m, ..., quote = quote, right = right)
    }
    invisible(x)
}

as.matrix.data.frame <- function (x, rownames.force = NA, ...)
{
    dm <- dim(x)
    rn <- if(rownames.force %in% FALSE) NULL
    else if(rownames.force %in% TRUE) row.names(x)
    else {if(.row_names_info(x) <= 0L) NULL else row.names(x)}
    dn <- list(rn, names(x))
    if(any(dm == 0L))
	return(array(NA, dim = dm, dimnames = dn))
    p <- dm[2L]
    pseq <- seq_len(p)
    n <- dm[1L]
    X <- x # will contain the result;
    ## the "big question" is if we return a numeric or a character matrix
    class(X) <- NULL
    non.numeric <- non.atomic <- FALSE
    all.logical <- TRUE
    for (j in pseq) {
        if(inherits(X[[j]], "data.frame") && ncol(xj) > 1L)
            X[[j]] <- as.matrix(X[[j]])
        xj <- X[[j]]
        j.logic <- is.logical(xj)
        if(all.logical && !j.logic) all.logical <- FALSE
	if(length(levels(xj)) > 0L || !(j.logic || is.numeric(xj) || is.complex(xj))
	   || (!is.null(cl <- attr(xj, "class")) && # numeric classed objects to format:
	       any(cl %in% c("Date", "POSIXct", "POSIXlt"))))
	    non.numeric <- TRUE
	if(!is.atomic(xj))
	    non.atomic <- TRUE
    }
    if(non.atomic) {
	for (j in pseq) {
	    xj <- X[[j]]
	    if(!is.recursive(xj))
		X[[j]] <- as.list(as.vector(xj))
	}
    } else if(all.logical) {
        ## do nothing for logical columns if a logical matrix will result.
    } else if(non.numeric) {
	for (j in pseq) {
	    if (is.character(X[[j]]))
		next
	    xj <- X[[j]]
            miss <- is.na(xj)
	    xj <- if(length(levels(xj))) as.vector(xj) else format(xj)
            is.na(xj) <- miss
            X[[j]] <- xj
	}
    }
    ## These coercions could have changed the number of columns
    ## (e.g. class "Surv" coerced to character),
    ## so only now can we compute collabs.
    collabs <- as.list(dn[[2L]])
    for (j in pseq) {
        xj <- X[[j]]
        dj <- dim(xj)
	if(length(dj) == 2L && dj[2L] > 1L) { # matrix with >=2 col
	    dnj <- colnames(xj)
	    collabs[[j]] <- paste(collabs[[j]],
				  if(length(dnj)) dnj else seq_len(dj[2L]),
				  sep = ".")
	}
     }
    X <- unlist(X, recursive = FALSE, use.names = FALSE)
    dim(X) <- c(n, length(X)/n)
    dimnames(X) <- list(dn[[1L]], unlist(collabs, use.names = FALSE))
    ##NO! don't copy buggy S-plus!  either all matrices have class or none!!
    ##NO class(X) <- "matrix"
    X
}

Math.data.frame <- function (x, ...)
{
    mode.ok <- sapply(x, function(x) is.numeric(x) || is.complex(x))
    if (all(mode.ok)) {
	x[] <- lapply(x, .Generic, ...)
	return(x)
    } else {
	vnames <- names(x)
	if (is.null(vnames)) vnames <- seq_along(x)
	stop("non-numeric variable in data frame: ", vnames[!mode.ok])
    }
}

Ops.data.frame <- function(e1, e2 = NULL)
{
    isList <- function(x) !is.null(x) && is.list(x)
    unary <- nargs() == 1L
    lclass <- nzchar(.Method[1L])
    rclass <- !unary && (nzchar(.Method[2L]))
    value <- list()
    rn <- NULL
    ## set up call as op(left, right)
    FUN <- get(.Generic, envir = parent.frame(), mode = "function")
    f <- if (unary) quote(FUN(left)) else quote(FUN(left, right))
    lscalar <- rscalar <- FALSE
    if(lclass && rclass) {
        nr <- .row_names_info(e1, 2L)
	if(.row_names_info(e1) > 0L) rn <- attr(e1, "row.names")
	cn <- names(e1)
	if(any(dim(e2) != dim(e1)))
	    stop(.Generic, " only defined for equally-sized data frames")
    } else if(lclass) {
	## e2 is not a data frame, but e1 is.
        nr <- .row_names_info(e1, 2L)
	if(.row_names_info(e1) > 0L) rn <- attr(e1, "row.names")
	cn <- names(e1)
	rscalar <- length(e2) <= 1L # e2 might be null
	if(isList(e2)) {
	    if(rscalar) e2 <- e2[[1L]]
	    else if(length(e2) != ncol(e1))
		stop(gettextf("list of length %d not meaningful", length(e2)),
                     domain = NA)
	} else {
	    if(!rscalar)
		e2 <- split(rep(as.vector(e2), length.out = prod(dim(e1))),
			    rep.int(seq_len(ncol(e1)),
                                    rep.int(nrow(e1), ncol(e1))))
	}
    } else {
	## e1 is not a data frame, but e2 is.
        nr <- .row_names_info(e2, 2L)
	if(.row_names_info(e2) > 0L) rn <- attr(e2, "row.names")
	cn <- names(e2)
	lscalar <- length(e1) <= 1L
	if(isList(e1)) {
	    if(lscalar) e1 <- e1[[1L]]
	    else if(length(e1) != ncol(e2))
		stop(gettextf("list of length %d not meaningful", length(e1)),
                     domain = NA)
	} else {
	    if(!lscalar)
		e1 <- split(rep(as.vector(e1), length.out = prod(dim(e2))),
			    rep.int(seq_len(ncol(e2)),
                                    rep.int(nrow(e2), ncol(e2))))
	}
    }
    for(j in seq_along(cn)) {
	left <- if(!lscalar) e1[[j]] else e1
	right <-if(!rscalar) e2[[j]] else e2
	value[[j]] <- eval(f)
    }
    if(.Generic %in% c("+","-","*","/","%%","%/%") ) {
	names(value) <- cn
	data.frame(value, row.names = rn, check.names = FALSE,
                   check.rows = FALSE)
    }
    else matrix(unlist(value, recursive = FALSE, use.names = FALSE),
		nrow = nr, dimnames = list(rn,cn))
}

Summary.data.frame <- function(..., na.rm)
{
    args <- list(...)
    args <- lapply(args, function(x) {
        x <- as.matrix(x)
        if(!is.numeric(x) && !is.complex(x))
            stop("only defined on a data frame with all numeric variables")
        x
    })
    do.call(.Generic, c(args, na.rm=na.rm))
}
#  File src/library/base/R/dates.R
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

## First shot at adding a "Date" class to base R.
## Representation is the number of whole days since 1970-01-01.

## The difftime class already covers time differences in days.

## Need to take timezone into account here
Sys.Date <- function() as.Date(as.POSIXlt(Sys.time()))

as.Date <- function(x, ...) UseMethod("as.Date")

as.Date.POSIXct <- function(x, ...) {
    z <- floor(unclass(x)/86400)
    attr(z, "tzone") <- NULL
    structure(z, class="Date")
}

as.Date.POSIXlt <- function(x, ...) .Internal(POSIXlt2Date(x))

as.Date.factor <- function(x, ...) as.Date(as.character(x), ...)


as.Date.character <- function(x, format="", ...)
{
    charToDate <- function(x) {
	xx <- x[1L]
        if(is.na(xx)) {
            j <- 1L
            while(is.na(xx) && (j <- j+1L) <= length(x)) xx <- x[j]
            if(is.na(xx)) f <- "%Y-%m-%d" # all NAs
        }
	if(is.na(xx) ||
	   !is.na(strptime(xx, f <- "%Y-%m-%d", tz="GMT")) ||
	   !is.na(strptime(xx, f <- "%Y/%m/%d", tz="GMT"))
           ) return(strptime(x, f))
	stop("character string is not in a standard unambiguous format")
    }
    res <- if(missing(format)) charToDate(x) else strptime(x, format, tz="GMT")
    as.Date(res)
}

as.Date.numeric <- function(x, origin, ...)
{
    if(missing(origin)) stop("'origin' must be supplied")
    as.Date(origin, ...) + x
}

as.Date.default <- function(x, ...)
{
    if(inherits(x, "Date")) return(x)
    if(is.logical(x) && all(is.na(x)))
        return(structure(as.numeric(x), class = "Date"))
    stop(gettextf("do not know how to convert '%s' to class \"Date\"",
                  deparse(substitute(x))))
}

## convert from package date
as.Date.date <- function(x, ...)
{
    if(inherits(x, "date")) {
        x <- (x - 3653) # origin 1960-01-01
        return(structure(x, class = "Date"))
    } else stop(gettextf("'%s' is not a \"date\" object",
                         deparse(substitute(x)) ))
}

## convert from package chron
as.Date.dates <- function(x, ...)
{
    if(inherits(x, "dates")) {
        z <- attr(x, "origin")
        x <- trunc(as.numeric(x))
        if(length(z) == 3L && is.numeric(z))
            x  <- x + as.numeric(as.Date(paste(z[3L], z[1L], z[2L], sep="/")))
        return(structure(x, class = "Date"))
    } else stop(gettextf("'%s' is not a \"dates\" object",
                         deparse(substitute(x)) ))
}

format.Date <- function(x, ...)
{
    xx <- format(as.POSIXlt(x), ...)
    names(xx) <- names(x)
    xx
}

print.Date <- function(x, ...)
{
    print(format(x), ...)
    invisible(x)
}

summary.Date <- function(object, digits = 12, ...)
{
    x <- summary.default(unclass(object), digits = digits, ...)[1L:6L]# not NA's
    class(x) <- oldClass(object)
    x
}

"+.Date" <- function(e1, e2)
{
    coerceTimeUnit <- function(x)
        as.vector(round(switch(attr(x,"units"),
                               secs = x/86400, mins = x/1440, hours = x/24,
                               days = x, weeks = 7*x)))

    if (nargs() == 1) return(e1)
    # only valid if one of e1 and e2 is a scalar.
    if(inherits(e1, "Date") && inherits(e2, "Date"))
        stop("binary + is not defined for Date objects")
    if (inherits(e1, "difftime")) e1 <- coerceTimeUnit(e1)
    if (inherits(e2, "difftime")) e2 <- coerceTimeUnit(e2)
    structure(unclass(e1) + unclass(e2), class = "Date")
}

"-.Date" <- function(e1, e2)
{
    coerceTimeUnit <- function(x)
        as.vector(round(switch(attr(x,"units"),
                               secs = x/86400, mins = x/1440, hours = x/24,
                               days = x, weeks = 7*x)))
    if(!inherits(e1, "Date"))
        stop("Can only subtract from Date objects")
    if (nargs() == 1) stop("unary - is not defined for Date objects")
    if(inherits(e2, "Date")) return(difftime(e1, e2, units="days"))
    if (inherits(e2, "difftime")) e2 <- coerceTimeUnit(e2)
    if(!is.null(attr(e2, "class")))
        stop("can only subtract numbers from Date objects")
    structure(unclass(as.Date(e1)) - e2, class = "Date")
}

Ops.Date <- function(e1, e2)
{
    if (nargs() == 1)
        stop("unary ", .Generic, " not defined for Date objects")
    boolean <- switch(.Generic, "<" =, ">" =, "==" =,
                      "!=" =, "<=" =, ">=" = TRUE,
                      FALSE)
    if (!boolean) stop(.Generic, " not defined for Date objects")
    ## allow character args to be coerced to dates
    if (is.character(e1)) e1 <- as.Date(e1)
    if (is.character(e2)) e2 <- as.Date(e2)
    NextMethod(.Generic)
}

Math.Date <- function (x, ...)
    stop(.Generic, " not defined for Date objects")

Summary.Date <- function (..., na.rm)
{
    ok <- switch(.Generic, max = , min = , range = TRUE, FALSE)
    if (!ok) stop(.Generic, " not defined for Date objects")
    val <- NextMethod(.Generic)
    class(val) <- oldClass(list(...)[[1L]])
    val
}

"[.Date" <- function(x, ..., drop = TRUE)
{
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[")
    class(val) <- cl
    val
}

"[[.Date" <- function(x, ..., drop = TRUE)
{
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[[")
    class(val) <- cl
    val
}

"[<-.Date" <- function(x, ..., value)
{
    if(!as.logical(length(value))) return(x)
    value <- as.Date(value)
    cl <- oldClass(x)
    class(x) <- class(value) <- NULL
    x <- NextMethod(.Generic)
    class(x) <- cl
    x
}

as.character.Date <- function(x, ...) format(x, ...)

as.data.frame.Date <- as.data.frame.vector

c.Date <- function(..., recursive=FALSE)
    structure(c(unlist(lapply(list(...), unclass))), class="Date")

mean.Date <- function (x, ...)
    structure(mean(unclass(x), ...), class = "Date")

seq.Date <- function(from, to, by, length.out=NULL, along.with=NULL, ...)
{
    if (missing(from)) stop("'from' must be specified")
    if (!inherits(from, "Date")) stop("'from' must be a Date object")
        if(length(as.Date(from)) != 1L) stop("'from' must be of length 1")
    if (!missing(to)) {
        if (!inherits(to, "Date")) stop("'to' must be a Date object")
        if (length(as.Date(to)) != 1L) stop("'to' must be of length 1")
    }
    if (!missing(along.with)) {
        length.out <- length(along.with)
    }  else if (!is.null(length.out)) {
        if (length(length.out) != 1L) stop("'length.out' must be of length 1")
        length.out <- ceiling(length.out)
    }
    status <- c(!missing(to), !missing(by), !is.null(length.out))
    if(sum(status) != 2)
        stop("exactly two of 'to', 'by' and 'length.out' / 'along.with' must be specified")
    if (missing(by)) {
        from <- unclass(as.Date(from))
        to <- unclass(as.Date(to))
        res <- seq.int(from, to, length.out = length.out)
        return(structure(res, class = "Date"))
    }

    if (length(by) != 1L) stop("'by' must be of length 1")
    valid <- 0L
    if (inherits(by, "difftime")) {
        by <- switch(attr(by,"units"), secs = 1/86400, mins = 1/1440,
                     hours = 1/24, days = 1, weeks = 7) * unclass(by)
    } else if(is.character(by)) {
        by2 <- strsplit(by, " ", fixed=TRUE)[[1L]]
        if(length(by2) > 2L || length(by2) < 1L)
            stop("invalid 'by' string")
        valid <- pmatch(by2[length(by2)],
                        c("days", "weeks", "months", "years"))
        if(is.na(valid)) stop("invalid string for 'by'")
        if(valid <= 2) {
            by <- c(1, 7)[valid]
            if (length(by2) == 2L) by <- by * as.integer(by2[1L])
        } else
            by <- if(length(by2) == 2L) as.integer(by2[1L]) else 1
    } else if(!is.numeric(by)) stop("invalid mode for 'by'")
    if(is.na(by)) stop("'by' is NA")

    if(valid <= 2L) {
        from <- unclass(as.Date(from))
        if(!is.null(length.out))
            res <- seq.int(from, by=by, length.out=length.out)
        else {
            to <- unclass(as.Date(to))
            ## defeat test in seq.default
            res <- seq.int(0, to - from, by) + from
        }
        return(structure(res, class="Date"))
    } else {  # months or years or DSTdays
        r1 <- as.POSIXlt(from)
        if(valid == 4L) {
            if(missing(to)) { # years
                yr <- seq.int(r1$year, by = by, length.out = length.out)
            } else {
                to <- as.POSIXlt(to)
                yr <- seq.int(r1$year, to$year, by)
            }
            r1$year <- yr
            res <- as.Date(r1)
        } else if(valid == 3L) { # months
            if(missing(to)) {
                mon <- seq.int(r1$mon, by = by, length.out = length.out)
            } else {
                to <- as.POSIXlt(to)
                mon <- seq.int(r1$mon, 12*(to$year - r1$year) + to$mon, by)
            }
            r1$mon <- mon
            res <- as.Date(r1)
        }
        return(res)
    }
}

cut.Date <-
    function (x, breaks, labels = NULL, start.on.monday = TRUE,
              right = FALSE, ...)
{
    if(!inherits(x, "Date")) stop("'x' must be a date-time object")
    x <- as.Date(x)

    if (inherits(breaks, "Date")) {
	breaks <- as.Date(breaks)
    } else if(is.numeric(breaks) && length(breaks) == 1L) {
	## specified number of breaks
    } else if(is.character(breaks) && length(breaks) == 1L) {
        by2 <- strsplit(breaks, " ", fixed=TRUE)[[1L]]
        if(length(by2) > 2L || length(by2) < 1L)
            stop("invalid specification of 'breaks'")
	valid <-
	    pmatch(by2[length(by2)], c("days", "weeks", "months", "years", "quarters"))
	if(is.na(valid)) stop("invalid specification of 'breaks'")
	start <- as.POSIXlt(min(x, na.rm=TRUE))
	if(valid == 1L) incr <- 1L
	if(valid == 2L) {
	    start$mday <- start$mday - start$wday
	    if(start.on.monday)
		start$mday <- start$mday + ifelse(start$wday > 0L, 1L, -6L)
	    incr <- 7L
	}
    if(valid == 3L) {
        start$mday <- 1L
        end <- as.POSIXlt(max(x, na.rm = TRUE))
        step <- ifelse(length(by2) == 2L, as.integer(by2[1L]), 1L)
        end <- as.POSIXlt(end + (31 * step * 86400))
        end$mday <- 1L
        breaks <- as.Date(seq(start, end, breaks))
    } else if(valid == 4L) {
        start$mon <- 0L
        start$mday <- 1L
        end <- as.POSIXlt(max(x, na.rm = TRUE))
        step <- ifelse(length(by2) == 2L, as.integer(by2[1L]), 1L)
        end <- as.POSIXlt(end + (366 * step * 86400))
        end$mon <- 0L
        end$mday <- 1L
        breaks <- as.Date(seq(start, end, breaks))
    } else if(valid == 5L) {
        qtr <- rep(c(0L, 3L, 6L, 9L), each = 3L)
        start$mon <- qtr[start$mon + 1L]
        start$mday <- 1L
        end <- as.POSIXlt(max(x, na.rm = TRUE))
        step <- ifelse(length(by2) == 2L, as.integer(by2[1L]), 1L)
        end <- as.POSIXlt(end + (93 * step * 86400))
        end$mon <- qtr[end$mon + 1L]
        end$mday <- 1L
        breaks <- as.Date(seq(start, end, paste(step * 3L, "months")))
    } else {
        start <- as.Date(start)
        if (length(by2) == 2L) incr <- incr * as.integer(by2[1L])
        maxx <- max(x, na.rm = TRUE)
        breaks <- seq.int(start, maxx + incr, breaks)
        breaks <- breaks[seq_len(1L+max(which(breaks <= maxx)))]
      }
    } else stop("invalid specification of 'breaks'")
    res <- cut(unclass(x), unclass(breaks), labels = labels,
               right = right, ...)
    if(is.null(labels)) levels(res) <- as.character(breaks[-length(breaks)])
    res
}

julian.Date <- function(x, origin = as.Date("1970-01-01"), ...)
{
    if(length(origin) != 1L) stop("'origin' must be of length one")
    structure(unclass(x) - unclass(origin), "origin" = origin)
}

weekdays.Date <- function(x, abbreviate = FALSE)
    format(x, ifelse(abbreviate, "%a", "%A"))

months.Date <- function(x, abbreviate = FALSE)
    format(x, ifelse(abbreviate, "%b", "%B"))

quarters.Date <- function(x, ...)
{
    x <- (as.POSIXlt(x)$mon) %/% 3L
    paste("Q", x+1L, sep = "")
}

## These only make sense for negative digits, but still ...
round.Date <- function(x, ...)
{
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod()
    class(val) <- cl
    val
}

## must avoid truncating forards dates prior to 1970-01-01.
trunc.Date <- function(x, ...) round(x - 0.4999999)

rep.Date <- function(x, ...)
{
    y <- NextMethod()
    structure(y, class="Date")
}

diff.Date <- function (x, lag = 1L, differences = 1L, ...)
{
    ismat <- is.matrix(x)
    xlen <- if (ismat) dim(x)[1L] else length(x)
    if (length(lag) > 1L || length(differences) > 1L || lag < 1L || differences < 1L)
        stop("'lag' and 'differences' must be integers >= 1")
    if (lag * differences >= xlen)
        return(structure(numeric(0L), class="difftime", units="days"))
    r <- x
    i1 <- -seq_len(lag)
    if (ismat)
        for (i in seq_len(differences)) r <- r[i1, , drop = FALSE] -
            r[-nrow(r):-(nrow(r) - lag + 1L), , drop = FALSE]
    else for (i in seq_len(differences))
        r <- r[i1] - r[-length(r):-(length(r) - lag + 1L)]
    r
}

## ---- additions in 2.6.0 -----

is.numeric.Date <- function(x) FALSE

## ---- additions in 2.8.0 -----

split.Date <-
function(x, f, drop = FALSE, ...)
{
    y <- split.default(as.integer(x), f, drop = drop)
    for(i in seq_along(y)) class(y[[i]]) <- "Date"
    y
}

xtfrm.Date <- function(x) as.numeric(x)
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
#  File src/library/base/R/dcf.R
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

read.dcf <-
function(file, fields = NULL, all = FALSE)
{
    if(is.character(file)){
        file <- gzfile(file, "r")
        on.exit(close(file))
    }
    if(!inherits(file, "connection"))
        stop("'file' must be a character string or connection")

    ## For historical reasons, the default is not to accumulate repeated
    ## fields in a record (in fact picking the *last* field occurrence).
    ## Use the internal code for performance reasons, but note that we
    ## could of course as well use
    ##   do.call("cbind",
    ##           lapply(out,
    ##                  function(s)
    ##                  if(is.atomic(s)) s
    ##                  else mapply("[[", s, sapply(s, length))))
    if(!all) return(.Internal(readDCF(file, fields)))

    .assemble_things_into_a_data_frame <- function(tags, vals, nums) {
        tf <- factor(tags, levels = unique(tags))

        cnts <- table(nums, tf)
        out <- array(NA_character_, dim = dim(cnts),
                     dimnames = list(NULL, levels(tf)))
        if(all(cnts <= 1L)) {
            ## No repeated tags ...
            out[cbind(nums, tf)] <- vals
            out <- as.data.frame(out, stringsAsFactors = FALSE)
        }
        else {
            levs <- colSums(cnts > 1L) == 0
            if(any(levs)) {
                inds <- tf %in% levels(tf)[levs]
                out[cbind(nums[inds], tf[inds])] <- vals[inds]
            }
            out <- as.data.frame(out, stringsAsFactors = FALSE)
            for(l in levels(tf)[!levs]) {
                out[[l]] <- rep.int(list(NA_character_), nrow(cnts))
                i <- tf == l
                out[[l]][unique(nums[i])] <- split(vals[i], nums[i])
            }
        }

        out
    }

    on.exit(Sys.setlocale("LC_CTYPE", Sys.getlocale("LC_CTYPE")), add = TRUE)
    Sys.setlocale("LC_CTYPE", "C")

    lines <- readLines(file)

    ## Try to find out about invalid things: mostly, lines which do not
    ## start with blanks but have no ':' ...
    ind <- grep("^[^[:blank:]][^:]*$", lines)
    if(length(ind)) {
        lines <- strtrim(lines[ind], 0.7 * getOption("width"))
        stop(gettextf("Invalid DCF format.\nRegular lines must have a tag.\nOffending lines start with:\n%s",
                      paste("  ", lines, sep = "", collapse = "\n")),
             domain = NA)
    }

    line_is_not_empty <- !grepl("^[[:space:]]*$", lines)
    nums <- cumsum(diff(c(FALSE, line_is_not_empty) > 0L) > 0L)
    ## Remove the empty ones so that nums knows which record each line
    ## belongs to.
    nums <- nums[line_is_not_empty]
    lines <- lines[line_is_not_empty]

    ## Deal with escaped blank lines (used by Debian at least for the
    ## Description: values, see man 5 deb-control):
    line_is_escaped_blank <- grepl("^[[:space:]]+\\.[[:space:]]*$", lines)
    if(any(line_is_escaped_blank))
        lines[line_is_escaped_blank] <- ""

    line_has_tag <- grepl("^[^[:blank:]][^:]*:", lines)

    ## Check that records start with tag lines.
    ind <- which(!line_has_tag[which(diff(nums) > 0L) + 1L])
    if(length(ind)) {
        lines <- strtrim(lines[ind], 0.7 * getOption("width"))
        stop(gettextf("Invalid DCF format.\nContinuation lines must not start a record.\nOffending lines start with:\n%s",
                      paste("  ", lines, sep = "", collapse = "\n")),
             domain = NA)
    }

    ## End positions of field entries.
    pos <- cumsum(rle(cumsum(line_has_tag))$lengths)

    tags <- sub(":.*", "", lines[line_has_tag])
    lines[line_has_tag] <-
        sub("[^:]*:[[:space:]]*", "", lines[line_has_tag])
    lines[!line_has_tag] <-
        sub("^[[:space:]]*", "", lines[!line_has_tag])

    vals <- mapply(function(from, to) paste(lines[from:to],
                                            collapse = "\n"),
                   c(1L, pos[-length(pos)] + 1L), pos)

    out <- .assemble_things_into_a_data_frame(tags, vals, nums[pos])

    if(!is.null(fields))
        out <- out[fields]

    out
}

write.dcf <-
function(x, file = "", append = FALSE,
         indent = 0.1 * getOption("width"),
         width = 0.9 * getOption("width"))
{
    if(file == "")
        file <- stdout()
    else if(is.character(file)) {
        file <- file(file, ifelse(append, "a", "w"))
        on.exit(close(file))
    }
    if(!inherits(file, "connection"))
        stop("'file' must be a character string or connection")

    ## We need to take care of two things:
    ## * We really should not write out NA entries.
    ## * We have to handle multiple fields per record.

    ## do not assume that the input is valid in this locale
    escape_paragraphs <- function(s)
	gsub("\n \\.([^\n])","\n  .\\1",
	     gsub("\n[[:space:]]*\n", "\n .\n ", s, useBytes=TRUE),
             useBytes=TRUE)

    if(!is.data.frame(x))
        x <- as.data.frame(x, stringsAsFactors = FALSE)
    nmx <- names(x)
    out <- matrix("", nrow(x), ncol(x))
    for(j in seq_along(x)) {
        xj <- x[[j]]
        if(is.atomic(xj)) {
            ## For atomic ("character") columns, things are simple ...
            i <- !is.na(xj)
            s <- formatDL(rep.int(nmx[j], sum(i)), xj[i],
                          style = "list", width = width,
                          indent = indent)
            out[i, j] <- escape_paragraphs(s)
        }
        else {
            ## Should be a list ...
            nmxj <- nmx[j]
            i <- !sapply(xj, function(s) (length(s) == 1L) && is.na(s))
            out[i, j] <-
                sapply(xj[i],
                       function(s) {
                           s <- formatDL(rep.int(nmxj, length(s)), s,
                                         style = "list", width = width,
                                         indent = indent)
                           paste(escape_paragraphs(s), collapse = "\n")
                       })
        }
    }
    out <- t(out)
    is_not_empty <- c(out != "")
    eor <- character(sum(is_not_empty))
    if(length(eor)) {
        ## Newline for end of record.
        ## Note that we do not write a trailing blank line.
        eor[ diff(c(col(out))[is_not_empty]) >= 1L ] <- "\n"
    }
    writeLines(paste(c(out[is_not_empty]), eor, sep = ""), file)
}
debug <- function(fun, text="", condition=NULL) 
    .Internal(debug(fun, text, condition))
debugonce <- function(fun, text="", condition=NULL) 
    .Internal(debugonce(fun, text, condition))
undebug <- function(fun) .Internal(undebug(fun))
isdebugged <- function(fun) .Internal(isdebugged(fun))

browserText <- function(n=1L) .Internal(browserText(n))
browserCondition <- function(n=1L) .Internal(browserCondition(n))
browserSetDebug <- function(n=1L) .Internal(browserSetDebug(n))
#  File src/library/base/R/delay.R
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

delayedAssign <-
    function(x, value, eval.env=parent.frame(1), assign.env=parent.frame(1))
    .Internal(delayedAssign(x, substitute(value), eval.env, assign.env))
#  File src/library/base/R/det.R
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

## det now uses Lapack and an LU decomposition.  The method argument is
##     no longer used.
## S-plus' Matrix pkg has arg. "logarithm = TRUE" and returns list
##        (which is necessary for keeping the sign when taking log ..)
## S-plus v 6.x has incorporated the Matrix pkg det as determinant

det <- function(x, ...)
{
    z <- determinant(x, logarithm = TRUE, ...)
    c(z$sign * exp(z$modulus))
}

determinant <- function(x, logarithm = TRUE, ...) UseMethod("determinant")

determinant.matrix <- function(x, logarithm = TRUE, ...)
{
    if ((n <- ncol(x)) != nrow(x))
        stop("'x' must be a square matrix")
    if (n < 1L)
	return(structure(list(modulus =
			      structure(if(logarithm) 0 else 1,
					logarithm = logarithm),
			      sign = 1L),
			 class = "det"))
    if (is.complex(x))
        stop("determinant not currently defined for complex matrices")
    storage.mode(x) <- "double"
    .Call("det_ge_real", x, logarithm, PACKAGE = "base")
}
#  File src/library/base/R/diag.R
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

diag <- function(x = 1, nrow, ncol)
{
    if (is.matrix(x)) {
        if (nargs() > 1L)
            stop("'nrow' or 'ncol' cannot be specified when 'x' is a matrix")

        if((m <- min(dim(x))) == 0L)
	    return(vector(typeof(x), 0L)) # logical, integer, also list ..

        y <- c(x)[1L + 0L:(m - 1L) * (dim(x)[1L] + 1L)]
        nms <- dimnames(x)
        if (is.list(nms) && !any(sapply(nms, is.null)) &&
            identical((nm <- nms[[1L]][seq_len(m)]), nms[[2L]][seq_len(m)]))
            names(y) <- nm
        return(y)
    }
    if(is.array(x) && length(dim(x)) != 1L)
        stop("'x' is an array, but not 1D.")

    if(missing(x))
	n <- nrow
    else if(length(x) == 1L && nargs() == 1L) {
	n <- as.integer(x)
	x <- 1
    }
    else n <- length(x)
    if(!missing(nrow))
	n <- nrow
    if(missing(ncol))
	ncol <- n
    p <- ncol
    y <- array(0, c(n, p))
    if((m <- min(n, p)) > 0L) y[1L + 0L:(m - 1L) * (n + 1L)] <- x
    y
}

"diag<-" <- function(x, value)
{
    dx <- dim(x)
    if(length(dx) != 2L)
	## no further check, to also work with 'Matrix'
	stop("only matrix diagonals can be replaced")
    len.i <- min(dx)
    i <- seq_len(len.i)
    len.v <- length(value)
    if(len.v != 1L && len.v != len.i)
	stop("replacement diagonal has wrong length")
    if(len.i > 0L) x[cbind(i, i)] <- value
    x
}
#  File src/library/base/R/diff.R
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

diff <- function(x, ...) UseMethod("diff")

diff.default <- function(x, lag = 1L, differences = 1L, ...)
{
    ismat <- is.matrix(x)
    xlen <- if(ismat) dim(x)[1L] else length(x)
    if (length(lag) > 1L || length(differences) > 1L ||
        lag < 1L || differences < 1L)
	stop("'lag' and 'differences' must be integers >= 1")
    if (lag * differences >= xlen)
	return(x[0]) # empty of proper mode
    r <- unclass(x)  # don't want class-specific subset methods
    i1 <- -seq_len(lag)
    if (ismat)
	for (i in seq_len(differences))
	    r <- r[i1, , drop = FALSE] -
                r[-nrow(r):-(nrow(r)-lag+1L), , drop = FALSE]
    else
        for (i in seq_len(differences))
            r <- r[i1] - r[-length(r):-(length(r)-lag+1L)]
    class(r) <- oldClass(x)
    r
}
#  File src/library/base/R/dput.R
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

dput <-
    function(x, file = "",
             control = c("keepNA", "keepInteger", "showAttributes"))
{
    if(is.character(file))
        if(nzchar(file)) {
            file <- file(file, "wt")
            on.exit(close(file))
        } else file <- stdout()
    opts <- .deparseOpts(control)
    if(isS4(x)) {
        ## FIXME: this should happen in C {deparse2() in main/deparse.c}
        ##        but we are missing a C-level slotNames()
	cat('new("', class(x),'"\n', file = file, sep = '')
	for(n in slotNames(x)) {
	    cat("    ,", n, "= ", file = file)
	    dput(slot(x, n), file = file, control = control)
	}
	cat(")\n", file = file)
	invisible()
    }
    else .Internal(dput(x, file, opts))
}

dget <- function(file)
    eval(parse(file = file))
#  File src/library/base/R/dump.R
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

dump <- function (list, file = "dumpdata.R", append = FALSE,
		  control = "all", envir = parent.frame(),
		  evaluate = TRUE)
{
    if(is.character(file)) {
	## avoid opening a file if there is nothing to dump
	ex <- sapply(list, exists, envir=envir)
	if(!any(ex)) return(invisible(character(0L)))
	if(nzchar(file)) {
	    file <- file(file, ifelse(append, "a", "w"))
	    on.exit(close(file), add = TRUE)
	} else file <- stdout()
    }
    opts <- .deparseOpts(control)
    .Internal(dump(list, file, envir, opts, evaluate))
}

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
#  File src/library/base/R/dynload.R
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

if(.Platform$OS.type == "windows") {
    dyn.load <- function(x, local = TRUE, now = TRUE, ...) {
        inDL <- function(x, local, now, ..., DLLpath = "")
            .Internal(dyn.load(x, local, now, DLLpath))
        inDL(x, as.logical(local), as.logical(now), ...)
    }
} else {
    dyn.load <- function(x, local = TRUE, now = TRUE, ...)
        .Internal(dyn.load(x, as.logical(local), as.logical(now), ""))
}

dyn.unload <- function(x)
    .Internal(dyn.unload(x))

is.loaded <- function(symbol, PACKAGE = "", type = "")
    .Internal(is.loaded(symbol, PACKAGE, type))

getNativeSymbolInfo <- function(name, PACKAGE, unlist = TRUE,
                                 withRegistrationInfo = FALSE)
{
    if(missing(PACKAGE)) PACKAGE <- ""

    if(is.character(PACKAGE))
        pkgName <- PACKAGE
    else if(inherits(PACKAGE, "DLLInfo")) {
        pkgName <- PACKAGE[["path"]]
        PACKAGE <- PACKAGE[["info"]]
    } else if(inherits(PACKAGE, "DLLInfoReference")) {
        pkgName <- character()
    } else
        stop("must pass a package name, DLLInfo or DllInfoReference object")

    syms <- lapply(name, function(id) {
	v <- .Call("R_getSymbolInfo", as.character(id), PACKAGE,
		   as.logical(withRegistrationInfo), PACKAGE = "base")
	if(is.null(v)) {
	    msg <- paste("no such symbol", id)
	    if(length(pkgName) && nzchar(pkgName))
		msg <- paste(msg, "in package", pkgName)
	    stop(msg)
	}
	names(v) <- c("name", "address", "package", "numParameters")[seq_along(v)]
	v
    })

   if(length(name) == 1L && unlist)
     syms <- syms[[1L]]
   else
     names(syms) <- name

   syms
}

getLoadedDLLs <- function()
{
    els <- .Call("R_getDllTable", PACKAGE = "base")
    names(els) = sapply(els, function(x) x[["name"]])
    els
}


getDLLRegisteredRoutines <- function(dll, addNames = TRUE)
    UseMethod("getDLLRegisteredRoutines")


getDLLRegisteredRoutines.character <- function(dll, addNames = TRUE)
{
    dlls <- getLoadedDLLs()
    w <- sapply(dlls, function(x) x[["name"]] == dll || x[["path"]] == dll)

    if(!any(w))
        stop("No DLL currently loaded with name or path ", dll)

    dll <- which(w)[1L]
    if(sum(w) > 1L)
        warning(gettextf("multiple DLLs match '%s'. Using '%s'",
                         dll, dll[["path"]]), domain = NA)

    getDLLRegisteredRoutines(dlls[[dll]], addNames)
}


getDLLRegisteredRoutines.DLLInfo <- function(dll, addNames = TRUE)
{
    ## Provide methods for the different types.
    if(!inherits(dll, "DLLInfo"))
        stop("must specify DLL via a DLLInfo object. See getLoadedDLLs()")

    info <- dll[["info"]]
    els <- .Call("R_getRegisteredRoutines", info, PACKAGE = "base")
    ## Put names on the elements by getting the names from each element.
    if(addNames) {
      els <- lapply(els, function(x) {
                              if(length(x))
                                 names(x) <- sapply(x, function(z) z$name)
                              x
                         })
    }
    class(els) <- "DLLRegisteredRoutines"
    els
}


print.NativeRoutineList <-
function(x, ...)
{
    m <- data.frame(numParameters = sapply(x, function(x) x$numParameters),
                    row.names = sapply(x, function(x) x$name))
    print(m, ...)
    invisible(x)
}

print.DLLRegisteredRoutines <-
      # This is arranged as a ragged data frame.  It may be confusing
      # if one reads it row-wise as the columns are related in pairs
      # but not across pairs.  We might leave it as  a list of lists
      # but that spans a great deal of vertical space and involves
      # a lot of scrolling for the user.
function(x, ...)
{
      # Create a data frame with as many rows as the maximum number
      # of routines in any category. Then fill the column with ""
      # and then the actual entries.

    n <- max(sapply(x, length))
    d <- list()
    sapply(names(x),
             function(id) {
                d[[id]] <<- rep("", n)
                names <- sapply(x[[id]], function(x) x$name)
                if(length(names))
                    d[[id]][seq_along(names)] <<- names

                d[[paste(id, "numParameters")]] <<- rep("", n)
                names <- sapply(x[[id]], function(x) x$numParameters)
                if(length(names))
                    d[[paste(id, "numParameters")]][seq_along(names)] <<- names
             })
    print(as.data.frame(d), ...)
    invisible(x)
}

getCallingDLLe <- function(e)
{
    if (is.null(env <- e$".__NAMESPACE__.")) env <- baseenv()
    if(exists("DLLs", envir = env) &&
       length(env$DLLs))
        return(env$DLLs[[1L]])
    NULL
}

getCallingDLL <-
function(f = sys.function(-1), doStop = FALSE)
{
    e <- environment(f)

    if(!isNamespace(e)) {
        if(doStop)
            stop("function is not in a namespace, so cannot locate associated DLL")
        else
            return(NULL)
    }

       # Please feel free to replace with a more encapsulated way to do this.
    if (is.null(env <- e$".__NAMESPACE__.")) env <- baseenv()
    if(exists("DLLs", envir = env) && length(env$DLLs))
        return(env$DLLs[[1L]])
    else {
        if(doStop)
            stop("looking for DLL for native routine call, but no DLLs in namespace of call")
        else
            NULL
    }
    NULL
}

print.DLLInfo <- function(x, ...)
{
    tmp <- as.data.frame.list(x[c("name", "path", "dynamicLookup")])
    names(tmp) <- c("DLL name", "Filename", "Dynamic lookup")
    write.dcf(tmp, ...)
    invisible(x)
}

print.DLLInfoList <- function(x, ...)
{
    if(length(x)) {
        m <- data.frame(Filename = sapply(x, function(x) x[["path"]]),
                        "Dynamic Lookup" =
                        sapply(x, function(x) x[["dynamicLookup"]]))
        print(m, ...)
    }
    invisible(x)
}


`$.DLLInfo` <- function(x, name)
    getNativeSymbolInfo(as.character(name), PACKAGE = x)




#  File src/library/base/R/eapply.R
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

eapply <- function (env, FUN, ..., all.names = FALSE, USE.NAMES = TRUE)
{
    FUN <- match.fun(FUN)
    .Internal(eapply(env, FUN, all.names, USE.NAMES))
}
#  File src/library/base/R/eigen.R
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


isSymmetric <- function(object, ...) UseMethod("isSymmetric")

isSymmetric.matrix <- function(object, tol = 100*.Machine$double.eps, ...)
{
    if(!is.matrix(object)) return(FALSE) ## we test for  symmetric *matrix*
    ## cheap pretest: is it square?
    d <- dim(object)
    if(d[1L] != d[2L]) return(FALSE)
    test <-
        if(is.complex(object))
            all.equal.numeric(object, Conj(t(object)), tolerance = tol, ...)
        else # numeric, character, ..
            all.equal(object, t(object), tolerance = tol, ...)
    isTRUE(test)
}

eigen <- function(x, symmetric, only.values = FALSE, EISPACK = FALSE)
{
    x <- as.matrix(x)
    if(!is.null(dimnames(x)))
        dimnames(x) <- list(NULL, NULL)  # or they appear on eigenvectors
    n <- nrow(x)
    if (!n) stop("0 x 0 matrix")
    if (n != ncol(x)) stop("non-square matrix in 'eigen'")

    complex.x <- is.complex(x)
    if(!complex.x && !is.double(x))
	storage.mode(x) <- "double"
    if (!all(is.finite(x))) stop("infinite or missing values in 'x'")

    if(missing(symmetric)) symmetric <- isSymmetric.matrix(x)

    if (!EISPACK) {
        if (symmetric) {
            z <- if(!complex.x)
                .Call("La_rs", x, only.values, PACKAGE = "base")
            else
                .Call("La_rs_cmplx", x, only.values, PACKAGE = "base")
            ord <- rev(seq_along(z$values))
        } else {
            z <- if(!complex.x)
                .Call("La_rg", x, only.values, PACKAGE = "base")
            else
                .Call("La_rg_cmplx", x, only.values, PACKAGE = "base")
            ord <- sort.list(Mod(z$values), decreasing = TRUE)
        }
        return(list(values = z$values[ord],
                    vectors = if (!only.values) z$vectors[, ord, drop = FALSE]))
    }

    dbl.n <- double(n)
    if(symmetric) {##--> real values
	if(complex.x) {
	    xr <- Re(x)
	    xi <- Im(x)
	    z <- .Fortran("ch",
			  n,
			  n,
			  xr,
			  xi,
			  values = dbl.n,
			  !only.values,
			  vectors = xr,
			  ivectors = xi,
			  dbl.n,
			  dbl.n,
			  double(2*n),
			  ierr = integer(1L),
                          PACKAGE="base")
	    if (z$ierr)
		stop(gettextf("'ch' returned code %d in 'eigen'", z$ierr),
                     domain = NA)
	    if(!only.values)
		z$vectors <- matrix(complex(real=z$vectors,
					    imaginary=z$ivectors), ncol=n)
	}
	else {
	    z <- .Fortran("rs",
			  n,
			  n,
			  x,
			  values = dbl.n,
			  !only.values,
			  vectors = x,
			  dbl.n,
			  dbl.n,
			  ierr = integer(1L),
                          PACKAGE="base")
	    if (z$ierr)
		stop(gettextf("'rs' returned code %d in 'eigen'", z$ierr),
                     domain = NA)
	}
	ord <- sort.list(z$values, decreasing = TRUE)
    }
    else {##- Asymmetric :
	if(complex.x) {
	    xr <- Re(x)
	    xi <- Im(x)
	    z <- .Fortran("cg",
			  n,
			  n,
			  xr,
			  xi,
			  values = dbl.n,
			  ivalues = dbl.n,
			  !only.values,
			  vectors = xr,
			  ivectors = xi,
			  dbl.n,
			  dbl.n,
			  dbl.n,
			  ierr = integer(1L),
                          PACKAGE="base")
	    if (z$ierr)
		stop(gettextf("'cg' returned code %d in 'eigen'", z$ierr),
                     domain = NA)
	    z$values <- complex(real=z$values,imaginary=z$ivalues)
	    if(!only.values)
		z$vectors <- matrix(complex(real=z$vectors,
					    imaginary=z$ivectors), ncol=n)
	}
	else {
	    z <- .Fortran("rg",
			  n,
			  n,
			  x,
			  values = dbl.n,
			  ivalues = dbl.n,
			  !only.values,
			  vectors = x,
			  integer(n),
			  dbl.n,
			  ierr = integer(1L),
                          PACKAGE="base")
	    if (z$ierr)
		stop(gettextf("'rg' returned code %d in 'eigen'", z$ierr),
                     domain = NA)
	    ind <- z$ivalues > 0L
	    if(any(ind)) {#- have complex (conjugated) values
		ind <- seq.int(n)[ind]
		z$values <- complex(real=z$values,imaginary=z$ivalues)
		if(!only.values) {
		    z$vectors[, ind] <- complex(real=z$vectors[,ind],
						imaginary=z$vectors[,ind+1])
		    z$vectors[, ind+1] <- Conj(z$vectors[,ind])
		}
	    }
	}
	ord <- sort.list(Mod(z$values), decreasing = TRUE)
    }
    list(values = z$values[ord],
	 vectors = if(!only.values) z$vectors[,ord, drop = FALSE])
}
#  File src/library/base/R/environment.R
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

environment <- function(fun=NULL) .Internal(environment(fun))

environmentName <- function(env) .Internal(environmentName(env))

env.profile <- function(env) .Internal(env.profile(env))
#  File src/library/base/R/eval.R
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

.GlobalEnv <- environment()
parent.frame <- function(n = 1) .Internal(parent.frame(n))

eval <-
    function(expr, envir = parent.frame(),
	     enclos = if(is.list(envir) || is.pairlist(envir))
                       parent.frame() else baseenv())
    .Internal(eval(expr, envir, enclos))

eval.parent <- function(expr, n = 1){
    p <- parent.frame(n + 1)
    eval(expr , p)
}

evalq <-
    function (expr, envir = parent.frame(), enclos = if (is.list(envir) ||
    is.pairlist(envir)) parent.frame() else baseenv())
      .Internal(eval(substitute(expr), envir, enclos)) 

new.env <- function (hash=FALSE, parent=parent.frame(), size=29L)
    .Internal(new.env(hash, parent, size))

parent.env <- function(env)
    .Internal(parent.env(env))

"parent.env<-" <- function(env, value)
    .Internal("parent.env<-"(env, value))

local <-
    function (expr, envir = new.env())
    eval.parent(substitute(eval(quote(expr), envir)))

Recall <- function(...) .Internal(Recall(...))

with <- function(data, expr, ...) UseMethod("with")
within <- function(data, expr, ...) UseMethod("within")

with.default <- function(data, expr, ...)
    eval(substitute(expr), data, enclos=parent.frame())

within.data.frame <- function(data, expr, ...)
{
    parent <- parent.frame()
    e <- evalq(environment(), data, parent)
    eval(substitute(expr), e)
    l <- as.list(e)
    l <- l[!sapply(l, is.null)]
    ## del: variables to *del*ete from data[]
    nD <- length(del <- setdiff(names(data), (nl <- names(l))))
    data[nl] <- l
    if(nD)
	data[del] <- if(nD == 1) NULL else vector("list", nD)
    data
}

within.list <- within.data.frame



force <- function(x) x
#  File src/library/base/R/exists.R
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

exists <-
    function (x, where = -1,
              envir = if(missing(frame)) as.environment(where) else sys.frame(frame),
              frame, mode = "any", inherits = TRUE)
    .Internal(exists(x, envir, mode, inherits))
#  File src/library/base/R/expand.grid.R
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

expand.grid <- function(..., KEEP.OUT.ATTRS = TRUE, stringsAsFactors = TRUE)
{
    ## x should either be a list or a set of vectors or factors
    nargs <- length(args <- list(...))
    if(!nargs) return(as.data.frame(list()))
    if(nargs == 1L && is.list(a1 <- args[[1L]]))
	nargs <- length(args <- a1)
    if(nargs == 0L) return(as.data.frame(list()))
    cargs <- args
    iArgs <- seq_len(nargs)
    nmc <- paste("Var", iArgs, sep="")
    nm <- names(args)
    if(is.null(nm))
	nm <- nmc
    else if(any(ng0 <- nzchar(nm)))
	nmc[ng0] <- nm[ng0]
    names(cargs) <- nmc
    rep.fac <- 1L
    d <- sapply(args, length)
    if(KEEP.OUT.ATTRS) {
	dn <- vector("list", nargs)
	names(dn) <- nmc
    }
    orep <- prod(d)
    if(orep == 0L) {
        for(i in iArgs) cargs[[i]] <- args[[i]][FALSE]
    } else {
        for(i in iArgs) {
            x <- args[[i]]
            if(KEEP.OUT.ATTRS)
                dn[[i]] <- paste(nmc[i], "=", if(is.numeric(x)) format(x) else x,
                                 sep = "")
            nx <- length(x)
            orep <- orep/nx
            x <- x[rep.int(rep.int(seq_len(nx),
                                   rep.int(rep.fac, nx)), orep)]
	    ## avoid sorting the levels of character variates
	    if(stringsAsFactors && !is.factor(x) && is.character(x))
		x <- factor(x, levels = unique(x))
            cargs[[i]] <- x
            rep.fac <- rep.fac * nx
        }
    }
    if(KEEP.OUT.ATTRS)
	attr(cargs, "out.attrs") <- list(dim=d, dimnames=dn)
    rn <- .set_row_names( as.integer(prod(d)) )
    structure(cargs, class = "data.frame", row.names = rn)
}
#  File src/library/base/R/factor.R
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

factor <- function(x = character(), levels, labels=levels,
                   exclude = NA, ordered = is.ordered(x))
{
    if(is.null(x)) x <- character()
    nx <- names(x)
    if (missing(levels)) {
	y <- unique(x)
	ind <- sort.list(y) # or possibly order(x) which is more (too ?) tolerant
	y <- as.character(y)
	levels <- unique(y[ind])
    }
    force(ordered)
    exclude <- as.vector(exclude, typeof(x))# may result in NA
    x <- as.character(x)
    levels <- levels[is.na(match(levels, exclude))]
    f <- match(x, levels)
    if(!is.null(nx))
	names(f) <- nx
    nl <- length(labels)
    nL <- length(levels)
    if(!any(nl == c(1L, nL)))
	stop(gettextf("invalid labels; length %d should be 1 or %d", nl, nL),
	     domain = NA)
    levels(f) <- ## nl == nL or 1
	if (nl == nL) as.character(labels)
	else paste(labels, seq_along(levels), sep="")
    class(f) <- c(if(ordered)"ordered", "factor")
    f
}

is.factor <- function(x) inherits(x, "factor")
as.factor <- function(x) if (is.factor(x)) x else factor(x)

## Help old S users:
category <- function(...) .Defunct()

levels <- function(x) UseMethod("levels")
levels.default <- function(x) attr(x, "levels")
nlevels <- function(x) length(levels(x))

## now a primitive
## "levels<-" <- function(x, value) UseMethod("levels<-")

## "levels<-.default" <- function(x, value)
## {
##     attr(x, "levels") <- value
##     x
## }

"levels<-.factor" <- function(x, value)
{
    xlevs <- levels(x)
    if (is.list(value)) {
        nlevs <- rep.int(names(value), lapply(value, length))
        value <- unlist(value)
        m <- match(value, xlevs, nomatch = 0L)
        xlevs[m] <- nlevs[m > 0L]
    } else {
        if (length(xlevs) > length(value))
            stop("number of levels differs")
        nlevs <- xlevs <- as.character(value)
        nlevs <- nlevs[!is.na(nlevs)]
    }
    ## take care here not to drop attributes, including class.
    ## factor(xlevs[x], levels = unique(nlevs))
    nlevs <- unique(nlevs)
    at <- attributes(x)
    at$levels <- nlevs
    y <- match(xlevs[x], nlevs)
    attributes(y) <- at
    y
}

as.vector.factor <- function(x, mode="any")
{
    if(mode=="list") as.list(x)
    else if(mode== "any" || mode== "character" || mode== "logical")
	as.vector(levels(x)[x], mode)
    else
	as.vector(unclass(x), mode)
}

as.character.factor <- function(x,...) levels(x)[x]

as.list.factor <- function(x,...)
{
    res <- vector("list", length(x))
    for(i in seq_along(x)) res[[i]] <- x[i]
    res
}

## for `factor' *and* `ordered' :
print.factor <- function (x, quote = FALSE, max.levels = NULL,
                          width = getOption("width"), ...)
{
    ord <- is.ordered(x)
    if (length(x) == 0L)
        cat(if(ord)"ordered" else "factor", "(0)\n", sep = "")
    else {
        ## The idea here is to preserve all relevant attributes such as
        ## names and dims
        xx <- x
        class(xx) <- NULL
        levels(xx) <- NULL
        xx[] <- as.character(x)
        print(xx, quote = quote, ...)
    }
    maxl <- if(is.null(max.levels)) TRUE else max.levels
    if (maxl) {
        n <- length(lev <- encodeString(levels(x), quote=ifelse(quote, '"', '')))
        colsep <- if(ord) " < " else " "
        T0 <- "Levels: "
        if(is.logical(maxl))
            maxl <- { ## smart default
                width <- width - (nchar(T0, "w") + 3L + 1L + 3L)
                                        # 3='...', 3=#lev, 1=extra
                lenl <- cumsum(nchar(lev, "w") + nchar(colsep, "w"))
                if(n <= 1L || lenl[n] <= width) n
                else max(1L, which(lenl > width)[1L] - 1L)
            }
        drop <- n > maxl
        cat(if(drop)paste(format(n),""), T0,
            paste(if(drop)c(lev[1L:max(1,maxl-1)],"...",if(maxl > 1) lev[n])
                      else lev, collapse= colsep), "\n", sep="")
    }
    invisible(x)
}


Math.factor <- function(x, ...) {
    stop(.Generic, " not meaningful for factors")
}
Summary.factor <- function(..., na.rm) {
    stop(.Generic, " not meaningful for factors")
}
Ops.factor <- function(e1, e2)
{
    ok <- switch(.Generic, "=="=, "!="=TRUE, FALSE)
    if(!ok) {
	warning(.Generic, " not meaningful for factors")
	return(rep.int(NA, max(length(e1), if(!missing(e2))length(e2))))
    }
    nas <- is.na(e1) | is.na(e2)
    ## Need this for NA *levels* as opposed to missing
    noNA.levels <- function(f) {
	r <- levels(f)
	if(any(ina <- is.na(r))) {
	    n <- "  NA "
	    while(n %in% r) n <- paste(n, ".")
	    r[ina] <- n
	}
	r
    }
    if (nzchar(.Method[1L])) { # e1 *is* a factor
	l1 <- noNA.levels(e1)
	e1 <- l1[e1]
    }
    if (nzchar(.Method[2L])) { # e2 *is* a factor
	l2 <- noNA.levels(e2)
	e2 <- l2[e2]
    }
    if (all(nzchar(.Method)) &&
	(length(l1) != length(l2) || !all(sort.int(l2) == sort.int(l1))))
	stop("level sets of factors are different")
    value <- NextMethod(.Generic)
    value[nas] <- NA
    value
}

"[.factor" <- function(x, ..., drop = FALSE)
{
    y <- NextMethod("[")
    attr(y,"contrasts")<-attr(x,"contrasts")
    ## NB factor has levels before class in attribute list (PR#6799)
    attr(y,"levels") <- attr(x,"levels")
    class(y) <- oldClass(x)
    if ( drop ) factor(y) else y
}

"[<-.factor" <- function(x, ..., value)
{
    lx <- levels(x)
    cx <- oldClass(x)
#    nas <- is.na(x) # unused
    if (is.factor(value))
	value <- levels(value)[value]
    m <- match(value, lx)
    if (any(is.na(m) & !is.na(value)))
	warning("invalid factor level, NAs generated")
    class(x) <- NULL
    x[...] <- m
    attr(x,"levels") <- lx
    class(x) <- cx
    x
}

"[[.factor" <- function(x, ...)
{
    y <- NextMethod("[[")
    attr(y,"contrasts") <- attr(x,"contrasts")
    ## NB factor has levels before class in attribute list (PR#6799)
    attr(y,"levels") <- attr(x,"levels")
    class(y) <- oldClass(x)
    y
}

## ordered factors ...

ordered <- function(x, ...) factor(x, ..., ordered=TRUE)

is.ordered <- function(x) inherits(x, "ordered")
as.ordered <- function(x) if(is.ordered(x)) x else ordered(x)

Ops.ordered <-
function (e1, e2)
{
    ok <- switch(.Generic,
		 "<" = , ">" = , "<=" = , ">=" = ,"=="=, "!=" =TRUE,
		 FALSE)
    if(!ok) {
	warning(sprintf("'%s' is not meaningful for ordered factors",
                        .Generic))
	return(rep.int(NA, max(length(e1), if(!missing(e2))length(e2))))
    }
    if (.Generic %in% c("==", "!="))
      return(NextMethod(.Generic))  ##not S-PLUS compatible, but saner
    nas <- is.na(e1) | is.na(e2)
    ord1 <- FALSE
    ord2 <- FALSE
    if (nzchar(.Method[1L])) {
	l1 <- levels(e1)
	ord1 <- TRUE
    }
    if (nzchar(.Method[2L])) {
	l2 <- levels(e2)
	ord2 <- TRUE
    }
    if (all(nzchar(.Method)) &&
        (length(l1) != length(l2) || !all(l2 == l1)))
	stop("level sets of factors are different")
    if (ord1 && ord2) {
	e1 <- as.integer(e1) # was codes, but same thing for ordered factor.
	e2 <- as.integer(e2)
    }
    else if (!ord1) {
	e1 <- match(e1, l2)
	e2 <- as.integer(e2)
    }
    else if (!ord2) {
	e2 <- match(e2, l1)
	e1 <- as.integer(e1)
    }
    value <- get(.Generic, mode = "function")(e1, e2)
    value[nas] <- NA
    value
}

"is.na<-.factor" <- function(x, value)
{
    lx <- levels(x)
    cx <- oldClass(x)
    class(x) <- NULL
    x[value] <- NA
    structure(x, levels = lx, class = cx)
}

"length<-.factor" <- function(x, value)
{
    cl <- class(x)
    levs <- levels(x)
    x <- NextMethod()
    structure(x, levels=levs, class=cl)
}

addNA <- function(x, ifany=FALSE)
{
    if (!is.factor(x)) x <- factor(x)
    if (ifany & !any(is.na(x))) return(x)
    ll <- levels(x)
    if (!any(is.na(ll))) ll <- c(ll, NA)
    factor(x, levels=ll, exclude=NULL)
}
#  File src/library/base/R/files.R
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

R.home <- function(component="home")
{
    rh <- .Internal(R.home())
    switch(component,
           "home" = rh,
           "share" = if(nzchar(p <- as.vector(Sys.getenv("R_SHARE_DIR")))) p
           else file.path(rh, component),
	   "doc" = if(nzchar(p <- as.vector(Sys.getenv("R_DOC_DIR")))) p
           else file.path(rh, component),
           "include" = if(nzchar(p <- as.vector(Sys.getenv("R_INCLUDE_DIR")))) p
           else file.path(rh, component),
           file.path(rh, component))
}

file.show <-
    function (..., header = rep("", nfiles), title = "R Information",
              delete.file = FALSE, pager = getOption("pager"), encoding = "")
{
    files <- path.expand(c(...))
    nfiles <- length(files)
    if(nfiles == 0)
        return(invisible(NULL))
    ## avoid re-encoding files to the current encoding.
    if(l10n_info()[["UTF-8"]] && encoding == "UTF-8") encoding <- ""
    if(l10n_info()[["Latin-1"]] && encoding == "latin1") encoding <- ""
    if(!is.na(encoding) && encoding != "") {
        for(i in seq_along(files)) {
            f <- files[i]
            tf <- tempfile()
            tmp <- readLines(f)
            tmp2 <- try(iconv(tmp, encoding, "", "byte"))
            if(inherits(tmp2, "try-error")) file.copy(f, tf)
            else writeLines(tmp2, tf)
            files[i] <- tf
            if(delete.file) unlink(f)
        }
        delete.file <- TRUE
    }
    if(is.function(pager))
	pager(files, header, title, delete.file)
    else
        .Internal(file.show(files, header, title, delete.file, pager))
}

file.append <- function(file1, file2)
    .Internal(file.append(file1, file2))

file.remove <- function(...)
    .Internal(file.remove(c(...)))

file.rename <- function(from, to)
    .Internal(file.rename(from, to))

list.files <- function(path = ".", pattern = NULL, all.files = FALSE,
                       full.names = FALSE, recursive = FALSE,
                       ignore.case = FALSE)
    .Internal(list.files(path, pattern, all.files, full.names,
                         recursive, ignore.case))

dir <- list.files

file.path <-
function(..., fsep=.Platform$file.sep)
    .Internal(file.path(list(...), fsep))


file.exists <- function(...) .Internal(file.exists(c(...)))

file.create <- function(..., showWarnings =  TRUE)
    .Internal(file.create(c(...), showWarnings))

file.choose <- function(new=FALSE) .Internal(file.choose(new))

file.copy <- function(from, to, overwrite = recursive, recursive = FALSE)
{
    if (!(nf <- length(from))) stop("no files to copy from")
    if (!(nt <- length(to)))   stop("no files to copy to")
    ## we don't use file_test as that is in utils.
    if (nt == 1 && file.exists(to) && file.info(to)$isdir) {
        ## on Windows we need \ for compiled code
        if(.Platform$OS.type == "windows") {
            from <- gsub("/", "\\", from, fixed = TRUE)
            to <- gsub("/", "\\", to, fixed = TRUE)
        }
        return(.Internal(file.copy(from, to, overwrite, recursive)))
        # to <- file.path(to, basename(from))
    } else if (nf > nt) stop("more 'from' files than 'to' files")
    else if (recursive) warning("'recursive' will be ignored")
    if(nt > nf) from <- rep(from, length.out = nt)
    okay <- file.exists(from)
    if (!overwrite) okay[file.exists(to)] <- FALSE
    if (any(from[okay] %in% to[okay]))
        stop("file can not be copied both 'from' and 'to'")
    if (any(okay)) { ## care: create could fail but append work.
    	okay[okay] <- file.create(to[okay])
    	if(any(okay)) okay[okay] <- file.append(to[okay], from[okay])
    }
    okay
}

file.symlink <- function(from, to) {
    if (!(length(from))) stop("no files to link from")
    if (!(nt <- length(to)))   stop("no files/directory to link to")
    if (nt == 1 && file.exists(to) && file.info(to)$isdir)
        to <- file.path(to, basename(from))
    .Internal(file.symlink(from, to))
}

file.info <- function(...)
{
    res <- .Internal(file.info(fn <- c(...)))
    class(res$mtime) <- class(res$ctime) <- class(res$atime) <-
        c("POSIXt", "POSIXct")
    class(res) <- "data.frame"
    attr(res, "row.names") <- fn # not row.names<- as that does a length check
    res
}

file.access <- function(names, mode = 0)
{
    res <- .Internal(file.access(names, mode))
    names(res) <- names
    res
}

dir.create <- function(path, showWarnings = TRUE, recursive = FALSE,
                       mode = "0777")
    invisible(.Internal(dir.create(path, showWarnings, recursive,
                                   as.octmode(mode))))

format.octmode <- function(x, ...)
{
    isna <- is.na(x)
    y <- x[!isna]
    class(y) <- NULL
    ans0 <- character(length(y))
    z <- NULL
    while(any(y > 0) || is.null(z)) {
        z <- y%%8
        y <- floor(y/8)
        ans0 <- paste(z, ans0, sep="")
    }
    ans <- rep.int(NA_character_, length(x))
    ans[!isna] <- ans0
    ans
}
as.character.octmode <- format.octmode

print.octmode <- function(x, ...)
{
    print(format(x), ...)
    invisible(x)
}

"[.octmode" <- function (x, i)
{
    cl <- oldClass(x)
    y <- NextMethod("[")
    oldClass(y) <- cl
    y
}

as.octmode <- function(x)
{
    if(inherits(x, "octmode")) return(x)
    if(length(x) != 1L) stop("'x' must have length 1")
    if(is.double(x) && x == as.integer(x)) x <- as.integer(x)
    if(is.integer(x)) return(structure(x, class="octmode"))
    if(is.character(x)) {
        xx <- strsplit(x, "")[[1L]]
        if(!all(xx %in% 0:7)) stop("invalid digits")
        z <- as.numeric(xx) * 8^(rev(seq_along(xx)-1))
        return(structure(sum(z), class="octmode"))
    }
    stop("'x' cannot be coerced to 'octmode'")
}

format.hexmode <- function(x, upper.case = FALSE, ...)
{
    isna <- is.na(x)
    y <- x[!isna]
    class(y) <- NULL
    ans0 <- character(length(y))
    z <- NULL
    while(any(y > 0) || is.null(z)) {
        z <- y%%16
        y <- floor(y/16)
        ans0 <- paste(c(0:9, if(upper.case) LETTERS else letters)[1+z],
                      ans0, sep = "")
    }
    ans <- rep.int(NA_character_, length(x))
    ans[!isna] <- ans0
    dim(ans) <- dim(x)
    dimnames(ans) <- dimnames(x)
    names(ans) <- names(x)
    ans
}
as.character.hexmode <- format.hexmode

print.hexmode <- function(x, ...)
{
    print(format(x), ...)
    invisible(x)
}

"[.hexmode" <- function (x, i)
{
    cl <- oldClass(x)
    y <- NextMethod("[")
    oldClass(y) <- cl
    y
}

as.hexmode <-
function(x)
{
    if(inherits(x, "hexmode")) return(x)
    if(length(x) != 1L) stop("'x' must be of length 1")
    if(is.double(x) && (x == as.integer(x))) x <- as.integer(x)
    if(is.integer(x)) return(structure(x, class = "hexmode"))
    if(is.character(x)) {
        xx <- strsplit(tolower(x), "")[[1L]]
        pos <- match(xx, c(0L:9L, letters[1L:6L]))
        if(any(is.na(pos))) stop("invalid digits")
        z <- (pos - 1L) * 16 ^ (rev(seq_along(xx) - 1))
        return(structure(as.integer(sum(z)), class = "hexmode"))
    }
    stop("'x' cannot be coerced to hexmode")
}

system.file <-
function(..., package = "base", lib.loc = NULL)
{
    if(nargs() == 0)
        return(file.path(.Library, "base"))
    if(length(package) != 1L)
        stop("'package' must be of length 1")
    packagePath <- .find.package(package, lib.loc, quiet = TRUE)
    if(length(packagePath) == 0L)
        return("")
    FILES <- file.path(packagePath, ...)
    present <- file.exists(FILES)
    if(any(present))
        FILES[present]
    else ""
}

getwd <- function()
    .Internal(getwd())
setwd <- function(dir)
    .Internal(setwd(dir))
basename <- function(path)
    .Internal(basename(path))
dirname <- function(path)
    .Internal(dirname(path))

Sys.info <- function()
    .Internal(Sys.info())

Sys.sleep <- function(time)
    invisible(.Internal(Sys.sleep(time)))

path.expand <- function(path)
    .Internal(path.expand(path))

Sys.glob <- function(paths, dirmark = FALSE)
    .Internal(Sys.glob(path.expand(paths), dirmark))

unlink <- function(x, recursive = FALSE)
    .Internal(unlink(as.character(x), recursive))

Sys.chmod <- function(paths, mode = "0777")
    .Internal(Sys.chmod(paths, as.octmode(mode)))

Sys.umask <- function(mode = "0000")
    .Internal(Sys.umask(as.octmode(mode)))

Sys.readlink <- function(paths)
    .Internal(Sys.readlink(paths))
#  File src/library/base/R/findInt.R
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
    if(has.na <- any(ix <- is.na(x))) x <- x[!ix]
    nx <- length(x)
    index <- integer(nx)
    ## lengths are always integer
    .C("find_interv_vec",
       xt = as.double(vec), n = length(vec),
       x  = as.double(x),  nx = nx,
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
#  File src/library/base/R/formals.R
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

formals <- function(fun = sys.function(sys.parent())) {
    if(is.character(fun))
	fun <- get(fun, mode = "function", envir = parent.frame())
    .Internal(formals(fun))
}

body <- function(fun = sys.function(sys.parent())) {
    if(is.character(fun))
	fun <- get(fun, mode = "function", envir = parent.frame())
    .Internal(body(fun))
}

alist <- function (...) as.list(sys.call())[-1L]

`body<-` <- function (fun, envir = environment(fun), value) {
    if (is.expression(value)) {
        if (length(value) > 1L)
            warning("using the first element of 'value' of type expression")
        value <- value[[1L]]
    }
    as.function(c(as.list(formals(fun)), list(value)), envir)
}

`formals<-` <- function (fun, envir = environment(fun), value)
{
    bd <- body(fun)
    as.function(c(value,
                  if(is.null(bd) || is.list(bd)) list(bd) else bd),
                envir)
}
#  File src/library/base/R/format.R
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

format <- function(x, ...) UseMethod("format")

format.default <-
    function(x, trim = FALSE, digits = NULL, nsmall = 0L,
	     justify = c("left", "right", "centre", "none"),
	     width = NULL, na.encode = TRUE, scientific = NA,
	     big.mark = "", big.interval = 3L,
	     small.mark = "", small.interval = 5L, decimal.mark = ".",
	     zero.print = NULL, drop0trailing = FALSE, ...)
{
    justify <- match.arg(justify)
    adj <- match(justify, c("left", "right", "centre", "none")) - 1L
    if(is.list(x)) {
	## do it this way to force evaluation of args
	if(missing(trim)) trim <- TRUE
	if(missing(justify)) justify <- "none"
	res <- lapply(x, FUN=function(xx, ...) format.default(unlist(xx),...),
		      trim = trim, digits = digits, nsmall = nsmall,
		      justify = justify, width = width, na.encode = na.encode,
		      scientific = scientific,
		      big.mark = big.mark, big.interval = big.interval,
		      small.mark = small.mark, small.interval = small.interval,
		      decimal.mark = decimal.mark, zero.print = zero.print,
		      drop0trailing = drop0trailing, ...)
	sapply(res, paste, collapse = ", ")
    } else {
	switch(mode(x),
	       NULL = "NULL",
	       character = .Internal(format(x, trim, digits, nsmall, width,
					    adj, na.encode, scientific)),
	       call=, expression=, "function"=, "(" = deparse(x),
	       raw = as.character(x),
           {
	       ## else: logical, numeric, complex, .. :
	       prettyNum(.Internal(format(x, trim, digits, nsmall, width,
					  3L, na.encode, scientific)),
			 big.mark = big.mark, big.interval = big.interval,
			 small.mark = small.mark,
			 small.interval = small.interval,
			 decimal.mark = decimal.mark,
			 zero.print = zero.print, drop0trailing = drop0trailing,
			 is.cmplx = is.complex(x),
			 preserve.width = if (trim) "individual" else "common")
           })
    }
}

format.pval <- function(pv, digits = max(1, getOption("digits")-2),
			eps = .Machine$double.eps, na.form = "NA")
{
    ## Format  P values; auxiliary for print.summary.[g]lm(.)

    if((has.na <- any(ina <- is.na(pv)))) pv <- pv[!ina]
    ## Better than '0.0' for very small values `is0':
    r <- character(length(is0 <- pv < eps))
    if(any(!is0)) {
	rr <- pv <- pv[!is0]
	## be smart -- differ for fixp. and expon. display:
	expo <- floor(log10(ifelse(pv > 0, pv, 1e-50)))
	fixp <- expo >= -3 | (expo == -4 & digits>1)
	if(any( fixp)) rr[ fixp] <- format(pv[ fixp], digits=digits)
	if(any(!fixp)) rr[!fixp] <- format(pv[!fixp], digits=digits)
	r[!is0]<- rr
    }
    if(any(is0)) {
	digits <- max(1,digits-2)
	if(any(!is0)) {
	    nc <- max(nchar(rr, type="w"))
	    if(digits > 1 && digits+6 > nc)
		digits <- max(1, nc - 7)
	    sep <- if(digits==1 && nc <= 6) "" else " "
	} else sep <- if(digits==1) "" else " "
	r[is0] <- paste("<", format(eps, digits=digits), sep = sep)
    }
    if(has.na) { ## rarely
	rok <- r
	r <- character(length(ina))
	r[!ina] <- rok
	r[ina] <- na.form
    }
    r
}

## Martin Maechler <maechler@stat.math.ethz.ch> , 1994-1998 :
formatC <- function (x, digits = NULL, width = NULL,
		     format = NULL, flag = "", mode = NULL,
		     big.mark = "", big.interval = 3L,
		     small.mark = "", small.interval = 5L,
		     decimal.mark = ".", preserve.width = "individual",
                     zero.print = NULL, drop0trailing = FALSE)
{
    format.char <- function (x, width, flag)
    {
	if(is.null(width)) width <- 0L
	else if(width < 0L) { flag <- "-"; width <- -width }
	format.default(x, width=width,
		       justify = if(flag=="-") "left" else "right")
    }
    blank.chars <- function(no)
	sapply(no+1L, function(n) paste(character(n), collapse=" "))

    if (!(n <- length(x))) return("")
    if (is.null(mode))	  mode <- storage.mode(x)
    else if (any(mode == c("double", "real", "integer")))  {
      ## for .C call later on
	if(mode=="real") mode <- "double"
	storage.mode(x) <- mode
    }
    else if (mode != "character") stop("'mode' must be \"double\" (\"real\"), \"integer\" or \"character\"")
    if (mode == "character" || (!is.null(format) && format == "s")) {
	if (mode != "character") {
	    warning('coercing argument to "character" for format="s"')
	    x <- as.character(x)
	}
	return(format.char(x, width=width, flag=flag))
    }
    if (missing(format) || is.null(format))
	format <- if (mode == "integer") "d" else "g"
    else {
	if (any(format == c("f", "e", "E", "g", "G", "fg"))) {
	    if (mode == "integer") mode <- storage.mode(x) <- "double"
	}
	else if (format == "d") {
	    if (mode != "integer") mode <- storage.mode(x) <- "integer"
	}
	else stop('\'format\' must be one of {"f","e","E","g","G", "fg", "s"}')
    }
    some.special <- !all(Ok <- is.finite(x))
    if (some.special) {
	rQ <- as.character(x[!Ok])
	rQ[is.na(rQ)] <- "NA"
	x[!Ok] <- as.vector(0, mode = mode)
    }
    if(is.null(width) && is.null(digits))
	width <- 1L
    if (is.null(digits))
	digits <- if (mode == "integer") 2L else 4L
    else if(digits < 0L)
	digits <- 6L
    else {
	maxDigits <- if(format != "f") 50L else ceiling(-(.Machine$double.neg.ulp.digits + .Machine$double.min.exp) / log2(10))
	if (digits > maxDigits) {
	    warning("'digits' reduced to ", maxDigits)
	    digits <- maxDigits
	}
    }
    if(is.null(width))	width <- digits + 1L
    else if (width == 0L) width <- digits
    i.strlen <-
	pmax(abs(width),
	     if(format == "fg" || format == "f") {
		 xEx <- as.integer(floor(log10(abs(x+ifelse(x==0,1,0)))))
		 as.integer(x < 0 | flag!="") + digits +
		     if(format == "f") {
			 2L + pmax(xEx, 0L)
		     } else {# format == "fg"
			 pmax(xEx, digits,digits+(-xEx)+1L) +
			     ifelse(flag != "", nchar(flag, "b"), 0L) + 1L
		     }
	     } else # format == "g" or "e":
	     rep.int(digits + 8L, n)
	     )
    ## sanity check for flags added 2.1.0
    flag <- as.character(flag)
    nf <- strsplit(flag, "")[[1L]]
    if(!all(nf %in% c("0", "+", "-", " ", "#")))
	stop("'flag' can contain only '0+- #'")
    if(digits > 0 && any(nf == "#"))
	digits <- -digits # C-code will notice "do not drop trailing zeros"

    attr(x, "Csingle") <- NULL	# avoid interpreting as.single
    r <- .C("str_signif",
	    x = x,
	    n = n,
	    mode   = as.character(mode),
	    width  = as.integer(width),
	    digits = as.integer(digits),
	    format = as.character(format),
	    flag   = as.character(flag),
	    result = blank.chars(i.strlen),
	    PACKAGE = "base")$result
    if (some.special)
	r[!Ok] <- format.char(rQ, width=width, flag=flag)

    if(big.mark != "" || small.mark != "" || decimal.mark != "." ||
       !is.null(zero.print) || drop0trailing)
	r <- prettyNum(r, big.mark = big.mark, big.interval = big.interval,
		       small.mark = small.mark, small.interval = small.interval,
		       decimal.mark = decimal.mark, preserve.width = preserve.width,
		       zero.print = zero.print, drop0trailing = drop0trailing,
		       is.cmplx = FALSE)

    if (!is.null(x.atr <- attributes(x)))
	attributes(r) <- x.atr
    r
}

format.factor <- function (x, ...)
    format(structure(as.character(x), names=names(x),
                     dim=dim(x), dimnames=dimnames(x)), ...)


format.data.frame <- function(x, ..., justify = "none")
{
    nr <- .row_names_info(x, 2L)
    nc <- length(x)
    rval <- vector("list", nc)
    for(i in 1L:nc)
	rval[[i]] <- format(x[[i]], ..., justify = justify)
    lens <- sapply(rval, NROW)
    if(any(lens != nr)) { # corrupt data frame, must have at least one column
	warning("corrupt data frame: columns will be truncated or padded with NAs")
	for(i in 1L:nc) {
	    len <- NROW(rval[[i]])
	    if(len == nr) next
	    if(length(dim(rval[[i]])) == 2L) {
		rval[[i]] <- if(len < nr)
		    rbind(rval[[i]], matrix(NA, nr-len, ncol(rval[[i]])))
		else rval[[i]][1L:nr,]
	    } else {
		rval[[i]] <- if(len < nr) c(rval[[i]], rep.int(NA, nr-len))
		else rval[[i]][1L:nr]
	    }
	}
    }
    for(i in 1L:nc) {
	if(is.character(rval[[i]]) && class(rval[[i]]) == "character")
	    oldClass(rval[[i]]) <- "AsIs"
    }
    cn <- names(x)
    m <- match(c("row.names", "check.rows", "check.names"), cn, 0L)
    if(any(m)) cn[m] <- paste("..dfd.", cn[m], sep="")
    names(rval) <- cn
    rval$check.names <- FALSE
    rval$row.names <- row.names(x)
    x <- do.call("data.frame", rval)
    ## x will have more cols than rval if there are matrix/data.frame cols
    if(any(m)) names(x) <- sub("^..dfd.", "", names(x))
    x
}

format.AsIs <- function(x, width = 12, ...)
{
    if(is.character(x)) return(format.default(x, ...))
    if(is.null(width)) width = 12L
    n <- length(x)
    rvec <- rep.int(NA_character_, n)
    for(i in 1L:n) {
        y <- x[[i]]
        ## need to remove class AsIs to avoid an infinite loop.
        cl <- oldClass(y)
        if(m <- match("AsIs", cl, 0L)) oldClass(y) <- cl[-m]
        rvec[i] <- toString(y, width = width, ...)
    }
    ## AsIs might be around a matrix, which is not a class.
    dim(rvec) <- dim(x)
    dimnames(rvec) <- dimnames(x)
    format.default(rvec, justify = "right")
}

prettyNum <-
    function(x,
	     big.mark = "", big.interval = 3L,
	     small.mark = "", small.interval = 5L,
	     decimal.mark = ".",
	     preserve.width = c("common", "individual", "none"),
	     zero.print = NULL, drop0trailing = FALSE, is.cmplx = NA, ...)
{
    if(!is.character(x)) {
        is.cmplx <- is.complex(x)
	x <- sapply(x, format, ...)
    }
    ## be fast in trivial case (when all options have their default):
    nMark <- big.mark== "" && small.mark== "" && decimal.mark== "."
    nZero <- is.null(zero.print) && !drop0trailing
    if(nMark && nZero)
	return(x)

    ## else
    if(!is.null(zero.print) && any(i0 <- as.numeric(x) == 0)) {
	## print zeros according to 'zero.print' (logical or string):
	if(length(zero.print) > 1L) stop("'zero.print' has length > 1")
	if(is.logical(zero.print))
	    zero.print <- if(zero.print) "0" else " "
	if(!is.character(zero.print))
	    stop("'zero.print' must be character, logical or NULL")
	nz <- nchar(zero.print, "c")
	nc <- nchar(x[i0], "c")
	ind0 <- regexpr("0", x[i0], fixed = TRUE)# first '0' in string
	substr(x[i0],ind0, ind0+nz-1) <- zero.print
	substr(x[i0],ind0+nz, nc) <- " "
    }
    if(nMark && !drop0trailing)# zero.print was only non-default
	return(x)

    ## else
    P0 <- function(...) paste(..., sep="")
    if(is.na(is.cmplx)) { ## find if 'x' is format from a *complex*
	ina <- is.na(x) | x == "NA"
	is.cmplx <-
	    if(all(ina)) FALSE
	    else length(grep("[0-9].*[-+][0-9].*i$", x)) > 0
    }
    if(is.cmplx) {
	## should be rare .. taking an easy route
	z.sp <- strsplit(sub("([0-9] *)([-+])( *[0-9])",
			     "\\1::\\2::\\3", x), "::", fixed=TRUE)
	z.im <- sapply(z.sp, `[[`, 3L)
	## drop ending 'i' (and later re-add it)
	has.i <- grep("i$", z.im)
	z.im[has.i] <- sub("i$", '', z.im[has.i])
	r <- lapply(list(sapply(z.sp, `[[`, 1L), z.im),
		    function(.)
		    prettyNum(.,
			      big.mark=big.mark, big.interval=big.interval,
			      small.mark=small.mark, small.interval=small.interval,
			      decimal.mark=decimal.mark, preserve.width=preserve.width,
			      zero.print=zero.print, drop0trailing=drop0trailing,
			      is.cmplx=FALSE, ...))
	r[[2]][has.i] <- P0(r[[2]][has.i], "i")
	return(paste(r[[1]], r[[2]], sep = sapply(z.sp, `[[`, 2L)))
    }
    preserve.width <- match.arg(preserve.width)
    x.sp <- strsplit(x, ".", fixed=TRUE)
    revStr <- function(cc)
	sapply(lapply(strsplit(cc,NULL), rev), paste, collapse="")
    B. <- sapply(x.sp, `[`, 1L)	    # Before "."
    A. <- sapply(x.sp, `[`, 2)	    # After  "." ; empty == NA
    if(any(iN <- is.na(A.))) A.[iN] <- ""

    if(nzchar(big.mark) &&
       length(i.big <- grep(P0("[0-9]{", big.interval + 1L,",}"), B.))
       ) { ## add 'big.mark' in decimals before "." :
	B.[i.big] <-
	    revStr(gsub(P0("([0-9]{",big.interval,"})\\B"),
			P0("\\1",big.mark), revStr(B.[i.big])))
    }
    if(nzchar(small.mark) &&
       length(i.sml <- grep(P0("[0-9]{", small.interval + 1L,",}"), A.))
       ) { ## add 'small.mark' in decimals after "."  -- but *not* trailing
	A.[i.sml] <- gsub(P0("([0-9]{",small.interval,"}\\B)"),
			  P0("\\1",small.mark), A.[i.sml])
    }
    if(drop0trailing) {
	a <- A.[!iN]
	if(length(hasE <- grep("e", a, fixed=TRUE))) {
	    a[ hasE] <- sub("e[+-]0+$", '', a[ hasE]) # also drop "e+00"
	    a[-hasE] <- sub("0+$",	'', a[-hasE])
	} else a <- sub("0+$", '', a)
	A.[!iN] <- a
	## iN := TRUE for those A.[]  which are ""
	iN <- !nzchar(A.)
    }
    ## extraneous trailing dec.marks: paste(B., A., sep = decimal.mark)
    A. <- P0(B., c(decimal.mark, "")[iN+ 1L], A.)
    if(preserve.width != "none") {
	nnc <- nchar(A., "c")
	d.len <- nnc - nchar(x, "c") # extra space added by 'marks' above
	if(any(ii <- d.len > 0L)) {
	    switch(preserve.width,
		   "individual" = {
		       ## drop initial blanks preserving original width
		       ## where possible:
		       A.[ii] <- sapply(which(ii), function(i)
					sub(sprintf("^ {1,%d}", d.len[i]), "",
					    A.[i]))
		   },
		   "common" = {
		       A. <- format(A., justify = "right")
		   })
	}
    }
    attributes(A.) <- attributes(x)
    class(A.) <- NULL
    A.
}
#  File src/library/base/R/frametools.R
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

subset.data.frame <- function (x, subset, select, drop = FALSE, ...)
{
    if(missing(subset))
	r <- TRUE
    else {
	e <- substitute(subset)
	r <- eval(e, x, parent.frame())
        if(!is.logical(r)) stop("'subset' must evaluate to logical")
	r <- r & !is.na(r)
    }
    if(missing(select))
	vars <- TRUE
    else {
	nl <- as.list(1L:ncol(x))
	names(nl) <- names(x)
	vars <- eval(substitute(select), nl, parent.frame())
    }
    x[r, vars, drop = drop]
}

subset <- function(x, ...) UseMethod("subset")

subset.default <- function(x, subset, ...) {
    if(!is.logical(subset)) stop("'subset' must be logical")
    x[subset & !is.na(subset)]
}

subset.matrix <- function(x, subset, select, drop = FALSE, ...)
{
    if(missing(select))
	vars <- TRUE
    else {
	nl <- as.list(1L:ncol(x))
	names(nl) <- colnames(x)
	vars <- eval(substitute(select), nl, parent.frame())
    }
    if(missing(subset)) subset <- TRUE
    else if(!is.logical(subset)) stop("'subset' must be logical")
    x[subset & !is.na(subset), vars, drop = drop]
}

### Notice use of non-syntactic variable name for the first argument
### This used to be "x", but then you couldn't create a variable
### called "x"...

transform.data.frame <- function (`_data`, ...)
{
    e <- eval(substitute(list(...)), `_data`, parent.frame())
    tags <- names(e)
    inx <- match(tags, names(`_data`))
    matched <- !is.na(inx)
    if (any(matched)) {
	`_data`[inx[matched]] <- e[matched]
	`_data` <- data.frame(`_data`)
    }
    if (!all(matched))  # add as separate arguments to get replication
	do.call("data.frame", c(list(`_data`), e[!matched]))
    else `_data`
}

transform <- function(`_data`,...) UseMethod("transform")

## Actually, I have no idea what to transform(), except dataframes.
## The default converts its argument to a dataframe and transforms
## that. This is probably marginally useful at best. --pd
transform.default <- function(`_data`,...)
    transform.data.frame(data.frame(`_data`),...)
#  File src/library/base/R/funprog.R
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

Reduce <-
function(f, x, init, right = FALSE, accumulate = FALSE)
{
    mis <- missing(init)
    len <- length(x)
    
    if(len == 0L) return(if(mis) NULL else init)
    
    f <- match.fun(f)

    ## Try to avoid the "obvious"
    ##   if(!mis) x <- if(right) c(x, init) else c(init, x)
    ## to be more efficient ...
    
    if(!is.vector(x) || is.object(x))
        x <- as.list(x)
    
    ind <- seq_len(len)

    if(mis) {
        if(right) {
            init <- x[[len]]
            ind <- ind[-len]
        }
        else {
            init <- x[[1L]]
            ind <- ind[-1L]
        }
    }
            
    if(!accumulate) {
        if(right) {
            for(i in rev(ind))
                init <- f(x[[i]], init)
        }
        else {
            for(i in ind)
                init <- f(init, x[[i]])
        }
        init
    }
    else {
        len <- length(ind) + 1L
        ## We need a list to accumulate the results as these do not 
        ## necessarily all have length one (e.g., reducing with c()).
        out <- vector("list", len)
        if(mis) {
            if(right) {
                out[[len]] <- init
                for(i in rev(ind)) {
                    init <- f(x[[i]], init)
                    out[[i]] <- init
                }
            } else {
                out[[1L]] <- init
                for(i in ind) {
                    init <- f(init, x[[i]])
                    out[[i]] <- init
                }
            }
        } else {
            if(right) {
                out[[len]] <- init
                for(i in rev(ind)) {
                    init <- f(x[[i]], init)
                    out[[i]] <- init
                }
            }
            else {
                for(i in ind) {
                    out[[i]] <- init
                    init <- f(init, x[[i]])
                }
                out[[len]] <- init
            }
        }
        ## If all results have length one, we can simplify.
        ## (Note that we do not simplify to arrays in case all results
        ## have a common length > 1.)
        if(all(sapply(out, length) == 1L))
            out <- unlist(out, recursive = FALSE)
        out
    }
}

Filter <-
function(f, x)
{
    ind <- as.logical(sapply(x, f))
    x[!is.na(ind) & ind]
}


Map <-
function(f, ...)
    mapply(f, ..., SIMPLIFY = FALSE)

Negate <-
function(f)
    function(...) ! match.fun(f)(...)

Position <-
function(f, x, right = FALSE, nomatch = NA_integer_)
{
    ind <- which(as.logical(sapply(x, f)))
    if(len <- length(ind))
        ind[if(right) len else 1L]
    else
        nomatch
}

Find <-
function(f, x, right = FALSE, nomatch = NULL)
{
    if((pos <- Position(f, x, right, nomatch = 0L)) > 0L)
        x[[pos]]
    else
        nomatch
}

identity <-
function(x)
    x
#  File src/library/base/R/get.R
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

get <-
    function (x, pos = -1, envir = as.environment(pos), mode = "any",
              inherits = TRUE)
    .Internal(get(x, envir, mode, inherits))

mget <- function(x, envir, mode = "any",
          ifnotfound= list(function(x)
	                stop(paste("value for '", x, "' not found", sep=""),
				call.=FALSE)),
          inherits = FALSE)
     .Internal(mget(x, envir, mode, ifnotfound, inherits))
#  File src/library/base/R/getenv.R
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

Sys.getenv <- function(x = NULL, unset = "")
{
    if (is.null(x)) {
        ## This presumes that '=' does not appear as part of the name
        ## of an environment variable.  That used to happen on Windows.
	x <- strsplit(.Internal(Sys.getenv(character(), "")), "=", fixed=TRUE)
	v <- n <- character(LEN <- length(x))
	for (i in 1L:LEN) {
	    n[i] <- x[[i]][1L]
	    v[i] <- paste(x[[i]][-1L], collapse = "=")
	}
	structure(v, names = n)[sort.list(n)]
    } else {
	structure(.Internal(Sys.getenv(as.character(x), as.character(unset))),
                  names = x)
    }
}

Sys.setenv <- function(...)
{
    x <- list(...)
    nm <- names(x)
    if(is.null(nm) || "" %in% nm)
        stop("all arguments must be named")
    .Internal(Sys.setenv(nm, as.character(unlist(x))))
}

Sys.unsetenv <- function(x) .Internal(Sys.unsetenv(as.character(x)))

Sys.getpid <- function() .Internal(Sys.getpid())
#  File src/library/base/R/gl.R
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

## gl function of GLIM
gl <- function (n, k, length = n*k, labels=1:n, ordered=FALSE)
    factor(rep(rep.int(1:n, rep.int(k,n)), length.out=length),
	   levels=1:n, labels=labels, ordered=ordered)
#  File src/library/base/R/grep.R
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

grep <-
function(pattern, x, ignore.case = FALSE, extended = TRUE, perl = FALSE,
         value = FALSE, fixed = FALSE, useBytes = FALSE, invert = FALSE)
{
    ## when value = TRUE we return names
    if(!is.character(x)) x <- structure(as.character(x), names=names(x))
    .Internal(grep(as.character(pattern), x, ignore.case, extended, value,
                   perl, fixed, useBytes, invert))
}

grepl <-
function(pattern, x, ignore.case = FALSE, extended = TRUE, perl = FALSE,
         fixed = FALSE, useBytes = FALSE)
{
    if(!is.character(x)) x <- as.character(x)
    .Internal(grepl(as.character(pattern), x, ignore.case, extended, FALSE,
                   perl, fixed, useBytes, FALSE))
}

sub <-
function(pattern, replacement, x, ignore.case = FALSE, extended = TRUE,
         perl = FALSE, fixed = FALSE, useBytes = FALSE)
{
    if (!is.character(x)) x <- as.character(x)
     .Internal(sub(as.character(pattern), as.character(replacement), x,
                  ignore.case, extended, perl, fixed, useBytes))
}

gsub <-
function(pattern, replacement, x, ignore.case = FALSE, extended = TRUE,
         perl = FALSE, fixed = FALSE, useBytes = FALSE)
{
    if (!is.character(x)) x <- as.character(x)
    .Internal(gsub(as.character(pattern), as.character(replacement), x,
                   ignore.case, extended, perl, fixed, useBytes))
}

regexpr <-
function(pattern, text, ignore.case = FALSE, extended = TRUE, perl = FALSE,
         fixed = FALSE, useBytes = FALSE)
    .Internal(regexpr(as.character(pattern), as.character(text),
                      ignore.case, extended, perl, fixed, useBytes))

gregexpr <-
function(pattern, text, ignore.case = FALSE, extended = TRUE, perl = FALSE,
         fixed = FALSE, useBytes = FALSE)
    .Internal(gregexpr(as.character(pattern), as.character(text),
                       ignore.case, extended, perl, fixed, useBytes))

agrep <-
function(pattern, x, ignore.case = FALSE, value = FALSE, max.distance = 0.1,
         useBytes = FALSE)
{
    pattern <- as.character(pattern)
    if(!is.character(x)) x <- as.character(x)
    ## behaves like == for NA pattern
    if (is.na(pattern)){
        if (value)
            return(structure(rep.int(NA_character_, length(x)),
                             names = names(x)))
        else
            return(rep.int(NA, length(x)))
    }

    if(!is.character(pattern) || length(pattern) != 1L || !nzchar(pattern))
        stop("'pattern' must be a non-empty character string")

    n <- nchar(pattern, "c")
    if(is.na(n)) stop("invalid multibyte string for 'pattern'")
    if(!is.list(max.distance)) {
        if(!is.numeric(max.distance) || (max.distance < 0))
            stop("'max.distance' must be non-negative")
        if(max.distance < 1)            # transform percentages
            max.distance <- ceiling(n * max.distance)
        max.insertions <- max.deletions <- max.substitutions <-
            max.distance
    } else {
        ## partial matching
        table <- c("all", "deletions", "insertions", "substitutions")
        ind <- pmatch(names(max.distance), table)
        if(any(is.na(ind)))
            warning("unknown match distance components ignored")
        max.distance <- max.distance[!is.na(ind)]
        names(max.distance) <- table[ind]
        ## sanity checks
        comps <- unlist(max.distance)
        if(!all(is.numeric(comps)) || any(comps < 0))
            stop("'max.distance' components must be non-negative")
        ## extract restrictions
        if(is.null(max.distance$all))
            max.distance$all <- 0.1
        max.insertions <- max.deletions <- max.substitutions <-
            max.distance$all
        if(!is.null(max.distance$deletions))
            max.deletions <- max.distance$deletions
        if(!is.null(max.distance$insertions))
            max.insertions <- max.distance$insertions
        if(!is.null(max.distance$substitutions))
            max.substitutions <- max.distance$substitutions
        max.distance <- max.distance$all
        ## transform percentages
        if(max.distance < 1)
            max.distance <- ceiling(n * max.distance)
        if(max.deletions < 1)
            max.deletions <- ceiling(n * max.deletions)
        if(max.insertions < 1)
            max.insertions <- ceiling(n * max.insertions)
        if(max.substitutions < 1)
            max.substitutions <- ceiling(n * max.substitutions)
    }

    .Internal(agrep(pattern, x, ignore.case, value, max.distance,
                    max.deletions, max.insertions, max.substitutions, useBytes))
}
#  File src/library/base/R/identical.R
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

identical <- function(x, y, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE)
    .Internal(identical(x,y, num.eq, single.NA, attrib.as.set))

isTRUE <- function(x) identical(TRUE, x)
#  File src/library/base/R/ifelse.R
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

ifelse <-
    function (test, yes, no)
{
    storage.mode(test) <- "logical"
    ans <- test
    nas <- is.na(test)
    if (any(test[!nas]))
        ans[test & !nas] <- rep(yes, length.out = length(ans))[test & !nas]
    if (any(!test[!nas]))
        ans[!test & !nas] <- rep(no, length.out = length(ans))[!test & !nas]
    ans[nas] <- NA
    ans
}
#  File src/library/base/R/interaction.R
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

### This is almost like the Primitive ":" for factors
### (that has no "drop = TRUE") --- it's not used anywhere in "standard R"
interaction <- function(..., drop = FALSE, sep = ".", lex.order = FALSE)
{
    args <- list(...)
    narg <- length(args)
    if (narg == 1L && is.list(args[[1L]])) {
	args <- args[[1L]]
	narg <- length(args)
    }
    for(i in narg:1L) {
        f <- args[[i]]
	if (!is.factor(f)) f <- factor(f)
	l <- levels(f)
        if1 <- as.integer(f) - 1L
        if(i != narg) {
            if(lex.order) {
                ll <- length(lvs)
                ans <- ans + ll * if1
                lvs <- ## as.vector(t(outer(l, lvs, paste, sep=sep)))
                    paste(rep(l, each= ll), rep(lvs, length(l)), sep=sep)
            } else {
                ans <- ans * length(l) + if1
                lvs <- ## as.vector(outer(l, lvs, paste, sep=sep))
                    paste(rep(l, length(lvs)), rep(lvs, each = length(l)), sep=sep)
            }
	    if(anyDuplicated(lvs)) { ## fix them up
		ulvs <- unique(lvs)
		while((i <- anyDuplicated(flv <- match(lvs, ulvs)))) {
		    lvs <- lvs[-i]
		    ans[ans+1L == i] <- match(flv[i], flv[1:(i-1)]) - 1L
		}
		lvs <- ulvs
	    }
        } else {
            ans <- if1
            lvs <- l
        }
    }
    ans <- structure(as.integer(ans+1L), levels=lvs, class = "factor")
    ans[ , drop=drop]
}
#  File src/library/base/R/is.R
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

is.vector <- function(x, mode="any") .Internal(is.vector(x,mode))

"is.na<-" <- function(x, value) UseMethod("is.na<-")

"is.na<-.default" <- function(x, value)
{
    x[value] <- NA
    x
}

is.primitive <- function(x)
    switch(typeof(x), "special" = , "builtin" = TRUE, FALSE)
#  File src/library/base/R/jitter.R
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

### Unimplemented Idea {for amount = NULL ?}
### Really "optimal" (e.g. for rug()), use a non-constant amount,
### e.g. use "d" = diff(xx)  BEFORE  taking min()...

jitter <- function(x, factor = 1, amount=NULL)
{
    if(length(x) == 0L)
	return(x)
    if(!is.numeric(x))
        stop("'x' must be numeric")
    z <- diff(r <- range(x[is.finite(x)]))
    if(z == 0) z <- abs(r[1L])
    if(z == 0) z <- 1

    if(is.null(amount)) {		# default: Find 'necessary' amount
	d <- diff(xx <- unique(sort.int(round(x, 3 - floor(log10(z))))))
	d <- if(length(d)) min(d) else if(xx!=0) xx/10 else z/10
	amount <- factor/5 * abs(d)
    } else if(amount == 0)		# only then: S compatibility
	amount <- factor * (z/50)

    x + stats::runif(length(x),  - amount, amount)
}
#  File src/library/base/R/kappa.R
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

kappa <- function(z, ...) UseMethod("kappa")

## Note that  all 4 Lapack version now work in the following
rcond <- function(x, norm = c("O","I","1"), triangular = FALSE, ...) {
    norm <- match.arg(norm)
    stopifnot(is.matrix(x))
    if({d <- dim(x); d[1L] != d[2L]})## non-square matrix -- use QR
        return(rcond(qr.R(qr(if(d[1L] < d[2L]) t(x) else x)), norm=norm, ...))

    ## x = square matrix :
    if(is.complex(x)) {
        if(triangular)
            .Call("La_ztrcon", x, norm, PACKAGE="base")
        else .Call("La_zgecon", x, norm, PACKAGE="base")
    }
    else {
        storage.mode(x) <- "double"
        if(triangular)
            .Call("La_dtrcon", x, norm, PACKAGE="base")
        else .Call("La_dgecon", x, norm, PACKAGE="base")
    }
}

kappa.default <- function(z, exact = FALSE,
                          norm = NULL, method = c("qr", "direct"), ...)
{
    method <- match.arg(method)
    z <- as.matrix(z)
    norm <- if(!is.null(norm)) match.arg(norm, c("2", "1","O", "I")) else "2"
    if(exact && norm == "2") {
        s <- svd(z, nu=0, nv=0)$d
        max(s)/min(s[s > 0])
    }
    else { ## exact = FALSE or norm in "1", "O", "I"
	if(exact)
	    warning(gettextf("norm '%s' currently always uses exact = FALSE",
			     norm))
        d <- dim(z)
        if(method == "qr" || d[1L] != d[2L])
	    kappa.qr(qr(if(d[1L] < d[2L]) t(z) else z),
		     exact=FALSE, norm=norm, ...)
        else kappa.tri(z, exact=FALSE, norm=norm, ...)
    }
}

kappa.lm <- function(z, ...) kappa.qr(z$qr, ...)

kappa.qr <- function(z, ...)
{
    qr <- z$qr
    R <- qr[1L:min(dim(qr)), , drop = FALSE]
    R[lower.tri(R)] <- 0
    kappa.tri(R, ...)
}

kappa.tri <- function(z, exact = FALSE, LINPACK = TRUE, norm=NULL, ...)
{
    if(exact) {
        stopifnot(is.null(norm) || identical("2", norm))
        kappa.default(z, exact = TRUE) ## using "2 - norm" !
    }
    else { ## norm is "1" ("O") or "I(nf)" :
	p <- nrow(z)
	if(p != ncol(z)) stop("triangular matrix should be square")
	if(is.null(norm)) norm <- "1"
	if(is.complex(z))
	    1/.Call("La_ztrcon", z, norm, PACKAGE="base")
	else if(LINPACK) {
	    if(norm == "I") # instead of "1" / "O"
		z <- t(z)
	    ##	dtrco  *differs* from Lapack's dtrcon() quite a bit
	    ## even though dtrco's doc also say to compute the
	    ## 1-norm reciprocal condition
	    1 / .Fortran("dtrco",
			 as.double(z),
			 p,
			 p,
			 k = double(1),
			 double(p),
			 1L,
			 PACKAGE="base")$k
	}
	else { ## Lapack
	    storage.mode(z) <- "double"
	    1/.Call("La_dtrcon", z, norm, PACKAGE="base")
	}
    }
}
#  File src/library/base/R/kronecker.R
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

kronecker <- function (X, Y, FUN = "*", make.dimnames = FALSE, ...)
{
    X <- as.array(X)
    Y <- as.array(Y)
    if (make.dimnames) {
	dnx <- dimnames(X)
	dny <- dimnames(Y)
    }
    dX <- dim(X)
    dY <- dim(Y)
    ld <- length(dX) - length(dY)
    if (ld < 0L)
	dX <- dim(X) <- c(dX, rep.int(1, -ld))
    else if (ld > 0L)
	dY <- dim(Y) <- c(dY, rep.int(1, ld))
    opobj <- outer(X, Y, FUN, ...)
    dp <- as.vector(t(matrix(1L:(2*length(dX)), ncol = 2)[, 2:1]))
    opobj <- aperm(opobj, dp)
    dim(opobj) <- dX * dY

    if (make.dimnames && !(is.null(dnx) && is.null(dny))) {
	if (is.null(dnx))
	    dnx <- vector("list", length(dX))
	else if (ld < 0L)
	    dnx <- c(dnx, vector("list", -ld))
	tmp <- which(sapply(dnx, is.null))
	dnx[tmp] <- lapply(tmp, function(i) rep.int("", dX[i]))

	if (is.null(dny))
	    dny <- vector("list", length(dY))
	else if (ld > 0)
	    dny <- c(dny, vector("list", ld))
	tmp <- which(sapply(dny, is.null))
	dny[tmp] <- lapply(tmp, function(i) rep.int("", dY[i]))

	k <- length(dim(opobj))
	dno <- vector("list", k)
	for (i in 1L:k) {
	    tmp <- outer(dnx[[i]], dny[[i]], FUN="paste", sep=":")
	    dno[[i]] <- as.vector(t(tmp))
	}
	dimnames(opobj) <- dno
    }
    opobj
}

## Binary operator, hence don't simply do "%x%" <- kronecker.
"%x%" <- function(X, Y) kronecker(X, Y)
#  File src/library/base/R/labels.R
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

labels <- function(object, ...) UseMethod("labels")

labels.default <- function(object, ...)
{
    if(length(d <- dim(object))) {	# array or data frame
	nt <- dimnames(object)
	if(is.null(nt)) nt <- vector("list", length(d))
	for(i in seq_along(d))
	    if(!length(nt[[i]])) nt[[i]] <- as.character(seq_len(d[i]))
    } else {
	nt <- names(object)
	if(!length(nt)) nt <- as.character(seq_along(object))
    }
    nt
}
#  File src/library/base/R/lapply.R
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

lapply <- function (X, FUN, ...)
{
    FUN <- match.fun(FUN)
    ## internal code handles all vector types, including expressions
    ## However, it would be OK to have attributes which is.vector
    ## disallows.
    if(!is.vector(X) || is.object(X)) X <- as.list(X)
    .Internal(lapply(X, FUN))
}

rapply <-
    function(object, f, classes = "ANY", deflt = NULL,
             how = c("unlist", "replace", "list"), ...)
{
    if(typeof(object) != "list")
        stop("'object' must be a list")
    how <- match.arg(how)
    res <- .Internal(rapply(object, f, classes, deflt, how))
    if(how == "unlist") unlist(res, recursive = TRUE) else res
}
#  File src/library/base/R/lazyload.R
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

## note that there is a version in ../baseloader.R that should be kept in step
lazyLoad <- function(filebase, envir = parent.frame(), filter)
{
    ##
    ## bootstrapping definitions so we can load base
    ## - not that this version is actually used to load base
    ##
    glue <- function (..., sep = " ", collapse = NULL)
        .Internal(paste(list(...), sep, collapse))
    readRDS <- function (file) {
        halt <- function (message) .Internal(stop(TRUE, message))
        gzfile <- function (description, open)
            .Internal(gzfile(description, open, "", 6))
        close <- function (con) .Internal(close(con, "rw"))
        if (! is.character(file)) halt("bad file name")
        con <- gzfile(file, "rb")
        on.exit(close(con))
        .Internal(unserializeFromConn(con, baseenv()))
    }
    `parent.env<-` <-
        function (env, value) .Internal(`parent.env<-`(env, value))
    existsInFrame <- function (x, env) .Internal(exists(x, env, "any", FALSE))
    getFromFrame <- function (x, env) .Internal(get(x, env, "any", FALSE))
    set <- function (x, value, env) .Internal(assign(x, value, env, FALSE))
    environment <- function () .Internal(environment(NULL))
    mkenv <- function() .Internal(new.env(TRUE, baseenv(), 29L))

    ##
    ## main body
    ##
    mapfile <- glue(filebase, "rdx", sep = ".")
    datafile <- glue(filebase, "rdb", sep = ".")
    env <- mkenv()
    map <- readRDS(mapfile)
    vars <- names(map$variables)
    rvars <- names(map$references)
    compressed <- map$compressed
    for (i in seq_along(rvars))
        set(rvars[i], map$references[[i]], env)
    envenv <- mkenv()
    envhook <- function(n) {
        if (existsInFrame(n, envenv))
            getFromFrame(n, envenv)
        else {
            e <- mkenv()
            set(n, e, envenv)           # MUST do this immediately
            key <- getFromFrame(n, env)
            data <- lazyLoadDBfetch(key, datafile, compressed, envhook)
            ## comment from r41494
            ## modified the loading of old environments, so that those
            ## serialized with parent.env NULL are loaded with the
            ## parent.env=emptyenv(); and yes an alternative would have been
            ## baseenv(), but that was seldom the intention of folks that
            ## set the environment to NULL.
            if (is.null(data$enclos))
                parent.env(e) <- emptyenv()
            else
                parent.env(e) <- data$enclos
            vars <- names(data$bindings)
            for (i in seq_along(vars))
                set(vars[i], data$bindings[[i]], e)
            e
        }
    }
    if (!missing(filter)) {
        use <- filter(vars)
        vars <- vars[use]
        vals <- map$variables[use]
        use <- NULL
    } else
        vals <-  map$variables

    expr <- quote(lazyLoadDBfetch(key, datafile, compressed, envhook))
    this <- environment()
    .Internal(makeLazy(vars, vals, expr, this, envir))

    ## reduce memory use
    map <- NULL
    vars <- NULL
    vals <- NULL
    rvars <- NULL
    mapfile <- NULL
    readRDS <- NULL
}
#  File src/library/base/R/library.R
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

testPlatformEquivalence <- function(built, run)
{
    ## args are "cpu-vendor-os", but os might be 'linux-gnu'!
    ## remove vendor field
    built <- gsub("([^-]*)-([^-]*)-(.*)", "\\1-\\3", built)
    run <- gsub("([^-]*)-([^-]*)-(.*)", "\\1-\\3", run)
    ## Mac OS X supports multiple CPUs by using 'universal' binaries
    if (length(grep("^universal-darwin", built)) && nzchar(.Platform$r_arch))
        built <- sub("^universal", R.version$arch, built)
    ## allow for small mismatches, e.g. OS version number and i686 vs i586.
    length(agrep(built, run)) > 0
}

library <-
function(package, help, pos = 2, lib.loc = NULL, character.only = FALSE,
         logical.return = FALSE, warn.conflicts = TRUE,
         keep.source = getOption("keep.source.pkgs"),
         verbose = getOption("verbose"))
{
    paste0 <- function(...) paste(..., sep="")
    testRversion <- function(pkgInfo, pkgname, pkgpath)
    {
        current <- getRversion()
        ## depends on R version?
        ## If installed >= 2.7.0 it will have Rdepends2
        ## Otherwise Rdepends, which this NULL or of length 1
        ## (installed < 2.6.0 only) or
        ## length 3 with valid components was checked at INSTALL time.
       if(length(Rdeps <- pkgInfo$Rdepends2)) {
            for(dep in Rdeps)
                if(length(dep) > 1L) {
                    target <- as.numeric_version(dep$version)
                    res <- eval(parse(text=paste("current", dep$op, "target")))
                    if(!res)
                        stop(gettextf("This is R %s, package '%s' needs %s %s",
                                      current, pkgname, dep$op, target),
                             call. = FALSE, domain = NA)
                }
        } else if(length(Rdeps <- pkgInfo$Rdepends) > 1L) {
            target <- as.numeric_version(Rdeps$version)
            res <- eval(parse(text=paste("current", Rdeps$op, "target")))
            if(!res)
                stop(gettextf("This is R %s, package '%s' needs %s %s",
                             current, pkgname, Rdeps$op, target),
                     call. = FALSE, domain = NA)
        }
        ## which version was this package built under?
        if(!is.null(built <- pkgInfo$Built)) {
            ## must be >= 2.0.0
            R_version_built_under <- as.numeric_version(built$R)
            if(R_version_built_under < "2.0.0")
                stop(gettextf("package '%s' was built before R 2.0.0: please re-install it",
                              pkgname), call. = FALSE, domain = NA)
            ## warn if later than this version
            if(R_version_built_under > current)
                warning(gettextf("package '%s' was built under R version %s",
                                 pkgname, as.character(built$R)),
                        call. = FALSE, domain = NA)
            ## warn if < 2.10.0, when the help format changed.
            if(R_version_built_under < "2.10.0") {
                if(.Platform$OS.type == "windows")
                    warning(gettextf("package '%s' was built under R version %s and help will not work correctly\nPlease re-install it",
                                     pkgname, as.character(built$R)),
                            call. = FALSE, domain = NA)
                else
                    warning(gettextf("package '%s' was built under R version %s and help may not work correctly",
                                     pkgname, as.character(built$R)),
                        call. = FALSE, domain = NA)
            } else {
                ## check that this was not under pre-2.10.0, but beware
                ## of bootstrapping standard packages
                if(file.exists(file.path(pkgpath, "help")) &&
                   !file.exists(file.path(pkgpath, "help", "paths.rds")))
                    warning(gettextf("package '%s' claims to be built under R version %s but is missing some help files and needs to be re-installed",
                                     pkgname, as.character(built$R)),
                            call. = FALSE, domain = NA)
            }
            if(.Platform$OS.type == "unix") {
                platform <- built$Platform
                r_arch <- .Platform$r_arch
                ## allow mismatches if r_arch is in use, e.g.
                ## i386-gnu-linux vs x86-gnu-linux depending on
                ## build system.
		if(!nzchar(r_arch) && length(grep("\\w", platform)) &&
                   !testPlatformEquivalence(platform, R.version$platform))
                    stop(gettextf("package '%s' was built for %s",
                                  pkgname, platform),
                         call. = FALSE, domain = NA)
                ## if using r_arch subdirs, check for presence
                if(nzchar(r_arch)
                   && file.exists(file.path(pkgpath, "libs"))
                   && !file.exists(file.path(pkgpath, "libs", r_arch)))
                    stop(gettextf("package '%s' is not installed for 'arch=%s'",
                                  pkgname, r_arch),
                         call. = FALSE, domain = NA)

            }
        }
        else
            stop(gettextf("package '%s' has not been installed properly\n",
                          pkgname),
                 gettext("See the Note in ?library"),
                 call. = FALSE, domain = NA)
    }

    checkNoGenerics <- function(env, pkg)
    {
        nenv <- env
        ns <- .Internal(getRegisteredNamespace(as.name(pkg)))
        if(!is.null(ns)) nenv <- asNamespace(ns)
        if (exists(".noGenerics", envir = nenv, inherits = FALSE))
            TRUE
        else {
            ## A package will have created a generic
            ## only if it has created a formal method.
            length(objects(env, pattern="^\\.__[MT]", all.names=TRUE)) == 0L
        }
    }

    checkConflicts <- function(package, pkgname, pkgpath, nogenerics, env)
    {
        dont.mind <- c("last.dump", "last.warning", ".Last.value",
                       ".Random.seed", ".First.lib", ".Last.lib",
                       ".packageName", ".noGenerics", ".required",
                       ".no_S3_generics")
        sp <- search()
        lib.pos <- match(pkgname, sp)
        ## ignore generics not defined for the package
        ob <- objects(lib.pos, all.names = TRUE)
        if(!nogenerics) {
            ##  Exclude generics that are consistent with implicit generic
            ## from another pacakge.  A better test would be to move this
            ## down into the loop and test against specific other package name
            ## but subtle conflicts like that are likely to be found elsewhere
            these <- objects(lib.pos, all.names = TRUE)
            these <- these[substr(these, 1L, 6L) == ".__T__"]
            gen <- gsub(".__T__(.*):([^:]+)", "\\1", these)
            from <- gsub(".__T__(.*):([^:]+)", "\\2", these)
            gen <- gen[from != package]
            ob <- ob[!(ob %in% gen)]
        }
        fst <- TRUE
	ipos <- seq_along(sp)[-c(lib.pos,
				 match(c("Autoloads", "CheckExEnv"), sp, 0L))]
        for (i in ipos) {
            obj.same <- match(objects(i, all.names = TRUE), ob, nomatch = 0L)
            if (any(obj.same > 0)) {
                same <- ob[obj.same]
                same <- same[!(same %in% dont.mind)]
                Classobjs <- grep("^\\.__", same)
                if(length(Classobjs)) same <- same[-Classobjs]
                ## report only objects which are both functions or
                ## both non-functions.
		same.isFn <- function(where)
		    sapply(same, exists,
                           where = where, mode = "function", inherits = FALSE)
		same <- same[same.isFn(i) == same.isFn(lib.pos)]
                ## if a package imports, and re-exports, there's no problem
		if(length(same))
		    same <- same[sapply(same, function(.)
					!identical(get(., i),
						   get(., lib.pos)))]
                if(length(same)) {
                    if (fst) {
                        fst <- FALSE
                        packageStartupMessage(gettextf("\nAttaching package: '%s'\n",
                                                       package),
                                              domain = NA)
                    }
		    packageStartupMessage(paste(
				"\n\tThe following object(s) are masked",
				if (i < lib.pos) "_by_" else "from", sp[i],
				":\n\n\t", paste(same, collapse=",\n\t "), "\n"))
                }
            }
        }
    }

    runUserHook <- function(pkgname, pkgpath) {
        hook <- getHook(packageEvent(pkgname, "attach")) # might be list()
        for(fun in hook) try(fun(pkgname, pkgpath))
    }

    bindTranslations <- function(pkgname, pkgpath)
    {
        popath <- file.path(pkgpath, "po")
        if(!file.exists(popath)) return()
        bindtextdomain(pkgname, popath)
        bindtextdomain(paste("R", pkgname, sep="-"), popath)
    }

    if(!missing(package)) {
        if (is.null(lib.loc)) lib.loc <- .libPaths()
        ## remove any non-existent directories
        lib.loc <- lib.loc[file.info(lib.loc)$isdir %in% TRUE]

	if(!character.only)
	    package <- as.character(substitute(package))
        if(length(package) != 1L)
            stop("'package' must be of length 1")
        if(is.na(package) || (package == ""))
            stop("invalid package name")

	pkgname <- paste("package", package, sep = ":")
	newpackage <- is.na(match(pkgname, search()))
	if(newpackage) {
            ## Check for the methods package before attaching this
            ## package.
            ## Only if it is _already_ here do we do cacheMetaData.
            ## The methods package caches all other libs when it is
            ## attached.

            pkgpath <- .find.package(package, lib.loc, quiet = TRUE,
                                     verbose = verbose)
            if(length(pkgpath) == 0L) {
                txt <- if(length(lib.loc))
                    gettextf("there is no package called '%s'", package)
                else
                    gettext("no library trees found in 'lib.loc'")
                if(logical.return) {
                    warning(txt, domain = NA)
		    return(FALSE)
		} else stop(txt, domain = NA)
            }
            which.lib.loc <- dirname(pkgpath)
            pfile <- system.file("Meta", "package.rds", package = package,
                                 lib.loc = which.lib.loc)
            if(!nzchar(pfile))
            	stop(gettextf("'%s' is not a valid installed package",
                              package), domain = NA)
            pkgInfo <- .readRDS(pfile)
            testRversion(pkgInfo, package, pkgpath)

            ## The check for inconsistent naming is now in .find.package

            if(is.character(pos)) {
                npos <- match(pos, search())
                if(is.na(npos)) {
                    warning(gettextf("'%s' not found on search path, using pos = 2", pos), domain = NA)
                    pos <- 2
                } else pos <- npos
            }
            .getRequiredPackages2(pkgInfo)
#                .getRequiredPackages2(pkgInfo, lib.loc = lib.loc)
            ## If the name space mechanism is available and the package
            ## has a name space, then the name space loading mechanism
            ## takes over.
            if (packageHasNamespace(package, which.lib.loc)) {
                ## this checks for 'depends on methods and installed < 2.4.0'
                tt <- try({
                    ns <- loadNamespace(package, c(which.lib.loc, lib.loc),
                                        keep.source = keep.source)
                    dataPath <- file.path(which.lib.loc, package, "data")
                    env <- attachNamespace(ns, pos = pos,
                                           dataPath = dataPath)
                })
                if (inherits(tt, "try-error"))
                    if (logical.return)
                        return(FALSE)
                    else stop(gettextf("package/namespace load failed for '%s'",
                                       package),
                              call. = FALSE, domain = NA)
                else {
                    on.exit(do.call("detach", list(name = pkgname)))
                    ## If there are S4 generics then the package should
                    ## depend on methods
                    nogenerics <-
                        !.isMethodsDispatchOn() || checkNoGenerics(env, package)
                    if(warn.conflicts &&
                       !exists(".conflicts.OK", envir = env, inherits = FALSE))
                        checkConflicts(package, pkgname, pkgpath,
                                       nogenerics, ns)
                    runUserHook(package, pkgpath)
                    on.exit()
                    if (logical.return)
                        return(TRUE)
                    else
                        return(invisible(.packages()))
                }
            }

            ## non-namespace branch
            dependsMethods <- "methods" %in% names(pkgInfo$Depends)
            if(dependsMethods && pkgInfo$Built$R < "2.4.0")
                stop("package was installed prior to 2.4.0 and must be re-installed")
            codeFile <- file.path(which.lib.loc, package, "R", package)
            ## create environment (not attached yet)
            loadenv <- new.env(hash = TRUE, parent = .GlobalEnv)
            ## save the package name in the environment
            assign(".packageName", package, envir = loadenv)
            ## source file into loadenv
            if(file.exists(codeFile)) {
                res <- try(sys.source(codeFile, loadenv,
                                      keep.source = keep.source))
                if(inherits(res, "try-error"))
                    stop(gettextf("unable to load R code in package '%s'",
                                  package),
                         call. = FALSE, domain = NA)
            } else if(verbose)
                warning(gettextf("package '%s' contains no R code",
                                 package), domain = NA)
            ## lazy-load data sets if required
            dbbase <- file.path(which.lib.loc, package, "data", "Rdata")
            if(file.exists(paste0(dbbase, ".rdb")))
                lazyLoad(dbbase, loadenv)
            ## lazy-load a sysdata database if present
            dbbase <- file.path(which.lib.loc, package, "R", "sysdata")
            if(file.exists(paste0(dbbase, ".rdb")))
                lazyLoad(dbbase, loadenv)
            ## now transfer contents of loadenv to an attached frame
            env <- attach(NULL, pos = pos, name = pkgname)
            ## detach does not allow character vector args
            on.exit(do.call("detach", list(name = pkgname)))
            attr(env, "path") <- file.path(which.lib.loc, package)
            ## the actual copy has to be done by C code to avoid forcing
            ## promises that might have been created using delayedAssign().
            .Internal(lib.fixup(loadenv, env))

            ## Do this before we use any code from the package
            bindTranslations(package, pkgpath)

            ## run .First.lib
            if(exists(".First.lib", mode = "function",
                      envir = env, inherits = FALSE)) {
                firstlib <- get(".First.lib", mode = "function",
                                envir = env, inherits = FALSE)
                tt<- try(firstlib(which.lib.loc, package))
                if(inherits(tt, "try-error"))
                    if (logical.return) return(FALSE)
                    else stop(gettextf(".First.lib failed for '%s'",
                                       package), domain = NA)
            }
            if(!is.null(firstlib <- getOption(".First.lib")[[package]])) {
                tt<- try(firstlib(which.lib.loc, package))
                if(inherits(tt, "try-error"))
                    if (logical.return) return(FALSE)
                    else stop(gettextf(".First.lib failed for '%s'",
                                       package), domain = NA)
            }
            ## If there are generics then the package should
            ## depend on methods and so have turned methods dispatch on.
            nogenerics <-
                !.isMethodsDispatchOn() || checkNoGenerics(env, package)
            if(warn.conflicts &&
               !exists(".conflicts.OK", envir = env, inherits = FALSE))
                checkConflicts(package, pkgname, pkgpath, nogenerics, env)

            if(!nogenerics)
                methods::cacheMetaData(env, TRUE, searchWhere = .GlobalEnv)
            runUserHook(package, pkgpath)
            on.exit()
	}
	if (verbose && !newpackage)
            warning(gettextf("package '%s' already present in search()",
                             package), domain = NA)

    }
    else if(!missing(help)) {
	if(!character.only)
	    help <- as.character(substitute(help))
        pkgName <- help[1L]              # only give help on one package
        pkgPath <- .find.package(pkgName, lib.loc, verbose = verbose)
        docFiles <- c(file.path(pkgPath, "Meta", "package.rds"),
                      file.path(pkgPath, "INDEX"))
        if(file.exists(vignetteIndexRDS <-
                       file.path(pkgPath, "Meta", "vignette.rds")))
            docFiles <- c(docFiles, vignetteIndexRDS)
        pkgInfo <- vector(length = 3L, mode = "list")
        readDocFile <- function(f) {
            if(basename(f) %in% "package.rds") {
                txt <- .readRDS(f)$DESCRIPTION
                if("Encoding" %in% names(txt)) {
                    to <- if(Sys.getlocale("LC_CTYPE") == "C") "ASCII//TRANSLIT"else ""
                    tmp <- try(iconv(txt, from=txt["Encoding"], to=to))
                    if(!inherits(tmp, "try-error"))
                        txt <- tmp
                    else
                        warning("'DESCRIPTION' has 'Encoding' field and re-encoding is not possible", call.=FALSE)
                }
                nm <- paste0(names(txt), ":")
                formatDL(nm, txt, indent = max(nchar(nm, "w")) + 3)
            } else if(basename(f) %in% "vignette.rds") {
                txt <- .readRDS(f)
                ## New-style vignette indices are data frames with more
                ## info than just the base name of the PDF file and the
                ## title.  For such an index, we give the names of the
                ## vignettes, their titles, and indicate whether PDFs
                ## are available.
                ## The index might have zero rows.
                if(is.data.frame(txt) && nrow(txt))
                    cbind(basename(gsub("\\.[[:alpha:]]+$", "",
                                        txt$File)),
                          paste(txt$Title,
                                paste0(rep.int("(source", NROW(txt)),
                                       ifelse(txt$PDF != "",
                                              ", pdf",
                                              ""),
                                       ")")))
                else NULL
            } else
                readLines(f)
        }
        for(i in which(file.exists(docFiles)))
            pkgInfo[[i]] <- readDocFile(docFiles[i])
        y <- list(name = pkgName, path = pkgPath, info = pkgInfo)
        class(y) <- "packageInfo"
        return(y)
    }
    else {
	## library():
        if(is.null(lib.loc))
            lib.loc <- .libPaths()
        db <- matrix(character(0L), nrow = 0L, ncol = 3L)
        nopkgs <- character(0L)

        for(lib in lib.loc) {
            a <- .packages(all.available = TRUE, lib.loc = lib)
            for(i in sort(a)) {
                ## All packages installed under 2.0.0 should have
                ## 'package.rds' but we have not checked.
                file <- system.file("Meta", "package.rds", package = i,
                                    lib.loc = lib)
                title <- if(file != "") {
                    txt <- .readRDS(file)
                    if(is.list(txt)) txt <- txt$DESCRIPTION
                    ## we may need to re-encode here.
                    if("Encoding" %in% names(txt)) {
                        to <- if(Sys.getlocale("LC_CTYPE") == "C") "ASCII//TRANSLIT" else ""
                        tmp <- try(iconv(txt, txt["Encoding"], to, "?"))
                        if(!inherits(tmp, "try-error"))
                            txt <- tmp
                        else
                            warning("'DESCRIPTION' has 'Encoding' field and re-encoding is not possible", call.=FALSE)
                    }
                    txt["Title"]
                } else NA
                if(is.na(title))
                    title <- " ** No title available ** "
                db <- rbind(db, cbind(i, lib, title))
            }
            if(length(a) == 0L)
                nopkgs <- c(nopkgs, lib)
        }
        dimnames(db) <- list(NULL, c("Package", "LibPath", "Title"))
        if(length(nopkgs) && !missing(lib.loc)) {
            pkglist <- paste(sQuote(nopkgs), collapse = ", ")
            msg <- sprintf(ngettext(length(nopkgs),
                                    "library %s contains no packages",
                                    "libraries %s contain no packages"),
                           pkglist)
            warning(msg, domain=NA)
        }

        y <- list(header = NULL, results = db, footer = NULL)
        class(y) <- "libraryIQR"
        return(y)
    }

    if (logical.return)
	TRUE
    else invisible(.packages())
}

print.libraryIQR <-
function(x, ...)
{
    db <- x$results
    ## Split according to LibPath, preserving order of libraries.
    libs <- db[, "LibPath"]
    libs <- factor(libs, levels=unique(libs))
    out <- if(nrow(db) == 0)
        NULL
    else lapply(split(1 : nrow(db), libs),
                function(ind) db[ind, c("Package", "Title"),
                                 drop = FALSE])
    outFile <- tempfile("RlibraryIQR")
    outConn <- file(outFile, open = "w")
    first <- TRUE
    for(lib in names(out)) {
        writeLines(gettextf("%sPackages in library '%s':\n",
                            ifelse(first, "", "\n"),
                            lib),
                   outConn)
        writeLines(formatDL(out[[lib]][, "Package"],
                            out[[lib]][, "Title"]),
                   outConn)
        first <- FALSE
    }
    if(first) {
        close(outConn)
        unlink(outFile)
        message("no packages found")
    }
    else {
        if(!is.null(x$footer))
            writeLines(c("\n", x$footer), outConn)
        close(outConn)
        file.show(outFile, delete.file = TRUE,
                  title = gettext("R packages available"))
    }
    invisible(x)
}

library.dynam <-
function(chname, package = NULL, lib.loc = NULL,
         verbose = getOption("verbose"),
         file.ext = .Platform$dynlib.ext, ...)
{
    dll_list <- .dynLibs()

    if(missing(chname) || (nc_chname <- nchar(chname, "c")) == 0)
        return(dll_list)

    ## Be defensive about possible system-specific extension for shared
    ## libraries, although the docs clearly say they should not be
    ## added.
    nc_file_ext <- nchar(file.ext, "c")
    if(substr(chname, nc_chname - nc_file_ext + 1L, nc_chname)
       == file.ext)
        chname <- substr(chname, 1L, nc_chname - nc_file_ext)

    for(pkg in .find.package(package, lib.loc, verbose = verbose)) {
        DLLpath <- if(nzchar(.Platform$r_arch))
                file.path(pkg, "libs", .Platform$r_arch)
	else    file.path(pkg, "libs")
        file <- file.path(DLLpath, paste(chname, file.ext, sep = ""))
        if(file.exists(file)) break else file <- ""
    }
    if(file == "")
        stop(gettextf("shared library '%s' not found", chname), domain = NA)
    ind <- sapply(dll_list, function(x) x[["path"]] == file)
    if(length(ind) && any(ind)) {
        if(verbose)
            message(gettextf("shared library '%s' already loaded", chname),
                    domain = NA)
        return(invisible(dll_list[[ seq_along(dll_list)[ind] ]]))
    }
    if(.Platform$OS.type == "windows") {
        ## Make it possible to find other DLLs in the same place as
        ## @code{file}, so that e.g. binary packages can conveniently
        ## provide possibly missing DLL dependencies in this place
        ## (without having to bypass the default package dynload
        ## mechanism).  Note that this only works under Windows, and a
        ## more general solution will have to be found eventually.
        ##
        ## 2.7.0: there's a more general mechanism in DLLpath=,
        ## so not clear if this is still needed.
        PATH <- Sys.getenv("PATH")
        Sys.setenv(PATH = paste(gsub("/", "\\\\", DLLpath), PATH, sep=";"))
        on.exit(Sys.setenv(PATH = PATH))
    }
    if(verbose)
        message(gettextf("now dyn.load(\"%s\") ...", file), domain = NA)
    dll <- if("DLLpath" %in% names(list(...))) dyn.load(file, ...)
    else dyn.load(file, DLLpath = DLLpath, ...)
    .dynLibs(c(dll_list, list(dll)))
    invisible(dll)
}

library.dynam.unload <-
function(chname, libpath, verbose = getOption("verbose"),
         file.ext = .Platform$dynlib.ext)
{
    dll_list <- .dynLibs()

    if(missing(chname) || (nc_chname <- nchar(chname, "c")) == 0)
        stop("no shared library was specified")

    ## Be defensive about possible system-specific extension for shared
    ## libraries, although the docs clearly say they should not be
    ## added.
    nc_file_ext <- nchar(file.ext, "c")
    if(substr(chname, nc_chname - nc_file_ext + 1L, nc_chname)
       == file.ext)
        chname <- substr(chname, 1L, nc_chname - nc_file_ext)

     file <- if(nzchar(.Platform$r_arch))
             file.path(libpath, "libs", .Platform$r_arch,
                       paste(chname, file.ext, sep = ""))
     else    file.path(libpath, "libs",
                       paste(chname, file.ext, sep = ""))

    pos <- which(sapply(dll_list, function(x) x[["path"]] == file))
    if(!length(pos))
        stop(gettextf("shared library '%s' was not loaded", chname),
             domain = NA)

    if(!file.exists(file))
        stop(gettextf("shared library '%s' not found", chname), domain = NA)
    if(verbose)
        message(gettextf("now dyn.unload(\"%s\") ...", file), domain = NA)
    dyn.unload(file)
    .dynLibs(dll_list[-pos])
    invisible(dll_list[[pos]])
}

require <-
function(package, lib.loc = NULL, quietly = FALSE, warn.conflicts = TRUE,
         keep.source = getOption("keep.source.pkgs"),
         character.only = FALSE, save = TRUE)
{
    if( !character.only )
        package <- as.character(substitute(package)) # allowing "require(eda)"
    loaded <- paste("package", package, sep = ":") %in% search()

    if (!loaded) {
	if (!quietly)
            packageStartupMessage(gettextf("Loading required package: %s",
                                           package), domain = NA)
	value <- library(package, lib.loc = lib.loc, character.only = TRUE,
                         logical.return = TRUE,
                         warn.conflicts = warn.conflicts,
                         keep.source = keep.source)
    } else value <- TRUE

    if(identical(save, FALSE)) {}
    else {
        ## update the ".required" variable
        if(identical(save, TRUE)) {
            save <- topenv(parent.frame())
            ## (a package namespace, topLevelEnvironment option or
            ## .GlobalEnv)
            if(identical(save, .GlobalEnv)) {
                ## try to detect call from .First.lib in  a package
                ## <FIXME>
                ## Although the docs have long and perhaps always had
                ##   .First.lib(libname, pkgname)
                ## the majority of CRAN packages seems to use arguments
                ## 'lib' and 'pkg'.
                objectsInParentFrame <- sort(objects(parent.frame()))
                if(identical(sort(c("libname", "pkgname")),
                             objectsInParentFrame))
                    save <-
                        as.environment(paste("package:",
                                             get("pkgname",
                                                 parent.frame()),
                                             sep = ""))
                else if(identical(sort(c("lib", "pkg")),
                                  objectsInParentFrame))
                    save <-
                        as.environment(paste("package:",
                                             get("pkg",
                                                 parent.frame()),
                                             sep = ""))
                ## </FIXME>
                ## else either from prompt or in the source for install
                ## with saved image ?
            }
        }
        else
            save <- as.environment(save)
        hasDotRequired <- exists(".required", save, inherits=FALSE)
        if(!isNamespace(save) || hasDotRequired) { ## so assignment allowed
            if(hasDotRequired)
                packages <- unique(c(package, get(".required", save)))
            else
                packages <- package
            assign(".required", packages, save)
        }
    }
    invisible(value)
}

.packages <- function(all.available = FALSE, lib.loc = NULL)
{
    if(is.null(lib.loc))
        lib.loc <- .libPaths()
    if(all.available) {
	ans <- character(0L)
        for(lib in lib.loc[file.exists(lib.loc)]) {
            a <- list.files(lib, all.files = FALSE, full.names = FALSE)
            pfile <- file.path(lib, a, "Meta", "package.rds")
            ans <- c(ans, a[file.exists(pfile)])
        }
        return(unique(ans))
    } ## else
    s <- search()
    return(invisible(substring(s[substr(s, 1L, 8L) == "package:"], 9)))
}

.path.package <- function(package = NULL, quiet = FALSE)
{
    if(is.null(package)) package <- .packages()
    if(length(package) == 0L) return(character(0L))
    s <- search()
    searchpaths <-
        lapply(seq_along(s), function(i) attr(as.environment(i), "path"))
    searchpaths[[length(s)]] <- system.file()
    pkgs <- paste("package", package, sep = ":")
    pos <- match(pkgs, s)
    if(any(m <- is.na(pos))) {
        if(!quiet) {
            if(all(m))
                stop("none of the packages are loaded")
            else
                warning(sprintf(ngettext(as.integer(sum(m)),
                                         "package %s is not loaded",
                                         "packages %s are not loaded"),
                                paste(package[m], collapse=", ")),
                        domain = NA)
        }
        pos <- pos[!m]
    }
    unlist(searchpaths[pos], use.names = FALSE)
}

## As from 2.9.0 ignore versioned installs
.find.package <-
function(package = NULL, lib.loc = NULL, quiet = FALSE,
         verbose = getOption("verbose"))
{
    if(is.null(package) && is.null(lib.loc) && !verbose) {
        ## We only want the paths to the attached packages.
        return(.path.package())
    }

    ## don't waste time looking for the standard packages:
    ## we know where they are and this can take a significant
    ## time with 1000+ packages installed.
    if(length(package) == 1L  &&
       package %in% c("base", "tools", "utils", "grDevices", "graphics",
                      "stats", "datasets", "methods", "grid", "splines",
                      "stats4", "tcltk"))
        return(file.path(.Library, package))

    use_attached <- FALSE
    if(is.null(package)) package <- .packages()
    if(is.null(lib.loc)) {
        use_attached <- TRUE
        lib.loc <- .libPaths()
    }

    if(!length(package)) return(character())

    bad <- character(0L)
    out <- character(0L)

    for(pkg in package) {
        paths <- character()
        for(lib in lib.loc) {
            dirs <- list.files(lib,
                               pattern = paste("^", pkg, "$", sep = ""),
                               full.names = TRUE)
            ## Note that we cannot use tools::file_test() here, as
            ## cyclic name space dependencies are not supported.  Argh.
            paths <- c(paths,
                       dirs[file.info(dirs)$isdir &
                            file.exists(file.path(dirs,
                                                  "DESCRIPTION"))])
        }
        if(use_attached
           && length(pos <- grep(paste("^package:", pkg, "$", sep = ""),
                                 search()))) {
            dirs <- sapply(pos, function(i) {
                if(identical(env <- as.environment(i), baseenv()))
                    system.file()
                else
                    attr(env, "path")
            })
            ## possibly NULL if no path attribute.
            dirs <- dirs[!sapply(dirs, is.null)]
            paths <- c(as.character(dirs), paths)
        }
        if(length(paths)) {
            paths <- unique(paths)
            valid_package_version_regexp <-
                .standard_regexps()$valid_package_version
            db <- lapply(paths, function(p) {
                ## Note that this is sometimes used for source
                ## packages, e.g. by promptPackage from package.skeleton
                pfile <- file.path(p, "Meta", "package.rds")
                info <- if(file.exists(pfile))
                    ## this must have these fields to get installed
                    .readRDS(pfile)$DESCRIPTION[c("Package", "Version")]
                else {
                    info <- tryCatch(read.dcf(file.path(p, "DESCRIPTION"),
                                              c("Package", "Version"))[1, ],
                                     error = identity)
                    if(inherits(info, "error")
                       || (length(info) != 2L)
                       || any(is.na(info)))
                        c(Package = NA, Version = NA) # need dimnames below
                    else
                        info
                }
            })
            db <- do.call("rbind", db)
            ok <- (apply(!is.na(db), 1L, all)
                   & (db[, "Package"] == pkg)
                   & (grepl(valid_package_version_regexp, db[, "Version"])))
            paths <- paths[ok]
        }
        if(length(paths) == 0L) {
            bad <- c(bad, pkg)
            next
        }
        if(length(paths) > 1L) {
            ## If a package was found more than once ...
            paths <- paths[1L]
            if(verbose)
                warning(gettextf("package '%s' found more than once,\nusing the one found in '%s'",
                                 pkg, paths), domain = NA)
        }
        out <- c(out, paths)
    }

    if(!quiet && length(bad)) {
        if(length(out) == 0L) {
            if(length(bad) == 1L) {
                stop(gettextf("there is no package called '%s'", pkg),
                     domain = NA)
            } else {
                stop(ngettext(length(bad),
                              "there is no package called",
                              "there are no packages called"), " ",
                     paste(shQuote(bad), collapse = ", "), domain = NA)

            }
        }
        for(pkg in bad)
            warning(gettextf("there is no package called '%s'", pkg),
                    domain = NA)
    }

    out
}

print.packageInfo <- function(x, ...)
{
    if(!inherits(x, "packageInfo")) stop("wrong class")
    outFile <- tempfile("RpackageInfo")
    outConn <- file(outFile, open = "w")
    vignetteMsg <-
        gettextf("Further information is available in the following vignettes in directory '%s':",
                 file.path(x$path, "doc"))
    headers <- c(gettext("Description:\n\n"),
                 gettext("Index:\n\n"),
                 paste(paste(strwrap(vignetteMsg), collapse = "\n"),
                       "\n\n", sep = ""))
    footers <- c("\n", "\n", "")
    formatDocEntry <- function(entry) {
        if(is.list(entry) || is.matrix(entry))
            formatDL(entry, style = "list")
        else
            entry
    }
    writeLines(gettextf("\n\t\tInformation on package '%s'\n", x$name),
               outConn)
    for(i in which(!sapply(x$info, is.null))) {
        writeLines(headers[i], outConn, sep = "")
        writeLines(formatDocEntry(x$info[[i]]), outConn)
        writeLines(footers[i], outConn, sep = "")
    }
    close(outConn)
    file.show(outFile, delete.file = TRUE,
              title = gettextf("Documentation for package '%s'", x$name))
    invisible(x)
}

.getRequiredPackages <-
    function(file="DESCRIPTION", quietly = FALSE, useImports = FALSE)
{
    ## OK to call tools as only used during installation.
    pkgInfo <- tools:::.split_description(tools:::.read_description(file))
    .getRequiredPackages2(pkgInfo, quietly, , useImports)
    invisible()
}

.getRequiredPackages2 <-
function(pkgInfo, quietly = FALSE, lib.loc = NULL, useImports = FALSE)
{
    pkgs <- unique(names(pkgInfo$Depends))
    if (length(pkgs)) {
        pkgname <- pkgInfo$DESCRIPTION["Package"]
        for(pkg in pkgs) {
            ## allow for multiple occurrences
            zs <- pkgInfo$Depends[names(pkgInfo$Depends) == pkg]
            have_vers <- any(sapply(zs, length) > 1L)
            if ( !paste("package", pkg, sep = ":") %in% search() ) {
                if (have_vers) {
                    pfile <- system.file("Meta", "package.rds",
                                         package = pkg, lib.loc = lib.loc)
                    if(nzchar(pfile) == 0)
                        stop(gettextf("package '%s' required by '%s' could not be found",
                                      pkg, pkgname),
                             call. = FALSE, domain = NA)
                    current <- .readRDS(pfile)$DESCRIPTION["Version"]
                    for(z in zs)
                        if(length(z) > 1L) {
                            target <- as.numeric_version(z$version)
                            if (!eval(parse(text=paste("current", z$op, "target"))))
                                stop(gettextf("package '%s' %s was found, but %s %s is required by '%s'",
                                              pkg, current, z$op, target, pkgname),
                                     call. = FALSE, domain = NA)
                        }
                }

                if (!quietly)
                    packageStartupMessage(gettextf("Loading required package: %s",
                                     pkg), domain = NA)
                library(pkg, character.only = TRUE, logical.return = TRUE,
                        lib.loc = lib.loc) ||
                stop(gettextf("package '%s' could not be loaded", pkg),
                     call. = FALSE, domain = NA)
            } else {
                ## check the required version number, if any
                if (have_vers) {
                    pfile <- system.file("Meta", "package.rds",
                                         package = pkg, lib.loc = lib.loc)
                    current <- .readRDS(pfile)$DESCRIPTION["Version"]
                    for(z in zs)
                        if (length(z) > 1L) {
                            target <- as.numeric_version(z$version)
                            if (!eval(parse(text=paste("current", z$op, "target"))))
                                stop(gettextf("package '%s' %s is loaded, but %s %s is required by '%s'",
                                              pkg, current, z$op, target, pkgname),
                                     call. = FALSE, domain = NA)
                        }
                }
            }
        }
    }
    if(useImports) {
        nss <- names(pkgInfo$Imports)
        for(ns in nss) loadNamespace(ns, lib.loc)
    }
}

.expand_R_libs_env_var <-
function(x)
{
    v <- paste(R.version[c("major", "minor")], collapse = ".")

    expand <- function(x, spec, expansion)
        gsub(paste("(^|[^%])(%%)*%", spec, sep = ""),
             sprintf("\\1\\2%s", expansion), x)

    ## %V => version x.y.z
    x <- expand(x, "V", v)
    ## %v => version x.y
    x <- expand(x, "v", sub("\\.[^.]*$", "", v))
    ## %p => platform
    x <- expand(x, "p", R.version$platform)
    ## %a => arch
    x <- expand(x, "a", R.version$arch)
    ## %o => os
    x <- expand(x, "o", R.version$os)

    gsub("%%", "%", x)
}
#  File src/library/base/R/license.R
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

licence <- license <- function() {
    cat("\nThis software is distributed under the terms of the GNU General\n")
    cat("Public License Version 2, June 1991.  The terms of this license\n")
    cat("are in a file called COPYING which you should have received with\n")
    cat("this software and which can be displayed by RShowDoc(\"COPYING\").\n")
    cat("\n")
    cat("If you have not received a copy of this file, you can obtain one\n")
    cat("at http://www.R-project.org/licenses/.\n")
    cat("\n")
    cat("A small number of files (the API header files listed in\n")
    cat("R_DOC_DIR/COPYRIGHTS) are distributed under the\n")
    cat("Lesser GNU General Public LIcense version 2.1.\n")
    cat("This can be displayed by RShowDoc(\"COPYING.LIB\"),\n")
    cat("or obtained at the URI given.\n")
    cat("\n")
    cat("'Share and Enjoy.'\n\n")
}
#  File src/library/base/R/load.R
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

load <-
    function (file, envir = parent.frame())
{
    if (is.character(file)) {
        ## files are allowed to be of an earlier format
        ## gzfile can open gzip, bzip2, xz and uncompressed files.
        con <- gzfile(file)
        on.exit(close(con))
        magic <- readChar(con, 5L, useBytes = TRUE)
        if (!grepl("RD[AX]2\n", magic)) {
            ## a check while we still know the args
            if(grepl("RD[ABX][12]\r", magic))
                stop("input has been corrupted, with LF replaced by CR")
            ## Not a version 2 magic number, so try the old way.
            warning(gettextf("file '%s' has magic number '%s'\n   Use of save versions prior to 2 is deprecated",
                             basename(file), gsub("[\n\r]*", "", magic)),
                    domain = NA, call. = FALSE)
            return(.Internal(load(file, envir)))
        }
    } else if (inherits(file, "connection")) {
        con <- if(inherits(file, "gzfile")) file else gzcon(file)
    } else stop("bad 'file' argument")

    .Internal(loadFromConn2(con, envir))
}

save <- function(..., list = character(0L),
                 file = stop("'file' must be specified"),
                 ascii = FALSE, version = NULL, envir = parent.frame(),
                 compress = !ascii, compression_level,
                 eval.promises = TRUE, precheck = TRUE)
{
    opts <- getOption("save.defaults")
    if (missing(compress) && ! is.null(opts$compress))
        compress <- opts$compress
    if (missing(ascii) && ! is.null(opts$ascii))
        ascii <- opts$ascii
    if (missing(version)) version <- opts$version
    if (!is.null(version) && version < 2)
        warning("Use of save versions prior to 2 is deprecated")

    names <- as.character( substitute( list(...)))[-1L]
    list<- c(list, names)
    if (! is.null(version) && version == 1)
        invisible(.Internal(save(list, file, ascii, version, envir,
                                 eval.promises)))
    else {
        if (precheck) {
            ## check for existence of objects before opening connection
            ## (and e.g. clobering file)
            ok <- unlist(lapply(list, exists, envir=envir))
            if(!all(ok)) {
                n <- sum(!ok)
                stop(sprintf(ngettext(n,
                                      "object %s not found",
                                      "objects %s not found"
                                      ),
                             paste(sQuote(list[!ok]), collapse = ", ")
                             ), domain = NA)
            }
        }
        if (is.character(file)) {
            if (file == "") stop("'file' must be non-empty string")
            con <- if (identical(compress, "bzip2")) {
                if (!missing(compression_level))
                    bzfile(file, "wb", compress = compression_level)
                else bzfile(file, "wb")
            } else if (identical(compress, "xz")) {
                if (!missing(compression_level))
                    xzfile(file, "wb", compress = compression_level)
                else xzfile(file, "wb", compress = 9)
            } else if (identical(compress, "gzip") || compress) {
                if (!missing(compression_level))
                    gzfile(file, "wb", compress = compression_level)
                else gzfile(file, "wb")
            } else file(file, "wb")
            on.exit(close(con))
        }
        else if (inherits(file, "connection"))
            con <- file
        else stop("bad file argument")
        if(isOpen(con) && summary(con)$text != "binary")
            stop("can only save to a binary connection")
        invisible(.Internal(saveToConn(list, con, ascii, version, envir,
                                       eval.promises)))
    }
}

save.image <- function (file = ".RData", version = NULL, ascii = FALSE,
                        compress = !ascii, safe = TRUE)
{
    if (! is.character(file) || file == "")
        stop("'file' must be non-empty string")

    opts <- getOption("save.image.defaults")
    if(is.null(opts)) opts <- getOption("save.defaults")

    if (missing(safe) && ! is.null(opts$safe))
        safe <- opts$safe
    if (missing(ascii) && ! is.null(opts$ascii))
        ascii <- opts$ascii
    if (missing(compress) && ! is.null(opts$compress))
        compress <- opts$compress
    if (missing(version)) version <- opts$version

    if (safe) {
        ## find a temporary file name in the same directory so we can
        ## rename it to the final output file on success
        outfile <- paste(file, "Tmp", sep = "")
        i <- 0
        while (file.exists(outfile)) {
            i <- i + 1
            outfile <- paste(file, "Tmp", i, sep = "")
        }
    }
    else outfile <- file

    on.exit(file.remove(outfile))
    save(list = ls(envir = .GlobalEnv, all.names = TRUE), file = outfile,
         version = version, ascii = ascii, compress = compress,
         envir = .GlobalEnv, precheck = FALSE)
    if (safe)
        if (! file.rename(outfile, file)) {
            on.exit()
            stop("image could not be renamed and is left in ", outfile)
        }
    on.exit()
}

sys.load.image <- function(name, quiet)
{
    if (file.exists(name)) {
        load(name, envir = .GlobalEnv)
        if (! quiet)
	    message("[Previously saved workspace restored]", "\n")
    }
}

sys.save.image <- function(name)
{
    ## Ensure that there is a reasonable chance that we can open a
    ## connection.
    closeAllConnections()
    save.image(name)
}

findPackageEnv <- function(info)
{
    if(info %in% search()) return(as.environment(info))
    message(gettextf("Attempting to load the environment '%s'", info),
            domain = NA)
    pkg <- substr(info, 9L, 1000L)
    if(require(pkg, character.only=TRUE, quietly = TRUE))
        return(as.environment(info))
    message("not found: using .GlobalEnv instead")
    return(.GlobalEnv)
}
#  File src/library/base/R/locales.R
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

Sys.getlocale <- function(category = "LC_ALL")
{
    category <- match(category, c("LC_ALL", "LC_COLLATE", "LC_CTYPE",
                                  "LC_MONETARY", "LC_NUMERIC", "LC_TIME",
                                  "LC_MESSAGES", "LC_PAPER", "LC_MEASUREMENT"))
    if(is.na(category)) stop("invalid 'category' argument")
    .Internal(Sys.getlocale(category))
}

Sys.setlocale <- function(category = "LC_ALL", locale = "")
{
    category <- match(category, c("LC_ALL", "LC_COLLATE", "LC_CTYPE",
                                  "LC_MONETARY", "LC_NUMERIC", "LC_TIME",
                                  "LC_MESSAGES", "LC_PAPER", "LC_MEASUREMENT"))
    if(is.na(category)) stop("invalid 'category' argument")
    .Internal(Sys.setlocale(category, locale))
}

Sys.localeconv <- function() .Internal(Sys.localeconv())
#  File src/library/base/R/lower.tri.R
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

lower.tri <- function(x, diag = FALSE)
{
    x <- as.matrix(x)
    if(diag) row(x) >= col(x)
    else row(x) > col(x)
}
#  File src/library/base/R/mapply.R
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

mapply <- function(FUN,..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE)
{
    FUN <- match.fun(FUN)
    dots <- list(...)

    answer <- .Call("do_mapply", FUN, dots, MoreArgs, environment(),
                    PACKAGE = "base")

    if (USE.NAMES && length(dots)) {
	if (is.null(names1 <- names(dots[[1L]])) && is.character(dots[[1L]]))
	    names(answer) <- dots[[1L]]
	else if (!is.null(names1))
	    names(answer) <- names1
    }
    if (SIMPLIFY && length(answer) &&
        length(common.len <- unique(unlist(lapply(answer, length)))) == 1L) {
        if (common.len == 1L)
            unlist(answer, recursive = FALSE)
        else if (common.len > 1L)
            array(unlist(answer, recursive = FALSE),
                  dim = c(common.len, max(sapply(dots,length))),
                  dimnames = list(names(answer[[1L]]), names(answer)))
        else answer
    }
    else answer
}

Vectorize <- function(FUN, vectorize.args = arg.names, SIMPLIFY = TRUE,
                      USE.NAMES = TRUE)
{
    arg.names <- as.list(formals(FUN))
    arg.names[["..."]] <- NULL
    arg.names <- names(arg.names)

    vectorize.args <- as.character(vectorize.args)

    if (!length(vectorize.args)) return(FUN)

    if (!all(vectorize.args %in% arg.names))
    	stop("must specify formal argument names to vectorize")

    FUNV <- function() { ## will set the formals below
        args <- lapply(as.list(match.call())[-1L], eval, parent.frame())
        names <- if(is.null(names(args))) character(length(args))
        else names(args)
        dovec <- names %in% vectorize.args
        do.call("mapply", c(FUN = FUN,
                            args[dovec],
                            MoreArgs = list(args[!dovec]),
                            SIMPLIFY = SIMPLIFY,
                            USE.NAMES = USE.NAMES))
    }
    formals(FUNV) <- formals(FUN)
    FUNV
}
#  File src/library/base/R/match.R
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

match <- function(x, table, nomatch = NA_integer_, incomparables = NULL)
    .Internal(match(if(is.factor(x)) as.character(x) else x,
                    if(is.factor(table)) as.character(table) else table,
                    nomatch, incomparables))


match.call <-
    function(definition=NULL, call=sys.call(sys.parent()), expand.dots=TRUE)
    .Internal(match.call(definition,call,expand.dots))

pmatch <- function(x, table, nomatch = NA_integer_, duplicates.ok = FALSE)
    .Internal(pmatch(as.character(x), as.character(table), nomatch,
                     duplicates.ok))

"%in%" <- function(x, table) match(x, table, nomatch = 0L) > 0L

match.arg <- function (arg, choices, several.ok = FALSE)
{
    if (missing(choices)) {
	formal.args <- formals(sys.function(sys.parent()))
	choices <- eval(formal.args[[deparse(substitute(arg))]])
    }
    if (is.null(arg)) return(choices[1L])
    else if(!is.character(arg))
	stop("'arg' must be NULL or a character vector")
    if (!several.ok) { # most important (default) case:
        ## the arg can be the whole of choices as a default argument.
        if(identical(arg, choices)) return(arg[1L])
        if(length(arg) > 1L) stop("'arg' must be of length 1")
    } else if(length(arg) == 0L) stop("'arg' must be of length >= 1")

    ## handle each element of arg separately
    i <- pmatch(arg, choices, nomatch = 0L, duplicates.ok = TRUE)
    if (all(i == 0L))
	stop(gettextf("'arg' should be one of %s",
                      paste(dQuote(choices), collapse = ", ")),
             domain = NA)
    i <- i[i > 0L]
    if (!several.ok && length(i) > 1)
        stop("there is more than one match in 'match.arg'")
    choices[i]
}

charmatch <- function(x, table, nomatch = NA_integer_)
    .Internal(charmatch(as.character(x), as.character(table), nomatch))

char.expand <- function(input, target, nomatch = stop("no match"))
{
    if(length(input) != 1L)
	stop("'input' must have length 1")
    if(!(is.character(input) && is.character(target)))
	stop("'input' and 'target' must be character vectors")
    y <- .Internal(charmatch(input, target, NA_integer_))
    if(any(is.na(y))) eval(nomatch)
    target[y]
}
#  File src/library/base/R/match.fun.R
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

### clean up FUN arguments to *apply, outer, sweep, etc.
### note that this grabs two levels back and is not designed
### to be called at top level
match.fun <- function (FUN, descend = TRUE)
{
    if ( is.function(FUN) )
        return(FUN)
    if (!(is.character(FUN) && length(FUN) == 1L || is.symbol(FUN))) {
        ## Substitute in parent
        FUN <- eval.parent(substitute(substitute(FUN)))
        if (!is.symbol(FUN))
            stop(gettextf("'%s' is not a function, character or symbol",
                          deparse(FUN)), domain = NA)
    }
    envir <- parent.frame(2)
    if( descend )
        FUN <- get(as.character(FUN), mode = "function", envir = envir)
    else {
        FUN <- get(as.character(FUN), mode = "any", envir = envir)
        if( !is.function(FUN) )
           stop(gettextf("found non-function '%s'", FUN), domain = NA)
    }
    return(FUN)
}
#  File src/library/base/R/matrix.R
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

matrix <- function(data=NA, nrow=1, ncol=1, byrow=FALSE, dimnames=NULL)
{
    data <- as.vector(data)
    if(missing(nrow))
        nrow <- ceiling(length(data)/ncol)
    else if(missing(ncol))
        ncol <- ceiling(length(data)/nrow)
    .Internal(matrix(data, nrow, ncol, byrow, dimnames))
}

nrow <- function(x) dim(x)[1L]
ncol <- function(x) dim(x)[2L]

NROW <- function(x) if(is.array(x)||is.data.frame(x)) nrow(x) else length(x)
NCOL <- function(x) if(is.array(x) && length(dim(x)) > 1L || is.data.frame(x)) ncol(x) else 1L

rownames <- function(x, do.NULL = TRUE, prefix = "row")
{
    dn <- dimnames(x)
    if(!is.null(dn[[1L]]))
	dn[[1L]]
    else {
        nr <- NROW(x)
	if(do.NULL) NULL
        else if(nr > 0L) paste(prefix, seq_len(nr), sep="")
        else character(0L)
    }
}

`rownames<-` <- function(x, value)
{
    if(is.data.frame(x)) {
        row.names(x) <- value
    } else {
        dn <- dimnames(x)
        if(is.null(dn)) {
            if(is.null(value)) return(x)
            if((nd <- length(dim(x))) < 1L)
                stop("attempt to set rownames on object with no dimensions")
            dn <- vector("list", nd)
        }
        if(length(dn) < 1L)
            stop("attempt to set rownames on object with no dimensions")
        if(is.null(value)) dn[1L] <- list(NULL) else dn[[1L]] <- value
        dimnames(x) <- dn
    }
    x
}

colnames <- function(x, do.NULL = TRUE, prefix = "col")
{
    if(is.data.frame(x) && do.NULL)
	return(names(x))
    dn <- dimnames(x)
    if(!is.null(dn[[2L]]))
	dn[[2L]]
    else {
        nc <- NCOL(x)
	if(do.NULL) NULL
        else if(nc > 0L) paste(prefix, seq_len(nc), sep="")
        else character(0L)
    }
}

`colnames<-` <- function(x, value)
{
    if(is.data.frame(x)) {
        names(x) <- value
    } else {
        dn <- dimnames(x)
        if(is.null(dn)) {
            if(is.null(value)) return(x)
            if((nd <- length(dim(x))) < 2L)
                stop("attempt to set colnames on object with less than two dimensions")
            dn <- vector("list", nd)
        }
        if(length(dn) < 2L)
            stop("attempt to set colnames on object with less than two dimensions")
        if(is.null(value)) dn[2L] <- list(NULL) else dn[[2L]] <- value
        dimnames(x) <- dn
    }
    x
}

row <- function(x, as.factor=FALSE)
{
    if(as.factor) {
        labs <- rownames(x, do.NULL=FALSE, prefix="")
        res <- factor(.Internal(row(dim(x))), labels=labs)
        dim(res) <- dim(x)
        res
    } else .Internal(row(dim(x)))
}

col <- function(x, as.factor=FALSE)
{
    if(as.factor) {
        labs <- colnames(x, do.NULL=FALSE, prefix="")
        res <- factor(.Internal(col(dim(x))), labels=labs)
        dim(res) <- dim(x)
        res
    } else .Internal(col(dim(x)))
}

crossprod <- function(x, y=NULL) .Internal(crossprod(x,y))
tcrossprod <- function(x, y=NULL) .Internal(tcrossprod(x,y))

t <- function(x) UseMethod("t")
## t.default is <primitive>
t.data.frame<- function(x)
{
    x <- as.matrix(x)
    NextMethod("t")
}
## as.matrix  is in "as"
#  File src/library/base/R/max.col.R
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

max.col <- function(m, ties.method=c("random", "first", "last"))
{
    ties.method <- match.arg(ties.method)
    m <- as.matrix(m)
    n <- nrow(m)
    .C("R_max_col",
       as.double(m),
       n,
       ncol(m),
       rmax = integer(n),
       tieM = which(ties.method == eval(formals()[["ties.method"]])),
       NAOK = TRUE,
       DUP  = FALSE,
       PACKAGE = "base")$rmax
}
#  File src/library/base/R/mean.R
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

mean <- function(x, ...) UseMethod("mean")

mean.default <- function(x, trim = 0, na.rm = FALSE, ...)
{
    if(!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
        warning("argument is not numeric or logical: returning NA")
        return(NA_real_)
    }
    if (na.rm)
	x <- x[!is.na(x)]
    if(!is.numeric(trim) || length(trim) != 1L)
        stop("'trim' must be numeric of length one")
    n <- length(x)
    if(trim > 0 && n) {
	if(is.complex(x))
	    stop("trimmed means are not defined for complex data")
	if(trim >= 0.5) return(stats::median(x, na.rm=FALSE))
	lo <- floor(n*trim)+1
	hi <- n+1-lo
	x <- sort.int(x, partial=unique(c(lo, hi)))[lo:hi]
    }
    .Internal(mean(x))
}

mean.data.frame <- function(x, ...) sapply(x, mean, ...)
#  File src/library/base/R/merge.R
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

merge <- function(x, y, ...) UseMethod("merge")

merge.default <- function(x, y, ...)
    merge(as.data.frame(x), as.data.frame(y), ...)

merge.data.frame <-
    function(x, y, by = intersect(names(x), names(y)), by.x = by, by.y = by,
             all = FALSE, all.x = all, all.y = all,
             sort = TRUE, suffixes = c(".x",".y"), incomparables = NULL,
             ...)
{
    fix.by <- function(by, df)
    {
        ## fix up 'by' to be a valid set of cols by number: 0 is row.names
        if(is.null(by)) by <- numeric(0L)
        by <- as.vector(by)
        nc <- ncol(df)
        if(is.character(by))
            by <- match(by, c("row.names", names(df))) - 1L
        else if(is.numeric(by)) {
            if(any(by < 0L) || any(by > nc))
                stop("'by' must match numbers of columns")
        } else if(is.logical(by)) {
            if(length(by) != nc) stop("'by' must match number of columns")
            by <- seq_along(by)[by]
        } else stop("'by' must specify column(s) as numbers, names or logical")
        if(any(is.na(by))) stop("'by' must specify valid column(s)")
        unique(by)
    }

    nx <- nrow(x <- as.data.frame(x)); ny <- nrow(y <- as.data.frame(y))
    by.x <- fix.by(by.x, x)
    by.y <- fix.by(by.y, y)
    if((l.b <- length(by.x)) != length(by.y))
        stop("'by.x' and 'by.y' specify different numbers of columns")
    if(l.b == 0L) {
        ## was: stop("no columns to match on")
        ## return the cartesian product of x and y, fixing up common names
        nm <- nm.x <- names(x)
        nm.y <- names(y)
        has.common.nms <- any(cnm <- nm.x %in% nm.y)
        if(has.common.nms) {
            names(x)[cnm] <- paste(nm.x[cnm], suffixes[1L], sep="")
            cnm <- nm.y %in% nm
            names(y)[cnm] <- paste(nm.y[cnm], suffixes[2L], sep="")
        }
        if (nx == 0L || ny == 0L) {
            res <- cbind(x[FALSE, ], y[FALSE, ])
        } else {
            ij <- expand.grid(seq_len(nx), seq_len(ny))
            res <- cbind(x[ij[, 1L], , drop = FALSE], y[ij[, 2L], , drop = FALSE])
        }
    }
    else {
        if(any(by.x == 0L)) {
            x <- cbind(Row.names = I(row.names(x)), x)
            by.x <- by.x + 1L
        }
        if(any(by.y == 0L)) {
            y <- cbind(Row.names = I(row.names(y)), y)
            by.y <- by.y + 1L
        }
        row.names(x) <- NULL
        row.names(y) <- NULL
        ## create keys from 'by' columns:
        if(l.b == 1L) {                  # (be faster)
            bx <- x[, by.x]; if(is.factor(bx)) bx <- as.character(bx)
            by <- y[, by.y]; if(is.factor(by)) by <- as.character(by)
        } else {
            ## Do these together for consistency in as.character.
            ## Use same set of names.
            bx <- x[, by.x, drop=FALSE]; by <- y[, by.y, drop=FALSE]
            names(bx) <- names(by) <- paste("V", seq_len(ncol(bx)), sep="")
            bz <- do.call("paste", c(rbind(bx, by), sep = "\r"))
            bx <- bz[seq_len(nx)]
            by <- bz[nx + seq_len(ny)]
        }
        comm <- match(bx, by, 0L)
        bxy <- bx[comm > 0L]             # the keys which are in both
        xinds <- match(bx, bxy, 0L, incomparables)
        yinds <- match(by, bxy, 0L, incomparables)
        if(nx > 0L && ny > 0L)
            m <- .Internal(merge(xinds, yinds, all.x, all.y))
        else
            m <- list(xi=integer(0L), yi=integer(0L),
                      x.alone=seq_len(nx), y.alone=seq_len(ny))
        nm <- nm.x <- names(x)[-by.x]
        nm.by <- names(x)[by.x]
        nm.y <- names(y)[-by.y]
        ncx <- ncol(x)
        if(all.x) all.x <- (nxx <- length(m$x.alone)) > 0L
        if(all.y) all.y <- (nyy <- length(m$y.alone)) > 0L
        lxy <- length(m$xi)             # == length(m$yi)
        ## x = [ by | x ] :
        has.common.nms <- any(cnm <- nm.x %in% nm.y)
        if(has.common.nms)
            nm.x[cnm] <- paste(nm.x[cnm], suffixes[1L], sep="")
        x <- x[c(m$xi, if(all.x) m$x.alone),
               c(by.x, seq_len(ncx)[-by.x]), drop=FALSE]
        names(x) <- c(nm.by, nm.x)
        if(all.y) { ## add the 'y.alone' rows to x[]
            ## need to have factor levels extended as well -> using [cr]bind
            ya <- y[m$y.alone, by.y, drop=FALSE]
            names(ya) <- nm.by
            ## this used to use a logical matrix, but that is not good
            ## enough as x could be zero-row.
            ya <- cbind(ya, x[rep.int(NA_integer_, nyy), nm.x, drop=FALSE ])
            x <- rbind(x, ya)
            #x <- rbind(x, cbind(ya, matrix(NA, nyy, ncx-l.b,
            #                               dimnames=list(NULL,nm.x))))
        }
        ## y (w/o 'by'):
        if(has.common.nms) {
            cnm <- nm.y %in% nm
            nm.y[cnm] <- paste(nm.y[cnm], suffixes[2L], sep="")
        }
        y <- y[c(m$yi, if(all.x) rep.int(1L, nxx), if(all.y) m$y.alone),
               -by.y, drop = FALSE]
        if(all.x)
            for(i in seq_along(y))
                ## do it this way to invoke methods for e.g. factor
                is.na(y[[i]]) <- (lxy+1L):(lxy+nxx)

        if(has.common.nms) names(y) <- nm.y
        res <- cbind(x, y)

        if (sort)
            res <- res[if(all.x || all.y) ## does NOT work
                       do.call("order", x[, seq_len(l.b), drop=FALSE])
            else sort.list(bx[m$xi]),, drop=FALSE]
    }
    ## avoid a copy
    ## row.names(res) <- NULL
    attr(res, "row.names") <- .set_row_names(nrow(res))
    res
}
#  File src/library/base/R/message.R
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

simpleMessage <-
function(message, call = NULL)
    structure(list(message = message, call = call),
              class = c("simpleMessage", "message", "condition"))

suppressMessages <-
function(expr)
    withCallingHandlers(expr,
                        message = function(c)
                        invokeRestart("muffleMessage"))

message <-
function(..., domain = NULL, appendLF = TRUE)
{
    args <- list(...)
    cond <- if (length(args) == 1L && inherits(args[[1L]], "condition")) {
        if(nargs() > 1L)
            warning("additional arguments ignored in message()")
        args[[1L]]
    } else {
        msg <- .makeMessage(..., domain=domain, appendLF = appendLF)
        call <- sys.call()
        simpleMessage(msg, call)
    }
    defaultHandler <- function(c) {
        ## Maybe use special connection here?
        cat(conditionMessage(c), file=stderr(), sep="")
    }
    withRestarts({
        signalCondition(cond)
        ## We don't get to the default handler if the signal
        ## is handled with a non-local exit, e.g. by
        ## invoking the muffleMessage restart.
        defaultHandler(cond)
    }, muffleMessage = function() NULL)
    invisible()
}

## also used by warning() and stop()
.makeMessage <- function(..., domain = NULL, appendLF = FALSE)
 {
    args <- list(...)
    msg <- if(length(args)) {
        args <- lapply(list(...), as.character)
        if(is.null(domain) || !is.na(domain))
            args <- .Internal(gettext(domain, unlist(args)))
        paste(args, collapse = "")
    } else ""
    if(appendLF) paste(msg, "\n", sep = "") else msg
}

.packageStartupMessage <- function (message, call = NULL)
    structure(list(message = message, call = call),
              class = c("packageStartupMessage", "condition", "message",
              "simpleMessage"))

suppressPackageStartupMessages <- function (expr)
    withCallingHandlers(expr, packageStartupMessage=function(c)
                        invokeRestart("muffleMessage"))

packageStartupMessage <- function(..., domain = NULL, appendLF = TRUE)
{
    call <- sys.call()
    msg <- .makeMessage(..., domain=domain, appendLF = appendLF)
    message(.packageStartupMessage(msg, call))
}
#  File src/library/base/R/methodsSupport.R
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

trace <- function(what, tracer, exit, at, print, signature, where = topenv(parent.frame()), edit = FALSE)
{
    needsAttach <- nargs() > 1L && !.isMethodsDispatchOn()
    if(needsAttach) {
        ns <- try(loadNamespace("methods"))
        if(isNamespace(ns))
            message("(loaded the methods namespace)")
        else
            stop("Tracing functions requires the methods package, but unable to load methods namespace")
    }
    else if(nargs() == 1L)
        return(.primTrace(what))
    tState <- tracingState(FALSE)
    on.exit(tracingState(tState))
    ## now call the version in the methods package, to ensure we get
    ## the correct name space (e.g., correct version of class())
    call <- sys.call()
    call[[1L]] <- quote(methods::.TraceWithMethods)
    call$where <- where
    value <- eval.parent(call)
    on.exit() ## no error
    tracingState(tState)
    value
}

untrace <- function(what, signature = NULL, where = topenv(parent.frame())) {
    ## NOTE: following test is TRUE after loadNamespace("methods") (even if not in search())
    MethodsDispatchOn <- .isMethodsDispatchOn()
    if(MethodsDispatchOn) {
        tState <- tracingState(FALSE)
        on.exit(tracingState(tState))
    }
    if(!MethodsDispatchOn)
        return(.primUntrace(what)) ## can't have called trace except in primitive form
    ## at this point we can believe that the methods namespace was successfully loaded
    ## now call the version in the methods package, to ensure we get
    ## the correct name space (e.g., correct version of class())
    call <- sys.call()
    call[[1L]] <- quote(methods::.TraceWithMethods)
    call$where <- where
    call$untrace <- TRUE
    value <- eval.parent(call)
    on.exit() ## no error
    tracingState(tState)
    invisible(value)
}

.isMethodsDispatchOn <- function(onOff = NULL)
    .Call("R_isMethodsDispatchOn", onOff, PACKAGE = "base")

tracingState <- function( on = NULL)
    .Call("R_traceOnOff", on, PACKAGE = "base")

isS4 <- function(object)
    .Call("R_isS4Object", object, PACKAGE = "base")

asS4 <- function(object, flag = TRUE, complete = TRUE) {
    flag <- methods::as(flag, "logical")
    if(length(flag) != 1L || is.na(flag))
      stop("Expected a single logical value for the S4 state flag")
    .Call("R_setS4Object", object, flag, complete, PACKAGE = "base")
  }

.doTrace <- function(expr, msg) {
    on <- tracingState(FALSE) # turn it off QUICKLY (via a .Call)
    if(on) {
        on.exit(tracingState(TRUE)) # restore on exit, keep off during trace
        if(!missing(msg)) {
            call <- deparse(sys.call(sys.parent(1L)))
            if(length(call) > 1L)
              call <- paste(call[[1L]], "....")
            cat("Tracing", call, msg, "\n")
        }
        exprObj <- substitute(expr)
        eval.parent(exprObj)
    }
    NULL
}
#  File src/library/base/R/mode.R
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

mode <- function(x) {
    if(is.expression(x)) return("expression")
    if(is.call(x))
	return(switch(deparse(x[[1L]])[1L],
		      "(" = "(",
		      ## otherwise
		      "call"))
    if(is.name(x)) "name" else
    switch(tx <- typeof(x),
	   double=, integer= "numeric",# 'real=' dropped, 2000/Jan/14
	   closure=, builtin=, special= "function",
	   ## otherwise
	   tx)
}

`mode<-` <- function(x, value)
{
    if (storage.mode(x) == value) return(x)
    if(is.factor(x)) stop("invalid to change the storage mode of a factor")
    mde <- paste("as.",value,sep="")
    atr <- attributes(x)
    isSingle <- !is.null(attr(x, "Csingle"))
    setSingle <- value == "single"
    x <- eval(call(mde,x), parent.frame())
    attributes(x) <- atr
    ## this avoids one copy
    if(setSingle != isSingle)
        attr(x, "Csingle") <- if(setSingle) TRUE # else NULL
    x
}

storage.mode <- function(x)
    switch(tx <- typeof(x),
	   closure=, builtin=, special= "function",
	   ## otherwise
	   tx)

### storage.mode<- is primitive as from R 2.6.0
#  File src/library/base/R/namespace.R
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

## give the base namespace a table for registered methods
".__S3MethodsTable__." <- new.env(hash = TRUE, parent = baseenv())

getNamespace <- function(name) {
    ns <- .Internal(getRegisteredNamespace(as.name(name)))
    if (! is.null(ns)) ns
    else tryCatch(loadNamespace(name), error = function(e) stop(e))
}

loadedNamespaces <- function()
    ls(.Internal(getNamespaceRegistry()), all.names = TRUE)

getNamespaceName <- function(ns) {
    ns <- asNamespace(ns)
    if (isBaseNamespace(ns)) "base"
    else getNamespaceInfo(ns, "spec")["name"]
}

getNamespaceVersion <- function(ns) {
    ns <- asNamespace(ns)
    if (isBaseNamespace(ns))
        c(version = paste(R.version$major, R.version$minor, sep = "."))
    else getNamespaceInfo(ns, "spec")["version"]
}

getNamespaceExports <- function(ns) {
    ns <- asNamespace(ns)
    if (isBaseNamespace(ns)) ls(.BaseNamespaceEnv, all.names = TRUE)
    else ls(getNamespaceInfo(ns, "exports"), all.names = TRUE)
}

getNamespaceImports <- function(ns) {
    ns <- asNamespace(ns)
    if (isBaseNamespace(ns)) NULL
    else getNamespaceInfo(ns, "imports")
}

getNamespaceUsers <- function(ns) {
    nsname <- getNamespaceName(asNamespace(ns))
    users <- character(0L)
    for (n in loadedNamespaces()) {
        inames <- names(getNamespaceImports(n))
        if (match(nsname, inames, 0L))
            users <- c(n, users)
    }
    users
}

getExportedValue <- function(ns, name) {
    getInternalExportName <- function(name, ns) {
        exports <- getNamespaceInfo(ns, "exports")
        if (! exists(name, envir = exports, inherits = FALSE))
            stop(gettextf("'%s' is not an exported object from 'namespace:%s'",
                          name, getNamespaceName(ns)),
                 call. = FALSE, domain = NA)
        get(name, envir = exports, inherits = FALSE)
    }
    ns <- asNamespace(ns)
    if (isBaseNamespace(ns)) get(name, envir = ns, inherits = FALSE)
    else get(getInternalExportName(name, ns), envir = ns)
}

"::" <- function(pkg, name) {
    pkg <- as.character(substitute(pkg))
    name <- as.character(substitute(name))
    ns <- tryCatch(asNamespace(pkg), hasNoNamespaceError = function(e) NULL)
    if (is.null(ns)) {
        pos <- match(paste("package", pkg, sep=":"), search(), 0L)
        if (pos == 0)
            stop(gettextf(paste("package '%s' has no name space and",
                                "is not on the search path"), pkg),
                 domain = NA)
        get(name, pos = pos, inherits = FALSE)
    }
    else getExportedValue(pkg, name)
}

":::" <- function(pkg, name) {
    pkg <- as.character(substitute(pkg))
    name <- as.character(substitute(name))
    get(name, envir = asNamespace(pkg), inherits = FALSE)
}

attachNamespace <- function(ns, pos = 2, dataPath = NULL) {
    runHook <- function(hookname, env, ...) {
        if (exists(hookname, envir = env, inherits = FALSE)) {
            fun <- get(hookname, envir = env, inherits = FALSE)
            if (! is.null(try( { fun(...); NULL })))
                stop(gettextf("%s failed in 'attachNamespace'", hookname),
                     call. = FALSE)
        }
    }
    ns <- asNamespace(ns, base.OK = FALSE)
    nsname <- getNamespaceName(ns)
    nspath <- getNamespaceInfo(ns, "path")
    attname <- paste("package", nsname, sep = ":")
    if (attname %in% search())
        stop("name space is already attached")
    env <- attach(NULL, pos = pos, name = attname)
    on.exit(detach(pos = pos))
    attr(env, "path") <- nspath
    exports <- getNamespaceExports(ns)
    importIntoEnv(env, exports, ns, exports)
    if(!is.null(dataPath)) {
        dbbase <- file.path(dataPath, "Rdata")
        if(file.exists(paste(dbbase, ".rdb", sep = ""))) lazyLoad(dbbase, env)
    }
    runHook(".onAttach", ns, dirname(nspath), nsname)
    lockEnvironment(env, TRUE)
    on.exit()
    invisible(env)
}

loadNamespace <- function (package, lib.loc = NULL,
                           keep.source = getOption("keep.source.pkgs"),
                           partial = FALSE, declarativeOnly = FALSE) {
    ## eventually allow version as second component; ignore for now.
    package <- as.character(package)[[1L]]

    ## check for cycles
    dynGet <- function(name,
                       notFound = stop(gettextf("%s not found", name),
                       domain = NA))
    {
        n <- sys.nframe()
        while (n > 1) {
            n <- n - 1
            env <- sys.frame(n)
            if (exists(name, envir = env, inherits = FALSE))
                return(get(name, envir = env, inherits = FALSE))
        }
        notFound
    }
    loading <- dynGet("__NameSpacesLoading__", NULL)
    if (match(package, loading, 0L))
        stop("cyclic name space dependencies are not supported")
    "__NameSpacesLoading__" <- c(package, loading)

    ns <- .Internal(getRegisteredNamespace(as.name(package)))
    if (! is.null(ns))
        ns
    else {
        runHook <- function(hookname, pkgname, env, ...) {
            if (exists(hookname, envir = env, inherits = FALSE)) {
                fun <- get(hookname, envir = env, inherits = FALSE)
                if (! is.null(try( { fun(...); NULL })))
                    stop(gettextf("%s failed in 'loadNamespace' for '%s'",
                                  hookname, pkgname),
                         call. = FALSE, domain = NA)
            }
        }
        runUserHook <- function(pkgname, pkgpath) {
            hook <- getHook(packageEvent(pkgname, "onLoad")) # might be list()
            for(fun in hook) try(fun(pkgname, pkgpath))
        }
        makeNamespace <- function(name, version = NULL, lib = NULL) {
            impenv <- new.env(parent = .BaseNamespaceEnv, hash = TRUE)
            attr(impenv, "name") <- paste("imports", name, sep=":")
            env <- new.env(parent = impenv, hash = TRUE)
            name <- as.character(as.name(name))
            version <- as.character(version)
            info <- new.env(hash = TRUE, parent = baseenv())
            assign(".__NAMESPACE__.", info, envir = env)
            assign("spec", c(name = name,version = version), envir = info)
            setNamespaceInfo(env, "exports", new.env(hash = TRUE, parent = baseenv()))
            setNamespaceInfo(env, "imports", list("base" = TRUE))
            setNamespaceInfo(env, "path", file.path(lib, name))
            setNamespaceInfo(env, "dynlibs", NULL)
            setNamespaceInfo(env, "S3methods", matrix(NA_character_, 0L, 3L))
            assign(".__S3MethodsTable__.",
                   new.env(hash = TRUE, parent = baseenv()),
                   envir = env)
            .Internal(registerNamespace(name, env))
            env
        }
        sealNamespace <- function(ns) {
            namespaceIsSealed <- function(ns)
               environmentIsLocked(ns)
            ns <- asNamespace(ns, base.OK = FALSE)
            if (namespaceIsSealed(ns))
                stop(gettextf("namespace '%s' is already sealed in loadNamespace",
                              getNamespaceName(ns)),
                     call. = FALSE, domain = NA)
            lockEnvironment(ns, TRUE)
            lockEnvironment(parent.env(ns), TRUE)
        }
        addNamespaceDynLibs <- function(ns, newlibs) {
            dynlibs <- getNamespaceInfo(ns, "dynlibs")
            setNamespaceInfo(ns, "dynlibs", c(dynlibs, newlibs))
        }

        bindTranslations <- function(pkgname, pkgpath)
        {
            popath <- file.path(pkgpath, "po")
            if(!file.exists(popath)) return()
            bindtextdomain(pkgname, popath)
            bindtextdomain(paste("R", pkgname, sep = "-"), popath)
        }

        assignNativeRoutines <- function(dll, lib, env, nativeRoutines) {
            if(length(nativeRoutines) == 0L)
                 return(NULL)

            if(nativeRoutines$useRegistration) {
               ## Use the registration information to register ALL the symbols
               fixes <- nativeRoutines$registrationFixes
               routines <- getDLLRegisteredRoutines.DLLInfo(dll, addNames = FALSE)
               lapply(routines,
                      function(type) {
                          lapply(type,
                                 function(sym) {
                                     varName <- paste(fixes[1L], sym$name, fixes[2L], sep = "")
                                     if(exists(varName, envir = env))
                                       warning("failed to assign RegisteredNativeSymbol for ",
                                               sym$name,
                                               paste(" to", varName),
                                               " since ", varName,
                                               " is already defined in the ", package,
                                               " namespace")
                                     else
                                       assign(varName, sym, envir = env)
                                 })
                      })

             }

            symNames <- nativeRoutines$symbolNames
            if(length(symNames) == 0L)
              return(NULL)

            symbols <- getNativeSymbolInfo(symNames, dll, unlist = FALSE,
                                               withRegistrationInfo = TRUE)
            sapply(seq_along(symNames),
                    function(i) {
                        ## could vectorize this outside of the loop
                        ## and assign to different variable to
                        ## maintain the original names.
                        varName <- names(symNames)[i]
                        origVarName <- symNames[i]
                        if(exists(varName, envir = env))
                           warning("failed to assign NativeSymbolInfo for ",
                                   origVarName,
                                   ifelse(origVarName != varName,
                                              paste(" to", varName), ""),
                                   " since ", varName,
                                   " is already defined in the ", package,
                                   " namespace")
                           else
                              assign(varName, symbols[[origVarName]],
                                     envir = env)

                    })



            symbols
          }

        ## find package and check it has a name space
        pkgpath <- .find.package(package, lib.loc, quiet = TRUE)
        if (length(pkgpath) == 0L)
            stop(gettextf("there is no package called '%s'", package),
                 domain = NA)
        bindTranslations(package, pkgpath)
        package.lib <- dirname(pkgpath)
        package <- basename(pkgpath) # need the versioned name
        if (! packageHasNamespace(package, package.lib)) {
            hasNoNamespaceError <-
                function (package, package.lib, call = NULL) {
                class <- c("hasNoNamespaceError", "error", "condition")
                msg <- gettextf("package '%s' does not have a name space",
                                package)
                structure(list(message = msg, package = package,
                               package.lib = package.lib, call = call),
                          class = class)
            }
            stop(hasNoNamespaceError(package, package.lib))
        }

        ## create namespace; arrange to unregister on error
        ## Can we rely on the existence of R-ng 'nsInfo.rds' and
        ## 'package.rds'?
        ## No, not during builds of standard packages
        ## stats4 depends on methods, but exports do not matter
        ## whilst it is being built on
        nsInfoFilePath <- file.path(pkgpath, "Meta", "nsInfo.rds")
        nsInfo <- if(file.exists(nsInfoFilePath)) .readRDS(nsInfoFilePath)
        else parseNamespaceFile(package, package.lib, mustExist = FALSE)

        pkgInfoFP <- file.path(pkgpath, "Meta", "package.rds")
        if(file.exists(pkgInfoFP)) {
            pkgInfo <- .readRDS(pkgInfoFP)
            version <- pkgInfo$DESCRIPTION["Version"]
            ## we need to ensure that S4 dispatch is on now if the package
            ## will require it, or the exports will be incomplete.
            dependsMethods <- "methods" %in% names(pkgInfo$Depends)
            if(dependsMethods && pkgInfo$Built$R < "2.4.0")
                stop("package was installed prior to 2.4.0 and must be re-installed")
            if(dependsMethods) loadNamespace("methods")
        } else {
            version <- read.dcf(file.path(pkgpath, "DESCRIPTION"),
                                fields = "Version")
            ## stats4 depends on methods, but exports do not matter
            ## whilst it is being build on Unix
            dependsMethods <- FALSE
        }
        ns <- makeNamespace(package, version = version, lib = package.lib)
        on.exit(.Internal(unregisterNamespace(package)))

        ## process imports
        for (i in nsInfo$imports) {
            if (is.character(i))
                namespaceImport(ns, loadNamespace(i, c(lib.loc, .libPaths())))
            else
                namespaceImportFrom(ns,
                                    loadNamespace(i[[1L]],
                                                  c(lib.loc, .libPaths())),
                                    i[[2L]])
        }
        for(imp in nsInfo$importClasses)
            namespaceImportClasses(ns, loadNamespace(imp[[1L]],
                                                     c(lib.loc, .libPaths())),
                                   imp[[2L]])
        for(imp in nsInfo$importMethods)
            namespaceImportMethods(ns, loadNamespace(imp[[1L]],
                                                     c(lib.loc, .libPaths())),
                                   imp[[2L]])



        ## dynamic variable to allow/disable .Import and friends
        "__NamespaceDeclarativeOnly__" <- declarativeOnly

        ## store info for loading name space for loadingNamespaceInfo to read
        "__LoadingNamespaceInfo__" <- list(libname = package.lib,
                                           pkgname = package)

        env <- asNamespace(ns)
        ## save the package name in the environment
        assign(".packageName", package, envir = env)

        ## load the code
        codename <- strsplit(package, "_", fixed = TRUE)[[1L]][1L]
        codeFile <- file.path(pkgpath, "R", codename)
        if (file.exists(codeFile)) {
            res <- try(sys.source(codeFile, env, keep.source = keep.source))
            if(inherits(res, "try-error"))
                stop(gettextf("unable to load R code in package '%s'", package),
                     call. = FALSE, domain = NA)
        } else warning(gettextf("package '%s' contains no R code", package),
                       domain = NA)

        ## partial loading stops at this point
        ## -- used in preparing for lazy-loading
        if (partial) return(ns)

        ## lazy-load any sysdata
        dbbase <- file.path(pkgpath, "R", "sysdata")
        if (file.exists(paste(dbbase, ".rdb", sep = ""))) lazyLoad(dbbase, env)

        ## register any S3 methods
        registerS3methods(nsInfo$S3methods, package, env)

        ## load any dynamic libraries
        dlls <- list()
        dynLibs <- nsInfo$dynlibs
        for (i in seq_along(dynLibs)) {
            lib <- dynLibs[i]
            dlls[[lib]]  <- library.dynam(lib, package, package.lib)
               assignNativeRoutines(dlls[[lib]], lib, env,
                                    nsInfo$nativeRoutines[[lib]])

            ## If the DLL has a name as in useDynLib(alias = foo),
            ## then assign DLL reference to alias.  Check if
            ## names() is NULL to handle case that the nsInfo.rds
            ## file was created before the names were added to the
            ## dynlibs vector.
            if(!is.null(names(nsInfo$dynlibs))
               && names(nsInfo$dynlibs)[i] != "")
                assign(names(nsInfo$dynlibs)[i], dlls[[lib]], envir = env)
            setNamespaceInfo(env, "DLLs", dlls)
        }
        addNamespaceDynLibs(env, nsInfo$dynlibs)


        ## run the load hook
        runHook(".onLoad", package, env, package.lib, package)

        ## process exports, seal, and clear on.exit action
        exports <- nsInfo$exports

        for (p in nsInfo$exportPatterns)
            exports <- c(ls(env, pattern = p, all.names = TRUE), exports)
        ##
        if(.isMethodsDispatchOn() && methods:::.hasS4MetaData(ns) &&
           !identical(package, "methods") ) {
            ## cache generics, classes in this namespace (but not methods itself,
            ## which pre-cached at install time
            methods:::cacheMetaData(ns, TRUE, ns)
            ## process class definition objects
            expClasses <- nsInfo$exportClasses
            ##we take any pattern, but check to see if the matches are classes
            pClasses <- character(0L)
            aClasses <- methods:::getClasses(ns)
            for (p in nsInfo$exportClassPatterns) {
                pClasses <- c(aClasses[grep(p, aClasses)], pClasses)
            }
            pClasses <- unique(pClasses)
            if( length(pClasses) ) {
                good <- sapply(pClasses, methods:::isClass, where = ns)
                if( !any(good) ) warning(gettextf("exportClassPattern specified but no matching classes in %s", package))
                expClasses <- c(expClasses, pClasses[good])
            }
            if(length(expClasses)) {
                missingClasses <-
                    !sapply(expClasses, methods:::isClass, where = ns)
                if(any(missingClasses))
                    stop(gettextf("in '%s' classes for export not defined: %s",
                                  package,
                                  paste(expClasses[missingClasses],
                                        collapse = ", ")),
                         domain = NA)
                expClasses <- paste(methods:::classMetaName(""), expClasses,
                                    sep = "")
            }
            ## process methods metadata explicitly exported or
            ## implied by exporting the generic function.
            allGenerics <- unique(c(methods:::.getGenerics(ns),
                                   methods:::.getGenerics(parent.env(ns))))
            expMethods <- nsInfo$exportMethods
            expTables <- character()
            expMLists <- character()
            if(length(allGenerics)) {
                expMethods <-
                    unique(c(expMethods,
                             exports[!is.na(match(exports, allGenerics))]))
                missingMethods <- !(expMethods %in% allGenerics)
                if(any(missingMethods))
                    stop(gettextf("in '%s' methods for export not found: %s",
                                  package,
                                  paste(expMethods[missingMethods],
                                        collapse = ", ")),
                         domain = NA)
                ## Deprecated note:  the mlistPattern objects are deprecated in 2.7.0
                ## and will disappear later.  For now, deal with them if they exist
                ## but don't complain if they do not.
                mlistPattern <- methods:::methodsPackageMetaName("M","")
                allMethodLists <-
                    unique(c(methods:::.getGenerics(ns, mlistPattern),
                             methods:::.getGenerics(parent.env(ns),
                                                    mlistPattern)))
                tPrefix <- methods:::.TableMetaPrefix()
                allMethodTables <-
                    unique(c(methods:::.getGenerics(ns, tPrefix),
                             methods:::.getGenerics(parent.env(ns), tPrefix)))
                needMethods <-
                    (exports %in% allGenerics) & !(exports %in% expMethods)
                if(any(needMethods))
                    expMethods <- c(expMethods, exports[needMethods])
                ## Primitives must have their methods exported as long
                ## as a global table is used in the C code to dispatch them:
                ## The following keeps the exported files consistent with
                ## the internal table.
                pm <- allGenerics[!(allGenerics %in% expMethods)]
                if(length(pm)) {
                    prim <- logical(length(pm))
                    for(i in seq_along(prim)) {
                        f <- methods:::getFunction(pm[[i]], FALSE, FALSE, ns)
                        prim[[i]] <- is.primitive(f)
                    }
                    expMethods <- c(expMethods, pm[prim])
                }
                for(i in seq_along(expMethods)) {
                    mi <- expMethods[[i]]
                    if(!(mi %in% exports) &&
                       exists(mi, envir = ns, mode = "function",
                              inherits = FALSE))
                        exports <- c(exports, mi)
                    pattern <- paste(tPrefix, mi, ":", sep="")
                    ii <- grep(pattern, allMethodTables, fixed = TRUE)
                    if(length(ii)) {
			if(length(ii) > 1L) {
			    warning("Multiple methods tables found for '",
				    mi, "'", call. = FALSE)
			    ii <- ii[1L]
			}
                        expTables[[i]] <- allMethodTables[ii]
                        if(exists(allMethodLists[[ii]], envir = ns))
                            expMLists <- c(expMLists, allMethodLists[[ii]])
                     }
                    else { ## but not possible?
                      warning(gettextf("Failed to find metadata object for \"%s\"", mi))
                    }
                }
            }
            else if(length(expMethods))
                stop(gettextf("in '%s' methods specified for export, but none defined: %s",
                              package,
                              paste(expMethods, collapse = ", ")),
                     domain = NA)
            exports <- c(exports, expClasses,  expTables, expMLists)
        }
        namespaceExport(ns, exports)
        sealNamespace(ns)
        ## run user hooks here
        runUserHook(package, file.path(package.lib, package))
        on.exit()
        ns
    }
}

loadingNamespaceInfo <- function() {
    dynGet <- function(name, notFound = stop(name, " not found")) {
        n <- sys.nframe()
        while (n > 1) {
            n <- n - 1
            env <- sys.frame(n)
            if (exists(name, envir = env, inherits = FALSE))
                return(get(name, envir = env, inherits = FALSE))
        }
        notFound
    }
    dynGet("__LoadingNamespaceInfo__", stop("not loading a name space"))
}

topenv <- function(envir = parent.frame(),
                   matchThisEnv = getOption("topLevelEnvironment")) {
    while (! identical(envir, emptyenv())) {
        nm <- attributes(envir)[["names", exact = TRUE]]
        if ((is.character(nm) && length(grep("^package:" , nm))) ||
	    ## matchThisEnv is used in sys.source
            identical(envir, matchThisEnv) ||
            identical(envir, .GlobalEnv) ||
            identical(envir, baseenv()) ||
            .Internal(isNamespaceEnv(envir)) ||
	    ## packages except base and those with a separate namespace have .packageName
            exists(".packageName", envir = envir, inherits = FALSE))
            return(envir)
        else envir <- parent.env(envir)
    }
    return(.GlobalEnv)
}

unloadNamespace <- function(ns)
{
    ## only used to run .onUnload
    runHook <- function(hookname, env, ...) {
        if (exists(hookname, envir = env, inherits = FALSE)) {
            fun <- get(hookname, envir = env, inherits = FALSE)
            res <- tryCatch(fun(...), error=identity)
            if (inherits(res, "error")) {
                stop(gettextf("%s failed in unloadNamespace(\"%s\"), details:\n  call: %s\n  message: %s",
                              hookname, nsname,
                              deparse(conditionCall(res))[1L],
                              conditionMessage(res)),
                     call. = FALSE, domain = NA)
            }
        }
    }
    ns <- asNamespace(ns, base.OK = FALSE)
    nsname <- getNamespaceName(ns)
    pos <- match(paste("package", nsname, sep = ":"), search())
    if (! is.na(pos)) detach(pos = pos)
    users <- getNamespaceUsers(ns)
    if (length(users))
        stop(gettextf("name space '%s' is still used by: %s",
                      getNamespaceName(ns),
                      paste(sQuote(users), collapse = ", ")),
             domain = NA)
    nspath <- getNamespaceInfo(ns, "path")
    hook <- getHook(packageEvent(nsname, "onUnload")) # might be list()
    for(fun in rev(hook)) try(fun(nsname, nspath))
    try(runHook(".onUnload", ns, nspath))
    .Internal(unregisterNamespace(nsname))
    if(.isMethodsDispatchOn() && methods:::.hasS4MetaData(ns))
        methods:::cacheMetaData(ns, FALSE, ns)
    .Call("R_lazyLoadDBflush",
          paste(nspath, "/R/", nsname, ".rdb", sep=""),
          PACKAGE="base")
    invisible()
}

.Import <- function(...) {
    dynGet <- function(name, notFound = stop(name, " not found")) {
        n <- sys.nframe()
        while (n > 1) {
            n <- n - 1
            env <- sys.frame(n)
            if (exists(name, envir = env, inherits = FALSE))
                return(get(name, envir = env, inherits = FALSE))
        }
        notFound
    }
    if (dynGet("__NamespaceDeclarativeOnly__", FALSE))
        stop("imperative name space directives are disabled")
    envir <- parent.frame()
    names <- as.character(substitute(list(...)))[-1L]
    for (n in names)
        namespaceImportFrom(envir, n)
}

.ImportFrom <- function(name, ...) {
    dynGet <- function(name, notFound = stop(name, " not found")) {
        n <- sys.nframe()
        while (n > 1) {
            n <- n - 1
            env <- sys.frame(n)
            if (exists(name, envir = env, inherits = FALSE))
                return(get(name, envir = env, inherits = FALSE))
        }
        notFound
    }
    if (dynGet("__NamespaceDeclarativeOnly__", FALSE))
        stop("imperative name space directives are disabled")
    envir <- parent.frame()
    name <-  as.character(substitute(name))
    names <- as.character(substitute(list(...)))[-1L]
    namespaceImportFrom(envir, name, names)
}

.Export <- function(...) {
    dynGet <- function(name, notFound = stop(name, " not found")) {
        n <- sys.nframe()
        while (n > 1) {
            n <- n - 1
            env <- sys.frame(n)
            if (exists(name, envir = env, inherits = FALSE))
                return(get(name, envir = env, inherits = FALSE))
        }
        notFound
    }
    if (dynGet("__NamespaceDeclarativeOnly__", FALSE))
        stop("imperative name space directives are disabled")
    ns <- topenv(parent.frame(), NULL)
    if (identical(ns, .BaseNamespaceEnv))
        warning("all objects in base name space are currently exported.")
    else if (! isNamespace(ns))
        stop("can only export from a name space")
    else {
        names <- as.character(substitute(list(...)))[-1L]
        namespaceExport(ns, names)
    }
}

.S3method <- function(generic, class, method) {
    dynGet <- function(name, notFound = stop(name, " not found")) {
        n <- sys.nframe()
        while (n > 1) {
            n <- n - 1
            env <- sys.frame(n)
            if (exists(name, envir = env, inherits = FALSE))
                return(get(name, envir = env, inherits = FALSE))
        }
        notFound
    }
    if (dynGet("__NamespaceDeclarativeOnly__", FALSE))
        stop("imperative name space directives are disabled")
    generic <- as.character(substitute(generic))
    class <- as.character(substitute(class))
    if (missing(method)) method <- paste(generic, class, sep = ".")
    registerS3method(generic, class, method, envir = parent.frame())
    invisible(NULL)
}

isNamespace <- function(ns) .Internal(isNamespaceEnv(ns))

isBaseNamespace <- function(ns) identical(ns, .BaseNamespaceEnv)

getNamespaceInfo <- function(ns, which) {
    ns <- asNamespace(ns, base.OK = FALSE)
    info <- get(".__NAMESPACE__.", envir = ns, inherits = FALSE)
    get(which, envir = info, inherits = FALSE)
}

setNamespaceInfo <- function(ns, which, val) {
    ns <- asNamespace(ns, base.OK = FALSE)
    info <- get(".__NAMESPACE__.", envir = ns, inherits = FALSE)
    assign(which, val, envir = info)
}

asNamespace <- function(ns, base.OK = TRUE) {
    if (is.character(ns) || is.name(ns))
        ns <- getNamespace(ns)
    if (! isNamespace(ns))
        stop("not a name space")
    else if (! base.OK && isBaseNamespace(ns))
        stop("operation not allowed on base name space")
    else ns
}

namespaceImport <- function(self, ...)
    for (ns in list(...)) namespaceImportFrom(self, asNamespace(ns))

namespaceImportFrom <- function(self, ns, vars, generics, packages)
{
    addImports <- function(ns, from, what) {
        imp <- structure(list(what), names = getNamespaceName(from))
        imports <- getNamespaceImports(ns)
        setNamespaceInfo(ns, "imports", c(imports, imp))
    }
    namespaceIsSealed <- function(ns)
       environmentIsLocked(ns)
    makeImportExportNames <- function(spec) {
        old <- as.character(spec)
        new <- names(spec)
        if (is.null(new)) new <- old
        else new[new == ""] <- old[new == ""]
        names(old) <- new
        old
    }
    whichMethodMetaNames <- function(impvars) {
        if(!.isMethodsDispatchOn())
            return(numeric())
        mm <- ".__T__"
        seq_along(impvars)[substr(impvars, 1L, nchar(mm, type = "c")) == mm]
    }
    if (is.character(self))
        self <- getNamespace(self)
    ns <- asNamespace(ns)
    nsname <- getNamespaceName(ns)
    impvars <- if (missing(vars)) getNamespaceExports(ns) else vars
    impvars <- makeImportExportNames(impvars)
    impnames <- names(impvars)
    if (anyDuplicated(impnames)) {
        stop("duplicate import names ",
             paste(impnames[duplicated(impnames)], collapse = ", "))
    }
    if (isNamespace(self) && isBaseNamespace(self)) {
        impenv <- self
        msg <- "replacing local value with import"
        register <- FALSE
    }
    else if (isNamespace(self)) {
        if (namespaceIsSealed(self))
            stop("cannot import into a sealed name space")
        impenv <- parent.env(self)
        msg <- "replacing previous import"
        register <- TRUE
    }
    else if (is.environment(self)) {
        impenv <- self
        msg <- "replacing local value with import"
        register <- FALSE
    }
    else stop("invalid import target")
    which <- whichMethodMetaNames(impvars)
    if(length(which)) {
	## If methods are already in impenv, merge and don't import
	delete <- integer()
	for(i in which) {
	    methodsTable <- .mergeImportMethods(impenv, ns, impvars[[i]])
	    if(is.null(methodsTable))
	    {} ## first encounter, just import it
	    else { ##
		delete <- c(delete, i)
		## eventually mlist objects will disappear, for now
		## just don't import any duplicated names
		mlname = sub("__T__", "__M__", impvars[[i]], fixed=TRUE)
		ii = match(mlname, impvars, 0L)
		if(ii > 0)
		    delete <- c(delete, ii)
		if(!missing(generics)) {
		    genName <- generics[[i]]
                    if(i > length(generics) || !nzchar(genName))
                      {warning("got invalid index for importing ",mlname); next}
		    fdef <- methods:::getGeneric(genName,
                                                 where = impenv,
                                                 package = packages[[i]])
		    if(is.null(fdef))
			warning(gettextf("Found methods to import for function \"%s\" but not the generic itself",
					 genName))
		    else
			methods:::.updateMethodsInTable(fdef, ns, TRUE)
		}
	    }
	}
	if(length(delete)) {
	    impvars <- impvars[-delete]
	    impnames <- impnames[-delete]
	}
    }
    for (n in impnames)
	if (exists(n, envir = impenv, inherits = FALSE)) {
	    if (.isMethodsDispatchOn() && methods:::isGeneric(n, ns)) {
		## warn only if generic overwrites a function which
		## it was not derived from
		genNs <- methods:::slot(get(n, envir = ns), "package")
		genImpenv <- environmentName(environment(get(n, envir = impenv)))
		if (!identical(genNs, genImpenv) ||
		    ## warning if generic overwrites another generic
		    methods:::isGeneric(n, impenv)) {}
                else next
	    }
            ## this is always called from another function, so reporting call
            ## is unhelpful
            warning(msg, " ", sQuote(n), " when loading ", sQuote(nsname),
                    call. = FALSE, domain = NA)
	}
    importIntoEnv(impenv, impnames, ns, impvars)
    if (register)
        addImports(self, ns, if (missing(vars)) TRUE else impvars)
}

namespaceImportClasses <- function(self, ns, vars) {
    for(i in seq_along(vars))
        vars[[i]] <- methods:::classMetaName(vars[[i]])
    namespaceImportFrom(self, asNamespace(ns), vars)
}

namespaceImportMethods <- function(self, ns, vars) {
    allVars <- character()
    allFuns <- methods:::.getGenerics(ns)
    packages <- attr(allFuns, "package")
    tPrefix <- methods:::.TableMetaPrefix()
    pkg <- methods:::getPackageName(ns)
    allMethodTables <- methods:::.getGenerics(ns, tPrefix)
    if(any(is.na(match(vars, allFuns))))
        stop(gettextf("requested 'methods' objects not found in environment/package '%s': %s",
                      pkg,
                      paste(vars[is.na(match(vars, allFuns))],
                            collapse = ", ")), domain = NA)
    for(i in seq_along(allFuns)) {
        ## import methods list objects if asked for
        ## or if the corresponding generic was imported
        g <- allFuns[[i]]
        if(exists(g, envir = self, inherits = FALSE) # already imported
           || g %in% vars) { # requested explicitly
            tbl <- methods:::.TableMetaName(g, packages[[i]])
            if(is.null(.mergeImportMethods(self, ns, tbl))) # a new methods table
               allVars <- c(allVars, tbl) # import it;else, was merged
        }
        if(g %in% vars && !exists(g, envir = self, inherits = FALSE) &&
           exists(g, envir = ns, inherits = FALSE) &&
           methods:::is(get(g, envir = ns), "genericFunction"))
            allVars <- c(allVars, g)
    }
    namespaceImportFrom(self, asNamespace(ns), allVars, allFuns, packages)
}

importIntoEnv <- function(impenv, impnames, expenv, expnames) {
    exports <- getNamespaceInfo(expenv, "exports")
    ex <- .Internal(ls(exports, TRUE))
    if(!all(expnames %in% ex)) {
        miss <- expnames[! expnames %in% ex]
        stop(sprintf(ngettext(length(miss),
                              "object '%s' is not exported by 'namespace:%s'",
                              "objects '%s' are not exported by 'namespace:%s'"),
                     paste(sQuote(miss), collapse = ", "),
                     getNamespaceName(expenv)),
             domain = NA)
    }
    expnames <- unlist(lapply(expnames, get, envir = exports, inherits = FALSE))
    if (is.null(impnames)) impnames <- character(0L)
    if (is.null(expnames)) expnames <- character(0L)
    .Internal(importIntoEnv(impenv, impnames, expenv, expnames))
}

namespaceExport <- function(ns, vars) {
    namespaceIsSealed <- function(ns)
       environmentIsLocked(ns)
    if (namespaceIsSealed(ns))
        stop("cannot add to exports of a sealed name space")
    ns <- asNamespace(ns, base.OK = FALSE)
    if (length(vars)) {
        addExports <- function(ns, new) {
            exports <- getNamespaceInfo(ns, "exports")
            expnames <- names(new)
            intnames <- new
            objs <- .Internal(ls(exports, TRUE))
            ex <- expnames %in% objs
            if(any(ex))
                warning(sprintf(ngettext(sum(ex),
                                         "previous export '%s' is being replaced",
                                         "previous exports '%s' are being replaced"),
                                paste(sQuote(expnames[ex]), collapse = ", ")),
                        call. = FALSE, domain = NA)
            for (i in seq_along(new))
                assign(expnames[i], intnames[i], envir = exports)
        }
        makeImportExportNames <- function(spec) {
            old <- as.character(spec)
            new <- names(spec)
            if (is.null(new)) new <- old
            else new[new == ""] <- old[new == ""]
            names(old) <- new
            old
        }
        new <- makeImportExportNames(unique(vars))
        ## calling exists each time is too slow, so do two phases
        undef <- new[! new %in% .Internal(ls(ns, TRUE))]
        undef <- undef[! sapply(undef, exists, envir = ns)]
        if (length(undef)) {
            undef <- do.call("paste", as.list(c(undef, sep = ", ")))
            stop("undefined exports: ", undef)
        }
        if(.isMethodsDispatchOn()) .mergeExportMethods(new, ns)
        addExports(ns, new)
    }
}

.mergeExportMethods <- function(new, ns) {
##    if(!.isMethodsDispatchOn()) return(FALSE)
    mm <- methods:::methodsPackageMetaName("M","")
    newMethods <- new[substr(new, 1L, nchar(mm, type = "c")) == mm]
    nsimports <- parent.env(ns)
    for(what in newMethods) {
        if(exists(what, envir = nsimports, inherits = FALSE)) {
            m1 <- get(what, envir = nsimports)
            m2 <- get(what, envir = ns)
            assign(what, envir = ns, methods:::mergeMethods(m1, m2))
        }
    }
}


## NB this needs a decorated name, foo_ver, if appropriate
packageHasNamespace <- function(package, package.lib) {
    namespaceFilePath <- function(package, package.lib)
        file.path(package.lib, package, "NAMESPACE")
    file.exists(namespaceFilePath(package, package.lib))
}

parseNamespaceFile <- function(package, package.lib, mustExist = TRUE)
{
    namespaceFilePath <- function(package, package.lib)
        file.path(package.lib, package, "NAMESPACE")

    ## These two functions are essentially local to the parsing of
    ## the namespace file and don't need to be made available to
    ## users.  These manipulate the data from useDynLib() directives
    ## for the same DLL to determine how to map the symbols to R
    ## variables.

    nativeRoutineMap <-
        ## Creates a new NativeRoutineMap.
        function(useRegistration, symbolNames, fixes) {
            proto <- list(useRegistration = FALSE,
                          symbolNames = character(0L))
            class(proto) <- "NativeRoutineMap"

            mergeNativeRoutineMaps(proto, useRegistration, symbolNames, fixes)
        }

    mergeNativeRoutineMaps <-
        ## Merges new settings into a NativeRoutineMap
        function(map, useRegistration, symbolNames, fixes) {
            if(!useRegistration)
                names(symbolNames) <-
                    paste(fixes[1L],  names(symbolNames), fixes[2L], sep = "")
            else
                map$registrationFixes <- fixes
            map$useRegistration <- map$useRegistration || useRegistration
            map$symbolNames <- c(map$symbolNames, symbolNames)
            map
        }

    nsFile <- namespaceFilePath(package, package.lib)
    descfile <- file.path(package.lib, package, "DESCRIPTION")
    enc <- NA
    if (file.exists(descfile)) {
        dcf <- read.dcf(file = descfile)
        if(NROW(dcf) >= 1) enc <- as.list(dcf[1, ])[["Encoding"]]
        if(is.null(enc)) enc <- NA
    }
    if (file.exists(nsFile))
        directives <- if (!is.na(enc) &&
                          ! Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX")) {
	    con <- file(nsFile, encoding=enc)
            on.exit(close(con))
	    parse(con)
        } else parse(nsFile)
    else if (mustExist)
        stop(gettextf("package '%s' has no NAMESPACE file", package),
             domain = NA)
    else directives <- NULL
    exports <- character(0L)
    exportPatterns <- character(0L)
    exportClasses <- character(0L)
    exportClassPatterns <- character(0L)
    exportMethods <- character(0L)
    imports <- list()
    importMethods <- list()
    importClasses <- list()
    dynlibs <- character(0L)
    S3methods <- matrix(NA_character_, 500L, 3L)
    nativeRoutines <- list()
    nS3 <- 0
    parseDirective <- function(e) {
        ## trying to get more helpful error message:
	asChar <- function(cc) {
	    r <- as.character(cc)
	    if(any(r == ""))
		stop(gettextf("empty name in directive '%s' in NAMESPACE file",
			      as.character(e[[1L]])),
		     domain = NA)
	    r
	}
        switch(as.character(e[[1L]]),
               "if" = if (eval(e[[2L]], .GlobalEnv))
               parseDirective(e[[3L]])
               else if (length(e) == 4L)
               parseDirective(e[[4L]]),
               "{" =  for (ee in as.list(e[-1L])) parseDirective(ee),
               "=", "<-" = {
                   parseDirective(e[[3L]])
                   if(as.character(e[[3L]][[1L]]) == "useDynLib")
                       names(dynlibs)[length(dynlibs)] <<- asChar(e[[2L]])
               },
               export = {
                   exp <- e[-1L]
                   exp <- structure(asChar(exp), names = names(exp))
                   exports <<- c(exports, exp)
               },
               exportPattern = {
                   pat <- asChar(e[-1L])
                   exportPatterns <<- c(pat, exportPatterns)
               },
               exportClassPattern = {
                   pat <- asChar(e[-1L])
                   exportClassPatterns <<- c(pat, exportClassPatterns)
               },
               exportClass = , exportClasses = {
                   exportClasses <<- c(asChar(e[-1L]), exportClasses)
               },
               exportMethods = {
                   exportMethods <<- c(asChar(e[-1L]), exportMethods)
               },
               import = imports <<- c(imports,as.list(asChar(e[-1L]))),
               importFrom = {
                   imp <- e[-1L]
                   ivars <- imp[-1L]
                   inames <- names(ivars)
                   imp <- list(asChar(imp[1L]),
                               structure(asChar(ivars), names = inames))
                   imports <<- c(imports, list(imp))
               },
               importClassFrom = , importClassesFrom = {
                   imp <- asChar(e[-1L])
                   pkg <- imp[[1L]]
                   impClasses <- imp[-1L]
                   imp <- list(asChar(pkg), asChar(impClasses))
                   importClasses <<- c(importClasses, list(imp))
               },
               importMethodsFrom = {
                   imp <- asChar(e[-1L])
                   pkg <- imp[[1L]]
                   impMethods <- imp[-1L]
                   imp <- list(asChar(pkg), asChar(impMethods))
                   importMethods <<- c(importMethods, list(imp))
               },
               useDynLib = {

                   ## This attempts to process as much of the
                   ## information as possible when NAMESPACE is parsed
                   ## rather than when it is loaded and creates
                   ## NativeRoutineMap objects to handle the mapping
                   ## of symbols to R variable names.

                   ## The name is the second element after useDynLib
                   dyl <- as.character(e[2L])
                   ## We ensure uniqueness at the end.
                   dynlibs <<-
                       structure(c(dynlibs, dyl),
                                 names = c(names(dynlibs),
                                 ifelse(!is.null(names(e)) &&
                                        names(e)[2L] != "", names(e)[2L], "" )))
                   if (length(e) > 2L) {
                       ## Author has specified some mappings for the symbols

                       symNames <- as.character(e[-c(1L, 2L)])
                       names(symNames) <- names(e[-c(1, 2)])

                       ## If there are no names, then use the names of
                       ## the symbols themselves.
                       if (length(names(symNames)) == 0L)
                           names(symNames) = symNames
                       else if (any(w <- names(symNames) == "")) {
                           names(symNames)[w] = symNames[w]
                       }

                       ## For each DLL, we build up a list the (R
                       ## variable name, symbol name) mappings. We do
                       ## this in a NativeRoutineMap object and we
                       ## merge potentially multiple useDynLib()
                       ## directives for the same DLL into a single
                       ## map.  Then we have separate NativeRoutineMap
                       ## for each different DLL.  E.g. if we have
                       ## useDynLib(foo, a, b, c) and useDynLib(bar,
                       ## a, x, y) we would maintain and resolve them
                       ## separately.

                       dup <- duplicated(names(symNames))
                       if (any(dup))
                           warning("duplicated symbol names ",
                                   paste(names(symNames)[dup],
                                         collapse = ", "),
                                   " in useDynLib(", dyl, ")")

                       symNames <- symNames[!dup]

                       ## Deal with any prefix/suffix pair.
                       fixes <- c("", "")
                       idx <- match(".fixes", names(symNames))
                       if(!is.na(idx)) {
                           ## Take .fixes and treat it as a call,
                           ## e.g. c("pre", "post") or a regular name
                           ## as the prefix.
                           if(symNames[idx] != "") {
                               e <- parse(text = symNames[idx])[[1L]]
                               if(is.call(e))
                                   val <- eval(e)
                               else
                                   val <- as.character(e)
                               if(length(val))
                                   fixes[seq_along(val)] <- val
                           }
                           symNames <- symNames[-idx]
                       }

                       ## Deal with a .registration entry. It must be
                       ## .registration = value and value will be coerced
                       ## to a logical.
                       useRegistration <- FALSE
                       idx <- match(".registration", names(symNames))
                       if(!is.na(idx)) {
                           useRegistration <- as.logical(symNames[idx])
                           symNames <- symNames[-idx]
                       }

                       ## Now merge into the NativeRoutineMap.
                       nativeRoutines[[ dyl ]] <<-
                           if(dyl %in% names(nativeRoutines))
                               mergeNativeRoutineMaps(nativeRoutines[[ dyl ]],
                                                      useRegistration,
                                                      symNames, fixes)
                           else
                               nativeRoutineMap(useRegistration, symNames,
                                                fixes)
                   }
               },
               S3method = {
                   spec <- e[-1L]
                   if (length(spec) != 2L && length(spec) != 3L)
                       stop(gettextf("bad 'S3method' directive: %s",
                                     deparse(e)),
                            call. = FALSE, domain = NA)
                   nS3 <<- nS3 + 1L
                   if(nS3 > 500L)
                       stop("too many 'S3method' directives", call. = FALSE)
                   S3methods[nS3, seq_along(spec)] <<- asChar(spec)
               },
               stop(gettextf("unknown namespace directive: %s", deparse(e)),
                    call. = FALSE, domain = NA)
               )
    }
    for (e in directives)
        parseDirective(e)

    dynlibs <- unique(dynlibs)
    list(imports = imports, exports = exports, exportPatterns = exportPatterns,
         importClasses = importClasses, importMethods = importMethods,
         exportClasses = exportClasses,  exportMethods = exportMethods,
         exportClassPatterns = exportClassPatterns,
         dynlibs = dynlibs, nativeRoutines = nativeRoutines,
         S3methods = S3methods[seq_len(nS3), ,drop = FALSE])
} ## end{parseNamespaceFile}

registerS3method <- function(genname, class, method, envir = parent.frame()) {
    addNamespaceS3method <- function(ns, generic, class, method) {
        regs <- getNamespaceInfo(ns, "S3methods")
        regs <- rbind(regs, c(generic, class, method))
        setNamespaceInfo(ns, "S3methods", regs)
    }
    groupGenerics <- c("Math", "Ops",  "Summary", "Complex")
    defenv <- if(genname %in% groupGenerics) .BaseNamespaceEnv
    else {
        genfun <- get(genname, envir = envir)
        if(.isMethodsDispatchOn() && methods:::is(genfun, "genericFunction"))
            genfun <- methods:::slot(genfun, "default")@methods$ANY
        if (typeof(genfun) == "closure") environment(genfun)
        else .BaseNamespaceEnv
    }
    if (! exists(".__S3MethodsTable__.", envir = defenv, inherits = FALSE))
        assign(".__S3MethodsTable__.", new.env(hash = TRUE, parent = baseenv()),
               envir = defenv)
    table <- get(".__S3MethodsTable__.", envir = defenv, inherits = FALSE)
    if (is.character(method)) {
        assignWrapped <- function(x, method, home, envir) {
            method <- method            # force evaluation
            home <- home                # force evaluation
            delayedAssign(x, get(method, envir = home), assign.env = envir)
        }
        if(!exists(method, envir = envir)) {
            warning(gettextf("S3 method '%s' was declared in NAMESPACE but not found",
                             method), call. = FALSE)
        } else {
	    assignWrapped(paste(genname, class, sep = "."), method, home = envir,
	    	    envir = table)
        }
    }
    else if (is.function(method))
        assign(paste(genname, class, sep = "."), method, envir = table)
    else stop("bad method")
    if (isNamespace(envir) && ! identical(envir, .BaseNamespaceEnv))
        addNamespaceS3method(envir, genname, class, method)
}


registerS3methods <- function(info, package, env)
{
    n <- NROW(info)
    if(n == 0) return()

    assignWrapped <- function(x, method, home, envir) {
	method <- method            # force evaluation
	home <- home                # force evaluation
	delayedAssign(x, get(method, envir = home), assign.env = envir)
    }
    .registerS3method <- function(genname, class, method, nm, envir)
    {
        ## S3 generics should either be imported explicitly or be in
        ## the base namespace, so we start the search at the imports
        ## environment, parent.env(envir), which is followed by the
        ## base namespace.  (We have already looked in the namespace.)
        ## However, in case they have not been imported, we first
        ## look up where some commonly used generics are (including the
        ## group generics).
        defenv <- if(!is.na(w <- .knownS3Generics[genname])) asNamespace(w)
        else {
            if(!exists(genname, envir = parent.env(envir)))
                stop(gettextf("object '%s' not found whilst loading namespace '%s'",
                              genname, package), call. = FALSE, domain = NA)
            genfun <- get(genname, envir = parent.env(envir))
            if(.isMethodsDispatchOn() && methods:::is(genfun, "genericFunction")) {
                genfun <- methods:::slot(genfun, "default")@methods$ANY
                warning(gettextf("found an S4 version of '%s' so it has not been imported correctly",
                                 genname), call. = FALSE, domain = NA)
            }
            if (typeof(genfun) == "closure") environment(genfun)
            else .BaseNamespaceEnv
        }
        if (! exists(".__S3MethodsTable__.", envir = defenv, inherits = FALSE))
            assign(".__S3MethodsTable__.", new.env(hash = TRUE, parent = baseenv()),
                   envir = defenv)
        table <- get(".__S3MethodsTable__.", envir = defenv, inherits = FALSE)
	assignWrapped(nm, method, home = envir, envir = table)
    }

    methname <- paste(info[,1], info[,2], sep = ".")
    z <- is.na(info[,3])
    info[z,3] <- methname[z]
    Info <- cbind(info, methname)
    loc <- .Internal(ls(env, TRUE))
    notex <- !(info[,3] %in% loc)
    if(any(notex))
        warning(sprintf(ngettext(sum(notex),
                                 "S3 method %s was declared in NAMESPACE but not found",
                                 "S3 methods %s were declared in NAMESPACE but not found"),
                        paste(sQuote(info[notex, 3]), collapse = ", ")),
                call. = FALSE, domain = NA)
    Info <- Info[!notex, , drop = FALSE]

    ## Do local generics first (this could be load-ed if pre-computed).
    ## However, the local generic could be an S4 takeover of a non-local
    ## (or local) S3 generic.  We can't just pass S4 generics on to
    ## .registerS3method as that only looks non-locally (for speed).
    l2 <- localGeneric <- Info[,1] %in% loc
    if(.isMethodsDispatchOn())
        for(i in which(localGeneric)) {
            genfun <- get(Info[i, 1], envir = env)
            if(methods:::is(genfun, "genericFunction")) {
                localGeneric[i] <- FALSE
                registerS3method(Info[i, 1], Info[i, 2], Info[i, 3], env)
            }
        }
    if(any(localGeneric)) {
        lin <- Info[localGeneric, , drop = FALSE]
        S3MethodsTable <-
            get(".__S3MethodsTable__.", envir = env, inherits = FALSE)
        ## we needed to move this to C for speed.
        ## for(i in seq_len(nrow(lin)))
        ##    assign(lin[i,4], get(lin[i,3], envir = env),
        ##           envir = S3MethodsTable)
        .Internal(importIntoEnv(S3MethodsTable, lin[,4], env, lin[,3]))
    }

    ## now the rest
    fin <- Info[!l2, , drop = FALSE]
    for(i in seq_len(nrow(fin)))
        .registerS3method(fin[i, 1], fin[i, 2], fin[i, 3], fin[i, 4], env)

    setNamespaceInfo(env, "S3methods",
                     rbind(info, getNamespaceInfo(env, "S3methods")))
}

.mergeImportMethods <- function(impenv, expenv, metaname)
{
    expMethods <- get(metaname, envir = expenv)
    if(exists(metaname, envir = impenv, inherits = FALSE)) {
        impMethods <- get(metaname, envir = impenv)
        assign(metaname,
               methods:::.mergeMethodsTable2(impMethods,
                                             expMethods, expenv, metaname),
               envir = impenv)
        impMethods
    } else NULL
}
#  File src/library/base/R/notyet.R
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

.NotYetImplemented <- function ()
    stop(gettextf("'%s' is not implemented yet",
                  as.character(sys.call(sys.parent())[[1L]])), call. = FALSE)

.NotYetUsed <- function(arg, error = TRUE) {
    msg <- gettextf("argument '%s' is not used (yet)", arg)
    if(error) stop(msg, domain = NA, call. = FALSE)
    else warning(msg, domain = NA, call. = FALSE)
}
#  File src/library/base/R/options.R
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

options <- function(...)
    .Internal(options(...))

getOption <- function(x, default = NULL)
{
    ## To avoid always performing the %in%,
    ## we use the original code if default is not specified.
    if(missing(default)) return(options(x)[[1L]])

    if(x %in% names(options())) options(x)[[1L]] else default
}
#  File src/library/base/R/outer.R
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

outer <- function (X, Y, FUN = "*", ...)
{
    # no.nx <- is.null(nx <- dimnames(X <- as.array(X))); dX <- dim(X)
    # no.ny <- is.null(ny <- dimnames(Y <- as.array(Y))); dY <- dim(Y)

    if(is.array(X)) {
        dX <- dim(X)
        nx <- dimnames(X)
        no.nx <- is.null(nx)
    } else { # a vector
        dX <- length(X)
        no.nx <- is.null(names(X))
        if(!no.nx) nx <- list(names(X))
    }
    if(is.array(Y)) {
        dY <- dim(Y)
        ny <- dimnames(Y)
        no.ny <- is.null(ny)
    } else { # a vector
        dY <- length(Y)
        no.ny <- is.null(names(Y))
        if(!no.ny) ny <- list(names(Y))
    }
    if (is.character(FUN) && FUN=="*") {
        # this is for numeric vectors, so dropping attributes is OK
        robj <- as.vector(X) %*% t(as.vector(Y))
        dim(robj) <- c(dX, dY)
    } else {
        FUN <- match.fun(FUN)
        ## Y may have a class, so don't use rep.int
        Y <- rep(Y, rep.int(length(X), length(Y)))
        ##  length.out is not an argument of the generic rep()
        ##  X <- rep(X, length.out = length(Y))
        if(length(X))
            X <- rep(X, times = ceiling(length(Y)/length(X)))
        robj <- FUN(X, Y, ...)
        dim(robj) <- c(dX, dY) # careful not to lose class here
    }
    ## no dimnames if both don't have ..
    if(no.nx) nx <- vector("list", length(dX)) else
    if(no.ny) ny <- vector("list", length(dY))
    if(!(no.nx && no.ny))
	dimnames(robj) <- c(nx, ny)
    robj
}

## Binary operator, hence don't simply do "%o%" <- outer.
"%o%" <- function(X, Y) outer(X, Y)
#  File src/library/base/R/pairlist.R
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

as.pairlist <- function(x) .Internal(as.vector(x, "pairlist"))
pairlist <- function(...) as.pairlist(list(...))
## This is now .Primitive:
##is.pairlist <- function(x) typeof(x) == "pairlist"
#  File src/library/base/R/parse.R
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

parse <- function(file = "", n = NULL, text = NULL, prompt = "?",
                  srcfile = NULL, encoding = "unknown")
{
    if(!is.null(text)) {
    	if (length(as.character(text)) == 0L)
	    return(expression())
	if (missing(srcfile) && isTRUE(getOption("keep.source")))
	    srcfile <- srcfilecopy("<text>", text)
    }
    if(is.character(file))
        if(file == "") file <- stdin()
        else {
            if (missing(srcfile) && isTRUE(getOption("keep.source")))
        	srcfile <- srcfile(file, encoding)
            file <- file(file, "r")
            on.exit(close(file))
        }
    .Internal(parse(file, n, text, prompt, srcfile, encoding))
}
#  File src/library/base/R/paste.R
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

paste <- function (..., sep = " ", collapse = NULL)
    .Internal(paste(list(...), sep, collapse))


##=== Could we consider a  .Primitive  *fast*
##  paste2 <- function(x,y)  paste(x,y, sep='')

##=== Could we extend  paste(.) to (optionally) accept a
##    2-vector for collapse ?	 With the following functionality

##- paste.extra <- function(r, collapse=c(", "," and ")) {
##-	    n <- length(r)
##-	    if(n <= 1) paste(r)
##-	    else
##-	      paste(paste(r[-n],collapse=collapse[1L]),
##-		    r[n], sep=collapse[min(2,length(collapse))])
##- }
#  File src/library/base/R/pmax.R
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

pmin.int <- function(..., na.rm = FALSE) .Internal(pmin(na.rm, ...))
pmax.int <- function(..., na.rm = FALSE) .Internal(pmax(na.rm, ...))

pmax <- function (..., na.rm = FALSE)
{
    elts <- list(...)
    if(length(elts) == 0L) stop("no arguments")
    if(all(sapply(elts, function(x) is.atomic(x) && !is.object(x)))) {
        ## NB: NULL passes is.atomic
        mmm <- .Internal(pmax(na.rm, ...))
    } else {
        mmm <- elts[[1L]]
        attr(mmm, "dim") <- NULL  # dim<- would drop names
        has.na <- FALSE
        for (each in elts[-1L]) {
            attr(each, "dim") <- NULL
            if(length(mmm) < (m <- length(each)))
                mmm <- rep(mmm, length.out=m)
            else if(length(each) < (m <- length(mmm)))
                each <- rep(each, length.out=m)
            nas <- cbind(is.na(mmm), is.na(each))
            if(has.na || (has.na <- any(nas))) {
                mmm[nas[, 1L]] <- each[nas[, 1L]]
                each[nas[, 2L]] <- mmm[nas[, 2L]]
            }
            change <- mmm < each
            change <- change & !is.na(change)
            mmm[change] <- each[change]
            if (has.na && !na.rm) mmm[nas[, 1L] | nas[, 2L]] <- NA
        }
    }
    mostattributes(mmm) <- attributes(elts[[1L]])
    mmm
}

pmin <- function (..., na.rm = FALSE)
{
    elts <- list(...)
    if(length(elts) == 0L) stop("no arguments")
    if(all(sapply(elts, function(x) is.atomic(x) && !is.object(x)))) {
        mmm <- .Internal(pmin(na.rm, ...))
    } else {
        mmm <- elts[[1L]]
        attr(mmm, "dim") <- NULL  # dim<- would drop names
        has.na <- FALSE
        for (each in elts[-1L]) {
            attr(each, "dim") <- NULL
            if(length(mmm) < (m <- length(each)))
                mmm <- rep(mmm, length.out=m)
            else if(length(each) < (m <- length(mmm)))
                each <- rep(each, length.out=m)
            nas <- cbind(is.na(mmm), is.na(each))
            if(has.na || (has.na <- any(nas))) {
                mmm[nas[, 1L]] <- each[nas[, 1L]]
                each[nas[, 2L]] <- mmm[nas[, 2L]]
            }
            change <- mmm > each
            change <- change & !is.na(change)
            mmm[change] <- each[change]
            if (has.na && !na.rm) mmm[nas[, 1L] | nas[, 2L]] <- NA
        }
    }
    mostattributes(mmm) <- attributes(elts[[1L]])
    mmm
}
#  File src/library/base/R/pretty.R
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

pretty <- function(x, n=5, min.n= n %/% 3, shrink.sml = 0.75,
                   high.u.bias = 1.5, u5.bias = .5 + 1.5*high.u.bias,
                   eps.correct = 0)
{
    x <- as.numeric(x)
    if(length(x)==0L)
	return(x)
    x <- x[is.finite(x)]
    if(is.na(n <- as.integer(n[1L])) || n < 0L)# n=0 !!
	stop("invalid 'n' value")
    if(!is.numeric(shrink.sml) || shrink.sml <= 0)
	stop("'shrink.sml' must be numeric > 0")
    if((min.n <- as.integer(min.n)) < 0 || min.n > n)
	stop("'min.n' must be non-negative integer <= n")
    if(!is.numeric(high.u.bias) || high.u.bias < 0)
	stop("'high.u.bias' must be non-negative numeric")
    if(!is.numeric(u5.bias) || u5.bias < 0)
	stop("'u5.bias' must be non-negative numeric")
    if((eps.correct <- as.integer(eps.correct)) < 0L || eps.correct > 2L)
	stop("'eps.correct' must be 0, 1, or 2")
    z <- .C("R_pretty", l=as.double(min(x)), u=as.double(max(x)),
            n = n,
            min.n,
	    shrink = as.double(shrink.sml),
            high.u.fact = as.double(c(high.u.bias, u5.bias)),
            eps.correct,
            DUP = FALSE, PACKAGE = "base")
    s <- seq.int(z$l, z$u, length.out = z$n+1)
    if(!eps.correct && z$n) { # maybe zap smalls from seq() rounding errors
        ## better than zapsmall(s, digits = 14) :
        delta <- diff(range(z$l, z$u)) / z$n
        if(any(small <- abs(s) < 1e-14 * delta))
            s[small] <- 0
    }
    s
}
#  File src/library/base/R/print.R
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

print <- function(x, ...) UseMethod("print")

##- Need '...' such that it can be called as  NextMethod("print", ...):
print.default <- function(x, digits = NULL, quote = TRUE, na.print = NULL,
                          print.gap = NULL, right = FALSE, max = NULL,
                          useSource = TRUE, ...)
{
    noOpt <- missing(digits) && missing(quote) && missing(na.print) &&
	missing(print.gap) && missing(right) && missing(max) &&
	missing(useSource) && length(list(...)) == 0L
    .Internal(print.default(x, digits, quote, na.print, print.gap, right, max,
			    useSource, noOpt))
}

prmatrix <-
    function (x, rowlab = dn[[1]], collab = dn[[2]],
              quote = TRUE, right = FALSE,
              na.print = NULL, ...)
{
    x <- as.matrix(x)
    dn <- dimnames(x)
    .Internal(prmatrix(x, rowlab, collab, quote, right, na.print))
}

noquote <- function(obj) {
    ## constructor for a useful "minor" class
    if(!inherits(obj,"noquote")) class(obj) <- c(attr(obj, "class"),"noquote")
    obj
}

as.matrix.noquote <- function(x, ...) noquote(NextMethod("as.matrix", x))
c.noquote <- function(..., recursive = FALSE)
    structure(NextMethod("c"), class = "noquote")

"[.noquote" <- function (x, ...) {
    attr <- attributes(x)
    r <- unclass(x)[...] ## shouldn't this be NextMethod?
    attributes(r) <- c(attributes(r),
		       attr[is.na(match(names(attr),
                                        c("dim","dimnames","names")))])
    r
}

print.noquote <- function(x, ...) {
    if(!is.null(cl <- attr(x, "class"))) {
	cl <- cl[cl != "noquote"]
        attr(x, "class") <-
          (if(length(cl)) cl else NULL)
      }
    print(x, quote = FALSE, ...)
}

## for alias:
print.listof <- function(x, ...)
{
    nn <- names(x)
    ll <- length(x)
    if(length(nn) != ll) nn <- paste("Component", seq.int(ll))
    for(i in seq_len(ll)) {
	cat(nn[i], ":\n"); print(x[[i]], ...); cat("\n")
    }
    invisible(x)
}

`[.listof` <- `[.AsIs`

## used for version:
print.simple.list <- function(x, ...)
    print(noquote(cbind("_"=unlist(x))), ...)

`[.simple.list` <- `[.listof`

print.function <- function(x, useSource = TRUE, ...)
    .Internal(print.function(x, useSource, ...))
#  File src/library/base/R/qr.R
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

#is.qr <- function(x) !is.null(x$qr) && !is.null(x$rank) && !is.null(x$qraux)

is.qr <- function(x) inherits(x, "qr")

qr <- function(x, ...) UseMethod("qr")

qr.default <- function(x, tol = 1e-07, LAPACK = FALSE, ...)
{
    x <- as.matrix(x)
    if(is.complex(x))
        return(structure(.Call("La_zgeqp3", x, PACKAGE = "base"), class="qr"))
    ## otherwise :
    if(!is.double(x))
	storage.mode(x) <- "double"
    if(LAPACK) {
        res <- .Call("La_dgeqp3", x, PACKAGE = "base")
        if(!is.null(cn <- colnames(x)))
            colnames(res$qr) <- cn[res$pivot]
        attr(res, "useLAPACK") <- TRUE
        class(res) <- "qr"
        return(res)
    }

    p <- ncol(x) # guaranteed to be integer
    n <- nrow(x)
    res <- .Fortran("dqrdc2",
	     qr=x,
	     n,
	     n,
	     p,
	     as.double(tol),
	     rank=integer(1L),
	     qraux = double(p),
	     pivot = as.integer(1L:p),
	     double(2*p),
	     PACKAGE="base")[c(1,6,7,8)]# c("qr", "rank", "qraux", "pivot")
    if(!is.null(cn <- colnames(x)))
        colnames(res$qr) <- cn[res$pivot]
    class(res) <- "qr"
    res
}

qr.coef <- function(qr, y)
{
    if( !is.qr(qr) )
	stop("first argument must be a QR decomposition")
    n <- nrow(qr$qr)
    p <- ncol(qr$qr)
    k <- as.integer(qr$rank)
    im <- is.matrix(y)
    if (!im) y <- as.matrix(y)
    ny <- ncol(y)
    if (p == 0L) return( if (im) matrix(0, p, ny) else numeric(0L) )
    ix <- if ( p > n ) c(seq_len(n), rep(NA, p - n)) else seq_len(p)
    if(is.complex(qr$qr)) {
	if(!is.complex(y)) y[] <- as.complex(y)
	coef <- matrix(NA_complex_, nrow = p, ncol = ny)
	coef[qr$pivot,] <-
            .Call("qr_coef_cmplx", qr, y, PACKAGE = "base")[ix, ]
	return(if(im) coef else c(coef))
    }
    ## else {not complex} :
    a <- attr(qr, "useLAPACK")
    if(!is.null(a) && is.logical(a) && a) {
        if(!is.double(y)) storage.mode(y) <- "double"
	coef <- matrix(NA_real_, nrow = p, ncol = ny)
	coef[qr$pivot,] <-
            .Call("qr_coef_real", qr, y, PACKAGE = "base")[ix,]
	return(if(im) coef else c(coef))
    }
    if (k == 0L) return( if (im) matrix(NA, p, ny) else rep.int(NA, p))

    storage.mode(y) <- "double"
    if( nrow(y) != n )
	stop("'qr' and 'y' must have the same number of rows")
    z <- .Fortran("dqrcf",
		  as.double(qr$qr),
		  n, k,
		  as.double(qr$qraux),
		  y,
		  ny,
		  coef=matrix(0, nrow=k,ncol=ny),
		  info=integer(1L),
		  NAOK = TRUE, PACKAGE="base")[c("coef","info")]
    if(z$info) stop("exact singularity in 'qr.coef'")
    if(k < p) {
	coef <- matrix(NA_real_, nrow=p, ncol=ny)
	coef[qr$pivot[1L:k],] <- z$coef
    }
    else coef <- z$coef

    if(!is.null(nam <- colnames(qr$qr)))
        if(k < p) rownames(coef)[qr$pivot] <- nam
        else rownames(coef) <- nam

    if(im && !is.null(nam <- colnames(y)))
        colnames(coef) <- nam

    if(im) coef else drop(coef)
}

qr.qy <- function(qr, y)
{
    if(!is.qr(qr)) stop("argument is not a QR decomposition")
    if(is.complex(qr$qr)) {
        y <- as.matrix(y)
        if(!is.complex(y)) y[] <- as.complex(y)
        return(.Call("qr_qy_cmplx", qr, y, 0, PACKAGE = "base"))
    }
    a <- attr(qr, "useLAPACK")
    if(!is.null(a) && is.logical(a) && a)
        return(.Call("qr_qy_real", qr, as.matrix(y), 0, PACKAGE = "base"))
    n <- nrow(qr$qr)
#    p <- ncol(qr$qr)
    k <- as.integer(qr$rank)
    ny <- NCOL(y)
    storage.mode(y) <- "double"
    if(NROW(y) != n)
	stop("'qr' and 'y' must have the same number of rows")
    .Fortran("dqrqy",
	     as.double(qr$qr),
	     n, k,
	     as.double(qr$qraux),
	     y,
	     ny,
	     qy = y,# incl. {dim}names
	     PACKAGE="base")$qy
}

qr.qty <- function(qr, y)
{
    if(!is.qr(qr)) stop("argument is not a QR decomposition")
    if(is.complex(qr$qr)){
        y <- as.matrix(y)
        if(!is.complex(y)) y[] <- as.complex(y)
        return(.Call("qr_qy_cmplx", qr, y, 1, PACKAGE = "base"))
    }
    a <- attr(qr, "useLAPACK")
    if(!is.null(a) && is.logical(a) && a)
        return(.Call("qr_qy_real", qr, as.matrix(y), 1, PACKAGE = "base"))

    n <- nrow(qr$qr)
#    p <- ncol(qr$qr)
    k <- as.integer(qr$rank)
    ny <- NCOL(y)
    if(NROW(y) != n)
	stop("'qr' and 'y' must have the same number of rows")
    storage.mode(y) <- "double"
    .Fortran("dqrqty",
	     as.double(qr$qr),
	     n, k,
	     as.double(qr$qraux),
	     y,
	     ny,
	     qty = y,# incl. {dim}names
             PACKAGE = "base")$qty
}

qr.resid <- function(qr, y)
{
    if(!is.qr(qr)) stop("argument is not a QR decomposition")
    if(is.complex(qr$qr)) stop("not implemented for complex 'qr'")
    a <- attr(qr, "useLAPACK")
    if(!is.null(a) && is.logical(a) && a)
        stop("not supported for LAPACK QR")
    k <- as.integer(qr$rank)
    if (k==0) return(y)
    n <- nrow(qr$qr)
#    p <- ncol(qr$qr)
    ny <- NCOL(y)
    if( NROW(y) != n )
	stop("'qr' and 'y' must have the same number of rows")
    storage.mode(y) <- "double"
    .Fortran("dqrrsd",
	     as.double(qr$qr),	     n, k,
	     as.double(qr$qraux),
             y,
	     ny,
	     rsd = y,# incl. {dim}names
	     PACKAGE="base")$rsd
}

qr.fitted <- function(qr, y, k=qr$rank)
{
    if(!is.qr(qr)) stop("argument is not a QR decomposition")
    if(is.complex(qr$qr)) stop("not implemented for complex 'qr'")
    a <- attr(qr, "useLAPACK")
    if(!is.null(a) && is.logical(a) && a)
        stop("not supported for LAPACK QR")
    n <- nrow(qr$qr)
    k <- as.integer(k)
    if(k > qr$rank) stop("'k' is too large")
    ny <- NCOL(y)
    if( NROW(y) != n )
	stop("'qr' and 'y' must have the same number of rows")
    storage.mode(y) <- "double"
    .Fortran("dqrxb",
	     as.double(qr$qr),
	     n, k,
	     as.double(qr$qraux),
	     y,
	     ny,
	     xb = y,# incl. {dim}names
             DUP=FALSE, PACKAGE="base")$xb
}

## qr.solve is defined in  ./solve.R

##---- The next three are from Doug Bates ('st849'):
qr.Q <- function (qr, complete = FALSE,
		  Dvec = rep.int(if (cmplx) 1 + 0i else 1,
		  if (complete) dqr[1] else min(dqr)))
{
    if(!is.qr(qr)) stop("argument is not a QR decomposition")
    dqr <- dim(qr$qr)
    cmplx <- mode(qr$qr) == "complex"
    D <-
	if (complete) diag(Dvec, dqr[1L])
	else {
	    ncols <- min(dqr)
	    diag(Dvec[1L:ncols], nrow = dqr[1L], ncol = ncols)
	}
    qr.qy(qr, D)
}

qr.R <- function (qr, complete = FALSE)
{
    if(!is.qr(qr)) stop("argument is not a QR decomposition")
    R <- qr$qr
    if (!complete)
	R <- R[seq.int(min(dim(R))), , drop = FALSE]
    R[row(R) > col(R)] <- 0
    R
}

qr.X <- function (qr, complete = FALSE,
		  ncol = if (complete) nrow(R) else min(dim(R)))
{
    if(!is.qr(qr)) stop("argument is not a QR decomposition")
    pivoted <- !identical(qr$pivot, ip <- seq_along(qr$pivot))
    R <- qr.R(qr, complete = TRUE)
    if(pivoted && ncol < length(qr$pivot))
        stop("need larger value of 'ncol' as pivoting occurred")
    cmplx <- mode(R) == "complex"
    p <- dim(R)[2L]
    if (ncol < p)
	R <- R[, 1L:ncol, drop = FALSE]
    else if (ncol > p) {
	tmp <- diag(if (!cmplx) 1 else 1 + 0i, nrow(R), ncol)
	tmp[, 1L:p] <- R
	R <- tmp
    }
    res <- qr.qy(qr, R)
    if(pivoted) # res may have more columns than length(qr$pivot)
	res[, qr$pivot] <- res[, ip]
    res
}
#  File src/library/base/R/quit.R
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

quit <- function(save = "default", status=0, runLast=TRUE)
    .Internal(quit(save, status, runLast))
q <- quit
#  File src/library/base/R/range.R
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

range.default <- function(..., na.rm = FALSE, finite = FALSE)
{
    x <- c(..., recursive = TRUE)
    if(is.numeric(x)) {
        if(finite) x <- x[is.finite(x)]
        else if(na.rm) x <- x[!is.na(x)]
	return (c(min(x), max(x)))
    }
    c(min(x, na.rm=na.rm), max(x, na.rm=na.rm))
}
#  File src/library/base/R/rank.R
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

rank <- function(x, na.last = TRUE,
		 ties.method=c("average", "first", "random", "max", "min"))
{
    nas <- is.na(x)
    nm <- names(x)
    ties.method <- match.arg(ties.method)
    ## To preserve past behaviour
    if(is.factor(x)) x <- as.integer(x)
    y <- switch(ties.method,
		"average"= , "min"= , "max" =
		.Internal(rank(x[!nas], ties.method)),
		"first" = sort.list(sort.list(x[!nas])),
		"random" = sort.list(order(x[!nas], stats::runif(sum(!nas)))))
    if(!is.na(na.last) && any(nas)) {
	## the internal code has ranks in [1, length(y)]
	yy <- integer(length(x))
	storage.mode(yy) <- storage.mode(y) # integer or double
	yy <- NA
	NAkeep <- (na.last == "keep")
	if(NAkeep || na.last) {
	    yy[!nas] <- y
	    if(!NAkeep) yy[nas] <- (length(y) + 1L) : length(yy)
	} else {
	    len <- sum(nas)
	    yy[!nas] <- y + len
	    yy[nas] <- 1L : len
	}
	y <- yy
	names(y) <- nm
    } else names(y) <- nm[!nas]
    y
}
#  File src/library/base/R/raw.R
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

raw <- function(length = 0) vector("raw", length)

#as.raw <- function(x) .Internal(as.raw(x))

charToRaw <- function(x) .Internal(charToRaw(x))
rawToChar <- function(x, multiple=FALSE) .Internal(rawToChar(x, multiple))

rawShift <- function(x, n) .Internal(rawShift(x, n))

rawToBits <- function(x) .Internal(rawToBits(x))
intToBits <- function(x) .Internal(intToBits(x))

packBits <- function(x, type=c("raw", "integer"))
{
    type <- match.arg(type)
    .Internal(packBits(x, type))
}

utf8ToInt <- function(x) .Internal(utf8ToInt(x))
intToUtf8 <- function(x, multiple=FALSE) .Internal(intToUtf8(x, multiple))
#  File src/library/base/R/rep.R
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

rep.int <- function(x, times) .Internal(rep.int(x, times))

rep.factor <- function(x, ...)
{
    y <- NextMethod()
    structure(y, class=class(x), levels=levels(x))
}
#  File src/library/base/R/replace.R
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

replace <-
    function (x, list, values)
{
    x[list] <- values
    x
}
#  File src/library/base/R/replicate.R
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

replicate <- function(n, expr, simplify = TRUE) 
        sapply(integer(n), 
           eval.parent(substitute(function(...)expr)), simplify = simplify)
#  File src/library/base/R/rev.R
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

rev <- function(x) UseMethod("rev")

rev.default <- function(x) if (length(x)) x[length(x):1L] else x
#  File src/library/base/R/rle.R
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

rle <- function(x)
{
    if (!is.vector(x) && !is.list(x))
        stop("'x' must be an atomic vector")
    n <- length(x)
    if (n == 0L)
        return(list(lengths = integer(0L), values = x))
    y <- x[-1L] != x[-n]
    i <- c(which(y | is.na(y)), n)
    structure(list(lengths = diff(c(0L, i)), values = x[i]),
              class = "rle")
}

print.rle <- function(x, digits = getOption("digits"), ...)
{
    if(is.null(digits)) digits <- getOption("digits")
    cat("Run Length Encoding\n  lengths:")
    utils::str(x$lengths)
    cat("  values :")
    utils::str(x$values, digits.d = digits)
    invisible(x)
}

inverse.rle <- function(x, ...)
{
    if(is.null(le <- x$lengths) ||
       is.null(v  <- x$values) || length(le) != length(v))
        stop("invalid 'rle' structure")
    rep.int(v, le)
}
#  File src/library/base/R/rm.R
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

rm <-
    function (..., list = character(0L), pos = -1, envir = as.environment(pos),
              inherits = FALSE)
{
    dots <- match.call(expand.dots=FALSE)$...
    if(length(dots) &&
       !all(sapply(dots, function(x) is.symbol(x) || is.character(x))))
       stop("... must contain names or character strings")
    names <- sapply(dots, as.character)
    if (length(names) == 0L) names <- character(0L)
    list <- .Primitive("c")(list, names)
    .Internal(remove(list, envir, inherits))
}

remove <- rm
#  File src/library/base/R/rowsum.R
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

rowsum <- function(x, group, reorder=TRUE, ...) UseMethod("rowsum")

rowsum.default <- function(x, group, reorder=TRUE, na.rm = FALSE, ...)
{
    if (!is.numeric(x))
        stop("'x' must be numeric")
    if (length(group) != NROW(x))
        stop("incorrect length for 'group'")
    if (any(is.na(group)))
        warning("missing values for 'group'")
    ugroup <- unique(group)
    if (reorder) ugroup <- sort(ugroup, na.last=TRUE, method="quick")

    rval <- .Call("Rrowsum_matrix", x, NCOL(x), group, ugroup, na.rm,
                  PACKAGE="base")

    dimnames(rval) <- list(as.character(ugroup),dimnames(x)[[2L]])
    rval
}

rowsum.data.frame <- function(x, group, reorder=TRUE, na.rm = FALSE, ...)
{
    if (!is.data.frame(x)) stop("not a data frame") ## make MM happy
    if (length(group) != NROW(x))
        stop("incorrect length for 'group'")
    if (any(is.na(group)))
        warning("missing values for 'group'")
    ugroup <- unique(group)
    if (reorder) ugroup <- sort(ugroup, na.last=TRUE, method="quick")

    rval <- .Call("Rrowsum_df", x, NCOL(x), group, ugroup, na.rm,
                  PACKAGE="base")

    as.data.frame(rval, row.names=as.character(ugroup))
}
#  File src/library/base/R/sample.R
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

sample <- function(x, size, replace=FALSE, prob=NULL)
{
    if(length(x) == 1L && is.numeric(x) && x >= 1) {
	if(missing(size)) size <- x
	.Internal(sample(x, size, replace, prob))
    }
    else {
	if(missing(size)) size <- length(x)
	x[.Internal(sample(length(x), size, replace, prob))]
    }
}

sample.int  <- function(n, size, replace=FALSE, prob=NULL)
    .Internal(sample(n, size, replace, prob))
#  File src/library/base/R/sapply.R
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

sapply <- function(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE)
{
    FUN <- match.fun(FUN)
    answer <- lapply(X, FUN, ...)
    if(USE.NAMES && is.character(X) && is.null(names(answer)))
                names(answer) <- X
    if(simplify && length(answer) &&
       length(common.len <- unique(unlist(lapply(answer, length)))) == 1L) {
	if(common.len == 1L)
	    unlist(answer, recursive = FALSE)
	else if(common.len > 1L)
	    array(unlist(answer, recursive = FALSE),
		  dim= c(common.len, length(X)),
		  dimnames= if(!(is.null(n1 <- names(answer[[1L]])) &
			         is.null(n2 <- names(answer)))) list(n1,n2))
	else answer
    } else answer
}

#  File src/library/base/R/scale.R
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

scale <- function(x, center = TRUE, scale = TRUE) UseMethod("scale")

scale.default <- function(x, center = TRUE, scale = TRUE)
{
    x <- as.matrix(x)
    nc <- ncol(x)
    if (is.logical(center)) {
	if (center) {
            center <- colMeans(x, na.rm=TRUE)
	    x <- sweep(x, 2L, center, check.margin=FALSE)
        }
    }
    else if (is.numeric(center) && (length(center) == nc))
	x <- sweep(x, 2L, center, check.margin=FALSE)
    else
	stop("length of 'center' must equal the number of columns of 'x'")
    if (is.logical(scale)) {
	if (scale) {
	    f <- function(v) {
		v <- v[!is.na(v)]
		sqrt(sum(v^2) / max(1, length(v) - 1L))
	    }
            scale <- apply(x, 2L, f)
	    x <- sweep(x, 2L, scale, "/", check.margin=FALSE)
	}
    }
    else if (is.numeric(scale) && length(scale) == nc)
	x <- sweep(x, 2L, scale, "/", check.margin=FALSE)
    else
	stop("length of 'scale' must equal the number of columns of 'x'")
    if(is.numeric(center)) attr(x, "scaled:center") <- center
    if(is.numeric(scale)) attr(x, "scaled:scale") <- scale
    x
}
#  File src/library/base/R/scan.R
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

scan <-
function(file = "", what = double(0), nmax = -1, n = -1, sep = "",
         quote = if(identical(sep, "\n")) "" else "'\"",
         dec = ".", skip = 0, nlines = 0,
         na.strings = "NA", flush = FALSE, fill = FALSE,
         strip.white = FALSE, quiet = FALSE, blank.lines.skip = TRUE,
         multi.line = TRUE, comment.char = "", allowEscapes = FALSE,
         encoding = "unknown")
{
    na.strings <- as.character(na.strings)# allow it to be NULL
    if(!missing(n)) {
        if(missing(nmax))
            nmax <- n / pmax(length(what), 1)
        else
            stop("either specify 'nmax' or 'n', but not both.")
    }
    if(is.character(file))
        if(file == "") file <- stdin()
        else {
            file <- file(file, "r")
            on.exit(close(file))
        }
    if(!inherits(file, "connection"))
        stop("'file' must be a character string or connection")
    .Internal(scan(file, what, nmax, sep, dec, quote, skip, nlines,
                   na.strings, flush, fill, strip.white, quiet,
                   blank.lines.skip, multi.line, comment.char,
                   allowEscapes, encoding))
}
#  File src/library/base/R/seq.R
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

seq <- function(...) UseMethod("seq")

seq.default <-
    function(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
             length.out = NULL, along.with = NULL, ...)
{
    if((One <- nargs() == 1L) && !missing(from)) {
	lf <- length(from)
	return(if(mode(from) == "numeric" && lf == 1L) 1L:from else
	       if(lf) 1L:lf else integer(0L))
    }
    if(!missing(along.with)) {
	length.out <- length(along.with)
	if(One) return(if(length.out) seq_len(length.out) else integer(0L))
    }
    else if(!missing(length.out))
	length.out <- ceiling(length.out)
    if(is.null(length.out))
	if(missing(by))
	    from:to
	else { # dealing with 'by'
	    del <- to - from
	    if(del == 0 && to == 0) return(to)
	    n <- del/by
	    if(!(length(n) && is.finite(n))) {
		if(length(by) && by == 0 && length(del) && del == 0)
		    return(from)
		stop("invalid (to - from)/by in seq(.)")
	    }
	    if(n < 0L)
		stop("wrong sign in 'by' argument")
	    if(n > .Machine$integer.max)
		stop("'by' argument is much too small")

	    dd <- abs(del)/max(abs(to), abs(from))
	    if (dd < 100*.Machine$double.eps) return(from)
	    n <- as.integer(n + 1e-7)
	    x <- from + (0L:n) * by
            ## correct for overshot because of fuzz
            if(by > 0) pmin(x, to) else pmax(x, to)
	}
    else if(!is.finite(length.out) || length.out < 0L)
	stop("length must be non-negative number")
    else if(length.out == 0L)
	integer(0L)
    else if (One) seq_len(length.out)
    else if(missing(by)) {
	# if(from == to || length.out < 2) by <- 1
	if(missing(to))
	    to <- from + length.out - 1L
	if(missing(from))
	    from <- to - length.out + 1L
	if(length.out > 2L)
	    if(from == to)
		rep.int(from, length.out)
	    else as.vector(c(from, from + seq_len(length.out - 2L) * by, to))
	else as.vector(c(from, to))[seq_len(length.out)]
    }
    else if(missing(to))
	from + (0L:(length.out - 1L)) * by
    else if(missing(from))
	to - ((length.out - 1L):0L) * by
    else stop("too many arguments")
}

## In reverence to the very first versions of R which already had sequence():
sequence <- function(nvec) unlist(lapply(nvec, seq_len))
#  File src/library/base/R/serialize.R
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

.saveRDS <-
function(object, file = "", ascii = FALSE, version = NULL,
         compress = TRUE, refhook = NULL)
{
    if(is.character(file)) {
        if(file == "") stop("'file' must be non-empty string")
        mode <- if(ascii) "w" else "wb"
        con <- if (identical(compress, "bzip2")) bzfile(file, mode)
            else if(compress) gzfile(file, mode) else file(file, mode)
        on.exit(close(con))
    }
    else if(inherits(file, "connection")) {
        con <- file
        if(missing(ascii)) ascii <- summary(con)$text == "text"
    }
    else
        stop("bad 'file' argument")
    invisible(.Internal(serializeToConn(object, con, ascii, version, refhook)))
}

.readRDS <-
function(file, refhook = NULL)
{
    if(is.character(file)) {
        con <- gzfile(file, "rb")
        on.exit(close(con))
    }
    else if(inherits(file, "connection")) {
        con <- gzcon(file)
        if(!isOpen(con, "rb")) {
            open(con, "rb")
            on.exit(close(con))
        }
    }
    else
        stop("bad 'file' argument")
    .Internal(unserializeFromConn(con, refhook))
}

serialize <- function(object, connection, ascii = FALSE, refhook = NULL)
{
    if (!is.null(connection)) {
        if (!inherits(connection, "connection"))
            stop("'connection' must be a connection")
        if (missing(ascii)) ascii <- summary(connection)$text == "text"
    }
    if (!ascii && inherits(connection, "sockconn"))
        .Call("R_serializeb", object, connection, refhook, PACKAGE="base")
    else
        .Call("R_serialize", object, connection, ascii, refhook,
              PACKAGE="base")
}

unserialize <- function(connection, refhook = NULL)
{
    if (typeof(connection) != "raw" &&
        !is.character(connection) &&
        !inherits(connection, "connection"))
        stop("'connection' must be a connection")
    .Call("R_unserialize", connection, refhook, PACKAGE="base")
}
#  File src/library/base/R/sets.R
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

union <- function(x, y) unique(c(as.vector(x), as.vector(y)))

intersect <- function(x, y)
{
    y <- as.vector(y)
    unique(y[match(as.vector(x), y, 0L)])
}

setdiff <- function(x, y)
{
    x <- as.vector(x)
    y <- as.vector(y)
    unique(if(length(x) || length(y)) x[match(x, y, 0L) == 0L] else x)
}
## Faster versions, see R-devel, Jan.4-6, 2000;  optimize later...
setequal <- function(x, y)
{
    x <- as.vector(x)
    y <- as.vector(y)
    all(c(match(x, y, 0L) > 0L, match(y, x, 0L) > 0L))
}

##  same as %in% ( ./match.R ) but different arg names:
is.element <- function(el, set) match(el, set, 0L) > 0L
#  File src/library/base/R/sink.R
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

sink <- function(file=NULL, append = FALSE, type = c("output", "message"),
                 split=FALSE)
{
    type <- match.arg(type)
    if(type == "message") {
        if(is.null(file)) file <- stderr()
        else if(!inherits(file, "connection") || !isOpen(file))
           stop("'file' must be NULL or an already open connection")
        if (split) stop("cannot split the message connection")
        .Internal(sink(file, FALSE, TRUE, FALSE))
    } else {
        closeOnExit <- FALSE
        if(is.null(file)) file <- -1L
        else if(is.character(file)) {
            file <- file(file, ifelse(append, "a", "w"))
            closeOnExit <- TRUE
        } else if(!inherits(file, "connection"))
            stop("'file' must be NULL, a connection or a character string")
        .Internal(sink(file, closeOnExit, FALSE,split))
    }
}

sink.number <- function(type = c("output", "message"))
{
    type <- match.arg(type)
    .Internal(sink.number(type != "message"))
}
#  File src/library/base/R/solve.R
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

solve.qr <- function(a, b, ...)
{
    if( !is.qr(a) )
	stop("this is the \"qr\" method for the generic function solve()")
    nc <- ncol(a$qr); nr <- nrow(a$qr)
    if( a$rank != min(nc, nr) )
    if( a$rank != nc )
	stop("singular matrix 'a' in 'solve'")
    if( missing(b) ) {
	if( nc != nr )
	    stop("only square matrices can be inverted")
	b <- diag(1, nc)
    }
    res <- qr.coef(a, b)
    res[is.na(res)] <- 0
    res
}

solve.default <-
    function(a, b, tol = ifelse(LINPACK, 1e-7, .Machine$double.eps),
	     LINPACK = FALSE, ...)
{
    if(is.complex(a) || (!missing(b) && is.complex(b))) {
	a <- as.matrix(a)
	if(missing(b)) {
            if(nrow(a) != ncol(a))
                stop("only square matrices can be inverted")
	    b <- diag(1.0+0.0i, nrow(a))
	    colnames(b) <- rownames(a)
	} else if(!is.complex(b)) b[] <- as.complex(b)
	if(!is.complex(a)) a[] <- as.complex(a)
	return (if (is.matrix(b)) {
            if(ncol(a) != nrow(b)) stop("'b' must be compatible with 'a'")
	    rownames(b) <- colnames(a)
	    .Call("La_zgesv", a, b, PACKAGE = "base")
	} else
	    drop(.Call("La_zgesv", a, as.matrix(b), PACKAGE = "base")))
    }
    if(is.qr(a)) {
	warning("solve.default called with a \"qr\" object: use 'qr.solve'")
	return(solve.qr(a, b, tol))
    }

    if(!LINPACK) {
	a <- as.matrix(a)
	if(missing(b)) {
            if(nrow(a) != ncol(a))
                stop("only square matrices can be inverted")
	    b <- diag(1.0, nrow(a))
	    colnames(b) <- rownames(a)
	} else storage.mode(b) <- "double"
	storage.mode(a) <- "double"
	return (if (is.matrix(b)) {
            if(ncol(a) != nrow(b)) stop("'b' must be compatible with 'a'")
	    rownames(b) <- colnames(a)
	    .Call("La_dgesv", a, b, tol, PACKAGE = "base")
	} else
	    drop(.Call("La_dgesv", a, as.matrix(b), tol, PACKAGE = "base")))
    }
    a <- qr(a, tol = tol)
    nc <- ncol(a$qr)
    if( a$rank != nc )
	stop("singular matrix 'a' in 'solve'")
    if( missing(b) ) {
	if( nc != nrow(a$qr) )
	    stop("only square matrices can be inverted")
	## preserve dimnames
	b <- diag(1, nc)
	colnames(b) <- rownames(a$qr)
    }
    qr.coef(a, b)
}

solve <- function(a, b, ...) UseMethod("solve")

qr.solve <- function(a, b, tol = 1e-7)
{
    if( !is.qr(a) )
	a <- qr(a, tol = tol)
    nc <- ncol(a$qr); nr <- nrow(a$qr)
    if( a$rank != min(nc, nr) )
	stop("singular matrix 'a' in solve")
    if( missing(b) ) {
	if( nc != nr )
	    stop("only square matrices can be inverted")
	b <- diag(1, nc)
    }
    res <- qr.coef(a, b)
    res[is.na(res)] <- 0
    res
}

#  File src/library/base/R/sort.R
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

sort <- function(x, decreasing = FALSE, ...)
{
    if(!is.logical(decreasing) || length(decreasing) != 1L)
        stop("'decreasing' must be a length-1 logical vector.\nDid you intend to set 'partial'?")
    UseMethod("sort")
}

sort.default <- function(x, decreasing = FALSE, na.last = NA, ...)
{
    ## The first case includes factors.
    if(is.object(x)) x[order(x, na.last = na.last, decreasing = decreasing)]
    else sort.int(x, na.last = na.last, decreasing = decreasing, ...)
}

sort.int <-
    function(x, partial = NULL, na.last = NA, decreasing = FALSE,
             method = c("shell", "quick"), index.return = FALSE)
{
    if(isfact <- is.factor(x)) {
        if(index.return) stop("'index.return' only for non-factors")
	lev <- levels(x)
	nlev <- nlevels(x)
 	isord <- is.ordered(x)
        x <- c(x)
    } else if(!is.atomic(x))
        stop("'x' must be atomic")

    if(has.na <- any(ina <- is.na(x))) {
        nas <- x[ina]
        x <-  x[!ina]
    }
    if(index.return && !is.na(na.last))
        stop("'index.return' only for 'na.last = NA'")
    if(!is.null(partial)) {
        if(index.return || decreasing || isfact || !missing(method))
	    stop("unsupported options for partial sorting")
        if(!all(is.finite(partial))) stop("non-finite 'partial'")
        y <- if(length(partial) <= 10L) {
            partial <- .Internal(qsort(partial, FALSE))
            .Internal(psort(x, partial))
        } else .Internal(qsort(x, FALSE))
    }
    else {
        nms <- names(x)
        method <- if(is.numeric(x)) match.arg(method) else "shell"
        switch(method,
               "quick" = {
                   if(!is.null(nms)) {
                       if(decreasing) x <- -x
                       y <- .Internal(qsort(x, TRUE))
                       if(decreasing) y$x <- -y$x
                       names(y$x) <- nms[y$ix]
                       if (!index.return) y <- y$x
                   } else {
                       if(decreasing) x <- -x
                       y <- .Internal(qsort(x, index.return))
                       if(decreasing)
                           if(index.return) y$x <- -y$x else y <- -y
                   }
               },
               "shell" = {
                   if(index.return || !is.null(nms)) {
                       o <- sort.list(x, decreasing = decreasing)
                       y <- if (index.return) list(x = x[o], ix = o) else x[o]
                       ## names(y) <- nms[o] # pointless!
                   }
                   else
                       y <- .Internal(sort(x, decreasing))
               })
    }
    if(!is.na(na.last) && has.na)
	y <- if(!na.last) c(nas, y) else c(y, nas)
    if(isfact)
        y <- (if (isord) ordered else factor)(y, levels=seq_len(nlev),
                                              labels=lev)
    y
}

order <- function(..., na.last = TRUE, decreasing = FALSE)
{
    z <- list(...)
    if(any(unlist(lapply(z, is.object)))) {
        z <- lapply(z, function(x) if(is.object(x)) xtfrm(x) else x)
        if(!is.na(na.last))
            return(do.call("order", c(z, na.last=na.last,
                                      decreasing=decreasing)))
    } else if(!is.na(na.last))
        return(.Internal(order(na.last, decreasing, ...)))
    ## remove nas
    if(any(diff(sapply(z, length)) != 0L))
        stop("argument lengths differ")
    ans <- sapply(z, is.na)
    if(is.list(ans)) return(integer(0L)) # happens for 0-length input
    ok <- if(is.matrix(ans)) !apply(ans, 1, any) else !any(ans)
    if(all(!ok)) return(integer(0L))
    z[[1L]][!ok] <- NA
    ans <- do.call("order", c(z, decreasing=decreasing))
    keep <- seq_along(ok)[ok]
    ans[ans %in% keep]
}

sort.list <- function(x, partial = NULL, na.last = TRUE, decreasing = FALSE,
                      method = c("shell", "quick", "radix"))
{
    method <- match.arg(method)
    if(!is.atomic(x))
        stop("'x' must be atomic for 'sort.list'\nHave you called 'sort' on a list?")
    if(!is.null(partial))
        .NotYetUsed("partial != NULL")
    if(method == "quick") {
        if(is.factor(x)) x <- as.integer(x) # sort the internal codes
        if(is.numeric(x))
            return(sort(x, na.last = na.last, decreasing = decreasing,
                        method = "quick", index.return = TRUE)$ix)
        else stop("method=\"quick\" is only for numeric 'x'")
    }
    if(method == "radix") {
        if(!typeof(x) == "integer") # do want to allow factors here
            stop("method=\"radix\" is only for integer 'x'")
        if(is.na(na.last))
            return(.Internal(radixsort(x[!is.na(x)], TRUE, decreasing)))
        else
            return(.Internal(radixsort(x, na.last, decreasing)))
    }
    ## method == "shell"
    if(is.na(na.last)) .Internal(order(TRUE, decreasing, x[!is.na(x)]))
    else .Internal(order(na.last, decreasing, x))
}

xtfrm <- function(x) UseMethod("xtfrm")
xtfrm.default <- function(x)
    as.vector(rank(x, ties.method="min", na.last="keep"))
xtfrm.factor <- function(x) as.integer(x) # primitive, so needs a wrapper
xtfrm.Surv <- function(x)
    if(ncol(x) == 2) order(x[,1], x[,2]) else order(x[,1], x[,2], x[,3]) # needed by 'party'

.gt <- function(x, i, j)
{
    xi <- x[i]; xj <- x[j]
    if (xi == xj) 0L else if(xi > xj) 1L else -1L;
}

.gtn <- function(x, strictly)
{
    n <- length(x)
    if(strictly) all(x[-1L] > x[-n]) else all(x[-1L] >= x[-n])
}
#  File src/library/base/R/source.R
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

source <-
function(file, local = FALSE, echo = verbose, print.eval = echo,
	 verbose = getOption("verbose"),
	 prompt.echo = getOption("prompt"),
	 max.deparse.length = 150, chdir = FALSE,
         encoding = getOption("encoding"),
         continue.echo = getOption("continue"),
         skip.echo = 0, keep.source = getOption("keep.source"))
{
    ## eval.with.vis is retained for historical reasons, including
    ## not changing tracebacks.
    ## Use withVisible(eval(...)) for less critical applications.
    ## A one line change is marked around line 166 to use it here as well.
    eval.with.vis <-
	function (expr, envir = parent.frame(),
		  enclos = if (is.list(envir) || is.pairlist(envir))
		  parent.frame() else baseenv())
	.Internal(eval.with.vis(expr, envir, enclos))

    envir <- if (local) parent.frame() else .GlobalEnv
    have_encoding <- !missing(encoding) && encoding != "unknown"
    if (!missing(echo)) {
	if (!is.logical(echo))
	    stop("'echo' must be logical")
	if (!echo && verbose) {
	    warning("'verbose' is TRUE, 'echo' not; ... coercing 'echo <- TRUE'")
	    echo <- TRUE
	}
    }
    if (verbose) {
	cat("'envir' chosen:")
	print(envir)
    }
    ofile <- file # for use with chdir = TRUE
    from_file <- FALSE
    srcfile <- NULL
    if(is.character(file)) {
        if(identical(encoding, "unknown")) {
            enc <- utils::localeToCharset()
            encoding <- enc[length(enc)]
        } else enc <- encoding
        if(length(enc) > 1L) {
            encoding <- NA
            owarn <- options("warn"); options(warn = 2)
            for(e in enc) {
                if(is.na(e)) next
                zz <- file(file, encoding = e)
                res <- tryCatch(readLines(zz), error = identity)
                close(zz)
                if(!inherits(res, "error")) { encoding <- e; break }
            }
            options(owarn)
        }
        if(is.na(encoding))
            stop("unable to find a plausible encoding")
        if(verbose) cat("encoding =", dQuote(encoding), "chosen\n")
        if(file == "") file <- stdin()
        else {
            if (isTRUE(keep.source))
            	srcfile <- srcfile(file, encoding = encoding)
	    file <- file(file, "r", encoding = encoding)
	    on.exit(close(file))
            from_file <- TRUE
            ## We translated the file (possibly via a quess),
            ## so don't want to mark the strings.as from that encoding
            ## but we might know what we have encoded to, so
            loc <- utils::localeToCharset()[1L]
            encoding <- if(have_encoding)
                switch(loc,
                       "UTF-8" = "UTF-8",
                       "ISO8859-1" = "latin1",
                       "unknown")
            else "unknown"
	}
    }
    exprs <- .Internal(parse(file, n = -1, NULL, "?", srcfile, encoding))
    Ne <- length(exprs)
    if (from_file) { # we are done with the file now
        close(file)
        on.exit()
    }
    if (verbose)
	cat("--> parsed", Ne, "expressions; now eval(.)ing them:\n")
    if (Ne == 0)
	return(invisible())
    if (chdir){
        if(is.character(ofile)) {
            isURL <- length(grep("^(ftp|http|file)://", ofile)) > 0L
            if(isURL)
                warning("'chdir = TRUE' makes no sense for a URL")
            if(!isURL && (path <- dirname(ofile)) != ".") {
                owd <- getwd()
                if(is.null(owd))
                    warning("cannot 'chdir' as current directory is unknown")
                else on.exit(setwd(owd), add=TRUE)
                setwd(path)
            }
        } else {
            warning("'chdir = TRUE' makes no sense for a connection")
        }
    }

    if (echo) {
	## Reg.exps for string delimiter/ NO-string-del /
	## odd-number-of-str.del needed, when truncating below
	sd <- "\""
	nos <- "[^\"]*"
	oddsd <- paste("^", nos, sd, "(", nos, sd, nos, sd, ")*",
		       nos, "$", sep = "")
    }
    srcrefs <- attr(exprs, "srcref")
    for (i in 1L:Ne) {
	if (verbose)
	    cat("\n>>>> eval(expression_nr.", i, ")\n\t	 =================\n")
	ei <- exprs[i]
	if (echo) {
	    if (i > length(srcrefs) || is.null(srcref <- srcrefs[[i]])) {
	        # Deparse.  Must drop "expression(...)"
		dep <- substr(paste(deparse(ei, control = c("showAttributes","useSource")),
	    		  collapse = "\n"), 12, 1e+06)
            	## We really do want chars here as \n\t may be embedded.
            	dep <- paste(prompt.echo,
            		     gsub("\n", paste("\n", continue.echo, sep=""), dep),
            		     sep="")
		nd <- nchar(dep, "c") - 1
	    } else {
	    	if (i == 1) lastshown <- min(skip.echo, srcref[3L]-1)
	    	dep <- getSrcLines(srcfile, lastshown+1, srcref[3L])
	    	leading <- srcref[1L]-lastshown
	    	lastshown <- srcref[3L]
	    	while (length(dep) && length(grep("^[[:blank:]]*$", dep[1L]))) {
	    	    dep <- dep[-1L]
	    	    leading <- leading - 1L
	    	}
	    	dep <- paste(rep.int(c(prompt.echo, continue.echo), c(leading, length(dep)-leading)),
	    		     dep, sep="", collapse="\n")
	    	nd <- nchar(dep, "c")
	    }
	    if (nd) {
		do.trunc <- nd > max.deparse.length
		dep <- substr(dep, 1L, if (do.trunc) max.deparse.length else nd)
		cat("\n", dep, if (do.trunc)
		    paste(if (length(grep(sd, dep)) && length(grep(oddsd, dep)))
		      " ...\" ..."
		      else " ....", "[TRUNCATED] "), "\n", sep = "")
	    }
	}
###  Switch comment below get rid of eval.with.vis
	yy <- eval.with.vis(ei, envir)
###	yy <- withVisible(eval(ei, envir))
	i.symbol <- mode(ei[[1L]]) == "name"
	if (!i.symbol) {
	    ## ei[[1L]] : the function "<-" or other
	    curr.fun <- ei[[1L]][[1L]]
	    if (verbose) {
		cat("curr.fun:")
		utils::str(curr.fun)
	    }
	}
	if (verbose >= 2) {
	    cat(".... mode(ei[[1L]])=", mode(ei[[1L]]), "; paste(curr.fun)=")
	    utils::str(paste(curr.fun))
	}
	if (print.eval && yy$visible) {
            if(isS4(yy$value))
                methods::show(yy$value)
            else
                print(yy$value)
        }
	if (verbose)
	    cat(" .. after ", sQuote(deparse(ei,
	    	control = c("showAttributes","useSource"))), "\n", sep = "")
    }
    invisible(yy)
}

sys.source <-
function(file, envir = baseenv(), chdir = FALSE,
	 keep.source = getOption("keep.source.pkgs"))
{
    if(!(is.character(file) && file.exists(file)))
	stop(gettextf("'%s' is not an existing file", file))
    oop <- options(keep.source = as.logical(keep.source),
		   topLevelEnvironment = as.environment(envir))
    on.exit(options(oop))
    exprs <- parse(n = -1, file = file)
    if (length(exprs) == 0L)
	return(invisible())
    if (chdir && (path <- dirname(file)) != ".") {
	owd <- getwd()
	on.exit(setwd(owd), add = TRUE)
	setwd(path)
    }
    for (i in exprs) eval(i, envir)
    invisible()
}
#  File src/library/base/R/split.R
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

split <- function(x, f, drop = FALSE, ...) UseMethod("split")

split.default <- function(x, f, drop = FALSE, ...)
{
    if(length(list(...))) .NotYetUsed(deparse(...), error = FALSE)

    if (is.list(f)) f <- interaction(f, drop = drop)
    else if (drop || !is.factor(f)) # drop extraneous levels
	f <- factor(f)
    storage.mode(f) <- "integer"  # some factors have double
    if (is.null(attr(x, "class")))
	return(.Internal(split(x, f)))
    ## else
    lf <- levels(f)
    y <- vector("list", length(lf))
    names(y) <- lf
    ind <- .Internal(split(seq_along(f), f))
    for(k in lf) y[[k]] <- x[ind[[k]]]
    y
}

split.data.frame <- function(x, f, drop = FALSE, ...)
    lapply(split(seq_len(nrow(x)), f, drop = drop, ...),
           function(ind) x[ind, , drop = FALSE])

"split<-" <- function(x, f, drop = FALSE, ..., value) UseMethod("split<-")

#"split<-.default" <- function(x, f, value)
#{
#    x[unlist(split(seq_along(x), f))] <- unlist(value)
#    x
#}

"split<-.default" <- function(x, f, drop = FALSE, ..., value)
{
    ix <- split(seq_along(x), f, drop = drop, ...)
    n <- length(value)
    j <- 0
    for (i in ix) {
        j <- j %% n + 1
        x[i] <- value[[j]]
    }
    x
}

"split<-.data.frame" <- function(x, f, drop = FALSE, ..., value)
{
    ix <- split(seq_len(nrow(x)), f, drop = drop, ...)
    n <- length(value)
    j <- 0
    for (i in ix) {
        j <- j %% n + 1
        x[i,] <- value[[j]]
    }
    x
}

unsplit <-function (value, f, drop = FALSE)
{
    len <- length(if (is.list(f)) f[[1L]] else f)
    if (is.data.frame(value[[1L]])) {
        x <- value[[1L]][rep(NA, len),, drop = FALSE]
        rownames(x) <- unsplit(lapply(value, rownames), f, drop = drop)
    } else
        x <- value[[1L]][rep(NA, len)]
    split(x, f, drop = drop) <- value
    x
}
#  File src/library/base/R/srcfile.R
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

# a srcfile is a file with a timestamp

srcfile <- function(filename, encoding = getOption("encoding")) {
    stopifnot(is.character(filename), length(filename) == 1L)

    e <- new.env(parent=emptyenv())

    e$wd <- getwd()
    e$filename <- filename

    # If filename is a URL, this will return NA
    e$timestamp <- file.info(filename)[1,"mtime"]
    
    if (identical(encoding, "unknown")) encoding <- "native.enc"
    e$encoding <- encoding

    class(e) <- "srcfile"
    return(e)
}

print.srcfile <- function(x, ...) {
    cat(x$filename, "\n")
    invisible(x)
}

open.srcfile <- function(con, line, ...) {

    srcfile <- con

    oldline <- srcfile$line
    if (!is.null(oldline) && oldline > line) close(srcfile)

    conn <- srcfile$conn
    if (is.null(conn)) {
        if (!is.null(srcfile$wd)) {
	    olddir <- setwd(srcfile$wd)
	    on.exit(setwd(olddir))
	}
	timestamp <- file.info(srcfile$filename)[1,"mtime"]
	if (!is.null(srcfile$timestamp) 
	    && !is.na(srcfile$timestamp) 
	    && ( is.na(timestamp) || timestamp != srcfile$timestamp) )
	    warning("Timestamp of '",srcfile$filename,"' has changed", call.=FALSE)
	if (is.null(srcfile$encoding)) encoding <- getOption("encoding")
	else encoding <- srcfile$encoding
	srcfile$conn <- conn <- file(srcfile$filename, open="rt", encoding=encoding)
	srcfile$line <- 1L
	oldline <- 1L
    } else if (!isOpen(conn)) {
	open(conn, open="rt")
	srcfile$line <- 1
	oldline <- 1L
    }
    if (oldline < line) {
	readLines(conn, line-oldline, warn=FALSE)
	srcfile$line <- line
    }
    invisible(conn)
}

close.srcfile <- function(con, ...) {
    srcfile <- con
    conn <- srcfile$conn
    if (is.null(conn)) return()
    else {
	close(conn)
	rm(list=c("conn", "line"), envir=srcfile)
    }
}

# srcfilecopy saves a copy of lines from a file

srcfilecopy <- function(filename, lines) {
    stopifnot(is.character(filename), length(filename) == 1L)

    e <- new.env(parent=emptyenv())

    e$filename <- filename
    e$lines <- as.character(lines)
    e$timestamp <- Sys.time()

    class(e) <- c("srcfilecopy", "srcfile")
    return(e)
}

open.srcfilecopy <- function(con, line, ...) {

    srcfile <- con

    oldline <- srcfile$line
    if (!is.null(oldline) && oldline > line) close(srcfile)

    conn <- srcfile$conn
    if (is.null(conn)) {
	srcfile$conn <- conn <- textConnection(srcfile$lines, open="r")
	srcfile$line <- 1L
	oldline <- 1L
    } else if (!isOpen(conn)) {
	open(conn, open="r")
	srcfile$line <- 1L
	oldline <- 1L
    }
    if (oldline < line) {
	readLines(conn, line-oldline, warn=FALSE)
	srcfile$line <- line
    }
    invisible(conn)
}

.isOpen <- function(srcfile) {
    conn <- srcfile$conn
    return( !is.null(conn) && isOpen(conn) )
}

getSrcLines <- function(srcfile, first, last) {
    if (first > last) return(character(0L))
    if (!.isOpen(srcfile)) on.exit(close(srcfile))
    conn <- open(srcfile, first)
    lines <- readLines(conn, n=last-first+1L, warn=FALSE)
    srcfile$line <- first + length(lines)
    return(lines)
}

# a srcref gives start and stop positions of text
# lloc entries are first_line, first_column, last_line, last_column
# all are inclusive

srcref <- function(srcfile, lloc) {
    stopifnot(inherits(srcfile, "srcfile"), length(lloc) == 4L)
    structure(as.integer(lloc), srcfile=srcfile, class="srcref")
}

as.character.srcref <- function(x, useSource = TRUE, ...)
{
    srcfile <- attr(x, "srcfile")
    if (!is.null(srcfile) && !inherits(srcfile, "srcfile")) class(srcfile) <- "srcfile"
    if (useSource) lines <- try(getSrcLines(srcfile, x[1L], x[3L]), TRUE)
    if (!useSource || inherits(lines, "try-error"))
    	lines <- paste("<srcref: file \"", srcfile$filename, "\" chars ",
                       x[1L],":",x[2L], " to ",x[3L],":",x[4L], ">", sep="")
    else {
        if (length(lines) < x[3L] - x[1L] + 1L)
            x[4L] <- .Machine$integer.max
    	lines[length(lines)] <- substring(lines[length(lines)], 1L, x[4L])
    	lines[1L] <- substring(lines[1L], x[2L])
    }
    lines
}

print.srcref <- function(x, useSource = TRUE, ...) {
    cat(as.character(x, useSource = useSource), sep="\n")
    invisible(x)
}
#  File src/library/base/R/stop.R
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

stop <- function(..., call. = TRUE, domain = NULL)
{
    args <- list(...)
    if (length(args) == 1L && inherits(args[[1L]], "condition")) {
        cond <- args[[1L]]
        if(nargs() > 1L)
            warning("additional arguments ignored in stop()")
        message <- conditionMessage(cond)
        call <- conditionCall(cond)
        .Internal(.signalCondition(cond, message, call))
        .Internal(.dfltStop(message, call))
    } else
        .Internal(stop(as.logical(call.), .makeMessage(..., domain = domain)))
}

stopifnot <- function(...)
{
    n <- length(ll <- list(...))
    if(n == 0L)
	return(invisible())
    mc <- match.call()
    for(i in 1L:n)
	if(!(is.logical(r <- ll[[i]]) && !any(is.na(r)) && all(r))) {
	    ch <- deparse(mc[[i+1]], width.cutoff = 60L)
	    if(length(ch) > 1L) ch <- paste(ch[1L], "....")
	    stop(paste(ch, " is not ", if(length(r) > 1L)"all ", "TRUE", sep=''),
		 call.= FALSE)
	}
    invisible()
}

warning <- function(..., call. = TRUE, immediate. = FALSE, domain = NULL)
{
    args <- list(...)
    if (length(args) == 1L && inherits(args[[1L]], "condition")) {
        cond <- args[[1L]]
        if(nargs() > 1L)
            cat(gettext("additional arguments ignored in warning()"),
                "\n", sep="", file = stderr())
        message <- conditionMessage(cond)
        call <- conditionCall(cond)
        withRestarts({
                .Internal(.signalCondition(cond, message, call))
                .Internal(.dfltWarn(message, call))
            }, muffleWarning = function() NULL) #**** allow simpler form??
        invisible(message)
    } else
        .Internal(warning(as.logical(call.), as.logical(immediate.),
                          .makeMessage(..., domain = domain)))
}

gettext <- function(..., domain = NULL) {
    args <- lapply(list(...), as.character)
    .Internal(gettext(domain, unlist(args)))
}

bindtextdomain <- function(domain, dirname = NULL)
    .Internal(bindtextdomain(domain, dirname))

ngettext <- function(n, msg1, msg2, domain = NULL)
    .Internal(ngettext(n, msg1, msg2, domain))

gettextf <- function(fmt, ..., domain = NULL)
    sprintf(gettext(fmt, domain = domain), ...)
#  File src/library/base/R/structure.R
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

## This remaps special names as they are used by deparsing, but why are they?
##
structure <- function (.Data, ...)
{
    attrib <- list(...)
    if(length(attrib)) {
        specials <- c(".Dim", ".Dimnames", ".Names", ".Tsp", ".Label")
        replace <- c("dim", "dimnames", "names", "tsp", "levels")
	m <- match(names(attrib), specials)
	ok <- (!is.na(m) & m)
	names(attrib)[ok] <- replace[m[ok]]
        ## prior to 2.5.0 factors would deparse to double codes
	if("factor" %in% attrib[["class", exact = TRUE]]
           && typeof(.Data) == "double")
	   storage.mode(.Data) <- "integer"
	attributes(.Data) <- c(attributes(.Data), attrib)
    }
    return(.Data)
}
#  File src/library/base/R/strwrap.R
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

strtrim <- function(x, width)
{
    if(!is.character(x)) x <- as.character(x)
    .Internal(strtrim(x, width))
}

strwrap <-
function(x, width = 0.9 * getOption("width"), indent = 0, exdent = 0,
         prefix = "", simplify = TRUE, initial = prefix)
{
    if(!is.character(x)) x <- as.character(x)
    ## Useful variables.
    indentString <- paste(rep.int(" ", indent), collapse = "")
    exdentString <- paste(rep.int(" ", exdent), collapse = "")
    y <- list()                         # return value
    ## input need not be valid in this locale, e.g. from write.dcf
    z <- lapply(strsplit(x, "\n[ \t\n]*\n", useBytes = TRUE),
                strsplit, "[ \t\n]", useBytes = TRUE)
    ## Now z[[i]][[j]] is a character vector of all "words" in
    ## paragraph j of x[i].

    for(i in seq_along(z)) {
        yi <- character(0L)
        for(j in seq_along(z[[i]])) {
            ## Format paragraph j in x[i].
            words <- z[[i]][[j]]
            nc <- nchar(words, type="w")
	    if(any(is.na(nc))) {
		## use byte count as a reasonable substitute
		nc0 <- nchar(words, type="b")
		nc[is.na(nc)] <- nc0[is.na(nc)]
	    }

            ## Remove extra white space unless after a period which
            ## hopefully ends a sentence.
            ## Add ? ! as other possible ends, and there might be
            ## quoted and parenthesised sentences.
            ## NB, input could be invalid here.
            if(any(nc == 0L)) {
                zLenInd <- which(nc == 0L)
                zLenInd <- zLenInd[!(zLenInd %in%
                                     (grep("[.?!][)\"']{0,1}$", words,
                                           perl = TRUE, useBytes = TRUE) + 1L))]
                if(length(zLenInd)) {
                    words <- words[-zLenInd]
                    nc <- nc[-zLenInd]
                }
            }

            if(!length(words)) {
                yi <- c(yi, "", initial)
                next
            }

            currentIndex <- 0L
            lowerBlockIndex <- 1L
            upperBlockIndex <- integer()
            lens <- cumsum(nc + 1L)

            first <- TRUE
            maxLength <- width - nchar(initial, type="w") - indent

            ## Recursively build a sequence of lower and upper indices
            ## such that the words in line k are the ones in the k-th
            ## index block.
            while(length(lens)) {
                k <- max(sum(lens <= maxLength), 1L)
                if(first) {
                    first <- FALSE
                    maxLength <- width - nchar(prefix, type="w") - exdent
                }
                currentIndex <- currentIndex + k
                if(nc[currentIndex] == 0L)
                    ## Are we sitting on a space?
                    upperBlockIndex <- c(upperBlockIndex,
                                         currentIndex - 1L)
                else
                    upperBlockIndex <- c(upperBlockIndex,
                                         currentIndex)
                if(length(lens) > k) {
                    ## Are we looking at a space?
                    if(nc[currentIndex + 1L] == 0L) {
                        currentIndex <- currentIndex + 1L
                        k <- k + 1L
                    }
                    lowerBlockIndex <- c(lowerBlockIndex,
                                         currentIndex + 1L)
                }
                if(length(lens) > k)
                    lens <- lens[-seq_len(k)] - lens[k]
                else
                    lens <- NULL
            }

            nBlocks <- length(upperBlockIndex)
            s <- paste(c(initial, rep.int(prefix, nBlocks - 1L)),
                       c(indentString, rep.int(exdentString, nBlocks - 1L)),
                       sep = "")
            initial <- prefix
            for(k in seq_len(nBlocks))
                s[k] <- paste(s[k], paste(words[lowerBlockIndex[k] :
                                                upperBlockIndex[k]],
                                          collapse = " "),
                              sep = "")

            yi <- c(yi, s, prefix)
        }
        y <- if(length(yi))
            c(y, list(yi[-length(yi)]))
        else
            c(y, "")
    }

    if(simplify) y <- unlist(y)
    y
}

formatDL <-
function(x, y, style = c("table", "list"),
         width = 0.9 * getOption("width"), indent = NULL)
{
    if(is.list(x)) {
        if((length(x) == 2L) && (diff(sapply(x, length)) == 0L)) {
            y <- x[[2L]]; x <- x[[1L]]
        }
        else
            stop("incorrect value for 'x'")
    }
    else if(is.matrix(x)) {
        if(NCOL(x) == 2L) {
            y <- x[, 2L]; x <- x[, 1L]
        }
        else
            stop("incorrect value for 'x'")
    }
    else if(missing(y) && !is.null(nms <- names(x))) {
        y <- x
        x <- nms
    }
    else if(length(x) != length(y))
        stop("'x' and 'y' must have the same length")
    x <- as.character(x)
    if(!length(x)) return(x)
    y <- as.character(y)

    style <- match.arg(style)

    if(is.null(indent))
        indent <- switch(style, table = width / 3, list = width / 9)
    if(indent > 0.5 * width)
        stop("incorrect values of 'indent' and 'width'")

    indentString <- paste(rep.int(" ", indent), collapse = "")

    if(style == "table") {
        i <- (nchar(x, type="w") > indent - 3L)
        if(any(i))
            x[i] <- paste(x[i], "\n", indentString, sep = "")
        i <- !i
        if(any(i))
            x[i] <- formatC(x[i], width = indent, flag = "-")
        y <- lapply(strwrap(y, width = width - indent,
                            simplify = FALSE),
                    paste,
                    collapse = paste("\n", indentString, sep = ""))
        r <- paste(x, unlist(y), sep = "")
    }
    else if(style == "list") {
        y <- strwrap(paste(x, ": ", y, sep = ""), exdent = indent,
                     width = width, simplify = FALSE)
        r <- unlist(lapply(y, paste, collapse = "\n"))
    }
    r
}
#  File src/library/base/R/summary.R
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

summary <- function (object, ...) UseMethod("summary")

summary.default <-
    function(object, ..., digits = max(3, getOption("digits") - 3))
{
    if(is.factor(object))
	return(summary.factor(object, ...))
    else if(is.matrix(object))
	return(summary.matrix(object, digits = digits, ...))

    value <- if(is.logical(object))# scalar or array!
	c(Mode = "logical",
          {tb <- table(object, exclude=NULL)# incl. NA s
           if(!is.null(n <- dimnames(tb)[[1L]]) && any(iN <- is.na(n)))
               dimnames(tb)[[1L]][iN] <- "NA's"
           tb
           })
    else if(is.numeric(object)) {
	nas <- is.na(object)
	object <- object[!nas]
	qq <- stats::quantile(object)
	qq <- signif(c(qq[1L:3L], mean(object), qq[4L:5L]), digits)
	names(qq) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
	if(any(nas))
	    c(qq, "NA's" = sum(nas))
	else qq
    } else if(is.recursive(object) && !is.language(object) &&
	      (n <- length(object))) {
	sumry <- array("", c(n, 3L), list(names(object),
					 c("Length", "Class", "Mode")))
	ll <- numeric(n)
	for(i in 1L:n) {
	    ii <- object[[i]]
	    ll[i] <- length(ii)
	    cls <- oldClass(ii)
	    sumry[i, 2L] <- if(length(cls)) cls[1L] else "-none-"
	    sumry[i, 3L] <- mode(ii)
	}
	sumry[, 1L] <- format(as.integer(ll))
	sumry
    }
    else c(Length= length(object), Class= class(object), Mode= mode(object))
    class(value) <- "table"
    value
}

summary.factor <- function(object, maxsum = 100, ...)
{
    nas <- is.na(object)
    ll <- levels(object)
    if(any(nas)) maxsum <- maxsum - 1
    tbl <- table(object)
    tt <- c(tbl) # names dropped ...
    names(tt) <- dimnames(tbl)[[1L]]
    if(length(ll) > maxsum) {
	drop <- maxsum:length(ll)
	o <- sort.list(tt, decreasing = TRUE)
	tt <- c(tt[o[ - drop]], "(Other)" = sum(tt[o[drop]]))
    }
    if(any(nas)) c(tt, "NA's" = sum(nas)) else tt
}

summary.matrix <- function(object, ...) {
    ## we do want this changed into separate columns, so use matrix method
    summary.data.frame(as.data.frame.matrix(object), ...)
}

## <FIXME> use encodeString here, and its justify options
summary.data.frame <-
    function(object, maxsum = 7, digits = max(3, getOption("digits") - 3), ...)
{
    # compute results to full precision.
    z <- lapply(as.list(object), summary, maxsum = maxsum, digits = 12, ...)
    nv <- length(object)
    nm <- names(object)
    lw <- numeric(nv)
    nr <- max(unlist(lapply(z, NROW)))
    for(i in 1L:nv) {
        sms <- z[[i]]
        if(is.matrix(sms)) {
            ## need to produce a single column, so collapse matrix
            ## across rows
            cn <- paste(nm[i], gsub("^ +", "", colnames(sms)), sep=".")
            tmp <- format(sms)
            if(nrow(sms) < nr)
                tmp <- rbind(tmp, matrix("", nr - nrow(sms), ncol(sms)))
            sms <- apply(tmp, 1L, function(x) paste(x, collapse="  "))
            ## produce a suitable colname: undoing padding
            wid <- sapply(tmp[1L, ], nchar, type="w")
            blanks <- paste(character(max(wid)), collapse = " ")
            pad0 <- floor((wid-nchar(cn, type="w"))/2)
            pad1 <- wid - nchar(cn, type="w") - pad0
            cn <- paste(substring(blanks, 1L, pad0), cn,
                        substring(blanks, 1L, pad1), sep = "")
            nm[i] <- paste(cn, collapse="  ")
            z[[i]] <- sms
        } else {
            lbs <- format(names(sms))
            sms <- paste(lbs, ":", format(sms, digits = digits), "  ",
                         sep = "")
            lw[i] <- nchar(lbs[1L], type="w")
            length(sms) <- nr
            z[[i]] <- sms
        }
    }
    z <- unlist(z, use.names=TRUE)
    dim(z) <- c(nr, nv)
    blanks <- paste(character(max(lw) + 2L), collapse = " ")
    pad <- floor(lw-nchar(nm, type="w")/2)
    nm <- paste(substring(blanks, 1, pad), nm, sep = "")
    dimnames(z) <- list(rep.int("", nr), nm)
    attr(z, "class") <- c("table") #, "matrix")
    z
}
#  File src/library/base/R/svd.R
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

svd <- function(x, nu = min(n,p), nv = min(n,p), LINPACK = FALSE)
{
    x <- as.matrix(x)
    if (any(!is.finite(x))) stop("infinite or missing values in 'x'")
    dx <- dim(x)
    n <- dx[1L]
    p <- dx[2L]
    if(!n || !p) stop("0 extent dimensions")
    if (is.complex(x)) {
        res <- La.svd(x, nu, nv)
        return(list(d = res$d, u = if(nu) res$u, v = if(nv) Conj(t(res$vt))))
    }
    if(!is.double(x))
	storage.mode(x) <- "double"
    if (!LINPACK) {
        res <- La.svd(x, nu, nv)
        return(list(d = res$d, u = if(nu) res$u, v = if(nv) t(res$vt)))
    }

    if(nu == 0L) {
	job <- 0L
	u <- double(0L)
    }
    else if(nu == n) {
	job <- 10L
	u <- matrix(0, n, n)
    }
    else if(nu == p) {
	job <- 20L
	u <- matrix(0, n, p)
    }
    else
	stop("'nu' must be 0, nrow(x) or ncol(x)")

    job <- job +
	if(nv == 0L) 0L else if(nv == p || nv == n) 1L else
    stop("'nv' must be 0 or ncol(x)")

    v <- if(job == 0L) double(0L) else matrix(0, p, p)

    mn <- min(n,p)
    mm <- min(n+1L,p)
    z <- .Fortran("dsvdc",
		  as.double(x),
		  n,
		  n,
		  p,
		  d=double(mm),
		  double(p),
		  u=u,
		  n,
		  v=v,
		  p,
		  double(n),
		  as.integer(job),
		  info=integer(1L),
		  DUP=FALSE, PACKAGE="base")[c("d","u","v","info")]
    if(z$info)
	stop(gettextf("error %d in 'dsvdc'", z$info), domain = NA)
    z$d <- z$d[1L:mn]
    if(nv && nv < p) z$v <- z$v[, 1L:nv, drop = FALSE]
    z[c("d", if(nu) "u", if(nv) "v")]
}
#  File src/library/base/R/sweep.R
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

sweep <- function(x, MARGIN, STATS, FUN = "-", check.margin=TRUE, ...)
{
    FUN <- match.fun(FUN)
    dims <- dim(x)
    if (check.margin) {
        dimmargin <- dims[MARGIN]
        dimstats <- dim(STATS)
        lstats <- length(STATS)
        if (lstats > prod(dimmargin)) {
            warning("STATS is longer than the extent of 'dim(x)[MARGIN]'")
        } else if (is.null(dimstats)) { # STATS is a vector
            cumDim <- c(1, cumprod(dimmargin))
            upper <- min(cumDim[cumDim >= lstats])
            lower <- max(cumDim[cumDim <= lstats])
            if (lstats && (upper %% lstats != 0L || lstats %% lower != 0L))
                warning("STATS does not recycle exactly across MARGIN")
        } else {
            dimmargin <- dimmargin[dimmargin > 1L]
            dimstats <- dimstats[dimstats > 1L]
            if (length(dimstats) != length(dimmargin) ||
                any(dimstats != dimmargin))
                warning("length(STATS) or dim(STATS) do not match dim(x)[MARGIN]")
        }
    }
    perm <- c(MARGIN, seq_along(dims)[ - MARGIN])
    FUN(x, aperm(array(STATS, dims[perm]), order(perm)), ...)
}
#  File src/library/base/R/switch.R
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

switch <- function(EXPR,...)
    .Internal(switch(EXPR,...))
#  File src/library/base/R/sys.R
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

sys.call <-function(which = 0)
    .Internal(sys.call(which))

sys.calls <-function()
    .Internal(sys.calls())

sys.frame <-function(which = 0)
    .Internal(sys.frame(which))

sys.function <-function(which = 0)
    .Internal(sys.function(which))

sys.frames <-function()
    .Internal(sys.frames())

sys.nframe <- function()
    .Internal(sys.nframe())

sys.parent <- function(n = 1)
    .Internal(sys.parent(n))

sys.parents <- function()
    .Internal(sys.parents())

sys.status <- function()
    list(sys.calls=sys.calls(), sys.parents=sys.parents(),
         sys.frames=sys.frames())

sys.on.exit <- function()
    .Internal(sys.on.exit())
#  File src/library/base/R/table.R
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

table <- function (..., exclude = if (useNA=="no") c(NA, NaN),
                   useNA = c("no", "ifany", "always"),
		   dnn = list.names(...), deparse.level = 1)
{
    list.names <- function(...) {
	l <- as.list(substitute(list(...)))[-1L]
	nm <- names(l)
	fixup <- if (is.null(nm)) seq_along(l) else nm == ""
	dep <- sapply(l[fixup], function(x)
	    switch (deparse.level + 1,
		    "", ## 0
		    if (is.symbol(x)) as.character(x) else "", ## 1
		    deparse(x, nlines=1)[1L]) ## 2
		      )
	if (is.null(nm))
	    dep
	else {
	    nm[fixup] <- dep
	    nm
	}
    }
    if (!missing(exclude) && is.null(exclude))
        useNA <- "always"

    useNA <- match.arg(useNA)
    args <- list(...)
    if (length(args) == 0L)
	stop("nothing to tabulate")
    if (length(args) == 1L && is.list(args[[1L]])) {
	args <- args[[1L]]
	if (length(dnn) != length(args))
	    dnn <- if (!is.null(argn <- names(args)))
		 argn
	    else
		 paste(dnn[1L], seq_along(args), sep = ".")
    }
    # 0L, 1L, etc: keep 'bin' and 'pd' integer - as long as tabulate() requires it
    bin <- 0L
    lens <- NULL
    dims <- integer(0L)
    pd <- 1L
    dn <- NULL
    for (a in args) {
	if (is.null(lens)) lens <- length(a)
	else if (length(a) != lens)
	    stop("all arguments must have the same length")
        cat <-
            if (is.factor(a)) {
                if (any(is.na(levels(a)))) # Don't touch this!
                    a
                else {
                    ## The logic here is tricky because it tries to do
                    ## something sensible if both 'exclude' and
                    ## 'useNA' is set. A non-null setting of 'exclude'
                    ## sets the excluded levels to missing, which is
                    ## different from the <NA> factor level. Excluded
                    ## levels are NOT tabulated, even if 'useNA' is
                    ## set.
                    if (is.null(exclude) && useNA != "no")
                        addNA(a, ifany = (useNA == "ifany"))
                    else {
                        if (useNA != "no")
                            a <- addNA(a, ifany = (useNA == "ifany"))
                        ll <- levels(a)
                        a <- factor(a, levels = ll[!(ll %in% exclude)],
                               exclude = if (useNA == "no") NA)
                    }
                }
            }
            else {
                a <- factor(a, exclude = exclude)
                if (useNA != "no")
                    addNA(a, ifany = (useNA == "ifany"))
                else
                    a
            }

	nl <- length(ll <- levels(cat))
	dims <- c(dims, nl)
	dn <- c(dn, list(ll))
	## requiring   all(unique(as.integer(cat)) == 1L:nlevels(cat))  :
	bin <- bin + pd * (as.integer(cat) - 1L)
	pd <- pd * nl
    }
    names(dn) <- dnn
    bin <- bin[!is.na(bin)]
    if (length(bin)) bin <- bin + 1L # otherwise, that makes bin NA
    y <- array(tabulate(bin, pd), dims, dimnames = dn)
    class(y) <- "table"
    y
}

## From  1999-12-19 till 2003-03-27:
## print.table <-
## function(x, digits = getOption("digits"), quote = FALSE, na.print = "", ...)
## {
##     print.default(unclass(x), digits = digits, quote = quote,
## 		  na.print = na.print, ...)
##     ## this does *not* return x !
## }

## Better (NA in dimnames *should* be printed):
print.table <-
function (x, digits = getOption("digits"), quote = FALSE, na.print = "",
	  zero.print = "0",
	  justify = "none", ...)
{
    xx <- format(unclass(x), digits = digits, justify = justify)
    ## na.print handled here
    if(any(ina <- is.na(x)))
	xx[ina] <- na.print

    if(zero.print != "0" && any(i0 <- !ina & x == 0) && all(x == round(x)))
	## MM thinks this should be an option for many more print methods...
	xx[i0] <- sub("0", zero.print, xx[i0])

    ## Numbers get right-justified by format(), irrespective of 'justify'.
    ## We need to keep column headers aligned.
    if (is.numeric(x) || is.complex(x))
        print(xx, quote = quote, right = TRUE, ...)
    else
        print(xx, quote = quote, ...)
    invisible(x)
}

summary.table <- function(object, ...)
{
    if(!inherits(object, "table"))
	stop("'object' must inherit from class \"table\"")
    n.cases <- sum(object)
    n.vars <- length(dim(object))
    y <- list(n.vars = n.vars,
	      n.cases = n.cases)
    if(n.vars > 1) {
	m <- vector("list", length = n.vars)
	relFreqs <- object / n.cases
	for(k in 1L:n.vars)
	    m[[k]] <- apply(relFreqs, k, sum)
	expected <- apply(do.call("expand.grid", m), 1L, prod) * n.cases
	statistic <- sum((c(object) - expected)^2 / expected)
	parameter <-
	    prod(sapply(m, length)) - 1L - sum(sapply(m, length) - 1L)
	y <- c(y, list(statistic = statistic,
		       parameter = parameter,
		       approx.ok = all(expected >= 5),
		       p.value = stats::pchisq(statistic, parameter, lower.tail=FALSE),
		       call = attr(object, "call")))
    }
    class(y) <- "summary.table"
    y
}

print.summary.table <-
function(x, digits = max(1, getOption("digits") - 3), ...)
{
    if(!inherits(x, "summary.table"))
	stop("'x' must inherit from class \"summary.table\"")
    if(!is.null(x$call)) {
	cat("Call: "); print(x$call)
    }
    cat("Number of cases in table:", x$n.cases, "\n")
    cat("Number of factors:", x$n.vars, "\n")
    if(x$n.vars > 1) {
	cat("Test for independence of all factors:\n")
	ch <- x$statistic
	cat("\tChisq = ",	format(round(ch, max(0, digits - log10(ch)))),
	    ", df = ",		x$parameter,
	    ", p-value = ",	format.pval(x$p.value, digits, eps = 0),
	    "\n", sep = "")
	if(!x$approx.ok)
	    cat("\tChi-squared approximation may be incorrect\n")
    }
    invisible(x)
}

as.data.frame.table <-
    function(x, row.names = NULL, ..., responseName = "Freq",
             stringsAsFactors = TRUE)
{
    x <- as.table(x)
    ex <- quote(data.frame(do.call("expand.grid",
                                   c(dimnames(x),
                                     stringsAsFactors = stringsAsFactors)),
                           Freq = c(x),
                           row.names = row.names))
    names(ex)[3L] <- responseName
    eval(ex)
}

is.table <- function(x) inherits(x, "table")
as.table <- function(x, ...) UseMethod("as.table")
as.table.default <- function(x, ...)
{
    if(is.table(x))
	return(x)
    else if(is.array(x) || is.numeric(x)) {
	x <- as.array(x)
	if(any(dim(x) == 0))
	    stop("cannot coerce into a table")
	## Try providing dimnames where missing.
	dnx <- dimnames(x)
	if(is.null(dnx))
	    dnx <- vector("list", length(dim(x)))
	for(i in which(sapply(dnx, is.null)))
	    dnx[[i]] <-
                make.unique(LETTERS[seq.int(from=0, length.out = dim(x)[i]) %% 26 + 1],
                            sep = "")
	dimnames(x) <- dnx
	class(x) <- c("table", oldClass(x))
	return(x)
    }
    else
	stop("cannot coerce into a table")
}

prop.table <- function(x, margin = NULL)
{
    if(length(margin))
	sweep(x, margin, margin.table(x, margin), "/", check.margin=FALSE)
    else
	x / sum(x)
}

margin.table <- function(x, margin = NULL)
{
    if(!is.array(x)) stop("'x' is not an array")
    if (length(margin)) {
	z <- apply(x, margin, sum)
	dim(z) <- dim(x)[margin]
	dimnames(z) <- dimnames(x)[margin]
    }
    else return(sum(x))
    class(z) <- oldClass(x) # avoid adding "matrix"
    z
}
#  File src/library/base/R/tabulate.R
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

tabulate <- function(bin, nbins = max(1L, bin, na.rm=TRUE))
{
    if(!is.numeric(bin) && !is.factor(bin))
	stop("'bin' must be numeric or a factor")
    .C("R_tabulate",
       as.integer(bin),
       as.integer(length(bin)),
       as.integer(nbins),
       ans = integer(nbins),
       NAOK = TRUE,
       PACKAGE="base")$ans
}
#  File src/library/base/R/tapply.R
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

tapply <- function (X, INDEX, FUN=NULL, ..., simplify=TRUE)
{
    FUN <- if (!is.null(FUN)) match.fun(FUN)
    if (!is.list(INDEX)) INDEX <- list(INDEX)
    nI <- length(INDEX)
    namelist <- vector("list", nI)
    names(namelist) <- names(INDEX)
    extent <- integer(nI)
    nx <- length(X)
    one <- 1L
    group <- rep.int(one, nx)#- to contain the splitting vector
    ngroup <- one
    for (i in seq.int(INDEX)) {
	index <- as.factor(INDEX[[i]])
	if (length(index) != nx)
	    stop("arguments must have same length")
	namelist[[i]] <- levels(index)#- all of them, yes !
	extent[i] <- nlevels(index)
	group <- group + ngroup * (as.integer(index) - one)
	ngroup <- ngroup * nlevels(index)
    }
    if (is.null(FUN)) return(group)
    ans <- lapply(split(X, group), FUN, ...)
    index <- as.integer(names(ans))
    if (simplify && all(unlist(lapply(ans, length)) == 1L)) {
	ansmat <- array(dim=extent, dimnames=namelist)
	ans <- unlist(ans, recursive = FALSE)
    } else {
	ansmat <- array(vector("list", prod(extent)),
			dim=extent, dimnames=namelist)
    }
    if(length(index)) {
        names(ans) <- NULL
        ansmat[index] <- ans
    }
    ansmat
}





#  File src/library/base/R/taskCallback.R
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

addTaskCallback <- function(f, data = NULL, name = character(0L))
{
    if(!is.function(f))
        stop("handler must be a function")
    val <- .Call("R_addTaskCallback", f, data, !missing(data),
                 as.character(name), PACKAGE="base")
    val + 1L
}

removeTaskCallback <- function(id)
{
    if(!is.character(id))
        id <- as.integer(id)

    .Call("R_removeTaskCallback", id, PACKAGE="base")
}

getTaskCallbackNames <-
function()
{
    .Call("R_getTaskCallbackNames", PACKAGE="base")
}


taskCallbackManager <-
  #
  #
  #
function(handlers = list(), registered = FALSE, verbose = FALSE)
{
    suspended <- FALSE
    .verbose <- verbose

    add <-
    #
    # this is used to register a callback.
    # It has the same call sequence and semantics
    # as addTaskCallback but provides an optional
    # name by which to identify the element.
    # This can be used to remove the value in the future.
    # The default name is the next available position in the
    # list.
    # The result is stored in the `handlers' list using the
    # name.
    #
    # The element in the list contains the function
    # in the `f' slot,  and optionally a data field
    # to store the `data' argument.
    #
    # This could arrange to register itself using
    # addTaskCallback() if the size of the handlers list
    # becomes 1.
        function(f, data = NULL, name = NULL, register = TRUE)
        {

      # generate default name if none supplied
            if(is.null(name))
                name <- as.character(length(handlers) + 1L)

      # Add to handlers, replacing any element with that name
      # if needed.
            handlers[[name]] <<- list(f = f)

      # If data was specified, add this to the new element
      # so that it will be included in the call for this function
            if(!missing(data))
                handlers[[name]][["data"]] <<- data

      # We could arrange to register the evaluate function
      # so that the handlers list would be active. However,
      # we would have to unregister it in the remove()
      # function when there were no handlers.
            if(!registered && register) {
                register()
            }

            name
        }

    remove <- function(which)
    {
        if(is.character(which)) {
            tmp <- seq_along(handlers)[!is.na(match(which, names(handlers)))]
            if(length(tmp))
                stop(gettextf("no such element '%s'", which), domain = NA)
            which <- tmp
        } else
        which <- as.integer(which)

        handlers <<- handlers[-which]

        return(TRUE)
    }


    evaluate <-
    #
    # This is the actual callback that is registered with the C-level
    # mechanism. It is invoked by R when a top-level task is completed.
    # It then calls each of the functions in the handlers list
    # passing these functions the arguments it received and any
    # user-level data for those functions registered in the call to
    # add() via the `data' argument.
    #
    # At the end of the evaluation, any function that returned FALSE
    # is discarded.
        function(expr, value, ok, visible)
        {
            if(suspended)
                return(TRUE)
            discard <- character(0L)
            for(i in names(handlers)) {
                h <- handlers[[i]]
                if(length(h) > 1L) {
                    val <- h[["f"]](expr, value, ok, visible, i[["data"]])
                } else {
                    val <- h[["f"]](expr, value, ok, visible)
                }
                if(!val) {
                    discard <- c(discard, i)
                }
            }
            if(length(discard)) {
                if(.verbose)
                    cat(gettext("Removing"), paste(discard, collapse=", "), "\n")
                idx <- is.na(match(names(handlers), discard))
                if(length(idx))
                    handlers <<- handlers[idx]
                else
                    handlers <<- list()
            }
            return(TRUE)
        }

    suspend <-
        function(status = TRUE) {
            suspended <<- status
        }

    register <-
        function(name = "R-taskCallbackManager", verbose = .verbose)
        {
            if(verbose)
                cat(gettext("Registering evaluate as low-level callback\n"))
            id <- addTaskCallback(evaluate, name = name)
            registered <<- TRUE
            id
        }

    list(add = add,
         evaluate = evaluate,
         remove = remove,
         register = register,
         suspend = suspend,
         callbacks = function()
         handlers
         )
}
#  File src/library/base/R/temp.R
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

tempfile <- function(pattern = "file", tmpdir = tempdir())
    .Internal(tempfile(pattern, tmpdir))

tempdir <- function() .Internal(tempdir())
#  File src/library/base/R/time.R
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

system.time <- function(expr, gcFirst = TRUE)
{
    ppt <- function(y) {
        if(!is.na(y[4L])) y[1L] <- y[1L] + y[4L]
        if(!is.na(y[5L])) y[2L] <- y[2L] + y[5L]
        y[1L:3L]
    }
    if(!exists("proc.time")) return(rep(NA_real_, 5L))
    if(gcFirst)  gc(FALSE)
    time <- proc.time()
    ## need on.exit after 'time' has been set:
    ## on some systems proc.time throws an error.
    on.exit(cat("Timing stopped at:", ppt(proc.time() - time), "\n"))
    expr # evaluated here because of lazy evaluation
    new.time <- proc.time()
    on.exit()
    structure(new.time - time, class="proc_time")
}
unix.time <- system.time

date <- function() .Internal(date())

print.proc_time <- function(x, ...)
{
    y <- x
    if(!is.na(y[4L])) y[1L] <- y[1L] + y[4L]
    if(!is.na(y[5L])) y[2L] <- y[2L] + y[5L]
    y <- y[1L:3L]
    names(y) <- c(gettext("user"), gettext("system"), gettext("elapsed"))
    print(y, ...)
    invisible(x)
}
#  File src/library/base/R/toString.R
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

#functions to convert their first argument to strings
toString <- function(x, ...) UseMethod("toString")

toString.default <- function(x, width = NULL, ...)
{
    string <- paste(x, collapse=", ")
    if( missing(width) || is.null(width) || width == 0) return(string)
    if( width < 0 ) stop("'width' must be positive")
    if(nchar(string, type = "w") > width) {
        width <- max(6, width) ## Leave something!
        string <- paste(strtrim(string, width - 4), "....", sep = "")
    }
    string
}
#  File src/library/base/R/traceback.R
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

traceback <-
function(x = NULL, max.lines = getOption("deparse.max.lines"))
{
    if(is.null(x) && (exists(".Traceback", envir = baseenv())))
	x <- get(".Traceback", envir = baseenv())
    n <- length(x)
    if(n == 0L)
        cat(gettext("No traceback available"), "\n")
    else {
        for(i in 1L:n) {
            label <- paste(n-i+1L, ": ", sep="")
            m <- length(x[[i]])            
            if (!is.null(srcref <- attr(x[[i]], "srcref"))) {
            	srcfile <- attr(srcref, "srcfile")
            	x[[i]][m] <- paste(x[[i]][m], " at ", 
            	            basename(srcfile$filename), "#", srcref[1L], sep="")
            }
            if(m > 1)
                label <- c(label, rep(substr("          ", 1L,
                                             nchar(label, type="w")),
                                      m - 1L))
            if(is.numeric(max.lines) && max.lines > 0L && max.lines < m) {
                cat(paste(label[1L:max.lines], x[[i]][1L:max.lines], sep = ""),
                    sep = "\n")
                cat(label[max.lines+1L], " ...\n")
            } else
            	cat(paste(label, x[[i]], sep=""), sep="\n")
        }
    }
    invisible()
}
#  File src/library/base/R/unix/system.unix.R
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

system <- function(command, intern = FALSE, ignore.stderr = FALSE,
                   wait = TRUE, input = NULL,
                   show.output.on.console = TRUE, minimized = FALSE,
                   invisible = TRUE)
{
    if(!missing(show.output.on.console) || !missing(minimized)
       || !missing(invisible))
        warning("arguments 'show.output.on.console', 'minimized' and 'invisible' are for Windows only")

    if(ignore.stderr) command <- paste(command, "2>/dev/null")
    if(!is.null(input)) {
        if(!is.character(input))
            stop("'input' must be a character vector or 'NULL'")
        f <- tempfile()
        on.exit(unlink(f))
        cat(input, file=f, sep="\n")
        command <- paste(command, "<", f)
    }
    if(!wait && !intern) command <- paste(command, "&")
    .Internal(system(command, intern))
}

Sys.which <- function(names)
{
    res <- character(length(names)); names(res) <- names
    for(i in names) {
        ans <- system(paste("which", i), intern=TRUE, ignore.stderr=TRUE)
        res[i] <- if(length(ans)) ans[1] else ""
    }
    res
}
#  File src/library/base/R/unlist.R
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

unlist <- function(x, recursive=TRUE, use.names=TRUE)
{
    if(.Internal(islistfactor(x, recursive))) {
        lv <- unique(.Internal(unlist(lapply(x, levels), recursive, FALSE)))
        nm <- if(use.names) names(.Internal(unlist(x, recursive, use.names)))
        res <- .Internal(unlist(lapply(x, as.character), recursive, FALSE))
        res <- match(res, lv)
        ## we cannot make this ordered as level set may have been changed
        structure(res, levels=lv, names=nm, class="factor")
    } else .Internal(unlist(x, recursive, use.names))
}
#  File src/library/base/R/unname.R
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

unname <- function (obj, force= FALSE) {
    if (!is.null(names(obj)))
        names(obj) <- NULL
    if (!is.null(dimnames(obj)) && (force || !is.data.frame(obj)))
        dimnames(obj) <- NULL
    obj
}
#  File src/library/base/R/upper.tri.R
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

upper.tri <- function(x, diag = FALSE)
{
    x <- as.matrix(x)
    if(diag) row(x) <= col(x)
    else row(x) < col(x)
}
#  File src/library/base/R/userhooks.R
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

.userHooksEnv <- new.env(hash = FALSE, parent = baseenv())

packageEvent <-
    function(pkgname, event=c("onLoad", "attach", "detach", "onUnload"))
{
    event <- match.arg(event)
    pkgname <- strsplit(pkgname, "_", fixed=TRUE)[[1L]][1L]
    paste("UserHook", pkgname, event, sep = "::")
}

getHook <- function(hookName)
{
    if (exists(hookName, envir = .userHooksEnv, inherits = FALSE))
        get(hookName, envir = .userHooksEnv, inherits = FALSE)
    else list()
}

setHook <- function(hookName, value,
                    action = c("append", "prepend", "replace"))
{
    action <- match.arg(action)
    old <- getHook(hookName)
    new <- switch(action,
                  "append" = c(old, value),
                  "prepend" = c(value, old),
                  "replace" = value)
    if (length(new))
        assign(hookName, new, envir = .userHooksEnv, inherits = FALSE)
    else if(exists(hookName, envir = .userHooksEnv, inherits = FALSE))
        remove(list=hookName, envir = .userHooksEnv, inherits = FALSE)
    invisible()
}
#  File src/library/base/R/utilities.R
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

mat.or.vec <- function(nr,nc)
    if(nc == 1L) numeric(nr) else matrix(0, nr, nc)

## Use  'version' since that exists in all S dialects :
is.R <-
    function() exists("version") && !is.null(vl <- version$language) && vl == "R"

#  File src/library/base/R/utils.R
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

shQuote <- function(string, type = c("sh", "csh", "cmd"))
{
    cshquote <- function(x) {
        xx <- strsplit(x, "'", fixed = TRUE)[[1L]]
        paste(paste("'", xx, "'", sep = ""), collapse="\"'\"")
    }
    if(missing(type) && .Platform$OS.type == "windows") type <- "cmd"
    type <- match.arg(type)
    if(type == "cmd") {
        paste('"', gsub('"', '\\\\"', string), '"', sep = "")
    } else {
        if(!length(string)) return("")
        has_single_quote <- grep("'", string)
        if(!length(has_single_quote))
            return(paste("'", string, "'", sep = ""))
        if(type == "sh")
            paste('"', gsub('(["$`\\])', "\\\\\\1", string), '"', sep="")
        else {
            if(!length(grep("([$`])", string))) {
                paste('"', gsub('(["!\\])', "\\\\\\1", string), '"', sep="")
            } else sapply(string, cshquote)
        }
    }
}

.standard_regexps <-
function()
{
    list(valid_package_name = "[[:alpha:]][[:alnum:].]*",
         valid_package_version = "([[:digit:]]+[.-]){1,}[[:digit:]]+",
         valid_R_system_version =
         "[[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+",
         valid_numeric_version = "([[:digit:]]+[.-])*[[:digit:]]+")
}
#  File src/library/base/R/vector.R
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

vector <- function(mode = "logical", length = 0).Internal(vector(mode,length))
logical <- function(length = 0) vector("logical", length)
character <- function(length = 0) vector("character", length)
integer <- function(length = 0) vector("integer", length)
double <- function(length = 0) vector("double", length)
real <- double
numeric <- double
complex <- function(length.out = 0,
		    real = numeric(), imaginary = numeric(),
		    modulus = 1, argument = 0) {
    if(missing(modulus) && missing(argument)) {
	## assume 'real' and 'imaginary'
	.Internal(complex(length.out, real, imaginary))
    } else {
	n <- max(length.out, length(argument), length(modulus))
	rep(modulus,length.out=n) *
	    exp(1i * rep(argument, length.out=n))
    }
}

single <- function(length = 0)
    structure(vector("double", length), Csingle=TRUE)
#  File src/library/base/R/version.R
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

## A simple S3 class for numeric versions (including package versions),
## and associated methods.

## We represent "vectors" of numeric versions as lists of sequences of
## integers, as obtained by splitting the version strings on the
## separators.  By default, only valid version specs (sequences of
## integers of suitable length), separated by '.' or '-', are allowed.
## If strictness is turned off, invalid specs result in integer()
## (rather than NA) to keep things simple.  (Note: using NULL would make
## subscripting more cumbersome ...)

## (In fact, the underlying mechanism could easily be extended to more
## general alphanumberic version specs.  E.g., one could allow "letters"
## in version numbers by replacing the non-sep characters in the version
## string by their ASCII codes.  However, this is not straightforward:
## alternatively, one could use an extended scheme with special markup
## for alpha, beta, release candidate, release, and patch versions, as
## used by many open source programs.  See e.g. the version::AlphaBeta
## module on CPAN.)

.make_numeric_version <-
function(x, strict = TRUE, regexp, classes = NULL)
{
    ## Internal creator for numeric version objects.

    x <- as.character(x)
    y <- rep.int(list(integer()), length(x))
    valid_numeric_version_regexp <- sprintf("^%s$", regexp)
    if(length(x)) {
        ok <- grepl(valid_numeric_version_regexp, x)
        if(!all(ok) && strict)
            stop("invalid version specification ", x, call. = FALSE)
        y[ok] <- lapply(strsplit(x[ok], "[.-]"), as.integer)
    }
    class(y) <- unique(c(classes, "numeric_version"))
    y
}

## Basic numeric versions.

numeric_version <-
function(x, strict = TRUE)
    .make_numeric_version(x, strict,
                          .standard_regexps()$valid_numeric_version)

is.numeric_version <-
function(x)
    inherits(x, "numeric_version")

as.numeric_version <-
function(x)
{
    if(is.numeric_version(x)) x
    else if(is.package_version(x)) {
        ## Pre 2.6.0 is.package_version() compatibility code ...
        ## Simplify eventually ...
        structure(x, class = c(class(x), "numeric_version"))
    }
    else numeric_version(x)
}

## Package versions must have at least two integers, corresponding to
## major and minor.

package_version <-
function(x, strict = TRUE)
{
    ## Special-case R version lists.
    ## Currently, do this here for backward compatibility.
    ## Should this be changed eventually?
    if(is.list(x) && all(c("major", "minor") %in% names(x)))
        return(R_system_version(paste(x[c("major", "minor")],
                                      collapse = ".")))
    .make_numeric_version(x, strict,
                          .standard_regexps()$valid_package_version,
                          "package_version")
}

is.package_version <-
function(x)
    inherits(x, "package_version")

as.package_version <-
function(x)
    if(is.package_version(x)) x else package_version(x)

## R system versions must have exactly three integers.
## (Not sure if reduced strictness makes a lot of sense here.)

R_system_version <-
function(x, strict = TRUE)
    .make_numeric_version(x, strict,
                          .standard_regexps()$valid_R_system_version,
                          c("R_system_version", "package_version"))

getRversion <-
function()
    package_version(R.version)

## Workhorses.

## <NOTE>
## Could use this for or as as.double.numeric_version() ...
## </NOTE>

.encode_numeric_version <-
function(x, base = NULL)
{
    if(!is.numeric_version(x)) stop("wrong class")
    if(is.null(base)) base <- max(unlist(x), 0, na.rm = TRUE) + 1
    classes <- class(x)
    lens <- as.numeric(sapply(x, length))
    ## We store the lengths so that we know when to stop when decoding.
    ## Alternatively, we need to be smart about trailing zeroes.  One
    ## approach is to increment all numbers in the version specs and
    ## base by 1, and when decoding only retain the non-zero entries and
    ## decrement by 1 one again.
    x <- as.numeric(sapply(x,
                           function(t)
                           sum(t / base^seq.int(0, length.out =
                                                length(t)))))
    structure(ifelse(lens > 0L, x, NA_real_),
              base = base, lens = lens, .classes = classes)
}

## <NOTE>
## Currently unused.
## Is there any point in having a 'base' argument?
## </NOTE>
.decode_numeric_version <-
function(x, base = NULL)
{
    if(is.null(base)) base <- attr(x, "base")
    if(!is.numeric(base)) stop("wrong argument")
    lens <- attr(x, "lens")
    y <- vector("list", length = length(x))
    for(i in seq_along(x)) {
        n <- lens[i]
        encoded <- x[i]
        decoded <- integer(n)
        for(k in seq_len(n)) {
            decoded[k] <- encoded %/% 1
            encoded <- base * (encoded %% 1)
        }
        y[[i]] <- as.integer(decoded)
    }
    class(y) <- unique(c(attr(x, ".classes"), "numeric_version"))
    y
}

## Methods.

`[.numeric_version` <-
function(x, i, j)
{
    y <- if(missing(j))
        unclass(x)[i]
    else
        lapply(unclass(x)[i], "[", j)
    ## Change sequences which are NULL or contains NAs to integer().
    bad <- as.logical(sapply(y,
                             function(t) is.null(t) || any(is.na(t))))
    if(any(bad))
        y[bad] <- rep.int(list(integer()), length(bad))
    class(y) <- class(x)
    y
}

`[[.numeric_version` <-
function(x, ..., exact = NA)
{
   if(length(list(...)) < 2L)
      structure(list(unclass(x)[[..., exact=exact]]), class = oldClass(x))
   else
      unclass(x)[[..1, exact=exact]][..2]
}

## allowed forms
## x[[i]] <- "1.2.3"; x[[i]] <- 1L:3L; x[[c(i,j)]] <- <single integer>
## x[[i,j]] <- <single integer>
`[[<-.numeric_version` <-
function(x, ..., value)
{
   z <- unclass(x)
   if(nargs() < 4L) {
       if(length(..1) < 2L) {
           if(is.character(value) && length(value) == 1L)
               value <- unclass(as.numeric_version(value))[[1L]]
           else if(!is.integer(value)) stop("invalid value")
       } else {
           value <- as.integer(value)
           if(length(value) != 1L) stop("invalid value")
       }
       z[[..1]] <- value
   } else {
       value <- as.integer(value)
       if(length(value) != 1L) stop("invalid value")
       z[[..1]][..2] <- value
   }
   structure(z, class = oldClass(x))
}


Ops.numeric_version <-
function(e1, e2)
{
    if(nargs() == 1L)
        stop(gettextf("unary '%s' not defined for \"numeric_version\" objects",
                      .Generic), domain = NA)
    boolean <- switch(.Generic, "<" = , ">" = , "==" = , "!=" = ,
        "<=" = , ">=" = TRUE, FALSE)
    if(!boolean)
        stop(gettextf("'%s' not defined for \"numeric_version\" objects",
                      .Generic), domain = NA)
    if(!is.numeric_version(e1)) e1 <- as.numeric_version(e1)
    if(!is.numeric_version(e2)) e2 <- as.numeric_version(e2)
    base <- max(unlist(e1), unlist(e2), 0) + 1
    e1 <- .encode_numeric_version(e1, base = base)
    e2 <- .encode_numeric_version(e2, base = base)
    NextMethod(.Generic)
}

Summary.numeric_version <-
function(..., na.rm)
{
    ok <- switch(.Generic, max = , min = , range = TRUE, FALSE)
    if(!ok)
        stop(.Generic, " not defined for numeric_version objects")
    x <- list(...)
    x <- do.call("c", lapply(x, as.numeric_version))
    v <- .encode_numeric_version(x)
    if(!na.rm && length(pos <- which(is.na(v)))) {
        y <- x[pos[1L]]
        if(as.character(.Generic) == "range")
            c(y, y)
        else
            y
    }
    else
        switch(.Generic,
               max = x[which.max(v)],
               min = x[which.min(v)],
               range = x[c(which.min(v), which.max(v))])
}

as.character.numeric_version <-
function(x, ...)
    ifelse(as.numeric(sapply(x, length)) > 0,
           as.character(unlist(lapply(x, paste, collapse = "."))),
           NA_character_)

as.data.frame.numeric_version <- as.data.frame.vector

as.list.numeric_version <-
function(x, ...)
    unclass(x)

c.numeric_version <-
function(..., recursive = FALSE)
{
    x <- lapply(list(...), as.numeric_version)
    ## Try to preserve common extension classes.
    ## Note that this does not attempt to turn character strings into
    ## *package* versions if possible.
    classes <- if(length(unique(lapply(x, class))) == 1L)
        class(x[[1L]])
    else
        "numeric_version"
    structure(unlist(x, recursive = FALSE), class = classes)
}

duplicated.numeric_version <-
function(x, incomparables = FALSE, ...)
{
    x <- .encode_numeric_version(x)
    NextMethod("duplicated")
}

is.na.numeric_version <-
function(x)
    is.na(.encode_numeric_version(x))

print.numeric_version <-
function(x, ...)
{
    y <- as.character(x)
    print(noquote(ifelse(is.na(y), NA_character_, sQuote(y))), ...)
    invisible(x)
}

rep.numeric_version <-
function(x, ...)
    structure(NextMethod("rep"), class = oldClass(x))

## not needed: duplicates xtfrm method
## sort.numeric_version <-
## function(x, decreasing = FALSE, na.last = NA, ...)
##     x[order(.encode_numeric_version(x),
##             na.last = na.last, decreasing = decreasing)]

unique.numeric_version <-
function(x, incomparables = FALSE, ...)
    x[!duplicated(x, incomparables, ...)]

xtfrm.numeric_version <-
function(x)
    .encode_numeric_version(x)

## <NOTE>
## Versions of R prior to 2.6.0 had only a package_version class.
## We now have package_version extend numeric_version.
## We only provide named subscripting for package versions.
## </NOTE>

`$.package_version` <-
function(x, name)
{
    name <- pmatch(name, c("major", "minor", "patchlevel"))
    switch(name,
           major = as.integer(sapply(x, "[", 1L)),
           minor = as.integer(sapply(x, "[", 2L)),
           patchlevel = as.integer(sapply(x, "[", 3L)))
    ## <NOTE>
    ## Older versions used
    ## patchlevel = {
    ##   as.integer(sapply(x,
    ##                     function(s) s[min(3, length(s))]))
    ## }
    ## apparently with the idea to always use the last component as the
    ## patchlevel ...
    ## </NOTE>
}
#  File src/library/base/R/warnings.R
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

warnings <- function(...)
{
    if(!exists("last.warning", envir=baseenv())) return()
    last.warning <- get("last.warning", envir=baseenv())
    if(!(n <- length(last.warning))) return()
    structure(last.warning, dots=list(...), class="warnings")
}

print.warnings <- function(x, ...)
{
    if(n <- length(x)) {
        cat(ngettext(n, "Warning message:\n", "Warning messages:\n"))
        msgs <- names(x)
        for(i in seq_len(n)) {
            ind <- if(n == 1L) "" else paste(i, ": ", sep="")
            out <- if(length(x[[i]])) {
                ## deparse can overshoot cutoff
                temp <- deparse(x[[i]], width.cutoff = 50L, nlines = 2L)
                ## Put on one line if narrow enough.
                sm <- strsplit(msgs[i], "\n")[[1L]]
                nl <- if(nchar(ind, "w") + nchar(temp[1L], "w") +
                         nchar(sm[1L], "w") <= 75L)
                    " " else "\n  "
                paste(ind, "In ",
                      temp[1L], if(length(temp) > 1L) " ...",
                      " :", nl, msgs[i], sep = "")
            } else paste(ind, msgs[i], sep = "")
            do.call("cat", c(list(out), attr(x, "dots"), fill=TRUE))
        }
    }
    invisible(x)
}
#  File src/library/base/R/which.R
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

which <- function(x, arr.ind = FALSE)
{
    if(!is.logical(x))
	stop("argument to 'which' is not logical")
    wh <- seq_along(x)[x & !is.na(x)]
    dl <- dim(x)
    if (is.null(dl) || !arr.ind) {
	names(wh) <- names(x)[wh]
    }
    else { ##-- return a matrix  length(wh) x rank
        m <- length(wh)
        rank <- length(dl)
        wh1 <- wh - 1L
        wh <- 1L + wh1 %% dl[1L]
        wh <- matrix(wh, nrow = m, ncol = rank,
                     dimnames =
                     list(dimnames(x)[[1L]][wh],
                          if(rank == 2L) c("row", "col")# for matrices
                          else paste("dim", seq_len(rank), sep="")))
        if(rank >= 2L) {
            denom <- 1L
            for (i in 2L:rank) {
                denom <- denom * dl[i-1L]
                nextd1 <- wh1 %/% denom # (next dim of elements) - 1
                wh[,i] <- 1L + nextd1 %% dl[i]
            }
        }
        storage.mode(wh) <- "integer"
    }
    wh
}

which.min <- function(x) .Internal(which.min(x))
which.max <- function(x) .Internal(which.max(x))
#  File src/library/utils/R/withVisible.R
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

withVisible <- function(x) .Internal(withVisible(x))
#  File src/library/base/R/write.R
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

write <- function(x, file = "data", ncolumns = if(is.character(x)) 1 else 5,
                  append = FALSE, sep = " ")
    cat(x, file = file, sep = c(rep.int(sep, ncolumns-1), "\n"),
        append = append)
#  File src/library/base/R/xor.R
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

xor <- function(x, y) { (x | y) & !(x & y) }
#  File src/library/base/R/zapsmall.R
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

zapsmall <- function(x, digits = getOption("digits"))
{
    if (length(digits) == 0)
        stop("invalid 'digits'")
    if (all(ina <- is.na(x)))
        return(x)
    mx <- max(abs(x[!ina]))
    round(x, digits = if(mx > 0) max(0L, digits - log10(mx)) else digits)
}
#  File src/library/base/R/zdatetime.R
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

## needs to run after paste()
.leap.seconds <- local({
    .leap.seconds <-
        c("1972-6-30", "1972-12-31", "1973-12-31", "1974-12-31",
          "1975-12-31", "1976-12-31", "1977-12-31", "1978-12-31",
          "1979-12-31", "1981-6-30", "1982-6-30", "1983-6-30",
          "1985-6-30", "1987-12-31", "1989-12-31", "1990-12-31",
          "1992-6-30", "1993-6-30", "1994-6-30","1995-12-31",
          "1997-6-30", "1998-12-31", "2005-12-31", "2008-12-31")
    .leap.seconds <- strptime(paste(.leap.seconds , "23:59:60"),
                              "%Y-%m-%d %H:%M:%S")
    c(as.POSIXct(.leap.seconds, "GMT")) # lose the timezone
})
#  File src/library/base/R/zdynvars.R
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

## Need to ensure this comes late enough ...
## Perhaps even merge it into the common profile?

.dynLibs <- local({
    ## <NOTE>
    ## Versions of R prior to 1.4.0 had .Dyn.libs in .AutoloadEnv
    ## (and did not always ensure getting it from there).
    ## Until 1.6.0, we consistently used the base environment.
    ## Now we have a dynamic variable instead.
    ## </NOTE>
    .Dyn.libs <- structure(list(), class = "DLLInfoList")
    function(new) {
        if(!missing(new)) {
            class(new) <- "DLLInfoList"
            .Dyn.libs <<- new
        }
        else
            .Dyn.libs
    }
})

.libPaths <- local({
    .lib.loc <- character(0L)            # Profiles need to set this.
    function(new) {
        if(!missing(new)) {
            new <- Sys.glob(path.expand(new))
            paths <- unique(path.expand(c(new, .Library.site, .Library)))
            .lib.loc <<- paths[file.info(paths)$isdir %in% TRUE]
        }
        else
            .lib.loc
    }
})
#  File src/library/base/R/zzz.R
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

## top-level assignments that need to be copied to baseloader.R
as.numeric <- as.real <- as.double
is.name <- is.symbol


## extracted from existing NAMESPACE files in Dec 2003
.knownS3Generics <- local({

    ## include the S3 group generics here
    baseGenerics <- c("Math", "Ops", "Summary", "Complex",
        "as.character", "as.data.frame", "as.matrix", "as.vector",
        "cbind", "labels", "print", "rbind", "rep", "seq", "seq.int",
        "solve", "summary", "t")

    utilsGenerics <- c("edit", "str")

    graphicsGenerics <- c("contour", "hist", "identify", "image",
        "lines", "pairs", "plot", "points", "text")

    statsGenerics <- c( "add1", "AIC", "anova", "biplot", "coef",
        "confint", "deviance", "df.residual", "drop1", "extractAIC",
        "fitted", "formula", "logLik", "model.frame", "model.matrix",
        "predict", "profile", "qqnorm", "residuals", "se.contrast",
        "terms", "update", "vcov")

    tmp <- rep.int(c("base", "utils", "graphics", "stats"),
                   c(length(baseGenerics), length(utilsGenerics),
                     length(graphicsGenerics), length(statsGenerics)))
    names(tmp) <-
        c(baseGenerics, utilsGenerics, graphicsGenerics, statsGenerics)
    tmp
})

.ArgsEnv <- new.env(hash = TRUE, parent = emptyenv())

assign("%*%", function(x, y) NULL, envir = .ArgsEnv)
assign(".C", function(name, ..., NAOK = FALSE, DUP = TRUE, PACKAGE,
                      ENCODING) NULL,
       envir = .ArgsEnv)
assign(".Fortran", function(name, ..., NAOK = FALSE, DUP = TRUE, PACKAGE,
                            ENCODING) NULL,
       envir = .ArgsEnv)
assign(".Call", function(name, ..., PACKAGE) NULL, envir = .ArgsEnv)
assign(".Call.graphics", function(name, ..., PACKAGE) NULL, envir = .ArgsEnv)
assign(".External", function(name, ..., PACKAGE) NULL, envir = .ArgsEnv)
assign(".External.graphics", function(name, ..., PACKAGE) NULL,
       envir = .ArgsEnv)
assign(".Internal", function(call) NULL, envir = .ArgsEnv)
assign(".Primitive", function(name) NULL, envir = .ArgsEnv)
assign(".primTrace", function(obj) NULL, envir = .ArgsEnv)
assign(".primUntrace", function(obj) NULL, envir = .ArgsEnv)
assign(".subset", function(x, ...) NULL, envir = .ArgsEnv)
assign(".subset2", function(x, ...) NULL, envir = .ArgsEnv)
assign("as.call", function(x) NULL, envir = .ArgsEnv)
assign("as.environment", function(object) NULL, envir = .ArgsEnv)
assign("attr", function(x, which, exact = FALSE) NULL, envir = .ArgsEnv)
assign("attr<-", function(x, which, value) NULL, envir = .ArgsEnv)
assign("attributes", function(obj) NULL, envir = .ArgsEnv)
assign("attributes<-", function(obj, value) NULL, envir = .ArgsEnv)
assign("baseenv", function() NULL, envir = .ArgsEnv)
assign("browser", function(text="", condition=NULL, expr = TRUE) NULL, 
       envir = .ArgsEnv)
assign("call", function(name, ...) NULL, envir = .ArgsEnv)
assign("class", function(x) NULL, envir = .ArgsEnv)
assign("class<-", function(x, value) NULL, envir = .ArgsEnv)
assign("emptyenv", function() NULL, envir = .ArgsEnv)
assign("environment<-", function(fun, value) NULL, envir = .ArgsEnv)
assign("expression", function(...) NULL, envir = .ArgsEnv)
assign("gc.time", function(on = TRUE) NULL, envir = .ArgsEnv)
assign("globalenv", function() NULL, envir = .ArgsEnv)
assign("interactive", function() NULL, envir = .ArgsEnv)
assign("invisible", function(x) NULL, envir = .ArgsEnv)
assign("is.atomic", function(x) NULL, envir = .ArgsEnv)
assign("is.call", function(x) NULL, envir = .ArgsEnv)
assign("is.character", function(x) NULL, envir = .ArgsEnv)
assign("is.complex", function(x) NULL, envir = .ArgsEnv)
assign("is.double", function(x) NULL, envir = .ArgsEnv)
assign("is.environment", function(x) NULL, envir = .ArgsEnv)
assign("is.expression", function(x) NULL, envir = .ArgsEnv)
assign("is.function", function(x) NULL, envir = .ArgsEnv)
assign("is.integer", function(x) NULL, envir = .ArgsEnv)
assign("is.language", function(x) NULL, envir = .ArgsEnv)
assign("is.list", function(x) NULL, envir = .ArgsEnv)
assign("is.logical", function(x) NULL, envir = .ArgsEnv)
assign("is.name", function(x) NULL, envir = .ArgsEnv)
assign("is.null", function(x) NULL, envir = .ArgsEnv)
assign("is.object", function(x) NULL, envir = .ArgsEnv)
assign("is.pairlist", function(x) NULL, envir = .ArgsEnv)
assign("is.raw", function(x) NULL, envir = .ArgsEnv)
assign("is.real", function(x) NULL, envir = .ArgsEnv)
assign("is.recursive", function(x) NULL, envir = .ArgsEnv)
assign("is.single", function(x) NULL, envir = .ArgsEnv)
assign("is.symbol", function(x) NULL, envir = .ArgsEnv)
assign("list", function(...) NULL, envir = .ArgsEnv)
assign("lazyLoadDBfetch", function(key, file, compressed, hook) NULL,
       envir = .ArgsEnv)
assign("missing", function(x) NULL, envir = .ArgsEnv)
assign("nargs", function() NULL, envir = .ArgsEnv)
assign("nzchar", function(x) NULL, envir = .ArgsEnv)
assign("oldClass", function(x) NULL, envir = .ArgsEnv)
assign("oldClass<-", function(x, value) NULL, envir = .ArgsEnv)
assign("on.exit", function(expr, add = FALSE) NULL, envir = .ArgsEnv)
assign("pos.to.env", function(x) NULL, envir = .ArgsEnv)
assign("proc.time", function() NULL, envir = .ArgsEnv)
assign("quote", function(expr) NULL, envir = .ArgsEnv)
assign("retracemem", function(x, previous = NULL) NULL, envir = .ArgsEnv)
assign("seq_along", function(along.with) NULL, envir = .ArgsEnv)
assign("seq_len", function(length.out) NULL, envir = .ArgsEnv)
assign("standardGeneric", function(f) NULL, envir = .ArgsEnv)
assign("storage.mode<-", function(x, value) NULL, envir = .ArgsEnv)
assign("substitute", function(expr, env) NULL, envir = .ArgsEnv)
assign("tracemem", function(x) NULL, envir = .ArgsEnv)
assign("unclass", function(x) NULL, envir = .ArgsEnv)
assign("untracemem", function(x) NULL, envir = .ArgsEnv)
assign("UseMethod", function(generic, object) NULL, envir = .ArgsEnv)


.S3PrimitiveGenerics <-
    c("as.character", "as.complex", "as.double", "as.integer",
    "as.logical", "as.numeric", "as.raw", "as.real", "c", "dim",
    "dim<-", "dimnames", "dimnames<-", "is.array", "is.finite",
    "is.infinite", "is.matrix",
    "is.na", "is.nan", "is.numeric", "length", "length<-", "levels<-",
    "names", "names<-", "rep", "seq.int")

.GenericArgsEnv <- local({
    env <- new.env(hash = TRUE, parent = emptyenv())
    for(f in .S3PrimitiveGenerics) {
        fx <- function(x) {}
        body(fx) <- substitute(UseMethod(ff), list(ff=f))
        environment(fx) <- .BaseNamespaceEnv
        assign(f, fx, envir = env)
    }
    ## now add the group generics
    ## round, signif, log, trunc are handled below
    fx <- function(x) {}
    for(f in c("abs", "sign", "sqrt", "floor", "ceiling",
               "exp", "expm1", "log1p", "log10", "log2",
               "cos", "sin", "tan", "acos", "asin", "atan", "cosh", "sinh",
               "tanh", "acosh", "asinh", "atanh",
               "gamma", "lgamma", "digamma", "trigamma",
               "cumsum", "cumprod", "cummax", "cummin")) {
        body(fx) <- substitute(UseMethod(ff), list(ff=f))
        environment(fx) <- .BaseNamespaceEnv
        assign(f, fx, envir = env)
    }

    ## ! is unary and handled below
    fx <- function(e1, e2) {}
    for(f in c("+", "-", "*", "/", "^", "%%", "%/%", "&", "|",
               "==", "!=", "<", "<=", ">=", ">")) {
        body(fx) <- substitute(UseMethod(ff), list(ff=f))
        environment(fx) <- .BaseNamespaceEnv
        assign(f, fx, envir = env)
    }

    for(f in c("all", "any", "sum", "prod", "max", "min", "range")) {
        fx <- function(..., na.rm = FALSE) {}
        body(fx) <- substitute(UseMethod(ff), list(ff=f))
        environment(fx) <- .BaseNamespaceEnv
        assign(f, fx, envir = env)
    }

    for(f in c("Arg", "Conj", "Im", "Mod", "Re")) {
        fx <- function(z) {}
        body(fx) <- substitute(UseMethod(ff), list(ff=f))
        environment(fx) <- .BaseNamespaceEnv
        assign(f, fx, envir = env)
    }
    env
})
### do these outside to get the base namespace as the environment.
assign("!", function(x) UseMethod("!"), envir = .GenericArgsEnv)
assign("as.character", function(x, ...) UseMethod("as.character"),
       envir = .GenericArgsEnv)
assign("as.complex", function(x, ...) UseMethod("as.complex"),
       envir = .GenericArgsEnv)
assign("as.double", function(x, ...) UseMethod("as.double"),
       envir = .GenericArgsEnv)
assign("as.integer", function(x, ...) UseMethod("as.integer"),
       envir = .GenericArgsEnv)
assign("as.logical", function(x, ...) UseMethod("as.logical"),
       envir = .GenericArgsEnv)
assign("as.raw", function(x) UseMethod("as.raw"), envir = .GenericArgsEnv)
assign("c", function(..., recursive = FALSE) UseMethod("c"),
       envir = .GenericArgsEnv)
assign("dimnames", function(x) UseMethod("dimnames"), envir = .GenericArgsEnv)
assign("dim<-", function(x, value) UseMethod("dim<-"), envir = .GenericArgsEnv)
assign("dimnames<-", function(x, value) UseMethod("dimnames<-"),
       envir = .GenericArgsEnv)
assign("length<-", function(x, value) UseMethod("length<-"),
       envir = .GenericArgsEnv)
assign("levels<-", function(x, value) UseMethod("levels<-"),
       envir = .GenericArgsEnv)
assign("log", function(x, base=exp(1)) UseMethod("log"),
       envir = .GenericArgsEnv)
assign("names<-", function(x, value) UseMethod("names<-"),
       envir = .GenericArgsEnv)
assign("rep", function(x, ...) UseMethod("rep"), envir = .GenericArgsEnv)
assign("round", function(x, digits=0) UseMethod("round"),
       envir = .GenericArgsEnv)
assign("seq.int", function(from, to, by, length.out, along.with, ...)
       UseMethod("seq.int"), envir = .GenericArgsEnv)
assign("signif", function(x, digits=6) UseMethod("signif"),
       envir = .GenericArgsEnv)
assign("trunc", function(x, ...) UseMethod("trunc"), envir = .GenericArgsEnv)

## make these the same object as as.double
assign("as.numeric", get("as.double", envir = .GenericArgsEnv),
       envir = .GenericArgsEnv)
assign("as.real", get("as.double", envir = .GenericArgsEnv),
       envir = .GenericArgsEnv)
