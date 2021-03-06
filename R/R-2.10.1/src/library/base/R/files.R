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
