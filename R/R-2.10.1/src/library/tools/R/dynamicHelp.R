#  File src/library/tools/R/dynamicHelp.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/


## This may be asked for
##  R.css, favicon.ico
##  searches with path = "/doc/html/Search"
##  documentation with path = "/doc/....", possibly updated under tempdir()/.R
##  html help, either by topic, /library/<pkg>/help/<topic> (pkg=NULL means any)
##             or by file, /library/<pkg>/html/<file>.html
httpd <- function(path, query, ...)
{
    .httpdHeader <- function(title)
    {
        paste('<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">\n',
              '<html><head><title>R: ', title , '</title>\n',
              '<meta http-equiv="Content-Type" content="text/html; charset="UTF-8">\n',
              '<link rel="stylesheet" type="text/css" href="/doc/html/R.css">\n',
              '</head><body>\n',
              sep = "")
    }

    .HTMLdirListing <- function(dir, base)
    {
        files <- list.files(dir)    # note, no hidden files are listed
        out <- c(.httpdHeader(dir),
                 paste('<h1>', "Listing of directory<br>", dir, '</h1>\n\n<hr>\n',
                       sep = ""))
        if(!length(files))
            out <- c(out, gettext("No files in this directory"))
        else {
            urls <- paste('<a href="', base, '/', files, '">', files, '</a>',
                          sep = "")
            out <- c(out, "<dl>",
                     paste("<dd>", mono(iconv(urls, "", "UTF-8")), "</dd>", sep = ""),
                     "</dl>")
        }
        out <- c(out, "<hr>\n</body></html>")
        list(payload = paste(out, collapse="\n"))
    }

    .HTMLsearch <- function(query)
    {
        res <- if(identical(names(query), "category"))
            help.search(keyword = query, verbose = 1L, use_UTF8 = TRUE)$matches
        else {
            nm <- names(query)
            m <- match("exact", nm)
            if(is.na(m)) help.search(query[1L], nm, verbose = 1L, use_UTF8 = TRUE)$matches
            else help.search(query[1L], nm[-m], agrep = FALSE, use_UTF8 = TRUE)$matches
        }
        title <- "Search Results"
        out <- c(.httpdHeader(title),
                 paste('<h1>', title, '</h1>\n',
                       'The search string was <b>"', query[1L], '"</b><hr>\n',
                       sep=""))
        if(nrow(res)) {
            paths <- paste("/library/", res[, "Package"], "/html/",
                           res[, "topic"], ".html", sep = "")
            urls <- paste('<a href="', paths, '">',
                          res[, "Package"], "::", res[, "topic"],
                          '</a>', sep = "")
            out <- c(out, "<dl>",
                     paste("<dt>", urls, "</dt>\n",
                           "<dd>", res[, "title"], "</dd>", sep = ""),
                     "</dl>")
        } else out <- c(out, gettext("No results found"))
        out <- c(out, "<hr>\n</body></html>")
        list(payload = paste(out, collapse="\n"))
    }

    unfix <- function(file)
    {
        ## we need to re-fix links altered by fixup.package.URLs
        ## in R < 2.10.0
        fixedfile <- sub("/html/.*", "/fixedHTMLlinks", file)
        if(file.exists(fixedfile)) {
            top <- readLines(fixedfile)
            lines <- readLines(file)
            lines <- gsub(paste(top, "library", sep="/"),
                          "../../", lines, fixed = TRUE)
            lines <- gsub(paste(top, "doc/", sep = "/"),
                          "../../../doc/", lines, fixed = TRUE)
            return(list(payload=paste(lines, collapse="\n")))
        }
        list(file = file)
    }

    mime_type <- function(path)
    {
        ext <- strsplit(path, ".", fixed = TRUE)[[1L]]
        if(n <- length(ext)) ext <- ext[n] else ""
        switch(ext,
               "css" = "text/css",
               "gif" = "image/gif", # in R2HTML
               "jpg" = "image/jpeg",
               "html" = "text/html",
               "pdf" = "application/pdf",
               "eps" =,
               "ps" = "application/postscript", # in GLMMGibbs, mclust
               "sgml"= "text/sgml", # in RGtk2
               "xml" = "text/xml",  # in RCurl
               "text/plain")
    }

    sQuote <- function(text)
        paste("&lsquo;", text, "&rsquo;", sep="")
    mono <- function(text)
        paste('<span class="samp">', text, "</span>", sep="")

    error_page <- function(msg)
        list(payload =
             paste(.httpdHeader("httpd error"),
                   msg,
                   "\n</body></html>",
                   sep = ""))

    if (grepl("R\\.css$", path))
        return(list(file = file.path(R.home("doc"), "html", "R.css")))
    else if(path == "/favicon.ico")
        return(list(file = file.path(R.home("doc"), "html", "favicon.ico")))
    else if(!grepl("^/(doc|library)/", path))
        return(error_page(paste("Only URLs under", mono("/doc"),
                                "and", mono("/library"), "are allowed")))

    ## ----------------------- per-package documentation ---------------------
    ## seems we got ../..//<pkg> in the past
    fileRegexp <- "^/library/+([^/]*)/html/([^/]*)\\.html$"
    topicRegexp <- "^/library/+([^/]*)/help/([^/]*)$"
    docRegexp <- "^/library/([^/]*)/doc(.*)"
    file <- NULL
    if (grepl(topicRegexp, path)) {
        ## ----------------------- package help by topic ---------------------
    	pkg <- sub(topicRegexp, "\\1", path)
    	if (pkg == "NULL") pkg <- NULL  # how can this occur?
    	topic <- sub(topicRegexp, "\\2", path)
        ## if a package is specified, look there first, then everywhere
    	if (!is.null(pkg)) # () avoids deparse here
    	    file <- help(topic, package = (pkg), help_type = "text")
    	if (!length(file))
            file <- help(topic, help_type = "text", try.all.packages = TRUE)
	if (!length(file)) {
            msg <- gettextf("No help found for topic %s in any package.",
                            mono(topic))
	    return(list(payload = error_page(msg)))
	} else if (length(file) == 1L) {
	    path <- dirname(dirname(file))
	    file <- paste('../../', basename(path), '/html/',
                          basename(file), '.html', sep='')
            ## cat("redirect to", file, "\n")
            ## We need to do this because there are static HTML pages
            ## with links to "<file>.html" for topics in the same
            ## package, and if we served one of such a page as a link from
            ## a different package those links on the page would not work.
	    return(list(payload = paste('Redirect to <a href="', file, '">"',
                        basename(file), '"</a>', sep=''),
	    		"content-type" = 'text/html',
	    		header = paste('Location: ', file, sep=''),
	    		"status code" = 302L)) # temporary redirect
	} else if (length(file) > 1L) {
            paths <- dirname(dirname(file))
            fp <- file.path(paths, "Meta", "Rd.rds")
            tp <- basename(file)
            titles <- tp
            for (i in seq_along(fp)) {
                tmp <- try(.readRDS(fp[i]))
                titles[i] <- if(inherits(tmp, "try-error"))
                    "unknown title" else
                    tmp[file_path_sans_ext(tmp$File) == tp[i], "Title"]
            }
            packages <- paste('<dt><a href="../../', basename(paths), '/html/',
                              basename(file), '.html">', titles,
                              '</a></dt><dd> (in package <a href="../../',
                              basename(paths),
                              '/html/00Index.html">', basename(paths),
                              '</a> in library ', dirname(paths), ")</dd>",
                              sep="", collapse="\n")

            return(list(payload =
                        paste("<p>",
                              gettextf("Help on topic '%s' was found in the following packages:", topic),
                              "</p><dl>\n",
                              packages, "</dl>", sep="", collapse="\n")
                        ))
        }
    } else if (grepl(fileRegexp, path)) {
        ## ----------------------- package help by file ---------------------
    	pkg <- sub(fileRegexp, "\\1", path)
    	h0 <- helpdoc <- sub(fileRegexp, "\\2", path)
        if (helpdoc == "00Index") {
            ## ------------------- package listing ---------------------
            file <- system.file("html", "00Index.html", package = pkg)
            if(!nzchar(file) || !file.exists(file)) {
                msg <- if(nzchar(system.file(package = pkg)))
                    gettextf("No package index found for package %s",
                             mono(pkg))
                else
                    gettextf("No package named %s could be found",
                             mono(pkg))
                return(error_page(msg))
            } else {
                if(.Platform$OS.type == "windows") return(unfix(file))
                return(list(file = file))
            }
    	}
        ## ----------------------- package help file ---------------------
        path <- system.file("help", package = pkg)
        if (!nzchar(path)) {
            msg <- if(nzchar(system.file(package = pkg)))
                gettextf("No help found for package %s", mono(pkg) )
            else
                gettextf("No package named %s could be found", mono(pkg))
            return(error_page(msg))
        }
        ## if 'topic' is not a help doc, try it as an alias in the package
        contents <- .readRDS(sub("/help", "/Meta/Rd.rds", path, fixed = TRUE))
        files <- sub("\\.[Rr]d$", "", contents$File)
        if(! helpdoc %in% files) {
            ## or call help()
            aliases <- contents$Aliases
            lens <- sapply(aliases, length)
            aliases <- structure(rep.int(contents$File, lens),
                                 names = unlist(aliases))
            tmp <- sub("\\.[Rr]d$", "", aliases[helpdoc])
            if(is.na(tmp)) {
                msg <- gettextf("Link %s in package %s could not be located",
                                mono(helpdoc), mono(pkg))
                files <- help(helpdoc, help_type = "text",
                              try.all.packages = TRUE)
                if (length(files)) {
                    path <- dirname(dirname(files))
                    files <- paste('/library/', basename(path), '/html/',
                                   basename(files), '.html', sep='')
                    msg <- c(msg, "<br>",
                             "However, you might be looking for one of",
                             "<p></p>",
                             paste('<p><a href="', files, '">',
                                   mono(files), "</a></p>", sep="")
                             )
                }
                return(error_page(paste(msg, collapse ="\n")))
            }
            helpdoc <- tmp
        }

        ## Now we know which document we want in which package
        ## It might be prebuilt, but we prefer to generate a dynamic version

	dirpath <- dirname(path)
	pkgname <- basename(dirpath)
	RdDB <- file.path(path, pkgname)
	if(file.exists(paste(RdDB, "rdx", sep="."))) {
	    outfile <- tempfile("Rhttpd")
	    temp <- Rd2HTML(tools:::fetchRdDB(RdDB, helpdoc),
                            out = outfile, package = dirpath,
                            dynamic = TRUE)
	    on.exit(unlink(outfile))
	    return(list(payload = paste(readLines(temp), collapse = "\n")))
	} else {
            ## Try for pre-generated HTML.
            file <- paste(file.path(dirpath, "html", helpdoc),
                          "html", sep = ".")
            if(file.exists(file)) {
                if(.Platform$OS.type == "windows") return(unfix(file))
                return(list(file = file))
            }
            ## we probably should not get here
            msg <- gettextf("Unable to locate help document %s in package %s",
                           mono(h0), mono(pkg))
            return(error_page(msg))
        }
    } else if (grepl(docRegexp, path)) {
        ## ----------------------- package doc directory ---------------------
    	pkg <- sub(docRegexp, "\\1", path)
    	rest <- sub(docRegexp, "\\2", path)
        docdir <- system.file("doc", package = pkg)
        if(!nzchar(docdir))
            return(error_page(gettextf("No docs found for package %s",
                                       mono(pkg))))
        if(nzchar(rest) && rest != "/") {
            ## FIXME should we check existence here?
            file <- paste(docdir, rest, sep = "")
            if(isTRUE(file.info(file)$isdir))
                return(.HTMLdirListing(file,
                                       paste("/library/", pkg, "/doc", rest,
                                             sep = "")))
            else
                return(list(file = file, "content-type" = mime_type(path)))
        } else {
            ## request to list <pkg>/doc
            return(.HTMLdirListing(docdir,
                                   paste("/library", pkg, "doc", sep="/")))
        }
    } else if (grepl("^/library/", path)) {
        descRegexp <- "^/library/+([^/]+)/+DESCRIPTION$"
        if(grepl(descRegexp, path)) {
            pkg <- sub(descRegexp, "\\1", path)
            file <- system.file("DESCRIPTION", package = pkg)
            return(list(file = file, "content-type" = "text/plain"))
        } else
            return(error_page(gettextf("Only help files, %s and files under %s in a package can be viewed", mono("DESCRIPTION"), mono("doc/"))))
    }


    ## ----------------------- R docs ---------------------
    if(path == "/doc/html/Search.html") {
        ## redirect to the page that has search enabled
        list(file = file.path(R.home("doc"), "html/SearchOn.html"))
    } else if(path == "/doc/html/Search") {
        .HTMLsearch(query)
    } else if(path == "/doc/html/packages.html") {
        ## remake as needed
        utils::make.packages.html(temp = TRUE)
        list(file = file.path(tempdir(), ".R", path))
    } else if(grepl("doc/html/.*html$" , path) &&
              file.exists(tmp <- file.path(tempdir(), ".R", path))) {
        ## use updated version, e.g. of packages.html
        list(file = tmp)
    } else {
        if(grepl("^/doc/", path)) {
            ## /doc/AUTHORS and so on.
            file <- file.path(R.home("doc"), sub("^/doc", "", path))
        } else return(error_page(gettextf("unsupported URL %s", mono(path))))
        if(!file.exists(file))
            error_page(gettextf("URL %s was not found", mono(path)))
        else
            list(file = file, "content-type" = mime_type(path))
    }
}

## 0 = untried, < 0 = failed to start,  > 0 = actual port
httpdPort <- 0L

startDynamicHelp <- function(start=TRUE)
{
    env <- environment(startDynamicHelp)
    if(nzchar(Sys.getenv("R_DISABLE_HTTPD"))) {
        unlockBinding("httpdPort", env)
        httpdPort <<- -1L
        lockBinding("httpdPort", env)
        warning("httpd server disabled by R_DISABLE_HTTPD", immediate. = TRUE)
        utils::flush.console()
        return(httpdPort)
    }
    if (start && httpdPort) {
        if(httpdPort > 0) stop("server already running")
        else stop("server could not be started on an earlier attempt")
    }
    if(!start && httpdPort <= 0L)
        stop("no running server to stop")
    unlockBinding("httpdPort", env)
    if (start) {
        message("starting httpd help server ...", appendLF = FALSE)
        utils::flush.console()
        OK <- FALSE
        ports <- getOption("help.ports")
        if (is.null(ports)) {
	    ## Choose 10 random port numbers between 10000 and 32000.
	    ## The random seed might match
	    ## on multiple instances, so add the time as well.  But the
	    ## time may only be accurate to seconds, so rescale it to
	    ## 5 minute units.
            ports <- 10000 + 22000*((stats::runif(10) + unclass(Sys.time())/300) %% 1)
        }
        ports <- as.integer(ports)
        for(i in seq_along(ports)) {
            ## the next can throw an R-level error,
            ## so do not assign port unless it succeeds.
	    status <- .Internal(startHTTPD("127.0.0.1", ports[i]))
	    if (status == 0L) {
                OK <- TRUE
                httpdPort <<- ports[i]
                break
            }
            if (status != -2L) break
            ## so status was -2, which means port in use
	}
        if (OK) {
            message(" done")
            utils::flush.console()
            ## FIXME: actually test the server
        } else {
            warning("failed to start the httpd server", immediate. = TRUE)
            utils::flush.console()
            httpdPort <<- -1L
        }
    } else {
        ## Not really tested
        .Internal(stopHTTPD())
    	httpdPort <<- 0L
    }
    lockBinding("httpdPort", env)
    invisible(httpdPort)
}
