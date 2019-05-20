#  File src/library/utils/R/RShowDoc.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

RShowDoc <- function(what, type=c("pdf", "html", "txt"), package)
{
    paste. <- function(x, ext) paste(x, ext, sep=".")
    pdf_viewer <- function(path) {
        if(.Platform$OS.type == "windows") shell.exec(path)
        else system(paste(shQuote(getOption("pdfviewer")), shQuote(path)),
                    wait = FALSE)
    }

    html_viewer <- function(path) {
        ## we don't use browseURL under Windows as shell.exec does
        ## not want an encoded URL.
        if(.Platform$OS.type == "windows") shell.exec(chartr("/", "\\", path))
        else browseURL(paste("file://", URLencode(path), sep=""))
    }

    type <- match.arg(type)
    if(missing(what) || length(what) != 1L || !is.character(what)) {
        message("   RShowDoc() should be used with a character string argument specifying\n   a documentation file")
        return(invisible())
    }
    if(!missing(package)) {
        pkgpath <- .find.package(package)
        if(type == "pdf") {
            path <- file.path(pkgpath, "doc", paste.(what, "pdf"))
            if(file.exists(path)) {
                pdf_viewer(path)
                return(invisible(path))
            }
            path <- file.path(pkgpath, paste.(what, "pdf"))
            if(file.exists(path)) {
                pdf_viewer(path)
                return(invisible(path))
            }
            type <- "html"
        }
        if(type == "html") {
            path <- file.path(pkgpath, "doc", paste.(what, "html"))
            if(file.exists(path)) {
                html_viewer(path)
                return(invisible(path))
            }
            path <- file.path(pkgpath, paste.(what, "html"))
            if(file.exists(path)) {
                html_viewer(path)
                return(invisible(path))
            }
        }
        path <- file.path(pkgpath, "doc", what)
        if(file.exists(path)) {
            file.show(path)
            return(invisible(path))
        }
        path <- file.path(pkgpath, what)
        if(file.exists(path)) {
            file.show(path)
            return(invisible(path))
        }
        stop(gettextf("no documentation for '%s' found in package '%s'",
                      what, package), domain = NA)
    }
    if(what == "FAQ") what <- "R-FAQ"
    if(what == "NEWS") {
        ## This is in UTF-8 and has a BOM on the first line
        path <- file.path(R.home(), what)
        tf <- tempfile()
        tmp <- readLines(path)
        tmp[1] <- ""
        writeLines(tmp, tf)
        file.show(tf, delete.file = TRUE, encoding = "UTF-8")
        return(invisible(path))
    } else if(what == "COPYING") {
        path <- file.path(R.home(), what)
        file.show(path)
        return(invisible(path))
    } else if(what %in% c("R-admin", "R-data", "R-exts", "R-FAQ", "R-intro",
                          "R-ints", "R-lang")) {
        if(type == "pdf") {
            path <- file.path(R.home("doc"), "manual", paste.(what, "pdf"))
            if(file.exists(path)) {
                pdf_viewer(path)
                return(invisible(path))
            }
            type <- "html"
        }
        if(type == "html") {
            path <- file.path(R.home("doc"), "manual", paste.(what, "html"))
            if(file.exists(path)) {
                html_viewer(path)
                return(invisible(path))
            }
        }
        if(what == "R-FAQ" &&
           file.exists(path <- file.path(R.home("doc"), "FAQ"))) {
            file.show(path)
            return(invisible(path))
        }
    } else if(.Platform$OS.type == "windows" && what %in% "rw-FAQ") {
        if(type == "pdf") type <- "html"
        if(type == "html") {
            path <- file.path(R.home("doc"), "html", paste.(what, "html"))
            if(file.exists(path)) {
                shell.exec(chartr("/", "\\", path))
                return(invisible(path))
            }
        }
        path <- file.path(R.home("doc"), what)
        if(file.exists(path)) {
            file.show(path)
            return(invisible(path))
        }
        path <- file.path(R.home(), "src", "gnuwin32", what)
        if(file.exists(path)) {
            file.show(path)
            return(invisible(path))
        }
    } else {
        rdocdir <- R.home("doc")
        docs <- dir(rdocdir, full.names=TRUE)
        docs <- docs[sapply(docs, function(x) file_test("-f", x))]
        m <- match(what, basename(docs), 0L)
        if(m > 0L) {
            file.show(docs[m])
            return(invisible(docs[m]))
        }
    }
    stop("document not found")
}
#  File src/library/utils/R/RSiteSearch.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

RSiteSearch <- function(string, restrict = c("Rhelp08", "functions", "views"),
			format = c("normal", "short"),
			sortby = c("score", "date:late", "date:early",
			"subject", "subject:descending",
			"from", "from:descending", "size", "size:descending"),
			matchesPerPage = 20)
{
    paste0 <- function(...) paste(..., sep = "")
    string <- paste0("http://search.r-project.org/cgi-bin/namazu.cgi?query=",
		     gsub(" ", "+", string))
    mpp <- paste0("max=", matchesPerPage)
    format <- paste0("result=", match.arg(format))

    restrictVALS <- c("Rhelp08", "Rhelp01", "Rhelp02", "functions", "views",
                      "R-devel", "R-sig-mixed-models")
    restr <- match.arg(restrict, choices = restrictVALS, several.ok = TRUE)
    restr <- paste(paste0("idxname=", restr), collapse = "&")

    sortby <- match.arg(sortby)
    sortby <- paste0("sort=",
		     switch(sortby,
			    "score"=, "date:late"=, "date:early" = sortby,
			    "subject"		 = "field:subject:ascending",
			    "subject:descending" = "field:subject:descending",
			    "from"		 = "field:from:ascending",
			    "from:descending"	 = "field:from:descending",
			    "size"		 = "field:size:ascending",
			    "size:descending"	 = "field:size:descending"))

    ## we know this is a http:// URL, so encoding should be safe.
    ## it seems that firefox on Mac OS needs it for {...}
    ## OTOH, Namazu does not decode in, say, sort=date:late.
    qstring <- paste(URLencode(string), mpp, format, sortby, restr, sep = "&")
    browseURL(qstring)
    cat(gettext("A search query has been submitted to"),
	"http://search.r-project.org\n")
    cat(gettext("The results page should open in your browser shortly\n"))
    invisible(qstring)
}
#  File src/library/utils/R/Rprof.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

Rprof <- function(filename = "Rprof.out", append = FALSE, interval =  0.02,
                  memory.profiling = FALSE)
{
    if(is.null(filename)) filename <- ""
    invisible(.Internal(Rprof(filename, append, interval, memory.profiling)))
}

Rprofmem <- function(filename = "Rprofmem.out", append = FALSE, threshold = 0)
{
    if(is.null(filename)) filename <- ""
    invisible(.Internal(Rprofmem(filename, append, as.double(threshold))))
}
#  File src/library/utils/R/Sweave.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

Sweave <- function(file, driver=RweaveLatex(),
                   syntax=getOption("SweaveSyntax"), ...)
{
    if(is.character(driver))
        driver <- get(driver, mode="function")()
    else if(is.function(driver))
        driver <- driver()


    if(is.null(syntax))
        syntax <- SweaveGetSyntax(file)
    if(is.character(syntax))
        syntax <- get(syntax, mode="list")

    drobj <- driver$setup(file=file, syntax=syntax, ...)
    on.exit(driver$finish(drobj, error=TRUE))

    text <- SweaveReadFile(file, syntax)
    syntax <- attr(text, "syntax")

    mode <- "doc"
    chunknr <- 0L
    chunk <- NULL

    namedchunks <- list()
    for(linenum in seq_along(text)) {
    	line <- text[linenum]
        if(length(grep(syntax$doc, line))){
            if(mode=="doc"){
                if(!is.null(chunk))
                    drobj <- driver$writedoc(drobj, chunk)
                mode <- "doc"
            }
            else{
                if(!is.null(chunkopts$label))
                    namedchunks[[chunkopts$label]] <- chunk
                if(!is.null(chunk))
                    drobj <- driver$runcode(drobj, chunk, chunkopts)
                mode <- "doc"
            }
            chunk <- NULL
        }
        else if(length(grep(syntax$code, line))){
            if(mode=="doc"){
                if(!is.null(chunk))
                    drobj <- driver$writedoc(drobj, chunk)
                mode <- "code"
            }
            else{
                if(!is.null(chunkopts$label))
                    namedchunks[[chunkopts$label]] <- chunk
                if(!is.null(chunk))
                    drobj <- driver$runcode(drobj, chunk, chunkopts)
                mode <- "code"
            }
            chunkopts <- sub(syntax$code, "\\1", line)
            chunkopts <- SweaveParseOptions(chunkopts,
                                            drobj$options,
                                            driver$checkopts)
            chunk <- NULL
            chunknr <- chunknr+1
            chunkopts$chunknr <- chunknr
        }
        else{
            if(mode=="code" && length(grep(syntax$coderef, line))){
                chunkref <- sub(syntax$coderef, "\\1", line)
                if(!(chunkref %in% names(namedchunks)))
                    warning(gettextf("reference to unknown chunk '%s'",
                                     chunkref), domain = NA)
                line <- namedchunks[[chunkref]]
            }
            srclines <- c(attr(chunk, "srclines"), rep(linenum, length(line)))
            if(is.null(chunk))
                chunk <- line
            else
                chunk <- c(chunk, line)
            attr(chunk, "srclines") <- srclines
        }
    }
    if(!is.null(chunk)){
        if(mode=="doc") drobj <- driver$writedoc(drobj, chunk)
        else drobj <- driver$runcode(drobj, chunk, chunkopts)
    }

    on.exit()
    driver$finish(drobj)
}

SweaveReadFile <- function(file, syntax)
{
    ## file can be a vector to keep track of recursive calls to
    ## SweaveReadFile.  In this case only the first element is
    ## tried to read in, the rest are forbidden names for further
    ## SweaveInput
    f <- file[1L]

    bf <- basename(f)
    df <- dirname(f)
    if(!file.exists(f)) {
        f <- list.files(df, full.names=TRUE,
                        pattern=paste(bf, syntax$extension, sep=""))

        if(length(f) == 0L)
            stop(gettextf("no Sweave file with name '%s' found", file[1L]),
                 domain = NA)
        else if(length(f) > 1L)
            stop(paste(gettextf("%d Sweave files for basename '%s' found:",
                                length(f), file),
                       paste("\n         ", f, collapse="")),
                 domain = NA)
    }

    ## An incomplete last line is not a real problem.
    text <- readLines(f[1L], warn = FALSE)

    ## <FIXME>
    ## This needs to be more refined eventually ...
    if(any(is.na(nchar(text, "c", TRUE)))) {
        ## Ouch, invalid in the current locale.
        ## (Can only happen in a MBCS locale.)
        ## Try re-encoding from Latin1.
        text <- iconv(text, "latin1", "")
    }
    ## </FIXME>

    pos <- grep(syntax$syntaxname, text)

    if(length(pos) > 1L)
        warning(gettextf("more than one syntax specification found, using the first one"), domain = NA)

    if(length(pos) > 0L) {
        sname <- sub(syntax$syntaxname, "\\1", text[pos[1L]])
        syntax <- get(sname, mode = "list")
        if(class(syntax) != "SweaveSyntax")
            stop(gettextf("object '%s' does not have class \"SweaveSyntax\"",
                          sname), domain = NA)
        text <- text[-pos]
    }

    if(!is.null(syntax$input)) {
        while(length(pos <- grep(syntax$input, text))) {
            pos <- pos[1L]
            ifile <- file.path(df, sub(syntax$input, "\\1", text[pos]))
            if(any(ifile == file)){
                stop(paste(gettextf("recursive Sweave input '%s' in stack",
                                    ifile),
                           paste("\n         ", seq_len(file), ": ",
                                 rev(file), collapse="")),
                 domain = NA)
            }
            itext <- SweaveReadFile(c(ifile, file), syntax)

            if(pos == 1L)
                text <- c(itext, text[-pos])
            else if(pos == length(text))
                text <- c(text[-pos], itext)
            else
                text <- c(text[seq_len(pos-1L)], itext, text[(pos+1L):length(text)])
        }
    }

    attr(text, "syntax") <- syntax
    text
}



###**********************************************************

SweaveSyntaxNoweb <-
    list(doc = "^@",
         code = "^<<(.*)>>=.*",
         coderef = "^<<(.*)>>.*",
         docopt = "^[[:space:]]*\\\\SweaveOpts\\{([^\\}]*)\\}",
         docexpr = "\\\\Sexpr\\{([^\\}]*)\\}",
         extension = "\\.[rsRS]?nw$",
         syntaxname = "^[[:space:]]*\\\\SweaveSyntax\\{([^\\}]*)\\}",
         input = "^[[:space:]]*\\\\SweaveInput\\{([^\\}]*)\\}",
         trans = list(
             doc = "@",
             code = "<<\\1>>=",
             coderef = "<<\\1>>",
             docopt = "\\\\SweaveOpts{\\1}",
             docexpr = "\\\\Sexpr{\\1}",
             extension = ".Snw",
             syntaxname = "\\\\SweaveSyntax{SweaveSyntaxNoweb}",
             input = "\\\\SweaveInput{\\1}")
         )

class(SweaveSyntaxNoweb) <- "SweaveSyntax"

SweaveSyntaxLatex <- SweaveSyntaxNoweb
SweaveSyntaxLatex$doc <-  "^[[:space:]]*\\\\end\\{Scode\\}"
SweaveSyntaxLatex$code <- "^[[:space:]]*\\\\begin\\{Scode\\}\\{?([^\\}]*)\\}?.*"
SweaveSyntaxLatex$coderef <- "^[[:space:]]*\\\\Scoderef\\{([^\\}]*)\\}.*"
SweaveSyntaxLatex$extension <- "\\.[rsRS]tex$"

SweaveSyntaxLatex$trans$doc <-  "\\\\end{Scode}"
SweaveSyntaxLatex$trans$code <- "\\\\begin{Scode}{\\1}"
SweaveSyntaxLatex$trans$coderef <- "\\\\Scoderef{\\1}"
SweaveSyntaxLatex$trans$syntaxname <- "\\\\SweaveSyntax{SweaveSyntaxLatex}"
SweaveSyntaxLatex$trans$extension <- ".Stex"

###**********************************************************

SweaveGetSyntax <- function(file){

    synt <- apropos("SweaveSyntax", mode="list")
    for(sname in synt){
        s <- get(sname, mode="list")
        if(class(s) != "SweaveSyntax") next
        if(length(grep(s$extension, file))) return(s)
    }
    return(SweaveSyntaxNoweb)
}


SweaveSyntConv <- function(file, syntax, output=NULL)
{
    if(is.character(syntax))
        syntax <- get(syntax)

    if(class(syntax) != "SweaveSyntax")
        stop("target syntax not of class \"SweaveSyntax\"")

    if(is.null(syntax$trans))
        stop("target syntax contains no translation table")

    insynt <- SweaveGetSyntax(file)
    text <- readLines(file)
    if(is.null(output))
        output <- sub(insynt$extension, syntax$trans$extension, basename(file))

    TN <- names(syntax$trans)

    for(n in TN){
        if(n != "extension")
            text <- gsub(insynt[[n]], syntax$trans[[n]], text)
    }

    cat(text, file=output, sep="\n")
    cat("Wrote file", output, "\n")
}




###**********************************************************

SweaveParseOptions <- function(text, defaults=list(), check=NULL)
{
    x <- sub("^[[:space:]]*(.*)", "\\1", text)
    x <- sub("(.*[^[:space:]])[[:space:]]*$", "\\1", x)
    x <- unlist(strsplit(x, "[[:space:]]*,[[:space:]]*"))
    x <- strsplit(x, "[[:space:]]*=[[:space:]]*")

    ## only the first option may have no name: the chunk label
    if(length(x)){
        if(length(x[[1L]]) == 1L) {
            x[[1L]] <- c("label", x[[1L]])
        }
    }
    else
        return(defaults)

    if(any(sapply(x, length) != 2L))
        stop(gettextf("parse error or empty option in\n%s", text), domain = NA)

    options <- defaults

    for(k in seq_along(x))
        options[[ x[[k]][1L] ]] <- x[[k]][2L]

    if(!is.null(options[["label"]]) && !is.null(options[["engine"]]))
        options[["label"]] <- sub(paste("\\.", options[["engine"]], "$",
                                        sep=""),
                                  "", options[["label"]])

    if(!is.null(check))
        options <- check(options)

    options
}

SweaveHooks <- function(options, run=FALSE, envir=.GlobalEnv)
{
    if(is.null(SweaveHooks <- getOption("SweaveHooks")))
        return(NULL)

    z <- character(0L)
    for(k in names(SweaveHooks)){
        if(k != "" && !is.null(options[[k]]) && options[[k]]){
            if(is.function(SweaveHooks[[k]])){
                z <- c(z, k)
                if(run)
                    eval(SweaveHooks[[k]](), envir=envir)
            }
        }
    }
    z
}





###**********************************************************


RweaveLatex <- function()
{
    list(setup = RweaveLatexSetup,
         runcode = RweaveLatexRuncode,
         writedoc = RweaveLatexWritedoc,
         finish = RweaveLatexFinish,
         checkopts = RweaveLatexOptions)
}

RweaveLatexSetup <-
    function(file, syntax, output=NULL, quiet=FALSE, debug=FALSE,
             stylepath, ...)
{
    dots <- list(...)
    if(is.null(output)) {
        prefix.string <- basename(sub(syntax$extension, "", file))
        output <- paste(prefix.string, "tex", sep=".")
    } else prefix.string <- basename(sub("\\.tex$", "", output))

    if(!quiet) cat("Writing to file ", output, "\n",
                   "Processing code chunks ...\n", sep="")
    output <- file(output, open="w+")

    if(missing(stylepath)) {
        p <- as.vector(Sys.getenv("SWEAVE_STYLEPATH_DEFAULT"))
        stylepath <- if(length(p) >= 1L && nzchar(p[1L])) identical(p, "TRUE") else FALSE
    }
    if(stylepath){
        styfile <- file.path(R.home("share"), "texmf", "Sweave")
        if(.Platform$OS.type == "windows")
            styfile <- gsub("\\\\", "/", styfile)
        if(length(grep(" ", styfile)))
            warning(gettextf("path to '%s' contains spaces,\n", styfile),
                    gettext("this may cause problems when running LaTeX"),
                    domain = NA)
    } else styfile <- "Sweave"

    options <- list(prefix=TRUE, prefix.string=prefix.string,
                    engine="R", print=FALSE, eval=TRUE,
                    fig=FALSE, pdf=TRUE, eps=TRUE,
                    width=6, height=6, term=TRUE,
                    echo=TRUE, keep.source=FALSE, results="verbatim",
                    split=FALSE, strip.white="true", include=TRUE,
                    pdf.version=grDevices::pdf.options()$version,
                    pdf.encoding=grDevices::pdf.options()$encoding,
                    concordance=FALSE, expand=TRUE)
    options[names(dots)] <- dots

    ## to be on the safe side: see if defaults pass the check
    options <- RweaveLatexOptions(options)

    list(output=output, styfile=styfile, havesty=FALSE, haveconcordance=FALSE,
         debug=debug, quiet=quiet, syntax = syntax,
         options=options, chunkout=list(), srclines=integer(0L),
         srcfile=srcfile(file))
}

makeRweaveLatexCodeRunner <- function(evalFunc=RweaveEvalWithOpt)
{
    ## Return a function suitable as the 'runcode' element
    ## of an Sweave driver.  evalFunc will be used for the
    ## actual evaluation of chunk code.
    RweaveLatexRuncode <- function(object, chunk, options)
      {
          if(!(options$engine %in% c("R", "S"))){
              return(object)
          }

          if(!object$quiet){
              cat(formatC(options$chunknr, width=2), ":")
              if(options$echo) cat(" echo")
              if(options$keep.source) cat(" keep.source")
              if(options$eval){
                  if(options$print) cat(" print")
                  if(options$term) cat(" term")
                  cat("", options$results)
                  if(options$fig){
                      if(options$eps) cat(" eps")
                      if(options$pdf) cat(" pdf")
                  }
              }
              if(!is.null(options$label))
                cat(" (label=", options$label, ")", sep="")
              cat("\n")
          }

          chunkprefix <- RweaveChunkPrefix(options)

          if(options$split){
              ## [x][[1L]] avoids partial matching of x
              chunkout <- object$chunkout[chunkprefix][[1L]]
              if(is.null(chunkout)){
                  chunkout <- file(paste(chunkprefix, "tex", sep="."), "w")
                  if(!is.null(options$label))
                    object$chunkout[[chunkprefix]] <- chunkout
              }
          }
          else
            chunkout <- object$output

	  saveopts <- options(keep.source=options$keep.source)
	  on.exit(options(saveopts))

          SweaveHooks(options, run=TRUE)

          chunkexps <- try(parse(text=chunk), silent=TRUE)
          RweaveTryStop(chunkexps, options)
          openSinput <- FALSE
          openSchunk <- FALSE

          if(length(chunkexps) == 0L)
            return(object)

          srclines <- attr(chunk, "srclines")
          linesout <- integer(0L)
          srcline <- srclines[1L]

	  srcrefs <- attr(chunkexps, "srcref")
	  if (options$expand)
	    lastshown <- 0L
	  else
	    lastshown <- srcline - 1L
	  thisline <- 0
          for(nce in seq_along(chunkexps))
            {
                ce <- chunkexps[[nce]]
                if (nce <= length(srcrefs) && !is.null(srcref <- srcrefs[[nce]])) {
                    if (options$expand) {
                	srcfile <- attr(srcref, "srcfile")
                	showfrom <- srcref[1L]
                	showto <- srcref[3L]
                    } else {
                    	srcfile <- object$srcfile
                    	showfrom <- srclines[srcref[1L]]
                    	showto <- srclines[srcref[3L]]
                    }
                    dce <- getSrcLines(srcfile, lastshown+1, showto)
	    	    leading <- showfrom-lastshown
	    	    lastshown <- showto
                    srcline <- srclines[srcref[3L]]
                    while (length(dce) && length(grep("^[[:blank:]]*$", dce[1L]))) {
	    		dce <- dce[-1L]
	    		leading <- leading - 1L
	    	    }
	    	} else {
                    dce <- deparse(ce, width.cutoff=0.75*getOption("width"))
                    leading <- 1L
                }
                if(object$debug)
                  cat("\nRnw> ", paste(dce, collapse="\n+  "),"\n")
                if(options$echo && length(dce)){
                    if(!openSinput){
                        if(!openSchunk){
                            cat("\\begin{Schunk}\n",
                                file=chunkout, append=TRUE)
                            linesout[thisline + 1] <- srcline
                            thisline <- thisline + 1
                            openSchunk <- TRUE
                        }
                        cat("\\begin{Sinput}",
                            file=chunkout, append=TRUE)
                        openSinput <- TRUE
                    }
		    cat("\n", paste(getOption("prompt"), dce[1L:leading], sep="", collapse="\n"),
		    	file=chunkout, append=TRUE, sep="")
                    if (length(dce) > leading)
                    	cat("\n", paste(getOption("continue"), dce[-(1L:leading)], sep="", collapse="\n"),
                    	    file=chunkout, append=TRUE, sep="")
		    linesout[thisline + seq_along(dce)] <- srcline
		    thisline <- thisline + length(dce)
                }

                                        # tmpcon <- textConnection("output", "w")
                                        # avoid the limitations (and overhead) of output text connections
                tmpcon <- file()
                sink(file=tmpcon)
                err <- NULL
                if(options$eval) err <- evalFunc(ce, options)
                cat("\n") # make sure final line is complete
                sink()
                output <- readLines(tmpcon)
                close(tmpcon)
                ## delete empty output
                if(length(output) == 1L & output[1L] == "") output <- NULL

                RweaveTryStop(err, options)

                if(object$debug)
                  cat(paste(output, collapse="\n"))

                if(length(output) & (options$results != "hide")){

                    if(openSinput){
                        cat("\n\\end{Sinput}\n", file=chunkout, append=TRUE)
                        linesout[thisline + 1L:2L] <- srcline
                        thisline <- thisline + 2L
                        openSinput <- FALSE
                    }
                    if(options$results=="verbatim"){
                        if(!openSchunk){
                            cat("\\begin{Schunk}\n",
                                file=chunkout, append=TRUE)
                            linesout[thisline + 1L] <- srcline
                            thisline <- thisline + 1L
                            openSchunk <- TRUE
                        }
                        cat("\\begin{Soutput}\n",
                            file=chunkout, append=TRUE)
                        linesout[thisline + 1L] <- srcline
                        thisline <- thisline + 1L
                    }

                    output <- paste(output,collapse="\n")
                    if(options$strip.white %in% c("all", "true")){
                        output <- sub("^[[:space:]]*\n", "", output)
                        output <- sub("\n[[:space:]]*$", "", output)
                        if(options$strip.white=="all")
                          output <- sub("\n[[:space:]]*\n", "\n", output)
                    }
                    cat(output, file=chunkout, append=TRUE)
                    count <- sum(strsplit(output, NULL)[[1L]] == "\n")
                    if (count > 0L) {
                    	linesout[thisline + 1L:count] <- srcline
                    	thisline <- thisline + count
                    }

                    remove(output)

                    if(options$results=="verbatim"){
                        cat("\n\\end{Soutput}\n", file=chunkout, append=TRUE)
                        linesout[thisline + 1L:2L] <- srcline
                        thisline <- thisline + 2L
                    }
                }
            }

          if(openSinput){
              cat("\n\\end{Sinput}\n", file=chunkout, append=TRUE)
              linesout[thisline + 1L:2L] <- srcline
              thisline <- thisline + 2L
          }

          if(openSchunk){
              cat("\\end{Schunk}\n", file=chunkout, append=TRUE)
              linesout[thisline + 1L] <- srcline
              thisline <- thisline + 1L
          }

          if(is.null(options$label) & options$split)
            close(chunkout)

          if(options$split & options$include){
              cat("\\input{", chunkprefix, "}\n", sep="",
                file=object$output, append=TRUE)
              linesout[thisline + 1L] <- srcline
              thisline <- thisline + 1L
          }

          if(options$fig && options$eval){
              if(options$eps){
                  grDevices::postscript(file=paste(chunkprefix, "eps", sep="."),
                                        width=options$width, height=options$height,
                                        paper="special", horizontal=FALSE)

                  err <- try({SweaveHooks(options, run=TRUE)
                              eval(chunkexps, envir=.GlobalEnv)})
                  grDevices::dev.off()
                  if(inherits(err, "try-error")) stop(err)
              }
              if(options$pdf){
                  grDevices::pdf(file=paste(chunkprefix, "pdf", sep="."),
                                 width=options$width, height=options$height,
                                 version=options$pdf.version,
                                 encoding=options$pdf.encoding)

                  err <- try({SweaveHooks(options, run=TRUE)
                              eval(chunkexps, envir=.GlobalEnv)})
                  grDevices::dev.off()
                  if(inherits(err, "try-error")) stop(err)
              }
              if(options$include) {
                  cat("\\includegraphics{", chunkprefix, "}\n", sep="",
                      file=object$output, append=TRUE)
                  linesout[thisline + 1L] <- srcline
                  thisline <- thisline + 1L
              }
          }
          object$linesout <- c(object$linesout, linesout)
          return(object)
      }
    RweaveLatexRuncode
}

RweaveLatexRuncode <- makeRweaveLatexCodeRunner()

RweaveLatexWritedoc <- function(object, chunk)
{
    linesout <- attr(chunk, "srclines")

    if(length(grep("\\usepackage[^\\}]*Sweave.*\\}", chunk)))
        object$havesty <- TRUE

    if(!object$havesty){
 	begindoc <- "^[[:space:]]*\\\\begin\\{document\\}"
 	which <- grep(begindoc, chunk)
 	if (length(which)) {
            chunk[which] <- sub(begindoc,
                                paste("\\\\usepackage{",
                                      object$styfile,
                                      "}\n\\\\begin{document}", sep=""),
                                chunk[which])
            linesout <- linesout[c(1L:which, which, seq(from=which+1L, length.out=length(linesout)-which))]
            object$havesty <- TRUE
        }
    }

    while(length(pos <- grep(object$syntax$docexpr, chunk)))
    {
        cmdloc <- regexpr(object$syntax$docexpr, chunk[pos[1L]])
        cmd <- substr(chunk[pos[1L]], cmdloc,
                      cmdloc+attr(cmdloc, "match.length")-1L)
        cmd <- sub(object$syntax$docexpr, "\\1", cmd)
        if(object$options$eval){
            val <- as.character(eval(parse(text=cmd), envir=.GlobalEnv))
            ## protect against character(0L), because sub() will fail
            if(length(val) == 0L) val <- ""
        }
        else
            val <- paste("\\\\verb{<<", cmd, ">>{", sep="")

        chunk[pos[1L]] <- sub(object$syntax$docexpr, val, chunk[pos[1L]])
    }
    while(length(pos <- grep(object$syntax$docopt, chunk)))
    {
        opts <- sub(paste(".*", object$syntax$docopt, ".*", sep=""),
                    "\\1", chunk[pos[1L]])
        object$options <- SweaveParseOptions(opts, object$options,
                                             RweaveLatexOptions)
        if (isTRUE(object$options$concordance)
              && !object$haveconcordance) {
            savelabel <- object$options$label
            object$options$label <- "concordance"
            prefix <- RweaveChunkPrefix(object$options)
            object$options$label <- savelabel
            object$concordfile <- paste(prefix, "tex", sep=".")
            chunk[pos[1L]] <- sub(object$syntax$docopt,
                                 paste("\\\\input{", prefix, "}", sep=""),
                                 chunk[pos[1L]])
            object$haveconcordance <- TRUE
        } else
            chunk[pos[1L]] <- sub(object$syntax$docopt, "", chunk[pos[1L]])
    }

    cat(chunk, sep="\n", file=object$output, append=TRUE)
    object$linesout <- c(object$linesout, linesout)

    return(object)
}

RweaveLatexFinish <- function(object, error=FALSE)
{
    outputname <- summary(object$output)$description
    inputname <- object$srcfile$filename
    if(!object$quiet && !error)
        cat("\n",
            gettextf("You can now run LaTeX on '%s'", outputname),
            "\n", sep = "")
    close(object$output)
    if(length(object$chunkout))
        for(con in object$chunkout) close(con)
    if (object$haveconcordance) {
    	# This output format is subject to change.  Currently it contains
    	# three parts, separated by colons:
    	# 1.  The output .tex filename
    	# 2.  The input .Rnw filename
    	# 3.  The input line numbers corresponding to each output line.
    	#     This are compressed using the following simple scheme:
    	#     The first line number, followed by
    	#     a run-length encoded diff of the rest of the line numbers.
        linesout <- object$linesout
        vals <- rle(diff(linesout))
        vals <- c(linesout[1L], as.numeric(rbind(vals$lengths, vals$values)))
    	concordance <- paste(strwrap(paste(vals, collapse=" ")), collapse=" %\n")
    	special <- paste("\\Sconcordance{concordance:", outputname, ":", inputname, ":%\n",
    			 concordance,"}\n", sep="")
    	cat(special, file=object$concordfile)
    }
    invisible(outputname)
}

RweaveLatexOptions <- function(options)
{

    ## ATTENTION: Changes in this function have to be reflected in the
    ## defaults in the init function!

    ## convert a character string to logical
    c2l <- function(x){
        if(is.null(x)) return(FALSE)
        else return(as.logical(toupper(as.character(x))))
    }

    NUMOPTS <- c("width", "height")
    NOLOGOPTS <- c(NUMOPTS, "results", "prefix.string",
                   "engine", "label", "strip.white",
                   "pdf.version", "pdf.encoding")

    for(opt in names(options)){
        if(! (opt %in% NOLOGOPTS)){
            oldval <- options[[opt]]
            if(!is.logical(options[[opt]])){
                options[[opt]] <- c2l(options[[opt]])
            }
            if(is.na(options[[opt]]))
                stop(gettextf("invalid value for '%s' : %s", opt, oldval),
                     domain = NA)
        }
        else if(opt %in% NUMOPTS){
            options[[opt]] <- as.numeric(options[[opt]])
        }
    }

    if(!is.null(options$results))
        options$results <- tolower(as.character(options$results))
    options$results <- match.arg(options$results,
                                 c("verbatim", "tex", "hide"))

    if(!is.null(options$strip.white))
        options$strip.white <- tolower(as.character(options$strip.white))
    options$strip.white <- match.arg(options$strip.white,
                                     c("true", "false", "all"))

    options
}


RweaveChunkPrefix <- function(options)
{
    if(!is.null(options$label)){
        if(options$prefix)
            chunkprefix <- paste(options$prefix.string, "-",
                                 options$label, sep="")
        else
            chunkprefix <- options$label
    }
    else
        chunkprefix <- paste(options$prefix.string, "-",
                             formatC(options$chunknr, flag="0", width=3),
                             sep="")

    return(chunkprefix)
}

RweaveEvalWithOpt <- function (expr, options){
    if(options$eval){
        res <- try(withVisible(eval(expr, .GlobalEnv)),
                   silent=TRUE)
        if(inherits(res, "try-error")) return(res)
        if(options$print | (options$term & res$visible))
            print(res$value)
    }
    return(res)
}


RweaveTryStop <- function(err, options){

    if(inherits(err, "try-error")){
        cat("\n")
        msg <- paste(" chunk", options$chunknr)
        if(!is.null(options$label))
            msg <- paste(msg, " (label=", options$label, ")", sep="")
        msg <- paste(msg, "\n")
        stop(msg, err, call.=FALSE)
    }
}





###**********************************************************

Stangle <- function(file, driver=Rtangle(),
                    syntax=getOption("SweaveSyntax"), ...)
{
    Sweave(file=file, driver=driver, ...)
}

Rtangle <-  function()
{
    list(setup = RtangleSetup,
         runcode = RtangleRuncode,
         writedoc = RtangleWritedoc,
         finish = RtangleFinish,
         checkopts = RweaveLatexOptions)
}


RtangleSetup <- function(file, syntax,
                         output=NULL, annotate=TRUE, split=FALSE,
                         prefix=TRUE, quiet=FALSE)
{
    if(is.null(output)){
        prefix.string <- basename(sub(syntax$extension, "", file))
        output <- paste(prefix.string, "R", sep=".")
    }
    else{
        prefix.string <- basename(sub("\\.[rsRS]$", "", output))
    }

    if(!split){
        if(!quiet)
            cat("Writing to file", output, "\n")
        output <- file(output, open="w")
    }
    else{
        if(!quiet)
            cat("Writing chunks to files ...\n")
        output <- NULL
    }

    options <- list(split=split, prefix=prefix,
                    prefix.string=prefix.string,
                    engine="R", eval=TRUE)

    list(output=output, annotate=annotate, options=options,
         chunkout=list(), quiet=quiet, syntax=syntax)
}


RtangleRuncode <-  function(object, chunk, options)
{
    if(!(options$engine %in% c("R", "S"))){
        return(object)
    }

    chunkprefix <- RweaveChunkPrefix(options)

    if(options$split){
        outfile <- paste(chunkprefix, options$engine, sep=".")
        if(!object$quiet)
            cat(options$chunknr, ":", outfile,"\n")
        ## [x][[1L]] avoids partial matching of x
        chunkout <- object$chunkout[chunkprefix][[1L]]
        if(is.null(chunkout)){
            chunkout <- file(outfile, "w")
            if(!is.null(options$label))
                object$chunkout[[chunkprefix]] <- chunkout
        }
    }
    else
        chunkout <- object$output

    if(object$annotate){
        cat("###################################################\n",
            "### chunk number ", options$chunknr,
            ": ", options$label,
            ifelse(options$eval, "", " eval=FALSE"), "\n",
            "###################################################\n",
            file=chunkout, append=TRUE, sep="")
    }

    hooks <- SweaveHooks(options, run=FALSE)
    for(k in hooks)
        cat("getOption(\"SweaveHooks\")[[\"", k, "\"]]()\n",
            file=chunkout, append=TRUE, sep="")

    if(!options$eval)
        chunk <- paste("##", chunk)

    cat(chunk,"\n", file=chunkout, append=TRUE, sep="\n")

    if(is.null(options$label) & options$split)
        close(chunkout)

    return(object)
}

RtangleWritedoc <- function(object, chunk)
{
    while(length(pos <- grep(object$syntax$docopt, chunk)))
    {
        opts <- sub(paste(".*", object$syntax$docopt, ".*", sep=""),
                    "\\1", chunk[pos[1L]])
        object$options <- SweaveParseOptions(opts, object$options,
                                             RweaveLatexOptions)
        chunk[pos[1L]] <- sub(object$syntax$docopt, "", chunk[pos[1L]])
    }
    return(object)
}


RtangleFinish <- function(object, error=FALSE)
{
    if(!is.null(object$output))
        close(object$output)

    if(length(object$chunkout)) {
        for(con in object$chunkout) close(con)
    }
}

#  File src/library/utils/R/URLencode.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

URLencode <- function(URL, reserved = FALSE)
{
    ## It is unsafe to use ranges here as collation is locale-dependent.
    ## We want to do this on characters and not on bytes.
    OK <- paste("[^-ABCDEFGHIJKLMNOPQRSTUVWXYZ",
		"abcdefghijklmnopqrstuvwxyz0123456789$_.+!*'(),",
		if(!reserved) ";/?:@=&", "]", sep="")
    x <- strsplit(URL, "")[[1L]]
    z <- grep(OK, x)
    if(length(z)) {
        y <- sapply(x[z], function(x)
                    paste("%", as.character(charToRaw(x)), sep="",
                          collapse = ""))
        x[z] <- y
    }
    paste(x, collapse="")
}

URLdecode <- function(URL)
{
    x <- charToRaw(URL)
    pc <- charToRaw("%")
    out <- raw(0L)
    i <- 1L
    while(i <= length(x)) {
        if(x[i] != pc) {
            out <- c(out, x[i])
            i <- i + 1L
        } else {
            y <- as.integer(x[i + 1L:2L])
            y[y > 96L] <- y[y > 96L] - 32L # a-f -> A-F
            y[y > 57L] <- y[y > 57L] - 7L  # A-F
            y <- sum((y - 48L) * c(16L, 1L))
            out <- c(out, as.raw(as.character(y)))
            i <- i + 3L
        }
    }
    rawToChar(out)
}
#  File src/library/utils/R/alarm.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

alarm <- function() cat("\a")

#  File src/library/utils/R/apropos.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

apropos <- function (what, where = FALSE, ignore.case = TRUE, mode = "any")
{
    stopifnot(is.character(what))
    x <- character(0L)
    check.mode <- mode != "any"
    for (i in seq_along(search())) {
	li <-
	    if(ignore.case)
		grep(what, ls(pos = i, all.names = TRUE),
		     ignore.case = TRUE, value = TRUE)
	    else ls(pos = i, pattern = what, all.names = TRUE)
	if(length(li)) {
	    if(check.mode)
		li <- li[sapply(li, exists, where = i,
				mode = mode, inherits = FALSE)]
	    x <- c(x, if(where) structure(li, names = rep.int(i, length(li))) else li)
	}
    }
    sort(x)
}

find <- function(what, mode = "any", numeric = FALSE, simple.words=TRUE)
{
    stopifnot(is.character(what))
    if(length(what) > 1L) {
        warning("elements of 'what' after the first will be ignored")
        what <- what[1L]
    }
#   would need to escape at least + * | as well
#     if(simple.words)
# 	what <- gsub("([.[])", "\\\\\\1", paste("^",what,"$", sep=""))
    len.s <- length(sp <- search())
    ind <- logical(len.s)
    check.mode <- mode != "any"
    for (i in 1L:len.s) {
        if(simple.words) {
            found <- what %in% ls(pos = i, all.names = TRUE)
            if(found && check.mode)
                found <- exists(what, where = i, mode = mode, inherits=FALSE)
            ind[i] <- found
        } else {
            li <- ls(pos = i, pattern = what, all.names = TRUE)
            ll <- length(li)
            if(ll > 0 && check.mode) {
                mode.ok <- sapply(li, exists, where = i, mode = mode,
                                  inherits = FALSE)
                ll <- sum(mode.ok)
                if(ll >= 2)
                    warning(gettextf("%d occurrences in %s", ll, sp[i]),
                            domain = NA)
            }
            ind[i] <- ll > 0L
        }
    }
    ## found name in  search()[ ind ]
    if(numeric) structure(which(ind), names=sp[ind]) else sp[ind]
}

aspell <-
function(files, filter, control = list(), encoding = "unknown")
{
    ## Take the given files and feed them through aspell in pipe mode.

    ## Think about options and more command line options eventually.

    filter_args <- list()
    if(missing(filter) || is.null(filter))
        filter <- NULL
    else if(is.character(filter)) {
        ## Look up filter in aspell filter db.
        filter <- aspell_filter_db[[filter[1L]]]
        ## Could warn if the filter was not found in the db.
    }
    else if(is.list(filter)) {
        ## Support
        ##   list("Rd", drop = "\\references"
        ## at least for now.
        filter_args <- filter[-1L]
        filter <- aspell_filter_db[[filter[[1L]][1L]]]
        ## Could warn if the filter was not found in the db.
    }
    else if(!is.function(filter))
        stop("Invalid 'filter' argument.")

    ## No special expansion of control argument for now.
    control <- paste(as.character(control), collapse = " ")

    encoding <- rep(encoding, length.out = length(files))
    
    db <- data.frame(Original = character(), File = character(),
                     Line = integer(), Column = integer(),
                     stringsAsFactors = FALSE)
    db$Suggestions <- list()
    
    tfile <- tempfile("aspell")
    on.exit(unlink(tfile))
    
    for (i in seq_along(files)) {

        file <- files[i]
        enc <- encoding[i]

        lines <- if(is.null(filter))
            readLines(file, encoding = enc)
        else {
            ## Assume that filter takes an input file (and additional
            ## arguments) and return a character vector.
            do.call(filter, c(list(file, encoding = enc), filter_args))
        }

        ## Need to escape all lines with carets to ensure Aspell handles
        ## them as data: the Aspell docs say
        ##   It is recommended that programmatic interfaces prefix every
        ##   data line with an uparrow to protect themselves against
        ##   future changes in Aspell. 
        writeLines(paste("^", lines, sep = ""), tfile)
        ## Note that this re-encodes character strings with marked
        ## encodings to the current encoding (which is definitely fine
        ## if this is UTF-8 and Aspell was compiled with full UTF-8
        ## support).  Alternatively, we could try using something along
        ## the lines of
        ##   writeLines(paste("^", lines, sep = ""), tfile,
        ##              useBytes = TRUE)
        ## ## Pass encoding info to Aspell in case we know it.
        ## if(!is.null(filter))  {
        ##     enc <- unique(Encoding(lines))
        ##     enc <- enc[enc != "unknown"]
        ##     if(length(enc) != 1L)
        ##         enc <- "unknown"
        ## }
        ## ## But only if we know how to tell Aspell about it.
        ## enc <- switch(enc,
        ##               "UTF-8" = "utf-8",
        ##               "latin1" = "iso-8859-1",
        ##               "unknown")
	## cmd <- sprintf("aspell pipe %s < %s",
        ##                if(enc == "unknown") control
        ##                else sprintf("%s --encoding=%s", control, enc),
        ##                tfile)

        exe <- Sys.which("aspell")
        if(exe == "") {
            exe <- Sys.which("ispell")
            if(exe == "")
                stop("Could not find aspell or ispell executable.")
        }
        
        cmd <- sprintf("%s -a %s < %s", shQuote(exe), control, tfile)

	out <- tools:::.shell_with_capture(cmd)

	if(out$status != 0L)
	    stop(gettextf("Running aspell failed with diagnostics:\n%s",
			  paste(out$stderr, collapse = "\n")))

	## Hopefully everything worked ok.
	lines <- out$stdout[-1L]
	pos <- cumsum(lines == "") + 1L

	## Format is as follows.
	## First line is a header.
	## Blank lines separate the results for each line.
	## Results for the word on each line are given as follows.
	## * If the word was found in the main dictionary, or your personal
	##   dictionary, then the line contains only a `*'. 
	## * If the word is not in the dictionary, but there are
	##   suggestions, then the line contains an `&', a space, the
	##   misspelled word, a space, the number of near misses, the number
	##   of characters between the beginning of the line and the
	##   beginning of the misspelled word, a colon, another space, and a
	##   list of the suggestions separated by commas and spaces. 
	## * If the word does not appear in the dictionary, and there are no
	##   suggestions, then the line contains a `#', a space, the
	##   misspelled word, a space, and the character offset from the
	##   beginning of the line.
	## This can be summarized as follows:
	##   OK: *
	##   Suggestions: & original count offset: miss, miss, ...
	##   None: # original offset

	## Look at words not in dictionary with suggestions.
	ind <- grepl("^&", lines)
	if(any(ind)) {
	    info <- strsplit(lines[ind], ": ", fixed = TRUE)
	    one <- strsplit(sapply(info, `[`, 1L), " ",  fixed = TRUE)
	    two <- strsplit(sapply(info, `[`, 2L), ", ", fixed = TRUE)
	    db1 <- data.frame(Original =
			      as.character(sapply(one, `[`, 2L)),
			      File = file,
			      Line = pos[ind],
			      Column =
			      as.integer(sapply(one, `[`, 4L)),
			      stringsAsFactors = FALSE)
	    db1$Suggestions <- two
	    db <- rbind(db, db1)
	}
	## Looks at words not in dictionary with no suggestions.
	ind <- grepl("^#", lines)
	if(any(ind)) {
	    one <- strsplit(lines[ind], " ", fixed = TRUE)
	    db1 <- data.frame(Original =
			      as.character(sapply(one, `[`, 2L)),
			      File = file,
			      Line = pos[ind],
			      Column =
			      as.integer(sapply(one, `[`, 3L)),
			      stringsAsFactors = FALSE)
	    db1$Suggestions <- vector("list", length(one))
	    db <- rbind(db, db1)
	}
    }
    structure(db, class = c("aspell", "data.frame"))
}

print.aspell <-
function(x, sort = TRUE, verbose = FALSE, indent = 2L, ...)
{
    ## A very simple printer ...
    if(!(nr <- nrow(x))) return(invisible(x))

    if (sort) 
    	x <- x[order(x$Original, x$File, x$Line, x$Column), ]
    
    if (verbose)
    	out <-
    	    sprintf("%sWord: %s (%s:%d:%d)\n%s",
    	            c("", rep.int("\n", nr - 1L)),
    	            x$Original, x$File, x$Line, x$Column,
    	            formatDL(rep.int("Suggestions", nr),
    	                     sapply(x$Suggestions, paste, collapse = " "),
    	                     style = "list"))
    else {
        s <- split(sprintf("%s:%d:%d", x$File, x$Line, x$Column),
                   x$Original)
        sep <- sprintf("\n%s",
                       paste(rep.int(" ", indent), collapse = ""))
        out <- paste(names(s),
                     sapply(s, paste, collapse = sep),
                     sep = sep, collapse = "\n\n")
    }
    writeLines(out)
    invisible(x)
}

summary.aspell <-
function(object, ...)
{
    words <- sort(unique(object$Original))
    if(length(words)) {
        writeLines("Possibly mis-spelled words:")
        print(words)
    }
    invisible(words)
}

aspell_filter_db <- new.env()
aspell_filter_db$Rd <- tools::RdTextFilter
aspell_filter_db$Sweave <- tools::SweaveTeXFilter

### Utilities.

## For spell-checking the R manuals:

aspell_control_R_manuals <-
    c("--master=en_US",
      "--add-extra-dicts=en_GB",
      "--mode=texinfo",
      "--add-texinfo-ignore=acronym",
      "--add-texinfo-ignore=deftypefun",
      "--add-texinfo-ignore=deftypefunx",
      "--add-texinfo-ignore=findex",
      "--add-texinfo-ignore=enindex",
      "--add-texinfo-ignore=include",
      "--add-texinfo-ignore=ifclear",
      "--add-texinfo-ignore=ifset",
      "--add-texinfo-ignore=math",
      "--add-texinfo-ignore=macro",
      "--add-texinfo-ignore=multitable",
      "--add-texinfo-ignore=node",
      "--add-texinfo-ignore=pkg",
      "--add-texinfo-ignore=printindex",
      "--add-texinfo-ignore=set",
      "--add-texinfo-ignore=vindex",
      "--add-texinfo-ignore-env=menu"
      )
    
aspell_R_manuals <-
function(which = NULL, dir = NULL)
{
    if(is.null(dir)) dir <- tools:::.R_top_srcdir_from_Rd()
    ## Allow specifying 'R-exts' and alikes, or full paths.
    files <- if(is.null(which)) {
        Sys.glob(file.path(dir, "doc", "manual", "*.texi"))
    } else {
        ind <- which(which ==
                     basename(tools::file_path_sans_ext(which)))
        which[ind] <-
            file.path(dir, "doc", "manual",
                      sprintf("%s.texi", which[ind]))
        which
    }
    
    aspell(files,
           control = aspell_control_R_manuals)
}

## For spell-checking the R Rd files:

aspell_control_R_Rd_files <-
    c("--master=en_US",
      "--add-extra-dicts=en_GB")

aspell_R_Rd_files <-
function(which = NULL, dir = NULL, drop = "\\references")
{
    if(is.null(dir)) dir <- tools:::.R_top_srcdir_from_Rd()
    if(is.null(which))
        which <- tools:::.get_standard_package_names()$base

    files <-
        unlist(lapply(file.path(dir, "src", "library", which, "man"),
                      tools::list_files_with_type,
                      "docs", OS_subdirs = c("unix", "windows")),
               use.names = FALSE)

    aspell(files,
           filter = list("Rd", drop = drop),
           control = aspell_control_R_Rd_files)
}

## For spell-checking Rd files in a package:

aspell_package_Rd_files <-
function(dir, drop = "\\references", control = list())
{
    dir <- tools::file_path_as_absolute(dir)
    
    man_dir <- file.path(dir, "man")
    files <- if(file_test("-d", man_dir))
        tools::list_files_with_type(man_dir,
                                    "docs",
                                    OS_subdirs = c("unix", "windows"))
    else character()

    meta <- tools:::.get_package_metadata(dir, installed = FALSE)
    if(is.na(encoding <- meta["Encoding"]))
        encoding <- "unknown"

    defaults <- .aspell_package_defaults(dir, encoding)$Rd_files
    if(!is.null(defaults)) {
        ## For now, allow providing defaults for drop/control directly,
        ## and for setting the personal dictionary (without hard-wiring
        ## the file path).  Direct settings currently override (could
        ## add a list add = TRUE mechanism eventually).
        if(!is.null(d <- defaults$drop))
            drop <- d
        if(!is.null(d <- defaults$control))
            control <- d
        if(!is.null(d <- defaults$personal))
            control <- c(control,
                         sprintf("--personal=%s",
                                 file.path(dir, ".aspell", d)))
    }
    
    aspell(files,
           filter = list("Rd", drop = drop),
           control = control,
           encoding = encoding)
}

## For spell-checking vignettes:

aspell_control_vignettes <-
    c("--mode=tex",
      "--add-tex-command='citep oop'",
      "--add-tex-command='Sexpr p'",
      "--add-tex-command='code p'",
      "--add-tex-command='pkg p'",
      "--add-tex-command='proglang p'",
      "--add-tex-command='samp p'"
      )

aspell_vignettes <-
function(files, control = list())
    aspell(files,
           filter = "Sweave",
           control = c(aspell_control_vignettes, control))

## For spell-checking the R vignettes:

aspell_control_R_vignettes <-
    c("--mode=tex",
      "--master=en_US",
      "--add-extra-dicts=en_GB",
      "--add-tex-command='code p'",
      "--add-tex-command='pkg p'")

aspell_R_vignettes <-
function()
{
    ## No arguments for now.
    ## Currently, all vignettes are in grid.
    files <- Sys.glob(file.path(tools:::.R_top_srcdir_from_Rd(),
                                "src", "library", "grid", "inst", "doc",
                                "*.Snw"))
    aspell(files,
           filter = "Sweave",
           control = aspell_control_R_vignettes)
}

## For spell-checking vignettes in a package:

aspell_package_vignettes <-
function(dir, control = list())
{
    dir <- file.path(dir, "inst", "doc")
    files <- if(file_test("-d", dir))
        tools::list_files_with_type(dir, "vignette")
    else character()

    meta <- tools:::.get_package_metadata(dir, installed = FALSE)
    if(is.na(encoding <- meta["Encoding"]))
        encoding <- "unknown"

    defaults <- .aspell_package_defaults(dir, encoding)$Rd_files
    if(!is.null(defaults)) {
        if(!is.null(d <- defaults$control))
            control <- d
        if(!is.null(d <- defaults$personal))
            control <- c(control,
                         sprintf("--personal=%s",
                                 file.path(dir, ".aspell", d)))
    }
    
    aspell_vignettes(files, control)
}

## For writing personal dictionaries:

aspell_write_personal_dictionary_file <-
function(x, out, language = "en")
{
    if(inherits(x, "aspell"))
        x <- sort(unique(x$Original))

    header <- sprintf("personal_ws-1.1 %s %d", language, length(x))
    ## In case UTF_8 is around ...
    ## This would be nice:
    ##   encodings <- unique(Encoding(x))
    ##   if(identical(encodings[encodings != "unknown"], "UTF-8"))
    ##       header <- paste(header, "UTF-8")
    ## but does not work as we currently do not canonicalized to UTF-8
    ## and do not mark encodings when reading Aspell results which are
    ## documented to be in the current encoding ...
    if(localeToCharset()[1L] == "UTF-8") {
        x <- iconv(x, "", "UTF-8")
        if(any(Encoding(x) == "UTF-8"))
            header <- paste(header, "UTF-8")
    }

    writeLines(c(header, x), out)
}

## For reading package defaults:

.aspell_package_defaults <-
function(dir, encoding = "unknown")
{
    dfile <- file.path(dir, ".aspell", "defaults.R")
    if(!file_test("-f", dfile))
        return(NULL)
    exprs <- parse(dfile, encoding = encoding)
    envir <- new.env()
    for(e in exprs) eval(e, envir)
    as.list(envir)
}
#  File src/library/utils/R/browseVignettes.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/



browseVignettes <- function(package = NULL, lib.loc = NULL, all = TRUE)
{
    ## adapted from vignette()
    if (is.null(package))
        package <- .packages(all.available = all, lib.loc)
    paths <- .find.package(package, lib.loc)
    paths <- paths[tools:::file_test("-d", file.path(paths, "doc"))]
    vignettes <- lapply(paths, function(dir) {
        tools::list_files_with_type(file.path(dir, "doc"), "vignette")
    })
    names(vignettes) <- basename(paths)
    getVinfo <- function(db) {
        dir <- dirname(dirname(db[1L]))
        entries <- NULL
        if (file.exists(INDEX <- file.path(dir, "Meta", "vignette.rds")))
            entries <- .readRDS(INDEX)
        if (NROW(entries) > 0) {
            cbind(Dir = dir,
                  File = basename(entries$File),
                  Title = entries$Title,
                  ## FIXME: test unnecessary once packages are reinstalled
                  R = if (is.null(entries$R)) "" else entries$R,
                  PDF = entries$PDF)[order(entries$Title), , drop=FALSE]
        }
        else NULL
    }
    vinfo <- lapply(vignettes[sapply(vignettes, length) > 0L], getVinfo)
    attr(vinfo, "call") <- sys.call()
    attr(vinfo, "footer") <-
        if (all) ""
        else sprintf(gettext("Use <code> %s </code> \n to list the vignettes in all <strong>available</strong> packages."),
                     "browseVignettes(all = TRUE)")
    class(vinfo) <- "browseVignettes"
    return(vinfo)
}



print.browseVignettes <- function(x, ...)
{
    if (length(x) == 0L) {
        message("No vignettes found by ",
                paste(deparse(attr(x, "call")), collapse=" "))
        return(invisible(x))
    }
    oneLink <- function(s)
    {
        if (length(s) == 0L) return(character(0L))
        title <- s[, "Title"]
        src <- file.path(s[, "Dir"], "doc", s[, "File"])
        pdf <- ifelse(s[, "PDF"] != "", # or nzchar(s[, "PDF"]),
                      file.path(s[, "Dir"], "doc", s[, "PDF"]),
                      "")
        rcode <- ifelse(s[, "R"] != "", # or nzchar(s[, "R"]),
                        file.path(s[, "Dir"], "doc", s[, "R"]),
                      "")
        sprintf("  <li>%s  -  \n    %s  \n    %s  \n    %s \n  </li>\n",
                title,
                ifelse(nzchar(pdf),
                       sprintf("<a href='file://%s'>PDF</a>&nbsp;", pdf),
                       ""),
                ifelse(nzchar(rcode),
                       sprintf("<a href='file://%s'>R</a>&nbsp;", rcode),
                       ""),
                sprintf("<a href='file://%s'>LaTeX/noweb</a>&nbsp;", src))
    }
    file <- sprintf("%s.html", tempfile("Rvig."))
    sink(file)
    css_file <- file.path(R.home("doc"), "html", "R.css")
    cat(sprintf("<!DOCTYPE html PUBLIC '-//W3C//DTD HTML 4.01 Transitional//EN'>
<html>
<head>
<title>R Vignettes</title>
<meta http-equiv='Content-Type' content='text/html; charset=iso-8859-1'>
<link rel='stylesheet' type='text/css' href='%s'>
</head>
<body>\n", css_file))
    cat(sprintf("<h2>Vignettes found by <code><q>%s</q></code></h2>",
                paste(deparse(attr(x, "call")), collapse=" ")))
    cat("<div class=\"vignettes\">")
    for (pkg in names(x))
    {
        cat(sprintf("<h3>Vignettes in package <code>%s</code></h3>\n", pkg))
        cat("<ul>\n")
        links <- oneLink(x[[pkg]])
        cat(paste(links), collapse = "\n")
        cat("\n</ul>\n")
    }
    cat("</div>")
    cat(sprintf("<hr/><p>%s</p>", attr(x, "footer")))
    cat("</body></html>\n")
    sink()
    ## the first two don't work on Windows with browser=NULL.
    ## browseURL(URLencode(sprintf("file://%s", file)))
    ## browseURL(URLencode(file))
    browseURL(sprintf("file://%s", file))
    ## browseURL(file)
    invisible(x)
}
bug.report <- function(subject = "", ccaddress = Sys.getenv("USER"),
                       method = getOption("mailer"),
                       address = "r-bugs@r-project.org",
                       file = "R.bug.report")
{
    create.post(instructions = "\\n<<insert bug report here>>\\n\\n\\n\\n",
                description = "bug report",
                subject = subject,
                ccaddress = ccaddress,
                method = method,
                address = address,
                file = file)
}
#  File src/library/utils/R/capture.output.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

capture.output <- function(..., file=NULL, append=FALSE)
{
    args <- substitute(list(...))[-1L]

    rval <- NULL; closeit <- TRUE
    if (is.null(file))
        file <- textConnection("rval", "w", local = TRUE)
    else if (is.character(file))
        file <- file(file, if(append) "a" else "w")
    else if (inherits(file, "connection")) {
	if (!isOpen(file)) open(file, if(append) "a" else "w")
	else closeit <- FALSE
    } else
        stop("'file' must be NULL, a character string or a connection")

    sink(file)
    ## for error recovery: all output will be lost if file=NULL
    on.exit({sink(); if(closeit) close(file)})

    pf <- parent.frame()
    evalVis <- function(expr)
        withVisible(eval(expr, pf))

    for(i in seq_along(args)) {
        expr <- args[[i]]
        tmp <- switch(mode(expr),
                      "expression" = lapply(expr, evalVis),
                      "call" =, "name" =  list(evalVis(expr)),
                       stop("bad argument"))
        for(item in tmp)
            if (item$visible) print(item$value)
    }
    ## we need to close the text connection before returning 'rval'
    on.exit()
    sink()
    if(closeit) close(file)
    if(is.null(rval)) invisible(NULL) else rval
}
#  File src/library/utils/R/citation.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/


citEntry <- function(entry, textVersion, header=NULL, footer=NULL, ...)
{
    z <- list(...)
    names(z) <- tolower(names(z))

    if("author" %in% names(z))
        z$author <- as.personList(z$author)

    attr(z, "entry") <- entry
    attr(z, "textVersion") <- textVersion
    attr(z, "header") <- header
    attr(z, "footer") <- footer
    class(z) <- "citation"
    z
}

citHeader <- function(...)
{
    z <- paste(...)
    class(z) <- "citationHeader"
    z
}

citFooter <- function(...)
{
    z <- paste(...)
    class(z) <- "citationFooter"
    z
}

readCitationFile <- function(file, meta = NULL)
{
    if(is.null(encoding <- meta$Encoding))
        ## Assume latin1 as a default for now, but maybe switch to
        ## "unknown" eventually ...
        encoding <- "latin1"
    con <- file(file, encoding = encoding)
    on.exit(close(con))
    pcf <- parse(con)
    z <- list()
    k <- 0L
    envir <- new.env()
    ## Make the package metadata available to the citation entries.
    assign("meta", meta, envir = envir)

    for(expr in pcf) {
        x <- eval(expr, envir = envir)
        if(class(x) == "citation")
            z[[k <- k+1L]] <- x
        else if(class(x) == "citationHeader")
            attr(z, "header") <- c(attr(z, "header"), x)
        else if(class(x) == "citationFooter")
            attr(z, "footer") <- c(attr(z, "footer"), x)
    }
    class(z) <- "citationList"
    z
}

###**********************************************************

print.citation <-
function(x, bibtex = TRUE, ...)
{
    if(!is.null(attr(x, "header"))){
        writeLines(strwrap(attr(x, "header")))
        cat("\n")
    }

    if(!is.null(attr(x, "textVersion"))){
        writeLines(strwrap(attr(x, "textVersion"), prefix="  "))
        cat("\n")
    }

    if(bibtex){
        cat("A BibTeX entry for LaTeX users is\n\n")
        print(toBibtex(x), prefix="  ")
    }

    if(!is.null(attr(x, "footer"))){
        cat("\n")
        writeLines(strwrap(attr(x, "footer")))
    }

    invisible(x)
}

print.citationList <- function(x, bibtex=length(x)==1, ...)
{
    cat("\n")
    if(!is.null(attr(x, "header"))){
        writeLines(strwrap(attr(x, "header")))
        cat("\n")
    }
    for(y in x)
        print(y, bibtex=bibtex)

    if(!is.null(attr(x, "footer"))){
        cat("\n")
        writeLines(strwrap(attr(x, "footer")))
    }
    cat("\n")
    invisible(x)
}

###**********************************************************

person <- function(first="", last="", middle="", email="")
{
    z <- list(name=c(first=first, middle=middle, last=last),
              email=email)
    class(z) <- "person"
    z
}

as.person <- function(x) UseMethod("as.person")

as.person.default <- function(x)
{
    if(class(x)=="person") return(x)

    x <- as.character(x)

    if(length(grep("<.*>", x)))
        email <- sub(".*<([^>]*)>.*", "\\1", x)
    else
        email <- NULL

    name <- sub("[[:space:]]*<[^>]*>", "", x)
    name = unlist(strsplit(name, "[[:space:]]"))

    ## fix for email address only
    if(length(name) == 0L) name = ""

    ## and now create appropriate person objects
    if(length(name) == 1L)
        z <- person(last = name, email = email)
    else if(length(name) == 2L)
        z <- person(first = name[1L], last = name[2L], email = email)
    else
        z <- person(first = name[1L],
                    last = name[length(name)],
                    middle = paste(name[-c(1L, length(name))],
                    collapse = " "),
                    email = email)
    z
}

personList <- function(...)
{
    z = list(...)
    if(any(sapply(z, function(x) class(x) != "person")))
        stop("all arguments must be of class \"person\"")

    class(z) <- "personList"
    z
}

as.personList <- function(x) UseMethod("as.personList")

as.personList.person <- function(x) personList(x)

as.personList.default <- function(x)
{
    if(class(x)=="personList") return(x)

    x <- as.character(x)

    ## first split into individual persons
    x <- unlist(strsplit(x,"[[:space:]]?(,|[[:space:]]and)[[:space:]]+"))
    x <- x[nzchar(x)]

    z <- list()
    for(k in seq_along(x)) z[[k]] <- as.person(x[k])
    class(z) <- "personList"
    z
}

as.character.person <- function(x, ...)
{
    paste(x$name[nzchar(x$name)], collapse=" ")
}

as.character.personList <- function(x, ...)
{
    sapply(x, as.character)
}

###**********************************************************

toBibtex.person <- function(object, ...)
{
    if(length(grep(" ", object$name["last"]))>0)
        object$name["last"] <- paste("{", object$name["last"], "}", sep="")
    as.character(object)
}

toBibtex.personList <- function(object, ...)
{
    z <- sapply(object, toBibtex)
    paste(z, collapse = " and ")
}

toBibtex.citation <- function(object, ...)
{
    z <- paste("@", attr(object, "entry"), "{,", sep="")

    if("author" %in% names(object)){
        object$author <- toBibtex(object$author)
    }

    for(n in names(object))
        z <- c(z, paste("  ", n, " = {", object[[n]], "},", sep=""))

    z <- c(z, "}")
    class(z) <- "Bibtex"
    z
}

toBibtex.citationList <- function(object, ...)
{
    lapply(object, toBibtex)
}

###**********************************************************

citation <-
function(package = "base", lib.loc = NULL)
{
    dir <- system.file(package = package, lib.loc = lib.loc)
    if(dir == "")
        stop(gettextf("package '%s' not found", package), domain = NA)

    meta <- packageDescription(pkg = package, lib.loc = dirname(dir))
    citfile <- file.path(dir, "CITATION")
    if(file_test("-f", citfile)) return(readCitationFile(citfile, meta))
    else if(package == "base") {
        ## Avoid infinite recursion for broken installation.
        stop("broken installation, no CITATION file in the base package.")
    }

    ## Auto-generate citation info.

    ## Base packages without a CITATION file use the base citation.
    if((!is.null(meta$Priority)) && (meta$Priority == "base")) {
    	cit <- citation("base")
    	attr(cit, "header")[1L] <-
            paste("The '", package, "' package is part of R.  ",
                  attr(cit, "header")[1L], sep = "")
    	return(cit)
    }

    year <- sub("-.*", "", meta$`Date/Publication`)
    if(!length(year)) {
        year <- sub(".*((19|20)[[:digit:]]{2}).*", "\\1", meta$Date)
        if(is.null(meta$Date)){
            warning(gettextf("no date field in DESCRIPTION file of package '%s'",
                             package),
                    domain = NA)
        }
        else if(!length(year)) {
            warning(gettextf("could not determine year for '%s' from package DESCRIPTION file",
                             package),
                    domain = NA)
        }
    }
        
    z <- list(title = paste(package, ": ", meta$Title, sep=""),
              author = as.personList(meta$Author),
              year = year,
              note = paste("R package version", meta$Version)
              )

    z$url <- if(identical(meta$Repository, "CRAN"))
        sprintf("http://CRAN.R-project.org/package=%s", package)
    else
        meta$URL

    if(identical(meta$Repository, "R-Forge")) {
        z$url <- if(!is.null(rfp <- meta$"Repository/R-Forge/Project"))
            sprintf("http://R-Forge.R-project.org/projects/%s/", rfp)
        else
            "http://R-Forge.R-project.org/"
        if(!is.null(rfr <- meta$"Repository/R-Forge/Revision"))
            z$note <- paste(z$note, rfr, sep = "/r")
    }
    
    class(z) <- "citation"
    attr(z, "entry") <- "Manual"
    attr(z, "package") <- package

    attr(z, "header") <-
        paste("To cite package", sQuote(package), "in publications use:")

    if(! "recommended" %in% meta$Priority) # we assume those are OK
        attr(z, "footer") <-
            paste("ATTENTION: This citation information has been auto-generated",
                  "from the package DESCRIPTION file and may need manual editing,",
                  "see ", sQuote("help(\"citation\")"), ".")

    author <- as.character(z$author)
    if(length(author) > 1L)
        author <- paste(paste(author[1L:(length(author)-1L)], collapse=", "),
                        author[length(author)], sep=" and ")

    attr(z, "textVersion") <-
        paste(author, " (", z$year, "). ",
              z$title, ". ", z$note, ". ", z$url, sep="")

    z
}
#  File src/library/utils/R/combn.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

combn <- function(x, m, FUN = NULL, simplify = TRUE, ...)
{
    ## DATE WRITTEN: 14 April 1994	    LAST REVISED:  10 July 1995
    ## AUTHOR:	Scott Chasalow
    ##
    ## DESCRIPTION:
    ##	Generate all combinations of the elements of x taken m at a time.
    ##	If x is a positive integer,  returns all combinations
    ##	of the elements of seq(x) taken m at a time.
    ##	If argument "FUN" is not null,	applies a function given
    ##	by the argument to each point.	If simplify is FALSE,  returns
    ##	a list; else returns a vector or an array.  "..." are passed
    ##	unchanged to function given by argument FUN,  if any.

    ##S : Change if (simplify = TRUE) return an array/matrix {not a 'vector'}
    stopifnot(length(m) == 1L)
    if(m < 0)
	stop("m < 0")
    if(m == 0)
	return(if(simplify) vector(mode(x), 0L) else list())
    if(is.numeric(x) && length(x) == 1L && x > 0 && trunc(x) == x)
	x <- seq.int(x)
    n <- length(x)
    if(n < m)
	stop("n < m")
    m <- as.integer(m)
    e <- 0
    h <- m
    a <- 1L:m
    nofun <- is.null(FUN)
    if(!nofun && !is.function(FUN))
	stop("'FUN' must be a function or NULL")
    # first result : what kind, what length,.. ?
    len.r <- length(r <- if(nofun) x[a] else FUN(x[a], ...))
    count <- as.integer(round(choose(n, m))) # >= 1
    if(simplify) {
	dim.use <-
	    if(nofun)
		c(m, count) # matrix also when count = 1
	    else {
		d <- dim(r)
		if(length(d) > 1L)
		    c(d, count)
		else if(len.r > 1L)
		    c(len.r, count)
		else # MM: *still* a matrix - a la "drop = FALSE"
		    c(d, count)
	    } ## NULL in all 'else' cases
##S	use.arr <- !is.null(dim.use)
    }
##S	else use.arr <- FALSE

    if(simplify) { # use atomic vector/array instead of list
##S	if(use.arr)
	    out <- matrix(r, nrow= len.r, ncol= count) # matrix for now
##S	else {
##S	    if(count > 1) {
##S		out <- vector(storage.mode(r), len.r * count)
##S		out[1L] <- r
##S	    }
##S	    else out <- r
##S	}
    }
    else {
	out <- vector("list", count)
	out[[1L]] <- r
    }

    i <- 2L
    nmmp1 <- n - m + 1L # using 1L to keep integer arithmetic
    while(a[1L] != nmmp1) {
	if(e < n - h) {
	    h <- 1L
	    e <- a[m]
	    j <- 1L
	}
	else {
	    e <- a[m - h]
	    h <- h + 1L
	    j <- 1L:h
	}
	a[m - h + j] <- e + j
	r <- if(nofun) x[a] else FUN(x[a], ...)
	if(simplify) ##S if(use.arr)
	    out[, i] <- r else out[[i]] <- r
	i <- i + 1L
    }
    if(simplify) ##S if(use.arr)
	array(out, dim.use) else out
}

### Copyright (C) 2006  Deepayan Sarkar <Deepayan.Sarkar@R-project.org>
###
### This file is part of the utils package for R.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
### MA 02110-1301, USA





### Note: try not to do things that might be slow due to network
### latency (think NFS).  For example, retrieving a list of available
### packages is potentially slow for this reason.


### Status: I'm mostly happy with things.  The only obvious
### improvement I can think of is figuring out when we are in
### continuation mode (R prompt == "+") and make use of previous lines
### in that case.  I haven't found a way to do that.



### Note: sprintf seems faster than paste based on naive benchmarking:

## > system.time(for (i in 1L:100000L) sprintf("foo%sbar%d", letters, 1L:26L) )
##            user          system           total   user.children system.children
##           4.796           0.088           4.887           0.000           0.000
## > system.time(for (i in 1L:100000L) paste("foo", letters, "bar", 1L:26L) )
##            user          system           total   user.children system.children
##           8.300           0.028           8.336           0.000           0.000

### so will change all pastes to sprintf.  However, we need to be
### careful because 0 length components in sprintf will cause errors.


## generic and built-in methods to generate completion after $

.DollarNames <- function(x, pattern)
    UseMethod(".DollarNames")

.DollarNames.default <- function(x, pattern = "") {
    if (is.atomic(x) || is.symbol(x)) character(0)
    else grep(pattern, names(x), value = TRUE)
}

.DollarNames.list <- function(x, pattern = "") {
    grep(pattern, names(x), value = TRUE)
}

.DollarNames.environment <- function(x, pattern = "") {
    ls(x, all.names = TRUE, pattern = pattern)
}

## if (is.environment(object))
## {
##     ls(object,
##        all.names = TRUE,
##        pattern = sprintf("^%s", makeRegexpSafe(suffix)))
## }
## else
## {
##     grep(sprintf("^%s", makeRegexpSafe(suffix)),
##          names(object), value = TRUE)
## }




## modifies settings:

rc.settings <- function(ops, ns, args, func, ipck, S3, data, help, argdb, files)
{
    checkAndChange <- function(what, value)
    {
        if ((length(value) == 1L) &&
            is.logical(value) &&
            !is.na(value))
            .CompletionEnv$settings[[what]] <- value
    }
    if (!missing(ops))   checkAndChange(  "ops",   ops)
    if (!missing(ns))    checkAndChange(   "ns",    ns)
    if (!missing(args))  checkAndChange( "args",  args)
    if (!missing(func))  checkAndChange( "func",  func)
    if (!missing(ipck))  checkAndChange( "ipck",  ipck)
    if (!missing(S3))    checkAndChange(   "S3",    S3)
    if (!missing(data))  checkAndChange( "data",  data)
    if (!missing(help))  checkAndChange( "help",  help)
    if (!missing(argdb)) checkAndChange("argdb", argdb)
    if (!missing(files)) checkAndChange("files", files)
    invisible()
}





## modifies options (adapted from similar functions in lattice):

rc.getOption <- function(name)
{
    get("options", envir = .CompletionEnv)[[name]]
}

rc.options <- function(...)
{
    new <- list(...)
    if (is.null(names(new)) && length(new) == 1L && is.list(new[[1L]]))
        new <- new[[1L]]
    old <- .CompletionEnv$options

    ## if no args supplied, returns full options list
    if (length(new) == 0L) return(old)
    ## typically getting options
    nm <- names(new)
    if (is.null(nm)) return(old[unlist(new)])

    isNamed <- nm != ""
    if (any(!isNamed)) nm[!isNamed] <- unlist(new[!isNamed])

    ## so now everything has non-"" names, but only the isNamed ones
    ## should be set.  Everything should be returned though.

    retVal <- old[nm]
    names(retVal) <- nm
    nm <- nm[isNamed]
    .CompletionEnv$options <- modifyList(old, new[nm])
    invisible(retVal)
}



## summarizes results of last completion attempt:

rc.status <- function()
{
    ## eapply(.CompletionEnv, function(x) x, all.names = TRUE)
    as.list(.CompletionEnv)
}


### Everything below is unexported


## accessors called from C (also .completeToken below):

.assignToken         <- function(text)  assign("token",      text,  envir = .CompletionEnv)
.assignLinebuffer    <- function(line)  assign("linebuffer", line,  envir = .CompletionEnv)
.assignStart         <- function(start) assign("start",      start, envir = .CompletionEnv)
.assignEnd           <- function(end)   assign("end",        end,   envir = .CompletionEnv)
.setFileComp         <- function(state) assign("fileName",   state, envir = .CompletionEnv)

.retrieveCompletions <- function()  unique(get("comps",             envir = .CompletionEnv))
.getFileComp         <- function()         get("fileName",          envir = .CompletionEnv)




## The following function is not required for GNU readline, but can be
## useful if one needs to break a line into tokens.  It requires
## linebuffer and end to be already set, and itself sets token and
## start.  It returns the token.

## FIXME: should this use getOption("rl_word_breaks")?

.guessTokenFromLine <-
    function(linebuffer = .CompletionEnv[["linebuffer"]],
             end = .CompletionEnv[["end"]])
{
    ## special rules apply when we are inside quotes (see fileCompletionPreferred() below)
    insideQuotes <- {
        lbss <- head(unlist(strsplit(linebuffer, "")), .CompletionEnv[["end"]])
        ((sum(lbss == "'") %% 2 == 1) ||
         (sum(lbss == '"') %% 2 == 1))
    }
    start <-
        if (insideQuotes)
            ## set 'start' to the location of the last quote
            suppressWarnings(gregexpr("['\"]",
                                      substr(linebuffer, 1L, end),
                                      perl = TRUE))[[1L]]
            ## suppressWarnings(gregexpr("[^\\.\\w:?$@[\\]\\\\/~ ]+", substr(linebuffer, 1L, end), perl = TRUE))[[1L]]
        else
            ##                    things that should not cause breaks
            ##                           ______________
            suppressWarnings(gregexpr("[^\\.\\w:?$@[\\]]+",
                                      substr(linebuffer, 1L, end),
                                      perl = TRUE))[[1L]]
    start <- ## 0-indexed
        if (all(start < 0L)) 0L
        else tail(start + attr(start, "match.length"), 1L) - 1L
    .CompletionEnv[["start"]] <- start
    .CompletionEnv[["token"]] <- substr(linebuffer, start + 1L, end)
    .CompletionEnv[["token"]]
}






## convert a string to something that escapes special regexp
## characters.  Doesn't have to be perfect, especially for characters
## that would cause breaks or be handled elsewhere.  All we really
## need is to handle ".", so that e.g. "heat." doesn't match
## "heatmap".


makeRegexpSafe <- function(s)
{
    ## the following can cause errors otherwise
    s <- gsub("\\", "\\\\", s, fixed = TRUE) ## has to be the first
    s <- gsub("(", "\\(", s, fixed = TRUE)
    s <- gsub("*", "\\*", s, fixed = TRUE)
    s <- gsub("+", "\\+", s, fixed = TRUE)
    s <- gsub("?", "\\?", s, fixed = TRUE)
    s <- gsub("[", "\\[", s, fixed = TRUE)
    s <- gsub("{", "\\{", s, fixed = TRUE)
    ## s <- gsub("]", "\\]", s, fixed = TRUE) # necessary?
    ## these are wildcards that we want to interpret literally
    s <- gsub(".", "\\.", s, fixed = TRUE)
    s <- gsub("^", "\\^", s, fixed = TRUE)
    ## what else?
    s
}



## Operators that are handled specially.  Order is important, ::: must
## come before :: (because :: will match :::)

specialOps <- c("$", "@", ":::", "::", "?", "[", "[[")



specialOpLocs <- function(text)
{
    ## does text contain a special operator?  There may be multiple
    ## occurrences, and we want the last one (whereas regexpr gives
    ## the first). So...

    ge <-
        sapply(specialOps,
               function(s) gregexpr(s, text, fixed = TRUE)[[1L]],
               simplify = FALSE)
    ## this gets the last ones
    ge <- sapply(ge, tail, 1)
    ge <- ge[ge > 0]
}



## accessing the help system: should allow anything with an index entry

matchAvailableTopics <-
    function(text)
{
    if (length(text) != 1L || text == "") return (character(0L))
    ll <- installed.packages()
    ll <- ll[intersect(rownames(ll), .packages()), "LibPath"]
    indexFiles <- file.path(ll, names(ll), "help", "AnIndex")
    unique(unlist(lapply(indexFiles,
                         function(f) {
                             if (!file.exists(f)) return (character(0L))
                             foo <-
                                 scan(f, what = list("", ""),
                                      sep = "\t",
                                      quote = "",
                                      na.strings = "",
                                      quiet = TRUE)[[1L]]
                             grep(sprintf("^%s", makeRegexpSafe(text)),
                                  foo, value = TRUE)
                         })))
}




## this is for requests of the form ?suffix[TAB] or prefix?suffix[TAB]

## can be improved when prefix is non-trivial, but that is rarely used
## (on the other hand, can be useful for exploring S4 methods; but
## usage of ? needs to be fixed in R first).  Anyway, that case is not
## currently handled

helpCompletions <- function(prefix, suffix)
{
    nc <-
        if (.CompletionEnv$settings[["help"]])
            matchAvailableTopics(suffix)
        else
            normalCompletions(suffix, check.mode = FALSE)
    if (length(nc)) sprintf("%s?%s", prefix, nc)
    else character(0L)
}


specialCompletions <- function(text, spl)
{

    ## we'll only try to complete after the last special operator, and
    ## assume that everything before is meaningfully complete.  A more
    ## sophisticated version of this function may decide to do things
    ## differently.

    ## Note that this will involve evaluations, which may have side
    ## effects.  This (side-effects) would not happen normally (except
    ## of lazy loaded symbols, which most likely would have been
    ## evaluated shortly anyway), because explicit function calls
    ## (with parentheses) are not evaluated.  In any case, these
    ## evaluations won't happen if settings$ops==FALSE

    ## spl (locations of matches) is guaranteed to be non-empty

    wm <- which.max(spl)
    op <- names(spl)[wm]
    opStart <- spl[wm]
    opEnd <- opStart + nchar(op)

    if (opStart < 1) return(character(0L)) # shouldn't happen
    prefix <- substr(text, 1L, opStart - 1L)
    suffix <- substr(text, opEnd, 1000000L)

    if (op == "?") return(helpCompletions(prefix, suffix))

    if (opStart <= 1) return(character(0L)) # not meaningful

    ## ( breaks words, so prefix should not involve function calls,
    ## and thus, hopefully no side-effects.

    tryToEval <- function(s)
    {
        try(eval(parse(text = s), envir = .GlobalEnv), silent = TRUE)
    }

    comps <-
        switch(op,
               "$" = {
                   if (.CompletionEnv$settings[["ops"]])
                   {
                       object <- tryToEval(prefix)
                       if (inherits(object, "try-error")) ## nothing else to do
                           suffix
                       else
                       {
                           ## ## suffix must match names(object) (or ls(object) for environments)
                           .DollarNames(object, pattern = sprintf("^%s", makeRegexpSafe(suffix)))
                           ## if (is.environment(object))
                           ## {
                           ##     ls(object,
                           ##        all.names = TRUE,
                           ##        pattern = sprintf("^%s", makeRegexpSafe(suffix)))
                           ## }
                           ## else
                           ## {
                           ##     grep(sprintf("^%s", makeRegexpSafe(suffix)),
                           ##          names(object), value = TRUE)
                           ## }
                       }
                   } else suffix
               },
               "@" = {
                   if (.CompletionEnv$settings[["ops"]])
                   {
                       object <- tryToEval(prefix)
                       if (inherits(object, "try-error")) ## nothing else to do
                           suffix
                       else
                       {
                           grep(sprintf("^%s", makeRegexpSafe(suffix)),
                                methods::slotNames(object), value = TRUE)
                       }
                   } else suffix
               },
               "::" = {
                   if (.CompletionEnv$settings[["ns"]])
                   {
                       nse <- try(getNamespaceExports(prefix), silent = TRUE)
                       if (inherits(nse, "try-error")) ## nothing else to do
                           suffix
                       else
                       {
                           grep(sprintf("^%s", makeRegexpSafe(suffix)),
                                nse, value = TRUE)
                       }
                   } else suffix
               },
               ":::" = {
                   if (.CompletionEnv$settings[["ns"]])
                   {
                       ns <- try(getNamespace(prefix), silent = TRUE)
                       if (inherits(ns, "try-error")) ## nothing else to do
                           suffix
                       else
                       {
                           ls(ns,
                              all.names = TRUE,
                              pattern = sprintf("^%s", makeRegexpSafe(suffix)))
                       }
                   } else suffix
               },
               "[" = ,  # can't think of anything else to do
               "[[" = {
                   comps <- normalCompletions(suffix)
                   if (length(comps)) comps
                   else suffix
               })
    if (length(comps) == 0L) comps <- ""
    sprintf("%s%s%s", prefix, op, comps)
}



## completions on special keywords (subset of those in gram.c).  Some
## issues with parentheses: e.g. mode(get("repeat")) is "function", so
## it is normally completed with a left-paren appended, but that is
## not normal usage.  Putting it here means that both 'repeat' and
## 'repeat(' will be valid completions (as they should be)


keywordCompletions <- function(text)
{
    grep(sprintf("^%s", makeRegexpSafe(text)),
         c("NULL", "NA", "TRUE", "FALSE", "Inf", "NaN",
           "NA_integer_", "NA_real_", "NA_character_", "NA_complex_",
           "repeat ", "in ", "next ", "break ", "else "),
         value = TRUE)
}




## 'package' environments in the search path.  These will be completed
## with a :: IIRC, that works even if there's no namespace, but I
## haven't actually checked.



attachedPackageCompletions <- function(text, add = rc.getOption("package.suffix"))
{
    if (.CompletionEnv$settings[["ns"]])
    {
        s <- grep("^package", search(), value = TRUE)
        comps <-
            grep(sprintf("^%s", makeRegexpSafe(text)),
                 substr(s, 9L, 1000000L),
                 value = TRUE)
        if (length(comps) && !is.null(add))
            sprintf("%s%s", comps, add)
        else
            comps
    }
    else character(0L)
}



## this provides the most basic completion, looking for completions in
## the search path using apropos, plus keywords.  Plus completion on
## attached packages if settings$ns == TRUE


normalCompletions <-
    function(text, check.mode = TRUE,
             add.fun = rc.getOption("function.suffix"))
{
    ## use apropos or equivalent
    if (text == "") character() ## too many otherwise
    else
    {
        comps <- apropos(sprintf("^%s", makeRegexpSafe(text)), ignore.case = FALSE)
        if (.CompletionEnv$settings[["func"]] && check.mode && !is.null(add.fun))
        {
            which.function <- sapply(comps, function(s) exists(s, mode = "function"))
            if (any(which.function))
                comps[which.function] <-
                    sprintf("%s%s", comps[which.function], add.fun)
            ##sprintf("\033[31m%s\033[0m%s", comps[which.function], add.fun)
        }
        c(comps, keywordCompletions(text), attachedPackageCompletions(text))
    }
}


## completion on function arguments.  This involves the most work (as
## we need to look back in the line buffer to figure out which
## function we are inside, if any), and is also potentially intensive
## when many functions match the function that we are supposedly in
## (which will happen for common generic functions like print (we are
## very optimistic here, erring on the side of
## whatever-the-opposite-of-caution-is (our justification being that
## erring on the side of caution is practically useless and not erring
## at all is expensive to the point of being impossible (we really
## don't want to evaluate the dotlot() call in "print(dotplot(x),
## positi[TAB] )" ))))


## this defines potential function name boundaries

breakRE <- "[^\\.\\w]"
## breakRE <- "[ \t\n \\\" '`><=-%;,&}\\\?\\\+\\\{\\\|\\\(\\\)\\\*]"




## for some special functions like library, data, etc, normal
## completion is rarely meaningful, especially for the first argument.
## Unfortunately, knowing whether the token being completed is the
## first arg of such a function involves more work than we would
## normally want to do.  On the other hand, inFunction() below already
## does most of this work, so we will add a piece of code (mostly
## irrelevant to its primary purpose) to indicate this.  The following
## two functions are just wrappers to access and modify this
## information.


setIsFirstArg <- function(v)
    .CompletionEnv[["isFirstArg"]] <- v

getIsFirstArg <- function()
    .CompletionEnv[["isFirstArg"]]



inFunction <-
    function(line = .CompletionEnv[["linebuffer"]],
             cursor = .CompletionEnv[["start"]])
{
    ## are we inside a function? Yes if the number of ( encountered
    ## going backwards exceeds number of ).  In that case, we would
    ## also like to know what function we are currently inside
    ## (ideally, also what arguments to it have already been supplied,
    ## but let's not dream that far ahead).

    parens <-
        sapply(c("(", ")"),
               function(s) gregexpr(s, substr(line, 1L, cursor), fixed = TRUE)[[1L]],
               simplify = FALSE)
    ## remove -1's
    parens <- lapply(parens, function(x) x[x > 0])

    ## The naive algo is as follows: set counter = 0; go backwards
    ## from cursor, set counter-- when a ) is encountered, and
    ## counter++ when a ( is encountered.  We are inside a function
    ## that starts at the first ( with counter > 0.

    temp <-
        data.frame(i = c(parens[["("]], parens[[")"]]),
                   c = rep(c(1, -1), sapply(parens, length)))
    if (nrow(temp) == 0) return(character(0L))
    temp <- temp[order(-temp$i), , drop = FALSE] ## order backwards
    wp <- which(cumsum(temp$c) > 0)
    if (length(wp)) # inside a function
    {
        ## return guessed name of function, letting someone else
        ## decide what to do with that name

        index <- temp$i[wp[1L]]
        prefix <- substr(line, 1L, index - 1L)
        suffix <- substr(line, index + 1L, cursor + 1L)

        ## note in passing whether we are the first argument (no '='
        ## and no ',' in suffix)

        if ((length(grep("=", suffix, fixed = TRUE)) == 0L) &&
            (length(grep(",", suffix, fixed = TRUE)) == 0L))
            setIsFirstArg(TRUE)


        if ((length(grep("=", suffix, fixed = TRUE))) &&
            (length(grep(",", substr(suffix,
                                     tail(gregexpr("=", suffix, fixed = TRUE)[[1L]], 1L),
                                     1000000L), fixed = TRUE)) == 0L))
        {
            ## we are on the wrong side of a = to be an argument, so
            ## we don't care even if we are inside a function
            return(character(0L))
        }
        else ## guess function name
        {
            possible <- suppressWarnings(strsplit(prefix, breakRE, perl = TRUE))[[1L]]
            possible <- possible[possible != ""]
            if (length(possible)) return(tail(possible, 1))
            else return(character(0L))
        }
    }
    else # not inside function
    {
        return(character(0L))
    }
}


argNames <-
    function(fname, use.arg.db = .CompletionEnv$settings[["argdb"]])
{
    if (use.arg.db) args <- .FunArgEnv[[fname]]
    if (!is.null(args)) return(args)
    ## else
    args <- do.call(argsAnywhere, list(fname))
    if (is.null(args))
        character(0L)
    else if (is.list(args))
        unlist(lapply(args, function(f) names(formals(f))))
    else
        names(formals(args))
}



specialFunctionArgs <- function(fun, text)
{
    ## certain special functions have special possible arguments.
    ## This is primarily applicable to library and require, for which
    ## rownames(installed.packages()).  This is disabled by default,
    ## because the first call to installed.packages() can be time
    ## consuming, e.g. on a network file system.  However, the results
    ## are cached, so subsequent calls are not that expensive.

    switch(EXPR = fun,

           library = ,
           require = {
               if (.CompletionEnv$settings[["ipck"]])
               {
                   grep(sprintf("^%s", makeRegexpSafe(text)),
                        rownames(installed.packages()), value = TRUE)
               }
               else character(0L)
           },

           data = {
               if (.CompletionEnv$settings[["data"]])
               {
                   grep(sprintf("^%s", makeRegexpSafe(text)),
                        data()$results[, "Item"], value = TRUE)
               }
               else character(0L)
           },

           ## otherwise,
           character(0L))
}



functionArgs <-
    function(fun, text,
             S3methods = .CompletionEnv$settings[["S3"]],
             S4methods = FALSE,
             add.args = rc.getOption("funarg.suffix"))
{
    if (length(fun) < 1L || any(fun == "")) return(character(0L))
    specialFunArgs <- specialFunctionArgs(fun, text)
    if (S3methods && exists(fun, mode = "function"))
        fun <-
            c(fun,
              tryCatch(methods(fun),
                       warning = function(w) {},
                       error = function(e) {}))
    if (S4methods)
        warning("Can't handle S4 methods yet")
    allArgs <- unique(unlist(lapply(fun, argNames)))
    ans <-
        grep(sprintf("^%s", makeRegexpSafe(text)),
             allArgs, value = TRUE)
    if (length(ans) && !is.null(add.args))
        ans <- sprintf("%s%s", ans, add.args)
    c(specialFunArgs, ans)
}



## Note: Inside the C code, we redefine
## rl_attempted_completion_function rather than
## rl_completion_entry_function, which means that if
## retrieveCompletions() returns a length-0 result, by default the
## fallback filename completion mechanism will be used.  This is not
## quite the best way to go, as in certain (most) situations filename
## completion will definitely be inappropriate even if no valid R
## completions are found.  We could return "" as the only completion,
## but that produces an irritating blank line on
## list-possible-completions (or whatever the correct name is).
## Instead (since we don't want to reinvent the wheel), we use the
## following scheme: If the character just preceding our token is " or
## ', we immediately go to file name completion.  If not, we do our
## stuff, and disable file name completion (using
## .Call("RCSuppressFileCompletion")) even if we don't find any
## matches.

## Note that under this scheme, filename completion will fail
## (possibly in unexpected ways) if the partial name contains 'unusual
## characters', namely ones that have been set (see C code) to cause a
## word break because doing so is meaningful in R syntax (e.g. "+",
## "-" ("/" is exempt (and treated specially below) because of its
## ubiquitousness in UNIX file names (where this package is most
## likely to be used))


## decide whether to fall back on filename completion.  Yes if the
## number of quotes between the cursor and the beginning of the line
## is an odd number.

isInsideQuotes <-
fileCompletionPreferred <- function()
{
    ((st <- .CompletionEnv[["start"]]) > 0 && {

        ## yes if the number of quote signs to the left is odd
        linebuffer <- .CompletionEnv[["linebuffer"]]
        lbss <- head(unlist(strsplit(linebuffer, "")), .CompletionEnv[["end"]])
        ((sum(lbss == "'") %% 2 == 1) ||
         (sum(lbss == '"') %% 2 == 1))

    })
}


## File name completion, used if settings$files == TRUE.  Front ends
## that can do filename completion themselves should probably not use
## this as they will do a better job.


fileCompletions <- function(token)
{
    ## uses Sys.glob (conveniently introduced in 2.5.0)
    ## assume token starts just after the begin quote

    ## Sys.glob doesn't work without expansion.  Is that intended?
    token.expanded <- path.expand(token)
    comps <- Sys.glob(sprintf("%s*", token.expanded), dirmark = TRUE)

    ## for things that only extend beyond the cursor, need to
    ## 'unexpand' path
    if (token.expanded != token)
        comps <- sub(path.expand("~"), "~", comps, fixed = TRUE)
    comps
}





## .completeToken() is the primary interface, and does the actual
## completion when called from C code.


.completeToken <- function()
{
    text <- .CompletionEnv[["token"]]
    if (isInsideQuotes())
    {

        ## If we're in here, that means we think the cursor is inside
        ## quotes.  In most cases, this means that standard filename
        ## completion is more appropriate, but probably not if we're
        ## trying to access things of the form x["foo... or x$"foo...
        ## The following tries to figure this out, but it won't work
        ## in all cases (e.g. x[, "foo<TAB>"])

        ## We assume that whoever determines our token boundaries
        ## considers quote signs as a breaking symbol.

        st <- .CompletionEnv[["start"]]
        probablyNotFilename <-
            (st > 2L &&
             (substr(.CompletionEnv[["linebuffer"]], st-1L, st-1L) %in% c("[", ":", "$")))


        ## If the 'files' setting is FALSE, we will make no attempt to
        ## do filename completion (this is likely to happen with
        ## front-ends that are capable of doing their own file name
        ## completion; such front-ends can fall back to their native
        ## file completion when rc.status("fileName") is TRUE.

        if (.CompletionEnv$settings[["files"]])
        {

            ## we bail out if probablyNotFilename == TRUE.  If we
            ## wanted to be really fancy, we could try to detect where
            ## we are and act accordingly, e.g. if it's
            ## foo[["bar<TAB>, pretend we are completing foo$bar, etc.
            ## This is not that hard, we just need to use
            ## .guessTokenFromLine(linebuffer, end = st - 1) to get the
            ## 'foo[[' part.

            .CompletionEnv[["comps"]] <-
                if (probablyNotFilename) character(0L)
                else fileCompletions(text)
            .setFileComp(FALSE)
        }
        else
        {
            .CompletionEnv[["comps"]] <- character(0L)
            .setFileComp(TRUE)
        }

    }
    else
    {
        .setFileComp(FALSE)
        setIsFirstArg(FALSE) # might be changed by inFunction() call
        ## make a guess at what function we are inside
        guessedFunction <-
            if (.CompletionEnv$settings[["args"]])
                inFunction(.CompletionEnv[["linebuffer"]],
                           .CompletionEnv[["start"]])
            else ""
        .CompletionEnv[["fguess"]] <- guessedFunction

        ## if this is not "", then we want to add possible arguments
        ## of that function(s) (methods etc).  Should be character(0L)
        ## if nothing matches

        fargComps <- functionArgs(guessedFunction, text)

        if (getIsFirstArg() && length(guessedFunction) &&
            guessedFunction %in%
            c("library", "require", "data"))
        {
            .CompletionEnv[["comps"]] <- fargComps
            ## don't try anything else
            return()
        }

        ## Is there an arithmetic operator in there in there?  If so,
        ## work on the part after that and append to prefix before
        ## returning.  It would have been easier if these were
        ## word-break characters, but that potentially interferes with
        ## filename completion.

        ## lastArithOp <- tail(gregexpr("/", text, fixed = TRUE)[[1L]], 1)
        lastArithOp <- tail(gregexpr("[\"'^/*+-]", text)[[1L]], 1)
        if (haveArithOp <- (lastArithOp > 0))
        {
            prefix <- substr(text, 1L, lastArithOp)
            text <- substr(text, lastArithOp + 1L, 1000000L)
        }

        spl <- specialOpLocs(text)
        comps <-
            if (length(spl))
                specialCompletions(text, spl)
            else
            {
                ## should we append a left-paren for functions?
                ## Usually yes, but not when inside certain special
                ## functions which often take other functions as
                ## arguments

                appendFunctionSuffix <-
                    !any(guessedFunction %in%

                         c("help", "args", "formals", "example",
                           "do.call", "environment", "page", "apply",
                           "sapply", "lapply", "tapply", "mapply",
                           "methods", "fix", "edit"))

                normalCompletions(text, check.mode = appendFunctionSuffix)
            }
        if (haveArithOp && length(comps))
        {
            comps <- paste(prefix, comps, sep = "")
        }
        comps <- c(comps, fargComps)
        .CompletionEnv[["comps"]] <- comps
    }
}



## support functions that attempt to provide tools useful specifically
## for the Windows Rgui.


## Note: even though these are unexported functions, changes in the
## API should be noted in man/rcompgen.Rd


.win32consoleCompletion <-
    function(linebuffer, cursorPosition,
             check.repeat = TRUE,
             minlength = -1)
{
    isRepeat <- ## is TAB being pressed repeatedly with this combination?
        if (check.repeat)
            (linebuffer == .CompletionEnv[["linebuffer"]] &&
             cursorPosition == .CompletionEnv[["end"]])
        else TRUE

    .assignLinebuffer(linebuffer)
    .assignEnd(cursorPosition)
    .guessTokenFromLine()
    token <- .CompletionEnv[["token"]]
    comps <-
        if (nchar(token, type = "chars") < minlength) character(0L)
        else
        {
            .completeToken()
            .retrieveCompletions()
        }

    ## FIXME: no idea how much of this is MBCS-safe

    if (length(comps) == 0L)
    {
        ## no completions
        addition <- ""
        possible <- character(0L)
    }
    else if (length(comps) == 1L)
    {
        ## FIXME (maybe): in certain cases the completion may be
        ## shorter than the token (e.g. when trying to complete on an
        ## impossible name inside a list).  It's debatable what the
        ## behaviour should be in this case, but readline and Emacs
        ## actually delete part of the token (at least currently).  To
        ## achieve this in Rgui one would need to do somewhat more
        ## work than I'm ready to do right now (especially since it's
        ## not clear that this is the right thing to do to begin
        ## with).  So, in this case, I'll just pretend that no
        ## completion was found.

        addition <- substr(comps, nchar(token, type = "chars") + 1L, 100000L)
        possible <- character(0L)
    }
    else if (length(comps) > 1L)
    {
        ## more than one completion.  The right thing to is to extend
        ## the line by the unique part if any, and list the multiple
        ## possibilities otherwise.

        additions <- substr(comps, nchar(token, type = "chars") + 1L, 100000L)
        if (length(table(substr(additions, 1L, 1L))) > 1L)
        {
            ## no unique substring
            addition <- ""
            possible <-
                if (isRepeat) capture.output(cat(format(comps, justify = "left"), fill = TRUE))
                else character(0L)
        }
        else
        {
            ## need to figure out maximal unique substr
            keepUpto <- 1
            while (length(table(substr(additions, 1L, keepUpto))) == 1L)
                keepUpto <- keepUpto + 1L
            addition <- substr(additions[1L], 1L, keepUpto - 1L)
            possible <- character(0L)
        }
    }
    list(addition = addition,
         possible = possible,
         comps = paste(comps, collapse = " "))
}




## usage:

## .addFunctionInfo(foo = c("arg1", "arg2"), bar = c("a", "b"))

.addFunctionInfo <- function(...)
{
    dots <- list(...)
    for (nm in names(dots))
        .FunArgEnv[[nm]] <- dots[[nm]]
}

.initialize.argdb <-
    function()
{
    ## lattice

    lattice.common <-
        c("data", "allow.multiple", "outer", "auto.key", "aspect",
          "panel", "prepanel", "scales", "strip", "groups", "xlab",
          "xlim", "ylab", "ylim", "drop.unused.levels", "...",
          "default.scales", "subscripts", "subset", "formula", "cond",
          "aspect", "as.table", "between", "key", "legend", "page",
          "main", "sub", "par.strip.text", "layout", "skip", "strip",
          "strip.left", "xlab.default", "ylab.default", "xlab",
          "ylab", "panel", "xscale.components", "yscale.components",
          "axis", "index.cond", "perm.cond", "...", "par.settings",
          "plot.args", "lattice.options")

    densityplot <-
        c("plot.points", "ref", "groups", "jitter.amount",
          "bw", "adjust", "kernel", "weights", "window", "width",
          "give.Rkern", "n", "from", "to", "cut", "na.rm")

    panel.xyplot <-
        c("type", "groups", "pch", "col", "col.line",
          "col.symbol", "font", "fontfamily", "fontface", "lty",
          "cex", "fill", "lwd", "horizontal")

    .addFunctionInfo(xyplot.formula = c(lattice.common, panel.xyplot),
                     densityplot.formula = c(lattice.common, densityplot))

    ## grid

    grid.clip <-
        c("x", "y", "width", "height", "just", "hjust", "vjust",
          "default.units", "name", "vp")
    grid.curve <-
        c("x1", "y1", "x2", "y2", "default.units", "curvature",
          "angle", "ncp", "shape", "square", "squareShape", "inflect",
          "arrow", "open", "debug", "name", "gp", "vp")
    grid.polyline <-
        c("x", "y", "id", "id.lengths", "default.units", "arrow",
          "name", "gp", "vp")
    grid.xspline <-
        c("x", "y", "id", "id.lengths", "default.units", "shape",
          "open", "arrow", "repEnds", "name", "gp", "vp")

    .addFunctionInfo(grid.clip = grid.clip,
                     grid.curve = grid.curve,
                     grid.polyline = grid.polyline,
                     grid.xspline = grid.xspline)

    ## par, options

    par <-
        c("xlog", "ylog", "adj", "ann", "ask", "bg", "bty", "cex",
          "cex.axis", "cex.lab", "cex.main", "cex.sub", "cin", "col",
          "col.axis", "col.lab", "col.main", "col.sub", "cra", "crt",
          "csi", "cxy", "din", "err", "family", "fg", "fig", "fin",
          "font", "font.axis", "font.lab", "font.main", "font.sub",
          "gamma", "lab", "las", "lend", "lheight", "ljoin", "lmitre",
          "lty", "lwd", "mai", "mar", "mex", "mfcol", "mfg", "mfrow",
          "mgp", "mkh", "new", "oma", "omd", "omi", "pch", "pin",
          "plt", "ps", "pty", "smo", "srt", "tck", "tcl", "usr",
          "xaxp", "xaxs", "xaxt", "xpd", "yaxp", "yaxs", "yaxt")

    options <- c("add.smooth", "browser", "check.bounds", "continue",
	"contrasts", "defaultPackages", "demo.ask", "device",
	"digits", "dvipscmd", "echo", "editor", "encoding",
	"example.ask", "expressions", "help.try.all.packages",
	"htmlhelp", "HTTPUserAgent", "internet.info", "keep.source",
	"keep.source.pkgs", "latexcmd", "locatorBell", "mailer",
	"max.print", "menu.graphics", "na.action", "OutDec", "pager",
	"papersize", "par.ask.default", "pdfviewer", "pkgType",
	"printcmd", "prompt", "repos", "scipen", "show.coef.Pvalues",
	"show.error.messages", "show.signif.stars", "str",
	"stringsAsFactors", "timeout", "ts.eps", "ts.S.compat",
	"unzip", "verbose", "warn", "warnings.length", "width")

    .addFunctionInfo(par = par, options = options)

    ## read.csv etc (... passed to read.table)

}



.CompletionEnv <- new.env()

## needed to save some overhead in .win32consoleCompletion
assign("linebuffer", "", env = .CompletionEnv)
assign("end", 1, env = .CompletionEnv)

assign("settings",
       list(ops = TRUE, ns = TRUE,
            args = TRUE, func = FALSE,
            ipck = FALSE, S3 = TRUE, data = TRUE,
            help = TRUE, argdb = TRUE,
            files = .Platform$OS.type == "windows"),
       env = .CompletionEnv)


assign("options",
       list(package.suffix = "::",
            funarg.suffix = "=",
            function.suffix = "("),
       env = .CompletionEnv)

.FunArgEnv <- new.env(hash = TRUE, parent = emptyenv())

.initialize.argdb() ## see argdb.R

#  File src/library/utils/R/data.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

data <-
function(..., list = character(0L), package = NULL, lib.loc = NULL,
         verbose = getOption("verbose"), envir = .GlobalEnv)
{
    fileExt <- function(x) {
        db <- grepl("\\.[^.]+\\.(gz|bz2|xz)$", x)
        ans <- sub(".*\\.", "", x)
        ans[db] <-  sub(".*\\.([^.]+\\.)(gz|bz2|xz)$", "\\1\\2", x[db])
        ans
    }

    names <- c(as.character(substitute(list(...))[-1L]), list)

    ## Find the directories of the given packages and maybe the working
    ## directory.
    if(!is.null(package)) {
        if(!is.character(package))
            stop("'package' must be a character string or NULL")
        if(any(package %in% "base"))
            warning("datasets have been moved from package 'base' to package 'datasets'")
        if(any(package %in% "stats"))
           warning("datasets have been moved from package 'stats' to package 'datasets'")
        package[package %in% c("base", "stats")] <- "datasets"
    }
    paths <- .find.package(package, lib.loc, verbose = verbose)
    if(is.null(lib.loc))
        paths <- c(.path.package(package, TRUE),
                   if(!length(package)) getwd(),
                   paths)
    paths <- unique(paths[file.exists(paths)])

    ## Find the directories with a 'data' subdirectory.
    paths <- paths[file_test("-d", file.path(paths, "data"))]

    dataExts <- tools:::.make_file_exts("data")

    if(length(names) == 0L) {
        ## List all possible data sets.

        ## Build the data db.
        db <- matrix(character(0L), nrow = 0L, ncol = 4L)
        for(path in paths) {
            entries <- NULL
            ## Use "." as the 'package name' of the working directory.
            packageName <-
                if(file_test("-f", file.path(path, "DESCRIPTION")))
                    basename(path)
                else
                    "."
            ## Check for new-style 'Meta/data.rds'
            if(file_test("-f", INDEX <- file.path(path, "Meta", "data.rds"))) {
                entries <- .readRDS(INDEX)
            } else {
                ## No index: should only be true for ./data >= 2.0.0
                dataDir <- file.path(path, "data")
                entries <- tools::list_files_with_type(dataDir, "data")
                if(length(entries)) {
                    entries <-
                        unique(tools::file_path_sans_ext(basename(entries)))
                    entries <- cbind(entries, "")
                }
            }
            if(NROW(entries)) {
                if(is.matrix(entries) && ncol(entries) == 2L)
                    db <- rbind(db, cbind(packageName, dirname(path), entries))
                else
                    warning(gettextf("data index for package '%s' is invalid and will be ignored", packageName), domain=NA, call.=FALSE)
            }
        }
        colnames(db) <- c("Package", "LibPath", "Item", "Title")

        footer <- if(missing(package))
            paste("Use ",
                  sQuote(paste("data(package =",
                               ".packages(all.available = TRUE))")),
                  "\n",
                  "to list the data sets in all *available* packages.",
                  sep = "")
        else
            NULL
        y <- list(title = "Data sets", header = NULL, results = db,
                  footer = footer)
        class(y) <- "packageIQR"
        return(y)
    }

    paths <- file.path(paths, "data")
    for(name in names) {
        found <- FALSE
        for(p in paths) {
            ## does this package have "Rdata" databases?
            if(file_test("-f", file.path(p, "Rdata.rds"))) {
                rds <- .readRDS(file.path(p, "Rdata.rds"))
                if(name %in% names(rds)) {
                    ## found it, so copy objects from database
                    found <- TRUE
                    if(verbose)
                        message(sprintf("name=%s:\t found in Rdata.rdb", name),
                                domain=NA)
                    thispkg <- sub(".*/([^/]*)/data$", "\\1", p)
                    thispkg <- sub("_.*$", "", thispkg) # versioned installs.
                    thispkg <- paste("package:", thispkg, sep="")
                    objs <- rds[[name]] # guaranteed an exact match
                    lazyLoad(file.path(p, "Rdata"), envir = envir,
                             filter = function(x) x %in% objs)
                    break
                }
            }
            ## check for zipped data dir
            if(file_test("-f", file.path(p, "Rdata.zip"))) {
                if(file_test("-f", fp <- file.path(p, "filelist")))
                    files <- file.path(p, scan(fp, what="", quiet = TRUE))
                else {
                    warning(gettextf("file 'filelist' is missing for directory '%s'", p), domain = NA)
                    next
                }
            } else {
                files <- list.files(p, full.names = TRUE)
            }
            files <- files[grep(name, files, fixed = TRUE)]
            if(length(files) > 1L) {
                ## more than one candidate
                o <- match(fileExt(files), dataExts, nomatch = 100L)
                paths0 <- dirname(files)
		## Next line seems unnecessary to MM (FIXME?)
		paths0 <- factor(paths0, levels= unique(paths0))
                files <- files[order(paths0, o)]
            }
            if(length(files)) {
                ## have a plausible candidate (or more)
                for(file in files) {
                    if(verbose)
                        message("name=", name, ":\t file= ...",
                                .Platform$file.sep, basename(file), "::\t",
                                appendLF = FALSE, domain = NA)
                    ext <- fileExt(file)
                    ## make sure the match is really for 'name.ext'
                    if(basename(file) != paste(name, ".", ext, sep = ""))
                        found <- FALSE
                    else {
                        found <- TRUE
                        Rdatadir <- file.path(tempdir(), "Rdata")
                        dir.create(Rdatadir, showWarnings=FALSE)
                        zfile <- zip.file.extract(file, "Rdata.zip", dir=Rdatadir)
                        if(zfile != file) on.exit(unlink(zfile))
                        switch(ext,
                               R = , r = {
                                   ## ensure utils is visible
                                   library("utils")
                                   sys.source(zfile, chdir = TRUE,
                                              envir = envir)
                               },
                               RData = , rdata = , rda =
                               load(zfile, envir = envir),
                               TXT = , txt = , tab = ,
                               tab.gz = , tab.bz2 = , tab.xz = ,
                               txt.gz = , txt.bz2 = , txt.xz =
                               assign(name,
                                      ## ensure default for as.is has not been
                                      ## overridden by options(stringsAsFactor)
                                      read.table(zfile, header = TRUE, as.is = FALSE),
                                      envir = envir),
                               CSV = , csv = ,
                               csv.gz = , csv.bz2 = , csv.xz =
                               assign(name,
                                      read.table(zfile, header = TRUE,
                                                 sep = ";", as.is = FALSE),
                                      envir = envir),
                               found <- FALSE)
                    }
                    if (found) break # from files
                }
                if(verbose) message(if(!found) "*NOT* ", "found", domain = NA)
            }
            if (found) break # from paths
        }

        if(!found)
            warning(gettextf("data set '%s' not found", name), domain = NA)
    }
    invisible(names)
}
#  File src/library/utils/R/databrowser.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

browseEnv <- function(envir = .GlobalEnv, pattern,
                      excludepatt = "^last\\.warning",
		      html = .Platform$OS.type != "mac",
		      expanded = TRUE, properties = NULL,
		      main = NULL, debugMe = FALSE)
{
    objlist <- ls(envir = envir, pattern = pattern)#, all.names = FALSE
    if(length(iX <- grep(excludepatt, objlist)))
        objlist <- objlist[ - iX]
    if(debugMe) { cat("envir= "); print(envir)
		  cat("objlist =\n"); print(objlist) }
    n <- length(objlist)
    if(n == 0L) {
	cat("Empty environment, nothing to do!\n")
	return(invisible())
    }

    str1 <- function(obj) {
	md <- mode(obj)
	lg <- length(obj)
	objdim <- dim(obj)
	if(length(objdim) == 0L)
	    dim.field <- paste("length:", lg)
	else{
	    dim.field <- "dim:"
	    for(i in seq_along(objdim))
		dim.field <- paste(dim.field,objdim[i])
	    if(is.matrix(obj))
		md <- "matrix"
	}
	obj.class <- oldClass(obj)
	if(!is.null(obj.class)) {
	    md <- obj.class[1L]
	    if(inherits(obj, "factor"))
		dim.field <- paste("levels:",length(levels(obj)))
	}
	list(type = md, dim.field = dim.field)
    }

    N <- 0L
    M <- n
    IDS <- rep.int(NA,n)
    NAMES <- rep.int(NA,n)
    TYPES <- rep.int(NA,n)
    DIMS <- rep.int(NA,n)

    IsRoot <- rep.int(TRUE,n)
    Container <- rep.int(FALSE,n)
    ItemsPerContainer <- rep.int(0,n)
    ParentID <- rep.int(-1,n)

    for( objNam in objlist ){
	N <- N+1L
	if(debugMe) cat("  ", N,":", objNam)
	obj    <- get(objNam, envir = envir)

	sOb <- str1(obj)

	if(debugMe) cat(", type=", sOb$type,",", sOb$dim.field,"\n")

	## Fixme : put these 4 in a matrix or data.frame row:
	IDS[N] <- N
	NAMES[N] <- objNam
	TYPES[N] <- sOb$type
	DIMS[N] <- sOb$dim.field

	if(is.recursive(obj) && !is.function(obj) && !is.environment(obj)
	    ## includes "list", "expression", also "data.frame", ..
	   && (lg <- length(obj))) {
	    Container[N] <- TRUE
	    ItemsPerContainer[N] <- lg
	    nm <- names(obj)
	    if(is.null(nm)) nm <- paste("[[", format(1L:lg), "]]", sep="")
	    for(i in 1L:lg) {
		M <- M+1
		ParentID[M] <- N
		if(nm[i] == "") nm[i] <- paste("[[", i, "]]", sep="")

		s.l <- str1(obj[[i]])
		##cat("	   objname:",nm[i],", type=",md.l,",",dim.field.l,"\n")
		IDS   <- c(IDS,M)
		NAMES <- c(NAMES, nm[i])
		TYPES <- c(TYPES, s.l$type)
		DIMS  <- c(DIMS,  s.l$dim.field)
	    }
	}## recursive

	else if(!is.null(class(obj))) {
	    ## treat some special __non-recursive__ classes:
	    if(inherits(obj, "table")) {
		obj.nms <- attr(obj,"dimnames")
		lg <- length(obj.nms)
		if(length(names(obj.nms)) >0)
		    nm <- names(obj.nms)
		else
		    nm <- rep.int("", lg)
		Container[N] <- TRUE
		ItemsPerContainer[N] <- lg
		for(i in 1L:lg){
		    M <- M+1L
		    ParentID[M] <- N
		    if(nm[i] == "") nm[i] = paste("[[",i,"]]",sep="")
		    md.l  <- mode(obj.nms[[i]])
		    objdim.l <- dim(obj.nms[[i]])
		    if(length(objdim.l) == 0L)
			dim.field.l <- paste("length:", length(obj.nms[[i]]))
		    else{
			dim.field.l <- "dim:"
			for(j in seq_along(objdim.l))
			    dim.field.l <- paste(dim.field.l,objdim.l[i])
		    }
		    ##cat("    objname:",nm[i],", type=",md.l,",",dim.field.l,"\n")
		    IDS <- c(IDS,M)
		    NAMES <- c(NAMES, nm[i])
		    TYPES <- c(TYPES, md.l)
		    DIMS <- c(DIMS,dim.field.l)
		}
	    }## "table"

	    else if(inherits(obj, "mts")) {

		nm <- dimnames(obj)[[2L]]
		lg <- length(nm)
		Container[N] <- TRUE
		ItemsPerContainer[N] <- lg
		for(i in 1L:lg){
		    M <- M+1L
		    ParentID[M] <- N
		    md.l  <- mode(obj[[i]])
		    dim.field.l <- paste("length:",dim(obj)[1L])
		    md.l <- "ts"
		    ##cat("    tseries:",nm[i],", type=",md.l,",",dim.field.l,"\n")
		    IDS <- c(IDS,M)
		    NAMES <- c(NAMES, nm[i])
		    TYPES <- c(TYPES, md.l)
		    DIMS <- c(DIMS,dim.field.l)
		}
	    }## "mts"

	} ## recursive or classed

    } ## "for each object"

    if(debugMe) cat(" __end {for}\n ")##; browser()

    Container	      <- c(Container,	  rep.int(FALSE, M-N))
    IsRoot	      <- c(IsRoot,	  rep.int(FALSE, M-N))
    ItemsPerContainer <- c(ItemsPerContainer, rep.int(0, M-N))

    if(is.null(main))
	main <- paste("R objects in", deparse(substitute(envir)))
    if(is.null(properties)) {
	properties <- as.list(c(date = format(Sys.time(), "%Y-%b-%d %H:%M"),
				local({
				    si <- Sys.info()
				    si[c("user","nodename","sysname")]})))
    }
    if(html)
	wsbrowser(IDS,IsRoot,Container,ItemsPerContainer, ParentID,
		  NAMES,TYPES,DIMS,
		  kind = "HTML", main = main, properties = properties,
		  expanded)
    else ## currently only for Mac:
	.Internal(wsbrowser(as.integer(IDS),IsRoot,Container,
			    as.integer(ItemsPerContainer),as.integer(ParentID),
			    NAMES,TYPES,DIMS))
}

wsbrowser <- function(IDS, IsRoot, IsContainer, ItemsPerContainer,
		      ParentID, NAMES, TYPES, DIMS, expanded=TRUE,
		      kind = "HTML",
		      main = "R Workspace", properties = list(),
		      browser = getOption("browser"))
{
    if(kind != "HTML")
        stop(gettextf("kind '%s' not yet implemented", kind), domain = NA)

    Pst <- function(...) paste(..., sep="")

    bold <- function(ch) Pst("<b>",ch,"</b>")
    ital <- function(ch) Pst("<i>",ch,"</i>")
    entry<- function(ch) Pst("<td>",ch,"</td>")
    Par	 <- function(ch) Pst("<P>",ch,"</P>")
    Trow <- function(N, ...) {
	if(length(list(...)) != N) stop("wrong number of table row entries")
	paste("<tr>", ..., "</tr>\n")
    }
    catRow <- function(...) cat(Trow(nCol, ...), file = Hfile)

#    n <- length(IDS)
    RootItems <- which(IsRoot)
    NumOfRoots <- length(RootItems)

    props <- properties
    if(length(props)) { ## translate named list into 2-column (vertical) table
	nms <- names(props)
	nms <- unlist(lapply(unlist(lapply(Pst(nms,":"),
					   bold)),
			     entry))
	props <- unlist(lapply(props, entry))
	props <-
	    paste("<table border=2>",
		  paste(Trow(1, paste(nms, props)), collapse=""),
		  "</table>", sep = "\n")
    }
    fname <- file.path(tempdir(), "wsbrowser.html")
    Hfile <- file(fname,"w")

    cat("<html>\n<title>", main, "browser</title>\n<body>",
	"<H1>",main,"</H1>\n",
	if(is.character(props)) Par(props),
	"<table border=1>\n", file = Hfile)
    nCol <- if(expanded) 4L else 3L
    catRow(entry(bold("Object")),
	   if(expanded) entry(bold(ital("(components)"))),
	   entry(bold("Type")),
	   entry(bold("Property")))

    for(i in 1L:NumOfRoots) {
	iid <- RootItems[i]
	catRow(entry(NAMES[iid]),
	       if(expanded) entry(""),
	       entry(ital(TYPES[iid])),
	       entry(DIMS[iid]))
	if(IsContainer[i] && expanded) {
	    items <- which(ParentID == i)
	    for(j in 1L:ItemsPerContainer[i]) {
		id <- IDS[items[j]]
		catRow(entry(""),
		       entry(NAMES[id]),#was Pst("$",NAMES[id]) : ugly for [[i]]
		       entry(ital(TYPES[id])),
		       entry(DIMS[id]))
	    }
	}
    }
    cat("</table>\n</body></html>",file=Hfile)
    close(Hfile)

    switch(.Platform$OS.type,
	   windows = , ## do we need anything here?
	   unix = { url <- fname },
	   )
    if(substr(url, 1L, 1L) != "/")
	url <- paste("/", url, sep = "")
    url <- paste("file://", URLencode(url), sep = "")

    browseURL(url = url, browser = browser)
    cat(main, "environment is shown in browser",
        if (is.character(browser)) paste("`",browser, "'", sep=""),"\n")

    invisible(fname)
}
#  File src/library/utils/R/de.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

de.ncols <- function(inlist)
{
    ncols <- matrix(0, nrow=length(inlist), ncol=2L)
    i <- 1L
    for( telt in inlist ) {
	if( is.matrix(telt) ) {
	    ncols[i, 1L] <- ncol(telt)
	    ncols[i, 2L] <- 2L
	}
	else if( is.list(telt) ) {
	    for( telt2 in telt )
		if( !is.vector(telt2) ) stop("wrong argument to 'dataentry'")
	    ncols[i, 1L] <- length(telt)
	    ncols[i, 2L] <- 3L
	}
	else if( is.vector(telt) ) {
	    ncols[i, 1L] <- 1L
	    ncols[i, 2L] <- 1L
	}
	else stop("wrong argument to 'dataentry'")
	i <- i+1L
    }
    return(ncols)
}

de.setup <- function(ilist, list.names, incols)
{
    ilen <- sum(incols)
    ivec <- vector("list", ilen)
    inames <- vector("list", ilen)
    i <- 1L
    k <- 0L
    for( telt in ilist ) {
	k <- k+1L
	if( is.list(telt) ) {
	    y <- names(telt)
	    for( j in seq_along(telt) ) {
		ivec[[i]] <- telt[[j]]
		if( is.null(y) || y[j]=="" )
		    inames[[i]] <- paste("var", i, sep="")
		else inames[[i]] <- y[j]
		i <- i+1L
	    }
	}
	else if( is.vector(telt) ) {
	    ivec[[i]] <- telt
	    inames[[i]] <- list.names[[k]]
	    i <- i+1
	}
	else if( is.matrix(telt) ) {
	    y <- dimnames(telt)[[2L]]
	    for( j in 1L:ncol(telt) ) {
		ivec[[i]] <- telt[, j]
		if( is.null(y) || y[j]=="" )
		    inames[[i]] <- paste("var", i, sep="")
		else inames[[i]] <- y[j]
		i <- i+1L
	    }
	}
	else stop("wrong argument to 'dataentry'")
    }
    names(ivec) <- inames
    return(ivec)
}

de.restore <- function(inlist, ncols, coltypes, argnames, args)
{
    ## take the data in inlist and restore it
    ## to the format described by ncols and coltypes
    p <- length(ncols)
    rlist <- vector("list", length=p)
    rnames <- vector("character", length=p)
    j <- 1L
    lnames <- names(inlist)
    if(p) for(i in 1L:p) {
	if(coltypes[i]==2) {
	    tlen <- length(inlist[[j]])
	    x <- matrix(0, nrow=tlen, ncol=ncols[i])
	    cnames <- vector("character", ncol(x))
	    for( ind1 in 1L:ncols[i]) {
		if(tlen != length(inlist[[j]]) ) {
		    warning("could not restore type information")
		    return(inlist)
		}
		x[, ind1] <- inlist[[j]]
		cnames[ind1] <- lnames[j]
		j <- j+1L
	    }
	    if( nrow(x) == nrow(args[[i]]) )
		rn <- dimnames(args[[i]])[[1L]]
	    else rn <- NULL
	    if( any(cnames!="") )
		dimnames(x) <- list(rn, cnames)
	    rlist[[i]] <- x
	    rnames[i] <- argnames[i]
	}
	else if(coltypes[i]==3) {
	    x <- vector("list", length=ncols[i])
	    cnames <- vector("character", ncols[i])
	    for( ind1 in 1L:ncols[i]) {
		x[[ind1]] <- inlist[[j]]
		cnames[ind1] <- lnames[j]
		j <- j+1L
	    }
	    if( any(cnames!="") )
		names(x) <- cnames
	    rlist[[i]] <- x
	    rnames[i] <- argnames[i]
	}
	else {
	    rlist[[i]] <- inlist[[j]]
	    j <- j+1
	    rnames[i] <- argnames[i]
	}
    }
    names(rlist) <- rnames
    return(rlist)
}

de <- function(..., Modes=list(), Names=NULL)
{
    sdata <- list(...)
    snames <- as.character(substitute(list(...))[-1L])
    if( is.null(sdata) ) {
	if( is.null(Names) ) {
	    odata <- vector("list", length=max(1,length(Modes)))
	}
	else {
	    if( (length(Names) != length(Modes)) && length(Modes) ) {
		warning("modes argument ignored")
		Modes <- list()
	    }
	    odata <- vector("list", length=length(Names))
	    names(odata) <- Names
	}
	ncols <- rep.int(1, length(odata))
	coltypes <- rep.int(1, length(odata))
    }
    else {
	ncols <- de.ncols(sdata)
	coltypes <- ncols[, 2L]
	ncols <- ncols[, 1]
	odata <- de.setup(sdata, snames, ncols)
	if(length(Names))
	    if( length(Names) != length(odata) )
		warning("'names' argument ignored")
	    else names(odata) <- Names
	if(length(Modes))
	    if(length(Modes) != length(odata)) {
		warning("'modes' argument ignored")
		Modes <- list()
	    }
    }
    rdata <- dataentry(odata, as.list(Modes))

    if(any(coltypes != 1L)) {
	if(length(rdata) == sum(ncols))
	    rdata <- de.restore(rdata, ncols, coltypes, snames, sdata)
	else warning("could not restore variables properly")
    }
    return(rdata)
}

data.entry <- function(..., Modes=NULL, Names=NULL)
{
    tmp1 <- de(..., Modes=Modes, Names=Names)
    j <- 1L
    nn <- names(tmp1)
    for(i in nn) {
	assign(i, tmp1[[j]], envir=.GlobalEnv)
	j <- j+1L
    }
    if(j == 1L) warning("did not assign() anything")
    invisible(nn)
}
#  File src/library/utils/R/debugger.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

dump.frames <- function(dumpto = "last.dump", to.file = FALSE)
{
    calls <- sys.calls()
    last.dump <- sys.frames()
    names(last.dump) <- limitedLabels(calls)
    last.dump <- last.dump[-length(last.dump)] # remove this function
    attr(last.dump, "error.message") <- geterrmessage()
    class(last.dump) <- "dump.frames"
    if(dumpto != "last.dump") assign(dumpto, last.dump)
    if (to.file) # compress=TRUE is now the default.
        save(list=dumpto, file = paste(dumpto, "rda", sep="."))
    else assign(dumpto, last.dump, envir=.GlobalEnv)
    invisible()
}

debugger <- function(dump = last.dump)
{
    debugger.look <- function(.selection)
    {
        for(.obj in ls(envir=dump[[.selection]], all.names=TRUE))
            assign(.obj, get(.obj, envir=dump[[.selection]]))
        cat(gettext("Browsing in the environment with call:\n   "),
            calls[.selection], "\n", sep="")
        rm(.obj, .selection)
        browser()
    }
    if (class(dump) != "dump.frames") {
        cat(gettext("'dump' is not an object of class 'dump.frames'\n"))
        return(invisible())
    }
    err.action <- getOption("error")
    on.exit(options(error=err.action))
    if (length(msg <- attr(dump, "error.message")))
        cat(gettext("Message: "), msg)
    n <- length(dump)
    calls <- names(dump)
    repeat {
        cat(gettext("Available environments had calls:\n"))
        cat(paste(1L:n, ": ", calls,  sep=""), sep="\n")
        cat(gettext("\nEnter an environment number, or 0 to exit  "))
        repeat {
            ind <- .Internal(menu(as.character(calls)))
            if(ind <= n) break
        }
        if(ind == 0L) return(invisible())
        debugger.look(ind)
    }
}

## allow for the numbering by menu here
limitedLabels <- function(value, maxwidth = getOption("width") - 5)
{
    srcrefs <- sapply(value, function(v) if (!is.null(srcref <- attr(v, "srcref"))) {
				srcfile <- attr(srcref, "srcfile")
				paste(basename(srcfile$filename), "#", srcref[1L],": ", sep="")
			     } else "")    
    value <- paste(srcrefs, as.character(value), sep="")
    if(is.null(maxwidth) || maxwidth < 40)
        maxwidth <- 40
    strtrim(value, maxwidth)
}

recover <-
  function()
{
    if(.isMethodsDispatchOn()) {
        ## turn off tracing
        tState <- tracingState(FALSE)
        on.exit(tracingState(tState))
    }
    ## find an interesting environment to start from
    calls <- sys.calls()
    from <- 0L
    n <- length(calls)
    if(identical(sys.function(n), recover))
        ## options(error=recover) produces a call to this function as an object
        n <- n - 1L
    ## look for a call inserted by trace() (and don't show frames below)
    ## this level.
    for(i in rev(seq_len(n))) {
        calli <- calls[[i]]
        fname <- calli[[1L]]
        ## deparse can use more than one line
        if(!is.na(match(deparse(fname)[1L],
                        c("methods::.doTrace", ".doTrace")))) {
            from <- i-1L
            break
        }
    }
  ## if no trace, look for the first frame from the bottom that is not
    ## stop or recover
    if(from == 0L)
      for(i in rev(seq_len(n))) {
        calli <- calls[[i]]
        fname <- calli[[1L]]
        if(!is.name(fname) ||
           is.na(match(as.character(fname), c("recover", "stop", "Stop")))) {
            from <- i
            break
        }
    }
    if(from > 0L) {
        if(!interactive()) {
            try(dump.frames())
            cat(gettext("recover called non-interactively; frames dumped, use debugger() to view\n"))
            return(NULL)
        }
        else if(identical(getOption("show.error.messages"), FALSE)) # from try(silent=TRUE)?
            return(NULL)
        calls <- limitedLabels(calls[1L:from])
        repeat {
            which <- menu(calls,
                          title="\nEnter a frame number, or 0 to exit  ")
            if(which)
                eval(quote(browser()), envir = sys.frame(which))
            else
                break
        }
    }
    else
        cat(gettext("No suitable frames for recover()\n"))
}
#  File src/library/utils/R/demo.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

demo <-
function(topic, package = NULL, lib.loc = NULL,
	 character.only = FALSE, verbose = getOption("verbose"),
	 echo = TRUE, ask = getOption("demo.ask"))
{
    paths <- .find.package(package, lib.loc, verbose = verbose)

    ## Find the directories with a 'demo' subdirectory.
    paths <- paths[file_test("-d", file.path(paths, "demo"))]
    ## Earlier versions remembered given packages with no 'demo'
    ## subdirectory, and warned about them.

    if(missing(topic)) {
	## List all possible demos.

	## Build the demo db.
	db <- matrix(character(0L), nrow = 0L, ncol = 4L)
	for(path in paths) {
	    entries <- NULL
	    ## Check for new-style 'Meta/demo.rds', then for '00Index'.
	    if(file_test("-f", INDEX <- file.path(path, "Meta", "demo.rds"))) {
		entries <- .readRDS(INDEX)
	    }
	    if(NROW(entries)) {
		db <- rbind(db,
			    cbind(basename(path), dirname(path),
				  entries))
	    }
	}
	colnames(db) <- c("Package", "LibPath", "Item", "Title")

	footer <- if(missing(package))
	    paste("Use ",
		  sQuote(paste("demo(package =",
			       ".packages(all.available = TRUE))")),
		  "\n",
		  "to list the demos in all *available* packages.",
		  sep = "")
	else
	    NULL
	y <- list(title = "Demos", header = NULL, results = db,
		  footer = footer)
	class(y) <- "packageIQR"
	return(y)
    }

    if(!character.only)
	topic <- as.character(substitute(topic))
    available <- character(0L)
    paths <- file.path(paths, "demo")
    for(p in paths) {
	files <- basename(tools::list_files_with_type(p, "demo"))
	## Files with base names sans extension matching topic
	files <- files[topic == tools::file_path_sans_ext(files)]
	if(length(files))
	    available <- c(available, file.path(p, files))
    }
    if(length(available) == 0L)
	stop(gettextf("No demo found for topic '%s'", topic), domain = NA)
    if(length(available) > 1L) {
	available <- available[1L]
	warning(gettextf("Demo for topic '%s' found more than once,\nusing the one found in '%s'",
                topic, dirname(available[1L])), domain = NA)
    }
    
    if(ask == "default")
        ask <- echo && grDevices::dev.interactive(orNone = TRUE)    
    
    if(.Device != "null device") {
	oldask <- grDevices::devAskNewPage(ask = ask)
        on.exit(grDevices::devAskNewPage(oldask), add = TRUE)
    }

    op <- options(device.ask.default = ask)
    on.exit(options(op), add = TRUE)
    
    if (echo) {
	cat("\n\n",
	    "\tdemo(", topic, ")\n",
	    "\t---- ", rep.int("~", nchar(topic, type="w")), "\n",
	    sep="")
	if(ask) {
	    cat("\nType  <Return>	 to start : ")
	    readline()
	}
    }
    source(available, echo = echo, max.deparse.length = Inf, keep.source=TRUE)
}
#  File src/library/utils/R/edit.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

dataentry <- function (data, modes)
{
    if(!is.list(data) || !length(data) || !all(sapply(data, is.vector)))
        stop("invalid 'data' argument")
    if(!is.list(modes) ||
       (length(modes) && !all(sapply(modes, is.character))))
        stop("invalid 'modes' argument")
    .Internal(dataentry(data, modes))
}

View <- function (x, title)
{
    ## could multi-line deparse with maliciously-designed inputs
    if(missing(title)) title <- paste("Data:", deparse(substitute(x))[1])
    as.num.or.char <- function(x)
    {
        if (is.character(x)) x
        else if (is.numeric(x)) {storage.mode(x) <- "double"; x}
        else as.character(x)
    }
    x0 <- as.data.frame(x)
    x <- lapply(x0, as.num.or.char)
    rn <- row.names(x0)
    if(any(rn != seq_along(rn))) x <- c(list(row.names = rn), x)
    if(!is.list(x) || !length(x) || !all(sapply(x, is.atomic)) ||
       !max(sapply(x, length)))
        stop("invalid 'x' argument")
    .Internal(dataviewer(x, title))
}

edit <- function(name,...)UseMethod("edit")

edit.default <-
    function (name = NULL, file = "", title = NULL,
              editor = getOption("editor"), ...)
{
    if(is.matrix(name) &&
       (mode(name) == "numeric" || mode(name) == "character"))
        edit.matrix(name=name, ...)
    else {
	if (is.null(title)) title <- deparse(substitute(name))
        if (is.function(editor))
            invisible(editor(name, file, title))
	else .Internal(edit(name, file, title, editor))
    }
}

edit.data.frame <-
    function(name, factor.mode = c("character", "numeric"),
             edit.row.names =  any(row.names(name) != 1L:nrow(name)), ...)
{
    if (.Platform$OS.type == "unix"  && .Platform$GUI != "AQUA")
        if(.Platform$GUI == "unknown" || Sys.getenv("DISPLAY") == "" )
            return (edit.default(name, ...))

    is.vector.unclass <- function(x) is.vector(unclass(x))
    if (length(name) && !all(sapply(name, is.vector.unclass)
                                 | sapply(name, is.factor)))
        stop("can only handle vector and factor elements")

    factor.mode <- match.arg(factor.mode)

    as.num.or.char <- function(x)
    {
        if (is.numeric(x)) x
        else if (is.factor(x) && factor.mode == "numeric") as.numeric(x)
        else as.character(x)
    }

    attrlist <- lapply(name, attributes)
    datalist <- lapply(name, as.num.or.char)
    factors <- if (length(name))
        which(sapply(name, is.factor))
    else
        numeric(0L)

    logicals <- if (length(name))
    	which(sapply(name, is.logical))
    else
    	numeric(0L)

    if(length(name)) {
        has_class <-
            sapply(name, function(x) (is.object(x) || isS4(x)) && !is.factor(x))
        if(any(has_class))
            warning(sprintf(ngettext(sum(has_class),
                                    "class discarded from column %s",
                                    "classes discarded from columns %s"),
                            paste(sQuote(names(name)[has_class]),
                                  collapse=", ")),
                    domain = NA, call. = FALSE, immediate. = TRUE)
    }

    modes <- lapply(datalist, mode)
    if (edit.row.names) {
        datalist <- c(list(row.names = row.names(name)), datalist)
        modes <- c(list(row.names = "character"), modes)
    }
    rn <- attr(name, "row.names")

    out <- .Internal(dataentry(datalist, modes))
    if(length(out) == 0L) {
        ## e.g. started with 0-col data frame or NULL, and created no cols
        return (name)
    }
    lengths <- sapply(out, length)
    maxlength <- max(lengths)
    if (edit.row.names) rn <- out[[1L]]
    for (i in which(lengths != maxlength))
         out[[i]] <- c(out[[i]], rep.int(NA, maxlength - lengths[i]))
    if (edit.row.names) {
        out <- out[-1L]
        if((ln <- length(rn)) < maxlength)
            rn <- c(rn, paste("row", (ln+1):maxlength, sep=""))
    } else if(length(rn) != maxlength) rn <- seq_len(maxlength)
    for (i in factors) {
        if(factor.mode != mode(out[[i]])) next # user might have switched mode
        a <- attrlist[[i]]
        if (factor.mode == "numeric") {
            o <- as.integer(out[[i]])
            ok <- is.na(o) | (o > 0 & o <= length(a$levels))
            if (any(!ok)) {
                warning(gettextf("invalid factor levels in '%s'", names(out)[i]),
                        domain = NA)
                o[!ok] <- NA
            }
	    attributes(o) <- a
        } else {
            o <- out[[i]]
            if (any(new <- is.na(match(o, c(a$levels, NA_integer_))))) {
                new <- unique(o[new])
                warning(gettextf("added factor levels in '%s'", names(out)[i]),
                        domain = NA)
                o <- factor(o, levels=c(a$levels, new),
                            ordered = is.ordered(o))
            } else {
                o <- match(o, a$levels)
                attributes(o) <- a
            }
        }
        out[[i]] <- o
    }
    for (i in logicals) out[[i]] <- as.logical(out[[i]])

    attr(out, "row.names") <- rn
    attr(out, "class") <- "data.frame"
    if (edit.row.names) {
        if(anyDuplicated(rn)) {
            warning("edited row names contain duplicates and will be ignored")
            attr(out, "row.names") <- seq_len(maxlength)
        }
    }
    out
}

edit.matrix <-
    function(name, edit.row.names = !is.null(dn[[1L]]), ...)
{
    if (.Platform$OS.type == "unix" && .Platform$GUI != "AQUA")
        if(.Platform$GUI == "unknown" || Sys.getenv("DISPLAY")=="" )
            return (edit.default(name, ...))
    if(!is.matrix(name) ||
       ! mode(name) %in% c("numeric", "character", "logical") ||
       any(dim(name) < 1))
        stop("invalid input matrix")
    ## logical matrices will be edited as character
    logicals <- is.logical(name)
    if (logicals) mode(name) <- "character"
    if(is.object(name) || isS4(name))
        warning("class(es) of 'name' will be discarded",
                call. = FALSE, immediate. = TRUE)

    dn <- dimnames(name)
    datalist <- split(name, col(name))
    if(!is.null(dn[[2L]])) names(datalist) <- dn[[2L]]
    else names(datalist) <- paste("col", 1L:ncol(name), sep = "")
    modes <- as.list(rep.int(mode(name), ncol(name)))
    ## guard aginst user error (PR#10500)
    if(edit.row.names && is.null(dn[[1L]]))
        stop("cannot edit NULL row names")
    if (edit.row.names) {
        datalist <- c(list(row.names = dn[[1L]]), datalist)
        modes <- c(list(row.names = "character"), modes)
    }

    out <- .Internal(dataentry(datalist, modes))

    lengths <- sapply(out, length)
    maxlength <- max(lengths)
    if (edit.row.names) rn <- out[[1L]]
    for (i in which(lengths != maxlength))
         out[[i]] <- c(out[[i]], rep.int(NA, maxlength - lengths[i]))
    if (edit.row.names) {
        out <- out[-1L]
        if((ln <- length(rn)) < maxlength)
            rn <- c(rn, paste("row", (ln+1L):maxlength, sep=""))
    }
    out <- do.call("cbind", out)
    if (edit.row.names)
        rownames(out) <- rn
    else if(!is.null(dn[[1L]]) && length(dn[[1L]]) == maxlength)
        rownames(out) <- dn[[1L]]
    if (logicals) mode(out) <- "logical"
    out
}

file.edit <-
  function (..., title = file, editor=getOption("editor"))
{
    file <- path.expand(c(...))
    title <- rep(as.character(title), len=length(file))
    if (is.function(editor)) invisible(editor(file = file, title = title))
    else .Internal(file.edit(file, title, editor))
}

vi <- function(name=NULL, file="")
    edit.default(name, file, editor="vi")

emacs <- function(name=NULL, file="")
    edit.default(name, file, editor="emacs")

xemacs <- function(name=NULL, file="")
    edit.default(name, file, editor="xemacs")

xedit <- function(name=NULL, file="")
    edit.default(name, file, editor="xedit")

pico <- function(name=NULL, file="")
    edit.default(name, file, editor="pico")

#  File src/library/utils/R/example.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

example <-
function(topic, package = NULL, lib.loc = NULL, local = FALSE,
	 echo = TRUE, verbose = getOption("verbose"), setRNG = FALSE,
         ask = getOption("example.ask"),
	 prompt.prefix = abbreviate(topic, 6))
{
    topic <- substitute(topic)
    if(!is.character(topic))
	topic <- deparse(topic)[1L]
    INDICES <- .find.package(package, lib.loc, verbose = verbose)
    file <- index.search(topic, INDICES, "AnIndex", "R-ex")
    if(file == "") {
	warning(gettextf("no help file found for '%s'", topic), domain = NA)
	return(invisible())
    }
    packagePath <- dirname(dirname(file))
    if(length(file) > 1L) {
	packagePath <- packagePath[1L]
	warning(gettextf("more than one help file found: using package '%s'",
		basename(packagePath)), domain = NA)
	file <- file[1L]
    }
    pkg <- basename(packagePath)
    lib <- dirname(packagePath)
    encoding <- NULL
    ## first step, on-demand conversion, then look for (possibly zipped) file
    RdDB <- file.path(packagePath, "help", pkg)
    if(file.exists(paste(RdDB, "rdx", sep="."))) {
        zfile <- tempfile("Rex")
        encoding <- "UTF-8"
        ## FIXME: use outputEncoding="" ?
        ## FUTURE: we already have the parsed file ....
        tools::Rd2ex(tools:::fetchRdDB(RdDB, sub("\\.R$", "", basename(file))),
                     zfile)
    } else {
	Rexdir <- file.path(tempdir(), "Rex")
	dir.create(Rexdir, showWarnings=FALSE)
	zfile <- zip.file.extract(file, "Rex.zip", dir=Rexdir)
    }
    if(!file.exists(zfile)) {
        warning(gettextf("'%s' has a help file but no examples", topic),
                domain = NA)
        return(invisible())
    }
    if(zfile != file) on.exit(unlink(zfile))
    if(pkg != "base")
	library(pkg, lib.loc = lib, character.only = TRUE)
    if(!is.logical(setRNG) || setRNG) {
	## save current RNG state:
	if((exists(".Random.seed", envir = .GlobalEnv))) {
	    oldSeed <- get(".Random.seed", envir = .GlobalEnv)
	    on.exit(assign(".Random.seed", oldSeed, envir = .GlobalEnv),
                    add = TRUE)
	} else {
	    oldRNG <- RNGkind()
	    on.exit(RNGkind(oldRNG[1L], oldRNG[2L]), add = TRUE)
	}
	## set RNG
	if(is.logical(setRNG)) { # i.e. == TRUE: use the same as R CMD check
	    ## see share/R/examples-header.R
	    RNGkind("default", "default")
	    set.seed(1)
	} else eval(setRNG)
    }
    zz <- readLines(zfile, n=1L)
    if(is.null(encoding)) {
        encoding <-
            if(length(enc <- localeToCharset()) > 1L)
                c(enc[-length(enc)], "latin1")
            else ""
        ## peek at the file, but note we can't usefully translate to C.
        if(length(grep("^### Encoding: ", zz))  &&
           !identical(Sys.getlocale("LC_CTYPE"), "C"))
            encoding <- substring(zz, 15L)
    }
    skips <- 0L
    if (echo) {
	## skip over header
	zcon <- file(zfile, open="rt")
	while(length(zz) && !length(grep("^### \\*\\*", zz))) {
	    skips <- skips + 1L
	    zz <- readLines(zcon, n=1L)
	}
	close(zcon)
    }
    if(ask == "default")
        ask <- echo && grDevices::dev.interactive(orNone = TRUE)
    if(ask) {
	if(.Device != "null device") {
	    oldask <- grDevices::devAskNewPage(ask = TRUE)
            if(!oldask) on.exit(grDevices::devAskNewPage(oldask), add = TRUE)
        }
        ## <FIXME>
        ## This ensures that any device opened by the examples will
        ## have ask = TRUE set, but it does not return the device to
        ## the expected 'ask' state if it is left as the current device.
        ## </FIXME>
        op <- options(device.ask.default = TRUE)
        on.exit(options(op), add = TRUE)
    }
    source(zfile, local, echo = echo,
           prompt.echo = paste(prompt.prefix, getOption("prompt"), sep=""),
           continue.echo = paste(prompt.prefix, getOption("continue"), sep=""),
           verbose = verbose, max.deparse.length = Inf, encoding = encoding,
    	   skip.echo = skips, keep.source=TRUE)
}
#  File src/library/utils/R/filetest.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

### ** file_test

file_test <-
function(op, x, y)
{
    ## Provide shell-style '-f', '-d', '-x', '-nt' and '-ot' tests. 
    ## Note that file.exists() only tests existence ('test -e' on some
    ## systems), and that our '-f' tests for existence and not being a
    ## directory (the GNU variant tests for being a regular file).
    ## Note: vectorized in x and y.
    switch(op,
           "-f" = !is.na(isdir <- file.info(x)$isdir) & !isdir,
           "-d" = !is.na(isdir <- file.info(x)$isdir) & isdir,
           "-nt" = (!is.na(mt.x <- file.info(x)$mtime)
                    & !is.na(mt.y <- file.info(y)$mtime)
                    & (mt.x > mt.y)),
           "-ot" = (!is.na(mt.x <- file.info(x)$mtime)
                    & !is.na(mt.y <- file.info(y)$mtime)
                    & (mt.x < mt.y)),
           "-x" = (file.access(x, 1L) == 0L),
           stop(gettextf("test '%s' is not available", op),
                domain = NA))
}
#  File src/library/utils/R/fineLineNum.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright 2009 Duncan Murdoch and the R Core Development Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/


.normalizePath <- function(path, wd) {
    if (!missing(wd) && !is.null(wd)) {
    	oldwd <- setwd(wd)
    	on.exit(setwd(oldwd))
    }
    res <- tryCatch(normalizePath(path), error = identity)
    if (inherits(res, "error")) path
    else res
}

fnLineNum <- function(f, srcfile, line, nameonly=TRUE) {

    stopifnot(length(line) == 1)

    targetfilename <- .normalizePath(srcfile$filename)

    fnsrc <- attr(body(f), "srcfile")
    if (is.null(fnsrc)) return(NULL)

    if (missing(srcfile)) {
    	srcfile <- fnsrc
    }

    isBrace <- function(expr)
        typeof(expr) == "symbol" && identical(as.character(expr), "{")

    lineNumInExpr <- function(expr, haveSrcrefs = FALSE) {
	if (typeof(expr) == "language") {
	    srcrefs <- attr(expr, "srcref")
	    for (i in 1:length(expr)) {
		srcref <- srcrefs[[i]]
		# Check for non-matching range
		if (!is.null(srcref) && (srcref[1] > line || line > srcref[3]))  next
		# We're in range.  See if there's a finer division
		finer <- lineNumInExpr(expr[[i]], haveSrcrefs || !is.null(srcrefs))
		if (!is.null(finer)) {
		    return(c(i, finer))
		}
		# Do we have a srcref?  It must point to this expression.
		# But do avoid matching the opening brace in a block:  match the whole block
		# instead.

		havebrace <- isBrace(expr[[i]])
		if (!is.null(srcref)
		    && (!haveSrcrefs || !havebrace)) {
		    return(i)
		}
	    }
	}
	return(NULL)
    }

    perfectMatch <- identical(.normalizePath(fnsrc$filename, fnsrc$wd), targetfilename)
    if (perfectMatch ||
        (nameonly && !is.null(fnsrc$filename) && basename(fnsrc$filename) == basename(targetfilename))) {
	if (!is.na(srcfile$timestamp) && fnsrc$timestamp != srcfile$timestamp)
	    timediff <- fnsrc$timestamp - srcfile$timestamp
	else
	    timediff <- 0
	source <- attr(f, "source")
	at <- lineNumInExpr(body(f))
	if (!is.null(at))
	  return(list(at=at, filename=.normalizePath(fnsrc$filename, fnsrc$wd), line=line,
	              timediff=timediff))
    }
    return(NULL)
}

findLineNum <- function(srcfile, line, nameonly=TRUE, envir=parent.frame(), lastenv) {
    count <- 0
    result <- list()

    if (!inherits(srcfile, "srcfile")) {
    	if (missing(line)) {
    	    line <- as.numeric(sub(".*#", "", srcfile))
    	    if (is.na(line)) stop("Line number missing")
    	    srcfile <- sub("#[^#]*", "", srcfile)
    	}

    	srcfile <- srcfile(srcfile)
    }

    if (missing(lastenv)) {
    	if (missing(envir)) lastenv <- globalenv()
    	else lastenv <- emptyenv()
    }

    fns <- character(0)
    envirs <- list()
    e <- envir
    repeat {
    	fns <- c(fns, lsf.str(envir=e, all=TRUE))
    	oldlen <- length(envirs)
    	length(envirs) <- length(fns)
    	if (length(envirs) > oldlen)
    	    for (i in seq.int(oldlen+1, length(envirs))) envirs[[i]] <- e
    	if (identical(e, lastenv) || identical(e, emptyenv())) break
    	e <- parent.env(e)
    }

    for (i in seq_along(fns)) {
	functionName <- fns[i]
	fn <- get(functionName, envir=envirs[[i]])
	loc <- fnLineNum(fn, srcfile=srcfile, line=line,
    	                  nameonly=nameonly)
    	if (!is.null(loc)) {
    	    count <- count + 1
    	    result[[count]] <- c(list(name=functionName, env=envirs[[i]]), loc)
    	}
    	gen <- tryCatch(methods::isGeneric(functionName, envirs[[i]], fdef=fn),
                        error = identity)
    	if (isTRUE(gen)) {
    	    e1 <- environment(fn)$.AllMTable
    	    if (!is.null(e1)) {
		sigs <- ls(e1)
		for (j in seq_along(sigs)) {
		    sig <- sigs[j]
		    fn <- get(sig, e1)
		    if (typeof(fn) != "closure") next

		    loc <- fnLineNum(fn, srcfile=srcfile, line=line,
				    nameonly=nameonly)
		    if (is.null(loc)
		        && length(body(fn)) > 1
		        && length(body(fn)[[2]]) > 2
		        && typeof(body(fn)[[c(2,3)]]) == "closure") {
				# desperate try:  look for
		    		# .local <- original defn
		    	fn2 <- body(fn)[[c(2,3)]]

		    	loc <- fnLineNum(fn2, srcfile=srcfile, line=line,
				    nameonly=nameonly)
		 	# FIXME:  can trace() set a breakpoint
		 	#	  within a function like this?
		        if (!is.null(loc)) loc$at <- c(2,3)
		    }
		    if (!is.null(loc)) {
			count <- count + 1
			result[[count]] <- c(list(name=functionName, env=envirs[[i]],
						signature=strsplit(sig, "#")[[1]]), loc)
		    }
		}
	    }
	}
    }
    return(structure(result, class="findLineNumResult"))
}

print.findLineNumResult <- function(x, ...) {
    if (!length(x)) cat("No source refs found.\n")
    filename <- NULL
    line <- 0
    for (i in seq_along(x)) {
    	if (!identical(filename, x[[i]]$filename) ||
    	    !identical(line, x[[i]]$line)) {
    	    filename <- x[[i]]$filename
    	    line <- x[[i]]$line
    	    cat(filename, "#", line, ":\n", sep="")
    	}
        cat(" ", x[[i]]$name, " step ", paste(x[[i]]$at, collapse=","), sep="")
        if (!is.null(x[[i]]$signature))
            cat(" signature ", paste(x[[i]]$signature, collapse=","), sep="")
        cat(" in ", format(x[[i]]$env), "\n", sep="")
    }
}


setBreakpoint <- function(srcfile, line, nameonly=TRUE, envir=parent.frame(), lastenv,
                          verbose = TRUE, tracer, print=FALSE,
                         ...) {

    if (missing(lastenv)) {
    	if (missing(envir)) lastenv <- globalenv()
    	else lastenv <- emptyenv()
    }
    locations <- findLineNum(srcfile, line, nameonly, envir, lastenv)
    if (verbose) print(locations)
    breakpoint <- missing(tracer)
    while (length(locations)) {
    	what <- locations[[1]]$name
    	where <- locations[[1]]$env
    	at <- list(locations[[1]]$at)
    	signature <- locations[[1]]$signature
    	if (breakpoint) {
    	    filename <- basename(locations[[1]]$filename)
    	    linenum <- locations[[1]]$line
    	    tracer <- bquote({cat(paste(.(filename), "#", .(linenum), "\n", sep="")); browser()})
    	}
    	locations[[1]] <- NULL
        i <- 1
    	while (i <= length(locations)) {
    	    if (what == locations[[i]]$name &&
    	        identical(where, locations[[i]]$env) &&
    	        identical(signature, locations[[i]]$signature))	 {
    	    	at <- c(at, list(locations[[i]]))
    	    	locations[[i]] <- NULL
    	    } else
    	    	i <- i+1
    	}
    	if (is.null(signature))
    	    trace(what, tracer, at=at, where=where, print=print, ...)
    	else
    	    trace(what, signature=signature, tracer, at=at, where=where, ...)
    }
}
#  File src/library/utils/R/fix.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

"fix" <-
    function (x, ...)
{
    subx <- substitute(x)
    if (is.name(subx))
        subx <- deparse(subx)
    if (!is.character(subx) || length(subx) != 1L)
        stop("'fix' requires a name")
    parent <- parent.frame()
    if (exists(subx, envir=parent, inherits = TRUE))
        x <- edit(get(subx, envir=parent), title = subx, ...)
    else {
        x <- edit(function(){}, title = subx, ...)
        environment(x) <- .GlobalEnv
    }
    assign(subx, x, envir = .GlobalEnv)
}
#  File src/library/utils/R/format.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

formatUL <-
function(x, label = "*", offset = 0,
         width = 0.9 * getOption("width"))
{
    if(!length(x))
        return(character())
    .format_rl_table(label, x, offset, width)
}

formatOL <-
function(x, type = "arabic", offset = 0, start = 1,
         width = 0.9 * getOption("width"))
{
    if(!length(x))
        return(character())
    type_tokens <- c("1", "A", "a", "I", "i")
    type_full_names <- c("arabic", "Alph", "alph", "Roman", "roman")
    type <- match.arg(type, c(type_tokens, type_full_names))
    if(nchar(type, "b") > 1L)
        type <- type_tokens[match(type, type_full_names)]
    len <- length(x)
    labels <- seq.int(start[1L], length.out = len)
    upper <- labels[len]
    if(type %in% c("A", "a")) {
        if(upper > 26L)
            stop("too many list items (at most up to number 26)")
        labels <- if(type == "A")
            LETTERS[labels]
        else
            letters[labels]
    }
    else if(type %in% c("I", "i")) {
        if(upper > 3899L)
            stop("too many list items (at most up to number 3899)")
        labels <- as.character(as.roman(labels))
        if(type == "i")
            labels <- tolower(labels)
    }
    .format_rl_table(sprintf("%s.", labels), x, offset, width)
}

.format_rl_table <-
function(labels, x, offset = 0, width = 0.9 * getOption("width"),
         sep = " ")
{
    ## Format a 2-column table with right-justified item labels and
    ## left-justified text.  Somewhat tricky because strwrap() eats up
    ## leading whitespace ...

    .make_empty_string <- function(n) {
        paste(rep.int(" ", n), collapse = "")
    }

    labels <- format(labels, justify = "right")
    len <- length(x)
    delta <- nchar(labels[1L], "width") + offset
    x <- strwrap(x, width = width - delta - nchar(sep, "width"),
                 simplify = FALSE)
    nlines <- cumsum(sapply(x, length))
    prefix <- rep.int(.make_empty_string(delta), nlines[len])
    prefix[1L + c(0L, nlines[-len])] <-
        paste(.make_empty_string(offset), labels, sep = "")
    paste(prefix, unlist(x), sep = sep)
}
#  File src/library/utils/R/frametools.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

stack <- function(x, ...) UseMethod("stack")

stack.data.frame <- function(x, select, ...)
{
    if (!missing(select)) {
	nl <- as.list(1L:ncol(x))
	names(nl) <- names(x)
	vars <- eval(substitute(select),nl, parent.frame())
        x <- x[, vars, drop=FALSE]
    }
    x <- x[, unlist(lapply(x, is.vector)), drop = FALSE]
    data.frame(values = unlist(unname(x)),
               ind = factor(rep.int(names(x), lapply(x, length))))
}

stack.default <- function(x, ...)
{
    x <- as.list(x)
    x <- x[unlist(lapply(x, is.vector))]
    data.frame(values = unlist(unname(x)),
               ind = factor(rep.int(names(x), lapply(x, length))))
}

unstack <- function(x, ...) UseMethod("unstack")

unstack.data.frame <- function(x, form, ...)
{
    form <- if(missing(form)) stats::formula(x) else stats::as.formula(form)
    if (length(form) < 3)
        stop("'form' must be a two-sided formula")
    res <- c(tapply(eval(form[[2L]], x), eval(form[[3L]], x), as.vector))
    if (length(res) < 2L || any(diff(unlist(lapply(res, length))) != 0L))
        return(res)
    data.frame(res)
}

unstack.default <- function(x, form, ...)
{
    x <- as.list(x)
    form <- stats::as.formula(form)
    if ((length(form) < 3) || (length(all.vars(form))>2))
        stop("'form' must be a two-sided formula with one term on each side")
    res <- c(tapply(eval(form[[2L]], x), eval(form[[3L]], x), as.vector))
    if (length(res) < 2L || any(diff(unlist(lapply(res, length))) != 0L))
        return(res)
    data.frame(res)
}
#  File src/library/utils/R/glob2rx.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

glob2rx <- function(pattern, trim.head = FALSE, trim.tail = TRUE)
{
    ## Purpose: Change 'ls' aka 'wildcard' aka 'globbing' _pattern_ to
    ##	      Regular Expression (as in grep, perl, emacs, ...)
    ## -------------------------------------------------------------------------
    ## Author: Martin Maechler ETH Zurich, ~ 1991
    ##	       New version using [g]sub() : 2004
    p <- gsub("\\.","\\\\.", paste("^", pattern, "$", sep=""))
    p <- gsub("\\?",	 ".",  gsub("\\*",  ".*", p))
    ## 'Escaping hell' : at least for '(', '[' and '{'
    p <- gsub("([^\\])\\(", "\\1\\\\(", p)
    p <- gsub("([^\\])\\[", "\\1\\\\[", p)
    p <- gsub("([^\\])\\{", "\\1\\\\{", p)
    ## these are trimming ".*$" and "^.*" - in most cases only for aesthetics
    if(trim.tail) p <- sub("\\.\\*\\$$", "", p)
    if(trim.head) p <- sub("\\^\\.\\*",  "", p)
    p
}
#  File src/library/utils/R/head.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

### placed in the public domain 2002
### Patrick Burns patrick@burns-stat.com
###
### Adapted for negative arguments by Vincent Goulet
### <vincent.goulet@act.ulaval.ca>, 2006

head <- function(x, ...) UseMethod("head")

head.default <- function(x, n = 6L, ...)
{
    stopifnot(length(n) == 1L)
    n <- if (n < 0L) max(length(x) + n, 0L) else min(n, length(x))
    x[seq_len(n)]
}

## head.matrix and tail.matrix are now exported (to be used for other classes)
head.data.frame <- head.matrix <- function(x, n = 6L, ...)
{
    stopifnot(length(n) == 1L)
    n <- if (n < 0L) max(nrow(x) + n, 0L) else min(n, nrow(x))
    x[seq_len(n), , drop=FALSE]
}
head.table  <- function(x, n = 6L, ...) {
    (if(length(dim(x)) == 2L) head.matrix else head.default)(x, n=n)
}

head.ftable <- function(x, n = 6L, ...) {
    r <- format(x)
    dimnames(r) <- list(rep.int("", nrow(r)), rep.int("", ncol(r)))
    noquote(head.matrix(r, n = n + nrow(r) - nrow(x), ...))
}

head.function <- function(x, n = 6L, ...)
{
    lines <- as.matrix(deparse(x))
    dimnames(lines) <- list(seq_along(lines),"")
    noquote(head(lines, n=n))
}

tail <- function(x, ...) UseMethod("tail")

tail.default <- function(x, n = 6L, ...)
{
    stopifnot(length(n) == 1L)
    xlen <- length(x)
    n <- if (n < 0L) max(xlen + n, 0L) else min(n, xlen)
    x[seq.int(to = xlen, length.out = n)]
}

tail.data.frame <- function(x, n = 6L, ...)
{
    stopifnot(length(n) == 1L)
    nrx <- nrow(x)
    n <- if (n < 0L) max(nrx + n, 0L) else min(n, nrx)
    x[seq.int(to = nrx, length.out = n), , drop = FALSE]
}

tail.matrix <- function(x, n = 6L, addrownums = TRUE, ...)
{
    stopifnot(length(n) == 1L)
    nrx <- nrow(x)
    n <- if (n < 0L) max(nrx + n, 0L) else min(n, nrx)
    sel <- seq.int(to = nrx, length.out = n)
    ans <- x[sel, , drop = FALSE]
    if (addrownums && is.null(rownames(x)))
    	rownames(ans) <- paste("[", sel, ",]", sep="")
    ans
}
tail.table  <- function(x, n = 6L, addrownums = TRUE, ...) {
    (if(length(dim(x)) == 2L) tail.matrix else tail.default)(x, n=n,
	      addrownums = addrownums, ...)
}

tail.ftable <- function(x, n = 6L, addrownums = FALSE, ...) {
    r <- format(x)
    dimnames(r) <- list(if(!addrownums) rep.int("", nrow(r)),
			rep.int("", ncol(r)))
    noquote(tail.matrix(r, n = n, addrownums = addrownums, ...))
}

tail.function <- function(x, n = 6L, ...)
{
    lines <- as.matrix(deparse(x))
    dimnames(lines) <- list(seq_along(lines),"")
    noquote(tail(lines, n=n))
}
#  File src/library/utils/R/help.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

help <-
function(topic, package = NULL, lib.loc = NULL,
         verbose = getOption("verbose"),
         try.all.packages = getOption("help.try.all.packages"),
         help_type = getOption("help_type"),
         chmhelp = getOption("chmhelp"),
         htmlhelp = getOption("htmlhelp"),
         offline = FALSE)
{
    if(!missing(package))
        if(is.name(y <- substitute(package)))
            package <- as.character(y)

    ## If no topic was given ...
    if(missing(topic)) {
        if(!missing(package))           # "Help" on package.
            return(library(help = package, lib.loc = lib.loc,
                           character.only = TRUE))
        if(!missing(lib.loc))           # "Help" on library.
            return(library(lib.loc = lib.loc))
        ## ultimate default is to give help on help
        topic <- "help"; package <- "utils"; lib.loc <- .Library
    }

    ischar <- tryCatch(is.character(topic) && length(topic) == 1L,
                       error = identity)
    if(inherits(ischar, "error")) ischar <- FALSE
    ## if this was not a length-one character vector, try for the name.
    if(!ischar) {
        ## the reserved words that could be parsed as a help arg:
        reserved <-
            c("TRUE", "FALSE", "NULL", "Inf", "NaN", "NA", "NA_integer_",
              "NA_real_", "NA_complex_", "NA_character_")
        stopic <- deparse(substitute(topic))
        if(!is.name(substitute(topic)) && ! stopic %in% reserved)
            stop("'topic' should be a name, length-one character vector or reserved word")
        topic <- stopic
    }

    help_type <- if(!length(help_type)) {
        if(offline) {
            warning('offline = TRUE is deprecated: use help_type ="postscript"')
            "ps"
        } else if(.Platform$OS.type == "windows" &&
                is.logical(chmhelp) && !is.na(chmhelp) && chmhelp) {
            warning('chmhelp = TRUE is no longer supported: using help_type ="text"')
            "text"
        } else if(is.logical(htmlhelp) && !is.na(htmlhelp) && htmlhelp) {
            warning('htmhelp = TRUE is deprecated: use help_type ="html"')
            "html"
        } else
            "text"
    } else match.arg(tolower(help_type),
                     c("text", "html", "postscript", "ps", "pdf"))
    type <- switch(help_type,
                   "text" = "help",
                   "postscript" =,
                   "ps" =,
                   "pdf" = "latex",
                   help_type)

    ## Note that index.search() (currently?) only returns the first
    ## match for the given sequence of indices, and returns the empty
    ## string in case of no match.
    paths <- sapply(.find.package(package, lib.loc, verbose = verbose),
                    function(p) index.search(topic, p, "AnIndex", type))
    paths <- paths[paths != ""]

    tried_all_packages <- FALSE
    if(!length(paths)
       && is.logical(try.all.packages) && !is.na(try.all.packages)
       && try.all.packages && missing(package) && missing(lib.loc)) {
        ## Try all the remaining packages.
        lib.loc <- .libPaths()
        packages <- .packages(all.available = TRUE, lib.loc = lib.loc)
        packages <- packages[is.na(match(packages, .packages()))]
        for(lib in lib.loc) {
            ## <FIXME>
            ## Why does this loop over packages *inside* the loop
            ## over libraries?
            for(pkg in packages) {
                dir <- system.file(package = pkg, lib.loc = lib)
                paths <- c(paths,
                           index.search(topic, dir, "AnIndex", "help"))
            }
            ## </FIXME>
        }
        paths <- paths[paths != ""]
        tried_all_packages <- TRUE
    }

    paths <- unique(paths)
    attributes(paths) <-
        list(call = match.call(), topic = topic,
             tried_all_packages = tried_all_packages, type = help_type)
    class(paths) <- "help_files_with_topic"
    paths
}

print.help_files_with_topic <-
function(x, ...)
{
    browser <- getOption("browser")
    topic <- attr(x, "topic")
    type <- attr(x, "type")
    if (.Platform$GUI == "AQUA" && type == "html") {
        browser <- function(x, ...) {
            .Internal(aqua.custom.print("help-files", x))
    	    return(invisible(x))
        }
    }
    paths <- as.character(x)
    if(!length(paths)) {
        writeLines(c(gettextf("No documentation for '%s' in specified packages and libraries:",
                              topic),
                     gettextf("you could try '??%s'",
                              topic)))
        return(invisible(x))
    }

    if(type == "html")
        if (tools:::httpdPort == 0L) tools::startDynamicHelp()

    if(attr(x, "tried_all_packages")) {
        paths <- unique(dirname(dirname(paths)))
        msg <- gettextf("Help for topic '%s' is not in any loaded package but can be found in the following packages:",
                        topic)
        if (type == "html" && tools:::httpdPort > 0L) {
            path <- file.path(tempdir(), ".R/doc/html")
            dir.create(path, recursive = TRUE, showWarnings = FALSE)
            out <- paste('<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">\n',
                         '<html><head><title>R: help</title>\n',
                         '<meta http-equiv="Content-Type" content="text/html; charset="UTF-8">\n',
                         '<link rel="stylesheet" type="text/css" href="/doc/html/R.css">\n',
                         '</head><body>\n\n<hr>\n', sep="")
            out <- c(out, '<p>', msg, '</p><br>')
            out <- c(out, '<table width="100%" summary="R Package list">\n',
                     '<tr align="left" valign="top">\n',
                     '<td width="25%">Package</td><td>Library</td></tr>\n')
            pkgs <- basename(paths)
            links <- paste('<a href="http://127.0.0.1:', tools:::httpdPort,
                           '/library/', pkgs, '/help/', topic, '">',
                           pkgs, '</a>', sep = "")
            out <- c(out, paste('<tr align="left" valign="top">\n',
                                '<td>', links, '</td><td>',
                                dirname(paths), '</td></tr>\n',
                                sep = ""))
            out <- c(out, "</table>\n</p>\n<hr>\n</body></html>")
            writeLines(out, file.path(path, "all.available.html"))
            browseURL(paste("http://127.0.0.1:", tools:::httpdPort,
                            "/doc/html/all.available.html", sep=""), browser)
        } else {
            writeLines(c(strwrap(msg), "",
                         paste(" ",
                               formatDL(c(gettext("Package"), basename(paths)),
                                        c(gettext("Library"), dirname(paths)),
                                        indent = 22))))
        }
    } else {
        if(length(paths) > 1L) {
            if (type == "html" && tools:::httpdPort > 0L) { # Redo the search if dynamic help is running
		browseURL(paste("http://127.0.0.1:", tools:::httpdPort,
                                "/library/NULL/help/", topic, sep=""), browser)
		return(invisible(x))
	    }
            file <- paths[1L]
            p <- paths
            msg <- gettextf("Help on topic '%s' was found in the following packages:",
                            topic)
            paths <- dirname(dirname(paths))
            txt <- formatDL(c("Package", basename(paths)),
                            c("Library", dirname(paths)),
                            indent = 22L)
            writeLines(c(strwrap(msg), "", paste(" ", txt), ""))
            if(interactive()) {
                fp <- file.path(paths, "Meta", "Rd.rds")
                tp <- basename(p)
                titles <- tp
                if(type == "html" || type == "latex")
                    tp <- tools::file_path_sans_ext(tp)
                for (i in seq_along(fp)) {
                    tmp <- try(.readRDS(fp[i]))
                    titles[i] <- if(inherits(tmp, "try-error"))
                        "unknown title" else
                    tmp[tools::file_path_sans_ext(tmp$File) == tp[i], "Title"]
                }
                txt <- paste(titles, " {", basename(paths), "}", sep="")
                ## FIXME: use html page for HTML help.
                res <- menu(txt, title = gettext("Choose one"),
                            graphics = getOption("menu.graphics"))
                if(res > 0) file <- p[res]
            } else {
                writeLines(gettext("\nUsing the first match ..."))
            }
        }
        else
            file <- paths

        if(type == "html") {
            if (tools:::httpdPort > 0L) {
		path <- dirname(file)
		dirpath <- dirname(path)
		pkgname <- basename(dirpath)
		browseURL(paste("http://127.0.0.1:", tools:::httpdPort,
                                "/library/", pkgname, "/html/", basename(file),
                                sep=""), browser)
            } else {
                warning("HTML help is unavailable", call. = FALSE)
                att <- attributes(x)
                xx <- sub("/html/([^/]*)\\.html$", "/help/\\1", x)
                attributes(xx) <- att
                attr(xx, "type") <- "text"
                print(xx)
            }
        } else if(type == "text") {
            path <- dirname(file)
            dirpath <- dirname(path)
            pkgname <- basename(dirpath)
            RdDB <- file.path(path, pkgname)
            if(file.exists(paste(RdDB, "rdx", sep="."))) {
                temp <- tools::Rd2txt(tools:::fetchRdDB(RdDB, basename(file)),
                                      out=tempfile("Rtxt"), package=pkgname)
                file.show(temp,
                          title = gettextf("R Help on '%s'", topic),
                          delete.file = TRUE)
            } else {
                ## Fallback to the old system with help pages
                ## stored as plain text
                zfile <- zip.file.extract(file, "Rhelp.zip")
                if (file.exists(zfile)) {
                    first <- readLines(zfile, n = 1L)
                    enc <- if(length(grep("\\(.*\\)$", first)))
                        sub("[^(]*\\((.*)\\)$", "\\1", first) else ""
                    if(enc == "utf8") enc <- "UTF-8"
                    ## allow for 'smart' quotes on Windows, which work
                    ## in all but CJK encodings
                    if(.Platform$OS.type == "windows" && enc == ""
                       && l10n_info()$codepage < 1000) enc <- "CP1252"
                    file.show(zfile,
                              title = gettextf("R Help on '%s'", topic),
                              delete.file = (zfile != file),
                              encoding = enc)
                } else
		    stop(gettextf("No text help for '%s' is available:\ncorresponding file is missing", topic), domain = NA)
            }
        }
        else if(type %in% c("ps", "postscript", "pdf")) {
            ok <- FALSE
            zfile <- zip.file.extract(file, "Rhelp.zip")
            if(zfile != file) on.exit(unlink(zfile))
            if(file.exists(zfile)) {
                .show_help_on_topic_offline(zfile, topic, type)
                ok <- TRUE
            } else {
                ## look for stored Rd files
                path <- dirname(file) # .../pkg/latex
                dirpath <- dirname(path)
                pkgname <- basename(dirpath)
                RdDB <- file.path(dirpath, "help", pkgname)
                if(file.exists(paste(RdDB, "rdx", sep="."))) {
                    ## message("on-demand Rd conversion for ", sQuote(topic))
                    key <- sub("\\.tex$", "", basename(file))
                    tf2 <- tempfile("Rlatex")
                    tools::Rd2latex(tools:::fetchRdDB(RdDB, key), tf2)
                    .show_help_on_topic_offline(tf2, topic, type)
                    ok <- TRUE
                }
            }
            if(!ok)
                stop(gettextf("No offline help for '%s' is available:\ncorresponding file is missing", topic), domain = NA)
        }
    }

    invisible(x)
}

.show_help_on_topic_offline <- function(file, topic, type = "postscript")
{
    encoding <-""
    lines <- readLines(file)
    encpatt <- "^\\\\inputencoding\\{(.*)\\}$"
    if(length(res <- grep(encpatt, lines, perl = TRUE, useBytes = TRUE)))
        encoding <- sub(encpatt, "\\1", lines[res],
                        perl = TRUE, useBytes = TRUE)
    texfile <- paste(topic, ".tex", sep = "")
    on.exit(unlink(texfile)) ## ? leave to helper
    opt <- if(type == "PDF") {
        if(nzchar(opt <- Sys.getenv("R_RD4PDF"))) opt else "times"
    } else {
        if(nzchar(opt <- Sys.getenv("R_RD4DVI"))) opt else "ae"
    }
    cat("\\documentclass[", getOption("papersize"), "paper]{article}\n",
        "\\usepackage[", opt, "]{Rd}\n",
        if(nzchar(encoding)) sprintf("\\usepackage[%s]{inputenc}\n", encoding),
        "\\InputIfFileExists{Rhelp.cfg}{}{}\n",
        "\\begin{document}\n",
        file = texfile, sep = "")
    file.append(texfile, file)
    cat("\\end{document}\n", file = texfile, append = TRUE)
    helper <- if (exists("offline_help_helper", envir = .GlobalEnv))
        get("offline_help_helper", envir = .GlobalEnv)
    else utils:::offline_help_helper
    helper(texfile, type)
    invisible()
}
help.request <- function (subject = "",
			  ccaddress = Sys.getenv("USER"),
			  method = getOption("mailer"),
			  address = "r-help@R-project.org",
			  file = "R.help.request")
{
    no <- function(answer) answer == "n"
    yes <- function(answer) answer == "y"
    webpage <- "corresponding web page"
    catPlease <- function()
	cat("Please do this first - the",
	    webpage,"has been loaded in your web browser\n")
    go <- function(url) {
	catPlease()
	browseURL(url)
    }
    readMyLine <- function(..., .A. = "(y/n)")
	readline(paste(paste(strwrap(paste(...)), collapse="\n"),
		       .A., "")) # space after question

    checkPkgs <- function(pkgDescs,
			  pkgtxt = paste("packages",
                          paste(names(pkgDescs), collapse=", ")))
    {
        cat("Checking if", pkgtxt, "are up-to-date; may take some time...\n")

        stopifnot(sapply(pkgDescs, inherits, what="packageDescription"))
        fields <- .instPkgFields(NULL)
	n <- length(pkgDescs)
	iPkgs <- matrix(NA_character_, n, 2L + length(fields),
		      dimnames=list(NULL, c("Package", "LibPath", fields)))
	for(i in seq_len(n)) {
	    desc <- c(unlist(pkgDescs[[i]]),
		      "LibPath" = dirname(dirname(dirname(attr(pkgDescs[[i]],
		      "file")))))
	    nms <- intersect(names(desc), colnames(iPkgs))
	    iPkgs[i, nms] <- desc[nms]
	}

	old <- old.packages(instPkgs = iPkgs)

	if (!is.null(old)) {
	    update <- readMyLine("The following installed packages are out-of-date:\n",
				 paste(strwrap(rownames(old),
					       width = 0.7 *getOption("width"),
					       indent= 0.15*getOption("width")),
				       collapse="\n"),
				 "would you like to update now?")
	    if (yes(update)) update.packages(oldPkgs = old, ask = FALSE)
	}
    }

    cat("Checklist:\n")
    post <- readline("Have you read the posting guide? (y/n) ")
    if (no(post)) return(go("http://www.r-project.org/posting-guide.html"))
    FAQ <- readline("Have you checked the FAQ? (y/n) ")
    if (no(FAQ)) return(go("http://cran.r-project.org/faqs.html"))
    intro <- readline("Have you checked An Introduction to R? (y/n) ")
    if (no(intro))
	return(go("http://cran.r-project.org/doc/manuals/R-intro.html"))
    NEWS <- readMyLine("Have you checked the NEWS of the latest development release?")
    if (no(NEWS)) return(go("https://svn.r-project.org/R/trunk/NEWS"))
    rsitesearch <- readline("Have you looked on RSiteSearch? (y/n) ")
    if (no(rsitesearch)) {
	catPlease()
	return(RSiteSearch(subject))
    }
    inf <- sessionInfo()
    if ("otherPkgs" %in% names(inf)) {
	oPkgs <- names(inf$otherPkgs)
        ## FIXME: inf$otherPkgs is a list of packageDescription()s
	other <-
	    readMyLine("You have packages",
                       paste("(", paste(sQuote(oPkgs), collapse=", "),")", sep=""),
                       "other than the base packages loaded. ",
		       "If your query relates to one of these, have you ",
		       "checked any corresponding books/manuals and",
		       "considered contacting the package maintainer?",
                       .A. = "(y/n/NA)")
	if(no(other)) return("Please do this first.")
    }

    man <- url("http://cran.r-project.org/manuals.html")
    ver <- scan(man, what = character(0L), sep = "\n", skip = 13L, nlines = 1L,
		quiet = TRUE)
    ver <- strsplit(ver, " ")[[1L]][3L]
    if (getRversion() < numeric_version(ver)) {
	update <- readMyLine("Your R version is out-of-date,",
			     "would you like to update now?")
	if(yes(update)) return(go(getOption("repos")))
    }
    if ("otherPkgs" %in% names(inf)) {
        checkPkgs(inf$otherPkgs)
    }
    ## To get long prompt!
    cat("Have you written example code that is\n",
	"- minimal\n - reproducible\n - self-contained\n - commented",
	"\nusing data that is either\n",
	"- constructed by the code\n - loaded by data()\n",
	"- reproduced using dump(\"mydata\", file = \"\")\n")
    code <- readMyLine("have you checked this code in a fresh R session",
		       "(invoking R with the --vanilla option if possible)",
		       "and is this code copied to the clipboard?")
    if (no(code))
	return(cat("\nIf your query is not directly related to code",
		   "(e.g. a general query \nabout R's capabilities),",
		   "email R-help@r-project.org directly. ",
		   "\nOtherwise prepare some example code first.\n"))
    change <- readline(paste("Would you like to change your subject line:",
			     subject, "to something more meaningful? (y/n) ",
			     sep = "\n"))
    if (yes(change))
	subject <- readline("Enter subject: \n")

    create.post(instructions = paste(
		"\\n<<SEND AS PLAIN TEXT!>>\\n\\n",
		"\\n<<Write your query here, using your example code to illustrate>>",
		"\\n<<End with your name and affiliation>>\\n\\n\\n\\n"),
		description = "help request",
		subject = subject,
		ccaddress = ccaddress,
		method = method,
		address = address,
		file = file)
}
#  File src/library/utils/R/help.search.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

.hsearch_db <- local({
    hdb <- NULL
    function(new) {
	if(!missing(new))
	    hdb <<- new
	else
	    hdb
    }
})

## FIXME: use UTF-8, either always or optionally
## (Needs UTF-8-savvy & fast agrep, and PCRE regexps.)
help.search <-
    function(pattern, fields = c("alias", "concept", "title"),
             apropos, keyword, whatis, ignore.case = TRUE,
             package = NULL, lib.loc = NULL,
             help.db = getOption("help.db"),
             verbose = getOption("verbose"),
             rebuild = FALSE, agrep = NULL, use_UTF8 = FALSE)
{
    WINDOWS <- .Platform$OS.type == "windows"

    ### Argument handling.
    TABLE <- c("alias", "concept", "keyword", "name", "title")

    if (is.logical(verbose)) verbose <- 2*as.integer(verbose)
    .wrong_args <- function(args)
	gettextf("argument '%s' must be a single character string", args)

    if(!missing(pattern)) {
	if(!is.character(pattern) || (length(pattern) > 1L))
	    stop(.wrong_args("pattern"), domain = NA)
	i <- pmatch(fields, TABLE)
	if(any(is.na(i)))
	    stop("incorrect field specification")
	else
	    fields <- TABLE[i]
    } else if(!missing(apropos)) {
	if(!is.character(apropos) || (length(apropos) > 1L))
	    stop(.wrong_args("apropos"), domain = NA)
	else {
	    pattern <- apropos
	    fields <- c("alias", "title")
	}
    } else if(!missing(keyword)) {
	if(!is.character(keyword) || (length(keyword) > 1L))
	    stop(.wrong_args("keyword"), domain = NA)
	else {
	    pattern <- keyword
	    fields <- "keyword"
	    if(is.null(agrep)) agrep <- FALSE
	}
    } else if(!missing(whatis)) {
	if(!is.character(whatis) || (length(whatis) > 1))
	    stop(.wrong_args("whatis"), domain = NA)
	else {
	    pattern <- whatis
	    fields <- "alias"
	}
    } else {
	stop("do not know what to search")
    }

    if(is.null(lib.loc))
	lib.loc <- .libPaths()

    if(!missing(help.db))
	warning("argument 'help.db' is deprecated")


    ### Set up the hsearch db.
    db <- eval(.hsearch_db())
    if(is.null(db))
	rebuild <- TRUE
    else if(!rebuild) {
	## Need to find out whether this has the info we need.
	## Note that when looking for packages in libraries we always
	## use the first location found.  Hence if the library search
	## path changes we might find different versions of a package.
	## Thus we need to rebuild the hsearch db in case the specified
	## library path is different from the one used when building the
	## hsearch db (stored as its "LibPaths" attribute).
	if(!identical(lib.loc, attr(db, "LibPaths")) ||
	   ## We also need to rebuild the hsearch db in case an existing
	   ## dir in the library path was modified more recently than
	   ## the db, as packages might have been installed or removed.
	   any(attr(db, "mtime") <
	       file.info(lib.loc[file.exists(lib.loc)])$mtime) ||
	   ## Or if the user changed the locale character type ...
	   !identical(attr(db, "ctype"), Sys.getlocale("LC_CTYPE"))
	   )
	    rebuild <- TRUE
        ## We also need to rebuild if 'packages' was used before and has
        ## changed.
        if (!is.null(package) &&
            any(! package %in% db$Base[, "Package"]))
            rebuild <- TRUE
    }
    if(rebuild) {
	if(verbose > 0L) {
            message("Rebuilding the help.search() database", " ", "...",
                    if(verbose > 1L) "...")
            flush.console()
        }
	## Check whether we can save the hsearch db later on.
	if(all(is.na(mem.limits()))) {
	    save_db <- save_db_to_memory <- TRUE
	} else {
	    save_db <- save_db_to_memory <- FALSE
	    dir <- file.path(tempdir(), ".R")
	    db_file <- file.path(dir, "hsearch.rds")
	    if( (file_test("-d", dir)
                 || ((unlink(dir) == 0L) && dir.create(dir)) )
	       && (unlink(db_file) == 0L) )
		save_db <- TRUE
	}

	if(!is.null(package)) {
	    packages_in_hsearch_db <- package
            package_paths <- NULL
	} else {
            ## local version of .packages(all.available = TRUE),
            ## recording paths
            ans <- character(0L); paths <- character(0L)
            lib.loc <- lib.loc[file.exists(lib.loc)]
            valid_package_version_regexp <-
                .standard_regexps()$valid_package_version
            for (lib in lib.loc) {
                a <- list.files(lib, all.files = FALSE, full.names = FALSE)
                for (nam in a) {
                    pfile <- file.path(lib, nam, "Meta", "package.rds")
                    if (file.exists(pfile))
                        info <- .readRDS(pfile)$DESCRIPTION[c("Package", "Version")]
                    else next
                    if ( (length(info) != 2L) || any(is.na(info)) ) next
                    if (!grepl(valid_package_version_regexp, info["Version"])) next
                    ans <- c(ans, nam)
                    paths <- c(paths, file.path(lib, nam))
                }
            }
            un <- !duplicated(ans)
	    packages_in_hsearch_db <-  ans[un]
            package_paths <- paths[un]
            names(package_paths) <- ans[un]
        }

	## Create the hsearch db.
	np <- 0L
	if(verbose >= 2L) {
	    message("Packages {.readRDS() sequentially}:")
            flush.console()
        }
        tot <- length(package_paths)
        incr <- 0L
        if(verbose && WINDOWS) {
            pb <- winProgressBar("R: creating the help.search() DB", max = tot)
            on.exit(close(pb))
        } else if(verbose == 1L) incr <- ifelse(tot > 500L, 100L, 10L)

	## Starting with R 1.8.0, prebuilt hsearch indices are available
	## in Meta/hsearch.rds, and the code to build this from the Rd
	## contents (as obtained from both new and old style Rd indices)
	## has been moved to tools:::.build_hsearch_index() which
	## creates a per-package list of base, aliases and keywords
	## information.	 When building the global index, it seems (see
	## e.g. also the code in tools:::Rdcontents()), most efficient to
	## create a list *matrix* (dbMat below), stuff the individual
	## indices into its rows, and finally create the base, alias,
	## keyword, and concept information in rbind() calls on the
	## columns.  This is *much* more efficient than building
	## incrementally.
	dbMat <- vector("list", length(packages_in_hsearch_db) * 4L)
	dim(dbMat) <- c(length(packages_in_hsearch_db), 4L)
	defunct_standard_package_names <-
	    tools:::.get_standard_package_names()$stubs

	for(p in packages_in_hsearch_db) {
            if(incr && np %% incr == 0L) {
                message(".", appendLF = FALSE)
                flush.console()
            }
	    np <- np + 1L
            if(verbose && WINDOWS) setWinProgressBar(pb, np)
	    if(verbose >= 2L) {
		message(" ", p, appendLF = ((np %% 5L) == 0L), domain=NA)
                flush.console()
            }
            path <- if(!is.null(package_paths)) package_paths[p]
	    else .find.package(p, lib.loc, quiet = TRUE)
	    if(length(path) == 0L) {
                if(is.null(package)) next
		else stop(gettextf("could not find package '%s'", p), domain = NA)
            }
	    ## Hsearch 'Meta/hsearch.rds' indices were introduced in
	    ## R 1.8.0.	 If they are missing, we really cannot use
	    ## the package (as library() will refuse to load it).
	    if(file.exists(hs_file <- file.path(path, "Meta", "hsearch.rds"))) {
		hDB <- .readRDS(hs_file)
		if(!is.null(hDB)) {
		    ## Fill up possibly missing information.
		    if(is.na(match("Encoding", colnames(hDB[[1L]]))))
			hDB[[1L]] <- cbind(hDB[[1L]], Encoding = "")
		    hDB[[1L]][, "LibPath"] <- path
		    ## Put the hsearch index for the np-th package into the
		    ## np-th row of the matrix used for aggregating.
		    dbMat[np, seq_along(hDB)] <- hDB
		} else if(verbose >= 2L) {
		    message(gettextf("package '%s' has empty hsearch data - strangely", p),
                            domain = NA)
                    flush.console()
                }
	    }
	    else if(!is.null(package))
                warning("no hsearch.rds meta data for package ", p)
	}

	if(verbose >= 2L)  {
	    message(ifelse(np %% 5L == 0L, "\n", "\n\n"),
                    sprintf("Built dbMat[%d,%d]", nrow(dbMat), ncol(dbMat)),
                    domain = NA)
            flush.console()
            ## DEBUG save(dbMat, file="~/R/hsearch_dbMat.rda", compress=TRUE)
        }

	## workaround methods:::rbind() misbehavior:
	if(.isMethodsDispatchOn()) {
	    bind_was_on <- methods:::bind_activation(FALSE)
	    if(bind_was_on) on.exit(methods:::bind_activation(TRUE))
	}

	## Create the global base, aliases, keywords and concepts tables
	## via calls to rbind() on the columns of the matrix used for
	## aggregating.
	db <- list(Base     = do.call("rbind", dbMat[, 1]),
		   Aliases  = do.call("rbind", dbMat[, 2]),
		   Keywords = do.call("rbind", dbMat[, 3]),
		   Concepts = do.call("rbind", dbMat[, 4]))
	if(is.null(db$Concepts))
	    db$Concepts <-
		matrix(character(), ncol = 3L,
		       dimnames = list(NULL,
		       c("Concepts", "ID", "Package")))
	## Make the IDs globally unique by prefixing them with the
	## number of the package in the global index.
	for(i in which(sapply(db, NROW) > 0L)) {
	    db[[i]][, "ID"] <-
		paste(rep.int(seq_along(packages_in_hsearch_db),
			      sapply(dbMat[, i], NROW)),
		      db[[i]][, "ID"],
		      sep = "/")
	}
	## And maybe re-encode ...
	if(!identical(Sys.getlocale("LC_CTYPE"), "C")) {
	    if(verbose >= 2L) {
                message("reencoding ...", appendLF=FALSE)
                flush.console()
            }
	    encoding <- db$Base[, "Encoding"]
            target <- ifelse(use_UTF8 && !l10n_info()$`UTF-8`, "UTF-8", "")
	    ## As iconv is not vectorized in the 'from' argument, loop
	    ## over groups of identical encodings.
	    for(enc in unique(encoding)) {
                if(enc != target) next
		IDs <- db$Base[encoding == enc, "ID"]
		for(i in seq_along(db)) {
		    ind <- db[[i]][, "ID"] %in% IDs
		    db[[i]][ind, ] <- iconv(db[[i]][ind, ], enc, "")
		}
	    }
	    if(verbose >= 2L) {
                message(" ", "done")
                flush.console()
            }
	}
	bad_IDs <-
	    unlist(sapply(db,
			  function(u)
			  u[rowSums(is.na(nchar(u, "c", TRUE))) > 0, "ID"]))
        ## FIXME: drop this fallback
	if(length(bad_IDs)) { ## try latin1
            for(i in seq_along(db)) {
                ind <- db[[i]][, "ID"] %in% bad_IDs
                db[[i]][ind, ] <- iconv(db[[i]][ind, ], "latin1", "")
            }
            bad_IDs <-
                unlist(sapply(db,
                              function(u)
                              u[rowSums(is.na(nchar(u, "c", TRUE))) > 0, "ID"]))
        }
	## If there are any invalid multi-byte character data
	## left, we simple remove all Rd objects with at least one
	## invalid entry, and warn.
        if(length(bad_IDs)) {
	    warning("removing all entries with invalid multi-byte character data")
	    for(i in seq_along(db)) {
		ind <- db[[i]][, "ID"] %in% bad_IDs
		db[[i]] <- db[[i]][!ind, ]
	    }
	}

	if(save_db) {
	    if(verbose >= 2L) {
                message("saving the database ...", appendLF=FALSE)
                flush.console()
            }
	    attr(db, "LibPaths") <- lib.loc
	    attr(db, "mtime") <- Sys.time()
	    attr(db, "ctype") <- Sys.getlocale("LC_CTYPE")
	    if(save_db_to_memory)
		.hsearch_db(db)
	    else {
		## If we cannot save to memory, serialize to a file ...
		.saveRDS(db, file = db_file, compress = TRUE)
		## and store a promise to unserialize from this file.
		.hsearch_db(substitute(.readRDS(con),
				       list(con = db_file)))
	    }
	    if(verbose >= 2L) {
                message(" ", "done")
                flush.console()
            }
	}
        if(verbose > 0L) {
            message("... database rebuilt")
            if(WINDOWS) {
                close(pb)
                on.exit() # clear closing of progress bar
            }
            flush.console()
        }
    }

    ### Matching.
    if(verbose >= 2L) {
	message("Database of ",
                NROW(db$Base), " Rd objects (",
                NROW(db$Aliases), " aliases, ",
                NROW(db$Concepts), " concepts, ",
                NROW(db$Keywords), " keywords)",
                domain = NA)
        flush.console()
    }
    if(!is.null(package)) {
	## Argument 'package' was given.  Need to check that all given
	## packages exist in the db, and only search the given ones.
	pos_in_hsearch_db <-
	    match(package, unique(db$Base[, "Package"]), nomatch = 0L)
        ## This should not happen for R >= 2.4.0
	if(any(pos_in_hsearch_db) == 0L)
	    stop(gettextf("no information in the database for package '%s': need 'rebuild = TRUE'?",
			  package[pos_in_hsearch_db == 0][1L]), domain = NA)
	db <-
	    lapply(db,
		   function(x) {
		       x[x[, "Package"] %in% package, , drop = FALSE]
		   })
    }

    ## <FIXME>
    ## No need continuing if there are no objects in the data base.
    ## But shouldn't we return something of class "hsearch"?
    if(!length(db$Base)) return(invisible())
    ## </FIXME>

    ## If agrep is NULL (default), we want to use fuzzy matching iff
    ## 'pattern' contains no characters special to regular expressions.
    ## We use the following crude approximation: if pattern contains
    ## only alphanumeric characters or whitespace or a '-', it is taken
    ## 'as is', and fuzzy matching is used unless turned off explicitly,
    ## or pattern has very few (currently, less than 5) characters.
    if(is.null(agrep) || is.na(agrep))
	agrep <-
	    (grepl("^([[:alnum:]]|[[:space:]]|-)+$", pattern)
	     && (nchar(pattern, type="c") > 4L))
    if(is.logical(agrep)) {
	if(agrep)
	    max.distance <- 0.1
    }
    else if(is.numeric(agrep) || is.list(agrep)) {
	max.distance <- agrep
	agrep <- TRUE
    }
    else
	stop("incorrect 'agrep' specification")

    searchFun <- function(x) {
	if(agrep)
	    agrep(pattern, x, ignore.case = ignore.case,
		  max.distance = max.distance)
	else
	    grep(pattern, x, ignore.case = ignore.case, perl = use_UTF8)
    }
    dbBase <- db$Base
    searchDbField <- function(field) {
	switch(field,
	       alias = {
		   aliases <- db$Aliases
		   match(aliases[searchFun(aliases[, "Aliases"]),
				 "ID"],
			 dbBase[, "ID"])
	       },
	       concept = {
		   concepts <- db$Concepts
		   match(concepts[searchFun(concepts[, "Concepts"]),
				  "ID"],
			 dbBase[, "ID"])
	       },

	       keyword = {
		   keywords <- db$Keywords
		   match(keywords[searchFun(keywords[, "Keywords"]),
				  "ID"],
			 dbBase[, "ID"])
	       },
	       searchFun(db$Base[, field]))
    }

    i <- NULL
    for(f in fields) i <- c(i, searchDbField(f))
    db <- dbBase[sort(unique(i)),
		 c("topic", "title", "Package", "LibPath"),
		 drop = FALSE]
    if(verbose>= 2L) {
        message(gettextf("matched %d objects.", NROW(db)), domain=NA)
        flush.console()
    }

    ## Retval.
    y <- list(pattern = pattern, fields = fields,
	      type = if(agrep) "fuzzy" else "regexp",
	      matches = db)
    class(y) <- "hsearch"
    y
}

print.hsearch <- function(x, ...)
    printhsearchInternal(x, ...)


printhsearchInternal  <- function(x, ...)
{
    fields <- paste(x$fields, collapse = " or ")
    type <- switch(x$type, fuzzy = "fuzzy", "regular expression")
    db <- x$matches
    if(NROW(db) > 0) {
	outFile <- tempfile()
	outConn <- file(outFile, open = "w")
	writeLines(c(strwrap(paste("Help files with", fields,
				   "matching", sQuote(x$pattern),
				   "using", type, "matching:")),
		     "\n"),
		   outConn)
	dbnam <- paste(db[, "Package"], "::", db[ , "topic"],
		       sep = "")
	dbtit <- paste(db[ , "title"], sep = "")
	writeLines(formatDL(dbnam, dbtit), outConn)
	writeLines(c("\n",
		     strwrap(paste("Type '?PKG::FOO' to",
				   "inspect entry 'PKG::FOO TITLE'."))),
		   outConn)
	close(outConn)
	file.show(outFile, delete.file = TRUE)
    } else {
	writeLines(strwrap(paste("No help files found with", fields,
				 "matching", sQuote(x$pattern),
				 "using", type, "matching.")))
    }

    invisible(x)
}
#  File src/library/utils/R/history.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

loadhistory <- function(file = ".Rhistory")
    invisible(.Internal(loadhistory(file)))

savehistory <- function(file = ".Rhistory")
    invisible(.Internal(savehistory(file)))

history <- function(max.show = 25, reverse = FALSE, pattern, ...)
{
    file1 <- tempfile("Rrawhist")
    savehistory(file1)
    rawhist <- readLines(file1)
    unlink(file1)
    if(!missing(pattern))
        rawhist <- unique(grep(pattern, rawhist, value = TRUE, ...))
    nlines <- length(rawhist)
    if(nlines) {
        inds <- max(1, nlines-max.show):nlines
        if(reverse) inds <- rev(inds)
    } else inds <- integer(0L)
    file2 <- tempfile("hist")
    writeLines(rawhist[inds], file2)
    file.show(file2, title = "R History", delete.file = TRUE)
}

timestamp <- function(stamp = date(), prefix = "##------ ",
                      suffix = " ------##", quiet = FALSE)
{
    stamp <- paste(prefix, stamp, suffix, sep = "")
    .Internal(addhistory(stamp))
    if (!quiet) cat(stamp, sep = "\n")
    invisible(stamp)
}
#  File src/library/utils/R/iconv.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/


## If you were wondering what these language codes stand for, see
## ftp://ftp.ilog.fr/pub/Users/haible/utf8/ISO_639
localeToCharset <- function(locale = Sys.getlocale("LC_CTYPE"))
{
    guess <- function(en)
    {
        if(en %in% c("aa", "af", "an", "br", "ca", "da", "de", "en",
                         "es", "et", "eu", "fi", "fo", "fr", "ga", "gl",
                         "gv", "id", "is", "it", "kl", "kw", "ml", "ms",
                         "nb", "nn", "no", "oc", "om", "pt", "so", "sq",
                         "st", "sv", "tl", "uz", "wa", "xh", "zu"))
                return("ISO8859-1")
        if(en %in% c("bs", "cs", "hr", "hu", "pl", "ro", "sk", "sl"))
            return("ISO8859-2")
        if(en %in% "mt") return("ISO8859-3")
            if(en %in% c("mk", "ru")) return("ISO8859-5")
        if(en %in% "ar") return("ISO8859-6")
        if(en %in% "el") return("ISO8859-7")
        if(en %in% c("he", "iw", "yi")) return("ISO8859-8")
        if(en %in% "tr") return("ISO8859-9")
        if(en %in% "lg") return("ISO8859-10")
        if(en %in% c("lt", "lv", "mi")) return("ISO8859-13")
        if(en %in% "cy") return("ISO8859-14")
        if(en %in% "uk") return("KOI8-U")
        if(en %in% "ja") return("EUC-JP")
        if(en %in% "ko") return("EUC-KR")
        if(en %in% "th") return("TIS-620")
        if(en %in% "tg") return("KOI8-T")
        if(en %in% "ka") return("GEORGIAN-PS")
        if(en %in% "kk") return("PT154")
        ## not safe to guess for zh
        return(NA_character_)
    }
    if(locale %in% c("C", "POSIX")) return("ASCII")
    if(.Platform$OS.type == "windows") {
        x <- strsplit(locale, ".", fixed=TRUE)[[1L]]
        if(length(x) != 2) return(NA_character_)
        ## PUTTY suggests mapping Windows code pages as
        ## 1250 -> ISO 8859-2
        ## 1251 -> KOI8-U
        ## 1252 -> ISO 8859-1
        ## 1253 -> ISO 8859-7
        ## 1254 -> ISO 8859-9
        ## 1255 -> ISO 8859-8
        ## 1256 -> ISO 8859-6
        ## 1257 -> ISO 8859-13
        switch(x[2L],
              # this is quite wrong "1250" = return("ISO8859-2"),
              # this is quite wrong "1251" = return("KOI8-U"),
               "1252" = return("ISO8859-1"),
              # "1253" = return("ISO8859-7"),
              # "1254" = return("ISO8859-9"),
              # "1255" = return("ISO8859-8"),
              # "1256" = return("ISO8859-6"),
               "1257" = return("ISO8859-13")
               )
        return(paste("CP", x[2L], sep=""))
    } else {
        ## Assume locales are like  en_US[.utf8[@euro]]
        x <- strsplit(locale, ".", fixed=TRUE)[[1L]]
        enc <- if(length(x) == 2) gsub("@.*$o", "", x[2L]) else ""
        if(enc == "UTF-8") enc <- "utf8" # for AIX
        if(nzchar(enc) && enc != "utf8") {
            enc <- tolower(enc)
            known <-
                c("ISO8859-1", "ISO8859-2", "ISO8859-3", "ISO8859-6",
                  "ISO8859-7", "ISO8859-8", "ISO8859-9", "ISO8859-10",
                  "ISO8859-13", "ISO8859-14", "ISO8859-15",
                  "CP1251", "CP1255", "EUC-JP", "EUC-KR", "EUC-TW",
                  "GEORGIAN-PS", "KOI8-R",  "KOI8-U", "TCVN",
                  "BIG5" , "GB2312", "GB18030", "GBK",
                  "TIS-620", "SHIFT_JIS", "GB2312", "BIG5-HKSCS")
            names(known) <-
                c("iso88591", "iso88592", "iso88593", "iso88596",
                  "iso88597", "iso88598", "iso88599", "iso885910",
                  "iso885913", "iso885914", "iso885915",
                  "cp1251", "cp1255", "eucjp", "euckr", "euctw",
                  "georgianps", "koi8r", "koi8u", "tcvn",
                  "big5" , "gb2312", "gb18030", "gbk",
                  "tis-620", "sjis", "eucn", "big5-hkscs")
	    if (length(grep("darwin",R.version$os))) {
	        k <- c(known, "ISO8859-1", "ISO8859-2", "ISO8859-4",
		  "ISO8859-7", "ISO8859-9", "ISO8859-13", "ISO8859-15",
		  "KOI8-U", "KOI8-R", "PT154", "ASCII", "ARMSCII-8",
		  "ISCII-DEV", "BIG5-HKCSC")
		names(k) <- c(names(known), "iso8859-1", "iso8859-2", "iso8859-4",
		  "iso8859-7", "iso8859-9", "iso8859-13", "iso8859-15",
		  "koi8-u", "koi8-r", "pt154", "us-ascii", "armscii-8",
		  "iscii-dev", "big5hkscs")
		known <- k
            }
	    if(enc %in% names(known)) return(as.vector(known[enc]))
            if(length(grep("^cp-", enc)))  # old Linux
                return(sub("cp-([0-9]+)", "CP\\1", enc))
            if(enc == "EUC") {
                ## let's hope it is a ll_* name.
                if(length(grep("^[[:alpha:]]{2}_", x[1L]))) {
                    ll <- substr(x[1L], 1L, 2L)
                    return(switch(ll, "jp"="EUC-JP", "kr"="EUC-KR",
                                  "zh"="GB2312"))
                }
            }
        }
	## on Darwin all real locales w/o encoding are UTF-8
	## HOWEVER! unlike the C code, we cannot filter out
	## invalid locales, so wit ill be wrong for non-supported
	## locales (why is this duplicated in R code anyway?)
	if (length(grep("darwin",R.version$os))) return("UTF-8")
        ## let's hope it is a ll_* name.
        if(length(grep("^[[:alpha:]]{2}_", x[1L]))) {
            ll <- substr(x[1L], 1L, 2L)
            if(enc == "utf8") return(c("UTF-8", guess(ll)))
            else return(guess(ll))
        }
        return(NA_character_)
    }
}
#  File src/library/utils/R/indices.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

packageDescription <- function(pkg, lib.loc=NULL, fields=NULL, drop=TRUE,
			       encoding = "")
{
    retval <- list()
    if(!is.null(fields)){
        fields <- as.character(fields)
        retval[fields] <- NA
    }

    ## If the NULL default for lib.loc is used, the loaded packages are
    ## searched before the libraries.
    pkgpath <-
	if(is.null(lib.loc)) {
	    if(pkg == "base")
		file.path(.Library, "base")
	    else if((envname <- paste("package:", pkg, sep = ""))
		    %in% search()) {
		pp <- attr(as.environment(envname), "path")
		## could be NULL if a perverse user has been naming
		## environments to look like packages.
	    } else if(pkg %in% loadedNamespaces())
		## correct path for a loaded (not attached) namespace:
		getNamespaceInfo(pkg, "path")
	}
    if(is.null(pkgpath)) pkgpath <- ""

    if(pkgpath == "") {
        libs <- if(is.null(lib.loc)) .libPaths() else lib.loc
        for(lib in libs)
            if(file.access(file.path(lib, pkg), 5) == 0L) {
                pkgpath <- file.path(lib, pkg)
                break
            }
    }

    ## we no longer have versioned installs:
##     if(pkgpath == "") {
##         ## This is slow and does a lot of checking we do here,
##         ## but is needed for versioned installs
##         pkgpath <- system.file(package = pkg, lib.loc = lib.loc)
##     }
    if(pkgpath == "") {
        warning(gettextf("no package '%s' was found", pkg), domain = NA)
        return(NA)
    }

    ## New in 2.7.0: look for installed metadata first.
    ## FIXME: how much longer should be drop back to the file?

    if(file.exists(file <- file.path(pkgpath, "Meta", "package.rds"))) {
        desc <- .readRDS(file)$DESCRIPTION
        if(length(desc) < 1)
            stop(gettextf("metadata of package '%s' is corrupt", pkg),
                 domain = NA)
        desc <- as.list(desc)
    } else if(file.exists(file <- file.path(pkgpath,"DESCRIPTION"))) {
        dcf <- read.dcf(file=file)
        if(NROW(dcf) < 1L)
            stop(gettextf("DESCRIPTION file of package '%s' is corrupt", pkg),
                 domain = NA)
        desc <- as.list(dcf[1,])
    } else file <- ""

    if(file != "") {
        ## read the Encoding field if any
        enc <- desc[["Encoding"]]
        if(!is.null(enc) && !is.na(encoding)) {
            ## Determine encoding and re-encode if necessary and possible.
            if((encoding != "" || Sys.getlocale("LC_CTYPE") != "C")) {
                ## might have an invalid encoding ...
                newdesc <- try(lapply(desc, iconv, from=enc, to=encoding))
                if(!inherits(newdesc, "try-error")) desc <- newdesc
                else
                    warning("'DESCRIPTION' file has 'Encoding' field and re-encoding is not possible", call. = FALSE)
            } else
            warning("'DESCRIPTION' file has 'Encoding' field and re-encoding is not possible", call. = FALSE)
        }
        if(!is.null(fields)){
            ok <- names(desc) %in% fields
            retval[names(desc)[ok]] <- desc[ok]
        }
        else
            retval[names(desc)] <- desc
    }

    if((file == "") || (length(retval) == 0)){
        warning(gettextf("DESCRIPTION file of package '%s' is missing or broken", pkg), domain = NA)
        return(NA)
    }

    if(drop & length(fields) == 1L)
        return(retval[[1L]])

    class(retval) <- "packageDescription"
    if(!is.null(fields)) attr(retval, "fields") <- fields
    attr(retval, "file") <- file
    retval
}


print.packageDescription <- function(x, ...)
{
    xx <- x
    xx[] <- lapply(xx, function(x) if(is.na(x)) "NA" else x)
    write.dcf(as.data.frame.list(xx, optional = TRUE))
    cat("\n-- File:", attr(x, "file"), "\n")
    if(!is.null(attr(x, "fields"))){
        cat("-- Fields read: ")
        cat(attr(x, "fields"), sep=", ")
        cat("\n")
    }
    invisible(x)
}

index.search <- function(topic, path, file = "AnIndex", type = "help")
    .Internal(index.search(topic, path, file, .Platform$file.sep, type))

print.packageIQR <-
function(x, ...)
{
    db <- x$results
    ## Split according to Package.
    out <- if(nrow(db) == 0L)
         NULL
    else
        lapply(split(1 : nrow(db), db[, "Package"]),
               function(ind) db[ind, c("Item", "Title"),
                                drop = FALSE])
    outFile <- tempfile("RpackageIQR")
    outConn <- file(outFile, open = "w")
    first <- TRUE
    for(pkg in names(out)) {
        writeLines(paste(ifelse(first, "", "\n"), x$title,
                         " in package ", sQuote(pkg), ":\n",
                         sep = ""),
                   outConn)
        writeLines(formatDL(out[[pkg]][, "Item"],
                            out[[pkg]][, "Title"]),
                   outConn)
        first <- FALSE
    }
    if(first) {
        close(outConn)
        unlink(outFile)
        writeLines(paste("no", tolower(x$title), "found"))
        if(!is.null(x$footer))
            writeLines(c("", x$footer))
    }
    else {
        if(!is.null(x$footer))
            writeLines(c("\n", x$footer), outConn)
        close(outConn)
        file.show(outFile, delete.file = TRUE,
                  title = paste("R", tolower(x$title)))
    }
    invisible(x)
}
#  File src/library/utils/R/menu.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

menu <- function(choices, graphics = FALSE, title = "")
{
    if(!interactive()) stop("menu() cannot be used non-interactively")
    if(graphics) {
        if(.Platform$OS.type == "windows" || .Platform$GUI == "AQUA") {
            res <- select.list(choices, multiple=FALSE, title=title)
            return(match(res, choices, nomatch = 0L))
        } else if(.Platform$OS.type == "unix"
                && capabilities("tcltk") && capabilities("X11")
                && nzchar(Sys.getenv("DISPLAY"))) {
            res <- tcltk::tk_select.list(choices, multiple=FALSE, title=title)
            return(match(res, choices, nomatch = 0L))
        }
    }
    nc <- length(choices)
    if(length(title) && nzchar(title[1L])) cat(title[1L], "\n")
    op <- paste(format(seq_len(nc)), ": ", choices, sep="")
    if(nc > 10L) {
        fop <- format(op)
        nw <- nchar(fop[1L], "w") + 2
        ncol <- getOption("width") %/% nw  # might be 0
        if(ncol > 1L)
            op <- paste(fop, c(rep("  ", ncol - 1), "\n"), sep="", collapse="")
        cat("", op, "", sep="\n")
    } else cat("", op, "", sep="\n")
    repeat {
	ind <- .Internal(menu(as.character(choices)))
	if(ind <= nc) return(ind)
	cat(gettext("Enter an item from the menu, or 0 to exit\n"))
    }
}
#  File src/library/utils/R/mirrorAdmin.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

mirror2html <- function(mirrors = NULL, file="mirrors.html",
                        head = "mirrors-head.html",
                        foot = "mirrors-foot.html")
{
    if(is.null(mirrors)){
        mirrors <- getCRANmirrors(all=FALSE, local.only=TRUE)
    }
    z <- NULL
    if(file.exists(head)) z <- readLines(head)
    z <- c(z, "<dl>")
    for(country in unique(mirrors$Country)){
        m = mirrors[mirrors$Country==country,]
        z <- c(z, paste("<dt>", country, "</dt>", sep=""),
               "<dd>",
               "<table border=0 width=90%>")
        for(k in 1L:nrow(m)){
            z <- c(z, "<tr>",
                   "<td width=45%>",
                   "<a href=\"", m$URL[k], "\" target=\"_top\">",
                   m$URL[k], "</a>",
                   "</td>\n",
                   "<td>", m$Host[k], "</td>",
                   "</tr>")
        }
        z <- c(z, "</table>", "</dd>")
    }
    z <- c(z, "</dl>")
    if(file.exists(foot)) z <- c(z, readLines(foot))
    if(file!="") writeLines(z, file)
    invisible(z)
}

checkCRAN <-function(method)
{
    master <- available.packages(contrib.url("http://cran.R-project.org"),
                                 method=method)
    m <- getCRANmirrors()
    z <- list()
    for(url in as.character(m$URL))
        z[[url]] = available.packages(contrib.url(url), method=method)
    lapply(z, function(a) all.equal(a, master))
}






#  File src/library/utils/R/modifyList.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

### Originates from Deepayan Sarkar as  updateList() from 'lattice' package
modifyList <- function(x, val)
{
    stopifnot(is.list(x), is.list(val))
    xnames <- names(x)
    for (v in names(val)) {
	x[[v]] <-
	    if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]]))
		modifyList(x[[v]], val[[v]]) else val[[v]]
    }
    x
}
news <-
function(query, package = "R", lib.loc = NULL,
         format = NULL, reader = NULL, db = NULL)
{
    if(is.null(db)) {
        db <- if(package == "R")
            tools:::.build_news_db_from_R_NEWS()
        else
            tools:::.build_news_db(package, lib.loc, format, reader)
    }
    if(is.null(db))
        return(invisible())
            
    ## Is there a way to directly call/use subset.data.frame?
    ## E.g.,
    ##   subset(db, query)
    ## does not work.
    if(missing(query))
        return(db)

    ## For queries we really need to force Version to package_version
    ## and Date to Date ...
    ## This is tricky because we do not necessarily have valid package
    ## versions (e.g., R NEWS has "2.8.1 patched") or could have the
    ## version info missing (and package_version() does not like NAs).

    has_bad_attr <-
        !is.null(bad <- attr(db, "bad")) && (length(bad) == NROW(db))

    ## Manipulate fields for querying (but return the original ones).
    db1 <- db
    ## Canonicalize version entries which *start* with a valid numeric
    ## version.
    version <- db$Version
    pos <- regexpr(sprintf("^%s",
                           .standard_regexps()$valid_numeric_version),
                   version)
    if(any(ind <- (pos > -1L)))
        version[ind] <-
            substring(version[ind], 1L, attr(pos, "match.length")[ind])
    db1$Version <- numeric_version(version, strict = FALSE)
    db1$Date <- as.Date(db$Date)

    r <- eval(substitute(query), db1, parent.frame())
    ## Do something if this is not logical ...
    if(is.null(r))
        return(db)
    else if(!is.logical(r) || length(r) != length(version))
        stop("invalid query")
    r <- r & !is.na(r)
    if(has_bad_attr)
        structure(db[r, ], bad = bad[r])
    else
        db[r, ]
}

print.news_db <-
function(x, ...)
{
    if(!(is.null(bad <- attr(x, "bad")))
       && (length(bad) == NROW(x))
       && all(!bad)) {
        ## Output news in the preferred input format:
        ##   Changes in $VERSION [($DATE)]:
        ##   [$CATEGORY$]
        ##   indented/formatted bullet list of $TEXT entries.
        ## <FIXME>
        ## Add support for DATE.
        ## </FIXME>
        print_items <- function(x)
            cat(paste("    o   ", gsub("\n", "\n\t", x), sep = ""),
                sep = "\n\n")
        vchunks <- split(x, x$Version)
        ## Re-order according to decreasing version.
        ## R NEWS has invalid "versions" such as ""2.4.1 patched" which
        ## we map to 2.4.1.1.
        vchunks <-
            vchunks[order(as.numeric_version(sub(" *patched", ".1",
                                                 names(vchunks))),
                                 decreasing = TRUE)]
        vheaders <-
            sprintf("%sChanges in version %s:\n\n",
                    c("", rep.int("\n", length(vchunks) - 1L)),
                    names(vchunks))
        for(i in seq_along(vchunks)) {
            cat(vheaders[i])
            vchunk <- vchunks[[i]]
            if(all(!is.na(category <- vchunk$Category)
                   & nzchar(category))) {
                cchunks <- split(vchunk, category)
                cheaders <-
                    sprintf("%s%s\n\n",
                            c("", rep.int("\n", length(cchunks) - 1L)),
                            names(cchunks))
                for(j in seq_along(cchunks)) {
                    cat(cheaders[j])
                    print_items(cchunks[[j]]$Text)
                }
            } else {
                print_items(vchunk$Text)
            }
        }
    } else {
        ## Simple and ugly.
        ## Should this drop all-NA variables?
        print(as.data.frame(x), right = FALSE, row.names = FALSE)
    }

    invisible(x)
}
#  File src/library/utils/R/object.size.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

object.size <- function(x)
    structure(.Internal(object.size(x)), class="object_size")

print.object_size <-
    function(x, quote = FALSE, units = c("b", "auto", "Kb", "Mb", "Gb"), ...)
{
    units <- match.arg(units)
    if (units == "auto") {
        if (x >= 1024^3) units <- "Gb"
        else if (x >= 1024^2) units <- "Mb"
        else if (x >= 1024) units <- "Kb"
        else units <- "b"
    }
    y <- switch(units,
                "b" = paste(x, "bytes"),
                "Kb" = paste(round(x/1024, 1L), "Kb"),
                "Mb" = paste(round(x/1024^2, 1L), "Mb"),
                "Gb" = paste(round(x/1024^3, 1L), "Gb")
                )
    if(quote) print.default(y, ...) else cat(y, "\n", sep="")
    invisible(x)
}
#  File src/library/utils/R/objects.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

## findGeneric(fname) :  is 'fname' the name of an S3 generic ?
##			[internal function used only in this file]
findGeneric <- function(fname, envir)
{
    if(!exists(fname, mode = "function", envir = envir)) return("")
    f <- get(fname, mode = "function", envir = envir)
    ## FIXME? In the first case, e.g. 'methods(qr)', we are very inefficient:
    ##  inside methods() we transform the 'qr' function object into a character,
    ##  whereas here, we revert this, searching around unnecessarily
    ##
    if(.isMethodsDispatchOn() && methods::is(f, "genericFunction")) {
	## maybe an S3 generic was turned into the S4 default
	## Try to find it, otherwise warn :
	fMethsEnv <- methods::getMethodsForDispatch(f)
	r <- lapply(grep("^ANY\\b", ls(envir = fMethsEnv), value=TRUE),
		    get, envir = fMethsEnv)
	if(any(ddm <- unlist(lapply(r, class)) == "derivedDefaultMethod"))
	    f <- r[ddm][[1]]@.Data
	else
	    warning(gettextf(
	"'%s' is a formal generic function; S3 methods will not likely be found",
			     fname), domain = NA)
    }
    isUMEbrace <- function(e) {
        for (ee in as.list(e[-1L]))
            if (nzchar(res <- isUME(ee))) return(res)
        ""
    }
    isUMEif <- function(e) {
        if (length(e) == 3L) isUME(e[[3L]])
        else {
            if (nzchar(res <- isUME(e[[3L]]))) res
            else if (nzchar(res <- isUME(e[[4L]]))) res
            else ""
        }
    }
    isUME <- function(e) { ## is it an "UseMethod() calling function" ?
        if (is.call(e) && (is.name(e[[1L]]) || is.character(e[[1L]]))) {
            switch(as.character(e[[1L]]),
                   UseMethod = as.character(e[[2L]]),
                   "{" = isUMEbrace(e),
                   "if" = isUMEif(e),
                   "")
        } else ""
    }
    isUME(body(f))
}

getKnownS3generics <- function()
    c(names(.knownS3Generics), tools:::.get_internal_S3_generics())

methods <- function (generic.function, class)
{
    rbindSome <- function(df, nms, msg) {
        ## rbind.data.frame() -- dropping rows with duplicated names
        n2 <- length(nms)
        dnew <- data.frame(visible = rep.int(FALSE, n2),
                           from    = rep.int(msg,   n2),
                           row.names = nms)
        n <- nrow(df)
        if(n == 0L) return(dnew)
        ## else
        keep <- !duplicated(c(rownames(df), rownames(dnew)))
        rbind(df  [keep[1L:n] , ],
              dnew[keep[(n+1L):(n+n2)] , ])
    }

    S3MethodsStopList <- tools:::.make_S3_methods_stop_list(NULL)
    knownGenerics <- getKnownS3generics()
    sp <- search()
    an <- lapply(seq_along(sp), ls)
    names(an) <- sp
    an <- unlist(an)
    an <- an[!duplicated(an)] # removed masked objects, *keep* names
    names(an) <- sub("[0-9]*$", "", names(an))
    info <- data.frame(visible = rep.int(TRUE, length(an)),
                       from = names(an),
                       row.names = an)
    if (!missing(generic.function)) {
	if (!is.character(generic.function))
	    generic.function <- deparse(substitute(generic.function))
        ## else
        if(!exists(generic.function, mode = "function",
                   envir = parent.frame()) &&
           !any(generic.function == c("Math", "Ops", "Complex", "Summary")))
            stop(gettextf("no function '%s' is visible", generic.function),
                 domain = NA)
        if(!any(generic.function == knownGenerics)) {
            truegf <- findGeneric(generic.function, parent.frame())
            if(truegf == "")
                warning(gettextf("function '%s' appears not to be generic",
                                 generic.function), domain = NA)
            else if(truegf != generic.function) {
                warning(gettextf("generic function '%s' dispatches methods for generic '%s'",
                        generic.function, truegf), domain = NA)
                generic.function <- truegf
            }
        }
	name <- paste("^", generic.function, ".", sep = "")
        name <- gsub("([.[$+*])", "\\\\\\1",name)
        info <- info[grep(name, row.names(info)), ]
        info <- info[! row.names(info) %in% S3MethodsStopList, ]
        ## check that these are all functions
        ## might be none at this point
        if(nrow(info)) {
            keep <- sapply(row.names(info),
                           function(nm) exists(nm, mode="function"))
            info <- info[keep, ]
        }

        ## also look for registered methods from namespaces
        ## we assume that only functions get registered.
        defenv <- if(!is.na(w <- .knownS3Generics[generic.function]))
            asNamespace(w)
        else {
            genfun <- get(generic.function, mode = "function",
                          envir = parent.frame())
            if(.isMethodsDispatchOn() && methods::is(genfun, "genericFunction"))
                genfun <- methods::slot(genfun, "default")@methods$ANY
            if (typeof(genfun) == "closure") environment(genfun)
            else .BaseNamespaceEnv
        }
        S3reg <- ls(get(".__S3MethodsTable__.", envir = defenv),
                    pattern = name)
        if(length(S3reg))
            info <- rbindSome(info, S3reg, msg =
                              paste("registered S3method for",
                                    generic.function))
        ## both all() and all.equal() are generic, so
        if(generic.function == "all")
            info <- info[-grep("^all\\.equal", row.names(info)), ]
    }
    else if (!missing(class)) {
	if (!is.character(class))
	    class <- paste(deparse(substitute(class)))
	name <- paste(".", class, "$", sep = "")
        name <- gsub("([.[])", "\\\\\\1", name)
        info <- info[grep(name, row.names(info)), ]
        info <- info[! row.names(info) %in% S3MethodsStopList, ]

        if(nrow(info)) {
            ## check if we can find a generic matching the name
            possible.generics <- gsub(name, "", row.names(info))
            keep <- sapply(possible.generics, function(nm) {
                if(nm %in% knownGenerics) return(TRUE)
                where <- find(nm, mode = "function")
                if(!length(where)) return(FALSE)
                any(sapply(where, function(w)
                           nzchar(findGeneric(nm, envir=as.environment(w)))))
            })
            info <- info[keep, ]
        }

        ## also look for registered methods in loaded namespaces.
        ## These should only be registered in environments containing
        ## the corresponding generic, so we don't check again.
        ## Note that the generic will not necessarily be visible,
        ## as the package may not be loaded.
        S3reg <- unlist(lapply(loadedNamespaces(), function(i) ls(get(".__S3MethodsTable__.", envir = asNamespace(i)), pattern = name)))
        ## now methods like print.summary.aov will be picked up,
        ## so we do look for such mismatches.
        if(length(S3reg))
            S3reg <- S3reg[sapply(gsub(name, "", S3reg), exists)]
        if(length(S3reg))
            info <- rbindSome(info, S3reg, msg = "registered S3method")
    }
    else stop("must supply 'generic.function' or 'class'")

    info <- info[sort.list(row.names(info)), ]
    res <- row.names(info)
    class(res) <- "MethodsFunction"
    attr(res, "info") <- info
    res
}

print.MethodsFunction <- function(x, ...)
{
    visible <- attr(x, "info")[["visible"]]
    if(length(x)) {
        print(paste(x, ifelse(visible, "", "*"), sep=""), quote=FALSE, ...)
        if(any(!visible))
            cat("\n", "   ",
                "Non-visible functions are asterisked", "\n", sep="")
    } else cat("no methods were found\n")
    invisible(x)
}


getS3method <-  function(f, class, optional = FALSE)
{
    if(!any(f == getKnownS3generics())) {
        truegf <- findGeneric(f, parent.frame())
        if(nzchar(truegf)) f <- truegf
        else {
            if(optional) return(NULL)
            else stop(gettextf("no function '%s' could be found", f), domain = NA)
        }
    }
    method <- paste(f, class, sep=".")
    if(exists(method, mode = "function", envir = parent.frame()))
        return(get(method, mode = "function", envir = parent.frame()))
    ## also look for registered method in namespaces
    defenv <- if(!is.na(w <- .knownS3Generics[f])) asNamespace(w)
    else if(f %in% tools:::.get_internal_S3_generics()) .BaseNamespaceEnv
    else {
        genfun <- get(f, mode="function", envir = parent.frame())
        if(.isMethodsDispatchOn() && methods::is(genfun, "genericFunction"))
            genfun <- methods::slot(genfun, "default")@methods$ANY
        if (typeof(genfun) == "closure") environment(genfun)
        else .BaseNamespaceEnv
    }
    S3Table <- get(".__S3MethodsTable__.", envir = defenv)
    if(exists(method, envir = S3Table, inherits = FALSE))
        return(get(method, envir = S3Table))
    if(optional) NULL else stop(gettextf("S3 method '%s' not found", method),
                                domain = NA)
}

getFromNamespace <- function(x, ns, pos = -1, envir = as.environment(pos))
{
    if(missing(ns)) {
        nm <- attr(envir, "name", exact = TRUE)
        if(is.null(nm) || substring(nm, 1L, 8L) != "package:")
            stop("environment specified is not a package")
        ns <- asNamespace(substring(nm, 9L))
    } else ns <- asNamespace(ns)
    get(x, envir = ns, inherits = FALSE)
}

assignInNamespace <-
    function(x, value, ns, pos = -1, envir = as.environment(pos))
{
    if(missing(ns)) {
        nm <- attr(envir, "name", exact = TRUE)
        if(is.null(nm) || substring(nm, 1L, 8L) != "package:")
            stop("environment specified is not a package")
        ns <- asNamespace(substring(nm, 9L))
    } else ns <- asNamespace(ns)
    if(bindingIsLocked(x, ns)) {
        unlockBinding(x, ns)
        assign(x, value, envir = ns, inherits = FALSE)
        w <- options("warn")
        on.exit(options(w))
        options(warn = -1)
        lockBinding(x, ns)
    } else {
        assign(x, value, envir = ns, inherits = FALSE)
    }
    if(!isBaseNamespace(ns)) {
        ## now look for possible copy as a registered S3 method
        S3 <- getNamespaceInfo(ns, "S3methods")
        if(!length(S3)) return(invisible(NULL))
        S3names <- S3[, 3L]
        if(x %in% S3names) {
            i <- match(x, S3names)
            genfun <- get(S3[i, 1L], mode = "function", envir = parent.frame())
            if(.isMethodsDispatchOn() && methods::is(genfun, "genericFunction"))
                genfun <- methods::slot(genfun, "default")@methods$ANY
            defenv <- if (typeof(genfun) == "closure") environment(genfun)
            else .BaseNamespaceEnv
            S3Table <- get(".__S3MethodsTable__.", envir = defenv)
            remappedName <- paste(S3[i, 1L], S3[i, 2L], sep = ".")
            if(exists(remappedName, envir = S3Table, inherits = FALSE))
                assign(remappedName, value, S3Table)
        }
    }
    invisible(NULL)
}

fixInNamespace <- function (x, ns, pos = -1, envir = as.environment(pos), ...)
{
    subx <- substitute(x)
    if (is.name(subx))
        subx <- deparse(subx)
    if (!is.character(subx) || length(subx) != 1L)
        stop("'fixInNamespace' requires a name")
    if(missing(ns)) {
        nm <- attr(envir, "name", exact = TRUE)
        if(is.null(nm) || substring(nm, 1L, 8L) != "package:")
            stop("environment specified is not a package")
        ns <- asNamespace(substring(nm, 9L))
    } else ns <- asNamespace(ns)
    x <- edit(get(subx, envir = ns, inherits = FALSE), ...)
    assignInNamespace(subx, x, ns)
}

getAnywhere <- function(x)
{
    x <- as.character(substitute(x))
    objs <- list(); where <- character(0L); visible <- logical(0L)
    ## first look on search path
    if(length(pos <- find(x, numeric = TRUE))) {
        objs <- lapply(pos, function(pos, x) get(x, pos=pos), x=x)
        where <- names(pos)
        visible <- rep.int(TRUE, length(pos))
    }
    ## next look for methods
    if(length(grep(".", x, fixed=TRUE))) {
        np <- length(parts <- strsplit(x, ".", fixed=TRUE)[[1L]])
        for(i in 2:np) {
            gen <- paste(parts[1L:(i-1)], collapse=".")
            cl <- paste(parts[i:np], collapse=".")
            if (gen == "" || cl == "") next
            # f might be a special, not a closure, and not have an environment, so
            # be careful below
            if(!is.null(f <- getS3method(gen, cl, TRUE)) && !is.null(environment(f))) {    
                ev <- topenv(environment(f), baseenv())
                nmev <- if(isNamespace(ev)) getNamespaceName(ev) else NULL
                objs <- c(objs, f)
                msg <- paste("registered S3 method for", gen)
                if(!is.null(nmev))
                    msg <- paste(msg, "from namespace", nmev)
                where <- c(where, msg)
                visible <- c(visible, FALSE)
            }
        }
    }
    ## now look in namespaces, visible or not
    for(i in loadedNamespaces()) {
        ns <- asNamespace(i)
        if(exists(x, envir = ns, inherits = FALSE)) {
            f <- get(x, envir = ns, inherits = FALSE)
            objs <- c(objs, f)
            where <- c(where, paste("namespace", i, sep=":"))
            visible <- c(visible, FALSE)
        }
    }
    # now check for duplicates
    ln <- length(objs)
    dups <- rep.int(FALSE, ln)
    objs2 <- lapply(objs, function(x) {
        if(is.function(x)) environment(x) <- baseenv()
        x
    })
    if(ln > 1L)
        for(i in 2L:ln)
            for(j in 1L:(i-1L))
                if(identical(objs2[[i]], objs2[[j]])) {
                    dups[i] <- TRUE
                    break
                }
    res <- list(name=x, objs=objs, where=where, visible=visible, dups=dups)
    class(res) <- "getAnywhere"
    res
}

print.getAnywhere <- function(x, ...)
{
    n <- sum(!x$dups)
    if(n == 0L) {
        cat("no object named", sQuote(x$name), "was found\n")
    } else if (n == 1L) {
        cat("A single object matching", sQuote(x$name), "was found\n")
        cat("It was found in the following places\n")
        cat(paste("  ", x$where, sep=""), sep="\n")
        cat("with value\n\n")
        print(x$objs[[1L]])
    } else {
        cat(n, "differing objects matching", sQuote(x$name),
            "were found\n")
        cat("in the following places\n")
        cat(paste("  ", x$where, sep=""), sep="\n")
        cat("Use [] to view one of them\n")
    }
    invisible(x)
}

"[.getAnywhere" <- function(x, i)
{
    if(!is.numeric(i)) stop("only numeric indices can be used")
    if(length(i) == 1L) x$objs[[i]]
    else x$objs[i]
}

argsAnywhere <- function(x) {
        name<-as.character(substitute(x))
        fs<-do.call(getAnywhere, list(name))
        if (sum(!fs$dups)==0)
            return(NULL)
        if (sum(!fs$dups)>1)
            sapply(fs$objs[!fs$dups],
                   function(f) if (is.function(f)) args(f))
        else args(fs$objs[[1L]])
}
#  File src/library/utils/R/package.skeleton.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

package.skeleton <-
    function(name = "anRpackage", list = character(), environment = .GlobalEnv,
	     path = ".", force = FALSE, namespace = FALSE,
             code_files = character())
{
    safe.dir.create <- function(path)
    {
	dirTest <- function(x) !is.na(isdir <- file.info(x)$isdir) & isdir
	if(!dirTest(path) && !dir.create(path))
	    stop(gettextf("cannot create directory '%s'", path), domain = NA)
    }

    if(!is.logical(namespace) || length(namespace) != 1L)
        stop("'namespace' must be a single logical")
    if(!is.character(code_files))
        stop("'code_files' must be a character vector")
    use_code_files <- length(code_files) > 0L

    envIsMissing <- missing(environment) # before R clobbers this information

    if(missing(list)) {
        if(use_code_files) {
            environment <- new.env()
            methods::setPackageName(name, environment)
            for(cf in code_files)
                sys.source(cf, envir = environment)
        }
	## all.names: crucial for metadata
	list <- ls(environment, all.names=TRUE)
    }
    if(!is.character(list))
	stop("'list' must be a character vector naming R objects")
    if(use_code_files || !envIsMissing) {
        classesList <- getClasses(environment)
        classes0 <- .fixPackageFileNames(classesList)
        names(classes0) <- classesList
        methodsList <- getGenerics(environment)
        methods0 <- .fixPackageFileNames(methodsList)
        names(methods0) <- methodsList
    }
    else { # nobody should  specify classes or methods as object names!
        classesList <- methodsList <- character()
    }
    usingS4 <- length(classesList) > 0L || length(methodsList) > 0L

    ## we need to test in the C locale
    curLocale <- Sys.getlocale("LC_CTYPE")
    on.exit(Sys.setlocale("LC_CTYPE", curLocale), add = TRUE)
    if(Sys.setlocale("LC_CTYPE", "C") != "C")
        warning("cannot turn off locale-specific chars via LC_CTYPE")

    have <- unlist(lapply(list, exists, envir = environment))
    if(any(!have))
        warning(sprintf(ngettext(sum(!have),
                                 "object '%s' not found",
                                 "objects '%s' not found"),
                        paste(sQuote(list[!have]), collapse=", ")),
                domain = NA)
    list <- list[have]
    if(!length(list))
	stop("no R objects specified or available")

    message("Creating directories ...")
    ## Make the directories
    dir <- file.path(path, name)
    if(file.exists(dir) && !force)
	stop(gettextf("directory '%s' already exists", dir), domain = NA)

    safe.dir.create(dir)
    safe.dir.create(code_dir <- file.path(dir, "R"))
    safe.dir.create(docs_dir <- file.path(dir, "man"))
    safe.dir.create(data_dir <- file.path(dir, "data"))

    ## DESCRIPTION
    message("Creating DESCRIPTION ...")
    description <- file(file.path(dir, "DESCRIPTION"), "wt")
    cat("Package: ", name, "\n",
	"Type: Package\n",
	"Title: What the package does (short line)\n",
	"Version: 1.0\n",
	"Date: ", format(Sys.time(), format="%Y-%m-%d"), "\n",
	"Author: Who wrote it\n",
	"Maintainer: Who to complain to <yourfault@somewhere.net>\n",
	"Description: More about what it does (maybe more than one line)\n",
	"License: What license is it under?\n",
	"LazyLoad: yes\n",
	if(usingS4) "Depends: methods\n",
	file = description, sep = "")
    close(description)

    ## NAMESPACE
    ## <NOTE>
    ## For the time being, we export all non-internal objects using the pattern
    ## of names beginning with alpha.  All S4 methods and classes are exported.
    ## S3 methods will be exported if the function's name would be exported.
    ## </NOTE>
    if(namespace) {
        message("Creating NAMESPACE ...")
        out <- file(file.path(dir, "NAMESPACE"), "wt")
        writeLines("exportPattern(\"^[[:alpha:]]+\")", out)
        if(length(methodsList)) {
            cat("exportMethods(\n    ", file = out)
            cat(paste('"', methodsList, '"', sep="", collapse = ",\n    "), "\n)\n", file = out)
        }
        if(length(classesList)) {
            cat("exportClasses(\n    ", file = out)
            cat(paste('"', classesList, '"', sep="", collapse = ",\n     "), "\n)\n", file = out)
        }
        close(out)
    }

    ## Read-and-delete-me
    message("Creating Read-and-delete-me ...")
    out <- file(file.path(dir, "Read-and-delete-me"), "wt")
    msg <-
        c("* Edit the help file skeletons in 'man', possibly combining help files for multiple functions.",
          if(namespace)
          "* Edit the exports in 'NAMESPACE', and add necessary imports.",
          "* Put any C/C++/Fortran code in 'src'.",
          if(namespace)
          "* If you have compiled code, add a useDynLib() directive to 'NAMESPACE'."
          else
          "* If you have compiled code, add a .First.lib() function in 'R' to load the shared library.",
          "* Run R CMD build to build the package tarball.",
          "* Run R CMD check to check the package tarball.",
          "",
          "Read \"Writing R Extensions\" for more information.")
    writeLines(strwrap(msg, exdent = 2), out)
    close(out)

    internalObjInds <- grep("^\\.", list)
    internalObjs <- list[internalObjInds]
    if(length(internalObjInds))
	list <- list[-internalObjInds]

    if(use_code_files) {
        list0 <- .fixPackageFileNames(list)
    } else {
        list0 <- list
    }
    names(list0) <- list

    ## Dump the items in 'data' or 'R'
    if(!use_code_files) {
        message("Saving functions and data ...")
        if(length(internalObjInds))
            dump(internalObjs,
                 file = file.path(code_dir,
                 sprintf("%s-internal.R", name)))
        for(item in list){
            if(is.function(get(item, envir = environment)))
                dump(item,
                     file = file.path(code_dir, sprintf("%s.R", list0[item])))
            else       # we cannot guarantee this is a valid file name
                try(save(list = item, envir = environment,
                         file = file.path(data_dir, sprintf("%s.rda", item))))
        }
    } else {
        message("Copying code files ...")
        file.copy(code_files, code_dir)
        ## Only "abc.R"-like files are really ok:
	R_files <- tools::list_files_with_type(code_dir, "code",
					       full.names = FALSE,
					       OS_subdirs = "")
        code_files <- basename(code_files)
	wrong <- code_files[is.na(match(code_files, R_files))]
	if(length(wrong)) {
	    warning("Invalid file name(s) for R code in ", code_dir,":\n",
		    strwrap(paste(sQuote(wrong), collapse = ", "), indent=2),
		    "\n are now renamed to 'z<name>.R'")
	    file.rename(from = file.path(code_dir, wrong),
			to = file.path(code_dir,
			paste("z", sub("(\\.[^.]*)?$", ".R", wrong), sep="")))
        }
    }

    ## Make help file skeletons in 'man'
    message("Making help files ...")
    if(!namespace && length(internalObjInds)) {
        notNeeded <- grep(methods:::.methodsPackageMetaNamePattern, internalObjs)
        notNeeded <- c(notNeeded, match(".packageName", internalObjs, 0L))
        if(length(notNeeded) < length(internalObjs)) {
            internalObjs <- internalObjs[-notNeeded]
            Rdfile <- file(file.path(docs_dir,
                                     sprintf("%s-internal.Rd", name)),
                           "wt")
            cat("\\name{", name, "-internal}\n",
                "\\title{Internal ", name, " objects}\n",
                file = Rdfile, sep = "")
            for(item in internalObjs) {
                cat("\\alias{", item, "}\n", file = Rdfile, sep = "")
            }
            cat("\\description{Internal ", name, " objects.}\n",
                "\\details{These are not to be called by the user.}\n",
                "\\keyword{internal}",
                file = Rdfile, sep = "")
            close(Rdfile)
        }
    }
    ## Suppress partially inappropriate messages from prompt().
    yy <- try(suppressMessages({
	promptPackage(name,
		      filename =
		      file.path(docs_dir,
				sprintf("%s-package.Rd", name)),
		      lib.loc = path)
	sapply(list,
	       function(item) {
		   prompt(get(item, envir = environment),
			  name = item,
			  filename =
			  file.path(docs_dir,
				    sprintf("%s.Rd", list0[item])))
	       })
	sapply(classesList,
	       function(item) {
		   methods::promptClass(item,
					filename =
					file.path(docs_dir,
						  sprintf("%s-class.Rd", classes0[item])),
					where = environment)
	       })
	sapply(methodsList,
	       function(item) {
		   methods::promptMethods(item,
					  filename =
					  file.path(docs_dir,
						    sprintf("%s-methods.Rd", methods0[item])),
					  findMethods(item, where = environment))
	       })
    }))
    ## don't document generic functions from other packages
    for(item in methodsList) {
        if(exists(item, envir = environment, inherits = FALSE)) {
            ff <- get(item, envir = environment)
            if(is(ff, "genericFunction") && !identical(ff@package, name)) # don't document
                file.remove(file.path(docs_dir, sprintf("%s.Rd", list0[item])))
        }
    }
    if(inherits(yy, "try-error"))
	stop(yy)

    ## Now we may have created an empty data or R directory
    if(length(list.files(code_dir)) == 0L)
        unlink(code_dir, recursive = TRUE)
    if(length(list.files(data_dir)) == 0L)
        unlink(data_dir, recursive = TRUE)

    message("Done.")
    message(gettextf("Further steps are described in '%s'.",
                     file.path(dir, "Read-and-delete-me")),
            domain = NA)
}

.fixPackageFileNames <- function(list) {
        ## Some object names may not be valid file names, especially
        ## replacement function names.  And if we start changing them
        ## they may collide.
        ## <NOTE>
        ## If we use given code files, we could still check whether
        ## these file are valid across platforms ...
        ## </NOTE>
        if(length(list) == 0L) return(list)
        list0 <- gsub("[[:cntrl:]\"*/:<>?\\|]", "_", list)
        wrong <- grep("^(con|prn|aux|clock\\$|nul|lpt[1-3]|com[1-4])(\\..*|)$",
                      list0)
        if(length(wrong))
            list0[wrong] <- paste("zz", list0[wrong], sep="")
        ok <- grep("^[[:alnum:]]", list0)
        if(length(ok) < length(list0))
            list0[-ok] <- paste("z", list0[-ok], sep="")
        ## now on Windows lower/uppercase will collide too
        list1 <- tolower(list0)
        list2 <- make.unique(list1, sep="_")
        changed <- (list2 != list1)
        list0[changed] <- list2[changed]
        list0
}
#  File src/library/utils/R/packageStatus.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

packageStatus <- function(lib.loc = NULL, repositories = NULL, method,
                          type = getOption("pkgType"))
{
    newestVersion <- function(x)
    {
        vers <- package_version(x)
	max <- vers[1L]
        for (i in seq_along(vers)) if (max < vers[i]) max <- vers[i]
	which(vers == max)[1L]
    }

    if(is.null(lib.loc))
        lib.loc <- .libPaths()
    if(is.null(repositories))
        repositories <- contrib.url(getOption("repos"), type = type)

    ## convert character matrices to dataframes
    char2df <- function(x)
    {
        y <- list()
        for(k in 1L:ncol(x)) y[[k]] <- x[,k]
        attr(y, "names") <- colnames(x)
        attr(y, "row.names") <- y[[1L]]
        class(y) <- "data.frame"
        y
    }

    y <- char2df(installed.packages(lib.loc = lib.loc))
    y[, "Status"] <- "ok"

    z <- available.packages(repositories, method)
    ## only consider the newest version of each package
    ## in the first repository where it appears
    ztab <- table(z[,"Package"])
    for(pkg in names(ztab)[ztab>1]){
        zrow <- which(z[,"Package"]==pkg)
        znewest <- newestVersion(z[zrow,"Version"])
        ## and now exclude everything but the newest
        z <- z[-zrow[-znewest],]
    }

    z <- cbind(z, Status = "not installed")
    z[z[,"Package"] %in% y$Package, "Status"] <- "installed"
    ## Careful: bundles can be partially installed!
    bundles <- which(!is.na(z[,"Bundle"]))
    for(bundle in bundles) {
        contains <- z[bundle, "Contains"]
        contains <- strsplit(contains, "[[:space:]]+")[[1L]]
        if(all(contains %in% y$Package)) z[bundle, "Status"] <- "installed"
    }

    z <- char2df(z)
    z$Package <- ifelse(is.na(z$Bundle), z$Package, z$Bundle)
    attr(z, "row.names") <- z$Package

    for(k in 1L:nrow(y)){
        pkg <- ifelse(is.na(y$Bundle[k]), y[k, "Package"], y[k, "Bundle"])
        if(pkg %in% z$Package) {
            if(package_version(y[k, "Version"]) <
               package_version(z[pkg, "Version"])) {
                y[k, "Status"] <- "upgrade"
            }
        } else {
            if(!(y[k, "Priority"] %in% "base")) y[k, "Status"] <- "unavailable"
        }
    }

    y$LibPath <- factor(y$LibPath, levels=lib.loc)
    y$Status <- factor(y$Status, levels=c("ok", "upgrade", "unavailable"))
    z$Repository <- factor(z$Repository, levels=repositories)
    z$Status <- factor(z$Status, levels=c("installed", "not installed"))

    retval <- list(inst=y, avail=z)
    class(retval) <- "packageStatus"
    retval
}

summary.packageStatus <- function(object, ...)
{
    cat("\nInstalled packages:\n")
    cat(  "-------------------\n")
    for(k in levels(object$inst$LibPath)){
        ok <- (object$inst$LibPath==k)
        cat("\n*** Library ", k, "\n", sep="")
        if(any(ok)){
            i <- object$inst
            Package <- ifelse(is.na(i$Bundle), i$Package,
                              paste(i$Bundle, i$Package, sep=":"))
            print(tapply(Package[ok], i$Status[ok],
                         function(x) sort(as.character(x))))
        }
    }
    cat("\n\nAvailable packages:\n")
    cat(    "-------------------\n")
    cat("(each package appears only once)\n")
    for(k in levels(object$avail$Repository)){
        cat("\n*** Repository ", k, "\n", sep="")
        ok <- object$avail$Repository==k
        if(any(ok))
            print(tapply(object$avail$Package[ok],
                         object$avail$Status[ok],
                         function(x) sort(as.character(x))))
    }
    invisible(object)
}

print.packageStatus <- function(x, ...)
{
    cat("Number of installed packages:\n")
    print(table(x$inst$LibPath, x$inst$Status))

    cat("\nNumber of available packages (each package/bundle counted only once):\n")
    print(table(x$avail$Repository, x$avail$Status))
    invisible(x)
}

update.packageStatus <-
    function(object, lib.loc=levels(object$inst$LibPath),
             repositories=levels(object$avail$Repository),
             ...)
{
    packageStatus(lib.loc=lib.loc, repositories=repositories)
}


upgrade <- function(object, ...)
    UseMethod("upgrade")

upgrade.packageStatus <- function(object, ask=TRUE, ...)
{
    update <- NULL
    old <- which(object$inst$Status == "upgrade")
    if(length(old) == 0L) {
        cat("Nothing to do!\n")
        return(invisible())
    }

    askprint <- function(x)
        write.table(x, row.names=FALSE, col.names=FALSE, quote=FALSE,
                    sep=" at ")

    haveasked <- character(0L)
    if(ask) {
        for(k in old) {
            pkg <- ifelse(is.na(object$inst[k, "Bundle"]),
                          object$inst[k, "Package"],
                          object$inst[k, "Bundle"])
            tmpstring <- paste(pkg, as.character(object$inst[k, "LibPath"]))
            if(tmpstring %in% haveasked) next
            haveasked <- c(haveasked, tmpstring)
            cat("\n")
            cat(pkg, ":\n")
            askprint(object$inst[k,c("Version", "LibPath")])
            askprint(object$avail[pkg, c("Version", "Repository")])
            answer <- substr(readline("Update (y/N/x)?  "), 1L, 1L)
            if(answer == "c" | answer == "c") {
                cat("cancelled by user\n")
                return(invisible())
            }
            if(answer == "y" | answer == "Y")
                update <-
                    rbind(update,
                          c(pkg, as.character(object$inst[k, "LibPath"]),
                            as.character(object$avail[pkg, "Repository"])))
        }
    } else {
        pkgs <- ifelse(is.na(object$inst[ ,"Bundle"]),
                       object$inst[ ,"Package"], object$inst[ ,"Bundle"])
        update <- cbind(pkgs, as.character(object$inst[ , "LibPath"]),
                        as.character(object$avail[pkgs, "Repository"]))
        update <- update[old, , drop=FALSE]
    }

    if(length(update)) {
        for(repo in unique(update[,3])) {
            ok <- update[, 3] == repo
            install.packages(update[ok, 1], update[ok, 2], contriburl = repo)
        }
    }
}
#  File src/library/utils/R/packages.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

available.packages <-
function(contriburl = contrib.url(getOption("repos"), type), method,
         fields = NULL, type = getOption("pkgType"), filters = NULL)
{
    requiredFields <-
        c(tools:::.get_standard_repository_db_fields(), "File")
    if (is.null(fields))
	fields <- requiredFields
    else {
	stopifnot(is.character(fields))
	fields <- unique(c(requiredFields, fields))
    }

    res <- matrix(NA_character_, 0L, length(fields) + 1L,
		  dimnames = list(NULL, c(fields, "Repository")))

    for(repos in contriburl) {
        localcran <- length(grep("^file:", repos)) > 0L
        if(localcran) {
            ## see note in download.packages
            if(substring(repos, 1L, 8L) == "file:///") {
                tmpf <- paste(substring(repos, 8L), "PACKAGES", sep = "/")
                if(.Platform$OS.type == "windows") {
                    if(length(grep("^/[A-Za-z]:", tmpf)))
                        tmpf <- substring(tmpf, 2L)
                }
            } else {
                tmpf <- paste(substring(repos, 6L), "PACKAGES", sep = "/")
            }
            res0 <- read.dcf(file = tmpf)
            if(length(res0)) rownames(res0) <- res0[, "Package"]
        } else {
            dest <- file.path(tempdir(),
                              paste("repos_",
                                    URLencode(repos, TRUE),
                                    ".rds", sep=""))
            if(file.exists(dest)) {
                res0 <- .readRDS(dest)
            } else {
                tmpf <- tempfile()
                on.exit(unlink(tmpf))
                op <- options("warn")
                options(warn = -1)
                ## This is a binary file
                z <- tryCatch(download.file(url = paste(repos, "PACKAGES.gz", sep = "/"),
                                            destfile = tmpf, method = method,
                                            cacheOK = FALSE, quiet = TRUE, mode = "wb"),
                         error = identity)
                if(inherits(z, "error")) {
                    ## read.dcf is going to interpret CRLF as LF, so use
                    ## binary mode to avoid CRCRLF.
                    z <- tryCatch(download.file(url=paste(repos, "PACKAGES", sep = "/"),
                                                destfile = tmpf, method = method,
                                                cacheOK = FALSE, quiet = TRUE,
                                                mode = "wb"),
                             error = identity)
                }
                options(op)
                if(inherits(z, "error")) {
                    warning(gettextf("unable to access index for repository %s", repos),
                            call. = FALSE, immediate. = TRUE, domain = NA)
                    next
                }
                res0 <- read.dcf(file = tmpf)
                if(length(res0)) rownames(res0) <- res0[, "Package"]
                .saveRDS(res0, dest, compress = TRUE)
                unlink(tmpf)
                on.exit()
            } # end of download vs cached
        } # end of localcran vs online
        if (length(res0)) {
            missingFields <- fields[!(fields %in% colnames(res0))]
            if (length(missingFields)) {
                toadd <- matrix(NA_character_, nrow=nrow(res0),
                                ncol=length(missingFields),
                                dimnames=list(NULL, missingFields))
                res0 <- cbind(res0, toadd)
            }
            if ("Path" %in% colnames(res0)) {
                rp <- rep.int(repos, nrow(res0))
                path <- res0[, "Path"]
                rp[!is.na(path)] <- paste(repos, path[!is.na(path)], sep="/")
            } else rp <- repos
            res0 <- cbind(res0[, fields, drop = FALSE], Repository = rp)
            res <- rbind(res, res0)
        }
    }

    if(!length(res)) return(res)

    if(is.null(filters)) {
        filters <- getOption("available_packages_filters")
        if(is.null(filters))
            filters <- available_packages_filters_default
    }
    if(is.list(filters)) {
        ## If filters is a list with an add = TRUE element, add the
        ## given filters to the default ones.
        if(identical(filters$add, TRUE)) {
            filters$add <- NULL
            filters <- c(available_packages_filters_default, filters)
        }
    }
    for(f in filters) {
        if(!length(res)) break
        if(is.character(f)) {
            ## Look up the filters db.
            ## Could be nice and allow abbrevs or ignore case.
            f <- available_packages_filters_db[[f[1L]]]
        }
        if(!is.function(f))
            stop("Invalid 'filters' argument.")
        res <- f(res)
    }

    res
}

available_packages_filters_default <-
    c("R_version", "OS_type", "duplicates")

available_packages_filters_db <- new.env()

available_packages_filters_db$R_version <-
function(db)
{
    ## Ignore packages which don't fit our version of R.
    depends <- db[, "Depends"]
    depends[is.na(depends)] <- ""
    ## Collect the (versioned) R depends entries.
    x <- lapply(strsplit(sub("^[[:space:]]*", "", depends),
                             "[[:space:]]*,[[:space:]]*"),
                function(s) s[grepl("^R[[:space:]]*\\(", s)])
    lens <- sapply(x, length)
    pos <- which(lens > 0L)
    if(!length(pos)) return(db)
    lens <- lens[pos]
    ## Unlist.
    x <- unlist(x)
    pat <- "^R[[:space:]]*\\(([[<>=!]+)[[:space:]]+(.*)\\)[[:space:]]*"
    ## Extract ops.
    ops <- sub(pat, "\\1", x)
    ## Split target versions accordings to ops.
    v_t <- split(sub(pat, "\\2", x), ops)
    ## Current R version.
    v_c <- getRversion()
    ## Compare current to target grouped by op.
    res <- logical(length(x))
    for(op in names(v_t))
        res[ops == op] <- do.call(op, list(v_c, v_t[[op]]))
    ## And assemble test results according to the rows of db.
    ind <- rep.int(TRUE, NROW(db))
    ind[pos] <- sapply(split(res, rep.int(seq_along(lens), lens)), all)
    db[ind, , drop = FALSE]
}

available_packages_filters_db$OS_type <-
function(db)
{
    ## Ignore packages that do not fit our OS.
    OS_type <- db[, "OS_type"]
    db[is.na(OS_type) | (OS_type == .Platform$OS.type), , drop = FALSE]
}

available_packages_filters_db$duplicates <-
function(db)
    tools:::.remove_stale_dups(db)

available_packages_filters_db$`license/FOSS` <-
function(db)
{
    ## What we need to do is find all non-FOSS-verifiable packages and
    ## all their recursive dependencies.  Somewhat tricky because there
    ## may be dependencies missing from the package db.
    ## Hence, for efficiency reasons, do the following.
    ## Create a data frame which already has Depends/Imports/LinkingTo
    ## info in package list form, and use this to compute the out of db
    ## packages.
    db1 <- data.frame(Package = db[, "Package"],
                      stringsAsFactors = FALSE)
    fields <- c("Depends", "Imports", "LinkingTo")
    for(f in fields)
        db1[[f]] <-
            lapply(db[, f], tools:::.extract_dependency_package_names)

    all_packages <- unique(unlist(db1[fields], use.names = FALSE))
    bad_packages <-
        all_packages[is.na(match(all_packages, db1$Package))]
    ## Dependency package names missing from the db can be
    ## A. base packages
    ## B. bundle packages
    ## C. really missing.
    ## We can ignore type A as these are known to be FOSS.
    ## We cannot really fully fix bundles as metadata for bundle
    ## packages (which might have license specs) are not recorded in the
    ## PACKAGES files.  And as bundles on the way out ...
    bad_packages <-
        bad_packages[is.na(match(bad_packages,
                                 unlist(tools:::.get_standard_package_names())))]
    ## <FIXME>
    ## Fix bundle packages ... maybe.
    ## </FIXME>

    ## Packages in the db not verifiable as FOSS.
    ind <- !tools:::analyze_licenses(db[, "License"])$is_verified
    ## Now find the recursive reverse dependencies of these and the
    ## packages missing from the db.
    depends <-
        tools:::.package_dependencies(db1$Package[ind], db = db1,
                                      reverse = TRUE, recursive = TRUE)
    depends <- unique(unlist(depends))
    ind[match(depends, db1$Package, nomatch = 0L)] <- TRUE

    ## And drop these from the db.
    db[!ind, , drop = FALSE]
}

## unexported helper function
simplifyRepos <- function(repos, type)
{
    tail <- substring(contrib.url("---", type), 4L)
    ind <- regexpr(tail, repos, fixed=TRUE)
    ind <- ifelse(ind > 0L, ind-1L, nchar(repos, type="c"))
    substr(repos, 1L, ind)
}

update.packages <- function(lib.loc = NULL, repos = getOption("repos"),
                            contriburl = contrib.url(repos, type),
                            method, instlib = NULL, ask = TRUE,
                            available = NULL, oldPkgs = NULL, ...,
                            checkBuilt = FALSE, type = getOption("pkgType"))
{
    force(ask)  # just a check that it is valid before we start work
    text.select <- function(old)
    {
        update <- NULL
        for(k in seq_len(nrow(old))) {
            cat(old[k, "Package"], ":\n",
                "Version", old[k, "Installed"],
                "installed in", old[k, "LibPath"],
                if(checkBuilt) paste("built under R", old[k, "Built"]),
                "\n",
                "Version", old[k, "ReposVer"], "available at",
                simplifyRepos(old[k, "Repository"], type))
            cat("\n")
            answer <- substr(readline("Update (y/N/c)?  "), 1L, 1L)
            if(answer == "c" | answer == "C") {
                cat("cancelled by user\n")
                return(invisible())
            }
            if(answer == "y" | answer == "Y")
                update <- rbind(update, old[k,])
        }
        update
    }

    if(is.null(lib.loc))
        lib.loc <- .libPaths()

    if(is.null(available))
        available <- available.packages(contriburl = contriburl,
                                        method = method)
    if(is.null(oldPkgs)) {
        ## since 'available' is supplied, 'contriburl' and 'method' are unused
	oldPkgs <- old.packages(lib.loc = lib.loc,
				contriburl = contriburl, method = method,
				available = available, checkBuilt = checkBuilt)
	if(is.null(oldPkgs))
	    return(invisible())
    }
    else if(!(is.matrix(oldPkgs) && is.character(oldPkgs)))
	stop("invalid 'oldPkgs'; must be a result from old.packages()")

    if(is.character(ask) && ask == "graphics") {
        if(.Platform$OS.type == "unix" && .Platform$GUI != "AQUA"
           && capabilities("tcltk") && capabilities("X11")) {
            k <- tcltk::tk_select.list(oldPkgs[,1L], oldPkgs[,1L], multiple = TRUE,
                                       title = "Packages to be updated")
            update <- oldPkgs[match(k, oldPkgs[,1L]), , drop=FALSE]
        } else if(.Platform$OS.type == "windows" || .Platform$GUI == "AQUA") {
            k <- select.list(oldPkgs[,1L], oldPkgs[,1L], multiple = TRUE,
                             title = "Packages to be updated")
            update <- oldPkgs[match(k, oldPkgs[,1L]), , drop=FALSE]
        } else update <- text.select(oldPkgs)
        if(nrow(update) == 0L) return(invisible())
    } else if(is.logical(ask) && ask) update <- text.select(oldPkgs)
    else update <- oldPkgs


    if(!is.null(update)) {
        if(is.null(instlib)) instlib <-  update[, "LibPath"]
        ## do this a library at a time, to handle dependencies correctly.
        libs <- unique(instlib)
        for(l in libs)
            install.packages(update[instlib == l , "Package"], l,
                             contriburl = contriburl, method = method,
                             available = available, ..., type = type)
    }
}

old.packages <- function(lib.loc = NULL, repos = getOption("repos"),
                         contriburl = contrib.url(repos, type),
                         instPkgs = installed.packages(lib.loc = lib.loc),
                         method, available = NULL, checkBuilt = FALSE,
                         type = getOption("pkgType"))
{
    if(is.null(lib.loc))
        lib.loc <- .libPaths()
    if(!missing(instPkgs)) {
        ## actually we need rather more than this
        if(!is.matrix(instPkgs) || !is.character(instPkgs[, "Package"]))
            stop("illformed 'instPkgs' matrix")
    }
    if(NROW(instPkgs) == 0L) return(NULL)

    available <- if(is.null(available))
        available.packages(contriburl = contriburl, method = method)
    else tools:::.remove_stale_dups(available)

    ## This should be true in the PACKAGES file, is on CRAN
    ok <- !is.na(available[, "Bundle"])
    available[ok, "Package"] <- available[ok, "Bundle"]

    update <- NULL

    currentR <- minorR <- getRversion()
    minorR[[c(1L, 3L)]] <- 0L # set patchlevel to 0
    for(k in 1L:nrow(instPkgs)) {
        if (instPkgs[k, "Priority"] %in% "base") next
        z <- match(instPkgs[k, "Package"], available[, "Package"])
        if(is.na(z)) {
            bundle <- instPkgs[k, "Bundle"]
            if(!is.na(bundle)) {
                ## no match to the package, so look for update to the bundle
                z <- match(bundle, available[, "Package"])
                if(is.na(z)) next
                instPkgs[k, "Package"] <- bundle
            } else next
        }
        onRepos <- available[z, ]
        ## works OK if Built: is missing (which it should not be)
	if((!checkBuilt || package_version(instPkgs[k, "Built"]) >= minorR) &&
           package_version(onRepos["Version"]) <=
           package_version(instPkgs[k, "Version"])) next
        deps <- onRepos["Depends"]
        if(!is.na(deps)) {
            Rdeps <- tools:::.split_dependencies(deps)[["R", exact=TRUE]]
            if(length(Rdeps) > 1L) {
                target <- Rdeps$version
                res <- eval(parse(text=paste("currentR", Rdeps$op, "target")))
                if(!res) next
            }
        }
        update <- rbind(update,
                        c(instPkgs[k, c("Package", "LibPath", "Version", "Built")],
                          onRepos["Version"], onRepos["Repository"]))
    }
    if(!is.null(update))
        colnames(update) <- c("Package", "LibPath", "Installed", "Built",
                              "ReposVer", "Repository")
    rownames(update) <- update[, "Package"]
    ## finally, remove duplicate rows (which bundles may give)
    update[!duplicated(update), , drop = FALSE]
}

new.packages <- function(lib.loc = NULL, repos = getOption("repos"),
                         contriburl = contrib.url(repos, type),
                         instPkgs = installed.packages(lib.loc = lib.loc),
                         method, available = NULL, ask = FALSE,
                         ..., type = getOption("pkgType"))
{
    ask  # just a check that it is valid before we start work
    if(is.null(lib.loc)) lib.loc <- .libPaths()
    if(!is.matrix(instPkgs))
        stop(gettextf("no installed packages for (invalid?) 'lib.loc=%s'",
                      lib.loc), domain = NA)
    if(is.null(available))
        available <- available.packages(contriburl = contriburl,
                                        method = method)

    installed <- unique(instPkgs[, "Package"])

    ## We need to work out what to do with bundles, and unfortunately some
    ## people have used the same name for a bundle and a package (both as
    ## one of the components and replacing a package by a bundle or v.v.).
    ## Since 'available' will have names of bundles or single packages, we
    ## add the bundle names to the list of installed packages: this means
    ## that if there is a package installed with the name of a bundle, the
    ## bundle is regarded as installed.
    ok <- !is.na(instPkgs[, "Bundle"])
    if(any(ok)) installed <- c(installed, unique(instPkgs[ok, "Bundle"]))

    poss <- sort(unique(available[ ,"Package"])) # sort in local locale
    res <- setdiff(poss, installed)

    update <- character(0L)
    if(is.character(ask) && ask == "graphics") {
        if(.Platform$OS.type == "unix"
           && capabilities("tcltk") && capabilities("X11")) {
            k <- tcltk::tk_select.list(res, multiple = TRUE,
                                       title = "New packages to be installed")
            update <- res[match(k, res)]
        } else if(.Platform$OS.type == "windows" || .Platform$GUI == "AQUA") {
            k <- select.list(res, multiple = TRUE,
                             title = "New packages to be installed")
            update <- res[match(k, res)]
        }
    } else if(is.logical(ask) && ask)
        update <- res[match(select.list(res, multiple = TRUE,
                                        title = "New packages to be installed")
                            , res)]
    if(length(update)) {
        install.packages(update, lib = lib.loc[1L], contriburl = contriburl,
                         method = method, available = available,
                         type = type, ...)
        # Now check if they were installed and update 'res'
        dirs <- list.files(lib.loc[1L])
        updated <- update[update %in% dirs]
        # Need to check separately for bundles
        av <- available[update, , drop = FALSE]
        bundles <- av[!is.na(av[, "Contains"]), , drop=FALSE]
        for(bundle in rownames(bundles)) {
            contains <- strsplit(bundles[bundle, "Contains"],
                                 "[[:space:]]+")[[1L]]
            if(all(contains %in% dirs)) updated <- c(updated, bundle)
        }
        res <- res[!res %in% updated]
    }
    res
}

.instPkgFields <- function(fields) {
    ## to be used in installed.packages() and similar
    requiredFields <-
        c(tools:::.get_standard_repository_db_fields(), "Built")
    if (is.null(fields))
	fields <- requiredFields
    else {
	stopifnot(is.character(fields))
	fields <- unique(c(requiredFields, fields))
    }
    ## Don't retain 'Package' and 'LibPath' fields as these are used to
    ## record name and path of installed packages.
    fields[! fields %in% c("Package", "LibPath")]
}


## Read packages' Description and aggregate 'fields' into a character matrix
## NB: this does not handle encodings, so only suitable for
## ASCII-only fields.
.readPkgDesc <- function(lib, fields, pkgs = list.files(lib))
{
    ## to be used in installed.packages() and similar
    ret <- matrix(NA_character_, length(pkgs), 2L+length(fields))
    for(i in seq_along(pkgs)) {
        pkgpath <- file.path(lib, pkgs[i])
        if(file.access(pkgpath, 5L)) next
        pkgpath <- file.path(pkgpath, "DESCRIPTION")
        if(file.access(pkgpath, 4L)) next
        desc <- tryCatch(read.dcf(pkgpath, fields = fields), error = identity)
        if(inherits(desc, "error")) {
            warning(gettextf("read.dcf() error on file '%s'", pkgpath),
                    domain=NA, call.=FALSE)
            next
        }
        desc <- desc[1,]
        Rver <- strsplit(strsplit(desc["Built"], ";")[[1L]][1L],
                         "[ \t]+")[[1L]][2L]
        desc["Built"] <- Rver
        ret[i, ] <- c(sub("_.*", "", pkgs[i]), lib, desc)
    }
    ret[!is.na(ret[, 1L]), ]
}

installed.packages <-
    function(lib.loc = NULL, priority = NULL, noCache = FALSE,
             fields = NULL)
{
    if(is.null(lib.loc))
        lib.loc <- .libPaths()
    if(!is.null(priority)) {
        if(!is.character(priority))
            stop("'priority' must be character or NULL")
        if(any(b <- priority %in% "high"))
            priority <- c(priority[!b], "recommended","base")
    }

    fields <- .instPkgFields(fields)
    retval <- matrix("", 0L, 2L + length(fields))
    for(lib in lib.loc) {
	dest <- file.path(tempdir(),
			  paste("libloc_", URLencode(lib, TRUE),
				paste(fields, collapse=","), ".rds",
				sep=""))
	if(!noCache && file.exists(dest) &&
	    file.info(dest)$mtime > file.info(lib)$mtime) {
	    ## use the cache file
	    retval <- rbind(retval, .readRDS(dest))
	} else {
	    ret0 <- .readPkgDesc(lib, fields)
	    if(length(ret0)) {
		retval <- rbind(retval, ret0)
		## save the cache file
		.saveRDS(ret0, dest, compress = TRUE)
	    }
	}
    }

    .fixupPkgMat(retval, fields, priority)
}

.fixupPkgMat <- function(mat, fields, priority) {
    ## to be used in installed.packages() and similar
    colnames(mat) <- c("Package", "LibPath", fields)
    if(length(mat) && !is.null(priority)) {
	keep <- !is.na(pmatch(mat[,"Priority"], priority,
			      duplicates.ok = TRUE))
	mat <- mat[keep, , drop=FALSE]
    }
    if (length(mat)) {
	rownames(mat) <- mat[, "Package"]
    }
    mat
}


remove.packages <- function(pkgs, lib)
{
    updateIndices <- function(lib) {
        ## This should eventually be made public, as it could also be
        ## used by install.packages() && friends.
        if(lib == .Library) {
            if(exists("link.html.help", mode = "function"))
                link.html.help()
        }
    }

    if(!length(pkgs)) return(invisible())

    if(missing(lib) || is.null(lib)) {
        lib <- .libPaths()[1L]
        warning(gettextf("argument 'lib' is missing: using %s", lib),
                immediate. = TRUE, domain = NA)
    }
    ## For bundles, remove all of the bundle.  This should remain
    ## this way even if bundles are unbundled, for back compatibility.
    have <- installed.packages(lib.loc=lib)
    is_bundle <- pkgs %in% have[, "Bundle"]
    pkgs0 <- pkgs; pkgs <- pkgs[!is_bundle]
    for(p in pkgs0[is_bundle]) {
        add <- have[have[, "Bundle"] %in% p, "Package"]
        pkgs <- c(pkgs, add)
    }

    paths <- .find.package(pkgs, lib)
    if(length(paths)) {
        unlink(paths, TRUE)
        for(lib in unique(dirname(paths))) updateIndices(lib)
    }
    invisible()
}

download.packages <- function(pkgs, destdir, available = NULL,
                              repos = getOption("repos"),
                              contriburl = contrib.url(repos, type),
                              method, type = getOption("pkgType"), ...)
{
    dirTest <- function(x) !is.na(isdir <- file.info(x)$isdir) & isdir

    nonlocalcran <- length(grep("^file:", contriburl)) < length(contriburl)
    if(nonlocalcran && !dirTest(destdir))
        stop("'destdir' is not a directory")
    if(is.null(available))
        available <- available.packages(contriburl=contriburl, method=method)

    retval <- matrix(character(0L), 0L, 2L)
    for(p in unique(pkgs))
    {
        ## Normally bundles have a Package field of the same name, but
        ## allow for repositories that do not.
        ok <- (available[,"Package"] == p) | (available[,"Bundle"] == p)
        ok <- ok & !is.na(ok)
        if(!any(ok))
            warning(gettextf("no package '%s' at the repositories", p),
                    domain = NA, immediate. = TRUE)
        else {
            if(sum(ok) > 1L) { # have multiple copies
                vers <- package_version(available[ok, "Version"])
                keep <- vers == max(vers)
                keep[duplicated(keep)] <- FALSE
                ok[ok][!keep] <- FALSE
            }
            if (substr(type, 1L, 10L) == "mac.binary") type <- "mac.binary"
            ## in Oct 2009 we introduced file names in PACKAGES files
            File <- available[ok, "File"]
            fn <- paste(p, "_", available[ok, "Version"],
                        switch(type,
                               "source" = ".tar.gz",
                               "mac.binary" = ".tgz",
                               "win.binary" = ".zip"),
                        sep = "")
            have_fn <- !is.na(File)
            fn[have_fn] <- File[have_fn]
            repos <- available[ok, "Repository"]
            if(length(grep("^file:", repos)) > 0L) { # local repository
                ## This could be file: + file path or a file:/// URL.
                if(substring(repos, 1L, 8L) == "file:///") {
                    ## We need to derive the file name from the URL
                    ## This is tricky as so many forms have been allowed,
                    ## and indeed external methods may do even more.
                    fn <- paste(substring(repos, 8L), fn, sep = "/")
                    ## This leaves a path beginning with /
                    if(.Platform$OS.type == "windows") {
                        if(length(grep("^/[A-Za-z]:", fn)))
                            fn <- substring(fn, 2L)
                    }
                } else {
                    fn <- paste(substring(repos, 6L), fn, sep = "/")
                }
                if(file.exists(fn))
                    retval <- rbind(retval, c(p, fn))
                else
                    warning(gettextf("package '%s' does not exist on the local repository", p),
                            domain = NA, immediate. = TRUE)
            } else {
                url <- paste(repos, fn, sep="/")
                destfile <- file.path(destdir, fn)

                res <- try(download.file(url, destfile, method, mode="wb", ...))
                if(!inherits(res, "try-error") && res == 0L)
                    retval <- rbind(retval, c(p, destfile))
                else
                    warning(gettextf("download of package '%s' failed", p),
                            domain = NA, immediate. = TRUE)
            }
        }
    }

    retval
}

contrib.url <- function(repos, type = getOption("pkgType"))
{
    if(is.null(repos)) return(NULL)
    if("@CRAN@" %in% repos && interactive()) {
        cat(gettext("--- Please select a CRAN mirror for use in this session ---\n"))
        flush.console()
        chooseCRANmirror()
        m <- match("@CRAN@", repos)
        nm <- names(repos)
        repos[m] <- getOption("repos")["CRAN"]
        if(is.null(nm)) nm <- rep("", length(repos))
        nm[m] <- "CRAN"
        names(repos) <- nm
    }
    if("@CRAN@" %in% repos) stop("trying to use CRAN without setting a mirror")

    ver <- paste(R.version$major,
                 strsplit(R.version$minor, ".", fixed=TRUE)[[1L]][1L], sep = ".")
    mac.subtype <- "universal"
    if (substr(type, 1L, 11L) == "mac.binary.") {
        mac.subtype <- substring(type, 12L)
        type <- "mac.binary"
    }
    res <- switch(type,
		"source" = paste(gsub("/$", "", repos), "src", "contrib", sep="/"),
                "mac.binary" = paste(gsub("/$", "", repos), "bin", "macosx", mac.subtype, "contrib", ver, sep = "/"),
                "win.binary" = paste(gsub("/$", "", repos), "bin", "windows", "contrib", ver, sep="/")
               )
    res
}


getCRANmirrors <- function(all=FALSE, local.only=FALSE)
{
    m <- NULL
    if(!local.only) {
        ## try to handle explicitly failure to connect to CRAN.
        con <- url("http://cran.r-project.org/CRAN_mirrors.csv")
        m <- try(open(con, "r"), silent = TRUE)
        if(!inherits(m, "try-error")) m <- try(read.csv(con, as.is=TRUE))
        close(con)
    }
    if(is.null(m) || inherits(m, "try-error"))
        m <- read.csv(file.path(R.home("doc"), "CRAN_mirrors.csv"), as.is=TRUE)
    if(!all) m <- m[as.logical(m$OK), ]
    m
}


chooseCRANmirror <- function(graphics = getOption("menu.graphics"))
{
    if(!interactive()) stop("cannot choose a CRAN mirror non-interactively")
    m <- getCRANmirrors(all=FALSE, local.only=FALSE)
    res <- menu(m[, 1L], graphics, "CRAN mirror")
    if(res > 0L) {
        URL <- m[res, "URL"]
        repos <- getOption("repos")
        repos["CRAN"] <- gsub("/$", "", URL[1L])
        options(repos = repos)
    }
    invisible()
}

setRepositories <-
    function(graphics = getOption("menu.graphics"), ind = NULL)
{
    if(is.null(ind) && !interactive())
        stop("cannot set repositories non-interactively")
    p <- file.path(Sys.getenv("HOME"), ".R", "repositories")
    if(!file.exists(p))
        p <- file.path(R.home("etc"), "repositories")
    a <- tools:::.read_repositories(p)
    pkgType <- getOption("pkgType")
    if(length(grep("^mac\\.binary", pkgType))) pkgType <- "mac.binary"
    thisType <- a[[pkgType]]
    a <- a[thisType, 1L:3L]
    repos <- getOption("repos")
    ## Now look for CRAN and any others in getOptions("repos")
    if("CRAN" %in% row.names(a) && !is.na(CRAN <- repos["CRAN"]))
        a["CRAN", "URL"] <- CRAN
    ## Set as default any already in the option.
    a[(a[["URL"]] %in% repos), "default"] <- TRUE
    new <- !(repos %in% a[["URL"]])
    if(any(new)) {
        aa <- names(repos[new])
        if(is.null(aa)) aa <- rep("", length(repos[new]))
        aa[aa == ""] <- repos[new][aa == ""]
        newa <- data.frame(menu_name=aa, URL=repos[new], default=TRUE)
        row.names(newa) <- aa
        a <- rbind(a, newa)
    }

    default <- a[["default"]]

    if(length(ind)) res <- as.integer(ind)
    else {
        res <- NULL
        if(graphics) {
            ## return an integer vector of row numbers, never NULL
            if(.Platform$OS.type == "windows" || .Platform$GUI == "AQUA")
                res <- match(select.list(a[, 1L], a[default, 1L], multiple = TRUE,
                                         "Repositories"), a[, 1L])
            else if(.Platform$OS.type == "unix" &&
                    capabilities("tcltk") && capabilities("X11"))
                res <- match(tcltk::tk_select.list(a[, 1L], a[default, 1L],
                                                   multiple = TRUE, "Repositories"),
                             a[, 1L])
        }
        if(is.null(res)) { ## pick up case where graphics method is N/A.
            cat(gettext("--- Please select repositories for use in this session ---\n"))
            nc <- length(default)
            cat("", paste(seq_len(nc), ": ",
                          ifelse(default, "+", " "), " ", a[, 1L],
                          sep=""),
                "", sep="\n")
            cat(gettext("Enter one or more numbers separated by spaces, or an empty line to cancel\n"))
            res <- scan("", what=0, quiet=TRUE, nlines=1L)
            if(!length(res) || (length(res) == 1L && !res[1L]))
                return(invisible())
            res <- res[1 <= res && res <= nc]
        }
    }
    if(length(res)) {
        repos <- a[["URL"]]
        names(repos) <- row.names(a)
        options(repos = repos[res])
    }
}

normalizePath <- function(path) .Internal(normalizePath(path))


## used in some BioC packages and their support in tools.
compareVersion <- function(a, b)
{
    if(is.na(a)) return(-1L)
    if(is.na(b)) return(1L)
    a <- as.integer(strsplit(a, "[\\.-]")[[1L]])
    b <- as.integer(strsplit(b, "[\\.-]")[[1L]])
    for(k in seq_along(a))
        if(k <= length(b)) {
            if(a[k] > b[k]) return(1) else if(a[k] < b[k]) return(-1L)
        } else return(1L)
    if(length(b) > length(a)) return(-1L) else return(0L)
}

## ------------- private functions --------------------
.find_bundles <- function(available, all=TRUE)
{
    ## Sort out bundles. Returns a named list of character vectors
    ## Once upon a time all=TRUE told it about the VR bundle.
    ## which might not have been in 'available' on Windows.
    bundles <- available[!is.na(available[, "Bundle"]), "Contains"]
    strsplit(bundles, "[[:space:]]+")
}

.clean_up_dependencies <- function(x, available = NULL)
{
    ## x is a character vector of Depends / Suggests / Imports entries
    ## returns a character vector of all the package dependencies mentioned
    x <- x[!is.na(x)]
    if(!length(x)) return(x)
    x <- unlist(strsplit(x, ","))
    unique(sub("^[[:space:]]*([[:alnum:].]+).*$", "\\1" , x))
}

.clean_up_dependencies2 <- function(x, installed, available)
{
    ## x is a character vector of Depends / Suggests / Imports entries.
    ## Returns a list of length 2, a character vector of the names of
    ## all the package dependencies mentioned that are not already
    ## satisfied and one of those which cannot be satisfied (possibly
    ## of the form "pkg (>= ver)')

    .split_dependencies <- function(x) {
        .split2 <- function(x) {
            ## some have had space before ,
            x <- sub('[[:space:]]+$', '', x)
            x <- unique(sub("^[[:space:]]*(.*)", "\\1" , x))
            names(x) <- sub("^([[:alnum:].]+).*$", "\\1" , x)
            x <- x[names(x) != "R"]
            ## FIXME: a better way to handle duplicates.
            ## However, there should not be any, and if there are
            ## Depends: should be the first.
            x <- x[!duplicated(names(x))]
            lapply(x, tools:::.split_op_version)
        }
        ## given one of more concatenations of Depends/Imports/Suggests fields,
        ## return a named list of list(name, [op, version])
        if(!any(nzchar(x))) return(list())
        unlist(lapply(strsplit(x, ","), .split2), FALSE, FALSE)
    }
    x <- x[!is.na(x)]
    if(!length(x)) return(list(character(0L), character(0L)))
    xx <- .split_dependencies(x)
    if(!length(xx)) return(list(character(0L), character(0L)))
    ## Then check for those we already have installed
    pkgs <- installed[, "Package"]
    have <- sapply(xx, function(x) {
        if(length(x) == 3L) {
            if (! x[[1L]] %in% pkgs ) return(FALSE)
            if(x[[2L]] != ">=") return(TRUE)
            ## We may have the package installed more than once
            ## which we get will depend on the .libPaths() order,
            ## so for now just see if any installed version will do.
            current <- as.package_version(installed[pkgs == x[[1L]], "Version"])
            target <- as.package_version(x[[3L]])
            eval(parse(text = paste("any(current", x$op, "target)")))
        } else x[[1L]] %in% pkgs
    })
    xx <- xx[!have]
    if(!length(xx)) return(list(character(0L), character(0L)))
    ## now check if we can satisfy the missing dependencies
    pkgs <- row.names(available)
    canget <- miss <- character(0L)
    for (i in seq_along(xx)) {
        x <- xx[[i]]
        if(length(x) == 3L) {
            if (! x[[1L]] %in% pkgs ) { miss <- c(miss, x[[1L]]); next }
            if(x[[2L]] != ">=") { canget <- c(canget, x[[1L]]); next }
            ## we may have the package available more than once
            ## install.packages() will find the highest version.
            current <- as.package_version(available[pkgs == x[[1L]], "Version"])
            target <- as.package_version(x[[3L]])
            res <- eval(parse(text = paste("any(current", x$op, "target)")))
            if(res) canget <- c(canget, x[[1L]])
            else  miss <- c(miss, paste(x[[1L]], " (>= ", x[[3L]], ")", sep=""))
        } else if(x[[1L]] %in% pkgs) canget <- c(canget, x[[1L]])
        else miss <- c(miss, x[[1L]])
    }
    list(canget, miss)
}

.make_dependency_list <-
function(pkgs, available, dependencies = c("Depends", "Imports", "LinkingTo"))
{
    ## given a character vector of packages,
    ## return a named list of character vectors of their dependencies
    if(!length(pkgs)) return(NULL)
    if(is.null(available))
        stop(gettextf("'%s' must be supplied", available), domain = NA)
    info <- available[pkgs, dependencies, drop = FALSE]
    ## we always want a list here, but apply can simplify to a matrix.
    ## x <- apply(info, 1L, .clean_up_dependencies)
    ## if(length(pkgs) == 1) {x <- list(as.vector(x)); names(x) <- pkgs}
    x <- vector("list", length(pkgs)); names(x) <- pkgs
    for (i in seq_along(pkgs))
        x[[i]] <- .clean_up_dependencies(info[i, ])
    bundles <- .find_bundles(available)
    x <- lapply(x, function(x) if(length(x)) {
        for(bundle in names(bundles))
            x[ x %in% bundles[[bundle]] ] <- bundle
        x <- x[! x %in% c("R", "NA")]
        unique(x)
    } else x)
    x
}

.find_install_order <- function(pkgs, dependencyList)
{
    ## given a character vector of packages, find an install order
    ## which reflects their dependencies.
    DL <- dependencyList[pkgs]
    ## some of the packages may be already installed, but the
    ## dependencies apply to those being got from CRAN.
    DL <- lapply(DL, function(x) x[x %in% pkgs])
    lens <- sapply(DL, length)
    if(all(lens > 0L)) {
        warning("every package depends on at least one other")
        return(pkgs)
    }
    done <- names(DL[lens == 0L]); DL <- DL[lens > 0L]
    while(length(DL)) {
        OK <- sapply(DL, function(x) all(x %in% done))
        if(!any(OK)) {
            warning(gettextf("packages %s are mutually dependent",
                             paste(sQuote(names(DL)), collapse = ", ")),
                    domain = NA)
            return(c(done,  names(DL)))
        }
        done <- c(done, names(DL[OK]))
        DL <- DL[!OK]
    }
    done
}

#  File src/library/utils/R/packages2.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

getDependencies <-
    function(pkgs, dependencies = NA, available = NULL, lib = .libPaths()[1L])
{
    oneLib <- length(lib) == 1L
    if(is.logical(dependencies) && is.na(dependencies))
        dependencies <- c("Depends", "Imports", "LinkingTo")
    depends <-
        is.character(dependencies) || (is.logical(dependencies) && dependencies)
    if(depends && is.logical(dependencies))
        dependencies <-  c("Depends", "Imports", "LinkingTo", "Suggests")
    if(depends && !oneLib) {
        warning("Do not know which element of 'lib' to install dependencies into\nskipping dependencies")
        depends <- FALSE
    }
    bundles <- .find_bundles(available)
    for(bundle in names(bundles))
        pkgs[ pkgs %in% bundles[[bundle]] ] <- bundle
    p0 <- unique(pkgs)
    miss <-  !p0 %in% row.names(available)
    if(sum(miss)) {
        warning(sprintf(ngettext(sum(miss),
                                 "package %s is not available",
                                 "packages %s are not available"),
                        paste(sQuote(p0[miss]), collapse=", ")),
                domain = NA)
        if (sum(miss) == 1L &&
            !is.na(w <- match(tolower(p0[miss]),
                              tolower(row.names(available))))) {
            warning(sprintf("Perhaps you meant %s ?",
                            sQuote( row.names(available)[w])),
                    call. = FALSE, domain = NA)
        }
        flush.console()
    }
    p0 <- p0[!miss]

    if(depends) { # check for dependencies, recursively
        p1 <- p0 # this is ok, as 1 lib only
        ## INSTALL prepends 'lib' to the libpath
        ## Here we are slightly more conservative
        libpath <- .libPaths()
        if(!lib %in% libpath) libpath <- c(lib, libpath)
        installed <- installed.packages(lib.loc = libpath,
                                        fields = c("Package", "Version"))
        not_avail <- character()
	repeat {
	    deps <- apply(available[p1, dependencies, drop = FALSE],
                          1L, function(x) paste(x[!is.na(x)], collapse=", "))
	    res <- .clean_up_dependencies2(deps, installed, available)
            not_avail <- c(not_avail, res[[2L]])
            deps <- unique(res[[1L]])
            ## R should not get to here, but be safe
            deps <- deps[!deps %in% c("R", pkgs)]
	    if(!length(deps)) break
	    pkgs <- c(deps, pkgs)
	    p1 <- deps
	}
        if(length(not_avail)) {
            not_avail <- unique(not_avail)
            warning(sprintf(ngettext(length(not_avail),
                                     "dependency %s is not available",
                                     "dependencies %s are not available"),
                            paste(sQuote(not_avail), collapse=", ")),
                    domain = NA, call. = FALSE, immediate. = TRUE)
            flush.console()
        }

        for(bundle in names(bundles))
            pkgs[ pkgs %in% bundles[[bundle]] ] <- bundle
        pkgs <- unique(pkgs)
        pkgs <- pkgs[pkgs %in% row.names(available)]
        if(length(pkgs) > length(p0)) {
            added <- setdiff(pkgs, p0)
            message(sprintf(ngettext(length(added),
                                     "also installing the dependency %s",
                                     "also installing the dependencies %s"),
                            paste(sQuote(added), collapse=", ")),
                    "\n", domain = NA)
            flush.console()
        }
        p0 <- pkgs
    }
    p0
}

install.packages <-
    function(pkgs, lib, repos = getOption("repos"),
             contriburl = contrib.url(repos, type),
             method, available = NULL, destdir = NULL, dependencies = NA,
             type = getOption("pkgType"),
             configure.args = getOption("configure.args"),
             configure.vars = getOption("configure.vars"),
             clean = FALSE, Ncpus = getOption("Ncpus"), ...)
{
    if (is.logical(clean) && clean)
        clean <- "--clean"
    if(is.logical(dependencies) && is.na(dependencies))
        dependencies <- if(!missing(lib) && length(lib) > 1L) FALSE
        else c("Depends", "Imports", "LinkingTo")

    ## Compute the configuration arguments for a given package.
    ## If configure.args is an unnamed character vector, use that.
    ## If it is named, match the pkg name to the names of the character vector
    ## and if we get a match, use that element.
    ## Similarly, configure.args is a list(), match pkg to the names pkg and
    ## use that element, collapsing it into a single string.

    getConfigureArgs <-  function(pkg)
    {
        if(.Platform$OS.type == "windows") return(character())
        ## Since the pkg argument can be the name of a file rather than
        ## a regular package name, we have to clean that up.
        pkg <- gsub("_\\.(zip|tar\\.gz)", "",
                    gsub(.standard_regexps()$valid_package_version, "", basename(pkg)))

        if(length(pkgs) == 1L && length(configure.args) &&
           length(names(configure.args)) == 0L)
            return(paste("--configure-args=",
                         shQuote(paste(configure.args, collapse = " ")),
                         sep = ""))

        if (length(configure.args) && length(names(configure.args))
              && pkg %in% names(configure.args))
            config <- paste("--configure-args=",
                            shQuote(paste(configure.args[[ pkg ]], collapse = " ")),
                            sep = "")
        else
            config <- character()

        config
    }

    getConfigureVars <-  function(pkg)
    {
        if(.Platform$OS.type == "windows") return(character())
        ## Since the pkg argument can be the name of a file rather than
        ## a regular package name, we have to clean that up.
        pkg <- gsub("_\\.(zip|tar\\.gz)", "",
                    gsub(.standard_regexps()$valid_package_version, "", basename(pkg)))

        if(length(pkgs) == 1L && length(configure.vars) &&
           length(names(configure.vars)) == 0L)
            return(paste("--configure-vars=",
                         shQuote(paste(configure.vars, collapse = " ")),
                         sep = ""))

        if (length(configure.vars) && length(names(configure.vars))
              && pkg %in% names(configure.vars))
            config <- paste("--configure-vars=",
                            shQuote(paste(configure.vars[[ pkg ]], collapse = " ")),
                            sep = "")
        else
            config <- character()

        config
    }

    if(missing(pkgs) || !length(pkgs)) {
        ## if no packages were specified, use a menu
	if(.Platform$OS.type == "windows" || .Platform$GUI == "AQUA") {
	    SelectList <- select.list
	} else if(.Platform$OS.type == "unix" &&
		  capabilities("tcltk") && capabilities("X11")) {
	    SelectList <- tcltk::tk_select.list
	} else
	    stop("no packages were specified")

	if(is.null(available))
	    available <- available.packages(contriburl = contriburl,
					    method = method)
	if(NROW(available)) {
            explode_bundles <- function(a)
            {
                contains <- .find_bundles(a, FALSE)
                extras <- unlist(lapply(names(contains), function(x)
                                        paste(contains[[x]], " (", x, ")", sep="")))
                sort(as.vector(c(a[, 1L], extras)))
            }

            implode_bundles <- function(pkgs)
            {
                bundled <- grep(".* \\(.*\\)$", pkgs)
                if (length(bundled)) {
                    bundles <- unique(gsub(".* \\((.*)\\)$", "\\1",
                                           pkgs[bundled]))
                    pkgs <- c(pkgs[-bundled], bundles)
                }
                pkgs
            }

	    a <- explode_bundles(available)
	    pkgs <- implode_bundles(SelectList(a, multiple = TRUE,
					       title = "Packages"))
            ## avoid duplicate entries in menus, since the latest will
            ## be picked up
            pkgs <- unique(pkgs)
	}
	if(!length(pkgs)) stop("no packages were specified")
    }

    if(missing(lib) || is.null(lib)) {
        lib <- .libPaths()[1L]
        if(length(.libPaths()) > 1L)
            warning(gettextf("argument 'lib' is missing: using '%s'", lib),
                    immediate. = TRUE, domain = NA)
    }

    ## check for writability by user
    ok <- file.info(lib)$isdir & (file.access(lib, 2) == 0)
    if(length(lib) > 1 && any(!ok))
        stop(sprintf(ngettext(sum(!ok),
                              "'lib' element '%s'  is not a writable directory",
                              "'lib' elements '%s' are not writable directories"),
                     paste(lib[!ok], collapse=", ")), domain = NA)
    if(length(lib) == 1 && .Platform$OS.type == "windows") {
        ## file.access is unreliable on Windows, especially Vista.
        ## the only known reliable way is to try it
        ok <- file.info(lib)$isdir
        if(ok) {
            fn <- file.path(lib, paste("_test_dir", Sys.getpid(), sep="_"))
            unlink(fn, recursive = TRUE) # precaution
            res <- try(dir.create(fn, showWarnings = FALSE))
            if(inherits(res, "try-error") || !res) ok <- FALSE
            else unlink(fn, recursive = TRUE)
        }
    }
    if(length(lib) == 1L && !ok) {
        warning(gettextf("'lib = \"%s\"' is not writable", lib),
                domain = NA, immediate. = TRUE)
        userdir <- unlist(strsplit(Sys.getenv("R_LIBS_USER"),
                                   .Platform$path.sep))[1L]
        if(interactive() && !file.exists(userdir)) {
            msg <- gettext("Would you like to create a personal library\n'%s'\nto install packages into?")
            if(.Platform$OS.type == "windows") {
                ans <- winDialog("yesno", sprintf(msg, userdir))
                if(ans != "YES") stop("unable to install packages")
            } else {
                ans <- readline(paste(sprintf(msg, userdir), " (y/n) "))
                if(substr(ans, 1L, 1L) == "n")
                    stop("unable to install packages")
            }
            if(!dir.create(userdir, recursive = TRUE))
                stop("unable to create ", sQuote(userdir))
            lib <- userdir
            .libPaths(c(userdir, .libPaths()))
        } else stop("unable to install packages")
    }

    ## check if we should infer repos=NULL
    if(length(pkgs) == 1L && missing(repos) && missing(contriburl)) {
        if((type == "source" && length(grep("\\.tar.gz$", pkgs))) ||
           (type == "win.binary" && length(grep("\\.zip$", pkgs))) ||
           (substr(type, 1L, 10L) == "mac.binary"
            && length(grep("\\.tgz$", pkgs)))) {
            repos <- NULL
            message("inferring 'repos = NULL' from the file name")
        }
    }

    if(.Platform$OS.type == "windows") {
        if(type == "mac.binary")
            stop("cannot install MacOS X binary packages on Windows")

        if(type == "win.binary") {      # include local .zip files
            .install.winbinary(pkgs = pkgs, lib = lib, contriburl = contriburl,
                               method = method, available = available,
                               destdir = destdir,
                               dependencies = dependencies, ...)
            return(invisible())
        }
        ## Avoid problems with spaces in pathnames.
        have_spaces <- grep(" ", pkgs)
        if(length(have_spaces)) {
            ## we want the short name for the directory,
            ## but not for a .tar.gz, and package names never contain spaces.
            p <- pkgs[have_spaces]
            dirs <- shortPathName(dirname(p))
            pkgs[have_spaces] <- file.path(dirs, basename(p))
        }
        ## Avoid problems with backslashes
        ## -- will mess up UNC names, but they don't work
        pkgs <- gsub("\\\\", "/", pkgs)
    } else {
        if(substr(type, 1L, 10L) == "mac.binary") {
            if(!length(grep("darwin", R.version$platform)))
                stop("cannot install MacOS X binary packages on this plaform")
            .install.macbinary(pkgs = pkgs, lib = lib, contriburl = contriburl,
                               method = method, available = available,
                               destdir = destdir,
                               dependencies = dependencies, ...)
            return(invisible())
        }

        if(type == "win.binary")
            stop("cannot install Windows binary packages on this plaform")

        if(!file.exists(file.path(R.home("bin"), "INSTALL")))
            stop("This version of R is not set up to install source packages\nIf it was installed from an RPM, you may need the R-devel RPM")
    }

    ## we need to ensure that R CMD INSTALL runs with the same
    ## library trees as this session.
    ## FIXME: At least on Windows, either run sub-R directly (to avoid sh)
    ## or run the install in the current process.
    libpath <- .libPaths()
    libpath <- libpath[! libpath %in% .Library]
    if(length(libpath)) libpath <- paste(libpath, collapse=.Platform$path.sep)
    cmd0 <- paste(file.path(R.home("bin"),"R"), "CMD INSTALL")
    if(length(libpath))
        if(.Platform$OS.type == "windows") {
            ## We don't have a way to set an environment variable for
            ## a single command, as we do not spawn a shell.
            oldrlibs <- Sys.getenv("R_LIBS")
            Sys.setenv(R_LIBS = libpath)
            on.exit(Sys.setenv(R_LIBS = oldrlibs))
        } else
            cmd0 <- paste(paste("R_LIBS", shQuote(libpath), sep="="), cmd0)

    if(is.null(repos) & missing(contriburl)) {
        ## install from local source tarballs
        update <- cbind(path.expand(pkgs), lib) # for side-effect of recycling to same length
        if (is.character(clean))
            cmd0 <- paste(cmd0, clean)

        for(i in seq_len(nrow(update))) {
            cmd <- paste(cmd0, "-l", shQuote(update[i, 2L]),
                         getConfigureArgs(update[i, 1L]),
                         getConfigureVars(update[i, 1L]),
                         shQuote(update[i, 1L]))
            if(system(cmd) > 0L)
                warning(gettextf(
                 "installation of package '%s' had non-zero exit status",
                                update[i, 1L]), domain = NA)
        }
        return(invisible())
    }

    tmpd <- destdir
    nonlocalcran <- length(grep("^file:", contriburl)) < length(contriburl)
    if(is.null(destdir) && nonlocalcran) {
        tmpd <- file.path(tempdir(), "downloaded_packages")
        if (!file.exists(tmpd) && !dir.create(tmpd))
            stop(gettextf("unable to create temporary directory '%s'", tmpd),
                 domain = NA)
    }

    if(is.null(available))
        available <- available.packages(contriburl = contriburl,
                                        method = method)
    pkgs <- getDependencies(pkgs, dependencies, available, lib)

    foundpkgs <- download.packages(pkgs, destdir = tmpd, available = available,
                                   contriburl = contriburl, method = method,
                                   type = "source", ...)

    ## at this point 'pkgs' may contain duplicates,
    ## the same pkg in different libs
    if(length(foundpkgs)) {
        update <- unique(cbind(pkgs, lib))
        colnames(update) <- c("Package", "LibPath")
        found <- pkgs %in% foundpkgs[, 1L]
        files <- foundpkgs[match(pkgs[found], foundpkgs[, 1L]), 2L]
        update <- cbind(update[found, , drop=FALSE], file = files)
        if(nrow(update) > 1L) {
            upkgs <- unique(pkgs <- update[, 1L])
            DL <- .make_dependency_list(upkgs, available)
            p0 <- .find_install_order(upkgs, DL)
            ## can't use update[p0, ] due to possible multiple matches
            update <- update[sort.list(match(pkgs, p0)), ]
        }
        if (is.character(clean))
            cmd0 <- paste(cmd0, clean)

        if (is.null(Ncpus)) Ncpus <- 1L
        if (Ncpus > 1L && nrow(update) > 1L) {
            cmd0 <- paste(cmd0, "--pkglock")
            tmpd <- file.path(tempdir(), "make_packages")
            if (!file.exists(tmpd) && !dir.create(tmpd))
                stop(gettextf("unable to create temporary directory '%s'", tmpd),
                     domain = NA)
            mfile <- file.path(tmpd, "Makefile")
            conn <- file(mfile, "wt")
            cat("all: ", paste(paste(update[, 1L], ".ts", sep=""),
                               collapse=" "),
                "\n", sep = "", file = conn)
            for(i in seq_len(nrow(update))) {
                pkg <- update[i, 1L]
                cmd <- paste(cmd0, "-l", shQuote(update[i, 2L]),
                             getConfigureArgs(update[i, 3L]),
                             getConfigureVars(update[i, 3L]),
                             update[i, 3L],
                             ">", paste(pkg, ".out", sep=""), "2>&1")
                deps <- DL[[pkg]]
                deps <- deps[deps %in% pkgs]
                deps <- if(length(deps))
                    paste(paste(deps, ".ts", sep=""), collapse=" ") else ""
                cat(paste(pkg, ".ts: ", deps, sep=""),
                    paste("\t@echo installing package", pkg),
                    paste("\t@", cmd, " && touch ", pkg, ".ts", sep=""),
                    paste("\t@cat ", pkg, ".out", sep=""),
                    "", sep="\n", file = conn)
            }
            close(conn)
            ## system(paste("cat ", mfile))
            cwd <- setwd(tmpd)
            on.exit(setwd(cwd))
            ## MAKE will be set by sourcing Renviron, but in case not
            make <- Sys.getenv("MAKE")
            if(!nzchar(make)) make <- "make"
            status <- system(paste(make, "-k -j", Ncpus))
            if(status > 0L) {
                ## Try to figure out which
                pkgs <- update[, 1L]
                tss <- sub("\\.ts$", "", dir(".", pattern = "\\.ts$"))
                failed <- pkgs[!pkgs %in% tss]
		for (pkg in failed) system(paste("cat ", pkg, ".out", sep=""))
                warning(gettextf("installation of one of more packages failed,\n  probably %s",
                                 paste(sQuote(failed), collapse = ", ")),
                        domain = NA)
            }
            setwd(cwd); on.exit()
            unlink(tmpd, recursive = TRUE)
        } else {
            for(i in seq_len(nrow(update))) {
                cmd <- paste(cmd0, "-l", shQuote(update[i, 2L]),
                             getConfigureArgs(update[i, 3L]),
                             getConfigureVars(update[i, 3L]),
                             update[i, 3L])
                status <- system(cmd)
                if(status > 0L)
                    warning(gettextf("installation of package '%s' had non-zero exit status",
                                     update[i, 1L]), domain = NA)
            }
        }
        if(!is.null(tmpd) && is.null(destdir))
            cat("\n", gettextf("The downloaded packages are in\n\t%s",
                               sQuote(normalizePath(tmpd))), "\n", sep = "")
        ## update packages.html on Unix only if .Library was installed into
        libs_used <- unique(update[, 2L])
        if(.Platform$OS.type == "unix" && .Library %in% libs_used)
            link.html.help(verbose = TRUE)
    } else if(!is.null(tmpd) && is.null(destdir)) unlink(tmpd, TRUE)

    invisible()
}
#  File src/library/utils/R/page.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

page <- function(x, method = c("dput", "print"), ...)
{
    ## local functions to parcel out '...'
    local.file.show <- function(file, title = subx, delete.file = TRUE,
                                pager = getOption("pager"), ...)
        file.show(file, title = title, delete.file = delete.file, pager = pager)
    local.dput <- function(x, file, title, delete.file, pager, ...)
        dput(x, file, ...)
    local.print <- function(x, title, delete.file, pager, ...)
        print(x, ...)

    if(is.character(x) && length(x) == 1L) {
        subx <- x
        parent <- parent.frame()
        if(exists(subx, envir = parent)) # inherits=TRUE is default
            x <- get(subx, envir = parent)
        else
            stop(gettextf("no object named '%s' to show", x), domain = NA)
    } else {
        subx <- deparse(substitute(x))
    }
    file <- tempfile("Rpage.")
    if(match.arg(method) == "dput")
        local.dput(x, file, ...)
    else {
        sink(file)
        local.print(x, ...)
        sink()
    }
    local.file.show(file, ...)
}
txtProgressBar <-
    function(min = 0, max = 1, initial = 0, char = "=",
             width = NA, title, label, style = 1)
{
    if(! style %in% 1L:3L) style <- 1
    .val <- initial
    .killed <- FALSE
    .nb <- 0L
    .pc <- 0L
    nw <- nchar(char, "w")
    if(is.na(width)) {
        width <- getOption("width")
        if(style == 3L) width <- width - 10L
        width <- trunc(width/nw)
    }

    up1 <- function(value) {
        if(!is.finite(value) || value < min || value > max) return()
        .val <<- value
        nb <- round(width*(value - min)/(max - min))
        if(.nb < nb) {
            cat(paste(rep.int(char, nb-.nb), collapse=""))
            flush.console()
        } else if (.nb > nb) {
            cat(paste(c("\r", rep.int(" ", .nb*nw)), collapse=""))
            cat(paste(c("\r", rep.int(char, nb)), collapse=""))
            flush.console()
        }
        .nb <<- nb
    }

    up2 <- function(value) {
        if(!is.finite(value) || value < min || value > max) return()
        .val <<- value
        nb <- round(width*(value - min)/(max - min))
        if(.nb <= nb) {
            cat(paste("\r", rep.int(char, nb), collapse=""))
            flush.console()
        } else {
            cat(paste(c("\r", rep.int(" ", .nb*nw)), collapse=""))
            cat(paste(c("\r", rep.int(char, nb)), collapse=""))
            flush.console()
        }
        .nb <<- nb
    }

    up3 <- function(value) {
        if(!is.finite(value) || value < min || value > max) return()
        .val <<- value
        nb <- round(width*(value - min)/(max - min))
        pc <- round(100*(value - min)/(max - min))
        if(nb == .nb && pc == .pc) return()
        cat(paste(c("\r  |", rep.int(" ", nw*width+6)), collapse=""))
        cat(paste(c("\r  |",
                    rep.int(char, nb),
                    rep.int(" ", nw*(width-nb)),
                    sprintf("| %3d%%", pc)
                    ), collapse=""))
        flush.console()
        .nb <<- nb
        .pc <<- pc
    }

    getVal <- function() .val
    kill <- function()
        if(!.killed) {
            cat("\n")
            flush.console()
            .killed <<- TRUE
        }
    up <- switch(style, up1, up2, up3)
    if(initial > min) up(initial)
    structure(list(getVal=getVal, up=up, kill=kill),
              class = "txtProgressBar")
}

getTxtProgressBar <- function(pb)
{
    if(!inherits(pb, "txtProgressBar"))
       stop("'pb' is not from class \"txtProgressBar\"")
    pb$getVal()
}

setTxtProgressBar <- function(pb, value, title = NULL, label = NULL)
{
    if(!inherits(pb, "txtProgressBar"))
       stop("'pb' is not from class \"txtProgressBar\"")
    oldval <- pb$getVal()
    pb$up(value)
    invisible(oldval)
}

close.txtProgressBar <- function(con, ...)
{
    con$kill()
    invisible(NULL)
}
#  File src/library/utils/R/prompt.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

prompt <-
function(object, filename = NULL, name = NULL, ...)
    UseMethod("prompt")

prompt.default <-
function(object, filename = NULL, name = NULL,
         force.function = FALSE, ...)
{
    paste0 <- function(...) paste(..., sep = "")

    is.missing.arg <- function(arg)
        typeof(arg) == "symbol" && deparse(arg) == ""

    if(missing(name))
        name <- if(is.character(object))
            object
        else {
            name <- substitute(object)
            ## <FIXME>
            ## This used to be:
            ##     if(is.language(name) && !is.name(name))
            ##         name <- eval(name)
            ##     as.character(name)
            ## but what is this trying to do?
            ## It seems that the eval() will typically give the given
            ## object, and surely we cannot use that as the name (even
            ## if the subsequent as.character() does not fail ...)
            ## Better to be defensive about this, and handle only cases
            ## we know will make sense ...
            if(is.name(name))
                as.character(name)
            else if(is.call(name)
                    && (as.character(name[[1L]]) %in%
                        c("::", ":::", "getAnywhere"))) {
                name <- as.character(name)
                name[length(name)]
            }
            else
                stop("cannot determine a usable name")
            ## </FIXME>
        }

    if(is.null(filename))
        filename <- paste0(name, ".Rd")

    x <- if(!missing(object))
        object
    else {
        ## Better than get(); works when called in fun :
        x <- get(name, envir = parent.frame())
    }

    ## <FIXME>
    ## If not a function or forced to document a function (?), always
    ## assume data set.
    if(!(is.function(x) || force.function))
        return(promptData(x, filename = filename, name = name))
    ## </FIXME>

    n <- length(argls <- formals(x))
    if(n) {
        arg.names <- arg.n <- names(argls)
        arg.n[arg.n == "..."] <- "\\dots"
    }
    ## Construct the 'call' for \usage.
    Call <- paste0(name, "(")
    for(i in seq_len(n)) {                       # i-th argument
        Call <- paste0(Call, arg.names[i],
                       if(!is.missing.arg(argls[[i]]))
                       paste0(" = ",
                              paste(deparse(argls[[i]], width.cutoff= 500L),
                                    collapse="\n")))
        if(i != n) Call <- paste0(Call, ", ")
    }

    ## Construct the definition for \examples.
    x.def <- attr(x, "source")
    if(is.null(x.def))
        x.def <- deparse(x)
    if(any(br <- substr(x.def, 1L, 1L) == "}"))
        x.def[br] <- paste(" ", x.def[br])

    ## escape "%" :
    x.def <- gsub("%", "\\\\%", x.def)

    Rdtxt <-
        list(name = paste0("\\name{", name, "}"),
#             version = "\\Rdversion{1.1}",
             aliases = c(paste0("\\alias{", name, "}"),
             paste("%- Also NEED an '\\alias' for EACH other topic",
                   "documented here.")),
             title = "\\title{\n%%  ~~function to do ... ~~\n}",
             description = c("\\description{",
             paste("%%  ~~ A concise (1-5 lines) description of what",
                   "the function does. ~~"),
             "}"),
             usage = c("\\usage{", paste0(Call, ")"), "}",
             paste("%- maybe also 'usage' for other objects",
                   "documented here.")),
             arguments = NULL,
             details = c("\\details{",
             paste("%%  ~~ If necessary, more details than the",
                   "description above ~~"),
             "}"),
             value = c("\\value{",
             "%%  ~Describe the value returned",
             "%%  If it is a LIST, use",
             "%%  \\item{comp1 }{Description of 'comp1'}",
             "%%  \\item{comp2 }{Description of 'comp2'}",
             "%% ...",
             "}"),
             references = paste("\\references{\n%% ~put references to the",
             "literature/web site here ~\n}"),
             author = "\\author{\n%%  ~~who you are~~\n}",
             note = c("\\note{\n%%  ~~further notes~~\n}",
             "",
             paste("%% ~Make other sections like Warning with",
                   "\\section{Warning }{....} ~"),
             ""),
             seealso = paste("\\seealso{\n%% ~~objects to See Also as",
             "\\code{\\link{help}}, ~~~\n}"),
             examples = c("\\examples{",
             "##---- Should be DIRECTLY executable !! ----",
             "##-- ==>  Define data, use random,",
             "##--	or do  help(data=index)  for the standard data sets.",
             "",
             "## The function is currently defined as",
             x.def,
             "}"),
             keywords = c(paste("% Add one or more standard keywords,",
             "see file 'KEYWORDS' in the"),
             "% R documentation directory.",
             "\\keyword{ ~kwd1 }",
             "\\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line"))

    Rdtxt$arguments <- if(n)
        c("\\arguments{",
          paste0("  \\item{", arg.n, "}{",
                 "\n%%     ~~Describe \\code{", arg.n, "} here~~\n}"),
          "}") ## else NULL

    if(is.na(filename)) return(Rdtxt)

    cat(unlist(Rdtxt), file = filename, sep = "\n")

    message(gettextf("Created file named '%s'.", filename),
            "\n",
            gettext("Edit the file and move it to the appropriate directory."),
            domain = NA)

    invisible(filename)
}

prompt.data.frame <-
function(object, filename = NULL, name = NULL, ...)
{
    paste0 <- function(...) paste(..., sep = "")

    if(missing(name))
        name <-
            if(is.character(object))
                object
            else {
                name <- substitute(object)
                if(is.name(name))
                    as.character(name)
                else
                    stop("cannot determine a usable name")
            }
    if(is.null(filename))
        filename <- paste0(name, ".Rd")

    x <- if(!missing(object))
        object
    else {
        ## Better than get(); works when called in fun :
        x <- get(name, envir = parent.frame())
    }

    ## <FIXME>
    ## Always assume data set ???
    promptData(x, filename = filename, name = name)
    ## </FIXME>
}

promptData <-
function(object, filename = NULL, name = NULL)
{
    paste0 <- function(...) paste(..., sep = "")

    if(missing(name))
        name <-
            if(is.character(object))
                object
            else {
                name <- substitute(object)
                if(is.name(name))
                    as.character(name)
                else
                    stop("cannot determine a usable name")
            }
    if(is.null(filename))
        filename <- paste0(name, ".Rd")

    x <- if(!missing(object))
        object
    else {
        ## Better than get(); works when called in fun :
        x <- get(name, envir = parent.frame())
    }

    ## Construct the format.
    if(is.data.frame(x)) {
        fmt <- c("\\format{",
                 paste("  A data frame with",
                       nrow(x),
                       "observations on the following",
                       ifelse(ncol(x) == 1,
                              "variable.",
                              paste(ncol(x), "variables."))),
                 "  \\describe{")
        for(i in names(x)) {
            xi <- x[[i]]
            fmt <-
                c(fmt,
                  paste0("    \\item{\\code{", i, "}}{",
                         if(inherits(xi, "ordered")) {
                             paste("an", data.class(xi),
                                   "factor with levels",
                                   paste0("\\code{", levels(xi), "}",
                                          collapse = " < "),
                                   collapse = " ")
                         } else if(inherits(xi, "factor")) {
                             paste("a factor with levels",
                                   paste0("\\code{", levels(xi), "}",
                                          collapse = " "),
                                   collapse = " ")
                         } else if(is.vector(xi)) {
                             paste("a", data.class(xi), "vector")
                         } else if(is.matrix(xi)) {
                             paste("a matrix with", ncol(xi), "columns")
                         } else {
                             paste("a", data.class(xi))
                         },
                         "}"))
        }
        fmt <- c(fmt, "  }", "}")
    }
    else {
        tf <- tempfile(); on.exit(unlink(tf))
        sink(tf) ; str(object) ; sink()
        fmt <- c("\\format{",
                 "  The format is:",
                 scan(tf, "", quiet = !getOption("verbose"), sep = "\n"),
                 "}")
    }

    Rdtxt <-
        list(name = paste0("\\name{", name, "}"),
#             version = "\\Rdversion{1.1}",
             aliases = paste0("\\alias{", name, "}"),
             docType = "\\docType{data}",
             title = "\\title{\n%%   ~~ data name/kind ... ~~\n}",
             description = c("\\description{",
             "%%  ~~ A concise (1-5 lines) description of the dataset. ~~",
             "}"),
             usage = paste0("\\usage{data(", name, ")}"),
             format = fmt,
             details = c("\\details{",
             paste("%%  ~~ If necessary, more details than the",
                   "__description__ above ~~"),
             "}"),
             source = c("\\source{",
             paste("%%  ~~ reference to a publication or URL",
                   "from which the data were obtained ~~"),
             "}"),
             references = c("\\references{",
             "%%  ~~ possibly secondary sources and usages ~~",
             "}"),
             examples = c("\\examples{",
             paste0("data(", name, ")"),
             paste0("## maybe str(", name, ") ; plot(", name, ") ..."),
             "}"),
             keywords = "\\keyword{datasets}")

    if(is.na(filename)) return(Rdtxt)

    cat(unlist(Rdtxt), file = filename, sep = "\n")

    message(gettextf("Created file named '%s'.", filename),
            "\n",
            gettext("Edit the file and move it to the appropriate directory."),
            domain = NA)

    invisible(filename)
}

promptPackage <-
function(package, lib.loc = NULL, filename = NULL, name = NULL, final = FALSE)
{
    ## Most of this should not be translated -- PR#11191
    ## need to do this as packageDescription and library(help=) have
    ## different conventions
    if (is.null(lib.loc)) lib.loc <- .libPaths()

    paste0 <- function(...) paste(..., sep = "")
    insert1 <- function(field, new) {
    	prev <- Rdtxt[[field]]
    	Rdtxt[[field]] <<- c(prev[-length(prev)], new, prev[length(prev)])
    }
    insert2 <- function(field, new) insert1(field, paste("~~", new, "~~"))
    tabular <- function(col1, col2)
        c("\\tabular{ll}{", paste0(col1, " \\tab ", col2, "\\cr"), "}")

    if(missing(name))
	name <- paste0(package, "-package")

    if(is.null(filename))
        filename <- paste0(name, ".Rd")

    Rdtxt <-
    	    list(name = paste0("\\name{", name, "}"),
#                 version = "\\Rdversion{1.1}",
    	         aliases = paste0("\\alias{", name, "}"),
    	         docType = "\\docType{package}",
    	         title = c("\\title{", "}"),
    	         description = c("\\description{","}"),
    	         details = c("\\details{","}"),
    	         author = c("\\author{","}"),
    	         references = character(0L),

    	         keywords = c("\\keyword{ package }")
    	     )

    desc <- packageDescription(package, lib.loc)

    if (length(desc) > 1) {
    	info <- library(help = package, lib.loc = lib.loc,
                        character.only = TRUE)

    	if (!length(grep(paste0("^", package, " "), info$info[[2L]])))
    	    Rdtxt$aliases <- c(Rdtxt$aliases, paste0("\\alias{", package, "}"))

        insert1("title", desc$Title)
	insert1("description", desc$Description)
	insert1("author", c(desc$Author, "",
                            paste(identity("Maintainer:"),desc$Maintainer)))

	desc <- desc[!(names(desc) %in%
                       c("Title", "Description", "Author", "Maintainer"))]

	insert1("details", tabular(paste0(names(desc), ":"), unlist(desc)))

	if (!is.null(info$info[[2L]]))
	    insert1("details",  c("", identity("Index:"), "\\preformatted{",
	                          info$info[[2L]], "}"))
	if (!is.null(info$info[[3L]]))
	    insert1("details",
                    c("",
        identity("Further information is available in the following vignettes:"),
                      tabular(paste0("\\code{", info$info[[3L]][,1], "}"),
                              info$info[[3L]][,2])))
    }

    if (!final) {
        insert2("title", identity("package title"))
        insert2("description",
                identity("A concise (1-5 lines) description of the package"))
        insert2("details",
                strwrap(identity("An overview of how to use the package, including the most important functions")))
        insert2("author",
                identity("The author and/or maintainer of the package"))
        Rdtxt$references <-
            c("\\references{",
              paste("~~",
                    identity("Literature or other references for background information"),
                    "~~"),
              "}")
        Rdtxt$seealso <- c("\\seealso{", "}")
        insert2("seealso",
                c(identity("Optional links to other man pages, e.g."),
                  "\\code{\\link[<pkg>:<pkg>-package]{<pkg>}}"))
        Rdtxt$examples <- c("\\examples{","}")
        insert2("examples",
                identity("simple examples of the most important functions"))
        insert2("keywords",
                strwrap(identity("Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory")))
    }

    if(is.na(filename)) return(Rdtxt)

    cat(unlist(Rdtxt), file = filename, sep = "\n")

    message(gettextf("Created file named '%s'.", filename),
            "\n",
            gettext("Edit the file and move it to the appropriate directory."),
            domain = NA)

    invisible(filename)
}
#  File src/library/utils/R/question.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

"?" <- function(e1, e2)
{
    if (missing(e2)) {
        type <- NULL
    	topicExpr <- substitute(e1)
    } else {
     	type <- substitute(e1)
     	topicExpr <- substitute(e2)
    }
    if (is.call(topicExpr) && topicExpr[[1L]] == "?") {
            # ??foo is parsed as `?`(`?`(foo))
            	search <- TRUE
            	topicExpr <- topicExpr[[2L]]
            	if (is.call(topicExpr) && topicExpr[[1L]] == "?"
            	    && is.call(topicExpr[[2L]]) && topicExpr[[2L]][[1L]] == "?") {
            	     cat("Contacting Delphi...")
            	     flush.console()
            	     Sys.sleep(2+rpois(1,2))
            	     cat("the oracle is unavailable.\nWe apologize for any inconvenience.\n")
            	     return(invisible())
            	 }
            } else
            	search <- FALSE

    if (is.call(topicExpr) && (topicExpr[[1L]] == "::" || topicExpr[[1L]] == ":::")) {
		package <- as.character(topicExpr[[2L]])
		topicExpr <- topicExpr[[3L]]
	    } else
		package <- NULL

    if (search) {
	if (is.null(type))
	    return(eval(substitute(help.search(TOPIC, package = PACKAGE),
							list(TOPIC = as.character(topicExpr),
							      PACKAGE = package))))
	else
	    return(eval(substitute(help.search(TOPIC, fields = FIELD, package = PACKAGE),
	    						list(TOPIC = as.character(topicExpr),
	    						      FIELD = as.character(type),
	    						      PACKAGE = package))))
    } else {
        if (is.null(type)) {
	    if (is.call(topicExpr))
		return(.helpForCall(topicExpr, parent.frame()))
	    if (is.name(topicExpr))
	    	topic <- as.character(topicExpr)
	    else
	    	topic <- e1
	    return(eval(substitute(help(TOPIC, package = PACKAGE),
							list(TOPIC = topic,
							      PACKAGE = package))))
	} else {
	    ## interpret e1 as a type, but to allow customization, do NOT
            ## force arbitrary expressions to be single character strings
            ## (so that methods can be defined for topicName).
            if (is.name(type))
		type <- as.character(type)
	    else
		type <- e1
	    if (is.name(topicExpr))
		topic <- as.character(topicExpr)
            else {
            	if (is.call(topicExpr) && identical(type, "method"))
            	    return(.helpForCall(topicExpr, parent.frame(), FALSE))
            	topic <- e2
	    }
	    doHelp <- .tryHelp(topicName(type, topic), package = package)
	    if(inherits(doHelp, "try-error")) {
                if(is.language(topicExpr))
                  topicExpr <- deparse(topicExpr)
		stop(gettextf("no documentation of type '%s' and topic '%s' (or error in processing help)", type, topicExpr), domain = NA)
            }
        }
    }
}

topicName <- function(type, topic)
{
    if((length(type) == 0L) || (length(topic) == 0L))
        character(0L)
    else
        paste(paste(topic, collapse = ","), type, sep = "-")
}

.helpForCall <- function(expr, envir, doEval = TRUE) {
    f <- expr[[1L]]                      # the function specifier
    where <- topenv(envir)              # typically .GlobalEnv
    if(is.name(f))
        f <- as.character(f)
    if(!.isMethodsDispatchOn() || !methods::isGeneric(f, where = where)) {
        if(!is.character(f) || length(f) != 1L)
            stop(gettextf("the object of class \"%s\" in the function call '%s' could not be used as a documentation topic",
                          class(f), deparse(expr)), domain = NA)
        h <- .tryHelp(f)
        if(inherits(h, "try-error"))
            stop(gettextf("no methods for '%s' and no documentation for it as a function", f), domain = NA)
    }
    else {
        ## allow generic function objects or names
        if(methods::is(f, "genericFunction")) {
            fdef <- f
            f <- fdef@generic
        }
        else
            fdef <- methods::getGeneric(f, where = where)
        call <- match.call(fdef, expr)
        ## make the signature
        sigNames <- fdef@signature
        sigClasses <- rep.int("ANY", length(sigNames))
        names(sigClasses) <- sigNames
        for(arg in sigNames) {
            argExpr <- methods::elNamed(call, arg)
            if(!is.null(argExpr)) {
                simple <- (is.character(argExpr) || is.name(argExpr))
                ## TODO:  ideally, if doEval is TRUE, we would like to
                ## create the same context used by applyClosure in
                ## eval.c, but then skip the actual evaluation of the
                ## body.  If we could create this environment then
                ## passing it to selectMethod is closer to the semantics
                ## of the "real" function call than the code below.
                ## But, seems to need a change to eval.c and a flag to
                ## the evaluator.
                if(doEval || !simple) {
                    argVal <- try(eval(argExpr, envir))
                    if(methods::is(argVal, "try-error"))
                        stop(gettextf("error in trying to evaluate the expression for argument '%s' (%s)",
                                      arg, deparse(argExpr)),
                             domain = NA)
                    sigClasses[[arg]] <- class(argVal)
                }
                else
                    sigClasses[[arg]] <- as.character(argExpr)
            }
        }
        method <- methods::selectMethod(f, sigClasses, optional=TRUE,
                                        fdef = fdef)
        if(methods::is(method, "MethodDefinition")) {
            sigClasses <- method@defined
            if(length(sigClasses) < length(sigNames))
              sigClasses<- c(sigClasses, rep("ANY", length(sigNames)-length(sigClasses)))
        }
        else
            warning(gettextf("no method defined for function '%s' and signature '%s'",
                             f, paste(sigNames, " = ", dQuote(sigClasses),
                                      sep = "", collapse = ", ")), domain = NA)
        topic <- topicName("method", c(f,sigClasses))
        h <- .tryHelp(topic)
        if(methods::is(h, "try-error"))
            stop(gettextf("no documentation for function '%s' and signature '%s'",
                 f, paste(sigNames, " = ", dQuote(sigClasses), sep = "",
                          collapse = ", ")), domain = NA)
    }
}

.tryHelp <- function(topic, package = NULL) {
    opts <- options(error = function() TRUE,
                    show.error.messages = FALSE)
    on.exit(options(opts))
    x <- try(do.call("help", list(topic, package = package)))
    ## If something was found, actually show it via print().
    ## Otherwise, give an error.
    if(!inherits(x, "try-error") && length(x))
        print(x)
    else
        try(stop())
    ## Argh ...
}
#  File src/library/utils/R/read.DIF.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

read.DIF <- function(file, header = FALSE, dec = ".",
         row.names, col.names, as.is = !stringsAsFactors,
         na.strings = "NA", colClasses = NA,
         nrows = -1, skip = 0,
         check.names = TRUE,
         blank.lines.skip = TRUE,
         stringsAsFactors = default.stringsAsFactors(),
	 transpose = FALSE)
{
    if (.Platform$OS.type == "windows" && identical(file, "clipboard")) {
	if (!(5 %in% getClipboardFormats()) ) stop("No DIF data on clipboard")
	lines <- readClipboard(5)
    } else {
	lines <- readLines(file)
    }
    if(length(lines) < 1L) stop("file had no lines")
    topic <- ""
    nrow <- NA
    ncol <- NA
    i <- 1L
    ## Read header info :
    while (topic != "DATA") {
	topic <- lines[i]
	vnum <- lines[i+1]
	num <- as.numeric(sub("^.*,","",vnum))
	## v <- as.numeric(sub(",.*$","",vnum))
	## value <- lines[i+2]
	i <- i + 3L
	if (topic == "VECTORS")
	    if(transpose) nrow <- num else ncol <- num
	else if (topic == "TUPLES")
	    if(transpose) ncol <- num else nrow <- num
    }
    if (is.na(nrow) || is.na(ncol)) stop("row and column counts not found")

    data <- matrix("", nrow, ncol)
    types <- matrix(NA_character_, nrow, ncol)

    row <- 0L
    while (i < length(lines)) {
	typenum <- lines[i]
	type <- as.numeric(sub(",.*$","",typenum))
	num <- as.numeric(sub("^.*,","",typenum))
	stringval <- lines[i+1]
	i <- i + 2L
	if (type == -1L) {
	    if (stringval == "BOT") {
		row <- row + 1L
                if(row > nrow)
                    stop("More rows than specified in header; maybe use 'transpose=TRUE'")
		col <- 0L
	    } else if (stringval == "EOD") break
	    else stop("Unrecognized special data value")
	} else {
	    col <- col + 1L
            if(col > ncol)
                stop("More columns than specified in header; maybe use 'transpose=TRUE'")
            if (type == 0L) {
                types[row, col] <- "numeric"
                if (stringval == "V") data[row, col] <- num
                else if (stringval == "NA") data[row, col] <- NA
                else if (stringval == "ERROR") data[row, col] <- NA
                else if (stringval == "TRUE") {
                    data[row, col] <- "TRUE"
                    types[row, col] <- "logical"
                }
                else if (stringval == "FALSE") {
                    data[row, col] <- "FALSE"
                    types[row, col] <- "logical"
                }
                else stop("Unrecognized value indicator")
            } else if (type == 1L) {
                types[row, col] <- "character"
                stringval <- sub("^\"", "", stringval)
                stringval <- sub("\"$", "", stringval)
                data[row, col] <- stringval
            }
        }
    }

    if(skip > 0L) data <- data[-(1L:skip),]

    ## determine header, no of cols.
    nlines <- nrow(data)

    if (!nlines) {
        if (missing(col.names))
            stop("no lines available in input")
        else {
            tmp <- vector("list", length(col.names))
            names(tmp) <- col.names
            class(tmp) <- "data.frame"
            return(tmp)
        }
    }
    first <- data[1L, ]
    if (first[1L] == "") first <- first[-1L]

    col1 <- if(missing(col.names)) length(first) else length(col.names)
    cols <- ncol

    ##	basic column counting and header determination;
    ##	rlabp (logical) := it looks like we have column names
    rlabp <- all(types[1L, ][-1L] == "character") && data[1L, 1L] == ""
    if(rlabp && missing(header))
	header <- TRUE
    if(!header) rlabp <- FALSE

    if (header) {
    	data <- data[-1L,] # skip over header
    	types <- types[-1L, ]
        if(missing(col.names)) col.names <- first
        else if(length(first) != length(col.names))
            warning("header and 'col.names' are of different lengths")

    } else if (missing(col.names))
	col.names <- paste("V", 1L:cols, sep = "")
    if(length(col.names) + rlabp < cols)
        stop("more columns than column names")
    if(cols > 0L && length(col.names) > cols)
        stop("more column names than columns")
    if(cols == 0L) stop("rows are empty: giving up")


    if(check.names) col.names <- make.names(col.names, unique = TRUE)
    if (rlabp) col.names <- c("row.names", col.names)

    nmColClasses <- names(colClasses)
    if(length(colClasses) < cols)
        if(is.null(nmColClasses)) {
            colClasses <- rep(colClasses, length.out=cols)
        } else {
            tmp <- rep(NA_character_, length.out=cols)
            names(tmp) <- col.names
            i <- match(nmColClasses, col.names, 0L)
            if(any(i <= 0L))
                warning("not all columns named in 'colClasses' exist")
            tmp[ i[i > 0L] ] <- colClasses
            colClasses <- tmp
        }


    ##	set up as if we'll scan the file.

    colClasses[colClasses %in% c("real", "double")] <- "numeric"
    known <- colClasses %in%
                c("logical", "integer", "numeric", "complex", "character")
    keep <- !(colClasses %in% "NULL")

    if (blank.lines.skip) data <- data[apply(data, 1L, function(x) !all(x == "")),]
    if (nrows > -1 && nrows < nrow(data)) data <- data[seq_len(nrows),]
    nlines <- nrow(data)

    data[data %in% na.strings] <- NA
    data <- as.data.frame(data, stringsAsFactors = FALSE)
    names(data) <- col.names

    ##	now we have the data;
    ##	convert to numeric or factor variables
    ##	(depending on the specified value of "as.is").
    ##	we do this here so that columns match up

    if(cols != length(data)) { # this should never happen
	warning("cols = ", cols, " != length(data) = ", length(data),
                domain = NA)
	cols <- length(data)
    }

    if(is.logical(as.is)) {
	as.is <- rep(as.is, length.out=cols)
    } else if(is.numeric(as.is)) {
	if(any(as.is < 1 | as.is > cols))
	    stop("invalid numeric 'as.is' expression")
	i <- rep.int(FALSE, cols)
	i[as.is] <- TRUE
	as.is <- i
    } else if(is.character(as.is)) {
        i <- match(as.is, col.names, 0L)
        if(any(i <= 0L))
            warning("not all columns named in 'as.is' exist")
        i <- i[i > 0L]
        as.is <- rep.int(FALSE, cols)
        as.is[i] <- TRUE
    } else if (length(as.is) != cols)
	stop(gettextf("'as.is' has the wrong length %d  != cols = %d",
                     length(as.is), cols), domain = NA)

    do <- keep & !known # & !as.is
    if(rlabp) do[1L] <- FALSE # don't convert "row.names"
    for (i in (1L:cols)[do]) {
        data[[i]] <-
	    if (is.na(colClasses[i])) {
		if (stringsAsFactors || all(types[,i] != "character"))
		    type.convert(data[[i]], as.is = as.is[i], dec = dec,
				 na.strings = character(0L))
		else data[[i]]
	    }
        ## as na.strings have already been converted to <NA>
            else if (colClasses[i] == "factor") as.factor(data[[i]])
            else if (colClasses[i] == "Date") as.Date(data[[i]])
            else if (colClasses[i] == "POSIXct") as.POSIXct(data[[i]])
            else methods::as(data[[i]], colClasses[i])
    }

    ##	now determine row names
    compactRN <- TRUE
    if (missing(row.names)) {
	if (rlabp) {
	    row.names <- data[[1L]]
	    data <- data[-1L]
            keep <- keep[-1L]
            compactRN <- FALSE
	}
	else row.names <- .set_row_names(as.integer(nlines))
    } else if (is.null(row.names)) {
	row.names <- .set_row_names(as.integer(nlines))
    } else if (is.character(row.names)) {
        compactRN <- FALSE
	if (length(row.names) == 1L) {
	    rowvar <- (1L:cols)[match(col.names, row.names, 0L) == 1L]
	    row.names <- data[[rowvar]]
	    data <- data[-rowvar]
            keep <- keep[-rowvar]
	}
    } else if (is.numeric(row.names) && length(row.names) == 1L) {
        compactRN <- FALSE
	rlabp <- row.names
	row.names <- data[[rlabp]]
	data <- data[-rlabp]
        keep <- keep[-rlabp]
    } else stop("invalid 'row.names' specification")
    data <- data[keep]

    ## rownames<- is interpreted, so avoid it for efficiency (it will copy)
    if(is.object(row.names) || !(is.integer(row.names)) )
        row.names <- as.character(row.names)
    if(!compactRN) {
        if (length(row.names) != nlines)
            stop("invalid 'row.names' length")
        if (anyDuplicated(row.names))
            stop("duplicate 'row.names' are not allowed")
        if (any(is.na(row.names)))
            stop("missing values in 'row.names' are not allowed")
    }

    ##	this is extremely underhanded
    ##	we should use the constructor function ...
    ##	don't try this at home kids

    attr(data, "row.names") <- row.names
    data
}
#  File src/library/utils/R/read.fortran.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/


read.fortran<-function(file, format, ...,as.is=TRUE, colClasses=NA){

     processFormat<-function(format){
       format<-toupper(format)
       template<-"^([0-9]*)([FXAI])([0-9]*)\\.?([0-9]*)"
       reps<-as.numeric(sub(template,"\\1",format))
       types<-sub(template, "\\2", format)
       lengths<-as.numeric(sub(template, "\\3", format))
       decimals<-as.numeric(sub(template, "\\4", format))

       reps[is.na(reps)]<-1L
       lengths[is.na(lengths) & types=="X"]<-1L

       charskip<-types=="X"
       lengths[charskip]<-reps[charskip]*lengths[charskip]
       reps[charskip]<-1

       if (any(is.na(lengths)))
         stop("missing lengths for some fields")

       lengths<-rep(lengths,reps)
       types<-rep(types,reps)
       decimals<-rep(decimals,reps)
       types<- match(types, c("F","D","X","A","I"))

       if (any(!is.na(decimals) & types>2L))
         stop("invalid format")
       colClasses <- c("numeric", "numeric", NA,
                       if(as.is) "character" else NA, "integer")[types]
       colClasses <- colClasses[!(types==3L)]
       decimals <-   decimals  [!(types==3L)]
       lengths[types==3]<- -lengths[types==3L]

       list(lengths,colClasses,decimals)
     }

     if(is.list(format)){
       ff<-lapply(format,processFormat)
       widths<-lapply(ff,"[[",1L)
       if (is.na(colClasses))
         colClasses<-do.call("c",lapply(ff,"[[",2L))
       decimals<-do.call("c",lapply(ff,"[[",3L))
     } else {
       ff<-processFormat(format)
       widths<-ff[[1L]]
       if (is.na(colClasses))
         colClasses<-ff[[2L]]
       decimals<-ff[[3L]]
     }
     rval<-read.fwf(file,widths=widths, ..., colClasses=colClasses)
     for(i in which(!is.na(decimals)))
       rval[,i]<-rval[,i]*(10^-decimals[i])
     rval
}
#  File src/library/utils/R/read.fwf.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

read.fwf <-
function(file, widths, header = FALSE, sep = "\t",
         skip = 0, row.names, col.names, n = -1, buffersize = 2000, ...)
{
    doone <- function(x) {
        x <- substring(x, first, last)
        x[!nzchar(x)] <- NA_character_
        x
    }

    if (is.list(widths)) {
        recordlength <- length(widths)
        widths <- do.call("c", widths)
    } else recordlength <- 1L

    drop <- (widths < 0L)
    widths <- abs(widths)


    buffersize <- (buffersize %/% recordlength) * recordlength

    FILENAME <- tempfile("Rfwf.")
    on.exit(unlink(FILENAME))
    FILE <- file(FILENAME,"a")
    on.exit(close(FILE),add=TRUE)

    if (is.character(file)) {
        file <- file(file, "r")
        on.exit(close(file), add=TRUE)
    } else if (!isOpen(file)) {
        file <- open(file, "r")
        on.exit(close(file), add=TRUE)
    }

    if (skip) readLines(file, n=skip)
    if (header) {
        headerline <- readLines(file, n=1L)
        cat(file=FILE, headerline, "\n")
    }

    repeat({
        if (n == 0L) break
        if (n == -1L)
            thisblock <- buffersize
        else
            thisblock <- min(buffersize,n)

        raw <- readLines(file, n = thisblock)
        nread <- length(raw)
        if (recordlength > 1L &&  nread %% recordlength) {
            raw<-raw[1L:(nread-nread %% recordlength)]
            warning(gettextf("last record incomplete, %d lines discarded",
                             nread %% recordlength), domain = NA)
        }
        if (recordlength > 1L) {
            raw <- matrix(raw, nrow=recordlength)
            raw <- apply(raw, 2L, paste, collapse="")
        }

        st <- c(1L, 1L+cumsum(widths))
        first <- st[-length(st)][!drop]
        last <- cumsum(widths)[!drop]
        cat(file = FILE, sapply(raw, doone),
            sep = c(rep(sep, length.out = length(first)-1L), "\n"))

        if (nread < thisblock) break
        if (n > 0L) n <- n - length(raw)
    })

    close(FILE)
    FILE <- file(FILENAME,"r")
    read.table(file = FILE, header = header, sep = sep,
	       row.names = row.names, col.names = col.names, quote = "", ...)
}
#  File src/library/utils/R/readhttp.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

url.show <-
    function (url,  title = url, file = tempfile(),
              delete.file = TRUE, method, ...)
{
    if (download.file(url, destfile = file, method = method))
        stop("transfer failure")
    file.show(file, delete.file = delete.file, title = title, ...)
}



defaultUserAgent <- function()
{
    Rver <- paste(R.version$major, R.version$minor, sep=".")
    Rdetails <- paste(Rver, R.version$platform, R.version$arch,
                      R.version$os)
    paste("R (", Rdetails, ")", sep="")
}


makeUserAgent <- function(format = TRUE) {
    agent <- getOption("HTTPUserAgent")
    if (is.null(agent)) {
        return(NULL)
    }
    if (length(agent) != 1L)
      stop(sQuote("HTTPUserAgent"),
           " option must be a length one character vector or NULL")
    if (format)
      paste("User-Agent: ", agent[1L], "\r\n", sep = "")
    else
      agent[1L]
}
#  File src/library/utils/R/readtable.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

count.fields <-
function(file, sep = "", quote = "\"'", skip = 0,
         blank.lines.skip = TRUE, comment.char = "#")
{
    if(is.character(file)) {
        file <- file(file)
        on.exit(close(file))
    }
    if(!inherits(file, "connection"))
        stop("'file' must be a character string or connection")
    .Internal(count.fields(file, sep, quote, skip, blank.lines.skip,
                           comment.char))
}


type.convert <-
function(x, na.strings = "NA", as.is = FALSE, dec = ".")
    .Internal(type.convert(x, na.strings, as.is, dec))


read.table <-
function(file, header = FALSE, sep = "", quote = "\"'", dec = ".",
         row.names, col.names, as.is = !stringsAsFactors,
         na.strings = "NA", colClasses = NA,
         nrows = -1, skip = 0,
         check.names = TRUE, fill = !blank.lines.skip,
         strip.white = FALSE, blank.lines.skip = TRUE,
         comment.char = "#", allowEscapes = FALSE, flush = FALSE,
         stringsAsFactors = default.stringsAsFactors(),
         fileEncoding = "", encoding = "unknown")
{
    if(is.character(file)) {
        file <- if(nzchar(fileEncoding))
            file(file, "rt", encoding = fileEncoding) else file(file, "rt")
        on.exit(close(file))
    }
    if(!inherits(file, "connection"))
        stop("'file' must be a character string or connection")
    if(!isOpen(file, "rt")) {
        open(file, "rt")
        on.exit(close(file))
    }

    if(skip > 0L) readLines(file, skip)
    ## read a few lines to determine header, no of cols.
    nlines <- n0lines <- if (nrows < 0L) 5 else min(5L, (header + nrows))

    lines <- .Internal(readTableHead(file, nlines, comment.char,
                                     blank.lines.skip, quote, sep))
    nlines <- length(lines)
    if(!nlines) {
        if(missing(col.names)) stop("no lines available in input")
        rlabp <- FALSE
        cols <- length(col.names)
    } else {
        if(all(!nzchar(lines))) stop("empty beginning of file")
        if(nlines < n0lines && file == 0L)  { # stdin() has reached EOF
            pushBack(c(lines, lines, ""), file)
            on.exit(.Internal(clearPushBack(stdin())))
        } else pushBack(c(lines, lines), file)
        first <- scan(file, what = "", sep = sep, quote = quote,
                      nlines = 1, quiet = TRUE, skip = 0,
                      strip.white = TRUE,
                      blank.lines.skip = blank.lines.skip,
                      comment.char = comment.char, allowEscapes = allowEscapes,
                      encoding = encoding)
        col1 <- if(missing(col.names)) length(first) else length(col.names)
        col <- numeric(nlines - 1L)
        if (nlines > 1L)
            for (i in seq_along(col))
                col[i] <- length(scan(file, what = "", sep = sep,
                                      quote = quote,
                                      nlines = 1, quiet = TRUE, skip = 0,
                                      strip.white = strip.white,
                                      blank.lines.skip = blank.lines.skip,
                                      comment.char = comment.char,
                                      allowEscapes = allowEscapes))
        cols <- max(col1, col)

        ##	basic column counting and header determination;
        ##	rlabp (logical) := it looks like we have column names

        rlabp <- (cols - col1) == 1L
        if(rlabp && missing(header))
            header <- TRUE
        if(!header) rlabp <- FALSE

        if (header) {
            readLines(file, 1L)          # skip over header
            if(missing(col.names)) col.names <- first
            else if(length(first) != length(col.names))
                warning("header and 'col.names' are of different lengths")

        } else if (missing(col.names))
            col.names <- paste("V", 1L:cols, sep = "")
        if(length(col.names) + rlabp < cols)
            stop("more columns than column names")
        if(fill && length(col.names) > cols)
            cols <- length(col.names)
        if(!fill && cols > 0L && length(col.names) > cols)
            stop("more column names than columns")
        if(cols == 0L) stop("first five rows are empty: giving up")
    }

    if(check.names) col.names <- make.names(col.names, unique = TRUE)
    if (rlabp) col.names <- c("row.names", col.names)

    nmColClasses <- names(colClasses)
    if(length(colClasses) < cols)
        if(is.null(nmColClasses)) {
            colClasses <- rep(colClasses, length.out=cols)
        } else {
            tmp <- rep(NA_character_, length.out=cols)
            names(tmp) <- col.names
            i <- match(nmColClasses, col.names, 0L)
            if(any(i <= 0L))
                warning("not all columns named in 'colClasses' exist")
            tmp[ i[i > 0L] ] <- colClasses
            colClasses <- tmp
        }


    ##	set up for the scan of the file.
    ##	we read unknown values as character strings and convert later.

    what <- rep.int(list(""), cols)
    names(what) <- col.names

    colClasses[colClasses %in% c("real", "double")] <- "numeric"
    known <- colClasses %in%
                c("logical", "integer", "numeric", "complex", "character")
    what[known] <- sapply(colClasses[known], do.call, list(0))
    what[colClasses %in% "NULL"] <- list(NULL)
    keep <- !sapply(what, is.null)

    data <- scan(file = file, what = what, sep = sep, quote = quote,
                 dec = dec, nmax = nrows, skip = 0,
		 na.strings = na.strings, quiet = TRUE, fill = fill,
                 strip.white = strip.white,
                 blank.lines.skip = blank.lines.skip, multi.line = FALSE,
                 comment.char = comment.char, allowEscapes = allowEscapes,
                 flush = flush, encoding = encoding)

    nlines <- length(data[[ which(keep)[1L] ]])

    ##	now we have the data;
    ##	convert to numeric or factor variables
    ##	(depending on the specified value of "as.is").
    ##	we do this here so that columns match up

    if(cols != length(data)) { # this should never happen
	warning("cols = ", cols, " != length(data) = ", length(data),
                domain = NA)
	cols <- length(data)
    }

    if(is.logical(as.is)) {
	as.is <- rep(as.is, length.out=cols)
    } else if(is.numeric(as.is)) {
	if(any(as.is < 1 | as.is > cols))
	    stop("invalid numeric 'as.is' expression")
	i <- rep.int(FALSE, cols)
	i[as.is] <- TRUE
	as.is <- i
    } else if(is.character(as.is)) {
        i <- match(as.is, col.names, 0L)
        if(any(i <= 0L))
            warning("not all columns named in 'as.is' exist")
        i <- i[i > 0L]
        as.is <- rep.int(FALSE, cols)
        as.is[i] <- TRUE
    } else if (length(as.is) != cols)
	stop(gettextf("'as.is' has the wrong length %d  != cols = %d",
                     length(as.is), cols), domain = NA)

    do <- keep & !known # & !as.is
    if(rlabp) do[1L] <- FALSE # don't convert "row.names"
    for (i in (1L:cols)[do]) {
        data[[i]] <-
            if (is.na(colClasses[i]))
                type.convert(data[[i]], as.is = as.is[i], dec = dec,
                             na.strings = character(0L))
        ## as na.strings have already been converted to <NA>
            else if (colClasses[i] == "factor") as.factor(data[[i]])
            else if (colClasses[i] == "Date") as.Date(data[[i]])
            else if (colClasses[i] == "POSIXct") as.POSIXct(data[[i]])
            else methods::as(data[[i]], colClasses[i])
    }

    ##	now determine row names
    compactRN <- TRUE
    if (missing(row.names)) {
	if (rlabp) {
	    row.names <- data[[1L]]
	    data <- data[-1L]
            keep <- keep[-1L]
            compactRN <- FALSE
	}
	else row.names <- .set_row_names(as.integer(nlines))
    } else if (is.null(row.names)) {
	row.names <- .set_row_names(as.integer(nlines))
    } else if (is.character(row.names)) {
        compactRN <- FALSE
	if (length(row.names) == 1L) {
	    rowvar <- (1L:cols)[match(col.names, row.names, 0L) == 1L]
	    row.names <- data[[rowvar]]
	    data <- data[-rowvar]
            keep <- keep[-rowvar]
	}
    } else if (is.numeric(row.names) && length(row.names) == 1L) {
        compactRN <- FALSE
	rlabp <- row.names
	row.names <- data[[rlabp]]
	data <- data[-rlabp]
        keep <- keep[-rlabp]
    } else stop("invalid 'row.names' specification")
    data <- data[keep]

    ## rownames<- is interpreted, so avoid it for efficiency (it will copy)
    if(is.object(row.names) || !(is.integer(row.names)) )
        row.names <- as.character(row.names)
    if(!compactRN) {
        if (length(row.names) != nlines)
            stop("invalid 'row.names' length")
        if (anyDuplicated(row.names))
            stop("duplicate 'row.names' are not allowed")
        if (any(is.na(row.names)))
            stop("missing values in 'row.names' are not allowed")
    }

    ##	this is extremely underhanded
    ##	we should use the constructor function ...
    ##	don't try this at home kids

    class(data) <- "data.frame"
    attr(data, "row.names") <- row.names
    data
}

read.csv <-
function (file, header = TRUE, sep = ",", quote = "\"", dec = ".",
          fill = TRUE, comment.char = "", ...)
    read.table(file = file, header = header, sep = sep,
               quote = quote, dec = dec, fill = fill,
               comment.char = comment.char,  ...)

read.csv2 <-
function (file, header = TRUE, sep = ";", quote = "\"", dec = ",",
          fill = TRUE, comment.char = "", ...)
    read.table(file = file, header = header, sep = sep,
               quote = quote, dec = dec, fill = fill,
               comment.char = comment.char, ...)

read.delim <-
function (file, header = TRUE, sep = "\t", quote = "\"", dec = ".",
          fill = TRUE, comment.char = "", ...)
    read.table(file = file, header = header, sep = sep,
               quote = quote, dec = dec, fill = fill,
               comment.char = comment.char, ...)

read.delim2 <-
function (file, header = TRUE, sep = "\t", quote = "\"", dec = ",",
          fill = TRUE, comment.char = "", ...)
    read.table(file = file, header = header, sep = sep,
               quote = quote, dec = dec, fill = fill,
               comment.char = comment.char, ...)

#  File src/library/utils/R/relist.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

# relist.R -- an inverse operator to unlist
# written by Andrew Clausen <clausen@econ.upenn.edu> in 2007
# with helpful suggestions from
#	Martin Maechler  ETHZ CH
#	Gabor Grothendieck at Gmail dotcom
#	Seth Falcon, sfalcon (near) fhcrc (comma) org
#
# Some functions need many parameters, which are most easily represented in
# complex structures.  Unfortunately, many mathematical functions in R,
# including optim, nlm, and grad can only operate on functions whose domain is
# a vector.  R has a function called "unlist" to convert complex objects into a
# vector representation.  This file provides an inverse operation called
# "relist" to convert vectors back to the convenient structural representation.
# Together, these functions allow structured functions to have simple
# mathematical interfaces.
#
# For example, a likelihood function for a multivariate normal model needs a
# variance-covariance matrix and a mean vector.	 It would be most convenient to
# represent it as a list containing a vector and a matrix.  A typical parameter
# might look like
#
#	list(mean=c(0, 1), vcov=cbind(c(1, 1), c(1, 0)))
#
# However, optim can't operate on functions that take lists as input; it
# only likes vectors.  The solution is conversion:
#
## 	initial.param <- list(mean=c(0, 1), vcov=cbind(c(1, 1), c(1, 0)))
## 	initial.param <- as.relistable(initial.param)
## #
## 	ll <- function(param.vector)
## 	{
## 		param <- relist(initial.param)
## 		-sum(dnorm(x, mean=param$mean, vcov=param$vcov, log=TRUE))
## 		# note: dnorm doesn't do vcov... but I hope you get the point
## 	}

## 	optim(unlist(initial.param), ll)
#
# "relist" takes two parameters: skeleton and flesh.  Skeleton is a sample
# object that has the right "shape" but the wrong content.  "flesh" is a vector
# with the right content but the wrong shape.  Invoking
#
#	relist(flesh, skeleton)
#
# will put the content of flesh on the skeleton.  You don't need to specify
# skeleton explicitly if the skeleton is stored as an attribute inside flesh.
# In particular, flesh was created from some object obj with
#
#	unlist(as.relistable(obj))
#
# then the skeleton attribute is automatically set.
#
# As long as "skeleton" has the right shape, it should be a precise inverse
# of unlist.  These equalities hold:
#
#	relist(unlist(x), skeleton) == x
#	unlist(relist(y, skeleton)) == y
#
#	x <- as.relistable(x)
#	relist(unlist(x)) == x

is.relistable <- function(x) inherits(x, "relistable")

as.relistable <- function(x)
{
    if (!inherits(x, "relistable"))
	class(x) <- c("relistable", class(x))
    x
}

## NB: unlist() is generic *internally* (i.e. not visible from 'unlist')
unlist.relistable <- function(x, recursive=TRUE, use.names=TRUE)
{
    if (!recursive)
	warning("relist() requires recursively unlisted objects.")
    skeleton <- x
### MM: FIXME?  I think this is just  NextMethod()
    ## remove 'relistable'
    class(x) <- setdiff(class(x), "relistable")
    result <- unlist(x, recursive, use.names)
    attr(result, "skeleton") <- skeleton
    result
}

relist <- function(flesh, skeleton=attr(flesh, "skeleton"))
{
    if (is.null(skeleton)) {
	stop("The flesh argument does not contain a skeleton attribute.\n",
	     "Either ensure you unlist a relistable object, or specify the skeleton separately.")
    }
    UseMethod("relist", skeleton)
}

## was 'relist.numeric' in Andrew's code
relist.default <- function(flesh, skeleton=attr(flesh, "skeleton"))
{
    result <- flesh
    names(result) <- names(skeleton)
    result
}

relist.list <- function(flesh, skeleton=attr(flesh, "skeleton"))
{
    ind <- 1L
    result <- skeleton
    for (i in seq_along(skeleton)) {
	size <- length(unlist(result[[i]]))
	result[[i]] <-
	    relist(flesh[ind:(ind + size - 1L)], result[[i]])
	ind <- ind + size
    }
    result
}


relist.matrix <- function(flesh, skeleton=attr(flesh, "skeleton"))
{
    if (is.numeric(skeleton[1,1]))
	return(matrix(flesh, nrow=nrow(skeleton),
		      dimnames=dimnames(skeleton)))
    n <- nrow(skeleton)
    m <- ncol(skeleton)
    result <- skeleton
    ind <- 1L
    for (j in 1L:m)
	for (i in 1L:n) {
	    size <- length(unlist(skeleton[[i, j]]))
	    result[[i, j]] <- relist(flesh[ind:(ind + size - 1)],
				     skeleton[[i, j]])
	    ind <- ind + size
	}
    result
}

relist.factor <- function(flesh, skeleton=attr(flesh, "skeleton"))
{
    as.factor(levels(skeleton)[flesh])
}
#  File src/library/utils/R/roman.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

as.roman <-
function(x)
{
    if(is.numeric(x))
        x <- as.integer(x)
    else if(is.character(x)) {
        ## Let's be nice: either strings that are *all* arabics, or
        ## (hopefully, for the time being) all romans.
        x <- if(all(grepl("^[[:digit:]]+$", x)))
            as.integer(x)
        else
            .roman2numeric(x)
    }
    else
        stop("cannot coerce 'x' to roman")
    x[(x <= 0L | x >= 3900L)] <- NA
    class(x) <- "roman"
    x
}

as.character.roman <-
function(x, ...)
    .numeric2roman(x)
format.roman <-
function(x, ...)
    format(as.character(x))
print.roman <-
function(x, ...)
{
    print(noquote(as.character(x)))
    invisible(x)
}
"[.roman" <-
function(x, i)
{
    cl <- oldClass(x)
    y <- NextMethod("[")
    oldClass(y) <- cl
    y
}

.numeric2roman <-
function(x) {
    romans <- c("M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX",
                "V", "IV", "I")
    numbers <- c(1000L, 900L, 500L, 400L, 100L, 90L, 50L, 40L, 10L, 9L,
                 5L, 4L, 1L)
    n2r <- function(z) {
        y <- character()
        for(i in seq_along(romans)) {
            d <- numbers[i]
            while(z >= d) {
                z <- z - d
                y <- c(y, romans[i])
            }
        }
        paste(y, collapse = "")
    }

    out <- character(length(x))
    x <- as.integer(x)
    ind <- is.na(x) | (x <= 0L) | (x >= 3900L)
    out[ind] <- NA
    if(any(!ind))
        out[!ind] <- sapply(x[!ind], n2r)
    out
}

.roman2numeric <-
function(x)
{
    ## <FIXME>
    ## What if this fails?
    ## Should say something like "Not a valid roman number ..."
    ## </FIXME>
    romans <- c("M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX",
                "V", "IV", "I")
    numbers <- c(1000L, 900L, 500L, 400L, 100L, 90L, 50L, 40L, 10L, 9L,
                 5L, 4L, 1L)
    out <- integer(length(x))
    ind <- is.na(x)
    out[ind] <- NA
    if(any(!ind)) {
        y <- toupper(x[!ind])
        y <- gsub("CM", "DCCCC", y)
        y <- gsub("CD", "CCCC", y)
        y <- gsub("XC", "LXXXX", y)
        y <- gsub("XL", "XXXX", y)
        y <- gsub("IX", "VIIII", y)
        y <- gsub("IV", "IIII", y)
        ok <- grepl("^M{,3}D?C{,4}L?X{,4}V?I{,4}$", y)
        if(any(!ok)) {
            warning(gettextf("Invalid roman numeral(s): %s",
                             paste(x[!ind][!ok], collapse = " ")))
            out[!ind][!ok] <- NA
        }
        if(any(ok))
            out[!ind][ok] <-
                sapply(strsplit(y[ok], ""),
                       function(z)
                       as.integer(sum(numbers[match(z, romans)])))
    }
    out
}
#  File src/library/tools/R/rtags.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/



### These utilities are intended to read R code files and produce tags
### in Emacs' etags format.  Doing so in R allows us to use R's
### parser.  Support for vi-style tags could be useful, but it needs
### the tags file needs to be sorted, making file-by-file processing
### difficult. It may be easier to write a script to convert an etags
### format file (see http://http://en.wikipedia.org/wiki/Ctags).



### * shorten.to.string

## The etags format requires the initial part of a matching line to be
## recorded in the TAGS file, with an optional entry for the `token'
## that is to be matched.  Exact matches to the token are preferred,
## but it seems that subsequent non-exact matches look in the initial
## string as well as other tokens with equal priority.  Such matches
## seem pointless, so an attempt is made to shorten the matching line.
## It is not clear to me whether there are restrictions on what this
## part could be, but a completely blank string doesn't seem to work.
## For now, I'm just keeping the first letter. May change if problems
## arise.

shorten.to.string <-
    function(line, token)
{
    if (FALSE) {
        ans <- regexpr(strsplit("token", ",", fixed = TRUE)[[1L]][1L],
                       line, fixed = TRUE)
        if (ans == -1L) line
        else substr(line, 1L, ans + attr(ans, "match.length") - 1L)
    }
    else {
        ## can we just put essentially nothing? Seems to work
        substr(line, 1L, 1L)
    }
}


### * write.etags

## this function is responsible for formatting the output for a single
## file given the relevant information.  The format was inferred from
## the "Ctags" wikipedia entry and by studying etags output.

write.etags <-
    function(src,
             tokens, startlines, lines, nchars,
             ...,
             shorten.lines = c("token", "simple", "none"))
{
    ## extra 1 for newline
    shorten.lines <- match.arg(shorten.lines)
    offsets <- (cumsum(nchars + 1L) - (nchars + 1L))[startlines]
    lines <-
        switch(shorten.lines,
               none = lines,
               simple = sapply(strsplit(lines, "function", fixed = TRUE), "[", 1),
               token = mapply(shorten.to.string, lines, tokens))
    tag.lines <-
        paste(sprintf("%s\x7f%s\x01%d,%d",
                      lines, tokens, startlines,
                      as.integer(offsets)),
              collapse = "\n")
    ## simpler format: tag.lines <- paste(sprintf("%s\x7f%d,%d", lines, startlines, as.integer(offsets)), collapse = "\n")
    tagsize <- nchar(tag.lines, type = "bytes") + 1L
    cat("\x0c\n", src, ",", tagsize, "\n", tag.lines, "\n", sep = "", ...)
}


### * expr2token

## this computes the tag name from an expression.  Currently, this
## returns the second thing in the expression; so
##
##   foo <- function(x) ...      ==> `<-`,       foo, ...
##   setMethod("foo", "bar" ...  ==> setMethod,  foo, ...
##   setGeneric("foo", "bar" ... ==> setGeneric, foo, ...
##
## which covers the typical uses.  We match against a list to restrict
## types of expressions that are tagged.  To reject things like
##
##   x[i] <- 10
##
## the second component is required to have length 1.  One limitation
## is that things like
##
##   if (require(pkg)) foo <- ... else foo <- ...
##
## will not be handled.

expr2token <-
    function(x,
             ok = c("<-", "=", "<<-", "assign",
                    "setGeneric", "setGroupGeneric", "setMethod",
                    "setClass", "setClassUnion"),
             extended = TRUE)
{
    id <- ""
    value <-
        if ((length(x) > 1L) &&
            (length(token <- as.character(x[[2L]])) == 1L) &&
            (length(id <- as.character(x[[1L]])) == 1L) &&
            (id %in% ok)) token
        else
            character(0L)
    if (extended && identical(id, "setMethod"))
    {
        ## try to add the signature, comma separated
        sig <- tryCatch(eval(x[[3L]]), error = identity)
        if (!inherits(sig, "error") && is.character(sig))
            value <- paste(c(value, sig), collapse=",")
    }
    value
}


### * rtags.file

## Handles a single file

rtags.file <-
    function(src, ofile = "", append = FALSE,
             write.fun = write.etags) ## getOption("writeTags")
{

    ## FIXME: do we need to worry about encoding etc.?
    elist <- parse(src, srcfile = srcfile(src))
    if (length(elist) == 0) return(invisible())
    lines <- readLines(src)
    tokens <- lapply(elist, expr2token)
    startlines <- sapply(attr(elist, "srcref"), "[", 1L)
    if (length(tokens) != length(startlines))
        stop("length mismatch: bug in code!")
    keep <- sapply(tokens, length) == 1L
    if (!any(keep)) return(invisible())
    tokens <- unlist(tokens[keep])
    startlines <- startlines[keep]
    write.fun(src = src,
              tokens = tokens,
              startlines = startlines,
              lines = lines[startlines],
              nchars = nchar(lines, type = "bytes"),
              file = ofile, append = append)
}

### * rtags

## Public interface.  Tags files under a specified directory, using
## regular expressions to filter out inappropriate files.


rtags <-
    function(path = ".", pattern = "\\.[RrSs]$",
             recursive = FALSE,
             src = list.files(path = path,
                              pattern = pattern,
                              full.names = TRUE,
                              recursive = recursive),
             keep.re = NULL,
             ofile = "", append = FALSE,
             verbose = getOption("verbose"))
{
    if (ofile != "" && !append) file.remove(ofile)
    if (!missing(keep.re))
        src <- grep(keep.re, src, value = TRUE)
    for (s in src)
    {
        if (verbose) message("Processing file ", s)
        tryCatch(
                 rtags.file(s, ofile = ofile, append = TRUE),
                 error = function(e) NULL)
    }
    invisible()
}



## Typical usage:

if (FALSE)
{

    ## to tag all .[RrSs] files under /path/to/src/repository/ that
    ## have "/R/" in the full path


}




### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***


#  File src/library/utils/R/sessionInfo.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

sessionInfo <- function(package=NULL)
{
    z <- list()
    z$R.version <- R.Version()
    z$locale <- Sys.getlocale()

    if(is.null(package)){
        package <- grep("^package:", search(), value=TRUE)
        # weed out environments which are not really packages
        keep <- sapply(package, function(x) x == "package:base" || !is.null(attr(as.environment(x), "path")))
        package <- sub("^package:", "", package[keep])
    }

    pkgDesc <- lapply(package, packageDescription)
    if(length(package) == 0) stop("no valid packages were specified")
    basePkgs <- sapply(pkgDesc,
                       function(x) !is.null(x$Priority) && x$Priority=="base")
    z$basePkgs <- package[basePkgs]
    if(any(!basePkgs)){
        z$otherPkgs <- pkgDesc[!basePkgs]
        names(z$otherPkgs) <- package[!basePkgs]
    }
    loadedOnly <- loadedNamespaces()
    loadedOnly <- loadedOnly[!(loadedOnly %in% package)]
    if (length(loadedOnly)) {
        names(loadedOnly) <- loadedOnly
        pkgDesc <- c(pkgDesc, lapply(loadedOnly, packageDescription))
        z$loadedOnly <- pkgDesc[loadedOnly]
    }
    class(z) <- "sessionInfo"
    z
}

print.sessionInfo <- function(x, locale=TRUE, ...)
{
    mkLabel <- function(L, n) {
        vers <- sapply(L[[n]], function(x) x[["Version"]])
        pkg <-  sapply(L[[n]], function(x) x[["Package"]])
        paste(pkg, vers, sep="_")
    }

    cat(x$R.version$version.string, "\n")
    cat(x$R.version$platform, "\n\n")
    if(locale){
        cat("locale:\n")
        print(strsplit(x$locale, ";", fixed=TRUE)[[1]], quote=FALSE)
        cat("\n")
    }
    cat("attached base packages:\n")
    print(x$basePkgs, quote=FALSE)
    if(!is.null(x$otherPkgs)){
        cat("\nother attached packages:\n")
        print(mkLabel(x, "otherPkgs"), quote=FALSE)
    }
    if(!is.null(x$loadedOnly)){
        cat("\nloaded via a namespace (and not attached):\n")
        print(mkLabel(x, "loadedOnly"), quote=FALSE)
    }
    invisible(x)
}

toLatex.sessionInfo <- function(object, locale=TRUE, ...)
{
    opkgver <- sapply(object$otherPkgs, function(x) x$Version)
    nspkgver <- sapply(object$loadedOnly, function(x) x$Version)
    z <- c("\\begin{itemize}\\raggedright",
           paste("  \\item ", object$R.version$version.string,
                 ", \\verb|", object$R.version$platform, "|", sep=""))
    
    if(locale){
        z <- c(z, 
               paste("  \\item Locale: \\verb|",
                     gsub(";","|, \\\\verb|", object$locale)
                     , "|", sep=""))
    }
    
    z <- c(z, strwrap(paste("\\item Base packages: ",
                         paste(sort(object$basePkgs), collapse=", ")),
                   indent=2, exdent=4))

    if(length(opkgver)){
        opkgver <- opkgver[sort(names(opkgver))]
        z <- c(z,
               strwrap(paste("  \\item Other packages: ",
                             paste(names(opkgver), opkgver, sep="~",
                                   collapse=", ")),
                       indent=2, exdent=4))
    }
    if(length(nspkgver)){
        nspkgver <- nspkgver[sort(names(nspkgver))]
        z <- c(z,
               strwrap(paste("  \\item Loaded via a namespace (and not attached): ",
                             paste(names(nspkgver), nspkgver, sep="~",
                                   collapse=", ")),
                       indent=2, exdent=4))
    }
    z <- c(z, "\\end{itemize}")
    class(z) <- "Latex"
    z
}
#  File src/library/utils/R/sock.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

print.socket <- function(x, ...)
{
    if(length(as.integer(x$socket)) != 1L)
	stop("invalid 'socket' argument")
    cat("Socket connection #", x$socket, "to", x$host,
	"on port", x$port, "\n")
    invisible(x)
}

make.socket <- function(host = "localhost", port, fail = TRUE, server = FALSE)
{
    if(length(port <- as.integer(port)) != 1L)
	stop("'port' must be integer of length 1")
    if(length(host <- as.character(host)) != 1L)
	stop("'host' must be character of length 1")
    if (!server){
	tmp2 <- .C("Rsockconnect",
                   port = port,
                   host = host,
                   PACKAGE = "base")
    }
    else{
	if (host != "localhost")
	    stop("can only receive calls on local machine")
	tmp <- .C("Rsockopen", port = port, PACKAGE="base")
	buffer <- paste(rep.int("#",256L), collapse = "")
	tmp2 <- .C("Rsocklisten", port = tmp$port,
                   buffer = buffer, len = 256L, PACKAGE="base")
	host <- substr(tmp2$buffer, 1L, tmp2$len)
	.C("Rsockclose", tmp$port, PACKAGE="base")
    }
    if (tmp2$port <= 0) {
	if (fail) stop("socket not established")
        else warning("socket not established")
    }
    rval <- list(socket = tmp2$port, host = host, port = port)
    class(rval) <- "socket"
    rval
}

close.socket <- function(socket, ...)
{
    if(length(port <- as.integer(socket$socket)) != 1L)
	stop("invalid 'socket' argument")
    as.logical(.C("Rsockclose", port, PACKAGE="base")[[1L]])
}

read.socket <- function(socket, maxlen=256, loop=FALSE)
{
    if(length(port <- as.integer(socket$socket)) != 1L)
	stop("invalid 'socket' argument")
    maxlen <- as.integer(maxlen)
    buffer <- paste(rep.int("#",maxlen), collapse="")
    repeat {
	tmp <- .C("Rsockread", port,
		  buffer = buffer, len = maxlen, PACKAGE="base")
	rval <- substr(tmp$buffer, 1L, tmp$len)
	if (nzchar(rval) || !loop) break
    }
    rval
}

write.socket <- function(socket, string)
{
    if(length(port <- as.integer(socket$socket)) != 1L)
	stop("invalid 'socket' argument")
    strlen <- length(strsplit(string,NULL)[[1L]])
    invisible(.C("Rsockwrite", port, string,
		 as.integer(0L), strlen, strlen, PACKAGE="base")[[5]])
}


#  File src/library/utils/R/str.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

####------ str : show STRucture of an R object
str <- function(object, ...) UseMethod("str")

## FIXME: convert to use sQuote
str.data.frame <- function(object, ...)
{
    ## Method to 'str' for  'data.frame' objects
    if(! is.data.frame(object)) {
	warning("str.data.frame() called with non-data.frame -- coercing to one.")
	object <- data.frame(object)
    }

    ## Show further classes // Assume that they do NOT have an own Method --
    ## not quite perfect ! (.Class = 'remaining classes', starting with current)
    cl <- oldClass(object); cl <- cl[cl != "data.frame"]  #- not THIS class
    if(0 < length(cl)) cat("Classes", paste(sQuote(cl), collapse=", "), "and ")

    cat("'data.frame':	", nrow(object), " obs. of  ",
	(p <- length(object)), " variable", if(p != 1)"s", if(p > 0)":",
	"\n",sep="")

    ## calling next method, usually  str.default:
    if(length(l <- list(...)) && any("give.length" == names(l)))
	invisible(NextMethod("str", ...))
    else invisible(NextMethod("str", give.length=FALSE,...))
}

str.POSIXt <- function(object, ...) {
    cl <- oldClass(object)
    ## be careful to be fast for large object:
    n <- length(object)
    if(n >= 1000L) object <- object[seq_len(1000L)]

    give.length <- TRUE ## default
    ## use 'give.length' when specified, else default = give.head
    if(length(larg <- list(...))) {
	nl <- names(larg)
	iGiveHead <- which(nl == "give.head")
	if (any(Bgl <- nl == "give.length"))
	    give.length <- larg[[which(Bgl)]]
	else if(length(iGiveHead))
	    give.length <- larg[[iGiveHead]]
	if(length(iGiveHead)) # eliminate it from arg.list
	    larg <- larg[ - iGiveHead ]
	if(is.numeric(larg[["nest.lev"]]) &&
	   is.numeric(v.len <- larg[["vec.len"]])) # typical call from data.frame
	    ## diminuish length for typical call:
	    larg[["vec.len"]] <-
		min(larg[["vec.len"]],
		    (larg[["width"]]- nchar(larg[["indent.str"]]) -31)%/% 19)
    }

    le.str <- if(give.length) paste("[1:",as.character(n),"]", sep="")
    cat(" ", cl[min(2, length(cl))], le.str,", format: ", sep = "")
    do.call(str, c(list(format(object), give.head = FALSE), larg))
}

strOptions <- function(strict.width = "no", digits.d = 3, vec.len = 4,
		       formatNum = function(x, ...)
		       format(x, trim=TRUE, drop0trailing=TRUE, ...))
    list(strict.width = strict.width, digits.d = digits.d, vec.len = vec.len,
	 formatNum = match.fun(formatNum))

str.default <-
    function(object, max.level = NA, vec.len = strO$vec.len,
	     digits.d = strO$digits.d,
	     nchar.max = 128, give.attr = TRUE,
	     give.head = TRUE, give.length = give.head,
	     width = getOption("width"), nest.lev = 0,
	     indent.str= paste(rep.int(" ", max(0,nest.lev+1)), collapse= ".."),
	     comp.str="$ ", no.list = FALSE, envir = baseenv(),
	     strict.width = strO$strict.width,
	     formatNum = strO$formatNum,
	     ...)
{
    ## Purpose: Display STRucture of any R - object (in a compact form).
    ## --- see HELP file --
    ## ------------------------------------------------------------------------
    ## Author: Martin Maechler <maechler@stat.math.ethz.ch>	1990--1997

    ## Get defaults for these
    oDefs <- c("vec.len", "digits.d", "strict.width", "formatNum")
    ## from
    strO <- getOption("str")
    if (!is.list(strO)) {
	warning("invalid options('str') -- using defaults instead")
	strO <- strOptions()
    }
    else {
        if (!all(names(strO) %in% oDefs))
            warning("invalid components in options('str'): ",
                    paste(setdiff(names(strO), oDefs), collapse = ", "))
        strO <- modifyList(strOptions(), strO)
    }
    strict.width <- match.arg(strict.width, choices = c("no", "cut", "wrap"))
    if(strict.width != "no") {
	## using eval() would be cleaner, but fails inside capture.output():
	ss <- capture.output(str.default(object, max.level = max.level,
				 vec.len = vec.len, digits.d = digits.d,
				 nchar.max = nchar.max,
				 give.attr= give.attr, give.head= give.head, give.length= give.length,
				 width = width, nest.lev = nest.lev,
				 indent.str = indent.str, comp.str= comp.str,
				 no.list= no.list || is.data.frame(object),
				 envir = envir, strict.width = "no", ...) )
	if(strict.width == "wrap") {
	    nind <- nchar(indent.str) + 2
	    ss <- strwrap(ss, width = width, exdent = nind)
					# wraps at white space (only)
	}
	if(any(iLong <- nchar(ss) > width))
	    ss[iLong] <- sub(sprintf("^(.{1,%d}).*", width-2), "\\1..",
			     ss[iLong])
	cat(ss, sep="\n")
	return(invisible())
    }

    oo <- options(digits = digits.d); on.exit(options(oo))
    le <- length(object)
    P0 <- function(...) paste(..., sep="")
    maybe_truncate <- function(x, e.x = x, Sep = "\"", ch = "| __truncated__")
    {
	trimmed <- strtrim(e.x, nchar.max)
	ii <- trimmed != e.x
	ii[is.na(ii)] <- FALSE
	if(any(ii)) x[ii] <- P0(trimmed[ii], Sep, ch)
	x
    }
    pClass <- function(cls)
	paste("Class", if(length(cls) > 1) "es",
              " '", paste(cls, collapse = "', '"), "' ", sep="")

    ## le.str: not used for arrays:
    le.str <-
	if(is.na(le)) " __no length(.)__ "
	else if(give.length) {
	    if(le > 0) P0("[1:", paste(le), "]") else "(0)"
	} else ""
    v.len <- vec.len # modify v.len, not vec.len!
    ## NON interesting attributes:
    std.attr <- "names"

    cl <- if((S4 <- isS4(object))) class(object) else oldClass(object)
    has.class <- S4 || !is.null(cl) # S3 or S4
    mod <- ""; char.like <- FALSE
    if(give.attr) a <- attributes(object)#-- save for later...

    if (is.null(object))
	cat(" NULL\n")
    else if(S4) {
	a <- sapply(methods::.slotNames(object), methods::slot,
		    object=object, simplify = FALSE)
	cat("Formal class", " '", paste(cl, collapse = "', '"),
	    "' [package \"", attr(cl,"package"), "\"] with ",
	    length(a)," slots\n", sep="")
	str(a, no.list = TRUE, comp.str = "@ ", # instead of "$ "
	    max.level = max.level, vec.len = vec.len, digits.d = digits.d,
	    indent.str = paste(indent.str,".."), nest.lev = nest.lev + 1,
	    nchar.max = nchar.max, give.attr = give.attr, width=width)
	return(invisible())
    }
    else if(is.function(object)) {
	cat(if(is.null(ao <- args(object))) deparse(object)
	else { dp <- deparse(ao); paste(dp[-length(dp)], collapse="\n") },"\n")
    } else if(is.list(object)) {
	i.pl <- is.pairlist(object)
	is.d.f <- is.data.frame(object)
	##?if(is.d.f) std.attr <- c(std.attr, "class", if(is.d.f) "row.names")
	if(le == 0) {
	    if(is.d.f) std.attr <- c(std.attr, "class", "row.names")
	    else cat(" ", if(i.pl)"pair", "list()\n",sep="")
	} else { # list, length >= 1 :
	    if(irregCl <- has.class && identical(object[[1L]], object)) {
		le <- length(object <- unclass(object))
		std.attr <- c(std.attr, "class")
	    }
	    if(no.list || (has.class &&
			   any(sapply(paste("str", cl, sep="."),
					#use sys.function(.) ..
				      function(ob)exists(ob, mode= "function",
							 inherits= TRUE))))) {
		## str.default is a 'NextMethod' : omit the 'List of ..'
		std.attr <- c(std.attr, "class", if(is.d.f) "row.names")
	    } else {
		cat(if(i.pl) "Dotted pair list" else
		    if(irregCl) paste(pClass(cl), "hidden list") else "List",
		    " of ", le, "\n", sep="")
	    }
	    if (is.na(max.level) || nest.lev < max.level) {
		nam.ob <-
		    if(is.null(nam.ob <- names(object))) rep.int("", le)
		    else { max.ncnam <- max(nchar(nam.ob, type="w"))
			   format(nam.ob, width = max.ncnam, justify="left")
		       }
		for(i in seq_len(le)) {
		    cat(indent.str, comp.str, nam.ob[i], ":", sep="")
		    envir <- # pass envir for 'promise' components:
			if(typeof(object[[i]]) == "promise") {
			    structure(object, nam= as.name(nam.ob[i]))
			} # else NULL
		    str(object[[i]], nest.lev = nest.lev + 1,
                        indent.str = paste(indent.str,".."),
                        nchar.max = nchar.max, max.level = max.level,
                        vec.len = vec.len, digits.d = digits.d,
                        give.attr = give.attr, give.head= give.head, give.length = give.length,
                        width = width, envir = envir)
		}
	    }
	}
    } else { #- not function, not list
	if(is.vector(object)
	   || (is.array(object) && is.atomic(object))
	   || is.vector(object, mode= "language")
	   || is.vector(object, mode= "symbol")## R bug(<=0.50-a4) should be part
	   ) { ##-- Splus: FALSE for 'named vectors'
	    if(is.atomic(object)) {
		##-- atomic:   numeric	complex	 character  logical
		mod <- substr(mode(object), 1, 4)
		if     (mod == "nume")
		    mod <- if(is.integer(object)) "int"
		    else if(has.class) cl[1L] else "num"
		else if(mod == "char") { mod <- "chr"; char.like <- TRUE }
		else if(mod == "comp") mod <- "cplx" #- else: keep 'logi'
		if(is.array(object)) {
		    rnk <- length(di. <- dim(object))
		    di <- P0(ifelse(di. > 1, "1:",""), di.,
			     ifelse(di. > 0, "" ," "))
		    pDi <- function(...) paste(c("[", ..., "]"), collapse = "")
		    le.str <- (if(rnk == 1) pDi(di[1L], "(1d)") else
			       pDi(P0(di[-rnk], ", "), di[rnk]))
		    std.attr <- "dim" #- "names"
		} else if(!is.null(names(object))) {
		    mod <- paste("Named", mod)
		    std.attr <- std.attr[std.attr != "names"]
		}
		if(has.class && length(cl) == 1) {
		    if(cl != mod && substr(cl, 1,nchar(mod)) != mod)
			mod <- P0("'",cl,"' ", mod)
		    ## don't show the class *twice*
		    std.attr <- c(std.attr, "class")
		}
		str1 <-
		    if(le == 1 && !is.array(object)) paste(NULL, mod)
		    else P0(" ", mod, if(le>0)" ", le.str)
	    } else { ##-- not atomic, but vector: #
		mod <- typeof(object)#-- typeof(.) is more precise than mode!
		str1 <- switch(mod,
			       call = " call",
			       language = " language",
			       symbol = " symbol",
			       expression = " ",# "expression(..)" by deparse(.)
			       name = " name",
			       ##not in R:argument = "",# .Argument(.) by deparse(.)
			       ## in R (once):	comment.expression

			       ## default :
			       paste("		#>#>", mod, NULL)
			       )
	    }
#  These are S-PLUS classes not found in R.
#	} else if (inherits(object,"rts") || inherits(object,"cts")
#		   || inherits(object,"its")) {
#	    tsp.a <- tspar(object)
#	    t.cl <- cl[b.ts <- substring(cl,2,3) == "ts"] # "rts" "cts" or "its"
#	    ts.kind <- switch(t.cl,
#			      rts="Regular", cts="Calendar", its="Irregular")
#	    ## from  print.summary.ts(.) :
#	    pars <- unlist(sapply(summary(object)$ pars, format,
#				  nsmall=0, digits=digits.d, justify = "none"))
#	    if(length(pars)>=4) pars <- pars[-3]
#	    pars <- paste(abbreviate(names(pars),min=2), pars,
#			  sep= "=", collapse=", ")
#	    str1 <- P0(ts.kind, " Time-Series ", le.str, " ", pars, ":")
#	    v.len <- switch(t.cl,rts=.8, cts=.6, its=.9) * v.len
#	    class(object) <- if(any(!b.ts)) cl[!b.ts]
#	    std.attr <- c(std.attr, "tspar")
	} else if(stats::is.ts(object)) {
	    tsp.a <- stats::tsp(object)
	    str1 <- P0(" Time-Series ", le.str, " from ", format(tsp.a[1L]),
		       " to ", format(tsp.a[2L]), ":")
	    std.attr <- c("tsp","class") #- "names"
	} else if (is.factor(object)) {
	    nl <- length(lev.att <- levels(object))
	    if(!is.character(lev.att)) {# should not happen..
		warning("'object' does not have valid levels()")
		nl <- 0
	    } else lev.att <- encodeString(lev.att, na.encode = FALSE, quote = '"')
	    ord <- is.ordered(object)
	    object <- unclass(object)
	    if(nl) {
		## as from 2.1.0, quotes are included ==> '-2':
		lenl <- cumsum(3 + (nchar(lev.att, type="w") - 2))# level space
		ml <- if(nl <= 1 || lenl[nl] <= 13)
		    nl else which(lenl > 13)[1L]
		lev.att <- maybe_truncate(lev.att[seq_len(ml)])
##		if((d <- lenl[ml] - if(ml>1)18 else 14) >= 3)# truncate last
##		    lev.att[ml] <-
##			P0(substring(lev.att[ml],1, nchar(lev.att[ml])-d), "..")
	    }
	    else # nl == 0
		ml <- length(lev.att <- "")

	    lsep <- if(ord) "<" else ","
	    str1 <- P0(if(ord)" Ord.f" else " F",
		       "actor w/ ", nl, " level", if(nl != 1) "s",
		       if(nl) " ",
		       if(nl) P0(lev.att, collapse = lsep),
		       if(ml < nl) P0(lsep, ".."), ":")

	    std.attr <- c("levels", "class")
	} else if(typeof(object) %in%
		  c("externalptr", "weakref", "environment")) {
	    ## Careful here, we don't want to change pointer objects
	    if(has.class)
                cat(pClass(cl))
	    le <- v.len <- 0
	    str1 <-
		if(is.environment(object)) format(object)
		else paste("<", typeof(object), ">", sep="")
	    has.class <- TRUE # fake for later
	    std.attr <- "class"
	    ## ideally we would figure out if as.character has a
	    ## suitable method and use that.
	} else if(has.class) {
	    cat("Class", if(length(cl) > 1) "es",
		" '", paste(cl, collapse = "', '"), "' ", sep="")
	    ## If there's a str.<method>, it should have been called before!
	    uo <- unclass(object)
	    if(!is.null(attributes(uo)$class)) {
		## another irregular case
		xtr <- c(if(identical(uo, object)) { # trap infinite loop
		    class(uo) <- NULL
		    "unclass()-immune"
		} else if(!is.object(object)) "not-object")
		if(!is.null(xtr)) cat("{",xtr,"} ", sep="")
	    }
	    str(uo,
		max.level = max.level, vec.len = vec.len, digits.d = digits.d,
		indent.str = paste(indent.str,".."), nest.lev = nest.lev + 1,
		nchar.max = nchar.max, give.attr = give.attr, width=width)
	    return(invisible())
	} else if(is.atomic(object)) {
	    if((1 == length(a <- attributes(object))) && (names(a) == "names"))
		str1 <- paste(" Named vector", le.str)
	    else {
		##-- atomic / not-vector  "unclassified object" ---
		str1 <- paste(" atomic", le.str)
	    }
	} else if(typeof(object) == "promise") {
	    cat(" promise ")
	    if (!is.null(envir)) {
		objExp <- eval(bquote(substitute(.(attr(envir, "nam")), envir)))
		cat("to ")
		str(objExp,
		    max.level= max.level, vec.len= vec.len, digits.d= digits.d,
		    indent.str = indent.str, nest.lev = nest.lev,
		    nchar.max = nchar.max, give.attr = give.attr, width=width)
	    } else cat(" <...>\n")
	    return(invisible())
	} else {
	    ##-- NOT-atomic / not-vector  "unclassified object" ---
	    ##str1 <- paste(" ??? of length", le, ":")
	    str1 <- paste("length", le)
	}
	##-- end  if else..if else...  {still non-list case}

	##-- This needs some improvement: Not list nor atomic --
	if ((is.language(object) || !is.atomic(object)) && !has.class) {
	    ##-- has.class superfluous --
	    mod <- mode(object)
	    give.mode <- FALSE
	    if (any(mod == c("call", "language", "(", "symbol"))
		|| is.environment(object)) {
		##give.mode <- !is.vector(object)# then it has not yet been done
		if(mod == "(") give.mode <- TRUE
		typ <- typeof(object)
		object <- deparse(object)

		le <- length(object) # is > 1 e.g. for {A;B} language
		format.fun <- function(x)x
		v.len <- round(.5 * v.len)
		if(le > 1 && typ=="language" && object[1L] == "{" && object[le]=="}") {
		    v.len <- v.len + 2
		    if(le >= 3) {
			object <- c(object[1L],
				    paste(sub("^ +", " ", object[2:(le-1)]),
					  collapse = ";"),
				    object[le])
			le <- length(object)
		    }
		}
	    } else if (mod == "expression") {
		format.fun <- function(x) deparse(as.expression(x))
		v.len <- round(.75 * v.len)
	    } else if (mod == "name"){
		object <- paste(object)#-- show `as' char
	    } else if (mod == "argument"){
		format.fun <- deparse
	    } else {
		give.mode <- TRUE
	    }
	    if(give.mode) str1 <- P0(str1, ', mode "', mod,'":')

	} else if(is.logical(object)) {
	    v.len <- 1.5 * v.len # was '3' originally (but S prints 'T' 'F' ..)
	    format.fun <- formatNum
	} else if(is.numeric(object)) {
	    iv.len <- round(2.5 * v.len)
	    if(iSurv <- inherits(object, "Surv"))
		std.attr <- c(std.attr, "class")
	    int.surv <- iSurv || is.integer(object)
	    if(!int.surv) {
		ob <- if(le > iv.len) object[seq_len(iv.len)] else object
		ao <- abs(ob <- ob[!is.na(ob)])
	    }
	    else if(iSurv)
		le <- length(object <- as.character(object))
	    if(int.surv || (all(ao > 1e-10 | ao==0) && all(ao < 1e10| ao==0) &&
			    all(abs(ob - signif(ob, digits.d)) <= 9e-16*ao))) {
		if(!iSurv || di.[2L] == 2) # "Surv" : implemented as matrix
		    ## use integer-like length
		    v.len <- iv.len
		format.fun <- formatNum
	    } else {
		v.len <- round(1.25 * v.len)
		format.fun <- formatNum
	    }
	} else if(is.complex(object)) {
	    v.len <- round(.75 * v.len)
	    format.fun <- formatNum
	}

	## Not sure, this is ever triggered:
	if(is.na(le)) { warning("'str.default': 'le' is NA"); le <- 0}

	if(char.like) {
	    ## if object is very long, drop the rest which won't be used anyway:
	    max.len <- max(100, width %/% 3 + 1, if(!missing(vec.len)) vec.len)
	    if(le > max.len) object <- object[seq_len(max.len)]
	    encObj <- encodeString(object, quote= '"', na.encode= FALSE)
					#O: encodeString(object)
	    v.len <-
		if(missing(vec.len)) {
		    max(1,sum(cumsum(3 + if(le>0) nchar(encObj, type="w") else 0) <
			      width - (4 + 5*nest.lev + nchar(str1, type="w"))))
		}		      # '5*ne..' above is fudge factor
		else round(v.len)
	    ile <- min(le, v.len)
	    if(ile >= 1) ## truncate if LONG char:
		object <- maybe_truncate(encObj[seq_len(ile)])
					#O: encodeString(object, quote= '"', na.encode= FALSE)
	    formObj <- function(x) paste(as.character(x), collapse=" ")
	}
	else {
	    if(!exists("format.fun", inherits=TRUE)) #-- define one --
		format.fun <-
		    if(mod == "num" || mod == "cplx") format else as.character
	    ## v.len <- max(1,round(v.len))
	    ile <- min(v.len, le)
	    formObj <- function(x) paste(format.fun(x), collapse = " ")
	}

	cat(if(give.head) P0(str1, " "),
	    formObj(if(ile >= 1) object[seq_len(ile)] else if(v.len > 0) object),
	    if(le > v.len) " ...", "\n", sep="")

    } ## else (not function nor list)----------------------------------------

    if(give.attr) { ## possible: || has.class && any(cl == "terms")
	nam <- names(a)
	for (i in seq_along(a))
	    if (all(nam[i] != std.attr)) {# only `non-standard' attributes:
		cat(indent.str, P0('- attr(*, "',nam[i],'")='),sep="")
		str(a[[i]],
		    indent.str= paste(indent.str,".."), nest.lev= nest.lev+1,
		    max.level = max.level, digits.d = digits.d,
		    nchar.max = nchar.max,
		    vec.len = if(nam[i] == "source") 1 else vec.len,
		    give.attr= give.attr, give.head= give.head, give.length= give.length, width= width)
	    }
    }
    invisible()	 ## invisible(object)#-- is SLOOOOW on large objects
}# end of `str.default()'

## An extended `ls()' whose print method will use str() :
ls.str <-
    function (pos = -1, name, envir, all.names = FALSE, pattern, mode = "any")
{
    if(missing(envir)) ## [for "lazy" reasons, this fails as default]
        envir <- as.environment(pos)
    nms <- ls(name, envir = envir, all.names=all.names, pattern=pattern)
    r <- unlist(lapply(nms, function(n)
                       exists(n, envir= envir, mode= mode, inherits=FALSE)))
    structure(nms[r], envir = envir, mode = mode, class = "ls_str")
}

lsf.str <- function(pos = -1, envir, ...) {
    if(missing(envir)) ## [for "lazy" reasons, this fails as default]
        envir <- as.environment(pos)
    ls.str(pos=pos, envir=envir, mode = "function", ...)
}

print.ls_str <- function(x, max.level = 1, give.attr = FALSE,
                         ..., digits = max(1, getOption("str")$digits.d))
{
    E <- attr(x, "envir")
    stopifnot(is.environment(E))
    M <- attr(x, "mode")
    args <- list(...)
    if(length(args) && "digits.d" %in% names(args)) {
        if(missing(digits))
            digits <- args$digits.d
        else
            warning("'digits' and 'digits.d' are both specified and the latter is not used")
        args$digits.d <- NULL
    }
    strargs <- c(list(max.level = max.level, give.attr = give.attr,
                      digits = digits), args)
    for(nam in x) {
	cat(nam, ": ")
	## check missingness, e.g. inside debug(.) :

##__ Why does this give	 too many <missing> in some case?
##__	if(eval(substitute(missing(.), list(. = as.name(nam))),
##__		envir = E))
##__	    cat("<missing>\n")
##__	else
##__	    str(get(nam, envir = E, mode = M),
##__		max.level = max.level, give.attr = give.attr, ...)

	o <- tryCatch(get(nam, envir = E, mode = M), error = function(e)e)
	if(inherits(o, "error")) {
	    cat(## FIXME: only works with "C" (or English) LC_MESSAGES locale!
		if(length(grep("missing|not found", o$message)))
		"<missing>" else o$message, "\n", sep='')
	}
	else
	    do.call(str, c(list(o), strargs),
		    quote = is.call(o)) # protect calls from evaluation
    }
    invisible(x)
}
#  File src/library/utils/R/summRprof.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/


summaryRprof <-
    function(filename = "Rprof.out", chunksize = 5000,
             memory = c("none", "both", "tseries", "stats"),
             index = 2, diff = TRUE, exclude = NULL)
{
    con <- file(filename, "rt")
    on.exit(close(con))
    firstline <- readLines(con, n = 1L)
    if(!length(firstline))
        stop(gettextf("no lines found in %s", sQuote(filename)), domain = NA)
    sample.interval <- as.numeric(strsplit(firstline, "=")[[1L]][2L])/1e6
    memory.profiling <- substr(firstline, 1L, 6L) == "memory"

    memory <- match.arg(memory)
    if(memory != "none" && !memory.profiling)
        stop("profile does not contain memory information")
    if (memory == "tseries")
        return(Rprof_memory_summary(filename = con, chunksize = chunksize,
                                    label = index, diff = diff, exclude = exclude,
                                    sample.interval = sample.interval))
    else if (memory == "stats")
        return(Rprof_memory_summary(filename = con,  chunksize = chunksize,
                                    aggregate = index, diff = diff, exclude = exclude,
                                    sample.interval = sample.interval))


    fnames <- NULL
    ucounts <- NULL
    fcounts <- NULL
    memcounts <- NULL
    umem <- NULL

    repeat({

       chunk <- readLines(con, n = chunksize)
       if (length(chunk) == 0L)
           break
       if (memory.profiling) {
           memprefix <- attr(regexpr(":[0-9]+:[0-9]+:[0-9]+:[0-9]+:", chunk), "match.length")
           if (memory == "both"){
               memstuff <- substr(chunk, 2L, memprefix-1L)
               memcounts <- pmax(apply(sapply(strsplit(memstuff, ":"), as.numeric), 1, diff), 0)
               memcounts <- c(0, rowSums(memcounts[, 1L:3L]))
               rm(memstuff)
           }
           chunk <- substr(chunk, memprefix+1L, nchar(chunk,  "c"))
           if(any((nc <- nchar(chunk, "c")) == 0L)) {
                chunk <- chunk[nc > 0L]
                memcounts <- memcounts[nc > 0L]
           }
       }

       chunk <- strsplit(chunk, " ")

       newfirsts <- sapply(chunk,  "[[",  1L)
       newuniques <- lapply(chunk,  unique)
       ulen <- sapply(newuniques, length)
       newuniques <- unlist(newuniques)

       new.utable <- table(newuniques)
       new.ftable <- table(factor(newfirsts, levels = names(new.utable)))
       if (memory == "both")
           new.umem <- rowsum(memcounts[rep.int(seq_along(memcounts), ulen)], newuniques)

       fcounts <- rowsum( c(as.vector(new.ftable), fcounts),
                         c(names(new.ftable), fnames) )
       ucounts <- rowsum( c(as.vector(new.utable), ucounts),
                         c(names(new.utable), fnames) )
       if(memory == "both")
           umem <- rowsum(c(new.umem, umem), c(names(new.utable), fnames))

       fnames <- sort(unique(c(fnames, names(new.utable))))

       if (length(chunk) < chunksize) break
    })

    if (sum(fcounts) == 0)
        stop("no events were recorded")

    digits <- ifelse(sample.interval < 0.01,  3L, 2L)
    firstnum <- round(fcounts*sample.interval, digits)
    uniquenum <- round(ucounts*sample.interval, digits)

    firstpct <- round(100*firstnum/sum(firstnum), 1)
    uniquepct <- round(100*uniquenum/sum(firstnum), 1)

    if (memory == "both") memtotal <-  round(umem/1048576, 1)     ## 0.1MB

    index1 <- order(-firstnum, -uniquenum)
    index2 <- order(-uniquenum, -firstnum)

    rval <- data.frame(firstnum, firstpct, uniquenum, uniquepct)
    names(rval) <- c("self.time", "self.pct", "total.time", "total.pct")
    rownames(rval) <- fnames
    if (memory == "both") rval$mem.total <- memtotal

    list(by.self = rval[index1, ],
         by.total = rval[index2, c(3L, 4L,  if(memory == "both") 5L, 1L, 2L)],
         sampling.time = sum(fcounts)*sample.interval)
}

Rprof_memory_summary <- function(filename, chunksize = 5000,
                                 label = c(1, -1), aggregate = 0, diff = FALSE,
                                 exclude = NULL, sample.interval){

    fnames <- NULL
    memcounts <- NULL
    firsts <- NULL
    labels <- vector("list", length(label))
    index <- NULL

    repeat({
       chunk <- readLines(filename, n = chunksize)
       if (length(chunk) == 0L)
           break
       memprefix <- attr(regexpr(":[0-9]+:[0-9]+:[0-9]+:[0-9]+:", chunk),
                         "match.length")
       memstuff <- substr(chunk, 2L, memprefix-1L)
       memcounts <- rbind(t(sapply(strsplit(memstuff, ":"), as.numeric)))

       chunk <- substr(chunk, memprefix+1, nchar(chunk,  "c"))
       if(any((nc <- nchar(chunk,  "c")) == 0L)) {
           memcounts <- memcounts[nc > 0L, ]
           chunk <- chunk[nc > 0L]
       }

       chunk <- strsplit(chunk, " ")

       if (length(exclude))
           chunk <- lapply(chunk,  function(l) l[!(l %in% exclude)])

       newfirsts <- sapply(chunk,  "[[",  1L)
       firsts <- c(firsts, newfirsts)

       if (!aggregate && length(label)){
           for(i in seq_along(label)){

               if (label[i] == 1)
                   labels[[i]] <- c(labels[[i]], newfirsts)
               else if (label[i]>1) {
                   labels[[i]] <- c(labels[[i]],  sapply(chunk,
                                                         function(line)
                                                         paste(rev(line)[1L:min(label[i], length(line))],
                                                               collapse = ":")))
               } else {
                   labels[[i]] <- c(labels[[i]], sapply(chunk,
                                                        function(line)
                                                        paste(line[1L:min(-label[i], length(line))],
                                                              collapse = ":")))
               }
           }
       } else if (aggregate) {
           if (aggregate > 0) {
               index <- c(index,  sapply(chunk,
                                         function(line)
                                         paste(rev(line)[1L:min(aggregate, length(line))],
                                               collapse = ":")))

           } else {
               index <- c(index,  sapply(chunk,
                                         function(line)
                                         paste(line[1L:min(-aggregate, length(line))],
                                               collapse = ":")))
           }
       }


       if (length(chunk) < chunksize)
           break
    })

    if (length(memcounts) == 0L) stop("no events were recorded")

    memcounts <- as.data.frame(memcounts)
    names(memcounts) <- c("vsize.small", "vsize.large", "nodes", "duplications")
    if (!aggregate) {
        rownames(memcounts) <- (1L:nrow(memcounts))*sample.interval
        names(labels) <- paste("stack", label, sep = ":")
        memcounts <- cbind(memcounts, labels)
    }

    if (diff)
        memcounts[-1L, 1L:3L]  <-  pmax(0L, apply(memcounts[, 1L:3L], 2L, diff))

    if (aggregate)
        memcounts <- by(memcounts, index,
                        function(these) with(these,
                                             round(c(vsize.small = mean(vsize.small),
                                                     max.vsize.small = max(vsize.small),
                                                     vsize.large = mean(vsize.large),
                                                     max.vsize.large = max(vsize.large),
                                                     nodes = mean(nodes),
                                                     max.nodes = max(nodes),
                                                     duplications = mean(duplications),
                                                     tot.duplications = sum(duplications),
                                                     samples = nrow(these)
                                                     ))
                                             )
                        )
    return(memcounts)
}
#  File src/library/utils/R/tar.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

untar <- function(tarfile, files = NULL, list = FALSE, exdir = ".",
                  compressed = NA, extras = NULL, verbose = FALSE,
                  tar = Sys.getenv("TAR"))
{
    if (inherits(tarfile, "connection") || identical(tar, "internal"))
        return(untar2(tarfile, files, list, exdir))

    TAR <- tar
    if (!nzchar(TAR) && .Platform$OS.type == "windows") {
        res <- tryCatch(system("tar.exe --version", intern = TRUE),
                        error = identity)
        if (!inherits(res, "error")) TAR <- "tar.exe"
    }
    if (!nzchar(TAR) || TAR == "internal")
        return(untar2(tarfile, files, list, exdir))

    cflag <- ""
    if (is.character(compressed)) {
        ## Any tar which supports -J does not need it for extraction
        switch(match.arg(compressed, c("gzip", "bzip2", "xz")),
               "gzip" = "z", "bzip2" = "j", "xz" = "J")
    } else if (is.logical(compressed)) {
        if (is.na(compressed)) {
            magic <- readBin(tarfile, "raw", n = 3)
            if(all(magic[1:2] == c(0x1f, 0x8b))) cflag <- "z"
            else if(all(magic[1:2] == c(0x1f, 0x9d))) cflag <- "z" # compress
            else if(rawToChar(magic[1:3]) == "BZh") cflag <- "j"
            else if(rawToChar(magic[1:5]) == "\xFD7zXZ") cflag <- "J"
        } else if (compressed) cflag <- "z"
    } else stop("'compressed' must be logical or character")

    gzOK <- .Platform$OS.type == "windows"
    if (!gzOK ) {
        ## version info may be sent to stdout or stderr
        tf <- tempfile()
        cmd <- paste(TAR, " -", cflag, "tf ", shQuote(tarfile), sep = "")
        system(paste(TAR, "--version >", tf, "2>&1"))
        if (file.exists(tf)) {
            gzOK <- any(grepl("GNU", readLines(tf), fixed = TRUE))
            unlink(tf)
        }
    }
    tarfile <- path.expand(tarfile)
    if (!gzOK && cflag == "z" && nzchar(ZIP <- Sys.getenv("R_GZIPCMD"))) {
        TAR <- paste(ZIP, "-dc", tarfile, "|", TAR)
        tarfile <- "-"
        cflag <- ""
    }
    if (!gzOK && cflag == "j" && nzchar(ZIP <- Sys.getenv("R_BZIPCMD"))) {
        TAR <- paste(ZIP,  "-dc", tarfile, "|", TAR)
        tarfile < "-"
        cflag <- ""
    }
    if (cflag == "J") {
        TAR <- paste("xz -dc", tarfile, "|", TAR)
        tarfile < "-"
        cflag <- ""
    }
    if (list) {
        cmd <- paste(TAR, " -", cflag, "tf ", shQuote(tarfile), sep = "")
        if (length(extras)) cmd <- paste(cmd, extras, collapse = " ")
        if (verbose) message("untar: using cmd = ", sQuote(cmd))
        system(cmd, intern = TRUE)
    } else {
        cmd <- paste(TAR, " -", cflag, "xf ", shQuote(tarfile), sep = "")
        if (!missing(exdir)) {
            dir.create(exdir, showWarnings = FALSE, recursive = TRUE)
            cmd <- paste(cmd, "-C", shQuote(exdir))
        }
        if (length(extras)) cmd <- paste(cmd, extras, collapse = " ")
        if (length(files))
            cmd <- paste(cmd, paste(shQuote(files), collapse = " "))
        if (verbose) message("untar: using cmd = ", sQuote(cmd))
        res <- system(cmd)
        if (res) warning(sQuote(cmd), " returned error code ", res,
                         domain = NA)
        invisible(res)
    }
}

untar2 <- function(tarfile, files = NULL, list = FALSE, exdir = ".")
{
    getOct <- function(x, offset, len)
    {
        x <- 0L
        for(i in offset + seq_len(len)) {
            z <- block[i]
            if(!as.integer(z)) break; # terminate on nul
            switch(rawToChar(z),
                   " " = {},
                   "0"=,"1"=,"2"=,"3"=,"4"=,"5"=,"6"=,"7"=
                   {x <- 8*x + (as.integer(z)-48)},
                   stop("invalid octal digit")
                   )
        }
        x
    }

    ## A tar file is a set of 512 byte records,
    ## a header record followed by file contents (zero-padded).
    ## See http://en.wikipedia.org/wiki/Tar_%28file_format%29
    if(is.character(tarfile)) {
        con <- gzfile(path.expand(tarfile), "rb") # reads compressed formats
        on.exit(close(con))
    } else if(inherits(tarfile, "connection")) con <- tarfile
    else stop("'tarfile' must be a character string or a connection")
    if (!missing(exdir)) {
        dir.create(exdir, showWarnings = FALSE, recursive = TRUE)
        od <- setwd(exdir)
        on.exit(setwd(od), add = TRUE)
    }
    contents <- character()
    llink <- lname <- NULL
    repeat{
        block <- readBin(con, "raw", n = 512L)
        if(!length(block)) break
        if(length(block) < 512L) stop("incomplete block on file")
        if(all(block == 0)) break
        ns <- max(which(block[1:100] > 0))
        name <- rawToChar(block[seq_len(ns)])
        magic <- rawToChar(block[258:262])
        if ((magic == "ustar") && block[346] > 0) {
            ns <- max(which(block[346:500] > 0))
            prefix <- rawToChar(block[345+seq_len(ns)])
            name <- file.path(prefix, name)
        }
        ## mode zero-padded 8 bytes (including nul) at 101
        ## Aargh: bsdtar has this one incorrectly with 6 bytes+space
        mode <- as.octmode(getOct(block, 100, 8))
        size <- getOct(block, 124, 12)
        ts <- getOct(block, 136, 12)
        ft <- as.POSIXct(as.numeric(ts), origin="1970-01-01", tz="UTC")
        csum <- getOct(block, 148, 6)
        block[149:156] <- charToRaw(" ")
        xx <- as.integer(block)
        checksum <- sum(xx) %% 2^24 # 6 bytes
        if(csum != checksum) {
            ## try it with signed bytes.
            checksum <- sum(ifelse(xx > 127, xx - 128, xx)) %% 2^24 # 6 bytes
            if(csum != checksum)
                warning(gettextf("checksum error for entry '%s'", name),
                        domain = NA)
        }
        type <- block[157L]
        ctype <- rawToChar(type)
        if(type == 0L || ctype == "0") {
            if(!is.null(lname)) {name <- lname; lname <- NULL}
            contents <- c(contents, name)
            remain <- size
            dothis <- !list
            if(dothis && length(files)) dothis <- name %in% files
            if(dothis) {
                dir.create(dirname(name), showWarnings = FALSE,
                           recursive = TRUE)
                out <- file(name, "wb")
            }
            for(i in seq_len(ceiling(size/512L))) {
                block <- readBin(con, "raw", n = 512L)
                if(length(block) < 512L)
                    stop("incomplete block on file")
                if (dothis) {
                    writeBin(block[seq_len(min(512L, remain))], out)
                    remain <- remain - 512L
                }
            }
            if(dothis) {
                close(out)
                Sys.chmod(name, mode)
                .Call("R_setFileTime", name, ft, PACKAGE = "base")
            }
        } else if(ctype %in% c("1", "2")) { # hard and symbolic links
            contents <- c(contents, name)
            ns <- max(which(block[158:257] > 0))
            name2 <- rawToChar(block[157L + seq_len(ns)])
            if(!is.null(lname)) {name <- lname; lname <- NULL}
            if(!is.null(llink)) {name2 <- llink; llink <- NULL}
            if(!list) {
                ## this will not work for links to dirs on Windows
                if(.Platform$OS.type == "windows") file.copy(name2, name)
                else file.symlink(name2, name)
            }
        } else if(ctype == "5") {
            contents <- c(contents, name)
            if(!list) {
                dir.create(name, showWarnings = FALSE, recursive = TRUE)
                Sys.chmod(name, mode)
                ## not much point, since dir will be populated afterwards
                ## .Call("R_setFileTime", name, ft)
            }
        } else if(ctype %in% c("L", "K")) {
            ## This is a GNU extension that should no longer be
            ## in use, but it is.
            name_size <- 512L * ceiling(size/512L)
            block <- readBin(con, "raw", n = name_size)
            if(length(block) < name_size)
                stop("incomplete block on file")
            ns <- max(which(block > 0)) # size on file may or may not include final nul
            if(ctype == "L")
                lname <- rawToChar(block[seq_len(ns)])
            else
                llink <- rawToChar(block[seq_len(ns)])
        } else stop("unsupported entry type ", sQuote(ctype))
    }
    if(list) contents else invisible(0L)
}

tar <- function(tarfile, files = NULL,
                compression = c("none", "gzip", "bzip2", "xz"),
                compression_level = 6, tar = Sys.getenv("tar"))
{
    if(is.character(tarfile)) {
        TAR <- tar
        if(nzchar(TAR) && TAR != "internal") {
            ## FIXME: could pipe through gzip etc: might be safer for xz
            ## as -J was lzma in GNU tar 1.20:21
            flags <- switch(match.arg(compression),
                            "none" = "cf",
                            "gzip" = "zcf",
                            "bzip2" = "jcf",
                            "xz" = "Jcf")
            cmd <- paste(shQuote(TAR), flags, shQuote(tarfile),
                         paste(shQuote(files), collapse=" "))
            return(invisible(system(cmd)))
        }
        con <- switch(match.arg(compression),
                      "none" =    file(tarfile, "wb"),
                      "gzip" =  gzfile(tarfile, "wb", compress = compression_level),
                      "bzip2" = bzfile(tarfile, "wb", compress = compression_level),
                      "xz" =    xzfile(tarfile, "wb", compress = compression_level))
        on.exit(close(con))
    } else if(inherits(tarfile, "connection")) con <- tarfile
    else stop("'tarfile' must be a character string or a connection")

    files <- list.files(files, recursive = TRUE, all.files = TRUE,
                        full.names = TRUE)
    ## this omits directories: get back the non-empty ones
    bf <- unique(dirname(files))
    files <- c(bf[!bf %in% c(".", files)], files)

    for (f in unique(files)) {
        info <- file.info(f)
        if(is.na(info$size)) {
            warning(gettextf("file '%s' not found", f), domain = NA)
            next
        }
        header <- raw(512L)
        ## add trailing / to dirs.
        if(info$isdir && !grepl("/$", f)) f <- paste(f, "/", sep = "")
        name <- charToRaw(f)
        if(length(name) > 100L) {
            if(length(name) > 255L) stop("file path is too long")
            s <- max(which(name[1:155] == charToRaw("/")))
            if(is.infinite(s) || s+100 < length(name))
                stop("file path is too long")
            warning("storing paths of more than 100 bytes is not portable:\n  ",
                    sQuote(f))
            prefix <- name[1:(s-1)]
            name <- name[-(1:s)]
            header[345+seq_along(prefix)] <- prefix
        }
        header[seq_along(name)] <- name
        header[101:107] <- charToRaw(sprintf("%07o", info$mode))
        if(!is.na(info$uid))
            header[109:115] <- charToRaw(sprintf("%07o", info$uid))
        if(!is.na(info$gid))
            header[117:123] <- charToRaw(sprintf("%07o", info$gid))
        ## size is 0 for directories and it seems for links.
        size <- ifelse(info$isdir, 0, info$size)
        header[137:147] <- charToRaw(sprintf("%011o", as.integer(info$mtime)))
        if (info$isdir) header[157L] <- charToRaw("5")
        else {
            lnk <- Sys.readlink(f)
            if(is.na(lnk)) lnk <- ""
            header[157L] <- charToRaw(ifelse(nzchar(lnk), "2", "0"))
            if(nzchar(lnk)) {
                ## we could use the GNU extension ...
                if(length(lnk) > 100L) stop("linked path is too long")
                header[157L + seq_len(nchar(lnk))] <- charToRaw(lnk)
                size <- 0
            }
        }
        header[125:135] <- charToRaw(sprintf("%011o", as.integer(size)))
        ## the next two are what POSIX says, not what GNU tar does.
        header[258:262] <- charToRaw("ustar")
        header[264:265] <- charToRaw("0")
        if(!is.na(s <- info$uname)) {
            ns <- nchar(s, "b")
            header[265L + (1:ns)] <- charToRaw(s)
        }
        if(!is.na(s <- info$grname)) {
            ns <- nchar(s, "b")
            header[297L + (1:ns)] <- charToRaw(s)
        }
        header[149:156] <- charToRaw(" ")
        checksum <- sum(as.integer(header)) %% 2^24 # 6 bytes
        header[149:154] <- charToRaw(sprintf("%06o", as.integer(checksum)))
        header[155L] <- as.raw(0L)
        writeBin(header, con)
        if(info$isdir || nzchar(lnk)) next
        inf <- file(f, "rb")
        for(i in seq_len(ceiling(info$size/512L))) {
            block <- readBin(inf, "raw", 512L)
            writeBin(block, con)
            if( (n <- length(block)) < 512L) writeBin(raw(512L - n), con)
        }
        close(inf)
    }
    ## trailer is two blocks of nuls.
    block <- raw(512L)
    writeBin(block, con)
    writeBin(block, con)
    invisible(0L)
}
#  File src/library/utils/R/toLatex.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

toBibtex <- function(object, ...) UseMethod("toBibtex")

print.Bibtex <- function(x, prefix="", ...)
{
    writeLines(paste(prefix, unclass(x), sep=""))
    invisible(x)
}

toLatex <- function(object, ...) UseMethod("toLatex")
    
print.Latex <- function(x, prefix="", ...)
{
    writeLines(paste(prefix, unclass(x), sep=""))
    invisible(x)
}
create.post <- function(instructions = "\\n",
                        description = "post",
                        subject = "",
                        ccaddress = Sys.getenv("USER"),
                        method = getOption("mailer"),
                        address ="the relevant mailing list",
                        file = "R.post")
{
    methods <- c("mailx", "gnudoit", "none", "ess")
    method <-
	if(is.null(method)) "none"
	else methods[pmatch(method, methods)]

    body <- paste(instructions,
		  "--please do not edit the information below--\\n\\n",
		  "Version:\\n ",
		  paste(names(R.version),R.version, sep=" = ",collapse="\\n "),
                  if (nzchar(Sys.getenv("R_GUI_APP_VERSION")))
                      paste("\\n\\nGUI:\\n R-GUI ",Sys.getenv("R_GUI_APP_VERSION"),
                            " (",Sys.getenv("R_GUI_APP_REVISION"),")",sep='')
                  else
                      ""
                  ,
                  "\\n\\n",
                  "Locale:\\n",
                  Sys.getlocale(),
		  "\\n\\n",
		  "Search Path:\\n ",
		  paste(search(), collapse=", "),
		  "\\n", sep="", collapse="")

    if(method == "gnudoit") {
	cmd <- paste("gnudoit -q '",
		     "(mail nil \"", address, "\")",
		     "(insert \"", body, "\")",
		     "(search-backward \"Subject:\")",
		     "(end-of-line)'",
		     sep="")
	system(cmd)
    } else if(method=="none") {
        disclaimer <-
            paste("# Your mailer is set to \"none\" (default on Windows),\n",
                  "# hence we cannot send the, ", description, " directly from R.\n",
                  "# Please copy the ", description, " (after finishing it) to\n",
                  "# your favorite email program and send it to\n#\n",
                  "#       ", address, "\n#\n",
                  "######################################################\n",
                  "\n\n", sep = "")

        cat(disclaimer, file=file)
	body <- gsub("\\\\n", "\n", body)
	cat(body, file=file, append=TRUE)
        cat("The", description, "is being opened for you to edit.\n")
	system(paste(getOption("editor"), file))
        cat("The unsent", description, "can be found in file", file, "\n")
    } else if(method == "mailx") {
        if(missing(subject)) stop("'subject' missing")

	body <- gsub("\\\\n", "\n", body)
	cat(body, file=file, append=FALSE)
        cat("The", description, "is being opened for you to edit.\n")
	system(paste(getOption("editor"), file))

        if(is.character(ccaddress) && nzchar(ccaddress)) {
            cmdargs <- paste("-s '", subject, "' -c", ccaddress,
                             address, "<", file, "2>/dev/null")
        }
        else
            cmdargs <- paste("-s '", subject, "'", address, "<",
                             file, "2>/dev/null")

        status <- 1L
        answer <- readline(paste("Email the ", description, " now? (yes/no) ",
                                 sep = ""))
        answer <- grep("yes", answer, ignore.case=TRUE)
        if(length(answer)) {
            cat("Sending email ...\n")
            status <- system(paste("mailx", cmdargs))
            if(status)
                status <- system(paste("Mail", cmdargs))
            if(status)
                status <- system(paste("/usr/ucb/mail", cmdargs))

            if(status == 0L) unlink(file)
            else{
                cat("Sending email failed!\n")
                cat("The unsent", description, "can be found in file",
                    file, "\n")
            }
        } else
            cat("The unsent", description, "can be found in file",
                file, "\n")
    }
    else if(method == "ess") {
	body <- gsub("\\\\n", "\n", body)
	cat(body)
    }
}
#  File src/library/utils/R/unix/download.file.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

download.file <- function(url, destfile, method,
                          quiet = FALSE, mode = "w", cacheOK = TRUE)
{
    method <- if (missing(method))
        ifelse(!is.null(getOption("download.file.method")),
               getOption("download.file.method"),
               "auto")
    else
        match.arg(method, c("auto", "internal", "wget", "lynx"))

    if(method == "auto") {
        if(capabilities("http/ftp"))
            method <- "internal"
        else if(length(grep("^file:", url))) {
            method <- "internal"
            url <- URLdecode(url)
        } else if(system("wget --help > /dev/null")==0)
            method <- "wget"
        else if(system("lynx -help > /dev/null")==0)
            method <- "lynx"
        else
            stop("no download method found")
    }
    if(method == "internal") {
        status <- .Internal(download(url, destfile, quiet, mode, cacheOK))
        ## needed for Mac GUI from download.packages etc
        if(!quiet) flush.console()
    } else if(method == "wget") {
        extra <- if(quiet) " --quiet" else ""
        if(!cacheOK) extra <- paste(extra, "--cache=off")
        status <- system(paste("wget", extra, " ", shQuote(url),
                               " -O ", path.expand(destfile), sep=""))
    } else if(method == "lynx")
        status <- system(paste("lynx -dump ", shQuote(url), " > ",
                               path.expand(destfile), sep=""))

    if(status) warning("download had nonzero exit status")

    invisible(status)
}

nsl <- function(hostname)
    .Internal(nsl(hostname))
#  File src/library/utils/R/unix/help.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/


offline_help_helper <- function(texfile, type = "postscript")
{
    PDF <- type == "pdf"
    tools::texi2dvi(texfile, pdf=PDF, clean=TRUE)
    ofile <- sub("tex$", if(PDF) "pdf" else "ps", texfile)
    if(!PDF) {
        dfile <- sub("tex$", "dvi", texfile)
        on.exit(unlink(dfile))
        dvips <- getOption("dvipscmd", default = "dvips")
        res <- system(paste(dvips, dfile, "> /dev/null 2>&1"))
        if(res)
            stop(gettextf("running '%s' failed", dvips), domain = NA)
        if(!file.exists(ofile)) {
            message(gettextf("'%s' produced no output file: sent to printer?",
                             dvips), domain = NA)
            return(invisible())
        }
    } else if(!file.exists(ofile))
        stop(gettextf("creation of '%s' failed", ofile), domain = NA)
    if(ofile != basename(ofile)) file.copy(ofile, basename(ofile))
    message("Saving help page to ", sQuote(basename(ofile)))
    invisible()
}
#  File src/library/utils/R/unix/help.start.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

help.start <-
    function (update = FALSE, gui = "irrelevant",
              browser = getOption("browser"), remote = NULL)
{
    ## should always be set, but might be empty
    if (!is.function(browser) && (length(browser) != 1 || !is.character(browser) || !nzchar(browser)))
	stop("invalid browser name, check options(\"browser\").")
    home <- if (is.null(remote)) {
        if (tools:::httpdPort == 0L) tools::startDynamicHelp()
        if (tools:::httpdPort > 0L) {
            if (update) make.packages.html()
            paste("http://127.0.0.1:", tools:::httpdPort, sep = "")
        } else stop("help.start() requires the HTTP server to be running",
                    call. = FALSE)
    } else remote
    url <- paste(home, "/doc/html/index.html", sep = "")

    if (is.character(browser)) {
        writeLines(strwrap(gettextf("If '%s' is already running, it is *not* restarted, and you must switch to its window.",
                                    browser),
                           exdent = 4L))
        writeLines(gettext("Otherwise, be patient ..."))
    }
    browseURL(url, browser = browser)
    invisible()
}

browseURL <- function(url, browser = getOption("browser"), encodeIfNeeded=FALSE)
{
    ## escape characters.  ' can occur in URLs, so we must use " to
    ## delimit the URL.  We need to escape $, but "`\ do not occur in
    ## valid URLs (RFC 2396, on the W3C site).
    shQuote <- function(string)
        paste('"', gsub("\\$", "\\\\$", string), '"', sep="")

    if (!is.character(url) || length(url) != 1L|| !nzchar(url))
        stop("'url' must be a non-empty character string")
    if (is.function(browser))
        return(invisible(browser(if(encodeIfNeeded) URLencode(url) else url)))
    if (!is.character(browser)
       || length(browser) != 1L
       || !nzchar(browser))
        stop("'browser' must be a non-empty character string")

    if (.Platform$GUI == "AQUA" ||
        length(grep("^(localhost|):", Sys.getenv("DISPLAY"))) )
      isLocal <- TRUE
    else
      isLocal <- FALSE

    quotedUrl <- shQuote(if(encodeIfNeeded) URLencode(url) else url)
    remoteCmd <- if (isLocal)
        switch(basename(browser),
               "gnome-moz-remote" =, "open" = quotedUrl,
               "galeon" = paste("-x", quotedUrl),
               "kfmclient" = paste("openURL", quotedUrl),
               "mozilla" =, "opera" =, "firefox" = {
                   paste("-remote \"openURL(",
                         ## Quote ',' and ')' ...
                         gsub("([,)$])", "%\\1", url), ")\"",
                         sep = "")
               }, quotedUrl)
    else quotedUrl
    system(paste(browser, remoteCmd, "> /dev/null 2>&1 ||",
                 browser, quotedUrl, "&"))
}

make.packages.html <-
    function(lib.loc = .libPaths(), temp = TRUE, verbose = TRUE)
{
    f.tg <- if (temp) {
        dir.create(file.path(tempdir(), ".R/doc/html"), recursive = TRUE,
                   showWarnings = FALSE)
        file.path(tempdir(), ".R/doc/html/packages.html")
    } else file.path(R.home("doc"), "html", "packages.html")
    op <- file.path(tempdir(), ".R/doc/html/libPaths.rds")
    if (temp && file.exists(f.tg) && file.exists(op)) {
        ## check if we can avoid remaking it.
        old <- .readRDS(op)$libs
        if(identical(lib.loc, old)) {
            dates <- file.info(c(f.tg, lib.loc))$mtime
            if(which.max(dates) == 1L) return(TRUE)
        }
    }
    if (!file.create(f.tg)) {
        # warning("cannot update HTML package index")
        return(FALSE)
    }
    if (verbose) {
        message("Making packages.html", " ", appendLF = FALSE)
        flush.console()
    }
    file.append(f.tg,
                file.path(R.home("doc"), "html", "packages-head-utf8.html"))
    out <- file(f.tg, open = "a")
    on.exit(close(out))
    ## find out how many
    pkgs <- vector("list", length(lib.loc))
    names(pkgs) <- lib.loc
    for (lib in lib.loc) {
        pg <- Sys.glob(file.path(lib, "*", "DESCRIPTION"))
        pkgs[[lib]] <- sort(sub(paste(lib, "([^/]*)", "DESCRIPTION$", sep="/"),
                                "\\1", pg))
    }
    tot <- sum(sapply(pkgs, length))
    incr <- ifelse(tot > 500L, 100L, 10L)
    npkgs <- 0L
    for (lib in lib.loc) {
        if (verbose && npkgs %% incr == 0L) {
            message(".", appendLF = FALSE)
            flush.console()
        }
        cat("<p><h3>Packages in ", lib,
            '</h3>\n<p><table width="100%" summary="R Package list">\n',
            sep = "", file=out)
        for (i in pkgs[[lib]]) {
            title <- packageDescription(i, lib.loc = lib, fields = "Title",
                                        encoding = "UTF-8")
            if (is.na(title)) title <- "-- Title is missing --"
            cat('<tr align="left" valign="top">\n',
                '<td width="25%"><a href="../../library/', i,
                '/html/00Index.html">', i, "</a></td><td>", title,
                "</td></tr>\n", file=out, sep="")
            npkgs <- npkgs + 1L
        }
        cat("</table>\n\n", file=out)
    }
    cat("</body></html>\n", file=out)
    if (verbose) {
        message(" ", "done")
        flush.console()
    }
    if (temp) .saveRDS(list(libs=lib.loc, npkgs=npkgs), op)
    invisible(TRUE)
}
#  File src/library/utils/R/unix/install.packages.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

link.html.help <- function(verbose=FALSE, lib.loc=.libPaths())
{
    if(verbose)
        message("Updating HTML index of packages in '.Library'")
    tools:::unix.packages.html(.Library)
}
#  File src/library/utils/R/aqua/install.packages.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/


if(substr(R.version$os, 1L, 6L) != "darwin") {
.install.macbinary <-
    function(pkgs, lib, repos = getOption("repos"),
             contriburl = contrib.url(repos, type="mac.binary"),
             method, available = NULL, destdir = NULL,
             dependencies = FALSE, ...)
    {}
} else {
## edited from windows/.install.winbinary
##
.install.macbinary <-
    function(pkgs, lib, repos = getOption("repos"),
             contriburl = contrib.url(repos, type="mac.binary"),
             method, available = NULL, destdir = NULL,
             dependencies = FALSE, ...)
{
    link.html.help<-function(verbose = FALSE, ...)
    {
        html <- getOption("htmlhelp")
        htype <- getOption("help_type")
        # update only if temporary help files exist
        if((!is.null(html) && html) || (!is.null(htype) && htype == "html")
            && file.exists(paste(tempdir(),"/.R/doc",sep=''))) {
            #.Script("sh", "help-links.sh", paste(tempdir(), paste(.libPaths(),
            #                                                      collapse = " ")))
            make.packages.html()
        }
    }
    untar<-function(what, where)
    {
        xcode <- system(paste("tar zxf \"", path.expand(what), "\" -C \"",
                              path.expand(where), "\"", sep=''), intern=FALSE)
        if (xcode)
            warning(gettextf("'tar' returned non-zero exit code %d", xcode),
                    domain = NA, call. = FALSE)
    }

    unpackPkg <- function(pkg, pkgname, lib)
    {
        ## Create a temporary directory and unpack the zip to it
        ## then get the real package & version name, copying the
        ## dir over to the appropriate install dir.
        tmpDir <- tempfile(, lib)
        if (!dir.create(tmpDir))
            stop(gettextf("unable to create temporary directory '%s'",
                          tmpDir),
                 domain = NA, call. = FALSE)
        cDir <- getwd()
        on.exit(setwd(cDir), add = TRUE)
        res <- untar(pkg, tmpDir)
        setwd(tmpDir)
        res <- tools::checkMD5sums(pkgname, file.path(tmpDir, pkgname))
        if(!is.na(res) && res) {
            cat(gettextf("package '%s' successfully unpacked and MD5 sums checked\n",
                         pkgname))
            flush.console()
        }

        ## Check to see if this is a bundle or a single package
        if (file.exists("DESCRIPTION")) {
            ## Bundle
            conts <- read.dcf("DESCRIPTION", fields="Contains")[1,]
            if (is.na(conts))
                stop("malformed bundle DESCRIPTION file, no Contains field")
            else
                pkgs <- strsplit(conts," ")[[1L]]
            ## now check the MD5 sums
            res <- TRUE
            for (curPkg in pkgs) res <- res &
            tools::checkMD5sums(pkgname, file.path(tmpDir, curPkg))
            if(!is.na(res) && res) {
                cat(gettextf("bundle '%s' successfully unpacked and MD5 sums checked\n",
                             pkgname))
                flush.console()
            }
        } else pkgs <- pkgname

        for (curPkg in pkgs) {
            desc <- read.dcf(file.path(curPkg, "DESCRIPTION"),
                             c("Package", "Version"))
            instPath <- file.path(lib, desc[1L,1L])

            ## If the package is already installed w/ this
            ## instName, remove it.  If it isn't there, the unlink call will
            ## still return success.
            ret <- unlink(instPath, recursive=TRUE)
            if (ret == 0L) {
                ## Move the new package to the install lib and
                ## remove our temp dir
                ret <- file.rename(file.path(tmpDir, curPkg), instPath)
                if(!ret)
                    warning(gettextf("unable to move temporary installation '%s' to '%s'",
                                     file.path(tmpDir, curPkg), instPath),
                            domain = NA, call. = FALSE)
            } else
                stop("cannot remove prior installation of package ",
                     sQuote(curPkg), call. = FALSE)
        }
        setwd(cDir)
        unlink(tmpDir, recursive=TRUE)
    }

    if(!length(pkgs)) return(invisible())

    if(is.null(contriburl)) {
        pkgnames <- basename(pkgs)
        pkgnames <- sub("\\.tgz$", "", pkgnames)
        pkgnames <- sub("\\.tar\\.gz$", "", pkgnames)
        pkgnames <- sub("_.*$", "", pkgnames)
        ## there is no guarantee we have got the package name right:
        ## foo.zip might contain package bar or Foo or FOO or ....
        ## but we can't tell without trying to unpack it.
        for(i in seq_along(pkgs))
            unpackPkg(pkgs[i], pkgnames[i], lib)
        link.html.help(verbose=TRUE)
        return(invisible())
    }
    tmpd <- destdir
    nonlocalcran <- length(grep("^file:", contriburl)) < length(contriburl)
    if(is.null(destdir) && nonlocalcran) {
        tmpd <- file.path(tempdir(), "downloaded_packages")
        if (!file.exists(tmpd) && !dir.create(tmpd))
            stop(gettextf("unable to create temporary directory '%s'", tmpd),
                 domain = NA)
    }

    if(is.null(available))
        available <- available.packages(contriburl = contriburl,
                                        method = method)
    pkgs <- getDependencies(pkgs, dependencies, available, lib)

    foundpkgs <- download.packages(pkgs, destdir = tmpd, available = available,
                                   contriburl = contriburl, method = method,
                                   type = "mac.binary", ...)

    if(length(foundpkgs)) {
        update <- unique(cbind(pkgs, lib))
        colnames(update) <- c("Package", "LibPath")
        for(lib in unique(update[,"LibPath"])) {
            oklib <- lib==update[,"LibPath"]
            for(p in update[oklib, "Package"])
            {
                okp <- p == foundpkgs[, 1L]
                if(any(okp))
                    unpackPkg(foundpkgs[okp, 2L], foundpkgs[okp, 1L], lib)
            }
        }
        if(!is.null(tmpd) && is.null(destdir))
            cat("\n", gettextf("The downloaded packages are in\n\t%s", tmpd),
                "\n", sep = "")
        link.html.help(verbose = TRUE)
    } else if(!is.null(tmpd) && is.null(destdir)) unlink(tmpd, TRUE)

    invisible()
}
}
#  File src/library/utils/R/unix/sysutils.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

memory.size <- function(max = FALSE)
{
    warning("'memory.size()' is Windows-specific", call.=FALSE)
    Inf
}

memory.limit <- function(size = NA)
{
   warning("'memory.limit()' is Windows-specific", call.=FALSE)
   Inf
}
#  File src/library/utils/R/utils-defunct.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
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
## Deprecated in 1.9.0
## Defunct in 2.0.0
package.contents <- function(pkg, lib.loc=NULL) .Defunct(package="utils")
## </entry>
#  File src/library/utils/R/utils-deprecated.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
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
## Deprecated in 2.2.0
CRAN.packages <- function(CRAN = getOption("repos"), method,
                          contriburl = contrib.url(CRAN))
{
    .Deprecated("available.packages")
    available.packages(contriburl = contriburl, method = method)
}
## </entry>
#  File src/library/utils/R/vignette.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

vignette <-
    function(topic, package = NULL, lib.loc = NULL, all = TRUE)
{
    if(is.null(package))
        package <- .packages(all.available = all, lib.loc)
    paths <- .find.package(package, lib.loc)

    ## Find the directories with a 'doc' subdirectory *possibly*
    ## containing vignettes.

    paths <- paths[file_test("-d", file.path(paths, "doc"))]

    vignettes <-
        lapply(paths,
               function(dir) {
                   tools::list_files_with_type(file.path(dir, "doc"),
                                               "vignette")
               })

    if(!missing(topic)) {
        topic <- topic[1L]               # Just making sure ...
        vignettes <- as.character(unlist(vignettes))
        vidx <- (tools::file_path_sans_ext(basename(vignettes)) == topic)
        if(any(vidx)) {

            pdf <- sub("\\.[[:alpha:]]+$", ".pdf", vignettes)
            pidx <- file_test("-f", pdf)
            ok <- vidx & pidx

            if(any(ok)){
                idx <- min(which(ok))
                if(sum(ok)>1){
                    ## <FIXME>
                    ## Should really offer a menu to select from.
                    warning(gettextf("vignette '%s' found more than once,\nusing the one found in '%s'", topic, dirname(pdf[idx])),
                            call. = FALSE, domain = NA)
                    ## </FIXME>
                }

                z <- list(file=vignettes[idx], pdf=pdf[idx])
            }
            else{
                z <- list(file=vignettes[vidx][1L], pdf=character(0L))
            }
            z$topic <- topic
            class(z) <- "vignette"
            return(z)
        }
        else
            warning(gettextf("vignette '%s' *not* found", topic),
                    call. = FALSE, domain = NA)
    }

    if(missing(topic)) {
        ## List all possible vignettes.

        vDB <- matrix(character(0L), nrow = 0L, ncol = 4L)
        colnames(vDB) <- c("Dir", "File", "Title", "PDF")

        for(db in vignettes[sapply(vignettes, length) > 0L]) {
            dir <- dirname(dirname(db[1L]))
            entries <- NULL
            ## Check for new-style 'Meta/vignette.rds' ...
            if(file.exists(INDEX <-
                           file.path(dir, "Meta", "vignette.rds")))
                entries <- .readRDS(INDEX)
            if(NROW(entries) > 0)
                vDB <- rbind(vDB,
                             cbind(dir,
                                   entries$File,
                                   entries$Title,
                                   entries$PDF))
        }

        ## Now compute info on available PDFs ...
        title <- if(NROW(vDB)) {
            paste(vDB[, "Title"],
                  paste(rep.int("(source", NROW(vDB)),
                        ifelse(vDB[, "PDF"] != "", ", pdf", ""),
                        ")",
                        sep = ""))
        }
        else
            character()
        ## ... and rewrite into the form used by packageIQR.
        db <- cbind(Package = basename(vDB[, "Dir"]),
                    LibPath = dirname(vDB[, "Dir"]),
                    Item = tools::file_path_sans_ext(basename(vDB[, "File"])),
                    Title = title)
	footer <- if (all) NULL else
		  paste("Use ",
	                sQuote("vignette(all = TRUE)"),
	                "\n",
	                "to list the vignettes in all *available* packages.",
                  	sep = "")

        y <- list(type = "vignette", title = "Vignettes", header = NULL,
                  results = db, footer = footer)
        class(y) <- "packageIQR"
        return(y)
    }
}

print.vignette <- function(x, ...){

    if(length(x$pdf)){
        ## <FIXME>
        ## Should really abstract this into a BioC style
        ## openPDF() along the lines of browseURL() ...
        if(.Platform$OS.type == "windows")
            shell.exec(x$pdf)
        else
            system(paste(shQuote(getOption("pdfviewer")), shQuote(x$pdf)),
                   wait = FALSE)
        ## </FIXME>
    } else {
        warning(gettextf("vignette '%s' has no PDF", x$topic),
                call. = FALSE, domain = NA)
    }
    invisible(x)
}

edit.vignette <- function(name, ...)
{

    f <- paste(tempfile(name$topic), ".R", sep="")
    Stangle(name$file, output=f, quiet=TRUE)
    file.edit(file=f, ...)
}
#  File src/library/utils/R/widgets.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

select.list <- function(list, preselect=NULL, multiple=FALSE, title=NULL)
{
    if(!interactive()) stop("select.list() cannot be used non-interactively")
    if(!is.null(title) && (!is.character(title) || length(title) != 1))
        stop("'title' must be NULL or a length-1 character vector")
    if(.Platform$OS.type == "windows" | .Platform$GUI == "AQUA")
        return(.Internal(select.list(list, preselect, multiple, title)))
    ## simple text-based alternatives.
    if(!multiple) {
        res <- menu(list, , title)
        if(res < 1L || res > length(list)) return("")
        else return(list[res])
    } else {
        nc <- length(list)
        cat(title, "\n")
        def <- if(is.null(preselect)) rep(FALSE, nc)
        else list %in% preselect
        op <- paste(format(seq_len(nc)), ": ",
                    ifelse(def, "+", " "), " ", list, sep="")
        if(nc > 10L) {
            fop <- format(op)
            nw <- nchar(fop[1L], "w") + 2
            ncol <- getOption("width") %/% nw
            if(ncol > 1L)
                op <- paste(fop, c(rep("  ", ncol - 1), "\n"),
                            sep ="", collapse="")
            cat("", op, sep="\n")
        } else cat("", op, "", sep="\n")
	cat(gettext("Enter zero or more numbers separated by space\n"))
	repeat {
            res <- tryCatch(scan("", what=0, quiet=TRUE, nlines=1),
                            error = identity)
	    if(!inherits(res, "error")) break
	    cat(gettext("Invalid input, please try again\n"))
	}
        if(!length(res) || (length(res) == 1L && !res[1L])) return(character(0L))
        res <- sort(res[1 <= res && res <= nc])
        return(list[res])
    }
}

flush.console <- function()
    if (.Platform$GUI == "AQUA" || .Platform$OS.type == "windows")
        .Internal(flush.console())
#  File src/library/utils/R/write.table.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

write.table <-
function (x, file = "", append = FALSE, quote = TRUE, sep = " ",
          eol = "\n", na = "NA", dec = ".", row.names = TRUE,
          col.names = TRUE, qmethod = c("escape", "double"))
{
    qmethod <- match.arg(qmethod)
    if(is.logical(quote) && (length(quote) != 1L || is.na(quote)))
        stop("'quote' must be 'TRUE', 'FALSE' or numeric")
    ## quote column names unless quote == FALSE (see help).
    quoteC <- if(is.logical(quote)) quote else TRUE
    qset <- is.logical(quote) && quote

    if(!is.data.frame(x) && !is.matrix(x)) x <- data.frame(x)

    makeRownames <- isTRUE(row.names)
    ## need col names if col.names is TRUE or NA
    makeColnames <- is.logical(col.names) && !identical(FALSE, col.names)
    if(is.matrix(x)) {
        ## fix up dimnames as as.data.frame would
        p <- ncol(x)
        d <- dimnames(x)
        if(is.null(d)) d <- list(NULL, NULL)
        if(is.null(d[[1L]]) && makeRownames) d[[1L]] <- seq_len(nrow(x))
        if(is.null(d[[2L]]) && makeColnames && p > 0L)
            d[[2L]] <- paste("V", 1L:p, sep="")
        if(qset)
            quote <- if(is.character(x)) seq_len(p) else numeric(0L)
    } else { ## data.frame
        if(qset)
            quote <- if(length(x))
                which(unlist(lapply(x, function(x)
                                    is.character(x) || is.factor(x))))
            else numeric(0L)
        ## fix up embedded matrix columns into separate cols:
        if(any(sapply(x, function(z) length(dim(z)) == 2 && dim(z)[2L] > 1))) {
            c1 <- names(x)
	    x <- as.matrix(x, rownames.force = makeRownames)
	    d <- dimnames(x)
	    if(qset) {
		ord <- match(c1, d[[2L]], 0L)
		quote <- ord[quote]; quote <- quote[quote > 0L]
	    }
        }
        else
            d <- list(if(makeRownames) row.names(x),
                      if(makeColnames) names(x))
        p <- ncol(x)
    }
    nocols <- p == 0L

    if(is.logical(quote)) # must be false
	quote <- NULL
    else if(is.numeric(quote)) {
	if(any(quote < 1L | quote > p))
	    stop("invalid numbers in 'quote'")
    } else
	stop("invalid 'quote' specification")

    rn <- FALSE
    rnames <- NULL
    if(is.logical(row.names)) {
	if(row.names) {rnames <- as.character(d[[1L]]); rn <- TRUE}
    } else {
	rnames <- as.character(row.names)
        rn <- TRUE
	if(length(rnames) != nrow(x))
            stop("invalid 'row.names' specification")
    }
    if(!is.null(quote) && rn) # quote the row names
	quote <- c(0, quote)

    if(is.logical(col.names)) {
        if(!rn && is.na(col.names))
            stop("col.names = NA makes no sense when row.names = FALSE")
        col.names <- if(is.na(col.names) && rn) c("", d[[2L]])
        else if(col.names) d[[2L]] else NULL
    } else {
	col.names <- as.character(col.names)
	if(length(col.names) != p)
	    stop("invalid 'col.names' specification")
    }

    if(file == "")
        file <- stdout()
    else if(is.character(file)) {
        file <- file(file, ifelse(append, "a", "w"))
        on.exit(close(file))
    } else if(!isOpen(file, "w")) {
        open(file, "w")
        on.exit(close(file))
    }
    if(!inherits(file, "connection"))
        stop("'file' must be a character string or connection")

    qstring <-                          # quoted embedded quote string
        switch(qmethod,
               "escape" = '\\\\"',
               "double" = '""')
    if(!is.null(col.names)) {
	if(append)
	    warning("appending column names to file")
	if(quoteC)
	    col.names <- paste("\"", gsub('"', qstring, col.names),
                               "\"", sep = "")
        writeLines(paste(col.names, collapse = sep), file, sep = eol)
    }

    if (nrow(x) == 0L) return(invisible())
    if (nocols && !rn) return(cat(rep.int(eol, NROW(x)), file=file, sep=""))

    ## convert list matrices to character - maybe not much use?
    if(is.matrix(x) && !is.atomic(x)) mode(x) <- "character"
    if(is.data.frame(x)) {
        ## convert columns we can't handle in C code
        x[] <- lapply(x, function(z) {
            if(is.object(z) && !is.factor(z)) as.character(z) else z
        })
    }

    .Internal(write.table(x, file, nrow(x), p, rnames, sep, eol, na, dec,
                          as.integer(quote), qmethod != "double"))
}

write.csv <- function(...)
{
    Call <- match.call(expand.dots = TRUE)
    for(argname in c("col.names", "sep", "dec", "qmethod"))
        if(!is.null(Call[[argname]]))
            warning(gettextf("attempt to set '%s' ignored", argname),
                    domain = NA)
    rn <- eval.parent(Call$row.names)
    Call$col.names <- if(is.logical(rn) && !rn) TRUE else NA
    Call$sep <- ","
    Call$dec <- "."
    Call$qmethod <- "double"
    Call[[1L]] <- as.name("write.table")
    eval.parent(Call)
}

write.csv2 <- function(...)
{
    Call <- match.call(expand.dots = TRUE)
    for(argname in c("col.names", "sep", "dec", "qmethod"))
        if(!is.null(Call[[argname]]))
            warning(gettextf("attempt to set '%s' ignored", argname),
                    domain = NA)
    rn <- eval.parent(Call$row.names)
    Call$col.names <- if(is.logical(rn) && !rn) TRUE else NA
    Call$sep <- ";"
    Call$dec <- ","
    Call$qmethod <- "double"
    Call[[1L]] <- as.name("write.table")
    eval.parent(Call)
}
#  File src/library/utils/R/zip.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

zip.file.extract <- function(file, zipname = "R.zip",
			     unzip = getOption("unzip"), dir = tempdir())
{
    path <- dirname(file)
    topic <- basename(file)
    if(file.exists(file.path(path, zipname))) {
        if(!is.character(unzip) || length(unzip) != 1L)
            stop("'unzip' must be a single character string")
        if(!nzchar(unzip)) unzip <- "internal"
        if(unzip != "internal") {
            cmd <- paste(unzip, "-oq", shQuote(file.path(path, zipname)),
                         topic, " -d ", dir)
            ## there is an unzip clone about that does not respect -q, so
            ## use redirection here.
            res <- if(.Platform$OS.type == "windows")
                system(cmd, invisible = TRUE) else system(paste(cmd, "> /dev/null"))
            if(!res) file <- file.path(dir, topic)
        } else {
            rc <- .Internal(unzip(file.path(path, zipname), topic, dir,
                                  FALSE, TRUE, FALSE))
            if (rc == 0L)
                file <- file.path(dir, topic)
        }
    }
    file
}

unzip <-
    function(zipfile, files = NULL, list = FALSE, overwrite = TRUE,
             junkpaths = FALSE, exdir = ".")
{
    if(!list && !missing(exdir))
        dir.create(exdir, showWarnings = FALSE, recursive = TRUE)
    res <- .Internal(unzip(zipfile, files, exdir, list, overwrite, junkpaths))
    if(list) {
        dates <- as.POSIXct(res[[3]], "%Y-%m-%d %H:%M",  tz="UTC")
        data.frame(Name = res[[1]], Length = res[[2]], Date = dates)
    } else invisible(attr(res, "extracted"))
}
#  File src/library/utils/R/zzz.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
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
    ## Set default options() related to functionality in 'utils' pkg
    op <- options()
    op.utils <-
	list(help.try.all.packages = FALSE,
	     internet.info = 2,
	     pkgType = .Platform$pkgType,
	     str = list(strict.width = "no", digits.d = 3, vec.len = 4),
	     demo.ask = "default", example.ask = "default",
	     HTTPUserAgent = defaultUserAgent(),
	     menu.graphics = TRUE)
    extra <-
        if(.Platform$OS.type == "windows") {
            list(mailer = "none",
                 unzip = "internal",
                 editor = if(length(grep("Rgui", commandArgs(), TRUE))) "internal" else "notepad",
                 repos = c(CRAN="@CRAN@",
                 CRANextra="http://www.stats.ox.ac.uk/pub/RWin")
                 )
        } else
            list(mailer = "mailx",
                 unzip = as.vector(Sys.getenv("R_UNZIPCMD")),
                 editor = as.vector(Sys.getenv("EDITOR")),
                 repos = c(CRAN="@CRAN@"))
    op.utils <- c(op.utils, extra)
    toset <- !(names(op.utils) %in% names(op))
    if(any(toset)) options(op.utils[toset])
}
