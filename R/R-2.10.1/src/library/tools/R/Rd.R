#  File src/library/tools/R/Rd.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

### * Rd_info

Rd_info <-
function(file, encoding = "unknown")
{
    ## <FIXME>
    ## This used to work only for a given Rd file.
    ## We now also allow for passing a parsed Rd object.
    ## Is the Rd file case still needed?

    if(inherits(file, "Rd")) {
        Rd <- file
        description <- attr(attr(Rd, "srcref"), "srcfile")$filename
    } else {
        if(is.character(file)) {
            file <- file(file)
            on.exit(close(file))
        }
        if(!inherits(file, "connection"))
            stop("argument 'file' must be a character string or connection")
        description <- summary(file)$description
        Rd <- prepare_Rd(file, encoding = encoding,
                         defines = .Platform$OS.type)
    }

    aliases <- .Rd_get_metadata(Rd, "alias")
    concepts <- .Rd_get_metadata(Rd, "concept")
    keywords <- .Rd_get_metadata(Rd, "keyword")

    ## Could be none or more than one ... argh.
    Rd_type <- .Rd_get_doc_type(Rd)
    encoding <- c(.Rd_get_metadata(Rd, "encoding"), "")[1L]

    Rd_name <- .Rd_get_name(Rd)
    if(!length(Rd_name)) {
        msg <-
            c(gettextf("missing/empty \\name field in '%s'",
                       description),
              gettext("Rd files must have a non-empty \\name."),
              gettext("See chapter 'Writing R documentation' in manual 'Writing R Extensions'."))
        stop(paste(msg, collapse = "\n"), domain = NA)
    }

    Rd_title <- .Rd_get_title(Rd)
    if(!length(Rd_title)) {
        msg <-
            c(gettextf("missing/empty \\title field in '%s'",
                       description),
              gettext("Rd files must have a non-empty \\title."),
              gettext("See chapter 'Writing R documentation' in manual 'Writing R Extensions'."))
        stop(paste(msg, collapse = "\n"), domain = NA)
    }

    list(name = Rd_name, type = Rd_type, title = Rd_title,
         aliases = aliases, concepts = concepts, keywords = keywords,
         encoding = encoding)
}

### * Rd_contents

Rd_contents <-
function(db)
{
    ## Compute contents db from Rd db.
    ## NB: Encoding is the encoding declared in the file, not
    ## that after parsing.
    if(!length(db)) {
        out <- data.frame(File = character(),
                          Name = character(),
                          Type = character(),
                          Title = character(),
                          Encoding = character(),
                          stringsAsFactors = FALSE)
        out$Aliases <- list()
        out$Concepts <- list()
        out$Keywords <- list()
        return(out)
    }

    entries <- c("Name", "Type", "Title", "Aliases", "Concepts",
                 "Keywords", "Encoding")
    contents <- vector("list", length(db) * length(entries))
    dim(contents) <- c(length(db), length(entries))
    for(i in seq_along(db)) {
        contents[i, ] <- Rd_info(db[[i]])
    }
    colnames(contents) <- entries

    title <- .Rd_format_title(unlist(contents[ , "Title"]))
    out <- data.frame(File = basename(names(db)),
                      Name = unlist(contents[ , "Name"]),
                      Type = unlist(contents[ , "Type"]),
                      Title = title,
                      Encoding = unlist(contents[ , "Encoding"]),
                      row.names = NULL, # avoid trying to compute row
                                        # names
                      stringsAsFactors = FALSE)
    out$Aliases <- contents[ , "Aliases"]
    out$Concepts <- contents[ , "Concepts"]
    out$Keywords <- contents[ , "Keywords"]
    out
}

### * .write_Rd_contents_as_RDS

.write_Rd_contents_as_RDS <-
function(contents, outFile)
{
    ## Save Rd contents db to @file{outFile}.

    ## <NOTE>
    ## To deal with possible changes in the format of the contents db
    ## in the future, use a version attribute and/or a formal class.
    .saveRDS(contents, file = outFile, compress = TRUE)
    ## </NOTE>
}

### * .write_Rd_contents_as_DCF

if(FALSE) {
.write_Rd_contents_as_DCF <-
function(contents, packageName, outFile)
{
    ## Write a @file{CONTENTS} DCF file from an Rd contents db.
    ## Note that these files currently have @samp{URL:} entries which
    ## contain the package name, whereas @code{Rd_contents()} works on
    ## collections of Rd files which do not necessarily all come from
    ## the same package ...

    ## If the contents is 'empty', return immediately.  (Otherwise,
    ## e.g. URLs would not be right ...)
    if(!NROW(contents)) return()

    ## <NOTE>
    ## This has 'html' hard-wired.
    ## Note that slashes etc. should be fine for URLs.
    URLs <- paste("../../../library/", packageName, "/html/",
                  file_path_sans_ext(contents[ , "File"]),
                  ".html",
                  sep = "")
    ## </NOTE>

    if(is.data.frame(contents))
        contents <-
            cbind(contents$Name,
                  sapply(contents$Aliases, paste, collapse = " "),
                  sapply(contents$Keywords, paste, collapse = " "),
                  contents$Title)
    else
        contents <-
            contents[, c("Name", "Aliases", "Keywords", "Title"),
                     drop = FALSE]

    cat(paste(c("Entry:", "Aliases:", "Keywords:", "Description:",
                "URL:"),
              t(cbind(contents, URLs))),
        sep = c("\n", "\n", "\n", "\n", "\n\n"),
        file = outFile)
}
}

### * .build_Rd_index

.build_Rd_index <-
function(contents, type = NULL)
{
    ## Build an Rd 'index' containing Rd "names" (see below) and titles,
    ## maybe subscripted according to the Rd type (\docType).

    keywords <- contents[ , "Keywords"]

    if(!is.null(type)) {
        idx <- contents[ , "Type"] %in% type
        ## Argh.  Ideally we only want to subscript according to
        ## \docType.  Maybe for 2.0 ...
        if(type == "data")
            idx <- idx | keywords == "datasets"
        ## (Note: we really only want the Rd objects which have
        ## 'datasets' as their *only* keyword.)
        contents <- contents[idx, , drop = FALSE]
        keywords <- keywords[idx]
    }

    ## Drop all Rd objects marked as 'internal' from the index.
    idx <- is.na(sapply(keywords, function(x) match("internal", x)))

    index <- contents[idx, c("Name", "Title"), drop = FALSE]
    if(nrow(index)) {
        ## If a \name is not a valid \alias, replace it by the first
        ## alias.
        aliases <- contents[idx, "Aliases"]
        bad <- which(!mapply("%in%", index[, 1L], aliases))
        if(any(bad)) {
            ## was [[, but that applies to lists not char vectors
            tmp <- sapply(aliases[bad], "[", 1L)
            tmp[is.na(tmp)] <- ""
            index[bad, 1L] <- tmp
        }
        ## and sort it by name
        index <- index[sort.list(index[, 1L]), ]
    }
    index
}

### * Rdindex

Rdindex <-
function(RdFiles, outFile = "", type = NULL,
         width = 0.9 * getOption("width"), indent = NULL)
{
    ## Create @file{INDEX} or @file{data/00Index} style files from Rd
    ## files.
    ##
    ## R version of defunct @code{R CMD Rdindex} (now removed).
    ##
    ## called from R CMD build

    if((length(RdFiles) == 1L) && file_test("-d", RdFiles)) {
        ## Compatibility code for the former @code{R CMD Rdindex}
        ## interface.
        docsDir <- RdFiles
        if(file_test("-d", file.path(docsDir, "man")))
            docsDir <- file.path(docsDir, "man")
        RdFiles <- list_files_with_type(docsDir, "docs")
    }

    if(outFile == "")
        outFile <- stdout()
    else if(is.character(outFile)) {
        outFile <- file(outFile, "w")
        on.exit(close(outFile))
    }
    if(!inherits(outFile, "connection"))
        stop("argument 'outFile' must be a character string or connection")

    db <- .build_Rd_db(files = RdFiles)
    index <- .build_Rd_index(Rd_contents(db), type = type)
    writeLines(formatDL(index, width = width, indent = indent), outFile)
}

### * Rd_db

Rd_db <-
function(package, dir, lib.loc = NULL)
{
    ## Build an Rd 'data base' from an installed package or the unpacked
    ## package sources as a list containing the parsed Rd objects.

    ## <NOTE>
    ## We actually also process platform conditionals.
    ## If this was to be changed, we could also need to arrange that Rd
    ## objects in *all* platform specific subdirectories are included.
    ## </NOTE>

    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- .find.package(package, lib.loc)
        ## Using package installed in @code{dir} ...
        docs_dir <- file.path(dir, "man")
        ## For an installed package, we might have
        ##
        ## 1) pre-2.10.0-style  man/package.Rd.gz
        ## file with suitable concatenated Rd sources,
        ##
        ## 2) help/package.rd[bx]
        ## with a DB of the parsed (and platform processed, see
        ## above) Rd objects.
        db_file <- file.path(dir, "help", package)
        if(file_test("-f", paste(db_file, "rdx", sep="."))) {
            db <- fetchRdDB(db_file)
            pathfile <- file.path(dir, "help", "paths.rds")
            if(file.exists(pathfile)) names(db) <- .readRDS(pathfile)
            return(db)
        }
        db_file <- file.path(docs_dir, sprintf("%s.Rd.gz", package))
        if(file_test("-f", db_file)) {
            lines <- .read_Rd_lines_quietly(db_file)
            eof_pos <-
                grep("^\\\\eof$", lines, perl = TRUE, useBytes = TRUE)
            db <- split(lines[-eof_pos],
                        rep(seq_along(eof_pos),
                            times = diff(c(0, eof_pos)))[-eof_pos])
        } else return(structure(list(), names = character()))

        ## NB: we only get here for pre-2.10.0 installs

        ## If this was installed using a recent enough version of R CMD
        ## INSTALL, information on source file names is available, and
        ## we use it for the names of the Rd db.  Otherwise, remove the
        ## artificial names attribute.
        paths <- as.character(sapply(db, "[", 1L))
        names(db) <-
            if(length(paths)
               && all(grepl("^% --- Source file: (.+) ---$", paths)))
                sub("^% --- Source file: (.+) ---$", "\\1", paths)
            else
                NULL
        ## Determine package encoding.
        encoding <- .get_package_metadata(dir, TRUE)["Encoding"]
        if(is.na(encoding)) encoding <- "unknown"
        db <- suppressWarnings(lapply(db,
                                      prepare_Rd_from_Rd_lines,
                                      encoding = encoding,
                                      defines = .Platform$OS.type,
                                      stages = "install"))
    }
    else {
        if(missing(dir))
            stop("you must specify 'package' or 'dir'")
        ## Using sources from directory @code{dir} ...
        if(!file_test("-d", dir))
            stop(gettextf("directory '%s' does not exist", dir),
                 domain = NA)
        else
            dir <- file_path_as_absolute(dir)
        db <- .build_Rd_db(dir)
    }

    db

}

prepare_Rd_from_Rd_lines <-
function(x, ...)
{
    con <- textConnection(x, "rt")
    on.exit(close(con))
    prepare_Rd(con, ...)
}

.build_Rd_db <-
function(dir = NULL, files = NULL, encoding = "unknown", db_file = NULL)
{
    if(!is.null(dir)) {
        man_dir <- file.path(dir, "man")
        if(!file_test("-d", man_dir))
            return(structure(list(), names = character()))
        if(is.null(files))
            files <- list_files_with_type(man_dir, "docs")
        encoding <- .get_package_metadata(dir, FALSE)["Encoding"]
        if(is.na(encoding)) encoding <- "unknown"
    } else if(is.null(files))
        stop("you must specify 'dir' or 'files'")

    .fetch_Rd_object <- function(f) {
        ## This calls parse_Rd
        Rd <- prepare_Rd(f, encoding = encoding,
                         defines = .Platform$OS.type,
                         stages = "install", warningCalls = FALSE)
        structure(Rd, prepared = 3L)
    }

    if(!is.null(db_file) && file_test("-f", db_file)) {
        ## message("updating database of parsed Rd files")
        db <- fetchRdDB(sub("\\.rdx$", "", db_file))
        db_names <- names(db) <-
            .readRDS(file.path(dirname(db_file), "paths.rds"))
        ## Files in the db in need of updating:
        ind <- (files %in% db_names) & file_test("-nt", files, db_file)
        if(any(ind))
            db[files[ind]] <- lapply(files[ind], .fetch_Rd_object)
        ## Files not in the db:
        ind <- !(files %in% db_names)
        if(any(ind)) {
            db1 <- lapply(files[ind], .fetch_Rd_object)
            names(db1) <- files[ind]
            db <- c(db, db1)
        }
        ## Db elements missing from files:
        ind <- !(db_names %in% files)
        if(any(ind))
            db <- db[!ind]
    } else {
        ## message("building database of parsed Rd files")
        db <- lapply(files, .fetch_Rd_object)
        names(db) <- files
    }

    db
}

### * Rd_aliases

## Called from undoc and .check_Rd_xrefs
Rd_aliases <-
function(package, dir, lib.loc = NULL)
{
    ## Get the Rd aliases (topics) from an installed package or the
    ## unpacked package sources.

    if(!missing(package)) {
        dir <- .find.package(package, lib.loc)
        rds <- file.path(dir, "Meta", "Rd.rds")
        if(file_test("-f", rds)) {
            aliases <- .readRDS(rds)$Aliases
            if(length(aliases)) sort(unlist(aliases)) else character()
        } else
            character()
        ## <NOTE>
        ## Alternatively, we could get the aliases from the help index
        ## (and in fact, earlier versions of this code, then part of
        ## undoc(), did so), along the lines of
        ## <CODE>
        ##   help_index <- file.path(dir, "help", "AnIndex")
        ##   all_doc_topics <- if(!file_test("-f", help_index))
        ##       character()
        ##   else
        ##       sort(scan(file = helpIndex, what = list("", ""),
        ##                 sep = "\t", quote = "", quiet = TRUE,
        ##                 na.strings = character())[[1L]])
        ## </CODE>
        ## This gets all topics the same way as index.search() would
        ## find individual ones.
        ## </NOTE>
    }
    else {
        if(file_test("-d", file.path(dir, "man"))) {
            db <- Rd_db(dir = dir)
            aliases <- lapply(db, .Rd_get_metadata, "alias")
            if(length(aliases))
                sort(unique(unlist(aliases, use.names = FALSE)))
            else character()
        }
        else
            character()
    }
}

### .build_Rd_xref_db

.build_Rd_xref_db <-
function(package, dir, lib.loc = NULL)
{
    db <- if(!missing(package))
        Rd_db(package, lib.loc = lib.loc)
    else
        Rd_db(dir = dir)
    lapply(db, .Rd_get_xrefs)
}

### * .Rd_get_metadata

.Rd_get_metadata <-
function(x, kind)
{
    x <- x[RdTags(x) == sprintf("\\%s", kind)]
    if(!length(x))
        character()
    else
        unique(.strip_whitespace(sapply(x, as.character)))
}

### * .Rd_get_section

.Rd_get_section <-
function(x, which, predefined = TRUE)
{
    if(predefined)
        x <- x[RdTags(x) == paste("\\", which, sep = "")]
    else {
        ## User-defined sections are parsed into lists of length 2, with
        ## the elements the title and the body, respectively.
        ## <FIXME>
        ## Section titles should really contain no Rd markup, but might
        ## they contain Rd comments?
        ## </FIXME>
        x <- x[RdTags(x) == "\\section"]
        if(length(x)) {
            ind <- sapply(x, function(e) .Rd_deparse(e[[1L]])) == which
            x <- lapply(x[ind], `[[`, 2L)
        }
    }
    if(!length(x)) x else structure(x[[1L]], class = "Rd")
}

### * .Rd_deparse

.Rd_deparse <-
function(x, tag = TRUE)
{
    ## <NOTE>
    ## This should eventually get an option controlling whether to
    ## escape Rd special characters as needed (thus providing valid Rd)
    ## or not.
    ## It might also be useful to have an option for dropping comments.
    ## </NOTE>
    if(!tag)
        attr(x, "Rd_tag") <- "Rd"
    paste(as.character.Rd(x), collapse = "")
}

### * .Rd_drop_comments

.Rd_drop_comments <-
function(x)
    .Rd_drop_nodes_with_tags(x, "COMMENT")

### * .Rd_drop_nodes_with_tags

.Rd_drop_nodes_with_tags <-
function(x, tags)
{
    recurse <- function(e) {
        if(is.list(e))
            structure(lapply(e[is.na(match(RdTags(e), tags))], recurse),
                      Rd_tag = attr(e, "Rd_tag"))
        else
            e
    }
    recurse(x)
}

### * .Rd_get_argument_names

.Rd_get_argument_names <-
function(x)
{
    x <- .Rd_get_section(x, "arguments")
    if(!length(x)) return(character())
    txt <- .Rd_get_item_tags(x)
    txt <- unlist(strsplit(txt, ", *"))
    txt <- gsub("\\\\l?dots", "...", txt)
    txt <- gsub("\\\\_", "_", txt)
    .strip_whitespace(txt)
}

### * .Rd_get_item_tags

.Rd_get_item_tags <-
function(x)
{
    ## Extract two-arg \item tags at top level ... non-recursive.

    x <- x[RdTags(x) == "\\item"]
    out <- lapply(x[sapply(x, length) == 2L],
                  function(e) .Rd_deparse(e[[1L]]))
    as.character(unlist(out))
}

### * .Rd_get_example_code

.Rd_get_example_code <-
function(x)
{
    x <- .Rd_get_section(x, "examples")
    if(!length(x)) return(character())

    ## Need to remove everything inside \dontrun (and drop comments),
    ## and "undefine" \dontshow and \testonly (which is achieved by
    ## changing the Rd tag to "Rd").

    ## <FIXME>
    ## Remove eventually.
    x <- .Rd_drop_comments(x)
    ## </FIXME>

    recurse <- function(e) {
        if(!is.null(tag <- attr(e, "Rd_tag"))
           && tag %in% c("\\dontshow", "\\testonly"))
            attr(e, "Rd_tag") <- "Rd"
        if(is.list(e)) {
            structure(lapply(e[is.na(match(RdTags(e), "\\dontrun"))],
                             recurse),
                      Rd_tag = attr(e, "Rd_tag"))
        }
        else e
    }

    .Rd_deparse(recurse(x), tag = FALSE)
}

### * .Rd_get_doc_type

.Rd_get_doc_type <-
function(x)
{
    c(attr(x, "meta")$docType, .Rd_get_metadata(x, "docType"), "")[1L]
}

### * .Rd_get_name

.Rd_get_name <-
function(x)
{
    x <- .Rd_get_section(x, "name")
    ## The name should really be plain text, so as.character() should be
    ## fine as well ...
    if(length(x))
        .strip_whitespace(.Rd_deparse(x, tag = FALSE))
    else
        character()
}

### * .Rd_get_title

.Rd_get_title <-
function(x)
{
    x <- .Rd_get_section(x, "title")

    if(length(x)) {
        ## <FIXME>
        ## Remove eventually.
        x <- .Rd_drop_comments(x)
        ## </FIXME>
        .strip_whitespace(.Rd_deparse(x, tag = FALSE))
    }
    else
        character()
}

### * .Rd_get_xrefs

.Rd_get_xrefs <-
function(x)
{
    out <- matrix(character(), nrow = 0L, ncol = 2L)
    recurse <- function(e) {
        if(identical(attr(e, "Rd_tag"), "\\link")) {
            val <- if(length(e)) { # mvbutils has empty links
                arg <- as.character(e[[1L]])
                opt <- attr(e, "Rd_option")
                c(arg, if(is.null(opt)) "" else as.character(opt))
            } else c("", "")
            out <<- rbind(out, val)
        }
        if(is.list(e)) lapply(e, recurse)
    }
    lapply(x, recurse)
    dimnames(out) <- list(NULL, c("Target", "Anchor"))
    out
}

### * .Rd_get_names_from_Rd_db

.Rd_get_names_from_Rd_db <-
function(db)
{
    Rd_names <- lapply(db, .Rd_get_name)
    ## If the Rd db was obtained from an installed package, we know that
    ## all Rd objects must have a \name entry---otherwise, Rd_info() and
    ## hence installing the package Rd contents db would have failed.
    ## For Rd dbs created from a package source directory, we now add
    ## the Rd file paths as the names attribute, so that we can point to
    ## the files with missing \name entries.
    idx <- as.integer(sapply(Rd_names, length)) == 0L
    if(any(idx)) {
        Rd_paths <- names(db)
        if(is.null(Rd_paths)) {
            ## This should not happen.
            ## We cannot refer to the bad Rd objects because we do not
            ## know their names, and have no idea which file they came
            ## from ...)
            stop("cannot deal with Rd objects with missing/empty names")
        }
        else {
            stop(paste(gettext("missing/empty \\name field in Rd file(s)"),
                       paste(" ", Rd_paths[idx], collapse = "\n"),
                       sep = "\n"),
                 call. = FALSE, domain = NA)
        }
    }
    unlist(Rd_names)
}

### * .Rd_format_title

.Rd_format_title <-
function(x)
{
    ## Although R-exts says about the Rd title slot that
    ## <QUOTE>
    ##   This should be capitalized, not end in a period, and not use
    ##   any markup (which would cause problems for hypertext search).
    ## </QUOTE>
    ## some Rd files have LaTeX-style markup, including
    ## * LaTeX-style single and double quotation
    ## * Medium and punctuation dashes
    ## * Escaped ampersand.
    ## Hence we try getting rid of these ...
    x <- gsub("(``|'')", "\"", x)
    x <- gsub("`", "'", x)
    x <- gsub("([[:alnum:]])--([[:alnum:]])", "\\1-\\2", x)
    x <- gsub("\\\\&", "&", x)
    x <- gsub("---", "--", x)
    ## Also remove leading and trailing whitespace.
    .strip_whitespace(x)
}


### * fetchRdDB

fetchRdDB <- function (filebase, key = NULL)
{
    data <- paste(filebase, "rdb", sep = ".")
    v <- .readRDS(paste(filebase, "rdx", sep = "."))$variables
    if(length(key)) {
        if(! key %in% names(v))
            stop(gettextf("No help on %s found in RdDB %s",
                          sQuote(key), sQuote(filebase)),
                 domain = NA)
        lazyLoadDBfetch(v[key][[1]], data, TRUE, function(n){})
    } else {
        res <- v # a list of the right names
        for(i in seq_along(res))
            res[[i]] <- lazyLoadDBfetch(v[i][[1]], data, TRUE, function(n){})
        invisible(res)
    }
}


### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
