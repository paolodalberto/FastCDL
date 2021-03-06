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
