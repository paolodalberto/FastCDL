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
