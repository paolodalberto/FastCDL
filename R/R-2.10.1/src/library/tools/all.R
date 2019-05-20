#  File src/library/tools/R/QC.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

## R CMD check uses
## .find_charset
## .check_namespace
## .check_package_depends
## .check_demo_index
## .check_vignette_index
## .check_package_subdirs
## .check_citation
## .check_package_ASCII_code
## .check_package_code_syntax
## .check_packages_used
## .checkS3methods
## .checkReplaceFuns
## .checkFF
## .check_package_code_shlib
## .check_code_usage_in_package
## .check_T_and_F
## .check_dotInternal
## .check_package_parseRd
## .check_Rd_xrefs
## undoc
## codoc
## codocData
## codocClass
## checkDocFiles
## checkDocStyle
## .check_package_datasets
## .check_make_vars
## .createExdotR (testing.R)
## .runPackageTestsR (testing.R)
## .get_LaTeX_errors_from_log_file

## R CMD build uses .check_package_subdirs

## utility for whether Rd sources are available.
.haveRds <- function(dir)
{
    ## either source package or pre-2.10.0 installed package
    if (file_test("-d", file.path(dir, "man"))) return(TRUE)
    file.exists((file.path(dir, "help", "paths.rds")))
}

### * undoc/F/out

undoc <-
function(package, dir, lib.loc = NULL)
{
    ## Argument handling.
    ## <NOTE>
    ## Earlier versions used to give an error if there were no Rd
    ## objects.  This is not right: if there is code or data but no
    ## documentation, everything is undocumented ...
    ## </NOTE>
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- .find.package(package, lib.loc)
        ## Using package installed in @code{dir} ...
        is_base <- package == "base"

        all_doc_topics <- Rd_aliases(package, lib.loc = dirname(dir))

        ## Load package into code_env.
        if(!is_base)
            .load_package_quietly(package, lib.loc)
        code_env <- .package_env(package)

        code_objs <- ls(envir = code_env, all.names = TRUE)
        pkgname <- package
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
        pkgname <- basename(dir)
        is_base <- pkgname == "base"

        all_doc_topics <- Rd_aliases(dir = dir)

        code_env <- new.env()
        code_dir <- file.path(dir, "R")
        if(file_test("-d", code_dir)) {
            dfile <- file.path(dir, "DESCRIPTION")
            meta <- if(file_test("-f", dfile))
                .read_description(dfile)
            else
                character()
            .source_assignments_in_code_dir(code_dir, code_env, meta)
            sys_data_file <- file.path(code_dir, "sysdata.rda")
            if(file_test("-f", sys_data_file))
                load(sys_data_file, code_env)
        }

        code_objs <- ls(envir = code_env, all.names = TRUE)

        ## Does the package have a NAMESPACE file?  Note that when
        ## working on the sources we (currently?) cannot deal with the
        ## (experimental) alternative way of specifying the namespace.
        if(file.exists(file.path(dir, "NAMESPACE"))) {
            nsInfo <- parseNamespaceFile(basename(dir), dirname(dir))
            ## Look only at exported objects (and not declared S3
            ## methods).
            OK <- intersect(code_objs, nsInfo$exports)
            for(p in nsInfo$exportPatterns)
                OK <- c(OK, grep(p, code_objs, value = TRUE))
            code_objs <- unique(OK)
        }
    }

    ## Find the data sets to work on.
    data_dir <- file.path(dir, "data")
    data_objs <- if(file_test("-d", data_dir))
        names(unlist(.try_quietly(list_data_in_pkg(dataDir = data_dir)),
                     use.names = FALSE))
    else
        character()

    ## There was a time when packages contained code or data (or both).
    ## But not anymore ...
    if(!missing(package)
       && (!length(code_objs))
       && (!length(data_objs))
       && getOption("verbose"))
        message("neither code nor data objects found")

    if(!is_base) {
        ## Code objects in add-on packages with names starting with a
        ## dot are considered 'internal' (not user-level) by
        ## convention.
        code_objs <- grep("^[^.].*", code_objs, value = TRUE)
        ## Note that this also allows us to get rid of S4 meta objects
        ## (with names starting with '.__C__' or '.__M__'; well, as long
        ## as there are none in base).

        ## Need to do something about S4 generic functions 'created' by
        ## setGeneric() or setMethod() on 'ordinary' functions.
        ## The test below exempts objects that are generic functions
        ## which are 'derived', either by importing from another
        ## package or from a default method.
        ## In the long run we need dynamic documentation.
        if(.isMethodsDispatchOn()) {
            code_objs <-
                Filter(function(f) {
                           fdef <- get(f, envir = code_env)
                           if(methods::is(fdef, "genericFunction"))
                               fdef@package == pkgname
                           else
                               TRUE
                       },
                       code_objs)
        }

        ## Allow group generics to be undocumented other than in base.
        ## In particular, those from methods partially duplicate base
        ## and are documented in base's groupGenerics.Rd.
        code_objs <-
	    code_objs %w/o% c("Arith", "Compare", "Complex", "Logic",
			      "Math", "Math2", "Ops", "Summary")
    }

    undoc_things <-
        list("code objects" =
             unique(code_objs %w/o% all_doc_topics),
             "data sets" =
             unique(data_objs %w/o% all_doc_topics))

    if(.isMethodsDispatchOn()) {
        ## Undocumented S4 classes?
        S4_classes <- methods::getClasses(code_env)
        ## <NOTE>
        ## There is no point in worrying about exportClasses directives
        ## in a NAMESPACE file when working on a package source dir, as
        ## we only source the assignments, and hence do not get any
        ## S4 classes or methods.
        ## </NOTE>
        ## The bad ones:
        S4_classes <-
            S4_classes[!sapply(S4_classes,
                               function(u) utils:::topicName("class", u))
                       %in% all_doc_topics]
        undoc_things <-
            c(undoc_things, list("S4 classes" = unique(S4_classes)))
    }

    if(.isMethodsDispatchOn()) {
        ## Undocumented S4 methods?
        ## <NOTE>
        ## There is no point in worrying about exportMethods directives
        ## in a NAMESPACE file when working on a package source dir, as
        ## we only source the assignments, and hence do not get any
        ## S4 classes or methods.
        ## </NOTE>
        .make_S4_method_siglist <- function(g) {
            mlist <- .get_S4_methods_list(g, code_env)
            sigs <- .make_siglist(mlist) #  s/#/,/g
            if(length(sigs))
                paste(g, ",", sigs, sep = "")
            else
                character()
        }
        S4_methods <- lapply(get_S4_generics_with_methods(code_env),
                             .make_S4_method_siglist)
        S4_methods <- as.character(unlist(S4_methods, use.names = FALSE))

        ## The bad ones:
        S4_methods <-
            S4_methods[!sapply(S4_methods,
                               function(u)
                               utils:::topicName("method", u))
                       %in% all_doc_topics]
        undoc_things <-
            c(undoc_things,
              list("S4 methods" =
                   unique(sub("([^,]*),(.*)",
                              "generic '\\1' and siglist '\\2'",
                              S4_methods))))
    }
    if(is_base) {
        ## we use .ArgsEnv and .GenericArgsEnv in checkS3methods and codoc,
        ## so we check here that the set of primiitives has not been changed.
        base_funs <- ls("package:base", all.names=TRUE)
        prim <- sapply(base_funs,
                       function(x) is.primitive(get(x, "package:base")))
        prims <- base_funs[prim]
        prototypes <- sort(c(ls(envir=.ArgsEnv, all.names=TRUE),
                             ls(envir=.GenericArgsEnv, all.names=TRUE)))
        extras <- prototypes %w/o% prims
        if(length(extras))
            undoc_things <- c(undoc_things, list(prim_extra=extras))
        langElts <- c("$","$<-","&&","(",":","@","[","[[",
                      "[[<-","[<-","{","||","~","<-","<<-","=","break","for",
                      "function","if","next","repeat","return", "while")
        miss <- prims %w/o% c(langElts, prototypes)
        if(length(miss))
            undoc_things <- c(undoc_things, list(primitives=miss))
    }

    class(undoc_things) <- "undoc"
    undoc_things
}

print.undoc <-
function(x, ...)
{
    for(i in which(sapply(x, length) > 0L)) {
        tag <- names(x)[i]
        msg <- switch(tag,
                      "code objects" =
                      gettext("Undocumented code objects:"),
                      "data sets" =
                      gettext("Undocumented data sets:"),
                      "S4 classes" =
                      gettext("Undocumented S4 classes:"),
                      "S4 methods" =
                      gettext("Undocumented S4 methods:"),
                      prim_extra =
                      gettext("Prototyped non-primitives:"),
                      gettextf("Undocumented %s:", tag))
        writeLines(msg)
        ## We avoid markup for indicating S4 methods, hence need to
        ## special-case output for these ...
        if(tag == "S4 methods")
            writeLines(strwrap(x[[i]], indent = 2L, exdent = 4L))
        else
            .pretty_print(x[[i]])
    }
    invisible(x)
}

### * codoc

codoc <-
function(package, dir, lib.loc = NULL,
         use.values = NULL, verbose = getOption("verbose"))
{
    has_namespace <- FALSE

    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- .find.package(package, lib.loc)
        ## Using package installed in @code{dir} ...
        code_dir <- file.path(dir, "R")
        if(!file_test("-d", code_dir))
            stop(gettextf("directory '%s' does not contain R code",
                          dir),
                 domain = NA)
        if(!.haveRds(dir))
            stop(gettextf("directory '%s' does not contain Rd objects", dir),
                 domain = NA)
        is_base <- basename(dir) == "base"

        ## Load package into code_env.
        if(!is_base)
            .load_package_quietly(package, lib.loc)
        code_env <- .package_env(package)

        objects_in_code <- objects(envir = code_env, all.names = TRUE)

        ## Does the package have a namespace?
        if(packageHasNamespace(package, dirname(dir))) {
            has_namespace <- TRUE
            ns_env <- asNamespace(package)
            S3Table <- get(".__S3MethodsTable__.", envir = ns_env)
            functions_in_S3Table <- ls(S3Table, all.names = TRUE)
            objects_in_ns <-
                (objects(envir = ns_env, all.names = TRUE) %w/o%
                 c(".__NAMESPACE__.", ".__S3MethodsTable__."))
            objects_in_code_or_namespace <-
                unique(c(objects_in_code, objects_in_ns))
            objects_in_ns <- objects_in_ns %w/o% objects_in_code
        }
        else
            objects_in_code_or_namespace <- objects_in_code
        package_name <- package
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
        code_dir <- file.path(dir, "R")
        if(!file_test("-d", code_dir))
            stop(gettextf("directory '%s' does not contain R code",
                          dir),
                 domain = NA)
        if(!.haveRds(dir))
            stop(gettextf("directory '%s' does not contain Rd objects", dir),
                 domain = NA)
        package_name <- basename(dir)
        is_base <- package_name == "base"

        code_env <- new.env()
        dfile <- file.path(dir, "DESCRIPTION")
        meta <- if(file_test("-f", dfile))
            .read_description(dfile)
        else
            character()
        .source_assignments_in_code_dir(code_dir, code_env, meta)
        sys_data_file <- file.path(code_dir, "sysdata.rda")
        if(file_test("-f", sys_data_file)) load(sys_data_file, code_env)

        objects_in_code <- objects(envir = code_env, all.names = TRUE)
        objects_in_code_or_namespace <- objects_in_code

        ## Does the package have a NAMESPACE file?  Note that when
        ## working on the sources we (currently?) cannot deal with the
        ## (experimental) alternative way of specifying the namespace.
        ## Also, do not attempt to find S3 methods.
        if(file.exists(file.path(dir, "NAMESPACE"))) {
            has_namespace <- TRUE
            objects_in_ns <- objects_in_code
            functions_in_S3Table <- character(0L)
            ns_env <- code_env
            nsInfo <- parseNamespaceFile(basename(dir), dirname(dir))
            ## Look only at exported objects.
            OK <- intersect(objects_in_code, nsInfo$exports)
            for(p in nsInfo$exportPatterns)
                OK <- c(OK, grep(p, objects_in_code, value = TRUE))
            objects_in_code <- unique(OK)
        }
    }

    ## Find the data sets to work on.
    data_dir <- file.path(dir, "data")
    data_sets_in_code <- if(file_test("-d", data_dir))
        names(.try_quietly(list_data_in_pkg(dataDir = data_dir)))
    else
        character()

    ## Find the function objects to work on.
    functions_in_code <-
        Filter(function(f) {
                   f <- get(f, envir = code_env)
                   is.function(f) && (length(formals(f)) > 0L)
               },
               objects_in_code)
    ## Sourcing all R code files in the package is a problem for base,
    ## where this misses the .Primitive functions.  Hence, when checking
    ## base for objects shown in \usage but missing from the code, we
    ## get the primitive functions from the version of R we are using.
    ## Maybe one day we will have R code for the primitives as well ...
    ## As from R 2.5.0 we do for most generics.
    if(is_base) {
        objects_in_base <-
            objects(envir = baseenv(), all.names = TRUE)
        objects_in_code <-
            c(objects_in_code,
              Filter(.is_primitive_in_base, objects_in_base),
              c(".First.lib", ".Last.lib", ".Random.seed",
                ".onLoad", ".onAttach", ".onUnload"))
        objects_in_code_or_namespace <- objects_in_code
        known_env <- .make_S3_primitive_generic_env(code_env, fixup=TRUE)
        extras <- ls(known_env, all.names = TRUE)
        functions_in_code <- c(functions_in_code, extras)
        code_env <- known_env
        known_env <- .make_S3_primitive_nongeneric_env(code_env)
        extras <- ls(known_env, all.names = TRUE)
        functions_in_code <- c(functions_in_code, extras)
        code_env <- known_env
    }

    ## Build a list with the formals of the functions in the code
    ## indexed by the names of the functions.
    function_args_in_code <-
        lapply(functions_in_code,
               function(f) formals(get(f, envir = code_env)))
    names(function_args_in_code) <- functions_in_code
    if(has_namespace) {
        functions_in_ns <-
            Filter(function(f) {
                       f <- get(f, envir = ns_env)
                       is.function(f) && (length(formals(f)) > 0L)
                   },
                   objects_in_ns)
        function_args_in_ns <-
            lapply(functions_in_ns,
                   function(f) formals(get(f, envir = ns_env)))
        names(function_args_in_ns) <- functions_in_ns

        function_args_in_S3Table <-
            lapply(functions_in_S3Table,
                   function(f) formals(get(f, envir = S3Table)))
        names(function_args_in_S3Table) <- functions_in_S3Table

        tmp <- c(function_args_in_code, function_args_in_S3Table,
                 function_args_in_ns)
        keep <- !duplicated(names(tmp))
        function_args_in_code <- tmp[keep]
        functions_in_code <- names(function_args_in_code)
    }
    if(.isMethodsDispatchOn()) {
        ## <NOTE>
        ## There is no point in worrying about exportMethods directives
        ## in a NAMESPACE file when working on a package source dir, as
        ## we only source the assignments, and hence do not get any
        ## S4 classes or methods.
        ## </NOTE>
        ## <NOTE>
        ## In principle, we can get codoc checking for S4 methods
        ## documented explicitly using the \S4method{GENERIC}{SIGLIST}
        ## markup by adding the corresponding "pseudo functions" using
        ## the Rd markup as their name.  However note that the formals
        ## recorded in the methods db only pertain to the signature, not
        ## to the ones of the function actually registered ... hence we
        ## use methods::unRematchDefinition() which knows how to extract
        ## the formals in the method definition from the
        ##   function(ARGLIST) {
        ##     .local <- function(FORMALS) BODY
        ##     .local(ARGLIST)
        ##   }
        ## redefinitions obtained by methods::rematchDefinition().
        ## </NOTE>
        check_S4_methods <-
            !identical(as.logical(Sys.getenv("_R_CHECK_CODOC_S4_METHODS_")),
                       FALSE)
        if(check_S4_methods) {
            get_formals_from_method_definition <- function(m)
                formals(methods::unRematchDefinition(m))
            lapply(get_S4_generics_with_methods(code_env),
                   function(f) {
                       mlist <- .get_S4_methods_list(f, code_env)
                       sigs <- .make_siglist(mlist)
                       if(!length(sigs)) return()
                       nm <- sprintf("\\S4method{%s}{%s}", f, sigs)
                       args <- lapply(mlist,
                                      get_formals_from_method_definition)
                       names(args) <- nm
                       functions_in_code <<-
                           c(functions_in_code, nm)
                       function_args_in_code <<-
                           c(function_args_in_code, args)
                   })
        }
    }

    check_codoc <- function(fName, ffd) {
        ## Compare the formals of the function in the code named 'fName'
        ## and formals 'ffd' obtained from the documentation.
        ffc <- function_args_in_code[[fName]]
        if(identical(use.values, FALSE)) {
            ffc <- names(ffc)
            ffd <- names(ffd)
            ok <- identical(ffc, ffd)
        } else {
            if(!identical(names(ffc), names(ffd)))
                ok <- FALSE
            else {
                vffc <- as.character(ffc) # values
                vffd <- as.character(ffd) # values
                if(!identical(use.values, TRUE)) {
                    ind <- nzchar(as.character(ffd))
                    vffc <- vffc[ind]
                    vffd <- vffd[ind]
                }
                ok <- identical(vffc, vffd)
            }
        }
        if(ok)
            NULL
        else
            list(list(name = fName, code = ffc, docs = ffd))
    }

    db <- if(!missing(package))
        Rd_db(package, lib.loc = dirname(dir))
    else
        Rd_db(dir = dir)

    names(db) <- db_names <- .Rd_get_names_from_Rd_db(db)

    ## pkg-defunct.Rd is not expected to list arguments
    ind <- db_names %in% paste(package_name, "defunct", sep="-")
    db <- db[!ind]
    db_names <- db_names[!ind]

    db_usages <- lapply(db, .Rd_get_section, "usage")
    db_synopses <- lapply(db, .Rd_get_section, "synopsis")
    ind <- sapply(db_synopses, length) > 0L
    db_usages[ind] <- db_synopses[ind]
    with_synopsis <- as.character(db_names[ind])
    db_usages <- lapply(db_usages, .parse_usage_as_much_as_possible)
    ind <- as.logical(sapply(db_usages,
                             function(x) !is.null(attr(x, "bad_lines"))))
    bad_lines <- lapply(db_usages[ind], attr, "bad_lines")

    functions_to_be_ignored <-
        c(.functions_to_be_ignored_from_usage(basename(dir)),
          .functions_with_no_useful_S3_method_markup())

    bad_doc_objects <- list()
    functions_in_usages <- character()
    variables_in_usages <- character()
    data_sets_in_usages <- character()
    functions_in_usages_not_in_code <- list()
    data_sets_in_usages_not_in_code <- list()

    for(docObj in db_names) {

        exprs <- db_usages[[docObj]]
        if(!length(exprs)) next

        ## Get variable names and data set usages first, mostly for
        ## curiosity.
        ind <- ! sapply(exprs, is.call)
        if(any(ind)) {
            variables_in_usages <-
                c(variables_in_usages,
                  sapply(exprs[ind], deparse))
            exprs <- exprs[!ind]
        }
        ind <- as.logical(sapply(exprs,
                                 function(e)
                                 (length(e) == 2L)
                                 && e[[1L]] == as.symbol("data")))
        if(any(ind)) {
            data_sets <- sapply(exprs[ind],
                                function(e) as.character(e[[2L]]))
            data_sets_in_usages <- c(data_sets_in_usages, data_sets)
            data_sets <- data_sets %w/o% data_sets_in_code
            if(length(data_sets))
                data_sets_in_usages_not_in_code[[docObj]] <- data_sets
            exprs <- exprs[!ind]
        }
        functions <- sapply(exprs, function(e) as.character(e[[1L]]))
        functions <- .transform_S3_method_markup(as.character(functions))
        ind <- (! functions %in% functions_to_be_ignored
                & functions %in% functions_in_code)
        bad_functions <-
            mapply(functions[ind],
                   exprs[ind],
                   FUN = function(x, y)
                   check_codoc(x, as.pairlist(as.alist.call(y[-1L]))),
                   SIMPLIFY = FALSE)
        ## Replacement functions.
        ind <- as.logical(sapply(exprs,
                                 .is_call_from_replacement_function_usage))
        if(any(ind)) {
            exprs <- exprs[ind]
            replace_funs <-
                paste(sapply(exprs,
                             function(e) as.character(e[[2L]][[1L]])),
                      "<-",
                      sep = "")
            replace_funs <- .transform_S3_method_markup(replace_funs)
            functions <- c(functions, replace_funs)
            ind <- (replace_funs %in% functions_in_code)
            if(any(ind)) {
                bad_replace_funs <-
                    mapply(replace_funs[ind],
                           exprs[ind],
                           FUN = function(x, y)
                           check_codoc(x,
                                      as.pairlist(c(as.alist.call(y[[2L]][-1L]),
                                                    as.alist.symbol(y[[3L]])))),
                           SIMPLIFY = FALSE)
                bad_functions <-
                    c(bad_functions, bad_replace_funs)
            }
        }

        bad_functions <- do.call("c", bad_functions)
        if(length(bad_functions))
            bad_doc_objects[[docObj]] <- bad_functions

        ## Determine functions with a \usage entry in the documentation
        ## but 'missing from the code'.  If a package has a namespace, we
        ## really need to look at all objects in the namespace (hence
        ## 'objects_in_code_or_namespace'), as one can access the internal
        ## symbols via ':::' and hence package developers might want to
        ## provide function usages for some of the internal functions.
        ## <FIXME>
        ## We may still have \S4method{}{} entries in functions, which
        ## cannot have a corresponding object in the code.  Hence, we
        ## remove these function entries, but should really do better,
        ## by comparing the explicit \usage entries for S4 methods to
        ## what is actually in the code.  We most likely also should do
        ## something similar for S3 methods.
        ind <- grep(.S4_method_markup_regexp, functions)
        if(any(ind))
            functions <- functions[!ind]
        ## </FIXME>
        bad_functions <-
            functions %w/o% c(objects_in_code_or_namespace,
                              functions_to_be_ignored)
        if(length(bad_functions))
            functions_in_usages_not_in_code[[docObj]] <- bad_functions

        functions_in_usages <- c(functions_in_usages, functions)
    }

    ## Determine (function) objects in the code without a \usage entry.
    ## Of course, these could still be 'documented' via \alias.
    ## </NOTE>
    ## Older versions only printed this information without returning it
    ## (in case 'verbose' was true).  We now add this as an attribute to
    ## the bad_doc_objects returned.
    ## </NOTE>
    objects_in_code_not_in_usages <-
        objects_in_code %w/o% c(functions_in_usages, variables_in_usages)
    functions_in_code_not_in_usages <-
        intersect(functions_in_code, objects_in_code_not_in_usages)
    ## (Note that 'functions_in_code' does not necessarily contain all
    ## (exported) functions in the package.)

    ## Determine functions which have no usage but really should have.
    ## If there is no name space (including base), we have no idea.
    ## If there is one, everything "exported" (in the package env)
    ## should also have a \usage, apart from
    ## * Defunct functions
    ## * S4 generics.  Note that as per R-exts,
    ##     exporting methods on a generic in the namespace will also
    ##     export the generic, and exporting a generic in the namespace
    ##     will also export its methods.
    ##   so it seems there is really no way to figure out whether an
    ##   exported S4 generic should have a \usage entry or not ...
    functions_missing_from_usages <-
        if(!has_namespace) character() else {
            functions <- functions_in_code_not_in_usages
            if(.isMethodsDispatchOn()) {
                ## Drop the functions which have S4 methods.
                functions <-
                    functions %w/o% get_S4_generics_with_methods(code_env)
            }
            ## Drop the defunct functions.
            is_defunct <- function(f) {
                f <- get(f, envir = code_env)
                if(!is.function(f)) return(FALSE)
                (is.call(b <- body(f))
                 && identical(as.character(b[[1L]]), ".Defunct"))
            }
            functions[!sapply(functions, is_defunct)]
        }
    objects_missing_from_usages <-
        if(!has_namespace) character() else {
            c(functions_missing_from_usages,
              (objects_in_code_not_in_usages
               %w/o% c(functions_in_code, data_sets_in_code)))
        }

    attr(bad_doc_objects, "objects_in_code_not_in_usages") <-
        objects_in_code_not_in_usages
    attr(bad_doc_objects, "functions_in_code_not_in_usages") <-
        functions_in_code_not_in_usages
    attr(bad_doc_objects, "functions_in_usages_not_in_code") <-
        functions_in_usages_not_in_code
    attr(bad_doc_objects, "function_args_in_code") <-
        function_args_in_code
    attr(bad_doc_objects, "data_sets_in_usages_not_in_code") <-
        data_sets_in_usages_not_in_code
    attr(bad_doc_objects, "objects_missing_from_usages") <-
        objects_missing_from_usages
    attr(bad_doc_objects, "functions_missing_from_usages") <-
        functions_missing_from_usages
    attr(bad_doc_objects, "has_namespace") <- has_namespace
    attr(bad_doc_objects, "with_synopsis") <- with_synopsis
    attr(bad_doc_objects, "bad_lines") <- bad_lines
    class(bad_doc_objects) <- "codoc"
    bad_doc_objects
}

print.codoc <-
function(x, ...)
{
    functions_in_usages_not_in_code <-
        attr(x, "functions_in_usages_not_in_code")
    if(length(functions_in_usages_not_in_code)) {
        for(fname in names(functions_in_usages_not_in_code)) {
            writeLines(gettextf("Functions/methods with usage in documentation object '%s' but not in code:",
                                fname))
            .pretty_print(unique(functions_in_usages_not_in_code[[fname]]))
            writeLines("")
        }
    }

    data_sets_in_usages_not_in_code <-
        attr(x, "data_sets_in_usages_not_in_code")
    if(length(data_sets_in_usages_not_in_code)) {
        for(fname in names(data_sets_in_usages_not_in_code)) {
            writeLines(gettextf("Data sets with usage in documentation object '%s' but not in code:",
                                fname))
            .pretty_print(unique(data_sets_in_usages_not_in_code[[fname]]))
            writeLines("")
        }
    }

    ## In general, functions in the code which only have an \alias but
    ## no \usage entry are not necessarily a problem---they might be
    ## mentioned in other parts of the Rd object documenting them, or be
    ## 'internal'.  However, if a package has a namespace, then all
    ## *exported* functions should have \usage entries (apart from
    ## defunct functions and S4 generics, see the above comments for
    ## functions_missing_from_usages).  Currently, this information is
    ## returned in the codoc object but not shown.  Eventually, we might
    ## add something like
    ##     functions_missing_from_usages <-
    ##         attr(x, "functions_missing_from_usages")
    ##     if(length(functions_missing_from_usages)) {
    ##         writeLines("Exported functions without usage information:")
    ##         .pretty_print(functions_in_code_not_in_usages)
    ##         writeLines("")
    ##     }
    ## similar to the above.

    if(!length(x))
        return(invisible(x))

    has_only_names <- is.character(x[[1L]][[1L]][["code"]])

    format_args <- function(s) {
        if(!length(s))
            "function()"
        else if(has_only_names)
            paste("function(", paste(s, collapse = ", "), ")", sep = "")
        else {
            s <- paste(deparse(s), collapse = "")
            s <- gsub(" = ([,\\)])", "\\1", s)
            s <- gsub("<unescaped bksl>", "\\", s, fixed = TRUE)
            gsub("^list", "function", s)
        }
    }

    summarize_mismatches_in_names <- function(nfc, nfd) {
        if(length(nms <- nfc %w/o% nfd))
            writeLines(c(gettext("  Argument names in code not in docs:"),
                         strwrap(paste(nms, collapse = " "),
                                 indent = 4L, exdent = 4L)))
        if(length(nms <- nfd %w/o% nfc))
            writeLines(c(gettext("  Argument names in docs not in code:"),
                         strwrap(paste(nms, collapse = " "),
                                 indent = 4L, exdent = 4L)))
        len <- min(length(nfc), length(nfd))
        if(len) {
            len <- seq_len(len)
            nfc <- nfc[len]
            nfd <- nfd[len]
            ind <- which(nfc != nfd)
            len <- length(ind)
            if(len) {
                if(len > 3L) {
                    writeLines(gettext("  Mismatches in argument names (first 3):"))
                    ind <- ind[1L:3L]
                } else {
                    writeLines(gettext("  Mismatches in argument names:"))
                }
                for(i in ind) {
                    writeLines(sprintf("    Position: %d Code: %s Docs: %s",
                                       i, nfc[i], nfd[i]))
                }
            }
        }
    }

    summarize_mismatches_in_values <- function(ffc, ffd) {
        ## Be nice, and match arguments by names first.
        nms <- intersect(names(ffc), names(ffd))
        vffc <- ffc[nms]
        vffd <- ffd[nms]
        ind <- which(as.character(vffc) != as.character(vffd))
        len <- length(ind)
        if(len) {
            if(len > 3L) {
                writeLines(gettext("  Mismatches in argument default values (first 3):"))
                ind <- ind[1L:3L]
            } else {
                writeLines(gettext("  Mismatches in argument default values:"))
            }
            for(i in ind) {
                cv <- deparse(vffc[[i]])
                dv <- deparse(vffd[[i]])
                dv <- gsub("<unescaped bksl>", "\\", dv, fixed = TRUE)
                writeLines(sprintf("    Name: '%s' Code: %s Docs: %s",
                                   nms[i], cv, dv))
            }
        }
    }

    summarize_mismatches <- function(ffc, ffd) {
        if(has_only_names)
            summarize_mismatches_in_names(ffc, ffd)
        else {
            summarize_mismatches_in_names(names(ffc), names(ffd))
            summarize_mismatches_in_values(ffc, ffd)
        }
    }

    for(fname in names(x)) {
        writeLines(gettextf("Codoc mismatches from documentation object '%s':",
                            fname))
        xfname <- x[[fname]]
        for(i in seq_along(xfname)) {
            ffc <- xfname[[i]][["code"]]
            ffd <- xfname[[i]][["docs"]]
            writeLines(c(xfname[[i]][["name"]],
                         strwrap(gettextf("Code: %s", format_args(ffc)),
                                 indent = 2L, exdent = 17L),
                         strwrap(gettextf("Docs: %s", format_args(ffd)),
                                 indent = 2L, exdent = 17L)))
            summarize_mismatches(ffc, ffd)
        }
        writeLines("")
    }

    invisible(x)
}

### * codocClasses

codocClasses <-
function(package, lib.loc = NULL)
{
    ## Compare the 'structure' of S4 classes in an installed package
    ## between code and documentation.
    ## Currently, only compares the slot names.

    ## <NOTE>
    ## This is patterned after the current codoc().
    ## It would be useful to return the whole information on class slot
    ## names found in the code and matching documentation (rather than
    ## just the ones with mismatches).
    ## Currently, we only return the names of all classes checked.
    ## </NOTE>

    bad_Rd_objects <- structure(NULL, class = "codocClasses")

    ## Argument handling.
    if(length(package) != 1L)
        stop("argument 'package' must be of length 1")
    dir <- .find.package(package, lib.loc)
    if(!file_test("-d", file.path(dir, "R")))
        stop(gettextf("directory '%s' does not contain R code", dir),
             domain = NA)
    if(!.haveRds(dir))
        stop(gettextf("directory '%s' does not contain Rd objects", dir),
             domain = NA)
    is_base <- basename(dir) == "base"

    ## Load package into code_env.
    if(!is_base)
        .load_package_quietly(package, lib.loc)
    code_env <- .package_env(package)

    if(!.isMethodsDispatchOn())
        return(bad_Rd_objects)

    S4_classes <- methods::getClasses(code_env)
    if(!length(S4_classes)) return(bad_Rd_objects)

    sApply <- function(X, FUN, ...) ## fast and special case - only
        unlist(lapply(X, FUN, ...), recursive=FALSE, use.names=FALSE)
    ## Build Rd data base.
    db <- Rd_db(package, lib.loc = dirname(dir))

    ## Need some heuristics now.  When does an Rd object document just
    ## one S4 class so that we can compare (at least) the slot names?
    ## Try the following:
    ## 1) \docType{} identical to "class";
    ## 2) either exactly one \alias{} or only one ending in "-class"
    ## 3) a non-empty user-defined section 'Slots'.

    ## As going through the db to extract sections can take some time,
    ## we do the vectorized metadata computations first, and try to
    ## subscript whenever possible.

    idx <- sApply(lapply(db, .Rd_get_doc_type), identical, "class")
    if(!any(idx)) return(bad_Rd_objects)
    db <- db[idx]
    stats <- c(n.S4classes = length(S4_classes), n.db = length(db))

    aliases <- lapply(db, .Rd_get_metadata, "alias")
    named_class <- lapply(aliases, grepl, pattern="-class$")
    nClass <- sApply(named_class, sum)
    oneAlias <- sApply(aliases, length) == 1L
    idx <- oneAlias | nClass == 1L
    if(!any(idx)) return(bad_Rd_objects)
    db <- db[idx]
    stats["n.cl"] <- length(db)

    ## keep only the foo-class alias in case there was more than one:
    multi <- idx & !oneAlias
    aliases[multi] <-
        mapply(`[`, aliases[multi], named_class[multi],
               SIMPLIFY = FALSE, USE.NAMES = FALSE)
    aliases <- unlist(aliases[idx], use.names = FALSE)

    Rd_slots <- lapply(db, .Rd_get_section, "Slots", FALSE)
    idx <- sapply(Rd_slots, length) > 0L
    if(!any(idx)) return(bad_Rd_objects)
    db <- db[idx]; aliases <- aliases[idx]; Rd_slots <- Rd_slots[idx]
    stats["n.final"] <- length(db)

    db_names <- .Rd_get_names_from_Rd_db(db)

    .get_slot_names <- function(x) {
        ## Get \describe (inside user-defined section 'Slots'):
        ## Should this allow for several \describe blocks?
        x <- .Rd_get_section(x, "describe")
        ## Get the \item tags inside \describe.
        txt <- .Rd_get_item_tags(x)
        if(!length(txt)) return(character())
        txt <- gsub("\\\\l?dots", "...", txt)
        ## And now strip enclosing '\code{...}:'
        txt <- gsub("\\\\code\\{([^}]*)\\}:?", "\\1", as.character(txt))
        txt <- unlist(strsplit(txt, ", *"))
        .strip_whitespace(txt)
    }

    .inheritedSlotNames <- function(ext) {
	supcl <- methods::.selectSuperClasses(ext)
	unique(unlist(lapply(lapply(supcl, methods::getClassDef),
			     methods::slotNames),
		      use.names=FALSE))
    }

    S4topics <- sApply(S4_classes, utils:::topicName, type="class")
    S4_checked <- S4_classes[has.a <- S4topics %in% aliases]
    idx <- match(S4topics[has.a], aliases)
    for(icl in seq_along(S4_checked)) {
        cl <- S4_checked[icl]
        cld <- methods::getClass(cl, where = code_env)
        ii <- idx[icl]
        ## Add sanity checking later ...
        scld <- methods::slotNames(cld)
        codeSlots <- if(!is.null(scld)) sort(scld) else character()
        docSlots  <- sort(.get_slot_names(Rd_slots[[ii]]))
        superSlots <- .inheritedSlotNames(cld@contains)
        if(length(superSlots)) ## allow '\dots' in docSlots
            docSlots <-
                docSlots[is.na(match(docSlots, c("...", "\\dots")))]
        ## was if(!identical(slots_in_code, slots_in_docs)) {
        if(!all(d.in.c <- docSlots %in% codeSlots) ||
           !all(c.in.d <- (codeSlots %w/o% superSlots) %in% docSlots) ) {
            bad_Rd_objects[[db_names[ii]]] <-
                list(name = cl,
                     code = codeSlots,
                     inherited = superSlots,
                     docs = docSlots)
        }
    }

    attr(bad_Rd_objects, "S4_classes_checked") <- S4_checked
    attr(bad_Rd_objects, "stats") <- stats
    bad_Rd_objects
} ## end{ codocClasses }

print.codocClasses <-
function(x, ...)
{
    if(!length(x))
        return(invisible(x))
    capWord <- function(w) sub("\\b(\\w)", "\\U\\1", w, perl=TRUE)
    wrapPart <- function(nam) {
	if(length(O <- docObj[[nam]]))
	    strwrap(sprintf("%s: %s", gettextf(capWord(nam)),
			    paste(O, collapse = " ")),
		    indent = 2L, exdent = 8L)
    }
    for (docObj in names(x)) {
        writeLines(gettextf("S4 class codoc mismatches from documentation object '%s':",
                            docObj))
        docObj <- x[[docObj]]
        writeLines(c(gettextf("Slots for class '%s'", docObj[["name"]]),
		     wrapPart("code"),
		     wrapPart("inherited"),
		     wrapPart("docs")))
        writeLines("")
    }
    invisible(x)
}

### * codocData

codocData <-
function(package, lib.loc = NULL)
{
    ## Compare the 'structure' of 'data' objects (variables or data
    ## sets) in an installed package between code and documentation.
    ## Currently, only compares the variable names of data frames found.

    ## <NOTE>
    ## This is patterned after the current codoc().
    ## It would be useful to return the whole information on data frame
    ## variable names found in the code and matching documentation
    ## (rather than just the ones with mismatches).
    ## Currently, we only return the names of all data frames checked.
    ## </NOTE>

    bad_Rd_objects <- structure(NULL, class = "codocData")

    ## Argument handling.
    if(length(package) != 1L)
        stop("argument 'package' must be of length 1")

    dir <- .find.package(package, lib.loc)

    ## Build Rd data base.
    db <- Rd_db(package, lib.loc = dirname(dir))

    is_base <- basename(dir) == "base"
    has_namespace <- !is_base && packageHasNamespace(package, dirname(dir))

    ## Load package into code_env.
    if(!is_base)
        .load_package_quietly(package, lib.loc)
    code_env <- .package_env(package)
    if(has_namespace) ns_env <- asNamespace(package)

    ## Could check here whether the package has any variables or data
    ## sets (and return if not).


    ## Need some heuristics now.  When does an Rd object document a
    ## data.frame (could add support for other classes later) variable
    ## or data set so that we can compare (at least) the names of the
    ## variables in the data frame?  Try the following:
    ## * just one \alias{};
    ## * if documentation was generated via prompt, there is a \format
    ##   section starting with 'A data frame with' (but many existing Rd
    ##   files instead have 'This data frame contains' and containing
    ##   one or more \describe sections inside.

    ## As going through the db to extract sections can take some time,
    ## we do the vectorized metadata computations first, and try to
    ## subscript whenever possible.
    aliases <- lapply(db, .Rd_get_metadata, "alias")
    idx <- sapply(aliases, length) == 1L
    if(!any(idx)) return(bad_Rd_objects)
    db <- db[idx]
    aliases <- aliases[idx]

    names(db) <- .Rd_get_names_from_Rd_db(db)

    .get_data_frame_var_names <- function(x) {
        ## Make sure that there is exactly one format section:
        ## using .Rd_get_section() would get the first one.
        x <- x[RdTags(x) == "\\format"]
        if(length(x) != 1L) return(character())
        ## Drop comments.
        ## <FIXME>
        ## Remove calling .Rd_drop_comments() eventually.
        x <- .Rd_drop_comments(x[[1L]])
        ## </FIXME>
        ## What did the format section start with?
        if(!grepl("^[ \n\t]*(A|This) data frame",
                  .Rd_deparse(x, tag = FALSE)))
            return(character())
        ## Get \describe inside \format.
        ## Should this allow for several \describe blocks?
        x <- .Rd_get_section(x, "describe")
        ## Get the \item tags inside \describe.
        txt <- .Rd_get_item_tags(x)
        if(!length(txt)) return(character())
        txt <- gsub("(.*):$", "\\1", as.character(txt))
        txt <- gsub("\\\\code\\{(.*)\\}:?", "\\1", txt)
        ## Argh.  Of course, variable names can have a '_', which needs
        ## to be escaped if not in \code{}, and the prompt() default is
        ## not to put variable names inside \code{}.
        txt <- gsub("\\\\_", "_", txt)
        txt <- unlist(strsplit(txt, ", *"))
        .strip_whitespace(txt)
    }

    Rd_var_names <- lapply(db, .get_data_frame_var_names)

    idx <- (sapply(Rd_var_names, length) > 0L)
    if(!length(idx)) return(bad_Rd_objects)
    aliases <- unlist(aliases[idx])
    Rd_var_names <- Rd_var_names[idx]

    db_names <- names(db)[idx]

    data_env <- new.env()
    data_dir <- file.path(dir, "data")
    ## with lazy data we have data() but don't need to use it.
    has_data <- file_test("-d", data_dir) &&
        !file_test("-f", file.path(data_dir, "Rdata.rdb"))
    data_exts <- .make_file_exts("data")

    ## Now go through the aliases.
    data_frames_checked <- character()
    for(i in seq_along(aliases)) {
        ## Store the documented variable names.
        var_names_in_docs <- sort(Rd_var_names[[i]])
        ## Try finding the variable or data set given by the alias.
        al <- aliases[i]
        if(exists(al, envir = code_env, mode = "list",
                  inherits = FALSE)) {
            al <- get(al, envir = code_env, mode = "list")
        } else if(has_namespace && exists(al, envir = ns_env, mode = "list",
                  inherits = FALSE)) {
            al <- get(al, envir = ns_env, mode = "list")
        } else if(has_data) {
            ## Should be a data set.
            if(!length(dir(data_dir)
                       %in% paste(al, data_exts, sep = "."))) {
                next                    # What the hell did we pick up?
            }
            ## Try loading the data set into data_env.
            utils::data(list = al, envir = data_env)
            if(exists(al, envir = data_env, mode = "list",
                      inherits = FALSE)) {
                al <- get(al, envir = data_env, mode = "list")
            }
            ## And clean up data_env.
            rm(list = ls(envir = data_env, all.names = TRUE),
               envir = data_env)
        }
        if(!is.data.frame(al)) next
        ## Now we should be ready:
        data_frames_checked <- c(data_frames_checked, aliases[i])
        var_names_in_code <- sort(names(al))
        if(!identical(var_names_in_code, var_names_in_docs))
            bad_Rd_objects[[db_names[i]]] <-
                list(name = aliases[i],
                     code = var_names_in_code,
                     docs = var_names_in_docs)
    }

    attr(bad_Rd_objects, "data_frames_checked") <-
        as.character(data_frames_checked)
    bad_Rd_objects
}

print.codocData <-
function(x, ...)
{
    format_args <- function(s) paste(s, collapse = " ")
    for(docObj in names(x)) {
        writeLines(gettextf("Data codoc mismatches from documentation object '%s':",
                            docObj))
        docObj <- x[[docObj]]
        writeLines(c(gettextf("Variables in data frame '%s'",
                              docObj[["name"]]),
                     strwrap(gettextf("Code: %s",
                                      format_args(docObj[["code"]])),
                             indent = 2L, exdent = 8L),
                     strwrap(gettextf("Docs: %s",
                                      format_args(docObj[["docs"]])),
                             indent = 2L, exdent = 8L)))
        writeLines("")
    }
    invisible(x)
}

### * checkDocFiles

checkDocFiles <-
function(package, dir, lib.loc = NULL)
{
    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- .find.package(package, lib.loc)
        ## Using package installed in @code{dir} ...
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
    }

    db <- if(!missing(package))
        Rd_db(package, lib.loc = dirname(dir))
    else
        Rd_db(dir = dir)

    db_aliases <- lapply(db, .Rd_get_metadata, "alias")
    db_keywords <- lapply(db, .Rd_get_metadata, "keyword")

    db_names <- .Rd_get_names_from_Rd_db(db)
    names(db) <- names(db_aliases) <- db_names

    db_usages <- lapply(db, .Rd_get_section, "usage")
    ## We traditionally also use the usage "texts" for some sanity
    ## checking ...
    ## <FIXME>
    ## Remove calling .Rd_drop_comments() eventually.
    db_usage_texts <-
        lapply(db_usages,
               function(e) .Rd_deparse(.Rd_drop_comments(e)))
    ## </FIXME>
    db_usages <- lapply(db_usages, .parse_usage_as_much_as_possible)
    ind <- as.logical(sapply(db_usages,
                             function(x) !is.null(attr(x, "bad_lines"))))
    bad_lines <- lapply(db_usages[ind], attr, "bad_lines")

    ## Exclude internal objects from further computations.
    ind <- sapply(db_keywords,
                  function(x) length(grep("^ *internal *$", x)) > 0L )
    if(any(ind)) {                      # exclude them
        db <- db[!ind]
        db_names <- db_names[!ind]
        db_aliases <- db_aliases[!ind]
    }

    db_argument_names <- lapply(db, .Rd_get_argument_names)

    functions_to_be_ignored <-
        .functions_to_be_ignored_from_usage(basename(dir))

    bad_doc_objects <- list()

    for(docObj in db_names) {

        exprs <- db_usages[[docObj]]
        if(!length(exprs)) next

        aliases <- db_aliases[[docObj]]
        arg_names_in_arg_list <- db_argument_names[[docObj]]

        ## Determine function names ('functions') and corresponding
        ## arguments ('arg_names_in_usage') in the \usage.  Note how we
        ## try to deal with data set documentation.
        ind <- as.logical(sapply(exprs,
                                 function(e)
                                 ((length(e) > 1L) &&
                                  !((length(e) == 2L)
                                    && e[[1L]] == as.symbol("data")))))
        exprs <- exprs[ind]
        ## Ordinary functions.
        functions <- as.character(sapply(exprs,
                                         function(e)
                                         as.character(e[[1L]])))
        ## (Note that as.character(sapply(exprs, "[[", 1L)) does not do
        ## what we want due to backquotifying.)
        ind <- ! functions %in% functions_to_be_ignored
        functions <- functions[ind]
        arg_names_in_usage <-
            unlist(sapply(exprs[ind],
                          function(e) .arg_names_from_call(e[-1L])))
        ## Replacement functions.
        ind <- as.logical(sapply(exprs,
                                 .is_call_from_replacement_function_usage))
        if(any(ind)) {
            replace_funs <-
                paste(sapply(exprs[ind],
                             function(e) as.character(e[[2L]][[1L]])),
                      "<-",
                      sep = "")
            functions <- c(functions, replace_funs)
            arg_names_in_usage <-
                c(arg_names_in_usage,
                  unlist(sapply(exprs[ind],
                                function(e)
                                c(.arg_names_from_call(e[[2L]][-1L]),
                                  .arg_names_from_call(e[[3L]])))))
        }
        ## And finally transform the S3 \method{}{} markup into the
        ## usual function names ...
        ## <NOTE>
        ## If we were really picky, we would worry about possible
        ## namespace renaming.
        functions <- .transform_S3_method_markup(functions)
        ## </NOTE>
        ## Also transform the markup for S4 replacement methods.
        functions <- .transform_S4_method_markup(functions)

        ## Now analyze what we found.
        arg_names_in_usage_missing_in_arg_list <-
            arg_names_in_usage %w/o% arg_names_in_arg_list
        arg_names_in_arg_list_missing_in_usage <-
            arg_names_in_arg_list %w/o% arg_names_in_usage
        if(length(arg_names_in_arg_list_missing_in_usage)) {
            usage_text <- db_usage_texts[[docObj]]
            bad_args <- character()
            ## In the case of 'over-documented' arguments, try to be
            ## defensive and reduce to arguments which either are not
            ## syntactically valid names or do not match the \usage text
            ## (modulo word boundaries).
            bad <- !grepl("^[[:alnum:]._]+$",
                          arg_names_in_arg_list_missing_in_usage)
            if(any(bad)) {
                bad_args <- arg_names_in_arg_list_missing_in_usage[bad]
                arg_names_in_arg_list_missing_in_usage <-
                    arg_names_in_arg_list_missing_in_usage[!bad]
            }
            bad <- sapply(arg_names_in_arg_list_missing_in_usage,
                          function(x)
                          !grepl(paste("\\b", x, "\\b", sep = ""),
                                 usage_text))
            arg_names_in_arg_list_missing_in_usage <-
                c(bad_args,
                  arg_names_in_arg_list_missing_in_usage[as.logical(bad)])
            ## Note that the fact that we can parse the raw \usage does
            ## not imply that over-documented arguments are a problem:
            ## this works for Rd files documenting e.g. shell utilities
            ## but fails for files with special syntax (Extract.Rd).
        }

        ## Also test whether the objects we found from the \usage all
        ## have aliases, provided that there is no alias which ends in
        ## '-deprecated' (see e.g. base-deprecated.Rd).
        if(!length(grep("-deprecated$", aliases))) {
            functions <-
                functions %w/o% .functions_with_no_useful_S3_method_markup()
            ## Argh.  There are good reasons for keeping \S4method{}{}
            ## as is, but of course this is not what the aliases use ...
            ## <FIXME>
            ## Should maybe use utils:::topicName(), but in any case, we
            ## should have functions for converting between the two
            ## forms, see also the code for undoc().
            aliases <- sub("([^,]+),(.+)-method$",
                           "\\\\S4method{\\1}{\\2}",
                           aliases)
            ## </FIXME>
            aliases <- gsub("\\\\%", "%", aliases)
            functions_not_in_aliases <- functions %w/o% aliases
        }
        else
            functions_not_in_aliases <- character()

        if((length(arg_names_in_usage_missing_in_arg_list))
           || anyDuplicated(arg_names_in_arg_list)
           || (length(arg_names_in_arg_list_missing_in_usage))
           || (length(functions_not_in_aliases)))
            bad_doc_objects[[docObj]] <-
                list(missing = arg_names_in_usage_missing_in_arg_list,
                     duplicated =
                     arg_names_in_arg_list[duplicated(arg_names_in_arg_list)],
                     overdoc = arg_names_in_arg_list_missing_in_usage,
                     unaliased = functions_not_in_aliases)

    }

    class(bad_doc_objects) <- "checkDocFiles"
    attr(bad_doc_objects, "bad_lines") <- bad_lines
    bad_doc_objects
}

print.checkDocFiles <-
function(x, ...)
{
    for(doc_obj in names(x)) {
        arg_names_in_usage_missing_in_arg_list <- x[[doc_obj]][["missing"]]
        if(length(arg_names_in_usage_missing_in_arg_list)) {
            writeLines(gettextf("Undocumented arguments in documentation object '%s'",
                                doc_obj))
            .pretty_print(unique(arg_names_in_usage_missing_in_arg_list))
        }
        duplicated_args_in_arg_list <- x[[doc_obj]][["duplicated"]]
        if(length(duplicated_args_in_arg_list)) {
            writeLines(gettextf("Duplicated \\argument entries in documentation object '%s':",
                                doc_obj))
            .pretty_print(duplicated_args_in_arg_list)
        }
        arg_names_in_arg_list_missing_in_usage <- x[[doc_obj]][["overdoc"]]
        if(length(arg_names_in_arg_list_missing_in_usage)) {
            writeLines(gettextf("Documented arguments not in \\usage in documentation object '%s':",
                                doc_obj))
            .pretty_print(unique(arg_names_in_arg_list_missing_in_usage))
        }
        functions_not_in_aliases <- x[[doc_obj]][["unaliased"]]
        if(length(functions_not_in_aliases)) {
            writeLines(gettextf("Objects in \\usage without \\alias in documentation object '%s':",
                                doc_obj))
            .pretty_print(unique(functions_not_in_aliases))
        }

        writeLines("")
    }

    if(!identical(as.logical(Sys.getenv("_R_CHECK_WARN_BAD_USAGE_LINES_")),
                  FALSE)
       && length(bad_lines <- attr(x, "bad_lines"))) {
        for(doc_obj in names(bad_lines)) {
            writeLines(gettextf("Bad \\usage lines found in documentation object '%s':",
                                doc_obj))
            writeLines(paste(" ", bad_lines[[doc_obj]]))
        }
        writeLines("")
    }

    invisible(x)
}

### * checkDocStyle

checkDocStyle <-
function(package, dir, lib.loc = NULL)
{
    has_namespace <- FALSE

    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- .find.package(package, lib.loc)
        ## Using package installed in 'dir' ...
        code_dir <- file.path(dir, "R")
        if(!file_test("-d", code_dir))
            stop(gettextf("directory '%s' does not contain R code",
                          dir),
                 domain = NA)
        if(!.haveRds(dir))
            stop(gettextf("directory '%s' does not contain Rd objects", dir),
                 domain = NA)
        package_name <- package
        is_base <- package_name == "base"

        ## Load package into code_env.
        if(!is_base)
            .load_package_quietly(package, lib.loc)
        code_env <- .package_env(package)

        objects_in_code <- objects(envir = code_env, all.names = TRUE)

        ## Does the package have a namespace?
        if(packageHasNamespace(package, dirname(dir))) {
            has_namespace <- TRUE
            ## Determine names of declared S3 methods and associated S3
            ## generics.
            ns_S3_methods_db <- getNamespaceInfo(package, "S3methods")
            ns_S3_generics <- ns_S3_methods_db[, 1L]
            ns_S3_methods <- ns_S3_methods_db[, 3L]
        }
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
        code_dir <- file.path(dir, "R")
        if(!file_test("-d", code_dir))
            stop(gettextf("directory '%s' does not contain R code",
                          dir),
                 domain = NA)
        if(!.haveRds(dir))
            stop(gettextf("directory '%s' does not contain Rd objects", dir),
                 domain = NA)
        package_name <- basename(dir)
        is_base <- package_name == "base"

        code_env <- new.env()
        dfile <- file.path(dir, "DESCRIPTION")
        meta <- if(file_test("-f", dfile))
            .read_description(dfile)
        else
            character()
        .source_assignments_in_code_dir(code_dir, code_env, meta)
        sys_data_file <- file.path(code_dir, "sysdata.rda")
        if(file_test("-f", sys_data_file)) load(sys_data_file, code_env)

        objects_in_code <- objects(envir = code_env, all.names = TRUE)

        ## Does the package have a NAMESPACE file?  Note that when
        ## working on the sources we (currently?) cannot deal with the
        ## (experimental) alternative way of specifying the namespace.
        if(file.exists(file.path(dir, "NAMESPACE"))) {
            has_namespace <- TRUE
            nsInfo <- parseNamespaceFile(basename(dir), dirname(dir))
            ## Determine exported objects.
            OK <- intersect(objects_in_code, nsInfo$exports)
            for(p in nsInfo$exportPatterns)
                OK <- c(OK, grep(p, objects_in_code, value = TRUE))
            objects_in_code <- unique(OK)
            ## Determine names of declared S3 methods and associated S3
            ## generics.
            ns_S3_methods_db <- .get_namespace_S3_methods_db(nsInfo)
            ns_S3_generics <- ns_S3_methods_db[, 1L]
            ns_S3_methods <- ns_S3_methods_db[, 3L]
        }

    }

    ## Find the function objects in the given package.
    functions_in_code <-
        Filter(function(f) is.function(get(f, envir = code_env)),
               objects_in_code)

    ## Find all S3 generics "as seen from the package".
    all_S3_generics <-
        unique(c(Filter(function(f) .is_S3_generic(f, envir = code_env),
                        functions_in_code),
                 .get_S3_generics_as_seen_from_package(dir,
                                                       !missing(package),
                                                       TRUE),
                 .get_S3_group_generics()))
    ## <FIXME>
    ## Not yet:
    code_env <- .make_S3_group_generic_env(parent = code_env)
    ## </FIXME>

    ## Find all methods in the given package for the generic functions
    ## determined above.  Store as a list indexed by the names of the
    ## generic functions.
    methods_stop_list <- .make_S3_methods_stop_list(basename(dir))
    methods_in_package <- sapply(all_S3_generics, function(g) {
        if(!exists(g, envir = code_env)) return(character())
        ## <FIXME>
        ## We should really determine the name g dispatches for, see
        ## a current version of methods() [2003-07-07].  (Care is needed
        ## for internal generics and group generics.)
        ## Matching via grep() is tricky with e.g. a '$' in the name of
        ## the generic function ... hence substr().
        name <- paste(g, ".", sep = "")
        methods <-
            functions_in_code[substr(functions_in_code, 1L,
                                     nchar(name, type="c")) == name]
        ## </FIXME>
        methods <- methods %w/o% methods_stop_list
        if(has_namespace) {
            ## Find registered methods for generic g.
            methods <- c(methods, ns_S3_methods[ns_S3_generics == g])
        }
        methods
    })
    all_methods_in_package <- unlist(methods_in_package)
    ## There are situations where S3 methods might be documented as
    ## functions (i.e., with their full name), if they do something
    ## useful also for arguments not inheriting from the class they
    ## provide a method for.  Let's allow for this in the case the
    ## package has a namespace and the method is exported (even though
    ## we strongly prefer using FOO(as.BAR(x)) to FOO.BAR(x) for such
    ## cases).
    if(has_namespace)
        all_methods_in_package <-
            all_methods_in_package %w/o% functions_in_code

    db <- if(!missing(package))
        Rd_db(package, lib.loc = dirname(dir))
    else
        Rd_db(dir = dir)

    names(db) <- db_names <- .Rd_get_names_from_Rd_db(db)

    ## Ignore pkg-deprecated.Rd and pkg-defunct.Rd.
    ind <- db_names %in% paste(package_name, c("deprecated", "defunct"),
                               sep = "-")
    db <- db[!ind]
    db_names <- db_names[!ind]

    db_usages <-
        lapply(db,
               function(Rd) {
                   Rd <- .Rd_get_section(Rd, "usage")
                   .parse_usage_as_much_as_possible(Rd)
               })
    ind <- as.logical(sapply(db_usages,
                             function(x) !is.null(attr(x, "bad_lines"))))
    bad_lines <- lapply(db_usages[ind], attr, "bad_lines")

    bad_doc_objects <- list()

    for(docObj in db_names) {

        ## Determine function names in the \usage.
        exprs <- db_usages[[docObj]]
        exprs <- exprs[sapply(exprs, length) > 1L]
        ## Ordinary functions.
        functions <-
            as.character(sapply(exprs,
                                function(e) as.character(e[[1L]])))
        ## (Note that as.character(sapply(exprs, "[[", 1L)) does not do
        ## what we want due to backquotifying.)
        ## Replacement functions.
        ind <- as.logical(sapply(exprs,
                                 .is_call_from_replacement_function_usage))
        if(any(ind)) {
            replace_funs <-
                paste(sapply(exprs[ind],
                             function(e) as.character(e[[2L]][[1L]])),
                      "<-",
                      sep = "")
            functions <- c(functions, replace_funs)
        }

        methods_with_full_name <-
            intersect(functions, all_methods_in_package)

        functions <- .transform_S3_method_markup(functions)

        methods_with_generic <-
            sapply(intersect(functions, all_S3_generics),
                   function(g)
                   intersect(functions, methods_in_package[[g]]),
                   simplify = FALSE)

        if((length(methods_with_generic)) ||
           (length(methods_with_full_name)))
            bad_doc_objects[[docObj]] <-
                list(withGeneric  = methods_with_generic,
                     withFullName = methods_with_full_name)

    }

    attr(bad_doc_objects, "bad_lines") <- bad_lines
    class(bad_doc_objects) <- "checkDocStyle"
    bad_doc_objects
}

print.checkDocStyle <-
function(x, ...) {
    for(docObj in names(x)) {
        ## <NOTE>
        ## With \method{GENERIC}{CLASS} now being transformed to show
        ## both GENERIC and CLASS info, documenting S3 methods on the
        ## same page as their generic is not necessarily a problem any
        ## more (as one can refer to the generic or the methods in the
        ## documentation, in particular for the primary argument).
        ## Hence, even if we still provide information about this, we
        ## no longer print it by default.  One can still access it via
        ##   lapply(checkDocStyle("foo"), "[[", "withGeneric")
        ## (but of course it does not print that nicely anymore),
        ## </NOTE>
        methods_with_full_name <- x[[docObj]][["withFullName"]]
        if(length(methods_with_full_name)) {
            writeLines(gettextf("S3 methods shown with full name in documentation object '%s':",
                                docObj))
            .pretty_print(methods_with_full_name)
            writeLines("")
        }
    }
    invisible(x)
}

### * checkFF

checkFF <-
function(package, dir, file, lib.loc = NULL,
         verbose = getOption("verbose"))
{
    has_namespace <- FALSE
    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- .find.package(package, lib.loc)
        ## Using package installed in @code{dir} ...
        code_dir <- file.path(dir, "R")
        if(!file_test("-d", code_dir))
            stop(gettextf("directory '%s' does not contain R code",
                          dir),
                 domain = NA)
        if(basename(dir) != "base")
            .load_package_quietly(package, lib.loc)
        code_env <- if(packageHasNamespace(package, dirname(dir))) {
            ce <- asNamespace(package)
            if(exists("DLLs", envir = ce$.__NAMESPACE__.)) {
                DLLs <- get("DLLs", envir = ce$.__NAMESPACE__.)
                has_namespace <- length(DLLs) > 0L
            }
            ce
        } else
            .package_env(package)
    }
    else if(!missing(dir)) {
        ## Using sources from directory @code{dir} ...
        if(!file_test("-d", dir))
            stop(gettextf("directory '%s' does not exist", dir),
                 domain = NA)
        else
            dir <- file_path_as_absolute(dir)
        dfile <- file.path(dir, "DESCRIPTION")
        enc <- if(file.exists(dfile))
            .read_description(dfile)["Encoding"] else NA
        if(file.exists(file.path(dir, "NAMESPACE"))) {
            nm <- parseNamespaceFile(basename(dir), dirname(dir))
            has_namespace <- length(nm$dynlibs)
        }
        code_dir <- file.path(dir, "R")
        if(!file_test("-d", code_dir))
            stop(gettextf("directory '%s' does not contain R code",
                          dir),
                 domain = NA)
        file <- tempfile()
        on.exit(unlink(file))
        if(!file.create(file)) stop("unable to create ", file)
        if(!all(.file_append_ensuring_LFs(file,
                                          list_files_with_type(code_dir,
                                                               "code"))))
            stop("unable to write code files")
    }
    else if(!missing(file))
        enc <- NA
    else
        stop("you must specify 'package', 'dir' or 'file'")

    if(missing(package) && !file_test("-f", file))
        stop(gettextf("file '%s' does not exist", file),
             domain = NA)

    ## Should there really be a 'verbose' argument?
    ## It may be useful to extract all foreign function calls but then
    ## we would want the calls back ...
    ## What we currently do is the following: if 'verbose' is true, we
    ## show all foreign function calls in abbreviated form with the line
    ## ending in either 'OK' or 'MISSING', and we return the list of
    ## 'bad' FF calls (i.e., where the 'PACKAGE' argument is missing)
    ## *invisibly* (so that output is not duplicated).
    ## Otherwise, if not verbose, we return the list of bad FF calls.

    bad_exprs <- list()
    FF_funs <- FF_fun_names <- c(".C", ".Fortran", ".Call", ".External",
                                 ".Call.graphics", ".External.graphics")
    ## As pointed out by DTL, packages could use non-base FF calls for
    ## which missing 'PACKAGE' arguments are not necessarily a problem.
    if(!missing(package)) {
        is_FF_fun_from_base <-
            sapply(FF_funs,
                   function(f) {
                       e <- .find_owner_env(f, code_env)
                       (identical(e, baseenv())
                        || identical(e, .BaseNamespaceEnv))
                   })
        FF_funs <- FF_funs[is_FF_fun_from_base]
    }
    ## Also, need to handle base::.Call() etc ...
    FF_funs <- c(FF_funs, sprintf("base::%s", FF_fun_names))

    find_bad_exprs <- function(e) {
        if(is.call(e) || is.expression(e)) {
            ## <NOTE>
            ## This picks up all calls, e.g. a$b, and they may convert
            ## to a vector.  The function is the first element in all
            ## the calls we are interested in.
            ## BDR 2002-11-28
            ## </NOTE>
            if(deparse(e[[1L]])[1L] %in% FF_funs) {
                if(!is.character(e[[2L]])) parg <- "Called with symbol"
                else {
                    parg <- e[["PACKAGE"]]
                    parg <- if(!is.null(parg) && (parg != "")) "OK"
                    else if(!has_namespace) {
                        bad_exprs <<- c(bad_exprs, e)
                        "MISSING"
                    } else "MISSING but in a function in a namespace"
                }
                if(verbose)
                    cat(deparse(e[[1L]]), "(", deparse(e[[2L]]),
                        ", ...): ", parg, "\n", sep = "")
            }
            for(i in seq_along(e)) Recall(e[[i]])
        }
    }

    if(!missing(package)) {
        exprs <- lapply(ls(envir = code_env, all.names = TRUE),
                        function(f) {
                            f <- get(f, envir = code_env)
                            if(typeof(f) == "closure")
                                body(f)
                            else
                                NULL
                        })
        if(.isMethodsDispatchOn()) {
            ## Also check the code in S4 methods.
            ## This may find things twice if a setMethod() with a bad FF
            ## call is from inside a function (e.g., InitMethods()).
            for(f in get_S4_generics_with_methods(code_env)) {
                mlist <- .get_S4_methods_list(f, code_env)
                exprs <- c(exprs, lapply(mlist, body))
            }
        }
    }
    else {
        if(!is.na(enc) &&
           !(Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX"))) {
	    con <- file(file, encoding=enc)
            on.exit(close(con))
	} else con <- file
        exprs <-
            tryCatch(parse(file = con, n = -1L),
                     error = function(e)
                     stop(gettextf("parse error in file '%s':\n%s",
                                   file,
                                   .massage_file_parse_error_message(conditionMessage(e))),
                               domain = NA, call. = FALSE))
    }
    for(i in seq_along(exprs)) find_bad_exprs(exprs[[i]])
    class(bad_exprs) <- "checkFF"
    if(verbose)
        invisible(bad_exprs)
    else
        bad_exprs
}

print.checkFF <-
function(x, ...)
{
    if(length(x)) {
        writeLines(gettextf("Foreign function calls without 'PACKAGE' argument:"))
        for(i in seq_along(x)) {
            writeLines(paste(deparse(x[[i]][[1L]]),
                             "(",
                             deparse(x[[i]][[2L]]),
                             ", ...)",
                             sep = ""))
        }
    }
    invisible(x)
}

### * checkS3methods

checkS3methods <-
function(package, dir, lib.loc = NULL)
{
    has_namespace <- FALSE
    ## If an installed package has a namespace, we need to record the S3
    ## methods which are registered but not exported (so that we can
    ## get() them from the right place).
    S3_reg <- character(0L)

    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- .find.package(package, lib.loc)
        ## Using package installed in @code{dir} ...
        code_dir <- file.path(dir, "R")
        if(!file_test("-d", code_dir))
            stop(gettextf("directory '%s' does not contain R code",
                          dir),
                 domain = NA)
        is_base <- basename(dir) == "base"

        ## Load package into code_env.
        if(!is_base)
            .load_package_quietly(package, lib.loc)
        code_env <- .package_env(package)

        objects_in_code <- objects(envir = code_env, all.names = TRUE)

        ## Does the package have a namespace?
        if(packageHasNamespace(package, dirname(dir))) {
            has_namespace <- TRUE
            ## Determine names of declared S3 methods and associated S3
            ## generics.
            ns_S3_methods_db <- getNamespaceInfo(package, "S3methods")
            ns_S3_generics <- ns_S3_methods_db[, 1L]
            ns_S3_methods <- ns_S3_methods_db[, 3L]
            ## Determine unexported but declared S3 methods.
            S3_reg <- ns_S3_methods %w/o% objects_in_code
        }
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
        code_dir <- file.path(dir, "R")
        if(!file_test("-d", code_dir))
            stop(gettextf("directory '%s' does not contain R code",
                          dir),
                 domain = NA)
        is_base <- basename(dir) == "base"

        code_env <- new.env()
        dfile <- file.path(dir, "DESCRIPTION")
        meta <- if(file_test("-f", dfile))
            .read_description(dfile)
        else
            character()
        .source_assignments_in_code_dir(code_dir, code_env, meta)
        sys_data_file <- file.path(code_dir, "sysdata.rda")
        if(file_test("-f", sys_data_file)) load(sys_data_file, code_env)

        objects_in_code <- objects(envir = code_env, all.names = TRUE)

        ## Does the package have a NAMESPACE file?  Note that when
        ## working on the sources we (currently?) cannot deal with the
        ## (experimental) alternative way of specifying the namespace.
        if(file.exists(file.path(dir, "NAMESPACE"))) {
            has_namespace <- TRUE
            nsInfo <- parseNamespaceFile(basename(dir), dirname(dir))
            ## Determine exported objects.
            OK <- intersect(objects_in_code, nsInfo$exports)
            for(p in nsInfo$exportPatterns)
                OK <- c(OK, grep(p, objects_in_code, value = TRUE))
            objects_in_code <- unique(OK)
            ## Determine names of declared S3 methods and associated S3
            ## generics.
            ns_S3_methods_db <- .get_namespace_S3_methods_db(nsInfo)
            ns_S3_generics <- ns_S3_methods_db[, 1L]
            ns_S3_methods <- ns_S3_methods_db[, 3L]
        }

    }

    ## Find the function objects in the given package.
    functions_in_code <-
        Filter(function(f) is.function(get(f, envir = code_env)),
               objects_in_code)

    ## This is the virtual groyp generics, not the members
    S3_group_generics <- .get_S3_group_generics()
    ## This includes the primitive group generics as from R 2.6.0
    S3_primitive_generics <- .get_S3_primitive_generics()

    checkArgs <- function(g, m) {
        ## Do the arguments of method m (in code_env) 'extend' those of
        ## the generic g as seen from code_env?  The method must have all
        ## arguments the generic has, with positional arguments of g in
        ## the same positions for m.
        ## Exception: '...' in the method swallows anything.
        genfun <- get(g, envir = code_env)
        gArgs <- names(formals(genfun))
        if(g == "plot") gArgs <- gArgs[-2L]
        ogArgs <- gArgs
        gm <- if(m %in% S3_reg) {
            ## See registerS3method() in namespace.R.
            defenv <-
                if (g %in% S3_group_generics || g %in% S3_primitive_generics)
                    .BaseNamespaceEnv
                else {
                    if(.isMethodsDispatchOn()
                       && methods::is(genfun, "genericFunction"))
                        genfun <-
                            methods::slot(genfun, "default")@methods$ANY
                    if (typeof(genfun) == "closure") environment(genfun)
                    else .BaseNamespaceEnv
                }
            if(!exists(".__S3MethodsTable__.", envir = defenv,
                       inherits = FALSE)) {
                ## Happens e.g. if for some reason, we get "plot" as
                ## standardGeneric for "plot" defined from package
                ## "graphics" with its own environment which does not
                ## contain an S3 methods table ...
                return(NULL)
            }
            S3Table <- get(".__S3MethodsTable__.", envir = defenv,
                           inherits = FALSE)
            if(!exists(m, envir = S3Table)) {
                warning(gettextf("declared S3 method '%s' not found",
                                 m),
                        domain = NA,
                        call. = FALSE)
                return(NULL)
            } else get(m, envir = S3Table)
        } else get(m, envir = code_env)
        mArgs <- omArgs <- names(formals(gm))
        ## If m is a formula method, its first argument *may* be called
        ## formula.  (Note that any argument name mismatch throws an
        ## error in current S-PLUS versions.)
        if(length(grep("\\.formula$", m))) {
            if(gArgs[1L] != "...") gArgs <- gArgs[-1L]
            mArgs <- mArgs[-1L]
        }
        dotsPos <- which(gArgs == "...")
        ipos <- if(length(dotsPos))
            seq.int(from = 1L, length.out = dotsPos[1L] - 1L)
        else
            seq_along(gArgs)

        ## careful, this could match multiply in incorrect funs.
        dotsPos <- which(mArgs == "...")
        if(length(dotsPos))
            ipos <- ipos[seq.int(from = 1L, length.out = dotsPos[1L] - 1L)]
        posMatchOK <- identical(gArgs[ipos], mArgs[ipos])
        argMatchOK <- all(gArgs %in% mArgs) || length(dotsPos) > 0L
        margMatchOK <- all(mArgs %in% c("...", gArgs)) || "..." %in% ogArgs
        if(posMatchOK && argMatchOK && margMatchOK)
            NULL
        else if (g %in% c("+", "-", "*", "/", "^", "%%", "%/%", "&", "|",
                          "!", "==", "!=", "<", "<=", ">=", ">")
                 && (length(ogArgs) == length(omArgs)) )
            NULL
        else {
            l <- list(ogArgs, omArgs)
            names(l) <- c(g, m)
            list(l)
        }
    }

    all_S3_generics <-
        unique(c(Filter(function(f) .is_S3_generic(f, envir = code_env),
                        functions_in_code),
                 .get_S3_generics_as_seen_from_package(dir,
                                                       !missing(package),
                                                       FALSE),
                 S3_group_generics, S3_primitive_generics))
    ## <FIXME>
    ## Not yet:
    code_env <- .make_S3_group_generic_env(parent = code_env)
    ## </FIXME>
    code_env <- .make_S3_primitive_generic_env(parent = code_env)

    ## Now determine the 'bad' methods in the function objects of the
    ## package.
    bad_methods <- list()
    methods_stop_list <- .make_S3_methods_stop_list(basename(dir))
    for(g in all_S3_generics) {
        if(!exists(g, envir = code_env)) next
        ## Find all methods in functions_in_code for S3 generic g.
        ## <FIXME>
        ## We should really determine the name g dispatches for, see
        ## a current version of methods() [2003-07-07].  (Care is
        ## needed for internal generics and group generics.)
        ## Matching via grep() is tricky with e.g. a '$' in the name
        ## of the generic function ... hence substr().
        name <- paste(g, ".", sep = "")
        methods <-
            functions_in_code[substr(functions_in_code, 1L,
                                     nchar(name, type="c")) == name]
        ## </FIXME>
        methods <- methods %w/o% methods_stop_list
        if(has_namespace) {
            ## Find registered methods for generic g.
            methods <- c(methods, ns_S3_methods[ns_S3_generics == g])
        }

        for(m in methods)
            ## Both all() and all.equal() are generic.
            bad_methods <- if(g == "all") {
                m1 <- m[-grep("^all\\.equal", m)]
                c(bad_methods, if(length(m1)) checkArgs(g, m1))
            } else c(bad_methods, checkArgs(g, m))
    }

    class(bad_methods) <- "checkS3methods"
    bad_methods
}

print.checkS3methods <-
function(x, ...)
{
    format_args <- function(s)
        paste("function(", paste(s, collapse = ", "), ")", sep = "")
    for(entry in x) {
        writeLines(c(paste(names(entry)[1L], ":", sep = ""),
                     strwrap(format_args(entry[[1L]]),
                             indent = 2L, exdent = 11L),
                     paste(names(entry)[2L], ":", sep = ""),
                     strwrap(format_args(entry[[2L]]),
                             indent = 2L, exdent = 11L),
                     ""))
    }
    invisible(x)
}

### * checkReplaceFuns

checkReplaceFuns <-
function(package, dir, lib.loc = NULL)
{
    has_namespace <- FALSE

    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- .find.package(package, lib.loc)
        ## Using package installed in @code{dir} ...
        code_dir <- file.path(dir, "R")
        if(!file_test("-d", code_dir))
            stop(gettextf("directory '%s' does not contain R code",
                          dir),
                 domain = NA)
        is_base <- basename(dir) == "base"

        ## Load package into code_env.
        if(!is_base)
            .load_package_quietly(package, lib.loc)
        ## In case the package has a namespace, we really want to check
        ## all replacement functions in the package.  (If not, we need
        ## to change the code for the non-installed case to only look at
        ## exported (replacement) functions.)
        if(packageHasNamespace(package, dirname(dir))) {
            has_namespace <- TRUE
            code_env <- asNamespace(package)
            ns_S3_methods_db <- getNamespaceInfo(package, "S3methods")
        }
        else
            code_env <- .package_env(package)
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
        code_dir <- file.path(dir, "R")
        if(!file_test("-d", code_dir))
            stop(gettextf("directory '%s' does not contain R code",
                          dir),
                 domain = NA)
        is_base <- basename(dir) == "base"

        code_env <- new.env()
        dfile <- file.path(dir, "DESCRIPTION")
        meta <- if(file_test("-f", dfile))
            .read_description(dfile)
        else
            character()
        .source_assignments_in_code_dir(code_dir, code_env, meta)
        sys_data_file <- file.path(code_dir, "sysdata.rda")
        if(file_test("-f", sys_data_file)) load(sys_data_file, code_env)

        ## Does the package have a NAMESPACE file?  Note that when
        ## working on the sources we (currently?) cannot deal with the
        ## (experimental) alternative way of specifying the namespace.
        if(file.exists(file.path(dir, "NAMESPACE"))) {
            has_namespace <- TRUE
            nsInfo <- parseNamespaceFile(basename(dir), dirname(dir))
            ns_S3_methods_db <- .get_namespace_S3_methods_db(nsInfo)
        }
    }

    objects_in_code <- objects(envir = code_env, all.names = TRUE)
    replace_funs <- character()

    if(has_namespace) {
        ns_S3_generics <- ns_S3_methods_db[, 1L]
        ns_S3_methods <- ns_S3_methods_db[, 3L]
        ## S3 replacement methods from namespace registration?
        idx <- grep("<-$", ns_S3_generics)
        if(length(idx)) replace_funs <- ns_S3_methods[idx]
        ## Now remove the functions registered as S3 methods.
        objects_in_code <- objects_in_code %w/o% ns_S3_methods
    }

    replace_funs <-
        c(replace_funs, grep("<-", objects_in_code, value = TRUE))

    .check_last_formal_arg <- function(f) {
        arg_names <- names(formals(f))
        if(!length(arg_names))
            TRUE                        # most likely a .Primitive()
        else
            identical(arg_names[length(arg_names)], "value")
    }

    ## Find the replacement functions (which have formal arguments) with
    ## last arg not named 'value'.
    bad_replace_funs <- if(length(replace_funs)) {
        Filter(function(f) {
                   ## Always get the functions from code_env ...
                   ## Should maybe get S3 methods from the registry ...
                   f <- get(f, envir = code_env)
                   if(!is.function(f)) return(FALSE)
                   ! .check_last_formal_arg(f)
               },
               replace_funs)
    } else character(0L)

    if(.isMethodsDispatchOn()) {
        S4_generics <- get_S4_generics_with_methods(code_env)
        ## Assume that the ones with names ending in '<-' are always
        ## replacement functions.
        S4_generics <- grep("<-$", S4_generics, value = TRUE)
        bad_S4_replace_methods <-
            sapply(S4_generics,
                   function(f) {
                       mlist <- .get_S4_methods_list(f, code_env)
                       ind <- !as.logical(sapply(mlist,
                                                 .check_last_formal_arg))
                       if(!any(ind))
                           character()
                       else {
                           sigs <- .make_siglist(mlist[ind])
                           sprintf("\\S4method{%s}{%s}", f, sigs)
                       }
                   })
        bad_replace_funs <-
            c(bad_replace_funs,
              unlist(bad_S4_replace_methods, use.names = FALSE))
    }

    class(bad_replace_funs) <- "checkReplaceFuns"
    bad_replace_funs
}

print.checkReplaceFuns <-
function(x, ...)
{
    if(length(x)) .pretty_print(unclass(x))
    invisible(x)
}

### * checkTnF

checkTnF <-
function(package, dir, file, lib.loc = NULL)
{
    code_files <- docs_files <- character(0L)

    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        ## Using package installed in @code{dir} ...
        dir <- .find.package(package, lib.loc)
        if(file.exists(file.path(dir, "R", "all.rda"))) {
            warning("cannot check R code installed as image")
        }
        code_file <- file.path(dir, "R", package)
        if(file.exists(code_file))      # could be data-only
            code_files <- code_file
        example_dir <- file.path(dir, "R-ex")
        if(file_test("-d", example_dir)) {
            code_files <- c(code_files,
                            list_files_with_exts(example_dir, "R"))
        }
    }
    else if(!missing(dir)) {
        ## Using sources from directory @code{dir} ...
        if(!file_test("-d", dir))
            stop(gettextf("directory '%s' does not exist", dir),
                 domain = NA)
        else
            dir <- file_path_as_absolute(dir)
        code_dir <- file.path(dir, "R")
        if(file_test("-d", code_dir))   # could be data-only
            code_files <- list_files_with_type(code_dir, "code")
        docs_dir <- file.path(dir, "man")
        if(file_test("-d", docs_dir))
            docs_files <- list_files_with_type(docs_dir, "docs")
    }
    else if(!missing(file)) {
        if(!file_test("-f", file))
            stop(gettextf("file '%s' does not exist", file),
                 domain = NA)
        else
            code_files <- file
    }
    else
        stop("you must specify 'package', 'dir' or 'file'")

    find_TnF_in_code <- function(file, txt) {
        ## If 'txt' is given, it contains the extracted examples from
        ## the R documentation file 'file'.  Otherwise, 'file' gives a
        ## file with (just) R code.
        matches <- list()
        TnF <- c("T", "F")
        find_bad_exprs <- function(e, p) {
            if(is.name(e)
               && (as.character(e) %in% TnF)
               && !is.null(p)) {
                ## Need the 'list()' to deal with T/F in function
                ## arglists which are pairlists ...
                matches <<- c(matches, list(p))
            }
            else if(is.recursive(e)) {
                for(i in seq_along(e)) Recall(e[[i]], e)
            }
        }
        exprs <- if(missing(txt))
            tryCatch(parse(file = file, n = -1L),
                     error = function(e)
                     stop(gettextf("parse error in file '%s':\n",
                                   file,
                                   .massage_file_parse_error_message(conditionMessage(e))),
                          domain = NA, call. = FALSE))
        else
            tryCatch(parse(text = txt),
                     error = function(e)
                     stop(gettextf("parse error in examples from file '%s':\n",
                                   file, conditionMessage(e)),
                          domain = NA, call. = FALSE))
        for(i in seq_along(exprs))
            find_bad_exprs(exprs[[i]], NULL)
        matches
    }

    bad_exprs <- list()
    for(file in code_files) {
        exprs <- find_TnF_in_code(file)
        if(length(exprs)) {
            exprs <- list(exprs)
            names(exprs) <- file
            bad_exprs <- c(bad_exprs, exprs)
        }
    }
    for(file in docs_files) {
        Rd <- prepare_Rd(file, defines = .Platform$OS.type)
        txt <- .Rd_get_example_code(Rd)
        exprs <- find_TnF_in_code(file, txt)
        if(length(exprs)) {
            exprs <- list(exprs)
            names(exprs) <- file
            bad_exprs <- c(bad_exprs, exprs)
        }
    }
    class(bad_exprs) <- "checkTnF"
    bad_exprs
}

print.checkTnF <-
function(x, ...)
{
    for(fname in names(x)) {
        writeLines(gettextf("File '%s':", fname))
        xfname <- x[[fname]]
        for(i in seq_along(xfname)) {
            writeLines(strwrap(gettextf("found T/F in %s",
                                        paste(deparse(xfname[[i]]),
                                              collapse = "")),
                               exdent = 4L))
        }
        writeLines("")
    }
    invisible(x)
}

### * .check__depends

## changed in 2.3.0 to refer to a source dir.

.check_package_depends <-
function(dir)
{
    if(length(dir) != 1L)
        stop("argument 'package' must be of length 1")

    ## We definitely need a valid DESCRIPTION file.
    db <- .read_description(file.path(dir, "DESCRIPTION"))

    dir_name <- basename(dir)
    package_name <- db["Package"]
    if(!identical(package_name, dir_name) &&
       (!is.character(package_name) || !nzchar(package_name))) {
	message(sprintf(
	"package name '%s' seems invalid; using directory name '%s' instead",
			package_name, dir_name))
	package_name <- dir_name
    }
    ldepends <- .get_requires_with_version_from_package_db(db, "Depends")
    limports <- .get_requires_with_version_from_package_db(db, "Imports")
    lsuggests <- .get_requires_with_version_from_package_db(db, "Suggests")
    depends <- sapply(ldepends, `[[`, 1L)
    imports <- sapply(limports, `[[`, 1L)
    suggests <- sapply(lsuggests, `[[`, 1L)
    ## Need this to handle bundles ...
    contains <- .get_contains_from_package_db(db)

    standard_package_names <- .get_standard_package_names()

    bad_depends <- list()

    ## Are all packages listed in Depends/Suggests/Imports installed?
    ## Need to treat specially the former stub packages.
    lreqs <- c(ldepends,
               limports,
               if(!identical(as.logical(Sys.getenv("_R_CHECK_FORCE_SUGGESTS_")), FALSE)) lsuggests)
    if(length(lreqs)) {
        reqs <- unique(sapply(lreqs, `[[`, 1L))
        ## this was changed for speed.
        ## installed <-  utils::installed.packages()[ , "Package"]
        installed <- character(0L)
        installed_in <- character(0L)
        for(lib in .libPaths()) {
            pkgs <- list.files(lib)
            pkgs <- pkgs[file.access(file.path(lib, pkgs, "DESCRIPTION"), 4) == 0]
            installed <- c(installed, pkgs)
            installed_in <- c(installed_in, rep.int(lib, length(pkgs)))
        }
        installed <- sub("_.*", "", installed) # obsolete versioned installs.
        reqs <- reqs %w/o% installed
        m <- reqs %in% standard_package_names$stubs
        if(length(reqs[!m]))
            bad_depends$required_but_not_installed <- reqs[!m]
        if(length(reqs[m]))
            bad_depends$required_but_stub <- reqs[m]
        ## now check versions
        have_ver <- unlist(lapply(lreqs, function(x) length(x) == 3L))
        lreqs3 <- lreqs[have_ver]
        if(length(lreqs3)) {
            bad <- character(0)
            for (r in lreqs3) {
                pkg <- r[[1L]]
                op <- r[[2L]]
                where <- which(installed == pkg)
                if(!length(where)) next
                ## want the first one
                desc <- .readRDS(file.path(installed_in[where[1L]], pkg,
                                           "Meta", "package.rds"))
                current <- desc$DESCRIPTION["Version"]
                target <- as.package_version(r[[3L]])
                if(eval(parse(text = paste("!(current", op, "target)"))))
                    bad <- c(bad, pkg)
            }
            if(length(bad))
                bad_depends$required_but_obsolete <- bad
        }
    }
    ## Are all vignette dependencies at least suggested or equal to
    ## the package name?
    vignette_dir <- file.path(dir, "inst", "doc")
    if(file_test("-d", vignette_dir)
       && length(list_files_with_type(vignette_dir, "vignette"))) {
        reqs <- unique(unlist(.build_vignette_index(vignette_dir)$Depends))
        ## For the time being, ignore base packages missing from the
        ## DESCRIPTION dependencies even if explicitly given as vignette
        ## dependencies.
        reqs <- reqs %w/o% c(depends, imports, suggests, package_name,
                             standard_package_names$base)
        if(length(reqs))
            bad_depends$missing_vignette_depends <- reqs
    }

    ## Are all namespace dependencies listed as package dependencies?
    if(file_test("-f", file.path(dir, "NAMESPACE"))) {
        reqs <- .get_namespace_package_depends(dir)
        ## <FIXME>
        ## Not clear whether we want to require *all* namespace package
        ## dependencies listed in DESCRIPTION, or e.g. just the ones on
        ## non-base packages.  Do the latter for time being ...
        ## Actually we need to know at least about S4-using packages,
        ## since we need to reinstall if those change.
        allowed_imports <-
            standard_package_names$base %w/o% c("methods", "stats4")
        reqs <- reqs %w/o% c(contains, imports, depends, allowed_imports)
        ## Note that for bundles we currently cannot have package
        ## dependencies different from bundle ones, and clearly a bundle
        ## cannot depend on something it contains ...
        ## </FIXME>
        if(length(reqs))
            bad_depends$missing_namespace_depends <- reqs
    }

    class(bad_depends) <- "check_package_depends"
    bad_depends
}

print.check_package_depends <-
function(x, ...)
{
    if(length(bad <- x$required_but_not_installed)) {
        writeLines(gettext("Packages required but not available:"))
        .pretty_print(bad)
        writeLines("")
    }
    if(length(bad <- x$required_but_obsolete)) {
        writeLines(gettext("Packages required and available but unsuitable version:"))
        .pretty_print(bad)
        writeLines("")
    }
    if(length(bad <- x$required_but_stub)) {
        writeLines(gettext("Former standard packages required but now defunct:"))
        .pretty_print(bad)
        writeLines("")
    }
    if(length(bad <- x$missing_vignette_depends)) {
        writeLines(gettext("Vignette dependencies not required:"))
        .pretty_print(bad)
        msg <- gettext("Vignette dependencies (\\VignetteDepends{} entries) must be contained in the DESCRIPTION Depends/Suggests/Imports entries.")
        writeLines(strwrap(msg))
        writeLines("")
    }
    if(length(bad <- x$missing_namespace_depends)) {
        writeLines(gettext("Namespace dependencies not required:"))
        .pretty_print(bad)
        writeLines("")
    }
    invisible(x)
}


### * .check_package_description

.check_package_description <-
function(dfile)
{
    dfile <- file_path_as_absolute(dfile)
    db <- .read_description(dfile)

    standard_package_names <- .get_standard_package_names()

    valid_package_name_regexp <-
        .standard_regexps()$valid_package_name
    valid_package_version_regexp <-
        .standard_regexps()$valid_package_version

    is_base_package <-
        !is.na(priority <- db["Priority"]) && priority == "base"

    out <- list()                       # For the time being ...

    ## Check encoding-related things first.

    ## All field tags must be ASCII.
    if(any(ind <- !.is_ASCII(names(db))))
        out$fields_with_non_ASCII_tags <- names(db)[ind]
    ## For all fields used by the R package management system, values
    ## must be ASCII as well (so that the RPM works in a C locale).
    ASCII_fields <- c(.get_standard_repository_db_fields(),
                      "Encoding", "License")
    ASCII_fields <- intersect(ASCII_fields, names(db))
    if(any(ind <- !.is_ASCII(db[ASCII_fields])))
        out$fields_with_non_ASCII_values <- ASCII_fields[ind]

    ## Determine encoding and re-encode if necessary and possible.
    if("Encoding" %in% names(db)) {
        encoding <- db["Encoding"]
        if((! Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX")))
            db <- iconv(db, encoding, "")
    }
    else if(!all(.is_ISO_8859(db))) {
        ## No valid Encoding metadata.
        ## Determine whether we can assume Latin1.
        out$missing_encoding <- TRUE
    }

    if(any(is.na(nchar(db, "c", TRUE)))) {
        ## Ouch, invalid in the current locale.
        ## (Can only happen in a MBCS locale.)
        ## Try re-encoding from Latin1.
        db <- iconv(db, "latin1", "")
    }

    ## Mandatory entries in DESCRIPTION:
    ##   Package, Version, License, Description, Title, Author,
    ##   Maintainer.
    required_fields <- c("Package", "Version", "License", "Description",
                         "Title", "Author", "Maintainer")
    if(length(i <- which(is.na(match(required_fields, names(db))) |
                         is.na(db[required_fields]))))
        out$missing_required_fields <- required_fields[i]

    val <- package_name <- db["Package"]
    if(!is.na(val)) {
        tmp <- character()
        if(!grepl(sprintf("^%s$", valid_package_name_regexp), val)
           && !grepl("^Translation-[[:alnum:].]+$", val))
            tmp <- c(tmp, gettext("Malformed package name"))
        if(!is_base_package) {
            if(val %in% standard_package_names$base)
                tmp <- c(tmp,
                         c(gettext("Invalid package name."),
                           gettext("This is the name of a base package.")))
            else if(val %in% standard_package_names$stubs)
                tmp <- c(tmp,
                         c(gettext("Invalid package name."),
                           gettext("This name was used for a base package and is remapped by library().")))
        }
        if(length(tmp))
            out$bad_package <- tmp
    }
    if(!is.na(val <- db["Version"])
       && !is_base_package
       && !grepl(sprintf("^%s$", valid_package_version_regexp), val))
        out$bad_version <- val
    if(!is.na(val <- db["Maintainer"])
       && !grepl(.valid_maintainer_field_regexp, val))
        out$bad_maintainer <- val

    ## Optional entries in DESCRIPTION:
    ##   Depends/Suggests/Imports/Enhances, Namespace, Priority.
    ## These must be correct if present.

    val <- db[match(c("Depends", "Suggests", "Imports", "Enhances"),
                    names(db), nomatch = 0L)]
    if(length(val)) {
        depends <- .strip_whitespace(unlist(strsplit(val, ",")))
        bad_dep_entry <- bad_dep_op <- bad_dep_version <- character()
        dep_regexp <-
            paste("^[[:space:]]*",
                  paste("(", valid_package_name_regexp, ")", sep = ""),
                  "([[:space:]]*\\(([^) ]+)[[:space:]]+([^) ]+)\\))?",
                  "[[:space:]]*$",
                  sep = "")
        for(dep in depends) {
            if(!grepl(dep_regexp, dep)) {
                ## Entry does not match the regexp.
                bad_dep_entry <- c(bad_dep_entry, dep)
                next
            }
            if(nzchar(sub(dep_regexp, "\\2", dep))) {
                ## If not just a valid package name ...
                if(!sub(dep_regexp, "\\3", dep) %in%
                   c("<=", ">=", "<", ">", "==", "!="))
                    bad_dep_op <- c(bad_dep_op, dep)
                else if(!grepl(sprintf("^%s$",
                                       valid_package_version_regexp),
                               sub(dep_regexp, "\\4", dep)))
                    bad_dep_version <- c(bad_dep_version, dep)
            }
        }
        if(length(c(bad_dep_entry, bad_dep_op, bad_dep_version)))
            out$bad_depends_or_suggests_or_imports <-
                list(bad_dep_entry = bad_dep_entry,
                     bad_dep_op = bad_dep_op,
                     bad_dep_version = bad_dep_version)
    }
    if(!is.na(val <- db["Namespace"])
       && !is.na(package_name)
       && (val != package_name))
        out$bad_namespace <- val
    if(!is.na(val <- db["Priority"])
       && !is.na(package_name)
       && (tolower(val) %in% c("base", "recommended", "defunct-base"))
       && !(package_name %in% unlist(standard_package_names)))
        out$bad_priority <- val

    class(out) <- "check_package_description"

    out
}

print.check_package_description <-
function(x, ...)
{
    if(length(x$missing_encoding))
        writeLines(c(gettext("Unknown encoding"), ""))
    if(length(x$fields_with_non_ASCII_tags)) {
        writeLines(gettext("Fields with non-ASCII tags:"))
        .pretty_print(x$fields_with_non_ASCII_tags)
        writeLines(c(gettext("All field tags must be ASCII."), ""))
    }
    if(length(x$fields_with_non_ASCII_values)) {
        writeLines(gettext("Fields with non-ASCII values:"))
        .pretty_print(x$fields_with_non_ASCII_values)
        writeLines(c(gettext("These fields must have ASCII values."), ""))
    }
    if(length(x$missing_required_fields)) {
        writeLines(gettext("Required fields missing:"))
        .pretty_print(x$missing_required_fields)
        writeLines("")
    }
    if(length(x$bad_package))
        writeLines(c(strwrap(x$bad_package), ""))
    if(length(x$bad_version))
        writeLines(c(gettext("Malformed package version."), ""))
    if(length(x$bad_maintainer))
        writeLines(c(gettext("Malformed maintainer field."), ""))

    if(any(as.integer(sapply(x$bad_depends_or_suggests_or_imports, length)) > 0L )) {
        bad <- x$bad_depends_or_suggests_or_imports
        writeLines(gettext("Malformed Depends or Suggests or Imports or Enhances field."))
        if(length(bad$bad_dep_entry)) {
            tmp <- c(gettext("Offending entries:"),
                     paste(" ", bad$bad_dep_entry),
                     strwrap(gettextf("Entries must be names of packages optionally followed by '<=' or '>=', white space, and a valid version number in parentheses.")))
            writeLines(tmp)
        }
        if(length(bad$bad_dep_op)) {
            tmp <- c(gettext("Entries with infeasible comparison operator:"),
                     paste(" ", bad$bad_dep_entry),
                     strwrap(gettextf("Only operators '<=' and '>=' are possible.")))

            writeLines(tmp)
        }
        if(length(bad$bad_dep_version)) {
            tmp <- c(gettext("Entries with infeasible version number:"),
                     paste(" ", bad$bad_dep_version),
                     strwrap(gettextf("Version numbers must be sequences of at least two non-negative integers, separated by single '.' or '-'.")))
            writeLines(tmp)
        }
        writeLines("")
    }
    if(length(x$bad_namespace))
        writeLines(c(gettext("Package name and namespace differ."), ""))
    if(length(x$bad_priority))
        writeLines(c(gettext("Invalid Priority field."),
                     strwrap(gettextf("Packages with priorities 'base' or 'recommended' or 'defunct-base' must already be known to R.")),
                     ""))

    if(any(as.integer(sapply(x, length)) > 0L))
        writeLines(c(strwrap(gettextf("See the information on DESCRIPTION files in section 'Creating R packages' of the 'Writing R Extensions' manual.")),
                     ""))

    invisible(x)
}

### * .check_package_description_encoding

.check_package_description_encoding <-
function(dfile)
{
    dfile <- file_path_as_absolute(dfile)
    db <- .read_description(dfile)
    out <- list()

    ## Check encoding-related things.

    ## All field tags must be ASCII.
    if(any(ind <- !.is_ASCII(names(db))))
        out$fields_with_non_ASCII_tags <- names(db)[ind]

    if(! "Encoding" %in% names(db)) {
        ind <- !.is_ASCII(db)
        if(any(ind)) {
            out$missing_encoding <- TRUE
            out$fields_with_non_ASCII_values <- names(db)[ind]
        }
    } else {
        enc <- db[["Encoding"]]
        if (! enc %in% c("latin1", "latin2", "UTF-8"))
            out$non_portable_encoding <- enc
    }

    class(out) <- "check_package_description_encoding"
    out
}

print.check_package_description_encoding <-
function(x, ...)
{
    if(length(x$non_portable_encoding))
       writeLines(c(gettextf("Encoding '%s' is not portable",
                             x$non_portable_encoding),
                    ""))
    if(length(x$missing_encoding))
        writeLines(gettext("Unknown encoding with non-ASCII data"))
    if(length(x$fields_with_non_ASCII_tags)) {
        writeLines(gettext("Fields with non-ASCII tags:"))
        .pretty_print(x$fields_with_non_ASCII_tags)
        writeLines(c(gettext("All field tags must be ASCII."), ""))
    }
    if(length(x$fields_with_non_ASCII_values)) {
        writeLines(gettext("Fields with non-ASCII values:"))
        .pretty_print(x$fields_with_non_ASCII_values)
    }
    if(any(as.integer(sapply(x, length)) > 0L))
        writeLines(c(strwrap(gettextf("See the information on DESCRIPTION files in section 'Creating R packages' of the 'Writing R Extensions' manual.")),
                     ""))

    invisible(x)
}

###

.check_package_license <-
function(dfile, dir)
{
    dfile <- file_path_as_absolute(dfile)
    db <- .read_description(dfile)

    if(missing(dir))
        dir <- dirname(dfile)

    ## Analyze the license information here.
    ## Cannot easily do this in .check_package_description(), as R CMD
    ## check's R::Utils::check_package_description() takes any output
    ## from this as indication of an error.

    out <- list()
    if(!is.na(val <- db["License"])) {
        ## If there is no License field, .check_package_description()
        ## will give an error.
        status <- analyze_license(val)
        ok <- status$is_canonical
        ## This analyzes the license specification but does not verify
        ## whether pointers exist, so let us do this here.
        if(length(pointers <- status$pointers)) {
            bad_pointers <-
                pointers[!file_test("-f", file.path(dir, pointers))]
            if(length(bad_pointers)) {
                status$bad_pointers <- bad_pointers
                ok <- FALSE
            }
        }
        ## Could always return the analysis results and not print them
        ## if ok, but it seems more standard to only return trouble.
        if(!ok)
            out <- c(list(license = val), status)
    }

    class(out) <- "check_package_license"
    out
}

print.check_package_license <-
function(x, ...)
{
    if(length(x)) {
        check <- Sys.getenv("_R_CHECK_LICENSE_")
        check <- if(check %in% c("maybe", ""))
            !(x$is_standardizable) || length(x$bad_pointers)
        else
            isTRUE(as.logical(check))
        if(check) {
            if(!(x$is_canonical))
                writeLines(c(gettext("Non-standard license specification:"),
                             strwrap(x$license, indent = 2L, exdent = 2L),
                             gettextf("Standardizable: %s",
                                      x$is_standardizable),
                             if(x$is_standardizable)
                             c(gettext("Standardized license specification:"),
                               strwrap(x$standardization,
                                       indent = 2L, exdent = 2L))))
            if(length(x$bad_pointers))
                writeLines(gettextf("Invalid license file pointers: %s",
                                    paste(x$bad_pointers, collapse = " ")))
        }
    }
    invisible(x)
}

### * .check_make_vars

.check_make_vars <-
function(dir)
{

    bad_flags <- list()
    class(bad_flags) <- "check_make_vars"

    paths <- file.path(dir, c("Makevars.in", "Makevars"))
    paths <- paths[file_test("-f", paths)]
    if(!length(paths)) return(bad_flags)
    mfile <- paths[1L]
    make <- Sys.getenv("MAKE")
    if(make == "") make <- "make"

    lines <-
        tryCatch(system(sprintf("%s -f %s -f %s",
                                make,
                                shQuote(mfile),
                                shQuote(file.path(R.home("share"),
                                                  "make", "check.mk"))),
                        intern = TRUE, ignore.stderr = TRUE),
                 error = identity)
    if(!length(lines) || inherits(lines, "error"))
        return(bad_flags)

    ## Try to be careful ...
    pkg_flags_re <- "^PKG_(CPP|C|CXX|F|FC|OBJC|OBJCCXX)FLAGS: "
    lines <- lines[grepl(pkg_flags_re, lines)]
    names <- sub(":.*", "", lines)
    lines <- sub(pkg_flags_re, "", lines)
    flags <- strsplit(lines, "[[:space:]]+")
    ## Bad flags:
    ##   -O*
    ##      (BDR: for example Sun Fortran compilers used to accept -O
    ##      but not -O2, and VC++ accepts -Ox (literal x) but not -O.)
    ##   -Wall -pedantic -ansi -traditional -std* -f* -m* [GCC]
    ##   -x [Solaris]
    ##   -q [AIX]
    ## It is hard to think of anything apart from -I* and -D* that is
    ## safe for general use ...
    bad_flags_regexp <-
        sprintf("^-(%s)$",
                paste(c("O.*",
                        "Wall", "ansi", "pedantic", "traditional",
                        "f.*", "m.*", "std.*",
                        "x",
                        "q"),
                      collapse = "|"))
    for(i in seq_along(lines)) {
        bad <- grep(bad_flags_regexp, flags[[i]], value = TRUE)
        if(length(bad))
            bad_flags <- c(bad_flags,
                           structure(list(bad), names = names[i]))
    }

    class(bad_flags) <- "check_make_vars"
    bad_flags
}

print.check_make_vars <-
function(x, ...)
{
    if(length(x)) {
        for(i in seq_along(x)) {
            writeLines(c(gettextf("Non-portable flags in variable '%s':",
                                  names(x)[i]),
                         sprintf("  %s", paste(x[[i]], collapse = " "))))
        }
    }
    invisible(x)
}

### * .check_code_usage_in_package

.check_code_usage_in_package <-
function(package, lib.loc = NULL)
{
    is_base <- package == "base"
    if(!is_base) {
        .load_package_quietly(package, lib.loc)

        .eval_with_capture({
            ## avoid warnings about code in other packages the package
            ## uses
            desc <- .readRDS(file.path(.find.package(package, NULL),
                                       "Meta", "package.rds"))
            pkgs1 <- sapply(desc$Suggests, "[[", "name")
            pkgs2 <- sapply(desc$Enhances, "[[", "name")
            for(pkg in unique(c(pkgs1, pkgs2)))
                ## tcltk warns if no DISPLAY variable
		##, errors if not compiled in
                suppressWarnings(suppressMessages(try(require(pkg,
                                                              character.only = TRUE,
                                                              quietly = TRUE),
                                                      silent = TRUE)))
        }, type = "output")

        runif(1) # create .Random.seed
        compat <- new.env(hash=TRUE)
        if(.Platform$OS.type != "unix") {
            assign("nsl", function(hostname) {}, envir = compat)
            assign("X11Font", function(font) {}, envir = compat)
            assign("X11Fonts", function(...) {}, envir = compat)
            assign("cairo_pdf",
                   function(filename =
                            if (onefile) "Rplots.pdf" else "Rplot%03d.pdf",
                            width = 7, height = 7, pointsize = 12,
                            onefile = FALSE, bg = "white", antialias) {},
                   envir = compat)
            assign("quartz",
                   function(display = "", width = 5, height = 5,
                            pointsize = 12, family = "Helvetica",
                            antialias = TRUE, autorefresh = TRUE) {},
                   envir = compat)
            assign("quartzFont", function(font) {}, envir = compat)
            assign("quartzFonts", function(...) {}, envir = compat)
        }
        if(.Platform$OS.type != "windows") {
            assign("bringToTop", function (which = dev.cur(), stay = FALSE) {},
                   envir = compat)
            assign("choose.dir",
                   function (default = "", caption = "Select folder") {},
                   envir = compat)
            assign("choose.files",
                   function (default = "", caption = "Select files",
                             multi = TRUE, filters = Filters,
                             index = nrow(Filters)) {Filters=NULL}, envir = compat)
            assign("DLL.version", function(path) {}, envir = compat)
            assign("getClipboardFormats", function() {}, envir = compat)
            assign("getIdentification", function() {}, envir = compat)
            assign("getWindowsHandle", function(which = "Console") {},
                   envir = compat)
            assign("getWindowTitle", function() {}, envir = compat)
            assign("readClipboard", function(format = 1, raw = FALSE) {},
                   envir = compat)
            assign("setWindowTitle",
                   function(suffix, title = paste(getIdentification(), suffix)) {},
                   envir = compat)
            assign("shell",
                   function(cmd, shell, flag = "/c", intern = FALSE,
                            wait = TRUE, translate = FALSE, mustWork = FALSE,
                            ...) {},
                   envir = compat)
            assign("shell.exec", function(file) {}, envir = compat)
            assign("shortPathName", function(path) {}, envir = compat)
            assign("win.version", function() {}, envir = compat)
            assign("zip.unpack", function(zipname, dest) {}, envir = compat)

            assign("bmp",
                   function (filename = "Rplot%03d.bmp", width = 480,
                             height = 480, units = "px", pointsize = 12,
                             bg = "white", res = NA, restoreConsole = TRUE) {},
                   envir = compat)
            assign("savePlot",
                   function (filename = "Rplot",
                             type = c("wmf", "emf", "png", "jpeg", "jpg",
                             "bmp", "ps", "eps", "pdf"),
                             device = dev.cur(), restoreConsole = TRUE) {},
                   envir = compat)
            assign("win.graph",
                   function(width = 7, height = 7, pointsize = 12,
                            restoreConsole = FALSE) {}, envir = compat)
            assign("win.metafile",
                   function (filename = "", width = 7, height = 7,
                             pointsize = 12, restoreConsole = TRUE) {},
                   envir = compat)
            assign("win.print",
                   function(width = 7, height = 7, pointsize = 12,
                            printer = "", restoreConsole = TRUE) {},
                   envir = compat)
            assign("windows",
                   function(width = 7, height = 7, pointsize = 12,
                            record = getOption("graphics.record"),
                            rescale = c("R", "fit", "fixed"), xpinch, ypinch,
                            bg = "transparent", canvas = "white",
                            gamma = getOption("gamma"),
                            xpos = NA, ypos = NA,
                            buffered = getOption("windowsBuffered"),
                            restoreConsole = FALSE) {}, envir = compat)
            assign("windowsFont", function(font) {}, envir = compat)
            assign("windowsFonts", function(...) {}, envir = compat)

            assign("winDialog", function(type = "ok", message) {},
                   envir = compat)
            assign("winDialogString", function(message, default) {},
                   envir = compat)
            assign("winMenuAdd", function(menuname) {}, envir = compat)
            assign("winMenuAddItem", function(menuname, itemname, action) {},
                   envir = compat)
            assign("winMenuDel", function(menuname) {}, envir = compat)
            assign("winMenuDelItem", function(menuname, itemname) {},
                   envir = compat)
            assign("winMenuNames", function() {}, envir = compat)
            assign("winMenuItems", function(menuname) {}, envir = compat)
            assign("winProgressBar",
                   function(title = "R progress bar", label = "",
                            min = 0, max = 1, initial = 0, width = 300) {},
                   envir = compat)
            assign("setWinProgressBar",
                   function(pb, value, title=NULL, label=NULL) {},
                   envir = compat)
            assign(".install.winbinary",
                   function(pkgs, lib, repos = getOption("repos"),
                            contriburl = contrib.url(repos),
                            method, available = NULL, destdir = NULL,
                            dependencies = FALSE, ...) {}, envir = compat)
        }
        attach(compat, name="compat", pos = length(search()),
               warn.conflicts = FALSE)
    }

    ## A simple function for catching the output from the codetools
    ## analysis using the checkUsage report mechanism.
    out <- character()
    foo <- function(x) out <<- c(out, x)
    ## (Simpler than using a variant of capture.output().)
    ## Of course, it would be nice to return a suitably structured
    ## result, but we can always do this by suitably splitting the
    ## messages on the double colons ...

    ## Not only check function definitions, but also S4 methods
    ## [a version of this should be part of codetools eventually] :
    checkMethodUsageEnv <- function(env, ...) {
	for (g in methods::getGenerics(where = env))
	    for (m in methods::findMethods(g, where = env)) {
		fun <- methods::getDataPart(m)
		signature <- paste(m@generic,
				   paste(m@target, collapse = "-"),
				   sep = ",")
		codetools::checkUsage(fun, signature, ...)
	    }
    }
    checkMethodUsagePackage <- function (pack, ...) {
	pname <- paste("package", pack, sep = ":")
	if (!pname %in% search())
	    stop("package must be loaded")
	checkMethodUsageEnv(if (pack %in% loadedNamespaces())
			    getNamespace(pack) else as.environment(pname), ...)
    }

    ## <NOTE>
    ## Eventually, we should be able to specify a codetools "profile"
    ## for checking.
    ## </NOTE>

    suppressMessages(codetools::checkUsagePackage(package,
                                                  report = foo,
                                                  suppressLocalUnused = TRUE,
                                                  skipWith = TRUE))
    suppressMessages(checkMethodUsagePackage     (package,
                                                  report = foo,
                                                  suppressLocalUnused = TRUE,
                                                  skipWith = TRUE))
    out <- unique(out)
    class(out) <- "check_code_usage_in_package"
    out
}

print.check_code_usage_in_package <-
function(x, ...)
{
    if(length(x))
        writeLines(strwrap(x, indent = 0L, exdent = 2L))
    invisible(x)
}

### * .check_Rd_xrefs

.check_Rd_xrefs <-
function(package, dir, lib.loc = NULL)
{
    ## Build a db with all possible link targets (aliases) in the base
    ## and recommended packages.
    base <- unlist(.get_standard_package_names()[c("base", "recommended")],
                   use.names = FALSE)
    aliases <- lapply(base, Rd_aliases, lib.loc = NULL)
    ## (Don't use lib.loc = .Library, as recommended packages may have
    ## been installed to a different place.)

    ## Now find the aliases in packages it depends on
    if(!missing(package)) {
        pfile <- system.file("Meta", "package.rds", package = package,
                             lib.loc = lib.loc)
        pkgInfo <- .readRDS(pfile)
    } else {
        outDir <- file.path(tempdir(), "fake_pkg")
        dir.create(file.path(outDir, "Meta"), FALSE, TRUE)
        .install_package_description(dir, outDir)
        pfile <- file.path(outDir, "Meta", "package.rds")
        pkgInfo <- .readRDS(pfile)
        unlink(outDir, recursive = TRUE)
    }
    ## only 'Depends' are guaranteed to be on the search path, but
    ## 'Imports' have to be installed and hence help there will be found
    deps <- c(names(pkgInfo$Depends), names(pkgInfo$Imports))
    pkgs <- unique(deps) %w/o% base
    try_Rd_aliases <- function(...) tryCatch(Rd_aliases(...), error = identity)
    aliases <- c(aliases, lapply(pkgs, try_Rd_aliases, lib.loc = lib.loc))
    aliases[sapply(aliases, class) == "error"] <- NULL

    ## See testRversion in library()
    new_only <- FALSE
    if(length(Rdeps <- pkgInfo$Rdepends2)) {
        ## has this if installed in > 2.7.0
        current <- as.numeric_version("2.9.2")
        for(dep in Rdeps)
            if(length(dep) > 1L) {
                target <- as.numeric_version(dep$version)
                res <- eval(parse(text=paste("current", dep$op, "target")))
                if(!res) new_only <- TRUE
            }
    }

    ## Add the aliases from the package itself, and build a db with all
    ## (if any) \link xrefs in the package Rd objects.
    if(!missing(package)) {
        aliases1 <- Rd_aliases(package, lib.loc = lib.loc)
        if(!length(aliases1))
            return(structure(NULL, class = "check_Rd_xrefs"))
        aliases <- c(aliases, list(aliases1))
        db <- .build_Rd_xref_db(package, lib.loc = lib.loc)
    } else {
        aliases1 <- Rd_aliases(dir = dir)
        if(!length(aliases1))
            return(structure(NULL, class = "check_Rd_xrefs"))
        aliases <- c(aliases, list(aliases1))
        db <- .build_Rd_xref_db(dir = dir)
    }

    ## Flatten the xref db into one big matrix.
    db <- cbind(do.call("rbind", db), rep(names(db), sapply(db, NROW)))
    if(nrow(db) == 0L) return(structure(NULL, class = "check_Rd_xrefs"))

    ## fixup \link[=dest] form
    anchor <- db[, 2L]
    have_equals <- grepl("^=", anchor)
    if(any(have_equals))
        db[have_equals, 1:2] <- cbind(sub("^=", "", anchor[have_equals]), "")

    db <- cbind(db, bad = FALSE, report = db[, 1L])
    have_anchor <- nzchar(anchor <- db[, 2L])
    db[have_anchor, "report"] <-
        paste("[", db[have_anchor, 2L], "]{", db[have_anchor, 1L], "}", sep = "")

    ## Check the targets from the non-anchored xrefs.
    db[!have_anchor, "bad"] <- !( db[!have_anchor, 1L] %in% unlist(aliases))

    ## and then check the anchored ones if we can.
    have_colon <- grepl(":", anchor, fixed = TRUE)
    unknown <- character()
    thispkg <- anchor
    thisfile <- db[, 1L]
    thispkg[have_colon] <- sub("([^:]*):(.*)", "\\1", anchor[have_colon])
    thisfile[have_colon] <- sub("([^:]*):(.*)", "\\2", anchor[have_colon])
    for (pkg in unique(thispkg[have_anchor])) {
        ## we can't do this on the current uninstalled package!
        if (missing(package) && pkg == basename(dir)) next
        this <- have_anchor & (thispkg %in% pkg)
        top <- system.file(package = pkg, lib.loc = lib.loc)
        if(nzchar(top)) {
            RdDB <- file.path(top, "help", "paths.rds")
            nm <- if(file.exists(RdDB))
                sub("\\.[Rr]d", "", basename(.readRDS(RdDB)))
            ## might be pre-2.10.0 and on Windows, hence zipped
            else if(file.exists(f <- file.path(top, "help", "Rhelp.zip")))
                utils::read.table(file.path(top, "help", "AnIndex"), sep="\t")[2]
            else
                list.files(file.path(top, "help"))
            good <- thisfile[this] %in% nm
            suspect <- if(any(!good)) {
                aliases1 <- if (pkg %in% names(aliases)) aliases[[pkg]]
                else Rd_aliases(pkg, lib.loc = lib.loc)
                !good & (thisfile[this] %in% aliases1)
            } else FALSE
        } else {
            unknown <- c(unknown, pkg)
            next
        }
        db[this, "bad"] <- !good & !suspect
    }

    unknown <- unique(unknown)
    obsolete <- unknown %in% c("ctest", "eda", "lqs", "mle", "modreg", "mva", "nls", "stepfun", "ts")
    if (any(obsolete)) {
        message(gettextf("Obsolete package(s) %s in Rd xrefs",
                         paste(sQuote(unknown[obsolete]), collapse = ", ")),
                domain = NA)
    }
    unknown <- unknown[!obsolete]
    if (length(unknown)) {
        repos <- .get_standard_repository_URLs()
        known <- try(utils::available.packages(utils::contrib.url(repos, "source"))[, "Package"])
        miss <- if(inherits(known, "try-error")) TRUE
        else unknown %in% c(known, c("BRugs", "GLMMGibbs", "survnnet", "yags"))
        ## from CRANextras
        if(any(miss))
            message(gettextf("Package(s) unavailable to check Rd xrefs: %s",
                             paste(sQuote(unknown[miss]), collapse = ", ")),
                    domain = NA)
        if(any(!miss))
            message(gettextf("Unknown package(s) %s in Rd xrefs",
                             paste(sQuote(unknown[!miss]), collapse = ", ")),
                    domain = NA)
    }
    ## The bad ones:
    bad <- db[, "bad"] == "TRUE"
    res1 <- split(db[bad, "report"], db[bad, 3L])
    structure(list(bad = res1), class = "check_Rd_xrefs")
}

print.check_Rd_xrefs <-
function(x, ...)
{
    xx <- x$bad
    if(length(xx)) {
        for(i in seq_along(xx)) {
            writeLines(gettextf("Missing link(s) in documentation object '%s':",
                                names(xx)[i]))
            ## NB, link might be empty, and was in mvbutils
            .pretty_print(sQuote(unique(xx[[i]])))
            writeLines("")
        }
        msg <- strwrap(gettextf("See the information in section 'Cross-references' of the 'Writing R Extensions' manual."))
        writeLines(c(msg, ""))
    }
    invisible(x)
}


### * .check_package_datasets

.check_package_datasets <-
function(pkgDir)
{
    Sys.setlocale("LC_CTYPE", "C")
    options(warn=-1)
    check_charsxp <- function(txt)
    {
        if(any(charToRaw(txt) > as.raw(127)))
            switch(Encoding(txt),
                   "latin1" = {latin1 <<- c(latin1, txt)},
                   "UTF-8" = {utf8 <<- c(utf8, txt)},
                   {non_ASCII <<- c(non_ASCII, txt)})
        invisible()
    }
    check_one <- function(x)
    {
        if(!length(x)) return()
        ## avoid as.list methods
        if(is.list(x)) lapply(unclass(x), check_one)
        if(is.character(x)) lapply(unclass(x), check_charsxp)
        a <- attributes(x)
        if(!is.null(a)) {
            lapply(a, check_one)
            check_one(names(a))
        }
        invisible()
    }

    sink(tempfile()) ## suppress startup messages to stdout
    files <- list_files_with_type(file.path(pkgDir, "data"), "data")
    files <- unique(basename(file_path_sans_ext(files)))
    ans <- vector("list", length(files))
    dataEnv <- new.env(hash=TRUE)
    names(ans) <- files
    old <- setwd(pkgDir)
    for(f in files)
        .try_quietly(utils::data(list = f, package = character(0L), envir = dataEnv))
    setwd(old)

    non_ASCII <- latin1 <- utf8 <- character(0L)
    ## avoid messages about loading packages that started with r48409
    suppressPackageStartupMessages({
        for(ds in ls(envir = dataEnv, all.names = TRUE))
            check_one(get(ds, envir = dataEnv))
    })
    sink()
    structure(list(latin1 = unique(latin1), utf8 = unique(utf8),
                   unknown = unique(non_ASCII)),
              class = "check_package_datasets")
}

print.check_package_datasets <-
function(x, ...)
{
    ## not sQuote as we have mucked about with locales.
    iconv0 <- function(x, ...) paste("'", iconv(x, ...), "'", sep="")

    if(n <- length(x$latin1))
        cat(sprintf("Note: found %d marked Latin-1 string(s)\n", n))
    if(n <- length(x$utf8))
        cat(sprintf("Note: found %d marked UTF-8 string(s)\n", n))
    if(n <- length(x$unknown)) {
        cat("Warning: found non-ASCII string(s)\n")
        cat(iconv0(x$unknown, "", "ASCII", sub="byte"), sep="  ", fill=70)
    }
    invisible(x)
}

### * .check_package_subdirs

## used by R CMD build
.check_package_subdirs <-
function(dir, doDelete = FALSE)
{
    OS_subdirs <- c("unix", "windows")

    mydir <- function(dir)
    {
        d <- list.files(dir, all.files = TRUE, full.names = FALSE)
        if(!length(d)) return(d)
        if(basename(dir) %in% c("R", "man"))
            for(os in OS_subdirs) {
                os_dir <- file.path(dir, os)
                if(file_test("-d", os_dir))
                    d <- c(d,
                           file.path(os,
                                     list.files(os_dir,
                                                all.files = TRUE,
                                                full.names = FALSE)))
            }
        d[file_test("-f", file.path(dir, d))]
    }

    if(!file_test("-d", dir))
        stop(gettextf("directory '%s' does not exist", dir), domain = NA)
    else
        dir <- file_path_as_absolute(dir)

    wrong_things <- list(R = character(0L), man = character(0L),
                         demo = character(0L), `inst/doc` = character(0L))

    code_dir <- file.path(dir, "R")
    if(file_test("-d", code_dir)) {
        all_files <- mydir(code_dir)
        ## Under Windows, need a Makefile.win for methods.
        R_files <- c("sysdata.rda", "Makefile.win",
                     list_files_with_type(code_dir, "code",
                                          full.names = FALSE,
                                          OS_subdirs = OS_subdirs))
        wrong <- all_files %w/o% R_files
        ## now configure might generate files in this directory
        generated <- grep("\\.in$", wrong)
        if(length(generated)) wrong <- wrong[-generated]
        if(length(wrong)) {
            wrong_things$R <- wrong
            if(doDelete) unlink(file.path(dir, "R", wrong))
        }
    }

    man_dir <- file.path(dir, "man")
    if(file_test("-d", man_dir)) {
        all_files <- mydir(man_dir)
        man_files <- list_files_with_type(man_dir, "docs",
                                          full.names = FALSE,
                                          OS_subdirs = OS_subdirs)
        wrong <- all_files %w/o% man_files
        if(length(wrong)) {
            wrong_things$man <- wrong
            if(doDelete) unlink(file.path(dir, "man", wrong))
        }
    }

    demo_dir <- file.path(dir, "demo")
    if(file_test("-d", demo_dir)) {
        all_files <- mydir(demo_dir)
        demo_files <- list_files_with_type(demo_dir, "demo",
                                           full.names = FALSE)
        wrong <- all_files %w/o% c("00Index", demo_files)
        if(length(wrong)) {
            wrong_things$demo <- wrong
            if(doDelete) unlink(file.path(dir, "demo", wrong))
        }
    }

    vign_dir <- file.path(dir, "inst", "doc")
    if(file_test("-d", vign_dir)) {
        vignettes <- list_files_with_type(vign_dir, "vignette",
                                          full.names = FALSE)
        vignettes <- c(vignettes,
                       list_files_with_exts(vign_dir, "pdf",
                                            full.names = FALSE))
        ## we specify ASCII filenames starting with a letter in R-exts
        ## do this in a locale-independent way.
        OK <- grep("^[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz][ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789._-]+$", vignettes)
        wrong <- vignettes
        if(length(OK)) wrong <- wrong[-OK]
        if(length(wrong)) wrong_things$`inst/doc` <- wrong
    }

    class(wrong_things) <- "subdir_tests"
    wrong_things
}

print.subdir_tests <-
function(x, ...)
{
    for(i in which(sapply(x, length) > 0L)) {
        tag <- names(x)[i]
        writeLines(sprintf("Subdirectory '%s' contains invalid file names:",
                           names(x)[i]))
        .pretty_print(x[[i]])
    }
    invisible(x)
}

### * .check_package_ASCII_code

.check_package_ASCII_code <-
function(dir, respect_quotes = FALSE)
{
    OS_subdirs <- c("unix", "windows")
    if(!file_test("-d", dir))
        stop(gettextf("directory '%s' does not exist", dir), domain = NA)
    else
        dir <- file_path_as_absolute(dir)

    code_dir <- file.path(dir, "R")
    wrong_things <- character(0L)
    if(file_test("-d", code_dir)) {
        R_files <- list_files_with_type(code_dir, "code",
                                        full.names = FALSE,
                                        OS_subdirs = OS_subdirs)
        for(f in R_files) {
            text <- readLines(file.path(code_dir, f), warn = FALSE)
            if(.Call(check_nonASCII, text, !respect_quotes))
                wrong_things <- c(wrong_things, f)
        }
    }
    if(length(wrong_things)) cat(wrong_things, sep="\n")
    invisible(wrong_things)
}

### * .check_package_code_syntax

.check_package_code_syntax <-
function(dir)
{
    if(!file_test("-d", dir))
        stop(gettextf("directory '%s' does not exist", dir), domain = NA)
    else
        dir <- file_path_as_absolute(dir)
    dir_name <- basename(dir)

    dfile <- file.path(dirname(dir), "DESCRIPTION")
    enc <- if(file.exists(dfile))
        .read_description(dfile)["Encoding"] else NA

    ## This was always run in the C locale < 2.5.0
    ## However, what chars are alphabetic depends on the locale,
    ## so as from R 2.5.0 we try to set a locale.
    ## Any package with no declared encoding should have only ASCII R code.
    if(!is.na(enc)) {  ## try to use the declared encoding
        if(.Platform$OS.type == "windows") {
            ## "C" is in fact "en", and there are no UTF-8 locales
            switch(enc,
                   "latin2" = Sys.setlocale("LC_CTYPE", 'polish'),
                   Sys.setlocale("LC_CTYPE", "C")
                   )
        } else {
            loc <- Sys.getenv("R_ENCODING_LOCALES", NA)
            if(!is.na(loc)) {
                loc <- strsplit(strsplit(loc, ":")[[1L]], "=")
                nm <- lapply(loc, "[[", 1L)
                loc <- lapply(loc, "[[", 2L)
                names(loc) <- nm
                if(!is.null(l <- loc[[enc]]))
                    Sys.setlocale("LC_CTYPE", l)
                else
                    Sys.setlocale("LC_CTYPE", "C")

            } else if(l10n_info()[["UTF-8"]]) {
                ## the hope is that the conversion to UTF-8 works and
                ## so we can validly test the code in the current locale.
            } else {
                ## these are the POSIX forms, but of course not all Unixen
                ## abide by POSIX.  These locales need not exist, but
                ## do in glibc.
                switch(enc,
                       "latin1" = Sys.setlocale("LC_CTYPE", "en_US"),
                       "utf-8"  =,  # not valid, but used
                       "UTF-8"  = Sys.setlocale("LC_CTYPE", "en_US.utf8"),
                       "latin2" = Sys.setlocale("LC_CTYPE", "pl_PL"),
                       "latin9" = Sys.setlocale("LC_CTYPE",
                       "fr_FR.iso885915@euro"),
                       Sys.setlocale("LC_CTYPE", "C")
                      )
            }
        }
    }

    collect_parse_woes <- function(f) {
        .error <- .warnings <- character()
        file <- file.path(dir, f)
        if(!is.na(enc) &&
           !(Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX"))) {
            con <- file(file, encoding = enc)
            on.exit(close(con))
        } else con <- file
        withCallingHandlers(tryCatch(parse(con),
                                     error = function(e)
                                     .error <<- conditionMessage(e)),
                            warning = function(e) {
                                .warnings <<- c(.warnings,
                                                conditionMessage(e))
                                invokeRestart("muffleWarning")
                            })
        ## (We show offending file paths starting with the base of the
        ## given directory as this provides "nicer" output ...)
        if(length(.error) || length(.warnings))
            list(File = file.path(dir_name, f),
                 Error = .error, Warnings = .warnings)
        else
            NULL
    }

    out <-
        lapply(list_files_with_type(dir, "code", full.names = FALSE,
                                    OS_subdirs = c("unix", "windows")),
               collect_parse_woes)
    Sys.setlocale("LC_CTYPE", "C")
    structure(out[sapply(out, length) > 0L],
              class = "check_package_code_syntax")
}

print.check_package_code_syntax <-
function(x, ...)
{
    first <- TRUE
    for(i in seq_along(x)) {
        if(!first) writeLines("") else first <- FALSE
        xi <- x[[i]]
        if(length(xi$Error)) {
            msg <- gsub("\n", "\n  ", sub("[^:]*: *", "", xi$Error),
			perl = TRUE, useBytes = TRUE)
            writeLines(c(sprintf("Error in file '%s':", xi$File),
                         paste(" ", msg)))
        }
        if(len <- length(xi$Warnings))
            writeLines(c(sprintf(ngettext(len,
                                          "Warning in file '%s':",
                                          "Warnings in file '%s':"),
                                 xi$File),
                         paste(" ", gsub("\n\n", "\n  ", xi$Warnings,
					 perl = TRUE, useBytes = TRUE))))
    }
    invisible(x)
}

### * .check_package_code_shlib

.check_package_code_shlib <-
function(dir)
{
    ## <NOTE>
    ## This is very similar to what happens with checkTnF() etc.
    ## We should really have a more general-purpose tree walker.
    ## </NOTE>

    dfile <- file.path(dir, "..", "DESCRIPTION")
    enc <- if(file.exists(dfile))
        .read_description(dfile)["Encoding"] else NA

    ## Workhorse function.
    filter <- function(file) {
        matches <- list()
        walker <- function(e) {
            if((length(e) > 1L)
               && is.call(e)
               && as.character(e[[1L]]) %in% c("library.dynam",
                                              "library.dynam.unload")
               && is.character(e[[2L]])
               && grepl("\\.(so|sl|dll)$", e[[2L]])
               )
                matches <<- c(matches, list(e))
            if(is.recursive(e))
                for(i in seq_along(e)) Recall(e[[i]])
        }
        ## assume that if locale if 'C' we can read encodings unchanged.
        exprs <- if(!is.na(enc) &&
                    !(Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX"))) {
	    con <- file(file, encoding=enc)
            on.exit(close(con))
	    parse(con)
	} else parse(file)
        for(i in seq_along(exprs)) walker(exprs[[i]])
        matches
    }

    code_files <-
        list_files_with_type(dir, "code",
                             OS_subdirs = c("unix", "windows"))
    x <- lapply(code_files, filter)
    names(x) <- code_files
    x <- x[sapply(x, length) > 0L]

    ## Because we really only need this for calling from R CMD check, we
    ## produce output here in case we found something.
    for(fname in names(x)) {
        writeLines(gettextf("File '%s':", fname))
        xfname <- x[[fname]]
        for(i in seq_along(xfname)) {
            writeLines(strwrap(gettextf("found %s",
                                        paste(deparse(xfname[[i]]),
                                              collapse = "")),
                               indent = 2L, exdent = 4L))
        }
    }

    invisible(x)
}


### * .check_packages_used

.check_packages_used <-
function(package, dir, lib.loc = NULL)
{
    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- .find.package(package, lib.loc)
        ## Using package installed in @code{dir} ...
        code_dir <- file.path(dir, "R")
        if(!file_test("-d", code_dir))
            stop(gettextf("directory '%s' does not contain R code",
                          dir),
                 domain = NA)
        if(basename(dir) != "base")
            .load_package_quietly(package, lib.loc)
        code_env <- if(packageHasNamespace(package, dirname(dir)))
            asNamespace(package)
        else
            .package_env(package)
        dfile <- file.path(dir, "DESCRIPTION")
        db <- .read_description(dfile)
     }
    else if(!missing(dir)) {
        ## Using sources from directory @code{dir} ...
        if(!file_test("-d", dir))
            stop(gettextf("directory '%s' does not exist", dir),
                 domain = NA)
        else
            dir <- file_path_as_absolute(dir)
        dfile <- file.path(dir, "DESCRIPTION")
        db <- .read_description(dfile)
        ## we need to check for a bundle here
        ## Need this to handle bundles ...
        contains <- .get_contains_from_package_db(db)
        if(length(contains)) {
            file <- tempfile()
            on.exit(unlink(file))
            if(!file.create(file)) stop("unable to create ", file)
            for(pkg in contains) {
                code_dir <- file.path(dir, pkg, "R")
                if(file_test("-d", code_dir)) {
                    if(!all(.file_append_ensuring_LFs(file,
                                                      list_files_with_type(code_dir,
                                                                           "code"))))
                        stop("unable to write code files")
                }
            }
        } else {
            code_dir <- file.path(dir, "R")
            if(file_test("-d", code_dir)) {
                file <- tempfile()
                on.exit(unlink(file))
                if(!file.create(file)) stop("unable to create ", file)
                if(!all(.file_append_ensuring_LFs(file,
                                                  list_files_with_type(code_dir,
                                                                       "code"))))
                    stop("unable to write code files")
            } else {
                return(invisible())
            }
        }
    }
    pkg_name <- db["Package"]
    depends <- .get_requires_from_package_db(db, "Depends")
    imports <- .get_requires_from_package_db(db, "Imports")
    suggests <- .get_requires_from_package_db(db, "Suggests")
    enhances <- .get_requires_from_package_db(db, "Enhances")

    ## Need this to handle bundles ...
    contains <- .get_contains_from_package_db(db)

    ## it is OK to refer to yourself and non-S4 standard packages
    standard_package_names <-
        .get_standard_package_names()$base %w/o% c("methods", "stats4")
    ## It helps to know if non-default standard packages are require()d
    default_package_names<-
         standard_package_names %w/o% c("grid", "splines", "tcltk", "tools")
    depends_suggests <- c(depends, suggests, pkg_name, contains,
                          default_package_names)
    imports <- c(imports, depends, suggests, enhances, pkg_name, contains,
                 standard_package_names)
    ## the first argument could be named, or could be a variable name.
    ## we just have a stop list here.
    common_names <- c("pkg", "pkgName", "package", "pos")

    bad_exprs <- character()
    bad_imports <- character()
    uses_methods <- FALSE
    find_bad_exprs <- function(e) {
        if(is.call(e) || is.expression(e)) {
            Call <- deparse(e[[1L]])[1L]
            if(length(e) >= 2L) pkg <- deparse(e[[2L]])
            if(Call %in% c("library", "require")) {
                ## Zelig has library()
                if(length(e) >= 2L) {
                    pkg <- sub('^"(.*)"$', '\\1', pkg)
                    ## <NOTE>
                    ## Using code analysis, we really don't know which
                    ## package was called if character.only = TRUE and
                    ## the package argument is not a string constant.
                    ## (Btw, what if character.only is given a value
                    ## which is an expression evaluating to TRUE?)
                    dunno <- FALSE
                    pos <- which(!is.na(pmatch(names(e),
                                               "character.only")))
                    if(length(pos)
                       && identical(e[[pos]], TRUE)
                       && !identical(class(e[[2L]]), "character"))
                        dunno <- TRUE
                    ## </NOTE>
                    ## <FIXME> could be inside substitute or a variable
                    ## and is in e.g. R.oo
                    if(! dunno
                       && ! pkg %in% c(depends_suggests, common_names))
                        bad_exprs <<- c(bad_exprs, pkg)
                }
            } else if(Call %in%  "::") {
                if(! pkg %in% imports)
                    bad_imports <<- c(bad_imports, pkg)
            } else if(Call %in%  ":::") {
                ## <FIXME> fathom out if this package has a namespace
                if(! pkg %in% imports)
                    bad_imports <<- c(bad_imports, pkg)
            } else if(Call %in% c("setClass", "setMethod")) {
                uses_methods <<- TRUE
            }
            for(i in seq_along(e)) Recall(e[[i]])
        }
    }

    if(!missing(package)) {
        ## <FIXME>
        ## Suggested way of checking for S4 metadata.
        ## Change to use as envir_has_S4_metadata() once this makes it
        ## into base or methods.
        if(length(objects(code_env, all.names = TRUE,
                          pattern = "^[.]__[CT]_")))
            uses_methods <- TRUE
        ## </FIXME>
        exprs <- lapply(ls(envir = code_env, all.names = TRUE),
                        function(f) {
                            f <- get(f, envir = code_env)
			    if(typeof(f) == "closure") body(f) # else NULL
                        })
        if(.isMethodsDispatchOn()) {
            ## Also check the code in S4 methods.
            ## This may find things twice.
            for(f in get_S4_generics_with_methods(code_env)) {
                mlist <- .get_S4_methods_list(f, code_env)
                exprs <- c(exprs, lapply(mlist, body))
            }
        }
    }
    else {
        enc <- db["Encoding"]
        if(!is.na(enc) &&
           !(Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX"))) {
	    con <- file(file, encoding=enc)
            on.exit(close(con))
        } else con <- file
        exprs <-
            tryCatch(parse(file = con, n = -1L),
                     error = function(e)
                     stop(gettextf("parse error in file '%s':\n%s",
                                   file,
                                   .massage_file_parse_error_message(conditionMessage(e))),
                               domain = NA, call. = FALSE))
    }

    for(i in seq_along(exprs)) find_bad_exprs(exprs[[i]])

    methods_message <-
        if(uses_methods && !"methods" %in% c(depends, imports))
            gettext("package 'methods' is used but not declared")
        else ""
    res <- list(others = unique(bad_exprs),
                imports = unique(bad_imports),
                methods_message = methods_message)
    class(res) <- "check_packages_used"
    res
}

print.check_packages_used <-
function(x, ...)
{
    if(length(x$imports)) {
        writeLines(gettext("'::' or ':::' imports not declared from:"))
        .pretty_print(x$imports)
    }
    if(length(x$others)) {
        writeLines(gettext("'library' or 'require' calls not declared from:"))
        .pretty_print(x$others)
    }
    if(nzchar(x$methods_message))
        writeLines(x$methods_message)
    invisible(x)
}


### * .check_T_and_F

## T and F checking, next generation.
##
## What are we really trying to do?
##
## In R, T and F are "just" variables which upon startup are bound to
## TRUE and FALSE, respectively, in the base package/namespace.  Hence,
## if code uses "global" variables T and F and dynamic lookup is in
## place (for packages, if they do not have a namespace), there may be
## trouble in case T or F were redefined.  So we'd like to warn about
## these cases.
##
## A few things to note:
## * Package code top-level bindings *to* T and F are not a problem for
##   packages installed for lazy-loading (as the top-level T and F get
##   evaluated "appropriately" upon installation.
## * Code in examples using "global" T and F is always a problem, as
##   this is evaluated in the global envionment by examples().
## * There is no problem with package code using T and F as local
##   variables.
##
## Our current idea is the following.  Function findGlobals() in
## codetools already provides a way to (approximately) determine the
## globals.  So we can try to get these and report them.
##
## Note that findGlobals() only works on closures, so we definitely miss
## top-level assignments to T or F.  This could be taken care of rather
## easily, though.
##
## Note also that we'd like to help people find where the offending
## globals were found.  Seems that codetools currently does not offer a
## way of recording e.g. the parent expression, so we do our own thing
## based on the legacy checkTnF code.

.check_T_and_F <-
function(package, dir, lib.loc = NULL)
{
    ## Seems that checking examples has several problems, and can result
    ## in "strange" diagnostic output.  Let's more or less disable this
    ## for the time being.
    check_examples <-
        isTRUE(as.logical(Sys.getenv("_R_CHECK_RD_EXAMPLES_T_AND_F_")))


    bad_closures <- character()
    bad_examples <- character()

    find_bad_closures <- function(env) {
        objects_in_env <- objects(env, all.names = TRUE)
        x <- lapply(objects_in_env,
                    function(o) {
                        v <- get(o, envir = env)
                        if (typeof(v) == "closure")
                            codetools::findGlobals(v)
                    })
        objects_in_env[sapply(x,
                              function(s) any(s %in% c("T", "F")))]
    }

    find_bad_examples <- function(txts) {
        env <- new.env()
        x <- lapply(txts,
                    function(txt) {
                        tryCatch({
                            eval(parse(text =
                                       paste("FOO <- function() {",
                                             paste(txt, collapse = "\n"),
                                             "}",
                                             collapse = "\n")),
                                 env)
                            find_bad_closures(env)
                        },
                                 error = function(e) character())
                    })
        names(txts)[sapply(x, length) > 0L]
    }

    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- .find.package(package, lib.loc)
        if((package != "base")
           && !packageHasNamespace(package, dirname(dir))) {
            .load_package_quietly(package, lib.loc)
            code_env <- .package_env(package)
            bad_closures <- find_bad_closures(code_env)
        }
        if(check_examples)
            example_texts <-
                .get_example_texts_from_example_dir(file.path(dir, "R-ex"))
    }
    else {
        ## The dir case.
        if(missing(dir))
            stop("you must specify 'package' or 'dir'")
        dir <- file_path_as_absolute(dir)
        code_dir <- file.path(dir, "R")
        if(!packageHasNamespace(basename(dir), dirname(dir))
           && file_test("-d", code_dir)) {
            code_env <- new.env()
            dfile <- file.path(dir, "DESCRIPTION")
            meta <- if(file_test("-f", dfile))
                .read_description(dfile)
            else
                character()
            .source_assignments_in_code_dir(code_dir, code_env, meta)
            bad_closures <- find_bad_closures(code_env)
        }
        if(check_examples)
            example_texts <- .get_example_texts_from_source_dir(dir)
    }

    if(check_examples)
        bad_examples <- find_bad_examples(example_texts)

    out <- list(bad_closures = bad_closures,
                bad_examples = bad_examples)
    class(out) <- "check_T_and_F"
    out
}

.get_example_texts_from_example_dir <-
function(dir)
{
    if(!file_test("-d", dir)) return(NULL)
    files <- list_files_with_exts(dir, "R")
    texts <- lapply(files,
                    function(f) paste(readLines(f, warn = FALSE),
                                      collapse = "\n"))
    names(texts) <- files
    texts
}

.get_example_texts_from_source_dir <-
function(dir)
{
    if(!file_test("-d", file.path(dir, "man"))) return(NULL)
    sapply(Rd_db(dir = dir), .Rd_get_example_code)
}

print.check_T_and_F <-
function(x, ...)
{
    if(length(x$bad_closures)) {
        msg <- ngettext(length(x$bad_closures),
                        "Found possibly global 'T' or 'F' in the following function:",
                        "Found possibly global 'T' or 'F' in the following functions:"
                        )
        writeLines(strwrap(msg))
        .pretty_print(x$bad_closures)
    }
    if(length(x$bad_examples)) {
        msg <- ngettext(length(x$bad_examples),
                        "Found possibly global 'T' or 'F' in the following Rd example file:",
                        "Found possibly global 'T' or 'F' in the following Rd example files:"
                        )
        writeLines(strwrap(msg))
        writeLines(paste(" ", x$bad_examples))
    }
    invisible(x)
}

### * .check_dotIntenal

.check_dotInternal <-
function(package, dir, lib.loc = NULL)
{
    bad_closures <- character()

    find_bad_closures <- function(env) {
        objects_in_env <- objects(env, all.names = TRUE)
        x <- lapply(objects_in_env,
                    function(o) {
                        v <- get(o, envir = env)
                        if (typeof(v) == "closure")
                            codetools::findGlobals(v)
                    })
        objects_in_env[sapply(x,
                              function(s) any(s %in% ".Internal"))]
    }

    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- .find.package(package, lib.loc)
        if(! package %in% .get_standard_package_names()$base) {
            .load_package_quietly(package, lib.loc)
            code_env <- if(packageHasNamespace(package, dirname(dir)))
                           asNamespace(package)
            else .package_env(package)
            bad_closures <- find_bad_closures(code_env)
        }
    }
    else {
        ## The dir case.
        if(missing(dir))
            stop("you must specify 'package' or 'dir'")
        dir <- file_path_as_absolute(dir)
        code_dir <- file.path(dir, "R")
        if(file_test("-d", code_dir)) {
            code_env <- new.env()
            dfile <- file.path(dir, "DESCRIPTION")
            meta <- if(file_test("-f", dfile))
                .read_description(dfile)
            else
                character()
            .source_assignments_in_code_dir(code_dir, code_env, meta)
            bad_closures <- find_bad_closures(code_env)
        }
    }

    out <- list(bad_closures = bad_closures)
    class(out) <- "check_dotInternal"
    out
}

print.check_dotInternal <-
function(x, ...)
{
    if(length(x$bad_closures)) {
        msg <- ngettext(length(x$bad_closures),
                        "Found .Internal call in the following function:",
                        "Found .Internal calls in the following functions:"
                        )
        writeLines(strwrap(msg))
        .pretty_print(x$bad_closures)
    }
    invisible(x)
}

### * .check_namespace

.check_namespace <-
function(dir)
{
    dir <- file_path_as_absolute(dir)
    invisible(tryCatch(parseNamespaceFile(basename(dir), dirname(dir)),
                       error = function(e) {
                           writeLines("Invalid NAMESPACE file, parsing gives:")
                           stop(e)
                       }))
}

### * .check_citation

.check_citation <-
function(cfile)
{
    cfile <- tools::file_path_as_absolute(cfile)
    meta <- if(basename(dir <- dirname(cfile)) == "inst")
        as.list(tools:::.get_package_metadata(dirname(dir)))
    else
        NULL

    out <- tryCatch(suppressMessages(utils::readCitationFile(cfile, meta)),
                    error = identity)
    if(inherits(out, "error")) {
        writeLines(conditionMessage(out))
        return(invisible())
    }

    if(is.null(encoding <- meta$Encoding))
        encoding <- "unknown"
    db <- get_CITATION_entry_fields(cfile, encoding)
    if(!NROW(db)) return(invisible())
    bad <- Map(find_missing_required_BibTeX_fields, db$Entry, db$Fields,
               USE.NAMES = FALSE)
    ind <- sapply(bad, identical, NA_character_)
    if(length(pos <- which(ind))) {
        entries <- db$Entry[pos]
        entries <-
            ifelse(nchar(entries) < 20L,
                   entries,
                   paste(substring(entries, 1L, 20L), "[TRUNCATED]"))
        writeLines(sprintf("citEntry %d: invalid type %s",
                           pos, sQuote(entries)))
    }
    pos <- which(!ind & (sapply(bad, length) > 0L))
    if(length(pos)) {
        writeLines(strwrap(sprintf("citEntry %d (%s): missing required field(s) %s",
                                   pos,
                                   tolower(db$Entry[pos]),
                                   sapply(bad[pos],
                                          function(s)
                                          paste(sQuote(s),
                                                collapse = ", "))),
                           indent = 0L, exdent = 2L))
    }
}

### * .check_package_parseRd

## FIXME: could use dumped files, except for use of encoding = "ASCII"
.check_package_parseRd <-
function(dir, silent = FALSE, def_enc = FALSE, minlevel = -1)
{
    if(file.exists(dfile <- file.path(dir, "DESCRIPTION"))) {
        enc <- read.dcf(dfile)[1L, ]["Encoding"]
        if(is.na(enc)) enc <- "ASCII"
        else def_enc <- TRUE
    } else enc <- "ASCII"
    owd <- setwd(file.path(dir, "man"))
    on.exit(setwd(owd))
    pg <- c(Sys.glob("*.Rd"), Sys.glob("*.rd"),
            Sys.glob(file.path("*", "*.Rd")),
            Sys.glob(file.path("*", "*.rd")))
    ## (Note that using character classes as in '*.[Rr]d' is not
    ## guaranteed to be portable.)
    bad <- character()
    for (f in pg) {
        ## Kludge for now
        if(basename(f) %in%  c("iconv.Rd", "showNonASCII.Rd")) def_enc <- TRUE
        tmp <- try(checkRd(f, encoding = enc, def_enc = def_enc), silent=TRUE)
        if(inherits(tmp, "try-error")) {
	    bad <- c(bad, f)
            if(!silent) message(geterrmessage())
        } else print(tmp, minlevel = minlevel)
    }
    if(length(bad)) bad <- sQuote(sub(".*/","", bad))
    if(length(bad) > 1L)
        cat("problems found in ", paste(bad, collapse=", "), "\n", sep="")
    else if(length(bad))
        cat("problem found in ", bad, "\n", sep="")
    invisible()
}

### * .check_package_CRAN_incoming

.check_package_CRAN_incoming <-
function(dir)
{
    out <- list()
    class(out) <- "check_package_CRAN_incoming"

    meta <- .get_package_metadata(dir, FALSE)

    urls <- .get_standard_repository_URLs()
    ## We do not want to use utils::available.packages() for now, as
    ## this unconditionally filters according to R version and OS type.
    .repository_db <- function(u) {
        con <- gzcon(url(sprintf("%s/src/contrib/PACKAGES.gz", u), "rb"))
        on.exit(close(con))
        ## hopefully all these fields are ASCII, or we need to re-encode.
        cbind(read.dcf(con,
                       c(.get_standard_repository_db_fields(), "Path")),
              Repository = u)

    }
    db <- tryCatch(lapply(urls, .repository_db), error = identity)
    if(inherits(db, "error")) return(out)
    db <- do.call(rbind, db)

    ## Package names must be unique within standard repositories when
    ## ignoring case.
    package <- meta["Package"]
    packages <- db[, "Package"]
    existing <-
        packages[(tolower(packages) == tolower(package)) &
                 (packages != package)]
    if(length(existing))
        out$bad_package <- list(package, existing)
    ## Could there be more than one?

    ## Is this an update for package already on CRAN?
    db <- db[(packages == package) &
             (db[, "Repository"] == urls[1L]) &
             is.na(db[, "Path"]), , drop = FALSE]
    ## This assumes the CRAN URL comes first, and drops packages in
    ## version-specific subdirectories.  It also does not know about
    ## archived versions.
    if(!NROW(db)) return(out)
    ## For now, there should be no duplicates ...

    ## Package versions should be newer than what we already have on
    ## CRAN.

    v_m <- package_version(meta["Version"])
    v_d <- package_version(db[, "Version"])
    if(v_m <= max(v_d))
        out$bad_version <- list(v_m, v_d)

    ## Watch out for maintainer changes.
    ## Note that we cannot get the maintainer info from the PACKAGES
    ## files.
    con <- url(sprintf("%s/web/packages/packages.rds", urls[1L]), "rb")
    db <- tryCatch(.readRDS(con), error = identity)
    close(con)
    if(inherits(db, "error")) return(out)

    m_m <- meta["Maintainer"]
    m_d <- db[db[, "Package"] == package, "Maintainer"]
    if(!all(m_m == m_d))
        out$new_maintainer <- list(m_m, m_d)

    out

}

print.check_package_CRAN_incoming <-
function(x, ...)
{
    if(length(y <- x$bad_package))
        writeLines(sprintf("Conflicting package names (submitted: %s, existing: %s)",
                           y[[1L]], y[[2L]]))
    if(length(y <- x$bad_version))
        writeLines(sprintf("Insufficient package version (submitted: %s, existing: %s)",
                           y[[1L]], y[[2L]]))
    if(length(y <- x$new_maintainer)) {
        writeLines(c("New maintainer:",
                     strwrap(y[[1L]], indent = 2L, exdent = 4L),
                     "Old maintainer(s):",
                     strwrap(y[[2L]], indent = 2L, exdent = 4L)))
    }
    invisible(x)
}

### * .check_Rd_metadata

.check_Rd_metadata <-
function(package, dir, lib.loc = NULL)
{
    ## Perform package-level Rd metadata checks:
    ## names and aliases must be unique within a package.

    ## Note that we cannot use Rd_aliases(), as this does
    ##   if(length(aliases))
    ##       sort(unique(unlist(aliases, use.names = FALSE)))

    out <- structure(list(), class = "check_Rd_metadata")

    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- .find.package(package, lib.loc)
        rds <- file.path(dir, "Meta", "Rd.rds")
        if(file_test("-f", rds)) {
            meta <- .readRDS(rds)
            files <- meta$File
            names <- meta$Name
            aliases <- meta$Aliases
        } else {
            return(out)
        }
    } else {
        if(file_test("-d", file.path(dir, "man"))) {
            db <- Rd_db(dir = dir)
            files <- basename(names(db))
            names <- sapply(db, .Rd_get_metadata, "name")
            aliases <- lapply(db, .Rd_get_metadata, "alias")
        } else {
            return(out)
        }
    }

    ## <FIXME>
    ## Remove eventually, as .Rd_get_metadata() and hence Rd_info() now
    ## eliminate duplicated entries ...
    aliases <- lapply(aliases, unique)
    ## </FIXME>

    files_grouped_by_names <- split(files, names)
    files_with_duplicated_names <-
        files_grouped_by_names[sapply(files_grouped_by_names,
                                      length) > 1L]
    if(length(files_with_duplicated_names))
        out$files_with_duplicated_names <-
            files_with_duplicated_names

    files_grouped_by_aliases <-
        split(rep.int(files, sapply(aliases, length)),
              unlist(aliases, use.names = FALSE))
    files_with_duplicated_aliases <-
        files_grouped_by_aliases[sapply(files_grouped_by_aliases,
                                      length) > 1L]
    if(length(files_with_duplicated_aliases))
        out$files_with_duplicated_aliases <-
            files_with_duplicated_aliases

    out
}

print.check_Rd_metadata <-
function(x, ...)
{
    if(length(x$files_with_duplicated_name)) {
        bad <- x$files_with_duplicated_name
        for(nm in names(bad)) {
            writeLines(gettextf("Rd files with duplicated name '%s':", nm))
            .pretty_print(bad[[nm]])
        }
    }

    if(length(x$files_with_duplicated_aliases)) {
        bad <- x$files_with_duplicated_aliases
        for(nm in names(bad)) {
            writeLines(gettextf("Rd files with duplicated alias '%s':", nm))
            .pretty_print(bad[[nm]])
        }
    }

    invisible(x)
}


### * .find_charset

.find_charset <-
function()
{
    l10n <- l10n_info()
    enc <- if(l10n[["UTF-8"]]) "UTF-8" else utils:::localeToCharset()
    cat("charset: ", enc, "\n", sep="")
    invisible()
}


### * Utilities

### ** as.alist.call

as.alist.call <-
function(x)
{
    y <- as.list(x)
    ind <- if(is.null(names(y)))
        seq_along(y)
    else
        which(names(y) == "")
    if(length(ind)) {
        names(y)[ind] <- sapply(y[ind],as.character)
        y[ind] <- rep.int(list(alist(irrelevant = )[[1L]]), length(ind))
    }
    y
}

### ** as.alist.symbol

as.alist.symbol <-
function(x)
{
    as.alist.call(call(as.character(x)))
}

### ** .arg_names_from_call

.arg_names_from_call <-
function(x)
{
    y <- as.character(x)
    if(!is.null(nx <- names(x))) {
        ind <- which(nx != "")
        y[ind] <- nx[ind]
    }
    y
}

### ** .dquote_method_markup

## See the notes below.
## An alternative and possibly more efficient implementation could be
## based using gregexpr(re, txt), massaging the matches and merging with
## the non-matched parts.

.dquote_method_markup <-
function(txt, re)
{
    out <- ""
    while((ipos <- regexpr(re, txt)) > -1L) {
        epos <- ipos + attr(ipos, "match.length") - 1L
        str <- substring(txt, ipos, epos)
        str <- sub("\"", "\\\"", str, fixed = TRUE)
        str <- sub("\\", "\\\\", str, fixed = TRUE)
        out <- sprintf("%s%s\"%s\"", out,
                       substring(txt, 1L, ipos - 1L), str)
        txt <- substring(txt, epos + 1L)
    }
    paste(out, txt, sep = "")
}

### ** .functions_to_be_ignored_from_usage

.functions_to_be_ignored_from_usage <-
function(package_name)
{
    c("<-", "=",
      if(package_name == "base")
      c("(", "{", "function", "if", "for", "while", "repeat",
        "Math", "Ops", "Summary", "Complex"),
      if(package_name == "utils") "?",
      if(package_name == "methods") "@")
}

### ** .functions_with_no_useful_S3_method_markup

## <FIXME>
## Remove eventually ...
.functions_with_no_useful_S3_method_markup <-
function()
{
    ## Once upon a time ... there was no useful markup for S3 methods
    ## for subscripting/subassigning and binary operators.

    c(if(identical(as.logical(Sys.getenv("_R_CHECK_RD_USAGE_METHOD_SUBSET_")),
                   FALSE))
      c("[", "[[", "$", "[<-", "[[<-", "$<-"),
      if(identical(as.logical(Sys.getenv("_R_CHECK_RD_USAGE_METHOD_BINOPS_")),
                   FALSE))
      c("+", "-", "*", "/", "^", "<", ">", "<=", ">=", "!=", "==", "%%",
        "%/%", "&", "|"),
      "!")
}
## </FIXME>

### ** get_S4_generics_with_methods

## FIXME: make option of methods::getGenerics()
## JMC agreed & proposed argument  'excludeEmpty = FALSE'
get_S4_generics_with_methods <-
function(env, verbose = getOption("verbose"))
{
    env <- as.environment(env)
    ##  Filter(function(g) methods::isGeneric(g, where = env),
    ##	       methods::getGenerics(env))
    r <- methods::getGenerics(env)
    if(length(r) && {
	hasM <- lapply(r, function(g)
		       tryCatch(methods::hasMethods(g, where = env),
				error = identity))
	if(any(hasErr <- sapply(hasM, inherits, what = "error"))) {
            dq <- function(ch) paste('"', ch ,'"', sep='')
            rErr <- r[hasErr]
            pkgs <- r@package[hasErr]
            ## FIXME: This warning should not happen here when called
            ## from R CMD check, but rather be part of a new "check"
            ## there !
	    warning("Generics g in env = ", format(env),
		    " where hasMethods(g, env) errors: ",
		    paste(sQuote(rErr), collapse = ", "),
		    "\nMay need something like\n\n",
		    paste("  importFrom(", paste(dq(pkgs), dq(rErr), sep=", "),
                          ")\n", sep=''),
		    "\nin NAMESPACE.")
	    hasM <- hasM[!hasErr]
	}
	!all(ok <- unlist(hasM))
    }) {
	if(verbose)
	    message("Generics without methods in ", format(env), ": ",
		    paste(sQuote(r[!ok]), collapse = ", "))
	r[ok]
    }
    else as.vector(r)# for back-compatibility and current ..../tests/reg-S4.R
}

### ** .get_S4_methods_list

.get_S4_methods_list <-
function(g, env)
{
    ## For the QC computations, we really only want the S4 methods
    ## defined in a package, so we try to exclude derived default
    ## methods as well as methods inherited from other environments.

    env <- as.environment(env)
    mlist <- methods::findMethods(g, env)

    ## First, derived default methods (signature w/ "ANY").
    if(any(ind <- as.logical(sapply(mlist, methods::is,
				    "derivedDefaultMethod"))))
	mlist <- mlist[!ind]

    if(length(mlist)) {
        ## Determining the methods defined in a package from the package
        ## env or the associated namespace seems rather tricky.  What we
        ## seem to observe is the following.
        ## * If there is a namespace N, methods defined in the package
        ##   have N as their environment, for both the package env and
        ##   the associated namespace.
        ## * If there is no namespace, methods defined in the package
        ##   have an environment E which is empty and has globalenv() as
        ##   its parent.  (If the package defines generics, these seem
        ##   to have E as their parent env.)
        ## However, in the latter case, there seems no way to infer E
        ## from the package env.  Hence, we fall back to comparing the
        ## methods in the package env with those in its parent env, and
        ## exclude the ones already found there.
        namespace <- .get_namespace_from_package_env(env)
        if(!is.null(namespace)) {
            mlist <- Filter(function(m)
                            identical(environment(m), namespace),
                            mlist)
        } else {
            penv <- parent.env(env)
            if((g %in% get_S4_generics_with_methods(penv)) &&
               length(pmlist <- methods::findMethods(g, penv))) {
                ## Alas,
                ##   match(mlist, pmlist)
                ## does not work ...
                ind <- sapply(mlist,
                              function(m)
                              any(sapply(pmlist, identical, m)))
                ## When worried about efficiency, we could try to
                ## compare just names and environments ...
                mlist <- mlist[!ind]
            }
        }
    }

    mlist
}

.get_namespace_from_package_env <-
function(env)
{
    package <-
        sub(".*:([^_]*).*", "\\1", attr(env, "name", exact = TRUE))
    ## (Ugly, but why not?)
    if(length(package) && nzchar(package)) {
        .Internal(getRegisteredNamespace(as.name(package)))
        ## Note that we could also use
        ##   getNamespace(package, error = function(e) NULL)
    }
}


### ** .is_call_from_replacement_function_usage

.is_call_from_replacement_function_usage <-
function(x)
{
    ((length(x) == 3L)
     && (identical(x[[1L]], as.symbol("<-")))
     && (length(x[[2L]]) > 1L)
     && is.symbol(x[[3L]]))
}

### ** .make_siglist

.make_siglist <-
function(x)
{
    ## Argument 'x' should be a named list of methods as obtained by
    ## methods::findMethods() or .get_S4_methods_list().
    gsub("#", ",", names(x), fixed = TRUE)
}

### ** .make_signatures

.make_signatures <-
function(cls)
{
    ## Note that (thanks JMC), when comparing signatures, the signature
    ## has to be stripped of trailing "ANY" elements (which are always
    ## implicit) or padded to a fixed length.
    sub("(#ANY)*$", "", unlist(lapply(cls, paste, collapse = "#")))
}

### ** .massage_file_parse_error_message

.massage_file_parse_error_message <-
function(x)
    sub("^[^:]+:[[:space:]]*", "", x)

### ** .package_env

.package_env <-
function(package_name)
{
    as.environment(paste("package", package_name, sep = ":"))
}

### ** .parse_text_as_much_as_possible

.parse_text_as_much_as_possible <-
function(txt)
{
    exprs <- tryCatch(parse(text = txt), error = identity)
    if(!inherits(exprs, "error")) return(exprs)
    exprs <- expression()
    lines <- unlist(strsplit(txt, "\n"))
    bad_lines <- character()
    while((n <- length(lines))) {
        i <- 1L; txt <- lines[1L]
        while(inherits(yy <- tryCatch(parse(text = txt),
                                      error = identity),
                       "error")
              && (i < n)) {
            i <- i + 1L; txt <- paste(txt, lines[i], collapse = "\n")
        }
        if(inherits(yy, "error")) {
            bad_lines <- c(bad_lines, lines[1L])
            lines <- lines[-1L]
        }
        else {
            exprs <- c(exprs, yy)
            lines <- lines[-seq_len(i)]
        }
    }
    attr(exprs, "bad_lines") <- bad_lines
    exprs
}

### ** .parse_usage_as_much_as_possible

.parse_usage_as_much_as_possible <-
function(x)
{
    if(!length(x)) return(expression())
    ## Drop specials and comments.
    ## <FIXME>
    ## Remove calling .Rd_drop_comments() eventually.
    x <- .Rd_drop_comments(x)
    ## </FIXME>
    txt <- .Rd_deparse(.Rd_drop_nodes_with_tags(x, "\\special"),
                       tag = FALSE)
    txt <- gsub("\\\\l?dots", "...", txt)
    txt <- .dquote_method_markup(txt, .S3_method_markup_regexp)
    txt <- .dquote_method_markup(txt, .S4_method_markup_regexp)
    ## Transform <<see below>> style markup so that we can catch and
    ## throw it, rather than "basically ignore" it by putting it in the
    ## bad_lines attribute.
    txt <- gsub("(<<?see below>>?)", "`\\1`", txt)
    ## \usage is only 'verbatim-like'
    ## <FIXME>
    ## 'LanguageClasses.Rd' in package methods has '"\{"' in its usage.
    ## But why should it use the backslash escape?
    txt <- gsub("\\{", "{", txt, fixed = TRUE)
    txt <- gsub("\\}", "}", txt, fixed = TRUE)
    ## </FIXME>
    ## now any valid escape by \ is
    ##   \a \b \f \n \r \t \u \U \v \x \' \" \\ or \octal
    txt <- gsub("(^|[^\\])\\\\($|[^abfnrtuUvx0-9'\"\\])",
                "\\1<unescaped bksl>\\2", txt)
    ## and since this may overlap, try again
    txt <- gsub("(^|[^\\])\\\\($|[^abfnrtuUvx0-9'\"\\])",
                "\\1<unescaped bksl>\\2", txt)
    .parse_text_as_much_as_possible(txt)
}

### ** .pretty_print

.pretty_print <-
function(x)
{
    writeLines(strwrap(paste(x, collapse = " "),
                       indent = 2L, exdent = 2L))
}

### ** .strip_backticks

.strip_backticks <-
function(x)
    gsub("`", "", x)

### ** .transform_S3_method_markup

.transform_S3_method_markup <-
function(x)
{
    ## Note how we deal with S3 replacement methods found.
    ## These come out named "\method{GENERIC}{CLASS}<-" which we
    ## need to turn into 'GENERIC<-.CLASS'.
    re <- sprintf("%s(<-)?", .S3_method_markup_regexp)
    ## Note that this is really only called on "function" names obtained
    ## by parsing the \usage texts, so that the method regexps possibly
    ## augmented by '<-' fully match if they match.
    ## We should be able to safely strip all backticks; alternatively,
    ## we could do something like
    ##   cl <- .strip_backticks(sub(re, "\\4", x))
    ##   sub(re, sprintf("\\3\\5.%s", cl), x)
    .strip_backticks(sub(re, "\\3\\5.\\4", x))
}

### ** .transform_S4_method_markup

.transform_S4_method_markup <-
function(x)
{
    re <- sprintf("%s(<-)?", .S4_method_markup_regexp)
    ## We should be able to safely strip all backticks; alternatively,
    ## we could do something like
    ##   sl <- .strip_backticks(sub(re, "\\3", x))
    ##   sub(re, sprintf("\\\\S4method{\\2\\7}{%s}", sl), x)
    .strip_backticks(sub(re, "\\\\S4method{\\2\\7}{\\3}", x))
}

### ** .S3_method_markup_regexp

## For matching \(S3)?method{GENERIC}{CLASS}.
## GENERIC can be
## * a syntactically valid name
## * one of $ [ [[
## * one of the binary operators
##   + - * / ^ < <= > >= != == | & %something%
## (as supported by Rdconv).
## See also .functions_with_no_useful_S3_method_markup.
## CLASS can be a syntactic name (we could be more precise about the
## fact that these must start with a letter or '.'), or anything quoted
## by backticks (not containing backticks itself for now).  Arguably,
## non-syntactic class names should best be avoided, but R has always
## had them at least for
## R> class(bquote({.}))
## [1] "{"
## R> class(bquote((.)))
## [1] "("

## <NOTE>
## Handling S3/S4 method markup is somewhat tricky.
## When using R to parse the usage entries, we turn the
##   \METHOD{GENERIC}{CLASS_OR_SIGLIST}(args)
## markup into (something which parses to) a function call by suitably
## quoting the \METHOD{GENERIC}{CLASS_OR_SIGLIST} part.  In case of a
## replacement method
##   \METHOD{GENERIC}{CLASS_OR_SIGLIST}(args) <- value
## parsing results in a
##   \METHOD{GENERIC}{CLASS_OR_SIGLIST}<-
## pseudo name, which need to be transformed to
##   \METHOD{GENERIC<-}{CLASS_OR_SIGLIST}
## We currently use double quoting for the parse step.  As we also allow
## for non-syntactic class names quoted by backticks, this means that
## double quotes and backslashes need to be escaped.  Alternatively, we
## could strip backticks right away and quote by backticks, but then the
## replacement method transformation would need different regexps.
## </NOTE>

.S3_method_markup_regexp <-
    sprintf("(\\\\(S3)?method\\{(%s)\\}\\{(%s)\\})",
            paste(c("[._[:alnum:]]*",
                    ## Subscripting
                    "\\$", "\\[\\[?",
                    ## Binary operators and unary '!'.
                    "\\+", "\\-", "\\*", "\\/", "\\^",
                    "<=?", ">=?", "!=?", "==", "\\&", "\\|",
                    "\\%[[:alnum:][:punct:]]*\\%"),
                  collapse = "|"),
            "[._[:alnum:]]+|`[^`]+`")

### ** .S4_method_markup_regexp

## For matching \S4method{GENERIC}{SIGLIST}.
## SIGLIST can be a comma separated list of CLASS specs as above.

.S4_method_markup_regexp <-
    sprintf("(\\\\S4method\\{(%s)\\}\\{(%s)\\})",
            paste(c("[._[:alnum:]]*",
                    ## Subscripting
                    "\\$", "\\[\\[?"),
                  collapse = "|"),
            "(([._[:alnum:]]+|`[^`]+`),)*([._[:alnum:]]+|`[^`]+`)")

### ** .valid_maintainer_field_regexp

.make_RFC_2822_email_address_regexp <-
function()
{
    ## Local part consists of ASCII letters and digits, the characters
    ##   ! # $ % * / ? | ^ { } ` ~ & ' + = _ -
    ## and . provided it is not leading or trailing or repeated, or must
    ## be a quoted string.
    ## Domain part consists of dot-separated elements consisting of
    ## ASCII letters, digits and hyphen.
    ## We could also check that the local and domain parts are no longer
    ## than 64 and 255 characters, respectively.
    ## See http://en.wikipedia.org/wiki/Email_address.
    ASCII_letters_and_digits <-
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    l <- sprintf("[%s%s]", ASCII_letters_and_digits, "!#$%*/?|^{}`~&'+=_-")
    d <- sprintf("[%s%s]", ASCII_letters_and_digits, "-")
    ## Be careful to arrange the hyphens to come last in the range spec.
    sprintf("(\\\".+\\\"|(%s+\\.)*%s+)@(%s+\\.)*%s+", l, l, d, d)
}

.valid_maintainer_field_regexp <-
    sprintf("^[[:space:]]*(.*<%s>|ORPHANED)[[:space:]]*$",
            .make_RFC_2822_email_address_regexp())


### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
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
#  File src/library/tools/R/Rd2HTML.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

## also used by Rd2latex, but only 'topic' and 'dest'
get_link <- function(arg, tag, Rdfile) {
    ## 'topic' is the name to display, 'dest' is the topic to link to
    ## optionaly in package 'pkg'.  If 'target' is set it is the file
    ## to link to in HTML help

    ## \link[=bar]{foo} means shows foo but treat this as a link to bar.
    ## \link[pkg]{bar} means show bar and link to *file* bar in package pkg
    ## \link{pkg:bar]{foo} means show foo and link to file bar in package pkg.
    ## As from 2.10.0, look for topic 'bar' if file not found.

    if (!all(RdTags(arg) == "TEXT"))
    	stopRd(arg, Rdfile, "Bad \\link text")

    option <- attr(arg, "Rd_option")

    topic <- dest <- paste(unlist(arg), collapse = "")
    targetfile <- NULL
    pkg <- NULL
    if (!is.null(option)) {
        if (!identical(attr(option, "Rd_tag"), "TEXT"))
    	    stopRd(option, Rdfile, "Bad \\link option -- must be text")
    	if (grepl("^=", option, perl = TRUE, useBytes = TRUE))
    	    dest <- psub1("^=", "", option)
    	else if (grepl(":", option, perl = TRUE, useBytes = TRUE)) {
    	    targetfile <- psub1("^[^:]*:", "", option)
    	    pkg <- psub1(":.*", "", option)
    	} else {
            targetfile <- dest
    	    pkg <- as.character(option)
    	}
    }
    if (tag == "\\linkS4class") dest <- paste(dest, "-class", sep="")
    list(topic = topic, dest = dest, pkg = pkg, targetfile = targetfile)
}


# translation of Utils.pm function of the same name, plus "unknown"
mime_canonical_encoding <- function(encoding)
{
    encoding[encoding %in% c("", "unknown")] <- localeToCharset()[1]
    encoding <- tolower(encoding)
    encoding <- sub("iso_8859-([0-9]+)", "iso-8859-\\1", encoding)
    encoding <- sub("iso8859-([0-9]+)", "iso-8859-\\1", encoding)
    encoding[encoding == "latin1"] <-  "iso-8859-1"
    encoding[encoding == "latin2"] <-  "iso-8859-2"
    encoding[encoding == "latin3"] <-  "iso-8859-3"
    encoding[encoding == "latin4"] <-  "iso-8859-4"
    encoding[encoding == "cyrillic"] <-"iso-8859-5"
    encoding[encoding == "arabic"] <-  "iso-8859-6"
    encoding[encoding == "greek"] <-   "iso-8859-7"
    encoding[encoding == "hebrew"] <-  "iso-8859-8"
    encoding[encoding == "latin5"] <-  "iso-8859-9"
    encoding[encoding == "latin6"] <-  "iso-8859-10"
    encoding[encoding == "latin8"] <-  "iso-8859-14"
    encoding[encoding == "latin-9"] <- "iso-8859-15"
    encoding[encoding == "latin10"] <- "iso-8859-16"
    encoding[encoding == "utf8"] <-    "utf-8"
    encoding[encoding == "ascii"] <-   "us-ascii" # from W3C validator
    encoding
}

## This gets used two ways:

## 1) With dynamic = TRUE from tools:::httpd()
##    Here generated links are of the forms
##    ../../pkg/help/topic
##    file.html
##    ../../pkg/html/file.html
##    and links are never missing: topics are always linked as
##    ../../pkg/help/topic for the current packages, and this means
##    'search this package then all the others, and show all matches
##    if we need to go outside this packages'

## 2) With dynamic = FALSE from .convertRdfiles (with Links[2], used for
##    prebuilt HTML pages) and .Rdconv (no link lookup)
##    Here generated links are of the forms
##    file.html
##    ../../pkg/html/file.html
##    and missing links (those without an explicit package, and
##    those topics not in Links[2]) don't get linked anywhere.

## FIXME: better to use XHTML
Rd2HTML <-
    function(Rd, out = "", package = "", defines = .Platform$OS.type,
             Links = NULL, Links2 = NULL,
             stages = "render", outputEncoding = "UTF-8",
             dynamic = FALSE, no_links = FALSE, ...)
{
    if (missing(no_links) && is.null(Links) && !dynamic) no_links <- TRUE
    version <- ""
    if(!identical(package, "")) {
        if(length(package) > 1L) {
            version <- package[2L]
            package <- package[1L]
        } else {
            dir <- dirname(package)
            if((dir != "") &&
               file_test("-f", dfile <- file.path(package,
                                                  "DESCRIPTION"))) {
                version <- .read_description(dfile)["Version"]
                package <- basename(package)
            } else {
                ## Should we really do this?
                ## Used when Rdconv is given a package argument.
                version <- utils::packageDescription(package,
                                                     fields = "Version")
            }
        }
        if(is.na(version)) version <- ""
    }

    ## writeLines by default re-encodes strings to the local encoding.
    ## Avoid that by useBytes=TRUE
    writeLinesUTF8 <-
        if (outputEncoding == "UTF-8" ||
           (outputEncoding == "" && l10n_info()[["UTF-8"]])) {
        function(x, con, outputEncoding, ...)
            writeLines(x, con, useBytes = TRUE, ...)
    } else {
        function(x, con, outputEncoding, ...) {
            x <- iconv(x, "UTF-8", outputEncoding, sub="byte", mark=FALSE)
            writeLines(x, con, useBytes = TRUE, ...)
        }
    }

    of <- function(...)
        writeLinesUTF8(paste(...), con, outputEncoding, sep = "")
    of0 <- function(...)
        writeLinesUTF8(paste(..., sep=""), con, outputEncoding, sep = "")
    of1 <- function(text)
        writeLinesUTF8(text, con, outputEncoding, sep = "")

    pendingClose <- pendingOpen <- character(0)  # Used for infix methods

### These correspond to HTML wrappers
    HTMLTags <- c("\\bold"="B",
    	          "\\cite"="CITE",
                  "\\code"="code",
                  "\\command"="CODE",
                  "\\dfn"="DFN",
                  "\\emph"="EM",
                  "\\kbd"="KBD",
                  "\\preformatted"="pre",
#                  "\\special"="PRE",
                  "\\strong"="STRONG",
                  "\\var"="VAR",
                  "\\verb"="PRE")
    # These have simple substitutions
    HTMLEscapes <- c("\\R"='<font face="Courier New,Courier" color="#666666"><b>R</b></font>',
    		     "\\cr"="<br>",
    		     "\\dots"="...",
    		     "\\ldots"="...")
    ## These correspond to idiosyncratic wrappers
    HTMLLeft <- c("\\acronym"='<acronym><span class="acronym">',
    		  "\\donttest"="",
    		  "\\env"='<span class="env">',
                  "\\file"='&lsquo;<span class="file">',
                  "\\option"='<span class="option">',
                  "\\pkg"='<span class="pkg">',
                  "\\samp"='<span class="samp">',
                  "\\sQuote"="&lsquo;",
                  "\\dQuote"="&ldquo;")
    HTMLRight <- c("\\acronym"='</span></acronym>',
    		   "\\donttest"="",
    		   "\\env"="</span>",
                   "\\file"='</span>&rsquo;',
                   "\\option"="</span>",
                   "\\pkg"="</span>",
                   "\\samp"="</span>",
                   "\\sQuote"="&rsquo;",
                   "\\dQuote"="&rdquo;")

    trim <- function(x) {
        x <- psub1("^\\s*", "", x)
        psub1("\\s*$", "", x)
    }

    addParaBreaks <- function(x, tag) {
        start <- attr(x, "srcref")[2L] # FIXME: what if no srcref?, start col
	if (isBlankLineRd(x)) "</p>\n<p>\n"
	else if (start == 1) psub("^\\s+", "", x)
        else x
    }

    htmlify <- function(x) {
	x <- fsub("&", "&amp;", x)
	x <- fsub("---", "&mdash;", x)
	x <- fsub("--", "&ndash;", x)
	x <- fsub("``", "&ldquo;", x)
	x <- fsub("''", "&rdquo;", x)
        x <- psub("`([^']+)'", "&lsquo;\\1&rsquo;", x)
	x <- fsub("`", "'", x)
	x <- fsub("<", "&lt;", x)
	x <- fsub(">", "&gt;", x)
	x
    }

    vhtmlify <- function(x) { # code version
	x <- fsub("&", "&amp;", x)
	x <- fsub("<", "&lt;", x)
	x <- fsub(">", "&gt;", x)
	x
    }

    HTMLeqn <- function(x)
    {
        x <- htmlify(x)
        ## historical escapes for math
        x <- psub("\\\\(Gamma|alpha|Alpha|pi|mu|sigma|Sigma|lambda|beta|epsilaon)", "&\\1;", x)
        x <- fsub("\\left(", "(", x)
        x <- fsub("\\right", ")", x)
        x <- fsub("\\le", "&lt;=", x)
        x <- fsub("\\ge", "&gt;=", x)
        x
    }

    writeWrapped <- function(tag, block) {
    	of0("<", HTMLTags[tag], ">")
    	writeContent(block, tag)
    	of0("</",  HTMLTags[tag], ">")
    }

    checkInfixMethod <- function(blocks)
    	# Is this a method which needs special formatting?
    	if ( length(blocks) == 1 && RdTags(blocks) == "TEXT" &&
    	     blocks[[1]] %in% c("[", "[[", "$") ) {
    	    pendingOpen <<- blocks[[1]]
    	    TRUE
    	} else FALSE

    writeLink <- function(tag, block) {
	parts <- get_link(block, tag, Rdfile)

        writeHref <- function() {
            if (!no_links) of0('<a href="', htmlfile, '">')
            writeContent(block, tag)
            if (!no_links) of1('</a>')
        }

    	if (is.null(parts$targetfile)) {
            ## ---------------- \link{topic} and \link[=topic]{foo}
            topic <- parts$dest
    	    if (dynamic) { # never called with package=""
                htmlfile <- paste("../../", package, "/help/", topic, sep="")
                writeHref()
                return()
            } else {
            	htmlfile  <- NA_character_
            	if (!is.null(Links)) {
            	    tmp <- Links[topic]
            	    if (!is.na(tmp)) htmlfile <- tmp
                    else {
                        tmp <- Links2[topic]
                        if (!is.na(tmp)) htmlfile <- tmp
                    }
            	}
            }
            if (is.na(htmlfile)) {
                ## Used to use the search engine, but we no longer have one,
                ## and we don't get here for dynamic help.
                if (!no_links)
                    warnRd(block, Rdfile, "missing link ", sQuote(topic))
                writeContent(block, tag)
            } else {
                ## treat links in the same package specially -- was needed for CHM
                pkg_regexp <- paste("^../../", package, "/html/", sep = "")
                if (grepl(pkg_regexp, htmlfile)) {
                    htmlfile <- sub(pkg_regexp, "", htmlfile)
                }
                writeHref()
            }
    	} else {
            ## ----------------- \link[pkg]{file} and \link[pkg:file]{bar}
            htmlfile <- paste(parts$targetfile, ".html", sep="")
            if (!dynamic && !no_links &&
               nzchar(pkgpath <- system.file(package = parts$pkg))) {
                ## check the link, only if the package is found
                OK <- FALSE
                if (!file.exists(file.path(pkgpath, "html", htmlfile))) {
                    ## does not exist as static HTML, so look harder
                    f <- file.path(pkgpath, "help", "paths.rds")
                    if (file.exists(f)) {
                        paths <- sub("\\.[Rr]d$", "", basename(.readRDS(f)))
                        OK <- parts$targetfile %in% paths
                    }
                } else OK <- TRUE
                if (!OK) {
                    ## so how about as a topic?
                    file <- index.search(parts$targetfile, pkgpath,
                                         type = "html")
                    if (nzchar(file)) {
                        warnRd(block, Rdfile,
                               "file link ", sQuote(parts$targetfile),
                               " in package ", sQuote(parts$pkg),
                               " does not exist and so has been treated as a topic")
                        parts$targetfile <- basename(file)
                    } else {
                        warnRd(block, Rdfile, "missing file link ",
                               sQuote(parts$targetfile))
                    }
                }
            }
            if (parts$pkg == package) {
                ## use href = "file.html"
                writeHref()
            } else {
                ## href = "../../pkg/html/file.html"
                htmlfile <- paste("../../", parts$pkg, "/html/", htmlfile,
                                  sep="")
                writeHref()
            }
        }
    }

    writeComment <- function(txt) {
       	txt <- psub1("^%", "", txt)
       	txt <- fsub1("\n", "", txt)
       	txt <- fsub("--", "- - ", txt)
       	txt <- fsub(">", "&gt;", txt)
	of("<!-- ", txt, " -->\n")
    }

    writeLR <- function(block, tag) {
        of1(HTMLLeft[tag])
        writeContent(block, tag)
        of1(HTMLRight[tag])
    }

    writeDR <- function(block, tag) {
        if (length(block) > 1L) {
            of1('## Not run: ')
            writeContent(block, tag)
            of1('\n## End(Not run)')
        } else {
            of1('## Not run: ')
            writeContent(block, tag)
       }
    }

    writeBlock <- function(block, tag, blocktag) {
	switch(tag,
               UNKNOWN =,
               VERB =,
               RCODE = of1(vhtmlify(block)),
               TEXT = of1(if(blocktag == "\\command") vhtmlify(block) else addParaBreaks(htmlify(block), blocktag)),
               COMMENT = {},
               LIST =,
               "\\describe"=,
               "\\enumerate"=,
               "\\itemize" = writeContent(block, tag),
               "\\bold" =,
               "\\cite" =,
               "\\code" =,
               "\\command" =,
               "\\dfn" =,
               "\\emph" =,
               "\\kbd" =,
               "\\preformatted" =,
               "\\strong" =,
               "\\var" =,
               "\\verb" = writeWrapped(tag, block),
               "\\special" = writeContent(block, tag), ## FIXME, verbatim?
               "\\linkS4class" =,
               "\\link" = writeLink(tag, block),
               ## cwhmisc has an empty \\email
               "\\email" = if (length(block)) {
                   url <- paste(as.character(block), collapse="")
                   url <- gsub("\n", "", url)
                   of0('<a href="mailto:', url, '">', htmlify(url), '</a>')},
               ## FIXME: encode, not htmlify
               ## watch out for empty URLs (TeachingDemos has one)
               "\\url" = if(length(block)) {
                   url <- paste(as.character(block), collapse="")
                   url <- gsub("\n", "", url)
                   of0('<a href="', url, '">', url, '</a>')
               },
               "\\Sexpr"= of0(as.character.Rd(block, deparse=TRUE)),
               "\\cr" =,
               "\\dots" =,
               "\\ldots" =,
               "\\R" = of1(HTMLEscapes[tag]),
               "\\acronym" =,
               "\\donttest" =,
               "\\env" =,
               "\\file" =,
               "\\option" =,
               "\\pkg" =,
               "\\samp" =,
               "\\sQuote" =,
               "\\dQuote" =  writeLR(block, tag),
               "\\dontrun"= writeDR(block, tag),
               "\\enc" = writeContent(block[[1L]], tag),
               "\\eqn" = {
                   of1("<i>")
                   block <- block[[length(block)]];
                   ## FIXME: space stripping needed: see Special.html
                   writeContent(block, tag)
                   of1("</i>")
               },
               "\\deqn" = {
                   of1('</p><p align="center"><i>')
                   block <- block[[length(block)]];
                   writeContent(block, tag)
                   of1('</i></p><p>')
               },
               "\\dontshow" =,
               "\\testonly" = {}, # do nothing
               "\\method" =,
               "\\S3method" = {
                   class <- as.character(block[[2L]])
                   if (class == "default")
                       of1('## Default S3 method:\n')
                   else {
                       of1("## S3 method for class '")
                       writeContent(block[[2L]], tag)
                       of1("':\n")
                   }
                   if (!checkInfixMethod(block[[1L]]))
                       writeContent(block[[1L]], tag)
               },
               "\\S4method" = {
                   of1("## S4 method for signature '")
                   writeContent(block[[2L]], tag)
                   of1("':\n")
                   if (!checkInfixMethod(block[[1L]]))
                       writeContent(block[[1L]], tag)
               },
               "\\tabular" = writeTabular(block),
               "\\if" =,
               "\\ifelse" =
               	    if (testRdConditional("html", block, Rdfile))
			writeContent(block[[2L]], tag)
		    else if (tag == "\\ifelse")
		    	writeContent(block[[3L]], tag),
               "\\out" = for (i in seq_along(block))
		   of1(block[[i]]),
               stopRd(block, Rdfile, "Tag ", tag, " not recognized")
               )
    }

    writeTabular <- function(table) {
    	format <- table[[1L]]
    	content <- table[[2L]]
    	if (length(format) != 1 || RdTags(format) != "TEXT")
    	    stopRd(table, Rdfile, "\\tabular format must be simple text")
    	format <- strsplit(format[[1L]], "", fixed = TRUE)[[1L]]
    	if (!all(format %in% c("l", "c", "r")))
    	    stopRd(table, Rdfile,
                   "Unrecognized \\tabular format: ", table[[1L]][[1L]])
        format <- c(l="left", c="center", r="right")[format]

        tags <- RdTags(content)

        of1('\n</p>\n<table summary="Rd table">\n')
        newrow <- TRUE
        newcol <- TRUE
        for (i in seq_along(tags)) {
            if (newrow) {
            	of1("<tr>\n ")
            	newrow <- FALSE
            	col <- 0
            	newcol <- TRUE
            }
            if (newcol) {
                col <- col + 1L
                if (col > length(format))
                    stopRd(table, Rdfile,
                           "Only ", length(format),
                           " columns allowed in this table")
            	of0('<td align="', format[col], '">')
            	newcol <- FALSE
            }
            switch(tags[i],
            "\\tab" = {
            	of1('</td>')
            	newcol <- TRUE
            },
            "\\cr" = {
            	if (!newcol) of1('</td>')
            	of1('\n</tr>\n')
            	newrow <- TRUE
            },
            writeBlock(content[[i]], tags[i], "\\tabular"))
        }
        if (!newcol) of1('</td>')
        if (!newrow) of1('\n</tr>\n')
        of1('\n</table><p>\n')
    }

    writeContent <- function(blocks, blocktag) {
        inlist <- FALSE
        itemskip <- FALSE

	tags <- RdTags(blocks)

	for (i in seq_along(tags)) {
            tag <- tags[i]
            block <- blocks[[i]]
            if (length(pendingOpen)) { # Handle $, [ or [[ methods
            	if (tag == "RCODE" && grepl("^\\(", block)) {
            	    block <- sub("^\\(", "", block)
            	    arg1 <- sub("[,)[:space:]].*", "", block)
            	    block <- sub(paste(arg1, "[[:space:]]*,[[:space:]]*",
                                       sep = ""), "", block)
            	    of0(arg1, pendingOpen)
            	    if (pendingOpen == "$")
            	    	pendingClose <<- ""
            	    else
            	    	pendingClose <<- chartr("[", "]", pendingOpen)
            	} else of0("`", pendingOpen, "`")
            	pendingOpen <<- character()
            }
            if (length(pendingClose) && tag == "RCODE"
                && grepl("\\)", block)) { # Finish it off...
            	of0(sub("\\).*", "", block), pendingClose)
            	block <- sub("[^)]*\\)", "", block)
            	pendingClose <<- character()
            }
            switch(tag,
            "\\item" = {
    	    	if (!inlist) {
    	    	    switch(blocktag,
                           "\\value" =  of1('<table summary="R valueblock">\n'),
                           "\\arguments" = of1('<table summary="R argblock">\n'),
                           "\\itemize" = of1("<ul>\n"),
                           "\\enumerate" = of1("<ol>\n"),
                           "\\describe" = of1("<dl>\n"))
    	    	    inlist <- TRUE
    		} else {
    		    if (blocktag %in% c("\\itemize", "\\enumerate")) {
    		    	of1("</li>\n")
                        ## We have \item ..., so need to skip the space.
                        itemskip <- TRUE
                    }
    		}
    		switch(blocktag,
   		"\\value"=,
     		"\\arguments"={
    		    of1('<tr valign="top"><td><code>')
    		    writeContent(block[[1L]], tag)
    		    of1('</code></td>\n<td>\n')
    		    writeContent(block[[2L]], tag)
    		    of1('</td></tr>')
    		},
    		"\\describe"= {
    		    of1("<dt>")
    		    writeContent(block[[1L]], tag)
    		    of1("</dt><dd>")
    		    writeContent(block[[2L]], tag)
    		    of1("</dd>")
    		},
    		"\\enumerate" =,
    		"\\itemize"= of1("<li>"))
    	    },
    	    { # default
    	    	if (inlist && !(blocktag %in% c("\\arguments", "\\itemize", "\\enumerate"))
    	    	           && !(tag == "TEXT" && isBlankRd(block))) {
    	    	    switch(blocktag,
     	    	    "\\value" = of1("</table>\n"),
    	    	    "\\describe" = of1("</dl>\n"))
    		    inlist <- FALSE
    		}
                if (itemskip) {
                    ## The next item must be TEXT, and start with a space.
                    itemskip <- FALSE
                    if (tag == "TEXT") {
                        txt <- psub("^ ", "", as.character(block))
                        of1(txt)
                    } else writeBlock(block, tag, blocktag) # should not happen
                } else writeBlock(block, tag, blocktag)
    	    })
	}
	if (inlist)
	    switch(blocktag,
		"\\value"=,
		"\\arguments" = of1("</table>\n"),
		"\\itemize" = of1("</ul>\n"),
		"\\enumerate" = of1("</ol>\n"),
		# "\\value"=,
		"\\describe" = of1("</dl>\n"))
    }

    writeSection <- function(section, tag) {
        if (tag %in% c("\\alias", "\\concept", "\\encoding", "\\keyword"))
            return() ## \alias only used on CHM header
    	of1("\n\n<h3>")
    	if (tag == "\\section") {
    	    title <- section[[1L]]
    	    section <- section[[2L]]
            ## FIXME: this needs trimming of whitespace
    	    writeContent(title, tag)
    	} else
    	    of1(sectionTitles[tag])
    	if (tag %in% c("\\examples", "\\synopsis", "\\usage"))
    	    para <- "pre" else para <- "p"
        of1("</h3>\n")
        ## \arguments is a single table, not a para
        if (tag == "\\arguments") para <- ""
    	if (nzchar(para)) of0("\n<", para, ">")
    	if (length(section)) {
	    ## There may be an initial \n, so remove that
	    s1 <- section[[1L]][1L]
	    if (RdTags(s1) == "TEXT" && s1 == "\n") section <- section[-1]
	    writeContent(section, tag)
	}
    	if (nzchar(para)) of0("</", para, ">\n")
    }

    if (is.character(out)) {
        if (out == "") {
            con <- stdout()
        } else {
	    con <- file(out, "wt")
	    on.exit(close(con))
	}
    } else {
    	con <- out
    	out <- summary(con)$description
    }

    Rd <- prepare_Rd(Rd, defines = defines, stages = stages, ...)
    Rdfile <- attr(Rd, "Rdfile")
    sections <- RdTags(Rd)

    title <- Rd[[1L]]
    name <- htmlify(Rd[[2L]][[1L]])

    of0('<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">\n',
        '<html><head><title>R: ')
    ## special for now, as we need to remove leading and trailing spaces
    title <- trim(as.character(title))
    title <- htmlify(paste(psub1("^\\s+", "", title[nzchar(title)]),
                           collapse = " "))
    of1(title)
    of0('</title>\n',
        '<meta http-equiv="Content-Type" content="text/html; charset=',
        mime_canonical_encoding(outputEncoding),
        '">\n')

    of0('<link rel="stylesheet" type="text/css"',
        if (no_links) 'href="R.css">' else 'href="../../R.css">',
        '\n</head><body>\n\n',
        '<table width="100%" summary="page for ', name, ' {', package,
        '}"><tr><td>',name,' {', package,
        '}</td><td align="right">R Documentation</td></tr></table>\n\n')

    of0("<h2>", title,'</h2>\n')

    for (i in seq_along(sections)[-(1:2)])
    	writeSection(Rd[[i]], sections[i])

    if(version != "")
        version <- paste('Package <em>', package,
                         '</em> version ', version,
                         ' ', sep='')
    of0('\n',
        '<hr><div align="center">[', version,
        if (!no_links) '<a href="00Index.html">Index</a>',
        ']</div>\n', '</body></html>\n')
    invisible(out)
}

findHTMLlinks <- function(pkgDir = "", lib.loc = NULL, level = 0:2)
{
    ## The priority order is
    ## This package (level 0)
    ## The standard packages (level 1)
    ## along lib.loc (level 2)

    if (is.null(lib.loc)) lib.loc <- .libPaths()

    Links <- list()
    if (2 %in% level)
        Links <- c(Links, lapply(rev(lib.loc), .find_HTML_links_in_library))
    if (1 %in% level) {
        base <- unlist(.get_standard_package_names()[c("base", "recommended")],
                       use.names = FALSE)
        Links <- c(Links,
                   lapply(file.path(.Library, base),
                          .find_HTML_links_in_package))
    }
    if (0 %in% level && nzchar(pkgDir))
        Links <- c(Links, list(.find_HTML_links_in_package(pkgDir)))
    Links <- unlist(Links)

    ## now latest names are newest, so
    Links <- rev(Links)
    Links <- Links[!duplicated(names(Links))]
    gsub("[Rr]d$", "html", Links)
}

.find_HTML_links_in_package <-
function(dir)
{
    if (file_test("-f", f <- file.path(dir, "Meta", "links.rds")))
        .readRDS(f)
    else if (file_test("-f", f <- file.path(dir, "Meta", "Rd.rds")))
        .build_links_index(.readRDS(f), basename(dir))
    else character()
}

.find_HTML_links_in_library <-
function(dir)
{
    if (file_test("-f", f <- file.path(dir, ".Meta", "links.rds")))
        .readRDS(f)
    else
        .build_library_links_index(dir)
}

.build_library_links_index <-
function(dir)
{
    unlist(lapply(rev(dir(dir, full.names = TRUE)),
                  .find_HTML_links_in_package))
}
#  File src/library/tools/R/Rd2ex.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

## This warns on multiple \examples sections, never fails.

Rd2ex <-
    function(Rd, out="", defines=.Platform$OS.type, stages="render",
             outputEncoding="UTF-8", ...)
{
    encode_warn <- FALSE
    WriteLines <- function(x, con, outputEncoding, ...) {
        if (outputEncoding != "UTF-8") {
            x <- iconv(x, "UTF-8", outputEncoding,  mark=FALSE)
            if (any(is.na(x))) {
                x <- iconv(x, "UTF-8", outputEncoding, sub="byte", mark=FALSE)
                encode_warn <<- TRUE
            }
        }
        writeLines(x, con, useBytes = TRUE, ...)
    }

    dropNewline <- FALSE # drop next char if newline

    of0 <- function(...)
        of1(paste(..., sep=""))
    of1 <- function(text) {
        if (dropNewline && length(text)) {
            text[1] <- psub("^\n", "", text[1])
            dropNewline <<- FALSE
        }
        WriteLines(text, con, outputEncoding, sep = "")
    }
    wr <- function(x)
        paste("###", strwrap(remap(x), 73L, indent=1L, exdent=3L),
              sep="", collapse="\n")

    remap <- function(x) {
        if(!length(x)) return(x)
        ## \link, \var are untouched in comments: e.g. is.R
        x <- psub("\\\\(link|var)\\{([^}]+)\\}", "\\2", x)
        ## not valid in perl: use lookbehind instead.
        ## x <- gsub("(^|[^\\])\\\\([%{])", "\\1\\2", x)
        x <- psub("(?<!\\\\)\\\\([%{])", "\\1", x)
        x <- psub("\\\\(l|)dots", "...", x)
        ## FIXME:  Previously said "Want to leave file bytes unchanged"
        x
    }

    render <- function(x, prefix = "")
    {
        tag <- attr(x, "Rd_tag")
        if(tag %in% c("\\dontshow", "\\testonly")) {
            of1("## Don't show: ")
            if (!grepl("^\n", x[[1L]][1L], perl = TRUE) && RdTags(x)[1] != "COMMENT")
                writeLines("", con)
            for(i in seq_along(x)) render(x[[i]], prefix)
            last <- x[[length(x)]]
            if (!grepl("\n$", last[length(last)], perl = TRUE))
                writeLines("", con)
            of1("## End Don't show")
        } else if (tag  == "\\dontrun") {
            ## Special case for one line.
            if (length(x) == 1L) {
                of1("## Not run: ")
                render(x[[1L]], prefix)
            } else {
                of1("## Not run: ")
                if (!grepl("^\n", x[[1L]][1L], perl = TRUE) && RdTags(x)[1] != "COMMENT") {
                    writeLines("", con)
                    render(x[[1L]], paste("##D", prefix))
                } else render(x[[1L]], prefix)
                for(i in 2:length(x)) render(x[[i]], paste("##D", prefix))
                last <- x[[length(x)]]
                if (!grepl("\n$", last[length(last)], perl = TRUE))
                    writeLines("", con)
                of1("## End(Not run)")
            }
        } else if (tag  == "\\donttest") {
            of1("## No test: ")
            if (!grepl("^\n", x[[1L]][1L], perl = TRUE) && RdTags(x)[1] != "COMMENT")
                writeLines("", con)
            for(i in seq_along(x)) render(x[[i]], prefix)
            last <- x[[length(x)]]
            if (!grepl("\n$", last[length(last)], perl = TRUE))
                writeLines("", con)
            of1("## End(No test)")
        } else if (tag == "COMMENT") {
            ## % can escape a whole line (e.g. beavers.Rd) or
            ## be trailing when we want a NL
            ## This is not right (leading spaces?) but it may do
            if(attr(x, "srcref")[2L] == 1L) dropNewline <<- TRUE
        } else if (tag %in% c("\\dots", "\\ldots")) {
            of1("...")
        } else if (tag == "\\if" || tag == "\\ifelse") {
            if (testRdConditional("example", x, Rdfile))
            	for(i in seq_along(x[[2]])) render(x[[2]][[i]], prefix)
            else if (tag == "\\ifelse")
            	for(i in seq_along(x[[3]])) render(x[[3]][[i]], prefix)
        } else if (tag == "\\out") {
            for (i in seq_along(x))
            	of1(x[[i]])
        } else {
            txt <- unlist(x)
            of0(prefix, remap(txt))
        }
    }

    Rd <- prepare_Rd(Rd, defines=defines, stages=stages, ...)
    Rdfile <- attr(Rd, "Rdfile")
    sections <- RdTags(Rd)

    ## FIXME should we skip empty \examples sections?
    where <- which(sections == "\\examples")
    if(length(where)) {
	if (is.character(out)) {
	    if(out == "") {
		con <- stdout()
	    } else {
		con <- file(out, "wt")
		on.exit(close(con))
	    }
        } else {
            con <- out
            out <- summary(con)$description
        }

        if(length(where) > 1L)
            warning("more than one \\examples section, using the first")
        ex <- Rd[[ where[1L] ]]
        exl <- unlist(ex)
        ## Do we need to output an encoding?
        if(length(exl) && any(Encoding(exl) != "unknown")) {
            if(any(f <- sections == "\\encoding")) {
                encoding <- unlist(Rd[[which(f)]])[1L]
                ## FIXME: which should win here?
                if(nzchar(outputEncoding))
                    encoding <- outputEncoding
                else
                    outputEncoding <- encoding
                of0("### Encoding: ", encoding, "\n\n") #
            }
        }
        nameblk <- sections == "\\name"
        if (any(nameblk)) {
            ## perl wrapped here, but it seems unnecessary
            name <- as.character(Rd[[ which(nameblk)[1L] ]])
            of0("### Name: ", name, "\n")
        }
        titleblk <- sections == "\\title"
        if (any(titleblk)) {
            title <- as.character(Rd[[ which(titleblk)[1L] ]])
            ## remove empty lines, leading whitespace
            title <- paste(psub1("^\\s+", "", title[nzchar(title)]),
                           collapse=" ")
            ## FIXME: more?
            title <- psub("(---|--)", "-", title)
        } else title <- "No title found"
        of0(wr(paste("Title: ", title, sep='')), "\n")
        aliasblks <- sections == "\\alias"
        if (any(aliasblks)) {
            aliases <- unlist(Rd[aliasblks])
            sp <- grep(" ", aliases, fixed = TRUE)
            aliases[sp] <- paste("'", aliases[sp], "'", sep = "")
            of0(wr(paste("Aliases: ", paste(aliases, collapse=" "), sep="")),
                "\n")
        }
        keyblks <- sections == "\\keyword"
        if (any(keyblks)) {
            ## some people have only empty keyword blocks.
            keys <- unlist(Rd[keyblks])
            if(length(keys)) {
                keys <- psub("^\\s+", "", keys)
                of0(wr(paste("Keywords: ",
                             paste(keys, collapse=" "), sep="")), "\n")
            }
        }
        writeLines(c("", "### ** Examples"), con)
        for (i in seq_along(ex)) render(ex[[i]])
        of1("\n\n\n")
    }
    invisible(out)
}
#  File src/library/tools/R/Rd2tex.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

## TODO: can we do something useful with cross-package links?

latex_canonical_encoding  <- function(encoding)
{
    if (encoding == "") encoding <- localeToCharset()[1]
    encoding <- tolower(encoding)
    encoding <- sub("iso_8859-([0-9]+)", "iso-8859-\\1", encoding)
    encoding <- sub("iso8859-([0-9]+)", "iso-8859-\\1", encoding)

    encoding[encoding == "iso-8859-1"] <-  "latin1"
    encoding[encoding == "iso-8859-2"] <-  "latin2"
    encoding[encoding == "iso-8859-3"] <-  "latin3"
    encoding[encoding == "iso-8859-4"] <-  "latin4"
    encoding[encoding == "iso-8859-5"] <-  "cyrillic"
    encoding[encoding == "iso-8859-6"] <-  "arabic"
    encoding[encoding == "iso-8859-7"] <-  "greek"
    encoding[encoding == "iso-8859-8"] <-  "hebrew"
    encoding[encoding == "iso-8859-9"] <-  "latin5"
    encoding[encoding == "iso-8859-10"] <-  "latin6"
    encoding[encoding == "iso-8859-14"] <-  "latin8"
    encoding[encoding %in% c("latin-9", "iso-8859-15")] <-  "latin9"
    encoding[encoding == "iso-8859-16"] <-  "latin10"
    encoding[encoding == "utf-8"] <-  "utf8"
    encoding
}

## 'encoding' is passed to parse_Rd, as the input encoding
Rd2latex <- function(Rd, out="", defines=.Platform$OS.type, stages="render",
		     outputEncoding = "ASCII", ...)
{
    encode_warn <- FALSE
    WriteLines <-
        if(outputEncoding == "UTF-8" ||
           (outputEncoding == "" && l10n_info()[["UTF-8"]])) {
            function(x, con, outputEncoding, ...)
                writeLines(x, con, useBytes = TRUE, ...)
        } else {
            function(x, con, outputEncoding, ...) {
                x <- iconv(x, "UTF-8", outputEncoding,  mark=FALSE)
                if (any(is.na(x))) {
                    x <- iconv(x, "UTF-8", outputEncoding,
                               sub="byte", mark=FALSE)
                    encode_warn <<- TRUE
                }
                writeLines(x, con, useBytes = TRUE, ...)
            }
    }

    last_char <- ""
    of0 <- function(...) of1(paste(..., sep=""))
    of1 <- function(text) {
        nc <- nchar(text)
        last_char <<- substr(text, nc, nc)
        WriteLines(text, con, outputEncoding, sep = "")
    }

    trim <- function(x) {
        x <- psub1("^\\s*", "", as.character(x))
        psub1("\\s*$", "", x)
    }

    envTitles <- c("\\description"="Description", "\\usage"="Usage",
        "\\synopsis"="Usage", "\\arguments"="Arguments",
        "\\format"="Format", "\\details"="Details", "\\note"="Note",
        "\\section"="", "\\author"="Author",
        "\\references"="References", "\\source"="Source",
        "\\seealso"="SeeAlso", "\\examples"="Examples",
        "\\value"="Value")

    sectionExtras <-
    c("\\usage"="verbatim",
      "\\synopsis"="verbatim",
      "\\arguments"="ldescription",
      "\\examples"="ExampleCode")

    inCodeBlock <- FALSE ## used to indicate to vtexify where we are
    inCode <- FALSE
    inEqn <- FALSE
    inPre <- FALSE

    addParaBreaks <- function(x, tag) {
        start <- attr(x, "srcref")[2L]
        if (isBlankLineRd(x)) "\n"
	else if (start == 1) psub("^\\s+", "", x)
        else x
    }

    texify <- function(x) {
        if(inEqn) return(x)
        # Need to be careful to handle backslash, so do it in three steps.
        # First, mark all the ones in the original text, but don't add
        # any other special chars
        x <- fsub("\\", "\\bsl", x)
        # Second, escape other things, introducing more backslashes
        x <- psub("([&$%_#])", "\\\\\\1", x)
        ## pretty has braces in text.
        x <- fsub("{", "\\{", x)
        x <- fsub("}", "\\}", x)
        x <- fsub("^", "\\textasciicircum{}", x)
        x <- fsub("~", "\\textasciitilde{}", x)
        # Third, add the terminal braces to the backslash
        x <- fsub("\\bsl", "\\bsl{}", x)
        x
    }

    ## version for RCODE and VERB
    ## inCodeBlock/inPre is in alltt, where only \ { } have their usual meaning
    vtexify <- function(x, code = TRUE) {
        if(inEqn) return(x)
        ## cat(sprintf("vtexify: '%s'\n", x))
        x <- psub("\\\\[l]{0,1}dots", "...", as.character(x))
        ## unescape (should not be escaped: but see kappa.Rd)
        x <- psub("\\\\([$^&~_#])", "\\1", x)
        if (inCodeBlock) {
            ## We do want to escape { }, but unmatched braces had
            ## to be escaped in earlier versions (e.g. Paren.Rd, body.tex).
            ## So fix up for now
            x <- fsub1('"\\{"', '"{"', x)
        } else if (inPre) {
            BSL = '@BSL@';
            BSL2 = '@BSLBSL@';
            #x <- fsub("\\dots", "...", x)
            ## escape any odd \, e.g. \n
            x <- fsub("\\\\", BSL, x) # change even ones
            x <- fsub("\\", BSL2, x)  # odd ones
            x <- fsub(BSL, "\\\\", x) # change back
            x <- psub("(?<!\\\\)\\{", "\\\\{", x)
            x <- psub("(?<!\\\\)}", "\\\\}", x)
            x <- fsub(BSL2, "\\bsl{}", x)
            x <- psub("\\\\\\\\var\\\\\\{([^\\\\]*)\\\\}", "\\\\var{\\1}", x)
        } else {
            ## cat(sprintf("\nvtexify in: '%s'\n", x))
            BSL = '@BSL@';
            x <- fsub("\\", BSL, x)
            x <- psub("(?<!\\\\)\\{", "\\\\{", x)
            x <- psub("(?<!\\\\)}", "\\\\}", x)
            x <- psub("(?<!\\\\)([&$%_#])", "\\\\\\1", x)
            x <- fsub("^", "\\textasciicircum{}", x)
            x <- fsub("~", "\\textasciitilde{}", x)
            x <- fsub(BSL, "\\bsl{}", x)
            ## avoid conversion to guillemets
            x <- fsub("<<", "<{}<", x)
            x <- fsub(">>", ">{}>", x)
            x <- fsub(",,", ",{},", x) # ,, is a ligature in the ae font.
            ## used to preserve \var: needed in code only, or not at all
            if(FALSE) # was code
                x <- psub("\\\\bsl{}var\\\\{([^}]+)\\\\}", "\\\\var{\\1}", x)
            ## cat(sprintf("\nvtexify out: '%s'\n", x))
        }
	x
    }

    # The quotes were Rd.sty macros, but Latex limitations (e.g. nesting \preformatted within)
    # mean we get better results expanding them here.

    wrappers <- list("\\dQuote" =c("``", "''"),
    		     "\\sQuote" =c("`", "'"),
    		     "\\cite"   =c("\\Cite{", "}"))

    writeWrapped <- function(block, tag) {
    	wrapper <- wrappers[[tag]]
    	if (is.null(wrapper))
    	    wrapper <- c(paste(tag, "{", sep=""), "}")
    	of1(wrapper[1])
    	writeContent(block, tag)
    	of1(wrapper[2])
    }

    writeURL <- function(block, tag) {
        ## really verbatim
    	of0(tag, "{",
            gsub("\n", "", paste(as.character(block), collapse="")),
            "}")
    }

    ## Currently ignores [option] except for [=dest] form
    ## (as documented)
    writeLink <- function(tag, block) {
        parts <- get_link(block, tag)
        of0("\\LinkA{", latex_escape_link(parts$topic), "}{",
            latex_link_trans0(parts$dest), "}")
    }

    writeComment <- function(txt) of0(txt, '\n')

    writeDR <- function(block, tag) {
        if (length(block) > 1L) {
            of1('## Not run: ')
            writeContent(block, tag)
            of1('\n## End(Not run)')
        } else {
            of1('## Not run: ')
            writeContent(block, tag)
       }
    }

    ltxstriptitle <- function(x)
    {
        x <- fsub("\\R", "\\R{}", x)
        x <- psub("(?<!\\\\)([&$%_#])", "\\\\\\1", x)
        x <- fsub("^", "\\textasciicircum{}", x)
        x <- fsub("~", "\\textasciitilde{}", x)
        x
    }

    latex_escape_name <- function(x)
    {
        x <- psub("([$#~_&])", "\\\\\\1", x) #- escape them
        x <- fsub("{", "\\textbraceleft{}", x)
        x <- fsub("}", "\\textbraceright{}", x)
        x <- fsub("^", "\\textasciicircum{}", x)
        x <- fsub("~", "\\textasciitilde{}", x)
        x <- fsub("%", "\\Rpercent{}", x)
        x <- fsub("\\\\", "\\textbackslash{}", x)
        ## avoid conversion to guillemets
        x <- fsub("<<", "<{}<", x)
        x <- fsub(">>", ">{}>", x)
        x
    }

    latex_escape_link <- function(x)
    {
        ## _ is already escaped
        x <- fsub("\\_", "_", x)
        latex_escape_name(x)
    }

    latex_link_trans0 <- function(x)
    {
        x <- fsub("\\Rdash", ".Rdash.", x)
        x <- fsub("-", ".Rdash.", x)
        x <- fsub("\\_", ".Rul.", x)
        x <- fsub("\\$", ".Rdol.", x)
        x <- fsub("\\^", ".Rcaret.", x)
        x <- fsub("^", ".Rcaret.", x)
        x <- fsub("_", ".Rul.", x)
        x <- fsub("$", ".Rdol.", x)
        x <- fsub("\\#", ".Rhash.", x) #
        x <- fsub("#", ".Rhash.", x)   #
        x <- fsub("\\&", ".Ramp.", x)
        x <- fsub("&", ".Ramp.", x)
        x <- fsub("\\~", ".Rtilde.", x)
        x <- fsub("~", ".Rtilde.", x)
        x <- fsub("\\%", ".Rpcent.", x)
        x <- fsub("%", ".Rpcent.", x)
        x <- fsub("\\\\", ".Rbl.", x)
        x <- fsub("{", ".Rlbrace.", x)
        x <- fsub("}", ".Rrbrace.", x)
        x
    }

    latex_code_trans  <- function(x)
    {
        BSL = '@BSL@';
        LATEX_SPECIAL = '$^&~_#'
        if(grepl(LATEX_SPECIAL, x)) {
            x <- fsub("\\\\", BSL, x)
            ## unescape (should not be escaped)
            x <- psub("\\\\([$^&~_#])", "\\1", x)
            x <- psub("[$^&~_#]", "\\1&", x) #- escape them
            x <- fsub("^", "\\textasciicircum{}", x) # ^ is SPECIAL
            x <- fsub("~", "\\textasciitilde{}", x)
            x <- fsub(BSL, "\\bsl{}", x)
            x <- fsub("\\", "\\bsl{}", x)
        }
        ## avoid conversion to guillemets
        x <- fsub("<<", "<{}<", x)
        x <- fsub(">>", ">{}>", x)
        x <- fsub(",,", ",{},", x) # ,, is a ligature in the ae font.
        x <- psub("\\\\bsl{}var\\\\{([^}]+)\\\\}", "\\var{\\1}", x)
        x
}

    latex_link_trans <- function(x)
    {
        x <- fsub("<-.", "<\\Rdash.", x)
        x <- psub("<-$", "<\\Rdash", x)
        x
    }

    latex_code_alias <- function(x)
    {
        x <- fsub("{", "\\{", x)
        x <- fsub("}", "\\}", x)
        x <- psub("(?<!\\\\)([&$%_#])", "\\\\\\1", x)
        x <- fsub("^", "\\textasciicircum{}", x)
        x <- fsub("~", "\\textasciitilde{}", x)
        x <- fsub("<-", "<\\Rdash", x)
        x <- psub("([!|])", '"\\1', x)
        x
    }

    latex_code_aliasAA <- function(x)
    {
        x <- latex_code_trans(x)
        x <- latex_link_trans(x)
        psub("\\\\([!|])", '"\\1', x)
    }

    currentAlias <- NA_character_

    writeAlias <- function(block, tag) {
        alias <- as.character(block)
        aa <- "\\aliasA{"
        ## some versions of hyperref have trouble indexing these
        ## |, || in base, |.bit, %||% in ggplot2 ...
        if(grepl("|", alias, fixed = TRUE)) aa <- "\\aliasB{"
        if(is.na(currentAlias)) currentAlias <<- name
        if (pmatch(paste(currentAlias, ".", sep=""), alias, 0L)) {
            aa <- "\\methaliasA{"
        } else currentAlias <<- alias
        ## 'name' is linked from the header
        if (alias == name) return()
        alias2 <- latex_link_trans0(alias)
        of0(aa, latex_code_alias(alias), "}{",
            latex_escape_name(name), "}{", alias2, "}\n")
    }

    writeBlock <- function(block, tag, blocktag) {
	switch(tag,
               UNKNOWN =,
               VERB = of1(vtexify(block, FALSE)),
               RCODE = of1(vtexify(block, TRUE)),
               TEXT = of1(addParaBreaks(texify(block), blocktag)),
               COMMENT = {},
               LIST = writeContent(block, tag),
               "\\describe"= { # Avoid the Rd.sty \describe, \Enumerate and \Itemize:
               		       # They don't support verbatim arguments, which we might need.
                   of1("\\begin{description}\n")
                   writeContent(block, tag)
                   of1("\n\\end{description}\n")
               },
               "\\enumerate"={
                   of1("\\begin{enumerate}\n")
                   writeContent(block, tag)
                   of1("\n\\end{enumerate}\n")
               },
               "\\itemize"= {
                   of1("\\begin{itemize}\n")
                   writeContent(block, tag)
                   of1("\n\\end{itemize}\n")
               },
               ## Verbatim-like
               "\\command"=,
               "\\env" =,
               "\\kbd"=,
               "\\option" =,
               "\\samp" = writeWrapped(block, tag),
               ## really verbatim
                "\\url"= writeURL(block, tag),
               ## R-like
               "\\code"= {
                   inCode <<- TRUE
                   writeWrapped(block, tag)
                   inCode <<- FALSE
               },
               ## simple wrappers
               "\\acronym" =,
               "\\bold"=,
               "\\dfn"=,
               "\\dQuote"=,
               "\\email"=,
               "\\emph"=,
               "\\file" =,
               "\\pkg" =,
               "\\sQuote" =,
               "\\strong"=,
               "\\var" =,
               "\\cite" =
                   if (inCodeBlock) writeContent(block, tag)
                   else writeWrapped(block, tag),
               "\\preformatted"= {
                   inPre <<- TRUE
                   of1("\\begin{alltt}")
                   writeContent(block, tag)
                   of1("\\end{alltt}\n")
                   inPre <<- FALSE
               },
               "\\Sexpr"= { of1("\\begin{verbatim}\n")  # This is only here if processing didn't get it...
	       	            of0(as.character.Rd(block, deparse=TRUE))
	       	            of1("\n\\end{verbatim}\n")
	       	          },

               "\\verb"= {
                   of0("\\AsIs{")
                   writeContent(block, tag)
                   of1("}")
               },
               "\\special"= writeContent(block, tag), ## FIXME, verbatim?
               "\\linkS4class" =,
               "\\link" = writeLink(tag, block),
               "\\cr" = of1("\\\\{}"), ## might be followed by [
               "\\dots" =,
               "\\ldots" = of1(if(inCode || inCodeBlock) "..."  else tag),
               "\\R" = of0(tag, "{}"),
               "\\donttest" = writeContent(block, tag),
               "\\dontrun"= writeDR(block, tag),
               "\\enc" = { # some people put more things in \enc than a word.
                   writeContent(block[[1L]], tag)
                   ##txt <- as.character(block[[1L]])
                   ##of1(txt)
               } ,
               "\\eqn" =,
               "\\deqn" = {
                   of0(tag, "{")
                   inEqn <<- TRUE
                   writeContent(block[[1L]], tag)
                   inEqn <<- FALSE
                   of0('}{}')
               },
               "\\dontshow" =,
               "\\testonly" = {}, # do nothing
               "\\method" =,
               "\\S3method" = {
                   ## should not get here
               },
               "\\S4method" = {
                   of1("## S4 method for signature '")
                   writeContent(block[[2L]], tag)
                   of1("':\n")
                   writeContent(block[[1L]], tag)
               },
               "\\tabular" = writeTabular(block),
               "\\if" =,
               "\\ifelse" =
		    if (testRdConditional("latex", block, Rdfile))
               		writeContent(block[[2L]], tag)
               	    else if (tag == "\\ifelse")
               	    	writeContent(block[[3L]], tag),
               "\\out" = for (i in seq_along(block))
		   of1(block[[i]]),
               stopRd(block, Rdfile, "Tag ", tag, " not recognized")
               )
    }

    writeTabular <- function(table) {
        ## FIXME does no check of correct format
    	format <- table[[1L]]
    	content <- table[[2L]]
    	if (length(format) != 1 || RdTags(format) != "TEXT")
    	    stopRd(table, Rdfile, "\\tabular format must be simple text")
        tags <- RdTags(content)
        of0('\n\\Tabular{', format, '}{')
        for (i in seq_along(tags)) {
            switch(tags[i],
                   "\\tab" = of1("&"),
                   "\\cr" = of1("\\\\{}"),
                   writeBlock(content[[i]], tags[i], "\\tabular"))
        }
        of1('}')
    }

    writeContent <- function(blocks, blocktag) {
        inList <- FALSE
        itemskip <- FALSE

	tags <- RdTags(blocks)

	for (i in seq_along(tags)) {
            block <- blocks[[i]]
            tag <- attr(block, "Rd_tag")
            ## this should not be null, but it might be in a erroneous Rd file
            if(!is.null(tag))
            switch(tag,
                   "\\method" =,
                   "\\S3method" = {
                       class <- as.character(block[[2L]])
                       generic <- as.character(block[[1L]])
                       ## R.huge has
                       ## \method{[}{FileMatrix}(this, i, j, drop=FALSE)
                       if (length(blocks) > 2L &&
                           generic %in% c("[", "[[", "$")) {
                           ## need to assemble the call
                           j <- i + 1L
                           txt <- ""
                           repeat {
                               this <- switch(tg <- attr(blocks[[j]], "Rd_tag"),
                                              "\\ldots" =, # not really right
                                              "\\dots" = "...",
                                              RCODE = as.character(blocks[[j]]),
                                              stopRd(block, Rdfile, sprintf("invalid markup '%s' in %s", tg, tag)))
                               txt <- paste(txt, this, sep = "")
                               blocks[[j]] <- structure("", Rd_tag = "COMMENT")
                               if(grepl("\n$", txt)) {
                                   res <- try(parse(text = paste("a", txt)))
                                   if(!inherits(res, "try-error")) break
                               }
                               j <- j + 1L
                           }
                           #print(txt)
                           txt <- psub1("\\(([^,]*),\\s*", "\\1@generic@", txt)
                           txt <- fsub1("@generic@", generic, txt)
                           if (generic == "[")
                               txt <- psub1("\\)([^)]*)$", "]\\1", txt)
                           else if (generic == "[[")
                               txt <- psub1("\\)([^)]*)$", "]]\\1", txt)
                           else if (generic == "$")
                               txt <- psub1("\\)([^)]*)$", "\\1", txt)
                           #print(txt)
                           if (grepl("<-\\s*value", txt))
                               of1("## S3 replacement method for class '")
                           else
                               of1("## S3 method for class '")
                           writeContent(block[[2L]], tag)
                           of1("':\n")
                           blocks[[i+1L]] <- structure(txt, Rd_tag = "RCODE")
                       } else {
                           if (class == "default")
                               of1('## Default S3 method:\n')
                           else if (grepl("<-\\s*value", blocks[[i+1]][[1L]])) {
                               of1("## S3 replacement method for class '")
                               writeContent(block[[2L]], tag)
                               of1("':\n")
                           } else {
                               of1("## S3 method for class '")
                               writeContent(block[[2L]], tag)
                               of1("':\n")
                           }
                           writeContent(block[[1L]], tag)
                       }
                   },
                   "\\item" = {
                       if (blocktag == "\\value" && !inList) {
                           of1("\\begin{ldescription}\n")
                           inList <- TRUE
                       }
                       switch(blocktag,
                              "\\describe" = {
                                  of1('\\item[')
                                  writeContent(block[[1L]], tag)
                                  of1('] ')
                                  writeContent(block[[2L]], tag)
                              },
                              "\\value"=,
                              "\\arguments"={
                                  of1('\\item[\\code{')
                                  inCode <<- TRUE
                                  writeContent(block[[1L]], tag)
                                  inCode <<- FALSE
                                  of1('}] ')
                                  writeContent(block[[2L]], tag)
                              },
                              "\\enumerate" =,
                              "\\itemize"= {
                                  of1("\\item ")
                                  itemskip <- TRUE
                              })
                       itemskip <- TRUE
                   },
                   "\\cr" = of1("\\\\{}"), ## might be followed by [
               { # default
                   if (inList && !(tag == "TEXT" && isBlankRd(block))) {
                       of1("\\end{ldescription}\n")
                       inList <- FALSE
                   }
                   if (itemskip) {
                       ## The next item must be TEXT, and start with a space.
                       itemskip <- FALSE
                       if (tag == "TEXT") {
                           txt <- psub("^ ", "", as.character(block))
                           of1(texify(txt))
                       } else writeBlock(block, tag, blocktag) # should not happen
                   } else writeBlock(block, tag, blocktag)
               })
	}
        if (inList) of1("\\end{ldescription}\n")
    }

    writeSectionInner <- function(section, tag)
    {
        if (length(section)) {
	    ## need \n unless one follows, so
	    nxt <- section[[1L]]
	    if (!attr(nxt, "Rd_tag") %in% c("TEXT", "RCODE") ||
		substr(as.character(nxt), 1L, 1L) != "\n") of1("\n")
	    writeContent(section, tag)
	    inCodeBlock <<- FALSE
	    if (last_char != "\n") of1("\n")
	}
    }

    writeSection <- function(section, tag) {
        if (tag %in% c("\\encoding", "\\concept"))
            return()
        if (tag == "\\alias")
            writeAlias(section, tag)
        else if (tag == "\\keyword") {
            key <- trim(section)
            of0("\\keyword{", latex_escape_name(key), "}{", ltxname, "}\n")
        } else if (tag == "\\section") {
            of0("%\n\\begin{Section}{")
            writeContent(section[[1L]], tag)
            of1("}")
    	    writeSectionInner(section[[2L]], tag)
            of1("\\end{Section}\n")
    	} else {
            title <- envTitles[tag]
            of0("%\n\\begin{", title, "}")
            if(tag %in% c("\\author", "\\description", "\\details", "\\note",
                          "\\references", "\\seealso", "\\source"))
                of1("\\relax")
            extra <- sectionExtras[tag]
            if(!is.na(extra)) of0("\n\\begin{", extra, "}")
            if(tag %in% c("\\usage", "\\examples")) inCodeBlock <<- TRUE
            writeSectionInner(section, tag)
 	    inCodeBlock <<- FALSE
            if(!is.na(extra)) of0("\\end{", extra, "}\n")
            of0("\\end{", title, "}\n")
        }
    }

    Rd <- prepare_Rd(Rd, defines=defines, stages=stages, ...)
    Rdfile <- attr(Rd, "Rdfile")
    sections <- RdTags(Rd)

    enc <- which(sections == "\\encoding")
    if (length(enc)) outputEncoding <- as.character(Rd[[enc[1L]]][[1L]])

    if (is.character(out)) {
        if(out == "") {
            con <- stdout()
        } else {
	    con <- file(out, "wt")
	    on.exit(close(con))
	}
    } else {
    	con <- out
    	out <- summary(con)$description
    }

   if (outputEncoding != "ASCII") {
        latexEncoding <- latex_canonical_encoding(outputEncoding)
        of0("\\inputencoding{", latexEncoding, "}\n")
    } else latexEncoding <- NA

    ## we know this has been ordered by prepare2_Rd, but
    ## need to sort the aliases (if any)
    nm <- character(length(Rd))
    isAlias <- sections == "\\alias"
    sortorder <- if (any(isAlias)) {
        nm[isAlias] <- sapply(Rd[isAlias], as.character)
        order(sectionOrder[sections], toupper(nm), nm)
    } else  order(sectionOrder[sections])
    Rd <- Rd[sortorder]
    sections <- sections[sortorder]

    title <- as.character(Rd[[1L]])
    ## remove empty lines, leading whitespace
    title <- trim(paste(psub1("^\\s+", "", title[nzchar(title)]),
                        collapse=" "))
    ## substitutions?

    name <- Rd[[2L]]

    name <- trim(as.character(Rd[[2L]][[1L]]))
    ltxname <- latex_escape_name(name)

    of0('\\HeaderA{', ltxname, '}{',
        ltxstriptitle(title), '}{',
        latex_link_trans0(name), '}\n')

    for (i in seq_along(sections)[-(1:2)])
        writeSection(Rd[[i]], sections[i])

    if (encode_warn)
        warnRd(Rd, Rdfile, "Some input could not be re-encoded to ",
               outputEncoding)
    invisible(structure(out, latexEncoding = latexEncoding))
}
#  File src/library/tools/R/Rd2txt.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

## This stops on
##  unrecognized tag
##  \\tabular format must be simple text
##  too many columns for format
##  invalid markup in \[S3]method
##  "Tag ", tag, " not expected in code block"

tabExpand <- function(x) {
    srcref <- attr(x, "srcref")
    if (is.null(srcref)) start <- 0L
    else start <- srcref[5L] - 1L
    .Call(doTabExpand, x, start)
}

Rd2txt <-
    function(Rd, out="", package = "", defines=.Platform$OS.type,
             stages = "render", outputEncoding = "",
             width = getOption("help_text_width", 80L), ...)
{
    ## these attempt to mimic pre-2.10.0 layout
    WIDTH <- 0.9 * width
    HDR_WIDTH <- WIDTH - 2L

    ## we need to keep track of where we are.
    buffer <- character()	# Buffer not yet written to con
    				# Newlines have been processed, each line in buffer is
    				# treated as a separate input line (but may be wrapped before output)
    linestart <- TRUE		# At start of line?
    indent <- 0L		# Default indent
    wrapping <- TRUE		# Do word wrap?
    keepFirstIndent <- FALSE	# Keep first line indent?
    dropBlank <- FALSE		# Drop initial blank lines?
    haveBlanks <- 0L		# How many blank lines have just been written?
    enumItem <- 0L		# Last enumeration item number
    inEqn <- FALSE		# Should we do edits needed in an eqn?

    startCapture <- function() {
    	save <- list(buffer=buffer, linestart=linestart, indent=indent,
                     wrapping=wrapping, keepFirstIndent=keepFirstIndent,
                     dropBlank=dropBlank, haveBlanks=haveBlanks,
                     enumItem=enumItem, inEqn=inEqn)
    	buffer <<- character()
    	linestart <<- TRUE
    	indent <<- 0L
    	wrapping <<- TRUE
    	keepFirstIndent <<- FALSE
    	dropBlank <<- FALSE
    	haveBlanks <<- 0L
    	enumItem <<- 0L
    	inEqn <<- FALSE
    	save
    }

    endCapture <- function(saved) {
    	result <- buffer
    	buffer <<- saved$buffer
    	linestart <<- saved$linestart
    	indent <<- saved$indent
    	wrapping <<- saved$wrapping
    	keepFirstIndent <<- saved$keepFirstIndent
    	dropBlank <<- saved$dropBlank
    	haveBlanks <<- saved$haveBlanks
    	enumItem <<- saved$enumItem
    	inEqn <<- saved$inEqn
    	result
    }

    ## for efficiency
    WriteLines <-
        if(outputEncoding == "UTF-8" ||
           (outputEncoding == "" && l10n_info()[["UTF-8"]])) {
        function(x, con, outputEncoding, ...)
            writeLines(x, con, useBytes = TRUE, ...)
    } else {
        function(x, con, outputEncoding, ...) {
            x <- iconv(x, "UTF-8", outputEncoding, sub="byte", mark=FALSE)
            writeLines(x, con, useBytes = TRUE, ...)
        }
    }

    wrap <- function(doWrap = TRUE)
	if (doWrap != wrapping) { flushBuffer(); wrapping <<- doWrap }

    putw <- function(...)  { wrap(TRUE); put(...) }

    putf <- function(...)  { wrap(FALSE); put(...) }

    put <- function(...) {
        txt <- paste(..., collapse="", sep="")
        trail <- grepl("\n$", txt)
        # Convert newlines
        txt <- strsplit(txt, "\n", fixed = TRUE)[[1]]
        if (dropBlank) {
            while(length(txt) && grepl("^[[:space:]]*$", txt[1L]))
            	txt <- txt[-1L]
            if (length(txt)) dropBlank <<- FALSE
        }
        if(!length(txt)) return()
        haveBlanks <<- 0

        if (linestart) buffer <<- c(buffer, txt)
        else if (length(buffer)) {
            buffer[length(buffer)] <<-
                paste(buffer[length(buffer)], txt[1L], sep="")
            buffer <<- c(buffer, txt[-1L])
        }
        else buffer <<- txt
        linestart <<- trail
    }

    blanks <- function(n)
	if (n) paste(rep(" ", n), collapse="") else ""

    flushBuffer <- function() {
    	if (!length(buffer)) return()

    	if (wrapping) {
	    if (keepFirstIndent) {
		first <- nchar(psub1("[^ ].*", "", buffer[1]))
		keepFirstIndent <<- FALSE
	    } else
		first <- indent

	    buffer <<- c(buffer, "")  # Add an extra blank sentinel
	    blankLines <- grep("^[[:space:]]*$", buffer)
	    result <- character()
	    start <- 1L
	    for (i in seq_along(blankLines)) {
		if (blankLines[i] > start) {
		    result <- c(result,
                                strwrap(paste(buffer[start:(blankLines[i]-1L)],
                                              collapse = " "),
                                        WIDTH, indent = first, exdent = indent))
		    first <- indent
                }
                result <- c(result, "")
		start <- blankLines[i]+1L
	    }
            ## we want to collapse multiple blank lines when wrapping
            ## and to remove the sentinel (which we need to do first or
            ## we will drop a single blank line)
            buffer <<- result[-length(result)]
            empty <- !nzchar(buffer)
            drop <- empty & c(FALSE, empty[-length(empty)])
            buffer <<- buffer[!drop]
	} else {  # Not wrapping
	    if (keepFirstIndent) {
		if (length(buffer) > 1L)
		    buffer[-1L] <<- paste(blanks(indent), buffer[-1L], sep="")
		keepFirstIndent <- FALSE
	    } else
		buffer <<- paste(blanks(indent), buffer, sep="")
	}

    	if (length(buffer)) WriteLines(buffer, con, outputEncoding)
    	buffer <<- character()
    	linestart <<- TRUE
    }

    encoding <- "unknown"

    li <- l10n_info()
    ## See the comment in ?Rd2txt as to why we do not attempt fancy quotes
    ## in Windows CJK locales -- and in any case they would need more work
    ## This covers the common single-byte locales and Thai (874)
    use_fancy_quotes <-
        (.Platform$OS.type == "windows" &&
         ((li$codepage >= 1250 && li$codepage <= 1258) || li$codepage == 874)) ||
        li[["UTF-8"]]

    if(getOption("useFancyQuotes") && use_fancy_quotes) {
        ## On Windows, Unicode literals are translated to local code page
    	LSQM <- intToUtf8("0x2018") # Left single quote
    	RSQM <- intToUtf8("0x2019") # Right single quote
    	LDQM <- intToUtf8("0x201c") # Left double quote
    	RDQM <- intToUtf8("0x201d") # Right double quote
    } else {
        LSQM <- RSQM <- "'"
        LDQM <- RDQM <- '"'
    }

    trim <- function(x) {
        x <- psub1("^\\s*", "", x)
        psub1("\\s*$", "", x)
    }

    striptitle <- function(text) {
        text <- fsub("\\", "", text)
        text <- fsub("---", "_", text)
        text <- fsub("--", "-", text)
        text
    }

    txt_striptitle <- function(text) {
        text <- striptitle(text)
        if(use_fancy_quotes) {
            text <- fsub("``", LDQM, text)
            text <- fsub("''", RDQM, text)
            text <- psub("`([^']+)'", paste(LSQM, "\\1", RSQM, sep=""), text)
            text <- fsub("`", "'", text)
        } else {
            text <- psub("(``|'')", '"', text)
            text <- fsub("`", "'", text)
        }
        text
    }

    ## underline via backspacing
    txt_header <- function(header) {
        header <- paste(strwrap(header, WIDTH), collapse="\n")
        letters <- strsplit(header, "", fixed = TRUE)[[1L]]
        isaln <- grep("[[:alnum:]]", letters)
        letters[isaln] <- paste("_\b", letters[isaln], sep="")
        paste(letters, collapse = "")
    }

    unescape <- function(x) {
        x <- psub("(---|--)", "-", x)
        x
    }

    writeCode <- function(x) {
        txt <- as.character(x)
        if(inEqn) txt <- txt_eqn(txt)
        txt <- fsub('"\\{"', '"{"', txt)
        ## \dots gets left in noquote.Rd
        txt <-fsub("\\dots",  "....", txt)
        put(txt)
    }

    # This function strips pending blank lines, then adds n new ones.
    blankLine <- function(n = 1L) {
    	while (length(buffer) &&
               grepl("^[[:blank:]]*$", buffer[length(buffer)]))
    	    buffer <<- buffer[-length(buffer)]
	flushBuffer()
	if (n > haveBlanks) {
	    buffer <<- rep("", n - haveBlanks)
	    flushBuffer()
	}
	haveBlanks <<- n
	dropBlank <<- TRUE
    }

    txt_eqn <- function(x) {
        x <- psub("\\\\(Gamma|alpha|Alpha|pi|mu|sigma|Sigma|lambda|beta|epsilaon|psi)", "\\1", x)
        x <- psub("\\\\(bold|strong|emph|var)\\{([^}]*)\\}", "\\2", x)
        x <- psub("\\\\(ode|samp)\\{([^}]*)\\}", "'\\2'", x)
        x
    }

    writeDR <- function(block, tag) {
        if (length(block) > 1L) {
            putf('## Not run:\n')
            writeCodeBlock(block, tag)
            blankLine(0L)
            putf('## End(Not run)\n')
        } else {
            putf('## Not run: ')
            writeCodeBlock(block, tag)
            blankLine(0L)
        }
    }

    writeQ <- function(block, tag, quote=tag)
    {
        if (use_fancy_quotes) {
            if (quote == "\\sQuote") {
                put(LSQM); writeContent(block, tag); put(RSQM)
            } else {
                put(LDQM); writeContent(block, tag); put(RDQM)
            }
        } else {
            if (quote == "\\sQuote") {
                put("'"); writeContent(block, tag); put("'")
            } else {
                put("\""); writeContent(block,tag); put("\"")
            }
        }
    }

    writeBlock <- function(block, tag, blocktag) {
        switch(tag,
               UNKNOWN =,
               VERB =,
               RCODE = writeCode(tabExpand(block)),
               TEXT = if(blocktag == "\\command") putw(block) else putw(unescape(tabExpand(block))),
               LIST =,
               COMMENT = {},
               "\\describe" = {
               	   blankLine(0L)
                   writeContent(block, tag)
                   blankLine()
               },
               "\\itemize"=,
               "\\enumerate"= {
               	   blankLine(0L)
                   enumItem0 <- enumItem
                   enumItem <<- 0L
                   indent0 <- indent
                   indent <<- max(10L, indent+4L)
                   dropBlank <<- TRUE
                   writeContent(block, tag)
                   blankLine()
                   indent <<- indent0
                   enumItem <<- enumItem0
               },
               "\\code"=,
               "\\command"=,
               "\\env"=,
               "\\file"=,
               "\\kbd"=,
               "\\option"=,
               "\\pkg"=,
               "\\samp" = writeQ(block, tag, quote="\\sQuote"),
               "\\email"=  put("<email: ",
                               gsub("\n", "", paste(as.character(block), collapse="")),
                               ">"),
                "\\url"= put("<URL: ",
                              gsub("\n", "", paste(as.character(block), collapse="")),
                              ">") ,
               "\\Sexpr"= put(as.character.Rd(block, deparse=TRUE)),
               "\\acronym" =,
               "\\cite"=,
               "\\dfn"= ,
               "\\special" = ,
               "\\var" = writeContent(block, tag),

               "\\bold"=,
               "\\strong"= {
                   put("*")
                   writeContent(block, tag)
                   put("*")
               },
               "\\emph"= {
                   put("_")
                   writeContent(block, tag)
                   put("_")
               },
               "\\sQuote" =,
               "\\dQuote"= writeQ(block, tag) ,
               "\\preformatted"= {
                   putf("\n")
                   writeCodeBlock(block, tag)
               },
               "\\verb"= put(block),
               "\\linkS4class" =,
               "\\link" = writeContent(block, tag),
               "\\cr" = {
                   ## we want to print out what we have, and if
                   ## followed immediately by \n (as it usually is)
                   ## discard that.  This is not entirely correct,
                   ## but it is better than before ....
                   flushBuffer()
                   dropBlank <<- TRUE
                   },
               "\\dots" =,
               "\\ldots" = put("..."),
               "\\R" = put("R"),
               "\\enc" = {
                   txt <- as.character(block[[2L]])
                   put(txt)
               } ,
               "\\eqn" = {
                   block <- block[[length(block)]]
                   ## FIXME: treat 2 of 2 differently?
                   inEqn0 <- inEqn
                   inEqn <<- TRUE
                   writeContent(block, tag)
                   inEqn <<- inEqn0
               },
               "\\deqn" = {
                   blankLine()
                   block <- block[[length(block)]]
                   save <- startCapture()
                   inEqn <<- TRUE
                   writeContent(block, tag)
                   eqn <- endCapture(save)
                   eqn <- format(eqn, justify="centre", width=WIDTH-indent)
                   putf(eqn)
    		   blankLine()
               },
               "\\tabular" = writeTabular(block),
               "\\if"=,
               "\\ifelse" =
                   if (testRdConditional("text", block, Rdfile))
               		writeContent(block[[2L]], tag)
               	   else if (tag == "\\ifelse")
               	   	writeContent(block[[3L]], tag),
               "\\out" = for (i in seq_along(block))
		   put(block[[i]]),
               stopRd(block, Rdfile, "Tag ", tag, " not recognized")
               )
    }

    writeTabular <- function(table) {
    	formats <- table[[1L]]
    	content <- table[[2L]]
    	if (length(formats) != 1L || RdTags(formats) != "TEXT")
    	    stopRd(table, Rdfile, "\\tabular format must be simple text")
    	formats <- strsplit(formats[[1L]], "", fixed = TRUE)[[1L]]
        tags <- RdTags(content)
        entries <- list()
        row <- 1L
        col <- 1L
        save <- startCapture()
        dropBlank <<- TRUE
        newEntry <- function() {
            entries <<- c(entries, list(list(text=trim(endCapture(save)),
	                   	             row=row, col=col)))
            save <<- startCapture()
            dropBlank <<- TRUE
        }
        for (i in seq_along(tags)) {
            switch(tags[i],
                  "\\tab" = {
                  	newEntry()
                   	col <- col + 1
                   	if (col > length(formats))
                   	    stopRd(content[[i]], Rdfile,
                                   sprintf("too many columns for format '%s'",
                                           table[[1L]]))
                   },
                   "\\cr" = {
                   	newEntry()
                   	row <- row + 1L
			col <- 1L
                    },
                   writeBlock(content[[i]], tags[i], "\\tabular")
                   )
        }
        newEntry()
        endCapture(save)
        entries <- with(entries[[length(entries)]],
        	    {
                        if (!length(text) && col == 1L)
                            entries[-length(entries)]
                        else
                            entries
                    })
        rows <- entries[[length(entries)]]$row
        cols <- max(sapply(entries, function(e) e$col))
        widths <- rep(0L, cols)
        lines <- rep(1L, rows)
        for (i in seq_along(entries)) {
            e <- entries[[i]]
            while(length(e$text) && !nzchar(e$text[length(e$text)])) {
            	e$text <- e$text[-length(e$text)]
            	entries[[i]] <- e
            }
            if (any(nzchar(e$text)))
            	widths[e$col] <- max(widths[e$col], max(nchar(e$text)))
            lines[e$row] <- max(lines[e$row], length(e$text))
        }
        result <- matrix("", sum(lines), cols)
        for (i in seq_len(cols))
            result[, i] <- blanks(widths[i])
        firstline <- c(1L, 1L+cumsum(lines))
        for (i in seq_along(entries)) {
            e <- entries[[i]]
            text <- format(e$text, justify=formats[e$col], width=widths[e$col])
            for (j in seq_along(text))
            	result[firstline[e$row] + j - 1L, e$col] <- text[j]
        }
        blankLine()
        indent0 <- indent
        indent <<- indent + 1L
        for (i in seq_len(nrow(result))) {
            for (j in seq_len(cols))
            	putf(" ", result[i,j], " ")
            putf("\n")
        }
        blankLine()
        indent <<- indent0
    }

    writeCodeBlock <- function(blocks, blocktag)
    {
    	tags <- RdTags(blocks)

	for (i in seq_along(tags)) {
            block <- blocks[[i]]
            tag <- attr(block, "Rd_tag")
            switch(tag,
                   "\\method" =,
                   "\\S3method" = {
                       class <- as.character(block[[2L]])
                       generic <- as.character(block[[1L]])
                       if(generic %in% c("[", "[[", "$")) {
                           ## need to assemble the call
                           j <- i + 1L
                           txt <- ""
                           repeat {
                               this <- switch(tg <- attr(blocks[[j]], "Rd_tag"),
                                              "\\ldots" =, # not really right
                                              "\\dots" = "...",
                                              RCODE = as.character(blocks[[j]]),
                                              stopRd(block, Rdfile, sprintf("invalid markup '%s' in %s", tg, tag)))
                               txt <- paste(txt, this, sep = "")
                               blocks[[j]] <- structure("", Rd_tag = "COMMENT")
                               if(grepl("\n$", txt)) {
                                   res <- try(parse(text = paste("a", txt)))
                                   if(!inherits(res, "try-error")) break
                               }
                               j <- j + 1L
                           }
                           txt <- psub1("\\(([^,]*),\\s*", "\\1@generic@", txt)
                           txt <- fsub1("@generic@", generic, txt)
                           if (generic == "[")
                               txt <- psub1("\\)([^)]*)$", "]\\1", txt)
                           else if (generic == "[[")
                               txt <- psub1("\\)([^)]*)$", "]]\\1", txt)
                           else if (generic == "$")
                               txt <- psub1("\\)([^)]*)$", "\\1", txt)
                           if (grepl("<-\\s*value", txt))
                               putf("## S3 replacement method for class '")
                           else
                               putf("## S3 method for class '")
                           writeCodeBlock(block[[2L]], tag)
                           putf("':\n")
                           blocks[[i + 1L]] <-
                               structure(txt, Rd_tag = "RCODE")
                       } else if(grepl(sprintf("^%s$",
                                               paste(c("\\+", "\\-", "\\*",
                                                       "\\/", "\\^", "<=?",
                                                       ">=?", "!=?", "==",
                                                       "\\&", "\\|",
                                                       "\\%[[:alnum:][:punct:]]*\\%"),
                                                     collapse = "|")),
                                       generic)) {
                           ## Binary operators and unary '!'.
                           ## Need to assemble the call.
                           j <- i + 1L
                           txt <- ""
                           repeat {
                               this <- switch(tg <- attr(blocks[[j]], "Rd_tag"),
                                              RCODE = as.character(tabExpand(blocks[[j]])),
                                              stopRd(block, Rdfile, sprintf("invalid markup '%s' in %s", tg, tag)))
                               txt <- paste(txt, this, sep = "")
                               blocks[[j]] <- structure("", Rd_tag = "COMMENT")
                               if(grepl("\n$", txt)) {
                                   res <- try(parse(text = paste("a", txt)))
                                   if(!inherits(res, "try-error")) break
                               }
                               j <- j + 1L
                           }
                           nms <- as.character(res[[1L]])[-1L]
                           len <- length(nms)
                           ## (There should be no default values).
                           if((len == 1L) && (generic == "!")) {
                               ## Unary: !.
                               txt <- sprintf("! %s\n", nms[1L])
                           } else if((len == 2L) && (generic != "!")) {
                               ## Binary: everything but !.
                               txt <- sprintf("%s %s %s\n",
                                              nms[1L], generic, nms[2L])
                           } else {
                               warnRd(block, Rdfile,
                                      sprintf("arity problem for \\method{%s}{%s}",
                                              generic, class))
                               txt <- paste(generic, txt)
                           }
                           putf("## S3 method for class '")
                           writeCodeBlock(block[[2L]], tag)
                           putf("':\n")
                           blocks[[i + 1L]] <-
                               structure(txt, Rd_tag = "RCODE")
                       } else {
                           if (class == "default")
                               putf('## Default S3 method:\n')
                           else if (grepl("<-\\s*value", blocks[[i+1]][[1L]])) {
                               putf("## S3 replacement method for class '")
                               writeCodeBlock(block[[2L]], tag)
                               putf("':\n")
                           } else {
                               putf("## S3 method for class '")
                               writeCodeBlock(block[[2L]], tag)
                               putf("':\n")
                           }
                           writeCodeBlock(block[[1L]], tag)
                       }
                  },
                   UNKNOWN =,
                   VERB =,
                   RCODE =,
                   TEXT = writeCode(tabExpand(block)),
                   "\\donttest" =,
                   "\\special" =,
                   "\\var" = writeCodeBlock(block, tag),
                   "\\dots" =, # \ldots is not really allowed
                   "\\ldots" = put("..."),
                   "\\dontrun"= writeDR(block, tag),
                   COMMENT =,
                   "\\dontshow" =,
                   "\\testonly" = {}, # do nothing
                   "\\S4method" = {
                       class <- as.character(block[[2L]])
                       generic <- as.character(block[[1L]])
                       if(generic %in% c("[", "[[", "$")) {
                           ## need to assemble the call
                           j <- i + 1L
                           txt <- ""
                           repeat {
                               this <- switch(tg <- attr(blocks[[j]], "Rd_tag"),
                                              "\\ldots" =, # not really right
                                              "\\dots" = "...",
                                              RCODE = as.character(tabExpand(blocks[[j]])),
                                              stopRd(block, Rdfile, sprintf("invalid markup '%s' in %s", tg, tag)))
                               txt <- paste(txt, this, sep = "")
                               blocks[[j]] <- structure("", Rd_tag = "COMMENT")
                               if(grepl("\n$", txt)) {
                                   res <- try(parse(text = paste("a", txt)))
                                   if(!inherits(res, "try-error")) break
                               }
                               j <- j + 1L
                           }
                           txt <- psub1("\\(([^,]*),\\s*", "\\1@generic@", txt)
                           txt <- fsub1("@generic@", generic, txt)
                           if (generic == "[")
                               txt <- psub1("\\)([^)]*)$", "]\\1", txt)
                           else if (generic == "[[")
                               txt <- psub1("\\)([^)]*)$", "]]\\1", txt)
                           else if (generic == "$")
                               txt <- psub1("\\)([^)]*)$", "\\1", txt)
                           if (grepl("<-\\s*value", txt))
                               putf("## S4 replacement method for signature '")
                           else
                               putf("## S4 method for signature '")
                           writeCodeBlock(block[[2L]], tag)
                           putf("':\n")
                           blocks[[i + 1L]] <-
                               structure(txt, Rd_tag = "RCODE")
                       } else {
                           putf("## S4 method for signature '")
                           writeCodeBlock(block[[2L]], tag)
                           putf("':\n")
                           writeCodeBlock(block[[1L]], tag)
                       }
                   },
                   ## All the markup such as \emph
                   stopRd(block, Rdfile, "Tag ", tag,
                          " not expected in code block")
                   )
        }
    }

    writeContent <- function(blocks, blocktag) {
        itemskip <- FALSE
	tags <- RdTags(blocks)

	for (i in seq_along(tags)) {
            tag <- tags[i]
            block <- blocks[[i]]
            switch(tag,
                   "\\item" = {
                       switch(blocktag,
                              "\\describe"= {
                                  blankLine()
                                  save <- startCapture()
                                  dropBlank <<- TRUE
                                  writeContent(block[[1L]], tag)
                                  DLlab <- endCapture(save)
                                  indent0 <- indent
                                  indent <<- max(10L, indent + 4L)
                                  keepFirstIndent <<- TRUE
                                  putw(paste(rep(" ", indent0), collapse=""),
                                       format(paste(DLlab,  sep=""),
                                              justify="left", width=indent),
                                       " ")
                                  writeContent(block[[2L]], tag)
			  	  blankLine(0L)
                                  indent <<- indent0
                              },
                              "\\value"=,
                              "\\arguments"= {
                                  blankLine()
                                  save <- startCapture()
                                  dropBlank <<- TRUE
                                  writeContent(block[[1L]], tag)
                                  DLlab <- endCapture(save)
                                  indent0 <- indent
                                  indent <<- max(10L, indent + 4L)
                                  keepFirstIndent <<- TRUE
                                  putw(format(paste(DLlab, ": ", sep=""),
                                              justify="right", width=indent))
                                  writeContent(block[[2L]], tag)
			  	  blankLine(0L)
                                  indent <<- indent0
                              },
                              "\\itemize" =,
                              "\\enumerate" = {
                              	  blankLine()
                              	  keepFirstIndent <<- TRUE
                              	  if (blocktag == "\\itemize")
                              	      label <- "* "
                              	  else {
                              	      enumItem <<- enumItem + 1L
                              	      label <- paste(enumItem, ". ", sep="")
                              	  }
                              	  putw(format(label, justify="right",
                                              width=indent))
                              })
                       itemskip <- TRUE
                   },
               { # default
                   if (itemskip) {
                       ## The next item must be TEXT, and start with a space.
                       itemskip <- FALSE
                       if (tag == "TEXT") {
                           txt <- psub("^ ", "", as.character(tabExpand(block)))
                           put(txt)
                       } else writeBlock(block, tag, blocktag) # should not happen
                   } else writeBlock(block, tag, blocktag)
               })
	}
    }

    toChar <- function(x)
    {
        out <- character()
        for(i in seq_along(x)) {
            this <- x[[i]]
            out <- c(out,
                     switch(attr(this, "Rd_tag"),
                            "\\ldots" =,
                            "\\dots" = "...",
                            "\\R" = "R",
                            "\\bold" =,
                            "\\strong" =,
                            "\\emph" = toChar(this),
                            "\\code" = sQuote(toChar(this)),
                            as.character(this))
                     )
        }
        paste(out, collapse = "")
    }

    writeSection <- function(section, tag) {
        if (tag %in% c("\\alias", "\\concept", "\\encoding", "\\keyword"))
            return()
    	blankLine(0L)
        indent <<- 5L
        keepFirstIndent <<- TRUE
        if (tag == "\\section") {
            ## section header could have markup
            putf(txt_header(toChar(section[[1L]])), ":")
            blankLine()
            dropBlank <<- TRUE
            wrapping <<- TRUE
            keepFirstIndent <<- FALSE
    	    writeContent(section[[2L]], tag)
    	} else if (tag %in% c("\\usage", "\\synopsis", "\\examples")) {
            putf(txt_header(sectionTitles[tag]), ":")
            blankLine()
            dropBlank <<- TRUE
            wrapping <<- FALSE
            keepFirstIndent <<- FALSE
            writeCodeBlock(section, tag)
    	} else {
            putf(txt_header(sectionTitles[tag]), ":")
            blankLine()
            dropBlank <<- TRUE
            wrapping <<- TRUE
            keepFirstIndent <<- FALSE
            writeContent(section, tag)
        }
        blankLine()
    }

    if (is.character(out)) {
        if(out == "") {
            con <- stdout()
        } else {
	    con <- file(out, "wt")
	    on.exit(close(con))
	}
    } else {
    	con <- out
    	out <- summary(con)$description
    }

    Rd <- prepare_Rd(Rd, defines=defines, stages=stages, ...)
    Rdfile <- attr(Rd, "Rdfile")
    sections <- RdTags(Rd)

    title <- as.character(Rd[[1L]])
    ## remove empty lines, leading and trailing whitespace, \n
    title <- trim(paste(psub1("^\\s+", "", title[nzchar(title)]), collapse=" "))
    title <- fsub("\n", "", title)

    name <- trim(Rd[[2L]][[1L]])

    if(nzchar(package)) {
        left <- name
        mid <- if(nzchar(package)) paste("package:", package, sep = "") else ""
        right <- "R Documentation"
        if(encoding != "unknown")
            right <- paste(right, "(", encoding, ")", sep="")
        pad <- max(HDR_WIDTH - nchar(left, "w") - nchar(mid, "w") - nchar(right, "w"), 0)
        pad0 <- pad %/% 2L
        pad1 <- paste(rep.int(" ", pad0), collapse = "")
        pad2 <- paste(rep.int(" ", pad - pad0), collapse = "")
        putf(paste(left, pad1, mid, pad2, right, "\n\n", sep=""))
    }

    putf(txt_header(txt_striptitle(title)))
    blankLine()

    for (i in seq_along(sections)[-(1:2)])
        writeSection(Rd[[i]], sections[i])

    blankLine(0L)
    invisible(out)
}
#  File src/library/tools/R/RdConv2.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/


RdTags <- function(Rd) {
    res <- sapply(Rd, attr, "Rd_tag")
    if (!length(res)) res <- character(0)
    res
}

isBlankRd <- function(x)
    length(grep("^[[:blank:]]*\n?$", x, perl = TRUE)) == length(x) # newline optional

isBlankLineRd <- function(x) {
    attr(x, "srcref")[2L] == 1 &&
    length(grep("^[[:blank:]]*\n", x, perl = TRUE)) == length(x)   # newline required
}

stopRd <- function(block, Rdfile, ...)
{
    srcref <- attr(block, "srcref")
    if (missing(Rdfile) && !is.null(srcref)) {
    	srcfile <- attr(srcref, "srcfile")
    	if (is.environment(srcfile))
    	    Rdfile <- srcfile$filename
    }
    if (missing(Rdfile) || is.null(Rdfile)) Rdfile <- ""
    else Rdfile <- paste(Rdfile, ":", sep="")

    msg <- if (is.null(srcref))
        paste(Rdfile, " ", ..., sep = "")
    else {
    	loc <- paste(Rdfile, srcref[1L], sep = "")
    	if (srcref[1L] != srcref[3L]) loc <- paste(loc, "-", srcref[3L], sep="")
    	paste(loc, ": ", ..., sep="")
    }
    stop(msg, call. = FALSE, domain = NA)
}

warnRd <- function(block, Rdfile, ...)
{
    srcref <- attr(block, "srcref")
    if (missing(Rdfile) && !is.null(srcref)) {
    	srcfile <- attr(srcref, "srcfile")
    	if (is.environment(srcfile))
    	    Rdfile <- srcfile$filename
    }
    if (missing(Rdfile) || is.null(Rdfile)) Rdfile <- ""
    else Rdfile <- paste(Rdfile, ":", sep="")

    msg <- if (is.null(srcref))
        paste(Rdfile, " ", ..., sep = "")
    else {
    	loc <- paste(Rdfile, srcref[1L], sep = "")
    	if (srcref[1L] != srcref[3L]) loc <- paste(loc, "-", srcref[3L], sep="")
        paste(loc, ": ", ..., sep = "")
    }
    warning(msg, call. = FALSE, domain = NA, immediate. = TRUE)
}

RweaveRdDefaults <- list(
    width = 6,
    height = 6,
    eval = TRUE,
    fig = FALSE,
    echo = FALSE,
    keep.source = TRUE,
    results = "text",
    strip.white = "true",
    stage = "install")

RweaveRdOptions <- function(options)
{

    ## convert a character string to logical
    c2l <- function(x){
        if(is.null(x)) return(FALSE)
        else return(as.logical(toupper(as.character(x))))
    }

    NUMOPTS <- c("width", "height")
    NOLOGOPTS <- c(NUMOPTS, "results", "stage", "strip.white")

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
                                 c("text", "verbatim", "rd", "hide"))
    if(!is.null(options$stage))
    	options$stage <- tolower(as.character(options$stage))
    options$stage <- match.arg(options$stage,
    				 c("build", "install", "render"))
    options
}

tagged <- function(x, tag) structure(x, Rd_tag=tag)

evalWithOpt <- function(expr, options, env)
{
    res <- structure("", Rd_tag="COMMENT")
    if(options$eval){
        result <- tryCatch(withVisible(eval(expr, env)), error=function(e) e)

        if(inherits(result, "error")) return(result)
        switch(options$results,
        "text" = if (result$visible)
		    res <- paste(as.character(result$value), collapse=" "),
        "verbatim" = if (result$visible) print(result$value),
        "rd" = res <- result$value)
    }
    return(res)
}

getDynamicFlags <- function(block) {
    flag <- attr(block, "dynamicFlag")
    if (is.null(flag)) c("#ifdef"=FALSE, "\\Sexpr"=FALSE)
    else c("#ifdef" = flag %% 2 > 0, "\\Sexpr" = flag %/% 2 > 0)
}

setDynamicFlags <- function(block, flags) {  # flags in format coming from getDynamicFlags
    flag <- sum(flags * c(1,2))
    if (flag == 0) flag <- NULL
    attr(block, "dynamicFlag") <- flag
    block
}

processRdChunk <- function(code, stage, options, env, Rdfile)
{
    if (is.null(opts <- attr(code, "Rd_option"))) opts <- ""
    codesrcref <- attr(code, "srcref")
    options <- utils:::SweaveParseOptions(opts, options, RweaveRdOptions)
    if (stage == options$stage) {
        #  The code below is very similar to RWeaveLatexRuncode, but simplified

        # Results as a character vector for now; convert to list later
        res <- character(0)
        code <- code[RdTags(code) != "COMMENT"]
	chunkexps <- tryCatch(parse(text = code), error = identity)
	if (inherits(chunkexps, "error")) stopRd(code, Rdfile, chunkexps)

	if(length(chunkexps) == 0L)
	    return(tagged(code, "LIST"))

	srcrefs <- attr(chunkexps, "srcref")
	lastshown <- 0L
	thisline <- 0
	err <- NULL
	for(nce in seq_along(chunkexps))
	{
	    ce <- chunkexps[[nce]]
	    if (nce <= length(srcrefs) && !is.null(srcref <- srcrefs[[nce]])) {
		srcfile <- attr(srcref, "srcfile")
		showfrom <- srcref[1L]
		showto <- srcref[3L]
		dce <- getSrcLines(srcfile, lastshown+1, showto)
		leading <- showfrom-lastshown
		lastshown <- showto
		while (length(dce) && grepl("^[[:blank:]]*$", dce[1L])) {
		    dce <- dce[-1L]
		    leading <- leading - 1L
		}
	    } else {
		dce <- deparse(ce, width.cutoff=0.75*getOption("width"))
		leading <- 1L
	    }
	    if(options$echo && length(dce)) {
		res <- c(res,"\n",
                         paste(getOption("prompt"), dce[1L:leading],
                               sep="", collapse="\n"))
		if (length(dce) > leading)
		    res <- c(res, "\n",
                             paste(getOption("continue"), dce[-(1L:leading)],
                                   sep="", collapse="\n"))
		thisline <- thisline + length(dce)
	    }

	    tmpcon <- file()
	    sink(file = tmpcon)
	    if(options$eval) err <- evalWithOpt(ce, options, env)
	    res <- c(res, "\n") # make sure final line is complete
	    sink()
	    output <- readLines(tmpcon)
	    close(tmpcon)
	    ## delete empty output
	    if(length(output) == 1L & output[1L] == "") output <- NULL

	    if (inherits(err, "error")) {
	    	attr(code, "srcref") <- codesrcref
	    	stopRd(code, Rdfile, err$message)
	    }

	    if(length(output) & (options$results != "hide")){

		output <- paste(output, collapse="\n")
		if(options$strip.white %in% c("all", "true")) {
		    output <- sub("^[[:space:]]*\n", "", output)
		    output <- sub("\n[[:space:]]*$", "", output)
		    if(options$strip.white == "all")
		      output <- sub("\n[[:space:]]*\n", "\n", output)
		}
		res <- c(res, output)
		remove(output)
	    }
	}
	if (options$results == "rd") {
	    res <- err   # The last value of the chunk
	    tmpcon <- file()
	    writeLines(res, tmpcon, useBytes = TRUE)
	    res <- parse_Rd(tmpcon, fragment=TRUE)
	    flag <- getDynamicFlags(res)
	    res <- tagged(res, "LIST")
	    res <- setDynamicFlags(res, flag)
	    close(tmpcon)
	    res <- prepare_Rd(res, defines = .Platform$OS.type, options=options,
	                           stage2 = FALSE, stage3 = FALSE)
	} else if (options$results == "text")
	    res <- tagged(err, "TEXT")
	else if (length(res)) {
	    res <- lapply(as.list(res), function(x) tagged(x, "VERB"))
	    res <- tagged(res, "\\verb")
	} else res <- tagged("", "COMMENT")
    } else res <- code
    attr(res, "srcref") <- codesrcref
    res
}

processRdIfdefs <- function(blocks, defines)
{
    recurse <- function(block) {
    	if (!(getDynamicFlags(block)["#ifdef"])) return(block)

        if (!is.null(tag <- attr(block, "Rd_tag"))) {
	    if (tag %in% c("#ifdef", "#ifndef")) {
		target <- block[[1L]][[1L]]
		# The target will have picked up some whitespace and a newline
		target <- psub("[[:blank:][:cntrl:]]*", "", target)
		if ((target %in% defines) == (tag == "#ifdef")) {
		    flag <- getDynamicFlags(block[[2L]])
		    block <- tagged(block[[2L]], "#expanded")
		    block <- setDynamicFlags(block, flag)
		} else
		    block <- structure(tagged(paste(tag, target, "not active"),
                                              "COMMENT"),
		    		       srcref = attr(block, "srcref"))
	    }
	}
	if (is.list(block)) {
	    i <- 1L
	    flags <- getDynamicFlags(NULL)
	    while (i <= length(block)) {
	    	newval <- recurse(block[[i]])
	    	newtag <- attr(newval, "Rd_tag")
	    	if (!is.null(newtag) && newtag == "#expanded") { # ifdef has expanded.
	    	    all <- seq_along(block)
	    	    before <- all[all < i]
	    	    after <- all[all > i]
	    	    block <- tagged(c(block[before], newval, block[after]), tag)
	    	} else {
	    	    flags <- flags | getDynamicFlags(newval)
		    block[[i]] <- newval
		    i <- i+1L
		}
	    }
	    block <- setDynamicFlags(block, flags)
	}
	block
    }

    recurse(blocks)
}

processRdSexprs <-
    function(block, stage, options = RweaveRdDefaults,
             env = new.env(parent=globalenv()))
{
    recurse <- function(block) {
    	if (!getDynamicFlags(block)["\\Sexpr"]) return(block)

        if (is.list(block)) {
            if (!is.null(tag <- attr(block, "Rd_tag"))) {
        	if (tag == "\\Sexpr")
            	    block <- processRdChunk(block, stage, options, env)
            	else if (tag == "\\RdOpts")
    	    	    options <<-
                        utils:::SweaveParseOptions(block, options, RweaveRdOptions)
    	    }
	    for (i in seq_along(block))
		block[[i]] <- recurse(block[[i]])
	}
	block
    }
    recurse(block)
}

prepare_Rd <-
    function(Rd, encoding = "unknown", defines = NULL, stages = NULL,
             options = RweaveRdDefaults,
             stage2 = TRUE, stage3 = TRUE, ..., msglevel = 0)
{
    if (is.character(Rd)) {
        Rdfile <- Rd
        ## do it this way to get info in internal warnings
        Rd <- eval(substitute(parse_Rd(f, encoding = enc, ...),
                              list(f = Rd, enc = encoding)))
    } else if(inherits(Rd, "connection")) {
        Rdfile <- summary(Rd)
        Rd <- parse_Rd(Rd, encoding = encoding, ...)
    } else Rdfile <- attr(Rd, "Rdfile")
    if (is.null(Rdfile) && !is.null(srcref <- attr(Rd, "srcref")))
    	Rdfile <- attr(srcref, "srcfile")$filename
    pratt <- attr(Rd, "prepared")
    if (is.null(pratt)) pratt <- 0L
    if ("build" %in% stages)
    	Rd <- processRdSexprs(Rd, "build", options)
    if (!is.null(defines))
    	Rd <- processRdIfdefs(Rd, defines)
    for (stage in c("install", "render"))
    	if (stage %in% stages)
    	    Rd <- processRdSexprs(Rd, stage, options)
    if (pratt < 2L && stage2)
        Rd <- prepare2_Rd(Rd, Rdfile)
    meta <- attr(Rd, "meta")
    if (pratt < 3L && stage3)
        Rd <- prepare3_Rd(Rd, Rdfile, msglevel = msglevel)

    # Restore flags from any sections that are left
    Rd <- setDynamicFlags(Rd, apply(sapply(Rd, getDynamicFlags), 1, any))

    structure(Rd, Rdfile = Rdfile, class = "Rd", meta = meta)
}

prepare2_Rd <- function(Rd, Rdfile)
{
    sections <- RdTags(Rd)

    ## FIXME: we no longer make any use of \Rdversion
    version <- which(sections == "\\Rdversion")
    if (length(version) > 1L)
    	stopRd(Rd[[version[2L]]], Rdfile,
               "Only one \\Rdversion declaration is allowed")

    ## Give warning (pro tem) for nonblank text outside a section
    if (length(bad <- grep("[^[:blank:][:cntrl:]]",
                           unlist(Rd[sections == "TEXT"]),
                           perl = TRUE, useBytes = TRUE )))
        for(s in bad)
            warnRd(Rd[sections == "TEXT"][[s]], Rdfile,
                   "All text must be in a section")

    drop <- rep.int(FALSE, length(sections))

    where <- which(sections == "\\examples")
    if(length(where) > 1L) {
        warnRd(Rd[where[[2L]]], Rdfile,
               "Only one \\examples section is allowed: the first will be used")
        drop[where[-1L]] <- TRUE
    }

    enc <- which(sections == "\\encoding")
    if (length(enc)) {
    	if (length(enc) > 1L) {
    	    warnRd(Rd[[enc[2L]]], Rdfile,
                   "Only one \\encoding declaration is allowed: the first will be used")
            drop[enc[-1L]] <- TRUE
            enc <- enc[[1L]]
        }
    	encoding <- Rd[[enc]]
    	if (!identical(RdTags(encoding), "TEXT"))
    	    stopRd(encoding, Rdfile, "'encoding' must be plain text")
    }

    dt <- which(sections == "\\docType")
    docTypes <- character(length(dt))
    if(length(dt)) {
        for(i in seq_along(dt)) {
            docType <- Rd[[dt[i]]]
            if(!identical(RdTags(docType), "TEXT"))
        	stopRd(docType, Rdfile, "'docType' must be plain text")
            docTypes[i] <- docType[[1L]]
         }
    }

    ## Drop all the parts that are not rendered
    extras <- c("COMMENT", "TEXT", "\\docType", "\\Rdversion", "\\RdOpts")
    drop <- drop | (sections %in% extras)
    bad <- ! sections %in% c(names(sectionOrder), extras)
    if (any(bad)) {
        for(s in which(bad))
            warnRd(Rd[[s]], Rdfile, "Section ",
                   sections[s], " is unrecognized and will be dropped")
        drop <- drop | bad
    }
    Rd <- Rd[!drop]
    sections <- sections[!drop]
    sortorder <- order(sectionOrder[sections])
    Rd <- Rd[sortorder]
    sections <- sections[sortorder]
    if (!identical(sections[1:2], c("\\title", "\\name")))
    	stopRd(Rd, Rdfile,
               "Sections \\title, and \\name must exist and be unique in Rd files")
    if (length(RdTags(Rd[[2L]])) > 1L)
        stopRd(RdTags(Rd[[2L]]), Rdfile,"\\name must only contain simple text")

    structure(Rd, meta = list(docType = docTypes))
}

prepare3_Rd <- function(Rd, Rdfile, msglevel = 0)
{
    ## Drop 'empty' sections: less rigorous than checkRd test
    keep <- rep(TRUE, length(Rd))
    checkEmpty <- function(x, this)
    {
        if(this) return(TRUE)
        if(is.list(x))
            for(xx in x) this <- checkEmpty(xx, this)
        else {
            tag <- attr(x, "Rd_tag")
            switch(tag,
                   COMMENT = {},
                   VERB =,
                   RCODE =,
                   TEXT = if(any(grepl("[^[:space:]]", s, perl = TRUE, useBytes=TRUE))) return(TRUE),
                   return(TRUE)
                   )
        }
        this
     }
    for (i in seq_along(Rd)) {
        this <- FALSE
        s0 <- section <- Rd[[i]]
        tag <- attr(section, "Rd_tag")
        if(tag == "\\section") {
            tagtitle <- sQuote(as.character(section[[1L]]))
            section <- section[[2L]]
        } else tagtitle <- tag
        for(s in section) this <- checkEmpty(s, this)
        keep[i] <- this
        if(!this && msglevel > 0)
            warnRd(s0, Rdfile, "Dropping empty section ", tagtitle)
    }
    Rd[keep]
}

sectionOrder <- c("\\title"=1, "\\name"=2, "\\alias"=2.1, "\\concept"=2.2,
                  "\\keyword"=2.3, "\\encoding"=2.4,
    "\\description"=3, "\\usage"=4, "\\synopsis"=4, "\\arguments"=5,
    "\\format"=6, "\\details"=7, "\\value"=8, "\\section"=9,
    "\\note"=10, "\\author" = 11, "\\source"=12, "\\references"=13,
    "\\seealso"=14, "\\examples"=15)

sectionTitles <-
    c("\\description"="Description", "\\usage"="Usage", "\\synopsis"="Usage",
      "\\arguments"="Arguments", "\\format"="Format", "\\details"="Details",
      "\\note"="Note", "\\section"="section", "\\author"="Author(s)",
      "\\references"="References", "\\source"="Source",
      "\\seealso"="See Also", "\\examples"="Examples", "\\value"="Value")

psub <- function(pattern, replacement, x)
##    gsub(pattern, replacement, x, perl = TRUE, useBytes = TRUE)
    .Internal(gsub(pattern, replacement, x, FALSE, TRUE, TRUE, FALSE, TRUE))

psub1 <- function(pattern, replacement, x)
##    sub(pattern, replacement, x, perl = TRUE, useBytes = TRUE)
    .Internal(sub(pattern, replacement, x, FALSE, TRUE, TRUE, FALSE, TRUE))

fsub <- function(pattern, replacement, x)
##    gsub(pattern, replacement, x, fixed = TRUE, useBytes = TRUE)
    .Internal(gsub(pattern, replacement, x, FALSE, TRUE, FALSE, TRUE, TRUE))

fsub1 <- function(pattern, replacement, x)
##    sub(pattern, replacement, x, fixed = TRUE, useBytes = TRUE)
    .Internal(sub(pattern, replacement, x, FALSE, TRUE, FALSE, TRUE, TRUE))


## for lists of messages, see ../man/checkRd.Rd
checkRd <- function(Rd, defines=.Platform$OS.type, stages="render",
                    unknownOK = TRUE, listOK = TRUE, ..., def_enc = FALSE)
{
    warnRd <- function(block, Rdfile, ..., level=0)
    {
        srcref <- attr(block, "srcref")
        msg <- if (is.null(srcref))
            paste("file '", Rdfile, "': ", ..., sep = "")
        else {
            loc <- paste(Rdfile, ":", srcref[1L], sep = "")
            if (srcref[1L] != srcref[3L]) loc <- paste(loc, "-", srcref[3L], sep="")
            paste(loc, ": ", ..., sep = "")
        }
        msg <- sprintf("checkRd: (%d) %s", level, msg)
        .messages <<- c(.messages, msg)
    }

    checkWrapped <- function(tag, block) checkContent(block, tag)

    checkLink <- function(tag, block) {
    	option <- attr(block, "Rd_option")
    	if(!is.null(option)) checkContent(option, tag)
    	checkContent(block, tag)
        get_link(block, tag, Rdfile) ## to do the same as Rd2HTML
    }

    ## blocktag is unused
    checkBlock <- function(block, tag, blocktag)
    {
	switch(tag,
               ## parser already warned here
               UNKNOWN = if (!unknownOK)
               stopRd(block, Rdfile, "Unrecognized macro ", block[[1L]]),
               VERB = ,
               RCODE = ,
               TEXT = {
                   if(!def_enc) {
                       ## check for encoding; this is UTF-8 if known
                       ## (but then def_enc = TRUE?)
                       msg2 <- if(inEnc2) "in second part of \\enc" else "without declared encoding"
                       if(Encoding(block) == "UTF-8")
                           warnRd(block, Rdfile, level = -1,
                                  "Non-ASCII contents ", msg2)
                       if(grepl("<[0123456789abcdef][0123456789abcdef]>", block))
                           warnRd(block, Rdfile, level = -3,
                                  "Apparent non-ASCII contents ", msg2)
                   }
                   ## check if this renders as non-whitespace
                   if(!grepl("^[[:space:]]*$", block)) has_text <<- TRUE
               },
               COMMENT = {},
               LIST = if (length(block)) {
                   deparse <- sQuote(paste(as.character.Rd(block), collapse=""))
                   if(!listOK)
                       stopRd(block, Rdfile, "Unnecessary braces at ", deparse)
                   else warnRd(block, Rdfile, level = -3,
                               "Unnecessary braces at ", deparse)
                   checkContent(block, tag)
               },
               "\\describe"=,
               "\\enumerate"=,
               "\\itemize"=,
               "\\bold"=,
               "\\cite"=,
               "\\command"=,
               "\\dfn"=,
               "\\emph"=,
               "\\kbd"= checkContent(block, tag),
               "\\code"=,
               "\\preformatted"= checkCodeBlock(block, tag),
               "\\Sexpr"=,
               "\\special"=,
               "\\strong"=,
               "\\var" =,
               "\\verb"= checkContent(block, tag),
               "\\linkS4class" =,
               "\\link" = checkLink(tag, block),
               "\\email" =,
               "\\url" = has_text <<- TRUE,
               "\\cr" ={},
               "\\dots" =,
               "\\ldots" =,
               "\\R" = has_text <<- TRUE,
               "\\acronym" =,
               "\\env" =,
               "\\file" =,
               "\\option" =,
               "\\pkg" =,
               "\\samp" =,
               "\\sQuote" =,
               "\\dQuote" = checkContent(block, tag),
               "\\method" =,
               "\\S3method" =,
               "\\S4method" =
                   warnRd(block, Rdfile, level = 7, "Tag ", tag,
                          " not valid outside a code block"),
               "\\enc" = {
                   checkContent(block[[1L]], tag)
                   ## second arg should always be ASCII
                   save_enc <- def_enc
                   def_enc <<- FALSE
                   inEnc2 <<- TRUE
                   checkContent(block[[2L]], tag)
                   def_enc <<- save_enc
                   inEnc2 <<- FALSE
               },
               "\\eqn" =,
               "\\deqn" = {
                   checkContent(block[[1L]])
                   if (length(block) > 1L) checkContent(block[[2L]])
               },
               "\\tabular" = checkTabular(block),
               "\\if" =,
               "\\ifelse" = {
    		   condition <- block[[1L]]
    		   tags <- RdTags(condition)
    		   if (!all(tags %in% c("TEXT", "\\Sexpr"))) 
    		       stopRd(block, Rdfile, "Condition must be \\Sexpr or plain text")
    		   condition <- condition[tags == "TEXT"]
    		   allow <- .strip_whitespace(strsplit(paste(condition, collapse=""), ",")[[1L]])
    		   unknown <- allow[!(allow %in% 
    		          c("", "latex", "example", "text", "html", "TRUE", "FALSE"))]
    		   if (length(unknown)) 
    		       warnRd(block, Rdfile, "Unrecognized format: ", unknown)
                   checkContent(block[[2L]])
                   if (tag == "\\ifelse")
                       checkContent(block[[3L]])
               },
               "\\out" = {
               	   tags <- RdTags(block)
               	   if (!all(tags == "VERB"))
               	       stopRd(block, Rdfile, "Must contain verbatim text")
               },
               warnRd(block, Rdfile, level = 7, "Tag ", tag, " not recognized"))
    }

    checkCodeBlock <- function(blocks, blocktag)
    {
	for (block in blocks) {
            tag <- attr(block, "Rd_tag")
            switch(tag,
                   ## parser already warned here
                   UNKNOWN = if (!unknownOK)
                   stopRd(block, Rdfile, "Unrecognized macro ", block[[1L]]),
                   VERB = ,
                   RCODE = ,
                   TEXT = {
                       if(!def_enc) {
                           ## check for encoding; this is UTF-8 if known
                           ## (but then def_enc = TRUE?)
                           msg2 <- if(inEnc2) "in second part of \\enc" else "without declared encoding"
                           if(Encoding(block) == "UTF-8")
                               warnRd(block, Rdfile, level = -1,
                                      "Non-ASCII contents ", msg2)
                           if(grepl("<[0123456789abcdef][0123456789abcdef]>", block))
                               warnRd(block, Rdfile, level = -3,
                                      "Apparent non-ASCII contents ", msg2)
                       }
                       ## check if this renders as non-whitespace
                       if(!grepl("^[[:space:]]*$", block)) has_text <<- TRUE
                   },
                   COMMENT = {},
                   "\\var" = checkCodeBlock(block, blocktag), # not preformatted, but the parser checks that
                   "\\special" = checkCodeBlock(block, blocktag),
                   "\\dots" = has_text <<- TRUE,
                   "\\ldots" = {
                       ## but it is rendered as ... in all converters
                       warnRd(block, Rdfile, level = -3,
                              "Tag ", tag, " is invalid in a code block")
                       has_text <<- TRUE
                   },
                   ## these are valid in \code, at least
                   "\\linkS4class" =,
                   "\\link" = checkLink(tag, block),
                   "\\method" =,
                   "\\S3method" =,
                   "\\S4method" = if(blocktag == "\\usage") {
                       checkContent(block[[1L]], tag) # generic
                       checkContent(block[[2L]], tag) # class
                   } else warnRd(block, Rdfile, level = 5,
                                 "Tag ", tag, " is only valid in \\usage"),
                   "\\dontrun" =,
                   "\\donttest" =,
                   "\\dontshow" =,
                   "\\testonly" = if(blocktag == "\\examples")
                   checkCodeBlock(block, blocktag)
                   else warnRd(block, Rdfile, level = 5,
                               "Tag ", tag, " is only valid in \\examples"),
                   {
                       warnRd(block, Rdfile, level = 5,
                              "Tag ", tag, " is invalid in a ",
                              blocktag, " block")
                       has_text <<- TRUE  # likely, e.g. \url
                   })
        }
    }

    checkTabular <- function(table) {
        has_text <<- TRUE
    	format <- table[[1L]]
    	content <- table[[2L]]
    	if (length(format) != 1 || RdTags(format) != "TEXT")
    	    warnRd(table, Rdfile, level = 7,
                   "\\tabular format must be simple text")
    	format <- strsplit(format[[1L]], "", fixed=TRUE)[[1L]]
    	if (!all(format %in% c("l", "c", "r")))
    	    warnRd(table, Rdfile, level = 7,
                   "Unrecognized \\tabular format: ", table[[1L]][[1L]])
        tags <- RdTags(content)

        newrow <- TRUE
        for (i in seq_along(tags)) {
            if (newrow) {
            	newrow <- FALSE
            	col <- 0
            	newcol <- TRUE
            }
            if (newcol) {
                col <- col + 1
                if (col > length(format))
                    warnRd(table, Rdfile, level = 7,
                           "Only ", length(format),
                           " columns allowed in this table")
            	newcol <- FALSE
            }
            switch(tags[i],
            "\\tab" = {
            	newcol <- TRUE
            },
            "\\cr" = {
            	newrow <- TRUE
            },
            checkBlock(content[[i]], tags[i], "\\tabular"))
        }
    }

    checkContent <- function(blocks, blocktag) {
        inlist <- FALSE

	tags <- RdTags(blocks)

	for (i in seq_along(tags)) {
            tag <- tags[i]
            block <- blocks[[i]]
            switch(tag,
            "\\item" = {
    	    	if (!inlist) inlist <- TRUE
    		switch(blocktag,
    		"\\arguments"= {
    		    checkContent(block[[1L]], tag)
    		    checkContent(block[[2L]], tag)
    		},
    		"\\value"=,
    		"\\describe"= {
    		    checkContent(block[[1L]], tag)
    		    checkContent(block[[2L]], tag)
    		},
    		"\\enumerate"=,
    		"\\itemize"= {})
    	    },
    	    { # default
    	    	if (inlist && !(blocktag %in% c("\\itemize", "\\enumerate"))
    	    	           && !(tag == "TEXT" && isBlankRd(block))) {
    		    inlist <- FALSE
    		}
    		checkBlock(block, tag, blocktag)
    	    })
	}
    }

    has_text <- FALSE
    checkSection <- function(section, tag) {
    	if (tag == "\\section") {
    	    title <- section[[1L]]
            ## should be simple text
            if(length(title) < 1L || attr(title[[1L]], "Rd_tag") != "TEXT") {
                warnRd(section, Rdfile, level = 5,
                       "Title of \\section must be non-empty plain text")
            }
    	    checkContent(title, tag)
    	    section <- section[[2L]]
            ## replace 'tag' in message below
            tagtitle <- sQuote(as.character(title))
    	} else tagtitle <- tag
        has_text <<- FALSE
        if (tag %in% c("\\usage", "\\synopsis", "\\examples"))
            checkCodeBlock(section, tag)
    	else checkContent(section, tag)
        if(!has_text) warnRd(section, Rdfile, level = 3,
                             "Empty section ", tagtitle)
    }

    checkUnique <- function(tag) {
    	which <- which(sections == tag)
    	if (length(which) < 1L)
    	    warnRd(Rd, Rdfile, level = 7, "Must have a ", tag)
    	else {
            if (length(which) > 1L)
    	    warnRd(Rd[[which[2L]]], Rdfile, level = 7,
                   "Only one ", tag, " is allowed")
            empty <- TRUE
            for(block in Rd[[which]]) {
                switch(attr(block, "Rd_tag"),
                       TEXT = if(!grepl("^[[:space:]]*$", block))
                       empty <- FALSE,
                       empty <- FALSE)
            }
            if(empty)
                warnRd(Rd[[which]], Rdfile, level = 5,
                       "Tag ", tag, " must not be empty")
        }
    }

    dt <- which(RdTags(Rd) == "\\docType")
    docTypes <- character(length(dt))
    if (length(dt)) {
        for (i in dt) {
            docType <- Rd[[i]]
            if(!identical(RdTags(docType), "TEXT"))
        	warnRd(docType, Rdfile, level = 7,
                       "'docType' must be plain text")
            docTypes[i] <- docType[[1L]]
         }
    }

    .messages <- character()
    .whandler <-     function(e) {
        .messages <<- c(.messages, paste("prepare_Rd:", conditionMessage(e)))
        invokeRestart("muffleWarning")
    }

    Rd <- withCallingHandlers({
        prepare_Rd(Rd, defines=defines, stages=stages,
                   warningCalls = FALSE, ..., msglevel = 1)
    }, warning = .whandler)
    Rdfile <- attr(Rd, "Rdfile")
    sections <- RdTags(Rd)

    enc <- which(sections == "\\encoding")
    ## sanity was checked in prepare2_Rd
    if (length(enc)) def_enc <- TRUE

    inEnc2 <- FALSE
    if(!identical("package", docTypes))
        checkUnique("\\description")
    for (i in seq_along(sections))
        checkSection(Rd[[i]], sections[i])

    structure(.messages, class = "checkRd")
}

print.checkRd <- function(x, minlevel = -Inf, ...)
{
    fromParse <- grepl("^prepare_Rd", x)
    x1 <- x[fromParse]
    x2 <- x[!fromParse]
    levs <- as.numeric(sub("^checkRd: \\(([-0123456789]+)(.*)", "\\1", x2))
    xx <- if(minlevel > 0) x2[levs >= minlevel] else c(x1, x2[levs >= minlevel])
    writeLines(unique(xx))
    invisible(x)
}

testRdConditional <- function(format, conditional, Rdfile) {
    condition <- conditional[[1L]]
    tags <- RdTags(condition)
    if (!all(tags == "TEXT")) stopRd(conditional, Rdfile, "condition must be plain text")
    
    allow <- .strip_whitespace(strsplit(paste(condition, collapse=""), ",")[[1L]])
    any(c("TRUE", format) %in% allow)
}


RdTextFilter <-
function(ifile, encoding = "unknown", keepSpacing = TRUE,
         drop = character(), keep = character())
{
    if (inherits(ifile, "Rd")) p <- ifile
    else p <- parse_Rd(ifile, encoding = encoding)
    tags <- RdTags(p)
    if ("\\encoding" %in% tags) {
	encoding <- p[[which(tags=="\\encoding")]][[1L]]
	if(encoding %in% c("UTF-8", "utf-8", "utf8")) encoding <- "UTF-8"
	p <- parse_Rd(ifile, encoding=encoding)
    } else
	encoding <- ""

    ## Directly using a text connection to accumulate the filtered
    ## output seems to be faster than using .eval_with_capture(): to use
    ## the latter, change mycat to cat, or use mycat <- cat, and create
    ## out via
    ## out <- .eval_with_capture({
    ##     show(p)
    ##     mycat("\n")
    ## })$output
    
    myval <- character()
    mycon <- textConnection("myval", open = "w", local = TRUE,
                            encoding = "UTF-8") 
    on.exit(close(mycon))
    mycat <- function(...) cat(..., file = mycon)

    prevline <- 1L
    prevcol <- 0L

    doPartialMarkup <- function(x, tags, i) { # handle things like \bold{pre}fix
        result <- FALSE
    	if (i < length(tags)
            && tags[i+1L] == "TEXT"
            && length(x[[i]]) == 1L
            && tags[i] %in% c("\\bold", "\\emph", "\\strong", "\\link")
            && !(tags[i] %in% drop)
            && RdTags(x[[i]]) == "TEXT") {
    	    text1 <- x[[i]][[1L]]
    	    if (length(grep("[^[:space:]]$", text1))) { # Ends in non-blank
    	    	text2 <- x[[i+1L]]
    	    	if (length(grep("^[^[:space:]]", text2))) { # Starts non-blank
    	    	    show(text1)
    	    	    prevcol <<- prevcol+1L # Shift text2 left by one column
    	    	    saveline <- prevline
    	    	    show(text2)
    	    	    if (prevline == saveline)
    	    	    	prevcol <<- prevcol-1L
    		    result <- TRUE
    		}
    	    }
	}
	result
    }

    show <- function(x) {
	srcref <- attr(x, "srcref")
	firstline <- srcref[1L]
	firstbyte <- srcref[2L]
	lastline <- srcref[3L]
	lastbyte <- srcref[4L]
	firstcol <- srcref[5L]
	lastcol <- srcref[6L]
	tag <- attr(x, "Rd_tag")
	if (is.null(tag)) tag <- "NULL"
	if (tag %in% drop) tag <- "DROP"
	else if (tag %in% keep) tag <- "KEEPLIST"  # Include both text and lists
	switch(tag,
	KEEP =,
	TEXT = {
	    if (prevline < firstline) {
		prevcol <<- 0L
		mycat(rep.int("\n",
                              if(keepSpacing) firstline - prevline else 1L))
	    }
	    if (keepSpacing)
                mycat(rep.int(" ", firstcol - prevcol - 1L), sep = "")
	    x <- as.character(srcref) # go back to original form
	    mycat(x, sep = "")
	    prevcol <<- lastcol
	    prevline <<- lastline
	},
	"\\docType"=,
	"\\encoding"=,
	"\\keyword"=,
	"\\email"=,
	"\\file"=,
	"\\linkS4class"=,
	"\\pkg"=,
	"\\var"=,
	"\\method"=,
	"\\S3method"=,
	"\\S4method"=,
	"\\link"=,
	DROP = {},  # do nothing

	"\\tabular"=,
	"#ifdef"=,
	"#ifndef"={  # Ignore the first arg, process the second
	    show(x[[2L]])
	},
	"\\item"={   # Ignore the first arg of a two-arg item
	    if (length(x) == 2L) show(x[[2L]])
	},
	{	# default
	    if (is.list(x)) {
             	tags <- RdTags(x)
             	i <- 0L
             	while (i < length(x)) {
             	    i <- i + 1L
             	    if (doPartialMarkup(x, tags, i))
             	    	i <- i + 1L
             	    else
             		show(x[[i]])
             	}
	    } else if (tag == "KEEPLIST") {
	    	attr(x, "Rd_tag") <- "KEEP"
	    	show(x)
	    }
	})
    }

    show(p)
    mycat("\n")
    
    out <- textConnectionValue(mycon)

    ## Ideally, we would always canonicalize to UTF-8.
    ## However, when using RdTextFilter() for aspell(), it is not clear
    ## whether this is a good idea: the aspell program does not need to
    ## have full UTF-8 support (and what precisely holds is not clear:
    ## the manuals says that aspell
    ##   can easily check documents in UTF-8 without having to use a
    ##   special dictionary.
    ## but also
    ##   If Aspell is compiled with a version of the curses library that
    ##   support wide characters then Aspell can also check UTF-8 text. 
    ## So at least until this can be resolved, turn filter results for
    ## Rd files originally in latin1 back to latin1.
    if(encoding == "latin1")
        out <- iconv(out, "UTF-8", "latin1")

    out
}
SweaveTeXFilter <- function(ifile, encoding = "unknown")
{
    lines <- readLines(ifile, encoding = encoding, warn = FALSE)

    TEXT <- 1L
    CODE <- 0L

    recs <- rbind( data.frame(line=grep("^@", lines), type=TEXT),
                   data.frame(line=grep("^<<(.*)>>=.*", lines), type=CODE))
    recs <- recs[order(recs$line),]
    last <- 0L
    state <- TEXT
    for (i in seq_len(nrow(recs))) {
    	line <- recs$line[i]
    	if (state == CODE)
    	    lines[(last+1L):line] <- ""
    	else
    	    lines[line] <- ""
    	state <- recs$type[i]
    	last <- line
    }
    lines
}
#  File src/library/tools/R/Vignettes.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

### * checkVignettes
###
### Run a tangle+source and a weave on all vignettes of a package.

checkVignettes <-
function(package, dir, lib.loc = NULL,
         tangle = TRUE, weave = TRUE, latex = FALSE,
         workdir = c("tmp", "src", "cur"),
         keepfiles = FALSE)
{
    vigns <- pkgVignettes(package = package, dir = dir, lib.loc = lib.loc)
    if(is.null(vigns)) return(NULL)

    workdir <- match.arg(workdir)
    wd <- getwd()
    if(workdir == "tmp") {
        tmpd <- tempfile("Sweave")
        if(!dir.create(tmpd))
            stop("unable to create temp directory ", tmpd)
        setwd(tmpd)
    }
    else {
        keepfiles <- TRUE
        if(workdir == "src") setwd(vigns$dir)
    }

    on.exit({
        setwd(wd)
        if(!keepfiles) unlink(tmpd, recursive = TRUE)
    })

    file.create(".check.timestamp")
    result <- list(tangle = list(), weave = list(),
                   source = list(), latex = list())

    for(f in vigns$docs) {
        if(tangle)
            .eval_with_capture(tryCatch(utils::Stangle(f, quiet = TRUE),
                                        error = function(e)
                                        result$tangle[[f]] <<-
                                        conditionMessage(e)))
        if(weave)
            .eval_with_capture(tryCatch(utils::Sweave(f, quiet = TRUE),
                                        error = function(e)
                                        result$weave[[f]] <<-
                                        conditionMessage(e)))
    }

    if(tangle) {
        ## Tangling can create several source files if splitting is on.
        sources <- list_files_with_exts(getwd(), c("r", "s", "R", "S"))
        sources <- sources[file_test("-nt", sources, ".check.timestamp")]
        for(f in sources)
            .eval_with_capture(tryCatch(source(f),
                                        error = function(e)
                                        result$source[[f]] <<-
                                        conditionMessage(e)))
    }
    if(weave && latex) {
        if(!("makefile" %in% tolower(list.files(vigns$dir)))) {
            ## <NOTE>
            ## This used to run texi2dvi on *all* vignettes, including
            ## the ones already known from the above to give trouble.
            ## In addition, texi2dvi errors were not caught, so that in
            ## particular the results of the previous QC analysis were
            ## *not* returned in case of such errors ...
            ## Hence, let us
            ## * Only run texi2dvi() on previously unproblematic vignettes
            ## * Catch texi2dvi() errors similar to the above.
            ## * Do *not* immediately show texi2dvi() output as part of
            ##   running checkVignettes().
            ## (For the future, maybe keep this output and provide it as
            ## additional diagnostics ...)
            ## </NOTE>
            bad_vignettes <- as.character(names(unlist(result)))
            bad_vignettes <- file_path_sans_ext(basename(bad_vignettes))
            for(f in vigns$docs) {
                bf <- file_path_sans_ext(basename(f))
                if(bf %in% bad_vignettes) break
                bft <- paste(bf, ".tex", sep = "")
                .eval_with_capture(tryCatch(texi2dvi(file = bft, pdf = TRUE,
                                                     clean = FALSE,
                                                     quiet = TRUE),
                                            error = function(e)
                                            result$latex[[f]] <<-
                                            conditionMessage(e)))
            }
        }
    }

    file.remove(".check.timestamp")
    class(result) <- "checkVignettes"
    result
}

print.checkVignettes <-
function(x, ...)
{
    mycat <- function(y, title) {
        if(length(y)){
            cat("\n", title, "\n\n", sep = "")
            for(k in seq_along(y)) {
                cat("File", names(y)[k], ":\n")
                cat(as.character(y[[k]]), "\n")
            }
        }
    }

    mycat(x$weave,  "*** Weave Errors ***")
    mycat(x$tangle, "*** Tangle Errors ***")
    mycat(x$source, "*** Source Errors ***")
    mycat(x$latex,  "*** PDFLaTeX Errors ***")

    invisible(x)
}

### * pkgVignettes
###
### Get an object of class pkgVignettes which contains a list of Sweave
### files and the name of the directory which contains them.

pkgVignettes <-
function(package, dir, lib.loc = NULL)
{
    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        docdir <- file.path(.find.package(package, lib.loc), "doc")
        ## Using package installed in @code{dir} ...
    }
    else {
        if(missing(dir))
            stop("you must specify 'package' or 'dir'")
        ## Using sources from directory @code{dir} ...
        if(!file_test("-d", dir))
            stop(gettextf("directory '%s' does not exist", dir),
                 domain = NA)
        else
            docdir <- file.path(file_path_as_absolute(dir), "inst",
                                "doc")
    }

    if(!file_test("-d", docdir)) return(NULL)

    docs <- list_files_with_type(docdir, "vignette")

    z <- list(docs=docs, dir=docdir)
    class(z) <- "pkgVignettes"
    z
}

### * buildVignettes
###
### Run a weave and pdflatex on all vignettes of a package and try to
### remove all temporary files that were created.

buildVignettes <-
function(package, dir, lib.loc = NULL, quiet = TRUE, clean = TRUE)
{
    vigns <- pkgVignettes(package = package, dir = dir, lib.loc = lib.loc)
    if(is.null(vigns)) return(invisible())

    wd <- getwd()
    on.exit(setwd(wd))
    setwd(vigns$dir)

    ## should this recurse into subdirs?
    origfiles <- list.files(all.files = TRUE)
    have.makefile <- "makefile" %in% tolower(origfiles)
    file.create(".build.timestamp")

    pdfs <- character()
    for(f in vigns$docs) {
        f <- basename(f)
        bf <- file_path_sans_ext(f)
        bft <- paste(bf, ".tex", sep = "")
        pdfs <- c(pdfs, paste(bf, ".pdf", sep = ""))

        tryCatch(utils::Sweave(f, quiet = quiet),
                 error = function(e) {
                     stop(gettextf("processing vignette '%s' failed with diagnostics:\n%s",
                                   f, conditionMessage(e)),
                          domain = NA, call. = FALSE)
                 })
        if(!have.makefile)
            texi2dvi(file = bft, pdf = TRUE, clean = FALSE, quiet = quiet)
    }

    if(have.makefile) {
    	make <- Sys.getenv("MAKE")
        if(!nzchar(make)) make <- "make"
        yy <- system(make)
        if(make == "" || yy > 0) stop("running 'make' failed")
    } else {
        if(clean) {
            f <- list.files(all.files = TRUE) %w/o% c(".", "..", pdfs)
            newer <- file_test("-nt", f, ".build.timestamp")
            file.remove(f[newer])
        }
        f <- list.files(all.files = TRUE)
        file.remove(f %w/o% c(".", "..", pdfs, origfiles))
    }

    file.remove(".build.timestamp")
    ## Might have been in origfiles ...

    invisible(NULL)
}

### * .build_vignette_index

.get_vignette_metadata <-
function(lines, tag)
{
    meta_RE <- paste("[[:space:]]*%+[[:space:]]*\\\\Vignette", tag,
                     "\\{([^}]*)\\}", sep = "")
    meta <- grep(meta_RE, lines, value = TRUE)
    .strip_whitespace(gsub(meta_RE, "\\1", meta))
}

vignetteInfo <-
function(file)
{
    lines <- readLines(file, warn = FALSE)

    ## <FIXME>
    ## Can only proceed with lines with are valid in the current locale.
    ## Unfortunately, vignette encodings are a mess: package encodings
    ## might apply, but be overridden by \inputencoding commands.
    ## For now, assume that vignette metadata occur in all ASCII lines.
    ## (Could also iconv() using sub = "byte".)
    lines[is.na(nchar(lines, "c", TRUE))] <- ""
    ## </FIXME>

    ## \VignetteIndexEntry
    title <- c(.get_vignette_metadata(lines, "IndexEntry"), "")[1L]
    ## \VignetteDepends
    depends <- .get_vignette_metadata(lines, "Depends")
    if(length(depends))
        depends <- unlist(strsplit(depends[1L], ", *"))
    ## \VignetteKeyword and old-style \VignetteKeywords
    keywords <- .get_vignette_metadata(lines, "Keywords")
    keywords <- if(!length(keywords)) {
        ## No old-style \VignetteKeywords entries found.
        .get_vignette_metadata(lines, "Keyword")
    }
    else
        unlist(strsplit(keywords[1L], ", *"))
    list(file = file, title = title, depends = depends,
         keywords = keywords)
}

.build_vignette_index <-
function(vignetteDir)
{
    if(!file_test("-d", vignetteDir))
        stop(gettextf("directory '%s' does not exist", vignetteDir),
             domain = NA)

    vignetteFiles <-
        path.expand(list_files_with_type(vignetteDir, "vignette"))

    if(!length(vignetteFiles)) {
        out <- data.frame(File = character(),
                          Title = character(),
                          PDF = character(),
                          stringsAsFactors = FALSE)
        out$Depends <- list()
        out$Keywords <- list()
        return(out)
    }

    contents <- vector("list", length = length(vignetteFiles) * 4L)
    dim(contents) <- c(length(vignetteFiles), 4L)
    for(i in seq_along(vignetteFiles))
        contents[i, ] <- vignetteInfo(vignetteFiles[i])
    colnames(contents) <- c("File", "Title", "Depends", "Keywords")

    ## (Note that paste(character(0L), ".pdf") does not do what we want.)
    vignettePDFs <- sub("$", ".pdf", file_path_sans_ext(vignetteFiles))

    vignetteTitles <- unlist(contents[, "Title"])

    vignettePDFs[!file_test("-f", vignettePDFs)] <- ""
    vignettePDFs <- basename(vignettePDFs)

    out <- data.frame(File = unlist(contents[, "File"]),
                      Title = vignetteTitles,
                      PDF = vignettePDFs,
                      row.names = NULL, # avoid trying to compute row
                                        # names
                      stringsAsFactors = FALSE)
    out$Depends <- contents[, "Depends"]
    out$Keywords <- contents[, "Keywords"]
    out
}

### * .check_vignette_index

.check_vignette_index <-
function(vignetteDir)
{
    if(!file_test("-d", vignetteDir))
        stop(gettextf("directory '%s' does not exist", vignetteDir),
             domain = NA)
    vignetteIndex <- .build_vignette_index(vignetteDir)
    badEntries <-
        vignetteIndex[grep("^[[:space:]]*$", vignetteIndex[, "Title"]),
                      "File"]
    class(badEntries) <- "check_vignette_index"
    badEntries
}

print.check_vignette_index <-
function(x, ...)
{
    if(length(x)) {
        writeLines(paste("Vignettes with missing or empty",
                         "\\VignetteIndexEntry:"))
        print(basename(file_path_sans_ext(unclass(x))), ...)
    }
    invisible(x)
}


### * .writeVignetteHtmlIndex

.writeVignetteHtmlIndex <-
function(pkg, con, vignetteIndex = NULL)
{
    html <- c('<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">',
              paste("<html><head><title>R:", pkg, "vignettes</title>"),
              "<link rel=\"stylesheet\" type=\"text/css\" href=\"../../R.css\">",
              "</head><body>",
              paste("<h2>Vignettes of package", pkg,"</h2>"))

    if(is.null(vignetteIndex) || nrow(vignetteIndex) == 0L) {
        html <- c(html, "Sorry, the package contains no vignette meta-information or index.",
                  "Please browse the <a href=\".\">directory</a>.")
    }
    else{
        html <- c(html, "<dl>")
        for(k in seq_len(nrow(vignetteIndex))){
            html <- c(html,
                      paste("<dt><a href=\"", vignetteIndex[k, "PDF"], "\">",
                            vignetteIndex[k, "PDF"], "</a>:", sep=""),
                      paste("<dd>", vignetteIndex[k, "Title"]))
        }
        html <- c(html, "</dl>")
    }
    html <- c(html, "</body></html>")
    writeLines(html, con=con)
}

vignetteDepends <-
function(vignette, recursive = TRUE, reduce = TRUE,
         local = TRUE, lib.loc = NULL)
{
    if (length(vignette) != 1L)
        stop("argument 'vignette' must be of length 1")
    if (!file.exists(vignette))
        stop(gettextf("file '%s' not found", vignette),
             domain = NA)

    vigDeps <- vignetteInfo(vignette)$depends

    depMtrx <- getVigDepMtrx(vigDeps)
    instPkgs <- utils::installed.packages(lib.loc=lib.loc)
    getDepList(depMtrx, instPkgs, recursive, local, reduce,
               lib.loc)
}

getVigDepMtrx <-
function(vigDeps)
{
    ## Taken almost directly out of 'package.dependencies'
    if (length(vigDeps)) {
        z <- unlist(strsplit(vigDeps, ",", fixed=TRUE))
        z <- sub("^[[:space:]]*(.*)", "\\1", z)
        z <- sub("(.*)[[:space:]]*$", "\\1", z)
        pat <- "^([^\\([:space:]]+)[[:space:]]*\\(([^\\)]+)\\).*"
        depMtrx <- cbind(sub(pat, "\\1", z),
                         sub(pat, "\\2", z),
                         NA)
        noversion <- depMtrx[, 1L] == depMtrx[, 2L]
        depMtrx[noversion, 2L] <- NA
        pat <- "[[:space:]]*([[<>=]+)[[:space:]]+(.*)"
        depMtrx[!noversion, 2:3] <-
            c(sub(pat, "\\1", depMtrx[!noversion, 2L]),
              sub(pat, "\\2", depMtrx[!noversion, 2L]))
        depMtrx
    }
    else
        NA
}


### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
#  File src/library/tools/R/admin.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/


### * .install_package_description

## called from basepkg.mk and .install_packages
.install_package_description <-
function(dir, outDir)
{
    ## Function for taking the DESCRIPTION package meta-information,
    ## checking/validating it, and installing it with the 'Built:'
    ## field added.  Note that from 1.7.0 on, packages without
    ## compiled code are not marked as being from any platform.

    ## Check first.  Note that this also calls .read_description(), but
    ## .check_package_description() currently really needs to know the
    ## path to the DESCRIPTION file, and returns an object with check
    ## results and not the package metadata ...
    ok <- .check_package_description(file.path(dir, "DESCRIPTION"))
    if(any(as.integer(sapply(ok, length)) > 0L)) {
        stop(paste(gettext("Invalid DESCRIPTION file") ,
                   paste(.eval_with_capture(print(ok))$output,
                         collapse = "\n"),
                   sep = "\n\n"),
             domain = NA,
             call. = FALSE)
    }

    ## This reads (in C locale) byte-by-byte, declares latin1 or UTF-8
    ## Maybe it would be better to re-encode others (there are none at
    ## present, at least in a UTF-8 locale?
    db <- .read_description(file.path(dir, "DESCRIPTION"))

    ## should not have a Built: field, so ignore it if it is there
    nm <- names(db)
    if("Built" %in% nm) {
        db <- db[-match("Built", nm)]
        warning(gettextf("*** someone has corrupted the Built field in package '%s' ***",
                         db["Package"]),
                domain = NA,
                call. = FALSE)
    }

    OS <- Sys.getenv("R_OSTYPE")
    OStype <- if(nzchar(OS) && OS == "windows")
        "i386-pc-mingw32"
    else
        R.version$platform
    if (length(grep("-apple-darwin",R.version$platform)) &&
        nzchar(Sys.getenv("R_ARCH")))
        OStype <- sub(".*-apple-darwin", "universal-apple-darwin", OStype)
    Built <-
        paste("R ",
              paste(R.version[c("major", "minor")],
                    collapse = "."),
              "; ",
              if(file_test("-d", file.path(dir, "src"))) OStype
              else "",
              "; ",
              ## Prefer date in ISO 8601 format, UTC.
              format(Sys.time(), tz = "UTC", usetz = TRUE),
              ## Sys.time(),
              "; ",
              .OStype(),
              sep = "")

    ## At some point of time, we had:
    ##   We must not split the Built: field across lines.
    ## Not sure if this is still true.  If not, the following could be
    ## simplified to
    ##   db["Built"] <- Built
    ##   write.dcf(rbind(db), file.path(outDir, "DESCRIPTION"))

    ## Avoid declared encodings
    dbn <- db; Encoding(dbn) <- "unknown"
    outConn <- file(file.path(outDir, "DESCRIPTION"), open = "w")
    write.dcf(rbind(dbn), outConn)
    writeLines(paste("Built", Built, sep = ": "), outConn)
    close(outConn)

    db["Built"] <- Built

    outMetaDir <- file.path(outDir, "Meta")
    if(!file_test("-d", outMetaDir) && !dir.create(outMetaDir))
         stop(gettextf("cannot open directory '%s'",
                       outMetaDir),
              domain = NA)
    saveInfo <- .split_description(db)
    .saveRDS(saveInfo, file.path(outMetaDir, "package.rds"))

    invisible()
}

### * .split_description

## also used in .getRequiredPackages
.split_description <-
function(db, verbose = FALSE)
{
    if(!is.na(Built <- db["Built"])) {
        Built <- as.list(strsplit(Built, "; ")[[1L]])
        if(length(Built) != 4L) {
            warning(gettextf("*** someone has corrupted the Built field in package '%s' ***",
                             db["Package"]),
                    domain = NA,
                    call. = FALSE)
            Built <- NULL
        } else {
            names(Built) <- c("R", "Platform", "Date", "OStype")
            Built[["R"]] <- R_system_version(sub("^R ([0-9.]+)", "\\1",
                                                 Built[["R"]]))
        }
    } else Built <- NULL
    ## might perhaps have multiple entries
    Depends <- .split_dependencies(db[names(db) %in% "Depends"])
    ## We only need Rdepends for R < 2.7.0, but we still need to be
    ## able to check that someone is not trying to load this into a
    ## very old version of R.
    if("R" %in% names(Depends)) {
        Rdeps2 <- Depends["R" == names(Depends)]
        names(Rdeps2) <- NULL
        if(verbose && !all(sapply(Rdeps2[-1L], function(x)
            		   x$op %in% c("<", "<=")
            		&& x$version >= package_version("2.7.0")))) {
            entries <- lapply(Rdeps2, function(x)
                paste(lapply(x, as.character), collapse=""))
            message("WARNING: 'Depends' entry has multiple dependencies on R: ",
                    paste(unlist(entries), collapse=", "),
                    "\n\tonly the first will be used in R < 2.7.0")
        }
        Rdeps <- Depends[["R", exact = TRUE]] # the first one
        Depends <- Depends[names(Depends) != "R"]
        ## several packages have 'Depends: R', which is a noop.
        if(verbose && length(Rdeps) == 1L)
             message("WARNING: omitting pointless dependence on 'R' without a version requirement")
        if(length(Rdeps) <= 1L) Rdeps <- NULL
    } else Rdeps2 <- Rdeps <- NULL
    Rdeps <- as.vector(Rdeps)
    Suggests <- .split_dependencies(db[names(db) %in% "Suggests"])
    Imports <- .split_dependencies(db[names(db) %in% "Imports"])
    structure(list(DESCRIPTION = db, Built = Built,
                   Rdepends = Rdeps, Rdepends2 = Rdeps2,
                   Depends = Depends, Suggests = Suggests,
                   Imports = Imports),
              class = "packageDescription2")
}

### * .vinstall_package_descriptions_as_RDS

## called from src/library/Makefile
.vinstall_package_descriptions_as_RDS <-
function(dir, packages)
{
    ## For the given packages installed in @file{dir}, install their
    ## DESCRIPTION package metadata as R metadata.
    ## Really only useful for base packages under Unix.
    ## See @file{src/library/Makefile.in}.

    for(p in unlist(strsplit(packages, "[[:space:]]+"))) {
        meta_dir <- file.path(dir, p, "Meta")
        if(!file_test("-d", meta_dir) && !dir.create(meta_dir))
            stop(gettextf("cannot open directory '%s'", meta_dir))
        package_info_dcf_file <- file.path(dir, p, "DESCRIPTION")
        package_info_rds_file <- file.path(meta_dir, "package.rds")
        if(file_test("-nt",
                     package_info_rds_file,
                     package_info_dcf_file))
            next
        .saveRDS(.split_description(.read_description(package_info_dcf_file)),
                 package_info_rds_file)
    }
    invisible()
}

### * .update_package_rds

## not used
.update_package_rds <-
function(lib.loc = NULL)
{
    ## rebuild the dumped package descriptions for all packages in lib.loc
    if (is.null(lib.loc)) lib.loc <- .libPaths()
    lib.loc <- lib.loc[file.exists(lib.loc)]
    for (lib in lib.loc) {
        a <- list.files(lib, all.files = FALSE, full.names = TRUE)
        for (nam in a) {
            dfile <- file.path(nam, "DESCRIPTION")
            if (file.exists(dfile)) {
                print(nam)
                .install_package_description(nam, nam)
            }
        }
    }
}

### * .install_package_code_files

.install_package_code_files <-
function(dir, outDir)
{
    if(!file_test("-d", dir))
        stop(gettextf("directory '%s' does not exist", dir),
             domain = NA)
    dir <- file_path_as_absolute(dir)

    ## Attempt to set the LC_COLLATE locale to 'C' to turn off locale
    ## specific sorting.
    curLocale <- Sys.getlocale("LC_COLLATE")
    on.exit(Sys.setlocale("LC_COLLATE", curLocale), add = TRUE)
    ## (Guaranteed to work as per the Sys.setlocale() docs.)
    lccollate <- "C"
    if(Sys.setlocale("LC_COLLATE", lccollate) != lccollate) {
        ## <NOTE>
        ## I don't think we can give an error here.
        ## It may be the case that Sys.setlocale() fails because the "OS
        ## reports request cannot be honored" (src/main/platform.c), in
        ## which case we should still proceed ...
        warning("cannot turn off locale-specific sorting via LC_COLLATE")
        ## </NOTE>
    }

    ## We definitely need a valid DESCRIPTION file.
    db <- .read_description(file.path(dir, "DESCRIPTION"))

    codeDir <- file.path(dir, "R")
    if(!file_test("-d", codeDir)) return(invisible())

    codeFiles <- list_files_with_type(codeDir, "code", full.names = FALSE)

    collationField <-
        c(paste("Collate", .OStype(), sep = "."), "Collate")
    if(any(i <- collationField %in% names(db))) {
        collationField <- collationField[i][1L]
        codeFilesInCspec <- .read_collate_field(db[collationField])
        ## Duplicated entries in the collation spec?
        badFiles <-
            unique(codeFilesInCspec[duplicated(codeFilesInCspec)])
        if(length(badFiles)) {
            out <- gettextf("\nduplicated files in '%s' field:",
                            collationField)
            out <- paste(out,
                         paste(" ", badFiles, collapse = "\n"),
                         sep = "\n")
            stop(out, domain = NA)
        }
        ## See which files are listed in the collation spec but don't
        ## exist.
        badFiles <- codeFilesInCspec %w/o% codeFiles
        if(length(badFiles)) {
            out <- gettextf("\nfiles in '%s' field missing from '%s':",
                            collationField,
                            codeDir)
            out <- paste(out,
                         paste(" ", badFiles, collapse = "\n"),
                         sep = "\n")
            stop(out, domain = NA)
        }
        ## See which files exist but are missing from the collation
        ## spec.  Note that we do not want the collation spec to use
        ## only a subset of the available code files.
        badFiles <- codeFiles %w/o% codeFilesInCspec
        if(length(badFiles)) {
            out <- gettextf("\nfiles in '%s' missing from '%s' field:",
                            codeDir,
                            collationField)
            out <- paste(out,
                         paste(" ", badFiles, collapse = "\n"),
                         sep = "\n")
            stop(out, domain = NA)
        }
        ## Everything's groovy ...
        codeFiles <- codeFilesInCspec
    }

    codeFiles <- file.path(codeDir, codeFiles)

    if(!file_test("-d", outDir) && !dir.create(outDir))
        stop(gettextf("cannot open directory '%s'", outDir),
             domain = NA)
    outCodeDir <- file.path(outDir, "R")
    if(!file_test("-d", outCodeDir) && !dir.create(outCodeDir))
        stop(gettextf("cannot open directory '%s'", outCodeDir),
             domain = NA)
    outFile <- file.path(outCodeDir, db["Package"])
    if(!file.create(outFile))
        stop(gettextf("unable to create '%s'", outFile), domain = NA)
    writeLines(paste(".packageName <- \"", db["Package"], "\"", sep=""),
               outFile)
    enc <- as.vector(db["Encoding"])
    need_enc <- !is.na(enc) # Encoding was specified
    ## assume that if locale is 'C' we can used 8-bit encodings unchanged.
    if(need_enc && !(Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX"))) {
        con <- file(outFile, "a")
        on.exit(close(con))  # Windows does not like files left open
        for(f in codeFiles) {
            tmp <- iconv(readLines(f, warn = FALSE), from = enc, to = "")
            if(any(is.na(tmp)))
               stop(gettextf("unable to re-encode '%s'", basename(f)),
                    domain = NA, call. = FALSE)
            writeLines(paste("#line 1 \"", f, "\"", sep=""), con)
            writeLines(tmp, con)
        }
	close(con); on.exit()
    } else {
        ## <NOTE>
        ## It may be safer to do
        ##   writeLines(sapply(codeFiles, readLines), outFile)
        ## instead, but this would be much slower ...
        ## use fast version of file.append that ensures LF between files
        if(!all(.file_append_ensuring_LFs(outFile, codeFiles)))
            stop("unable to write code files")
        ## </NOTE>
    }
    ## A syntax check here, so that we do not install a broken package.
    ## FIXME:  this is only needed if we don't lazy load, as the lazy loader
    ## would detect the error.
    op <- options(showErrorCalls=FALSE)
    on.exit(options(op))
    parse(outFile)
    invisible()
}


### * .install_package_indices

.install_package_indices <-
function(dir, outDir)
{
    options(warn = 1)                   # to ensure warnings get seen
    if(!file_test("-d", dir))
        stop(gettextf("directory '%s' does not exist", dir),
             domain = NA)
    if(!file_test("-d", outDir))
        stop(gettextf("directory '%s' does not exist", outDir),
             domain = NA)

    ## If there is an @file{INDEX} file in the package sources, we
    ## install this, and do not build it.
    if(file_test("-f", file.path(dir, "INDEX")))
        if(!file.copy(file.path(dir, "INDEX"),
                      file.path(outDir, "INDEX"),
                      overwrite = TRUE))
            stop(gettextf("unable to copy INDEX to '%s'",
                          file.path(outDir, "INDEX")),
                 domain = NA)

    outMetaDir <- file.path(outDir, "Meta")
    if(!file_test("-d", outMetaDir) && !dir.create(outMetaDir))
         stop(gettextf("cannot open directory '%s'", outMetaDir),
              domain = NA)
    .install_package_Rd_indices(dir, outDir)
    .install_package_vignette_index(dir, outDir)
    .install_package_demo_index(dir, outDir)
    invisible()
}

### * .install_package_Rd_indices

.install_package_Rd_indices <-
function(dir, outDir)
{
    dir <- file_path_as_absolute(dir)
    docsDir <- file.path(dir, "man")
    dataDir <- file.path(outDir, "data")
    outDir <- file_path_as_absolute(outDir)

    ## <FIXME>
    ## Not clear whether we should use the basename of the directory we
    ## install to, or the package name as obtained from the DESCRIPTION
    ## file in the directory we install from (different for versioned
    ## installs).  We definitely do not want the basename of the dir we
    ## install from.
    packageName <- basename(outDir)
    ## </FIXME>

    allRd <- if(file_test("-d", docsDir))
        list_files_with_type(docsDir, "docs") else character()
    ## some people have man dirs without any valid .Rd files
    if(length(allRd)) {
        ## we want the date of the newest .Rd file we will install
        newestRd <- max(file.info(allRd)$mtime)
        ## these files need not exist, which gives NA.
        indices <- c(file.path("Meta", "Rd.rds"),
                     file.path("Meta", "hsearch.rds"),
                     file.path("Meta", "links.rds"),
                     "INDEX")
        upToDate <- file.info(file.path(outDir, indices))$mtime >= newestRd
        if(file_test("-d", dataDir)
           && length(dataFiles <- list.files(dataDir))) {
            ## Note that the data index is computed from both the package's
            ## Rd files and the data sets actually available.
            newestData <- max(file.info(dataFiles)$mtime)
            upToDate <- c(upToDate,
                          file.info(file.path(outDir, "Meta", "data.rds"))$mtime >=
                          max(newestRd, newestData))
        }
        ## Note that this is not quite good enough: an Rd file or data file
        ## might have been removed since the indices were made.
        RdsFile <- file.path("Meta", "Rd.rds")
        if(file.exists(RdsFile)) { ## for Rd files
            ## this has file names without path
            files <- .readRDS(RdsFile)$File
            if(!identical(basename(allRd), files)) upToDate <- FALSE
        }
        ## we want to proceed if any is NA.
        if(all(upToDate %in% TRUE)) return(invisible())

        ## Rd objects should already have been installed.
        db <- tryCatch(Rd_db(basename(outDir), lib.loc = dirname(outDir)),
                       error = function(e) NULL)
        ## If not, we build the Rd db from the sources:
        if(is.null(db)) db <- .build_Rd_db(dir, allRd)
        contents <- Rd_contents(db)

        .write_Rd_contents_as_RDS(contents,
                                  file.path(outDir, "Meta", "Rd.rds"))

        defaultEncoding <- as.vector(.readRDS(file.path(outDir, "Meta", "package.rds"))$DESCRIPTION["Encoding"])
        if(is.na(defaultEncoding)) defaultEncoding <- NULL
        .saveRDS(.build_hsearch_index(contents, packageName, defaultEncoding),
                 file.path(outDir, "Meta", "hsearch.rds"))

        .saveRDS(.build_links_index(contents, packageName),
                 file.path(outDir, "Meta", "links.rds"))

        ## If there is no @file{INDEX} file in the package sources, we
        ## build one.
        ## <NOTE>
        ## We currently do not also save this in RDS format, as we can
        ## always do
        ##   .build_Rd_index(.readRDS(file.path(outDir, "Meta", "Rd.rds"))
        if(!file_test("-f", file.path(dir, "INDEX")))
            writeLines(formatDL(.build_Rd_index(contents)),
                       file.path(outDir, "INDEX"))
        ## </NOTE>
    } else {
        contents <- NULL
        .saveRDS(.build_hsearch_index(contents, packageName, defaultEncoding),
                 file.path(outDir, "Meta", "hsearch.rds"))

        .saveRDS(.build_links_index(contents, packageName),
                 file.path(outDir, "Meta", "links.rds"))

    }
    if(file_test("-d", dataDir))
        .saveRDS(.build_data_index(dataDir, contents),
                 file.path(outDir, "Meta", "data.rds"))
    invisible()
}

### * .install_package_vignette_index

.install_package_vignette_index <-
function(dir, outDir)
{
    dir <- file_path_as_absolute(dir)
    vignetteDir <- file.path(dir, "inst", "doc")
    ## Create a vignette index only if the vignette dir exists.
    if(!file_test("-d", vignetteDir))
        return(invisible())

    outDir <- file_path_as_absolute(outDir)
    ## <FIXME>
    ## Not clear whether we should use the basename of the directory we
    ## install to, or the package name as obtained from the DESCRIPTION
    ## file in the directory we install from (different for versioned
    ## installs).  We definitely do not want the basename of the dir we
    ## install from.
    packageName <- basename(outDir)
    ## </FIXME>
    outVignetteDir <- file.path(outDir, "doc")
    if(!file_test("-d", outVignetteDir) && !dir.create(outVignetteDir))
        stop(gettextf("cannot open directory '%s'", outVignetteDir),
             domain = NA)

    ## If there is an HTML index in the @file{inst/doc} subdirectory of
    ## the package source directory (@code{dir}), we do not overwrite it
    ## (similar to top-level @file{INDEX} files).  Installation already
    ## copies/d this over.
    hasHtmlIndex <- file_test("-f", file.path(vignetteDir, "index.html"))
    htmlIndex <- file.path(outDir, "doc", "index.html")

    ## Write dummy HTML index if no vignettes are found and exit.
    if(!length(list_files_with_type(vignetteDir, "vignette"))) {
        ## we don't want to write an index if the directory is in fact empty
        files <- list.files(vignetteDir, all.files = TRUE) # includes . and ..
        if((length(files) > 2L) && !hasHtmlIndex)
            .writeVignetteHtmlIndex(packageName, htmlIndex)
        return(invisible())
    }

    vignetteIndex <- .build_vignette_index(vignetteDir)
    ## For base package vignettes there is no PDF in @file{vignetteDir}
    ## but there might/should be one in @file{outVignetteDir}.
    if(NROW(vignetteIndex) > 0L) {
        vignettePDFs <-
            sub("$", ".pdf",
                basename(file_path_sans_ext(vignetteIndex$File)))
        ind <- file_test("-f", file.path(outVignetteDir, vignettePDFs))
        vignetteIndex$PDF[ind] <- vignettePDFs[ind]

        ## install tangled versions of all vignettes
        cwd <- getwd()
        setwd(outVignetteDir)
        for(srcfile in vignetteIndex$File)
            tryCatch(utils::Stangle(srcfile, quiet = TRUE),
                     error = function(e)
                     stop(gettextf("running Stangle on vignette '%s' failed with message:\n%s",
                                   srcfile, conditionMessage(e)),
                          domain = NA, call. = FALSE))
        vignetteIndex$R <-
            sub("$", ".R", basename(file_path_sans_ext(vignetteIndex$File)))
        setwd(cwd)
    }

    if(!hasHtmlIndex)
        .writeVignetteHtmlIndex(packageName, htmlIndex, vignetteIndex)

    .saveRDS(vignetteIndex,
             file = file.path(outDir, "Meta", "vignette.rds"))

    invisible()
}

### * .install_package_demo_index

.install_package_demo_index <-
function(dir, outDir)
{
    demoDir <- file.path(dir, "demo")
    if(!file_test("-d", demoDir)) return(invisible())
    demoIndex <- .build_demo_index(demoDir)
    .saveRDS(demoIndex,
             file = file.path(outDir, "Meta", "demo.rds"))
    invisible()
}

### * .vinstall_package_indices

## called from src/library/Makefile
.vinstall_package_indices <-
function(src_dir, out_dir, packages)
{
    ## For the given packages with sources rooted at @file{src_dir} and
    ## installations rooted at @file{out_dir}, install the package
    ## indices.
    ## Really only useful for base packages under Unix.
    ## See @file{src/library/Makefile.in}.
    ## These days this is mostly installing the metadata

    for(p in unlist(strsplit(packages, "[[:space:]]+")))
        .install_package_indices(file.path(src_dir, p),
                                         file.path(out_dir, p))
    unix.packages.html(.Library)
    invisible()
}

### * .install_package_vignettes

## called from src/library/Makefile
## this is only used when building R, to build the 'grid' vignettes.
.install_package_vignettes <-
function(dir, outDir, keep.source = FALSE)
{
    dir <- file_path_as_absolute(dir)
    vignetteDir <- file.path(dir, "inst", "doc")
    if(!file_test("-d", vignetteDir))
        return(invisible())
    vignetteFiles <- list_files_with_type(vignetteDir, "vignette")
    if(!length(vignetteFiles))
        return(invisible())

    outDir <- file_path_as_absolute(outDir)
    outVignetteDir <- file.path(outDir, "doc")
    if(!file_test("-d", outVignetteDir) && !dir.create(outVignetteDir))
        stop(gettextf("cannot open directory '%s'", outVignetteDir),
             domain = NA)
    ## For the time being, assume that no PDFs are available in
    ## vignetteDir.
    vignettePDFs <-
        file.path(outVignetteDir,
                  sub("$", ".pdf",
                      basename(file_path_sans_ext(vignetteFiles))))
    upToDate <- file_test("-nt", vignettePDFs, vignetteFiles)
    if(all(upToDate))
        return(invisible())

    ## For the time being, the primary use of this function is to
    ## build and install vignettes in base packages (which means grid).
    ## Hence, we build in a subdir of the current directory rather than
    ## a temp dir:
    ## this allows inspection of problems and automatic cleanup via Make.
    cwd <- getwd()
    buildDir <- file.path(cwd, ".vignettes")
    if(!file_test("-d", buildDir) && !dir.create(buildDir))
        stop(gettextf("cannot create directory '%s'", buildDir), domain = NA)
    on.exit(setwd(cwd))
    setwd(buildDir)

    for(srcfile in vignetteFiles[!upToDate]) {
        base <- basename(file_path_sans_ext(srcfile))
        message("processing '", basename(srcfile), "'")
        texfile <- paste(base, ".tex", sep = "")
        tryCatch(utils::Sweave(srcfile, pdf = TRUE, eps = FALSE,
                               quiet = TRUE, keep.source = keep.source,
                               stylepath = FALSE),
                 error = function(e)
                 stop(gettextf("running Sweave on vignette '%s' failed with message:\n%s",
                               srcfile, conditionMessage(e)),
                      domain = NA, call. = FALSE))
        ## In case of an error, do not clean up: should we point to
        ## buildDir for possible inspection of results/problems?
        ## We need to ensure that vignetteDir is in TEXINPUTS and BIBINPUTS.
        ## <FIXME>
        ## What if this fails?
        texi2dvi(texfile, pdf = TRUE, quiet = TRUE, texinputs = vignetteDir)
        ## </FIXME>
        pdffile <-
            paste(basename(file_path_sans_ext(srcfile)), ".pdf", sep = "")
        if(!file.exists(pdffile))
            stop(gettextf("file '%s' was not created", pdffile),
                 domain = NA)
        if(!file.copy(pdffile, outVignetteDir, overwrite = TRUE))
            stop(gettextf("cannot copy '%s' to '%s'",
                          pdffile,
                          outVignetteDir),
                 domain = NA)
    }
    ## Need to change out of this dir before we delete it,
    ## at least on Windows.
    setwd(cwd)
    unlink(buildDir, recursive = TRUE)
    ## Now you need to update the HTML index!
    .install_package_vignette_index(dir, outDir)
    invisible()
}

### * .install_package_namespace_info

.install_package_namespace_info <-
function(dir, outDir)
{
    dir <- file_path_as_absolute(dir)
    nsFile <- file.path(dir, "NAMESPACE")
    if(!file_test("-f", nsFile)) return(invisible())
    nsInfoFilePath <- file.path(outDir, "Meta", "nsInfo.rds")
    if(file_test("-nt", nsInfoFilePath, nsFile)) return(invisible())
    nsInfo <- parseNamespaceFile(basename(dir), dirname(dir))
    outMetaDir <- file.path(outDir, "Meta")
    if(!file_test("-d", outMetaDir) && !dir.create(outMetaDir))
        stop(gettextf("cannot open directory '%s'", outMetaDir),
             domain = NA)
    .saveRDS(nsInfo, nsInfoFilePath)
    invisible()
}

### * .vinstall_package_namespaces_as_RDS

## called from src/library/Makefile
.vinstall_package_namespaces_as_RDS <-
function(dir, packages)
{
    ## For the given packages installed in @file{dir} which have a
    ## NAMESPACE file, install the namespace info as R metadata.
    ## Really only useful for base packages under Unix.
    ## See @file{src/library/Makefile.in}.

    for(p in unlist(strsplit(packages, "[[:space:]]+")))
        .install_package_namespace_info(file.path(dir, p),
                                        file.path(dir, p))
    invisible()
}

### * .install_package_Rd_objects

## called from src/library/Makefile
.install_package_Rd_objects <-
function(dir, outDir, encoding = "unknown")
{
    mandir <- file.path(dir, "man")
    manfiles <- if(!file_test("-d", mandir)) character()
    else list_files_with_type(mandir, "docs")
    manOutDir <- file.path(outDir, "help")
    dir.create(manOutDir, FALSE)
    db_file <- file.path(manOutDir,
                         paste(basename(outDir), ".rdx", sep = ""))
    ## Avoid (costly) rebuilding if not needed.
    ## Actually, it seems no more costly than these tests, which it also does
    pathsFile <- file.path(manOutDir, "paths.rds")
    if(!file_test("-f", db_file) || !file.exists(pathsFile) ||
       !identical(sort(manfiles), sort(.readRDS(pathsFile))) ||
       !all(file_test("-nt", db_file, manfiles))) {
        db <- .build_Rd_db(dir, manfiles, db_file = db_file,
                           encoding = encoding)
        nm <- names(db)
        .saveRDS(nm, pathsFile)
        names(db) <- sub("\\.[Rr]d$", "", basename(nm))
        makeLazyLoadDB(db, file.path(manOutDir, basename(outDir)))
    }
    invisible()
}

### * .install_package_demos

## called from basepkg.mk and .install_packages
.install_package_demos <-
function(dir, outDir)
{
    ## NB: we no longer install 00Index
    demodir <- file.path(dir, "demo")
    if(!file_test("-d", demodir)) return()
    demofiles <- list_files_with_type(demodir, "demo", full.names = FALSE)
    if(!length(demofiles)) return()
    demoOutDir <- file.path(outDir, "demo")
    if(!file_test("-d", demoOutDir)) dir.create(demoOutDir)
    file.copy(file.path(demodir, demofiles), demoOutDir,
              overwrite = TRUE)
}


### * .find_cinclude_paths

.find_cinclude_paths <-
function(pkgs, lib.loc = NULL, file = NULL)
{
    ## given a character string of comma-separated package names,
    ## find where the packages are installed and generate
    ## -I"/path/to/package/include" ...

    if(!is.null(file)) {
        tmp <- read.dcf(file, "LinkingTo")[1L, 1L]
        if(is.na(tmp)) return(invisible())
        pkgs <- tmp
    }
    pkgs <- strsplit(pkgs[1L], ",[[:blank:]]*")[[1L]]
    paths <- .find.package(pkgs, lib.loc, quiet=TRUE)
    if(length(paths))
        cat(paste(paste('-I"', paths, '/include"', sep=""), collapse=" "))
    return(invisible())
}

### * .vcreate_bundle_package_descriptions

## called from .install_packages
.vcreate_bundle_package_descriptions <-
function(dir, packages)
{
    .canonicalize_metadata <- function(m) {
        ## Drop entries which are NA or empty.
        m[!is.na(m) & (regexpr("^[[:space:]]*$", m) < 0L)]
    }

    dir <- file_path_as_absolute(dir)

    ## Bundle level metadata.
    meta <- .read_description(file.path(dir, "DESCRIPTION"))
    meta <- .canonicalize_metadata(meta)
    if(missing(packages)) packages <- meta[["Contains"]]

    for(p in unlist(strsplit(.strip_whitespace(packages), "[[:space:]]+"))) {
        bmeta <- meta
        ## Package metadata.
        this <- file.path(dir, p, "DESCRIPTION.in")
        if(file_test("-f", this)) {
            pmeta <- .read_description(this)
            pmeta <- .canonicalize_metadata(pmeta)
            ## Need to merge dependency fields in *both* metadata.
            fields_to_merge <-
                c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
            fields <- intersect(intersect(names(bmeta), fields_to_merge),
                                intersect(names(pmeta), fields_to_merge))
            if(length(fields)) {
                bmeta[fields] <-
                    paste(bmeta[fields], pmeta[fields], sep = ", ")
                pmeta <- pmeta[!(names(pmeta) %in% fields)]
            }
        } else {
            warning(gettextf("missing 'DESCRIPTION.in' for package '%s'", p),
                    domain = NA)
            d <- sprintf("Package '%s' from bundle '%s'", p, meta[["Bundle"]])
            pmeta <- c(p, d, d)
            names(pmeta) <- c("Package", "Description", "Title")
        }
        write.dcf(rbind(c(bmeta, pmeta)),
                  file.path(dir, p, "DESCRIPTION"))
    }

    invisible()
}

### * .test_package_depends_R_version

.Rtest_package_depends_R_version <-
function(dir)
{
    if(missing(dir)) dir <- "."
    meta <- .read_description(file.path(dir, "DESCRIPTION"))
    deps <- .split_description(meta, verbose = TRUE)$Rdepends2
    status <- 0
    current <- getRversion()
    for(depends in deps) {
        ## .split_description will have ensured that this is NULL or
        ## of length 3.
        if(length(depends) > 1L) {
            ## .check_package_description will insist on these operators
            if(!depends$op %in% c("<=", ">=", "<", ">", "==", "!="))
                message("WARNING: malformed 'Depends' field in 'DESCRIPTION'")
            else
                status <- !do.call(depends$op,
                                   list(current, depends$version))
            if(status != 0) {
                package <- Sys.getenv("R_PACKAGE_NAME")
                if(!nzchar(package))
                    package <- meta["Package"]
                if(nzchar(package))
                    msg <- gettextf("ERROR: this R is version %s, package '%s' requires R %s %s",
                                    current, package,
                                    depends$op, depends$version)
                else if (nzchar(bundle <-  meta["Bundle"]) && !is.na(bundle))
                    msg <- gettextf("ERROR: this R is version %s, bundle '%s' requires R %s %s",
                                    current, bundle,
                                    depends$op, depends$version)
                else
                    msg <- gettextf("ERROR: this R is version %s, required is R %s %s",
                                    current, depends$op, depends$version)
                message(strwrap(msg, exdent = 2L))
                break
            }
        }
    }
    status
}

## no longer used
.test_package_depends_R_version <-
function(dir)
    q(status = .Rtest_package_depends_R_version(dir))


### * checkRdaFiles

checkRdaFiles <- function(paths)
{
    if(length(paths) == 1L && isTRUE(file.info(paths)$isdir))
        paths <- Sys.glob(c(file.path(paths, "*.rda"),
                            file.path(paths, "*.RData")))
    res <- data.frame(size = NA_real_, ASCII = NA,
                      compress = NA_character_, version = NA_integer_,
                      stringsAsFactors = FALSE)
    res <- res[rep(1L, length(paths)), ]
    row.names(res) <- paths
    keep <- file.exists(paths)
    res$size[keep] <- file.info(paths)$size[keep]
    for(p in paths[keep]) {
        magic <- readBin(p, "raw", n = 5)
        res[p, "compress"] <- if(all(magic[1:2] == c(0x1f, 0x8b))) "gzip"
        else if(rawToChar(magic[1:3]) == "BZh") "bzip2"
        else if(magic[1] == 0xFD && rawToChar(magic[2:5]) == "7zXZ") "xz"
        else if(grepl("RD[ABX][12]", rawToChar(magic), useBytes = TRUE)) "none"
        else "unknown"
        con <- gzfile(p)
        magic <- readChar(con, 5L, useBytes = TRUE)
        close(con)
        res[p, "ASCII"]  <- if (grepl("RD[ABX][12]", magic, useBytes = TRUE))
            substr(magic, 3, 3) == "A" else NA
        ver <- sub("(RD[ABX])([12]*)", "\\2", magic, useBytes = TRUE)
        res$version <- as.integer(ver)
    }
    res
}

### * resaveRdaFiles

resaveRdaFiles <- function(paths,
                           compress = c("auto", "gzip", "bzip2", "xz"),
                           compression_level)
{
    if(length(paths) == 1L && isTRUE(file.info(paths)$isdir))
        paths <- Sys.glob(c(file.path(paths, "*.rda"),
                            file.path(paths, "*.RData")))
    compress <- match.arg(compress)
    if (missing(compression_level))
        compression_level <- switch(compress, "gzip" = 6, 9)
    for(p in paths) {
        env <- new.env()
        load(p, envir = env)
        if(compress == "auto") {
            f1 <- tempfile()
            save(file = f1, list = ls(env, all=TRUE), envir = env)
            f2 <- tempfile()
            save(file = f2, list = ls(env, all=TRUE), envir = env,
                 compress = "bzip2")
            ss <- file.info(c(f1, f2))$size * c(0.9, 1.0)
            names(ss) <- c(f1, f2)
            if(ss[1L] > 10240) {
                f3 <- tempfile()
                save(file = f3, list = ls(env, all=TRUE), envir = env,
                     compress = "xz")
                ss <- c(ss, file.info(f3)$size)
		names(ss) <- c(f1, f2, f3)
            }
            nm <- names(ss)
            ind <- which.min(ss)
            file.copy(nm[ind], p, overwrite = TRUE)
            unlink(nm)
        } else
            save(file = p, list = ls(env, all=TRUE), envir = env,
                 compress = compress, compression_level = compression_level)
    }
}

### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
## Tools for computing on CITATION info.
## Currently only for validation, and hence not in utils.

BibTeX_entry_field_db <-
    list(Article = c("author", "title", "journal", "year"),
         Book = c("author|editor", "title", "publisher", "year"),
         Booklet = c("title"),
         InBook =
         c("author|editor", "title", "chapter", "publisher", "year"),
         InCollection =
         c("author", "title", "booktitle", "publisher", "year"),
         InProceedings = c("author", "title", "booktitle", "year"),
         Manual = c("title"),
         MastersThesis = c("author", "title", "school", "year"),
         Misc = character(),
         PhdThesis = c("author", "title", "school", "year"),
         Proceedings = c("title", "year"),
         TechReport = c("author", "title", "institution", "year"),
         Unpublished = c("author", "title", "note")
         )
## See e.g. lisp/textmodes/bibtex.el in the GNU Emacs sources.

get_CITATION_entry_fields <-
function(file, encoding = "unknown")
{
    ## Assume that citEntry() only occurs at top level.

    ## The usual encoding nightmare.
    ## readCitationFile() uses the package encoding, or if this is
    ## unset, latin1.  Let's simply try latin1 and UTF-8 if simple
    ## parsing fails in case there is no given encoding.

    exprs <- tryCatch(parse(file(file, encoding = encoding)),
                      error = identity)
    if(inherits(exprs, "error")) {
        if(encoding != "unknown")
            return()
        exprs <- tryCatch(parse(file(file, encoding = "latin1")),
                          error = identity)
        if(inherits(exprs, "error"))
            exprs <- tryCatch(parse(file(file, encoding = "UTF-8")),
                          error = identity)
        if(inherits(exprs, "error"))
            return()
    }

    ## Argh.  citEntry() has formals
    ##   (entry, textVersion, header = NULL, footer = NULL, ...)
    ## so we cannot simply compute of the names of the citEntry() calls.
    FOO <- function(entry, textVersion, header = NULL, footer = NULL, ...)
        match.call()

    out <- lapply(exprs,
           function(e) {
               if(as.character(e[[1L]]) != "citEntry") return()
               e[[1L]] <- as.name("FOO")
               e <- as.list(eval(e))
               entry <- e$entry
               entry <- if(!is.character(entry)) NA_character_ else entry[1L]
               fields <- names(e)
               ## Retain fields textVersion/header/footer, so these must
               ## be removed in subsequent BibTeX validation computations.
               fields <- fields[is.na(match(fields, c("", "entry")))]
               list(entry = entry, fields = fields)
           })

    out <- Filter(Negate(is.null), out)
    entries <- sapply(out, `[[`, 1L)
    fields <- lapply(out, `[[`, 2L)
    out <- data.frame(File = file,
                      Entry = entries,
                      stringsAsFactors = FALSE)
    out$Fields <- fields
    out
}


find_missing_required_BibTeX_fields <-
function(entry, fields)
{
    if(!length(fields)) return(character())
    pos <- match(tolower(entry),
                 tolower(names(BibTeX_entry_field_db)))
    if(is.na(pos)) {
        ## Invalid entry.
        return(NA_character_)
    }
    rfields <- BibTeX_entry_field_db[[pos]]
    if(!length(rfields)) return(character())
    ## Drop non-BibTeX citEntry() fields.
    fields <- tolower(fields[!fields %in%
                             c("textVersion", "header", "footer")])
    ## Go for legibility/generality rather than efficiency.
    ok <- sapply(strsplit(rfields, "|", fixed = TRUE),
                 function(f) any(f %in% fields))
    rfields[!ok]
}
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
#  File src/library/tools/R/encodings.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

get_IANA_character_sets <-
function(file = NULL)
{
    ## Master URI is
    ##   http://www.iana.org/assignments/character-sets
    if(is.null(file))
        file <- file.path(R.home("share"), "encodings",
                          "character-sets")
    lines <- readLines(file)
    ## Start with first Name: entry, and end with REFERENCES.
    spos <- min(grep("^Name:", lines))
    epos <- min(grep("^REFERENCES", lines)) - 1
    lines <- lines[spos : epos]
    ## Omit 'Alias: None' and similar lines.
    if(any(ind <- grep("^[[:alnum:]]+:[[:space:]]+None[[:space:]]*$",
                       lines)))
        lines <- lines[-ind]
    ## And be nice (version last updated 2007-05-14 was invalid DCF).
    if(any(ind <- grep("^[^[:blank:]][^:]*$", lines)))
        lines <- lines[-ind]
    entries <- paste(lines, collapse = "\n")
    ## What we now have is in DCF format, with multiple fields.
    con <- textConnection(entries)
    on.exit(close(con))
    out <- read.dcf(con,
                    fields = c("Name", "MIBenum", "Source", "Alias"),
                    all = TRUE)
    ## Prefer 'Aliases' for historical reasons.
    names(out)[names(out) == "Alias"] <- "Aliases"
    ## Preferred MIME names.
    MIME <- sapply(mapply("c", out$Name, out$Aliases),
                   function(u) {
                       if(any(ind <- grep("preferred MIME name", u)))
                           sapply(strsplit(u[ind], " +"), "[[", 1L)
                       else
                           character()
                   })
    out$MIME <- MIME
    out$Name <- sub(" +.*", "", out$Name)
    out$Aliases <- lapply(out$Aliases, function(s) sub(" +.*", "", s))
    out$MIBenum <- as.integer(out$MIBenum)

    out
}

charset_to_Unicode <- local({
    ISOLatin1 <- c(0:127, rep.int(0, 32), 160:255)
    ISOLatin2 <- c(0:127, rep.int(0, 32),
  0x00a0, 0x0104, 0x02d8, 0x0141, 0x00a4, 0x013d, 0x015a, 0x00a7,
  0x00a8, 0x0160, 0x015e, 0x0164, 0x0179, 0x00ad, 0x017d, 0x017b,
  0x00b0, 0x0105, 0x02db, 0x0142, 0x00b4, 0x013e, 0x015b, 0x02c7,
  0x00b8, 0x0161, 0x015f, 0x0165, 0x017a, 0x02dd, 0x017e, 0x017c,
  0x0154, 0x00c1, 0x00c2, 0x0102, 0x00c4, 0x0139, 0x0106, 0x00c7,
  0x010c, 0x00c9, 0x0118, 0x00cb, 0x011a, 0x00cd, 0x00ce, 0x010e,
  0x0110, 0x0143, 0x0147, 0x00d3, 0x00d4, 0x0150, 0x00d6, 0x00d7,
  0x0158, 0x016e, 0x00da, 0x0170, 0x00dc, 0x00dd, 0x0162, 0x00df,
  0x0155, 0x00e1, 0x00e2, 0x0103, 0x00e4, 0x013a, 0x0107, 0x00e7,
  0x010d, 0x00e9, 0x0119, 0x00eb, 0x011b, 0x00ed, 0x00ee, 0x010f,
  0x0111, 0x0144, 0x0148, 0x00f3, 0x00f4, 0x0151, 0x00f6, 0x00f7,
  0x0159, 0x016f, 0x00fa, 0x0171, 0x00fc, 0x00fd, 0x0163, 0x02d9)
    ISOLatin7 <- c(0:127, rep.int(0, 32),
  0x00a0, 0x201d, 0x00a2, 0x00a3, 0x00a4, 0x201e, 0x00a6, 0x00a7,
  0x00d8, 0x00a9, 0x0156, 0x00ab, 0x00ac, 0x00ad, 0x00ae, 0x00c6,
  0x00b0, 0x00b1, 0x00b2, 0x00b3, 0x201c, 0x00b5, 0x00b6, 0x00b7,
  0x00f8, 0x00b9, 0x0157, 0x00bb, 0x00bc, 0x00bd, 0x00be, 0x00e6,
  0x0104, 0x012e, 0x0100, 0x0106, 0x00c4, 0x00c5, 0x0118, 0x0112,
  0x010c, 0x00c9, 0x0179, 0x0116, 0x0122, 0x0136, 0x012a, 0x013b,
  0x0160, 0x0143, 0x0145, 0x00d3, 0x014c, 0x00d5, 0x00d6, 0x00d7,
  0x0172, 0x0141, 0x015a, 0x016a, 0x00dc, 0x017b, 0x017d, 0x00df,
  0x0105, 0x012f, 0x0101, 0x0107, 0x00e4, 0x00e5, 0x0119, 0x0113,
  0x010d, 0x00e9, 0x017a, 0x0117, 0x0123, 0x0137, 0x012b, 0x013c,
  0x0161, 0x0144, 0x0146, 0x00f3, 0x014d, 0x00f5, 0x00f6, 0x00f7,
  0x0173, 0x0142, 0x015b, 0x016b, 0x00fc, 0x017c, 0x017e, 0x2019)
    ISOLatin9 <- c(0:127, rep.int(0, 32),
  0x00a0, 0x00a1, 0x00a2, 0x00a3, 0x20ac, 0x00a5, 0x0160, 0x00a7,
  0x0161, 0x00a9, 0x00aa, 0x00ab, 0x00ac, 0x00ad, 0x00ae, 0x00af,
  0x00b0, 0x00b1, 0x00b2, 0x00b3, 0x017d, 0x00b5, 0x00b6, 0x00b7,
  0x017e, 0x00b9, 0x00ba, 0x00bb, 0x0152, 0x0153, 0x0178, 0x00bf,
                   192:255)
    Cyrillic <- c(0:127, rep.int(0, 32),
  0x00a0, 0x0401, 0x0402, 0x0403, 0x0404, 0x0405, 0x0406, 0x0407,
  0x0408, 0x0409, 0x040a, 0x040b, 0x040c, 0x00ad, 0x040e, 0x040f,
  0x0410, 0x0411, 0x0412, 0x0413, 0x0414, 0x0415, 0x0416, 0x0417,
  0x0418, 0x0419, 0x041a, 0x041b, 0x041c, 0x041d, 0x041e, 0x041f,
  0x0420, 0x0421, 0x0422, 0x0423, 0x0424, 0x0425, 0x0426, 0x0427,
  0x0428, 0x0429, 0x042a, 0x042b, 0x042c, 0x042d, 0x042e, 0x042f,
  0x0430, 0x0431, 0x0432, 0x0433, 0x0434, 0x0435, 0x0436, 0x0437,
  0x0438, 0x0439, 0x043a, 0x043b, 0x043c, 0x043d, 0x043e, 0x043f,
  0x0440, 0x0441, 0x0442, 0x0443, 0x0444, 0x0445, 0x0446, 0x0447,
  0x0448, 0x0449, 0x044a, 0x044b, 0x044c, 0x044d, 0x044e, 0x044f,
  0x2116, 0x0451, 0x0452, 0x0453, 0x0454, 0x0455, 0x0456, 0x0457,
  0x0458, 0x0459, 0x045a, 0x045b, 0x045c, 0x00a7, 0x045e, 0x045f)
    Greek <- c(0:127, rep.int(0, 32),
  0x00a0, 0x2018, 0x2019, 0x00a3, 0x20ac, 0x20af, 0x00a6, 0x00a7,
  0x00a8, 0x00a9, 0x037a, 0x00ab, 0x00ac, 0x00ad, 0xfffd, 0x2015,
  0x00b0, 0x00b1, 0x00b2, 0x00b3, 0x0384, 0x0385, 0x0386, 0x00b7,
  0x0388, 0x0389, 0x038a, 0x00bb, 0x038c, 0x00bd, 0x038e, 0x038f,
  0x0390, 0x0391, 0x0392, 0x0393, 0x0394, 0x0395, 0x0396, 0x0397,
  0x0398, 0x0399, 0x039a, 0x039b, 0x039c, 0x039d, 0x039e, 0x039f,
  0x03a0, 0x03a1, 0xfffd, 0x03a3, 0x03a4, 0x03a5, 0x03a6, 0x03a7,
  0x03a8, 0x03a9, 0x03aa, 0x03ab, 0x03ac, 0x03ad, 0x03ae, 0x03af,
  0x03b0, 0x03b1, 0x03b2, 0x03b3, 0x03b4, 0x03b5, 0x03b6, 0x03b7,
  0x03b8, 0x03b9, 0x03ba, 0x03bb, 0x03bc, 0x03bd, 0x03be, 0x03bf,
  0x03c0, 0x03c1, 0x03c2, 0x03c3, 0x03c4, 0x03c5, 0x03c6, 0x03c7,
  0x03c8, 0x03c9, 0x03ca, 0x03cb, 0x03cc, 0x03cd, 0x03ce, 0xfffd)
    KOI8R <- c(0:127,
  0x2500, 0x2502, 0x250c, 0x2510, 0x2514, 0x2518, 0x251c, 0x2524,
  0x252c, 0x2534, 0x253c, 0x2580, 0x2584, 0x2588, 0x258c, 0x2590,
  0x2591, 0x2592, 0x2593, 0x2320, 0x25a0, 0x2219, 0x221a, 0x2248,
  0x2264, 0x2265, 0x00a0, 0x2321, 0x00b0, 0x00b2, 0x00b7, 0x00f7,
  0x2550, 0x2551, 0x2552, 0x0451, 0x2553, 0x2554, 0x2555, 0x2556,
  0x2557, 0x2558, 0x2559, 0x255a, 0x255b, 0x255c, 0x255d, 0x255e,
  0x255f, 0x2560, 0x2561, 0x0401, 0x2562, 0x2563, 0x2564, 0x2565,
  0x2566, 0x2567, 0x2568, 0x2569, 0x256a, 0x256b, 0x256c, 0x00a9,
  0x044e, 0x0430, 0x0431, 0x0446, 0x0434, 0x0435, 0x0444, 0x0433,
  0x0445, 0x0438, 0x0439, 0x043a, 0x043b, 0x043c, 0x043d, 0x043e,
  0x043f, 0x044f, 0x0440, 0x0441, 0x0442, 0x0443, 0x0436, 0x0432,
  0x044c, 0x044b, 0x0437, 0x0448, 0x044d, 0x0449, 0x0447, 0x044a,
  0x042e, 0x0410, 0x0411, 0x0426, 0x0414, 0x0415, 0x0424, 0x0413,
  0x0425, 0x0418, 0x0419, 0x041a, 0x041b, 0x041c, 0x041d, 0x041e,
  0x041f, 0x042f, 0x0420, 0x0421, 0x0422, 0x0423, 0x0416, 0x0412,
  0x042c, 0x042b, 0x0417, 0x0428, 0x042d, 0x0429, 0x0427, 0x042a)
    KOI8U <- c(0:127,
  0x2500, 0x2502, 0x250c, 0x2510, 0x2514, 0x2518, 0x251c, 0x2524,
  0x252c, 0x2534, 0x253c, 0x2580, 0x2584, 0x2588, 0x258c, 0x2590,
  0x2591, 0x2592, 0x2593, 0x2320, 0x25a0, 0x2219, 0x221a, 0x2248,
  0x2264, 0x2265, 0x00a0, 0x2321, 0x00b0, 0x00b2, 0x00b7, 0x00f7,
  0x2550, 0x2551, 0x2552, 0x0451, 0x0454, 0x2554, 0x0456, 0x0457,
  0x2557, 0x2558, 0x2559, 0x255a, 0x255b, 0x0491, 0x255d, 0x255e,
  0x255f, 0x2560, 0x2561, 0x0401, 0x0404, 0x2563, 0x0406, 0x0407,
  0x2566, 0x2567, 0x2568, 0x2569, 0x256a, 0x0490, 0x256c, 0x00a9,
  0x044e, 0x0430, 0x0431, 0x0446, 0x0434, 0x0435, 0x0444, 0x0433,
  0x0445, 0x0438, 0x0439, 0x043a, 0x043b, 0x043c, 0x043d, 0x043e,
  0x043f, 0x044f, 0x0440, 0x0441, 0x0442, 0x0443, 0x0436, 0x0432,
  0x044c, 0x044b, 0x0437, 0x0448, 0x044d, 0x0449, 0x0447, 0x044a,
  0x042e, 0x0410, 0x0411, 0x0426, 0x0414, 0x0415, 0x0424, 0x0413,
  0x0425, 0x0418, 0x0419, 0x041a, 0x041b, 0x041c, 0x041d, 0x041e,
  0x041f, 0x042f, 0x0420, 0x0421, 0x0422, 0x0423, 0x0416, 0x0412,
  0x042c, 0x042b, 0x0417, 0x0428, 0x042d, 0x0429, 0x0427, 0x042a)
    CP1250 <- c(0:127,
  0x20ac, 0xfffd, 0x201a, 0xfffd, 0x201e, 0x2026, 0x2020, 0x2021,
  0xfffd, 0x2030, 0x0160, 0x2039, 0x015a, 0x0164, 0x017d, 0x0179,
  0xfffd, 0x2018, 0x2019, 0x201c, 0x201d, 0x2022, 0x2013, 0x2014,
  0xfffd, 0x2122, 0x0161, 0x203a, 0x015b, 0x0165, 0x017e, 0x017a,
  0x00a0, 0x02c7, 0x02d8, 0x0141, 0x00a4, 0x0104, 0x00a6, 0x00a7,
  0x00a8, 0x00a9, 0x015e, 0x00ab, 0x00ac, 0x00ad, 0x00ae, 0x017b,
  0x00b0, 0x00b1, 0x02db, 0x0142, 0x00b4, 0x00b5, 0x00b6, 0x00b7,
  0x00b8, 0x0105, 0x015f, 0x00bb, 0x013d, 0x02dd, 0x013e, 0x017c,
  0x0154, 0x00c1, 0x00c2, 0x0102, 0x00c4, 0x0139, 0x0106, 0x00c7,
  0x010c, 0x00c9, 0x0118, 0x00cb, 0x011a, 0x00cd, 0x00ce, 0x010e,
  0x0110, 0x0143, 0x0147, 0x00d3, 0x00d4, 0x0150, 0x00d6, 0x00d7,
  0x0158, 0x016e, 0x00da, 0x0170, 0x00dc, 0x00dd, 0x0162, 0x00df,
  0x0155, 0x00e1, 0x00e2, 0x0103, 0x00e4, 0x013a, 0x0107, 0x00e7,
  0x010d, 0x00e9, 0x0119, 0x00eb, 0x011b, 0x00ed, 0x00ee, 0x010f,
  0x0111, 0x0144, 0x0148, 0x00f3, 0x00f4, 0x0151, 0x00f6, 0x00f7,
  0x0159, 0x016f, 0x00fa, 0x0171, 0x00fc, 0x00fd, 0x0163, 0x02d9)
    CP1251 <- c(0:127,
  0x0402, 0x0403, 0x201a, 0x0453, 0x201e, 0x2026, 0x2020, 0x2021,
  0x20ac, 0x2030, 0x0409, 0x2039, 0x040a, 0x040c, 0x040b, 0x040f,
  0x0452, 0x2018, 0x2019, 0x201c, 0x201d, 0x2022, 0x2013, 0x2014,
  0xfffd, 0x2122, 0x0459, 0x203a, 0x045a, 0x045c, 0x045b, 0x045f,
  0x00a0, 0x040e, 0x045e, 0x0408, 0x00a4, 0x0490, 0x00a6, 0x00a7,
  0x0401, 0x00a9, 0x0404, 0x00ab, 0x00ac, 0x00ad, 0x00ae, 0x0407,
  0x00b0, 0x00b1, 0x0406, 0x0456, 0x0491, 0x00b5, 0x00b6, 0x00b7,
  0x0451, 0x2116, 0x0454, 0x00bb, 0x0458, 0x0405, 0x0455, 0x0457,
  0x0410, 0x0411, 0x0412, 0x0413, 0x0414, 0x0415, 0x0416, 0x0417,
  0x0418, 0x0419, 0x041a, 0x041b, 0x041c, 0x041d, 0x041e, 0x041f,
  0x0420, 0x0421, 0x0422, 0x0423, 0x0424, 0x0425, 0x0426, 0x0427,
  0x0428, 0x0429, 0x042a, 0x042b, 0x042c, 0x042d, 0x042e, 0x042f,
  0x0430, 0x0431, 0x0432, 0x0433, 0x0434, 0x0435, 0x0436, 0x0437,
  0x0438, 0x0439, 0x043a, 0x043b, 0x043c, 0x043d, 0x043e, 0x043f,
  0x0440, 0x0441, 0x0442, 0x0443, 0x0444, 0x0445, 0x0446, 0x0447,
  0x0448, 0x0449, 0x044a, 0x044b, 0x044c, 0x044d, 0x044e, 0x044f)
    CP1252 <- c(0:127,
  0x20ac, 0xfffd, 0x201a, 0x0192, 0x201e, 0x2026, 0x2020, 0x2021,
  0x02c6, 0x2030, 0x0160, 0x2039, 0x0152, 0xfffd, 0x017d, 0xfffd,
  0xfffd, 0x2018, 0x2019, 0x201c, 0x201d, 0x2022, 0x2013, 0x2014,
  0x02dc, 0x2122, 0x0161, 0x203a, 0x0153, 0xfffd, 0x017e, 0x0178,
                160:255)
    CP1253 <- c(0:127,
  0x20ac, 0xfffd, 0x201a, 0x0192, 0x201e, 0x2026, 0x2020, 0x2021,
  0xfffd, 0x2030, 0xfffd, 0x2039, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
  0xfffd, 0x2018, 0x2019, 0x201c, 0x201d, 0x2022, 0x2013, 0x2014,
  0xfffd, 0x2122, 0xfffd, 0x203a, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
  0x00a0, 0x0385, 0x0386, 0x00a3, 0x00a4, 0x00a5, 0x00a6, 0x00a7,
  0x00a8, 0x00a9, 0xfffd, 0x00ab, 0x00ac, 0x00ad, 0x00ae, 0x2015,
  0x00b0, 0x00b1, 0x00b2, 0x00b3, 0x0384, 0x00b5, 0x00b6, 0x00b7,
  0x0388, 0x0389, 0x038a, 0x00bb, 0x038c, 0x00bd, 0x038e, 0x038f,
  0x0390, 0x0391, 0x0392, 0x0393, 0x0394, 0x0395, 0x0396, 0x0397,
  0x0398, 0x0399, 0x039a, 0x039b, 0x039c, 0x039d, 0x039e, 0x039f,
  0x03a0, 0x03a1, 0xfffd, 0x03a3, 0x03a4, 0x03a5, 0x03a6, 0x03a7,
  0x03a8, 0x03a9, 0x03aa, 0x03ab, 0x03ac, 0x03ad, 0x03ae, 0x03af,
  0x03b0, 0x03b1, 0x03b2, 0x03b3, 0x03b4, 0x03b5, 0x03b6, 0x03b7,
  0x03b8, 0x03b9, 0x03ba, 0x03bb, 0x03bc, 0x03bd, 0x03be, 0x03bf,
  0x03c0, 0x03c1, 0x03c2, 0x03c3, 0x03c4, 0x03c5, 0x03c6, 0x03c7,
  0x03c8, 0x03c9, 0x03ca, 0x03cb, 0x03cc, 0x03cd, 0x03ce, 0xfffd)
    CP1257 <-c(0:127,
  0x20ac, 0xfffd, 0x201a, 0xfffd, 0x201e, 0x2026, 0x2020, 0x2021,
  0xfffd, 0x2030, 0xfffd, 0x2039, 0xfffd, 0x00a8, 0x02c7, 0x00b8,
  0xfffd, 0x2018, 0x2019, 0x201c, 0x201d, 0x2022, 0x2013, 0x2014,
  0xfffd, 0x2122, 0xfffd, 0x203a, 0xfffd, 0x00af, 0x02db, 0xfffd,
  0x00a0, 0xfffd, 0x00a2, 0x00a3, 0x00a4, 0xfffd, 0x00a6, 0x00a7,
  0x00d8, 0x00a9, 0x0156, 0x00ab, 0x00ac, 0x00ad, 0x00ae, 0x00c6,
  0x00b0, 0x00b1, 0x00b2, 0x00b3, 0x00b4, 0x00b5, 0x00b6, 0x00b7,
  0x00f8, 0x00b9, 0x0157, 0x00bb, 0x00bc, 0x00bd, 0x00be, 0x00e6,
  0x0104, 0x012e, 0x0100, 0x0106, 0x00c4, 0x00c5, 0x0118, 0x0112,
  0x010c, 0x00c9, 0x0179, 0x0116, 0x0122, 0x0136, 0x012a, 0x013b,
  0x0160, 0x0143, 0x0145, 0x00d3, 0x014c, 0x00d5, 0x00d6, 0x00d7,
  0x0172, 0x0141, 0x015a, 0x016a, 0x00dc, 0x017b, 0x017d, 0x00df,
  0x0105, 0x012f, 0x0101, 0x0107, 0x00e4, 0x00e5, 0x0119, 0x0113,
  0x010d, 0x00e9, 0x017a, 0x0117, 0x0123, 0x0137, 0x012b, 0x013c,
  0x0161, 0x0144, 0x0146, 0x00f3, 0x014d, 0x00f5, 0x00f6, 0x00f7,
  0x0173, 0x0142, 0x015b, 0x016b, 0x00fc, 0x017c, 0x017e, 0x02d9)
    AdobeSymbol <- c(0:31,
       0x0020, 0x0021, 0x2200, 0x0023, 0x2203, 0x0025, 0x0026, 0x220D,
       0x0028, 0x0029, 0x2217, 0x002B, 0x002C, 0x2212, 0x002E, 0x002F,
       0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037,
       0x0038, 0x0039, 0x003A, 0x003B, 0x003C, 0x003D, 0x003E, 0x003F,
       0x2245, 0x0391, 0x0392, 0x03A7, 0x0394, 0x0395, 0x03A6, 0x0393,
       0x0397, 0x0399, 0x03D1, 0x039A, 0x039B, 0x039C, 0x039D, 0x039F,
       0x03A0, 0x0398, 0x03A1, 0x03A3, 0x03A4, 0x03A5, 0x03C2, 0x03A9,
       0x039E, 0x03A8, 0x0396, 0x005B, 0x2234, 0x005D, 0x22A5, 0x005F,
       0xF8E5, 0x03B1, 0x03B2, 0x03C7, 0x03B4, 0x03B5, 0x03C6, 0x03B3,
       0x03B7, 0x03B9, 0x03D5, 0x03BA, 0x03BB, 0x03BC, 0x03BD, 0x03BF,
       0x03C0, 0x03B8, 0x03C1, 0x03C3, 0x03C4, 0x03C5, 0x03D6, 0x03C9,
       0x03BE, 0x03C8, 0x03B6, 0x007B, 0x007C, 0x007D, 0x223C, rep.int(0, 33),
       0x20AC, 0x03D2, 0x2032, 0x2264, 0x2044, 0x221E, 0x0192, 0x2663,
       0x2666, 0x2665, 0x2660, 0x2194, 0x2190, 0x2191, 0x2192, 0x2193,
       0x00B0, 0x00B1, 0x2033, 0x2265, 0x00D7, 0x221D, 0x2202, 0x2022,
       0x00F7, 0x2260, 0x2261, 0x2248, 0x2026, 0xF8E6, 0xF8E7, 0x21B5,
       0x2135, 0x2111, 0x211C, 0x2118, 0x2297, 0x2295, 0x2205, 0x2229,
       0x222A, 0x2283, 0x2287, 0x2284, 0x2282, 0x2286, 0x2208, 0x2209,
       0x2220, 0x2207, 0xF6DA, 0xF6D9, 0xF6DB, 0x220F, 0x221A, 0x22C5,
       0x00AC, 0x2227, 0x2228, 0x21D4, 0x21D0, 0x21D1, 0x21D2, 0x21D3,
       0x25CA, 0x2329, 0xF8E8, 0xF8E9, 0xF8EA, 0x2211, 0xF8EB, 0xF8EC,
       0xF8ED, 0xF8EE, 0xF8EF, 0xF8F0, 0xF8F1, 0xF8F2, 0xF8F3, 0xF8F4,
       0,      0x232A, 0x222B, 0x2320, 0xF8F5, 0x2321, 0xF8F6, 0xF8F7,
       0xF8F8, 0xF8F9, 0xF8FA, 0xF8FB, 0xF8FC, 0xF8FD, 0xF8FE, 0)
    M <- cbind(ISOLatin1, ISOLatin2, ISOLatin7, ISOLatin9, Cyrillic, Greek,
               KOI8R, KOI8U,
               CP1250, CP1251, CP1252, CP1253, CP1257, AdobeSymbol)
    rownames(M) <- format.hexmode(0:255)
    storage.mode(M) <- "integer"
    class(M) <- c("noquote", "hexmode")
    M
})

Adobe_glyphs <- local({
    a <- scan(file.path(R.home("share"), "encodings", "Adobe-glyphlist"),
              what=list(adobe="", unicode=""), quiet=TRUE,
              sep=";", comment.char="#")
    a <- as.data.frame(a, stringsAsFactors=FALSE)
    a[order(a$unicode, a$adobe),]
})
#  File src/library/tools/R/index.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

### Miscellaneous indexing functions.

## <NOTE>
## Currently indices are represented as 2-column character matrices.
## To 'merge' indices in the sense of using the values from index B for
## all keys in index A also present in index B, we currently use
##   idx <- match(indA[ , 1L], indB[ , 1L], 0L)
##   indA[which(idx != 0L), 2L] <- indB[idx, 2L]
## which could be abstracted into a function .mergeIndexEntries().
## </NOTE>

### * .build_data_index

.build_data_index <-
function(dataDir, contents)
{
    ## Build an index with information about all available data sets.
    ## See .build_demo_index() for an explanation of what we do here.

    ## <NOTE>
    ## We could also have an interface like
    ##   .build_data_index(dir, contents = NULL)
    ## where @code{dir} is the path to a package's root source dir and
    ## contents is
    ##    Rd_contents(list_files_with_type(file.path(dir, "man"),
    ##                                     "docs")).
    ## </NOTE>

    if(!file_test("-d", dataDir))
        stop(gettextf("directory '%s' does not exist", dataDir),
             domain = NA)
    ## dataFiles <- list_files_with_type(dataDir, "data")
    dataTopics <- list_data_in_pkg(dataDir=dataDir)
    if(!length(dataTopics)) return(matrix("", 0L, 2L))
    names(dataTopics) <- paste(names(dataTopics), "/", sep="")
    datasets <- unlist(dataTopics)
    ## it is possible to have topics that create no object:
    ## BioC's makecdfenv did.
    if(!length(datasets)) return(matrix("", 0L, 2L))
    names(datasets) <- sub("/[^/]*$", "", names(datasets))
    datasets <- sort(datasets)
    dataIndex <- cbind(datasets, "")
    ## Note that NROW(contents) might be 0.
    if(length(datasets) && NROW(contents)) {
        aliasIndices <-
            rep(1 : NROW(contents), sapply(contents$Aliases, length))
        idx <- match(datasets, unlist(contents$Aliases), 0L)
        dataIndex[which(idx != 0L), 2L] <-
            contents[aliasIndices[idx], "Title"]
    }
    if(length(datasets))
        dataIndex[, 1L] <-
            as.vector(ifelse(datasets == names(datasets), datasets,
                             paste(datasets, " (", names(datasets), ")", sep="")))
    dimnames(dataIndex) <- NULL
    dataIndex
}

### * .build_demo_index

.build_demo_index <-
function(demoDir)
{
    ## Build an index with information about all available demos.

    ## <NOTE>
    ## We use both the contents of @file{00Index} (if possible) and the
    ## information which demos are actually available to build the real
    ## demo index.
    ## This ensures that demo() really lists all *available* demos, even
    ## if some might be 'undocumented', i.e., without index information.
    ## Use .check_demo_index() to check whether available demo code and
    ## docs are in sync.
    ## </NOTE>

    if(!file_test("-d", demoDir))
        stop(gettextf("directory '%s' does not exist", demoDir),
             domain = NA)
    demoFiles <- list_files_with_type(demoDir, "demo")
    demoTopics <- unique(basename(file_path_sans_ext(demoFiles)))
    if(!length(demoTopics)) return(matrix("", 0L, 2L))
    demoIndex <- cbind(demoTopics, "")
    if(file_test("-f", INDEX <- file.path(demoDir, "00Index"))) {
        demoEntries <- tryCatch(read.00Index(INDEX), error = identity)
        if(inherits(demoEntries, "error"))
            warning(gettextf("cannot read index information in file '%s'",
                             INDEX),
                    domain = NA)
        else {
            idx <- match(demoTopics, demoEntries[ , 1L], 0L)
            demoIndex[which(idx != 0L), 2L] <- demoEntries[idx, 2L]
        }
    }
    dimnames(demoIndex) <- NULL
    demoIndex
}

### * .check_demo_index

.check_demo_index <-
function(demoDir)
{
    if(!file_test("-d", demoDir))
        stop(gettextf("directory '%s' does not exist", demoDir),
             domain = NA)
    info_from_build <- .build_demo_index(demoDir)
    info_from_index <-
        tryCatch(read.00Index(file.path(demoDir, "00Index")),
                 error = function(e)
                 stop(gettextf("cannot read index information in file '%s'",
                               file.path(demoDir, "00Index")),
                      domain = NA))
    bad_entries <-
        list(missing_from_index =
             info_from_build[grep("^[[:space:]]*$",
                                  info_from_build[ , 2L]),
                             1L],
             missing_from_demos =
             info_from_index[!info_from_index[ , 1L]
                             %in% info_from_build[ , 1L],
                             1L])
    class(bad_entries) <- "check_demo_index"
    bad_entries
}

print.check_demo_index <-
function(x, ...)
{
    if(length(x$missing_from_index)) {
        writeLines("Demos with missing or empty index information:")
        print(x$missing_from_index)
    }
    if(length(x$missing_from_demos)) {
        writeLines("Demo index entries without corresponding demo:")
        print(x$missing_from_demos)
    }
    invisible(x)
}

### * .build_hsearch_index

.build_hsearch_index <-
function(contents, packageName, defaultEncoding = NULL)
{
    ## Build an index of the Rd contents in 'contents', of a package
    ## named 'packageName' in a form useful for help.search().
    ## As from 2.3.0 the installation directory is no longer recorded,
    ## but the format is kept for back-compatibility.

    dbAliases <- dbConcepts <- dbKeywords <-
        matrix(character(), ncol = 3L)

    if((nr <- NROW(contents)) > 0L) {
        ## IDs are used for indexing the Rd objects in the help.search
        ## db.
        IDs <- seq_len(nr)
        if(!is.data.frame(contents)) {
            colnames(contents) <-
                c("Name", "Aliases", "Title", "Keywords")
            base <- contents[, c("Name", "Title"), drop = FALSE]
            ## If the contents db is not a data frame, then it has the
            ## aliases collapsed.  Split again as we need the first
            ## alias as the help topic to indicate for matching Rd
            ## objects.
            aliases <- strsplit(contents[, "Aliases"], " +")
            ## Don't do this for keywords though, as these might be
            ## non-standard (and hence contain white space ...).
            encoding <- NULL
        }
        else {
            base <- as.matrix(contents[, c("Name", "Title")])
            aliases <- contents[, "Aliases"]
            encoding <- contents$Encoding # may not be there ...
        }
        if(is.null(encoding))
            encoding <- character(length = nr)
        if(!is.null(defaultEncoding))
            encoding[!nzchar(encoding)] <- defaultEncoding
        keywords <- contents[, "Keywords"]
        ## We create 4 character matrices (cannot use data frames for
        ## efficiency reasons): 'dbBase' holds all character string
        ## data; 'dbAliases', 'dbConcepts' and 'dbKeywords' hold
        ## character vector data in a 3-column character matrix format
        ## with entry, ID of the Rd object the entry comes from, and the
        ## package the object comes from.  The latter is useful when
        ## subscripting the help.search db according to package.
        dbBase <- cbind(packageName, "", IDs, base,
                        topic = sapply(aliases, "[", 1L), encoding)
        ## If there are no aliases at all, cbind() below would give
        ## matrix(packageName, ncol = 1L).  (Of course, Rd objects
        ## without aliases are useless ...)
        if(length(tmp <- unlist(aliases)))
            dbAliases <-
                cbind(tmp, rep.int(IDs, sapply(aliases, length)),
                      packageName)
        ## And similarly if there are no keywords at all.
        if(length(tmp <- unlist(keywords)))
            dbKeywords <-
                cbind(tmp, rep.int(IDs, sapply(keywords, length)),
                      packageName)
        ## Finally, concepts are a feature added in R 1.8 ...
        if("Concepts" %in% colnames(contents)) {
            concepts <- contents[, "Concepts"]
            if(length(tmp <- unlist(concepts)))
                dbConcepts <-
                    cbind(tmp, rep.int(IDs, sapply(concepts, length)),
                          packageName)
        }
    }
    else
        dbBase <- matrix(character(), ncol = 7L)

    colnames(dbBase) <-
        c("Package", "LibPath", "ID", "name", "title", "topic",
          "Encoding")
    colnames(dbAliases) <-
        c("Aliases", "ID", "Package")
    colnames(dbKeywords) <-
        c("Keywords", "ID", "Package")
    colnames(dbConcepts) <-
        c("Concepts", "ID", "Package")

    list(dbBase, dbAliases, dbKeywords, dbConcepts)
}

### * .build_links_index

.build_links_index <-
function(contents, package)
{
    if(length(contents)) {
        aliases <- contents$Aliases
        lens <- sapply(aliases, length)
        files <- sub("\\.[Rr]d$", "\\.html", contents$File)
        structure(file.path("../..", package, "html", rep.int(files, lens)),
                  names = unlist(aliases))
    } else character()
}


### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
#  File src/library/tools/R/install.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

#### R based engine for  R CMD INSTALL SHLIB Rdconv Rd2dvi
####

##' @param args

##' @return ...
.install_packages <- function(args = NULL)
{
    ## calls system() on Windows for
    ## sh (configure.win/cleanup.win) make zip

    ## we don't want to load utils just for this
    .file_test <- function(op, x)
        switch(op,
               "-f" = !is.na(isdir <- file.info(x)$isdir) & !isdir,
               "-x" = (file.access(x, 1L) == 0L),
               stop(sprintf("test '%s' is not available", op), domain = NA))
    dir.exists <- function(x) !is.na(isdir <- file.info(x)$isdir) & isdir

    ## global variables
    bundle_pkgs <- character() # list of packages in current pkg/bundle
    lockdir <- ""
    is_first_package <- TRUE
    stars <- "*"

    on.exit(do_exit_on_error())
    WINDOWS <- .Platform$OS.type == "windows"

    paste0 <- function(...) paste(..., sep="")

    MAKE <- Sys.getenv("MAKE")
    TAR <- shQuote(Sys.getenv("TAR"))
    GZIP <- Sys.getenv("R_GZIPCMD")
    if (!nzchar(GZIP)) GZIP <- "gzip"
    if (WINDOWS) zip <- "zip"
    rarch <- Sys.getenv("R_ARCH")

    SHLIB_EXT <- if (WINDOWS) ".dll" else {
        ## can we do better?
        mconf <- file.path(R.home(), paste0("etc", rarch), "Makeconf")
        sub(".*= ", "", grep("^SHLIB_EXT", readLines(mconf), value = TRUE))
    }

    options(warn = 1)
    invisible(Sys.setlocale("LC_COLLATE", "C")) # discard output

    if (WINDOWS) {
        rhome <- chartr("\\", "/", R.home())
        ## These might be needed for configure.win and Make{file,vars}.win
        ## Some people have *assumed* that R_HOME uses /
        Sys.setenv(R_HOME = rhome)
        ## and others have assumed that RHOME is set:
        Sys.setenv(RHOME = rhome)
    }

    Usage <- function() {
        cat("Usage: R CMD INSTALL [options] pkgs",
            "",
            "Install the add-on packages specified by pkgs.  The elements of pkgs can",
            "be relative or absolute paths to directories with the package (bundle)",
            "sources, or to gzipped package 'tar' archives.  The library tree",
            "to install to can be specified via '--library'.  By default, packages are",
            "installed in the library tree rooted at the first directory in",
            ".libPaths() for an R session run in the current environment",
            "",
            "Options:",
            "  -h, --help		print short help message and exit",
            "  -v, --version		print INSTALL version info and exit",
            "  -c, --clean		remove files created during installation",
            "      --preclean	remove files created during a previous run",
            "  -d, --debug		turn on script and build-help debugging",
            "  -l, --library=LIB	install packages to library tree LIB",
            "      --no-configure    do not use the package's configure script",
            "      --no-docs		do not install HTML, LaTeX or examples help",
            "      --html		build HTML help",
            "      --no-html		do not build HTML help",
            "      --latex      	install LaTeX help",
            "      --example		install R code for help examples",
            "      --use-zip-data	collect data files in zip archive",
            "      --fake		do minimal install for testing purposes",
            "      --no-lock, --unsafe",
            "			install on top of any existing installation",
            "			without using a lock directory",
            "      --pkglock		use a per-package lock directory",
            "      --build    	build binaries of the installed package(s)",
            "      --data-compress=	none, gzip (default), bzip2 or xz compression",
            "			to be used for lazy-loading of data",
            "      --resave-data	re-save data files as compactly as possible",
           "\nfor Unix",
            "      --configure-args=ARGS",
            "			set arguments for the configure scripts (if any)",
            "      --configure-vars=VARS",
            "			set variables for the configure scripts (if any)",
            "      --libs-only	only install the libs directory",
            "      --no-multiarch	build only the main architecture",
            "      --install-tests	install package-specific tests (if any)",
            "      --no-R, --no-libs, --no-data, --no-help, --no-demo, --no-exec,",
            "      --no-inst",
            "			suppress installation of the specified part of the",
            "			package for testing or other special purposes",
            "\nand on Windows only",
            "      --auto-zip	select whether to zip data automatically",
            "",
            "Which of --html or --no-html is the default depends on the build of R:",
            paste("for this one it is ",
                  ifelse(static_html, "--html", "--no-html"), ".", sep = ""),
            "",
            "Report bugs to <r-bugs@r-project.org>.", sep="\n")
    }

    do_cleanup <- function()
    {
        do_cleanup_tmpdir()
        if (!is_first_package) {
            ## Only need to do this in case we successfully installed at least
            ## *one* package ... well not so sure for bundles.
            file.copy(file.path(R.home("doc"), "html", "R.css"), lib)
            if (lib == .Library) {
                if (build_help)
                    unix.packages.html(.Library, docdir = R.home("doc"))
            }
        }
        if (lock && nzchar(lockdir)) unlink(lockdir, recursive = TRUE)
    }

    do_cleanup_tmpdir <- function()
    {
        ## Solaris will not remove any directory in the current path
        setwd(startdir)
        if (dir.exists(tmpdir)) unlink(tmpdir, recursive=TRUE)
    }

    do_exit_on_error <- function()
    {
        # message("*** do_exit_on_error ***")
        ## If we are not yet processing a package, we will not have
        ## set bundle_pkgs
        for(p in bundle_pkgs) {
            if (is.na(p) || !nzchar(p)) next
            pkgdir <- file.path(lib, p)
            if (nzchar(pkgdir) && dir.exists(pkgdir)) {
                starsmsg(stars, "removing ", sQuote(pkgdir))
                unlink(pkgdir, recursive = TRUE)
            }
            if (lock && nzchar(lockdir) &&
                dir.exists(lp <- file.path(lockdir, p))) {
                starsmsg(stars, "restoring previous ", sQuote(pkgdir))
                if (WINDOWS) {
                    file.copy(lp, dirname(pkgdir), recursive = TRUE)
                    unlink(lp, recursive = TRUE)
                } else system(paste("mv", lp, pkgdir))
            }
        }

        do_cleanup()
        q("no", status = 1, runLast = FALSE)
    }

    fullpath <- function(dir)
    {
        owd <- setwd(dir)
        full <- getwd()
        setwd(owd)
        full
    }


    parse_description_field <- function(desc, field, default=TRUE)
    {
        tmp <- desc[field]
        if (is.na(tmp)) default
        else switch(tmp,
                    "yes"=, "Yes" =, "true" =, "True" =, "TRUE" = TRUE,
                    "no" =, "No" =, "false" =, "False" =, "FALSE" = FALSE,
                    ## default
                    errmsg("invalid value of ", field, " field in DESCRIPTION")
                    )
    }

    starsmsg <- function(stars, ...)
        message(stars, " ", ..., domain=NA)

    errmsg <- function(...)
    {
        message("ERROR: ", ...)
        do_exit_on_error()
    }

    pkgerrmsg <- function(msg, pkg)
    {
        message("ERROR: ", msg, " for package ", sQuote(pkg), domain = NA)
        do_exit_on_error()
    }

    ## 'pkg' is the absolute path to package/bundle sources.
    do_install <- function(pkg)
    {
        setwd(pkg)
        desc <- read.dcf(file.path(pkg, "DESCRIPTION"))[1, ]
        ## Let's see if we have a bundle
        bundle_name <- desc["Bundle"]
        is_bundle <- !is.na(bundle_name)
        if (is_bundle) {
            contains <- .get_contains_from_package_db(desc)
            for(p in contains) {
                if (dir.exists(file.path(pkg, p))) {
                    pkgs <- c(pkgs, p)
                } else {
                    warning("incorrect Contains metadata for bundle ",
                            sQuote(bundle_name),
                            ": there is no package '", sQuote(p),
                            call. = FALSE, domain = NA)
                    warning("skipping installation of bundle ",
                            sQuote(bundle_name), call. = FALSE, domain = NA)
                    contains <- character()
                    break
                }
            }
            ## binary bundles are special.  Like source bundles they
            ## have a top-level DESCRIPTION file, but they have no
            ## 'Built' field in it, and no */DESCRIPTION.in
            if (length(contains) && length(Sys.glob("*/DESCRIPTION.in"))) {
                ## Create the package level DESCRIPTION files from the bundle
                ## level DESCRIPTION and the package level DESCRIPTION.in ones.
                res <- try(.vcreate_bundle_package_descriptions(pkg, paste(contains, collapse=" ")))
                if (inherits(res, "try-error"))
                    warning("problem installing per-package DESCRIPTION files",
                            call. = FALSE, domain = NA)
            }
            ## This cannot create a binary bundle, no top-level DESCRIPTION
            if (tar_up)
                errmsg("cannot create a binary bundle: use 'R CMD build --binary' to do so")
            bundle_pkgs <<- contains
        } else {
            bundle_name <- desc["Package"]
            if (is.na(bundle_name)) errmsg("no 'Package' field in 'DESCRIPTION'")
            bundle_pkgs <<- bundle_name
        }

        for(p in bundle_pkgs) {
            if (is_bundle) {
                pkg_dir <- file.path(pkg, p)
                setwd(pkg_dir)
                desc <- read.dcf("DESCRIPTION")[1, ]
            } else pkg_dir <- pkg
            pkg_name <- desc["Package"]
            Sys.setenv(R_PACKAGE_NAME = pkg_name)
            instdir <- file.path(lib, pkg_name)
            Sys.setenv(R_PACKAGE_DIR = instdir) ## installation dir
            ## if (WINDOWS) Sys.setenv(DPKG = instdir) ## assumed by some

            ## FIXME: do this at bundle level?
            ## Could different packages have different version requirements?
            status <- .Rtest_package_depends_R_version()
            if (status) do_exit_on_error()

            dir.create(instdir, recursive = TRUE, showWarnings = FALSE)
            if (!dir.exists(instdir)) {
                message("ERROR: unable to create ", sQuote(instdir),
                        domain = NA)
                do_exit_on_error()
            }

            ## Make sure we do not attempt installing to srcdir.
            owd <- setwd(instdir)
            if (owd == getwd()) pkgerrmsg("cannot install to srcdir", pkg_name)
            setwd(owd)

            ## Figure out whether this is a source or binary package.
            is_source_package <- is.na(desc["Built"])

            if (!is_first_package) cat("\n")

            if (is_source_package)
                do_install_source(pkg_name, instdir, pkg_dir, desc)
            else
                do_install_binary(pkg_name, instdir, desc)

            ## Add read permission to all, write permission to owner
            .Internal(dirchmod(instdir))
            ##    system(paste("find", shQuote(instdir),  "-exec chmod a+r \\{\\} \\;"))
            if (is_bundle)
                starsmsg(stars, "DONE (", pkg_name, ")")
            is_first_package <<- FALSE
        }

        if (tar_up) {
            version <- desc["Version"]
            filename <- paste0(bundle_name, "_", version, "_R_",
                               Sys.getenv("R_PLATFORM"), ".tar")
            filepath <- shQuote(file.path(startdir, filename))
            owd <- setwd(lib)
            system(paste(TAR, "-chf", filepath,
                         paste(bundle_pkgs, collapse = " ")))
            system(paste(GZIP, "-9f", filepath))
            message("packaged installation of ",
                    sQuote(bundle_name), " as ", filename, ".gz",
                    domain = NA)
            setwd(owd)
        }

        if (zip_up) {
            ZIP <- "zip"                # Windows only
            version <- desc["Version"]
            filename <- paste0(bundle_name, "_", version, ".zip")
            filepath <- shQuote(file.path(startdir, filename))
            ## system(paste("rm -f", filepath))
            unlink(filepath)
            owd <- setwd(lib)
            system(paste(ZIP, "-r9Xq", filepath,
                         paste(bundle_pkgs, collapse = " ")))
	    if (is_bundle) {
                ## need to add top-level DESCRIPTION file
                setwd(pkg)
                system(paste(ZIP, "-9Xq", filepath, "DESCRIPTION"))
	    }
            setwd(owd)
            message("packaged installation of ",
                    sQuote(bundle_name), " as ", filename)
        }

        starsmsg(stars, "DONE (", bundle_name, ")")

        bundle_pkgs <<- character()
    }


    ## Unix only
    do_install_binary <- function(pkg, instdir, desc)
    {
        starsmsg(stars, "installing *binary* package ", sQuote(pkg), " ...")

        if (file.exists(file.path(instdir, "DESCRIPTION"))) {
            if (lock) system(paste("mv", instdir, file.path(lockdir, pkg)))
            dir.create(instdir, recursive = TRUE, showWarnings = FALSE)
        }
        res <- system(paste("cp -r .", shQuote(instdir),
                            "|| (", TAR, "cd - .| (cd", shQuote(instdir), "&&", TAR, "xf-))"
                            ))
        if (res) errmsg("installing binary package failed")

        if (tar_up) {
            starsmsg(stars, sQuote(pkg),
                     " was already a binary package and will not be rebuilt")
            tar_up <- FALSE
        }
    }

    ## to be run from package source directory
    run_clean <- function()
    {
        if (dir.exists("src")) {
            owd <- setwd("src")
            if (WINDOWS) {
                if (file.exists("Makefile.win"))
                    system(paste(MAKE, "-f Makefile.win clean"))
                else
                    unlink(c("Makedeps",
                             Sys.glob("*_res.rc"),
                             Sys.glob("*.[do]")))
                    # system("rm -f *_res.rc *.o *.d Makedeps")
            } else {
                if (file.exists("Makefile")) system(paste(MAKE, "clean"))
                else ## we will be using SHLIB --preclean
                    unlink(Sys.glob(paste0("*", SHLIB_EXT)))
            }
            setwd(owd)
        }
        if (WINDOWS) {
            if (file.exists("cleanup.win")) system("sh ./cleanup.win")
        } else if (.file_test("-x", "cleanup")) system("./cleanup")
        else if (file.exists("cleanup"))
            warning("'cleanup' exists but is not executable -- see the 'R Installation and Adminstration Manual'", call. = FALSE)

    }

    do_install_source <- function(pkg_name, instdir, pkg_dir, desc)
    {
        cp_r <- function(from, to)
        {
            ## used for inst/
            if (WINDOWS) {
                file.copy(Sys.glob(file.path(from, "*")), to, recursive = TRUE)
                # system(paste0("cp -r ", shQuote(from), "/* ", shQuote(to)))
            } else {
                from <- shQuote(from)
                to <- shQuote(to)
                system(paste0("cp -r ", from, "/* ", to,
                              " || (cd ", from, " && ", TAR, " cf - . | (cd '",
                              to, "' && ", TAR, "xf - ))"))
            }
        }

        shlib_install <- function(instdir, arch)
        {
            files <- Sys.glob(paste0("*", SHLIB_EXT))
            if (length(files)) {
                libarch <- if (nzchar(arch)) paste0("libs", arch) else "libs"
                dest <- file.path(instdir, libarch)
                dir.create(dest, recursive = TRUE, showWarnings = FALSE)
                file.copy(files, dest, overwrite = TRUE)
                if (!WINDOWS)
                    Sys.chmod(Sys.glob(file.path(dest, "*")), "755")
            }
        }

        run_shlib <- function(pkg_name, srcs, instdir, arch)
        {
            args <- c(shargs, "-o", paste0(pkg_name, SHLIB_EXT), srcs)
            if (debug) message("about to run ",
                               "R CMD SHLIB ", paste(args, collapse= " "),
                               domain = NA)
            if (.shlib_internal(args) == 0L) {
                shlib_install(instdir, arch)
                return(FALSE)
            } else return(TRUE)
        }

        ## Make the destination directories available to the developer's
        ## installation scripts (e.g. configure)
        Sys.setenv(R_LIBRARY_DIR = lib)

        if (nzchar(lib0)) {
            ## FIXME: is this needed?
            ## set R_LIBS to include the current installation directory
            rlibs <- Sys.getenv("R_LIBS")
            rlibs <- if (nzchar(rlibs)) paste(lib, rlibs, sep=.Platform$path.sep) else lib
            Sys.setenv(R_LIBS = rlibs)
            ## This is needed
            .libPaths(c(lib, .libPaths()))
        }

        Type <- desc["Type"]
        if (!is.na(Type) && Type == "Frontend") {
            if (WINDOWS) errmsg("'Frontend' packages are Unix-only")
            starsmsg(stars, "installing *Frontend* package ", sQuote(pkg_name), " ...")
            if (preclean) system(paste(MAKE, "clean"))
            if (use_configure) {
                if (.file_test("-x", "configure")) {
                    res <- system(paste(paste(configure_vars, collapse = " "),
                                        "./configure",
                                        paste(configure_args, collapse = " ")))
                    if (res) pkgerrmsg("configuration failed", pkg_name)
                } else if (file.exists("configure"))
                    errmsg("'configure' exists but is not executable -- see the 'R Installation and Adminstration Manual'")
            }
            if (file.exists("Makefile"))
                if (system(MAKE)) pkgerrmsg("make failed", pkg_name)
            if (clean) system(paste(MAKE, "clean"))
            return()
        }

        if (!is.na(Type) && Type == "Translation") {
            starsmsg(stars, "installing *Translation* package ", sQuote(pkg_name), " ...")
            if (dir.exists("share")) {
                files <- Sys.glob("share/*")
                if (length(files)) file.copy(files, R.home("share"), TRUE)
            }
            if (dir.exists("library")) {
                ## FIXME use file.copy
                system(paste("cp -r ./library", R.home()))
            }
            return()
        }

        OS_type <- desc["OS_type"]
        if (WINDOWS) {
            if ((!is.na(OS_type) && OS_type == "unix") && !fake)
                errmsg(" Unix-only package")
        } else {
            if ((!is.na(OS_type) && OS_type == "windows") && !fake)
                errmsg(" Windows-only package")
        }

        starsmsg(stars, "installing *source* package ", sQuote(pkg_name), " ...")

        stars <- "**"

        if (file.exists(file.path(instdir, "DESCRIPTION"))) {
            ## Back up a previous version
            if (lock) {
                if (debug) starsmsg(stars, "backing up earlier installation")
                if(WINDOWS) {
                    file.copy(instdir, lockdir, recursive = TRUE)
                    unlink(instdir, recursive = TRUE)
                } else
                    system(paste("mv", instdir, file.path(lockdir, pkg_name)))
            } else if (more_than_libs) unlink(instdir, recursive = TRUE)
            dir.create(instdir, recursive = TRUE, showWarnings = FALSE)
        }

        if (preclean) run_clean()

        if (auto_zip || zip_up) { ## --build implies --auto-zip
            thislazy <- parse_description_field(desc, "LazyData",
                                                default = lazy_data)
            thiszip <- parse_description_field(desc, "ZipData",
                                               default = TRUE)
            if (!thislazy && thiszip && dir.exists("data")) {
                fi <- file.info(dir("data", full.names=TRUE))
                if (sum(fi$size) > 100000) {
                    this <- sub("\\.[a-zA-Z]+$", "", row.names(fi))
                    if (!anyDuplicated(this)) use_zip_data <- TRUE
                }
                if (use_zip_data)
                     message("\n  Using auto-selected zip option ",
                             sQuote("--use-zip-data"), "\n", domain = NA)
            }
        }

        if (use_configure) {
            if (WINDOWS) {
                if (file.exists("configure.win")) {
                    res <- system("sh ./configure.win")
                    if (res) pkgerrmsg("configuration failed", pkg_name)
                } else if (file.exists("configure"))
                    message("\n",
                            "   **********************************************\n",
                            "   WARNING: this package has a configure script\n",
                            "         It probably needs manual configuration\n",
                            "   **********************************************\n\n", domain = NA)
            } else {
                ## FIXME: should these be quoted?
                if (.file_test("-x", "configure")) {
                    cmd <- paste(paste(configure_vars, collapse = " "),
                                 "./configure",
                                 paste(configure_args, collapse = " "))
                    if (debug) message("configure command: ", sQuote(cmd),
                                       domain = NA)
                    res <- system(cmd)
                    if (res) pkgerrmsg("configuration failed", pkg_name)
                }  else if (file.exists("configure"))
                    errmsg("'configure' exists but is not executable -- see the 'R Installation and Adminstration Manual'")
            }
        }


        if (more_than_libs) {
            for (f in c("NAMESPACE", "LICENSE", "LICENCE", "COPYING", "NEWS"))
                if (file.exists(f)) {
                    file.copy(f, instdir, TRUE)
                    Sys.chmod(file.path(instdir, f), "644")
                }

            ## This cannot be done in a MBCS: write.dcf fails
            ctype <- Sys.getlocale("LC_CTYPE")
            Sys.setlocale("LC_CTYPE", "C")
            res <- try(.install_package_description(".", instdir))
            Sys.setlocale("LC_CTYPE", ctype)
            if (inherits(res, "try-error"))
                pkgerrmsg("installing package DESCRIPTION failed", pkg_name)
        }

        if (install_libs && dir.exists("src")) {
            system_makefile <- file.path(R.home(), paste0("etc", rarch),
                                         "Makeconf")
            starsmsg(stars, "libs")
            if (!file.exists(file.path(R.home("include"), "R.h")))
                ## maybe even an error?  But installing Fortran-based packages should work
                warning("R include directory is empty -- perhaps need to install R-devel.rpm or similar", call. = FALSE)
            has_error <- FALSE
            linkTo <- desc["LinkingTo"]
            if (!is.na(linkTo)) {
                lpkgs <- strsplit(linkTo, ",[[:blank:]]*")[[1L]]
                paths <- .find.package(lpkgs, quiet=TRUE)
                if (length(paths)) {
                    clink_cppflags <- paste(paste0('-I"', paths, '/include"'),
                                            collapse=" ")
                    Sys.setenv(CLINK_CPPFLAGS = clink_cppflags)
                }
            } else clink_cppflags <- ""
            libdir <- file.path(instdir, paste0("libs", rarch))
            dir.create(libdir, showWarnings = FALSE)
            if (WINDOWS) {
                owd <- setwd("src")
                makefiles <- character()
                if (file.exists(f <- path.expand("~/.R/Makevars.win")))
                    makefiles <- f
                else if (file.exists(f <- path.expand("~/.R/Makevars")))
                    makefiles <- f
                if (file.exists("Makefile.win")) {
                    makefiles <- c("Makefile.wIn", makefiles)
                    message("  running src/Makefile.win ...")
                    res <- system(paste("make --no-print-directory",
                                        paste("-f", shQuote(makefiles), collapse = " ")))
                    if (res == 0) shlib_install(instdir, "")
                    else has_error <- TRUE
                } else {
                    message("  making DLL ...")
                    srcs <- dir(pattern = "\\.([cfmCM]|cc|cpp|f90|f95|mm)$")
                    has_error <- run_shlib(pkg_name, srcs, instdir, "")
                    message("  ... done")
                }
                setwd(owd)
            } else { # not WINDOWS
                if (file.exists("src/Makefile")) {
                    arch <- substr(rarch, 2, 1000)
                    starsmsg(stars, "arch - ", arch)
                    owd <- setwd("src")
                    makefiles <- c(system_makefile, "Makefile")
                    if (file.exists(f <- path.expand(paste("~/.R/Makevars",
                                                           Sys.getenv("R_PLATFORM"), sep="-"))))
                        makefiles <- c(makefiles, f)
                    else if (file.exists(f <- path.expand("~/.R/Makevars")))
                        makefiles <- c(makefiles, f)
                    res <- system(paste(MAKE,
                                        paste("-f", shQuote(makefiles), collapse = " ")))
                    if (res == 0) shlib_install(instdir, rarch)
                    else has_error <- TRUE
                    setwd(owd)
                } else { ## no src/Makefile
                    owd <- setwd("src")
                    srcs <- dir(pattern = "\\.([cfmCM]|cc|cpp|f90|f95|mm)$")
                    ## This allows Makevars to set OBJECTS or its own targets.
                    allfiles <- if (file.exists("Makevars")) c("Makevars", srcs) else srcs
                    wd2 <- setwd(file.path(R.home(), "bin", "exec"))
                    archs <- Sys.glob("*")
                    setwd(wd2)
                    if (length(allfiles)) {
                        ## if there is a configure script we install only the main
                        ## sub-architecture
                        if (!multiarch ||
                            .file_test("-x", "../configure")) {
                            if (nzchar(rarch))
                                starsmsg(stars, "arch - ", substr(rarch, 2, 1000))
                            has_error <- run_shlib(pkg_name, srcs, instdir, rarch)
                        } else {
                            for(arch in archs) {
                                system("rm -f *.o *.so *.sl *.dylib")
                                if (arch == "R") {
                                    ## top-level, so one arch without subdirs
                                    has_error <- run_shlib(pkg_name, srcs, instdir, "")
                                } else if (arch == "Rgnome") {
                                    ## ignore
                                } else {
                                    starsmsg(stars, "arch - ", arch)
                                    ra <- paste0("/", arch)
                                    ## FIXME: do this lower down
                                    Sys.setenv(R_ARCH = ra)
                                    has_error <- run_shlib(pkg_name, srcs, instdir, ra)
                                    if (has_error) break
                                    Sys.setenv(R_ARCH = rarch)
                                }
                            }
                        }
                    } else warning("no source files found", call. = FALSE)
                }
                setwd(owd)
            }
            if (has_error)
                pkgerrmsg("compilation failed", pkg_name)
        }                               # end of src dir

	if (install_R && dir.exists("R")) {
	    starsmsg(stars, "R")
	    dir.create(file.path(instdir, "R"), recursive = TRUE,
		       showWarnings = FALSE)
	    ## This cannot be done in a C locale
	    res <- try(.install_package_code_files(".", instdir))
	    if (inherits(res, "try-error"))
		pkgerrmsg("unable to collate files", pkg_name)

	    if (file.exists(file.path("R", "sysdata.rda"))) {
		res <- try(sysdata2LazyLoadDB("R/sysdata.rda",
						      file.path(instdir, "R")))
		if (inherits(res, "try-error"))
		    pkgerrmsg("unable to build sysdata DB", pkg_name)
	    }
	    if (fake) {
		if (file.exists("NAMESPACE")) {
		    cat("",
			'.onLoad <- .onAttach <- function(lib, pkg) NULL',
			sep = "\n",
			file = file.path(instdir, "R", pkg_name), append = TRUE)
		    ## <NOTE>
		    ## Tweak fake installation to provide an 'empty'
		    ## useDynLib() for the time being.  Completely
		    ## removing the directive results in checkFF()
		    ## being too aggresive in the case where the
		    ## presence of the directive enables unambiguous
		    ## symbol resolution w/out 'PACKAGE' arguments.
		    ## However, empty directives are not really meant
		    ## to work ...

		    ## encoding issues ... so need useBytes = TRUE
		    ## FIXME: some packages have useDynlib()
		    ## spread over several lines.
		    writeLines(sub("useDynLib.*", 'useDynLib("")',
				   readLines("NAMESPACE", warn = FALSE),
				   perl = TRUE, useBytes = TRUE),
			       file.path(instdir, "NAMESPACE"))
		    ## </NOTE>
		} else {
		    cat("",
			'.First.lib <- function(lib, pkg) NULL',
			sep = "\n",
			file = file.path(instdir, "R", pkg_name), append = TRUE)
		}
	    }
	}                           # end of R

	if (install_data && dir.exists("data")) {
	    starsmsg(stars, "data")
	    files <- Sys.glob(file.path("data", "*"))
	    if (length(files)) {
		is <- file.path(instdir, "data")
		dir.create(is, recursive = TRUE, showWarnings = FALSE)
		file.remove(Sys.glob(file.path(instdir, "data", "*")))
		file.copy(files, is, TRUE)
		thislazy <- parse_description_field(desc, "LazyData",
						    default = lazy_data)
		if (!thislazy && resave_data) {
		    paths <- Sys.glob(c(file.path(is, "*.rda"),
					file.path(is, "*.RData")))
		    if (pkg_name == "cyclones")
			paths <-
			    c(paths, Sys.glob(file.path(is, "*.Rdata")))
		    if (length(paths)) {
			starsmsg(paste0(stars, "*"), "resaving rda files")
			resaveRdaFiles(paths, compress = "auto")
		    }
		}
		Sys.chmod(Sys.glob(file.path(instdir, "data", "*")), "644")
		if (thislazy) {
		    ## This also had an extra space in the sh version
		    starsmsg(stars, " moving datasets to lazyload DB")
		    ## 'it is possible that data in a package will
		    ## make use of the code in the package, so ensure
		    ## the package we have just installed is on the
		    ## library path.'
		    ## (We set .libPaths)
		    res <- try(data2LazyLoadDB(pkg_name, lib,
					       compress = data_compress))
		    if (inherits(res, "try-error"))
			pkgerrmsg("lazydata failed", pkg_name)
		} else if (use_zip_data &&
			   (WINDOWS ||
			   (nzchar(Sys.getenv("R_UNZIPCMD")) &&
			   nzchar(zip <- Sys.getenv("R_ZIPCMD"))) )) {
		    owd <- setwd(file.path(instdir, "data"))
		    writeLines(dir(), "filelist")
		    system(paste(zip, "-q -m Rdata * -x filelist 00Index"))
		    setwd(owd)
		}
	    } else warning("empty 'data' directory", call. = FALSE)
	}

	if (install_demo && dir.exists("demo")) {
	    starsmsg(stars, "demo")
	    dir.create(file.path(instdir, "demo"), recursive = TRUE,
		       showWarnings = FALSE)
	    file.remove(Sys.glob(file.path(instdir, "demo", "*")))
	    res <- try(.install_package_demos(".", instdir))
	    if (inherits(res, "try-error"))
		pkgerrmsg("ERROR: installing demos failed")
	    Sys.chmod(Sys.glob(file.path(instdir, "demo", "*")), "644")
	}

	if (install_exec && dir.exists("exec")) {
	    starsmsg(stars, "exec")
	    dir.create(file.path(instdir, "exec"), recursive = TRUE,
		       showWarnings = FALSE)
	    file.remove(Sys.glob(file.path(instdir, "exec", "*")))
	    files <- Sys.glob(file.path("exec", "*"))
	    if (length(files)) {
		file.copy(files, file.path(instdir, "exec"), TRUE)
		Sys.chmod(Sys.glob(file.path(instdir, "exec", "*")), "755")
	    }
	}

	if (install_inst && dir.exists("inst")) {
	    starsmsg(stars, "inst")
	    ## FIXME avoid installing .svn etc?
	    cp_r("inst", instdir)
	    ## file.copy("inst", "instdir", recursive = TRUE)
	}

	if (install_tests && dir.exists("tests")) {
	    starsmsg(stars, "tests")
	    file.copy("tests", instdir, recursive = TRUE)
	}

	## Defunct:
	## FIXME: remove these at some point
	if (file.exists("install.R"))
	    warning("use of file 'install.R' is no longer supported",
		    call. = FALSE, domain = NA)
	if (file.exists("R_PROFILE.R"))
	    warning("use of file 'R_PROFILE.R' is no longer supported",
		    call. = FALSE, domain = NA)
	value <- parse_description_field(desc, "SaveImage", default = NA)
	if (!is.na(value))
	    warning("field 'SaveImage' is defunct: please remove it",
		    call. = FALSE, domain = NA)

	## LazyLoading
	value <- parse_description_field(desc, "LazyLoad", default = lazy)
	if (install_R && dir.exists("R") && value) {
	    starsmsg(stars, "preparing package for lazy loading")
	    ## Something above, e.g. lazydata,  might have loaded the namespace
	    if (pkg_name %in% loadedNamespaces())
		unloadNamespace(pkg_name)
	    ## suppress second round of parse warnings
	    options(warnEscapes = FALSE)
	    res <- try({.getRequiredPackages(quietly = TRUE)
			makeLazyLoading(pkg_name, lib)})
	    options(warnEscapes = TRUE)
	    if (inherits(res, "try-error"))
		pkgerrmsg("lazy loading failed", pkg_name)
	    ## FIXME: still needed?  If so needs a pretest
	    ## file.remove(file.path(instdir, "R", "all.rda"))
	}

	if (install_help) {
	    starsmsg(stars, "help")
	    if (!dir.exists("man") ||
	       !length(list_files_with_type("man", "docs")))
		cat("No man pages found in package ", sQuote(pkg_name), "\n")
	    encoding <- desc["Encoding"]
	    if (is.na(encoding)) encoding <- "unknown"
	    res <- try(.install_package_Rd_objects(".", instdir, encoding))
	    if (inherits(res, "try-error"))
		pkgerrmsg("installing Rd objects failed", pkg_name)


	    starsmsg(paste0(stars, "*"), "installing help indices")
	    ## always want HTML package index
	    .writePkgIndices(pkg_dir, instdir)
	    if (build_help) {
		## This is used as the default outputEncoding for latex
		outenc <- desc["Encoding"]
		if (is.na(outenc)) outenc <- "latin1" # or ASCII
		.convertRdfiles(pkg_dir, instdir,
				types = build_help_types,
				outenc = outenc)
	    }
	}

	## pkg indices
	if (install_inst || install_demo || install_help) {
	    starsmsg(stars, "building package indices ...")
	    res <- try(.install_package_indices(".", instdir))
	    if (inherits(res, "try-error"))
		errmsg("installing package indices failed")
	}
	
	## Install a dump of the parsed NAMESPACE file
	if (install_R && file.exists("NAMESPACE") && !fake) {
	    res <- try(.install_package_namespace_info(".", instdir))
	    if (inherits(res, "try-error"))
		errmsg("installing namespace metadata failed")
	}

        ## <NOTE>
        ## Remove stuff we should not have installed in the first place.
        ## When installing from a source directory under version control, we
        ## should really exclude the subdirs CVS, .svn (Subversion) and
        ## .arch-ids (arch).
        for(d in c("CVS", ".svn", ".arch-ids", ".git")) {
            ## FIXME
            if (!WINDOWS)
                system(paste("find",  shQuote(instdir), "-name", d,
                             "-type d -prune -exe rm \\{\\} \\;"),
                       ignore.stderr = TRUE)
        }

        if (clean) run_clean()

        if (WINDOWS) { ## Add MD5 sums: only for --build?
            starsmsg(stars, "MD5 sums")
            .installMD5sums(instdir)
        }

    }

    options(showErrorCalls=FALSE)
    pkgs <- character(0)
    if (is.null(args)) {
        args <- commandArgs(TRUE)
        ## it seems that splits on spaces, so try harder.
        args <- paste(args, collapse=" ")
        args <- strsplit(args,'nextArg', fixed = TRUE)[[1]][-1]
    }

    startdir <- getwd()

    lib <- lib0 <- ""
    clean <- FALSE
    preclean <- FALSE
    debug <- FALSE
    static_html <- nzchar(system.file("html", "mean.html", package="base"))
    build_html <- static_html
    build_latex <- FALSE
    build_example <- FALSE
    use_configure <- TRUE
    use_zip_data <- FALSE
    auto_zip <- FALSE
    configure_args <- character(0)
    configure_vars <- character(0)
    fake <- FALSE
    lazy <- TRUE
    lazy_data <- FALSE
    lock <- TRUE
    pkglock <- FALSE
    pkglockname <- ""
    libs_only <- FALSE
    tar_up <- zip_up <- FALSE
    shargs <- character(0)
    multiarch <- TRUE

    get_user_libPaths <- FALSE
    data_compress <- TRUE # FALSE (none), TRUE (gzip), 2 (bzip2), 3 (xz)
    resave_data <- FALSE
    
    install_libs <- TRUE
    install_R <- TRUE
    install_data <- TRUE
    install_demo <- TRUE
    install_exec <- TRUE
    install_inst <- TRUE
    install_help <- TRUE
    install_tests <- FALSE      
    
    while(length(args)) {
        a <- args[1]
        if (a %in% c("-h", "--help")) {
            Usage()
            q("no", runLast = FALSE)
        }
        else if (a %in% c("-v", "--version")) {
            cat("R add-on package installer: ",
                R.version[["major"]], ".",  R.version[["minor"]],
                " (r", R.version[["svn rev"]], ")\n", sep = "")
            cat("",
                "Copyright (C) 2000-2009 The R Core Development Team.",
                "This is free software; see the GNU General Public License version 2",
                "or later for copying conditions.  There is NO warranty.",
                sep="\n")
            q("no", runLast = FALSE)
        } else if (a %in% c("-c", "--clean")) {
            clean <- TRUE
            shargs <- c(shargs, "--clean")
        } else if (a == "--preclean") {
            preclean <- TRUE
            shargs <- c(shargs, "--preclean")
        } else if (a %in% c("-d", "--debug")) {
            debug <- TRUE
        } else if (a == "--no-configure") {
            use_configure <- FALSE
        } else if (a == "--no-docs") {
            build_html <- build_latex <- build_example <- FALSE
        } else if (a == "--no-html") {
            build_html <- FALSE
        } else if (a == "--html") {
            build_html <- TRUE
        } else if (a == "--latex") {
            build_latex <- TRUE
        } else if (a == "--example") {
            build_example <- TRUE
        } else if (a == "--use-zipdata") {
            use_zip_data <- TRUE
        } else if (a == "--auto-zip") {
            if (WINDOWS) auto_zip <- TRUE
            else warning("'--auto-zip' is for Windows only", call. = FALSE)
        } else if (a == "-l") {
            if (length(args) >= 2) {lib <- args[2]; args <- args[-1]}
            else stop("-l option without value", call. = FALSE)
        } else if (substr(a, 1, 10) == "--library=") {
            lib <- substr(a, 11, 1000)
        } else if (substr(a, 1, 17) == "--configure-args=") {
            configure_args <- c(configure_args, substr(a, 18, 1000))
        } else if (substr(a, 1, 17) == "--configure-vars=") {
            configure_vars <- c(configure_vars, substr(a, 18, 1000))
        } else if (a == "--fake") {
            fake <- TRUE
        } else if (a %in% c("--no-lock", "--unsafe")) {
            lock <- FALSE
        } else if (a == "--pkglock") {
            pkglock <- TRUE
        } else if (a == "--libs-only") {
            libs_only <- TRUE
        } else if (a == "--no-multiarch") {
            multiarch <- FALSE
        } else if (a == "--maybe-get-user-libPaths") {
            get_user_libPaths <- TRUE
        } else if (a == "--build") {
            if (WINDOWS) zip_up <- TRUE else tar_up <- TRUE
        } else if (substr(a, 1, 16) == "--data-compress=") {
            dc <- substr(a, 17, 1000)
            dc <- match.arg(dc, c("none", "gzip", "bzip2", "xz"))
            data_compress <- switch(dc,
                                    "none" = FALSE,
                                    "gzip" = TRUE,
                                    "bzip2" = 2,
                                    "xz" = 3)
        } else if (a == "--resave-data") {
            resave_data <- TRUE
        } else if (a == "--install-tests") {
            install_tests <- TRUE        
        } else if (a == "--no-inst") {
            install_inst <- FALSE
        } else if (a == "--no-R") {
            install_R <- FALSE
        } else if (a == "--no-libs") {
            install_libs <- FALSE
        } else if (a == "--no-data") {
            install_data <- FALSE
        } else if (a == "--no-demo") {
            install_demo <- FALSE
        } else if (a == "--no-exec") {
            install_exec <- FALSE
        } else if (a == "--no-help") {
            install_help <- FALSE
        } else if (substr(a, 1, 1) == "-") {
            message("Warning: unknown option ", sQuote(a))
        } else pkgs <- c(pkgs, a)
        args <- args[-1]
    }

    tmpdir <- tempfile("R.INSTALL")
    if (!dir.create(tmpdir))
        stop("cannot create temporary directory")

    ## now unpack tarballs and do some basic checks
    allpkgs <- character()
    for(pkg in pkgs) {
        if (debug) message("processing ", sQuote(pkg), domain = NA)
        if (.file_test("-f", pkg)) {
            if (debug) message("a file", domain = NA)
            pkgname <- basename(pkg) # or bundle name
            ## Also allow for 'package.tgz' ...
            pkgname <- sub("\\.(tgz|tar\\.gz|tar\\.bz2)$", "", pkgname)
            pkgname <- sub("_.*", "", pkgname)
            res <- if (WINDOWS) {
                utils::untar(pkg, exdir = chartr("\\", "/", tmpdir))
            } else {
                utils::untar(pkg, exdir = tmpdir)
            }
            if (res) errmsg("error unpacking tarball")
            ## If we have a binary bundle distribution, there should be
            ## a DESCRIPTION file at top level.
            if (file.exists(ff <- file.path(tmpdir, "DESCRIPTION"))) {
                con <- read.dcf(ff, "Contains")
                if (!is.na(con)) {
                    starsmsg(stars, "looks like a binary bundle")
                    allpkgs <- c(allpkgs, tmpdir)
                } else {
                    message("unknown package layout", domain = NA)
                    do_cleanup_tmpdir()
                    q("no", status = 1, runLast = FALSE)
                }
            } else if (file.exists(file.path(tmpdir, pkgname, "DESCRIPTION"))) {
                allpkgs <- c(allpkgs, file.path(tmpdir, pkgname))
            } else errmsg("cannot extract package from ", sQuote(pkg))
        } else if (file.exists(file.path(pkg, "DESCRIPTION"))) {
            if (debug) message("a directory", domain = NA)
            pkgname <- basename(pkg)
            allpkgs <- c(allpkgs, fullpath(pkg))
        } else {
            warning("invalid package ", sQuote(pkg), call. = FALSE)
            next
        }
        if (pkglock) {
            if (nzchar(pkglockname)) {
                warning("--pkglock applies only to a single bundle/package",
                        call. = FALSE)
                pkglock <- FALSE
            } else pkglockname <- pkgname
        }
    }

    if (!length(allpkgs))
        stop("ERROR: no packages specified", call.=FALSE)

    if (!nzchar(lib)) {
        lib <- if (get_user_libPaths) { ## need .libPaths()[1] *after* the site- and user-initialization
	    system(paste(file.path(R.home("bin"), "Rscript"),
                         "-e 'cat(.libPaths()[1])'"),
                   intern = TRUE)
        }
        else .libPaths()[1]
        starsmsg(stars, "installing to library ", sQuote(lib))
    } else {
        lib0 <- lib <- path.expand(lib)
        ## lib is allowed to be a relative path.
        ## should be OK below, but be sure.
        cwd <- tryCatch(setwd(lib), error = function(e)
                        stop("ERROR: cannot cd to directory ", sQuote(lib), call. = FALSE))
        lib <- getwd()
        setwd(cwd)
    }
    ok <- dir.exists(lib)
    if (ok) {
        if (WINDOWS) {
            ## file.access is unreliable on Windows
            ## the only known reliable way is to try it
            fn <- file.path(lib, paste("_test_dir", Sys.getpid(), sep="_"))
            unlink(fn, recursive = TRUE) # precaution
            res <- try(dir.create(fn, showWarnings = FALSE))
            if (inherits(res, "try-error") || !res) ok <- FALSE
            else unlink(fn, recursive = TRUE)
        } else ok <- file.access(lib, 2L) == 0
    }
    if (!ok)
        stop("ERROR: no permission to install to directory ",
             sQuote(lib), call. = FALSE)

    if (libs_only) {
        lock <- FALSE
        tar_up <- FALSE
	install_R <- FALSE
	install_data <- FALSE
	install_demo <- FALSE
	install_exec <- FALSE
	install_inst <- FALSE
	install_help <- FALSE
    }
    more_than_libs <- !libs_only


    if (lock) {
        lockdir <- if (pkglock) file.path(lib, paste("00LOCK", pkglockname, sep="-"))
        else file.path(lib, "00LOCK")
        if (file.exists(lockdir)) {
            message("ERROR: failed to lock directory ", sQuote(lib),
                    " for modifying\nTry removing ", sQuote(lockdir))
            do_cleanup_tmpdir()
            q("no", status = 3, runLast = FALSE)
        }
        dir.create(lockdir, recursive = TRUE)
        if (!dir.exists(lockdir)) {
            message("ERROR: failed to create lock directory ", sQuote(lockdir))
            do_cleanup_tmpdir()
            q("no", status = 3, runLast = FALSE)
        }
        if (debug) starsmsg(stars, "created lock directory ", sQuote(lockdir))
    }

    if  ((tar_up || zip_up) && fake)
        stop("building a fake installation is disallowed")

    if (fake) {
        use_configure <- FALSE
        build_html <- FALSE
        build_latex <- FALSE
        build_example <- FALSE
	install_libs <- FALSE
	install_demo <- FALSE
	install_exec <- FALSE
	install_inst <- FALSE
    }

    build_help_types <- character(0)
    if (build_html) build_help_types <- c(build_help_types, "html")
    if (build_latex) build_help_types <- c(build_help_types, "latex")
    if (build_example) build_help_types <- c(build_help_types, "example")
    build_help <- length(build_help_types) > 0L
    if (build_help && !install_help) {
	warning("--no-help overrides ", 
	        paste("--", build_help_types, sep="", collapse=" "),
                call. = FALSE)
    }
    
    if (debug)
        starsmsg(stars, "build_help_types=",
                 paste(build_help_types, collapse=" "))

    if (debug)
        starsmsg(stars, "DBG: R CMD INSTALL' now doing do_install")

    for(pkg in allpkgs) do_install(pkg)
    do_cleanup()
    on.exit()
    invisible()
}

## for R CMD SHLIB on all platforms
.SHLIB <- function()
{
    status <- .shlib_internal(commandArgs(TRUE))
    q("no", status = (status != 0), runLast=FALSE)
}

## for .SHLIB and R CMD INSTALL on all platforms
.shlib_internal <- function(args)
{
    Usage <- function()
        cat("Usage: R CMD SHLIB [options] files | linker options",
            "",
            "Build a shared library for dynamic loading from the specified source or",
            "object files (which are automagically made from their sources) or",
            "linker options.  If not given via '--output', the name for the shared",
            "library is determined from the first source or object file.",
            "",
            "Options:",
            "  -h, --help		print short help message and exit",
            "  -v, --version		print version info and exit",
            "  -o, --output=LIB	use LIB as (full) name for the built library",
            "  -c, --clean		remove files created during compilation",
            "  --preclean		remove files created during a previous run",
            "  -n, --dry-run		dry run, showing commands that would be used",
            "",
            "Windows only:",
            "  -d, --debug		build a debug DLL",
            "",
            "Report bugs to <r-bugs@r-project.org>.",
            sep="\n")

    p0 <- function(...) paste(..., sep="")
    ## FIXME shQuote here?
    p1 <- function(...) paste(..., collapse=" ")

    WINDOWS <- .Platform$OS.type == "windows"
    if (!WINDOWS) {
        mconf <- readLines(file.path(R.home(),
                                     p0("etc", Sys.getenv("R_ARCH")),
                                     "Makeconf"))
        SHLIB_EXT <- sub(".*= ", "", grep("^SHLIB_EXT", mconf, value = TRUE))
        SHLIB_LIBADD <- sub(".*= ", "", grep("^SHLIB_LIBADD", mconf, value = TRUE))
        MAKE <- Sys.getenv("MAKE")
    } else {
        rhome <- chartr("\\", "/", R.home())
        Sys.setenv(R_HOME = rhome)
        SHLIB_EXT <- ".dll"
        SHLIB_LIBADD <- ""
        MAKE <- "make"
    }

    OBJ_EXT <- ".o" # all currrent compilers, but not some on Windows

    objs <- character()
    shlib <- ""
    makefiles <-
        file.path(R.home("share"), "make",
                  if (WINDOWS) "winshlib.mk" else "shlib.mk")
    shlib_libadd <- if (nzchar(SHLIB_LIBADD)) SHLIB_LIBADD else character()
    with_cxx <- FALSE
    with_f77 <- FALSE
    with_f9x <- FALSE
    with_objc <- FALSE
    pkg_libs <- character()
    clean <- FALSE
    preclean <- FALSE
    dry_run <- FALSE
    debug <- FALSE

    while(length(args)) {
        a <- args[1]
        if (a %in% c("-h", "--help")) {
            Usage()
            return(0L)
        }
        else if (a %in% c("-v", "--version")) {
            cat("R shared library builder: ",
                R.version[["major"]], ".",  R.version[["minor"]],
                " (r", R.version[["svn rev"]], ")\n", sep = "")
            cat("",
                "Copyright (C) 2000-2009 The R Core Development Team.",
                "This is free software; see the GNU General Public License version 2",
                "or later for copying conditions.  There is NO warranty.",
                sep="\n")
            return(0L)
        } else if (a %in% c("-n", "--dry-run")) {
            dry_run <- TRUE
        } else if (a %in% c("-d", "--debug")) {
            debug <- TRUE
        } else if (a %in% c("-c", "--clean")) {
            clean <- TRUE
        } else if (a == "--preclean") {
            preclean <- TRUE
        } else if (a == "-o") {
            if (length(args) >= 2) {shlib <- args[2]; args <- args[-1]}
            else stop("-o option without value", call. = FALSE)
        } else if (substr(a, 1, 9) == "--output=") {
            shlib <- substr(a, 10, 1000)
        } else {
            ## a source file or something like -Ldir -lfoo
            base <- sub("\\.[[:alnum:]]*$", "", a)
            ext <- sub(p0(base, "."),  "", a)
            nobj <- ""
            if (nzchar(ext)) {
                if (ext %in% c("cc", "cpp", "C")) {
                    with_cxx <- TRUE
                    nobj <- base
                } else if (ext == "m") {
                    with_objc <- TRUE
                    nobj <- base
                } else if (ext %in% c("mm", "M")) {
                    ## ObjC++ implies ObjC because we need ObjC runtime
                    ## ObjC++ implies C++ because we use C++ linker
                    with_objc <- with_cxx <- TRUE
                    nobj <- base
                } else if (ext == "f") {
                    with_f77 <- TRUE
                    nobj <- base
                } else if (ext %in% c("f90", "f95")) {
                    with_f9x <- TRUE
                    nobj <- base
                } else if (ext == "c") {
                    nobj <- base
                } else if (ext == "o") {
                    nobj <- base
                }
                if (nzchar(nobj) && !nzchar(shlib))
                    shlib <- p0(nobj, SHLIB_EXT)
            }
            if (nzchar(nobj)) objs <- c(objs, nobj)
            else pkg_libs <- c(pkg_libs, a)
        }
        args <- args[-1]
    }

    if (length(objs)) objs <- p0(objs, OBJ_EXT, collapse=" ")

    if (WINDOWS) {
        if (file.exists(f <- path.expand("~/.R/Makevars.win")))
            makefiles <- c(makefiles, f)
        else if (file.exists(f <- path.expand("~/.R/Makevars")))
            makefiles <- c(makefiles, f)
    } else {
        if (file.exists(f <- path.expand(paste("~/.R/Makevars",
                                               Sys.getenv("R_PLATFORM"), sep="-"))))
            makefiles <- c(makefiles, f)
        else if (file.exists(f <- path.expand("~/.R/Makevars")))
            makefiles <- c(makefiles, f)
    }

    makeobjs <- p0("OBJECTS=", shQuote(objs))
    if (WINDOWS && file.exists("Makevars.win")) {
        makefiles <- c("Makevars.win", makefiles)
        lines <- readLines("Makevars.win", warn = FALSE)
        if (length(grep("^OBJECTS *=", lines, perl=TRUE, useBytes=TRUE)))
            makeobjs <- ""
    } else if (file.exists("Makevars")) {
        makefiles <- c("Makevars", makefiles)
        lines <- readLines("Makevars", warn = FALSE)
        if (length(grep("^OBJECTS *=", lines, perl=TRUE, useBytes=TRUE)))
            makeobjs <- ""
    }

    makeargs <- p0("SHLIB=", shQuote(shlib))
    if (with_f9x) {
        makeargs <- c("SHLIB_LDFLAGS='$(SHLIB_FCLDFLAGS)'",
                      "SHLIB_LD='$(SHLIB_FCLD)'", makeargs)
    } else if (with_cxx) {
        makeargs <- c("SHLIB_LDFLAGS='$(SHLIB_CXXLDFLAGS)'",
                      "SHLIB_LD='$(SHLIB_CXXLD)'", makeargs)
    }
    if (with_objc) shlib_libadd <- c(shlib_libadd, "$(OBJC_LIBS)")
    if (with_f77) shlib_libadd <- c(shlib_libadd, "$(FLIBS)")

    if (length(pkg_libs))
        makeargs <- c(makeargs,
                      p0("PKG_LIBS='", p1(pkg_libs), "'"))
    if (length(shlib_libadd))
        makeargs <- c(makeargs,
                      p0("SHLIB_LIBADD='", p1(shlib_libadd), "'"))

    ## removed in 2.10.0
    ## if (WINDOWS) makeargs <- c(makeargs, "all")
    if (WINDOWS && debug) makeargs <- c(makeargs, "DEBUG=T")

    cmd <- paste(MAKE, p1(paste("-f", makefiles)), p1(makeargs), p1(makeobjs))
    if (dry_run) {
        cat("make cmd is\n  ", cmd, "\n\nmake would use\n", sep = "")
        system(paste(cmd, "-n"))
        res <- 0
    } else {
        if (preclean) system(paste(cmd, "shlib-clean"))
        res <- system(cmd)
        if (clean) system(paste(cmd, "shlib-clean"))
    }
    res # probably a multiple of 256
}

## base packages do not have versions and this is called on
## DESCRIPTION.in
## encodings are tricky: this may be done in a foreign encoding
## (e.g., Latin-1 in UTF-8)
.DESCRIPTION_to_latex <- function(descfile, outfile, version = "Unknown")
{
    desc <- read.dcf(descfile)[1, ]
    if (is.character(outfile)) {
        out <- file(outfile, "a")
        on.exit(close(out))
    } else out <- outfile
    cat("\\begin{description}", "\\raggedright{}", sep="\n", file=out)
    fields <- names(desc)
    fields <- fields[! fields %in% c("Bundle", "Package", "Packaged", "Built")]
    if ("Encoding" %in% fields)
        cat("\\inputencoding{", latex_canonical_encoding(desc["Encoding"]),
            "}\n", sep = "", file = out)
    for (f in fields) {
        text <- desc[f]
        ## munge 'text' appropriately (\\, {, }, "...")
        ## not sure why just these: copied from Rd2dvi, then added to.
        ## KH: the LaTeX special characters are
        ##   # $ % & _ ^ ~ { } \
        ## \Rd@AsIs@dospecials in Rd.sty handles the first seven, so
        ## braces and backslashes need explicit handling.
        text <- gsub('"([^"]*)"', "\\`\\`\\1''", text, useBytes = TRUE)
        text <- gsub("\\", "\\textbackslash{}", text,
                     fixed = TRUE, useBytes = TRUE)
        text <- gsub("([{}$#_])", "\\\\\\1", text, useBytes = TRUE)
        text <- gsub("@VERSION@", version, text, fixed = TRUE, useBytes = TRUE)
        ## text can have paras, and digest/DESCRIPTION does.
        ## \AsIs is per-para.
        text <- strsplit(text, "\n\n", fixed = TRUE, useBytes = TRUE)[[1]]
        Encoding(text) <- "unknown"
        wrap <- paste("\\AsIs{", text, "}", sep = "")
        if(f %in% c("Author", "Maintainer"))
            wrap <- gsub("<([^@]+)@([^>]+)>", "\\\\email{\\1@\\2}",
                         wrap, useBytes = TRUE)
        if(f == "URL")
            wrap <- gsub("(http://|ftp://)([^[:space:]]+)",
                         "\\\\url{\\1\\2}", wrap, useBytes = TRUE)
        ## Not entirely safe: in theory, tags could contain \ ~ ^.
        cat("\\item[", gsub("([#$%&_{}])", "\\\\\\1", f),
            "]", paste(wrap, collapse = "\n\n"),  "\n", sep = "", file=out)
    }
    cat("\\end{description}\n", file = out)
}

## workhorse of .Rd2dvi
.Rdfiles2tex <-
    function(files, outfile, encoding = "unknown", outputEncoding = "latin1",
             append = FALSE, extraDirs = NULL, internals = FALSE,
             silent = FALSE)
{
    if (file_test("-d", files))
        .pkg2tex(files, outfile, encoding = encoding, append = append,
                 asChapter = FALSE, extraDirs = extraDirs,
                 internals = internals, silent = silent)
    else {
        files <- strsplit(files, "[[:space:]]+")[[1]]
        latexdir <- tempfile("ltx")
        dir.create(latexdir)
        if (!silent) message("Converting Rd files to LaTeX ...")
        if (is.character(outfile)) {
            outfile <- file(outfile, if (append) "at" else "wt")
            on.exit(close(outfile))
        }
        latexEncodings <- character()
        for(f in files) {
            cat("  ", basename(f), "\n", sep="")
            if (!internals) {
                lines <- readLines(f)
                if (any(grepl("\\\\keyword\\{\\s*internal\\s*\\}",
                         lines, perl = TRUE))) next
            }
            out <-  file.path(latexdir, sub("\\.[Rr]d$", ".tex", basename(f)))
            ## people have file names with quotes in them.
            latexEncodings <- c(latexEncodings,
                                attr(Rd2latex(f, out, encoding=encoding,
                                              outputEncoding=outputEncoding),
                                     "latexEncoding"))
            writeLines(readLines(out), outfile)
        }
        unique(latexEncodings[!is.na(latexEncodings)])
    }
}

## used for the refman (from doc/manual/Makefile*)
## and for directories from .Rdfiles2tex  (with asChapter = FALSE)
.pkg2tex <-
    function(pkgdir, outfile, internals = FALSE, asChapter = TRUE,
             encoding = "unknown", outputEncoding = "latin1",
             extraDirs = NULL, append = FALSE, silent = FALSE)
{
    ## sort order for topics, a little tricky
    re <- function(x) x[order(toupper(x), x)]

    ## given an installed package with a latex dir or a source package
    ## with a man dir, make a single file for use in the refman.

    options(warn = 1)
    if (missing(outfile))
        outfile <- paste(basename(pkgdir), "-pkg.tex", sep="")

    latexEncodings <- character() # Record any encodings used in the output

    ## First check for a latex dir.
    ## Second guess is this is a >= 2.10.0 package with stored .rds files.
    ## If it does not exist, guess this is a source package.
    latexdir <- file.path(pkgdir, "latex")
    if (!file_test("-d", latexdir)) {
        if (file_test("-d", file.path(pkgdir, "help"))) {
            ## So convert it
            latexdir <- tempfile("ltx")
            dir.create(latexdir)
            if (!silent) message("Converting parsed Rd's to LaTeX ",
                                 appendLF = FALSE, domain = NA)
            Rd <- Rd_db(basename(pkgdir), lib.loc = dirname(pkgdir))
            if (!length(Rd)) {
                if (is.character(outfile))
                    close(file(outfile, if (append) "at" else "wt"))
                return(invisible(character()))
            }
            cnt <- 0L
            for(f in names(Rd)) {
                bf <- basename(f)
                cnt <- cnt + 1L
                if (!silent && cnt %% 10L == 0L)
                    message(".", appendLF=FALSE, domain=NA)
                out <-  sub("[Rr]d$", "tex", basename(f))
                latexEncodings <- c(latexEncodings,
                                    attr(Rd2latex(Rd[[f]],
                                                  file.path(latexdir, out),
                                                  encoding = encoding,
                                                  outputEncoding = outputEncoding,
                                                  defines = NULL),
                                         "latexEncoding"))
            }
            if (!silent) message(domain = NA)
        } else {
            files <- c(Sys.glob(file.path(pkgdir, "*.Rd")),
                       Sys.glob(file.path(pkgdir, "*.rd")))
            if (!length(files)) {
                ## is this a source package?  That has man/*.Rd files.
                files <- c(Sys.glob(file.path(pkgdir, "man", "*.Rd")),
                           Sys.glob(file.path(pkgdir, "man", "*.rd")))
                if (!length(files))
                    stop("this package does not have either a ", sQuote("latex"),
                         " or a (source) ", sQuote("man"), " directory",
                         domain = NA)
                if (is.null(extraDirs)) extraDirs <- .Platform$OS.type
                for(e in extraDirs)
                    files <- c(files,
                               Sys.glob(file.path(pkgdir, "man", e, "*.Rd")),
                               Sys.glob(file.path(pkgdir, "man", e, "*.rd")))
            }
            latexdir <- tempfile("ltx")
            dir.create(latexdir)
            message("Converting Rd files to LaTeX ...")
            for(f in files) {
                cat("  ", basename(f), "\n", sep="")
                out <-  sub("\\.[Rr]d$", ".tex", basename(f))
                latexEncodings <-
                    c(latexEncodings,
                      attr(Rd2latex(f, file.path(latexdir, out),
                                    encoding = encoding,
                                    outputEncoding = outputEncoding),
                           "latexEncoding"))
            }
        }
    }
    ## they might be zipped up
    if (file.exists(f <- file.path(latexdir, "Rhelp.zip"))) {
        dir.create(newdir <- tempfile("latex"))
        unzip(f, exdir = newdir)
        ## res <- system(paste("unzip -q", f, "-d", newdir))
        ## if (res) stop("unzipping latex files failed")
        latexdir <- newdir
    }
    ## There are some restrictions, but the former "[[:alnum:]]+\\.tex$" was
    ## too strict.
    files <- dir(latexdir, pattern = "\\.tex$", full.names = TRUE)
    if (!length(files))
        stop("no validly-named files in the ", sQuote("latex"), " directory",
             domain = NA)

    if (is.character(outfile)) {
        outcon <- file(outfile, if (append) "at" else "wt")
        on.exit(close(outcon))
    } else outcon <- outfile

    if (asChapter)
        cat("\n\\chapter{The \\texttt{", basename(pkgdir), "} package}\n",
            sep = "", file = outcon)
    topics <- rep.int("", length(files)); names(topics) <- files
    scanForEncoding <- !length(latexEncodings)
    for (f in files) {
        lines <- readLines(f)  # This reads as "unknown", no re-encoding done
        hd <- grep("^\\\\HeaderA", lines, value = TRUE,
                   perl = TRUE, useBytes = TRUE)
        if (!length(hd)) {
            warning("file ", sQuote(f), " lacks a header: skipping",
                    domain = NA)
            next
        }
        this <- sub("\\\\HeaderA\\{\\s*([^}]*)\\}.*", "\\1", hd[1], perl = TRUE)
        if (!internals &&
           any(grepl("\\\\keyword\\{\\s*internal\\s*\\}", lines, perl = TRUE)))
            next
        if (scanForEncoding) {
	    enc <- lines[grepl('^\\\\inputencoding', lines, perl = TRUE)]
	    latexEncodings <- c(latexEncodings,
	                        sub("^\\\\inputencoding\\{(.*)\\}", "\\1", enc))
	}
        topics[f] <- this
    }

    topics <- topics[nzchar(topics)]
    summ <- grep("-package$", topics, perl = TRUE)
    topics <- if (length(summ)) c(topics[summ], re(topics[-summ])) else re(topics)
    for (f in names(topics)) writeLines(readLines(f), outcon)

    if (asChapter)
        cat("\\clearpage\n", file = outcon)

    invisible(latexEncodings)
}

## replacement for tools/Rdnewer.pl
.Rdnewer <- function(dir, file)
    q("no", status = ..Rdnewer(dir, file), runLast = FALSE)

..Rdnewer <- function(dir, file, OS = .Platform$OS.type)
{
    ## Test whether any Rd file in the 'man' and 'man/$OS'
    ## subdirectories of directory DIR is newer than a given FILE.
    ## Return 0 if such a file is found (i.e., in the case of
    ## 'success'), and 1 otherwise, so that the return value can be used
    ## for shell 'if' tests.

    ## <NOTE>
    ## For now only used for the R sources (/doc/manual/Makefile.in)
    ## hence no need to also look for Rd files with '.rd' extension.
    ## </NOTE>

    if (!file.exists(file)) return(0L)
    age <- file.info(file)$mtime

    if (any(file.info(c(Sys.glob(file.path(dir, "man", "*.Rd")),
                        Sys.glob(file.path(dir, "man", "*.rd")))
                      )$mtime > age))
        return(0L)

    if (isTRUE(file.info(file.path(dir, OS))$isdir)) {
        if (any(file.info(c(Sys.glob(file.path(dir, "man", OS, "*.Rd")),
                            Sys.glob(file.path(dir, "man", OS, "*.rd")))
                          )$mtime > age))
            return(0L)
    }

    1L
}

## called for base packages from src/Makefile[.win] and from
## .install.packages in this file.
.writePkgIndices <-
    function(dir, outDir, OS = .Platform$OS.type, html = TRUE)
{
    re <- function(x)
    {
        ## sort order for topics, a little tricky
        ## FALSE sorts before TRUE
        xx <- rep(TRUE, length(x))
        xx[grep("-package", x, fixed = TRUE)] <- FALSE
        order(xx, toupper(x), x)
    }

    html_header <- function(pkg, title, version, encoding, conn)
    {
        cat('<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">\n',
            '<html><head><title>R: ', title, '</title>\n',
            '<meta http-equiv="Content-Type" content="text/html; charset=',
            encoding, '">',
            '<link rel="stylesheet" type="text/css" href="../../R.css">\n',
            '</head><body>\n',
            '<h1>', title, ' <img class="toplogo" src="../../../doc/html/logo.jpg" alt="[R logo]"></h1>\n\n<hr>\n\n',
            '<div align="center">\n<a href="../../../doc/html/packages.html"><img src="../../../doc/html/left.jpg"\n',
            'alt="[Package List]" width="30" height="30" border="0"></a>\n',
            '<a href="../../../doc/html/index.html"><img src="../../../doc/html/up.jpg"\n',
            'alt="[Top]" width="30" height="30" border="0"></a>\n</div>\n\n',
            '<h2>Documentation for package &lsquo;', pkg, '&rsquo; version ',
            version, '</h2>\n\n', sep ='', file = conn)

        if (file.exists(file.path(outDir, "doc")))
		    cat('<h2>User Guides and Package Vignettes</h2>\n',
		        'Read <a href="../doc/index.html">overview</a> or ',
		        'browse <a href="../doc">directory</a>.\n\n',
	        sep = '', file=conn)

        cat('<h2>Help Pages</h2>\n\n\n',
            sep ='', file = conn)
    }

    firstLetterCategory <- function(x)
    {
        x[grep("-package$", x)] <- " "
        x <- toupper(substr(x, 1, 1))
        x[x > "Z"] <- "misc"
        x[x < "A" & x != " "] <- ""
        x
    }

    ## This may well already have been done:
    Rd <- if (file.exists(f <- file.path(outDir, "Meta", "Rd.rds")))
        .readRDS(f)
    else {
        ## Keep this in sync with .install_package_Rd_indices().
        ## Rd objects should already have been installed.
        db <- tryCatch(Rd_db(basename(outDir), lib.loc = dirname(outDir)),
                       error = function(e) NULL)
        ## If not, we build the Rd db from the sources:
        if (is.null(db)) db <- Rd_db(dir = dir)
        Rd <- Rd_contents(db)
        .saveRDS(Rd, file.path(outDir, "Meta", "Rd.rds"))
        Rd
    }

    topics <- Rd$Aliases
    M <- if (!length(topics)) {
        data.frame(Topic = character(),
                   File = character(),
                   Title = character(),
                   Internal = character(),
                   stringsAsFactors = FALSE)
    } else {
        lens <- sapply(topics, length)
        files <- sub("\\.[Rr]d$", "", Rd$File)
        internal <- sapply(Rd$Keywords, function(x) "internal" %in% x)
        data.frame(Topic = unlist(topics),
                   File = rep.int(files, lens),
                   Title = rep.int(Rd$Title, lens),
                   Internal = rep.int(internal, lens),
                   stringsAsFactors = FALSE)
    }
    ## FIXME duplicated aliases warning
    outman <- file.path(outDir, "help")
    dir.create(outman, showWarnings = FALSE)
    MM <- M[re(M[, 1]), 1:2]
    write.table(MM, file.path(outman, "AnIndex"),
                quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
    a <- structure(MM[, 2L], names=MM[, 1L])
    .saveRDS(a, file.path(outman, "aliases.rds"))

    ## no HTML indices if no help pages?
    outman <- file.path(outDir, "html")
    dir.create(outman, showWarnings = FALSE)
    outcon <- file(file.path(outman, "00Index.html"), "wt")
    on.exit(close(outcon))
    desc <- read.dcf(file.path(outDir, "DESCRIPTION"))[1,]
    ## re-encode if necessary
    if(!is.na(enc <- desc["Encoding"])) {
        ## should be valid in UTF-8, might be invalid in declared encoding
        desc <- iconv(desc, enc, "UTF-8", sub = "byte")
    }
    ## drop internal entries
    M <- M[!M[, 4], ]
    if (desc["Package"] %in% c("base", "graphics", "stats", "utils")) {
        for(pass in 1:2) {
            ## we skip method aliases
            gen <- gsub("\\.data\\.frame", ".data_frame", M$Topic)
            gen <- sub("\\.model\\.matrix$", ".modelmatrix", gen)
            gen <- sub("^(all|as|is|file|Sys|row|na|model)\\.", "\\1_", gen)
            gen <- sub("^(.*)\\.test", "\\1_test", gen)
            gen <- sub("([-[:alnum:]]+)\\.[^.]+$", "\\1", gen)
            last <- nrow(M)
            nongen <- gen %in% c("ar", "bw", "contr", "dyn", "lm", "qr", "ts", "which", ".Call", ".External", ".Library", ".First", ".Last")
            nc <- nchar(gen)
            asg <- (nc > 3) & substr(gen, nc-1, nc) == "<-"
            skip <- (gen == c("", gen[-last])) & (M$File == c("", M$File[-last])) & !nongen
            skip <- skip | asg
            ##N <- cbind(M$Topic, gen, c("", gen[-last]), skip)
            M <- M[!skip, ]
            M <- M[re(M[, 1]), ]
        }
    } else M <- M[re(M[, 1]), ]
    ## encode some entries.
    htmlize <- function(x, backtick)
    {
        x <- gsub("&", "&amp;", x, fixed = TRUE)
        x <- gsub("<", "&lt;", x, fixed = TRUE)
        x <- gsub(">", "&gt;", x, fixed = TRUE)
        if (backtick) {
            x <- gsub("---", "-", x, fixed = TRUE)
            x <- gsub("--", "-", x, fixed = TRUE)
            ## these have been changed in the Rd parser
            #x <- gsub("``", "&ldquo;", x, fixed = TRUE)
            #x <- gsub("''", "&rdquo;", x, fixed = TRUE)
            #x <- gsub("\\`([^']+)'", "&lsquo;\\1&rsquo;", x)
            #x <- gsub("`", "'", x, fixed = TRUE)
        }
        x
    }
    M$HTopic <- htmlize(M$Topic, FALSE)
    M$Title <- htmlize(M$Title, TRUE)

    ## No need to handle encodings: everything is in UTF-8

    html_header(desc["Package"], desc["Title"], desc["Version"], "UTF-8", outcon)

    use_alpha <- (nrow(M) > 100)
    if (use_alpha) {
        first <- firstLetterCategory(M$Topic)
        nm <- sort(names(table(first)))
        m <- match(" ", nm, 0L)
        if (m) nm <- c(" ", nm[-m])
        writeLines("<p align=\"center\">", outcon)
        writeLines(paste("<a href=\"#", nm, "\">", nm, "</a>", sep = ""),
                   outcon)
        writeLines("</p>\n", outcon)

        for (f in nm) {
            MM <- M[first == f, ]
            cat("\n<h2><a name=\"", f, "\">-- ", f, " --</a></h2>\n\n",
                sep = "", file = outcon)
            writeLines('<table width="100%">', outcon)
            writeLines(paste('<tr><td width="25%"><a href="', MM[, 2], '.html">',
                             MM$HTopic, '</a></td>\n<td>', MM[, 3],'</td></tr>',
                             sep = ''), outcon)
            writeLines("</table>", outcon)
       }
    } else if (nrow(M)) {
        writeLines('<table width="100%">', outcon)
        writeLines(paste('<tr><td width="25%"><a href="', M[, 2], '.html">',
                         M$HTopic, '</a></td>\n<td>', M[, 3],'</td></tr>',
                         sep = ''), outcon)
        writeLines("</table>", outcon)
    } else { # no rows
         writeLines("There are no help pages in this package", outcon)
    }
    writeLines('</body></html>', outcon)
}

### * .convertRdfiles

## possible types are "html", "latex", "example"
## outenc is used as the default output encoding for latex conversion
.convertRdfiles <-
    function(dir, outDir, types = "html", silent = FALSE, outenc = "latin1")
{
    showtype <- function(type) {
    	if (!shown) {
            nc <- nchar(bf)
            if (nc < 38L)
                cat("    ", bf, rep(" ", 40L - nc), sep = "")
            else
                cat("    ", bf, "\n", rep(" ", 44L), sep = "")
            shown <<- TRUE
        }
        ## 'example' is always last, so 5+space
        cat(type, rep(" ", max(0L, 6L - nchar(type))), sep="")
    }

    dirname <- c("html", "latex", "R-ex")
    ext <- c(".html", ".tex", ".R", ".html")
    names(dirname) <- names(ext) <- c("html", "latex", "example")
    mandir <- file.path(dir, "man")
    if (!file_test("-d", mandir)) return()
    desc <- .readRDS(file.path(outDir, "Meta", "package.rds"))$DESCRIPTION
    pkg <- desc["Package"]
    ver <- desc["Version"]

    for(type in types)
        dir.create(file.path(outDir, dirname[type]), showWarnings = FALSE)

    cat("  converting help for package ", sQuote(pkg), "\n", sep="")

    ## FIXME: add this lib to lib.loc?
    if ("html" %in% types) {
        ## may be slow, so add a message
        if (!silent) message("    finding HTML links ...", appendLF = FALSE)
        Links <- findHTMLlinks(outDir, level = 0:1)
        if (!silent) message(" done")
        .Links2 <- function() {
            message("\n    finding level-2 HTML links ...", appendLF = FALSE)
            Links2 <- findHTMLlinks(level = 2)
            message(" done")
            Links2
        }
        delayedAssign("Links2", .Links2())
    }

    ## Rd objects may already have been installed.
    db <- tryCatch(Rd_db(basename(outDir), lib.loc = dirname(outDir)),
                   error = function(e) NULL)
    ## If not, we build the Rd db from the sources:
    if (is.null(db)) db <- Rd_db(dir = dir)

    if (!length(db)) return()

    files <- names(db)

    .whandler <-  function(e) {
        .messages <<- c(.messages,
                        paste("Rd warning:", conditionMessage(e)))
        invokeRestart("muffleWarning")
    }
    .ehandler <- function(e) {
        message("") # force newline
        unlink(ff)
        stop(conditionMessage(e), domain = NA, call. = FALSE)
    }
    .convert <- function(expr)
        withCallingHandlers(tryCatch(expr, error = .ehandler),
                            warning = .whandler)

    for(f in files) {
        .messages <- character()
        Rd <- db[[f]]
        attr(Rd, "source") <- NULL
        bf <- sub("\\.[Rr]d$", "", basename(f))

        shown <- FALSE

        if ("html" %in% types) {
            type <- "html"
            ff <- file.path(outDir, dirname[type],
                            paste(bf, ext[type], sep = ""))
            if (!file_test("-f", ff) || file_test("-nt", f, ff)) {
                showtype(type)
                ## assume prepare_Rd was run when dumping the .rds
                ## so use defines = NULL for speed
                .convert(Rd2HTML(Rd, ff, package = c(pkg, ver),
                                 defines = NULL,
                                 Links = Links, Links2 = Links2))
            }
        }
        if ("latex" %in% types) {
            type <- "latex"
            ff <- file.path(outDir, dirname[type],
                            paste(bf, ext[type], sep = ""))
            if (!file_test("-f", ff) || file_test("-nt", f, ff)) {
                showtype(type)
                .convert(Rd2latex(Rd, ff, defines = NULL,
                                  outputEncoding = outenc))
            }
        }
        if ("example" %in% types) {
            type <- "example"
            ff <- file.path(outDir, dirname[type],
                            paste(bf, ext[type], sep = ""))
            if (!file_test("-f", ff) || file_test("-nt", f, ff)) {
                .convert(Rd2ex(Rd, ff, defines = NULL))
                if (file_test("-f", ff)) showtype(type)
            }
        }
        if (shown) {
            cat("\n")
            if (length(.messages)) writeLines(unique(.messages))
        }
    }

    ## Now check for files to remove.
    bfs <- sub("\\.[Rr]d$", "", basename(files)) # those to keep
    if ("html" %in% types) {
        type <- "html"
        have <- list.files(file.path(outDir, dirname[type]))
        have2 <- sub("\\.html", "", basename(have))
        drop <- have[! have2 %in% c(bfs, "00Index")]
        unlink(file.path(outDir, dirname[type], drop))
    }
    if ("latex" %in% types) {
        type <- "latex"
        have <- list.files(file.path(outDir, dirname[type]))
        have2 <- sub("\\.tex", "", basename(have))
        drop <- have[! have2 %in% bfs]
        unlink(file.path(outDir, dirname[type], drop))
    }
    if ("example" %in% types) {
        type <- "example"
        have <- list.files(file.path(outDir, dirname[type]))
        have2 <- sub("\\.R", "", basename(have))
        drop <- have[! have2 %in% bfs]
        unlink(file.path(outDir, dirname[type], drop))
    }

}

### * .makeDllRes

.makeDllRes <-
function(name="", version = "0.0")
{
    if (file.exists(f <- "../DESCRIPTION") ||
        file.exists(f <- "../../DESCRIPTION")) {
        desc <- read.dcf(f)[[1]]
        if (!is.na(f <- desc["Package"])) name <- f
        if (!is.na(f <- desc["Version"])) version <- f
    }
    writeLines(c('#include <windows.h>',
                 '#include "Rversion.h"',
                 '',
                 'VS_VERSION_INFO VERSIONINFO',
                 'FILEVERSION R_FILEVERSION',
                 'PRODUCTVERSION 3,0,0,0',
                 'FILEFLAGSMASK 0x3L',
                 'FILEOS VOS__WINDOWS32',
                 'FILETYPE VFT_APP',
                 'BEGIN',
                 '    BLOCK "StringFileInfo"',
                 '    BEGIN',
                 '        BLOCK "040904E4"',
                 '        BEGIN'))
    cat("            VALUE \"FileDescription\", \"DLL for R package `", name,"'\\0\"\n",
        "            VALUE \"FileVersion\", \"", version, "\\0\"\n", sep="")
    writeLines(c(
                 '            VALUE "Compiled under R Version", R_MAJOR "." R_MINOR " (" R_YEAR "-" R_MONTH "-" R_DAY ")\\0"',
                 '            VALUE "Project info", "http://www.r-project.org\\0"',
                 '        END',
                 '    END',
                 '    BLOCK "VarFileInfo"',
                 '    BEGIN',
                 '        VALUE "Translation", 0x409, 1252',
                 '    END',
                 'END'))
}

### * .Rdconv

## replacement R code for Perl-based R CMD Rdconv

.Rdconv <- function(args = NULL)
{
    Usage <- function() {
        cat("Usage: R CMD Rdconv [options] FILE",
            "",
            "Convert R documentation in FILE to other formats such as plain text,",
            "HTML or LaTeX.",
            "",
            "Options:",
            "  -h, --help		print short help message and exit",
            "  -v, --version		print version info and exit",
            "  -t, --type=TYPE	convert to format TYPE",
            "  --encoding=enc        use 'enc' as the output encoding",
            "  --package=pkg         use 'pkg' as the package name",
            "  -o, --output=OUT	use 'OUT' as the output file",
            "      --os=NAME		assume OS 'NAME' (unix or windows)",
            "      --OS=NAME		the same as '--os'",
            "",
            "Possible format specifications are 'txt' (plain text), 'html', 'latex',",
            "and 'example' (extract R code in the examples).",
            "",
            "The default is to send output to stdout, which is also given by '-o -'.",
            "Using '-o \"\"' will choose an output filename by removing a '.Rd'",
            "extension from FILE and adding a suitable extension.",
            "",
            "Report bugs to <r-bugs@r-project.org>.", sep = "\n")
    }

    options(showErrorCalls = FALSE, warn = 1)
    files <- character(0L)
    type <- "unknown"
    enc <- ""
    pkg <- ""
    out <- NULL
    os <- ""

    if (is.null(args)) {
        args <- commandArgs(TRUE)
        ## it seems that splits on spaces, so try harder.
        args <- paste(args, collapse=" ")
        args <- strsplit(args,'nextArg', fixed = TRUE)[[1L]][-1L]
    }

    while(length(args)) {
        a <- args[1L]
        if (a %in% c("-h", "--help")) {
            Usage()
            q("no", runLast = FALSE)
        }
        else if (a %in% c("-v", "--version")) {
            cat("Rdconv: ",
                R.version[["major"]], ".",  R.version[["minor"]],
                " (r", R.version[["svn rev"]], ")\n", sep = "")
            cat("",
                "Copyright (C) 1997-2009 The R Core Development Team.",
                "This is free software; see the GNU General Public License version 2",
                "or later for copying conditions.  There is NO warranty.",
                sep="\n")
            q("no", runLast = FALSE)
        } else if (a == "-t") {
            if (length(args) >= 2) {type <- args[2]; args <- args[-1]}
            else stop("-t option without value", call. = FALSE)
        } else if (substr(a, 1, 7) == "--type=") {
            type <- substr(a, 8, 1000)
        } else if (substr(a, 1, 10) == "--encoding=") {
            enc <- substr(a, 11, 1000)
        } else if (substr(a, 1, 10) == "--package=") {
            pkg <- substr(a, 11, 1000)
        } else if (a == "-o") {
            if (length(args) >= 2) {out <- args[2]; args <- args[-1]}
            else stop("-o option without value", call. = FALSE)
        } else if (substr(a, 1, 9) == "--output=") {
            out <- substr(a, 10, 1000)
        } else if (substr(a, 1, 5) %in% c("--os=", "--OS=")) {
            os <- substr(a, 6, 1000)
        } else if (substr(a, 1, 1) == "-") {
            message("Warning: unknown option ", sQuote(a))
        } else files <- c(files, a)
        args <- args[-1L]
    }
    if (length(files) != 1L)
        stop("exactly one Rd file must be specified", call. = FALSE)
    if (is.character(out) && !nzchar(out)) {
        ## choose 'out' from filename
        bf <- sub("\\.[Rr]d$", "", file)
        exts <- c(txt=".txt", html=".html", latex=".tex", exmaple=".R")
        out <- paste(bf,  exts[type], sep = "")
    } else if (is.null(out)) out <- ""
    if (!nzchar(os)) os <- .Platform$OS.type
    switch(type,
           "txt" = {
               Rd2txt(files, out, package=pkg, defines=os,
                      outputEncoding = enc)
           },
           "html" = {
               if (!nzchar(enc)) enc <- "UTF-8"
               Rd2HTML(files, out, package = pkg, defines = os,
                       outputEncoding = enc, no_links = TRUE)
           },
           "latex" = {
               if (!nzchar(enc)) enc <- "latin1"
               Rd2latex(files, out, defines = os,
                        outputEncoding = enc)
           },
           "example" = {
               if (!nzchar(enc)) enc <- "UTF-8"
               Rd2ex(files, out, defines = os, outputEncoding = enc)
           },
           "unknown" = stop("no 'type' specified", call. = FALSE),
           stop("'type' must be one of 'txt', 'html', 'latex' or 'example'",
                call. = FALSE)
           )
    invisible()
}

### * .Rd2dvi

.Rd2dvi <-
function(pkgdir, outfile, is_bundle, title, batch = FALSE,
         description = TRUE, only_meta = FALSE,
         enc = "unknown", outputEncoding = "latin1", files_or_dir, OSdir,
         internals = "no", index = "true")
{
    # print(match.call())

    ## %in% and others cause problems for some page layouts.
    if (basename(pkgdir) == "base") index <- "false"
    # Write directly to the final location.  Encodings may mean we need
    # to make edits, but for most files one pass should be enough.
    out <- file(outfile, "wt")
    if (!nzchar(enc)) enc <- "unknown"
    description <- description == "true"
    only_meta <- only_meta == "true"
    internals <- internals != "no"
    index <- index != "false"

    desc <- NULL
    if (file.exists(f <- file.path(pkgdir, "DESCRIPTION"))) {
        desc <- read.dcf(f)[1,]
        if (enc == "unknown") {
            pkg_enc <- desc["Encoding"]
            if (!is.na(pkg_enc)) {
            	enc <- pkg_enc
            	outputEncoding <- pkg_enc
            }
        }
    }

    ## Rd2.tex part 1: header
    if (batch == "true") writeLines("\\nonstopmode{}", out)
    cat("\\documentclass[", Sys.getenv("R_PAPERSIZE"), "paper]{book}\n",
        "\\usepackage[", Sys.getenv("R_RD4DVI", "ae"), "]{Rd}\n",
        sep = "", file = out)
    if (index) writeLines("\\usepackage{makeidx}", out)
    ## this needs to be canonical, e.g. 'utf8'
    setEncoding <- paste("\\usepackage[", latex_canonical_encoding(outputEncoding), "]{inputenc} % @SET ENCODING@", sep="")
    writeLines(c(setEncoding,
                 "\\makeindex{}",
                 "\\begin{document}"), out)
    if (is_bundle == "no") {
        if (!nzchar(title)) {
            if (is.character(desc))
                title <- paste("Package `", desc["Package"], "'", sep = "")
            else if (file.exists(f <- file.path(pkgdir, "DESCRIPTION.in"))) {
                desc <- read.dcf(f)[1,]
                title <- paste("Package `", desc["Package"], "'", sep = "")
            } else {
                if (file_test("-d", pkgdir)) {
                    subj <- paste("all in \\file{", pkgdir, "}", sep ="")
                } else {
                    files <- strsplit(files_or_dir, "[[:space:]]+")[[1]]
                    subj1 <- if (length(files) > 1) " etc." else ""
                    subj <- paste("\\file{", pkgdir, "}", subj1, sep = "")
                }
                subj <- gsub("[_$]", "\\\\1", subj)
                title <- paste("\\R{} documentation}} \\par\\bigskip{{\\Large of", subj)
            }
        }
        cat("\\chapter*{}\n",
            "\\begin{center}\n",
            "{\\textbf{\\huge ", title, "}}\n",
            "\\par\\bigskip{\\large \\today}\n",
            "\\end{center}\n", sep = "", file = out)
        if (description && file.exists(f <- file.path(pkgdir, "DESCRIPTION")))
            .DESCRIPTION_to_latex(f, out)
        ## running on the sources of a base package will have DESCRIPTION.in,
        ## only.
        if (description &&
           file.exists(f <- file.path(pkgdir, "DESCRIPTION.in"))) {
            version <- readLines(file.path(pkgdir, "../../../VERSION"))
            .DESCRIPTION_to_latex(file.path(pkgdir, "DESCRIPTION.in"),
                                  out, version)
        }
    } else { ## bundle case
        if (!nzchar(title) && is.character(desc))
            title <- paste("Bundle `", desc["Bundle"], "'", sep = "")
        cat("\\pagenumbering{Roman}\n",
            "\\begin{titlepage}\n",
            "\\strut\\vfill\n",
            "\\begin{center}\n",
            "{\\textbf{\\Huge ", title, "}}\n",
            "\\par\\bigskip{\\large \\today}\n",
            "\\end{center}\n",
            "\\par\\bigskip\n", sep = "", file = out)
        if (description)
            .DESCRIPTION_to_latex(file.path(pkgdir, "DESCRIPTION"), out)
        writeLines("\\vfill\\vfill\n\\end{titlepage}", out)
    }

    ## Rd2.tex part 2: body
    toc <- if (file_test("-d", files_or_dir)) {
        "\\Rdcontents{\\R{} topics documented:}"
    } else ""

    latexEncodings <- character(0)
    if (is_bundle == "no") {
        ## if this looks like a package with no man pages, skip body
        if (file.exists(file.path(pkgdir, "DESCRIPTION")) &&
           !(file_test("-d", file.path(pkgdir, "man")) ||
             file_test("-d", file.path(pkgdir, "help")) ||
             file_test("-d", file.path(pkgdir, "latex")))) only_meta <- TRUE
        if (!only_meta) {
            if (nzchar(toc)) writeLines(toc, out)
            latexEncodings <-
                .Rdfiles2tex(files_or_dir, out, encoding = enc, append = TRUE,
                             extraDirs = OSdir, internals = internals,
                             silent = (batch == "true"))
        }
    } else {
        writeLines(c("\\setcounter{secnumdepth}{-1}",
                     "\\pagenumbering{roman}",
                     "\\tableofcontents{}",
                     "\\cleardoublepage{}",
                     "\\pagenumbering{arabic}"), out)
        desc <- read.dcf(file.path(pkgdir, "DESCRIPTION"))[1,]
        bundle_pkgs <- .get_contains_from_package_db(desc)
        for (p in bundle_pkgs) {
            message("Bundle package: ", p)
            cat("\\chapter{Package `", p, "'}\n", sep = "", file = out)
            if (description &&
                file.exists(f <- file.path(pkgdir, p, "DESCRIPTION.in")))
                .DESCRIPTION_to_latex(f, out)
            if (!only_meta)
                latexEncodings <- c(latexEncodings, .pkg2tex(file.path(pkgdir, p), out, encoding = enc,
                         append = TRUE, asChapter = FALSE,
                         internals = internals))
            writeLines("\\clearpage{}", out)
        }
        writeLines("\\cleardoublepage{}", out)
    }

    ## Rd2.tex part 3: footer
    if (index) writeLines("\\printindex{}", out)
    writeLines("\\end{document}", out)
    close(out)

    ## Fix up encodings
    ## FIXME cyrillic probably only works with times, not ae.
    latexEncodings <- unique(latexEncodings)
    latexEncodings <- latexEncodings[!is.na(latexEncodings)]
    cyrillic <- if (nzchar(Sys.getenv("_R_CYRILLIC_TEX_"))) "utf8" %in% latexEncodings else FALSE
    latex_outputEncoding <- latex_canonical_encoding(outputEncoding)
    encs <- latexEncodings[latexEncodings != latex_outputEncoding]
    if (length(encs) || cyrillic) {
        lines <- readLines(outfile)
	encs <- paste(encs, latex_outputEncoding, collapse=",", sep=",")

	if (!cyrillic) {
	    lines[lines == setEncoding] <-
		paste("\\usepackage[", encs, "]{inputenc}", sep = "")
	} else {
	    lines[lines == setEncoding] <-
		paste(
"\\usepackage[", encs, "]{inputenc}
\\IfFileExists{t2aenc.def}{\\usepackage[T2A]{fontenc}}{}", sep = "")
	}
	writeLines(lines, outfile)
    }

    invisible(NULL)
}


### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
#  File src/library/tools/R/license.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

## <NOTE>
## We want *standardized* license specs so that we can compute on them.
## In particular, we want to know whether licenses are recognizable as
## free (FSF) or open source (OSI) licenses.
## With the current standardization scheme, standardized license specs
## specify free or open source software licenses or refer to LICENSE or
## LICENCE files (requiring inspection by maintainers or installers).
## AGPL is a particular nuisance: the FSF considers it a free software
## license, but additional (e.g., attribution) clauses make it necessary
## for installers to inspect these.
## The R distribution contains a basic free or open source software
## license db, but it would be good to have a more extensible mechanism
## eventually.
##
## See in particular
##   http://www.fsf.org/licensing/licenses
##   http://en.wikipedia.org/wiki/List_of_FSF_approved_software_licences
##   http://en.wikipedia.org/wiki/List_of_OSI_approved_software_licences
## for more information.
## </NOTE>

re_anchor <-
function(s)
    if(length(s)) paste("^", s, "$", sep = "") else character()

re_group <-
function(s)
    if(length(s)) paste("(", s, ")", sep = "") else character()

re_or <-
function(s, group = TRUE) {
    if(!length(s))
        character()
    else if(group)
        re_group(paste(s, collapse = "|"))
    else
        paste(s, collapse = "|")
}

.make_license_regexps <-
function()
{
    ## Build license regexps according to the specs.

    ## Read in the license data base, and precompute some variables.
    license_db <-
        read.dcf(file.path(R.home("share"), "licenses", "license.db"))
    license_db[is.na(license_db)] <- ""
    ## (Could also keeps NAs and filter on is.finite().)
    license_db <- data.frame(license_db, stringsAsFactors = FALSE)

    has_abbrev <- nzchar(license_db$Abbrev)
    has_version <- nzchar(license_db$Version)

    license_short_specs <-
        unique(c(Filter(nzchar, license_db$SSS),
                 do.call(paste,
                         c(license_db[has_abbrev & has_version,
                                      c("Abbrev", "Version")],
                           list(sep = "-")))))
    license_names_or_abbrevs_without_version <-
        Filter(nzchar,
               unlist(subset(license_db, Version == "",
                             c("Name", "Abbrev")),
                      use.names = FALSE))
    license_names_or_abbrevs_with_version <-
        unique(Filter(nzchar,
                      unlist(subset(license_db, Version != "",
                                    c("Name", "Abbrev")),
                             use.names = FALSE)))

    operators <- c("<", "<=", ">", ">=", "==", "!=")
    re_for_numeric_version <- .standard_regexps()$valid_numeric_version
    re_for_single_version_spec <-
        paste("[[:space:]]*",
              re_or(operators),
              "[[:space:]]*",
              re_for_numeric_version,
              "[[:space:]]*",
              sep = "")
    re_for_version_spec <-
        paste("\\(",
              paste("(", re_for_single_version_spec, ",)*", sep = ""),
              re_for_single_version_spec,
              "\\)",
              sep = "")
    re_for_free_or_open_software_spec <-
        re_or(c(re_or(license_names_or_abbrevs_without_version),
                ## We currently considers names or abbrevs of versioned
                ## licenses, *possibly* followed by a version spec, as
                ## canonical.  This is not quite perfect, as ideally a
                ## version spec should be provided in case it matters.
                ## Let us use the interpretation that no version spec
                ## means "any version" (which is correct for GPL).
                paste(re_or(license_names_or_abbrevs_with_version),
                      "[[:space:]]*",
                      paste("(", re_for_version_spec, ")*", sep = ""),
                      sep = ""),
                ## Also allow for things like
                ##   GNU General Public License version 2
                ##   Apache License Version 2.0
                ## as one can argue that these are really the full names
                ## of these licenses.
                re_or(paste(license_db$Name[has_version],
                            "[[:space:]]+([Vv]ersion[[:space:]]+)?",
                            license_db$Version[has_version],
                            sep = ""))))

    re_for_license_short_spec <- re_or(license_short_specs)
    re_for_license_file <- "file LICEN[CS]E"
    re_for_license_extension <-
        sprintf("[[:space:]]*\\+[[:space:]]*%s", re_for_license_file)
    ## <NOTE>
    ## Many standard licenses actually do not allow extensions.
    ## Ideally, we would only allow the extension markup for extensible
    ## standard licenses, as identified via an Extensible: TRUE field in
    ## the license db.  But version ranges make this tricky: e.g.,
    ##   GPL (>= 2) + file LICENSE
    ## is not right as GPL-2 does not allow extensions ...
    ## Hence, for now allow the extension markup with all standard
    ## licenses.
    ## </NOTE>
    re_for_component <-
        re_anchor(re_or(c(sprintf("%s(%s)?",
                                  re_or(c(re_for_license_short_spec,
                                          re_for_free_or_open_software_spec)),
                                  re_for_license_extension),
                          re_for_license_file,
                          "Unlimited")))
    list(re_for_component = re_for_component,
         re_for_license_file = re_for_license_file)
}

license_regexps <- .make_license_regexps()

## Standardizable and other free or open source software license specs:

## License specifications found on CRAN/BioC/Omegahat and manually
## classified as standardizable (hence currently free or open source)
## software licenses (even though not standardized/canonical), provided
## as a list of license specs named by the respective standardizations.
## With ongoing standardization this should gradually be eliminated.
## Last updated: 2009-02-19.

## Nasty issues.
## * There really is no GPL version 2.0.
##   Unfortunately, the FSF uses 2.0 in URLs or links
##   (http://www.gnu.org/licenses/old-licenses/gpl-2.0.html)
##   The text clearly says "Version 2, June 1991".
## * There really is no LGPL version 2.0.
##   Unfortunately, the FSF uses 2.0 in URLs or links
##   (http://www.gnu.org/licenses/old-licenses/).
##   The text clearly says "Version 2, June 1991".
## * CeCILL is a bit of a mess: the current version is referred to as
##   "version 2" (http://www.cecill.info/licences.en.html) but
##    internally uses "Version 2.0 dated 2006-09-05" 
##    (http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt).

.standardizable_license_specs <-
list("Artistic-2.0" =
     c("The Artistic License, Version 2.0",
       "Artistic 2.0",
       "Artistic-2.0, see http://www.opensource.org/licenses/artistic-license-2.0.php"
       ),

     "CeCILL-2" =
     c("CeCILL-2.0"
       ),
     
     "GPL" =
     c("GNU Public License",
       "Gnu GPL",
       "GNU GPL",
       "GPL (http://www.gnu.org/copyleft/gpl.html)"
       ),
     
     "GPL-2" =
     c(## <NOTE>
       ## There is no GPL 2.0, see above.
       "GNU General Public License 2.0.",
       "GPL 2.0",
       "GPL version 2.0",
       "GPL2.0",
       ## </NOTE>
       "GPL Version 2",
       "GNU GPL Version 2",
       "GNU GPL version 2",
       "GNU GPL version 2.",
       "GPL (version 2)",
       "GPL 2",
       "GPL 2.",
       "GPL version 2",
       "GPL version 2 (June, 1991)",
       "GPL version 2.",
       "GPL2",
       ## BioC:
       "GPL V2",
       "GPL, version 2"
       ),
     
     "GPL-3" =
     c("GPL Version 3",
       "GPL version 3",
       "GNU General Public Licence (GPLv3)",
       "GPL 3",
       "GPL v3"
       ),

     "GPL (>= 2)" =
     c(## <NOTE>
       ## There is no GPL 2.0, see above.
       "GNU GPL v2.0 or greater",
       "GPL 2.0 or higher",
       "GPL 2.0 or newer",
       "GPL version 2.0 or later",
       "GPL version 2.0 or newer",
       ## </NOTE>
       "GNU GPL (version 2 or later)",
       "GNU GPL (version 2 or later); see the file COPYING for details",
       "GNU GPL version 2 or newer",
       "GNU General Public License version 2 or newer",
       "GPL version 2 or later",
       "GPL ( version 2 or later)",
       "GPL (Version 2 or above)",
       "GPL (Version 2 or later)",
       "GPL (version 2 or higher)",
       "GPL (version 2 or later)",
       "GPL (version 2 or later, see the included file GPL)",
       "GPL (version 2 or newer)",
       "GPL 2 or later",
       "GPL 2 or newer",
       "GPL version 2 or any later version",
       "GPL Version 2 or later",
       "GPL Version 2 or later.",
       "GPL Version 2 or newer",
       "GPL Version 2 or newer.",
       "GPL version 2 (June, 1991) or later",
       "GPL version 2 (or newer)",
       "GPL version 2 or later.",
       "GPL version 2 or newer",
       "GPL version 2 or newer (http://www.gnu.org/copyleft/gpl.html)",
       "GPL version 2 or newer (see README).",
       "GPL version 2 or newer.",
       "GPL version 2 or newer. http://www.gnu.org/copyleft/gpl.html",
       "GPL version 2, or, at your option, any newer version.",
       "GPL Version 2 (or later)",
       "GPL version 2 (or later)",
       "GPL version 2 or higher",
       "GPL2 or later",
       "GPL>=2",
       "GNU General Public License (version 2 or later)"
       ),

     "GPL (>= 3)" =
     c("GPL (version 3 or later)",
       "GPL >=3"
       ),

     "GPL | LGPL" =
     c("GPL or LGPL by your choice"
       ),

     "GPL | BSD" =
     c("GPL, BSD"
       ),

     "GPL-2 | file LICENSE" =
     c("use under GPL2, or see file LICENCE"
       ),
     
     "LGPL" =
     c("LGPL (see <http://www.opensource.org/licenses/lgpl-license.php>).",
       "GNU LGPL (same as wxWidgets)."
       ),
     
     "LGPL-2" =
     c("LGPL2",
       "LGPL2.0"
       ),
     
     "LGPL-2.1" =
     c("LGPL version 2.1"
       ),

     "LGPL-3" =
     c("LGPL-v3"
       ),
       
     "LGPL (>= 2.0)" =
     c(## <NOTE>
       ## There is no LGPL-2.0, see above.
       "LGPL >= 2.0",
       ## </NOTE>
       "LGPL Version 2 or later.",
       "LGPL version 2 or newer",
       "LGPL (version 2 or later)",
       "LGPL version 2 or later"
       ),
     
     "LGPL (>= 2.1)" =
     c("LGPL version 2.1 or later"
       ),

     "LGPL (>= 3.0)" =
     c("LGPL >=3"
       ),

     "X11" =
     c("X11 (http://www.x.org/Downloads_terms.html)"
       ),

     "Unlimited" =
     c("Unlimited use and distribution."
       )
)

.standardizable_license_specs_db <-
data.frame(ispecs =
           unlist(.standardizable_license_specs),
           ospecs =
           rep.int(names(.standardizable_license_specs),
                   sapply(.standardizable_license_specs,
                          length)),
           stringsAsFactors = FALSE)

## These used to be in .safe_license_specs_in_standard_repositories:
##    "Artistic",
##    "GPL AFFERO 3.0 (with citation)",
##    "caBIG"
##    "Unlimited distribution.",
## These are safe from a distribution point of view, but clearly not
## standardizable ... hence:
## A list of license specs we cannot standardize, but safely classify as
## free or open source software licenses:
.other_free_or_open_license_specs <-
c("Artistic",
  "GPL AFFERO 3.0 (with citation)",
  ## https://cabig-kc.nci.nih.gov/CTMS/KC/index.php/C3PR_caBIG_License  
  "caBIG"
  )

.safe_license_specs <-
    c(.standardizable_license_specs_db$ispecs,
      .other_free_or_open_license_specs)

analyze_license <-
function(x)
{
    .make_results <- function(is_empty = FALSE,
                              is_canonical = FALSE,
                              bad_components = character(),
                              is_standardizable = FALSE,
                              is_verified = FALSE,
                              standardization = NA_character_,
                              is_extended = NA,
                              pointers = NULL)
        list(is_empty = is_empty,
             is_canonical = is_canonical,
             bad_components = bad_components,
             is_standardizable = is_standardizable,
             is_verified = is_verified,
             standardization = standardization,
             is_extended = is_extended,
             pointers = pointers)


    x <- .strip_whitespace(x)
    if(is.na(x) || (x == "")) {
        ## Not really a lot to check ...
        ## (Note that non-standardizable license specs are dropped by
        ## writePACKAGES() and friends.)
        return(.make_results(is_empty = TRUE))
    }

    ## Try splitting into the individual components.
    components <-
        .strip_whitespace(unlist(strsplit(x, "|", fixed = TRUE)))

    ## Now analyze the individual components.
    ok <- grepl(license_regexps$re_for_component, components)
    bad_components <- components[!ok]

    ## Is the license specification "safe" in the sense of automatically
    ## verifiable as a free or open source software license?
    ## For the time being, test whether the spec is canonical and
    ## different from just a pointer to a license file, or in the list
    ## of safe specifications derived from license specifications in the
    ## standard repositories.
    is_verified <-
        ((all(ok)
          && !all(grepl(re_anchor(license_regexps$re_for_license_file),
                        components)))
         || all(components %in% .safe_license_specs))

    ## Is the license specification standardizable?
    standardizable <-
        components %in% .standardizable_license_specs_db$ispecs
    is_standardizable <- (all(ok) || all(standardizable))

    standardization <- if(is_standardizable) {
        ## Standardize the ones which are standardizable but not yet
        ## standardized.
        ind <- !ok & standardizable
        if(any(ind))
            components[ind] <-
                .standardize_license_components(components[ind])
        ## Canonicalize the standardized ones a bit more (as we are
        ## rather generous about using whitespace).
        ind <- ok & grepl("\\(", components)
        if(any(ind)) {
            s <- sub("[[:space:]]*\\(", " \\(", components[ind])
            s <- gsub("[[:space:]]*,[[:space:]]*", ", ", s)
            ## Really re_or(operators) ...
            s <- gsub("[[:space:]]+(<=?|>=?|==|!=)", " \\1", s)
            components[ind] <-
                gsub(sprintf("[[:space:]]*(%s)",
                             .standard_regexps()$valid_numeric_version),
                     " \\1", s)
        }
        paste(components, collapse = " | ")
    } else NA_character_

    pointers <- NULL
    is_extended <- NA
    ## Analyze components provided that we know we can standardize.
    if(is_standardizable) {
        components <- 
            .strip_whitespace(unlist(strsplit(standardization, "|",
                                              fixed = TRUE)))
        ind <- grep(sprintf("%s$",
                            license_regexps$re_for_license_file),
                     components)
        if(length(ind)) {
            pointers <- sub(".*file ", "", components[ind])
            is_extended <-
                any(grepl("+", components[ind], fixed = TRUE))
        } else {
            is_extended <- FALSE
        }
    }
    
    .make_results(is_canonical = all(ok),
                  bad_components = bad_components,
                  is_standardizable = is_standardizable,
                  is_verified = is_verified,
                  standardization = standardization,
                  is_extended = is_extended,
                  pointers = pointers)
}

.standardize_license_components <-
function(x)
{
    with(.standardizable_license_specs_db,
         ospecs[match(x, ispecs)])
}

analyze_licenses <-
function(x)
{
    x <- as.character(x)
    if(!length(x)) return(NULL)
    ## As analyzing licenses is costly, only analyze the unique specs.
    v <- unique(x)
    status <- lapply(v, analyze_license)
    ## And now turn into a data frame.
    out <- data.frame(matrix(0, length(v), 0L))
    for(j in seq_along(status[[1L]]))
        out[[j]] <- sapply(status, "[[", j)
    names(out) <- names(status[[1L]])
    ## And re-match specs to the unique specs.
    out <- out[match(x, v), ]
    rownames(out) <- NULL
    out
}

build_license_db <-
function(dir, unpacked = FALSE)
{
    ## Note that currently (2007-10-20), the license info is *NOT*
    ## written into the repository db (file 'PACKAGES') anymore.

    CRAN <- getOption("repos")["CRAN"]
    if(missing(dir) && substring(CRAN, 1L, 7L) == "file://")
        dir <- file.path(substring(CRAN, 8L), "src", "contrib")

    fields <- c("License", "Maintainer")
    db <- .build_repository_package_db(dir, fields, unpacked = unpacked)
    ## Actually, for Omegehat this is not a good idea as this retains
    ## old versions in the "main" src/contrib directory.  But let's not
    ## worry about this for now ...

    db <- do.call("rbind", db)
    is_bundle <- is.na(db[, "Package"])
    db[is_bundle, "Package"] <- db[is_bundle, "Bundle"]

    ## Retain what is needed ...
    data.frame(db[ , c("Package", "Version", fields)],
               stringsAsFactors = FALSE)
}

analyze_licenses_in_license_db <-
function(db)
    cbind(db, analyze_licenses(db$License))

analyze_licenses_in_repository <-
function(dir, unpacked = FALSE, full = TRUE)
{
    db <- build_license_db(dir, unpacked)
    if(!full) {
        ## Only keep the highest available versions.
        ## Such an option might be useful for build_license_db()
        ## itself.
        db <- .remove_stale_dups(db)
    }
    analyze_licenses_in_license_db(db)
}

summarize_license_db <-
function(db)
{
    packages <- db$Package
    if(any(duplicated(packages)))
        packages <- sprintf("%s_%s", packages, db$Version)
    packages <- split(packages, db$License)
    licenses <- names(packages)
    out <- data.frame(Licenses = licenses, stringsAsFactors = FALSE)
    ## To get the 'packages' list into a data frame without I() ...
    out$Packages <- packages
    cat(formatDL(out$Licenses,
                 sapply(out$Packages,
                        function(p) paste(unique(p), collapse = " ")),
                 style = "list"),
        sep = "\n\n")
    invisible(out)
}

find_unused_safe_license_specs <-
function(...)
{
    ldb <- do.call("rbind", list(...))
    .safe_license_specs %w/o% unique(ldb$License)
}

find_canonical_safe_license_specs <-
function()
{
    grep(license_regexps$re_for_component,
         .safe_license_specs,
         value = TRUE)
}
#  File src/library/tools/R/makeLazyLoad.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

code2LazyLoadDB <-
    function(package, lib.loc = NULL,
             keep.source = getOption("keep.source.pkgs"),
             compress = TRUE)
{
    pkgpath <- .find.package(package, lib.loc, quiet = TRUE)
    if(!length(pkgpath))
        stop(gettextf("there is no package called '%s'", package),
             domain = NA)
    barepackage <- sub("([^-]+)_.*", "\\1", package)
    loadenv <- new.env(hash=TRUE)
    codeFile <- file.path(pkgpath, "R", barepackage)
    dbbase <- file.path(pkgpath, "R", barepackage)
    if (packageHasNamespace(package, dirname(pkgpath))) {
        if (! is.null(.Internal(getRegisteredNamespace(as.name(package)))))
            stop("name space must not be loaded.")
        ns <- loadNamespace(package, lib.loc, keep.source, TRUE, TRUE)
        makeLazyLoadDB(ns, dbbase)
    }
    else {
        loadenv <- new.env(hash = TRUE, parent = .GlobalEnv)
        if(file.exists(codeFile))
            sys.source(codeFile, loadenv, keep.source = keep.source)
        ## now transfer contents of loadenv to a new env to mimic library
        ## the actual copy has to be done by C code to avoid forcing
        ## promises that might have been created using delay().
        env <- new.env(hash=TRUE)
        .Internal(lib.fixup(loadenv, env))
        ## save the package name in the environment
        assign(".packageName", barepackage, envir = env)
        makeLazyLoadDB(env, dbbase, compress = compress)
    }
}

sysdata2LazyLoadDB <- function(srcFile, destDir, compress = TRUE)
{
    e <- new.env(hash=TRUE)
    load(srcFile, e)
    makeLazyLoadDB(e, file.path(destDir, "sysdata"), compress = compress)
}

list_data_in_pkg <- function(package, lib.loc = NULL, dataDir = NULL)
{
    if(is.null(dataDir)) {
        pkgpath <- .find.package(package, lib.loc, quiet = TRUE)
        if(!length(pkgpath))
            stop(gettextf("there is no package called '%s'", package),
                 domain = NA)
        dataDir <- file.path(pkgpath, "data")
    } else {
        pkgpath <- sub("/data$", "", dataDir)
        package <- basename(pkgpath)
	# avoid builddir != srcdir problems -- assume package has been installed
        lib.loc <- c(dirname(pkgpath), .libPaths())
    }
    if(file_test("-d", dataDir)) {
        if(file.exists(sv <- file.path(dataDir, "Rdata.rds"))) {
            ans <- .readRDS(sv)
        } else if(file.exists(sv <- file.path(dataDir, "datalist"))) {
            ans <- strsplit(readLines(sv), ":")
            nms <- lapply(ans, function(x) x[1L])
            ans <- lapply(ans, function(x)
                          if(length(x) == 1L) x[1L] else
                          strsplit(x[2L], " +")[[1L]][-1L])
            names(ans) <- nms
        } else {
            files <- list_files_with_type(dataDir, "data")
            files <- unique(basename(file_path_sans_ext(files)))
            ans <- vector("list", length(files))
            dataEnv <- new.env(hash=TRUE)
            names(ans) <- files
            for(f in files) {
                utils::data(list = f, package = package, lib.loc = lib.loc,
                            envir = dataEnv)
                ans[[f]] <- ls(envir = dataEnv, all.names = TRUE)
                rm(list = ans[[f]], envir = dataEnv)
            }
        }
        ans
    } else NULL
}

data2LazyLoadDB <- function(package, lib.loc = NULL, compress = TRUE)
{
    options(warn=1)
    pkgpath <- .find.package(package, lib.loc, quiet = TRUE)
    if(!length(pkgpath))
        stop(gettextf("there is no package called '%s'", package),
             domain = NA)
    dataDir <- file.path(pkgpath, "data")
    ## set the encoding for text files to be read, if specified
    enc <- .read_description(file.path(pkgpath, "DESCRIPTION"))["Encoding"]
    if(!is.na(enc)) {
        op <- options(encoding=enc)
        on.exit(options(encoding=op[[1L]]))
    }
    if(file_test("-d", dataDir)) {
        if(file.exists(file.path(dataDir, "Rdata.rds")) &&
	    file.exists(file.path(dataDir, paste(package, "rdx", sep="."))) &&
	    file.exists(file.path(dataDir, paste(package, "rdb", sep="."))) ){
            warning("package seems to be using lazy loading for data already")
        }
	else {
            dataEnv <- new.env(hash=TRUE)
            tmpEnv <- new.env()
            f0 <- files <- list_files_with_type(dataDir, "data")
            files <- unique(basename(file_path_sans_ext(files)))
            dlist <- vector("list", length(files))
            names(dlist) <- files
            loaded <- character(0L)
            for(f in files) {
                utils::data(list = f, package = package, lib.loc = lib.loc,
                        envir = dataEnv)
                utils::data(list = f, package = package, lib.loc = lib.loc,
                        envir = tmpEnv)
                tmp <- ls(envir = tmpEnv, all.names = TRUE)
                rm(list = tmp, envir = tmpEnv)
                dlist[[f]] <- tmp
                loaded <- c(loaded, tmp)
            }
            dup<- duplicated(loaded)
            if(any(dup))
                warning(gettextf("object(s) %s are created by more than one data call",
                                 paste(sQuote(loaded[dup]),
                                       collapse=", ")),
                        domain = NA)

            if(length(loaded)) {
                dbbase <- file.path(dataDir, "Rdata")
                makeLazyLoadDB(dataEnv, dbbase, compress = compress)
                .saveRDS(dlist, file.path(dataDir, "Rdata.rds"),
                         compress = compress)
                unlink(f0)
                if(file.exists(file.path(dataDir, "filelist")))
                    unlink(file.path(dataDir, c("filelist", "Rdata.zip")))
            }
        }
    }
}

makeLazyLoadDB <- function(from, filebase, compress = TRUE, ascii = FALSE,
                           variables)
{
    envlist <- function(e) {
        names <- ls(e, all.names=TRUE)
        .Call("R_getVarsFromFrame", names, e, FALSE, PACKAGE="base")
    }

    envtable <- function() {
        idx <- 0
        envs <- NULL
        enames <- character(0L)
        find <- function(v, keys, vals) {
            for (i in seq_along(keys))
                if (identical(v, keys[[i]]))
                    return(vals[i])
	    NULL
	}
        getname <- function(e) find(e, envs, enames)
        getenv <- function(n) find(n, enames, envs)
        insert <- function(e) {
            idx <<- idx + 1
            name <- paste("env", idx, sep="::")
            envs <<- c(e, envs)
            enames <<- c(name, enames)
            name
        }
        list(insert = insert, getenv = getenv, getname = getname)
    }

    lazyLoadDBinsertValue <- function(value, file, ascii, compress, hook)
        .Call("R_lazyLoadDBinsertValue", value, file, ascii, compress, hook,
              PACKAGE = "base")

    lazyLoadDBinsertListElement <- function(x, i, file, ascii, compress, hook)
        .Call("R_lazyLoadDBinsertValue", x[[i]], file, ascii, compress, hook,
              PACKAGE = "base")

    lazyLoadDBinsertVariable <- function(n, e, file, ascii, compress, hook) {
        x <- .Call("R_getVarsFromFrame", n, e, FALSE, PACKAGE="base")
       .Call("R_lazyLoadDBinsertValue", x[[1L]], file, ascii, compress, hook,
              PACKAGE = "base")
    }

    mapfile <- paste(filebase, "rdx", sep = ".")
    datafile <- paste(filebase, "rdb", sep = ".")
    close(file(datafile, "wb")) # truncate to zero
    table <- envtable()
    varenv <- new.env(hash = TRUE)
    envenv <- new.env(hash = TRUE)

    envhook <- function(e) {
        if (is.environment(e)) {
            name <- table$getname(e)
            if (is.null(name)) {
                name <- table$insert(e)
                data <- list(bindings = envlist(e),
                             enclos = parent.env(e))
                key <- lazyLoadDBinsertValue(data, datafile, ascii,
                                             compress, envhook)
                assign(name, key, envir = envenv)
            }
            name
        }
    }

    if (is.null(from) || is.environment(from)) {
        if (! missing(variables))
            vars <- variables
        else vars <- ls(from, all.names = TRUE)
    }
    else if (is.list(from)) {
        vars <- names(from)
        if (length(vars) != length(from) || any(!nzchar(vars)))
            stop("source list must have names for all elements")
    }
    else stop("source must be an environment or a list")

    for (i in seq_along(vars)) {
        key <- if (is.null(from) || is.environment(from))
            lazyLoadDBinsertVariable(vars[i], from, datafile,
                                     ascii, compress,  envhook)
        else lazyLoadDBinsertListElement(from, i, datafile, ascii,
                                         compress, envhook)
        assign(vars[i], key, envir = varenv)
    }

    vals <- lapply(vars, get, envir = varenv, inherits = FALSE)
    names(vals) <- vars

    rvars <- ls(envenv, all.names = TRUE)
    rvals <- lapply(rvars, get, envir = envenv, inherits = FALSE)
    names(rvals) <- rvars

    val <- list(variables = vals, references = rvals,
                compressed = compress)
   .saveRDS(val, mapfile)
}

makeLazyLoading <-
    function(package, lib.loc = NULL, compress = TRUE,
             keep.source = getOption("keep.source.pkgs"))
{
    options(warn=1)
    findpack <- function(package, lib.loc) {
        pkgpath <- .find.package(package, lib.loc, quiet = TRUE)
        if(!length(pkgpath))
            stop(gettextf("there is no package called '%s'", package),
                 domain = NA)
        pkgpath
    }

    pkgpath <- findpack(package, lib.loc)
    barepackage <- sub("([^-]+)_.*", "\\1", package)

    if (package == "base")
        stop("this cannot be used for package 'base'")
    else if (packageHasNamespace(package, dirname(pkgpath)))
        loaderFile <- file.path(R.home("share"), "R", "nspackloader.R")
    else
        loaderFile <- file.path(R.home("share"), "R", "packloader.R")
    codeFile <- file.path(pkgpath, "R", barepackage)

    if (!file.exists(codeFile)) {
        warning("package contains no R code")
        return(invisible())
    }
    if (file.info(codeFile)["size"] == file.info(loaderFile)["size"])
        warning("package seems to be using lazy loading already")
    else {
        code2LazyLoadDB(package, lib.loc = lib.loc,
                        keep.source = keep.source, compress = compress)
        file.copy(loaderFile, codeFile, TRUE)
    }

    invisible()
}
#  File src/library/tools/R/md5.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

md5sum <- function(files)
    structure(.Call(Rmd5, files), names=files)

.installMD5sums <- function(pkgDir, outDir = pkgDir)
{
    ## <FIXME> what if dot == NULL?
    dot <- getwd()
    setwd(pkgDir)
    x <- md5sum(dir(pkgDir, recursive=TRUE))
    setwd(dot)
    x <- x[names(x) != "MD5"]
    cat(paste(x, names(x), sep=" *"), sep="\n",
        file=file.path(outDir, "MD5"))
}

checkMD5sums <- function(package, dir)
{
    if(missing(dir)) dir <- .find.package(package, quiet=TRUE)
    if(!length(dir)) return(NA)
    md5file <- file.path(dir, "MD5")
    if(!file.exists(md5file)) return(NA)
    inlines <- readLines(md5file)
    ## now split on the first space.
    xx <- sub("^([0-9a-fA-F]*)(.*)", "\\1", inlines)
    nmxx <- names(xx) <- sub("^[0-9a-fA-F]* [ |*](.*)", "\\1", inlines)
    ## <FIXME> what if dot == NULL?
    dot <- getwd()
    setwd(dir)
    x <- md5sum(dir(dir, recursive=TRUE))
    setwd(dot)
    x <- x[names(x) != "MD5"]
    nmx <- names(x)
    res <- TRUE
    not.here <- !(nmxx %in% nmx)
    if(any(not.here)) {
        res <- FALSE
        cat("files", paste(nmxx[not.here], collapse=", "),
            "are missing\n", sep=" ")
    }
    nmxx <- nmxx[!not.here]
    diff <- xx[nmxx] != x[nmxx]
    if(any(diff)) {
        res <- FALSE
        cat("files", paste(nmxx[diff], collapse=", "),
            "have the wrong MD5 checksums\n", sep=" ")
    }
    return(res)
}
.build_news_db_from_R_NEWS <-
function()
{
    db <- readNEWS(chop = "keepAll")
    ## This currently is a list of x.y lists of x.y.z lists of
    ## categories list of entries.
    flatten <- function(e)
        cbind(rep.int(names(e), sapply(e, length)),
              unlist(lapply(e,
                            function(s) {
                                ## Also remove leading white space and
                                ## trailing blank lines.
                                lapply(s,
                                       function(e)
                                           sub("[[:space:]]*$", "",
                                               paste(sub("^ ", "", e),
                                                     collapse = "\n")))
                            }),
                            use.names = FALSE))
    db <- lapply(Reduce(c, db), flatten)
    db <- do.call(rbind, Map(cbind, names(db), db))
    ## Squeeze in an empty date column.
    .make_news_db(cbind(db[, 1L], NA_character_, db[, -1L]),
                  logical(nrow(db)))
}

.build_news_db <-
function(package, lib.loc = NULL, format = NULL, reader = NULL)
{
    dir <- system.file(package = package, lib.loc = lib.loc)
    ## Or maybe use .find.package()?

    ## Eventually add support for DESCRIPTION
    ##   News/File
    ##   News/Format
    ##   News/Reader
    ##   News/Reader@R
    ## entries.
    ## For now, only look for files
    ##   NEWS inst/NEWS
    ## and ignore
    ##   ChangeLog inst/ChangeLog
    ## in the package directory.
    files <- file.path(dir,
                       c("NEWS",
                         file.path("inst", "NEWS")))
    nfile <- files[file_test("-f", files)][1L]

    if(is.na(nfile)) return(invisible())
    ## Return NULL for now, no message that there is no NEWS or
    ## ChangeLog file.

    if(!is.null(format))
        .NotYetUsed("format", FALSE)
    if(!is.null(reader))
        .NotYetUsed("reader", FALSE)

    reader <- .news_reader_default

    reader(nfile)
}

.news_reader_default <-
function(file)
{
    verbose <- getOption("verbose")

    .collapse <- function(s) paste(s, collapse = "\n")
    
    lines <- readLines(file, warn = FALSE)

    ## Re-encode if necessary.    
    if(any(ind <- is.na(nchar(lines, allowNA = TRUE)))) {
        dir <- dirname(file)
        if(basename(dir) == "inst")
            dir <- dirname(file)
        ## This should now contain the DESCRIPTION file.
        encoding <-
            if(file.exists(dfile <- file.path(dir, "DESCRIPTION")))
                .read_description(dfile)["Encoding"]
            else
                NA
        if(!is.na(encoding))
            lines[ind] <- iconv(lines[ind], encoding, "")
        ## Last resort.
        if(any(is.na(nchar(lines[ind], allowNA = TRUE))))
            lines[ind] <- iconv(lines[ind], "", "", sub = "byte")
    }

    ## Save what we read in case we cannot figure out the news, in which
    ## case we simply return one entry with the whole text.
    olines <- lines
    ## Get rid of underlines and friends.
    lines <-
        lines[!grepl("^[[:space:]]*[[:punct:]]*[[:space:]]*$", lines)]

    ## Determine lines containing version numbers, without being too
    ## liberal.
    re_valid_package_name <- .standard_regexps()$valid_package_name
    re_v <- sprintf("^([[:space:]]*(%s)|(%s))(%s).*$",
                    paste("CHANGES? *(IN|FOR).*VERSION *",
                          "CHANGES? *(IN|FOR|TO) *",
                          sep = "|"),
                    sprintf(paste(## TeachingDemos pomp ouch
                                  "NEW IN .*",
                                  ## HyperbolicDist nls2 proto
                                  "VERSION:? *",
                                  "%s +",
                                  ## E.g., lattice:
                                  ##   Changes in lattice 0.17
                                  "CHANGES IN %s +",
                                  ## sv*
                                  "== Changes in %s +",
                                  ## tcltk2
                                  "== Version +",
                                  ## R2WinBUGS
                                  "update *",
                                  "v *",
                                  "",
                                  sep = "|"),
                            re_valid_package_name,
                            re_valid_package_name,
                            re_valid_package_name),
                    .standard_regexps()$valid_package_version
                    )
    ## Some people use
    ##   $PACKAGE version $VERSION
    ## Let us try handling this later, or ask people to write their own
    ## readers.
    ind <- grepl(re_v, lines, ignore.case = TRUE)

    if(!any(ind))
        return(.make_news_db(cbind(NA_character_,
                                   NA_character_,
                                   NA_character_,
                                   .collapse(olines))))
    ## Could add an empty list of bad chunks (as none were found).

    ## Everything before the first version line is a header which will
    ## be dropped.
    if(!ind[1L]) {
        pos <- seq_len(which(ind)[1L] - 1L)
        lines <- lines[-pos]
        ind <- ind[-pos]
    }

    ## Try catching date entries at the end of version lines as well.
    re_d <- sprintf("^.*(%s)[[:punct:][:space:]]*$",
                    "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}")
    ## Could try to allow for non ISO date specs ...

    ## Version lines determine the chunks, which after the version line
    ## should either start with a line tag (category) or an itemize
    ## "bullet".
    chunks <- split(lines, cumsum(ind))

    do_chunk <- function(chunk, header = NA_character_) {
        ## Process a single chunk.

        ## If there is no category header, the first line is the version
        ## line, after which the next non blank line should start with a
        ## line tag (category) or an itemize "bullet".
        if(!is.na(header))
            date <- NA_character_
        else {
            txt <- chunk[1L]
            header <- sub(re_v, "\\6", txt, ignore.case = TRUE)
            date <- if(grepl(re_d, txt))
                sub(re_d, "\\1", txt)
            else
                NA_character_
        }
            
        lines <- chunk[-1L]
        s <- .collapse(lines)
        if(grepl("^[[:space:]]*([o*+-])", s)) {
            sep <- sub("^[[:space:]]*([o*+-]).*$", "\\1", s)
            ire <- sprintf("^[[:space:]]*([%s])[[:space:]]+", sep)
            ind <- grepl(ire, lines)
            list(entries =
                 sapply(split(lines, cumsum(ind)),
                        function(s)
                        sub(ire, "", .collapse(sub("^\t?", "", s)))
                        ),
                 header = header,
                 chunk = chunk,
                 date = date)
        } else {
            ## Categories should be non-empty starting in column 1.
            re_c <- "^([[:alpha:]].*)[[:space:]]*$"
            ind <- grepl(re_c, lines)
            ## If we detect neither bullet items nor categories, the
            ## chunk is in a different format than we can recognize.
            ## Return no entries, and have the finisher give the whole
            ## chunk and push it onto the bad chunk list.
            if(!any(ind)) {
                list(entries = character(),
                     header = header,
                     chunk = chunk,
                     date = date)
            } else {
                pos <- cumsum(ind) > 0
                list(entries =
                     Map(do_chunk,
                         split(lines[pos], cumsum(ind)[pos]),
                         sub("[[:punct:]]*$", "",
                             sub(re_c, "\\1", lines[ind]))),
                     header = header,
                     chunk = chunk,
                     date = date)
            }
        }
    }

    out <- lapply(chunks, do_chunk)
    ## Now assemble pieces.
    reporter <- function(x) {
        if(verbose)
            message(gettextf("Cannot process chunk/lines:\n%s",
                             .collapse(x)))
        NULL
    }
    finisher <- function(x) {
        entries <- x$entries
        version <- x$header
        date <- x$date
        if(is.list(entries)) {
            do.call(rbind,
                    lapply(entries,
                           function(x) {
                               entries <- x$entries
                               bad <- if(!length(entries)) {
                                   reporter(x$chunk)
                                   entries <-
                                       sub("^[[:space:]]*", "",
                                           .collapse(x$chunk[-1L]))
                                   TRUE
                               }
                               else FALSE
                               cbind(version, date, x$header, entries,
                                     bad)
                           }))
        }
        else {
            bad <- if(!length(entries)) {
                reporter(x$chunk)
                entries <-
                    sub("^[[:space:]]*", "",
                        .collapse(x$chunk[-1L]))
                TRUE
            }
            else FALSE
            cbind(version, date, NA_character_, entries, bad)
        }
    }

    out <- do.call(rbind, lapply(out, finisher))

    ## Try to remove a common 'exdent' from the entries.
    entries <- out[, 4L]
    exdent <-
        unlist(lapply(gregexpr("\n *", entries), attr, "match.length"))
    exdent <- exdent[exdent > 1L]
    if(length(exdent)) {
        out[, 4L] <-
            gsub(sprintf("\n%s",
                         paste(rep.int(" ", min(exdent) - 1L),
                               collapse = "")),
                 "\n", entries)
    }

    .make_news_db(out[, -5L, drop = FALSE], as.logical(out[, 5L]))
}

.make_news_db <-
function(x, bad = NULL)
{
    ## Expect x to be a 4 column
    ##   version date category text
    ## character matrix.
    ## Could of course check for this using
    ##   if(!is.character(x) || ncol(x) != 4L)
    out <- data.frame(x, row.names = NULL, stringsAsFactors = FALSE)
    ## Note that we cannot do
    ##   dimnames(out) <- list(NULL,
    ##                         c("Version", "Date", "Category", "Text"))
    colnames(out) <- c("Version", "Date", "Category", "Text")
    if(!is.null(bad))
        attr(out, "bad") <- bad
    class(out) <- c("news_db", "data.frame")
    out
}
#  File src/library/tools/R/package.dependencies.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

## This is called only with check = FALSE in getDepMtrx/getRemotePkgDepends

package.dependencies <-
    function(x, check = FALSE, depLevel = c("Depends", "Imports", "Suggests"))
{
    depLevel <- match.arg(depLevel)

    if(!is.matrix(x))
        x <- matrix(x, nrow = 1L, dimnames = list(NULL, names(x)))

    deps <- list()
    for(k in 1L:nrow(x)){
        z <- x[k, depLevel]
        if(!is.na(z) & z != ""){
            ## split dependencies, remove leading and trailing whitespace
            z <- unlist(strsplit(z, ",", fixed=TRUE))
            z <- sub("^[[:space:]]*(.*)", "\\1", z)
            z <- sub("(.*)[[:space:]]*$", "\\1", z)

            ## split into package names and version
            pat <- "^([^\\([:space:]]+)[[:space:]]*\\(([^\\)]+)\\).*"
            deps[[k]] <-
                cbind(sub(pat, "\\1", z), sub(pat, "\\2", z), NA)

            noversion <- deps[[k]][,1] == deps[[k]][,2]
            deps[[k]][noversion,2] <- NA

            ## split version dependency into operator and version number
            pat <- "[[:space:]]*([[<>=]+)[[:space:]]+(.*)"
            deps[[k]][!noversion, 2:3] <-
                c(sub(pat, "\\1", deps[[k]][!noversion, 2]),
                  sub(pat, "\\2", deps[[k]][!noversion, 2]))
        }
        else
            deps[[k]] <- NA
    }

    if(check){
        z <- rep.int(TRUE, nrow(x))
        for(k in 1L:nrow(x)) {
            ## currently we only check the version of R itself
            if(!is.na(deps[[k]]) &&
               any(ok <- deps[[k]][,1] == "R")) {
                ## NOTE: currently operators must be `<=' or `>='.
                if(!is.na(deps[[k]][ok, 2])
                   && deps[[k]][ok, 2] %in% c("<=", ">=")) {
                    ## careful.  We don't want 1.9.1 < 1.50
                    op <- deps[[k]][ok,2]
                    x1 <- rep.int(0, 6)
                    y <- c(R.version$major,
                           strsplit(R.version$minor, ".", fixed=TRUE)[[1L]])
                    x1[seq_along(y)] <- y
                    y <- strsplit(deps[[k]][ok,3], ".", fixed=TRUE)[[1L]]
                    x1[3+seq_along(y)] <- y
                    x1 <- format(x1, justify="right")
                    x2 <- paste(x1[4:6], collapse=".")
                    x1 <- paste(x1[1L:3], collapse=".")
                    comptext <- paste("'", x1, "' ", op,
                                      " '", x2, "'", sep = "")
                    compres <- try(eval(parse(text = comptext)))
                    if(!inherits(compres, "try-error")) {
                        z[k] <- compres
                    }
                }
            }
        }
        names(z) <- x[,"Package"]
        return(z)
    }
    else{
        names(deps) <- x[,"Package"]
        return(deps)
    }
}
#  File src/library/tools/R/packageshtml.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

win.packages.html <-
    function(lib.loc=.libPaths(), docdir = R.home("doc"), libdir = .Library)
{
    f.tg <- file.path(docdir, "html", "packages.html")
    f.hd <- file.path(docdir, "html", "packages-head-utf8.html")
    if(!file.create(f.tg)) {
        warning("cannot update HTML package index")
        return(FALSE)
    }
    file.append(f.tg, f.hd)
    out <- file(f.tg, open="a")
    rh <- chartr("\\", "/", R.home())
    drive <- substring(rh, 1L, 2L)
    for (lib in lib.loc) {
        pg <- sort(.packages(all.available = TRUE, lib.loc = lib))
        ## use relative indexing for .Library
        if(is.na(pmatch(rh, lib))) {
            libname <- chartr("/", "\\", lib)
            lib0 <- if(substring(lib, 2L, 2L) != ":")
                paste(drive, lib, sep="") else lib
            lib0 <- paste("file:///", lib0, sep="")
        } else {
            lib0 <- "../../library"
            libname <- "the standard library"
        }
        if(length(lib.loc) > 1L)
            cat("<p><h3>Packages in ", libname, "</h3>\n",
                sep = "", file = out)
        if(libname != "the standard library")
            cat("<p>Cross-links from this library to other libraries may not work.\n\n", file = out)
        cat("<p>\n<table width=\"100%\" summary=\"R Package list\">\n",
            file = out)
        for (i in  pg) {
            ## there is a bootstrapping problem using packageDescription
            file <- system.file("Meta", "package.rds", package = i,
                                    lib.loc = lib)
            title <- if(file != "") {
                txt <- .readRDS(file)
                if(is.list(txt)) txt <- txt$DESCRIPTION
                ## we may need to re-encode here.
                if("Encoding" %in% names(txt)) {
                        to <- "UTF-8"
                        tmp <- try(iconv(txt, txt["Encoding"], to, "?"))
                        if(!inherits(tmp, "try-error"))
                            txt <- tmp
                        else
                            warning("'DESCRIPTION' has 'Encoding' field and re-encoding is not possible", call.=FALSE)
                    }
                txt["Title"]
            } else NA

            if (is.na(title)) title <- "-- Title is missing --"
            cat('<tr align="left" valign="top">\n',
                '<td width="25%"><a href="', lib0, '/', i,
                '/html/00Index.html">', i, "</a></td><td>", title,
                "</td></tr>\n", file=out, sep="")
        }
        cat("</table>\n\n", file=out)
    }
    cat("</body></html>\n", file=out)
    close(out)
    invisible(TRUE)
}

unix.packages.html <-
    function(lib.loc=.libPaths(), docdir = R.home("doc"), libdir = .Library)
{
    f.tg <- file.path(docdir, "html", "packages.html")
    if(!file.create(f.tg)) {
        warning("cannot create HTML package index")
        return(FALSE)
    }
    file.append(f.tg, file.path(docdir, "html", "packages-head-utf8.html"))
    out <- file(f.tg, open="a")
    for (lib in lib.loc) {
        pg <- sort(.packages(all.available = TRUE, lib.loc = lib))
        ## use relative indexing for .Library
        if(lib != libdir) {
            libname <- lib
            lib0 <- paste("file:///", lib, sep="")
        } else {
            lib0 <- "../../library"
            libname <- "the standard library"
        }
        cat("<p><h3>Packages in ", libname,
            '</h3>\n<p>\n<table width="100%" summary="R Package list">\n',
            sep = "", file=out)
        for (i in pg) {
            title <- utils::packageDescription(i, lib.loc = lib,
                                               fields = "Title",
                                               encoding = "UTF-8")
            if (is.na(title)) title <- "-- Title is missing --"
            cat('<tr align="left" valign="top">\n',
                '<td width="25%"><a href="', lib0, '/', i,
                '/html/00Index.html">', i, "</a></td><td>", title,
                "</td></tr>\n", file=out, sep="")
        }
        cat("</table>\n\n", file=out)
    }
    cat("</body></html>\n", file=out)
    close(out)
    invisible(TRUE)
}
#  File src/library/tools/R/parseRd.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

parse_Rd <- function(file, srcfile = NULL, encoding = "unknown",
                     verbose = FALSE, fragment = FALSE,
                     warningCalls = TRUE)
{
    if(is.character(file)) {
        file0 <- file
        if(file == "") {
            file <- stdin()
        } else {
            ## keep.source is FALSE in batch use
            ## encoding issues here, for now use file encoding
            if (missing(srcfile)) ## && isTRUE(getOption("keep.source")))
                srcfile <- srcfile(file)
        }
    } else file0 <- "<connection>"
    lines <- readLines(file, warn = FALSE)
    ## remove old-style marking for data, keep line nos
    lines[lines == "\\non_function{}"] <- ""
    ## Extract the encoding if marked in the file:
    ## do this in two steps to minimize warnings in MBCS locales
    ## Note this is required to be on a line by itself,
    ## but some people have preceding whitespace
    enc <- grep("\\encoding{", lines, fixed = TRUE, useBytes=TRUE)
    enc <- grep("^[[:space:]]*\\\\encoding\\{([^}]*)\\}.*", lines[enc], value=TRUE)
    if(length(enc)) {
        if(length(enc) > 1L)
            warning(file0, ": multiple \\encoding lines, using the first",
                    domain = NA, call. = warningCalls)
        ## keep first one
        enc <- enc[1L]
        enc <- sub("^[[:space:]]*\\\\encoding\\{([^}]*)\\}.*", "\\1", enc)
        if(verbose) message("found encoding ", enc)
        encoding <- if(enc %in% c("UTF-8", "utf-8", "utf8")) "UTF-8" else enc
    }
    if (encoding == "unknown") encoding <- ""

    ## the internal function must get some sort of srcfile
    if (!inherits(srcfile, "srcfile"))
    	srcfile <- srcfile(file0)
    basename <- basename(srcfile$filename)
    srcfile$encoding <- encoding

    if (encoding == "ASCII") {
        if (any(is.na(iconv(lines, "", "ASCII"))))
            stop(file0, ": non-ASCII input and no declared encoding",
                 domain = NA, call. = warningCalls)
    } else if (encoding != "UTF-8")
    	lines <- iconv(lines, encoding, "UTF-8", sub = "byte")

    tcon <- file()
    writeLines(lines, tcon, useBytes = TRUE)
    on.exit(close(tcon))

    .Internal(parse_Rd(tcon, srcfile, "UTF-8",
                       verbose, basename, fragment, warningCalls))
}

print.Rd <- function(x, deparse = FALSE, ...)
{
    cat(as.character.Rd(x, deparse = deparse), sep = "", collapse = "")
    invisible(x)
}

as.character.Rd <- function(x, deparse = FALSE, ...)
{
    ZEROARG <- c("\\cr", "\\dots", "\\ldots", "\\R", "\\tab") # Only these cause trouble when {} is added
    TWOARG <- c("\\section", "\\item", "\\enc", "\\method", "\\S3method",
                "\\S4method", "\\tabular")
    EQN <- c("\\deqn", "\\eqn")
    modes <- c(RLIKE=1L, LATEXLIKE=2L, VERBATIM=3L, INOPTION=4L, COMMENTMODE=5L, UNKNOWNMODE=6L)
    tags  <- c(RCODE=1L, TEXT=2L,      VERB=3L,                  COMMENT=5L,     UNKNOWN=6L)
    state <- c(braceDepth=0L, inRString=0L)
    needBraces <- FALSE  # if next character is alphabetic, separate by braces.
    inEqn <- 0L

    pr <- function(x, quoteBraces) {
        tag <- attr(x, "Rd_tag")
        if (is.null(tag) || tag == "LIST") tag <- ""
    	if (is.list(x)) {
    	    savestate <- state
    	    state <<- c(0L, 0L)
    	    needBraces <<- FALSE
    	    if (tag == "Rd") { # a whole file
    	        result <- character()
    	    	for (i in seq_along(x))
                    result <- c(result, pr(x[[i]], quoteBraces))
    	    } else if (length(grep("^#", tag))) {
    	    	if (deparse) {
    	    	    dep <- deparseRdElement(x[[1L]][[1L]],
                                            c(state, modes["LATEXLIKE"],
                                              inEqn,
                                              as.integer(quoteBraces)))
    	    	    result <- c(tag, dep[[1L]])
    	    	} else
    	    	    result <- c(tag, x[[1L]][[1L]])
    	    	for (i in seq_along(x[[2L]]))
                    result <- c(result, pr(x[[2L]][[i]], quoteBraces))
    	    	result <- c(result, "#endif\n")
    	    } else if (tag %in% ZEROARG) {
    	    	result <- tag
    	    	needBraces <<- TRUE
    	    } else if (tag %in% TWOARG) {
    	    	result <- tag
    	    	for (i in seq_along(x))
                    result <- c(result, pr(x[[i]], quoteBraces))
    	    } else if (tag %in% EQN) {
    	    	result <- tag
    	    	inEqn <<- 1L
    	    	result <- c(result, pr(x[[1]], quoteBraces))
    	    	inEqn <<- 0L
    	    	if (length(x) > 1L)
    	    	    result <- c(result, pr(x[[2]], quoteBraces))
    	    } else {
    	    	result <- tag
    	    	if (!is.null(option <- attr(x, "Rd_option")))
    	    	    result <- c(result, "[", pr(option, quoteBraces), "]")
    	    	result <- c(result, "{")
    	    	for (i in seq_along(x))
                    result <- c(result, pr(x[[i]], quoteBraces))
    	    	result <- c(result, "}")
    	    }
    	    if (state[1L])  # If braces didn't match within the list, try again, quoting them
    	    	result <- pr(x, TRUE)
    	    state <<- savestate
    	} else {
    	    if (deparse) {
    		dep <- deparseRdElement(as.character(x), c(state, tags[tag], inEqn, as.integer(quoteBraces)))
    	    	result <- dep[[1L]]
    	    	state <<- dep[[2L]][1L:2L]
    	    } else
    	    	result <- as.character(x)
    	    if (needBraces) {
    	    	if (grepl("^[[:alpha:]]", result)) result <- c("{}", result)
    	    	needBraces <<- FALSE
    	    }
        }
    	result
    }
    if (is.null(attr(x, "Rd_tag"))) attr(x, "Rd_tag") <- "Rd"
    pr(x, quoteBraces = FALSE)
}

deparseRdElement <- function(element, state)
    .Internal(deparseRd(element, state))
#  File src/library/tools/R/pkgDepends.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

pkgDepends <- function(pkg, recursive=TRUE, local=TRUE,
                       reduce=TRUE, lib.loc=NULL) {
    if (length(pkg) != 1L)
        stop("argument 'pkg' must be of length 1")

    instPkgs <- utils::installed.packages(lib.loc=lib.loc)

    depMtrx <- getDepMtrx(pkg, instPkgs, local)
    if (is.null(depMtrx))               # Package was not found
        stop(gettextf("package '%s' was not found", pkg),
             domain = NA)

    getDepList(depMtrx, instPkgs, recursive, local, reduce, lib.loc)
}

getDepList <- function(depMtrx, instPkgs, recursive=TRUE,
                       local=TRUE, reduce=TRUE, lib.loc=NULL) {
    out <- list(Depends=character(), Installed=character(),
                Found=list(), NotFound=character(),
                R=character())
    class(out) <- c("DependsList", class(out))

    if ((!is.matrix(depMtrx))&&(is.na(depMtrx))) # no dependencies
        return(out)

    mtrxList <- buildDepList(depMtrx, instPkgs, recursive, lib.loc)

    if (local == FALSE) {
        toFind <- mtrxList$Depends[!apply(mtrxList$Depends, 1,
                                          isSatisfied,
                                          mtrxList$Installed),,drop=FALSE]

        if (reduce)
            toFind <- reduceDepends(toFind)

        if (length(toFind)) {
            found <- foundDepends(toFind)
            out$Found <- found$Found
            mtrxList$NotFound <- found$NotFound
        }
    }

    if (reduce == TRUE) {       # Found and NotFound are already reduced
        mtrxList$R <- reduceDepends(mtrxList$R)
        mtrxList$Depends <- reduceDepends(mtrxList$Depends)
        mtrxList$Installed <- reduceDepends(mtrxList$Installed)
    }


    ## Massage the matrices back into dependency strings.  out$Found
    ## is already assigned.
    out$R <- depMtrxToStrings(mtrxList$R)
    out$Depends <- depMtrxToStrings(mtrxList$Depends)
    out$Installed <- depMtrxToStrings(mtrxList$Installed)
    out$NotFound <- depMtrxToStrings(mtrxList$NotFound)

    out
}

isSatisfied <- function(dep, instMtrx) {
    triplets <- apply(instMtrx, 1L, paste, collapse=":")
    match(paste(dep,collapse=":"), triplets, nomatch=0L) > 0L
}

buildDepList <- function(depMtrx, instPkgs, recursive=TRUE,
                         lib.loc=NULL) {
    mtrxList <- list(Depends=matrix(nrow=0L,ncol=3L),
                     Installed=matrix(nrow=0L,ncol=3L), R=matrix(nrow=0L,ncol=3L))


    ## First check to see if there is a dependency on R
    ## If there is, then check it
    whichR <- which(depMtrx[,1] == "R")
    if (length(whichR)) {
        mtrxList$R <- depMtrx[whichR,,drop=FALSE]
        depMtrx <- depMtrx[-whichR,,drop=FALSE]
    }

    ## Get which of the direct depends are installed
    instDeps <- depMtrx[installedDepends(depMtrx, instPkgs),,drop=FALSE]

    if (recursive == TRUE) {
        mtrxList$Depends <- depMtrx
        mtrxList$Installed <- instDeps

        for (curPkg in depMtrx[,1]) {
            depMtrx <- getDepMtrx(curPkg, instPkgs)
            ## Make sure this package was found & has deps
            if ((is.null(depMtrx))||(is.na(depMtrx)))
                next

            curMtrxList <- buildDepList(depMtrx, instPkgs,
                                             recursive=recursive,
                                             lib.loc=lib.loc)
            mtrxList$R <- rbind(mtrxList$R, curMtrxList$R)
            mtrxList$Depends <- rbind(mtrxList$Depends,
                                      curMtrxList$Depends)
            mtrxList$Installed <- rbind(mtrxList$Installed,
                                        curMtrxList$Installed)
        }
    }
    else {                              # recurse is FALSE
        mtrxList$Depends <- depMtrx
        mtrxList$Installed <- instDeps
    }

    mtrxList
}

getDepMtrx <- function(pkg, instPkgs, local=TRUE) {

    ## Need to see if pkg is installed - if not, get online
    row <- match(pkg,instPkgs[,"Package"])
    if (!is.na(row))                    # package is installed
        pkgDeps <- package.dependencies(instPkgs[row,])[[1L]]
    else {
        if (local)
            pkgDeps <- NULL
        else
            pkgDeps <- getRemotePkgDepends(pkg)
    }

    pkgDeps        # Either a matrix, NA if no deps or NULL if not found
}

getRemotePkgDepends <- function(pkg, contriburl=getOption("repos")) {
    ## Will get the dependencies of a package from
    ## online repositories.  Returns NULL if it
    ## cannot be found, otherwise returns the row provided
    ## by available.packages().

    if(is.null(contriburl))
        contriburl <- utils::contrib.url(getOption("repos"))

    cran <- utils::available.packages(contriburl=contriburl)
    whichRow <- which(pkg == cran[,"Package"])
    if (length(whichRow)) {
        return(package.dependencies(cran[whichRow,])[[1L]])
    }
    else
        NULL
}

installedDepends <- function(depMtrx, instPkgs) {
    ## Given a matrix of packages, will return a vector of row
    ## numbers that correspond to packages in the matrix where
    ## the dependency is met by installed packages

    pkgs <- depMtrx[,1]
    passPkgs <- character()
    if (length(pkgs)) {
        installed <- (match(pkgs, instPkgs[,"Package"], nomatch=0L) > 0L)

        curPkgs <- depMtrx[installed,,drop=FALSE]
        if (nrow(curPkgs)) {
            passVersReq <- apply(curPkgs, 1L, function(x) {
                pkgVers <- instPkgs[instPkgs[,1]==x[1L],"Version"]
                if (is.na(x[2L])||
                    (compareDependsPkgVersion(pkgVers,
                                              x[2L], x[3L]) >= 0))
                    TRUE
                else
                    FALSE
            })
            passPkgs <- c(passPkgs,curPkgs[passVersReq,1])

            return(which(match(depMtrx[,1],passPkgs,nomatch=0L) > 0L))
        }
    }

    return(numeric())
}

foundDepends <- function(depMtrx, contriburl=getOption("repos")) {
    out <- list(Found=list())
    foundRows <- numeric()

    if(is.null(contriburl))
        contriburl <-
            utils::contrib.url(c(CRAN = getOption("repos")["CRAN"],
                                 BIOC = getOption("BIOC")))


    for (j in seq_along(contriburl)) {
        cur <- character()
        cran <- utils::available.packages(contriburl=contriburl[j])

        if (nrow(depMtrx) > 0) {
            for (i in 1L:nrow(depMtrx)) {
                found <- FALSE
                cranRow <- which(depMtrx[i,1] == cran[,1])
                if (length(cranRow)) {
                    ## Found it in repos
                    if (is.na(depMtrx[i,2])) # no version, automatically okay
                        found <- TRUE
                    else if(compareDependsPkgVersion(cran[cranRow, "Version"],
                                                     depMtrx[i,2],
                                                     depMtrx[i,3]))
                        found <- TRUE
                }
                if (found) {
                    foundRows <- c(foundRows,i)
                    cur <- c(cur,depMtrx[i,1])
                }
            }
        }

        if (length(cur))
            out$Found[contriburl[j]] <- cur
    }

    if (length(foundRows) != nrow(depMtrx))
        out$NotFound <- depMtrx[-foundRows,,drop=FALSE]

    out
}

compareDependsPkgVersion <- function(curVersion, versOper, versionReq) {
    ## Returns -1 if FALSE, 0 or 1 if TRUE
    if(versOper == ">=")
        return(utils::compareVersion(curVersion, versionReq))
    if(versOper == "<=")
        return(utils::compareVersion(versionReq, curVersion))
    else
        stop("bad operand")
}

reduceDepends <- function(depMtrx, quietly=TRUE) {
    if ((is.null(depMtrx))||nrow(depMtrx)==0)
        return(character())

    pkgList <- split(depMtrx, depMtrx[,1])
    out <- lapply(pkgList, function(x, quietly) {
        pkgMtrx <- matrix(x,ncol=3L)
        ## there are no version requirements so just return
        ## the pkg name
        if (all(is.na(pkgMtrx[,2])))
            outRow <- 1
        else {
            ## Have version requirements
            ## Get the maximum ">=" requirement if one exists
            gts <- pkgMtrx[pkgMtrx[,2] == ">=",,drop=FALSE]
            if (nrow(gts) > 0) {
               maxGts <- gts[1,3]
               outRow <- 1
               for (i in 1L:nrow(gts)) {
                   if (utils::compareVersion(gts[i,3], maxGts) > 0) {
                       maxGts <- gts[i,3]
                       outRow <- i
                   }
               }
            }

            ## Find the minimal <= requirement if one exists
            lts <- pkgMtrx[pkgMtrx[,2] == "<=",,drop=FALSE]
            if (nrow(lts) > 0) {
                minLts <- lts[1,3]
                minRow <- 1
                for (i in 1L:nrow(lts)) {
                    if (utils::compareVersion(lts[i,3], minLts) < 0) {
                        minLts <- lts[i,3]
                        minRow <- i
                    }
                }
                ## If there is a maxGts and it is larger then
                ## the minLts then we need to record both
                if (exists(maxGts))
                    if (maxGts > minLts)
                        outRow <- c(outRow, minRow)
                else
                    outRow <- minRow
            }
            if(quietly == FALSE)
                warning(gettextf("Package '%s' had its dependencies reduced to a minimal set.",
                                 pkgMtrx[1,]),
                        domain = NA)
        }
	pkgMtrx[outRow,]
    }, quietly)

    matrix(unlist(out), ncol=3L, byrow=TRUE)
}

depMtrxToStrings <- function(depMtrx) {
    if (length(depMtrx)) {
        apply(depMtrx, 1L, function(x){
            if (is.na(x[2L]))
                x[1L]
            else
                paste(x[1L]," (",x[2L]," ",x[3L],")",sep="")
        })
    }
    else
        character()
}

installFoundDepends <- function(depPkgList, ...) {
    urls <- names(depPkgList)
    for (i in seq_along(depPkgList)) {
        if (length(depPkgList[[i]]))
            utils::install.packages(depPkgList[[i]],
                                    contriburl = urls[i],
                                    ...)
    }

    NULL
}
#  File src/library/tools/R/read.00Index.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

read.00Index <-
function(file)
{
    if(is.character(file)) {
        if(file == "") file <- stdin()
        else {
            file <- file(file, "r")
            on.exit(close(file))
        }
    }
    if(!inherits(file, "connection"))
        stop(gettextf("argument '%s' must be a character string or connection",
                      file),
             domain = NA)

    y <- matrix("", nrow = 0L, ncol = 2L)
    x <- paste(readLines(file), collapse = "\n")

    ## <FIXME>
    ## We cannot necessarily assume that the 00Index-style file to be
    ## read in was generated by @code{Rdindex()} or by R using
    ## formatDL(style = "table").  In particular, some packages have
    ## 00Index files with (section) headers and footers in addition to
    ## the data base chunks which are description lists rendered in
    ## tabular form.  Hence, we need some heuristic for identifying the
    ## db chunks.  Easy to the human eye (is there a column for aligning
    ## entries?) but far from trivial ... as a first approximation we
    ## try to consider chunks containing at least one tab or three
    ## spaces a db chunk.  (A better heuristic would be the following:
    ## entries rendered in one line have item and description separated
    ## by at least 3 spaces or tabs; entries with a line break have
    ## continuation lines starting with whitespace (no test whether for
    ## alignment).  If a chunk is made of such entries only it is
    ## considered a db chunk.  But not all current packages follow this
    ## scheme.  Argh.)
    ## Clearly we need to move to something better in future versions.
    ## </FIXME>

    ## First split into paragraph chunks separated by whitespace-only
    ## lines.
    for(chunk in unlist(strsplit(x, "\n[ \t\n]*\n"))) {
        entries <- tryCatch({
            if(!grepl("(   |\t)", chunk))
                NULL
            else {
                ## Combine entries with continuation lines.
                chunk <- gsub("\n[ \t]+", "\t", chunk)
                ## Split into lines and then according to whitespace.
                x <- strsplit(unlist(strsplit(chunk, "\n")), "[ \t]")
                cbind(unlist(lapply(x, "[[", 1L)),
                      unlist(lapply(x, function(t) {
                          paste(t[-c(1L, which(!nzchar(t)))],
                                collapse = " ")
                      })))
            }
        },
                            error = identity)
        if(!inherits(entries, "error") && NCOL(entries) == 2L)
            y <- rbind(y, entries)
    }
    colnames(y) <- c("Item", "Description")
    y
}
#  File src/library/tools/R/readNEWS.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

readNEWS <- function(file = file.path(R.home(), "NEWS"),
                     trace = FALSE, chop = c("first", "1", "par1", "keepAll"))
{
    ## Purpose: read R's NEWS file - or a file similarly organized
    ## ----------------------------------------------------------------------
    ## Arguments: trace: is used in  "inner functions"
    ## ----------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 26 Jun 2006, 15:34

    p0 <- function(...) paste(..., sep="")

    rmIniTABs	   <- function(ch) sub("^\t+", "", ch)
    rmTABs	   <- function(ch) gsub("\t+", "", ch)
    collapseWSpace <- function(ch) gsub("[\t ]+", " ", ch)
    "%nIN%" <- function(x,table) is.na(match(x, table))

    chop1st <- function(cvec) {
	## The first non-empty line
	n <- length(cvec); if(n <= 1L) return(cvec)
	## else	 n >= 2
	empty <- grep("^[\t ]*$", cvec)
	cvec[if(any(!empty)) which(!empty)[1L] else 1]
    }

    chopPara <- function(cvec) {
	n <- length(cvec); if(n <= 1L) return(cvec)
	## else	 n >= 2
	empty <- grep("^[\t ]*$", cvec)
	## the first non-empty ``from the right''
	nm <- 1L:n %nIN% (n + 1 - rev(empty))
	if(any(nm))
	    cvec[1L:(n - (which(nm)[1L] - 1))]
	else ## all are empty; return just one
	    cvec[1L]
    }

### FIXME: default for 'chop' should be (something like) "dropEmptyTrail"

    cl <- match.call()
    E.prefix <- "^    o[\t ]+"
    chop <- match.arg(chop)
    chopFun <- switch(chop,
		      "first" = chop1st,
		      "1" = function(x) x[1L],
		      "par1" = chopPara,
		      "keepAll" = function(x)x)

    parseEntry <- function(ll)
    {
	## Purpose: parse a single NEWS entry
	## Arguments: ll: lines of text (character vector)
	nl <- length(ll)
	ll[1L] <- sub(E.prefix, "", ll[1L])

	##cat("	    entry with",nl, " lines of text")
	chv <- collapseWSpace(ll)
	chopFun(chv)
    }

    parseSection <- function(ll, kind)
    {
	## Purpose: parse one section (e.g., "BUG FIXES") of NEWS
	## Arguments: ll: lines of text (character vector)
	nl <- length(ll)
	if(trace) cat("	    section '", kind,"' : ", nl, " lines", sep="")

	## if(trace) cat(head(ll, min(3, nl)), if(nl > 5) ".............", "", sep="\n")
	## if (nl > 3) if(trace) cat(tail(ll, min(2, nl-3)), "", sep="\n")

	iS <- grep(E.prefix, ll)
	if(trace) cat("	 with ", length(iS), "entries\n")
	entries <- as.list(iS)
	## entries have no labels !

	iS <- c(iS, nl+1L)
	for(i in seq_along(entries))
	    entries[[i]] <- parseEntry(ll[iS[i] : (iS[i+1] - 1L)])
	entries
    }

    parseVersion <- function(ll, ver)
    {
	## Purpose: parse NEWS of one "x.y.z" R version
	## Arguments: ll: lines of text (character vector)
	##	     ver: version number of version, e.g., '2.2.1 patched'
	if(trace) cat("	 parseVersion(*, ver =", ver,"),")
	s.pre <- "^[A-Z]+"
	iC <- grep(s.pre, ll)
	if(trace) cat("	 with ", length(iC), "sections\n")
	sections <- as.list(iC)
	names(sections) <- ll[iC]
	iC <- c(iC, length(ll)+1L) # such that	 iC[i] : (iC[i+1]-1)  makes sense
	for(i in seq_along(sections))
	    sections[[i]] <- parseSection(ll[(iC[i]+ 1L) : (iC[i+1] - 1L)],
					  kind = names(sections)[i])
	sections
    }

    parseSeries <- function(ll, ver)
    {
	## Purpose: parse NEWS of full (half year) "x.y" series of R version
	## Arguments: ll: lines of text (character vector)
	##	     ver: version number of series, e.g., '2.3'
	if(trace) cat("\nparseSeries(*, ver =", ver,"):\n")
	s.pre <- "^[\t ]*CHANGES IN R VERSION "
	iC <- grep(s.pre, ll)
	versions <- as.list(iC)
	names(versions) <- sub(s.pre, "", ll[iC])

	iC <- c(iC, length(ll)+1L) # such that	 iC[i] : (iC[i+1]-1)  makes sense
	for(i in seq_along(versions))
	    versions[[i]] <- parseVersion(ll[(iC[i]+ 1L) : (iC[i+1] - 1L)],
					  ver = names(versions)[i])
	versions
    }
    
    # Check if the lines are in a native encoding
    # but have a UTF-8 byte-order mark
    
    hasBOM <- function(lines) {
        length(lines) >= 1 &&
    	Encoding(line <- lines[1]) == "unknown" && 
    	nchar(line, type="bytes") >= 3 &&
    	identical( as.integer(charToRaw(line)[1:3]),
    	                         c(0xefL, 0xbbL, 0xbfL) )
    }

    tfile <- file
    if(is.character(file)) {
        tfile <- normalizePath(file)
        file <- file(file, "r")
        on.exit(close(file))
    }
    if(!inherits(file, "connection"))
        stop("'file' must be a character string or connection")
    if(!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
    ## We could read in parts ...
    ll <- readLines(file)
    if (hasBOM(ll)) Encoding(ll) <- "UTF-8"
    
    nl <- length(ll)
    if(trace) {
        if(is.character(tfile))
            cat("successfully read ", nl, " lines from ",
                sQuote(tfile), "\n", sep="")
        else
            cat("successfully read ", nl, " lines\n", sep="")
    }

    s.pre <- "^\t*\\*[\t ]+ " ##  REGEXP prefix used to identify series begin
    s.post <- " SERIES NEWS"

    iS <- grep(p0(s.pre, "[1-9]+\\.[0-9]+", s.post), ll)
    series <- as.list(iS)
    names(series) <- sub(p0(s.post,"[\t ]*\\*$"), "",
			 sub(s.pre, "", ll[iS]))
    if(trace) {
        cat(s.post, ":\n")
        print(unlist(series))
        cat("Now parsing each: ...\n")
    }

    iS <- c(iS, nl+1L) # such that  iS[i] : (iS[i+1]-1)  makes sense
    ## At least for 'keepAll', we need to get rid of the whole series
    ## header (could also do this in general, of course):
    if(chop == "keepAll") {
        hl <- grep(sprintf("^\t%s",
                           paste(rep.int("\\*", 30), collapse = "")),
                   ll)
        for(i in seq_along(series))
            series[[i]] <-
                parseSeries(ll[(hl[2L * i] + 1L) :
                               (hl[2L * i + 1L] - 1L)],
                            ver = names(series)[i])
    } else {
        for(i in seq_along(series))
            series[[i]] <-
                parseSeries(ll[(iS[i] + 1L) : (iS[i+1] - 1L)],
                            ver = names(series)[i])
    }
    attr(series, "call") <- cl
    class(series) <- "newsTree"
    series
}

# Check for common formatting errors in a NEWS file.

checkNEWS <- function(file = file.path(R.home(), "NEWS")) {
    check <- function(item) {
	if (is.list(item)) return(all(unlist(lapply(item, check))))

	if (length(grep("^ o[[:blank:]]", item))) {
	    cat("Item marker found within item:\n", paste(item, collapse="\n"), "\n\n")
	    return(FALSE)
	}
	return(TRUE)
    }

    check(readNEWS(file, chop="keepAll") )
}
#  File src/library/tools/R/recode.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

### Remap a character string from encoded text to LaTeX escapes
encoded_text_to_latex <-
    function(x, encoding = c("latin1", "latin2", "latin9", "UTF-8", "utf8"))
{
    encoding <- match.arg(encoding)
    do_latin1 <- function(x) {
        xx <- charToRaw(x)
        paste(latin1table[as.integer(xx)], collapse="")}
    do_latin2 <- function(x) {
        xx <- charToRaw(x)
        paste(latin2table[as.integer(xx)], collapse="")}
    do_latin9 <- function(x) {
        xx <- charToRaw(x)
        paste(latin9table[as.integer(xx)], collapse="")}
    do_utf8 <- function(x) {
        xx <- utf8ToInt(x)
        y <- rep("?", length(x))
        y[xx < 512] <- utf8table[xx]
        y[xx == 0x02C6] <- "{\\textasciicircum}"
        y[xx == 0x02C7] <- "{\\textasciicaron}"
        y[xx == 0x02CA] <- "{\\textasciitilde}"
        y[xx == 0x02D8] <- "{\\textasciibreve}"
        y[xx == 0x02D9] <- "{\\textperiodcentered}"
        y[xx == 0x02DD] <- "{\\textacutedbl}"
        y[xx == 0x200C] <- "{\\textcompwordmark}"
        y[xx == 0x2018] <- "{\\textquoteleft}"
        y[xx == 0x2019] <- "{\\textquoteright}"
        y[xx == 0x201C] <- "{\\textquotedblleft}"
        y[xx == 0x201D] <- "{\\textquotedblright}"
        y[xx == 0x2020] <- "{\\textdagger}"
        y[xx == 0x2022] <- "{\\textbullet}"
        y[xx == 0x2026] <- "{\\textellipsis}"
        y[xx == 0x20AC] <- "{\\texteuro}"
        paste(y, collapse="")
    }
    as.vector(switch(encoding,
                        "latin1" = sapply(x, do_latin1),
                        "latin2" = sapply(x, do_latin2),
                        "latin9" = sapply(x, do_latin9),
                        "UTF-8"  = sapply(x, do_utf8),
                        "utf8"   = sapply(x, do_utf8),
                        stop("unimplemented encoding")
                        ))
}

latin1table <- c(
     rep("?", 31), ## omit 0x0
     ## 0x20 to %x7F
     rawToChar(as.raw(seq(32, 126)), multiple=TRUE), "?",
     ## 0x80 to 0x9F
     rep("?", 32),
     ## 0xA0 = 160 on
     "{\\nobreakspace}", "{\\textexclamdown}", "{\\textcent}", "{\\textsterling}", "{\\textcurrency}", "{\\textyen}", "{\\textbrokenbar}", "{\\S}",
     '\\"{}', "{\\textcopyright}", "{\\textordfeminine}", "{\\guillemotleft}", "{\\textlnot}", "\\-", "{\\textregistered}", "{\\a={}}",
     "{\\textdegree}", "{\\textpm}", "{\\mathtwosuperior}", "{\\maththreesuperior}", "{\\a'{}}", "{\\textmu}", "{\\P}", "{\\textperiodcentered}",
     "{\\c\\ }", "{\\mathonesuperior}", "{\\textordmasculine}", "{\\guillemotright}", "{\\textonequarter}", "{\\textonehalf}", "{\\textthreequarters}", "{\\textquestiondown}",
     "{\\a`A}", "{\\a'A}", "{\\^A}", "{\\~A}", '{\\"A}', "{\\r A}", "{\\AE}", "{\\c C}",
     "{\\a`E}", "{\\a'E}", "{\\^E}", "{\\a`I}", "{\\a'I}", "{\\^I}", "{\\~I}", '{\\"I}',
     "{\\DH}", "{\\~N}", "{\\a`O}", "{\\a'O}", "{\\^O}", "{\\~O}", '{\\"O}', "{\\texttimes}",
     "{\\O}", "{\\a`u}", "{\\a'U}", "{\\^U}", '{\\"U}', "{\\a`Y}", "{\\TH}", "{\\ss}",
     "{\\a`a}", "{\\a'a}", "{\\^a}", "{\\~a}", '{\\"a}', "{\\r a}", "{\\ae}", "{\\c c}",
     "{\\a`e}", "{\\a'e}", "{\\^e}", '{\\"e}',"{\\a`\\i}", "{\\a'\\i}", "{\\^\\i}", '{\\"\\i}',
     "{\\dh}", "{\\~n}", "{\\a`o}", "{\\a'o}", "{\\^o}", "{\\~o}", '{\\"o}', "{\\textdiv}",
     "{\\o}", "{\\a`u}", "{\\a'u}", "{\\^u}", '{\\"u}', "{\\a`y}", "{\\th}", '{\\"y}'
     )

latin2table <- c(
     rep("?", 31), ## omit 0x0
     ## 0x20 to %x7F
     rawToChar(as.raw(seq(32, 126)), multiple=TRUE), "?",
     ## 0x80 to 0x9F
     rep("?", 32),
     ## 0xA0 = 160 on
     "{\\nobreakspace}", "{\\k A}", "{\\u{}}", "{\\L}", "{\\textcurrency}", "{\\v L}", "{\\a'S}", "{\\S}",
     '\\"{}', "{\\v S}", "{\\c S}", "{\\v T}", "{\\\'Z}", "\\-", "{\\v Z}", "{\\.Z}",
     "{\\textdegree}", "{\\k A}", "{\\k\\ }", "{\\l}", "{\\a'{}}", "{\\v l}", "{\\a's}", "{\\v{}}",
     "{\\c\\ }", "{\\v s}", "{\\c s}", "{\\v t}", "{\\'z}", "{\\H{}}", "{\\v z}", "{\\.z}",
     "{\\a'R}", "{\\a'A}", "{\\^A}", "{\\u A}", '{\\"A}', "{\\'L}", "{\\a'C}", "{\\c C}",
     "{\\v C}", "{\\a'E}", "{\\k E}", '{\\"E}', "{\\v E}", "{\\'I}", "{\\^I}", '{\\v D}',
     "{\\DJ}", "{\\a'N}", "{\\v N}", "{\\a'O}", "{\\^O}", "{\\H O}", '{\\"O}', "{\\texttimes}",
     "{\\v R}", "{\\r U}", "{\\a'U}", "{\\H U}", '{\\"U}', "{\\a`Y}", "{\\c I}", "{\\ss}",
     "{\\a'r}", "{\\a'a}", "{\\^a}", "{\\u a}", '{\\"a}', "{\\'l}", "{\\a'c}", "{\\c c}",
     "{\\v c}", "{\\a'e}", "{\\k e}", '{\\"e}', "{\\v e}", "{\\'\\i}", "{\\^\\i}", '{\\v d}',
     "{\\dj}", "{\\a'n}", "{\\c n}", "{\\a'o}", '{\\"a}', "{\\H o}", '{\\"o}', "{\\textdiv}",
     "{\\v r}", "{\\r u}", "{\\a'u}", "{\\H u}", '{\\"u}', "{\\a`y}", "{\\c t}", '{\\.{}}'
     )

latin9table <- c(
     rep("?}", 31),
     ## 0x20 to %x7F
     rawToChar(as.raw(seq(32, 126)), multiple=TRUE), "?}",
     ## 0x80 to 0x9F
     rep("?}", 32),
     ## 0xA0 = 160 on
     "{\\nobreakspace}", "{\\textexclamdown}", "{\\textcent}", "{\\textsterling}", "{\\texteuro}", "{\\textyen}", "{\\v S}", "{\\S}",
     '{\\v s}', "{\\copyright}", "{\\textordfeminine}", "{\\guillemotleft}", "{\\textlnot}", "\\-", "{\\textregistered}", "{\\a={}}",
     "{\\textdegree}", "{\\textpm}", "{\\mathtwosuperior}", "{\\maththreesuperior}", "{\\v Z}", "{\\textmu}", "{\\P}", "{\\textperiodcentered}",
     "{\\v z}", "{\\mathonesuperior}", "{\\textordmasculine}", "{\\guillemotright}", "{\\OE}", "{\\oe}", '{\\"Y}', "{\\textquestiondown}",
     "{\\a`A}", "{\\a'A}", "{\\^A}", "{\\~A}", '{\\"A}', "{\\r A}", "{\\AE}", "{\\c C}",
     "{\\a`E}", "{\\a'E}", "{\\^E}", "{\\a`I}", "{\\a'I}", "{\\^I}", "{\\~I}", '{\\"I}',
     "{\\DH}", "{\\~N}", "{\\a`O}", "{\\a'O}", "{\\^O}", "{\\~O}", '{\\"O}', "{\\texttimes}",
     "{\\O}", "{\\a`u}", "{\\a'U}", "{\\^U}", '\\"U', "{\\a`Y}", "{\\TH}", "{\\ss}",
     "{\\a`a}", "{\\a'a}", "{\\^a}", "{\\~a}", '{\\"a}', "{\\r a}", "{\\ae}", "{\\c c}",
     "{\\a`e}", "{\\a'e}", "{\\^e}", '{\\"e}',"{\\a`\\i}", "{\\a'\\i}", "{\\^\\i}", '{\\"\\i}',
     "{\\dh}", "{\\~n}", "{\\a`o}", "{\\a'o}", "{\\^o}", "{\\~o}", '{\\"o}', "{\\textdiv}",
     "{\\o}", "{\\a`u}", "{\\a'u}", "{\\^u}", '\\"u', "{\\a`y}", "{\\th}", '{\\"y}'
     )

utf8table <- c(latin1table, rep("?", 256))

utf8table[0x0102:0x107] <-
    c("{\\u A}","{\\u a}", "{\\k A}", "{\\k a}", "{\\a'C}", "{\\a'c}")
utf8table[0x010C:0x111] <-
    c( "{\\v C}","{\\v c}","{\\v D}","{\\v d}","{\\DJ}","{\\dj}")

utf8table[0x0118:0x11B] <- c("{\\k E}","{\\k e}", "{\\v E}","{\\v e}")
utf8table[0x011E:0x11F] <- c("{\\u G}","{\\u g}")
utf8table[0x0130:0x131] <- c("{\\.I}","{\\i}")
utf8table[0x0139:0x13A] <- c("{\\a'L}","{\\a'l}")
utf8table[0x013D:0x13E] <- c("{\\v L}","{\\v l}")
utf8table[0x0141L:0x144] <- c("{\\L}","{\\l}","{\\a'N}","{\\a'n}")
utf8table[0x0147:0x14B] <- c("{\\v N}","{\\v N}","?","{\\NG}","{\\ng}")
utf8table[0x0150:0x155] <- c("{\\H O}","{\\H o}","{\\OE}","{\\oe}","{\\a'r}","{\\a'r}")
utf8table[0x0158:0x15B] <- c("{\\v R}","{\\v r}","{\\a'S}","{\\a's}")
utf8table[0x015E:0x165] <- c("{\\c S}","{\\c S}","{\\v S}","{\\c s}",
                             "{\\c T}","{\\c t}","{\\v T}","{\\c t}")
utf8table[0x016E:0x171] <- c("{\\r U}","{\\r u}","{\\H U}","{\\H u}")
utf8table[0x0178:0x17E] <- c('{\\"Y}',"{\\a'Z}","{\\a'z}","{\\.Z}", "{\\.z}","{\\v Z}","{\\v z}")
utf8table[0x0192] <- "{\\textflorin}"
#  File src/library/tools/R/testing.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

## functions principally for testing R and packages

massageExamples <- function(pkg, files, outFile = stdout())
{
    if(file_test("-d", files[1]))
        files <- sort(Sys.glob(file.path(files, "*.R")))

    if(is.character(outFile)) {
        out <- file(outFile, "wt")
        on.exit(close(out))
    } else out <- outFile

#    lines <- readLines(file.path(R.home("share"), "R", "examples-header.R"))
#    cat(sub("@PKG@", pkg, lines), sep = "\n", file = out)
    lines <- c(paste('pkgname <- "', pkg, '"', sep =""),
               'source(file.path(R.home("share"), "R", "examples-header.R"))',
               "options(warn = 1)")
    cat(lines, sep = "\n", file = out)
    if(.Platform$OS.type == "windows")
        cat("options(pager = \"console\")\n", file = out)

    if(pkg == "tcltk") {
        if(capabilities("tcltk")) cat("require('tcltk')\n\n", file = out)
        else cat("q()\n\n", file = out)
    } else if(pkg != "base")
        cat("library('", pkg, "')\n\n", sep = "", file = out)

    cat("assign(\".oldSearch\", search(), pos = 'CheckExEnv')\n", file = out)
    ##cat("assign(\".oldNS\", loadedNamespaces(), pos = 'CheckExEnv')\n", file = out)

    for(file in files) {
        nm <- sub("\\.R$", "", basename(file))
        ## make a syntactic name out of the filename
        nm <- gsub("[^- .a-zA-Z0-9_]", ".", nm, perl = TRUE, useBytes = TRUE)
        if (pkg == "graphics" && nm == "text") next
        if(!file.exists(file)) stop("file ", file, " cannot be opened")
        lines <- readLines(file)
        have_examples <- any(grepl("_ Examples _|### \\*+ Examples",
                                   lines, perl = TRUE, useBytes = TRUE))
        ## skip comment lines
        com <- grep("^#", lines, perl = TRUE, useBytes = TRUE)
        lines1 <- if(length(com)) lines[-com] else lines
        have_par <- any(grepl("[^a-zA-Z0-9.]par\\(|^par\\(",
                                lines1, perl = TRUE, useBytes = TRUE))
        have_contrasts <- any(grepl("options\\(contrasts",
                                   lines1, perl = TRUE, useBytes = TRUE))

        if(have_examples)
            cat("cleanEx()\nnameEx(\"", nm, "\")\n", sep = "", file = out)

        cat("### * ", nm, "\n\n", sep = "", file = out)
        cat("flush(stderr()); flush(stdout())\n\n", file = out)
        dont_test <- FALSE
        for (line in lines) {
            if(any(grepl("^[[:space:]]*## No test:", line, perl = TRUE, useBytes = TRUE)))
                dont_test <- TRUE
            if(!dont_test) cat(line, "\n", sep = "", file = out)
            if(any(grepl("^[[:space:]]*## End\\(No test\\)",
                         line, perl = TRUE, useBytes = TRUE)))
                dont_test <- FALSE
        }

        if(have_par)
            cat("graphics::par(get(\"par.postscript\", pos = 'CheckExEnv'))\n", file = out)
        if(have_contrasts)
            cat("options(contrasts = c(unordered = \"contr.treatment\",",
                "ordered = \"contr.poly\"))\n", sep="", file = out)
    }

    cat(readLines(file.path(R.home("share"), "R", "examples-footer.R")),
        sep="\n", file = out)
}

## compares 2 files
Rdiff <- function(from, to, useDiff = FALSE, forEx = FALSE)
{
    clean <- function(txt)
    {
        ## remove R header
        if(length(top <- grep("^(R version|R : Copyright)", txt,
                              perl = TRUE, useBytes = TRUE)) &&
           length(bot <- grep("quit R.$", txt, perl = TRUE, useBytes = TRUE)))
            txt <- txt[-(top[1]:bot[1])]
        ## remove BATCH footer
        nl <- length(txt)
        if(grepl("^> proc.time()", txt[nl-2])) txt <- txt[1:(nl-3)]
        ## regularize fancy quotes.
        txt <- gsub("(\xe2\x80\x98|\xe2\x80\x99)", "'", txt,
                      perl = TRUE, useBytes = TRUE)
        if(.Platform$OS.type == "windows") # not entirely safe ...
            txt <- gsub("(\x93|\x94)", "'", txt, perl = TRUE, useBytes = TRUE)
        pat <- '(^Time |^Loading required package|^Package [A-Za-z][A-Za-z0-9]+ loaded|^<(environment|promise|pointer): )'
        txt[!grepl(pat, txt, perl = TRUE, useBytes = TRUE)]
    }
    clean2 <- function(txt)
    {
        eoh <- grep("^> options\\(warn = 1\\)$", txt)
        if(length(eoh)) txt[-(1:eoh[1])] else txt
    }

    left <- clean(readLines(from))
    right <- clean(readLines(to))
    if (forEx) {
        left <- clean2(left)
        right <- clean2(right)
    }
    if (!useDiff && (length(left) == length(right))) {
        bleft <- gsub("[[:space:]]+", " ", left)
        bright <- gsub("[[:space:]]+", " ", right)
        if(all(bleft == bright)) return(0L)
        cat("\n")
        diff <- bleft != bright
        ## FIXME do run lengths here
        for(i in which(diff)) {
            cat(i,"c", i, "\n< ", left[i], "\n", "---\n> ", right[i], "\n",
                sep = "")
        }
        return(1L)
    } else {
        ## FIXME: use C code, or something like merge?
        ## The files can be very big.
        if(!useDiff) cat("\nfiles differ in number of lines:\n")
        a <- tempfile()
        b <- tempfile()
        writeLines(left, a)
        writeLines(right, b)
        return(system(paste("diff -bw", shQuote(a), shQuote(b))))
    }
}

testInstalledPackages <-
    function(outDir = ".", errorsAreFatal = TRUE,
             scope = c("both", "base", "recommended"),
             types = c("examples", "tests", "vignettes"))
{
    ow <- options(warn = 1)
    on.exit(ow)
    scope <- match.arg(scope)
    status <- 0L
    pkgs <- character()
    known_packages <- .get_standard_package_names()
    if (scope %in% c("both", "base"))
        pkgs <- known_packages$base
    if (scope %in% c("both", "recommended"))
        pkgs <- c(pkgs, known_packages$recommended)
    ## It *should* be an error if any of these are missing
    for (pkg in pkgs) {
        res <- testInstalledPackage(pkg, .Library, outDir, types)
        if (res) {
            status <- 1L
            msg <- gettextf("testing '%s' failed", pkg)
            if (errorsAreFatal) stop(msg, domain=NA, call.=FALSE)
            else warning(msg, domain=NA, call.=FALSE, immediate.=TRUE)
        }
    }
    return(invisible(status))
}

testInstalledPackage <-
    function(pkg, lib.loc = NULL, outDir = ".",
             types = c("examples", "tests", "vignettes"))
{
    types <- pmatch(types, c("examples", "tests", "vignettes"))
    pkgdir <- .find.package(pkg, lib.loc)
    exdir <- file.path(pkgdir, "R-ex")
    owd <- setwd(outDir)
    on.exit(setwd(owd))

    if (1 %in% types) { # && file_test("-d", exdir)) {
        message("\nCollecting examples for package ", sQuote(pkg))
        Rfile <- .createExdotR(pkg, pkgdir)
        if (length(Rfile)) {
            outfile <- paste(pkg, "-Ex.Rout", sep = "")
            failfile <- paste(outfile, "fail", sep = "." )
            savefile <- paste(outfile, "prev", sep = "." )
            if (file.exists(outfile)) file.rename(outfile, savefile)
            unlink(failfile)
            message("Running examples in package ", sQuote(pkg))
            ## Create as .fail in case this R session gets killed
            cmd <- paste(shQuote(file.path(R.home(), "bin", "R")),
                         "CMD BATCH --vanilla --no-timing",
                         shQuote(Rfile), shQuote(failfile))
            if (.Platform$OS.type == "windows") Sys.setenv(R_LIBS="")
            else cmd <- paste("R_LIBS=", cmd)
            res <- system(cmd)
            if (res) return(invisible(1L)) else file.rename(failfile, outfile)

            savefile <- paste(outfile, "prev", sep = "." )
            if (file.exists(savefile)) {
                message("  Comparing ", sQuote(outfile), " to ",
                        sQuote(basename(savefile)), " ...", appendLF = FALSE)
                res <- Rdiff(outfile, savefile)
                if (!res) message(" OK")
            }
        } else warning("no examples found")
    }

    ## FIXME merge with code in .runPackageTests
    if (2 %in% types && file_test("-d", d <- file.path(pkgdir, "tests"))) {
        this <- paste(pkg, "tests", sep="-")
        unlink(this, recursive = TRUE)
        dir.create(this)
        ## system(paste("cp -pr", file.path(d, "*"), this))
        file.copy(Sys.glob(file.path(d, "*")), this, recursive = TRUE)
        setwd(this)
        message("Running specific tests for package ", sQuote(pkg))
        Rfiles <- dir(".", pattern="\\.R$")
        for(f in Rfiles) {
            message("  Running ", sQuote(f))
            outfile <- paste(f, "out", sep = "")
            cmd <- paste(shQuote(file.path(R.home(), "bin", "R")),
                         "CMD BATCH --vanilla --no-timing",
                         shQuote(f), shQuote(outfile))
            cmd <- if (.Platform$OS.type == "windows") paste(cmd, "LANGUAGE=C")
            else paste("LANGUAGE=C", cmd)
           res <- system(cmd)
            if (res) {
                file.rename(outfile, paste(outfile, "fail", sep="."))
                return(invisible(1L))
            }
            savefile <- paste(outfile, "save", sep = "." )
            if (file.exists(savefile)) {
                message("  Comparing ", sQuote(outfile), " to ",
                        sQuote(savefile), " ...", appendLF = FALSE)
                res <- Rdiff(outfile, savefile)
                if (!res) message(" OK")
            }
        }
        setwd(owd)
    }

    if (3 %in% types && file_test("-d", d <- file.path(pkgdir, "doc"))) {
        message("Running vignettes for package ", sQuote(pkg))
        checkVignettes(pkg, lib.loc, latex = FALSE, weave =TRUE)
    }

    invisible(0L)
}

## run all the tests in a directory: for use by R CMD check.
## trackObjs has .Rin files

.runPackageTestsR <- function(...)
{
    cat("\n");
    status <- .runPackageTests(...)
    q("no", status = status)
}

## used by R CMD check
.runPackageTests <- function(use_gct = FALSE, use_valgrind = FALSE)
{
    runone <- function(f)
    {
        message("  Running ", sQuote(f))
        outfile <- paste(f, "out", sep = "")
        cmd <- paste(shQuote(file.path(R.home(), "bin", "R")),
                     "CMD BATCH --vanilla --no-timing",
                     if(use_valgrind) "--debugger=valgrind",
                     shQuote(f), shQuote(outfile))
        if (.Platform$OS.type == "windows") {
            Sys.setenv(LANGUAGE="C")
            Sys.setenv(R_TESTS="startup.Rs")
        } else
            cmd <- paste("LANGUAGE=C", "R_TESTS=startup.Rs", cmd)
        res <- system(cmd)
        if (res) {
            file.rename(outfile, paste(outfile, "fail", sep="."))
            return(1L)
        }
        savefile <- paste(outfile, "save", sep = "." )
        if (file.exists(savefile)) {
            message("  Comparing ", sQuote(outfile), " to ",
                    sQuote(savefile), " ...", appendLF = FALSE)
            res <- Rdiff(outfile, savefile, TRUE)
            if (!res) message(" OK")
        }
        0L
    }

    file.copy(file.path(R.home("share"), "R", "tests-startup.R"), "startup.Rs")
    if (use_gct) cat("gctorture(TRUE)" , file = "startup.Rs", append = TRUE)
    nfail <- 0L ## allow for later running all tests even if some fail.
    Rinfiles <- dir(".", pattern="\\.Rin$")
    for(f in Rinfiles) {
        Rfile <- sub("\\.Rin$", ".R", f)
        message("  Creating ", sQuote(Rfile), domain = NA)
        cmd <- paste(shQuote(file.path(R.home(), "bin", "R")),
                     "CMD BATCH --no-timing --vanilla --slave", f)
        if (system(cmd))
            warning("creation of ", sQuote(Rfile), " failed")
        else if (file.exists(Rfile)) nfail <- nfail + runone(Rfile)
        if (nfail > 0) return(nfail)
    }

    Rfiles <- dir(".", pattern="\\.R$")
    for(f in Rfiles) {
        nfail <- nfail + runone(f)
        if (nfail > 0) return(nfail)
    }
    return(nfail)
}

.createExdotR <- function(pkg, pkgdir, silent = FALSE)
{
    Rfile <- paste(pkg, "-Ex.R", sep = "")
    ## might be zipped:
    exdir <- file.path(pkgdir, "R-ex")
    if (file_test("-d", exdir)) {
        if (file.exists(fzip <- file.path(exdir, "Rex.zip"))) {
            filedir <- tempfile()
            unzip(fzip, exdir = filedir)
            on.exit(unlink(filedir, recursive = TRUE))
        } else filedir <- exdir
    } else {
        db <- Rd_db(basename(pkgdir), lib.loc = dirname(pkgdir))
        if (!length(db)) {
            message("no parsed files found")
            return(invisible(NULL))
        }
        if (!silent) message("  Extracting from parsed Rd's ",
                             appendLF = FALSE, domain = NA)
        files <- names(db)
        filedir <- tempfile()
        dir.create(filedir)
        on.exit(unlink(filedir, recursive = TRUE))
        cnt <- 0L
        for(f in files) {
            ## names are 'fullpath.Rd' if from 'man' dir, 'topic' if from RdDB
            nm <- sub("\\.[Rr]d$", "", basename(f))
            Rd2ex(db[[f]],
                  file.path(filedir, paste(nm, "R", sep = ".")),
                  defines = NULL)
            cnt <- cnt + 1L
            if(!silent && cnt %% 10L == 0L)
                message(".", appendLF = FALSE, domain = NA)
        }
        if (!silent) message()
        nof <- length(Sys.glob(file.path(filedir, "*.R")))
        if(!nof) return(invisible(NULL))
    }
    massageExamples(pkg, filedir, Rfile)
    invisible(Rfile)
}

testInstalledBasic <- function(scope = c("basic", "devel", "both"))
{
    scope <- match.arg(scope)

    ## We need to force C collation: might not work
    Sys.setlocale("LC_COLLATE", "C")
    tests1 <- c("eval-etc", "simple-true", "arith-true", "lm-tests",
                "ok-errors", "method-dispatch", "d-p-q-r-tests")
    tests2 <- c("complex", "print-tests", "lapack", "datasets")
    tests3 <- c("reg-tests-1", "reg-tests-2", "reg-IO", "reg-IO2", "reg-S4")

    runone <- function(f, diffOK = FALSE, inC = TRUE)
    {
        f <- paste(f, "R", sep = ".")
        if (!file.exists(f)) {
            if (!file.exists(fin <- paste(f, "in", sep = "")))
                stop("file ", sQuote(f), " not found", domain = NA)
            message("creating ", sQuote(f))
            cmd <- paste(shQuote(file.path(R.home(), "bin", "R")),
                         "CMD BATCH --no-timing --vanilla --slave", fin)
            if (system(cmd))
                stop("creation of ", sQuote(f), " failed")
            on.exit(unlink(f))
        }
        message("  running code in ", sQuote(f))
        outfile <- paste(f, "out", sep = "")
        cmd <- paste(shQuote(file.path(R.home(), "bin", "R")),
                     "CMD BATCH --vanilla --no-timing",
                     shQuote(f), shQuote(outfile))
        extra <- paste("LANGUAGE=C", "R_DEFAULT_PACKAGES=", "SRCDIR=.")
        if (inC) extra <- paste(extra,  "LC_ALL=C")
        if (.Platform$OS.type == "windows") {
            Sys.setenv(LANGUAGE="C")
            Sys.setenv(R_DEFAULT_PACKAGES="")
            Sys.setenv(SRCDIR=".")
            ## ignore inC and hope
        } else cmd <- paste(extra, cmd)
        res <- system(cmd)
        if (res) {
            file.rename(outfile, paste(outfile, "fail", sep="."))
            message("FAILED")
            return(1L)
        }
        savefile <- paste(outfile, "save", sep = "." )
        if (file.exists(savefile)) {
            message("  comparing ", sQuote(outfile), " to ",
                    sQuote(savefile), " ...", appendLF = FALSE)
            res <- Rdiff(outfile, savefile, TRUE)
            if (!res) message(" OK")
            else if (!diffOK) return(1L)
        }
        0L
    }
    owd <- setwd(file.path(R.home(), "tests"))
    on.exit(setwd(owd))

    if (scope %in% c("basic", "both")) {
        message("running strict specific tests")
        for (f in tests1) if (runone(f)) return(1L)
        message("running sloppy specific tests")
        for (f in tests2) runone(f, TRUE)
        message("running regression tests")
        for (f in tests3) {
            if (runone(f)) return(invisible(1L))
            if (f == "reg-plot") {
                message("  comparing 'reg-plot.ps' to 'reg-plot.ps.save' ...",
                        appendLF = FALSE)
                system("diff reg-plot.ps reg-plot.ps.save")
                message("OK")
            }
        }
        runone("reg-tests-3", TRUE)
        message("running tests of plotting Latin-1")
        message("  expect failure or some differences if not in a Latin or UTF-8 locale")

        runone("reg-plot-latin1", TRUE, FALSE)
        message("  comparing 'reg-plot-latin1.ps' to 'reg-plot-latin1.ps.save' ...",
                appendLF = FALSE)
        system("diff reg-plot-latin1.ps reg-plot-latin1.ps.save")
        message("OK")
    }

    if (scope %in% c("devel", "both")) {
        message("running tests of consistency of as/is.*")
        runone("isas-tests")
        message("running tests of random deviate generation -- fails occasionally")
        runone("p-r-random-tests", TRUE)
        message("running tests of primitives")
        if (runone("primitives")) return(invisible(1L))
        message("running regexp regression tests")
        if (runone("utf8-regex", inC = FALSE)) return(invisible(1L))
        message("running tests to possibly trigger segfaults")
        if (runone("no-segfault")) return(invisible(1L))
    }

    invisible(0L)
}
#  File src/library/tools/R/tools-defunct.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

Rd_parse <-
function(file, text = NULL) .Defunct("parse_Rd")
#  File src/library/tools/R/tools-deprecated.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
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
## Deprecated in 2.1.0.
## </entry>
#  File src/library/tools/R/utils.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

### * File utilities.

### ** file_path_as_absolute

file_path_as_absolute <-
function(x)
{
    ## Turn a possibly relative file path absolute, performing tilde
    ## expansion if necessary.
    ## Seems the only way we can do this is 'temporarily' change the
    ## working dir and see where this takes us.
    if(!file.exists(epath <- path.expand(x)))
        stop(gettextf("file '%s' does not exist", x),
             domain = NA)
    cwd <- getwd()
    on.exit(setwd(cwd))
    if(file_test("-d", epath)) {
        ## Combining dirname and basename does not work for e.g. '.' or
        ## '..' on Unix ...
        setwd(epath)
        getwd()
    }
    else {
        setwd(dirname(epath))
        ## getwd() can be "/" or "d:/"
        file.path(sub("/$", "", getwd()), basename(epath))
    }
}

### ** file_path_sans_ext

file_path_sans_ext <-
function(x)
{
    ## Return the file paths without extensions.
    ## (Only purely alphanumeric extensions are recognized.)
    sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
}

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

### ** list_files_with_exts

list_files_with_exts <-
function(dir, exts, all.files = FALSE, full.names = TRUE)
{
    ## Return the paths or names of the files in @code{dir} with
    ## extension in @code{exts}.
    ## Might be in a zipped dir on Windows.
    if(file.exists(file.path(dir, "filelist")) &&
       any(file.exists(file.path(dir, c("Rdata.zip", "Rex.zip", "Rhelp.zip")))))
    {
        files <- readLines(file.path(dir, "filelist"))
        if(!all.files)
            files <- grep("^[^.]", files, value = TRUE)
    } else {
        files <- list.files(dir, all.files = all.files)
    }
    ## does not cope with exts with '.' in.
    ## files <- files[sub(".*\\.", "", files) %in% exts]
    patt <- paste("\\.(", paste(exts, collapse="|"), ")$", sep = "")
    files <- grep(patt, files, value = TRUE)
    if(full.names)
        files <- if(length(files))
            file.path(dir, files)
        else
            character()
    files
}

### ** list_files_with_type

list_files_with_type <-
function(dir, type, all.files = FALSE, full.names = TRUE,
         OS_subdirs = .OStype())
{
    ## Return a character vector with the paths of the files in
    ## @code{dir} of type @code{type} (as in .make_file_exts()).
    ## When listing R code and documentation files, files in OS-specific
    ## subdirectories are included (if present) according to the value
    ## of @code{OS_subdirs}.

    exts <- .make_file_exts(type)
    files <-
        list_files_with_exts(dir, exts, all.files = all.files,
                             full.names = full.names)

    if(type %in% c("code", "docs")) {
        for(os in OS_subdirs) {
            os_dir <- file.path(dir, os)
            if(file_test("-d", os_dir)) {
                os_files <- list_files_with_exts(os_dir, exts,
                                                 all.files = all.files,
                                                 full.names = FALSE)
                os_files <- file.path(if(full.names) os_dir else os,
                                      os_files)
                files <- c(files, os_files)
            }
        }
    }
    ## avoid ranges since they depend on the collation order in the locale.
    ## in particular, Estonian sorts Z after S.
    if(type %in% c("code", "docs")) { # only certain filenames are valid.
        files <- files[grep("^[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789]", basename(files))]
    }
    if(type %in% "demo") {           # only certain filenames are valid.
        files <- files[grep("^[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]", basename(files))]
    }
    files
}

### ** showNonASCII

showNonASCII <-
function(x)
{
    ## All that is needed here is an 8-bit encoding that includes ASCII.
    ## The only one we guarantee to exist is 'latin1'.
    ## The default sub=NA is faster.
    ind <- is.na(iconv(x, "latin1", "ASCII"))
    if(any(ind))
        cat(paste(which(ind), ": ",
                  iconv(x[ind], "latin1", "ASCII", sub="byte"), sep=""),
            sep="\n")
}

### * Text utilities.

### ** delimMatch

delimMatch <-
function(x, delim = c("{", "}"), syntax = "Rd")
{
    if(!is.character(x))
        stop("argument 'x' must be a character vector")
    ## FIXME: bytes or chars?
    if((length(delim) != 2L) || any(nchar(delim) != 1L))
        stop("argument 'delim' must specify two characters")
    if(syntax != "Rd")
        stop("only Rd syntax is currently supported")

    .Call(delim_match, x, delim)
}


### * LaTeX utilities

### ** texi2dvi

texi2dvi <-
function(file, pdf = FALSE, clean = FALSE, quiet = TRUE,
         texi2dvi = getOption("texi2dvi"), texinputs = NULL)
{
    ## Run texi2dvi on a latex file, or emulate it.

    if(is.null(texi2dvi) || !nzchar(texi2dvi))
        texi2dvi <- Sys.which("texi2dvi")

    envSep <- .Platform$path.sep
    Rtexmf <- file.path(R.home("share"), "texmf")
    ## "" forces use of default paths.
    texinputs <- paste(c(texinputs, Rtexmf, ""), collapse = envSep)
    ## not clear if this is needed, but works
    if(.Platform$OS.type == "windows")
        texinputs <- gsub("\\\\", "/", texinputs)

    otexinputs <- Sys.getenv("TEXINPUTS", unset = NA)
    if(is.na(otexinputs)) {
        on.exit(Sys.unsetenv("TEXINPUTS"))
        otexinputs <- "."
    } else on.exit(Sys.setenv(TEXINPUTS = otexinputs))
    Sys.setenv(TEXINPUTS = paste(otexinputs, texinputs, sep = envSep))
    bibinputs <- Sys.getenv("BIBINPUTS", unset = NA)
    if(is.na(bibinputs)) {
        on.exit(Sys.unsetenv("BIBINPUTS"), add = TRUE)
        bibinputs <- "."
    } else on.exit(Sys.setenv(BIBINPUTS = bibinputs, add = TRUE))
    Sys.setenv(BIBINPUTS = paste(bibinputs, texinputs, sep = envSep))
    bstinputs <- Sys.getenv("BSTINPUTS", unset = NA)
    if(is.na(bstinputs)) {
        on.exit(Sys.unsetenv("BSTINPUTS"), add = TRUE)
        bstinputs <- "."
    } else on.exit(Sys.setenv(BSTINPUTS = bstinputs), add = TRUE)
    Sys.setenv(BSTINPUTS = paste(bstinputs, texinputs, sep = envSep))

    if(nzchar(texi2dvi) && .Platform$OS.type != "windows") {
        opt_pdf <- if(pdf) "--pdf" else ""
        opt_quiet <- if(quiet) "--quiet" else ""
        opt_extra <- ""
        out <- .shell_with_capture(paste(shQuote(texi2dvi), "--help"))
        if(length(grep("--no-line-error", out$stdout)))
            opt_extra <- "--no-line-error"
        ## (Maybe change eventually: the current heuristics for finding
        ## error messages in log files should work for both regular and
        ## file line error indicators.)

        file.create(".timestamp")
        out <- .shell_with_capture(paste(shQuote(texi2dvi), opt_pdf,
                                         opt_quiet, opt_extra,
                                         shQuote(file)))

        ## We cannot necessarily rely on out$status, hence let us
        ## analyze the log files in any case.
        errors <- character()
        ## (La)TeX errors.
        log <- paste(file_path_sans_ext(file), "log", sep = ".")
        if(file_test("-f", log)) {
            lines <- .get_LaTeX_errors_from_log_file(log)
            if(length(lines))
                errors <- paste("LaTeX errors:",
                                paste(lines, collapse = "\n"),
                                sep = "\n")
        }
        ## BibTeX errors.
        log <- paste(file_path_sans_ext(file), "blg", sep = ".")
        if(file_test("-f", log)) {
            lines <- .get_BibTeX_errors_from_blg_file(log)
            if(length(lines))
                errors <- paste("BibTeX errors:",
                                paste(lines, collapse = "\n"),
                                sep = "\n")
        }

        msg <- ""
        if(out$status) {
            ## <NOTE>
            ## If we cannot rely on out$status, we could test for
            ##   if(out$status || length(errors))
            ## But shouldn't we be able to rely on out$status on Unix?
            ## </NOTE>
            msg <- gettextf("Running 'texi2dvi' on '%s' failed.", file)
            ## Error messages from GNU texi2dvi are rather terse, so
            ## only use them in case no additional diagnostics are
            ## available (e.g, makeindex errors).
            if(length(errors))
                msg <- paste(msg, errors, sep = "\n")
            else if(length(out$stderr))
                msg <- paste(msg, "Messages:",
                             paste(out$stderr, collapse = "\n"),
                             sep = "\n")
            if(!quiet)
                msg <- paste(msg, "Output:",
                             paste(out$stdout, collapse = "\n"),
                             sep = "\n")
        }

        ## Clean up as needed.
        if(clean) {
            out_file <- paste(file_path_sans_ext(file),
                              if(pdf) "pdf" else "dvi",
                              sep = ".")
            files <- list.files(all.files = TRUE) %w/o% c(".", "..",
                                                          out_file)
            file.remove(files[file_test("-nt", files, ".timestamp")])
        }
        file.remove(".timestamp")

        if(nzchar(msg))
            stop(msg, domain = NA)
        else if(!quiet)
            message(paste(paste(out$stderr, collapse = "\n"),
                          paste(out$stdout, collapse = "\n"),
                          sep = "\n"))
    } else if(nzchar(texi2dvi)) {       # Windows
        extra <- ""
        ext <- if(pdf) "pdf" else "dvi"
        pdf <- if(pdf) "--pdf" else ""
        file.create(".timestamp")
        quiet <- if(quiet) "--quiet" else ""

        ## look for MiKTeX (which this almost certainly is)
        ## and set the path to R's style files.
        ## -I works in MiKTeX >= 2.4, at least
        ver <- system(paste(shQuote(texi2dvi), "--version"), intern = TRUE)
        if(length(grep("MiKTeX", ver[1L]))) {
            paths <- paste ("-I", shQuote(texinputs))
            extra <- paste(extra, paste(paths, collapse = " "))
        }
        ## this only gives a failure in some cases, e.g. not for bibtex errors.
        system(paste(shQuote(texi2dvi), quiet, pdf,
                     shQuote(file), extra),
               intern=TRUE, ignore.stderr=TRUE)
        msg <- ""
        ## (La)TeX errors.
        log <- paste(file_path_sans_ext(file), "log", sep = ".")
        if(file_test("-f", log)) {
            lines <- .get_LaTeX_errors_from_log_file(log)
            if(length(lines))
                msg <- paste(msg, "LaTeX errors:",
                             paste(lines, collapse = "\n"),
                             sep = "\n")
        }
        ## BibTeX errors.
        log <- paste(file_path_sans_ext(file), "blg", sep = ".")
        if(file_test("-f", log)) {
            lines <- .get_BibTeX_errors_from_blg_file(log)
            if(length(lines))
                msg <- paste(msg, "BibTeX errors:",
                             paste(lines, collapse = "\n"),
                             sep = "\n")
        }

        if(nzchar(msg))
            msg <- paste(gettextf("running 'texi2dvi' on '%s' failed", file),
                         msg, "", sep = "\n")
        if(clean) {
            out_file <- paste(file_path_sans_ext(file), ext, sep = ".")
            files <- list.files(all.files = TRUE) %w/o% c(".", "..",
                                                          out_file)
            file.remove(files[file_test("-nt", files, ".timestamp")])
        }
        file.remove(".timestamp")

        if(nzchar(msg)) stop(msg, domain = NA)
    } else {
        ## Do not have texi2dvi
        ## Needed at least on Windows except for MiKTeX
        ## Note that this does not do anything about running quietly,
        ## nor cleaning, but is probably not used much anymore.

        texfile <- shQuote(file)
        base <- file_path_sans_ext(file)
        idxfile <- paste(base, ".idx", sep="")
        if(pdf) {
            latex <- Sys.getenv("PDFLATEX")
            if(!nzchar(latex)) latex <- "pdflatex"
        } else {
            latex <- Sys.getenv("LATEX")
            if(!nzchar(latex)) latex <- "latex"
        }
        bibtex <- Sys.getenv("BIBTEX")
        if(!nzchar(bibtex)) bibtex <- "bibtex"
        makeindex <- Sys.getenv("MAKEINDEX")
        if(!nzchar(makeindex)) makeindex <- "makeindex"
        if(system(paste(shQuote(latex), "-interaction=nonstopmode", texfile)))
            stop(gettextf("unable to run %s on '%s'", latex, file), domain = NA)
        nmiss <- length(grep("^LaTeX Warning:.*Citation.*undefined",
                             readLines(paste(base, ".log", sep = ""))))
        for(iter in 1L:10L) { ## safety check
            ## This might fail as the citations have been included in the Rnw
            if(nmiss) system(paste(shQuote(bibtex), shQuote(base)))
            nmiss_prev <- nmiss
            if(file.exists(idxfile)) {
                if(system(paste(shQuote(makeindex), shQuote(idxfile))))
                    stop(gettextf("unable to run %s on '%s'", makeindex, idxfile),
                         domain = NA)
            }
            if(system(paste(shQuote(latex), "-interaction=nonstopmode", texfile)))
                stop(gettextf("unable to run %s on '%s'", latex, file), domain = NA)
            Log <- readLines(paste(base, ".log", sep = ""))
            nmiss <- length(grep("^LaTeX Warning:.*Citation.*undefined", Log))
            if(nmiss == nmiss_prev &&
               !length(grep("Rerun to get", Log)) ) break
        }
    }
}

### * Internal utility variables.

### ** .BioC_version_associated_with_R_version

.BioC_version_associated_with_R_version <-
    numeric_version("2.5")
## (Could also use something programmatically mapping (R) 2.10.x to
## (BioC) 2.5, 2.9.x to 2.4, ..., 2.1.x to 1.6, but what if R 3.0.0
## comes out?)

### * Internal utility functions.

### ** %w/o%

## x without y, as in the examples of ?match.
`%w/o%` <-
function(x, y)
    x[!x %in% y]

### ** .OStype

.OStype <-
function()
{
    OS <- Sys.getenv("R_OSTYPE")
    if(nzchar(OS)) OS else .Platform$OS.type
}

### .R_top_srcdir

## Find the root directory of the source tree used for building this
## version of R (corresponding to Unix configure @top_srcdir@).
## Seems this is not recorded anywhere, but we can find our way ...

.R_top_srcdir_from_Rd <-
function() {
    filebase <-
        file_path_sans_ext(system.file("help", "tools.rdb",
                                       package = "tools"))
    path <- attr(fetchRdDB(filebase, "QC"), "Rdfile")
    ## We could use 5 dirname() calls, but perhaps more easily:
    substring(path, 1L, nchar(path) - 28L)
}

## Unfortunately,
##   .R_top_srcdir <- .R_top_srcdir_from_Rd()
## does not work because when tools is installed there are no Rd pages
## yet ...

### ** .eval_with_capture

.eval_with_capture <-
function(expr, type = NULL)
{
    ## Evaluate the given expression and return a list with elements
    ## 'value', 'output' and 'message' (with obvious meanings).

    ## <NOTE>
    ## The current implementation gives character() if capturing was not
    ## attempted of gave nothing.  If desired, one could modify the code
    ## to return NULL in the former case.
    ## </NOTE>

    if(is.null(type))
        capture_output <- capture_message <- TRUE
    else {
        type <- match.arg(type, c("output", "message"))
        capture_output <- type == "output"
        capture_message <- !capture_output
    }

    outcon <- file(open = "w+", encoding = "UTF-8")
    msgcon <- file(open = "w+", encoding = "UTF-8")
    if(capture_output) {
        sink(outcon, type = "output")
        on.exit(sink(type = "output"))
    }
    if(capture_message) {
        sink(msgcon, type = "message")
        on.exit(sink(type = "message"), add = capture_output)
    }
    on.exit({ close(outcon) ; close(msgcon) }, add = TRUE)

    value <- eval(expr)
    list(value = value,
         output = readLines(outcon, encoding = "UTF-8", warn = FALSE),
         message = readLines(msgcon, encoding = "UTF-8", warn = FALSE))
}


### ** .file_append_ensuring_LFs

.file_append_ensuring_LFs <-
function(file1, file2)
{
    ## Use a fast version of file.append() that ensures LF between
    ## files.
    .Internal(codeFiles.append(file1, file2))
}

### ** .find_owner_env

.find_owner_env <-
function(v, env, last = NA, default = NA) {
    while(!identical(env, last))
        if(exists(v, envir = env, inherits = FALSE))
            return(env)
        else
            env <- parent.env(env)
    default
}

### ** .get_BibTeX_errors_from_blg_file

.get_BibTeX_errors_from_blg_file <-
function(con)
{
    ## Get BibTeX error info, using non-header lines until the first
    ## warning or summary, hoping for the best ...
    lines <- readLines(con, warn = FALSE)
    if(any(ind <- is.na(nchar(lines, allowNA = TRUE))))
        lines[ind] <- iconv(lines[ind], "", "", sub = "byte")

    ## How can we find out for sure that there were errors?  Try
    ## guessing ... and peeking at tex-buf.el from AUCTeX.
    really_has_errors <-
        (length(grep("^---", lines)) ||
         regexpr("There (was|were) ([0123456789]+) error messages?",
                 lines[length(lines)]) > -1L)
    ## (Note that warnings are ignored for now.)
    ## MiKTeX does not give usage, so '(There were n error messages)' is
    ## last.
    pos <- grep("^(Warning|You|\\(There)", lines)
    if(!really_has_errors || !length(pos) ) return(character())
    ind <- seq.int(from = 3L, length.out = pos[1L] - 3L)
    lines[ind]
}

### ** .get_LaTeX_errors_from_log_file

.get_LaTeX_errors_from_log_file <-
function(con, n = 4L)
{
    ## Get (La)TeX lines with error plus n (default 4) lines of trailing
    ## context.
    lines <- readLines(con, warn = FALSE)
    if(any(ind <- is.na(nchar(lines, allowNA = TRUE))))
        lines[ind] <- iconv(lines[ind], "", "", sub = "byte")

    ## Try matching both the regular error indicator ('!') as well as
    ## the file line error indicator ('file:line:').
    pos <- grep("^(!|.*:[0123456789]+:)", lines)
    if(!length(pos)) return(character())
    ## Error chunk extends to at most the next error line.
    mapply(function(from, to) paste(lines[from : to], collapse = "\n"),
           pos, pmin(pos + n, c(pos[-1L], length(lines))))
}

### ** .get_contains_from_package_db

.get_contains_from_package_db <-
function(db)
{
    if("Contains" %in% names(db))
        unlist(strsplit(db["Contains"], "[[:space:]]+"))
    else
        character()
}

### ** .get_internal_S3_generics

.get_internal_S3_generics <-
function(primitive = TRUE) # primitive means 'include primitives'
{
    out <-
        ## Get the names of R internal S3 generics (via DispatchOrEval(),
        ## cf. zMethods.Rd).
        c("[", "[[", "$", "[<-", "[[<-", "$<-",
          "as.vector", "unlist",
          .get_S3_primitive_generics()
          ## ^^^^^^^ now contains the members of the group generics from
          ## groupGeneric.Rd.
          )
    if(!primitive)
        out <- out[!sapply(out, .is_primitive_in_base)]
    out
}

### ** .get_namespace_package_depends

.get_namespace_package_depends <-
function(dir)
{
    nsInfo <- .check_namespace(dir)
    depends <- c(sapply(nsInfo$imports, "[[", 1L),
                 sapply(nsInfo$importClasses, "[[", 1L),
                 sapply(nsInfo$importMethods, "[[", 1L))
    unique(sort(as.character(depends)))
}

### ** .get_namespace_S3_methods_db

.get_namespace_S3_methods_db <-
function(nsInfo)
{
    ## Get the registered S3 methods for an 'nsInfo' object returned by
    ## parseNamespaceFile(), as a 3-column character matrix with the
    ## names of the generic, class and method (as a function).
    S3_methods_list <- nsInfo$S3methods
    if(!length(S3_methods_list)) return(matrix(character(), ncol = 3L))
    idx <- is.na(S3_methods_list[, 3L])
    S3_methods_list[idx, 3L] <-
        paste(S3_methods_list[idx, 1L],
              S3_methods_list[idx, 2L],
              sep = ".")
    S3_methods_list
}

### ** .get_package_metadata

.get_package_metadata <-
function(dir, installed = FALSE)
{
    ## Get the package DESCRIPTION metadata for a package with root
    ## directory 'dir'.  If an unpacked source (uninstalled) package,
    ## base packages (have only a DESCRIPTION.in file with priority
    ## "base") and bundle packages (have a DESCRIPTION.in file and need
    ## additional metadata from the the bundle DESCRIPTION file) need
    ## special attention.
    dir <- file_path_as_absolute(dir)
    dfile <- file.path(dir, "DESCRIPTION")
    if(file_test("-f", dfile)) return(.read_description(dfile))
    if(installed) stop("File 'DESCRIPTION' is missing.")
    dfile <- file.path(dir, "DESCRIPTION.in")
    if(file_test("-f", dfile))
        meta <- .read_description(dfile)
    else
        stop("Files 'DESCRIPTION' and 'DESCRIPTION.in' are missing.")
    if(identical(as.character(meta["Priority"]), "base")) return(meta)
    ## Otherwise, this must be a bundle package.
    bdfile <- file.path(dirname(dir), "DESCRIPTION")
    if(file_test("-f", bdfile)) {
        file <- tempfile()
        on.exit(unlink(file))
        writeLines(c(readLines(bdfile), readLines(dfile)), file)
        .read_description(file)
        ## Using an anonymous tempfile (con <- file()) would work too
        ## for accumulating, e.g.,
        ##   con <- file(open = "w+")
        ##   on.exit(close(con))
        ##   writeLines(c(readLines(bdfile), readLines(dfile)), con)
        ## but .read_description() insists on an existing file (maybe
        ## this should be changed?).
    }
    else
        stop("Bundle 'DESCRIPTION' is missing.")
}

### ** .get_requires_from_package_db

.get_requires_from_package_db <-
function(db,
         category = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"))
{
    category <- match.arg(category)
    if(category %in% names(db)) {
        requires <- unlist(strsplit(db[category], ","))
        requires <-
            sub("^[[:space:]]*([[:alnum:].]+).*$", "\\1", requires)
        if(category == "Depends")
            requires <- requires[requires != "R"]
    }
    else
        requires <- character()
    requires
}

### ** .get_requires_with_version_from_package_db

.get_requires_with_version_from_package_db <-
function(db,
         category = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"))
{
    category <- match.arg(category)
    if(category %in% names(db)) {
        res <- .split_dependencies(db[category])
        if(category == "Depends") res[names(res) != "R"] else res
    } else list()
}

### ** .get_S3_generics_as_seen_from_package

.get_S3_generics_as_seen_from_package <-
function(dir, installed = TRUE, primitive = FALSE)
{
    ## Get the S3 generics "as seen from a package" rooted at
    ## @code{dir}.  Tricky ...
    if(basename(dir) == "base")
        env_list <- list()
    else {
        ## Always look for generics in the whole of the former base.
        ## (Not right, but we do not perform run time analyses when
        ## working off package sources.)  Maybe change this eventually,
        ## but we still cannot rely on packages to fully declare their
        ## dependencies on base packages.
        env_list <-
            list(baseenv(),
                 as.environment("package:graphics"),
                 as.environment("package:stats"),
                 as.environment("package:utils"))
        if(installed) {
            ## Also use the loaded namespaces and attached packages
            ## listed in the DESCRIPTION Depends and Imports fields.
            ## Not sure if this is the best approach: we could also try
            ## to determine which namespaces/packages were made
            ## available by loading the package (which should work at
            ## least when run from R CMD check), or we could simply
            ## attach every package listed as a dependency ... or
            ## perhaps do both.
            db <- .read_description(file.path(dir, "DESCRIPTION"))
            depends <- .get_requires_from_package_db(db, "Depends")
            imports <- .get_requires_from_package_db(db, "Imports")
            reqs <- intersect(c(depends, imports), loadedNamespaces())
            if(length(reqs))
                env_list <- c(env_list, lapply(reqs, getNamespace))
            ## note .packages give versioned names.
            reqs <- intersect(depends %w/o% loadedNamespaces(),
                              .packages())
            if(length(reqs))
                env_list <- c(env_list, lapply(reqs, .package_env))
            env_list <- unique(env_list)
        }
    }
    unique(c(.get_internal_S3_generics(primitive),
             unlist(lapply(env_list,
                           function(env) {
                               nms <- objects(envir = env,
                                              all.names = TRUE)
                               if(".no_S3_generics" %in% nms)
                                   character()
                               else Filter(function(f)
                                           .is_S3_generic(f, envir = env),
                                           nms)
                           }))))
}

### ** .get_S3_group_generics

.get_S3_group_generics <-
function()
    c("Ops", "Math", "Summary", "Complex")

### ** .get_S3_primitive_generics

.get_S3_primitive_generics <-
function(include_group_generics = TRUE)
{
    if(include_group_generics)
        c(base::.S3PrimitiveGenerics,
          "abs", "sign", "sqrt", "floor", "ceiling", "trunc", "round",
          "signif", "exp", "log", "expm1", "log1p",
          "cos", "sin", "tan", "acos", "asin", "atan",
          "cosh", "sinh", "tanh", "acosh", "asinh", "atanh",
          "lgamma", "gamma", "digamma", "trigamma",
          "cumsum", "cumprod", "cummax", "cummin",
          "+", "-", "*", "/", "^", "%%", "%/%", "&", "|", "!", "==",
          "!=", "<", "<=", ">=", ">",
          "all", "any", "sum", "prod", "max", "min", "range",
          "Arg", "Conj", "Im", "Mod", "Re")
    else
        base::.S3PrimitiveGenerics
}

### ** .get_standard_Rd_keywords

.get_standard_Rd_keywords <-
function()
{
    lines <- readLines(file.path(R.home("doc"), "KEYWORDS.db"))
    lines <- grep("^.*\\|([^:]*):.*", lines, value = TRUE)
    lines <- sub( "^.*\\|([^:]*):.*", "\\1", lines)
    lines
}

### ** .get_standard_package_names

## we cannot assume that file.path(R.home("share"), "make", "vars.mk")
## is installed, as it is not on Windows
.get_standard_package_names <-
local({
    lines <- readLines(file.path(R.home("share"), "make", "vars.mk"))
    lines <- grep("^R_PKGS_[[:upper:]]+ *=", lines, value = TRUE)
    out <- strsplit(sub("^R_PKGS_[[:upper:]]+ *= *", "", lines), " +")
    names(out) <-
        tolower(sub("^R_PKGS_([[:upper:]]+) *=.*", "\\1", lines))
    eval(substitute(function() {out}, list(out=out)), envir=NULL)
})

### ** .get_standard_repository_URLs

.get_standard_repository_URLs <-
function() {
    repos <- Sys.getenv("_R_CHECK_XREFS_REPOSITORIES_", "")
    repos <- if(nzchar(repos)) {
        .expand_BioC_repository_URLs(strsplit(repos, " +")[[1L]])
    } else {
        p <- file.path(Sys.getenv("HOME"), ".R", "repositories")
        if(file_test("-f", p)) {
            a <- .read_repositories(p)
            a[c("CRAN", "Omegahat", "BioCsoft", "BioCann", "BioCexp"),
              "URL"]
        } else {
            a <- .read_repositories(file.path(R.home("etc"),
                                              "repositories"))
            c("http://cran.r-project.org", a[3:6, "URL"])
        }
    }
    repos
}

### ** .get_standard_repository_db_fields

.get_standard_repository_db_fields <-
function()
    c("Package", "Version", "Priority",
      "Bundle", "Contains",
      "Depends", "Imports", "LinkingTo", "Suggests", "Enhances",
      "OS_type", "License")

### ** .is_ASCII

.is_ASCII <-
function(x)
{
    ## Determine whether the strings in a character vector are ASCII or
    ## not.
    as.logical(sapply(as.character(x),
                      function(txt)
                      all(charToRaw(txt) <= as.raw(127))))
}

### ** .is_ISO_8859

.is_ISO_8859 <-
function(x)
{
    ## Determine whether the strings in a character vector could be in
    ## some ISO 8859 character set or not.
    raw_ub <- charToRaw("\x7f")
    raw_lb <- charToRaw("\xa0")
    as.logical(sapply(as.character(x),
                      function(txt) {
                          raw <- charToRaw(txt)
                          all(raw <= raw_ub | raw >= raw_lb)
                      }))
}

### ** .is_primitive_in_base

.is_primitive_in_base <-
function(fname)
{
    ## Determine whether object named 'fname' found in the base
    ## environment is a primitive function.
    is.primitive(get(fname, envir = baseenv(), inherits = FALSE))
}

### ** .is_S3_generic

.is_S3_generic <-
function(fname, envir, mustMatch = TRUE)
{
    ## Determine whether object named 'fname' found in environment
    ## 'envir' is (to be considered) an S3 generic function.  Note,
    ## found *in* not found *from*, so envir does not have a default.
    ##
    ## If it is, does it despatch methods of fname?  We need that to
    ## look for possible methods as functions named fname.* ....
    ##
    ## Provided by LT with the following comments:
    ##
    ## This is tricky.  Figuring out what could possibly dispatch
    ## successfully some of the time is pretty much impossible given R's
    ## semantics.  Something containing a literal call to UseMethod is
    ## too broad in the sense that a UseMethod call in a local function
    ## doesn't produce a dispatch on the outer function ...
    ##
    ## If we use something like: a generic has to be
    ##      function(e) <UME>  # UME = UseMethod Expression
    ## with
    ##	    <UME> = UseMethod(...) |
    ##             if (...) <UME> [else ...] |
    ##             if (...) ... else <UME>
    ##             { ... <UME> ... }
    ## then a recognizer for UME might be as follows.

    f <- get(fname, envir = envir, inherits = FALSE)
    if(!is.function(f)) return(FALSE)
    isUMEbrace <- function(e) {
        for (ee in as.list(e[-1L])) if (nzchar(res <- isUME(ee))) return(res)
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
    isUME <- function(e) {
        if (is.call(e) && (is.name(e[[1L]]) || is.character(e[[1L]]))) {
            switch(as.character(e[[1L]]),
                   UseMethod = as.character(e[[2L]]),
                   "{" = isUMEbrace(e),
                   "if" = isUMEif(e),
                   "")
        } else ""
    }
    res <- isUME(body(f))
    if(mustMatch) res == fname else nzchar(res)
}

### ** .load_package_quietly

.load_package_quietly <-
function(package, lib.loc)
{
    ## Load (reload if already loaded) @code{package} from
    ## @code{lib.loc}, capturing all output and messages.
    ## Don't do anything for base.
    ## Earlier versions did not attempt reloading methods as this used
    ## to cause trouble, but this now (2009-03-19) seems ok.
    ## Otoh, it seems that unloading tcltk is a bad idea ...
    ## Also, do not unload ourselves (but shouldn't we be "in use"?).
    ##
    ## All QC functions use this for loading packages because R CMD
    ## check interprets all output as indicating a problem.
    if(package != "base")
        .try_quietly({
            pos <- match(paste("package", package, sep = ":"), search())
            if(!is.na(pos)) {
                detach(pos = pos,
                       unload = ! package %in% c("tcltk", "tools"))
            }
            library(package, lib.loc = lib.loc, character.only = TRUE,
                    verbose = FALSE)
        })
}

### ** .make_file_exts

.make_file_exts <-
function(type = c("code", "data", "demo", "docs", "vignette"))
{
    ## Return a character vector with the possible/recognized file
    ## extensions for a given file type.
    switch(type,
           code = c("R", "r", "S", "s", "q"),
           ## Keep in sync with the order given in base's data.Rd.
           data = c("R", "r",
                    "RData", "rdata", "rda",
                    "tab", "txt", "TXT",
                    "tab.gz", "txt.gz",
                    "tab.bz2", "txt.bz2",
                    "tab.xz", "txt.xz",
                    "csv", "CSV",
                    "csv.gz", "csv,bz2", "csv.xz"),
           demo = c("R", "r"),
           docs = c("Rd", "rd", "Rd.gz", "rd.gz"),
           vignette = c(outer(c("R", "r", "S", "s"), c("nw", "tex"),
                              paste, sep = "")))
}

### ** .make_S3_group_generic_env

.make_S3_group_generic_env <-
function(parent = parent.frame())
{
    ## Create an environment with pseudo-definitions for the S3 group
    ## methods.
    env <- new.env(parent = parent)
    assign("Math", function(x, ...) UseMethod("Math"),
           envir = env)
    assign("Ops", function(e1, e2) UseMethod("Ops"),
           envir = env)
    assign("Summary", function(..., na.rm = FALSE) UseMethod("Summary"),
           envir = env)
    assign("Complex", function(z) UseMethod("Complex"),
           envir = env)
    env
}

### ** .make_S3_primitive_generic_env

.make_S3_primitive_generic_env <-
function(parent = parent.frame(), fixup = FALSE)
{
    ## Create an environment with pseudo-definitions for the S3 primitive
    ## generics
    env <- new.env(parent = parent)
    for(f in ls(base::.GenericArgsEnv))
        assign(f, get(f, envir=base::.GenericArgsEnv), envir = env)
    if(fixup) {
        ## now fixup the operators
        for(f in c('+', '-', '*', '/', '^', '%%', '%/%', '&', '|',
                   '==', '!=', '<', '<=', '>=', '>')) {
            fx <- get(f, envir = env)
            formals(fx) <- alist(x=, y=)
            assign(f, fx, envir = env)
        }
    }
    env
}

### ** .make_S3_primitive_nongeneric_env

.make_S3_primitive_nongeneric_env <-
function(parent = parent.frame())
{
    ## Create an environment with pseudo-definitions
    ## for the S3 primitive non-generics
    env <- new.env(parent = parent)
    for(f in ls(base::.ArgsEnv))
        assign(f, get(f, envir=base::.ArgsEnv), envir = env)
    env
}

### ** .make_S3_methods_stop_list

.make_S3_methods_stop_list <-
function(package)
{
    ## Return a character vector with the names of the functions in
    ## @code{package} which 'look' like S3 methods, but are not.
    ## Using package=NULL returns all known examples

    ## round.POSIXt is a method for S3 and S4 group generics with
    ## deliberately different arg names.
    stopList <-
        list(base = c("all.equal", "all.names", "all.vars",
             "format.char", "format.info", "format.pval",
             "kappa.tri",
             "max.col",
             "print.atomic", "print.coefmat",
             "qr.Q", "qr.R", "qr.X", "qr.coef", "qr.fitted", "qr.qty",
             "qr.qy", "qr.resid", "qr.solve",
             "rep.int", "round.POSIXt",
             "seq.int", "sort.int", "sort.list"),
             BSDA = "sign.test",
             Hmisc = c("abs.error.pred", "t.test.cluster"),
             HyperbolicDist = "log.hist",
             MASS = c("frequency.polygon",
             "gamma.dispersion", "gamma.shape",
             "hist.FD", "hist.scott"),
             ## FIXME: since these are already listed with 'base',
             ##        they should not need to be repeated here:
             Matrix = c("qr.Q", "qr.R", "qr.coef", "qr.fitted", "qr.qty", "qr.qy", "qr.resid"),
             SMPracticals = "exp.gibbs",
             XML = "text.SAX",
             ape = "sort.index",
	     assist = "chol.new",
             boot = "exp.tilt",
             car = "scatterplot.matrix",
	     calibrator = "t.fun",
             ctv = "update.views",
             equivalence = "sign.boot",
             fields = c("qr.q2ty", "qr.yq2"),
             grDevices = "boxplot.stats",
             graphics = c("close.screen",
             "plot.design", "plot.new", "plot.window", "plot.xy",
             "split.screen"),
             hier.part = "all.regs",
             lasso2 = "qr.rtr.inv",
             mratios = c("t.test.ratio.default", "t.test.ratio.formula"),
             quadprog = c("solve.QP", "solve.QP.compact"),
             reposTools = "update.packages2",
             sac = "cumsum.test",
             sm = "print.graph",
             stats = c("anova.lmlist", "fitted.values", "lag.plot",
             "influence.measures", "t.test"),
             supclust = c("sign.change", "sign.flip"),
	     tensorA = "chol.tensor",
             utils = c("close.socket", "flush.console", "update.packages")
             )
    if(is.null(package)) return(unlist(stopList))
    thisPkg <- stopList[[package, exact = TRUE]] # 'st' matched 'stats'
    if(!length(thisPkg)) character() else thisPkg
}

### ** .package_apply

.package_apply <-
function(packages = NULL, FUN, ...)
{
    ## Apply FUN and extra '...' args to all given packages.
    ## The default corresponds to all installed packages with high
    ## priority.
    if(is.null(packages))
        packages <-
            unique(utils::installed.packages(priority = "high")[ , 1L])
    out <- lapply(packages, function(p)
                  tryCatch(FUN(p, ...),
                           error = function(e)
                           noquote(paste("Error:",
                                         conditionMessage(e)))))
    ## (Just don't throw the error ...)
    names(out) <- packages
    out
}

### ** .read_Rd_lines_quietly

.read_Rd_lines_quietly <-
function(con)
{
    ## Read lines from a connection to an Rd file, trying to suppress
    ## "incomplete final line found by readLines" warnings.
    if(is.character(con)) {
        con <- if(length(grep("\\.gz$", con))) gzfile(con, "r") else file(con, "r")
        on.exit(close(con))
    }
    .try_quietly(readLines(con, warn=FALSE))
}

### ** .read_collate_field

.read_collate_field <-
function(txt)
{
    ## Read Collate specifications in DESCRIPTION files.
    ## These consist of file paths relative to the R code directory,
    ## separated by white space, possibly quoted.  Note that we could
    ## have newlines in DCF entries but do not allow them in file names,
    ## hence we gsub() them out.
    con <- textConnection(gsub("\n", " ", txt))
    on.exit(close(con))
    scan(con, what = character(), strip.white = TRUE, quiet = TRUE)
}

### ** .read_description

.read_description <-
function(dfile)
{
    ## Try reading in package metadata from a DESCRIPTION file.
    ## (Never clear whether this should work on the path of the file
    ## itself, or on that of the directory containing it.)
    ## <NOTE>
    ## As we do not have character "frames", we return a named character
    ## vector.
    ## </NOTE>
    if(!file_test("-f", dfile))
        stop(gettextf("file '%s' does not exist", dfile), domain = NA)
    out <- tryCatch(read.dcf(dfile)[1L, ],
                    error = function(e)
                    stop(gettextf("file '%s' is not in valid DCF format",
                                  dfile),
                         domain = NA, call. = FALSE))
    if(!is.na(encoding <- out["Encoding"])) {
        ## could convert everything to UTF-8
        if (encoding %in% c("latin1", "UTF-8"))
            Encoding(out) <- encoding
        else out <- iconv(out, encoding, "", sub = "byte")
    }
    out
}

### ** .read_repositories

.read_repositories <-
function(file)
{
    db <- utils::read.delim(file, header = TRUE, comment.char = "#",
                            colClasses =
                            c(rep.int("character", 3L),
                              rep.int("logical", 4L)))
    db[, "URL"] <- .expand_BioC_repository_URLs(db[, "URL"])
    db
}

.expand_BioC_repository_URLs <-
function(x) {
    x <- sub("%bm",
             as.character(getOption("BioC_mirror",
                                    "http://www.bioconductor.org")),
             x, fixed = TRUE)
    sub("%v",
        as.character(.BioC_version_associated_with_R_version),
        x, fixed = TRUE)
}

### ** .shell_with_capture

.shell_with_capture <-
function(command, input = NULL)
{
    ## Invoke a system command using a shell and capture its status,
    ## stdout and stderr into separate components.

    ## Should try some sanity checking that there is no redirection in
    ## command thus far ...
    outfile <- tempfile("xshell")
    errfile <- tempfile("xshell")
    on.exit(unlink(c(outfile, errfile)))
    status <- if(.Platform$OS.type == "windows")
        shell(sprintf("%s > %s 2> %s", command, outfile, errfile),
              input = input, shell = "cmd.exe")
    else system(sprintf("%s > %s 2> %s", command, outfile, errfile),
                input = input)
    list(status = status,
         stdout = readLines(outfile, warn = FALSE),
         stderr = readLines(errfile, warn = FALSE))
}

### ** .source_assignments

.source_assignments <-
function(file, envir, enc = NA)
{
    ## Read and parse expressions from @code{file}, and then
    ## successively evaluate the top-level assignments in @code{envir}.
    ## Apart from only dealing with assignments, basically does the same
    ## as @code{sys.source(file, envir, keep.source = FALSE)}.
    oop <- options(keep.source = FALSE)
    on.exit(options(oop))
    assignmentSymbolLM <- as.symbol("<-")
    assignmentSymbolEq <- as.symbol("=")
    if(!is.na(enc) &&
       !(Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX"))) {
        con <- file(file, encoding = enc)
        on.exit(close(con))
    } else con <- file
    exprs <- parse(n = -1L, file = con)
    if(!length(exprs))
        return(invisible())
    for(e in exprs) {
        if(e[[1L]] == assignmentSymbolLM || e[[1L]] == assignmentSymbolEq)
            eval(e, envir)
    }
    invisible()
}

### .source_assignments_in_code_dir

.source_assignments_in_code_dir <-
function(dir, envir, meta = character())
{
    ## Combine all code files in @code{dir}, read and parse expressions,
    ## and successively evaluate the top-level assignments in @code{envir}.
    con <- tempfile("Rcode")
    on.exit(unlink(con))
    if(!file.create(con))
        stop("unable to create ", con)
    ## If the (DESCRIPTION) metadata contain a Collate specification,
    ## use this for determining the code files and their order.
    txt <- meta[c(paste("Collate", .OStype(), sep = "."), "Collate")]
    ind <- which(!is.na(txt))
    files <- if(any(ind))
        Filter(function(x) file_test("-f", x),
               file.path(dir, .read_collate_field(txt[ind[1L]])))
    else
        list_files_with_type(dir, "code")
    if(!all(.file_append_ensuring_LFs(con, files)))
        stop("unable to write code files")
    tryCatch(.source_assignments(con, envir, enc = meta["Encoding"]),
             error =
             function(e)
             stop("cannot source package code\n",
                  conditionMessage(e),
                  call. = FALSE))
}

### * .split_dependencies

.split_dependencies <-
function(x)
{
    ## given one or more Depends: or Suggests: fields from DESCRIPTION
    ## return a named list of list (name, [op, version])
    if(!length(x)) return(list())
    x <- unlist(strsplit(x, ","))
    ## some have had space before ,
    x <- sub('[[:space:]]+$', '', x)
    x <- unique(sub("^[[:space:]]*(.*)", "\\1" , x))
    names(x) <- sub("^([[:alnum:].]+).*$", "\\1" , x)
    lapply(x, .split_op_version)
}

### * .split_op_version

.split_op_version <-
function(x)
{
    ## given a single piece of dependency
    ## return a list of components (name, [op, version])
    ## NB this relies on trailing space having been removed
    pat <- "^([^\\([:space:]]+)[[:space:]]*\\(([^\\)]+)\\).*"
    x1 <- sub(pat, "\\1", x)
    x2 <- sub(pat, "\\2", x)
    if(x2 != x1) {
        pat <- "[[:space:]]*([[<>=!]+)[[:space:]]+(.*)"
        list(name = x1, op = sub(pat, "\\1", x2),
             version = package_version(sub(pat, "\\2", x2)))
    } else list(name = x1)
}

### ** .strip_whitespace

.strip_whitespace <-
function(x)
{
    ## Strip leading and trailing whitespace.
    x <- sub("^[[:space:]]+", "", x)
    x <- sub("[[:space:]]+$", "", x)
    x
}

### ** .try_quietly

.try_quietly <-
function(expr)
{
    ## Try to run an expression, suppressing all 'output'.  In case of
    ## failure, stop with the error message and a "traceback" ...

    oop <- options(warn = 1)
    on.exit(options(oop))
    outConn <- file(open = "w+")         # anonymous tempfile
    sink(outConn, type = "output")
    sink(outConn, type = "message")
    yy <- tryCatch(withRestarts(withCallingHandlers(expr, error = {
        function(e) invokeRestart("grmbl", e, sys.calls())
    }),
                                grmbl = function(e, calls) {
                                    n <- length(sys.calls())
                                    ## Chop things off as needed ...
                                    calls <- calls[-seq.int(length.out = n - 1L)]
                                    calls <- rev(calls)[-c(1L, 2L)]
                                    tb <- lapply(calls, deparse)
                                    stop(conditionMessage(e),
                                         "\nCall sequence:\n",
                                         paste(.eval_with_capture(traceback(tb))$output,
                                               collapse = "\n"),
                                         call. = FALSE)
                                }),
                   error = identity,
                   finally = {
                       sink(type = "message")
                       sink(type = "output")
                       close(outConn)
                   })
    if(inherits(yy, "error"))
        stop(yy)
    yy
}

### ** .wrong_args

.wrong_args <-
function(args, msg)
{
    len <- length(args)
    if(!len)
        character()
    else if(len == 1L)
        paste("argument", sQuote(args), msg)
    else
        paste("arguments",
              paste(c(rep.int("", len - 1L), "and "),
                    sQuote(args),
                    c(rep.int(", ", len - 1L), ""),
                    sep = "", collapse = ""),
              msg)
}


### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
#  File src/library/tools/R/writePACKAGES.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

write_PACKAGES <-
function(dir = ".", fields = NULL,
         type = c("source", "mac.binary", "win.binary"),
         verbose = FALSE, unpacked = FALSE, subdirs = FALSE,
         latestOnly = TRUE, addFiles = FALSE)
{
    if(missing(type) && .Platform$OS.type == "windows")
        type <- "win.binary"
    type <- match.arg(type)
    nfields <- 0
    out <- file(file.path(dir, "PACKAGES"), "wt")
    outgz <- gzfile(file.path(dir, "PACKAGES.gz"), "wt")

    paths <- ""
    if(is.logical(subdirs) && subdirs) {
        owd <- setwd(dir)
        paths <- unique(dirname(list.files(".", recursive=TRUE)))
        setwd(owd)
        paths <- c("", paths[paths != "."])
    } else if(is.character(subdirs)) paths <- c("", subdirs)

    for(path in paths) {
        this <- if(nzchar(path)) file.path(dir, path) else dir
        desc <- .build_repository_package_db(this, fields, type, verbose,
                                             unpacked)

        if(length(desc)) {
            Files <- names(desc)
            fields <- names(desc[[1L]])
            desc <- matrix(unlist(desc), ncol = length(fields), byrow = TRUE)
            colnames(desc) <- fields
            bundle <- !is.na(desc[,"Bundle"])
            desc[bundle, "Package"] <- desc[bundle, "Bundle"]
            desc <- cbind(desc, File = Files)
            if(addFiles) desc <- cbind(desc, File = Files)
            if(latestOnly) desc <- .remove_stale_dups(desc)

            ## Standardize licenses or replace by NA.
            license_info <- analyze_licenses(desc[, "License"])
            desc[, "License"] <-
                ifelse(license_info$is_standardizable,
                       license_info$standardization,
                       NA)

            ## Writing PACKAGES file from matrix desc linewise in order to
            ## omit NA entries appropriately:
            for(i in seq_len(nrow(desc))){
                desci <- desc[i, !(is.na(desc[i, ]) | (desc[i, ] == "")),
                              drop = FALSE]
                write.dcf(desci, file = out)
                if(nzchar(path)) cat("Path: ", path, "\n", sep="", file = out)
                cat("\n", file = out)
                write.dcf(desci, file = outgz)
                if(nzchar(path)) cat("Path: ", path, "\n", sep="", file = outgz)
                cat("\n", file = outgz)
            }
            nfields <- nfields + nrow(desc)
        }
    }

    close(out)
    close(outgz)
    invisible(nfields)
}

## this is OK provided all the 'fields' are ASCII -- so be careful
## what you add.
.build_repository_package_db <-
function(dir, fields = NULL,
         type = c("source", "mac.binary", "win.binary"),
         verbose = getOption("verbose"),
         unpacked = FALSE)
{
    if(unpacked)
        return(.build_repository_package_db_from_source_dirs(dir,
                                                             fields,
                                                             verbose))

    type <- match.arg(type)

    ## FIXME: might the source pattern be more general?
    ## was .tar.gz prior to 2.10.0
    package_pattern <- switch(type,
                              "source" = "_.*\\.tar\\..*$",
                              "mac.binary" = "_.*\\.tgz$",
                              "win.binary" = "_.*\\.zip$")
    files <- list.files(dir, pattern = package_pattern)

    if(!length(files))
        return(list())

    ## Add the standard set of fields required to build a repository's
    ## PACKAGES file:
    fields <- unique(c(.get_standard_repository_db_fields(), fields))
    packages <- sapply(strsplit(files, "_", fixed = TRUE), "[", 1L)
    db <- vector(length(files), mode = "list")
    names(db) <- files
    ## Many (roughly length(files)) warnings are *expected*, hence
    ## suppressed.
    op <- options(warn = -1)
    on.exit(options(op))
    if(verbose) message("Processing packages:")
    if(type == "win.binary") {
        files <- file.path(dir, files)
        for(i in seq_along(files)) {
            if(verbose) message(paste(" ", files[i]))
            ## package zips have <name>/DESCRIPTION, rarer bundle zips do not.
            ## So try package case first.
            con <- unz(files[i], file.path(packages[i], "DESCRIPTION"))
            temp <- tryCatch(read.dcf(con, fields = fields)[1L, ],
                             error = identity)
            if(inherits(temp, "error")) {
                close(con)
                ## bundle zips may have a top-level DESCRIPTION file
                con <- unz(files[i], "DESCRIPTION")
                temp <- tryCatch(read.dcf(con, fields = fields)[1L, ],
                                 error = identity)
                if(inherits(temp, "error")) {
                    close(con)
                    ## otherwise look for the DESCRIPTION file of first package.
                    inzip <- as.character(unzip(files[i], list = TRUE)$Name)
                    d <- grepl("DESCRIPTION$", inzip)
                    if(any(d)) {
                        con <- unz(files[i], (inzip[d])[1])
                        temp <- tryCatch(read.dcf(con, fields = fields)[1L, ],
                                         error = identity)
                    }
                    if(inherits(temp, "error")) {
                        close(con)
                        next
                    }
                }
            }
            db[[i]] <- temp
            close(con)
        }
    } else {
        dir <- file_path_as_absolute(dir)
        files <- file.path(dir, files)
        cwd <- getwd()
        td <- tempfile("PACKAGES")
        if(!dir.create(td)) stop("unable to create ", td)
        on.exit(unlink(td, recursive = TRUE), add = TRUE)
        setwd(td)
        for(i in seq_along(files)) {
            if(verbose) message(paste(" ", files[i]))
            p <- file.path(packages[i], "DESCRIPTION")
            ## temp <- try(system(paste("tar zxf", files[i], p)))
            temp <- try(utils::untar(files[i], files = p))
            if(!inherits(temp, "try-error")) {
                temp <- tryCatch(read.dcf(p, fields = fields)[1L, ],
                                 error = identity)
                if(!inherits(temp, "error"))
                    db[[i]] <- temp
            }
            unlink(packages[i], recursive = TRUE)
        }
        setwd(cwd)
    }
    if(verbose) message("done")

    db
}

.build_repository_package_db_from_source_dirs <-
function(dir, fields = NULL, verbose = getOption("verbose"))
{
    dir <- file_path_as_absolute(dir)
    fields <- unique(c(.get_standard_repository_db_fields(), fields))
    paths <- list.files(dir, full.names = TRUE)
    paths <- paths[file_test("-d", paths) &
                   file_test("-f", file.path(paths, "DESCRIPTION"))]
    db <- vector(length(paths), mode = "list")
    if(verbose) message("Processing packages:")
    for(i in seq_along(paths)) {
        if(verbose) message(paste(" ", basename(paths[i])))
        temp <- tryCatch(read.dcf(file.path(paths[i], "DESCRIPTION"),
                                  fields = fields)[1L, ],
                         error = identity)
        if(!inherits(temp, "error"))
            db[[i]] <- temp
    }
    if(verbose) message("done")
    names(db) <- basename(paths)
    db
}

dependsOnPkgs <-
function(pkgs,
         dependencies = c("Depends", "Imports", "LinkingTo"),
         recursive = TRUE,
         lib.loc = NULL,
         installed = installed.packages(lib.loc, fields = "Enhances"))
{
    need <- apply(installed[, dependencies, drop = FALSE], 1L,
                  function(x)
                  any(pkgs %in% utils:::.clean_up_dependencies(x)) )
    uses <- rownames(installed)[need]
    if(recursive) {
        p <- pkgs
        repeat {
            p <- unique(c(p, uses))
            need <- apply(installed[, dependencies, drop = FALSE], 1L,
                          function(x)
                          any(p %in% utils:::.clean_up_dependencies(x)) )
            uses <- unique(c(p, rownames(installed)[need]))
            if(length(uses) <= length(p)) break
        }
    }
    setdiff(uses, pkgs)
}

.remove_stale_dups <-
function(ap)
{
    ## Given a matrix from available.packages, return a copy
    ## with no duplicate packages, being sure to keep the packages
    ## with highest version number.
    ## (Also works for data frame package repository dbs.)
    pkgs <- ap[ , "Package"]
    dup_pkgs <- pkgs[duplicated(pkgs)]
    stale_dups <- integer(length(dup_pkgs))
    i <- 1L
    for (dp in dup_pkgs) {
        wh <- which(dp == pkgs)
        vers <- package_version(ap[wh, "Version"])
        keep_ver <- max(vers)
        keep_idx = which(vers == keep_ver)[1L] # they might all be max
        wh <- wh[-keep_idx]
        end_i <- i + length(wh) - 1L
        stale_dups[i:end_i] <- wh
        i <- end_i + 1L
    }
    ## Possible to have only one package in a repository
    if(length(stale_dups)) ap[-stale_dups, , drop = FALSE] else ap
}

.package_dependencies <-
function(packages = NULL, db,
         which = c("Depends", "Imports", "LinkingTo"),
         recursive = FALSE, reverse = FALSE)
{
    ## <FIXME>
    ## What about duplicated entries?
    ## </FIXME>

    ## For given packages which are not found in the db, return "list
    ## NAs" (i.e., NULL entries), as opposed to character() entries
    ## which indicate no dependencies.

    ## For forward non-recursive depends, we can simplify matters by
    ## subscripting the db right away---modulo boundary cases.

    out_of_db_packages <- character()
    if(!recursive && !reverse) {
        if(!is.null(packages)) {
            ind <- match(packages, db[, "Package"], nomatch = 0L)
            db <- db[ind, , drop = FALSE]
            out_of_db_packages <- packages[ind == 0L]
        }
    }

    depends <-
        do.call(Map,
                c(list("c"),
                  ## Try to make this work for dbs which are character
                  ## matrices as from available.packages(), or data
                  ## frame variants thereof.
                  lapply(which,
                         function(f) {
                             if(is.list(d <- db[, f])) d
                             else lapply(d,
                                         .extract_dependency_package_names)
                         }),
                  list(USE.NAMES = FALSE)))

    if(!recursive && !reverse) {
        names(depends) <- db[, "Package"]
        if(length(out_of_db_packages)) {
            depends <-
                c(depends,
                  structure(vector("list", length(out_of_db_packages)),
                            names = out_of_db_packages))
        }
        return(depends)
    }

    all_packages <- sort(unique(c(db[, "Package"], unlist(depends))))

    if(!recursive) {
        ## Need to invert.
        depends <-
            split(rep.int(db[, "Package"], sapply(depends, length)),
                  factor(unlist(depends), levels = all_packages))
        if(!is.null(packages)) {
            depends <- depends[match(packages, names(depends))]
            names(depends) <- packages
        }
        return(depends)
    }

    ## Recursive dependencies.
    ## We need to compute the transitive closure of the dependency
    ## relation, but e.g. Warshall's algorithm (O(n^3)) is
    ## computationally infeasible.
    ## Hence, in principle, we do the following.
    ## Take the current list of pairs (i,j) in the relation.
    ## Iterate over all j and whenever i R j and j R k add (i,k).
    ## Repeat this until no new pairs get added.
    ## To do this in R, we use a 2-column matrix of (i,j) rows.
    ## We then create two lists which for all j contain the i and k
    ## with i R j and j R k, respectively, and combine these.
    ## This works reasonably well, but of course more efficient
    ## implementations should be possible.
    tab <- if(reverse)
        split(match(rep.int(db[, "Package"],
                            sapply(depends, length)),
                    all_packages),
              factor(match(unlist(depends), all_packages),
                     levels = seq_along(all_packages)))
    else
        split(match(unlist(depends), all_packages),
              factor(match(rep.int(db[, "Package"],
                                   sapply(depends, length)),
                           all_packages),
                     levels = seq_along(all_packages)))
    if(is.null(packages)) {
        if(reverse) {
            packages <- all_packages
            p_L <- seq_along(all_packages)
        } else {
            packages <- db[, "Package"]
            p_L <- match(packages, all_packages)
        }
    } else {
        p_L <- match(packages, all_packages, nomatch = 0L)
        if(any(ind <- (p_L == 0L))) {
            out_of_db_packages <- packages[ind]
            packages <- packages[!ind]
            p_L <- p_L[!ind]
        }
    }
    p_R <- tab[p_L]
    pos <- cbind(rep.int(p_L, sapply(p_R, length)), unlist(p_R))
    ctr <- 1L
    verbose <- getOption("verbose")
    repeat {
        if(verbose) cat("Cycle:", ctr)
        p_L <- split(pos[, 1L], pos[, 2L])
        new <- do.call(rbind,
                       Map(function(i, k)
                           cbind(rep.int(i, length(k)),
                                     rep(k, each = length(i))),
                           p_L, tab[as.integer(names(p_L))]))
        npos <- unique(rbind(pos, new))
        nnew <- nrow(npos) - nrow(pos)
        if(verbose) cat(" NNew:", nnew, "\n")
        if(!nnew) break
        pos <- npos
        ctr <- ctr + 1L
    }
    depends <-
        split(all_packages[pos[, 2L]],
              factor(all_packages[pos[, 1L]], levels = packages))
    if(length(out_of_db_packages)) {
        depends <-
            c(depends,
              structure(vector("list", length(out_of_db_packages)),
                        names = out_of_db_packages))
    }
    depends
}

.extract_dependency_package_names <-
function(x) {
    ## Assume a character *string*.
    if(is.na(x)) return(character())
    x <- unlist(strsplit(x, ",[[:space:]]*"))
    x <- sub("[[:space:]]*([[:alnum:].]+).*", "\\1", x)
    x[nzchar(x) & (x != "R")]
}
#  File src/library/tools/R/xgettext.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

xgettext <-
function(dir, verbose = FALSE, asCall = TRUE)
{
    dir <- file_path_as_absolute(dir)
    bn <- basename(dir)
    dir <- file.path(dir, "R")
    exts <- .make_file_exts("code")
    R_files <- list_files_with_exts(dir, exts)
    for(d in c("unix", "windows")) {
        OSdir <- file.path(dir, d)
        if(file_test("-d", OSdir))
            R_files <- c(R_files, list_files_with_exts(OSdir, exts))
    }
    if(bn == "base") {
        ## include loader files in R_HOME/share/R
        shdir <- file.path(dir, "../../../../share/R")
        R_files <- c(R_files, list_files_with_exts(shdir, exts))
    }
    out <- vector("list", length = length(R_files))
    names(out) <- R_files

    find_strings <- function(e) {
        find_strings2 <- function(e, suppress) {
            if(is.character(e)) {
                if(!suppress) strings <<- c(strings, e)
            } else if(is.call(e)) {
                if(is.name(e[[1L]])
                   && (as.character(e[[1L]])
                       %in% c("gettext", "gettextf"))) {
                    domain <- e[["domain"]]
                    suppress <- !is.null(domain) && !is.name(domain) && is.na(domain)
                    if(as.character(e[[1L]]) %in% "gettextf")
                        e <- e[2L] # just look at first arg
                }
                for(i in seq_along(e)) find_strings2(e[[i]], suppress)
            }
        }
        if(is.call(e)
           && is.name(e[[1L]])
           && (as.character(e[[1L]])
               %in% c("warning", "stop", "message", "packageStartupMessage",
                      "gettext", "gettextf"))) {
             domain <- e[["domain"]]
             suppress <- !is.null(domain) && !is.name(domain) && is.na(domain)
             ## remove named args
             if(!is.null(names(e)))
                 e <- e[!names(e) %in% c("call.", "immediate.", "domain")]
             if(asCall) {
                 if(!suppress) strings <<- c(strings, as.character(e)[-1L])
             } else for(i in seq_along(e)) find_strings2(e[[i]], suppress)
        } else if(is.recursive(e))
            for(i in seq_along(e)) Recall(e[[i]])
    }

    for(f in R_files) {
        if(verbose) message(gettextf("parsing '%s'", f), domain = NA)
        strings <- character()
        for(e in parse(file = f)) find_strings(e)
        ## strip leading and trailing white space
        strings <- sub("^[ \t\n]*", "", strings)
        strings <- sub("[ \t\n]*$", "", strings)
        out[[f]] <- structure(unique(strings), class="xgettext")
    }

    out[sapply(out, length) > 0L]
}

print.xgettext <-
function(x, ...)
{
    cat(x, sep="\n")
    invisible(x)
}

print.xngettext <-
function(x, ...)
{
    lapply(x, function(x)
           cat("\nmsgid        = ", x[1L],
               "\nmsgid_plural = ", x[2L],
               "\n", sep=""))
    invisible(x)
}

xngettext <-
function(dir, verbose = FALSE)
{
    dir <- file_path_as_absolute(dir)
    dir <- file.path(dir, "R")
    exts <- .make_file_exts("code")
    R_files <- list_files_with_exts(dir, exts)
    for(d in c("unix", "windows", "aqua")) {
        OSdir <- file.path(dir, d)
        if(file_test("-d", OSdir))
            R_files <- c(R_files, list_files_with_exts(OSdir, exts))
    }
    out <- vector("list", length = length(R_files))
    names(out) <- R_files

    find_strings <- function(e) {
        if(is.call(e) && is.name(e[[1L]])
           && as.character(e[[1L]]) %in% "ngettext") {
             domain <- e[["domain"]]
             ## remove named domain arg
             if(!is.null(names(e))) e <- e[!names(e) %in% "domain"]
             ## for now, take second and third remaining args.
             ## <FIXME> emulate full arg-matching
             if(is.character(e[[3L]]) && is.character(e[[4L]]))
                 strings <<- c(strings, list(c(msg1=e[[3L]], msg2=e[[4L]])))
        } else if(is.recursive(e))
            for(i in seq_along(e)) Recall(e[[i]])
    }

    for(f in R_files) {
        if(verbose) message(gettextf("parsing '%s'", f), domain = NA)
        strings <- list()
        for(e in parse(file = f)) find_strings(e)
        out[[f]] <- structure(strings, class="xngettext")
    }

    out[sapply(out, length) > 0L]
}

xgettext2pot <-
function(dir, potFile)
{
    dir <- file_path_as_absolute(dir)
    if(missing(potFile))
        potFile <- paste("R-", basename(dir), ".pot", sep="")
    tmp <- unique(unlist(xgettext(dir, asCall = FALSE)))
    tmp <- tmp[nzchar(tmp)]
    tmp <- shQuote(encodeString(tmp), type="cmd")  # need to quote \n, \t etc
    con <- file(potFile, "wt")
    on.exit(close(con))
    writeLines(con=con,
               c('msgid ""',
                 'msgstr ""',
                 sprintf('"Project-Id-Version: R %s.%s\\n"',
                         R.version$major, R.version$minor),
                 '"Report-Msgid-Bugs-To: bugs@r-project.org\\n"',
                 paste('"POT-Creation-Date: ',
                       format(Sys.time(), "%Y-%m-%d %H:%M"), # %z is not portable
                       '\\n"', sep=''),
                 '"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\\n"',
                 '"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\n"',
                 '"Language-Team: LANGUAGE <LL@li.org>\\n"',
                 '"MIME-Version: 1.0\\n"',
                 '"Content-Type: text/plain; charset=CHARSET\\n"',
                 '"Content-Transfer-Encoding: 8bit\\n"', ''))
    for(e in tmp)
        writeLines(con=con, c('', paste('msgid', e), 'msgstr ""'))
    tmp <- xngettext(dir)
    un <- unique(unlist(tmp, recursive=TRUE))
    for(ee in tmp)
        for(e in ee)
            if(e[1L] %in% un) {
                writeLines(con=con, c('',
                           paste('msgid       ',
                                 shQuote(encodeString(e[1L]), type="cmd")),
                           paste('msgid_plural',
                                 shQuote(encodeString(e[2L]), type="cmd")),
                           'msgstr[0]    ""', 'msgstr[1]    ""')
                           )
                un <- un[-match(e, un)]
            }
}
#  File src/library/tools/R/zzz.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
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

.onUnload <-
function(libpath)
    library.dynam.unload("tools", libpath)
