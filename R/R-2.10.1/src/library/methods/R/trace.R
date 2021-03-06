#  File src/library/methods/R/trace.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

## some temporary (!) hooks to trace the tracing code
.doTraceTrace <- function(on) {
 .assignOverBinding(".traceTraceState", on,
                    environment(.doTraceTrace), FALSE)
  on
}

.traceTraceState <- FALSE

## the internal functions in the evaluator.  These are all prohibited,
## although some of them could just barely be accomodated, with some
## specially designed new definitions (not using ..., for example).
## The gain does not seem worth the inconsistencies; and "if" can
## never be traced, since it has to be used to determine if tracing is
## on.  (see .doTrace())
## The remaining invalid functions create miscellaneous bugs, maybe
## related to the use of "..." as the introduced arguments.  Aside from
## .Call, tracing them seems of marginal value.

.InvalidTracedFunctions <- c("if", "where", "for", "repeat", "(", "{",
                            "next", "break", ".Call", ".Internal", ".Primitive")

.TraceWithMethods <- function(what, tracer = NULL, exit = NULL, at =
                              numeric(), print = TRUE, signature =
                              NULL, where = .GlobalEnv, edit = FALSE,
                              from = NULL, untrace = FALSE) {
    if(is.function(where)) {
        ## start from the function's environment:  important for
        ## tracing from a namespace
        if(is(where, "genericFunction"))
            where <- parent.env(environment(where))
        else
            where <- environment(where)
        fromPackage <- getPackageName(where)
    }
    else fromPackage <- ""
    doEdit <- !identical(edit, FALSE)
    whereF <- NULL
    pname <- character()
    def <- NULL
    if(is.function(what)) {
        def <- what
        if(is(def, "genericFunction")) {
            what <- def@generic
            whereF <- .genEnv(what, where)
            pname <- def@package
        }
        else {
            fname <- substitute(what)
            if(is.name(fname)) {
                what <- as.character(fname)
                temp <- .findFunEnvAndName(what, where)
                whereF <- temp$whereF
                pname <- temp$pname
            }
            else if(is.call(fname) && identical(fname[[1L]], as.name("::"))) {
                whereF <-as.character(fname[[2L]])
                require(whereF, character.only = TRUE)
                whereF <- as.environment(paste("package", whereF, sep=":"))
                pname <-  fname[[2L]]
                what <- as.character(fname[[3L]])
            }
            else if(is.call(fname) && identical(fname[[1L]], as.name(":::"))) {
                pname <- paste(fname[[2L]], "(not-exported)")
                whereF <- loadNamespace(as.character(fname[[2L]]))
                what <- as.character(fname[[3L]])
            }
            else
                stop("argument 'what' should be the name of a function")
        }
    }
    else {
        what <- as(what, "character")
        if(length(what) != 1) {
            for(f in what) {
                if(nargs() == 1)
                    trace(f)
                else
                    Recall(f, tracer, exit, at, print, signature, where, edit, from, untrace)
            }
            return(what)
        }
        temp <- .findFunEnvAndName(what, where, signature)
        whereF <- temp$whereF
        pname <- temp$pname
    }
    if(what %in% .InvalidTracedFunctions)
      stop(gettextf("Tracing the internal function \"%s\" is not allowed",
                    what))
 if(.traceTraceState) {
    message(".TraceWithMethods: after computing what, whereF")
    browser()
  }
    if(nargs() == 1)
        return(.primTrace(what)) # for back compatibility
    if(is.null(whereF)) {
        allWhere <- findFunction(what, where = where)
        if(length(allWhere)==0)
            stop(gettextf("no function definition for \"%s\" found", what),
                 domain = NA)
        whereF <- as.environment(allWhere[[1L]])
    }
    ## detect use with no action specified (old-style R trace())
    if(is.null(tracer) && is.null(exit) && identical(edit, FALSE))
      tracer <- quote({})
        if(is.null(def))
            def <- getFunction(what, where = whereF)
        if(is(def, "traceable") && identical(edit, FALSE) && !untrace)
          def <- .untracedFunction(def)
        if(!is.null(signature)) {
            fdef <- if(is.primitive(def))  getGeneric(what, TRUE, where) else def
            def <- selectMethod(what, signature, fdef = fdef, optional = TRUE)
            if(is.null(def)) {
              warning(gettextf("Can't untrace method for \"%s\"; no method defined for this signature: %s",
                               what, paste(signature, collapse = ", ")))
              return(def)
          }
        }
    if(untrace) {
     if(.traceTraceState) {
    message(".TraceWithMethods: untrace case")
    browser()
  }

      if(is.null(signature)) {
        ## ensure that the version to assign is untraced
        if(is(def, "traceable")) {
          newFun <- .untracedFunction(def)
        }
        else {
          .primUntrace(what) # to be safe--no way to know if it's traced or not
          return(what)
        }
      }
      else {
        if(is(def, "traceable"))
          newFun <- .untracedFunction(def)
        else {
          warning(gettextf("the method for \"%s\" for this signature was not being traced", what), domain = NA)
          return(what)
        }
      }
    }
    else {
        if(!is.null(exit)) {
            if(is.function(exit)) {
                tname <- substitute(exit)
                if(is.name(tname))
                    exit <- tname
                exit <- substitute(TRACE(), list(TRACE=exit))
            }
        }
        if(!is.null(tracer)) {
            if(is.function(tracer)) {
                tname <- substitute(tracer)
                if(is.name(tname))
                    tracer <- tname
                tracer <- substitute(TRACE(), list(TRACE=tracer))
            }
        }
        original <- .untracedFunction(def)
         ## calls .makeTracedFunction via the initialize method for
        ## "traceable"
        traceClass <- .traceClassName(class(original))
        if(is.null(getClassDef(traceClass)))
          traceClass <- .makeTraceClass(traceClass, class(original))
        newFun <- new(traceClass,
                      def = if(doEdit) def else original, tracer = tracer, exit = exit, at = at,
                      print = print, doEdit = edit)
    }
    global <- identical(whereF, .GlobalEnv)
 if(.traceTraceState) {
    message(".TraceWithMethods: about to assign or setMethod")
    browser()
  }
    if(is.null(signature)) {
        if(bindingIsLocked(what, whereF))
            .assignOverBinding(what, newFun, whereF, global)
        else
            assign(what, newFun, whereF)
        if(length(grep("[^.]+[.][^.]+", what)) > 0) { #possible S3 method
            ## check for a registered version of the object
            S3MTableName <- ".__S3MethodsTable__."
            tracedFun <- get(what, envir = whereF, inherits = TRUE)
            if(exists(S3MTableName, envir = whereF, inherits = FALSE)) {
                tbl <- get(S3MTableName, envir = whereF, inherits = FALSE)
                if(exists(what, envir = tbl, inherits = FALSE))
                  assign(what, tracedFun, envir = tbl)
            }
        }
    }
    else {
        if(untrace && is(newFun, "MethodDefinition") &&
           !identical(newFun@target, newFun@defined))
          ## we promoted an inherited method for tracing, now we have
          ## to remove that method.  Assertion is that there was no directly
          ## specified method, or else defined, target would be identical
          newFun <- NULL
        ## arrange for setMethod to put the new method in the generic
        ## but NOT to assign the methods list object (binding is ignored)
        setMethod(fdef, signature, newFun, where = baseenv())
    }
    if(!global) {
        action <- if(untrace)"Untracing" else "Tracing"
        nameSpaceCase <- FALSE
        location <- if(.identC(fromPackage, "")) {
            if(length(pname)==0  && !is.null(whereF))
                pname <- getPackageName(whereF)
            nameSpaceCase <- isNamespace(whereF) &&
                !is.na(match(pname, loadedNamespaces())) &&
                 identical(whereF, getNamespace(pname))
            if(length(pname)==0)  # but not possible from getPackagename ?
                "\""
            else {
                if(nameSpaceCase)
                  paste("\" in environment <namespace:",  pname, ">", sep="")
                else
                  paste("\" in package \"",  pname, "\"", sep="")
              }
        }
        else paste("\" as seen from package \"", fromPackage, "\"", sep="")
        object <- if(is.null(signature)) " function \"" else " specified method for function \""
        .message(action, object, what, location)
        if(nameSpaceCase && !untrace && exists(what, envir = .GlobalEnv)) {
          untcall<- paste("untrace(\"", what, "\", where = getNamespace(\"",
                          pname, "\"))", sep="")
          .message("Warning: Tracing only in the namespace; to untrace you will need:\n    ",untcall, "\n")
        }
    }
    what
}

.makeTracedFunction <- function(def, tracer, exit, at, print, doEdit) {
    switch(typeof(def),
           builtin = , special = {
               fBody <- substitute({.prim <- DEF; .prim(...)},
                                   list(DEF = def))
               def <- eval(function(...)NULL)
               body(def, envir = .GlobalEnv) <- fBody
               warning("making a traced version of a primitive; arguments will be treated as '...'")
           }
           )
    if(!identical(doEdit, FALSE)) {
        if(is.character(doEdit) || is.function(doEdit)) {
            editor <- doEdit
            doEdit <- TRUE
        }
        else
            editor <- getOption("editor")
    }
    ## look for a request to edit the definition
    if(doEdit) {
        if(is(def, "traceable"))
            def <- as(def, "function") # retain previous tracing if editing
        if(is(editor, "character") && !is.na(match(editor, c("emacs","xemacs")))) {
            ## cater to the usual emacs modes for editing R functions
            file <- tempfile("emacs")
            file <- sub('..$', ".R", file)
        }
        else
            file <- ""
        ## insert any requested automatic tracing expressions before editing
        if(!(is.null(tracer) && is.null(exit) && length(at)==0))
            def <- Recall(def, tracer, exit, at, print, FALSE)
        def2 <- utils::edit(def, editor = editor, file = file)
        if(!is.function(def2))
            stop(gettextf("the editing in trace() can only change the body of the function; got an object of class \"%s\"", class(def2)), domain = NA)
        if(!identical(args(def), args(def2)))
            stop("the editing in trace() can only change the body of the function, not the arguments or defaults")
        fBody <- body(def2)
    }
    else {
        def <- .untracedFunction(def) # throw away earlier tracing
        fBody <- body(def)
        if(length(at) > 0) {
            if(is.null(tracer))
                stop("cannot use 'at' argument without a trace expression")
            else if(class(fBody) != "{")
                stop("cannot use 'at' argument unless the function body has the form '{ ... }'")
            for(i in at) {
                if(print)
                    expri <- substitute({.doTrace(TRACE, MSG); EXPR},
                                        list(TRACE = tracer,
                                        MSG = paste("step",paste(i, collapse=",")),
                                        EXPR = fBody[[i]]))
                else
                    expri <- substitute({.doTrace(TRACE); EXPR},
                                        list(TRACE=tracer, EXPR = fBody[[i]]))
                fBody[[i]] <- expri
            }
        }
        else if(!is.null(tracer)){
            if(print)
                fBody <- substitute({.doTrace(TRACE, MSG); EXPR},
                                    list(TRACE = tracer, MSG = paste("on entry"), EXPR = fBody))
            else
                fBody <- substitute({.doTrace(TRACE); EXPR},
                                    list(TRACE=tracer, EXPR = fBody))
        }
        if(!is.null(exit)) {
            if(print)
                exit <- substitute(.doTrace(EXPR, MSG),
                                   list(EXPR = exit, MSG = paste("on exit")))
            else
                exit <- substitute(.doTrace(EXPR),
                                   list(EXPR = exit))
            fBody <- substitute({on.exit(TRACE); BODY},
                                list(TRACE=exit, BODY=fBody))
        }
    }
    body(def, envir = environment(def)) <- fBody
    def
}

## return the untraced version of f
.untracedFunction <- function(f) {
    while(is(f, "traceable"))
        f <- f@original
    f
}


.InitTraceFunctions <- function(envir)  {
    setClass("traceable", representation(original = "PossibleMethod"), contains = "VIRTUAL",
             where = envir); clList <- "traceable"
    ## create the traceable classes
    for(cl in c("function", "MethodDefinition", "MethodWithNext", "genericFunction",
                "standardGeneric", "nonstandardGeneric", "groupGenericFunction",
                "derivedDefaultMethod")) {
        .makeTraceClass(.traceClassName(cl), cl, FALSE)
        clList <- c(clList, .traceClassName(cl))
    }
    assign(".SealedClasses", c(get(".SealedClasses", envir), clList), envir)
    setMethod("initialize", "traceable",
              function(.Object, def, tracer, exit, at, print, doEdit) {
                  oldClass <- class(def)
                  oldClassDef <- getClass(oldClass)
                  if(!is.null(oldClassDef) && length(oldClassDef@slots) > 0)
                      as(.Object, oldClass) <- def # to get other slots in def
                  .Object@original <- def
                  if(!is.null(elNamed(getSlots(getClass(class(def))), ".Data")))
                      def <- def@.Data
                  .Object@.Data <- .makeTracedFunction(def, tracer, exit, at, print, doEdit)
                  .Object
              }, where = envir)
    if(!isGeneric("show", envir))
        setGeneric("show", where = envir, simpleInheritanceOnly = TRUE)
    setMethod("show", "traceable", .showTraceable, where = envir)
}

.showTraceable <- function(object) {
        cat("Object with tracing code, class \"", class(object),
        "\"\nOriginal: \n", sep="")
        callGeneric(object@original)
        cat("\n## (to see the tracing code, look at body(object))\n")
    }

.doTracePrint <- function(msg = "") {
    call <- deparse(sys.call(sys.parent(1)))
    if(length(call)>1)
        call <- paste(call[[1L]], "....")
    cat("Tracing", call, msg, "\n")
}

.traceClassName <- function(className) {
    className[] <- paste(className, "WithTrace", sep="")
    className
}

.assignOverBinding <- function(what, value, where, verbose = TRUE) {
    pname <- getPackageName(where)
    if(verbose) {
        msg <-
            gettextf("assigning over the binding of symbol \"%s\" in environment/package \"%s\"",
                     what, pname)
        message(strwrap(msg), domain = NA)
    }
    warnOpt <- options(warn= -1) # kill the obsolete warning from R_LockBinding
    on.exit(options(warnOpt))
    if(is.function(value)) {
        ## assign in the namespace for the function as well
        fenv <- environment(value)
        if(is.null(fenv)) # primitives
          fenv <- baseenv()
        if(!identical(fenv, where) && exists(what, envir = fenv, inherits = FALSE #?
                                             ) && bindingIsLocked(what, fenv)) {
            unlockBinding(what, fenv)
            assign(what, value, fenv)
            lockBinding(what, fenv)
        }
    }
    if(exists(what, envir = where, inherits = FALSE) && bindingIsLocked(what, where)) {
      unlockBinding(what, where)
      assign(what, value, where)
      lockBinding(what, where)
    }
    else
      assign(what, value, where)
}

.setMethodOverBinding <- function(what, signature, method, where, verbose = TRUE) {
    if(verbose)
        warning(gettextf("setting a method over the binding of symbol \"%s\" in environment/package \"%s\"", what, getPackageName(where)), domain = NA)
    if(exists(what, envir = where, inherits = FALSE)) {
        fdef <- get(what, envir = where)
        hasFunction <- is(fdef, "genericFunction")
    }

        hasFunction <- FALSE
    if(hasFunction) {
        ## find the generic in the corresponding namespace
        where2 <- findFunction(what, where = environment(fdef))[[1L]] # must find it?
        unlockBinding(what, where)
        setMethod(what, signature, method, where = where)
        lockBinding(what, where)
        ## assign in the package namespace as well
        unlockBinding(what, where2)
        setMethod(what, signature, method, where = where2)
        lockBinding(what, where2)
    }
    else {
        setMethod(what, signature, method, where = where)
    }
}

### finding the package name for a loaded namespace -- kludgy but is there
### a table in this direction anywhere?
.searchNamespaceNames <- function(env) {
    namespaces <- .Internal(getNamespaceRegistry())
    names <- objects(namespaces, all.names = TRUE)
    for(what in names)
        if(identical(get(what, envir=namespaces), env))
            return(paste("namespace", what, sep=":"))
    return(character())
}

.findFunEnvAndName <- function(what, where, signature = NULL) {
    pname <- character()
    if(is.null(signature)) {
        whereF <- findFunction(what, where = where)
        if(length(whereF)>0)
            whereF <- whereF[[1L]]
        else return(list(pname = pname, whereF = baseenv()))
    } else
        whereF <- .genEnv(what, where)

    ## avoid partial matches to "names"
    if("name" %in% names(attributes(whereF)))
        pname <- gsub("^.*:", "", attr(whereF, "name"))
    else if(isNamespace(whereF))
        pname <- .searchNamespaceNames(whereF)
    list(pname = pname, whereF = whereF)
}

.makeTraceClass <- function(traceClassName, className, verbose = TRUE) {
  ## called because the traceClassName not a class
  ## first check whether it may exist but not in the same package
  if(isClass(as.character(traceClassName)))
    return(as.character(traceClassName))
  if(verbose)
    message("Constructing traceable class \"",traceClassName, "\"")
  env <- .classEnv(className)
  if(environmentIsLocked(env)) {
    message("Environment of class \"", className,
            "\" is locked; using global environment for new class")
    env <- .GlobalEnv
    packageSlot(traceClassName) <- NULL
  }
  setClass(traceClassName,
                 contains = c(className, "traceable"), where = env)
  if(existsMethod("show", className, env)) # override it for traceClassName
    setMethod("show", traceClassName, .showTraceable)
  traceClassName
}


