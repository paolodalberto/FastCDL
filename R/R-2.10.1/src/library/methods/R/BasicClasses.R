#  File src/library/methods/R/BasicClasses.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

.InitBasicClasses <- function(envir)
{
    ## setClass won't allow redefining basic classes,
    ## so make the list of these empty for now.
    assign(".BasicClasses", character(), envir)
    ## hide some functions that would break because the basic
    ## classes are not yet defined
    real.reconcileP <- reconcilePropertiesAndPrototype
    assign("reconcilePropertiesAndPrototype",
           function(name, properties, prototype, extends, where) {
               list(properties=properties, prototype = prototype, extends = extends)
           }, envir)
    clList = character()
    setClass("VIRTUAL", where = envir); clList <- c(clList, "VIRTUAL")
    setClass("ANY", where = envir); clList <- c(clList, "ANY")
    setClass("vector", where = envir); clList <- c(clList, "vector")
    setClass("missing", where = envir); clList <- c(clList, "missing")
    ## "numeric" is the class returned by class() for double vectors
    vClasses <- c("logical", "numeric", "character",
                  "complex", "integer", "raw",
                  "expression", "list")
    ## now some pseudo-classes in base, marked specially for new()
    for(.class in vClasses) {
        .setBaseClass(.class, prototype = newBasic(.class), where = envir)
    }
    .setBaseClass("expression", prototype = expression(), where = envir)
    clList <- c(clList, vClasses)
    nullF <- function()NULL; environment(nullF) <- .GlobalEnv
    .setBaseClass("function", prototype = nullF, where = envir); clList <- c(clList, "function")

    setClass("language", where = envir); clList <- c(clList, "language")
    .setBaseClass("environment", prototype = new.env(), where = envir); clList <- c(clList, "environment")

    .setBaseClass("externalptr", prototype = .newExternalptr(), where = envir); clList <- c(clList, "externalptr")

    ## S4, S3 are basic classes that are used to define methods related to being S4, S3 object
    for(cl in c("S4", "S3")) {
        tmp <- newClassRepresentation(className=cl, prototype = defaultPrototype(), virtual=TRUE, package = "methods")
        assignClassDef(cl, tmp, where = envir); clList <- c(clList, cl)
    }

    ## NULL is weird in that it has NULL as a prototype, but is not virtual
    tmp <- newClassRepresentation(className="NULL", prototype = NULL, virtual=FALSE, package = "methods")
    assignClassDef("NULL", tmp, where = envir); clList <- c(clList, "NULL")
    ## the pseudo-NULL used to store NULL as a slot
    ## must match the C code in attrib.c (would be better to use that
    ## code to create .pseudoNULL)
    assign(".pseudoNULL", as.name("\001NULL\001"), envir = envir)


    setClass("structure", where = envir); clList <- c(clList, "structure")
    setClass("nonStructure",  where = envir); #NOT a basic class
    stClasses <- c("matrix", "array") # classes that have attributes, but no class attr.
    for(.class in stClasses) {
        .setBaseClass(.class, prototype = newBasic(.class), where = envir)
    }
    ## "ts" will be defined below as an S3 class, but it is still
    ## included in .BasicClasses, to allow its coerce() method to use
    ## as.ts().  This decision may be revisited.
    clList <- c(clList, stClasses, "ts")
    assign(".BasicClasses", clList, envir)

    ## Now we can define the SClassExtension class and use it to instantiate some
    ## is() relations.
    .InitExtensions(envir)

    for(.class in vClasses)
        setIs(.class, "vector", where = envir)

    setIs("integer", "numeric", where = envir)

    setIs("structure", "vector", coerce = .gblEnv(function(object) as.vector(object)),
          replace = .gblEnv(function(from, to, value) {
              attributes(value) <- attributes(from)
              value
          }),
          where = envir)

    setIs("array", "structure", where = envir)
    setIs("matrix", "array", where = envir)
    setIs("array", "matrix", test = .gblEnv(function(object) length(dim(object)) == 2),
          replace = .gblEnv(function(from, to, value) {
              if(is(value, "matrix"))
                  value
              else
                  stop("replacement value is not a matrix")
          }),
          where = envir)

    ## Some class definitions extending "language", delayed to here so
    ## setIs will work.
    .setBaseClass("name", "language", prototype = as.name("<UNDEFINED>"), where = envir); clList <- c(clList, "name")
    .setBaseClass("call", "language", prototype = quote("<undef>"()), where = envir); clList <- c(clList, "call")
    .setBaseClass("{", "language", prototype = quote({}), where = envir); clList <- c(clList, "{")
    .setBaseClass("if", "language", prototype = quote(if(NA) TRUE else FALSE), where = envir); clList <- c(clList, "if")
    .setBaseClass("<-", "language", prototype = quote("<undef>"<-NULL), where = envir); clList <- c(clList, "<-")
    .setBaseClass("for", "language", prototype = quote(for(NAME in logical()) NULL), where = envir); clList <- c(clList, "for")
    .setBaseClass("while", "language", prototype = quote(while(FALSE) NULL), where = envir); clList <- c(clList, "while")
    .setBaseClass("repeat", "language", prototype = quote(repeat{break}), where = envir); clList <- c(clList, "repeat")
    .setBaseClass("(", "language", prototype = quote((NULL)), where = envir); clList <- c(clList, "(")

    ## a virtual class used to allow NULL as an indicator that a possible function
    ## is not supplied (used, e.g., for the validity slot in classRepresentation
    setClass("OptionalFunction", where = envir)
    setIs("function", "OptionalFunction", where = envir)
    setIs("NULL", "OptionalFunction")
    assign(".BasicClasses", clList, envir)
    assign(".SealedClasses", clList, envir)
    ## restore the true definition of the hidden functions
    assign("reconcilePropertiesAndPrototype", real.reconcileP, envir)
}

.InitS3Classes <- function(envir) {
    ## create a virtual class from which all S3 classes will inherit the .S3Class slot
    setClass("oldClass", representation(.S3Class = "character"),
             contains = "VIRTUAL", prototype = prototype(.S3Class = character()),
             where = envir)
    ## call setOldClass on some known old-style classes.  Ideally this would be done
    ## in the code that uses the classes, but that code doesn't know about the methods
    ## package.
    ## Two steps; first, those classes with a known prototype.  These
    ## can be non-Virtual
    clList <- get(".SealedClasses", envir = envir)
    for(i in seq_along(.OldClassesPrototypes)) {
        el <- .OldClassesPrototypes[[i]]
        if(is.list(el) && length(el) > 1)
            setOldClass(el[[1L]], prototype = el[[2L]],  where = envir)
        else
            warning("OOPS: something wrong with line ",i, " in .OldClassesPrototypes")
    }
    setGeneric("slotsFromS3", where = envir)
    ## the method for "oldClass" is really a constant, just hard to express that way
    setMethod("slotsFromS3", "oldClass", function(object) getClass("oldClass")@slots,
              where = envir)

    setClass("ts", contains = "structure", representation(tsp = "numeric"),
             prototype = prototype(NA, tsp = rep(1,3)), where = envir)

    setOldClass("ts", S4Class = "ts", where = envir)

    setClass("mts", contains=c("matrix", "ts"), prototype =
             prototype(matrix(NA,1,1), tsp = rep(1,3), .S3Class = c("mts", "ts")))
    .init_ts <-	 function(.Object,  ...) {
	if(nargs() < 2) # guaranteed to be called with .Object from new
	    return(.Object)
	args <- list(...)
	argnames <- names(args)
	slotnames <- if(is.null(argnames)) FALSE else {
            nzchar(argnames) & is.na(match(argnames, .tsArgNames)) }
	if(any(slotnames)) {
	    value <- do.call(stats::ts, args[!slotnames])
	    .mergeAttrs(value, .Object, args[slotnames])
	}
	else
	    .mergeAttrs(stats::ts(...), .Object)
    }
    setMethod("initialize", "ts", .init_ts, where = envir)
    setMethod("initialize", "mts", .init_ts, where = envir) #else, it's ambiguous
    ## the following mimics settings for other basic classes ("ts" was
    ## not defined at the time these are done).
    setMethod("coerce", c("ANY", "ts"), function (from, to, strict = TRUE)
              {
                  value <- as.ts(from)
                  if(strict) {
                      attrs <- attributes(value)
                      if(length(attrs) > 2)
                        attributes(value) <- attrs[c("class", "tsp")]
                      value <- .asS4(value)
                  }
                  value
              },
              where = envir)
    setClass("factor", contains = "integer", representation(levels = "character"),
	     validity = function(object) {
		 levs <- levels(object)
		 if (!is.character(levs))
		     return("factor levels must be \"character\"")
		 if (d <- anyDuplicated(levs))
		     return(sprintf("duplicated level [%d] in factor", d))
		 ## 'else'	ok :
		 TRUE
	     },
	     where = envir)
    setOldClass("factor", S4Class = "factor", where = envir)
    setMethod("show", "oldClass", function(object) {
        if(!isS4(object))  {
            print(object)
            return(invisible())
        }
        cl <- as.character(class(object))
        S3Class <- object@.S3Class
        if(length(S3Class)) S3Class <- S3Class[[1L]]
        else S3Class <- "oldClass"      # or error?
        cat("Object of class \"", cl, "\"\n", sep = "")
        print(S3Part(object, strict = TRUE))
        otherSlots <- slotNames(cl)
        S3slots <- slotNames(S3Class)
        otherSlots <- otherSlots[is.na(match(otherSlots, S3slots))]
        for(what in otherSlots) {
            cat('Slot "', what, '":\n', sep = "")
            show(slot(object, what))
            cat("\n")
        }
        NULL
    }, where = envir)
   .initS3 <- function(.Object, ...) {
         if(nargs() < 2)
           return(.Object)
         Class <- class(.Object)
         ClassDef <- getClass(Class)
         S3Class <- attr(ClassDef@prototype, ".S3Class")
         if(is.null(S3Class)) # not a class set up by setOldClass()
             return(callNextMethod())
        S3ClassP <- S3Class[[1L]]
         args <- list(...)
        ## separate the slots, superclass objects
        snames <- allNames(args)
        which <- nzchar(snames)
        elements <- args[which]
        supers <- args[!which]
        thisExtends <- names(ClassDef@contains)
        slotDefs <- ClassDef@slots
        dataPart <- elNamed(slotDefs, ".Data")
        if(is.null(dataPart))
          dataPart <- "missing" # nothing will extend this => no data part args allowed
        if(length(supers) > 0) {
            for(i in rev(seq_along(supers))) {
                obj <- el(supers, i)
                Classi <- class(obj)
                defi <- getClassDef(Classi)
                if(is.null(defi))
                  stop(gettextf("unnamed argument to initialize() for S3 class must have a class defintion; \"%s\" does not", Classi), domain = NA)
                if(is(obj, S3ClassP)) {
                    ## eligible to be the S3 part; merge other slots from prototype;
                    ## obj then becomes the object, with its original class as the S3Class
                    if(is.null(attr(obj, ".S3Class"))) # must be an S3 object; use its own class
                       attr(obj, ".S3Class") <- Classi
                    .Object <- .asS4(.mergeAttrs(obj, .Object))
                }
                else if(is(obj, dataPart)) {
                    ## the S3Class stays from the prototype
                    .Object <- .mergeAttrs(obj, .Object)
                }
                else stop(gettextf("unnamed argument must extend either the S3 class or the class of the data part; not true of class \"%s\"", Classi), domain = NA)

            }
        }
        ## named slots are done as in the default method, which will also call validObject()
        if(length(elements)>0) {
            elements <- c(list(.Object), elements)
            .Object <- do.call(`callNextMethod`, elements)
        }
         else
           validObject(.Object)
         .Object
    }
    setMethod("initialize", "oldClass", .initS3, where = envir)
    ## Next, miscellaneous S3 classes.
    for(cl in .OldClassesList)
        setOldClass(cl, where = envir)
    ## some S3 classes have inheritance on an instance basis, that breaks the S4 contains
    ## model.  To emulate their (unfortunate) behavior requires a setIs with a test.
    for(cl in .OldIsList)
        .setOldIs(cl, envir)
    setClassUnion("data.frameRowLabels", c("character", "integer"), where = envir)
    setClass("data.frame",
             representation(names = "character", row.names = "data.frameRowLabels"),
             contains = "list", prototype = unclass(data.frame()), where = envir) # the S4 version
    setOldClass("data.frame", S4Class = "data.frame", where = envir)
    ## methods to go from S4 to S3; first, using registered class; second, general S4 object
    setMethod("coerce", c("oldClass", "S3"), function (from, to, strict = TRUE)
              {
                  from <- .notS4(from) # not needed? ensures that class() can return >1 string
                  cl <- class(from)
                  cl1 <- .class1(from)
                  classDef <- getClassDef(cl1)
                  S3Class <- attr(classDef@prototype, ".S3Class")
                  if(length(S3Class) > length(cl))  #add S3 inheritance
                      attr(from, "class") <- S3Class
                  from
              },
              where = envir)
    setMethod("coerce", c("ANY", "S3"), function (from, to, strict = TRUE)
              {
                  switch(typeof(from),
                         S4 = stop(gettextf("Class \"%s\" does not have an S3 data part, and so is of type \"S4\"; no S3 equivalent",class(from)), domain = NA),
                         .notS4(from) )
              },
              where = envir)
    setMethod("coerce", c("ANY", "S4"), function (from, to, strict = TRUE)
              {
                  if(isS4(from)) {
                      value <- from
                  }
                  else {
                      cl <- .class1(from)
                      classDef <- getClass(cl)
                      if(identical(classDef@virtual, TRUE))
                        stop(gettextf("Class \"%s\" is VIRTUAL; not meaningful to create an S4 object from this class", cl), domain = NA)
                      pr <- classDef@prototype
                      value <- new(cl)
                      slots <- classDef@slots
                      if(match(".Data", names(slots), 0L) > 0L) {
                          data <- unclass(from)
                          if(!is(data, slots[[".Data"]]))
                            stop(gettextf("Object must be a valid data part for class \"%s\"; not true of type \"%s\"", cl, class(data)),
                                 domain = NA)
                          value@.Data <- unclass(from)
                      }
                      ## copy attributes:  Note that this copies non-slots as well
                      ## but checks the slots for validity
                      anames <- names(attributes(from))
                      isSlot <- anames %in% names(slots)
                      for(i in seq_along(anames)) {
                          what <- anames[[i]]
                          if(isSlot[[i]])
                            slot(value, what) <- attr(from, what)
                          else
                            attr(value, what) <- attr(from, what)
                      }
                  }
                  if(strict)
                    ## validate.  If we created S4 object, slots were tested; else, not
                    ## so complete= is set accordingly.
                      validObject(value, complete = isS4(from))
                  value
              })
    assign(".SealedClasses", c(clList,unique(unlist(.OldClassesList))),  envir)
}

### create a class definition for one of the pseudo-classes in base
### The class name does _not_ have a package attribute, which signals
### the C coded for new() to return an object w/o explicit class
### attribute, to be consistent with older R code
.setBaseClass <- function(cl, ..., where) {
    setClass(cl, ..., where = where)
    def <- getClassDef(cl, where)
    def@className <- as.character(def@className)
    def@prototype <- .notS4(def@prototype)
    assignClassDef(cl, def, where = where)
}


.tsArgNames <- names(formals(stats::ts))

### The following methods are now activated
### via the last line of the function .InitMethodDefinitions in ./MethodsListClass.R
###
### Tradeoff between intuition of users that
### new("matrix", ...) should be like matrix(...) vs consistency of new().
### Relevant when new class has basic class as its data part.
.InitBasicClassMethods <- function(where) {
    ## methods to initialize "informal" classes by using the
    ## functions of the same name.

    ## These methods are designed to be inherited or extended
    setMethod("initialize", "matrix", where = where,
	      function(.Object, data = NA, nrow = 1, ncol = 1,
		       byrow = FALSE, dimnames = NULL, ...) {
		  if((na <- nargs()) < 2) # guaranteed to be called with .Object from new
		      .Object
		  else if(length(dots <- list(...)) && ".Data" %in% names(dots)) {
		      if(na == 2)
			  .mergeAttrs(dots$.Data, .Object)
		      else {
			  dat <- dots$.Data
			  dots <- dots[names(dots) != ".Data"]
			  if(na == 2 + length(dots)) {
			      .mergeAttrs(as.matrix(dat), .Object, dots)
			  }
			  else
			      stop("Cannot specify matrix() arguments when specifying .Data")
		      }
		  }
		  else if(is.matrix(data) && na == 2 + length(dots))
		      .mergeAttrs(data, .Object, dots)
		  else {
		      if (missing(nrow))
			  nrow <- ceiling(length(data)/ncol)
		      else if (missing(ncol))
			  ncol <- ceiling(length(data)/nrow)
		      value <- matrix(data, nrow, ncol, byrow, dimnames)
		      .mergeAttrs(value, .Object, dots)
		  }
	      })

    setMethod("initialize", "array", where = where,
	      function(.Object, data = NA, dim = length(data),
		       dimnames = NULL, ...) {
		  if((na <- nargs()) < 2) # guaranteed to be called with .Object from new
		      .Object
		  else if(length(dots <- list(...)) && ".Data" %in% names(dots)) {
		      if(na == 2)
			  .mergeAttrs(dots$.Data, .Object)
		      else {
			  dat <- dots$.Data
			  dots <- dots[names(dots) != ".Data"]
			  if(na == 2 + length(dots)) {
			      .mergeAttrs(as.array(dat), .Object, dots)
			  }
			  else
			      stop("Cannot specify array() arguments when specifying .Data")
		      }
		  }
		  else if(is.array(data) && na == 2 + length(dots))
		      .mergeAttrs(data, .Object, dots)
		  else {
		      value <- array(data, dim, dimnames)
		      .mergeAttrs(value, .Object, dots)
		  }
	      })
    ## following should not be needed if data_class2 returns "array",...
##     setMethod("[", # a method to avoid invalid objects from an S4 class
##               signature(x = "array"), where = where,
##               function (x, i, j, ..., drop = TRUE)
##               {
##                 value <- callNextMethod()
##                 if(is(value, class(x)))
##                   value@.Data
##                 else
##                   value
##               })

}

## .OldClassList is a purely heuristic list of known old-style classes, with emphasis
## on old-style class inheritiance.  Used in .InitBasicClasses to call setOldClass for
## each known class pattern.
## .OldClassesPrototypes is a list of S3 classes for which prototype
## objects are known & reasonable.  The classes will reappear in
## .OldClassesList, but will have been initialized first in
## .InitBasicClasses.  NB:  the methods package will NOT set up
## prototypes for S3 classes except those in package base and for "ts"
## (and would rather not do those either).  The package that owns the
## S3 class should have code to call setOldClass in its
## initialization.
.OldClassesPrototypes <-
  list(
       list("data.frame",  data.frame(), "data.frame"),
       list("factor",  factor()),
       list(c("ordered", "factor"), ordered(character())),
       list("table",  table(factor())),
       list("summary.table",  summary.table(table(factor())))
       , list("ts", stats::ts())
       )
.OldClassesList <-
    list(
         c("anova", "data.frame"),
         c("mlm", "lm"),
         c("aov", "lm"), # see also .OldIsList
         c("maov", "mlm", "lm"),
         "POSIXt", "POSIXct", "POSIXlt", # see .OldIsList
         "Date",
         "dump.frames",
         c("ordered", "factor"),
         c("glm.null", "glm", "lm"),
         c("anova.glm.null", "anova.glm"),
         "hsearch",
         "integrate",
         "packageInfo",
         "libraryIQR",
         "packageIQR",
         "mtable",
         "table",
         "summary.table",
         "recordedplot",
         "socket",
         "packageIQR",
         "density",
         "formula",
         "logLik",
         "rle"
)

# These relations sometimes hold, sometimes not:  have to look in the S3
# class attribute to test.
.OldIsList <- list(
                   c("POSIXt", "POSIXct"),
                   c("POSIXt", "POSIXlt"),
                   c("aov","mlm")
                   )

.InitSpecialTypes <- function(where) {
    if(!exists(".S3MethodsClasses", envir = where, inherits = FALSE)) {
      S3table <- new.env()
      assign(".S3MethodsClasses", S3table, envir = where)
    }
    else S3table <- get(".S3MethodsClasses", envir = where)
  for(cl in c("environment", "externalptr", "name", "NULL")) {
      ncl <- paste(".",cl, sep="")
      setClass(ncl, representation(.xData = cl), where = where)
      setIs(ncl, cl, coerce = function(from) from@.xData,
        replace = function(from, value){ from@.xData <- value; from},
        where = where)
      ## these classes need explicit coercion for S3 methods
      assign(cl, getClass(cl, where), envir = S3table)
    }
  setMethod("$<-", ".environment", function (x, name, value) {
    call <- sys.call()
    call[[2]] <- x@.Data
    eval.parent(call)
    x
  })
  setMethod("[[<-", ".environment", function (x, i, j, ..., value) {
    call <- sys.call()
    call[[2]] <- x@.Data
    eval.parent(call)
    x
  })
}
