#  File src/library/methods/R/promptClass.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

promptClass <-
function (clName, filename = NULL, type = "class",
	  keywords = "classes", where = topenv(parent.frame()))
{
    classesInSig <- function(g, where) {
    ## given a generic g, obtain list of all classes
    ## named among its signatures
	mlist <- getMethods(g, where) # TODO: change this to findMethods()
	if(is.null(mlist))
	    return(NULL)
	tmp <- listFromMlist(mlist)
	if ((lt <- length(tmp[[1L]])) == 0L)
	    NULL
	else if (lt == 1)
	    unlist(tmp[[1L]])
	else { ## lt >= 2
	    lapply(tmp[[1L]], unlist)
	}
    }
    genWithClass <- function(cl, where) {
    ## given a class cl
    ## obtain list of all generics with cl in
    ## one of its signatures
	allgen <- getGenerics(where = where)
	o <- sapply(allgen, classesInSig, where = where, simplify = FALSE)
	genl <- NULL
	nmok <- names(o)
	for (i in seq_along(o)) {
	    if (!all(is.na(match(unlist(o[[i]]), cl))))
		genl <- c(genl, nmok[i])
	}
	genl
    }

    sigsList <- function (g, where)
    ## given a generic g, obtain list with one element per signature
    {
	tmp <- listFromMlist(getMethods(g, where)) # TODO: change this to findMethods()
	if (length(tmp[[1L]])) tmp[[1L]] # else NULL
    }
    slotClassWithSource <- function(clname) {
	clDef <- getClassDef(clname)
	extds <- names(clDef@contains)
	allslots <- getSlots(clDef) ## establishes all slots, in the right order
	for(j in rev(seq_along(extds))) {
	    i <- extds[[j]]
	    slotsi <- getSlots(getClass(i))
	    if(length(slotsi))
		allslots[names(slotsi)] <- paste0("\"", as.character(slotsi),
						  "\", from class \"", i, "\"")
	}
	slotsi <- getSlots(clDef)
	if(length(slotsi))
	    allslots[names(slotsi)] <- paste0("\"", as.character(slotsi),"\"")
	allslots
    }
    paste0 <- function(...) paste(..., sep = "")
    pastePar <- function(x) {
	xn <- names(x); x <- as.character(x)
	xn <- if(length(xn) == length(x)) paste(xn, "= ") else ""
	paste("(", paste(xn, "\"", x, "\"", sep = "", collapse = ", "),
	")", sep = "")
    }
    escape <- function(txt) gsub("%", "\\\\%", txt)

    if(is.null(filename))
	filename <- paste0(utils:::topicName(type, clName), ".Rd")
    if(!missing(where) && !is.na(match(clName, getClasses(where))))
      whereClass <- where
    else {
        whereClass <- find(classMetaName(clName))
        if(length(whereClass) == 0L)
          stop(gettextf("no definition of class \"%s\" found", clName),
               domain = NA)
        else if(length(whereClass) > 1L) {
            if(identical(where, topenv(parent.frame()))) {
                whereClass <- whereClass[[1L]]
                warning(gettextf("multiple definitions of \"%s\" found; using the one on %s",
                                 clName, whereClass), domain = NA)
            }
            else {
                if(exists(classMetaName(clName), where, inherits = FALSE))
                  whereClass <- where
                else
                  stop(gettextf("no definition of class \"%s\" in the specified position, %s, definition(s) on : %s",
                                clName, where,
                                paste(whereClass, collapse = ", ")),
                       domain = NA)
            }
        }
    }
    fullName <- utils:::topicName("class", clName)
    clDef <- getClass(clName, where = whereClass)
    .name <- paste0("\\name{", fullName, "}")
    .type <- paste0("\\docType{", type, "}")
    .alias <- paste0("\\alias{", fullName, "}")
    .title <- paste0("\\title{Class \"", clName, "\" ~~~ }")
    .desc <- paste0("\\description{", "	 ~~ A concise (1-5 lines) description of what the class is.  ~~",
	"}")
    slotclasses <- getSlots(clDef)
    slotnames <- names(slotclasses)
    slotclasses <- as.character(slotclasses)
    nslots <- length(slotclasses)
    .usage <- "\\section{Objects from the Class}"
    clNameQ <- paste0('"', clName, '"')
    if(isVirtualClass(clName)) {
	.usage <- paste0(.usage, "{A virtual Class: No objects may be created from it.}")
    }
    else {
	initMethod <- unRematchDefinition(selectMethod("initialize", clName))
	argNames <- formalArgs(initMethod)
	## but for new() the first argument is the class name
	argNames[[1L]] <- clNameQ
	.usage <- c(paste0(.usage,"{"),
		    paste0("Objects can be created by calls of the form \\code{",
                           .makeCallString(initMethod, "new", argNames), "}."),
		    "	 ~~ describe objects here ~~ ", "}")
    }
    .slots <- if (nslots > 0) {
	slotclasses <- slotClassWithSource(clName)
	slotnames <- names(slotclasses)
	.slots.head <- c("\\section{Slots}{", "  \\describe{")
	.slots.body <-	paste0("    \\item{\\code{", slotnames,
                               "}:}", "{Object of class \\code{",
                               slotclasses, "} ~~ }")
	.slots.tail <- c("  }","}")
	c(.slots.head,  .slots.body,	.slots.tail)
    } else character()
    .extends <- clDef@contains
## FIXME: the superclass slots should be marked as such
##       and left *optional* to be documented
    if(length(.extends)) {
	.extends <- showExtends(.extends, printTo = FALSE)
	.extends <-
	    c("\\section{Extends}{",
	      paste0("Class \\code{\"\\linkS4class{",
		    .extends$what,
		    "}\"}, ",
		    ## Add Rd markup to 'by class "CLASS"' results
		    gsub("^(by class) (\".*\")$", "\\1 \\\\code{\\2}",
			 .extends$how),
		    "."),
	      "}")
    }
    else
	.extends <- character()
    nmeths <- length(methnms <- genWithClass(clName, where = whereClass))
    .meths.head <- "\\section{Methods}{"
    .methAliases <- ""
    if (nmeths > 0) {
	.meths.body <- "  \\describe{"
	for (i in 1L:nmeths) {
	    .sig <- sigsList(methnms[i], where = whereClass)
	    for (j in seq_along(.sig)) {
		if (!all(is.na(match(.sig[[j]],clName)))) {
		    methn.i <- escape(methnms[i])
		    .meths.body <-
			c(.meths.body,
			  paste0("    \\item{",
				 methn.i, "}{\\code{signature",
				 pastePar(.sig[[j]]), "}: ... }"))

		    cur <- paste(.sig[[j]], collapse = ",")
		    .methAliases <- paste0(.methAliases, "\\alias{",
					   methn.i, ",", cur, "-method}\n")
		}
	    }
	}
	.meths.body <- c(.meths.body, "	 }")
    }
    else {
	.meths.head <- "\\section{Methods}{"
	.meths.body <- paste("No methods defined with class", clNameQ,
                             "in the signature.")
    }
    .meths.tail <- "}"
    .keywords <- paste0("\\keyword{", keywords, "}")

    Rdtxt <-
	list(name = .name,
             version = "\\Rdversion{1.1}",
	     type = .type,
	     aliases = .alias,
	     methAliases = .methAliases,
	     title = .title,
	     description = .desc,
	     "section{Objects from the Class}" = .usage,
	     "section{Slots}" = .slots,
	     "section{Extends}" = .extends,
	     "section{Methods}" =
	     c(.meths.head, .meths.body, .meths.tail),
	     references = paste("\\references{ ~put references to the",
	     "literature/web site here ~ }"),
	     author = "\\author{ ~~who you are~~ }",
	     note =
	     c("\\note{ ~~further notes~~ }",
	       "",
	       paste(" ~Make other sections like Warning with",
		     "\\section{Warning }{....} ~"),
	       ""),
	     seealso =
	     c("\\seealso{",
	       paste("	~~objects to See Also as",
		     "\\code{\\link{~~fun~~}}, ~~~"),
	       paste("	or \\code{\\linkS4class{CLASSNAME}}",
		     "for links to other classes"),
	       "}"),
	     examples = c("\\examples{",
	     paste0("showClass(", clNameQ, ")"),
	     "}"),
	     keywords = .keywords)

    if(is.na(filename)) return(Rdtxt)

    cat(unlist(Rdtxt), file = filename, sep = "\n")
    .message("A shell of class documentation has been written",
             .fileDesc(filename), ".\n")
    invisible(filename)
}

## used in promptClass() above and in promptMethods() :
.fileDesc <- function(file) {
    if(is.character(file)) {
	if(nzchar(file))
	    paste(" to the file", sQuote(file))
	else
	    " to the standard output connection"
    }
    else if(inherits(file, "connection"))
	paste(" to the connection",
              sQuote(summary(file)$description))
    else "" # what, indeed?
}


.makeCallString <- function (def, name = substitute(def), args = formalArgs(def))
{
##
## need this for experimentation because the function is not exported
##
    if (is.character(def)) {
	if (missing(name))
	    name <- def
	def <- getFunction(def)
    }
    if (is(def, "function"))
	paste(name, "(", paste(args, collapse = ", "), ")", sep = "")
    else ""
}
