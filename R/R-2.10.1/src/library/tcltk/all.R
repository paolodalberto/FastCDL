#  File src/library/tcltk/R/Tk.R
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

### ------ Basics ------


.Tcl <- function(...)
    structure(.External("dotTcl", ..., PACKAGE = "tcltk"),
              class="tclObj")
.Tcl.objv <- function(objv)
    structure(.External("dotTclObjv", objv, PACKAGE = "tcltk"),
              class="tclObj")

.Tcl.callback <- function(...)
    .External("dotTclcallback", ..., PACKAGE = "tcltk")

.Tcl.args <- function(...) {
    ## Eek! (See .Tcl.args.objv for explanation)
    pframe <- parent.frame(3)
    ## Convert argument tags to option names (i.e. stick "-" in front)
    name2opt <- function(x)
        if ( x != "")
            paste("-",x,sep="")
        else ""

    isCallback <- function(x)
	is.function(x) || is.call(x) || is.expression(x)

    makeAtomicCallback <- function(x, e) {
	if (is.name(x))
	    x <- eval(x, e)
	if (is.call(x)){
	    if(identical(x[[1L]], as.name("break")))
		return("break")
	    if(identical(x[[1L]], as.name("function")))
                x <- eval(x, e)
        }
	.Tcl.callback(x, e)
    }

    makeCallback <- function(x, e) {
	if (is.expression(x))
	    paste(lapply(x,makeAtomicCallback, e),collapse=";")
	else
	    makeAtomicCallback(x, e)
    }

    ## Convert arguments. Callbacks and windows require special treatment
    ## everything else is converted to strings
    val2string <- function(x) {
        if (is.null(x)) return("")
        if (is.tkwin(x)){current.win <<- x ; return (.Tk.ID(x))}
	if (inherits(x,"tclVar")) return(ls(unclass(x)$env))
        if (isCallback(x)){
	    # Jump through some hoops to protect from GC...
	    ref <- local({value<-x; envir<-pframe; environment()})
            callback <- makeCallback(get("value",envir=ref),
		                     get("envir",envir=ref))
	    callback <- paste("{", callback, "}")
            assign(callback, ref, envir=current.win$env)
            return(callback)
        }
        ## quoting hell...
        x <- gsub("\\\\", "\\\\\\\\", as.character(x))
        x <- gsub("\"","\\\\\"", as.character(x))
        x <- gsub("\\[","\\\\[", as.character(x))
        x <- gsub("\\$","\\\\$", as.character(x))
        paste("\"", x, "\"", sep = "", collapse = " ")
    }

    val <- list(...)
    nm <- names(val)

    if (length(val) == 0L) return("")
    nm <- if (is.null(nm))
        rep("", length(val))
    else
        sapply(nm, name2opt)

    ## This is a bit dodgy: we need to ensure that callbacks don't get
    ## garbage collected, so we try registering them with the relevant
    ## window, which is assumed to be the last preceding window
    ## argument during val2string processing if one occurs, or the
    ## "win" variable of the caller (tkwidget calls) or as a last
    ## resort .TkRoot. What a mess!

    current.win <-
        if (exists("win", envir=parent.frame()))
            get("win", envir=parent.frame())
        else .TkRoot

    val <- sapply(val, val2string)
    paste(as.vector(rbind(nm, val)), collapse=" ")
}

.Tcl.args.objv <- function(...) {

    ## Eek! This is broken by design...
    ## The issue is that if a callback is given in the form of an expression,
    ## then we need to ensure that it is evaluated in the proper environment
    ## The typical case is that tkbind() calls tcl() calls  .Tcl.args.objv()
    ## so we grab 3 levels back. This will break direct calls to tcl(), though.

    pframe <- parent.frame(3)

    isCallback <- function(x)
	is.function(x) || is.call(x) || is.expression(x)

    makeAtomicCallback <- function(x, e) {
	if (is.name(x))
	    x <- eval(x, e)
	if (is.call(x)){
	    if(identical(x[[1L]], as.name("break")))
		return("break")
	    if(identical(x[[1L]], as.name("function")))
                x <- eval(x, e)
        }
	.Tcl.callback(x, e)
    }

    makeCallback <- function(x, e) {
	if (is.expression(x))
	    paste(lapply(x,makeAtomicCallback, e),collapse=";")
	else
	    makeAtomicCallback(x, e)
    }

    ## Convert arguments. Callbacks and windows require special treatment
    ## everything else is converted to strings
    val2obj <- function(x) {
        if (is.null(x)) return(NULL)
        if (is.tkwin(x)){current.win <<- x ; return(as.tclObj(.Tk.ID(x)))}
	if (inherits(x,"tclVar")) return(as.tclObj(ls(unclass(x)$env)))
        if (isCallback(x)){
	    # Jump through some hoops to protect from GC...
	    ref <- local({value<-x; envir<-pframe; environment()})
            callback <- makeCallback(get("value",envir=ref),
		                     get("envir",envir=ref))
            assign(callback, ref, envir=current.win$env)
            return(as.tclObj(callback, drop=TRUE))
        }
        as.tclObj(x, drop=TRUE)
    }

    val <- list(...)

    ## This is a bit dodgy: we need to ensure that callbacks don't get
    ## garbage collected, so we try registering them with the relevant
    ## window, which is assumed to be the last preceding window
    ## argument during val2string processing if one occurs,
    ## or as a last resort .TkRoot. What a mess!

    current.win <- .TkRoot

    lapply(val, val2obj)
}


.Tk.ID <- function(win) win$ID

.Tk.newwin <- function(ID){
    win <- list(ID=ID, env=evalq(new.env(),.GlobalEnv))
    evalq(num.subwin <- 0, win$env)
    class(win) <- "tkwin"
    win
}

.Tk.subwin <- function(parent) {
    ID <- paste(parent$ID,evalq(num.subwin<-num.subwin+1, parent$env),
                sep=".")
    win<-.Tk.newwin(ID)
    assign(ID, win, envir=parent$env)
    assign("parent", parent, envir=win$env)
    win
}

tkdestroy  <- function(win) {
    tcl("destroy", win)
    ID <- .Tk.ID(win)
    env <- get("parent", envir=win$env)$env
    if (exists(ID, envir=env, inherits=FALSE))
        rm(list=ID, envir=env)
}

is.tkwin <- function(x) inherits(x, "tkwin")

tclVar <- function(init="") {
   n <- evalq(TclVarCount <- TclVarCount + 1, .TkRoot$env)
   name <- paste("::RTcl", n, sep="")
   l <- list(env=new.env())
   assign(name,NULL,envir=l$env)
   reg.finalizer(l$env,function(env)tcl("unset",ls(env)))
   class(l)<-"tclVar"
   tclvalue(l) <- init
   l
}

tclObj <- function(x) UseMethod("tclObj")
"tclObj<-" <- function(x, value) UseMethod("tclObj<-")

tclObj.tclVar <- function(x){
    z <- .External("RTcl_ObjFromVar", ls(x$env), PACKAGE="tcltk")
    class(z) <- "tclObj"
    z
}

"tclObj<-.tclVar" <- function(x, value){
    value <- as.tclObj(value)
    .External("RTcl_AssignObjToVar", ls(x$env), value, PACKAGE="tcltk")
    x
}

tclvalue <- function(x) UseMethod("tclvalue")
"tclvalue<-" <- function(x, value) UseMethod("tclvalue<-")

tclvalue.tclVar <- function(x) tclvalue(tclObj(x))
tclvalue.tclObj <- function(x) .External("RTcl_StringFromObj", x,
                                         PACKAGE="tcltk")
print.tclObj <- function(x,...) {
    z <- tclvalue(x)
    if (length(z)) cat("<Tcl>", z, "\n")
    invisible(x)
}

"tclvalue<-.tclVar" <- function(x, value) {
    name <- ls(unclass(x)$env)
    tcl("set", name, value)
    x
}

tclvalue.default <- function(x) tclvalue(tcl("set", as.character(x)))

"tclvalue<-.default" <- function(x, value) {
    name <- as.character(x)
    tcl("set", name, value)
    x
}

as.character.tclVar <- function(x, ...) ls(unclass(x)$env)

as.character.tclObj <- function(x, ...)
    .External("RTcl_ObjAsCharVector", x, PACKAGE="tcltk")
as.double.tclObj <- function(x, ...)
    .External("RTcl_ObjAsDoubleVector", x, PACKAGE="tcltk")
as.integer.tclObj <- function(x, ...)
    .External("RTcl_ObjAsIntVector", x, PACKAGE="tcltk")
as.logical.tclObj <- function(x, ...)
    as.logical(.External("RTcl_ObjAsIntVector",
                         x, PACKAGE="tcltk"))
as.raw.tclObj <- function(x, ...)
    .External("RTcl_ObjAsRawVector", x, PACKAGE="tcltk")

is.tclObj <- function(x) inherits(x, "tclObj")

as.tclObj <- function(x, drop=FALSE) {
    if (is.tclObj(x)) return(x)
    z <- switch(storage.mode(x),
                character =
                .External("RTcl_ObjFromCharVector", x, drop, PACKAGE="tcltk"),
                double =
                .External("RTcl_ObjFromDoubleVector", x,drop,PACKAGE="tcltk"),
                integer =
                .External("RTcl_ObjFromIntVector", x, drop, PACKAGE="tcltk"),
                logical =
                .External("RTcl_ObjFromIntVector", as.integer(x), drop,
                          PACKAGE="tcltk"),
                stop(gettextf("cannot handle object of mode '%s'",
                              storage.mode(x)), domain = NA)
                )
    class(z) <- "tclObj"
    z
}
# Actually makes .default and .tclVar methods equivalent, the latter
# just saves a level of function dispatching

tclServiceMode <- function(on = NULL) .External("RTcl_ServiceMode", as.logical(on), PACKAGE="tcltk")

#----

.TkRoot <- .Tk.newwin("")
tclvar  <- structure(NULL,class="tclvar")
evalq(TclVarCount <- 0, .TkRoot$env)


# ------ Widgets ------

tkwidget <- function (parent, type, ...) # generic
{
    win <- .Tk.subwin(parent)
    # older version had .Tk.ID(win) here, but this makes for easier
    # current.win handling
    tcl(type, win, ...)
    win
}

tkbutton      <- function(parent, ...) tkwidget(parent, "button", ...)
tkcanvas      <- function(parent, ...) tkwidget(parent, "canvas", ...)
tkcheckbutton <- function(parent, ...) tkwidget(parent, "checkbutton", ...)
tkentry       <- function(parent, ...) tkwidget(parent, "entry", ...)
tkframe       <- function(parent, ...) tkwidget(parent, "frame", ...)
tklabel       <- function(parent, ...) tkwidget(parent, "label", ...)
tklistbox     <- function(parent, ...) tkwidget(parent, "listbox", ...)
tkmenu        <- function(parent, ...) tkwidget(parent, "menu", ...)
tkmenubutton  <- function(parent, ...) tkwidget(parent, "menubutton", ...)
tkmessage     <- function(parent, ...) tkwidget(parent, "message", ...)
tkradiobutton <- function(parent, ...) tkwidget(parent, "radiobutton", ...)
tkscale       <- function(parent, ...) tkwidget(parent, "scale", ...)
tkscrollbar   <- function(parent, ...) tkwidget(parent, "scrollbar", ...)
tktext        <- function(parent, ...) tkwidget(parent, "text", ...)

ttkbutton      <- function(parent, ...) tkwidget(parent, "ttk::button", ...)
ttkcheckbutton <- function(parent, ...) tkwidget(parent, "ttk::checkbutton", ...)
ttkcombobox    <- function(parent, ...) tkwidget(parent, "ttk::combobox", ...)
ttkentry       <- function(parent, ...) tkwidget(parent, "ttk::entry", ...)
ttkframe       <- function(parent, ...) tkwidget(parent, "ttk::frame", ...)
ttkimage       <- function(parent, ...) tkwidget(parent, "ttk::image", ...)
ttklabel       <- function(parent, ...) tkwidget(parent, "ttk::label", ...)
ttklabelframe  <- function(parent, ...) tkwidget(parent, "ttk::labelframe", ...)
ttkmenubutton  <- function(parent, ...) tkwidget(parent, "ttk::menubutton", ...)
ttknotebook    <- function(parent, ...) tkwidget(parent, "ttk::notebook", ...)
ttkpanedwindow <- function(parent, ...) tkwidget(parent, "ttk::panedwindow", ...)
ttkprogressbar <- function(parent, ...) tkwidget(parent, "ttk::progressbar", ...)
ttkradiobutton <- function(parent, ...) tkwidget(parent, "ttk::radiobutton", ...)
ttkscrollbar   <- function(parent, ...) tkwidget(parent, "ttk::scrollbar", ...)
ttkseparator   <- function(parent, ...) tkwidget(parent, "ttk::separator", ...)
ttksizegrip    <- function(parent, ...) tkwidget(parent, "ttk::sizegrip", ...)
ttktreeview    <- function(parent, ...) tkwidget(parent, "ttk::treeview", ...)


tktoplevel    <- function(parent=.TkRoot,...) {
    w <- tkwidget(parent,"toplevel",...)
    ID <- .Tk.ID(w)
    tkbind(w, "<Destroy>",
           function() {
               if (exists(ID, envir=parent$env, inherits=FALSE))
                   rm(list=ID, envir=parent$env)
               tkbind(w, "<Destroy>","")
           })
    w
}
### ------ Window & Geometry managers, widget commands &c ------

tcl <- function(...) .Tcl.objv(.Tcl.args.objv(...))

tktitle <- function(x) tcl("wm", "title", x)

"tktitle<-" <- function(x, value) {
    tcl("wm", "title", x, value)
    x
}

tkbell     <- function(...) tcl("bell", ...)
tkbind     <- function(...) tcl("bind", ...)
tkbindtags <- function(...) tcl("bindtags", ...)
tkfocus    <- function(...) tcl("focus", ...)
tklower    <- function(...) tcl("lower", ...)
tkraise    <- function(...) tcl("raise", ...)


tkclipboard.append <- function(...) tcl("clipboard", "append", ...)
tkclipboard.clear  <- function(...) tcl("clipboard", "clear", ...)


tkevent.add      <- function(...) tcl("event", "add", ...)
tkevent.delete   <- function(...) tcl("event", "delete", ...)
tkevent.generate <- function(...) tcl("event", "generate", ...)
tkevent.info     <- function(...) tcl("event", "info", ...)


tkfont.actual    <- function(...) tcl("font", "actual", ...)
tkfont.configure <- function(...) tcl("font", "configure", ...)
tkfont.create    <- function(...) tcl("font", "create", ...)
tkfont.delete    <- function(...) tcl("font", "delete", ...)
tkfont.families  <- function(...) tcl("font", "families", ...)
tkfont.measure   <- function(...) tcl("font", "measure", ...)
tkfont.metrics   <- function(...) tcl("font", "metrics", ...)
tkfont.names     <- function(...) tcl("font", "names", ...)

tkgrab         <- function(...) tcl("grab", ...)
tkgrab.current <- function(...) tcl("grab", "current", ...)
tkgrab.release <- function(...) tcl("grab", "release", ...)
tkgrab.set     <- function(...) tcl("grab", "set", ...)
tkgrab.status  <- function(...) tcl("grab", "status", ...)

tkimage.cget     <- function(...) tcl("image","cget",...)
tkimage.configure<- function(...) tcl("image","configure",...)
tkimage.create   <- function(...) tcl("image","create",...)
tkimage.names    <- function(...) tcl("image","names",...)

## NB: some widgets also have a selection.clear command, hence the "X".
## tkselection.clear might be made a generic function instead.
tkXselection.clear  <- function(...) tcl("selection", "clear", ...)
tkXselection.get    <- function(...) tcl("selection", "get", ...)
tkXselection.handle <- function(...) tcl("selection", "handle", ...)
tkXselection.own    <- function(...) tcl("selection", "own", ...)

tkwait.variable  <- function(...) tcl("tkwait", "variable", ...)
tkwait.visibility<- function(...) tcl("tkwait", "visibility", ...)
tkwait.window    <- function(...) tcl("tkwait", "window", ...)

## Standard dialogs
tkgetOpenFile    <- function(...) tcl("tk_getOpenFile", ...)
tkgetSaveFile    <- function(...) tcl("tk_getSaveFile", ...)
tkchooseDirectory<- function(...) tcl("tk_chooseDirectory", ...)
tkmessageBox     <- function(...) tcl("tk_messageBox", ...)
tkdialog         <- function(...) tcl("tk_dialog", ...)
tkpopup          <- function(...) tcl("tk_popup", ...)


## File handling functions

tclfile.tail <- function(...) tcl("file", "tail", ...)
tclfile.dir  <- function(...) tcl("file", "dir", ...)
tclopen      <- function(...) tcl("open", ...)
tclclose     <- function(...) tcl("close", ...)
tclputs      <- function(...) tcl("puts", ...)
tclread      <- function(...) tcl("read", ...)

## Tkwinfo actually has a bazillion subcommands, but it's rarely
## used, so let's be lazy

tkwinfo <- function(...) tcl("winfo", ...)

## Not so with tkwm.

tkwm.aspect          <- function(...) tcl("wm", "aspect", ...)
tkwm.client          <- function(...) tcl("wm", "client", ...)
tkwm.colormapwindows <- function(...) tcl("wm", "colormapwindows", ...)
tkwm.command         <- function(...) tcl("wm", "command", ...)
tkwm.deiconify       <- function(...) tcl("wm", "deiconify", ...)
tkwm.focusmodel      <- function(...) tcl("wm", "focusmodel", ...)
tkwm.frame           <- function(...) tcl("wm", "frame", ...)
tkwm.geometry        <- function(...) tcl("wm", "geometry", ...)
tkwm.grid            <- function(...) tcl("wm", "grid", ...)
tkwm.group           <- function(...) tcl("wm", "group", ...)
tkwm.iconbitmap      <- function(...) tcl("wm", "iconbitmap", ...)
tkwm.iconify         <- function(...) tcl("wm", "iconify", ...)
tkwm.iconmask        <- function(...) tcl("wm", "iconmask", ...)
tkwm.iconname        <- function(...) tcl("wm", "iconname ", ...)
tkwm.iconposition    <- function(...) tcl("wm", "iconposition", ...)
tkwm.iconwindow      <- function(...) tcl("wm", "iconwindow ", ...)
tkwm.maxsize         <- function(...) tcl("wm", "maxsize", ...)
tkwm.minsize         <- function(...) tcl("wm", "minsize", ...)
tkwm.overrideredirect<- function(...) tcl("wm", "overrideredirect", ...)
tkwm.positionfrom    <- function(...) tcl("wm", "positionfrom", ...)
tkwm.protocol        <- function(...) tcl("wm", "protocol", ...)
tkwm.resizable       <- function(...) tcl("wm", "resizable", ...)
tkwm.sizefrom        <- function(...) tcl("wm", "sizefrom", ...)
tkwm.state           <- function(...) tcl("wm", "state", ...)
tkwm.title           <- function(...) tcl("wm", "title", ...)
tkwm.transient       <- function(...) tcl("wm", "transient", ...)
tkwm.withdraw        <- function(...) tcl("wm", "withdraw", ...)


### Geometry managers

tkgrid                 <- function(...) tcl("grid", ...)
tkgrid.bbox            <- function(...) tcl("grid", "bbox", ...)
tkgrid.columnconfigure <- function(...) tcl("grid", "columnconfigure", ...)
tkgrid.configure       <- function(...) tcl("grid", "configure", ...)
tkgrid.forget          <- function(...) tcl("grid", "forget", ...)
tkgrid.info            <- function(...) tcl("grid", "info", ...)
tkgrid.location        <- function(...) tcl("grid", "location", ...)
tkgrid.propagate       <- function(...) tcl("grid", "propagate", ...)
tkgrid.rowconfigure    <- function(...) tcl("grid", "rowconfigure", ...)
tkgrid.remove          <- function(...) tcl("grid", "remove", ...)
tkgrid.size            <- function(...) tcl("grid", "size", ...)
tkgrid.slaves          <- function(...) tcl("grid", "slaves", ...)

tkpack           <- function(...) tcl("pack", ...)
tkpack.configure <- function(...) tcl("pack", "configure", ...)
tkpack.forget    <- function(...) tcl("pack", "forget", ...)
tkpack.info      <- function(...) tcl("pack", "info", ...)
tkpack.propagate <- function(...) tcl("pack", "propagate", ...)
tkpack.slaves    <- function(...) tcl("pack", "slaves", ...)

tkplace           <- function(...) tcl("place", ...)
tkplace.configure <- function(...) tcl("place", "configure", ...)
tkplace.forget    <- function(...) tcl("place", "forget", ...)
tkplace.info      <- function(...) tcl("place", "info", ...)
tkplace.slaves    <- function(...) tcl("place", "slaves", ...)



### Widgets commands

tkactivate      <- function(widget, ...) tcl(widget, "activate", ...)
tkadd           <- function(widget, ...) tcl(widget, "add", ...)
tkaddtag        <- function(widget, ...) tcl(widget, "addtag", ...)
tkbbox          <- function(widget, ...) tcl(widget, "bbox", ...)
tkcanvasx       <- function(widget, ...) tcl(widget, "canvasx", ...)
tkcanvasy       <- function(widget, ...) tcl(widget, "canvasy", ...)
tkcompare       <- function(widget, ...) tcl(widget, "compare", ...)
tkconfigure     <- function(widget, ...) tcl(widget, "configure", ...)
tkcoords        <- function(widget, ...) tcl(widget, "coords", ...)
tkcreate        <- function(widget, ...) tcl(widget, "create", ...)
tkcget          <- function(widget, ...) tcl(widget, "cget", ...)
tkcoords        <- function(widget, ...) tcl(widget, "coords", ...)
tkcurselection  <- function(widget, ...) tcl(widget, "curselection", ...)
tkdchars        <- function(widget, ...) tcl(widget, "dchars", ...)
tkdebug         <- function(widget, ...) tcl(widget, "debug", ...)
tkdelete        <- function(widget, ...) tcl(widget, "delete", ...)
tkdelta         <- function(widget, ...) tcl(widget, "delta", ...)
tkdeselect      <- function(widget, ...) tcl(widget, "deselect", ...)
tkdlineinfo     <- function(widget, ...) tcl(widget, "dlineinfo", ...)
tkdtag          <- function(widget, ...) tcl(widget, "dtag", ...)
tkdump          <- function(widget, ...) tcl(widget, "dump", ...)
tkentryconfigure<- function(widget, ...) tcl(widget, "entryconfigure", ...)
tkentrycget     <- function(widget, ...) tcl(widget, "entrycget", ...)
tkfind          <- function(widget, ...) tcl(widget, "find", ...)
tkflash         <- function(widget, ...) tcl(widget, "flash", ...)
tkfraction      <- function(widget, ...) tcl(widget, "fraction", ...)
tkget           <- function(widget, ...) tcl(widget, "get", ...)
tkgettags       <- function(widget, ...) tcl(widget, "gettags", ...)
tkicursor       <- function(widget, ...) tcl(widget, "icursor", ...)
tkidentify      <- function(widget, ...) tcl(widget, "identify", ...)
tkindex         <- function(widget, ...) tcl(widget, "index", ...)
tkinsert        <- function(widget, ...) tcl(widget, "insert", ...)
tkinvoke        <- function(widget, ...) tcl(widget, "invoke", ...)
tkitembind      <- function(widget, ...) tcl(widget, "bind", ...)
tkitemcget      <- function(widget, ...) tcl(widget, "itemcget", ...)
tkitemconfigure <- function(widget, ...) tcl(widget, "itemconfigure", ...)
tkitemfocus     <- function(widget, ...) tcl(widget, "focus", ...)
tkitemlower     <- function(widget, ...) tcl(widget, "lower", ...)
tkitemraise     <- function(widget, ...) tcl(widget, "raise", ...)
tkitemscale     <- function(widget, ...) tcl(widget, "scale", ...)
tkmark.gravity  <- function(widget, ...) tcl(widget, "mark", "gravity", ...)
tkmark.names    <- function(widget, ...) tcl(widget, "mark", "names", ...)
tkmark.next     <- function(widget, ...) tcl(widget, "mark", "next", ...)
tkmark.previous <- function(widget, ...) tcl(widget, "mark", "previous", ...)
tkmark.set      <- function(widget, ...) tcl(widget, "mark", "set", ...)
tkmark.unset    <- function(widget, ...) tcl(widget, "mark", "unset", ...)
tkmove          <- function(widget, ...) tcl(widget, "move", ...)
tknearest       <- function(widget, ...) tcl(widget, "nearest", ...)
tkpost          <- function(widget, ...) tcl(widget, "post", ...)
tkpostcascade   <- function(widget, ...) tcl(widget, "postcascade", ...)
tkpostscript    <- function(widget, ...) tcl(widget, "postscript", ...)
tkscan.mark     <- function(widget, ...) tcl(widget, "scan", "mark", ...)
tkscan.dragto   <- function(widget, ...) tcl(widget, "scan", "dragto", ...)
tksearch        <- function(widget, ...) tcl(widget, "search", ...)
tksee           <- function(widget, ...) tcl(widget, "see", ...)
tkselect        <- function(widget, ...) tcl(widget, "select", ...)
tkselection.adjust   <- function(widget, ...)
    tcl(widget, "selection", "adjust", ...)
tkselection.anchor   <- function(widget, ...)
    tcl(widget, "selection", "anchor", ...)
tkselection.clear    <- function(widget, ...)
    tcl(widget, "selection", "clear", ...)
tkselection.from    <- function(widget, ...)
    tcl(widget, "selection", "from", ...)
tkselection.includes <- function(widget, ...)
    tcl(widget, "selection", "includes", ...)
tkselection.present    <- function(widget, ...)
    tcl(widget, "selection", "present", ...)
tkselection.range    <- function(widget, ...)
    tcl(widget, "selection", "range", ...)
tkselection.set      <- function(widget, ...)
    tcl(widget, "selection", "set", ...)
tkselection.to    <- function(widget,...)
    tcl(widget, "selection", "to", ...)
tkset           <- function(widget, ...) tcl(widget, "set", ...)
tksize          <- function(widget, ...) tcl(widget, "size", ...)
tktoggle        <- function(widget, ...) tcl(widget, "toggle", ...)
tktag.add       <- function(widget, ...) tcl(widget, "tag", "add", ...)
tktag.bind      <- function(widget, ...) tcl(widget, "tag", "bind", ...)
tktag.cget      <- function(widget, ...) tcl(widget, "tag", "cget", ...)
tktag.configure <- function(widget, ...) tcl(widget, "tag", "configure", ...)
tktag.delete    <- function(widget, ...) tcl(widget, "tag", "delete", ...)
tktag.lower     <- function(widget, ...) tcl(widget, "tag", "lower", ...)
tktag.names     <- function(widget, ...) tcl(widget, "tag", "names", ...)
tktag.nextrange <- function(widget, ...) tcl(widget, "tag", "nextrange", ...)
tktag.prevrange <- function(widget, ...) tcl(widget, "tag", "prevrange", ...)
tktag.raise     <- function(widget, ...) tcl(widget, "tag", "raise", ...)
tktag.ranges    <- function(widget, ...) tcl(widget, "tag", "ranges", ...)
tktag.remove    <- function(widget, ...) tcl(widget, "tag", "remove", ...)
tktype          <- function(widget, ...) tcl(widget, "type", ...)
tkunpost        <- function(widget, ...) tcl(widget, "unpost", ...)
tkwindow.cget     <-function(widget, ...)tcl(widget, "window", "cget", ...)
tkwindow.configure<-function(widget, ...)tcl(widget,"window","configure",...)
tkwindow.create   <-function(widget, ...)tcl(widget, "window", "create", ...)
tkwindow.names    <-function(widget, ...)tcl(widget, "window", "names", ...)
tkxview         <- function(widget, ...) tcl(widget, "xview", ...)
tkxview.moveto  <- function(widget, ...)tcl(widget, "xview", "moveto", ...)
tkxview.scroll  <-function(widget, ...)tcl(widget, "xview", "scroll", ...)
tkyposition     <- function(widget, ...) tcl(widget, "ypositions", ...)
tkyview         <- function(widget, ...) tcl(widget, "yview", ...)
tkyview.moveto  <- function(widget, ...)tcl(widget, "yview", "moveto", ...)
tkyview.scroll  <- function(widget, ...)tcl(widget, "yview", "scroll", ...)




tkpager <- function(file, header, title, delete.file)
{
    title <- paste(title, header)
    for ( i in seq_along(file) ) {
        zfile <- file[[i]]
        tt <- tktoplevel()
        tkwm.title(tt, if (length(title))
                   title[(i-1L) %% length(title)+1L] else "")
###        courier font comes out awfully small on some systems
###        txt <- tktext(tt, bg="grey90", font="courier")
        txt <- tktext(tt, bg="grey90")
        scr <- tkscrollbar(tt, repeatinterval=5,
                           command=function(...)tkyview(txt,...))
	tkconfigure(txt,yscrollcommand=function(...)tkset(scr,...))
        tkpack(txt, side="left", fill="both", expand=TRUE)
        tkpack(scr, side="right", fill="y")

        chn <- tcl("open", zfile)
        tkinsert(txt, "end", gsub("_\b","",tclvalue(tcl("read", chn))))
        tcl("close", chn)

        tkconfigure(txt, state="disabled")
        tkmark.set(txt,"insert","0.0")
        tkfocus(txt)

        if (delete.file) tcl("file", "delete", zfile)
    }
}



#  File src/library/tcltk/R/tclarray.R
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

tclArray <- function() {
    x <- tclVar()
    tcl("unset", x)
    tcl("array", "set", x, "")
    class(x) <- c(class(x), "tclArray")
    x
}

"[[.tclArray" <- function(x, ...) {
    name <- as.character(x)
    i <- paste(...,sep=",")
    rval <- .External("RTcl_GetArrayElem", name, i, PACKAGE = "tcltk")
    if (!is.null(rval)) class(rval)<-"tclObj"
    rval
}

"[[<-.tclArray" <- function(x, ..., value){
    name <- as.character(x)
    i <- paste(..., sep=",")
    if (is.null(value))
        .External("RTcl_RemoveArrayElem", name, i, PACKAGE = "tcltk")
    else {
        value <- as.tclObj(value)
        .External("RTcl_SetArrayElem", name, i, value, PACKAGE = "tcltk")
    }
    x
}

"$.tclArray" <- function(x, i) {
    name <- as.character(x)
    i <- as.character(i)
    rval <- .External("RTcl_GetArrayElem", name, i, PACKAGE = "tcltk")
    if (!is.null(rval)) class(rval)<-"tclObj"
    rval
}

"$<-.tclArray" <- function(x, i, value){
    name <- as.character(x)
    i <- as.character(i)
    if (is.null(value))
        .External("RTcl_RemoveArrayElem", name, i, PACKAGE = "tcltk")
    else {
        value <- as.tclObj(value)
        .External("RTcl_SetArrayElem", name, i, value, PACKAGE = "tcltk")
    }
    x
}

names.tclArray <- function(x)
    as.character(tcl("array", "names", x))

"names<-.tclArray" <- function(x, value)
    stop("cannot change names on Tcl array")

length.tclArray <- function(x)
    as.integer(tcl("array", "size", x))

"length<-.tclArray" <- function(x, value)
    stop("cannot set length of Tcl array")



#  File src/library/tcltk/R/tclsearch.R
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


addTclPath <- function(path = ".")
{
    ## Tcl uses Unix-style paths on Windows
    if(.Platform$OS.type == "windows")
        path <- gsub("\\\\", "/", path)
    a <- tclvalue(tcl("set", "auto_path"))
    paths <- strsplit(a, " ", fixed=TRUE)[[1L]]
    if (! path %in% paths)
        tcl("lappend", "auto_path", path)
    invisible(paths)
}

tclRequire <- function(package, warn = TRUE)
{
    a <- tryCatch(tcl("package", "require", package), error = identity)
    if (inherits(a, "error")) {
        if (warn)
            warning(gettextf("Tcl package '%s' not found", package),
                    domain = NA)
        return(FALSE)
    } else return(a)
}
#  File src/library/tcltk/R/tcltk-defunct.R
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

## deprecated at r16598, 2001-11-04
"$.tclvar" <- function(x, name)
	.Defunct("tclVar and tclvalue")

"$<-.tclvar" <- function(x, name, value)
    .Defunct("tclVar and tclvalue<-")

## deprecated in R 2.3.0
tkcmd <- function(...)
    .Defunct("tcl")

tkfile.tail <- function(...)
    .Defunct("tclfile.tail")

tkfile.dir <- function(...)
    .Defunct("tclfile.dir")

tkopen <- function(...)
    .Defunct("tclopen")

tkclose <- function(...)
    .Defunct("tclclose")

tkputs <- function(...)
    .Defunct("tclputs")

tkread <- function(...)
    .Defunct("tclread")

#  File src/library/tcltk/R/tkGUI.R
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

tkStartGUI <- function() {
    ## Philippe Grosjean: this is added for more explicit error message under Windows
    if (.Platform$OS.type == "windows")
    	stop("The tkGUI is not available under Windows")
    tclServiceMode(FALSE)
    tcl("source", file.path(.Library, "tcltk", "exec", "console.tcl"))
    .C("RTcl_ActivateConsole", PACKAGE = "tcltk")
    Menu <- .Tk.newwin(".menu")
    Term <- .Tk.newwin(".tk-R.term")
    Toolbar <- .Tk.newwin(".tk-R.toolbar")
    options(pager=tkpager)

    fileMenu <- tkmenu(Menu)
    demoMenu <- tkmenu(Menu)
    packageMenu <- tkmenu(Menu)
    helpMenu <- tkmenu(Menu)
    quitMenu <- tkmenu(fileMenu)

    tkadd(Menu,"cascade",label=gettext("File"),menu=fileMenu)
    tkadd(Menu,"cascade",label=gettext("Demos"),menu=demoMenu)
    tkadd(Menu,"cascade",label=gettext("Packages"),menu=packageMenu)
    tkadd(Menu,"cascade",label=gettext("Help"),menu=helpMenu)

    tkadd(fileMenu,"command",label=gettext("Source R code"),
	  command=function(){f <- as.character(tkgetOpenFile())
                             if (length(f)) source(f)})
    tkadd(fileMenu,"cascade", label=gettext("Quit"), menu=quitMenu)

    tkadd(quitMenu,"command",label=gettext("Save workspace"),
          command=quote(q("yes")))
    tkadd(quitMenu,"command",label=gettext("Don't save workspace"),
          command=quote(q("no")))

    tkadd(demoMenu,"command",label=gettext("t test"),
          command=quote(demo(tkttest)))
    tkadd(demoMenu,"command",label=gettext("Density"),
          command=quote(demo(tkdensity)))
    tkadd(demoMenu,"command",label=gettext("Interactive linear fitting"),
          command=quote(demo(tkcanvas)))
    tkadd(demoMenu,"command",label=gettext("R FAQ"),
          command=quote(demo(tkfaq)))

    loadpackageWidget <- function()
    {
	pkglist <- .packages(all.available=TRUE)
        lvar <- tclVar()
	tclObj(lvar) <- pkglist
	box <- tklistbox(tt<-tktoplevel(),
		listvariable=lvar, selectmode="multiple")
	load <- function() {
	   s <- as.integer(tkcurselection(box))
	   if (length(s) == 0) return
	   lapply(pkglist[s+1],require,character.only=TRUE)
	   tkdestroy(tt)
	}
	tkpack(box)
	tkpack(tkbutton(tt,text=gettext("Load"),command=load))
    }

    CRANpackageWidget <- function()
    {
        CRANurl <- utils::contrib.url(getOption("repos")["CRAN"])
	l <- utils::available.packages(CRANurl)[,1]
        lvar <- tclVar()
	tclObj(lvar) <- l
	box <- tklistbox(tt<-tktoplevel(),
                         listvariable=lvar, selectmode="multiple")
	gogetem <- function() {
            s <- as.integer(tkcurselection(box))
            if (length(s) == 0) return
            utils::install.packages(l[s+1])
            tkdestroy(tt)
	}
	tkpack(box)
	tkpack(tkbutton(tt,text=gettext("Go get them!"),command=gogetem))
    }

    tkadd(packageMenu,"command",label=gettext("Load packages"),
          command=loadpackageWidget)
    tkadd(packageMenu,"command",label=gettext("Install packages from CRAN"),
          command=CRANpackageWidget)


    local({
        label <- tklabel(Toolbar,text=gettext("Help topic:"))
        txtvar <- tclVar()
        entry <- tkentry(Toolbar,textvariable=txtvar)
        showhelp <-  function() {
            s <- as.character(tclObj(txtvar))[1L]
            if (length(s) == 0) return
            nm <- as.name(s)
            print(eval(substitute(help(nm))))
            tclvalue(txtvar)<-""
        }
        tkpack(label,side="left")
        tkpack(entry,side="left")
        tkbind(entry, "<Return>", showhelp)
    })

    manuals <- matrix(c(
	"R-FAQ",     "Frequently asked questions",
	"R-intro",   "An Introduction to R",
	"R-admin",   "R Administrators Manual",
	"R-data",    "R Data Import/Export",
	"R-exts",    "Writing R extensions",
	"R-lang",    "R Language Reference",
	"refman",    "R Reference Manual"
    ), ncol=2, byrow=TRUE)

    helpPDFMenu <- tkmenu(helpMenu)
    tkadd(helpMenu,"cascade", label=gettext("Manuals in PDF format"),
          menu=helpPDFMenu)
    pdfBase <- file.path(R.home("doc"), "manual")
    apply(manuals, 1L, function(x) {
	f <- file.path(pdfBase, paste(x[1L], ".pdf", sep="") )
        cmd <- function() system(paste(shQuote(getOption("pdfviewer")),
                                       shQuote(f)),
                                 wait = FALSE)
	tkadd(helpPDFMenu, "command", label=x[2L], command=cmd,
              state=if (file.exists(f)) "normal" else "disabled")
    })
    #tkadd(helpMenu,"command", label=gettext("Help on topic..."), command=topicHelp)
    assign(".GUIenv", environment(), envir=.GlobalEnv)
    invisible(tclServiceMode(TRUE))
}
#  File src/library/tcltk/R/unix/zzz.R
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

.onLoad <- function(lib, pkg)
{
    ## This will get interrupted if there is no display,
    ## so we choose to have the space here.
    packageStartupMessage("Loading Tcl/Tk interface ...", " ",
                          domain = "R-tcltk", appendLF = FALSE)

    ## Use local = FALSE to allow easy loading of Tcl extensions
    library.dynam("tcltk", pkg, lib, local = FALSE)
    .C("tcltk_init", PACKAGE="tcltk")
    addTclPath(system.file("exec", package = "tcltk"))
    packageStartupMessage("done", domain = "R-tcltk")
}

.onUnload <- function(libpath) {
    ## precaution in case the DLL has been unloaded without the namespace
    if(is.loaded("delTcl", PACKAGE="tcltk")) {
        .C("delTcl", PACKAGE="tcltk")
        ## if we unload the DLL, get a segfault if we try to use tcltk again.
        ## library.dynam.unload("tcltk", libpath)
    }
}
#  File src/library/tcltk/R/utils.R
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

tk_select.list <-
    function(list, preselect = NULL, multiple = FALSE, title = NULL)
{
    have_ttk <- as.character(tcl("info", "tclversion")) >= "8.5"
    if(!have_ttk) ttkbutton <- tkbutton
    lvar <- tclVar()
    tclObj(lvar) <- list
    oldmode <- tclServiceMode(FALSE)
    dlg <- tktoplevel()
    tkwm.title(dlg, title)
    tkwm.deiconify(dlg)
    tkgrab.set(dlg)
    tkfocus(dlg)
    if(!is.null(title) && nzchar(title)) {
        lab <- if(have_ttk) ttklabel(dlg, text = title, foreground = "blue")
        else tklabel(dlg, text = title, fg = "blue")
        tkpack(lab, side="top")
    }
    onOK <- function() {
        res <- 1L + as.integer(tkcurselection(box))
        ans.select_list <<- list[res]
        tkgrab.release(dlg)
        tkdestroy(dlg)
    }
    onCancel <- function() {
        tkgrab.release(dlg)
        tkdestroy(dlg)
    }
    buttons <- tkframe(dlg)
    tkpack(buttons, side="bottom")
    OK <- ttkbutton(buttons, text = gettext("OK"), width = 6, command = onOK)
    Cancel <- ttkbutton(buttons, text = gettext("Cancel"), command = onCancel)
    tkpack(OK, Cancel, side="left", fill="x", padx="2m")

    scht <- as.numeric(tclvalue(tkwinfo("screenheight", dlg))) - 200L
    ## allow for win furniture and buttons, and for e.g. KDE panel
    ht <- min(length(list), scht %/% 20) # a guess of font height
    box <- tklistbox(dlg, height = ht,
                     listvariable = lvar, bg = "white", setgrid = 1,
                     selectmode = ifelse(multiple, "multiple", "single"))
    tmp <- tcl("font", "metrics", tkcget(box, font=NULL))
    ## fudge factor here seems to be 1 on Windows, 3 on X11.
    tmp <- as.numeric(sub(".*linespace ([0-9]+) .*", "\\1", tclvalue(tmp)))+3
    ht <- min(length(list), scht %/% tmp)
    tkdestroy(box)
    if(ht < length(list)) {
        scr <- if(have_ttk) ttkscrollbar(dlg, command = function(...) tkyview(box, ...))
        else tkscrollbar(dlg, repeatinterval=5, command = function(...) tkyview(box, ...))
        box <- tklistbox(dlg, height = ht, width = 0,
                         listvariable = lvar, bg = "white", setgrid = 1,
                         selectmode = ifelse(multiple, "multiple", "single"),
                         yscrollcommand = function(...)tkset(scr,...))
        tkpack(box, side="left", fill="both", expand=TRUE)
        tkpack(scr, side="right", fill="y")
    } else {
        box <- tklistbox(dlg, height = ht, width = 0,
                         listvariable = lvar, bg = "white",
                         selectmode = ifelse(multiple, "multiple", "single"))
        tkpack(box, side="left", fill="both")
    }
    preselect <- match(preselect, list)
    ans.select_list <- character() # avoid name conflicts
    for(i in preselect[preselect > 0L])
        tkselection.set(box, i - 1L) # 0-based

    tkbind(dlg, "<Destroy>", onCancel)
    tkbind(dlg, "<Double-ButtonPress-1>", onOK)
    tkfocus(box)
    tclServiceMode(oldmode)
    tkwait.window(dlg)
    Sys.sleep(0.1) # allow time for window to be removed.
    if(!multiple && !length(ans.select_list)) ans.select_list <- ""
    ans.select_list
}

tkProgressBar <- function(title = "R progress bar", label = "",
                          min = 0, max = 1, initial = 0, width = 300)
{
    useText <- FALSE
    have_ttk <- as.character(tcl("info", "tclversion")) >= "8.5"
    if(!have_ttk && as.character(tclRequire("PBar")) == "FALSE") useText <- TRUE


    .win <- tktoplevel()
    .val <- initial
    .killed <- FALSE

    tkwm.geometry(.win, sprintf("%dx80", width+40))
    tkwm.title(.win, title)
    fn <- tkfont.create(family="helvetica", size=12)

    if(useText) {
        ## currently unused
        .lab <- tklabel(.win, text=label, font=fn, padx=20)
        tkpack(.lab, side = "left")
        fn2 <- tkfont.create(family="helvetica", size=16)
       .vlab <- tklabel(.win, text="0%", font=fn2, padx=20)
        tkpack(.vlab, side = "right")
        up <- function(value) {
            if(!is.finite(value) || value < min || value > max) return()
            .val <<- value
             tkconfigure(.vlab,
                         text=sprintf("%d%%",
                         round(100*(value - min)/(max - min))))
        }
    } else {
        .lab <- tklabel(.win, text=label, font=fn, pady=10)
       .tkval <- tclVar(0)
        tkpack(.lab, side="top")
        tkpack(tklabel(.win, text="", font = fn), side="bottom")

        pBar <- if(have_ttk) ttkprogressbar(.win, length=width, variable=.tkval) else tkwidget(.win, "ProgressBar", width=width, variable=.tkval)
        tkpack(pBar, side="bottom")
        up <- function(value) {
            if(!is.finite(value) || value < min || value > max) return()
            .val <<- value
            tclvalue(.tkval) <<- 100*(value - min)/(max - min)
        }
    }
    getVal <- function() .val
    kill <- function() if(!.killed) {tkdestroy(.win); .killed <<- TRUE}
    title <- function(title) tkwm.title(.win, title)
    lab <- function(label) tkconfigure(.lab, text=label)
    tkbind(.win, "<Destroy>", kill)
    up(initial)

    structure(list(getVal=getVal, up=up, title=title, label=lab, kill=kill),
              class = "tkProgressBar")
}

getTkProgressBar <- function(pb)
{
    if(!inherits(pb, "tkProgressBar"))
       stop("'pb' is not from class \"tkProgressBar\"")
    pb$getVal()
}

setTkProgressBar <- function(pb, value, title = NULL, label = NULL)
{
    if(!inherits(pb, "tkProgressBar"))
       stop("'pb' is not from class \"tkProgressBar\"")
    oldval <- pb$getVal()
    pb$up(value)
    if(!is.null(title)) pb$title(title)
    if(!is.null(label)) pb$label(label)
    tcl("update", "idletasks")
    invisible(oldval)
}

close.tkProgressBar <- function(con, ...)
{
    con$kill()
    invisible(NULL)
}

tk_choose.files <-
    function(default = '', caption = 'Select files', multi = TRUE,
             filters = NULL, index = 1)
{
    args <- list("tk_getOpenFile", title = caption, multiple = multi)
    if(nzchar(default)) args <- c(args, initialdir = dirname(default),
                                   initialfile = basename(default))
    if(!is.null(filters)) {
        if(!is.character(filters) || length(dim(filters)) != 2 || ncol(filters) != 2)
            stop("'filters' must be a 2-column character matrix")
        f <- filters
        f[] <- paste("{", filters, "}", sep="")
        ff <- apply(f, 1, paste, collapse = " ")
        fff <- paste("{", ff, "}", sep="")
        args <- c(args, filetypes = paste(fff, collapse = " "))
    }
    res <- tclvalue(do.call(tcl, args))
    if(nzchar(res))
        if(multi) {
            ## Filenames with spaces will be surrounded by { }
            ans <- character()
            pat <- "([^{])*\\{([^}]*)\\}(.*)"
            while(grepl(pat, res)) {
                ans <- c(ans, sub(pat, "\\2", res))
                res <- sub(pat, "\\1\\3", res)
            }
            ans <- c(ans, strsplit(res, " ", fixed = TRUE)[[1]])
            ans[nzchar(ans)]
        } else res
    else character()
}


tk_choose.dir <- function(default = '', caption = 'Select directory')
{
    res <- tclvalue(tcl("tk_chooseDirectory", initialdir = default, title = caption))
    if(nzchar(res)) res else NA_character_
}

tk_messageBox <-
    function(type = c("ok", "okcancel", "yesno", "yesnocancel",
                      "retrycancel", "aburtretrycancel"),
             message, caption = "", default = "", ...)
{
    type <- match.arg(type)
    args <- list("tk_messageBox", type=type, message=message,
                 title=caption, ...)
    if(nzchar(default)) args <- c(args, default=default)
    tclvalue(do.call("tcl", args))
}
