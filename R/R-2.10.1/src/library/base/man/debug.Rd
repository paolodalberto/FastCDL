% File src/library/base/man/debug.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{debug}
\title{Debug a Function}
\usage{
debug(fun, text="", condition=NULL)
debugonce(fun, text="", condition=NULL)
undebug(fun)
isdebugged(fun)
}
\alias{debug}
\alias{debugonce}
\alias{undebug}
\alias{isdebugged}
\arguments{
\item{fun}{any interpreted \R function.}
\item{text}{a text string that can be retrieved when the browser is entered.}
\item{condition}{a condition that can be retrieved when the browser is entered.}
}
\description{
  Set, unset or query the debugging flag on a function.
 The \code{text} and \code{condition} arguments are the same as those
 that can be supplied via a call to \code{browser}. They can be retrieved
 by the user once the browser has been entered, and provide a mechanism to
 allow users to identify which breakpoint has been activated.
}
\details{
  When a function flagged for debugging is entered, normal execution
  is suspended and the body of function is executed one statement at a
  time.  A new browser context is initiated for each step (and the
  previous one destroyed).

  At the debug prompt the user can enter commands or \R expressions.
  The commands are

  \describe{
    \item{\code{n}}{(or just return).  Advance to the next step.}
    \item{\code{c}}{continue to the end of the current context: e.g. to the
      end of the loop if within a loop or to the end of the function.}
    \item{\code{cont}}{synonym for \code{c}.}
    \item{\code{where}}{print a stack trace of all active function calls.}
    \item{\code{Q}}{exit the browser and the current evaluation and
      return to the top-level prompt.}
  }
  (Leading and trailing whitespace is ignored, except for return).

  Anything else entered at the debug prompt is interpreted as an
  \R expression to be evaluated in the calling environment: in
  particular typing an object name will cause the object to be printed,
  and \code{ls()} lists the objects in the calling frame.  (If you want
  to look at an object with a name such as \code{n}, print it explicitly.)

  If a function is defined inside a function, single-step though to the
  end of its definition, and then call \code{debug} on its name.

  Using \code{debug} is persistent, and unless debugging is turned off
  the debugger will be entered on every invocation (note that if the
  function is removed and replaced the debug state is not preserved).
  Use \code{debugonce} to enter the debugger only the next time the
  function is invoked.

  In order to debug S4 methods (see \code{\link{Methods}}), you
  need to use \code{\link{trace}}, typically calling \code{\link{browser}},
  e.g., as \cr
  \code{trace("plot", browser, exit=browser, signature = c("track", "missing"))}
}
\seealso{
  \code{\link{browser}}, \code{\link{trace}};
  \code{\link{traceback}} to see the stack after an \code{Error: \dots}
  message; \code{\link{recover}} for another debugging approach.
}
\keyword{programming}
\keyword{environment}
