% File src/library/base/man/Primitive.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{Primitive}
\alias{.Primitive}
\alias{primitive}
\title{Call a ``Primitive'' Internal Function}
\description{
  \code{.Primitive} returns an entry point to a \sQuote{primitive}
  (internally implemented) function.
}
\usage{
.Primitive(name)
}
\arguments{
  \item{name}{name of the \R function.}
}
\details{
  The advantage of \code{.Primitive} over \code{\link{.Internal}}
  functions is the potential efficiency of argument passing.  However,
  this is done by ignoring argument names and using positional matching
  of arguments (unless arranged differently for specific primitives such
  as \code{\link{log}}, \code{\link{round}}, \code{\link{signif}},
  \code{\link{rep}} and \code{\link{seq.int}}), so this is discouraged
  for functions of more than one argument.

  All primitive functions are in the base name space.
}
\seealso{
  \code{\link{.Internal}}.
}
\examples{
mysqrt <- .Primitive("sqrt")
c
.Internal # this one *must* be primitive!
get("if") # just 'if' or 'print(if)' are not syntactically ok.
}
\keyword{interface}
