% File src/library/base/man/eapply.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{eapply}
\title{Apply a Function Over Values in an Environment}
\usage{
eapply(env, FUN, \dots, all.names = FALSE, USE.NAMES = TRUE)
}
\alias{eapply}
\arguments{
  \item{env}{environment to be used.}
  \item{FUN}{the function to be applied, found \emph{via}
    \code{\link{match.fun}}.
    In the case of functions like \code{+}, \code{\%*\%}, etc., the
    function name must be backquoted or quoted.}
  \item{\dots}{optional arguments to \code{FUN}.}
  \item{all.names}{a logical indicating whether to apply the function to
    all values.}
  \item{USE.NAMES}{logical indicating whether the resulting list should
    have \code{\link{names}}.  Setting this to \code{FALSE} is faster,
    useful however typically only when \code{\link{ls}(env)} is already
    given.}
}
\description{
  \code{eapply} applies \code{FUN} to the named values from an
  \code{\link{environment}} and returns the results as a list.  The user
  can request that all named objects are used (normally names that begin with a
  dot are not). The output is not sorted and no parent environments are
  searched.
}
\seealso{
  \code{\link{environment}}, \code{\link{lapply}}.
}
\examples{
require(utils); require(stats)

env <- new.env()
env$a <- 1:10
env$beta <- exp(-3:3)
env$logic <- c(TRUE,FALSE,FALSE,TRUE)
# what have we there?
eapply(env, str)
## note however, that  'ls.str(env)' is slightly nicer ..

# compute the mean for each list element
       eapply(env, mean)
unlist(eapply(env, mean, USE.NAMES = FALSE))

# median and quartiles for each element (making use of "..." passing):
eapply(env, quantile, probs = 1:3/4)
eapply(env, quantile)
}
\keyword{iteration}
\keyword{environment}
\keyword{list}
