% File src/library/base/man/duplicated.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Development Team
% Distributed under GPL 2 or later

\name{duplicated}
\title{Determine Duplicate Elements}
\alias{duplicated}
\alias{duplicated.default}
\alias{duplicated.data.frame}
\alias{duplicated.matrix}
\alias{duplicated.array}
\alias{anyDuplicated}
\alias{anyDuplicated.default}
\alias{anyDuplicated.array}
\alias{anyDuplicated.matrix}
\alias{anyDuplicated.data.frame}
\description{
  Determines which elements of a vector or data frame are duplicates
  of elements with smaller subscripts, and returns a logical vector
  indicating which elements (rows) are duplicates.
}
\usage{
duplicated(x, incomparables = FALSE, \dots)

\method{duplicated}{default}(x, incomparables = FALSE,
           fromLast = FALSE, \dots)

\method{duplicated}{array}(x, incomparables = FALSE, MARGIN = 1,
           fromLast = FALSE, \dots)

anyDuplicated(x, incomparables = FALSE, \dots)
\method{anyDuplicated}{default}(x, incomparables = FALSE,
           fromLast = FALSE, \dots)
\method{anyDuplicated}{array}(x, incomparables = FALSE,
           MARGIN = 1, fromLast = FALSE, \dots)
}
\arguments{
  \item{x}{a vector or a data frame or an array or \code{NULL}.}
  \item{incomparables}{a vector of values that cannot be compared.
    \code{FALSE} is a special value, meaning that all values can be
    compared, and may be the only value accepted for methods other than
    the default.  It will be coerced internally to the same type as
       \code{x}.}
  \item{fromLast}{logical indicating if duplication should be considered
    from the reverse side, i.e., the last (or rightmost) of identical
    elements would correspond to \code{duplicated=FALSE}.}
  \item{\dots}{arguments for particular methods.}
  \item{MARGIN}{the array margin to be held fixed: see
    \code{\link{apply}}.}
}
\details{
  These are generic functions with methods for vectors (including
  lists), data frames and arrays (including matrices).

  For the default methods, and whenever there are equivalent method
  definitions for \code{duplicated} and \code{anyDuplicated},
  \code{anyDuplicated(x,...)} is a \dQuote{generalized} shortcut for
  \code{any(duplicated(x,...))}, in the sense that it returns the
  \emph{index} \code{i} of the first duplicated entry \code{x[i]} if
  there is one, and \code{0} otherwise.  Their behaviours may be
  different when at least one of \code{duplicated} and
  \code{anyDuplicated} has a relevant method.

  \code{duplicated(x, fromLast=TRUE)} is equivalent to but faster than
  \code{rev(duplicated(rev(x)))}.

  The data frame method works by pasting together a character
  representation of the rows separated by \code{\\r}, so may be imperfect
  if the data frame has characters with embedded carriage returns or
  columns which do not reliably map to characters.

  The array method calculates for each element of the sub-array
  specified by \code{MARGIN} if the remaining dimensions are identical
  to those for an earlier (or later, when \code{fromLast=TRUE}) element
  (in row-major order).  This would most commonly be used to find
  duplicated rows (the default) or columns (with \code{MARGIN = 2}).

  Missing values are regarded as equal, but \code{NaN} is not equal to
  \code{NA_real_}.

  Values in \code{incomparables} will never be marked as duplicated.
  This is intended to be used for a fairly small set of values and will
  not be efficient for a very large set.
}
\value{
    \code{duplicated()}:
    For a vector input, a logical vector of the same length as
    \code{x}.  For a data frame, a logical vector with one element for
    each row.  For a matrix or array, a logical array with the same
    dimensions and dimnames.

    \code{anyDuplicated()}: a non-negative integer (of length one).
}
\section{Warning}{
  Using this for lists is potentially slow, especially if the elements
  are not atomic vectors (see \code{\link{vector}}) or differ only
  in their attributes.  In the worst case it is \eqn{O(n^2)}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{\code{\link{unique}}.}
\examples{
x <- c(9:20, 1:5, 3:7, 0:8)
## extract unique elements
(xu <- x[!duplicated(x)])
## similar, but not the same:
(xu2 <- x[!duplicated(x, fromLast = TRUE)])

## xu == unique(x) but unique(x) is more efficient
stopifnot(identical(xu,  unique(x)),
          identical(xu2, unique(x, fromLast = TRUE)))

duplicated(iris)[140:143]

duplicated(iris3, MARGIN = c(1, 3))
anyDuplicated(iris) ## 143
\dontshow{% array & data.frame methods:
stopifnot(identical(anyDuplicated(iris), 143L),
          identical(anyDuplicated(iris3, MARGIN = c(1, 3)), 143L))
}
anyDuplicated(x)
anyDuplicated(x, fromLast = TRUE)
}
\keyword{logic}
\keyword{manip}
