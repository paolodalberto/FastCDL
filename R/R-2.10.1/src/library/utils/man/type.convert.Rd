% File src/library/utils/man/type.convert.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Development Team
% Distributed under GPL 2 or later

\name{type.convert}
\alias{type.convert}
\title{Type Conversion on Character Variables}
\description{
  Convert a character vector to logical, integer, numeric, complex or
  factor as appropriate.
}
\usage{
type.convert(x, na.strings = "NA", as.is = FALSE, dec = ".")
}
\arguments{
  \item{x}{a character vector.}

  \item{na.strings}{a vector of strings which are to be interpreted as
    \code{\link{NA}} values.  Blank fields are also considered to be
    missing values in logical, integer, numeric or complex vectors.}

  \item{as.is}{logical.  See \sQuote{Details}.}

  \item{dec}{the character to be assumed for decimal points.}
}
\details{
  This is principally a helper function for \code{\link{read.table}}.
  Given a character vector, it attempts to convert it to logical,
  integer, numeric or complex, and failing that converts it to factor
  unless \code{as.is = TRUE}.  The first type that can accept all the
  non-missing values is chosen.

  Vectors which are entirely missing values are converted to logical,
  since \code{NA} is primarily logical.

  Since this is a helper function, the caller should always pass an
  appropriate value of \code{as.is}.
}
\value{
  A vector of the selected class, or a factor.
}

\seealso{\code{\link{read.table}}}

\keyword{manip}
