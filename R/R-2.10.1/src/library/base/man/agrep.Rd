% File src/library/base/man/agrep.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{agrep}
\alias{agrep}
\alias{fuzzy matching}
\title{Approximate String Matching (Fuzzy Matching)}
\description{
  Searches for approximate matches to \code{pattern} (the first argument)
  within the string \code{x} (the second argument) using the Levenshtein
  edit distance.
}
\usage{
agrep(pattern, x, ignore.case = FALSE, value = FALSE,
      max.distance = 0.1, useBytes = FALSE)
}
\arguments{
  \item{pattern}{a non-empty character string to be matched (\emph{not}
    a regular expression!).  Coerced by \code{as.character} to a string
    if possible.}
  \item{x}{character vector where matches are sought.  Coerced by
    \code{as.character} to a character vector if possible.}
  \item{ignore.case}{if \code{FALSE}, the pattern matching is \emph{case
      sensitive} and if \code{TRUE}, case is ignored during matching.}
  \item{value}{if \code{FALSE}, a vector containing the (integer)
    indices of the matches determined is returned and if \code{TRUE}, a
    vector containing the matching elements themselves is returned.}
  \item{max.distance}{Maximum distance allowed for a match.  Expressed
    either as integer, or as a fraction of the \emph{pattern} length (will be
    replaced by the smallest integer not less than the corresponding
    fraction of the pattern length), or a list with possible components
    \describe{
      \item{\code{all}:}{maximal (overall) distance}
      \item{\code{insertions}:}{maximum number/fraction of insertions}
      \item{\code{deletions}:}{maximum number/fraction of deletions}
      \item{\code{substitutions}:}{maximum number/fraction of
        substitutions}
    }
    If \code{all} is missing, it is set to 10\%, the other components
    default to \code{all}.  The component names can be abbreviated.
  }
  \item{useBytes}{logical. in a multibyte locale, should the comparison
    be character-by-character (the default) or byte-by-byte.}
}
\details{
  The Levenshtein edit distance is used as measure of approximateness:
  it is the total number of insertions, deletions and substitutions
  required to transform one string into another.

  As from \R 2.10.0 this uses \code{tre} by Ville Laurikari
  (\url{http://http://laurikari.net/tre/}), which supports MBCS
  character matching much better than the previous version.
}
\value{
  Either a vector giving the indices of the elements that yielded a
  match, or, if \code{value} is \code{TRUE}, the matched elements (after
  coercion, preserving names but no other attributes).
}
\author{
  Original version by David Meyer. Current version by Brian Ripley.
}
\seealso{
  \code{\link{grep}}
}
\examples{
agrep("lasy", "1 lazy 2")
agrep("lasy", c(" 1 lazy 2", "1 lasy 2"), max = list(sub = 0))
agrep("laysy", c("1 lazy", "1", "1 LAZY"), max = 2)
agrep("laysy", c("1 lazy", "1", "1 LAZY"), max = 2, value = TRUE)
agrep("laysy", c("1 lazy", "1", "1 LAZY"), max = 2, ignore.case = TRUE)
}
\keyword{character}
