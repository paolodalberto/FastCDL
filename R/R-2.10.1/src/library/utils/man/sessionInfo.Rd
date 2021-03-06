% File src/library/utils/man/sessionInfo.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{sessionInfo}
\title{Collect Information About the Current R Session}
\alias{sessionInfo}
\alias{toLatex.sessionInfo}
\alias{print.sessionInfo}
\description{
  Print version information about \R and attached or loaded packages.
}
\usage{
sessionInfo(package=NULL)
\S3method{print}{sessionInfo}(x, locale=TRUE, ...)
\S3method{toLatex}{sessionInfo}(object, locale=TRUE, ...)
}
\arguments{
 \item{package}{a character vector naming installed packages.  By
   default all attached packages are used.}
  \item{x}{an object of class \code{"sessionInfo"}.}
  \item{object}{an object of class \code{"sessionInfo"}.}
  \item{locale}{show locale information?}
  \item{\dots}{currently not used.}
}
\seealso{\code{\link{R.version}}
}
\examples{
sessionInfo()
toLatex(sessionInfo(), locale=FALSE)
}
\keyword{misc}
