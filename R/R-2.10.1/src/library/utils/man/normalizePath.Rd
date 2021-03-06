% File src/library/utils/man/normalizePath.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{normalizePath}
\alias{normalizePath}
\title{Express File Paths in Canonical Form}
\description{
  Convert file paths to canonical form for the platform, to display them
  in a user-understandable form.
}
\usage{
normalizePath(path)
}
\arguments{
  \item{path}{character vector of file paths.}
}
\details{
#ifdef unix
  Where the platform supports it this turns paths into absolute paths
  in their canonical form (no \samp{./}, \samp{../} nor symbolic links).
#endif
#ifdef windows
  This converts relative paths to absolute paths, and converts short
  names to long names.  It will always use backslashes as the path separator.
#endif

  If the path is not a real path the result is undefined.
On Unix-alikes, this will likely be the corresponding input element.
On Windows, it will likely result in an error being signalled.
}
\value{
  A character vector.
}
#ifdef windows
\seealso{
  \code{\link{shortPathName}}
}
#endif
\examples{
cat(normalizePath(c(R.home(), tempdir())), sep = "\n")
}
\keyword{ utilities }
