% File src/library/base/man/load.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{load}
\alias{load}
\title{Reload Saved Datasets}
\description{
  Reload datasets written with the function \code{save}.
}
\usage{
load(file, envir = parent.frame())
}
\arguments{
  \item{file}{a (readable binary) connection or a character string
    giving the name of the file to load.}
  \item{envir}{the environment where the data should be loaded.}
}
\details{
  \code{load} can load \R objects saved in the current or any earlier
  format.  It can read a compressed file (see \code{\link{save}})
  directly from a file or from a suitable connection (including a call
  to \code{\link{url}}).
  
  A not-open connection will be opened in mode \code{"rb"} and closed
  after use. 
  
  Only \R objects saved in the current format (used since \R 1.4.0)
  can be read from a connection.  If no input is available on a
  connection a warning will be given, but any input not in the current
  format will result in a error.
  
  Loading from an earlier version will give a warning about the
  \sQuote{magic number}: magic numbers \code{1971:1977} are from \R <
  0.99.0, and \code{R[ADX]1} from \R 0.99.0 to \R 1.3.1.
}
\value{
  A character vector of the names of objects created, invisibly.
}
\section{Warning}{
  Saved \R objects are binary files, even those saved with
  \code{ascii = TRUE}, so ensure that they are transferred without
  conversion of end of line markers.  \code{load} tries to detect this
  case and give an informative error message.
}
\seealso{
  \code{\link{save}}, \code{\link{download.file}}.
}
\examples{
## save all data
xx <- pi # to ensure there is some data
save(list = ls(all=TRUE), file= "all.Rdata")
rm(xx)

## restore the saved values to the current environment
local({
   load("all.Rdata")
   ls()
})
## restore the saved values to the user's workspace
load("all.Rdata", .GlobalEnv)

unlink("all.Rdata")

\dontrun{
con <- url("http://some.where.net/R/data/example.rda")
## print the value to see what objects were created.
print(load(con))
close(con) # url() always opens the connection
}}
\keyword{file}
