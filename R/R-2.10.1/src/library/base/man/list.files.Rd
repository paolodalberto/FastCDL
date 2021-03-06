% File src/library/base/man/list.files.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Development Team
% Distributed under GPL 2 or later

\name{list.files}
\alias{list.files}
\alias{dir}
\title{List the Files in a Directory/Folder}
\usage{
list.files(path = ".", pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE)

       dir(path = ".", pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE)
}
\arguments{
  \item{path}{a character vector of full path names; the default
    corresponds to the working directory \code{\link{getwd}()}.  Missing
    values will be ignored.}
  \item{pattern}{an optional \link{regular expression}.  Only file names
    which match the regular expression will be returned.}
  \item{all.files}{a logical value. If \code{FALSE}, only the
    names of visible files are returned.  If \code{TRUE},
    all file names will be returned.}
  \item{full.names}{a logical value.  If \code{TRUE}, the directory
    path is prepended to the file names.  If \code{FALSE}, only
    the file names are returned.}
  \item{recursive}{logical. Should the listing recurse into directories?}
  \item{ignore.case}{logical. Should pattern-matching be case-insensitive?}
}
\description{
  These functions produce a character vector of the names of files
  in the named directory.
}
\value{
  A character vector containing the names of the files in the
  specified directories, or \code{""} if there were no files. If a
  path does not exist or is not a directory or is unreadable it
  is skipped, with a warning.

  The files are sorted in alphabetical order, on the full path
  if \code{full.names = TRUE}.  Directories are included only if
  \code{recursive = FALSE}.
}
\author{Ross Ihaka, Brian Ripley}
\note{
  File naming conventions are platform dependent.

  \code{recursive = TRUE} is not supported on all platforms and may be
  ignored (with a warning).
}
\seealso{\code{\link{file.info}}, \code{\link{file.access}}
  and \code{\link{files}} for many more file handling functions and 
  \code{\link{file.choose}} 
#ifdef windows
and \code{\link{choose.files}}
#endif
for interactive selection.

  \code{\link{glob2rx}} to convert wildcards (as used by system file
  commands and shells) to regular expressions.

  \code{\link{Sys.glob}} for wildcard expansion on file paths.
}
\examples{
list.files(R.home())
## Only files starting with a-l or r
## Note that a-l is locale-dependent, but using case-insensitive
## matching makes it unambiguous in English locales
dir("../..", pattern = "^[a-lr]",full.names=TRUE, ignore.case = TRUE)
}
\keyword{file}
