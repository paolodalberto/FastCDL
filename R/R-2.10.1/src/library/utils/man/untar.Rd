% File src/library/utils/man/untar.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009 R Core Development Team
% Distributed under GPL 2 or later

\name{untar}
\alias{untar}
\title{
  Extract or List Tar Archives
}
\description{
  Extract files from or list a tar archive.
}
\usage{
untar(tarfile, files = NULL, list = FALSE, exdir = ".",
      compressed = NA, extras = NULL, verbose = FALSE,
      tar = Sys.getenv("TAR"))
}
\arguments{
  \item{tarfile}{The pathname of the tar file: tilde expansion (see
    \code{\link{path.expand}}) will be performed.  Alternatively, a
    connection that can be used for binary reads.}

  \item{files}{A character vector of recorded filepaths to be extracted:
    the default is to extract all files.}
  
  \item{list}{If \code{TRUE}, just list the files.  The equivalent of
    \command{tar -tf}.  Otherwise extract the files (the equivalent of
    \command{tar -xf}).}
    
  \item{exdir}{The directory to extract files to (the equivalent of
    \command{tar -C}).  It will be created if necessary.}

  \item{compressed}{logical or character.  Values \code{"gzip"},
    \code{"bzip2"} and \code{"xz"} select that form of compression (and
    may be abbreviated to the first letter).  \code{TRUE} indicates gzip
    compression, \code{FALSE} no known compression (but the
    \command{tar} command may detect compression automagically), and
    \code{NA} (the default) that the type is inferred from the file
    header.}

  \item{extras}{\code{NULL} or a character string: further command-line
    flags such as \option{-p} to be passed to the \command{tar} program.}

  \item{verbose}{logical: if true echo the command used.}

  \item{tar}{character string: the path to the command to be used.}
}

\details{
  This is either a wrapper for a \command{tar} command or for an
  internal implementation written in \R.  The latter is used if
  \code{tarfile} is a connection or if the argument \code{tar} is
  \code{"internal"} or \code{""} (except on Windows, when
  \command{tar.exe} is tried first).

  What options are supported will depend on the \command{tar} used.
  Modern GNU flavours of \command{tar} will support compressed archives,
  and since 1.15 are able to detect the type of compression
  automatically: version 1.20 added support for \command{lzma} and
  version 1.22 for \command{xz} compression using LZMA2.  For other
  flavours of \command{tar}, environment variable \env{R_GZIPCMD} gives
  the command to decompress \command{gzip} and \command{compress} files,
  and \command{R_BZIPCMD} for its files.  (There is a \command{bsdtar}
  command from the \samp{libarchive} project used by Mac OS 10.6
  (\sQuote{Snow Leopard}) which can also detect \command{gzip} and
  \command{bzip2} compression automatically, as can the \command{tar}
  from the \sQuote{Heirloom Toolchest} project.)
  
  Arguments \code{compressed}, \code{extras} and \code{verbose} are only
  used when an external \command{tar} is used.

  The internal implementation restores links (hard and soft) as symbolic
  links on a Unix-alike, and as file copies on Windows (which works only
  for files, not for directories).  Since it uses \code{\link{gzfile}}
  to read a file it can handle files compressed by any of the methods
  that function can handle: at least \command{compress}, \command{gzip},
  \command{bzip2} and \command{xz} compression, and some types of
  \command{lzma} compression.  It does not guard against restoring
  absolute file paths, as some \command{tar} implementations do.  It
  will create the parent directories for directories or files in the
  archive if necessary. It handles both the standard (USTAR/POSIX) and
  GNU ways of handling file paths of more than 100 bytes.
  
  The standards only support ASCII filenames (indeed, only alphanumeric
  plus period, underscore and hyphen).  \code{untar} makes no attempt to map
  filenames to those acceptable on the current system, and treats the
  filenames in the archive as applicable without any re-encoding in the
  current locale.
}

\value{
  If \code{list = TRUE}, a character vector of (relative or absolute)
  paths of files contained in the tar archive.

  Otherwise the return code from \code{\link{system}}, invisibly.
}

\seealso{
  \code{\link{tar}}, \code{\link{unzip}}.  
}

\keyword{file}
\keyword{utilities}
