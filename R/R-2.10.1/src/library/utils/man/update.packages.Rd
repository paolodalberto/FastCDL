% File src/library/utils/man/update.packages.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{update.packages}
\alias{update.packages}
\alias{available.packages}
\alias{old.packages}
\alias{new.packages}
\alias{download.packages}
\alias{install.packages}
\alias{contrib.url}
\title{Download Packages from CRAN-like repositories}
\description{
  These functions can be used to automatically compare the version
  numbers of installed packages with the newest available version on
  the repositories and update outdated packages on the fly.
}
\usage{
update.packages(lib.loc = NULL, repos = getOption("repos"),
                contriburl = contrib.url(repos, type),
                method, instlib = NULL,
                ask = TRUE, available = NULL,
                oldPkgs = NULL, \dots, checkBuilt = FALSE,
                type = getOption("pkgType"))

available.packages(contriburl = contrib.url(getOption("repos"), type),
                   method, fields = NULL, type = getOption("pkgType"),
                   filters = NULL)

old.packages(lib.loc = NULL, repos = getOption("repos"),
             contriburl = contrib.url(repos, type),
             instPkgs = installed.packages(lib.loc = lib.loc),
             method, available = NULL, checkBuilt = FALSE,
             type = getOption("pkgType"))

new.packages(lib.loc = NULL, repos = getOption("repos"),
             contriburl = contrib.url(repos, type),
             instPkgs = installed.packages(lib.loc = lib.loc),
             method, available = NULL, ask = FALSE, \dots,
             type = getOption("pkgType"))

download.packages(pkgs, destdir, available = NULL,
                  repos = getOption("repos"),
                  contriburl = contrib.url(repos, type),
                  method, type = getOption("pkgType"), \dots)

install.packages(pkgs, lib, repos = getOption("repos"),
                 contriburl = contrib.url(repos, type),
                 method, available = NULL, destdir = NULL,
                 dependencies = NA, type = getOption("pkgType"),
                 configure.args = getOption("configure.args"),
                 configure.vars = getOption("configure.vars"),
                 clean = FALSE, Ncpus = getOption("Ncpus"), \dots)

contrib.url(repos, type = getOption("pkgType"))
}
\arguments{
  \item{lib.loc}{character vector describing the location of R
    library trees to search through (and update packages therein), or
    \code{NULL} for all known trees (see \code{\link{.libPaths}}).}
  \item{repos}{character vector, the base URL(s) of the repositories
    to use, i.e., the URL of the CRAN master such as
    \code{"http://cran.r-project.org"} or its Statlib mirror,
    \code{"http://lib.stat.cmu.edu/R/CRAN"}.
    Can be \code{NULL} to install from local files
    (\file{.tar.gz} for source packages).
  }
  \item{contriburl}{URL(s) of the contrib sections of the
    repositories.  Use this argument only if your repository mirror is
    incomplete, e.g., because you burned only the \file{contrib} section on a
    CD.  Overrides argument \code{repos}.
    As \code{repos}, can also be \code{NULL} to install from local files.
  }
  \item{method}{Download method, see \code{\link{download.file}}.}
  \item{pkgs}{character vector of the short names of packages/bundles whose
    current versions should be downloaded from the repositories.
    If \code{repos = NULL}, a character vector of file paths of
#ifdef windows
    \file{.zip} files containing binary builds of packages.  With
    suitable tools installed (see the \sQuote{Details} below) source directories
    or \file{.tar.gz} archives may also be installed.
#endif
#ifdef unix
    \file{.tar.gz} files.  These can be source archives or binary
    package/bundle archive files (as created by
    \command{R CMD build --binary}).
    Tilde-expansion will be done on the file paths.
#endif
    If this is a zero-length character vector, a listbox of available
    packages (including those contained in bundles) is presented where
    possible.
  }
  \item{destdir}{directory where downloaded packages are stored.}
  \item{available}{an object listing packages available at the repositories
    as returned by \code{available.packages}.}
  \item{lib}{character vector giving the library directories where to
    install the packages.  Recycled as needed.  If missing, defaults to
    \code{\link{.libPaths}()[1]}.}
  \item{ask}{logical indicating whether to ask user before packages
    are actually downloaded and installed, or the character string
    \code{"graphics"}, which brings up a widget to allow the user to
    (de-)select from the list of packages which could be updated.  The
    latter only works on systems with a GUI version of
    \code{\link{select.list}}, and is otherwise equivalent to \code{ask
      = TRUE}.
  }
  \item{checkBuilt}{If \code{TRUE}, a package built under an earlier
    minor version of \R is considered to be \sQuote{old}.}
  \item{instlib}{character string giving the library directory where to
    install the packages.}
  \item{dependencies}{logical indicating to also install uninstalled
    packages on which these packages depend/suggest/import
    (and so on recursively).  Not used if \code{repos = NULL}.
    Can also be a character vector, a subset of
    \code{c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")}.

    Only supported if \code{lib} is of length one (or missing),
    so it is unambiguous where to install the dependent packages.

    The default, \code{NA}, means
    \code{c("Depends", "Imports", "LinkingTo")}
    if \code{lib} is unambiguous, and \code{FALSE} otherwise.
  }
  \item{configure.args}{(not Windows) a character vector or a named list.
    If a character vector with no names is supplied, the elements are
    concatenated into a single string (separated by a space) and used
    as the value for the \option{--configure-args}
    flag in the call to \command{R CMD INSTALL}.
    If the character vector has names these are assumed to identify
    values for \option{--configure-args} for individual packages.
    This allows one to specify settings for an entire collection of packages
    which will be used if any of those packages are to be installed.
    (These settings can therefore be reused and act as default settings.)
    
    A named list can be used also to the same effect, and that
    allows multi-element character strings for each package
    which are concatenated to a single string to be used as the
    value for \option{--configure-args}.
  }
  \item{configure.vars}{(not Windows) similar, for
    \option{--configure-vars}, which is used to set environment variables
    for the \command{configure} run.}
  \item{oldPkgs}{
    if specified as non-NULL, \code{update.packages()} only considers
    these packages for updating. 
  }
  \item{instPkgs}{
    by default all installed packages,
    \code{\link{installed.packages}(lib.loc=lib.loc)}.  A subset can be 
    specified; currently this must be in the same (character matrix)
    format as returned by \code{installed.packages()}.
  }
  \item{\dots}{
    (for \code{update.packages}).  Arguments such as \code{destdir},
    \code{installWithVers} and \code{dependencies} to be passed to
    \code{install.packages}.

    (for \code{new.packages}).  Arguments such as \code{destdir}
    and \code{dependencies} to be passed to \code{install.packages}.

    (for \code{install.packages} and \code{download.packages}) arguments
    to be passed to \code{\link{download.file}}.
  }
  \item{type}{character, indicating the type of package to download and
    install.
    
    Possible values except on Windowsare \code{"source"} (the default
    except under the CRAN Mac OS X build), \code{"mac.binary"} and
    \code{"win.binary"} (which can be downloaded but not installed).

    Possible values on Windows are \code{"win.binary"} (the default) and
    \code{"source"} (for which suitable tools may need to be installed:
    see the \sQuote{Details}).  Value \code{"mac.binary"} can be used to
    explore and download Mac OS X binaries.
  }
  \item{clean}{a logical value indicating whether to specify
    to add the \option{--clean} flag to the call to
    \command{R CMD INSTALL}.
    This is sometimes used to perform additional operations at the end
    of the package installation in addition to removing intermediate files.
  }
  \item{Ncpus}{The number of parallel processes to use for a parallel
    install of source packages.  Values greater than one are supported
    only if GNU \command{make} is in use (more precisely, if
    \command{make -j \var{Ncpus}} works).  Defaults to \code{1} is the
    option is unset.
  }
  \item{fields}{a character vector giving the fields to extract from
    the \code{PACKAGES} file(s) in addition to the default ones, or
    \code{NULL} (default).  Unavailable fields result in \code{NA}
    values.
  }
  \item{filters}{a character vector or list specifying the filter
    operations to be performed on the packages found in the
    repositories, or \code{NULL} (default).  Specified filters can be
    one of the strings \code{"R_version"}, \code{"OS_type"},
    \code{"duplicates"}, or \code{"license/FOSS"}, indicating built-in
    filters which retain only packages whose version and OS type
    requirements are met by the running version of \R, the latest
    versions of packages, and packages for which installation can
    proceed solely based on packages which can be verified as Free or
    Open Source Software (FOSS, e.g.,
    \url{http://en.wikipedia.org/wiki/FOSS}) employing the available
    license specifications, respectively.  Filters can also be
    user-defined functions which subscript the rows of objects returned
    by \code{available.packages}.  The default corresponds to the
    specification \code{c("R_version", "OS_type", "duplicates")}, and
    may be changed by setting option \code{available_packages_filters}
    to something non-\code{NULL}.  If the filters specification used is
    a list with an \code{add = TRUE} element, the other elements are
    used for filtering in addition to the default filters.
  }
}
\details{
  All of these functions work with the names of a package or bundle (and
  not the component packages of a bundle, except for
  \code{install.packages} if the repository provides the necessary
  information).
  
  \code{available.packages} returns a matrix of details corresponding to
  packages/bundles currently available at one or more repositories. The
  current list of packages is downloaded over the internet (or copied
  from a local mirror).  By default, it returns only packages whose
  version and OS type requirements are met by the running version of \R,
  and only information on the latest versions or packages.
  
  \code{old.packages} compares the information from
  \code{available.packages} with that from \code{instPkgs} (computed by
  \code{\link{installed.packages}} by default) and reports installed
  packages/bundles that have newer versions on the repositories or, if
  \code{checkBuilt = TRUE}, that were built under an earlier minor
  version of \R (for example built under 2.8.x when running \R 2.9.0).
#ifdef windows
  (There is no check that the version on the repository was built under
  the current minor version of \R, but it is advertised as being
  suitable for this version.)
#endif

  \code{new.packages} does the same comparison but reports uninstalled
  packages/bundles that are available at the repositories.  It will
  give warnings about incompletely installed bundles (provided the
  information is available) and bundles whose contents has changed.
  If \code{ask != FALSE} it asks which packages should be installed
  in the first element of \code{lib.loc}.

  \code{download.packages} takes a list of package/bundle names and a
  destination directory, downloads the newest versions and saves them in
  \code{destdir}.  If the list of available packages is not given as
  argument, it is obtained from repositories.  If a repository is local,
  i.e. the URL starts with \code{"file:"}, then the packages are not
  downloaded but used directly.  Both \code{"file:"} and
  \code{"file:///"} are allowed as prefixes to a file path.  Use the
  latter only for URLs: see \code{\link{url}} for their interpretation.
  (Other forms of \samp{file://} URLs are not supported.)
  
  The main function of the set is \code{update.packages}.  First a list
  of all packages/bundles found in \code{lib.loc} is created and
  compared with those available at the repositories.  If
  \code{ask = TRUE} (the default) packages/bundles with a newer version
  are reported and for each one the user can specify if it should be
  updated.
#ifdef unix
  If so, the package sources are downloaded from the repositories and
  installed in the respective library path (or \code{instlib}
  if specified) using the \R \code{\link{INSTALL}} mechanism.
#endif
#ifdef windows
  If so, the pre-compiled packages are downloaded from the repositories
  and installed in the respective library path (or \code{instlib} if
  specified).
#endif

  \code{install.packages} can be used to install new
  packages/bundles. It takes a vector of names and a destination
  library, downloads the packages from the repositories and installs
  them.  (If the library is omitted it defaults to the first directory
  in \code{.libPaths()}, with a warning if there is more than one.)  If
  \code{lib} is omitted or is of length one and is not a (group)
  writeable directory, the code offers to create a personal library tree
  (the first element of \code{Sys.getenv("R_LIBS_USER")}) and install
  there.
#ifdef windows
  Detection of a writeable directory is problematic on Windows: see the
  Note section.
#endif
  
#ifdef unix
  If a repository is used (rather than local \file{.tar.gz} files),
  an attempt is made to install the packages in an order that respects
  their dependencies.  This does assume that all the entries in
  \code{lib} are on the default library path for installs (set by
  \env{R_LIBS}). 
#endif

  \code{contrib.url} adds the appropriate type-specific path within a
  repository to each URL in \code{repos}.

  For \code{install.packages}, \code{destdir}
  is the directory to which packages will be downloaded.  If it is
  \code{NULL} (the default) a directory \code{downloaded_packages} of
  the session temporary directory will be used (and the files will be
  deleted at the end of the session).
  
  If \code{repos} or \code{contriburl} is a vector of length greater than
  one, the newest version of the package compatible with this version of \R
  is fetched from the first repository on the list within which it is found.
#ifdef windows
  
  Using packages with \code{type = "source"} works on Windows provided
  the package contains no C/C++/Fortran code that needs compilation,
  Otherwise you will need to have installed the Rtools collection as
  described in the \sQuote{R for Windows FAQ} \emph{and} you must have the
  \env{PATH} set up as required by the Rtools.
#endif
}
\value{
  For \code{available.packages}, a matrix with one row per
  package/bundle, row names the package names and 
  column names
  \code{"Package"}, \code{"Version"}, \code{"Priority"},
  \code{"Bundle"}, \code{"Contains"},
  \code{"Depends"}, \code{"Imports"}, \code{"LinkingTo"},
  \code{"Suggests"}, \code{"Enhances"},
  \code{"OS_type"}, \code{"License"},
  \code{"File"} and \code{"Repository"}.
  Additional columns can be specified using the \code{fields} argument.

  For \code{old.packages}, \code{NULL} or a matrix with one row per
  package/bundle, row names the package names and column names
  \code{"Package"}, \code{"LibPath"}, \code{"Installed"} (the version),
  \code{"Built"} (the version built under), \code{"ReposVer"} and
  \code{"Repository"}.
  
  For \code{new.packages} a character vector of package/bundle names,
  \emph{after} any have been installed.

  For \code{download.packages}, a two-column matrix of names and
  destination file names, for those packages/bundles successfully
  downloaded.  If packages are not available or there is a problem with
  the download, suitable warnings are given.

  \code{install.packages} and \code{update.packages} have no return value.
}
\section{Warning}{
  Take care when using \code{dependencies} with \code{update.packages},
  for it is unclear where new dependencies should be installed.  The
  current implementation will only allow it if all the packages to be
  updated are in a single library, when that library will be used.

  You are advised to run \code{update.packages} before
  \code{install.packages} to ensure that any installed dependencies have
  their latest versions.
}
#ifdef unix
\note{
  Some binary distributions of \R have \code{INSTALL} in a separate
  bundle, e.g. an \code{R-devel} RPM.  \code{install.packages} will
  give an error if called with \code{type = "source"} on such a system.
}
#endif
#ifdef windows
\note{
  \code{install.packages} tries to detect if you have write permission
  on the library directories specified, but Windows reports unreliably.
  If there is only one library directory (the default), \R even tries to
  find out by creating a test directory, but even this need not be the
  whole story.  Under Windows Vista you may have permission to write in
  a library directory but lack permission to write binary files (such as
  \file{.dll} files) there.  See the \sQuote{R for Windows FAQ} for workarounds.
}
#endif
\seealso{
  \code{\link{installed.packages}}, \code{\link{remove.packages}}
  
  See \code{\link{download.file}} for how to handle proxies and
  other options to monitor file transfers.

  \code{\link{INSTALL}}, \code{\link{REMOVE}},
  \code{\link{library}}, \code{\link{.packages}}, \code{\link{read.dcf}}

  The \sQuote{R Installation and Administration} manual for how to 
  set up a repository.
}

\examples{

\dontrun{
install.packages(
    c("XML_0.99-5.tar.gz",
      "../../Interfaces/Perl/RSPerl_0.8-0.tar.gz"),
    repos = NULL,
    configure.args = c(XML = '--with-xml-config=xml-config',
                       RSPerl = "--with-modules='IO Fcntl'"))
}
}
\keyword{utilities}
