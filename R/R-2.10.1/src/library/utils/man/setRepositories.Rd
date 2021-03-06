% File src/library/utils/man/setRepositories.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{setRepositories}
\alias{setRepositories}
\title{Select Package Repositories}
\description{
  Interact with the user to choose the package repositories to be used.
}
\usage{
setRepositories(graphics = getOption("menu.graphics"), ind = NULL)
}
\arguments{
  \item{graphics}{Logical.
#ifdef windows
    If true use a listbox,
#endif
#ifdef unix
    If true and \pkg{tcltk} and an X server are available, use a Tk
    widget, or if under the AQUA interface use a Mac OS X widget,
#endif
    otherwise use a text list in the console.}
  \item{ind}{\code{NULL} or a vector of integer indices, which have the
    same effect as if they were entered at the prompt for
    \code{graphics=FALSE}.}
}
\details{
  The default list of known repositories is stored in the file
  \file{\var{\link{R_HOME}}/etc/repositories}.
  That file can be edited for a site, or a user can have a personal copy
  in \file{\var{HOME}/.R/repositories} which will take precedence.
  
  A Bioconductor mirror can be selected by setting
  \code{options("BioC_mirror")}: the default value is
  \samp{"http://www.bioconductor.org"}.
  
  The items that are preselected are those that are currently in
  \code{options("repos")} plus those marked as default in the
  list of known repositories.

  The list of repositories offered depends on the setting of option
  \code{"pkgType"} as some repositories only offer a subset of types
  (e.g. only source packages or not Mac OS X binary packages).

  This function requires the \R session to be interactive unless
  \code{ind} is supplied.
}
\value{
  This function is invoked mainly for its side effect of updating
  \code{options("repos")}.  It returns (invisibly) the previous
  \code{repos} options setting (as a \code{\link{list}} with component
  \code{repos}) or \code{\link{NULL}} if no changes were applied.
}
\seealso{
  \code{\link{chooseCRANmirror}}, \code{\link{install.packages}}.
}
\keyword{ utilities }
