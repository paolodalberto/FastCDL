% File src/library/stats/man/logLik.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{logLik}
\alias{logLik}
\alias{print.logLik}
\alias{str.logLik}
\alias{logLik.lm}
\title{Extract Log-Likelihood}
\usage{
logLik(object, \dots)

\method{logLik}{lm}(object, REML = FALSE, \dots)
}
\arguments{
 \item{object}{any object from which a log-likelihood value, or a
   contribution to a log-likelihood value, can be extracted.}
 \item{\dots}{some methods for this generic function require additional
   arguments.}
  \item{REML}{an optional logical value.  If \code{TRUE} the restricted
    log-likelihood is returned, else, if \code{FALSE}, the
    log-likelihood is returned.  Defaults to \code{FALSE}.}
}
\description{
  This function is generic; method functions can be written to handle
  specific classes of objects.  Classes which already have methods for
  this function include: \code{glm}, \code{lm}, \code{nls}, \code{Arima}
  and \code{gls}, \code{lme} and others in package \pkg{nlme}.
}
%   \code{corStruct}, \code{lmList}, \code{lmeStruct}, \code{reStruct}, and
%   \code{varFunc}.
\details{
  For a \code{"glm"} fit the \code{\link{family}} does not have to specify
  how to calculate the log-likelihood, so this is based on using the
  family's \code{aic()} function to compute the AIC.  For the
  \code{\link{gaussian}}, \code{\link{Gamma}} and
  \code{\link{inverse.gaussian}} families it assumed that the dispersion
  of the GLM is estimated has been counted as a parameter in the AIC
  value, and for all other families it is assumed that the dispersion is
  known.

  Note that this procedure does not give the maximized likelihood for
  \code{"glm"} fits from the Gamma and inverse gaussian families, as the
  estimate of dispersion used is not the MLE.

  For \code{"lm"} fits it is assumed that the scale has been estimated
  (by maximum likelihood or REML), and all the constants in the
  log-likelihood are included.
}
\value{
  Returns an object, say \code{r}, of class \code{logLik} which is a
  number with attributes, \code{attr(r, "df")} (\bold{d}egrees of
  \bold{f}reedom) giving the number of (estimated) parameters in the model.
  There is a simple \code{print} method for \code{logLik} objects.

  The details depend on the method function used; see the appropriate
  documentation.
}
\seealso{
  \code{\link[nlme:logLik.lme]{logLik.gls}}, \code{\link[nlme]{logLik.lme}}, in
  package \pkg{nlme}, etc.
}
\references{
  For \code{logLik.lm}:
  
  Harville, D.A. (1974).
  Bayesian inference for variance components using only error contrasts.
  \emph{Biometrika}, \bold{61}, 383--385.
}
\author{Jose Pinheiro and Douglas Bates}
\examples{
x <- 1:5
lmx <- lm(x ~ 1)
logLik(lmx) # using print.logLik() method
utils::str(logLik(lmx))

## lm method
(fm1 <- lm(rating ~ ., data = attitude))
logLik(fm1)
logLik(fm1, REML = TRUE)

utils::data(Orthodont, package="nlme")
fm1 <- lm(distance ~ Sex * age, Orthodont)
logLik(fm1)
logLik(fm1, REML = TRUE)
}
\keyword{models}
