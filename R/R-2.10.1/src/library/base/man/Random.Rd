% File src/library/base/man/Random.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{Random}
\alias{RNG}
\alias{RNGkind}
\alias{RNGversion}
\alias{set.seed}
\alias{.Random.seed}
\title{Random Number Generation}
\description{
  \code{.Random.seed} is an integer vector, containing the random number
  generator (RNG) \bold{state} for random number generation in \R.  It
  can be saved and restored, but should not be altered by the user.

  \code{RNGkind} is a more friendly interface to query or set the kind
  of RNG in use.

  \code{RNGversion} can be used to set the random generators as they
  were in an earlier \R version (for reproducibility).

  \code{set.seed} is the recommended way to specify seeds.
}
\usage{
.Random.seed <- c(rng.kind, n1, n2, \dots)
save.seed <- .Random.seed

RNGkind(kind = NULL, normal.kind = NULL)
RNGversion(vstr)
set.seed(seed, kind = NULL, normal.kind = NULL)
}
\arguments{
  \item{kind}{character or \code{NULL}.  If \code{kind} is a character
    string, set \R's RNG to the kind desired.  Use \code{"default"} to
    return to the \R default.  See \sQuote{Details} for the
    interpretation of \code{NULL}.}
  \item{normal.kind}{character string or \code{NULL}.  If it is a character
    string, set the method of Normal generation.  Use \code{"default"}
    to return to the \R default.  \code{NULL} makes no change.}
  \item{seed}{a single value, interpreted as an integer.}
  \item{vstr}{a character string containing a version number,
    e.g., \code{"1.6.2"}}
  \item{rng.kind}{integer code in \code{0:k} for the above \code{kind}.}
  \item{n1, n2, \dots}{integers.  See the details for how many are required
    (which depends on \code{rng.kind}).}
}
%% source and more detailed references:  ../../../main/RNG.c
\details{
  The currently available RNG kinds are given below.  \code{kind} is
  partially matched to this list.  The default is
  \code{"Mersenne-Twister"}.
  \describe{
    \item{\code{"Wichmann-Hill"}}{
    The seed, \code{.Random.seed[-1] == r[1:3]} is an integer vector of
    length 3, where each \code{r[i]} is in \code{1:(p[i] - 1)}, where
    \code{p} is the length 3 vector of primes, \code{p = (30269, 30307,
      30323)}.
    The Wichmann--Hill generator has a cycle length of
    \eqn{6.9536 \times 10^{12}}{6.9536e12} (=
    \code{prod(p-1)/4}, see \emph{Applied Statistics} (1984)
    \bold{33}, 123 which corrects the original article).}

    \item{\code{"Marsaglia-Multicarry"}:}{
    A \emph{multiply-with-carry} RNG is used, as recommended by George
    Marsaglia in his post to the mailing list \file{sci.stat.math}.
    It has a period of more than \eqn{2^{60}}{2^60} and has passed
    all tests (according to Marsaglia).  The seed is two integers (all
    values allowed).}

    \item{\code{"Super-Duper"}:}{
    Marsaglia's famous Super-Duper from the 70's.  This is the original
    version which does \emph{not} pass the MTUPLE test of the Diehard
    battery.  It has a period of \eqn{\approx 4.6\times 10^{18}}{about
      4.6*10^18} for most initial seeds.  The seed is two integers (all
    values allowed for the first seed: the second must be odd).

    We use the implementation by Reeds et al.\ (1982--84).

    The two seeds are the Tausworthe and congruence long integers,
    respectively.  A one-to-one mapping to S's \code{.Random.seed[1:12]}
    is possible but we will not publish one, not least as this generator
    is \bold{not} exactly the same as that in recent versions of S-PLUS.}

    \item{\code{"Mersenne-Twister":}}{
      From Matsumoto and Nishimura (1998). A twisted GFSR with period
      \eqn{2^{19937} - 1}{2^19937 - 1} and equidistribution in 623
      consecutive dimensions (over the whole period).  The \sQuote{seed} is a
      624-dimensional set of 32-bit integers plus a current position in
      that set.
    }

    \item{\code{"Knuth-TAOCP-2002":}}{
      A 32-bit integer GFSR using lagged Fibonacci sequences with
      subtraction.  That is, the recurrence used is
      \deqn{X_j = (X_{j-100} - X_{j-37}) \bmod 2^{30}%
      }{X[j] = (X[j-100] - X[j-37]) mod 2^30}
      and the \sQuote{seed} is the set of the 100 last numbers (actually
      recorded as 101 numbers, the last being a cyclic shift of the
      buffer).  The period is around \eqn{2^{129}}{2^129}.
    }

    \item{\code{"Knuth-TAOCP":}}{
      An earlier version from Knuth (1997).

      The 2002 version was not backwards compatible with the earlier
      version: the initialization of the GFSR from the seed was altered.
      \R did not allow you to choose consecutive seeds, the reported
      \sQuote{weakness}, and already scrambled the seeds.

      Initialization of this generator is done in interpreted \R code
      and so takes a short but noticeable time.
    }

    \item{\code{"user-supplied":}}{
      Use a user-supplied generator. See \code{\link{Random.user}} for
      details.
    }
  }

  \code{normal.kind} can be \code{"Kinderman-Ramage"},
  \code{"Buggy Kinderman-Ramage"} (not for \code{set.seed}),
  \code{"Ahrens-Dieter"}, \code{"Box-Muller"}, \code{"Inversion"} (the
  default), or \code{"user-supplied"}.  (For inversion, see the
  reference in \code{\link{qnorm}}.)  The Kinderman-Ramage generator
  used in versions prior to 1.7.1 (now called \code{"Buggy"} had several
  approximation errors and should only be used for reproduction of older
  results.  The \code{"Box-Muller"} generator is stateful as pairs of
  normals are generated and returned sequentially.  The state is reset
  whenever it is selected (even if it is the current normal generator)
  and when \code{kind} is changed.

  \code{set.seed} uses its single integer argument to set as many seeds
  as are required.  It is intended as a simple way to get quite different
  seeds by specifying small integer arguments, and also as a way to get
  valid seed sets for the more complicated methods (especially
  \code{"Mersenne-Twister"} and \code{"Knuth-TAOCP"}).

  The use of \code{kind=NULL} or \code{normal.kind=NULL} in
  \code{RNGkind} or \code{set.seed} selects the currently-used
  generator (including that used in the previous session if the
  workspace has been restored): if no generator has been used it selects
  \code{"default"}.
}
\note{
  Initially, there is no seed;  a new one is created from the current
  time when one is required.  Hence, different sessions started at
  (sufficiently) different times will give different simulation results,
  by default.  However, the seed might be restored from a previous
  session if a previously saved workspace is restored.

  \code{.Random.seed} saves the seed set for the uniform random-number
  generator, at least for the system generators.  It does not
  necessarily save the state of other generators, and in particular does
  not save the state of the Box--Muller normal generator.  If you want
  to reproduce work later, call \code{set.seed} (preferably with
  explicit values for \code{kind} and \code{normal.kind}) rather than
  set \code{.Random.seed}.

  The object \code{.Random.seed} is only looked for in the user's
  workspace.

  Do not rely on randomness of low-order bits from RNGs.  Most of the
  supplied uniform generators return 32-bit integer values that are
  converted to doubles, so they take at most \eqn{2^{32}}{2^32} distinct
  values and long runs will return duplicated values (Wichmann-Hill is
  the exception, and all give at least 30 varying bits.)
}
\value{
  \code{.Random.seed} is an \code{\link{integer}} vector whose first
  element \emph{codes} the kind of RNG and normal generator. The lowest
  two decimal digits are in \code{0:(k-1)}
  where \code{k} is the number of available RNGs.  The hundreds
  represent the type of normal generator (starting at \code{0}).

  In the underlying C, \code{.Random.seed[-1]} is \code{unsigned};
  therefore in \R \code{.Random.seed[-1]} can be negative, due to
  the representation of an unsigned integer by a signed integer.

  \code{RNGkind} returns a two-element character vector of the RNG and
  normal kinds selected \emph{before} the call, invisibly if either
  argument is not \code{NULL}.  A type starts a session as the default,
  and is selected either by a call to \code{RNGkind} or by setting
  \code{.Random.seed} in the workspace.

  \code{RNGversion} returns the same information as \code{RNGkind} about
  the defaults in a specific \R version.

  \code{set.seed} returns \code{NULL}, invisibly.
}
\references{
  Ahrens, J. H. and Dieter, U. (1973)
  Extensions of Forsythe's method for random sampling from the normal
  distribution.
  \emph{Mathematics of Computation} \bold{27}, 927-937.

  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole. (\code{set.seed}, storing in \code{.Random.seed}.)

  Box, G. E. P. and Muller, M. E. (1958) A note on the generation of normal
  random deviates. \emph{Annals of Mathematical Statistics} \bold{29},
  610--611.

  De Matteis, A. and Pagnutti, S. (1993)
  \emph{Long-range Correlation Analysis of the Wichmann-Hill Random
      Number Generator}, Statist. Comput., \bold{3}, 67--70.

  Kinderman, A. J. and Ramage, J. G. (1976)
  Computer generation of normal random variables.
  \emph{Journal of the American Statistical Association} \bold{71},
  893-896.

  Knuth, D. E. (1997)
  \emph{The Art of Computer Programming.} Volume 2, third edition.\cr
  Source code at \url{http://www-cs-faculty.stanford.edu/~knuth/taocp.html}.

  Knuth, D. E. (2002)
  \emph{The Art of Computer Programming.} Volume 2, third edition, ninth
  printing.\cr
  See \url{http://Sunburn.Stanford.EDU/~knuth/news02.html}.

  Marsaglia, G. (1997) \emph{A random number generator for C.} Discussion
  paper, posting on Usenet newsgroup \code{sci.stat.math} on
  September 29, 1997.

  Marsaglia, G. and Zaman, A. (1994) Some portable very-long-period
  random number generators. \emph{Computers in Physics}, \bold{8},
  117--121.

  Matsumoto, M. and Nishimura, T. (1998)
  Mersenne Twister: A 623-dimensionally equidistributed uniform
  pseudo-random number generator,
  \emph{ACM Transactions on Modeling and Computer Simulation},
  \bold{8}, 3--30.\cr
  Source code at \url{http://www.math.keio.ac.jp/~matumoto/emt.html}.

  Reeds, J., Hubert, S. and Abrahams, M. (1982--4) C implementation of
  SuperDuper, University of California at Berkeley.  (Personal
  communication from Jim Reeds to Ross Ihaka.)

  Wichmann, B. A.  and Hill, I. D. (1982)
  \emph{Algorithm AS 183: An Efficient and Portable Pseudo-random Number
    Generator}, Applied Statistics, \bold{31}, 188--190; Remarks:
  \bold{34}, 198 and \bold{35}, 89.
}
\author{of RNGkind: Martin Maechler. Current implementation, B. D. Ripley}

\seealso{\code{\link{runif}}, \code{\link{rnorm}}, \ldots.}
%       this is ./Uniform.Rd
\examples{
require(stats)

## the default random seed is 626 integers, so only print a few
runif(1); .Random.seed[1:6]; runif(1); .Random.seed[1:6]
## If there is no seed, a "random" new one is created:
rm(.Random.seed); runif(1); .Random.seed[1:6]

ok <- RNGkind()
RNGkind("Wich")# (partial string matching on 'kind')

## This shows how 'runif(.)' works for Wichmann-Hill,
## using only R functions:

p.WH <- c(30269, 30307, 30323)
a.WH <- c(  171,   172,   170)
next.WHseed <- function(i.seed = .Random.seed[-1])
  { (a.WH * i.seed) \%\% p.WH }
my.runif1 <- function(i.seed = .Random.seed)
  { ns <- next.WHseed(i.seed[-1]); sum(ns / p.WH) \%\% 1 }
rs <- .Random.seed
(WHs <- next.WHseed(rs[-1]))
u <- runif(1)
stopifnot(
 next.WHseed(rs[-1]) == .Random.seed[-1],
 all.equal(u, my.runif1(rs))
)

## ----
.Random.seed
RNGkind("Super")#matches  "Super-Duper"
RNGkind()
.Random.seed # new, corresponding to  Super-Duper

## Reset:
RNGkind(ok[1])

## ----
sum(duplicated(runif(1e6))) # around 110 for default generator
## and we would expect about almost sure duplicates beyond about
qbirthday(1-1e-6, classes=2e9) # 235,000
}
\keyword{distribution}
\keyword{sysdata}
