% File src/library/base/man/Quotes.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Development Team
% Distributed under GPL 2 or later

\name{Quotes}
\alias{Quotes}
\alias{backtick}
\alias{backquote}
\alias{'}%'
\alias{"}%"
\alias{`}%`
\concept{quotes}
\concept{backslash}
\title{Quotes}
\description{
  Descriptions of the various uses of quoting in \R.
}
\details{
  Three types of quote are part of the syntax of \R: single and double
  quotation marks and the backtick (or back quote, \samp{`}).  In
  addition, backslash is used for quoting the following characters
  inside character constants.
}
\section{Character constants}{
  Single and double quotes delimit character constants.  They can be used
  interchangeably but double quotes are preferred (and character
  constants are printed using double quotes), so single quotes are
  normally only used to delimit character constants containing double
  quotes.

  Backslash is used to start an escape sequence inside character constants.
  Unless specified in the following table, an escaped character is
  interpreted as the character itself.  (Note that the parser will warn
  about most such uses, as they are most often erroneous, e.g.
  using \samp{\\.} where \samp{\\\\.} was intended.)
  
  Single quotes need to be escaped by backslash in single-quoted
  strings, and double quotes in double-quoted strings.

  \tabular{ll}{
    \samp{\\n}\tab newline\cr
    \samp{\\r}\tab carriage return\cr
    \samp{\\t}\tab tab\cr
    \samp{\\b}\tab backspace\cr
    \samp{\\a}\tab alert (bell)\cr
    \samp{\\f}\tab form feed\cr
    \samp{\\v}\tab vertical tab\cr
    \samp{\\\\}\tab backslash \samp{\\}\cr
    \samp{\\nnn}\tab character with given octal code (1, 2 or 3 digits)\cr
    \samp{\\xnn}\tab character with given hex code (1 or 2 hex digits)\cr
    \samp{\\unnnn}\tab Unicode character with given code (1--4 hex digits)\cr
    \samp{\\Unnnnnnnn}\tab Unicode character with given code (1--8 hex digits)\cr
  }
  The last two are only supported on versions of \R built with MBCS
  support: they are an error on other versions.  Alternative forms are
  \samp{\\u\{nnnn\}} and \samp{\\U\{nnnnnnnn\}}.  All except the Unicode
  escape sequences are also supported when reading character strings by
  \code{\link{scan}} and \code{\link{read.table}} if
  \code{allowEscapes = TRUE}.  On most platforms they can be used to
  enter Unicode characters not in the current locale (which will be
  stored internally in UTF-8).

  These forms will also be used by \code{\link{print.default}}
  when outputting non-printable characters (including backslash).

  Embedded nuls are not allowed in character strings, so using escapes
  (such as \samp{\\0}) for a nul will result in the string being
  truncated at that point (usually with a warning).
}
\section{Names and Identifiers}{
  Identifiers consist of a sequence of letters, digits, the period
  (\code{.}) and the underscore.  They must not start with a digit nor
  underscore, nor with a period followed by a digit.  \link{Reserved}
  words are not valid identifiers.

  The definition of a \emph{letter} depends on the current locale, but
  only ASCII digits are considered to be digits.

  Such identifiers are also known as \emph{syntactic names} and may be used
  directly in \R code.  Almost always, other names can be used
  provided they are quoted.  The preferred quote is the backtick
  (\samp{`}), and \code{\link{deparse}} will normally use it, but under
  many circumstances single or double quotes can be used (as a character
  constant will often be converted to a name).  One place where
  backticks may be essential is to delimit variable names in formulae:
  see \code{\link{formula}}.
}
\seealso{
  \code{\link{Syntax}} for other aspects of the syntax.

  \code{\link{sQuote}} for quoting English text.

  \code{\link{shQuote}} for quoting OS commands.

  The \emph{R Language Definition} manual.
}
\keyword{documentation}
