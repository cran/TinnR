\name{trCallTip}
\alias{trCallTip}

\title{
  Show function arguments in a human-readable way - get a call tip
}

\description{
  Code tips in Tinn-R under DDE protocol.
}

\usage{
  trCallTip(code, file=NULL, onlyargs=FALSE, width=60, location=FALSE)
}

\arguments{
  \item{code}{A piece of R code (in a character string) to analyze.}
  \item{file}{A file where to return the result ("", or NULL for none). You
    can use "clipboard" to send the result to the clipboard under
    Windows only.}
  \item{onlyargs}{Do we return the whole calltip or only the function
    arguments?}
  \item{width}{Reformat the calltip to with (use 0 for not reformatting it).}
  \item{location} {If \code{TRUE} then the location (in which package the
    function resides) is appended to the calltip between square brackets.}
}

\value{
  A string with the calling syntax of the function
}

\note{
  \code{Args()} is supposed to display S3/S4 methods, and primitives
  adequately,... but this is not implemented yet in the current version!
}

\author{
  Adaptations: Jose Claudio Faria \email{joseclaudio.faria@gmail.com}\cr
  Original author: Philippe Grosjean \email{phgrosjean@sciviews.org}
}

\seealso{
  \code{\link[svMISC]{CallTip}},
  \code{\link[svMisc]{Complete}}
  \code{\link{trComplete}}
  \code{\link{trDDEInstall}}
}

\keyword{ utilities }
