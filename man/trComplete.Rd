\name{trComplete}
\alias{trComplete}

\title{
  Get a completion list for a R code fragment
}

\description{
  A list with matching items is returned in a string.
}

\usage{
  trComplete(code, file=NULL, givetype=FALSE, sep="|")
}

\arguments{
  \item{code}{A piece of R code (in a character string) to analyze.}
  \item{file}{A file where to return the result ("", or NULL for none). You
    can use "clipboard" to send the result to the clipboard under Windows only.}
  \item{givetype}{Return also the type of each object in the completion list
    (possibly to use set a different icon in the list displayed by the external
    program).}
  \item{sep}{Separator to use between items in the file (if file is not NULL).}
}

\value{
  A string with the completion list (items separated by 'sep'), or
  an unique string completing unambiguously the current code, starting with
  'sep'.
}

\details{
  The completion list is context-dependent, and it is calculated as if the
  code was entered at the command line.

  If the code ends with \code{$}, then the function look for
  items in a list or data.frame whose name is the last identifier.

  If the code ends with \code{@}, then the function look for slots of the
  corresponding S4 object.

  If the code ends with \code{::}, then it looks for objects in a namespace.

  If the code ends with a partial identifier name, the function returns all
  visible matching keywords.

  There are other situations where the completion can be calculated, see
  the help of \code{rc.settings()}.
}

\note{
  This function uses \code{utils:::.win32consoleCompletion()} as completion
  engine, then, it makes the result compatible with old version of
  \code{trComplete()}.
}

\author{
  Adaptations: Jose Claudio Faria \email{joseclaudio.faria@gmail.com}\cr
  Original author: Philippe Grosjean \email{phgrosjean@sciviews.org}
}

\seealso{
  \code{\link[svMisc]{Complete}}
  \code{\link[svMISC]{CallTip}},
  \code{\link{trCallTip}}
  \code{\link{trDDEInstall}}
}

\examples{
  \dontrun{
    ## a data.frame
    data(iris)
    trComplete("item <- iris$")
    trComplete("item <- iris[[", TRUE)

    ## An S4 object
    setClass("track", representation(x = "numeric", y = "numeric"))
    t1 <- new("track", x=1:20, y=(1:20)^2)
    trComplete("item2 <- t1@", TRUE)

    ## A namespace
    trComplete("utils::")

    ## A partial identifier
    trComplete("item3 <- va", TRUE)

    ## Otherwise, a list with the content of .GlobalEnv
    trComplete("item4 <- ")
  }
}

\keyword{utilities}
