\name{trStartIDE}
\alias{trStartIDE}

\title{
  Starts IDE (Windows only)
}

\description{
  Starts the IDE under Windows.
}

\usage{
  trStartIDE()
}

\arguments{
}

\value{
}

\author{
  Adaptations: Jose Claudio Faria \email{joseclaudio.faria@gmail.com}\cr
  Original author: Philippe Grosjean \email{phgrosjean@sciviews.org}
}

\note{
  If \code{options(IDE = "[path.to.exe]")}  is defined,
  then that IDE is automatically started afterward.
}

\examples{
  \dontrun{
    ## The bet way to use this function is add it in the Rprofile.site file
    ## located in the folder 'etc' where R is installed.
    trStartIDE()
  }
}

\keyword{utilities}
