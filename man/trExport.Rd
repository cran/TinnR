\name{trExport}
\alias{trExport}
\alias{trExport.default}
\alias{trExport.data.frame}
\alias{trExport.matrix}

\title{
  Export the content of an object in a foreign format
}

\description{
  \code{trExport} is a generic function (S3 based) to convert objects into
  various foreign formats and write it to a file or a connection.
}

\usage{
  trExport(x, \dots)
  \method{trExport}{default}(x, type="raw", file, append=FALSE,
    objname=deparse(substitute(x)), \dots)
  \method{trExport}{data.frame}(x, type="raw", file=NULL, append=FALSE,
    objname=deparse(substitute(x)), \dots)
  \method{trExport}{matrix}(x, type="raw", file, append=FALSE,
    objname=deparse(substitute(x)), \dots)
}

\arguments{
  \item{x}{The object to trExport.}
  \item{type}{\code{type} gives the format in which the object should be
    converted. The list of recognized formats for this object is obtained with
    \code{type="typelist"}.}
  \item{file}{The path to the file where the converted object should be
    extracted.}
  \item{append}{Do we append this object to this file?}
  \item{objname}{The name of the object (by default, it is the name of the
    variable that holds it).}
  \item{\dots}{Further argument passed to specific trExport methods.}
}

\value{
  Return the result from specif trExportation methods (invisibly if the file
  is written).
}

\author{
  Adaptations: Jose Claudio Faria \email{joseclaudio.faria@gmail.com}\cr
  Original author: Philippe Grosjean \email{phgrosjean@sciviews.org}
}

\seealso{
  \code{\link{trCopy}}
}

\examples{
  \dontrun{
    data(iris)
    trExport(iris, type="ascii", file="iris.txt")
    # Inspect the file 'iris.txt', and then...
    unlink("iris.txt")
  }
}

\keyword{IO}
\keyword{file}
