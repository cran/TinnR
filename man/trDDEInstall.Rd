\name{trDDEInstall}
\alias{trDDEInstall}

\title{
  Install a DDE server (Windows only) for Tinn-R
}

\description{
  Install and manage a DDE server to return context-sensitive
  calltips or completion lists to Tinn-R GUI/Editor under Windows.
}

\usage{
  trDDEInstall()
}

\arguments{
}

\value{
  Theis function should be used to interact with Tinn-R. Their result
  is returned invisibly for debugging purposes and is not intended to be use
  in R.
}

\author{
  Adaptations: Jose Claudio Faria \email{joseclaudio.faria@gmail.com}\cr
  Original author: Philippe Grosjean \email{phgrosjean@sciviews.org}
}

\note{
  DDE is a communication protocol that exists only under Windows. Consequently,
  those functions cannot be used (yet) on other platforms.

  On loading of the package, if the \code{option(use.DDE = TRUE)} is defined,
  the DDE server (\code{trDDEInstall()}) is automatically installed when the
  package is loaded. Also if \code{options(IDE = "[path.to.exe]")}  is defined,
  then that IDE is automatically started afterward.
}

\seealso{
  \code{\link[svMisc]{CallTip}},
  \code{\link{trCallTip}},
  \code{\link[svMisc]{Complete}}
  \code{\link{trComplete}}
}

\examples{
  \dontrun{
    ## DDE exchange protocol is available ONLY under Windows!

    ## Also, this cannot be run by examples() but should be OK when pasted
    ## into an interactive R session with the tcltk package loaded

    ## Here is how you can test these features under Windows
    options(use.DDE = TRUE)
    library(TinnR) # This should automatically start the
    # DDE server named 'TclEval Tinn-R', according to the option set

    # Get some data in the user workspace
    data(trees)
    a <- 1
    b <- "some text in the first instance of R"

    #######################################################
    # To test these functions in an external program, we need now
    # to start a second instance of R. In this second instance, enter:
    library(tcltk)
    .Tcl("package require dde")
    .Tcl("dde services TclEval {}")
    # You should get 'TclEval Tinn-R' in the list
    # if the server in the first instance is running

    # Now, request a calltip for the function 'ls'
    # This is done in two steps:
    # 1) Execute the command 'trCallTip' with this code chunk as argument
    .Tcl("dde execute TclEval Tinn-R {trCallTip {res <- ls(}}")
    # 2) Retrieve the calltip from the variable tr_CallTip
    .Tcl("dde request TclEval Tinn-R tr_CallTip")

    # Another way to trigger DDE commands (for programs that do not support
    # DDE is to use 'execdde.exe' of the tcltk2 package (see ?tk2dde)

    # It is also possible to copy the calltip to a file, or to the clipboard
    # by specifying it after the command (also the two additional arguments
    # have their default values changed)
    .Tcl("dde execute TclEval Tinn-R {trCallTip {library(} clipboard TRUE 40 TRUE}")
    # Look at what is in the clipboard
    cat(readClipboard(), "\n")

    # The process is similar to get completion lists
    .Tcl("dde execute TclEval Tinn-R {trComplete {iris$}}")
    .Tcl("dde request TclEval Tinn-R tr_Complete")

    # Get the list of variables in the user workspace of the first R instance
    # into the clipboard (use also the other arguments to get the type of objects)
    .Tcl("dde execute TclEval Tinn-R {trComplete {} clipboard TRUE { - }}")
    # Look at what is in the clipboard
    cat(readClipboard(), "\n")
  }
}

\keyword{utilities}
