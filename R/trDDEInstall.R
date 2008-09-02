#=======================================================================
# Orinal author: Philippe Grosjean
# Original code: [svIDE] guiDDEInstall function
# Adapted by   : Jose Claudio Faria
# Objective    : To supply the current necessity of the Tinn-R project
# Date         : 09/03/2008 1:25 PM
#=======================================================================

trDDEInstall <-
function()
{
  # Register a dde server for R and install callbacks for serveur functions
  # Make sure tcl/tk dde is operational
  if(.Platform$OS.type != "windows")
    return("DDE not installed: this is not Windows!")
  if(!capabilities("tcltk"))
    return("DDE not installed: this version of R cannot use Tcl/Tk!")
  if(!require(tcltk))
    return("DDE not installed: impossible to load tcltk package!")
  tclRequire("dde", warn=TRUE)
  # Should be installed by default with the tcltk package under Windows

  # Register a "Tinn-R" server
  topic <- "Tinn-R"

  # Verify if I am not already registered under this topic
  if(!tclvalue(.Tcl("dde servername {}")) == topic) {
    # Check that this server name does not exist yet
    if(length(grep(paste("[{]TclEval ", topic, "[}]", sep=""),
              tclvalue(.Tcl("dde services TclEval {}")))) > 0) {
      mes <- "DDE not installed: server name already in use!"
      return(invisible(mes))
    }

    # Register me as a dde server with this topic name
    .Tcl(paste("dde servername", topic))
    # Check that the server is set correctly (if not, return an error)
    if(!tclvalue(.Tcl("dde servername {}")) == topic) {
      mes <- "DDE not installed: unknown error while starting the server!"
      return(invisible(mes))
    }
  }

  # Install callbacks for trXXXX functions, for DDE clients to access them
  # trCallTip()... Take care: must be adapted if you change trCallTip()!
  res <- .Tcl.args(trCallTip)
  .Tcl(paste("proc trCallTip {code {file \"\"} {onlyargs FALSE}",
             " {width 60} {location FALSE} }", gsub("%", "$", res), sep=""))

  # trComplete()... Take care: must be adapted if you change trComplete()!
  res <- .Tcl.args(trComplete)
  .Tcl(paste("proc trComplete {code {file \"\"} {givetype FALSE}",
             " {sep |} }", gsub("%", "$", res), sep=""))

  # Done
  return(invisible("")) # OK!
}
