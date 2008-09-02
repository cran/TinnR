#=======================================================================
# Orinal author: Philippe Grosjean
# Original code: [svMisc] Args CallTip and guiCallTip functions
# Adapted by   : Jose Claudio Faria
# Objective    : To supply the current necessity of the Tinn-R project
# date         : 09/03/2008 1:25 PM
#=======================================================================

trCallTip <-
function(code, file=NULL, onlyargs=FALSE, width=60, location=FALSE)
{
  tr.Args <-
  function(name, only.args=FALSE)
  {
    #### TODO: handle primitives and S3/S4 methods for generic functions
    ret <- try(res <- eval(parse(text=paste("argsAnywhere(", name, ")",
      sep=""))), silent=TRUE)
    if(inherits(ret, "try-error") || is.null(res))
      # Function 'name' not found
      return("")
    res <- deparse(res)
    res <- paste(res[-length(res)], collapse = "\n")
    if(only.args) {
      res <- sub("^function *[(]", "", res)
      res <- sub(" *[)] *$", "", res)
    } else {
      res <- sub("^function", name, res)
      res <- sub(" *$", "", res)
    }
    return(res)
  }

  tr.CallTip <-
  function(code, only.args=FALSE, location=FALSE)
  {
    # Get a call tip, given a part of the code
    # Extract the last variable name, given it is either at the end,
    # or terminated by '('
    code <- sub(" *\\($", "", code[1])
    pos <- regexpr("[a-zA-Z0-9_\\.]+$", code)
    code <- substring(code, pos)

    # Get the corresponding Call Tip
    # Default value, in case the function does not exist
    ctip <- ""
    if(code != "")
      ctip <- tr.Args(code, only.args=only.args)
    if(is.null(ctip))
      return("")
    # Do we need to append an indication of where this function is located?
    if(location == TRUE) {
      ### TODO: use getAnywhere() instead
      pkg <- sub("^package:", "", find(code, mode="function"))
      if(length(pkg) > 0 && pkg != ".GlobalEnv")
        ctip <- paste(ctip, " [", pkg, "]", sep="")
    }
    return(ctip)
  }

  # This is an interface to CallTip for Tinn-R
  .Tcl("set ::tr_CallTip {}")

  # Using a callback, all args are strings => convert
  if(length(file) == 0 || file == "" || file == "NULL") file <- NULL
  onlyargs <- as.logical(onlyargs[1])
  width <- as.integer(width[1])

  # Get the call tip
  ctip <- tr.CallTip(code, only.args=onlyargs, location=location)

  # Possibly break long lines at reasonables widths
  ifelse(onlyargs, Exdent <- 0, Exdent <- 4)
  if(!is.null(width) && !width < 1)
    ctip <- paste(strwrap(ctip, width=width, exdent=Exdent), collapse="\n")

  # Copy the result to a Tcl variable
  .Tcl(paste("set ::tr_CallTip {", ctip, "}", sep=""))

  # Copy it also to the clipboard or a file
  if(!is.null(file))
    # if file = clipboard and this is Windows, copy it to the clipboard
    if(file == "clipboard")
      ifelse(.Platform$OS.type == "windows", writeClipboard(ctip),
        stop("'clipboard' not supported yet on platforms different than Windows!"))
    # copy the call tip to the file
    else
      cat(ctip, file=file)

  invisible(ctip)
}
