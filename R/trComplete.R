#=======================================================================
# Orinal author: Philippe Grosjean
# Original code: [svMisc] Complete and guiComplete function
# Adapted by   : Jose Claudio Faria
# Objective    : To supply the current necessity of the Tinn-R project
# Date         : 09/03/2008 1:25 PM
#=======================================================================

trComplete <-
function(code, file=NULL, givetype=FALSE, sep="|")
{
  nComplete <-
  function(code, givetype=FALSE, sep="\t")
  {
    ### TODO: implement 'givetype'!
    # Get a completion list, given a part of the code
    code <- paste(as.character(code), collapse="\n")
    if(is.null(code) || length(code) == 0 || code == "")
      return("")

    # Use the internal win32consoleCompletion function in utils package
    res <- utils:::.win32consoleCompletion(code, nchar(code))

    # Is there a single addition?
    if(res$addition != "")
      res <- paste(sep, gsub("=", " = ", res$addition), sep = "")
    # Is there a list of possible tokens?
    else if (res$comps != "") {
      # Replace space by fieldsep
      if (sep != " ") res <- gsub(" ", sep, res$comps)
      res <- gsub("=", " = ", res)
    } else
      return("")

    # Do we have to return something to complete?
    if(regexpr(paste("\\", sep, sep = ""), res) == -1)
      return("")
    else
      return(res)
  }

  # This is an interfacte to Complete for external programs
  .Tcl("set ::tr_Complete {}")

  # Using a callback, all args are strings => convert
  if(length(file) == 0 || file == "" || file == "NULL") file <- NULL
  givetype <- as.logical(givetype[1])
  sep = sep[1]

  # Get the completion list
  clist <- nComplete(code, givetype = givetype, sep = sep)

  # Copy the result to a Tcl variable
  .Tcl(paste("set ::tr_Complete {", clist, "}", sep = ""))

  # Copy it also to the clipboard or a file
  if(!is.null(file))
    # if file = clipboard and this is Windows, copy it to the clipboard
    if(file == "clipboard")
      ifelse(.Platform$OS.type == "windows", writeClipboard(clist),
        stop("'clipboard' not supported yet on platforms different than Windows!"))
    # copy the completion list to the file
    else
      cat(clist, file = file)
  invisible(clist)
}
