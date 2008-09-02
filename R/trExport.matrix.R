#=======================================================================
# Orinal author: Philippe Grosjean
# Original code: [svIO] export.matrix function
# Adapted by   : Jose Claudio Faria
# Objective    : To supply the current necessity of the Tinn-R project
# Date         : 09/03/2008 1:25 PM
#=======================================================================

trExport.matrix <-
function(x, type="raw", file, append=FALSE, objname=deparse(substitute(x)), ...)
{
  if(is.null(file) || file == "clipboard")
    # Make sure we do not append to the clipboard!
    append <- FALSE
  objname <- objname
  # Compute the expression
  xexp <- try(ifelse(inherits(x, "expression"), x, NULL), silent=TRUE)
  if(inherits(xexp, "try-error") || is.null(xexp)) {
    xexp <- substitute(x)
    # To make sure that non conventional names will be correctly evaluated,
    # we use backticks!
    if(is.character(xexp))
      xexp <- parse(text=paste("`", xexp, "`", sep=""))
    xexp <- as.expression(xexp)
  }
  res <- export.data.frame(x=xexp, type=type, file=file, append=append,
                           objname=objname, ...)
  ifelse(type == "typelist", return(res), invisible(res))
}
