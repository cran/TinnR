#======================================================================= 
# Orinal author: Philippe Grosjean 
# Original code: [svIO] export.data.frame function 
# Adapted by   : Jose Claudio Faria 
# Objective    : To supply the current necessity of the Tinn-R project 
# Date         : 09/03/2008 1:25 PM 
#======================================================================= 
 
trExport.data.frame <- 
function(x, type="raw", file=NULL, append=FALSE, 
objname=deparse(substitute(x)), ...) 
{ 
  exportASCII <-
  function(x, file, append, ...) 
  { 
    col1 <- dimnames(x)[[1]] 
    for (i in 1:ncol(x)) 
      col1 <- paste(col1, x[, i], sep="\t") 
    col1 <- paste(col1, collapse="\n") 
    row1 <- c("", dimnames(x)[[2]]) 
    txt <- paste(row1, collapse="\t") 
    txt <- paste(txt, col1, sep="\n") 
    # write code to file connection 
    ifelse(file != "", tmpfile <- file(file, c("w","a")[append+1]), 
      tmpfile <- file) 
    cat(txt, file=tmpfile, ...) 
    if(file != "") 
      close(tmpfile) 
    invisible(return(txt)) 
  } 
 
  if(is.null(file) || file == "clipboard") 
    # Make sure we do not append to the clipboard! 
    append=FALSE 
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
  # Process the command in the standard function 
  x <- eval(xexp, envir=.GlobalEnv) 
  res <- switch(type, 
    "typelist" = unique(c("raw", "ascii", "html", "latex", 
                          listCustoms("export", "data.frame"))), 
    "ascii"    = exportASCII(x, file, append, ...), 
    export.default(x=xexp, type=type, file=file, append=append, ...)) 
  ifelse(type == "typelist", return(res), invisible(res)) 
} 
