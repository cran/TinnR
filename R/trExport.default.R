#=======================================================================
# Orinal author: Philippe Grosjean
# Original code: [svIO] export.default function
# Adapted by   : Jose Claudio Faria
# Objective    : To supply the current necessity of the Tinn-R project
# Date         : 09/03/2008 1:25 PM
#=======================================================================

trExport.default <-
function(x, type="raw", file, append=FALSE, objname=deparse(substitute(x)), ...)
{
  require(R2HTML)
  # HTML sub-function
  exportHTML <-
  function(x, file, append, ...) {
    ifelse(file != "", tmpfile <- file(file, c("w", "a")[append + 1]),
      tmpfile <- file)
    HTML(x, file=tmpfile, ...)
    if(file != "")
      close(tmpfile)
    invisible(return(TRUE))
  }

  require(Hmisc)
  # LaTeX sub-function
  exportLaTeX <- 
  function(x, file, append, ...) {
    ifelse(file != "", tmpfile <- file(file, c("w", "a")[append+1]),
      tmpfile <- file)
    latex(x, file=tmpfile, ...)
    if(file != "")
      close(tmpfile)
    invisible(return(TRUE))
  }

  # ASCII sub-function
  exportASCII <- 
  function(x, file, append, ...) {
    treated <- FALSE
    if(is.vector(x)) {
      txt <- paste(x, collapse = "\t")
      treated <- TRUE
    }
    if(is.matrix(x)) {
      txt <- as.character(x)
      txt <- paste(apply(x, 1, FUN=paste, collapse="\t"), collapse="\n")
      treated <- TRUE
    }
    if(!treated) {
      tmpfile <- tempfile()
      sink(tmpfile)
      print(x)
      sink()
      txt <- readLines(tmpfile)
      txt <- paste(txt, collapse="\n")
    }

    ifelse(file != "", tmpfile <- file(file, c("w", "a")[append+1]),
      tmpfile <- file)
    cat(txt, file=tmpfile, ...)
    if(file != "")
      close(tmpfile)
    invisible(return(TRUE))
  }

  # RAW sub-function
  exportRaw <- 
  function(x, file, ...) {
    ifelse(file != "", tmpfile <- file(file, c("w", "a")[append+1]),
      tmpfile <- file)
    dput(x, file=tmpfile)
    if(file != "")
      close(tmpfile)
    invisible(return(TRUE))
  }

  # Main function
  if(is.null(file) || file == "clipboard")
    # Make sure we do not append to the clipboard!
    append <- FALSE
  objname <- objname

  # Compute the expression
  xexp <- try(ifelse(inherits(x, "expression"), x, NULL), silent=TRUE)
  if(inherits(xexp, "try-error") || is.null(xexp)) {
    xexp <- substitute(x)
    # To make sure that non conventional names will be correctly evaluated, we use backticks!
    if(is.character(xexp))
      xexp <- parse(text=paste("`", xexp, "`", sep=""))
    xexp <- as.expression(xexp)
  }

  # Process the command in the standard function 
  x <- eval(xexp, envir=.GlobalEnv)
  res <- switch(type,
    "typelist"= unique(c("raw", "ascii", "html", "latex",
                         listCustoms("export", "default"))),
    "html"    = exportHTML(x, file, append, ...),
    "latex"   = exportLaTeX(x, file, append, ...),
    "ascii"   = exportASCII(x, file, append, ...),
    exportRaw(x, file, append, ...))

  ifelse(type == "typelist", return(res), invisible(res))
}
