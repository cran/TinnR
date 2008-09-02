#=======================================================================
# Orinal author : Philippe Grosjean
# Original code : [svMisc] objSearch function
# Adapted by    : Jose Claudio Faria
# Objective     : To supply the current necessity of the Tinn-R project
# Date          : 09/03/2008 1:25 PM
#=======================================================================

trObjSearch <-
function(sep='\t', path=NULL)
{
  res <- data.frame(search())
  if(is.null(path))
    return(res)
  else
    write.table(res, file=path,row.names=FALSE, quote=FALSE, sep=sep)
}
