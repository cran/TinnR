#======================================================================= 
# Orinal author: Philippe Grosjean 
# Original code: [svIDE] Startup function 
# Adapted by   : Jose Claudio Faria 
# Objective    : To supply the current necessity of the Tinn-R project 
# Date         : 02/08/2009 9:44 AM 
#======================================================================= 
 
trStartIDE <- function() 
{ 
  # If an IDE is defined, start it now 
  IDE <- getOption("IDE") 
  if (!is.null(IDE) && file.exists(IDE)) 
    system(paste("\"", IDE, "\"", sep=""), wait=FALSE) 
}
