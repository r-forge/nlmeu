traceR.off <- function(){

 if (exists(".traceRmap", envir = .GlobalEnv)){
   res  <- .traceRmap$res
   rnms <- res[nchar(res)>0]
   rnms <- intersect(rnms, Objects()) 
   if (length(rnms)) {
     Remove(list = rnms)   
     cat("- ", length(rnms), " Objects removed from .R_Cache\n", sep="") 
   }
   remove(.traceRmap, envir = .GlobalEnv)
   }
   options(traceR = NULL)
 }
