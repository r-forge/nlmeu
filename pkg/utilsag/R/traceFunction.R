##
## Template functions for options(traceR =
## See tests/traceR.R for examples
##


traceDefault <- function(id, xp, funNm, msg, lbl){}

tracePrint <- function(id, 
  xp = NULL,                       
  funNm = character(0),
  msg = paste("Trace ", lbl, " executed", sep = ""),
  lbl = as.character(id)
){ 
 tt <- paste(funNm, lbl,  sep = "_") 
 tt <- paste("---", tt, ". Msg: ", msg,". \n", sep = "")
 cat(tt)                 
 if (!is.null(xp)) print(xp)                  
}

traceList <- function(id, 
   xp = NULL, 
   funNm = character(0),
   msg = character(0),
   lbl = as.character(id)    
 ){
## .traceList <- list()
if (is.null(xp)) xp <- paste("Msg: ",  msg, sep = "")
objL <- list(xp)  
if (mode(xp) == "NULL") {   # For example objects returned by str()
 objL <- list("Object of mode NULL. Consider to use capture.output(object)" )
 
}

names(objL) <- paste(funNm, lbl, sep = ":") 
assign(".traceList", c(.traceList, objL), envir =.GlobalEnv)
}

