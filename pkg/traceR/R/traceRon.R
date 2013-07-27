traceR.on <- function(
  ...,                
  defaultOptions  = list()
){
## Examples of ... arguments
##  TF1 = list(id = c(1, 2))
##  TF2 = list(id = c(1,70)),
##- Examples of default argument
## list(prefix = "x_", fun =.traceREnv)

 dots <- as.list(substitute(list(...)))[-1L]
 ## print(str(dots))
 tracex <- lapply(dots, eval)
 
 ## Create attRX list with attributes
 attrX <- defaultOptions
 dnms <- names(defaultOptions)
 names(attrX) <- dnms
 
 nmsdots <- names(dots)
 ## print(nmsdots)
 ## if (!is.null(nmsdots)) attrX <- c(attrX, list(nmsdots = nmsdots))
 
 dinit <- defaultOptions[["fun"]]
 if (is.null(dinit)) attrX <- c(attrX, fun = eval(.traceRdump, envir = .GlobalEnv)) # eval(traceR:::x)
 
 tmp <- defaultOptions[["asList"]]
 if (is.null(tmp)) attrX <- c(attrX, asList = FALSE)
 
 prfx <- defaultOptions[["prefix"]]
 if (is.null(prfx)) attrX <- c(attrX, prefix = "e_")
 
 mapNms <- defaultOptions[["mapVars"]]
 mapnms <- c("recno", "fLbl", "id", "idLbl", "fTree", "env", "nObjAll", "nObj", "store", "first", "auto")
 
 if (is.null(mapNms)){
      pos <- length(attrX) + 1
      attrX[[pos]] <- mapnms
      names(attrX)[pos] <- "mapVars"
 }
      
 
 mapPrint <- defaultOptions[["mapPrint"]]
 mapPrintDefault <- c("recno", "fLbl", "id", "idLbl", "fTree", "env", "nObjAll", "nObj", "store", "first", "auto")
 
 if (is.null(mapPrint)){
    pos <- length(attrX) + 1
    attrX[[pos]] <- mapPrintDefault
    names(attrX)[pos] <- "mapPrint"
}
 attributes(tracex) <- attrX
 
 assign(".traceRmap", data.frame(character(0)), envir = .GlobalEnv) 
 cat("- .traceRmap initialized \n") 
 names(tracex) <- nmsdots
 options(traceR = tracex)
 invisible(tracex)
}
