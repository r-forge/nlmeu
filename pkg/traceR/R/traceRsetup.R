traceRsetup<- function(
  ...,                
  defaultOptions  = list()
){
## Examples of ... arguments
##  TF1 = list(id = c(1, 2), xin = "x"),
##  TF2 = list(id = c(1,70), xin = c("x","y"), xout ="zz1"),
##- Examples of default argument
## list(prefix = "x_")
 dots <- as.list(substitute(list(...)))[-1L]
 ## print(str(dots))
 traceR <- lapply(dots, eval)
 
 ## Create attRX list with attributes
 attrX <- defaultOptions
 dnms <- names(defaultOptions)
 names(attrX) <- dnms
 
 nmsdots <- names(dots)
 ## print(nmsdots)
 ## if (!is.null(nmsdots)) attrX <- c(attrX, list(nmsdots = nmsdots))
 
 dinit <- defaultOptions[["init"]]
 if (is.null(dinit)) attrX <- c(attrX, init = eval(.traceRdefault, envir = .GlobalEnv)) # eval(traceR:::x)
 
 prfx <- (length(dnms["prefix"]) == 0)
 if (prfx) attrX <- c(attrX, prefix = "r_")
 
 attributes(traceR) <- attrX
 # c(traceR, default = attrX)
 # traceR Ok
  pfx <- attrX[["prefix"]]
 
 tt <- defaultOptions[["map"]]
 if (is.null(tt))  assign(".traceRmap", data.frame(character(0)), envir = .GlobalEnv)
 cat("- .traceRmap initiated \n") 
 names(traceR) <- nmsdots
 options(traceR = traceR)
 invisible(traceR)
}
