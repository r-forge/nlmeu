## Generic functions exported. See NAMESPACE for list of methods

logLik1 <-  function(modfit, dt1, dtInit) UseMethod("logLik1")
Pwr <-  function(object, ...) UseMethod("Pwr")
simulateY <- function(object, nsim = 1, seed = NULL, ...) UseMethod("simulateY")
sigma <-  function(object, ...) UseMethod("sigma")
sigma.default <- function(object, ...) object$sigma



"missPat" <- function(...){
     args  <- as.list(substitute(list(...)))[-1]
     args.t <- paste("miss.frame$",args,sep="",collapse=",")
     miss.frame <- as.data.frame(ifelse(is.na(data.frame(args)),"X","-"))
     ## Try do.call("paste", c(miss.frame,sep=""))
     txt <- c("paste(", args.t, ",sep='')")
     txt<-paste(txt,sep='')
     eval(parse(text=txt))
}





runScript <- function(script= NA,  package = "nlmeU", subdir = "scripts", echo = TRUE){
    scriptsDir <- system.file(subdir, package = package)
    scriptsList <- list.files(scriptsDir)
    scriptFile <- file.path(scriptsDir, script)
    if (!(script %in% scriptsList)) {
    if (is.na(script)) {
            errFun <- message
            errMsg <- ""
        }
        else {
            errFun <- stop
            errMsg <- paste("Example", example, "does not exist. ")
        }
        errFun(errMsg, "Valid scripts are \"", paste(scriptsList, 
            collapse = "\", \""), "\"")
    }
    else {
        sourceText <- source(scriptFile, echo=echo)
        sourceText
    }
}




## Could not import generic functions from nlme (Aug 2012) 
## Had to to define them explicitly
"coef<-" <- function (object, ..., value) UseMethod("coef<-")
corMatrix <- function (object, ...) UseMethod("corMatrix")
isInitialized <- function (object) UseMethod("isInitialized")
"matrix<-" <- function (object, value) UseMethod("matrix<-")
logDet <- function (object, ...) UseMethod("logDet")
Names <- function (object, ...)  UseMethod("Names")
"Names<-" <- function (object, ..., value) UseMethod("Names<-")
pdConstruct <- function (object, value, form, nam, data, ...) UseMethod("pdConstruct")
pdFactor <- function (object) UseMethod("pdFactor")
pdMatrix <- function (object, factor = FALSE) UseMethod("pdMatrix")
VarCorr <- function (x, sigma = 1, rdig = 3) UseMethod("VarCorr")
