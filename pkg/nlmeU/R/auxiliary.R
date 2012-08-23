
"missPat" <- function(...){
     args  <- as.list(substitute(list(...)))[-1]
     args.t <- paste("miss.frame$",args,sep="",collapse=",")
     miss.frame <- as.data.frame(ifelse(is.na(data.frame(args)),"X","-"))
     ## Try do.call("paste", c(miss.frame,sep=""))
     txt <- c("paste(", args.t, ",sep='')")
     txt<-paste(txt,sep='')
     eval(parse(text=txt))
}
sigma <-  function(object, ...) UseMethod("sigma")

sigma.default <- function(object, ...) object$sigma

.traceFunction <- function(...) {  # Place holder
}

## Could not import generic functions from nlme(Aug 2012) 
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