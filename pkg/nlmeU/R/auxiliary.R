
### Generic functions

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
    scriptsList <- list.files(scriptsDir, pattern = "[[:alnum:]][.][R]$")
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
        errFun(errMsg, "Valid scripts are: \"", paste(scriptsList, 
            collapse = "\", \n"), "\"")
    }
    else {
        sourceText <- source(scriptFile, echo=echo)
        sourceText
    }
}



meltx <-
function (data, ...) 
{
    data.frame(value = data)
}
