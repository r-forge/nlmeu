
### Generic functions

logLik1 <-  function(modfit, dt1, dtInit) UseMethod("logLik1")
Pwr <-  function(object, ...) UseMethod("Pwr")
simulateY <- function(object, nsim = 1, seed = NULL, ..., verbose = FALSE, sigma) UseMethod("simulateY")
sigma <-  function(object, ...) UseMethod("sigma")




sigma.default <- function(object, ...) object$sigma

"missPat" <- function(..., symbols = c("X","-"), collapse = NULL, missData = FALSE){
     funNm <- "missPat"
     .traceRinit <- attr(options()$traceR, "init")
     .traceR <-   if (is.null(.traceRinit))
     function(...){} else .traceRinit(funNm) 
     .traceR(1, "missPat STARTS", funNm, msg = TRUE)
     args  <- as.list(substitute(list(...)))[-1]
     .traceR(10, args, funNm)  
     argsL <- lapply(args, eval)
     dt <- data.frame(argsL)
     nms <- lapply(args, FUN= function(el){
       elx <- eval(el)
       if (is.null(colnames(elx))) {
        nc <- ncol(elx)
        if (is.null(nc)) as.character(el) else paste(el, 1:nc, sep = ":")
     }   
         else colnames(elx)
     })
     cx1 <- symbols[1]
     cx2 <- symbols[2]
     miss.frame <- as.data.frame(ifelse(is.na(dt), cx1, cx2))
     names(miss.frame) <- unlist(nms)
     .traceR(12, miss.frame, funNm) 
     res <- apply(miss.frame, 1, paste, collapse = collapse)
     attr(res, "cnames") <- unlist(nms)
     if (missData) attr(res, "missData") <- miss.frame
     .traceR(1, "missPat ENDS", funNm, msg = TRUE)
     res
}



runScript <- function(script= NA,  package = "nlmeU", subdir = "scriptsR15.0", echo = TRUE){
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
        errFun(errMsg, "Valid scripts in ", scriptsDir, " are: \n", paste("\"",scriptsList, 
            collapse = "\", \n", sep=""), "\"")
    }
    else {
        sourceText <- source(scriptFile, echo=echo)
        sourceText
    }
}



