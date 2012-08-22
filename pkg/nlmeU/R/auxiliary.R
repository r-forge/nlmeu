
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