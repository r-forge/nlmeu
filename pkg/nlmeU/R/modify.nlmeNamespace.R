modify.nlmeNamespace <- function(fun = nlme::lme, 
        modifyList = list("model.matrix.reStruct" = model.matrix.reStruct.U) ){
##  Syntax: modify.nlmeNamespace() 
##  library(nlme)
##  ....
##  detach(package:nlme)
##  unloadNamespace("nlme")
 
 funNm <- "modify.nlmeNamespace"
 .traceFunction(1, "modify.nlmeNamespace STARTS =======", funNm, tags = c("1","msg"))
 if ("package:nlme" %in% search()) detach("package:nlme")
 cat("Namespace of nlme package will be modified \n")
 cat('To restore previous namespace use command: unloadNamespace("nlme") \n', sep="")
 readline("Press <Enter> to continue or <Esc> to abort \n")
 pkgnm <- "nlme"
 value <- modifyList[[1]]
 .traceFunction(3, value, funNm, alt= cat(head(capture.output(value)), sep="\n"))
 nm <- "model.matrix.reStruct"
 ns  <- loadNamespace(pkgnm)  # "nlme"
 env <- environment(fun)
 .traceFunction(5, as.list(env)[[nm]], funNm, alt = cat(tail(capture.output(as.list(env)[[nm]])), sep = "\n"))
 unlockBinding(nm, env)
 environment(nm) <- environment(value) <- env
 assign(nm, value, envir=env)
 assignInNamespace(nm, value, ns= ns, envir = env)    
 lockBinding(nm, env) 
 .traceFunction(7, as.list(env)[[nm]], funNm, alt = cat(tail(capture.output(as.list(env)[[nm]])), sep = "\n"))
 .traceFunction(1, "modify.nlmeNamespace ENDS =======", funNm, tags =c("1", "msg"))
 return(invisible())
 }
