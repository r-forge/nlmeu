modify.nlmeNamespace <- function(fun = nlme::lme, 
        modifyList = list("model.matrix.reStruct" = model.matrix.reStruct.U) ){
##  Syntax: modify.nlmeNamespace() 
##  library(nlme)
##  ....
##  detach(package:nlme)
##  unloadNamespace("nlme")
 
 funNm <- "modify.nlmeNamespace"
 Xverbose(1, "modify.nlmeNamespace STARTS =======", funNm)
 if ("package:nlme" %in% search()) detach("package:nlme")
 cat("Namespace of nlme package will be modified \n")
 cat('To restore previous namespace use command: unloadNamespace("nlme") \n', sep="")
 readline("Press <Enter> to continue or <Esc> to abort \n")
 pkgnm <- "nlme"
 value <- modifyList[[1]]
 Xverbose(3, cat(head(capture.output(value)), sep="\n"), funNm)
 nm <- "model.matrix.reStruct"
 ns  <- loadNamespace(pkgnm)  # "nlme"
 env <- environment(fun)
 Xverbose(5, cat(tail(capture.output(as.list(env)[[nm]])), sep = "\n"), funNm)
 unlockBinding(nm, env)
 environment(nm) <- environment(value) <- env
 assign(nm, value, envir=env)
 assignInNamespace(nm, value, ns= ns, envir = env)    
 lockBinding(nm, env) 
 Xverbose(7, cat(tail(capture.output(as.list(env)[[nm]])), sep = "\n"), funNm)
 Xverbose(1, "modify.nlmeNamespace ENDS =======", funNm)
 return(invisible())
 }
