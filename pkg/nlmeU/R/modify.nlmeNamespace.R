modify.nlmeNamespace <- function(fun = nlme::lme, 
        modifyList = list("model.matrix.reStruct" = model.matrix.reStruct.U) ){
##  Syntax: modify.nlmeNamespace() 
##  library(nlme)
##  ....
##  detach(package:nlme)
##  unloadNamespace("nlme")
 
 funNm <- "modify.nlmeNamespace"
 hl <-  options()$traceR            # List
 htrace <- hl[["modify.nlmeNamespace"]]   # Function. Change Function name, after cutting and pasting!!!               
 if (is.null(htrace)) htrace <- attr(hl, "default")
 .traceR <- if (is.null(htrace))  function(id, xp , funNm, msg, lbl){ } else  htrace 
 

 
 .traceR(1, , funNm, "modify.nlmeNamespace STARTS =======")
 if ("package:nlme" %in% search()) detach("package:nlme")
 cat("Namespace of nlme package will be modified \n")
 cat('To restore previous namespace use command: unloadNamespace("nlme") \n', sep="")
 readline("Press <Enter> to continue or <Esc> to abort \n")
 pkgnm <- "nlme"
 value <- modifyList[[1]]
 .traceR(3, value, funNm)
 nm <- "model.matrix.reStruct"
 ns  <- loadNamespace(pkgnm)  # "nlme"
 env <- environment(fun)
 .traceR(5, as.list(env)[[nm]], funNm)
 unlockBinding(nm, env)
 environment(nm) <- environment(value) <- env
 assign(nm, value, envir=env)
 assignInNamespace(nm, value, ns= ns, envir = env)    
 lockBinding(nm, env) 
 .traceR(7, as.list(env)[[nm]], funNm)
 .traceR(1,  , funNm, "modify.nlmeNamespace ENDS =======")
 return(invisible())
 }
