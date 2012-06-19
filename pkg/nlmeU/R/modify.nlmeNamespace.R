modify.nlmeNamespace()<- function{
 cat("Namespace of nlme package will be modified \n")
 cat('To restore previous namespace use command: unloadNamespace("nlme") \n', sep="")
 readline("Press <Enter> to continue or <Esc> to abort \n")
 pkgnm <- "nlme"
 value <- list("model.matrix.reStruct" =  nlmeU:::model.matrix.reStruct.U) 
 nm <- "model.matrix.reStruct"
 ns  <- loadNamespace(pkgnm)  # "nlme"
 env <- environment(nlme::lme)
 unlockBinding(nm, env)
 environment(nm) <- environment(value) <- env
 assign(nm, value, envir=env)
 assignInNamespace(nm, value, ns= ns, envir = env)    
 lockBinding(nm, env) 
)
