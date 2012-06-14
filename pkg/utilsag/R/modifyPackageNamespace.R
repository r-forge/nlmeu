  

modifyPackageNamespace <- function(fun, modifyList = list()){
 # Modify namespace of the package containing function fun
 # Usage:
 # modify.nlmeNamespaceList <- list("model.matrix.reStruct" =  nlmeU:::model.matrix.reStruct.U) 
 # 
 # modifyPackageNamespace(nlme::lme, modify.nlmeNamespaceList) # nlme::lme allows to extract namespace
 #
 if (length(modifyList) == 0) return(cat("modifyList is empty. No changes made. \n"))
 envc <- as.character(capture.output(environment(fun)))
 print(envc)
  
 i0 <-max(gregexpr(":", envc, fixed=TRUE)[[1]])
 i1 <-as.numeric(regexpr(">", envc, fixed=TRUE))
 pkgnm  <- substr(envc, i0+1, i1-1)
 cat("Namespace ", envc, "enclosed in package ", pkgnm, " will be modified \n")
 cat("To restore previous namespace use command: unloadNamespace(", pkgnm, ") \n", sep="")
 readline("Press <Enter> to continue or <Esc> to abort \n")

MODIFYns <- 
  function(el){
  value <- modifyList[[el]]
  nm <- nms[el]
   
  ## ns: environment class, methods:= as.list(ns), 579 components/functions.
  ## env:  579 components
  vnm <- deparse(substitute(value))
  ###cat(paste("{", nm, "} <- {", vnm, "} \n", sep=""))
  ns  <- loadNamespace(pkgnm)  # "nlme"
  env <- environment(fun)
  unlockBinding(nm, env)
  environment(nm) <- environment(value) <- env
  assign(nm, value, envir=env)
  assignInNamespace(nm, value, ns= ns, envir = env)    
  lockBinding(nm, env) 
 }
  nms <- names(modifyList)
  sapply(as.list(1:length(modifyList)), MODIFYns)
}

