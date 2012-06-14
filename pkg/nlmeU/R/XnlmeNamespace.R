modify.nlmeNamespaceList <- list(
   "model.matrix.reStruct" =  utilsag:::model.matrix.reStruct.U
)
   

modifyPackageNamespace <- function(fun, modifyList = list()){
 # Modify enclosing namespace containing function fun
 if (length(modifyList) == 0) return(cat("modifyList is empty. No changes made. \n"))
 envc <- as.character(capture.output(environment(fun)))
 print(envc)
 pkgnm <- "nlme"
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
  #MODIFY1Ns("model.matrix.reStruct", utilsag:::model.matrix.reStruct.U)   
  nms <- names(modifyList)
  sapply(as.list(1:length(modifyList)), MODIFYns)
}

modifyPackageNamespace(nlme::lme, modify.nlmeNamespaceList)


sessionInfo()
capture.output(getS3method("model.matrix","reStruct"))[1:5]

unloadNamespace("nlme")
capture.output(getS3method("model.matrix","reStruct"))[1:5]
