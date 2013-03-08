modifyPackageNamespace <- function(fun, modifyList = list()){
 ## Modify namespace of the package containing function fun
 ## Usage:
 ## lme <- function(){"DUMMY FUNCTION"}  # Needed!!! ERROR otherwise
 ## modify.nlmeNamespaceList <- list(
 ##   "model.matrix.reStruct" =  nlmeU:::model.matrix.reStruct.U,
 ##   "lme.formula" = lme.formula.U,     # Insert utilsag:::
 ##   "lmeControl" =  lmeControl.U) 
 ##    
 ##  modifyPackageNamespace(nlme::nlme, modify.nlmeNamespaceList) # nlme::lme allows to extract namespace
 ##  rm(lme)  !!!
 ##  library(nlme)
 ##  detach(package:nlme)
 ##  unloadNamespace("nlme")
 
 if (length(modifyList) == 0) return(cat("modifyList is empty. No changes made. \n"))
 envc <- as.character(capture.output(environment(fun)))
 print(envc)
  
 i0 <- max(gregexpr(":", envc, fixed=TRUE)[[1]])
 i1 <-as.numeric(regexpr(">", envc, fixed=TRUE))
 pkgnm  <- substr(envc, i0+1, i1-1)
 pkgstr <- paste("package:", pkgnm, sep="")
 if (pkgstr %in% search()) detach(pkgstr, character.only= TRUE)
 cat("Namespace ", envc, "enclosed in package ", pkgnm, " will be modified \n")
 cat("To restore previous namespace use command: unloadNamespace(", pkgnm, ") \n", sep="")
 readline("Press <Enter> to continue or <Esc> to abort \n")

MODIFYns <- 
  function(el){
  value <- modifyList[[el]]
  
  nm <- nms[el]
  fnm <- "modifyPackageNamespace"
  
  .traceRinit <- attr(options()$traceR, "init")
  .traceR <- if (is.null(.traceRinit))
      function(...){} else .traceRinit(fnm) 

  .traceR(101, "MODIFYns called from modifyPackageNamespace STARTS here", fnm, msg = TRUE)
  .traceR(102, nm, fnm)
  .traceR(103, cat(head(value), sep="\n"), fnm)
  .traceR(104, cat(tail(value), sep="\n"), fnm)
  ## ns: environment class, methods:= as.list(ns), 579 components/functions in nlme package.
  ## env:  579 components
  vnm <- deparse(substitute(value))
  ###cat(paste("{", nm, "} <- {", vnm, "} \n", sep=""))
  .traceR(111, pkgnm, fnm)
  ns  <- loadNamespace(pkgnm)  # "nlme"
  nsF <- as.character(capture.output(ns))
  .traceR(115, nsF, fnm) 
  env <- environment(fun)
  envF <- as.character(capture.output(env))
  .traceR(121, envF, fnm)  
  unlockBinding(nm, env)
  .traceR(125, "Binding unlocked", fnm) 
  environment(nm) <- environment(value) <- env
  .traceR(130, "New values for env(nm), env(value)" , fnm, msg = TRUE) 
  assign(nm, value, envir = env)
  
  .traceR(135, "Assigned", fnm)
  assignInNamespace(nm, value, ns= ns, envir = env) 
  .traceR(140, "done assignInNamespace", fnm, msg = TRUE)
  lockBinding(nm, env) 
 }
  nms <- names(modifyList)
  .traceR(1, "MODIFYns called from modifyPackageNamespace ENDS here", fnm, msg = TRUE)

  sapply(as.list(1:length(modifyList)), MODIFYns)

  # print(tail(as.list(ns)[[nm]]))  

}

## 
## unloadNamespace("nlme")
## lme <- function(){}  # Dummy function. Remove later
## modify.nlmeNamespaceList <- list(
##     "model.matrix.reStruct" =  nlmeU:::model.matrix.reStruct.U,
##     "lme.formula" = lme.formula.U,     # Insert utilsag:::
##     "lmeControl" =  lmeControl.U) 

## modifyPackageNamespace(nlme:::lme, modify.nlmeNamespaceList)
## nlme:::lmeControl
