MODIFY1.nlmeNs <- 
  function(nm, value  
   ){
  ## ns: environment class, methods:= as.list(ns), 579 components/functions.
  ## env:  579 components
  vnm <- deparse(substitute(value))
  packageStartupMessage (paste("{", nm, "} <- {", vnm, "}", sep=""), appendLF = TRUE)
  ns  <- loadNamespace("nlme")
  env <-  environment(nlme::nlme)
  unlockBinding(nm, env)
  environment(nm) <- environment(value) <- env
  assign(nm, value, envir=env)
  assignInNamespace(nm, value, ns=ns, envir = env)    
  lockBinding(nm, env) 
 }

.onLoad <- function(libname, pkgname){
  packageStartupMessage("NOTE: Namespace of the nlme package has been temporarily modified!", appendLF = TRUE)  
  packageStartupMessage("To reinstate it, use detach(package:nlmeU) command.", appendLF = TRUE)  
  print(libname)
  print(pkgname)
  MODIFY1.nlmeNs("model.matrix.reStruct", model.matrix.reStruct.U) # Change utilsag to nlmeU later
  # MODIFY1.nlmeNs("lmeControl", nlmeU::lmeControl.U)
  # MODIFY1.nlmeNs("lme.formula", nlmeU::lme.formula.U)
  # print(as.list(ns)[["model.matrix.reStruct"]])
  #unloadNamespace("nlme") 
}

.Last.lib <- function(libpath) {
  print(libpath)
  unloadNamespace("nlme")
}
