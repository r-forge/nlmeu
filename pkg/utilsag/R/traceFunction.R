##
## Template functions for options(traceR =
## See tests/traceR.R for examples
##
   
traceRinit <- function(flbl){      # Function label/name
   hl <-  options()$traceR         # List
   htrace <- hl[[flbl]]           # Trace function
   if (is.null(htrace)) htrace <- attr(hl, "default")
   .traceR <- if (is.null(htrace))  function(...){ } else  htrace 
   if (is.null(htrace) ) function(...){} else htrace
 }

traceRdefault<- function(...){}
        
## Auxiliary functions

traceRinfoAux <- function(
 id, # Not used by this function
 flbl, msg, lbl 
){ 
 tt <- paste(flbl, lbl,  sep = "_") 
 paste("---", tt, ". Msg: ", msg,". \n", sep = "")                    
}

traceRselect <- function(id, lbl, flbl){
## Selected traces based on id and lbl
ids <- attr(options()$traceR, "id")
ids <- if(is.list(ids))  ids[[flbl]] else ids
lbls <- attr(options()$traceR, "lbl")
lbls <- if(is.list(lbls)) lbls[[flbl]] else lbls 
opt <- attr(options()$traceR, "option")[[flbl]]  # Default option is :and: 
sel1 <- id %in% ids || is.null(ids)
sel2 <- lbl %in% lbls || is.null(lbls)
xor <-  sel1 || sel2
xand <-  sel1 && sel2
sel <- if(is.null(opt)) xand else xor  
return(sel)
}

## Main traceR functions: Need to have: id, object, flbl, msg, lbl arguments.

traceRprint1 <- function(id, object = NULL, flbl, 
 msg =paste("Trace ", lbl, " executed", sep = ""),
 lbl = as.character(id)){ 
## All traces will be printed
info <- utilsag:::traceRinfoAux(id, flbl, msg, lbl)
cat(info)
if (!is.null(object)) print(object)
}

traceRprint <- function(id, object = NULL, flbl, 
 msg = paste("Trace ", lbl, " executed", sep = ""), 
 lbl = as.character(id)){ 
sel <- utilsag:::traceRselect(id,lbl, flbl)
if (sel ) traceRprint1(id, object, flbl, msg, lbl)
}


traceRlist1 <- function(id, 
object = NULL, 
flbl,
msg = character(0),
lbl = as.character(id)    
 ){
if (is.null(object)) object <- paste("Msg: ",  msg, sep = "")
objL <- list(object)  
if (mode(object) == "NULL") {   # For example objects returned by str()
 objL <- list("Object of mode NULL. Consider to use capture.output(object)" )
 
}
names(objL) <- paste(flbl, lbl, sep = ":") 
assign(".traceRlist", c(.traceRlist, objL), envir =.GlobalEnv) 
}
 

traceRlist <- function(id, 
object = NULL, 
flbl,
msg = character(0),
lbl = as.character(id)    
 ){
## .traceRlist <- list()
sel <- utilsag:::traceRselect(id, lbl, flbl)
if (sel ) traceRlist1(id, object, flbl, msg, lbl)
}

