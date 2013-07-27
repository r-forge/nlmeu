traceReditor <- function (fun, lbl, idx)
{    # function annotates fun with .traceR statements
  if (!is.null(attr(fun, "locked"))){
  message ("Function: ", lbl,  " already annotated. No changes made.")
  return(invisible(fun))
}
curle_brackect_symbol <- as.symbol("{")
out0 <- list(
     quote(`{`),
     substitute(.functionLabel <- tx, list(tx = lbl)),
     quote(.traceR <- attr(options()$traceR, "fun")),
     quote(.traceR <- if (is.null(.traceR)) function(...){} else .traceR)
)

b_f <- body(fun)

if (is.null(b_f)) stop("Body of ", fun, " is NULL.") 
 
L <- if (is.symbol(b_f)){
        # print(paste("-- is.symbol", lbl))
        c(curle_brackect_symbol, b_f)
      } else {
        if (as.character(b_f[1]) != curle_brackect_symbol){
          # print(paste("-- curle bracket", lbl))
          c(curle_brackect_symbol, b_f)
          } else {
          as.list(b_f)
      }}
          
T <- lapply(1:length(L), function(el){
        ix <- (idx *100 + el)/100
        lblx <- as.character(as.expression(L[[el]]))
        if (el == 1) {   
        substitute(.traceR(ix, lbl, first = TRUE, auto = TRUE), list(ix = ix, lbl = lblx))
        } else {
        substitute(.traceR(ix, lbl, auto = TRUE), list(ix = ix, lbl = lblx ))
        }})
     
out <- vector("list", 2 * length(L)-2 )
for (i in seq_along(L)) {
  if (i > 1) out[2*i-2] <- L[i]
  if (i < length(L)) out[2*i-1]  <- T[i] 
}

funR <- fun
lx <- c(out0,out)
body(funR) <- as.call(lx)
attr(funR, "locked") <- TRUE
attr(funR, "oldFun") <- fun
funR     
}


traceReditf <- function(x, lbl = ".", ...){
   res <- traceReditor(x, lbl  = lbl, ...)
   res
}



traceReditFUN_ns <- function(cx = NULL, ns, pos = -1, envir = as.environment(pos), verbose = FALSE){
## cx  is character vector containing object names (only *functions* in namespace ns will be annotated)
    
   if (missing(ns)) {
        nm <- attr(envir, "name", exact = TRUE)
        if (is.null(nm) || substring(nm, 1L, 8L) != "package:") 
            stop("environment specified is not a package")
        ns <- asNamespace(substring(nm, 9L))
   } else ns <- asNamespace(ns)
  
  if (is.null(cx))   cx  <-  ls(ns)  # By default all functions in ns
 
  idx <-  1:length(cx)


  for (i in seq_along(cx)){ 
     subx <- substitute(x, list(x = cx[i]))
    if (is.name(subx))  subx <- deparse(subx)
 
    if (!is.character(subx) || length(subx) != 1L) 
        stop("'fixInNamespace' requires a name")
        
    gsubx <- get(subx, envir = ns, inherits = FALSE)

    if (isFunctionClass(gsubx)){
    x <- traceReditor(gsubx, lbl = cx[i], idx = idx[i])
    if (verbose) message("Note:", subx, "function was annotated.")
    assignInNamespace(subx, x, ns)
    } else if (verbose) message("Note: ",  subx, " is of ", class(gsubx)[1], "class and it was NOT annotated")
}
invisible(cx)
}

isFunctionClass <- function(f) class(f)[1] == "function" 

traceRedit <- function(cx = NULL, ns, pos = -1, envir = as.environment(pos), verbose = FALSE){

  traceReditFUN_ns(cx, ns, pos = -1, envir = as.environment(pos), verbose = verbose)

}





  

