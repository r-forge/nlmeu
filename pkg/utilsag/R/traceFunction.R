traceOptions <- function(        # Default control list for *all* functions
          xtrace = "traceList",  # Dispatches traceList() or tracePrint() with (potentially) alternative main argument   
          xuid   = numeric(),    # numeric() for all ids
          xtags  = character(),  # All tags are allowed
          funL  = list()         # *Named* list for tested functions similar to:
                                 ##- list(testedFun = list(xid = -1, xtags = c("START","return","exit")))
){
## Function returns default Control list for traceFun function 
## This list is returned by options()$trace
## Syntax: 
##  trops <- traceOptions()
##  options(trace = trops)
##  options()$trace

Cntrl <- list(xtrace = xtrace, xuid = xuid,  xtags = xtags)
#print("here")
#print(cntrl)
if (length(funL)) res <- lapply(funL,  # Go through functions and modify funL
  FUN= function(el){
  tt  <- Cntrl           #  tentatively Control
  nel <- names(el)
  tt[nel] <- el[nel]
  tt
}) else res <- list()

attr(res, "Control") <- Cntrl
res
}

.traceFunction <- function(uid, expr, funNm, tags = character(), alt=expr){
## traceFunction() dispatches traceList() or similar function

opt <- options()$trace 
if (is.null(opt)) return()      # option trace not set, skip

cntrl <- attr(opt, "Control")   # Tentatively set to general options

if (length(opt)){               # cntrl modified for specific functions 
    nmsL <- names(opt)          # Function names
    if (funNm %in% nmsL) {
      el <- opt[[funNm]]        # Extract options for a given function
      nel <- names(el)          # Option names
      cntrl[nel] <- el          # Modify cntrl
}}
#print(names(cntrl))
xtrace <- cntrl$xtrace 
if (xtrace == "tracePrint") expr = alt
#print(xtrace)
xtags <- cntrl$xtags
xuid <- cntrl$xuid
go1 <- uid %in% xuid     || !length(xuid) 
go2 <- FALSE
if (!length(xtags)) go2 <- TRUE   # For xtags= character(0)

tix <- intersect(tags, xtags)
if (length(tags) && length(tix) > 0){
 go2 <- TRUE
 tags <- tix                      # 'active' tags
}

if (go1 && go2)  do.call(xtrace, list(uid=uid, expr=expr, funNm=funNm, tags=tags)) else return()
}

traceList <- function(uid, expr, funNm, tags = character()){
## .traceList <- list()
objL <- list(expr)  
if (mode(expr) == "NULL") {   # For example objects returned by str()
 objL <- list("Object of mode NULL. Consider to use capture.output(object)" )
}
tags1 <- paste(tags, collapse=".")
names(objL) <- paste(uid, funNm, tags1, sep=":")
assign(".traceList", c(.traceList, objL), env =.GlobalEnv)
}

tracePrint <- function(uid, expr, funNm, tags) {
### funNm needs to be defined inside the function
tmp <- max(ceiling(10/uid), 1)  # From 1 to 10

if (tmp == 10)  a <- paste(paste(rep("#",   9, sep=""), collapse=""), "=>", collapse="", sep="")     # uid =1
if (tmp == 5)   a <- paste(paste(rep("=", tmp, sep=""), collapse=""), ">", collapse="", sep="")      # uid =2
if (tmp < 5)    a <- paste(paste(rep("-", tmp, sep=""), collapse=""), ">", collapse="", sep="")      # uid =3:9
if (tmp <= 1)    a <- "-"
# if (tmp >  9) a <- paste(rep("#",9,sep=""), collapse="")

# a <- paste(a,"> ",sep="")
# a <-""
if (length(expr)==0 && !is.null(expr)){
  cat(a, uid, ": Object", as.character(substitute(expr)), " has length 0 and is of mode:",  mode(expr), "\n", sep=" ")
  return(2)
}

if (!is.call(expr) && !is.name(expr) && mode(expr) != "list"){
#cat("if !is.call \n")
if (!is.null(expr) && !substitute(expr) == expr[1]){
cat(a, uid,":", as.character(substitute(expr)),"=", expr, "\n",sep=" ")
} else { cat(a, uid,":", expr, "\n",sep=" ")
}} # !is.call

if (is.call(expr) || is.name(expr) || mode(expr) == "list"){
#cat("if is.call \n")
if (!is.null(expr) && !substitute(expr) == expr){
cat(a, uid,":", as.character(substitute(expr)),"=", "\n", sep= " ")
} else {
cat(a, uid,":\n",sep=" ")}
print(expr)
} # is.call(expr
#cat("xverbose ends \n")
return()
}
