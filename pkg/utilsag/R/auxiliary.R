# source("C:\\Users\\agalecki\\Google Drive\\MySoftware\\_rforgenlmeU\\pkg\\utilsag\\R\\auxiliary.R")

.XverboseControl <- function(){
list(
 testedFunction = numeric(),
 model.matrix.reStruct.U = numeric()  # REMOVE this line later
)}

XverboseControl <- function(...){
   ### code borrowed  from lattice.options()
    new <- list(...)
    if (is.null(names(new)) && length(new) == 1 && is.list(new[[1]])) new <- new[[1]]
    old <- .utilsagEnv$XverboseControl
 
    ## if no args supplied, returns full options list
    if (length(new) == 0) return(old)

    nm <- names(new)
    if (is.null(nm)) return(old[unlist(new)]) ## typically getting options, not setting
    isNamed <- nm != "" ## typically all named when setting, but could have mix
    if (any(!isNamed)) nm[!isNamed] <- unlist(new[!isNamed])

    ## so now everything has non-"" names, but only the isNamed ones should be set
    ## everything should be returned, however

    retVal <- old[nm]
    names(retVal) <- nm
    nm <- nm[isNamed]

    .utilsagEnv$XverboseControl <- lattice:::updateList(old, new[nm])

    ## return changed entries invisibly
    invisible(retVal)
}

Xverbose <- function(xv, object, funNm) {
  #cat ("xverbose starts \n")
  xvcntrl <-  XverboseControl()
  xvnms <- names(xvcntrl)
  if (!(funNm %in% funNm)) return()
  xverbose <- xvcntrl[[funNm]]

 ret <- !(xv %in% xverbose) 
 if (ret)  return()
tmp <- max(ceiling(10/xv), 1)  # From 1 to 10

if (tmp == 10)  a <- paste(paste(rep("#",   9, sep=""), collapse=""), "=>", collapse="", sep="")     # xv =1
if (tmp == 5)   a <- paste(paste(rep("=", tmp, sep=""), collapse=""), ">", collapse="", sep="")      # xv =2
if (tmp < 5)    a <- paste(paste(rep("-", tmp, sep=""), collapse=""), ">", collapse="", sep="")      # xv =3:9
if (tmp <= 1)    a <- "-"
# if (tmp >  9) a <- paste(rep("#",9,sep=""), collapse="")

# a <- paste(a,"> ",sep="")
# a <-""
if (length(object)==0 && !is.null(object)){
  cat(a, xv, ": Object", as.character(substitute(object)), " has length 0 and is of mode:",  mode(object), "\n", sep=" ")
  return(2)
}

if (!is.call(object) && !is.name(object) && mode(object) != "list"){
#cat("if !is.call \n")
if (!is.null(object) && !substitute(object) == object[1]){
cat(a, xv,":", as.character(substitute(object)),"=", object, "\n",sep=" ")
} else { cat(a, xv,":", object, "\n",sep=" ")
}} # !is.call

if (is.call(object) || is.name(object) || mode(object) == "list"){
#cat("if is.call \n")
if (!is.null(object) && !substitute(object) == object){
cat(a, xv,":", as.character(substitute(object)),"=", "\n", sep= " ")
} else {
cat(a, xv,":\n",sep=" ")}
print(object)
} # is.call(object
#cat("xverbose ends \n")
return()
}
