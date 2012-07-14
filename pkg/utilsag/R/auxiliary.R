
XverboseControl <- function(){
## Function returns very tentative list for Xverbose
list(
 testedFunction = numeric(),
 modifyPackageNamespace = 101:151,
 lme.formula.U = 1:999
)}

Xverbose <- function(xv, object, funNm) {
### funNm needs to be defined inside the function
### Example of using Xverbose function
## Xv <- list(fun1 = 1:999, fun2 = 1:11)
## str(Xv)
## options(Xverbose = Xv)
## options(Xverbose = NULL) # To turn off Xverbose
  onms <- names(options())            
      
  if (!("Xverbose" %in% onms)) return()
  xvcntrl <- options()$Xverbose
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
