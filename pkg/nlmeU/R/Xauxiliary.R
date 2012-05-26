"missPat" <- function(...)
{    args  <- as.list(substitute(list(...)))[-1]
     args.t <- paste("miss.frame$",args,sep="",collapse=",")
     miss.frame <- as.data.frame(ifelse(is.na(data.frame(args)),"X","-"))
     txt <- c("paste(", args.t, ",sep='')")
     txt<-paste(txt,sep='')
     eval(parse(text=txt))
}
sigma <-  function(object, ...) UseMethod("sigma")

sigma.default <- function(object) object$sigma
"sigma<-" <-  function(object, ..., value) UseMethod("sigma<-")

"sigma<-.lme" <- function(object, xverbose = list(), ..., value){
 
  xverbos <- XverboseControl()[["sigma<-.lme"]]
  if (!missing(xverbose)) xverbos <- xverbose[["sigma<-.lme"]]
  Xverbose(1, "'sigma<-.lme' STARTS", xverbose=xverbos)
  
  sigma0 <- object$sigma 
  Xverbose(2, sigma0, xverbose=xverbos)  
  val <- value * value
  sc  <- sqrt(val)/sigma0  
  object$sigma <- sqrt(val)
  resids <- object$residuals
  resids <- resids * sc  
  std <- attr(resids,"std")*sc
  attr(object$residuals,"std") <- std
  
  Xverbose(3, object$sigma, xverbose=xverbos)
  Xverbose(4, sc, xverbose=xverbos)
  attr(object$fixDF, "varFixFact") <- 
      sc*attr(object$fixDF, "varFixFact") # Rescaled for anova
  Xverbose(5, vcov(object)*sc*sc, xverbose=xverbos)

   object$varFix  <- object$varFix*sc*sc  # vcov rescaled  
   Xverbose(1, "'sigma<-.lme' EXITS", xverbose=xverbos)
   object
}

XverboseControl <- function(
  logLik1 = numeric(),
 `sigma<-.lme` = numeric()
 
){
# xcontrl <- XverboseControl(`sigma<-.lme` = 1:900)
list(
  logLik1       = logLik1,
  `sigma<-.lme` = eval(`sigma<-.lme`)
)
}


Xverbose <- function(xv,object, xverbose= numeric()) {
#cat ("xverbose starts \n")
 ret <- !(xv %in% xverbose) 
 if (ret)  return(1)
tmp <- max(ceiling(10/xv), 1)  # From 1 to 10

if (tmp == 10)  a <- paste(paste(rep("#",   9, sep=""), collapse=""), "=>", collapse="", sep="")           # xv =1
if (tmp == 5)   a <- paste(paste(rep("=", tmp, sep=""), collapse=""), ">", collapse="", sep="")          # xv =2
if (tmp < 5)    a <- paste(paste(rep("-", tmp, sep=""), collapse=""), ">", collapse="", sep="")         # xv =3:9
if (tmp <= 1)    a <- "-"
# if (tmp >  9) a <- paste(rep("#",9,sep=""), collapse="")

# a <- paste(a,"> ",sep="")
# a <-""
if (length(object)==0){
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
