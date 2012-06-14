
"missPat" <- function(...)
{    args  <- as.list(substitute(list(...)))[-1]
     args.t <- paste("miss.frame$",args,sep="",collapse=",")
     miss.frame <- as.data.frame(ifelse(is.na(data.frame(args)),"X","-"))
     txt <- c("paste(", args.t, ",sep='')")
     txt<-paste(txt,sep='')
     eval(parse(text=txt))
}
sigma <-  function(object, ...) UseMethod("sigma")

sigma.default <- function(object, ...) object$sigma


XverboseControl <- function(
  logLik1 = numeric(),
  simulateY.lme = numeric(), 
  print.Pwr =numeric(),
  Pwr.lme = numeric()
  
){
# xcontrl <- XverboseControl(Pwr.lme = 1:900)
list(
  logLik1        = logLik1,
  simulateY.lme  = simulateY.lme,
  print.Pwr  = print.Pwr,
  Pwr.lme  = Pwr.lme
)
}
Xverbose <- function(xv,object, xverbose= numeric()) {
#cat ("xverbose starts \n")
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

