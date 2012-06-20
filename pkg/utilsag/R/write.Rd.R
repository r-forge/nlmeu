write.Rd <- function(object, ...) UseMethod("write.Rd")


#write.Rd(Orthodont, package="nlme")            # by default to working directory
#write.Rd(Orthodont, package="nlme", dir.path = "") # To terminal
write.Rd.data.frame <- function(object, ..., dir.path = getwd(), package=""){
 #srch <- search()
 #tmp <- paste("package:", package, collapse="", sep="")
 #insrch <- tmp %in% srch
 #if (!insrch) warning("Package ", package, " not in search path")
 
 
 dfn <- as.character(substitute(object))
 
 dm <- dim(object)    # Dimensions
 dmc0 <- paste(dm, collapse=" x ")
 dmc  <- paste(" (", dmc0, ")", sep="")

 if (dir.path == ""){
    th <- stdout()   #  to terminal
 } else {
 
 fp1 <- dir.path    # Path, for example g 
 tp2 <- paste(dfn,".Rd", collapse="",sep="")
 fpath <- file.path(fp1, tp2)
 th <- file(fpath, open="w")
 }
 
 
 tt <- paste("% ",package,"/man/",dfn,".Rd", sep="")
 writeLines(tt,th)
 tt <- paste("% Part of the ", package," package for R ", sep="")
 writeLines(tt, th)

 tt <- paste("% Distributed under GPL or later: see ", package, "/LICENCE", sep="")
 writeLines(tt, th)

 tt <- paste("\\name{", dfn, "}",sep ="")
 writeLines(tt, th)

 tt <- paste("\\alias{", dfn, "}", sep ="")
 writeLines(tt, th)

 tt <- "\\docType{data}"
 writeLines(tt, th)

 tt <- paste("\\title{", dfn, " Data", dmc ,"}% Data Title", sep ="")
 writeLines(tt, th)

 # Description
 tt <- paste("\\description{Data ", dfn, sep ="")
 
 writeLines(tt, th)

 tt <- "  ..."
 writeLines(tt, th)


 tt <- "}"
 writeLines(tt, th)

 # Usage
 tt <- paste("\\usage{data(", dfn, ")}",sep ="")
 writeLines(tt, th)



 # Format
 tt <- "\\format{"
 writeLines(tt, th)

 
 tt <- paste("The \\code{", dfn,"} data frame has ", dm[1]," rows and ", dm[2], " columns", sep="")
 writeLines(tt, th)
 

 # format section
 nms <- names(object)
 #print (nms)
 modx <- lapply(object, class)

 tt <- "\\describe{"  # describe starts
 writeLines(tt, th)
 
 ## sapply(as.list(1:length(modx)), FUN=function(el)
 tmp <- NULL 
 for (el in 1:length(modx)) {
  nx <- nms[el]
  mx <- modx[[el]]
  #print("mx")
  #print(mx)
  mx <- mx[length(mx)]   # ordered factor to factor
  tt <- paste("\\item{", nx,"}{", sep="")
  tmp <- c(tmp, tt)
  #writeLines(tt, th)
  prefix <- "  a "
  postfix <-" vector"
  if (mx %in% c("integer","numeric")) {    
    if (mx == "integer") prefix  <- "  an " 
    rngx <- as.character(range(object[[el]], na.rm=TRUE))
    if (!is.na(rngx[1])){
      tmp0  <- paste(rngx ,sep="")
      postfix <- paste(" vector with values from ", rngx[1], " to ", rngx[2], sep="")
     }
 }

  if (mx == "factor"){              # Factor 
   levs <- levels(object[[el]])
   nlevs <- length(levs)
   levshort <- levs[1:min(5,nlevs)] # maximum 5 
   tmp0 <- paste(" \\code{", levshort,"}", sep="",collapse=",")
   if (nlevs > 5 ) tmp0 <- paste(tmp0,", ...", sep ="", collapse="")
   mx  <- paste(" factor with ", nlevs," levels ",sep="")
   postfix <- paste(tmp0, sep="",collapse=",")
  }
 
  tt <- paste(prefix, mx, postfix, sep="")
  tmp <- c(tmp, tt)

  #writeLines(tt, th)
  tt <- paste("}", sep="")
  #writeLines(tt, th)
  tmp <- c(tmp, tt)
 }
 
  #print ("tmp1")
  tmp <- paste("   ",tmp, sep="")
  writeLines(tmp, th)
  #print("tmp2")
 
  tt <- "}}"
  writeLines(tt, th)
 
 # Details
 tt <- "\\details{"
 writeLines(tt, th)

 tt <- paste(" ...", sep="")
 writeLines(tt, th)


 tt <- paste("}", sep="")
 writeLines(tt, th)

# source
 tt <- "\\source{"
 writeLines(tt, th)

 tt <- "  ???? Doe, A. B. and Doe, J. M. (xxxx), \\emph{Title},"
 writeLines(tt, th)

 tt <- "  Springer, New York.  (Appendix A.17)"
 writeLines(tt, th)

 tt <- paste("}", sep="")
 writeLines(tt, th)

# examples
 tt <- "\\examples{"
 writeLines(tt, th)
 
 tt <- paste("summary(", dfn, ")", sep="")
 writeLines(tt, th)

 tt <- paste("}", sep="")
 writeLines(tt, th)


 tt <- paste("\\keyword{datasets}", sep="")
 writeLines(tt, th)
 if (dir.path != ""){
    close(th)
    return(cat(fpath,"\n"))
 }
 #return()
}

