#  Some of the methods in this file need to be tested
### methods(class=pdKronecker)
#  [1] [.              coef.          coef<-.       
#  [4] corMatrix.*     formula.       isInitialized.
#  [7] logDet.*        matrix<-.Needed      Names.        
# [10] Names<-.*       pdConstruct.   pdFactor.     
# [13] pdMatrix.*      solve.         summary.      
# [16] VarCorr.*   


"[.pdKronecker" <- function (x, i, j, drop = TRUE) 
{
   #cat("===> !!![.pdKronecker starts here \n") 
   xx <- x
    x <- as.matrix(x)
    mCall <- match.call()
    mCall[[1]] <- get("[")
    mCall[["x"]] <- x
    mCall[["drop"]] <- drop
    if (length(i) == length(j) && mode(i) == mode(j) && all(i == 
        j)) {
        mCall[["drop"]] <- FALSE
        val <- eval(mCall)
        vNames <- colnames(val)
        auxNames <- lapply(Names(xx, TRUE), function(el, vN) {
            aux <- match(vN, el)
            if (any(aux1 <- !is.na(aux))) {
                el[aux[aux1]]
            }
        }, vN = vNames)
        auxWhich <- !unlist(lapply(auxNames, is.null))
        if (sum(auxWhich) == 1) {
            return(pdConstruct(as.list(xx)[auxWhich][[1]], val))
        }
        auxNames <- auxNames[auxWhich]
        auxClass <- unlist(lapply(xx, function(el) class(el)[1]))[auxWhich]
        return(pdConstruct(xx, val, nam = auxNames, form = NULL, 
            pdClass = auxClass))
    }
    else {
        eval(mCall)
    }
}


#coef.pdKronecker <- function (object, unconstrained = TRUE, ...) 
#  Replaced with newer version on Jul. 16, 2010 
#{
#  #cat("===> coef.pdKronecker starts here \n") 
#  coefSum <- coef(object[[1]],unconstrained) 
#  coefL <- lapply(object,coef,unconstrained)
#  coefs <- unlist(lapply(coefL, FUN=function(x) x[-1]))
#  ###coefSum <- sum(unlist(lapply(object,FUN = function(x) attr(x,"lsgma11") )))  
#  #cat("=> coef.pdKronecker EXIT \n") 
#  c(coefSum, coefs)    # a11 = exp(4*coefSum) 
#}

coef.pdKronecker <-
function (object, unconstrained = TRUE, ...) 
{
    coefSum <- coef(object[[1]], unconstrained)
    coefL <- lapply(object, coef, unconstrained)
    coefs <- unlist(lapply(coefL, FUN = function(x) x[-1]))
# Remove duplicate names, if any 
    coefAll  <- c(coefSum, coefs)
    nms      <- names(coefAll) 
    dupi     <- duplicated(nms)*(1:length(nms)) 
    idx      <- dupi[dupi>0]
    nmsidx   <- nms[idx]
    nms[idx] <- paste(nmsidx,idx,sep="")
    names(coefAll) <- nms
    coefAll
}

"coef<-.pdKronecker" <- function (object, ..., value) 
{
    #cat("===> coef<-.pdKronecker starts here \n") 
    if (is.null(plen <- attr(object, "plen"))) {
        stop(paste("Cannot change the parameter when", "length of parameters is undefined"))
    }
    plen <- plen -1
    plen[1] <-1
    #print(plen)
    if (length(value) != sum(plen)) {
        stop("Cannot change parameter length of initialized pdMat object")
    }
    ends <- cumsum(plen)
    starts <- 1 + c(0, ends[-length(ends)])
    #cat("-> Starts,ends \n")
    #print(starts)
    #print(ends)
    attrx <- attributes(object)
    coef(object[[1]]) <- value[1] 
    for (i in 2:length(object)) {      
        coef(object[[i]]) <- c(0,value[(starts[i]):(ends[i])])
    }
    attributes(object) <- attrx
    object
}

corMatrix.pdKronecker <- function (object, ...) 
{
    #cat("===> corMatrix.pdKronecker starts here \n") 
    if (!isInitialized(object)) {
        stop("Cannot access the matrix of uninitialized objects")
    }
    if (length(Names(object)) == 0) {
        stop("Cannot access the matrix of object without names")
    }
    namesList <- Names(object, TRUE)
    Ncol <- Dim(object)[2]
    value <- array(0, c(Ncol, Ncol), attr(object, "Dimnames"))
    stdDev <- double(Ncol)
    names(stdDev) <- colnames(value)
    for (i in seq_along(object)) {
        aux <- corMatrix(object[[i]])
        value[namesList[[i]], namesList[[i]]] <- as.vector(aux)
        stdDev[namesList[[i]]] <- attr(aux, "stdDev")
    }
    attr(value, "stdDev") <- stdDev
    value
}


formula.pdKronecker <- function (x, asList = FALSE, ...) # Changed to FALSE: Nov. 2011
{
    #cat("===> formula.pdKronecker starts here \n") 
    val <- lapply(x, formula)
    isNULL <- unlist(lapply(val, is.null))
    if (all(isNULL)) 
        return(NULL)
    if (any(isNULL)) {
        stop("All elements must have formulas, when any has a formula.")
    }
    if (asList) 
        return(val)
    isTwoSided <- unlist(lapply(val, function(el) {
        inherits(el, "listForm")
    }))
    if (all(isTwoSided)) {
        val <- do.call("c", val)
        class(val) <- "listForm"
        return(val)
    }
    if (any(isTwoSided)) {
        stop(paste("All elements of formula must be list of two-sided formulae", 
            "or two-sided formulae"))
    }
    val <- lapply(rev(val), terms)  # reversed Nov. 2011
    aux <- paste(unlist(lapply(val, function(el) attr(el, "term.labels"))), 
        collapse = ":")
    ##tmp <- lapply(val, function(el) attr(el, "intercept"))
    ##print(tmp) #intercepts present?
    if (!all(unlist(lapply(val, function(el) attr(el, "intercept"))))) {
        aux <- paste(aux, " - 1")
    }
    eval(parse(text = paste("~", aux)))
}

isInitialized.pdKronecker <- function (object) 
{
    #cat("===> isInitialized.pdKronecker starts here \n") 
    InitAll <- all(unlist(lapply(object, isInitialized)))
    #print(InitAll)
    InitAll
}

logDet.pdKronecker <- function (object, ...) 
{
    #cat("===> logDet.pdKronecker starts here \n") 
    sum(unlist(lapply(object, logDet)))
}


unlistFun <- function(namesList){
### Instead of unlist(namesList)
    ## cat("unlistFun Nov2011 \n") 
    # Shorten namesList 
    lenx <- sapply(namesList, length)
     namesList <- namesList[lenx>1]
    len1 <- length(namesList)     # It was -1 
    namesList <- rev(namesList)   # Changes rev(.).   namesList[-1] to namesList 
    #print(namesList)    
     dt <- expand.grid(namesList)
      allNames <- dt[[len1]]
  # print(allNames)
  # print(namesList)

  for (i in (len1-1):1) {    # It was 2:len1
      #cat("i=", i,"\n")
      aux <- dt[[i]]
      #print(aux)      
      allNames <- paste(allNames,dt[[i]], sep=":") 
      #print(allNames)
      }
  #print(allNames)
  allNames <- melt(allNames)
  #print(allNames)
  allNames <- allNames[,"value"]
  allNames <- as.character(allNames)
  #print(allNames)
  #stop("here")  
  allNames

}
#nmsList <- attr(tmp,"namesList")
#tt2 <- c(list(X="X"),nmsList)
#unlistFun(tt2)

# 
# t1 <- kronecker(d,a,paste,sep=":")
# t2 <- kronecker(t1,b,paste,sep=":")
# t3 <- as.array(t2)
# melt(t3)


"matrix<-.pdKronecker" <- function (object, value) 
{
## Take pdKronecker matrix stored in value(matrix)
## Recover component matrices from the value,
## Update component objects with component matrices 
## Created list does not have to be changed but, attrx needs to be recovered

    #cat("===> matrix<-.pdKronecker starts here \n") 
    #print(str(object))
    #print(str(value))

    value <- as.matrix(value)
    namesList <- Names(object, TRUE)

    Ncol <- Dim(object)[2]
    dims <- dim(value)
    #cat("-> Ncol, dims \n") 
    #print(Ncol)
    #print(dims)
    if (!((dims[1] == dims[2]) && (dims[1] == Ncol))) {
        stop("Cannot change the number of columns on an initialized object")
    }   
    #cat("-> rownames.value \n")
    #print(rownames(value))
   if (is.null(vNames <- rownames(value))) {
        vNames <- nlmeU:::unlistFun(namesList)
        dimnames(value) <- list(vNames, vNames)
    }   else {
       #cat("-> namesList \n")
       #print(namesList)  
       #print(vNames)

       if (!(all(match(nlmeU:::unlistFun(namesList), vNames, nomatch = 0)))) {
            stop("Names of object and value must match.")
        }
        attr(object, "Dimnames") <- list(vNames, vNames)
    }
    #cat("-> Before For \n")
    #print(length(object)) 

    
    #for (i in seq_along(object)) {
    #    cat("-> i=",i,"\n")
    #    tmp1 <- namesList[[i]]
    #    print(tmp1)
    #    cat("-> str.value \n")
    #    print(str(value))
    #    cat("-> str.object.i \n")
    #    print(str(object[[i]]))
    #    tmp2 <- value[namesList[[i]], namesList[[i]]]
    #    print(tmp2)
    #  Based on namesList odtworzyc component matrices
    #    
    #    matrix(object[[i]]) <- value[namesList[[i]], namesList[[i]]]
    #}

## pdIdent component
    val1 <- as.matrix(value[1,1])
    nmsi <- namesList[[1]]   
    dimnames(val1) <- list(nmsi,nmsi)
    matrix(object[[1]]) <- val1


   len <- length(object)
   val <- value/val1[1,]



   for (i in len:2){
      ##cat("--> i=",i,"\n")
      nmsi <- namesList[[i]]
      ###print(nmsi)
      leni    <- length(nmsi)        
      nv  <- nrow(val)
      ##cat("-> val1 \n")
      ##print(as.numeric(diag(val))) 
      idx <- seq(1,to =leni)    ####,by=nv/leni) 
      val1  <- as.matrix(val[idx,idx]) #  ith   mtx  from current a@b@c
      ##cat("-> val1")
      ##print(as.numeric(diag(val1)))
      dimnames(val1) <- list(nmsi,nmsi)
      matrix(object[[i]]) <- val1
      # calculate new value value/val     
      ##print(idx)
      idx <- seq(1, to =nv, by=leni)
      val <- as.matrix(val[idx,idx])/val1[1,1]
      ##print(as.numeric(diag(val)))
    }
     
   
    #cat("=> matrix<-.pdKronecker ends here \n") 
  
    object
}






Names.pdKronecker <- function (object, asList = FALSE, ...) 
{
    #cat("===> Names.pdKronecker starts here \n") 
     if (asList) 
        attr(object, "namesList") else 
    attr(object, "Dimnames")[[2]]
}


"Names<-.pdKronecker" <- function (object, ..., value) 
{
    fnm <- "Names<-.pdKronecker"
    Xverbose(210, "===> Names<-.pdKronecker starts here", fnm)
    Xverbose(220, value, fnm)
    tmp <- Names(object)
    Xverbose(221, tmp, fnm)
    Xverbose(222,"before if",fnm)
    if (!is.null(Names(object))) { 
      Xverbose(230, str(object),fnm)
      Xverbose(240, "-> Before NextMethod", fnm)
      clss <- class(object)   # Used instead 
      class(object) <- "pdMat"
      Names(object) <-  value
      class(object) <-  clss
      #xverbose(240, "-> After NextMethod")
      Xverbose(210, "===> Names<-.pdKronecker EXIT1",fnm)
      #obj
      object
      }
    else {
     Xverbose(210, "===> Names<-.pdKronecker EXIT2", fnm)
        object
    }
}

pdFactor.pdKronecker <- function (object) 
{
    #cat("===> pdFactor.pdKronecker starts here \n") 
    pdMatrix(object, factor = TRUE)
}


pdMatrix.pdKronecker <-
function (object, factor = FALSE) 
{
    if (!isInitialized(object)) {
        stop("Cannot access the matrix of uninitialized objects")
    }

    if (length(Names(object)) == 0) {
        stop("Cannot access the matrix of object without names")
    }

    namesList <- Names(object, TRUE)
    Ncol <- Dim(object)[2]
    ### Drop value <- array(0, c(Ncol, Ncol), attr(object, "Dimnames"))
 
    ###value <- aux <- pdMatrix(object[[1]], factor)  # first 
    
    if (factor) {
       lD <- 0
    }
    
    value <- matrix(1)
    len <- length(object)
    for (i in 1:len) {   # CHANGED from len:1 to 1:len
        aux <- pdMatrix(object[[i]], factor)
        value <- value %x% aux
        if (factor) 
            lD <- lD + attr(aux, "logDet")
    }
    if (factor) 
        attr(value, "logDet") <- lD
 
    ##coef1 <- coef(object)[1]
    ##value <- value * exp(2*coef1)
    dimnames(value) <- attr(object,"Dimnames")
    value
}




solve.pdKronecker <- function (a, b, ...) 
{
    if (!isInitialized(a)) {
        stop("Cannot get the inverse of an uninitialized object")
    }

   ### For reference two lines used in pdBlocked
   ###  coef(a) <- unlist(lapply(a, function(el) coef(solve(el), 
   ###     TRUE)))
 
    pdx <- lapply(a,solve)  # Solves every component separately
    attributes(pdx) <- attributes(a)    # Restore attributes
    pdx <- KroneckAux(pdx)
    #print(str(pdx))
    pdx
}



summary.pdKronecker <- function (object, structName = "pdKronecker", ...) 
{
    #cat("===> summary.pdKronecker starts here \n") 
    value <- lapply(object, summary)
    names(value) <- unlist(lapply(object, function(el) paste(Names(el), 
        collapse = ", ")))
    attr(value, "structName") <- structName
    attr(value, "elementName") <- "KBlock"
    class(value) <- "summary.pdMatX"
    #print(str(value))
    #cat("===> summary.pdKronecker ENDS here \n") 
    value
}

VarCorr.pdKronecker <- function (x, sigma = 1, rdig = 3) 
{
    #cat("===> varCorr.pdKronecker starts here \n") 
    m <- lapply(x, VarCorr, sigma = sigma, rdig = rdig)
    bd <- do.call("rbind", m)
    attr(bd, "formStr") <- paste(sapply(m, attr, which = "formStr"), 
        collapse = ", ")
    bd
}

print.pdKronecker <- function (x, opt=1, ...) 
{
    if (isInitialized(x)) {
        cat("Positive definite matrix structure of class", class(x)[1], 
            "representing\n")
        print(as.matrix(x), ...)
    if (opt==1) {
      cat("Matrix is a Kronecker product of the following covariance profiles: \n")
 
     lapply(x, FUN=function(el) print(as.matrix(el), ...))
      }
    }
    else {
        cat("Uninitialized positive definite matrix structure of class ", 
            class(x)[1], ".\n", sep = "")
    }
    invisible(x)
}







   