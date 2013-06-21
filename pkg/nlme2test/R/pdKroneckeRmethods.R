## methods(class = pdMat)
##    [.pdMat*             
##    [<-.pdMat*
##    as.matrix.pdMat*
##    coef.pdMat*          obligatory, method used by pdKroneckeR
##    coef<-.pdMat*       
##    corMatrix.pdMat*
##    Dim.pdMat*
##    formula.pdMat*
##    isInitialized.pdMat*
##    logDet.pdMat*       
##    matrix<-.pdMat*
##    Names.pdMat*         method used by pdKroneckeR
##    Names<-.pdMat*
##    pdConstruct.pdMat*   obligatory,  written 
##    pdFactor.pdMat*     
##    pdMatrix.pdMat*      obligatory
##    plot.pdMat*
##    print.pdMat*
##    solve.pdMat*         written for pdKroneckeR 
##    summary.pdMat*      
##    VarCorr.pdMat* 

logDet.pdKroneckeR <- function (object, ...) 
{
    if (!isInitialized(object)) {
        stop(paste("Cannot extract the log of the determinant", 
            "from an uninitialized object"))
    }
    pdB <- attr(object,"pdB")
    sum(unlist(lapply(pdB, logDet)))
}

Names.pdKroneckeR <- function (object, asList = FALSE, ...) 
{
    if (asList) 
        attr(attr(object,"pdB"), "namesList")
    else {
     Dnms <- attr(object, "Dimnames")[[2]]   
     if (is.null(Dnms)) return(character(0))                       ##  May 2013: Modified 
     sep <- if (length(grep(":", Dnms, fixed = TRUE))) ":" else "."
     tmpx <- strsplit(Dnms, sep, fixed =TRUE)
     ##   A:a, A:b, B:a, B:b
     sapply(tmpx, function(el) paste(rev(el), sep = "", collapse = sep))
    }
}

"Names<-.pdKroneckeR" <- function (object, ..., value) 
 {
    .functionLabel <- "Names<-.pdKroneckeR"            # Function label
   .traceR <- attr(options()$traceR, "fun")
   .traceR <- if (is.null(.traceR)) function(...){} else .traceR 

   .traceR(1, lbl = "Names<-.pdKroneckeR STARTS")
   
    ###  Modify value 
    sep <- if (length(grep(":", value, fixed = TRUE))) ":" else "."
    val <- strsplit(value, sep, fixed = TRUE)
    
     ##   a:A, b:A, a:B, b:B
    value <- sapply(val, function(el) paste(rev(el), sep = "", collapse = sep))

    if (!is.null(Names(object))){ 
       .traceR(11, lbl= "Names<-.pdKR EXIT 1")
        NextMethod()
    }  else {
       .traceR(12, lbl = "Names<-.pdKR EXIT 2")
        object
    }
}




pdFactor.pdKroneckeR <- function (object) 
{
    #cat("===> pdFactor.pdKroneckeR starts here \n") 
    pdB <- attr(object, "pdB")
    pdFactorL <- lapply(pdB, pdMatrix, factor = TRUE)                               ### Work here

    mtx <- as.matrix(1)
    for (i in seq_along(pdFactorL)) {
      mtxi <- pdFactorL[[i]]           # order????
      mtx <- mtx %x% mtxi
}
     as.vector(mtx)     
}





pdMatrix.pdKroneckeR <-
function (object, factor = FALSE) 
{
    .functionLabel <- "pdMatrix.pdKroneckeR"            # Function label
   .traceR <- attr(options()$traceR, "fun")
   .traceR <- if (is.null(.traceR)) function(...){} else .traceR 

   .traceR(1, lbl = "-> STARTS")
 
    if (!isInitialized(object)) {
        stop("Cannot access the matrix of uninitialized objects")
    }

    if (length(xNames(object)) == 0) {
        stop("Cannot access the matrix of object without names")
    }

    ## namesList <- Names(object, TRUE)
    Ncol <- Dim(object)[2]
    ### Drop value <- array(0, c(Ncol, Ncol), attr(object, "Dimnames"))
 
    ###value <- aux <- pdMatrix(object[[1]], factor)  # first 
    
    if (factor) {
       lD <- 0
    }
    Dnms <- attr(object,"Dimnames")[[1]]
    object <- attr(object,"pdB")

    xDnms <- xNames(object)
     
    mtch <- match(Dnms, xDnms)  # 
    .traceR(110)  # print(mtch)
    value <- matrix(1)
    
    len <- length(object)
    # print(str(object))
    for (i in 1:len) {   # CHANGED from len:1 to 1:len
      .traceR(810) 
       oi  <- object[[i]]
        
        aux <- pdMatrix(oi, factor)
        value <- value %x% aux
        if (factor) 
            lD <- lD + attr(aux, "logDet")
    }
    if (factor) 
        attr(value, "logDet") <- lD
 
    ##coef1 <- coef(object)[1]
    ##value <- value * exp(2*coef1)
    ## !!!! Reorder rows and cols.
    value <- value[mtch, mtch]
    dimnames(value) <- list(Dnms, Dnms)
    .traceR(1,lbl = "ENDS <- ")
    value
}

   ### For reference two lines used in pdBlocked
   ###  coef(a) <- unlist(lapply(a, function(el) coef(solve(el), 
   ###     TRUE)))
 

solve.pdKroneckeR <- function (a, b, ...) 
{
   .functionLabel <- "solve.pdKroneckeR"            # Function label
   .traceR <- attr(options()$traceR, "fun")
   .traceR <-   if (is.null(.traceR)) function(...){} else .traceR 

   .traceR(1, lbl = "-> STARTS")


    if (!isInitialized(a)) {
        stop("Cannot get the inverse of an uninitialized object")
    }
    
    pdBo <- attr(a, "pdB")
    #print(str(pdBo)
    pdx <- lapply(pdBo, solve)  # Solves every component separately  
    .traceR(1, lbl = "ENDS <-. pdConstruct called.")
    pdConstruct(a, pdx)
}



