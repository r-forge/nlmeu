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

pdFactor.pdKroneckeR <- function (object) 
{
    #cat("===> pdFactor.pdKroneckeR starts here \n") 
    pdMatrix(object, factor = TRUE)                               ### Work here
}

pdMatrix.pdKroneckeR <-
function (object, factor = FALSE) 
{
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
    # print(str(Dnms))
    object <- attr(object,"pdB")
    xDnms <- xNames(object)
    # print(xDnms)  
    mtch <- match(Dnms, xDnms)  # 
    # print(mtch)
    value <- matrix(1)
    len <- length(object)
    # print(str(object))
    for (i in 1:len) {   # CHANGED from len:1 to 1:len
        oi  <- object[[i]]
        
        aux <- pdMatrix(oi, factor)
        print(aux)
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
    value
}

   ### For reference two lines used in pdBlocked
   ###  coef(a) <- unlist(lapply(a, function(el) coef(solve(el), 
   ###     TRUE)))
 

solve.pdKroneckeR <- function (a, b, ...) 
{
    if (!isInitialized(a)) {
        stop("Cannot get the inverse of an uninitialized object")
    }
    
    pdBo <- attr(a, "pdB")
    #print(str(pdBo)
    pdx <- lapply(pdBo, solve)  # Solves every component separately   
    pdConstruct(a, pdx)
}
