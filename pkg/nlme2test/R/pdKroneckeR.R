pdKroneckeR <- function (value = numeric(0), form = NULL, nam = NULL, data = sys.frame(sys.parent())) 
{
    object <- numeric(0)
    class(object) <- c("pdKroneckeR", "pdMat")
    pdConstruct(object, value, form, nam, data
      # , pdClass
     )
}

pdConstruct.pdKroneckeR <-  function (
            object, 
            value = numeric(0), 
            form  =  formula(object),    # NOT a list
            nam   =  Names(object),      # NOT a list
            data = sys.frame(sys.parent()), ... 
){

   .functionLabel <- "pdConstruct.pdKroneckeR"            # Function label
   .traceR <- attr(options()$traceR, "fun")
   .traceR <-   if (is.null(.traceR)) function(...){} else .traceR 
    
## pdBo is updated if value is:
##    1. list of pdMat,  
##    2. pdBlocked  
##    3. pdKroneckeR, 
##
## object is updated using pdMat if value is:
##    4. numeric vector. Will be split btwn ?
##    5. positive definite matrix (has to be split)  
##
## Arguments form and nam 
##    6. argument form is used to update formula 


   .traceR(1, lbl = "-> pdConstruct.pdKroneckeR STARTS") 
  
   if (is.list(form) || is.list(nam)) stop("form/nam argument(s) should not be a list")
   
   ##----> object -> object (w/o pdB attribute)  + pdBo
   pdBo <-  attr(object, "pdB") 
   
   if (is.null(pdBo)) {
       pdBo <- numeric(0)
       class(pdBo) <- c("pdBlocked", "pdMat")
   }
   attr(object, "pdB") <- NULL  ## Changed pdBo to pdB 
   .traceR(10)
   
                 
   ## Create pdBv and pdKv from value argument
   ## We first consider argument value , which has 
   
   pdBv <- NULL   
   pdKv <- NULL                                             # pdKv initialized
   xform <- NULL
  if (is.list(value) || inherits(value, "pdBlocked")){
  ## 1. List of pdMats or pdBlocked
 
     pdBv <- value
     value <- xcoef(value)          # numeric vector
     if ((length(value) != length(object)) && length(object)> 0) {
            stop("Cannot change the length of the parameter after initialization of pdKroneckeR object(1)")
     }
      .traceR(101)
  }    
    
  if (inherits(value, "pdKroneckeR")){
  ## 3. pdKroneckeR
 
        if ((length(value) != length(object)) && length(object)> 0) {
            stop("Cannot change the length of the parameter after initialization of pdKroneckeR object (2)")
     }

     pdBv <- attr(value, "pdB")
     if (length(formula(value)) && (length(form) == 0)) form <- formula(value)
     if (length(Names(value))  &&  (length(nam) == 0))  nam  <- Names(value) 
     value <- coef(value) 
     .traceR(103)
  }
  
  if (!is.null(pdBv)){                            #  attribute pdB from value argument of pdKronecker class 
 
    pdBo <- pdConstruct(pdBo, pdBv, data = data) # data

    if (length(form) == 0)  form <- xformula(pdBo)  # Changes April 22, 2013 Skipped:  && is.null(formula(object)) 
 
    ## if (!length(nam)  && !length(Names(object)))  nam <- xNames(pdBo)  ## skipped May 2013
     if (length(nam) == 0)  nam <- xNames(pdBo)      # adde may 2013 pL1nidt <-  pdKroneckeR(pL1ni, data = dt) 
    .traceR(105)
  }

## ---- pdBo structure updated: Yes

## ---  pdBo + matrix   -> pdBo  + pdKv
   if (is.matrix(value)) { # Split matrix into components and build in pdBo STARTS
     
     ## ---pdConstruct.pdKroneckeR_5. Msg: Argument value is matrix (IF4 executed). 
     # Error in Dim.pdMat(pdBo) : 
     # Cannot access the number of columns of uninitialized objects without names.


     Ncolx <- sapply(pdBo, FUN= function(el) Dim(el)[2])
     Ncol <- cumprod(Ncolx)                        
     Ncol <- Ncol[length(Ncol)]                          # Number of cols based on updated pdBo 
     dims <- dim(value)
     
     if (!((dims[1] == dims[2]) && (dims[1] == Ncol))) {
        stop("Cannot change the number of columns on an initialized object")
     }  
        
     rnmsv <- if (is.null(rownames(value))) nam else rownames(value)
     
     if (is.null(rnmsv)) stop("rnsmv needed \n")
        namesList <- Names(pdBo, TRUE)
        xDnms <- xNames(pdBo)
        if (!(all(mtch <- match(rnmsv, xDnms, nomatch = 0)))) {
            stop("Names of object and value must match.")
        }
        value <- value[mtch, mtch]
        nam1 <- rnmsv                  ## rownames(value)
        if (!(all(nam1 == xDnms)))  stop("Order of names in nam1 does not correspond to Kronecker product.")
      ## attr(pdBo, "Dimnames") <- list(vNames, vNames)
      ## value is a matrix. Order of col/row names corresponds to that in Kronecker product
      ## Splits value into component matrices and updates pdBo object  

    ## pdIdent component  
    val1 <- as.matrix(value[1,1])
    nmsi <- namesList[[1]]   
    dimnames(val1) <- list(nmsi,nmsi)
    matrix(pdBo[[1]]) <- val1


   len <- length(pdBo)
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
      matrix(pdBo[[i]]) <- val1
      # calculate new value value/val     
      ##print(idx)
      idx <- seq(1, to =nv, by=leni)
      val <- as.matrix(val[idx,idx])/val1[1,1]
      ##print(as.numeric(diag(val)))
    } 
   
     value <- xcoef(pdBo)       # pdKronecker coefficients saved
    .traceR(110)    
  }  # # Split matrix into components and build into pdBo ENDS
 
   if (is.numeric(value) && is.vector(value) && length(value)){
   pdKv <- value
    plen  <- attr(pdBo, "plen")   
    # print(str(pdBo))
    if (is.null(plen <- attr(pdBo, "plen"))) {
        stop(paste("Cannot change the parameter when", "length of parameters is undefined"))
    }
    
    
    plen <- plen - 1
    plen[1] <-1
    
     
    if (length(value) != (sum(plen))) stop("Cannot change parameter length of initialized pdMat object")
      
    endsi <- 0
    for (i in seq_along(pdBo)) {
      if (plen[i] > 0) {
        endsi <- endsi + plen[i]
        startsi <- endsi - plen[i] +1 
        vali <- value[startsi:endsi]        
        if (i > 1) vali <- c(0, vali)  
        coef(pdBo[[i]]) <- vali 
      }
     }  
   .traceR(115)
   }   
   
  
   # if ((logi1  && logi2) && length(formula(object)) == 0) {
   #       form <- xform
   #       nam <-  gsub(":",".", namx, fixed=TRUE)
   #}   
   
    logi <- FALSE          # formula changed  through form
    if (length(form) == 0) {
       form <- xformula(pdBo);   
       logi = TRUE         # formula changed through formx
    } 
    
    if (length(nam) == 0 && logi){
        namx <- xNames(pdBo)
        nam  <- gsub(":",".", namx, fixed = TRUE)
    }
    object <- pdConstruct.pdMat(object, pdKv, form = form, nam = nam, data = data)  ## nlme:::
    attr(object, "pdB") <- pdBo   
   .traceR(1, lbl = "pdConstruct.pdKroneckeR ENDS <-") 
   object
}   

