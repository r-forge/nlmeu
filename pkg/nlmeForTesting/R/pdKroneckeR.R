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
            form  =  formula(object),   # NOT a list
            nam   =  Names(object),      # NOT a list
            data = sys.frame(sys.parent()), ... 
){
    funNm <- "pdConstruct.pdKroneckeR"            # Function label
   .traceRinit <- attr(options()$traceR, "init")
   .traceR <-   if (is.null(.traceRinit))
      function(...){} else .traceRinit(funNm) 
   
## pdBo is updated if value is:
##    1. list of pdMat,  
##    2. pdBlocked  
##    3. pdKroneckeR, 
##
## pdK is updated using pdMat if value is:
##    4. numeric vector 
##    5. positive definite matrix (has to be split)  
##  


   .traceR(1,"==== pdConstruct.pdKroneckeR STARTS", funNm, msg = TRUE) 
   .traceR(101, str(object), funNm)
   .traceR(111, form, funNm, msg = "form argument") 
   .traceR(112, nam, funNm,  msg = "nam argument")
 
   if (is.list(form) || is.list(nam)) stop("form/nam argument(s) should not be a list")
   
   ##----> object -> object (w/o pdB attribute)  + pdBo
   pdBo <-  attr(object, "pdB") 
   
   if (is.null(pdBo)) {
       pdBo <- numeric(0)
       class(pdBo) <- c("pdBlocked", "pdMat")
   }
   attr(object, "pdBo") <- NULL
   .traceR(115, str(object), funNm,  msg = "Argument object structure")
   
                 
   ## Create pdBv and pdKv from value argument
   ## We first consider argument value , which has 
   
   pdBv <- NULL   
   pdKv <- NULL                                             # pdKv initialized
  
  if (is.list(value) || inherits(value, "pdBlocked")){
  ## 1. List of pdMats or pdBlocked
     .traceR(5,"Argument value is a list or pdBlocked (IF1 executed)", funNm, msg =TRUE)
     pdBv <- value
     value <- xcoef(value)          # numeric vector
     if ((length(value) != length(object)) && length(object)> 0) {
            stop("Cannot change the length of the parameter after initialization of pdKroneckeR object(1)")
     }
  
  }    
    
  if (inherits(value, "pdKroneckeR")){
  ## 3. pdKroneckeR
     .traceR(5,"Argument value is pdKroneckeR (IF2 executed)", funNm, msg =TRUE)
  
        if ((length(value) != length(object)) && length(object)> 0) {
            stop("Cannot change the length of the parameter after initialization of pdKroneckeR object (2)")
     }

     pdBv <- attr(value, "pdB") 
     value <- coef(value)     # numeric vector
  }
  
  if (!is.null(pdBv)){
    .traceR(5,"object pdBv for pdBlocked() is not NULL (IF3 executed)", funNm, msg =TRUE)
    pdBo <- pdConstruct(pdBo, pdBv, data = data) # data
    .traceR(125, is.null(form), funNm)
    .traceR(126, is.null(formula(object)), funNm)
    if (is.null(form) && is.null(formula(object))) form <- xformula(pdBo)  # Changes April 22, 2013
    .traceR(127, form, funNm)
    if (!length(nam)  && !length(Names(object)))  nam <- xNames(pdBo)
  }

## ---- pdBo structure updated: Yes

## ---  pdBo + matrix   -> pdBo  + pdKv
   if (is.matrix(value)) { # Split matrix into components and build in pdBo STARTS
     .traceR(5,"Argument value is matrix (IF4 executed)", funNm, msg =TRUE)     
     Ncol <- Dim(pdBo)[2]
     dims <- dim(value)
     
     if (!((dims[1] == dims[2]) && (dims[1] == Ncol))) {
        stop("Cannot change the number of columns on an initialized object")
     }     
     if (is.null(nam <- rownames(value))) stop("rows/cols of matrix in value argument need to have names \n")
        namesList <- Names(pdBo, TRUE)
        xDnms <- xNames(pdBo)
        if (!(all(mtch <- match(nam, xDnms, nomatch = 0)))) {
            stop("Names of object and value must match.")
        }
        value <- value[mtch, mtch]
        nam1 <- rownames(value)
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
  }  # # Split matrix into components and build into pdBo ENDS
 
   if (is.numeric(value) && is.vector(value) && length(value)){
    .traceR(5,"Argument value is numeric vector (IF5 executed)", funNm, msg =TRUE)
   pdKv <- value
    plen  <- attr(pdBo, "plen")   
    print(str(pdBo))
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
   }   
   if (length(nam)) data <- NULL
   object <- pdConstruct.pdMat(object, pdKv, form = form, nam = nam, data = data)  ## nlme:::
   attr(object, "pdB") <- pdBo
   object
}   
   
