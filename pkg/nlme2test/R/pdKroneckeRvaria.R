pdKroneckeRform1 <- function (valx,  ...) {
## Based on pdKronecker.formula. valx stands for a list of formulas. At least one component.
  .functionLabel <- "pdKroneckeRform1"
  # print(funNm)
  .traceR <- attr(options()$traceR, "fun")
  .traceR <-   if (is.null(.traceR)) function(...){} else .traceR 


  .traceR(901, lbl = "-> STARTS") 
  if (is.null(unlist(valx))) return(NULL)
  valx <- lapply(rev(valx), terms)
    aux <- paste(unlist(lapply(valx, function(el) attr(el, "term.labels"))), 
        collapse = ":")      
    if (!all(unlist(lapply(valx, function(el) attr(el, "intercept"))))) {
        aux <- paste(aux, " - 1")
    }
   .traceR(901, lbl = "ENDS <- ") 
   eval(parse(text = paste("~", aux)), envir=.GlobalEnv)   ###             .GlobalEnv?
}

xformula <- function(object){
## Object can be pdBlocked or pdKroneckeR


valx <- if(inherits(object, "pdBlocked")){
  formula(object, asList = TRUE)
} else {
  lapply(attr(object, "pdB"), formula) 
}
pdKroneckeRform1(valx)
}

xNames <- function(object){
## Object can be pdBlocked or pdKroneckeR
nam <- if(inherits(object, "pdBlocked")){
  Names(object, asList = TRUE)
} else {
  lapply(attr(object, "pdB"), Names)
}
  if (is.null(nam)) return(NULL)
  
  ## apply(expand.grid(rev(nam)), 1, FUN= function(el) paste(rev(el)[-1], collapse=":" ))
  apply(expand.grid(rev(nam)), 1, FUN = function(el) paste(el[-length(el)], collapse = ":" ))
}

pdKroneckeRcoefs  <- function(pdB){
  .functionLabel <- "pdKroneckeRcoefs" 
  .traceR <- attr(options()$traceR, "fun")
  .traceR <- if (is.null(.traceR)) function(...){} else .traceR 
  .traceR(801, lbl ="-> STARTS")
  
  ## Extracts vector of coefficients from pdBlocked or from a list of pdMats for use by pdKroneckeRcoefs
  coefsList <- lapply(pdB, FUN=function(el) coef(el, unconstrained=TRUE))
  coefAll <- sum(sapply(coefsList, FUN=function(el) el[1]))
  if (is.na(coefAll)) coefAll <- numeric(0)
  for (i in seq_along(pdB)){
    pdxi <- pdB[[i]]
    clss <- class(pdxi)[1]
    if (! (clss %in% c("pdDiag","pdIdent","pdCompSymm","pdLogChol"))) stop("Class:",
       clss, " not allowed") 
    coefs <- coefsList[[i]]
    # print(coefs)
    coefs1 <- coefs
    if (clss=="pdDiag")     coefs1 <- coefs - coefs[1] 
    if (clss=="pdIdent")    coefs1 <- coefs[1] 
    if (clss=="pdCompSymm") coefs1[1] <- 0 
    if (clss=="pdLogChol"){
    Ncol <- round((sqrt(8 * length(coefs) + 1) - 1)/2)
    x1 <- exp(-coefs[1])
    #print("x1")
    #print(x1)
    coefs1[1:Ncol] <- coefs[1:Ncol] - coefs[1]
    coefs1[(Ncol+1):length(coefs)] <- x1* coefs[(Ncol+1):length(coefs)] 
   }
   coefs1 <- coefs1[-1]         ##  ????
   if (length(coefs1) == 0)  coefs1 <- NULL
   coefAll <- c(coefAll, coefs1)
  }
.traceR(801, lbl = "ENDS<-")

coefAll
}


xcoef <- function(object){
# object is pdBlocked or 
if(inherits(object, "pdKroneckeR")) object <- attr(object, "pdB")
pdKroneckeRcoefs(object)
}

