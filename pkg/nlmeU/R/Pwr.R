## source ("D:/Dropbox/GaleckiPrivate/nlmeURforge/pkg/nlmeU/R/Pwr.R")



Pwr.lme <- function (object, ..., 
    type = c("sequential", "marginal"), 
    Terms, L, verbose = FALSE, sigma, ddf= numeric(0), alpha=0.05,
    altB = NULL, tol = 1e-10) 
{
## Arguments: 
##  1. object: one object only
#   2. adjustSigma set to FALSE. (Try to explore adjustSigma argument if missing(sigma) 
#   3. Alternative altB name

   funNm <- "Pwr.lme"
   .traceRinit <- attr(options()$traceR, "init")
   .traceR <-   if (is.null(.traceRinit))
      function(...){} else .traceRinit(funNm) 
   
  
  .traceR(1, , funNm, "Pwr.lme STARTS")
   if (!inherits(object, "lme")) {
            stop("Object must inherit from class \"lme\" ")
}

  Lmiss <- missing(L)
  Tmiss <- missing(Terms)    
  Lx <- !Tmiss || !Lmiss     # Contrasts matrix L can be created 
  fixefs <- object$coefficients$fixed
  fixefNms <- names(fixefs)
  if (!Lx && !missing(sigma)) stop("L or Terms arguments need to be specified with non-missing sigma argument") 
  ### optx <- if (object$sigma < tol) FALSE else TRUE
  
  
if (!Tmiss && Lmiss){          # IF 1  (Check this part)
##  Based on Terms argument L matrix is created (with colnames)
##  Colnames assigned
.traceR(2,"IF 1 executed: Matrix L created from Terms argument" ,   funNm, msg = TRUE)
cLnms <- fixefNms
assign <- attr(object$fixDF, "assign")
nTerms <- length(assign)
nX <- length(unlist(assign))
L <- diag(nX)[unlist(assign[Terms]), , drop = FALSE]
colnames(L) <- cLnms 
cLnms <- NULL
}



.traceR(52, object$sigma, funNm, "object with sigma before rescaling")
if (!missing(sigma)) object <- nlmeU:::sigmaTolme(object,  value=sigma)
.traceR(53, object$sigma, funNm, "object with sigma after rescaling")
# .traceR(100, object, funNm, "object with adjusted sigma", lbl="100object1")  


.traceR(2, , funNm, "anova call")
.traceR(55, if (Lx) L else !Lmiss, funNm, "argumnt L for anova call")
x <- anova(object, adjustSigma=FALSE,
   test=TRUE,type=type, L=L, verbose=verbose)  # ANOVA results stored in x 
.traceR(101, x, funNm, "ANOVA stored in x")  


rt <- attr(x,"rt")   ### Check whether rt is needed

ndf <- x[["numDF"]]

ddf2 <- if (length(ddf)) ddf else x[["denDF"]] #  ddf2 is needed
rankL <- ndf



if (Lx){  
 ## Terms or L present: cLnms created for later use
.traceR(2,  , funNm, "IF 2 executed. cLnms created")
dimL <-  if (Lmiss) NULL else dim(L)             # Extract clNms from L argument
condt <- !Lmiss && is.null(dimL)
## cLnms over-written
cLnms <- if (condt) names(L) else colnames(L)  

}


.traceR(51, Lx, funNm, "L created?")
.traceR(51, if (Lx) cLnms, funNm, "cLnms created conditionally")
.traceR(51, if(Lx) L, funNm, "L matrix/vector (conditionally)")



if (!is.null(altB) && Lx){ # START
.traceR(2, , funNm, "IF 4 executed")

 if (!Lmiss && is.null(dimL))  {
.traceR(2, , funNm, "IF 3 executed")
   #print("if here 2")
   dim(L) <- c(1,length(L))  # L is matrix
   colnames(L) <- cLnms
}


  ### Go through altB
  altBdt <- as.data.frame(altB)
  altBnrow <- nrow(altBdt)
  altBnms  <- names(altBdt)
  fixefx <- object$coefficients$fixed
  dt1 <- data.frame(matrix(rep(fixefx, altBnrow), nrow= altBnrow, byrow = TRUE))
  names(dt1) <- fixefNms
  #print(names(dt1))
  dt1[, altBnms] <- altBdt
  #print(names(dt1))

# Trimming dimensions
vcovb <- object$varFix
#print(vcovb)
  if (length(cLnms) < length(fixefNms)){
  vcovb <- vcovb[cLnms,cLnms]
  dt1efs  <- as.matrix(dt1[cLnms])
  }

#print(vcovb)
#print(str(dt1efs))

         
Fstat <- function(bx) {
   b1 <- L %*% bx
   res0 <- t(b1) %*% solve( L %*% vcovb %*% t(L)) %*% b1
   # Return Lcontrasts i.e. b1 vector together with res0
   res0/rankL
}

FstatAll <- apply(dt1efs, 1, Fstat)

#dtAll1  <- data.frame(dt1,  numDF = ndf, denDF = ddf2, 
#          Fvalue = FstatAll)

dtAll2 <- within(dt1, {
   numDF <- ndf
   denDF <- ddf2
   Fvalue <- FstatAll
   Fcrit <- qf(1-alpha, numDF, denDF)
   nc   <- Fvalue * numDF
   Power <- 1- pf(Fcrit, numDF,denDF,nc)
   })
return(dtAll2)
}  # END



Fcrit <- qf(1-alpha, ndf, ddf2)

ncx <- x[["F-value"]]*ndf  # Rescaling not needed

Power <- 1- pf(Fcrit, ndf, ddf2, ncx) 

F0val <- x[["F-value"]] 
.traceR(20, F0val, funNm, "Incorrect value for adjusted sigma")

ret <- data.frame(x$numDF, ddf2, F0val, ncx, Power) #Fcrit omitted


    varNames <- c("numDF", "denDF","F-value", "nc", "Power")  # Fcrit omitted
          vcovb <- object$varFix 
   #print("strx")
   #print(str(x))

          if (!is.null(axL <- attr(x, "L"))) {  # L mtx specified
           .traceR(2, , funNm, "IF 5 executed")        
          if (!Tmiss) lab <- paste("Power calculations for effect(s):", 
               paste(Terms, collapse = ", "), "\n",
               " represented by linear combination: \n")
         
          if (!Lmiss)  lab <- "Power calculations for a linear combination: \n"
            # probe/modify L= attr(x,"L")
            dimL <- dim(L)
            #print(str(L))
            if (is.null(dimL)) names(L) <- cLnms
            attr(ret,"L") <- L
          names(ret) <- varNames
          } else {   #  L not-specified. Effects tested one by one.
         .traceR(2,  ,"ELSE 5 executed", funNm)       
            # lab <- paste("Power calculations for effect(s):", Terms,"\n")  
                   
           # print("?2")
           dimnames(ret) <- list(rownames(x),varNames)  ## ???
           #print("?2a")
         }
  ### Modify lab.
  ### if alpha= ne 0.05 then paste(lab, alpha=)
  ### if ddf is NULL then warning ddf2 incorrect
  if (!is.null(attr(x, "label"))) {  
      attr(ret,"label") <- lab 
      } else {
      attr(ret,"label") <- "Power calculations: \n" 
      }
attr(ret, "coefficients") <- object$coefficients$fixed
attr(ret,"varFixed")   <- vcovb
  
attr(ret,"alpha") <- alpha
attr(ret,"rt") <- rt

class(ret)  <- c("Pwr","data.frame")
.traceR(1, , funNm, "Pwr.lme ENDS <=#######")
ret
}

print.Pwr <- function (x, verbose = attr(x, "verbose"), ...) 
{
    if ((rt <- attr(x, "rt")) == 1) {
        if (!is.null(lab <- attr(x, "label"))) {
            cat(lab)
            if (!is.null(L <- attr(x, "L"))) {
                print(zapsmall(L))
            }
        }
        ## cat("??print \n")
        pval <- format(round(x[, "Power"], 4))
        pval[as.double(pval) == 0] <- "<.0001"
        x[, "F-value"] <- format(zapsmall(x[, "F-value"]))
        x[, "nc"] <- format(zapsmall(x[, "nc"]))
        ## x[, "Fcrit"] <- format(zapsmall(round(x[, "Fcrit"], 3)))
        x[, "Power"] <- pval
        print(as.data.frame(x))
    }
}

