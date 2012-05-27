Pwr <-  function(object, ...) UseMethod("Pwr")

Pwr.lme <- function (object, type = c("sequential", "marginal"), 
    Terms, L, verbose = FALSE, sigma, ddf= numeric(0), alpha=0.05,
    altB = NULL, xverbose = list()
) 
{
# Arguments: object: one object only
# adjustSigma set to FALSE. Explore adjustSigma argument if missing(sigma) adjust object$sigma
# altB name

sigmaTolme <- function(object, value){ 
 ### Use this function only with Pwr(), because it  corrupts lme.object
  Xverbose(1001, "sigmaTolme STARTS", xverbose=xverbos)
  sigma0 <- object$sigma 
  Xverbose(1002, sigma0, xverbose=xverbos)  
  val <- value * value
  sc  <- sqrt(val)/sigma0  
  object$sigma <- sqrt(val)
  resids <- object$residuals
  resids <- resids * sc  
  std <- attr(resids,"std")*sc
  attr(object$residuals,"std") <- std
  
  Xverbose(1003, object$sigma, xverbose=xverbos)
  Xverbose(1004, sc, xverbose=xverbos)
  attr(object$fixDF, "varFixFact") <- 
      sc*attr(object$fixDF, "varFixFact") # Rescaled for anova
  Xverbose(1005, vcov(object)*sc*sc, xverbose=xverbos)

   object$varFix  <- object$varFix*sc*sc  # vcov rescaled  
   Xverbose(1001, "sigmaTolme ENDS", xverbose=xverbos)
   object
}

xverbos <- XverboseControl()[["Pwr.lme"]]
  if (!missing(xverbose)) xverbos <- xverbose[["Pwr.lme"]]
  Xverbose(1, "Pwr.lme STARTS", xverbose=xverbos)

  Lmiss <- missing(L)      # More or less addressed
  #print(Terms)
  Tmiss <- missing(Terms)  # Work on Terms argument!!!
  #print(Tmiss)
  cLnms <- names(fixef(object))

if (!inherits(object, "lme")) {
            stop("Object must inherit from class \"lme\" ")
}

fixefNms <- names(fixef(object))
  
if (!Tmiss && Lmiss){ # Based on Terms argument L matrix is created (assuming that L is missing)
Xverbose(2, "IF 1 executed", xverbose=xverbos)
assign <- attr(object$fixDF, "assign")
nTerms <- length(assign)
nX <- length(unlist(assign))
L <- diag(nX)[unlist(assign[Terms]), , drop = FALSE]
colnames(L) <- cLnms 
#print(str(L)) 
}


if (!Tmiss || !Lmiss){  # Terms or L present cLnms
Xverbose(2, "IF 2 executed", xverbose=xverbos)
#print("dimL 1")
dimL <-  if (Lmiss) NULL else dim(L)             # Extract clNms from L argument
#print("dimL 1a")
#print(dimL)
condt <- !Lmiss && is.null(dimL)
#print(condt)
namesL <- names(L)
#print(namesL)
cLnms <- if (condt) names(L) else colnames(L)  
#print(cLnms)
#print("dimL 1b")
}



#print("anova0")

if (!missing(sigma)){
 Xverbose(2, "IF sigmaTolme executed", xverbose=xverbos)
 object <- sigmaTolme(object,  value=sigma)
}

Xverbose(2, "anova call", xverbose=xverbos)
# No Terms argument

#print("anova")
### print(L)
x <- anova(object, adjustSigma=FALSE,
   test=TRUE,type=type, L=L, verbose=verbose)  # ANOVA results stored in x 
#print("anova2")
#print(str(x))
#print(dim(x))
#print(str(x))
#print(L)
#print(cLnms)


rt <- attr(x,"rt")   ### Check whether rt is needed
F0val <- x[["F-value"]] ### F-stat extracted from anova

###   !!!!  Adjust F.stat  Divide by sigma^2???
### F0val <- F0val *(scale^2)
ndf <- x[["numDF"]]

ddf2 <- if (length(ddf)) ddf else x[["denDF"]] #  ddf2 is needed
rankL <- ndf



if (!Lmiss && is.null(dimL))  {
Xverbose(2, "IF 3 executed", xverbose=xverbos)
   #print("if here 2")
   dim(L) <- c(1,length(L))  # L is matrix
   colnames(L) <- cLnms
}



if (!is.null(altB) && (!Tmiss || !Lmiss)){ # START
Xverbose(2, "IF 4 executed", xverbose=xverbos)
  ### Go through altB
  altBdt <- as.data.frame(altB)
  altBnrow <- nrow(altBdt)
  altBnms  <- names(altBdt)
  fixefx <- fixef(object)
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
#print(x)
#print(str(x))
#print(ddf2)
#print(ncx)

ret <- data.frame(x$numDF, ddf2, F0val, ncx, Power) #Fcrit omitted


    varNames <- c("numDF", "denDF","F-value", "nc", "Power")  # Fcrit omitted
          vcovb <- object$varFix 
   #print("strx")
   #print(str(x))

          if (!is.null(axL <- attr(x, "L"))) {  # L mtx specified
           Xverbose(2, "IF 5 executed", xverbose=xverbos)        
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
         Xverbose(2, "ELSE 5 executed", xverbose=xverbos)       
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
attr(ret, "coefficients") <- fixef(object)
attr(ret,"varFixed")   <- vcovb
  
attr(ret,"alpha") <- alpha
attr(ret,"rt") <- rt

class(ret)  <- c("Pwr","data.frame")
Xverbose(1, "Pwr.lme ENDS <=#######", xverbose=xverbos)
 
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

