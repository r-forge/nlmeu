logLik1 <- function(modfit, dt1, dtInit=NULL, xverbose = list()){
  # Calculates profile likelihood (with beta profiled out) for *one* subject
  # Data with *one* level of grouping
  # correlation component in modelStruct not allowed
  # Continue to work here, if dtInit ne NULL 

  xverbos <- XverboseControl()[["logLik1"]]
  if (!missing(xverbose)) xverbos <- xverbose[["logLik1"]]
  Xverbose(1, "logLik1 STARTS   <=####", xverbose=xverbos)
  

    m            <- modfit$modelStruct                 # Model structure
    sigma        <- modfit$sigma                       # sigma
    Xverbose(111, sigma, xverbose=xverbos)
   
    Xverbose(2, "About D matrix", xverbose=xverbos)
 
    D            <- as.matrix(m$reStruct[[1]])         # "subject"
    Xverbose(115, D, xverbose=xverbos)
    D            <- D  * sigma^2                       # Matrix D 
    Xverbose(116, D, xverbose=xverbos)
   
    Xverbose(2, "IF process $weights component =====", xverbose=xverbos)
    
    clw  <- modfit$call$weights
    vecR <- rep(sigma, nrow(dt1))                      
    if (length(clw)){
    Xverbose(211, clw, xverbose=xverbos)
  
    if (inherits(eval(clw),"formula")) clw <- call("varFixed", clw) 
    clwl  <-  as.list(clw) 
    Xverbose(212, clwl, xverbose=xverbos)
 
    
    varFun <- as.character(clwl[[1]])                  # VarFun="varPower"
    varSt <- m$varStruct
    vf.coef      <- coef(varSt,                        # Variance function ...
                         unconstrained=FALSE)          # coef. extracted
 
    Xverbose(221, vf.coef, xverbose=xverbos)
    names(vf.coef) <- NULL
    Xverbose(222, vf.coef, xverbose=xverbos)
 
    #####varFun <- "varPower"
    #args   <- list(form = ~time)  ### <----
    args    <- as.list(clwl[-1]) 
    args    <- c(args, value = vf.coef) # Replace value? In some cases?
    vf      <- do.call(varFun, as.list(args))
    
    Xverbose(2, "IF dtInit is not NULL  =====", xverbose=xverbos)
    if (!is.null(dtInit)){
        dfAug    <- rbind(dtInit,dt1)
        vf.x     <- Initialize(vf, data=dfAug)}      # ... initialized
    if (is.null(dtInit)) vf.x    <- Initialize(vf, data=dt1)

    vecR     <- sigma/varWeights(vf.x)              # AugDiagonal of R_i
    zz      <- sum(log(varWeights(vf.x)))   # Works fine. Work here if dtInit ne NULL
    }
    
    
    
    if (!is.null(dtInit)){
    #cat("?2")
        idxInit      <- c(1:nrow(dtInit))            # Indices for dtInit
        vecR         <- vecR[-idxInit]
    }                                               # Diagonal of R_i matrix
    
    Xverbose(2, "Wrap-up  =====", xverbose=xverbos)
    #return(zz)  # Continue to work here
    vecR2        <- vecR^2
    R            <- diag(vecR2, nrow=length(vecR))        # R_i matrix     
    Z            <- model.matrix(m$reStruc, data=dt1)     # Z_i matrix
    V            <- Z %*% D %*% t(Z) + R                  # V_i matrix
    predY        <- predict(modfit, dt1, level=0)         # Predict fixed
    
    ##formula(modfit)[[2]]
    dvName       <- as.character(formula(modfit)[[2]])

    r            <- dt1[[dvName]] - predY                 # Residuals
    n            <- nrow(dt1)                 # No. of obs for subject
    lLik         <- n*log(2*pi) + log(det(V)) + 
                    t(r) %*% solve(V) %*% r
    Xverbose(1, "logLik1 ENDS   <=######", xverbose=xverbos)               
    return(-0.5 * as.numeric(lLik))
}
