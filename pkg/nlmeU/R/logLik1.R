logLik1 <-  function(object, ...) UseMethod("logLik1")

logLik1.lme <- function(modfit, dt1, dtInit=NULL){
  # Calculates profile likelihood (with beta profiled out) for *one* subject
  # Data with *one* level of grouping
  # correlation component in modelStruct not implemented
  # Continue to work here, if dtInit ne NULL 

   funNm <-  "logLik1.lme"
   .traceFunction(1, "logLik1.lme STARTS   <=####", funNm, tags= c("1","msg"))
  
    m            <- modfit$modelStruct                 # Model structure
    sigma        <- modfit$sigma                       # sigma
    .traceFunction(111, sigma, funNm)
   
    .traceFunction(2, "About D matrix", funNm, tags = "msg")
 
    D            <- as.matrix(m$reStruct[[1]])         # "subject"
    .traceFunction(115, D, funNm)
    D            <- D  * sigma^2                       # Matrix D 
    .traceFunction(116, D, funNm)
   
    .traceFunction(2, "IF process $weights component =====", funNm, tags=c("msg"))
    
    clw  <- modfit$call$weights
    vecR <- rep(sigma, nrow(dt1))                      
    if (length(clw)){
    .traceFunction(211, clw, funNm)
  
    if (inherits(eval(clw),"formula")) clw <- call("varFixed", clw) 
    clwl  <-  as.list(clw) 
    .traceFunction(212, clwl, funNm)
 
    
    varFun <- as.character(clwl[[1]])                  # VarFun="varPower"
    varSt <- m$varStruct
    vf.coef      <- coef(varSt,                        # Variance function ...
                         unconstrained=FALSE)          # coef. extracted
 
    .traceFunction(221, vf.coef, funNm)
    names(vf.coef) <- NULL
    .traceFunction(222, vf.coef, funNm)
 
    #####varFun <- "varPower"
    #args   <- list(form = ~time)  ### <----
    args    <- as.list(clwl[-1]) 
    args    <- c(args, value = vf.coef) # Replace value? In some cases?
    vf      <- do.call(varFun, as.list(args))
    
    .traceFunction(2, "IF dtInit is not NULL  =====", funNm, tags= c("msg"))
    if (!is.null(dtInit)){
        dfAug    <- rbind(dtInit,dt1)
        vf.x     <- Initialize(vf, data=dfAug)}      # ... initialized
    if (is.null(dtInit)) vf.x    <- Initialize(vf, data=dt1)

    vecR     <- sigma/varWeights(vf.x)              # AugDiagonal of R_i
    zz      <- sum(log(varWeights(vf.x)))   # Works fine. Work here if dtInit ne NULL
    }
    
    
   
    if (!is.null(dtInit)){
        .traceFunction(222, "IF dtInit", funNm)
        idxInit      <- c(1:nrow(dtInit))            # Indices for dtInit
        vecR         <- vecR[-idxInit]
    }                                               # Diagonal of R_i matrix
    
    .traceFunction(2, "Wrap-up  =====", funNm, tags="msg")
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
    .traceFunction(1, "logLik1.lme ENDS   <=######", funNm, tags=c("1","msg"))               
    return(-0.5 * as.numeric(lLik))
}
