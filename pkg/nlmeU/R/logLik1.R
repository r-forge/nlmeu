logLik1 <-  function(object, ...) UseMethod("logLik1")

logLik1.lme <- function(modfit, dt1, dtInit=NULL){
  # Calculates profile likelihood (with beta profiled out) for *one* subject
  # Data with *one* level of grouping
  # correlation component in modelStruct not implemented
  # Continue to work here, if dtInit ne NULL 

   funNm <-  "logLik1.lme"
   hl <-  options()$traceR            # List
   htrace <- hl[["logLik1.lme"]]   # Function                 
   if (is.null(htrace)) htrace <- attr(hl, "default")
   .traceR <- if (is.null(htrace))  function(id, xp , funNm, msg, lbl){ } else  htrace 

   .traceR(1, , funNm, "logLik1.lme STARTS   <=####")
  
    m            <- modfit$modelStruct                 # Model structure
    sigma        <- modfit$sigma                       # sigma
    .traceR(111, sigma, funNm, "sigma from model fit")
   
    .traceR(2,  , funNm, "About D matrix")
 
    D            <- as.matrix(m$reStruct[[1]])         # "subject"
    .traceR(115, D, funNm)
    D            <- D  * sigma^2                       # Matrix D 
    .traceR(116, D, funNm)
   
    .traceR(2,  , funNm, "IF process $weights component =====")
    
    clw  <- modfit$call$weights
    vecR <- rep(sigma, nrow(dt1))                      
    if (length(clw)){
    .traceR(211, clw, funNm)
  
    if (inherits(eval(clw),"formula")) clw <- call("varFixed", clw) 
    clwl  <-  as.list(clw) 
    .traceR(212, clwl, funNm)
 
    
    varFun <- as.character(clwl[[1]])                  # VarFun="varPower"
    varSt <- m$varStruct
    vf.coef      <- coef(varSt,                        # Variance function ...
                         unconstrained=FALSE)          # coef. extracted
 
    names(vf.coef) <- NULL
    .traceR(222, vf.coef, funNm)
 
    #####varFun <- "varPower"
    #args   <- list(form = ~time)  ### <----
    args    <- as.list(clwl[-1]) 
    args    <- c(args, value = vf.coef) # Replace value? In some cases?
    vf      <- do.call(varFun, as.list(args))
    
    .traceR(2, ,  funNm, "IF dtInit is not NULL  =====")
    if (!is.null(dtInit)){
        dfAug    <- rbind(dtInit,dt1)
        vf.x     <- Initialize(vf, data=dfAug)}      # ... initialized
    if (is.null(dtInit)) vf.x    <- Initialize(vf, data=dt1)

    vecR     <- sigma/varWeights(vf.x)              # AugDiagonal of R_i
    zz      <- sum(log(varWeights(vf.x)))   # Works fine. Work here if dtInit ne NULL
    }
    
    
   
    if (!is.null(dtInit)){
        .traceR(222, , funNm, "IF dtInit")
        idxInit      <- c(1:nrow(dtInit))            # Indices for dtInit
        vecR         <- vecR[-idxInit]
    }                                               # Diagonal of R_i matrix
    
    .traceR(2, , funNm, "Wrap-up  =====")
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
    .traceR(1, , funNm, "logLik1.lme ENDS   <=######")               
    return(-0.5 * as.numeric(lLik))
}
