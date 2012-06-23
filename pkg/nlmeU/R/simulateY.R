simulateY <- function(object, nsim = 1, seed = NULL, ...) UseMethod("simulateY")

simulateY.lme <- function (object, nsim =1, seed = as.integer(runif(1, 0, .Machine$integer.max)), ...,
  verbose = FALSE, sigma) 
{
# Data with one level of grouping only.
  funNm <- "simulateY.lme"
  Xverbose(1, "simulateY.lme STARTS   <=####", funNm)
 
if (!inherits(object, "lme"))  stop("Object must inherit from class \"lme\" ")

Xverbose(110, missing(sigma), funNm)

if (!missing(sigma))  object <- nlmeU::sigmaTolme(object, sigma) # sigma(object) <- sigma

   groups <- object$groups[[1]]
Xverbose(120, str(groups), funNm)

        ugroups <- unique(groups)
            individuals <- as.matrix(ugroups)
        if (is.numeric(individuals)) 
            individuals <- ugroups[individuals]
Xverbose(150, str(individuals), funNm)
Vlist <- getVarCov(object, individuals, type="marginal")
fitd0 <- fitted(object, level=0)
chVlist <- lapply(Vlist, chol)

nx <- nsim * length(fitd0)
noise.mtx <- matrix(rnorm(nx), nrow = length(fitd0), ncol = nsim)

Xverbose(2, "lapply STARTS here ***", funNm)
dimN   <-   sapply(chVlist, ncol)  # Number of records per subject
cdimN1 <- cumsum(dimN)
cdimN0 <- cumsum(dimN) - dimN + 1
cdimN  <- cbind(cdimN0, cdimN1)
tList <- vector(length(dimN), mode = "list")
tList[] <- 1:length(dimN)
auxF1 <- 
  function(el){ # 1,2, ...
     Xverbose(1001, "Local auxF1() within simulateY.lme() STARTS here ***", funNm)
     Xverbose(1015, el, funNm)
     chV <- chVlist[[el]]
     ix <- cdimN[el,]
     Xverbose(1020, ix, funNm)
     i0 <- ix[1]
     i1 <- ix[2]
     noisex <- noise.mtx[i0:i1, ]
     Xverbose(1025, dim(noisex), funNm)
     tx <-t(chV) %*% noisex   # Check transpose here
     Xverbose(1001, "Local auxF1() within simulateY.lme() ENDS here ***", funNm)
     tx
}
res <- lapply(tList, FUN= auxF1)
Xverbose(2, "lapply ENDED ***", funNm)

Xverbose(2, "resAll STARTS***", funNm)
resAll <- NULL 
for (i in 1:length(dimN)) resAll <- rbind(resAll, res[[i]])
Xverbose(2, "resAll ENDS***", funNm)
Xverbose(1, "simulateY.lme ENDS   <=####", funNm)
return(resAll + fitd0)
}

sigmaTolme <- function(object, value){ 
 ### Use this function only with Pwr() and simulateY(), because it  corrupts lme.object
  funNm <- "sigmaTolme"

  Xverbose(1, "sigmaTolme STARTS", funNm)
  sigma0 <- object$sigma 
  Xverbose(2, sigma0, funNm)  
  val <- value * value
  sc  <- sqrt(val)/sigma0  
  object$sigma <- sqrt(val)
  resids <- object$residuals
  resids <- resids * sc  
  std <- attr(resids,"std")*sc
  attr(object$residuals,"std") <- std
  
  Xverbose(3, object$sigma, funNm)
  Xverbose(4, sc, funNm)
  attr(object$fixDF, "varFixFact") <- 
      sc*attr(object$fixDF, "varFixFact") # Rescaled for anova
  Xverbose(5, vcov(object)*sc*sc, funNm)

   object$varFix  <- object$varFix*sc*sc  # vcov rescaled  
   Xverbose(1, "sigmaTolme ENDS", funNm)
   object
}