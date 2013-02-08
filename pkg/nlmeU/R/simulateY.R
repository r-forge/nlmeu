simulateY <- function(object, nsim = 1, seed = NULL, ...) UseMethod("simulateY")

simulateY.lme <- function (object, nsim =1, seed = as.integer(runif(1, 0, .Machine$integer.max)), ...,
  verbose = FALSE, sigma) 
{
# Data with one level of grouping only.
  funNm <- "simulateY.lme"
   hl <-  options()$traceR            # List
   htrace <- hl[["TracedFunction"]]   # Function. Change Function name, after cutting and pasting!!!               
   if (is.null(htrace)) htrace <- attr(hl, "default")
   .traceR <- if (is.null(htrace))  function(id, xp , funNm, msg, lbl){ } else  htrace 
   
  .traceR(1, , funNm, "simulateY.lme STARTS   <=####")
 
if (!inherits(object, "lme"))  stop("Object must inherit from class \"lme\" ")

.traceR(110, missing(sigma), funNm)

if (!missing(sigma))  object <- nlmeU:::sigmaTolme(object, sigma) # sigma(object) <- sigma

   groups <- object$groups[[1]]
.traceR(120, groups, funNm)

        ugroups <- unique(groups)
            individuals <- as.matrix(ugroups)
        if (is.numeric(individuals)) 
            individuals <- ugroups[individuals]
.traceR(150, individuals, funNm)
Vlist <- getVarCov(object, individuals, type="marginal")
fitd0 <- fitted(object, level=0)
chVlist <- lapply(Vlist, chol)

nx <- nsim * length(fitd0)
noise.mtx <- matrix(rnorm(nx), nrow = length(fitd0), ncol = nsim)

.traceR(2, ,funNm, "lapply STARTS here ***")
dimN   <-   sapply(chVlist, ncol)  # Number of records per subject
cdimN1 <- cumsum(dimN)
cdimN0 <- cumsum(dimN) - dimN + 1
cdimN  <- cbind(cdimN0, cdimN1)
tList <- vector(length(dimN), mode = "list")
tList[] <- 1:length(dimN)
auxF1 <- 
  function(el){ # 1,2, ...
     .traceR(1001, , funNm, "Local auxF1() within simulateY.lme() STARTS here ***")
     .traceR(1015, el, funNm)
     chV <- chVlist[[el]]
     ix <- cdimN[el,]
     .traceR(1020, ix, funNm)
     i0 <- ix[1]
     i1 <- ix[2]
     noisex <- noise.mtx[i0:i1, ]
     .traceR(1025, dim(noisex), funNm)
     tx <-t(chV) %*% noisex   # Check transpose here
     .traceR(1001, ,funNm, "Local auxF1() within simulateY.lme() ENDS here ***")
     tx
}
res <- lapply(tList, FUN= auxF1)
.traceR(2, , funNm, "lapply ENDED ***")

.traceR(2, , funNm, "resAll STARTS***")
resAll <- NULL 
for (i in 1:length(dimN)) resAll <- rbind(resAll, res[[i]])
.traceR(2, , funNm, "resAll ENDS***")
.traceR(1, , funNm, "simulateY.lme ENDS   <=####")
return(resAll + fitd0)
}

sigmaTolme <- function(object, value){ 
 ### Use this function only with Pwr() and simulateY(), because it  corrupts lme.object
  funNm <- "sigmaTolme"
 
   hl <-  options()$traceR            # List
   htrace <- hl[["sigmaTolme"]]   # Function. Change Function name, after cutting and pasting!!!               
   if (is.null(htrace)) htrace <- attr(hl, "default")
   .traceR <- if (is.null(htrace))  function(id, xp , funNm, msg, lbl){ } else  htrace 
 
  


  .traceR(1, , funNm, "sigmaTolme STARTS")
  sigma0 <- object$sigma 
  .traceR(2, sigma0, funNm)  
  val <- value * value
  sc  <- sqrt(val)/sigma0  
  object$sigma <- sqrt(val)
  resids <- object$residuals
  resids <- resids * sc  
  std <- attr(resids,"std")*sc
  attr(object$residuals,"std") <- std
  
  .traceR(3, object$sigma, funNm)
  .traceR(4, sc, funNm)
  attr(object$fixDF, "varFixFact") <- 
      sc*attr(object$fixDF, "varFixFact") # Rescaled for anova
  .traceR(5, vcov(object)*sc*sc, funNm)

   object$varFix  <- object$varFix*sc*sc  # vcov rescaled  
   .traceR(1, , funNm, "sigmaTolme ENDS")
   object
}
