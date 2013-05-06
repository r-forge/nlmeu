## ->  simulateY  generic function
#' Simulates values of the dependent variable based on a model fit
#'
#' This function is generic; method functions can be written to handle specific classes of objects.
#'
#' @param object an object with a model fit for which dependent variable is to be simulated.
#' @param nsim number of simulations. nsim = 1 by default.
#' @param seed integer scalar used to initiate random numbers generator.
#' @param \dots some methods for this generic function may require additional arguments.
#' @param verbose logical. If TRUE basic information about arguments is provided. By default set to FALSE.
#' @param sigma numeric scalar. Allows to perform simulations employing alternative value of the scale parameter.
#' @return numeric matrix. Number of columns determined by nsim argument.
#' @author Andrzej Galecki and Tomasz Burzykowski
#' @examples
#'
#'  ## simulateY (fm1)
#'
#' @export
simulateY <- function(object, nsim = 1, seed = NULL, ..., verbose = FALSE, sigma) UseMethod("simulateY")



#' @S3method simulateY lme
simulateY.lme <- function (object, nsim = 1, seed = as.integer(runif(1, 0, .Machine$integer.max)), ...,
  verbose = FALSE, sigma) 
{
# Data with one level of grouping only.
  funNm <- "simulateY.lme"
  .traceRinit <- attr(options()$traceR, "init")
  .traceR <-   if (is.null(.traceRinit))
      function(...){} else .traceRinit(funNm) 
     
  .traceR(1, , funNm, "simulateY.lme STARTS   <=####")

if (verbose) print(paste("nsim = ", nsim ,", seed = ", seed, sep = ""))

if (!inherits(object, "lme"))  stop("Object must inherit from class \"lme\" ")

.traceR(110, missing(sigma), funNm)

if (!missing(sigma))  object$sigma <- sigma

   groups <- object$groups[[1]]
.traceR(120, groups, funNm)

        ugroups <- unique(groups)
            individuals <- as.matrix(ugroups)
        if (is.numeric(individuals)) 
            individuals <- ugroups[individuals]
.traceR(150, individuals, funNm)
Vlist <- nlme::getVarCov(object, individuals, type="marginal")
fitd0 <- fitted(object, level=0)
chVlist <- lapply(Vlist, chol)

nx <- nsim * length(fitd0)

set.seed(seed)
noise.mtx <- matrix(rnorm(nx), nrow = length(fitd0), ncol = nsim)

.traceR(2, "lapply STARTS here ***",funNm, msg = TRUE)
dimN   <-   sapply(chVlist, ncol)  # Number of records per subject
cdimN1 <- cumsum(dimN)
cdimN0 <- cumsum(dimN) - dimN + 1
cdimN  <- cbind(cdimN0, cdimN1)
tList <- vector(length(dimN), mode = "list")
tList[] <- 1:length(dimN)
auxF1 <- 
  function(el){ # 1,2, ...
     .traceR(1001,"Local auxF1() within simulateY.lme() STARTS here ***" , funNm, msg = TRUE)
     .traceR(1015, el, funNm)
     chV <- chVlist[[el]]
     ix <- cdimN[el,]
     .traceR(1020, ix, funNm)
     i0 <- ix[1]
     i1 <- ix[2]
     noisex <- noise.mtx[i0:i1, ]
     .traceR(1025, dim(noisex), funNm)
     tx <-t(chV) %*% noisex   # Check transpose here
     .traceR(1001,"Local auxF1() within simulateY.lme() ENDS here ***" ,funNm, msg=TRUE)
     tx
}
res <- lapply(tList, FUN= auxF1)
.traceR(2,"lapply ENDED ***" , funNm, msg = TRUE )

.traceR(2, "resAll STARTS***", funNm, msg = TRUE)
resAll <- NULL 
for (i in 1:length(dimN)) resAll <- rbind(resAll, res[[i]])
.traceR(2, "resAll ENDS***", funNm, msg = TRUE)
.traceR(1, "simulateY.lme ENDS   <=####", funNm, msg =TRUE )
return(resAll + fitd0)
}

sigmaTolme <- function(object, value){ 
 ### Use this function only with Pwr() and simulateY(), because it  corrupts lme.object
  funNm <- "sigmaTolme"
 
  .traceRinit <- attr(options()$traceR, "init")
   .traceR <-   if (is.null(.traceRinit))
      function(...){} else .traceRinit(funNm) 
   
  .traceR(1, "sigmaTolme STARTS", funNm, msg=TRUE)
  sigma0 <- object$sigma 
  .traceR(2, sigma0, funNm)  
  val <- value * value
  sc  <- sqrt(val)/sigma0  
  object$sigma <- sqrt(val)
  #resids <- object$residuals
  #resids <- resids * sc  
  #std <- attr(resids,"std")*sc
  ## attr(object$residuals,"std") <- std
  
  .traceR(3, object$sigma, funNm)
  .traceR(4, sc, funNm)
   attr(object$fixDF, "varFixFact") <- 
      sc*attr(object$fixDF, "varFixFact") # Rescaled for anova
  .traceR(5, vcov(object)*sc*sc, funNm)

   object$varFix  <- object$varFix*sc*sc  # vcov rescaled  
   .traceR(1,"sigmaTolme ENDS" , funNm, msg = TRUE)
   object
}
