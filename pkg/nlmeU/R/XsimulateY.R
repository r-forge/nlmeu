simulateY <- function(object, nsim = 1, seed = NULL, ...) UseMethod("simulateY")

simulateY.lme <- function (object, nsim =1, seed = as.integer(runif(1, 0, .Machine$integer.max)), 
  verbose = FALSE, sigma, xverbose = list(), ...) 
{
# Data with one level of grouping only.
  xverbos <- XverboseControl()[["logLik1"]]
  if (!missing(xverbose)) xverbos <- xverbose[["simulateY.lme"]]
  Xverbose(1, "simulateY.lme STARTS   <=####", xverbose=xverbos)
 
if (!inherits(object, "lme"))  stop("Object must inherit from class \"lme\" ")

Xverbose(110, missing(sigma), xverbose=xverbos)

if (!missing(sigma)) sigma(object) <- sigma

   groups <- object$groups[[1]]
Xverbose(120, str(groups), xverbose=xverbos)

        ugroups <- unique(groups)
            individuals <- as.matrix(ugroups)
        if (is.numeric(individuals)) 
            individuals <- ugroups[individuals]
Xverbose(150, str(individuals), xverbose=xverbos)
Vlist <- getVarCov(object, individuals, type="marginal")
fitd0 <- fitted(object, level=0)
chVlist <- lapply(Vlist, chol)

nx <- nsim * length(fitd0)
noise.mtx <- matrix(rnorm(nx), nrow = length(fitd0), ncol = nsim)

Xverbose(2, "lapply STARTS here ***", xverbose=xverbos)
dimN   <-   sapply(chVlist, ncol)  # Number of records per subject
cdimN1 <- cumsum(dimN)
cdimN0 <- cumsum(dimN) - dimN + 1
cdimN  <- cbind(cdimN0, cdimN1)
tList <- vector(length(dimN), mode = "list")
tList[] <- 1:length(dimN)
auxF1 <- 
  function(el){ # 1,2, ...
     Xverbose(1001, "Local auxF1() within simulateY.lme() STARTS here ***", xverbose=xverbos)
     Xverbose(1015, el, xverbose=xverbos)
     chV <- chVlist[[el]]
     ix <- cdimN[el,]
     Xverbose(1020, ix, xverbose=xverbos)
     i0 <- ix[1]
     i1 <- ix[2]
     noisex <- noise.mtx[i0:i1, ]
     Xverbose(1025, dim(noisex), xverbose=xverbos)
     tx <-t(chV) %*% noisex   # Check transpose here
     Xverbose(1001, "Local auxF1() within simulateY.lme() ENDS here ***", xverbose=xverbos)
     tx
}
res <- lapply(tList, FUN= auxF1)
Xverbose(2, "lapply ENDED ***", xverbose=xverbos)

Xverbose(2, "resAll STARTS***", xverbose=xverbos)
resAll <- NULL 
for (i in 1:length(dimN)) resAll <- rbind(resAll, res[[i]])
Xverbose(2, "resAll ENDS***", xverbose=xverbos)
Xverbose(1, "simulateY.lme ENDS   <=####", xverbose=xverbos)
return(resAll + fitd0)
}
