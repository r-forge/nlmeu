
###            Fit a general linear mixed effects model
###
### Copyright 1997-2003  Jose C. Pinheiro,
###                      Douglas M. Bates <bates@stat.wisc.edu>
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#

lmeControl.U <- 
function (maxIter = 50, msMaxIter = 50, tolerance = 1e-06, niterEM = 25, ## V4
    msMaxEval = 200, msTol = 1e-07, msScale = lmeScale, msVerbose = FALSE, 
    returnObject = FALSE, gradHess = TRUE, apVar = TRUE, .relStep = (.Machine$double.eps)^(1/3), 
    minAbsParApVar = 0.05, nlmStepMax = 100, opt = c("nlminb", 
        "optim"), optimMethod = "BFGS", natural = TRUE, keepT=FALSE
) 

{
    list(maxIter = maxIter, msMaxIter = msMaxIter, tolerance = tolerance, 
        niterEM = niterEM, msMaxEval = msMaxEval, msTol = msTol, 
        msScale = msScale, msVerbose = msVerbose, returnObject = returnObject, 
        gradHess = gradHess, apVar = apVar, .relStep = .relStep, 
        nlmStepMax = nlmStepMax, opt = match.arg(opt), optimMethod = optimMethod, 
        minAbsParApVar = minAbsParApVar, natural = natural, keepT=keepT
)
}



lme.formula.U <- 
function (fixed, data = sys.frame(sys.parent()), random = pdSymm(eval(as.call(fixed[-2]))), 
    correlation = NULL, weights = NULL, subset, method = c("REML", 
        "ML"), na.action = na.fail, control = list(), contrasts = NULL, 
    keep.data = TRUE) 
{
##  lme.formula() function created 
    funNm <- "lme.formula.U"
    .traceFunction(1, "lme.formula.U STARTS here ====", funNm, tags =c("1","msg"))
    Call <- match.call()
    miss.data <- missing(data) || !is.data.frame(data)
    controlvals <- lmeControl()
    
    if (!missing(control)) {
        if (!is.null(control$nlmStepMax) && control$nlmStepMax < 
            0) {
            warning("Negative control$nlmStepMax - using default value")
            control$nlmStepMax <- NULL
        }
        controlvals[names(control)] <- control
    }
    keepT <- controlvals[["keepT"]]                       ####  <<<<
    if (!inherits(fixed, "formula") || length(fixed) != 3) {
        stop("\nFixed-effects model must be a formula of the form \"resp ~ pred\"")
    }
    method <- match.arg(method)
    REML <- method == "REML"
    reSt <- reStruct(random, REML = REML, data = NULL)
    .traceFunction(110, reSt, funNm, tags = c("reSt"))
     groups <- getGroupsFormula(reSt)
    .traceFunction(120, groups, funNm, tags = c("msg"))
    if (is.null(groups)) {
        if (inherits(data, "groupedData")) {
            groups <- getGroupsFormula(data)
            namGrp <- rev(names(getGroupsFormula(data, asList = TRUE)))
            Q <- length(namGrp)
            if (length(reSt) != Q) {
                if (length(reSt) != 1) {
                  stop("Incompatible lengths for \"random\" and grouping factors")
                }
                randL <- vector("list", Q)
                names(randL) <- rev(namGrp)
                for (i in 1:Q) randL[[i]] <- random
                randL <- as.list(randL)
                reSt <- reStruct(randL, REML = REML, data = NULL)
                .traceFunction(120, reSt, funNm, tags = c("reSt"))
            }
            else {
                names(reSt) <- namGrp
                .traceFunction(130, reSt, funNm, tags = c("reSt"))
           }
        }
        else {
            groups <- ~1
            names(reSt) <- "1"
            .traceFunction(140, reSt, funNm, tags = c("reSt"))
        }
    }
    if (!is.null(correlation)) {
        if (!is.null(corGrpsForm <- getGroupsFormula(correlation, 
            asList = TRUE))) {
            corGrpsForm <- unlist(lapply(corGrpsForm, function(el) deparse(el[[2]])))
            corQ <- length(corGrpsForm)
            lmeGrpsForm <- unlist(lapply(splitFormula(groups), 
                function(el) deparse(el[[2]])))
            lmeQ <- length(lmeGrpsForm)
            if (corQ <= lmeQ) {
                if (any(corGrpsForm != lmeGrpsForm[1:corQ])) {
                  stop(paste("Incompatible formulas for groups in \"random\"", 
                    "and \"correlation\""))
                }
                if (corQ < lmeQ) {
                  warning(paste("Cannot use smaller level of grouping for", 
                    "\"correlation\" than for \"random\". Replacing", 
                    "the former with the latter."))
                  attr(correlation, "formula") <- eval(parse(text = paste("~", 
                    c_deparse(getCovariateFormula(formula(correlation))[[2]]), 
                    "|", deparse(groups[[2]]))))
                }
            }
            else {
                if (any(lmeGrpsForm != corGrpsForm[1:lmeQ])) {
                  stop(paste("Incompatible formulas for groups in \"random\"", 
                    "and \"correlation\""))
                }
            }
        }
        else {
            attr(correlation, "formula") <- eval(parse(text = paste("~", 
                c_deparse(getCovariateFormula(formula(correlation))[[2]]), 
                "|", deparse(groups[[2]]))))
            corQ <- lmeQ <- 1
        }
    }
    else {
        corQ <- lmeQ <- 1
    }
    lmeSt <- lmeStruct(reStruct = reSt, corStruct = correlation, 
        varStruct = varFunc(weights))
    .traceFunction(250, reSt, funNm, tags = c("reSt"))
    mfArgs <- list(formula = asOneFormula(formula(lmeSt), fixed, 
        groups), data = data, na.action = na.action)
    if (!missing(subset)) {
        mfArgs[["subset"]] <- asOneSidedFormula(Call[["subset"]])[[2]]
    }
    mfArgs$drop.unused.levels <- TRUE
    dataMix <- do.call("model.frame", mfArgs)
    origOrder <- row.names(dataMix)
    for (i in names(contrasts)) contrasts(dataMix[[i]]) = contrasts[[i]]
    grps <- getGroups(dataMix, groups)
    if (inherits(grps, "factor")) {
        ord <- order(grps)
        grps <- data.frame(grps)
        row.names(grps) <- origOrder
        names(grps) <- as.character(deparse((groups[[2]])))
    }
    else {
        ord <- do.call("order", grps)
        for (i in 2:ncol(grps)) {
            grps[, i] <- as.factor(paste(as.character(grps[, 
                i - 1]), as.character(grps[, i]), sep = "/"))
            NULL
        }
    }
    if (corQ > lmeQ) {
        ord <- do.call("order", getGroups(dataMix, getGroupsFormula(correlation)))
    }
    grps <- grps[ord, , drop = FALSE]
    dataMix <- dataMix[ord, , drop = FALSE]
    revOrder <- match(origOrder, row.names(dataMix))
    N <- nrow(grps)
    .traceFunction(300, reSt, funNm, tags = c("reSt"))
    Z <- model.matrix(reSt, dataMix)
    ncols <- attr(Z, "ncols")
    Names(lmeSt$reStruct) <- attr(Z, "nams")
    .traceFunction(350, lmeSt$reStruct, funNm, tags = c("reSt"))
    contr <- attr(Z, "contr")
    X <- model.frame(fixed, dataMix)
    Terms <- attr(X, "terms")
    auxContr <- lapply(X, function(el) if (inherits(el, "factor") && 
        length(levels(el)) > 1) 
        contrasts(el))
    contr <- c(contr, auxContr[is.na(match(names(auxContr), names(contr)))])
    contr <- contr[!unlist(lapply(contr, is.null))]
    X <- model.matrix(fixed, data = X)
    y <- eval(fixed[[2]], dataMix)
    ncols <- c(ncols, dim(X)[2], 1)
    Q <- ncol(grps)
    attr(lmeSt, "conLin") <- list(Xy = array(c(Z, X, y), c(N, 
        sum(ncols)), list(row.names(dataMix), c(colnames(Z), 
        colnames(X), deparse(fixed[[2]])))), dims = MEdims(grps, 
        ncols), logLik = 0)
    tmpDims <- attr(lmeSt, "conLin")$dims
    if (max(tmpDims$ZXlen[[1]]) < tmpDims$qvec[1]) {
        warning(paste("Fewer observations than random effects in all level", 
            Q, "groups"))
    }
    fixDF <- getFixDF(X, grps, attr(lmeSt, "conLin")$dims$ngrps, 
        terms = Terms)
    lmeSt <- Initialize(lmeSt, dataMix, grps, control = controlvals)
    parMap <- attr(lmeSt, "pmap")
    if (length(lmeSt) == 1) {
        oldConLin <- attr(lmeSt, "conLin")
        decomp <- TRUE
        attr(lmeSt, "conLin") <- MEdecomp(attr(lmeSt, "conLin"))
    }
    else decomp <- FALSE
    numIter <- 0
    repeat {
        oldPars <- coef(lmeSt)
        optRes <- if (controlvals$opt == "nlminb") {
            nlminb(c(coef(lmeSt)), function(lmePars) -logLik(lmeSt, 
                lmePars), control = list(iter.max = controlvals$msMaxIter, 
                eval.max = controlvals$msMaxEval, trace = controlvals$msVerbose))
        }
        else {
            optim(c(coef(lmeSt)), function(lmePars) -logLik(lmeSt, 
                lmePars), control = list(trace = controlvals$msVerbose, 
                maxit = controlvals$msMaxIter, reltol = if (numIter == 
                  0) controlvals$msTol else 100 * .Machine$double.eps), 
                method = controlvals$optimMethod)
        }
        numIter0 <- NULL
        coef(lmeSt) <- optRes$par
        attr(lmeSt, "lmeFit") <- MEestimate(lmeSt, grps)
        if (!needUpdate(lmeSt)) {
            if (optRes$convergence) {
                msg <- paste(controlvals$opt, " problem, convergence error code = ", 
                  optRes$convergence, "\n  message = ", optRes$message, 
                  sep = "")
                if (!controlvals$returnObject) 
                  stop(msg)
                else warning(msg)
            }
            break
        }
        numIter <- numIter + 1
        lmeSt <- update(lmeSt, dataMix)
        aConv <- coef(lmeSt)
        conv <- abs((oldPars - aConv)/ifelse(aConv == 0, 1, aConv))
        aConv <- NULL
        for (i in names(lmeSt)) {
            if (any(parMap[, i])) {
                aConv <- c(aConv, max(conv[parMap[, i]]))
                names(aConv)[length(aConv)] <- i
            }
        }
        if (max(aConv) <= controlvals$tolerance) {
            break
        }
        if (numIter > controlvals$maxIter) {
            msg <- paste("Maximum number of iterations", "(lmeControl(maxIter)) reached without convergence.")
            if (controlvals$returnObject) {
                warning(msg)
                break
            }
            else stop(msg)
        }
    }
    lmeFit <- attr(lmeSt, "lmeFit")
    names(lmeFit$beta) <- namBeta <- colnames(X)
    attr(fixDF, "varFixFact") <- varFix <- lmeFit$sigma * lmeFit$varFix
    varFix <- crossprod(varFix)
    dimnames(varFix) <- list(namBeta, namBeta)
    Fitted <- fitted(lmeSt, level = 0:Q, conLin = if (decomp) 
        oldConLin
    else attr(lmeSt, "conLin"))[revOrder, , drop = FALSE]
    Resid <- y[revOrder] - Fitted
    rownames(Resid) <- rownames(Fitted) <- origOrder
    attr(Resid, "std") <- lmeFit$sigma/(varWeights(lmeSt)[revOrder])
    grps <- grps[revOrder, , drop = FALSE]
    lmeSt$reStruct <- solve(lmeSt$reStruct)
    .traceFunction(400, lmeSt$reStruct, funNm, tags = c("reSt"))
    dims <- attr(lmeSt, "conLin")$dims[c("N", "Q", "qvec", "ngrps", 
        "ncol")]
    if (controlvals$apVar) {
        apVar <- lmeApVar(lmeSt, lmeFit$sigma, .relStep = controlvals[[".relStep"]], 
            minAbsPar = controlvals[["minAbsParApVar"]], natural = controlvals[["natural"]])
    }
    else {
        apVar <- "Approximate variance-covariance matrix not available"
    }
    if (!keepT){                                                          ### <<<<<
     attr(lmeSt, "conLin") <- NULL
     attr(lmeSt, "lmeFit") <- NULL
    }
    estOut <- list(modelStruct = lmeSt, dims = dims, contrasts = contr, 
        coefficients = list(fixed = lmeFit$beta, random = lmeFit$b), 
        varFix = varFix, sigma = lmeFit$sigma, apVar = apVar, 
        logLik = lmeFit$logLik, numIter = if (needUpdate(lmeSt)) numIter else numIter0, 
        groups = grps, call = Call, terms = Terms, method = method, 
        fitted = Fitted, residuals = Resid, fixDF = fixDF, na.action = attr(dataMix, 
            "na.action"))
    if (keep.data && !miss.data) 
        estOut$data <- data
    if (inherits(data, "groupedData")) {
        attr(estOut, "units") <- attr(data, "units")
        attr(estOut, "labels") <- attr(data, "labels")
    }

    class(estOut) <- "lme"
    .traceFunction(1, "lme.formula.U ENDS here ====", funNm, tags="1")
    estOut
}
