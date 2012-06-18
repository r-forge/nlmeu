# .U Functions used to modify nlme namespace
# Use MODIFY.nlme
model.matrix.reStruct.U <-
function (object, data, contrast = NULL, ..., xverbose=list()) 
{
  xverbos <- do.call("nlmeUXverboseControl", args=list( ))[["model.matrix.reStruct.U"]]
  if (!missing(xverbose)) xverbos <- xverbose[["model.matrix.reStruct.U"]]
    Xverbose(1, "=====> model.matrix.reStruct.U STARTS", xverbose=xverbos)
    pdDef <- !(length(object) == 1 && inherits(object[[1]],"pdKronecker"))

    if (is.null(form <- formula(object, asList = TRUE))) {
        stop("Cannot extract model matrix without formula")
    }
    form1 <- asOneFormula(form)
    if (length(form1) > 0) {
        data <- model.frame(form1, data = data)
    }
    else {
        data <- data.frame("(Intercept)" = rep(1, nrow(data)))
    }
###
    any2list <- function(object, data, contrast) {       # <===
        form2list <- function(form, data, contrast) {    # <===
            if (length(asOneFormula(form)) == 0) {
                return(list("(Intercept)" = rep(1, dim(data)[1])))
            }
            as.data.frame(unclass(model.matrix(form, model.frame(form, 
                data), contrast)))
        }
        if (inherits(object, "formula")) {
            return(form2list(object, data, contrast))
        }
        if (is.list(object)) {
            return(unlist(lapply(object, form2list, data = data, 
                contrast = contrast), recursive = FALSE))
        }
        return(NULL)
    }
###

    if (pdDef){     # No pdKroneck
        value <- as.list(lapply(form, any2list, data = data, contrast = contrast))
        ncols <- as.vector(unlist(lapply(value, length)))
        nams <- if (length(value) == 1) {
        names(value[[1]])
    }
    else {
        paste(rep(names(value), ncols), unlist(lapply(value, 
            names)), sep = ".")
    }
    val <- matrix(unlist(value), nrow = nrow(data), dimnames = list(row.names(data), 
        nams))
    attr(val, "nams")  <- as.list(lapply(value, names))

}
    
    if (!pdDef){    # pdKroneck

       form2 <- formula(object, asList = FALSE)
       xverbose(801,form2)
       form2name <- names(form2) 
       xverbose(802,form2name)
       form2 <- form2[[1]]
       xverbose(803,form2)
       form  <- form2   ####  <- 
       ###print(str(data))
       val <- model.matrix(form,data)
       ncols <- ncol(val)
       nams <- dimnames(val)[[2]]
       xverbose(804,nams)
       #cat("-> nams \n")
       #print(nams)
       namsL <- list(nams)
       names(namsL) <- form2name 
       attr(val,"nams") <- namsL
    }    

    contr <- as.list(lapply(as.data.frame(data), function(x) if (inherits(x, 
        "factor") && length(levels(x)) > 1) 
        contrasts(x)
    else NULL))
    contr[names(contrast)] <- contrast

    attr(val, "ncols") <- ncols
    attr(val, "contr") <- contr
    attr(val,"names")  <- NULL
    Xverbose(1, "=====> model.matrix.reStruct.U STARTS", xverbose=xverbos)
    val
}
