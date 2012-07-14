# .U Functions used to modify nlme namespace
# Use MODIFY.nlme
model.matrix.reStruct.U <-
function (object, data, contrast = NULL, ...) 
{
    fnm <- "model.matrix.reStruct.U"
    Xverbose(1, "model.matrix.reStruct.U STARTS", fnm)
    pdDef <- !(length(object) == 1 && inherits(object[[1]],"pdKronecker"))

    if (is.null(form <- formula(object, asList = TRUE))) {
        stop("Cannot extract model matrix without formula")
    }
    form1 <- asOneFormula(form)
    Xverbose(20, form1, fnm)
    if (length(form1) > 0) {
        data <- model.frame(form1, data = data)
    }
    else {
        data <- data.frame("(Intercept)" = rep(1, nrow(data)))
    }
###
    Xverbose(30, str(data), fnm)
    any2list <- function(object, data, contrast) {       # <===
        Xverbose(940, "-any2list in <model.matrix.reStruct.U> STARTS", fnm)
        form2list <- function(form, data, contrast) {    # <===
        Xverbose(950, "form2list in <model.matrix.reStruct.U> STARTS", fnm)

            if (length(asOneFormula(form)) == 0) {
                tt1 <- list("(Intercept)" = rep(1, dim(data)[1]))
                Xverbose(951, str(tt1), fnm)   
                Xverbose(951, "tt1 returned from <form2list>. EXIT1.", fnm)   
                return(tt1)
            }
            tt2 <- as.data.frame(unclass(model.matrix(form, model.frame(form, 
                data), contrast)))
                 Xverbose(952, str(tt2), fnm)   
                 Xverbose(952, "tt2 returned from <form2list>. EXIT2", fnm)  
            tt2    
        }
        Xverbose(941, "any2list in <model.matrix.reStruct.U> CONTINUES", fnm)      
        if (inherits(object, "formula")) {
            tt3 <- form2list(object, data, contrast)
            Xverbose(953, str(tt3), fnm)   
            Xverbose(953, "tt3 returned from <form2list>. EXIT3", fnm)  
            return(tt3)
        }
        if (is.list(object)) {
            return(unlist(lapply(object, form2list, data = data, 
                contrast = contrast), recursive = FALSE))
        }
        Xverbose(940, "any2list in <model.matrix.reStruct.U> returns NULL", fnm)
        return(NULL)
    }
###
    Xverbose(50, pdDef, fnm)
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
       Xverbose(801, form2, fnm)
       form2name <- names(form2) 
       Xverbose(802, form2name, fnm)
       form2 <- form2[[1]]
       Xverbose(803, form2, fnm)
       form  <- form2   ####  <- 
       val <- model.matrix(form,data)
       ncols <- ncol(val)
       nams <- dimnames(val)[[2]]
       Xverbose(804,nams, fnm)
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
    Xverbose(1, "model.matrix.reStruct.U ENDS", fnm)
    val
}
## unloadNamespace("nlme")
## nlmeU:::modify.nlmeNamespace()
## getS3method("model.matrix","reStruct")
