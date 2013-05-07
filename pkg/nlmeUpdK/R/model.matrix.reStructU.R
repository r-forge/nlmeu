model.matrix.reStruct.U<-
function (object, data, contrast = NULL, ...) 
{
    fnm <- "model.matrix.reStruct.U"
    .traceRinit <- attr(options()$traceR, "init")
    .traceR <-   if (is.null(.traceRinit))
      function(...){} else .traceRinit(fnm) 
      
    .traceR(1, , fnm, "model.matrix.reStruct.U STARTS")
    pdDef <- !(length(object) == 1 && inherits(object[[1]],"pdKronecker"))

    if (is.null(form <- formula(object, asList = TRUE))) {
        stop("Cannot extract model matrix without formula")
    }
    form1 <- asOneFormula(form)
    .traceR(20, form1, fnm)
    .traceR(20, length(form1), fnm)
    if (length(form1) > 0) {
        data <- model.frame(form1, data = data)
    }
    else {
        data <- data.frame("(Intercept)" = rep(1, nrow(data)))
    }
###
    .traceR(30, head(data), fnm)
    any2list <- function(object, data, contrast) {       # <===
        .traceR(940, , fnm, "-any2list in <model.matrix.reStruct.U> STARTS")
        form2list <- function(form, data, contrast) {    # <===
        .traceR(950, ,fnm, "form2list in <model.matrix.reStruct.U> STARTS")

            if (length(asOneFormula(form)) == 0) {
                tt1 <- list("(Intercept)" = rep(1, dim(data)[1]))
                .traceR(951, tt1, fnm)   
                .traceR(951, , fnm, "tt1 returned from <form2list>. EXIT1.")   
                return(tt1)
            }
            tt2 <- as.data.frame(unclass(model.matrix(form, model.frame(form, 
                data), contrast)))
                 .traceR(952, head(tt2), fnm)   
                 .traceR(952,    , fnm, "tt2 returned from <form2list>. EXIT2")  
            tt2    
        }
        .traceR(941, , fnm, "any2list in <model.matrix.reStruct.U> CONTINUES")      
        if (inherits(object, "formula")) {
            tt3 <- form2list(object, data, contrast)
            .traceR(953, str(tt3), fnm, "str(tt3) returned")   
            .traceR(953,    ,      fnm, "tt3 returned from <form2list>. EXIT3")  
            return(tt3)
        }
        if (is.list(object)) {
            return(unlist(lapply(object, form2list, data = data, 
                contrast = contrast), recursive = FALSE))
        }
        .traceR(940,  , fnm, "any2list in <model.matrix.reStruct.U> returns NULL")
        return(NULL)
    }
###
    .traceR(50, pdDef, fnm)
    if (pdDef){     # No pdKronecker
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
       .traceR(801, form2, fnm)
       form2name <- names(form2) 
       .traceR(802, form2name, fnm)
       form2 <- form2[[1]]
       .traceR(803, form2, fnm)
       form  <- form2   ####  <- 
       val <- model.matrix(form,data)
       ncols <- ncol(val)
       nams <- dimnames(val)[[2]]
       .traceR(804, nams, fnm)
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
    .traceR(970, str(val), fnm, "str(val)")
    .traceR(1, "model.matrix.reStruct.U ENDS", fnm, msg= TRUE)
    val
}
