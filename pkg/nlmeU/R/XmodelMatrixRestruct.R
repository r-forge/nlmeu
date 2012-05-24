### Inactivated model.matrix.reStruct in reStruct.R file (May 2012)

model.matrix.reStruct <-
  function(object, data, contrast = NULL, ...)
{
## Changes made to accommodate pdKronecker class and methods (May, 2012)
  if (is.null(form <- formula(object, asList = TRUE))) {
    stop("Cannot extract model matrix without formula")
  }
  form1 <- asOneFormula(form)
  if (length(form1) > 0) {
    data <- model.frame(form1, data = data)
  } else {
    data <- data.frame("(Intercept)" = rep(1, nrow(data)))
  }
  any2list <- function( object, data, contrast ) {
    form2list <- function(form, data, contrast) {
      if (length(asOneFormula( form )) == 0) {# the ~ 1 case
        return(list("(Intercept)" = rep(1, dim(data)[1])))
      }
      as.data.frame(unclass(model.matrix(form,
                                         model.frame(form, data),
                                         contrast)))
    }
    if (inherits( object, "formula" )) {
      return( form2list( object, data, contrast ) )
    }
    if (is.list( object ) ) {
      return( unlist(lapply(object, form2list, data = data, contrast = contrast),
                     recursive = FALSE ) )
    }
    return( NULL)
  }
  value <- as.list(lapply(form, any2list,
                          data = data, contrast = contrast))
  ## save the contrasts currently in effect for later predictions
  contr <- as.list(lapply( as.data.frame(data), function(x)
                  if( inherits( x, "factor" ) &&
                     length(levels(x)) > 1) contrasts(x) else NULL ))
  contr[names(contrast)] <- contrast

  ncols <- as.vector(unlist(lapply(value, length)))
  nams <- if (length(value) == 1) {
    names(value[[1]])
  } else {
    paste(rep(names(value), ncols), unlist(lapply(value, names)), sep = ".")
  }
  val <- matrix(unlist(value), nrow = nrow(data),
                dimnames = list(row.names(data), nams))
  attr(val, "ncols") <- ncols
  attr(val, "nams") <- as.list(lapply(value, names))
  attr(val, "contr") <- contr
  val
}
