## From nlme package

isInitialized.pdMat <-
  function(object)
{
  length(object) > 0
}

"coef<-.pdMat" <-
  function(object, ..., value)
{
  value <- as.numeric(value)
  if (isInitialized(object)) {
    if (length(value) != length(object)) {
      stop("Cannot change the length of the parameter after initialization")
    }
  } else {
    return(pdConstruct(object, value))
  }
  class(value) <- class(object)
  attributes(value) <- attributes(object)
  value
}

Names.pdMat <-
  function(object, ...)
{
  as.character(attr(object, "Dimnames")[[2]])
}

