traceR.o <- function(nms, funLab, Map){
  Mapx <- if (missing(Map)) get(".traceRmap", envir = .GlobalEnv) else Map
  nmsx <- names(Mapx)
  chknms <- c("recno", "fLbl", "id", "env", "list", "optim", "added", "modified", "removed")
  chknms
}


traceR.summary <- function(map){
  nms <- attr(options()$traceR, "mapVars")
  nms <- c(nms, "list", "optim", "added", "modified", "removed")
  Mapx <- if (missing(map)) get(".traceRmap", envir = .GlobalEnv) else map
  nmsx <- names(Mapx)
  Mapx <- Mapx[, intersect(nms, nmsx)]
  print(names(Mapx))
  tmp <- "#-->"
  mapx <- within(Mapx, {
      xid <- paste("#===>",  fLbl, ", id =", id)
      
      xid2 <- ifelse(fLbl == fTree, "", paste(tmp,  fTree, ""))   
      added2 <- paste("- added: ", added, sep = "")
      cx1 <- ifelse(nchar(added), added2, "")
      print(modified)
      modified2 <- paste("- modified: ",  modified)
      cx2 <- ifelse(nchar(modified), modified2, c(""))
      print(cx2)
      cx <- paste(cx1,cx2, sep = "")
      print(cx)
      xad <- ifelse(nchar(cx), paste("#-", cx, "\n"), "")
  })

#v <- melt(mapx, id.vars = "recno", measure.vars = c("idLbl", "xid","xid2", "xad"))
#v$variable <- ordered(v$variable, levels = c("xid", "idLbl","xad"))
#ord <- order(v$recno, v$variable)
 
# writeLines(v)
#vx <- v[ord,]
# val <- ifelse(nchar(vx$value) ,  vx$value
mapx
}


Work_LATER <- function(){
vx <- traceR.summary(ttx)
writeLines(vx$value)


f_9 <- function(id.vars, measure.vars){
varnames <- names(data)
if (!missing(measure.vars) && is.numeric(measure.vars)) measure.vars <- varnames[measure.vars]

if (missing(id.vars) && missing(measure.vars)) {
    categorical <- sapply(data, function(x) class(x)[1]) %in% c("factor", "ordered", "character")
    id.vars <- varnames[categorical]
    measure.vars <- varnames[!categorical]
    message("Using ", paste(id.vars, collapse = ", "), " as id variables")
}
res <- list(categorical = categorical, measure.vars = measure.vars, id.vars = id.vars,varnames=varnames)
attr(res,"added") <- "xyz"
return(list= res)
}
environment(f_9) <- e_2
f_9()
}


traceRead.default <- function(el) names(el)

traceRead <- function(traceRmap = .traceRmap, hook = traceRead.default){
res  <- .traceRmap$res
res <- res[res != ""]
# print(res)

resx <- lapply(as.list(res), function(el) names(eval(parse(text=el))))
names(resx) <- res
resx
}

