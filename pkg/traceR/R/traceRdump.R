fOpts <- function(fname, tracef, attrs){ 

  fun <- tracef[["fun"]]
  if (is.null(fun)) fun <- attrs[["fun"]]
 
  ids <- tracef[["id"]]
  if (is.null(ids)) ids <- attrs[["id"]]
  
  modifyEnv <- tracef[["modifyEnv"]]
  if (is.null(modifyEnv)) modifyEnv <- attrs[["modifyEnv"]]
 
  asList <- tracef[["asList"]]
  if (is.null(asList)) asList <- attrs[["asList"]]
 
  list(ids = ids, fun = fun, modifyEnv = modifyEnv, asList = asList)
}

fNames <- function(fname){

  cnms <- if (nrow(.traceRmap)) as.character(.traceRmap[nrow(.traceRmap), c("fTree")]) else  character(0)

 
  cNms <- if (length(cnms)) unlist(strsplit(cnms, "->", fixed = TRUE))  else character(0)

  mtch <- match(fname, rev(cNms))
  mtch <- length(cNms) + 1 - mtch
  if (is.na(mtch)) mtch <- 0
  cNms <- if (mtch)  cNms[1:mtch]  else c(cNms, fname)
  paste(cNms, collapse = "->")
}

.traceRdump <- function(id, lbl = character(0), store = TRUE, first = FALSE, auto = FALSE){
  .traceRfunctionEnv <- new.env()
  .traceRfunctionEnv <- parent.frame()
  fn <- exists(".functionLabel", envir = .traceRfunctionEnv)
  fname <- if (fn) eval(expression(.functionLabel), envir = .traceRfunctionEnv) else "."

  traceR <- options()$traceR
  tracef <- traceR[[fname]]
  attrs <- attributes(traceR)

  fopts <- fOpts(fname, tracef, attrs)
 
  prefix <- attr(traceR, "prefix")
  select <- id %in% fopts$ids || is.null(fopts$ids)
  
  if (!select || (is.list(tracef) && !length(tracef))) return()

  fNms <- fNames(fname)

  recno <- nrow(.traceRmap) + 1
  lbl <- if (length(lbl)) lbl else ""
  

  Nms <- ls(.traceRfunctionEnv)
  if (!is.null(fopts$modifyEnv)){
    modifyEnv <- fopts$modifyEnv
    res <- new.env()
    res <- modifyEnv(.traceRfunctionEnv)
    .traceRfunctionEnv <- res
  }
  nms <- ls(.traceRfunctionEnv)

  .envInfo <- list(recno = recno, msg = paste("Env created by .traceRdump"))
  assign(".envInfo", .envInfo, .traceRfunctionEnv)
  
  elist <- if (fopts$asList) {
     if (length(nms)) mget(nms, .traceRfunctionEnv) else list()
  } else {
    if (length(nms)) .traceRfunctionEnv else emptyenv()
  }  
  xname <- if (length(nms) && store) paste(prefix, recno, sep = "") else ""

  if (nchar(xname)) {
    assign(xname, elist)
    Store(list = xname)
  }
  
  tmp <- data.frame(recno = recno,  fLbl = fname, id = id, idLbl =lbl, 
  fTree = fNms, env = xname, nObj = length(nms), nObjAll = length(Nms), store = store, 
   first = first, auto= auto, stringsAsFactors = FALSE)
  assign(".traceRmap", rbind(.traceRmap, tmp), envir= .GlobalEnv)
}
