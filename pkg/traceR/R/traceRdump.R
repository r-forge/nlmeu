fOpts <- function(fname, tracef, attrs){ 

  fun <- tracef[["fun"]]
  if (is.null(fun)) fun <- attrs[["fun"]]
 
  ids <- tracef[["id"]]
  if (is.null(ids)) ids <- attrs[["id"]]
  
  xin <- tracef[["xin"]]
  if (is.null(xin)) xin <- attrs[["xin"]]
 
  xout <- tracef[["xout"]]
  if (is.null(xout)) xout <- attrs[["xout"]]
  
  asList <- tracef[["asList"]]
  if (is.null(asList)) asList <- attrs[["asList"]]
 
  list(ids = ids, fun = fun, xin = xin, xout = xout, asList = asList)
}

fNames <- function(fname){

  cnms <- if (nrow(.traceRmap)) as.character(.traceRmap[nrow(.traceRmap), c("fTree")]) else  character(0)

  ## cat("-1.cnms \n")
  ## print(cnms)
  cNms <- if (length(cnms)) unlist(strsplit(cnms, "->", fixed = TRUE))  else character(0)
  ## cat("--1.cNms \n")
  ## print(cNms)

  mtch <- match(fname, rev(cNms))
  mtch <- length(cNms) + 1 - mtch
  if (is.na(mtch)) mtch <- 0
  ## cat("-mtch")
  ## print(mtch)
  cNms <- if (mtch)  cNms[1:mtch]  else c(cNms, fname)
  ## cat("--2.cNms \n")
  ## print(cNms)
  paste(cNms, collapse = "->")
}

.traceRdump <- function(id, lbl = character(0), store = TRUE){
  ## .traceRfunctionEnv <- new.env()
  .traceRfunctionEnv <- parent.frame()
  fn <- exists(".functionLabel", envir = .traceRfunctionEnv)
  fname <- if (fn) eval(expression(.functionLabel), envir = .traceRfunctionEnv) else "."

  traceR <- options()$traceR
  tracef <- traceR[[fname]]  # hlf
  attrs <- attributes(traceR)

  fopts <- fOpts(fname, tracef, attrs)
 
  prefix <- attr(traceR, "prefix")
  select <- id %in% fopts$ids || is.null(fopts$ids)
  
  if (!select || (is.list(tracef) && !length(tracef))) return()

  fNms <- fNames(fname)

  recno <- nrow(.traceRmap) + 1
  lbl <- if (length(lbl)) lbl else ""

  Nms <- ls(.traceRfunctionEnv)
  nms <- setdiff(Nms, fopts$xout)
  nms <- if (is.null(fopts$xin)) nms else intersect(nms, fopts$xin)

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
  fTree = fNms, res = xname, nObj = length(nms), nObjAll = length(Nms), store = store, stringsAsFactors = FALSE)
  assign(".traceRmap", rbind(.traceRmap, tmp), envir= .GlobalEnv)
}

