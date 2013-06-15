.traceRdefault <- function(id, lbl = character(0), store = TRUE){

## print("===> id <=======================================================================")
## print(id)
x <- options()$traceR
xNms <- names(x)
hl <- x[xNms]

attrNms <- names(attributes(x))
.traceRfunctionEnv <- new.env()
.traceRfunctionEnv <- parent.frame()

fn <- exists(".functionLabel", envir = .traceRfunctionEnv)
fname <- if (fn) eval(expression(.functionLabel), envir = .traceRfunctionEnv) else "."
## cat("-fname \n")
## print(fname)

hlf <- hl[[fname]]
ids <- hlf[["id"]]                          # vector of ids specific to a function
if (is.null(ids)) ids <- attr(x, "id")
## print(ids)
sel1 <- id %in% ids || is.null(ids)
## print(sel1)
if (!sel1 || (is.list(hlf) && !length(hlf))) return()

## cat("- traceRmap \n")
## print(.traceRmap)
cNms1 <- if (nrow(.traceRmap)) as.character(.traceRmap[nrow(.traceRmap), c("fTree")]) else  character(0)


## cat("-1.cNms1 \n")
## print(cNms1)
cNms <- if (length(cNms1)) unlist(strsplit(cNms1, "->", fixed = TRUE))  else character(0)
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
fNms <- paste(cNms, collapse = "->")
## cat("--fNms \n")
## print(fNms)

# cNms1 <- c(id, fNms)
## cat("-2. cNms1 \n")
## print(cNms1)
recno <- nrow(.traceRmap) + 1
lbl <- if (length(lbl)) lbl else ""

prefix <- attr(x, "prefix")

xin <- hlf[["xin"]]                    
if (is.null(xin)) xin <- attr(x, "xin")

## print("-xin")
## print(xin)
elist <- as.list(.traceRfunctionEnv)
elistNms <- names(elist)
enms <- if (is.null(xin)) elistNms else intersect(elistNms, xin)

xout <- hlf[["xout"]]                    
if (is.null(xout)) xout <- attr(x, "xout")

## print(xout)

enms <- setdiff(enms, xout)
## print(enms)
elist <- if (length(enms)) elist[enms] else list() 
elistNms1 <- names(elist)
elistLen <- length(elistNms1)
xname <- if (elistLen && store) paste(prefix, recno, sep = "") else ""

if (elistLen && store){
  assign(xname, elist)
  Store(list = xname)
}

  .tmp <- data.frame(recno = recno,  fLbl = fname, id = id, idLbl =lbl, 
  fTree = fNms, res = xname, nObj = length(enms), nObjAll = length(elistNms), stored = store, stringsAsFactors = FALSE)
## print(".tmp")
## print(.tmp)

assign(".traceRmap", rbind(.traceRmap, .tmp), envir= .GlobalEnv)
}


