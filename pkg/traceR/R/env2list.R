process_el <- function(x, env){

## Extracts one element from env, inspects and returns
  cx <- as.character(x)
 
  tC <- tryCatch.W.E(env[[cx]])
  val <- tC$value
  if (missing(val)) return(env[[cx]])
  if (inherits(val, "error")){
     txt <- paste("substitute(", cx, ", env)")
     eval(parse(text = txt))
   } else get(cx, envir = env)
 
}  
  
env2list <- function(env, all.names = FALSE){
  environment(tryCatch.W.E) <- env
  nms <- ls(envir = env, all.names = all.names)
  res <- lapply(as.list(nms), FUN = function(nm){
     asnm <- as.name(nm)
     process_el(asnm, env)  
   })
  names(res) <- nms
  res
}


env2list_all <- function(Map, all.names = FALSE, pfxl = "l_", ...){
  nms <- attr(options()$traceR, "mapVars")
  Mapx <- if (missing(Map)) .traceRmap else  Map
  nmsx <- names(Mapx)
  Mapx <- Mapx[, intersect(nms, nmsx)]
  map <- Mapx[Mapx$env != "", ]  # Skip empty
  rec <- map$recno
  res1 <- map$env 
  # print(rec)
  auxF <- function(el){
    #print(str(el))
    env2list(eval(parse(text = el)), all.names = all.names) 
  }
  res <- lapply(as.list(res1), FUN = auxF)

  nmsl <- paste(pfxl, rec, sep ="")
  names(res) <- nmsl
  # print(nmsl)
  
  for (i in seq_along(res)){
    # print(paste("---", i))
    resi <- res[[i]] 
    nmi <- names(res)[i]
    # print(nmi)
    assign(nmi, resi)
    Store(list = nmi)
  }
  
  Mapx[rec, "list"] <- nmsl 
  if (missing(Map)) {
    assign(".traceRmap", Mapx, envir = .GlobalEnv)
    return(invisible())
 } else return(Mapx)
 
 }
 


