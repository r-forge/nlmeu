setdiffin.list <- function(x, y){
  nms1 <- names(x)
  nms2 <- names(y)
  if (!identical(nms1,nms2)) stop("Names are different")
  if (!length(nms1)) stop("List is empty")
  res <- mapply(identical, x, y)
  names(res)[!res]
}



setdiff.list <- function(x, y, diffin = FALSE){
# if diffin is TRUE then *modified* components  
  nms1 <- names(x)
  nms2 <- names(y)
  nmsa <- setdiff(nms1, nms2)  # Names added
  nmsr <- setdiff(nms2, nms1)  # Names removed
  nmsi <- intersect(nms1, nms2)

  nmsd <- character(0)
  if (diffin  && length(nmsi)) { 
    nmsd <- setdiffin.list(x[nmsi], y[nmsi])
  }

  if (!length(c(nmsa,nmsr,nmsd))) return(list())
  nmsl <- nmsa 
  if (diffin) nmsl <- c(nmsl, nmsd)
  res <- x[nmsl]
  if (length(nmsa)) attr(res, "added") <- nmsa
  if (length(nmsr)) attr(res, "removed") <-  nmsr
  if (length(nmsd)) attr(res, "modified") <-  nmsd
  res  
}

union.list <- function(x,y){
  nms1 <- names(x)
  nms2 <- names(y)
  nmsu <- union(nms1,nms2)
  res <- vector("list", length(nmsu))
  names(res) <- nmsu
  res[nms1] <- x
  res[nms2] <- y  # Elements in y over-write those corresponding in x
  res
}



traceR.optim <- function(Map, all.names = FALSE, pfxl = "l_", pfxd ="d_"){
  nms <- attr(options()$traceR, "mapVars") 
  Mapx <- if (missing(Map)) get(".traceRmap", envir = .GlobalEnv) else Map
  nmsx <- names(Mapx)
  Mapx <- Mapx[, intersect(nms, nmsx)] 
  
 map <- Mapx[Mapx$env != "", ]  # Skip empty
 #print("--map")
 #print(str(map))
 env2list_all(map, all.names = all.names, pfxl = pfxl) 
 map[, c("list", "optim", "added","modified", "removed")] <- ""   
 #print(str(map))
 
 rec <- map$recno
 map[, "list"] <- paste(pfxl, map[ ,"recno"], sep = "") 

  # pfx2 <- "d_"
 nr <- nrow(map) 
 cuml <- list()
 cumadded <- character(0)
 map[1, "optim"] <- map[1,"list"]
 for (i in 2:nr){
   #cat("----> i = ", i, "processed \n") 
   rx <-  c(i-1,i)
   # print(rx)
   flb <- map[rx,"fLbl"]
   # print(flb)
   if (flb[1] == flb[2]) {
     # print("if1")
     
     res <- map[rx, "list"]
     # print(res)
     # e <- new.env() 
     
     t2 <- eval(parse(text = res[2]))
     t1 <- eval(parse(text = res[1]))
     dl <- setdiff.list(t2, t1, diffin = TRUE)
     # print(names(dl))
     modified <- attr(dl, "modified")
     added <- attr(dl, "added")
     cuml <- union.list(cuml, dl)
     # print(names(cuml))
     
     map[rx[2], "added"] <- paste(added, collapse =",") 
 
     if (length(modified) || (i == nr)){
      # print("if2")
      # print(names(cuml))
      # dump cumenv at i
      map[rx[2], "modified"] <- paste(modified, collapse =",")
      # print(map)
      xname <- paste(pfxd, map[rx[2],"recno"], sep ="")
      map[rx[2], "optim"] <- xname
      # print(xname)
      assign(xname, cuml)
      Store(list = xname) 
      cuml <- list()
      # cumadded <- character(0)
     }
     
     added1 <-  unlist(strsplit( map[rx[1], "added"], split = ",", fixed = TRUE))
     cumadded <- c(cumadded, added1)
     # print("-cumadded")
     # print(cumadded)

     if (length(intersect(added1, modified))) {
     # print("-if3")
     # print(rx)
     #print(intersect(cumadded, modified))
     #xname <- paste(pfxd, map[rx[1],"recno"], sep ="") 
     #map[rx[1], "optim"] <- xname
     #assign(xname, cuml)
     #Store(list = xname) 
     #rmx <- setdiff(cumadded, modified)
     #txt1 <- unlist(strsplit(rmx, split = "','", fixed = TRUE))
     #print(txt1)
     #stmt <- paste(pfxd, map[rx[2],"recno"], "[c('", txt1,"')] <- NULL", sep ="") 
     #print(stmt)

     #parse(text = stmt)
     #print("if3e")
     }
   } 
      # print("-iflb")
      #print(flb)
      if (flb[1] != flb[2]) {
       ## map[rx[1], "modified"] <- paste(modified, collapse =",")
       # print(i)
       # print(map[rx, c("optim","list")])
       map[rx[2], "optim"] <- map[rx[2], "list"]
       
       cuml <- list()  
       cumadded <- character(0)  
      }
}
nms <- c("list", "optim", "added", "modified","removed")
map <- map[, nms]
Mapx <- data.frame(Mapx, list = "", optim = "", added = "", modified = "", removed ="", stringsAsFactors = FALSE)  
# print(map)
# print(Mapx)
Mapx[rec, nms]  <- map

if (missing(Map)) assign(".traceRmap", Mapx, .GlobalEnv) else return(Mapx)
}

