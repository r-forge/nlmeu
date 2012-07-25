TextSectionsCreate <- function(con, contrl  = list()) {
## Text divide into sections based on contrl

lines <- readLines(con)
ilns <- 1:length(lines)

cnms <- names(contrl)
Dt <- contrl$SectionData
if (!length(cnms) || is.null(Dt)) return()  # No changes
nmsecL <- rownames(Dt)
Dtnms <- names(Dt) 
srch <- "stringr" %in% search()
if (!srch)  require(stringr)

## Variables: 
##  Ladj  -  
##  Start
##  Multiple
Dtx <- within(Dt, {
  if (!("Lnadj" %in% Dtnms)) Lnadj <- rep(0, nrow(Dt)) 
  if (!("Start" %in% Dtnms)) Start <- rep(1, nrow(Dt)) 
  if (!("Mandt" %in% Dtnms)) Mandt <- rep(TRUE, nrow(Dt)) 
  if (!("Mult"  %in% Dtnms)) Mult <- rep(TRUE, nrow(Dt)) 
}) 
  
resin <- lapply(X=as.list(rownames(Dt)), FUN= function(el){
  ## Returns matrices containing information about *All* occurences 
  ## Go through all sections specified in Dt 
   tx <- str_locate(lines, Dt[el,"SectionLabel"])  # 
   i0 <- ilns[!is.na(tx[, 1])]
   if (length(i0) == 0) return()    # NULL returned
   #print(i0)
   tx0 <- matrix(tx[i0, ], nrow = length(i0))
   #print(tx0)
   mi0 <- matrix(i0, ncol = 1)
   mtx <- cbind(mi0, matrix(tx0, ncol=2, byrow= TRUE))
   colnames(mtx) <- c("lnx","start","end")
   mtx                                     # matrix returned
   })
names(resin) <- nmsecL 

## Create res0 from resin
res0<- lapply(X=as.list(rownames(Dt)), FUN= function(el){
  ## Returns vectors containing information about occurences matching Start variable 
  ## Go through all sections specified in Dt 
  Strt <- Dtx[el, "Start"]
  tres <-  resin[[el]] 
  if (is.null(tres)) return()
  dnms <- dimnames(tres)
  #print(str(tres))
  logi <- (1:nrow(tres))[Strt == tres[ , "start"]]
  tt <- tres[logi,]
  if (is.vector(tt)) tt <- as.matrix(t(tt))
  tt
})
names(res0) <- nmsecL
## Summary information extracted from res0

ltot <- sapply(res0, FUN=function(el) if (is.null(el)) 0  else nrow(el))  # Total number of occurences
lmin <- sapply(res0, FUN=function(el) if (is.null(el)) return(NA)  else el[1,"lnx"]) # Min line number
names(lmin) <- nmsecL

lmax <- sapply(res0, FUN=function(el) if (is.null(el)) NA  else el[nrow(el),"lnx"])
names(lmax) <- nmsecL

#msg1 <- paste("Mandatory section NOT found")
#msg2 <- paste("Mandatory section found with no corresponding Start value.)

warn1 <- ifelse(ltot == 0 & Dtx$Mandt, 1,0)    # Warning: See msg1

warn0 <-  sapply(as.list(names(res0)), 
     FUN = function(el){
     t1 <- res0[[el]][,"start"] # multiple
     t2 <- Dtx[el, "Start"]
     tt <- warn1[el]
     #print(tt)
     if (!tt) { 
        if (t2 %in% t1)  0 else 2
     } else tt
     })
     
DtxAll <- data.frame(Dtx, ltot, lmin, lmax, warn0)

Dtx2 <- subset(DtxAll, subset = warn0 == 0)

dtx <- within(Dtx2,{
  Linit <- lmin + Lnadj
  Lend  <- c(Linit[-1], length(lines) + 1) - 1
  names(Lend) <- names(Linit)   
})
# dtx <- data.frame(Dtx2,ltot, lmin, lmax, Linit, Lend, warn0)
rawSections<- lapply(X=as.list(rownames(dtx)), FUN= function(el){
   #print(el)
   idx <- (dtx[el,"Linit"]): (dtx[el,"Lend"])
   #print(idx)
   lines[idx]   
})
names(rawSections) <- rownames(dtx)
attr(rawSections, "LogData") <- dtx
attr(rawSections, "FUN") <- contrl$FUN 
class(rawSections) <- "TextSections"  # Raw text sections
rawSections
} # SectionsCreate ENDS

