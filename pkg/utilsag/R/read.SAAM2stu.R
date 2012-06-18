# source("C:\\Users\\agalecki\\Google Drive\\MySoftware\\_rforgenlmeU\\pkg\\utilsag\\R\\read.SAAM2stu.R")
read.SAAM2stu <- function(file, xverbose= list()){
  xverbos <- do.call("utilsagXverboseControl", args=list())[["read.SAAM2stu"]]
  if (!missing(xverbose)) xverbos <- xverbose[["read.SAAM2stu"]]
  Xverbose(1, "read.SAAM2stu  STARTS   <=####", xverbose=xverbos)
 
 auxFunSAAM2Stu <- function(cmpnt, xverbose = xverbos){
    # For example cmpnt = "EXPER", "XFER"
    Xverbose(901, paste("---", cmpnt, "---"), xverbose=xverbos)
    ocmpnt <- cList[[cmpnt]]    # Character vector
    Xverbose(911, str(ocmpnt), xverbose=xverbos)
    trmd <- stringr::str_trim(ocmpnt)
    blnk1 <- stringr::str_locate(trmd, " ")
    blnk1 <- blnk1[,1] 
    strl <- stringr::str_length(trmd)
    #print("here1")
    split0 <- strsplit(trmd, " ") 
    variable <- sapply(split0, FUN= function(el) el[1])
    Xverbose(913, variable, xverbose=xverbos)
    if (cmpnt  %in% c("STUDY", "MODEL","NOTES")) return(trmd)
    if (cmpnt  %in% "GLOBL.EQ"){ 
        len <- stringr::str_length(trmd)
        return(stringr::str_trim(stringr::str_sub(trmd, 9, len)))
    }
    #ulabs0 <- unique(labs0)  # Unique labs
    cval <- stringr::str_sub(trmd,blnk1,strl)
    value <- stringr::str_trim(cval, side="left")  
    Cblock <- cumsum(ifelse(variable == cmpnt, 1 ,0))
    # Rename dup EQ variable  
    eqLoc <-  ifelse(variable == "EQ",1 ,0)
    eqLoc2 <- unlist(tapply(eqLoc, Cblock, cumsum))
    eqLoc3 <- ifelse(eqLoc2 == 0 ,"",as.character(eqLoc2))
    variable <- paste(variable, eqLoc3, sep="")
    uCblock <- unique(Cblock)
    Xverbose(916, length(uCblock), xverbose=xverbos)
    
    if (length(uCblock) == 1){              # e.g. "COMPDEF","SAMPLE"
      #Xverbose(925, value, xverbose=xverbos)
      #Xverbose(925, variable, xverbose=xverbos)  
      names(value) <- variable
      Xverbose(924, str(value), xverbose=xverbos)  
      return(value)
     } 
    if (length(uCblock) > 1){
      
      dt <- data.frame(BlockNo=Cblock, variable, value)
      idvars <- c("BlockNo","variable")
      dtm <- melt.data.frame(dt, id.vars= idvars)
      cdtm <-cast(dtm)
      res <- as.data.frame(apply(cdtm,2, FUN= function(el) ifelse(is.na(el), "",el)))
      return(res)
    }
    Xverbose(950, str(cdtm), xverbose=xverbos)

 }
 

 bsnm <- basename(file)
 Xverbose(2, bsnm, xverbose=xverbos)
 logi <- grep(".", bsnm, fixed =TRUE)
 if (length(logi) == 0) logi = FALSE
 if (!logi)  file <- paste(file, ".stu", sep="")

 lines <- readLines(file)
 Xverbose(3, dim(lines), xverbose=xverbos)
  # Extracting data
  i0 <- charmatch("STARTDATA",lines)
  i1 <- charmatch("ENDDATA",lines)
  logi <- (i0 > 0) && (i1>0)
  if (!logi) warning("No data in stu file")
  
  cdata <- NULL  
  if (i0 + 1 > i1 - 1) stop("Data corrupted") else cdata <- lines[i0 : i1] 
  
  # Data skipped
  lins0 <- lines[-(i0:i1)]
  bseps <- c("STUDY","MODEL", "COMPDEF","COMPART","XFER","LOSS",
            "SAMPLE","EXOG.IN","EXPERIMENT","EVENTLINE","GLOBL.EQ", "PARAM", "NOTES")
  lines0  <- stringr::str_replace_all(lins0,"\t", " ")
  ix  <- !is.na(stringr::str_locate(stringr::str_sub(lines0, 1, 5), "NOTES")[,2]) 
  i0 <- (1:length(ix))[ix]  # i0 contais line number for NOTES
  
  labs0 <- stringr::word(stringr::str_trim(lines0), 1 )   # Trimmed line labels, some are ""
  if (length(i0)) ifelse((1:length(labs0))> i0, "",labs0 )
  
  labloc <-  stringr::str_locate(lines0, labs0)  # Matrix (2 cols), start and end position of label
  
  idx0 <- 1:length(labs0)
  logi0 <- labs0 %in% bseps
 
  idx1  <- idx0[logi0]
  labs1 <- labs0[idx1]
  
  idx2 <- match(bseps, labs1)
  ln0  <- idx1[idx2]
  labs2 <- labs0[ln0]
  ln1  <- c(ln0[-1]-1,length(lines0))
  
  # lines0 <- gsub("\t"," ", lines0, fixed=TRUE)
  aux1F <- function(el, ...){
   ix <- ln0[el]: ln1[el]
   lines0[ix]
  }
  
  res <- sapply(as.list(1:length(labs2)), aux1F)
  cList <- c(res, list(cdata)) 
  names(cList) <- c(labs2, "DATA")
  res2 <- sapply(as.list(labs2), auxFunSAAM2Stu)
  Xverbose(11, length(res2), xverbose=xverbos)
  names(res2) <- labs2
  Xverbose(1, "read.SAAM2stu ENDS   <=####", xverbose=xverbos)
  return(res2)
}
#dp <- file.path("C:","Users","agalecki","Google Drive","_rforgenlme","pkg","utilsag","inst","SAAM2")
#fp <- file.path(dp,"CpeptideKineticsIVGTT")
# tt <- read.SAAM2stu(fpin)

