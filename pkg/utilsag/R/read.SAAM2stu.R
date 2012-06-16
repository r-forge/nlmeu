read.SAAM2stu <- function(file){
 auxFunSAAM2Stu <- function(cmpnt){
  # For example cmpnt = "EXPER", "XFER"
  print(cmpnt)
  ocmpnt <- cList[[cmpnt]]
  trmd <- stringr::str_trim(ocmpnt)
  blnk1 <- stringr::str_locate(trmd, " ")
  blnk1 <- blnk1[,1] 
  strl <- stringr::str_length(trmd)
  #print("here1")
  split0 <- strsplit(trmd, " ") 
  variable <- sapply(split0, FUN= function(el) el[1])
  if (variable  %in% c("STUDY", "MODEL", "GLOBL.EQ")) return(trmd)
  #ulabs0 <- unique(labs0)  # Unique labs
  cval <- stringr::str_sub(trmd,blnk1,strl)
  value <- stringr::str_trim(cval, side="left")  
  Cblock <- cumsum(ifelse(variable == cmpnt, 1 ,0))
  print("here2")
  # Rename dup EQ variable  
  eqLoc <-  ifelse(variable == "EQ",1 ,0)
  eqLoc2 <- unlist(tapply(eqLoc, Cblock, cumsum))
  eqLoc3 <- ifelse(eqLoc2 == 0 ,"",as.character(eqLoc2))
  variable <- paste(variable, eqLoc3, sep="")
  dt <- data.frame(BlockNo=factor(Cblock), variable, value)
  if (variable  %in% c("COMPDEF","SAMPLE")) return(trmd)
  dtm <- melt.data.frame(dt, id.vars= c("BlockNo","variable"))
  cdtm <-cast(dtm)
  # cdtx <- is.na(cdtm)
  res <- as.data.frame(apply(cdtm,2, FUN= function(el) ifelse(is.na(el), "",el)))
  res
 }
 

 bsnm <- basename(file)
 # print(bsnm)
 logi <- grep(".", bsnm, fixed =TRUE)
 if (length(logi) == 0) logi = FALSE
 if (!logi)  file <- paste(file, ".stu", sep="")

  lines <- readLines(file)

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
  sapply(as.list(labs2), auxFunSAAM2Stu)


}
#dp <- file.path("C:","Users","agalecki","Google Drive","_rforgenlme","pkg","utilsag","inst","SAAM2")
#fp <- file.path(dp,"CpeptideKineticsIVGTT")
# tt <- read.SAAM2stu(fpin)

