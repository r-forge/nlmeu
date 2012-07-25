# source(file.path(dpR,"read.SAAM2.R"))
# ## tt <- read.SAAM2("GlucoseIVGTT")

SAAMstuControl <- function(){ 
## Controls sectioning and processing of SAAM stu. file 
## Used by utilsag:::processTextFile
require(reshape)
SectionLabel <-  c(
  STUDY = "STUDY",
  MODEL = "MODEL", 
  COMPDEF = "COMPDEF",
  COMPART = "COMPART",
  XFER = "XFER",
  LOSS = "LOSS",
  SAMPLE = "SAMPLE",
  EXOG.IN = "EXOG.IN",
  CHANGE = "CHANGE",
  EXPERIMENT ="EXPERIMENT",
  EVENTLINE = "EVENTLINE",
  GLOBL.EQ = "GLOBL.EQ", 
  PARAM = "PARAM", 
  NOTES = "NOTES",
  STARTDATA = "STARTDATA");
  
  FUNdef <- list(    # Functions from stringr package are used
  
  init0 = function(txt, dt = NULL)  str_replace_all(txt, "\t", " "),
     
  trimmed = function(txt, dt = NULL){
        txt0 <- str_replace_all(txt, "\t", " ")
        str_trim(txt0) 
  },
  
  variable = function(txt, dt = NULL){
        trmd  <- trimmed(txt)   
        split0 <- strsplit(trmd, " ") 
        sapply(split0, FUN= function(el) el[1])   # Returns first word 
  },
 
  value = function(txt, dt =NULL){
      trmd  <- trimmed(txt)
      blnk1 <- str_locate(trmd, " ")
      blnk1 <- blnk1[, 1] 
      strl <- str_length(trmd)
      split0 <- strsplit(trmd, " ") 
      cval <- str_sub(trmd,blnk1,strl)
      str_trim(cval, side="left")                 # Returns second word
  },
  
  retx = function(txt, dt){
    value <- value(txt, dt)
    variable <- variable(txt, dt)
    nm <- rownames(dt)
    Cblock <- cumsum(ifelse(variable == nm, 1 ,0)) 
   
    ## Rename dup EQ variable    !!! ???   ????
    uCblock <- unique(Cblock)
    if (length(uCblock) == 1){              # e.g. "COMPDEF","SAMPLE"
      #Xverbose(925, value, fnm)
      #Xverbose(925, variable, fnm)  
      names(value) <- variable
      ##Xverbose(924, str(value), fnm)  
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
  },
     globleq = function(txt, dt){
          trmd <- init0(txt)
          len <- str_length(trmd)
          return(str_trim(str_sub(trmd, 9, len)))  
      }  
            
  );  # FUNDef ENDS here
 
  FUNx <- rep("", length(SectionLabel))                           # Functions assignment
  names(FUNx) <- names(SectionLabel)
  FUNx[c("STUDY", "MODEL", "COMPDEF", "NOTES")] <- "init0"
  FUNx[c("GLOBL.EQ")] <- "globleq"
  FUNx[c("COMPART","XFER","LOSS","SAMPLE","EXOG.IN", "EXPERIMENT",
          "EVENTLINE","PARAM")] <- "retx"     
  secDt <- data.frame(SectionLabel, FUN = FUNx, stringsAsFactors = FALSE)
  list(SectionData = secDt,  FUN = FUNdef)
} 


SAAMtxtControl <- function(){ 
## Controls sectioning and processing of SAAM's detailed summary output 
## Used by utilsag:::process TextFile
SectionLabel <-  c(
  SN = "Study Nam:",
  CS = "Computational Settings",
  EA = "Experiment Attributes",
  ME = "Model Equations",
  Dt = "Data",
  EI = "Exogenous Inputs",
  CC = "Change Conditions",
  GM = "General Model Information",
  EX = "Experiments",
  PM = "Parameter Values",
  V0 = "Values at",
  SV = "Calculated Sample Values and Data",
  ST = "Statistics",
  NT = "Notes"
)

  
## Lnadj: Adjust line number to start section  
  Lnadj <- rep(-2,length(SectionLabel))   # Move up two lines
  names(Lnadj) <- names(SectionLabel)
  Lnadj["SN"] <- 0

secDt <- data.frame(SectionLabel, Lnadj, Mult = FALSE, stringsAsFactors = FALSE)
list(SectionData = secDt)
}




