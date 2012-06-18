
"missPat" <- function(...)
{    args  <- as.list(substitute(list(...)))[-1]
     args.t <- paste("miss.frame$",args,sep="",collapse=",")
     miss.frame <- as.data.frame(ifelse(is.na(data.frame(args)),"X","-"))
     txt <- c("paste(", args.t, ",sep='')")
     txt<-paste(txt,sep='')
     eval(parse(text=txt))
}
sigma <-  function(object, ...) UseMethod("sigma")

sigma.default <- function(object, ...) object$sigma


nlmeUXverboseControl <- function(
  logLik1.lme = numeric(),
  model.matrix.reStruct.U= numeric(),
  simulateY.lme = numeric(), 
#  print.Pwr =numeric(),
  Pwr.lme = numeric()
  
){
# xcontrl <- nlmeUXverboseControl(Pwr.lme = 1:900)
list(
  logLik1.lme  = logLik1.lme,
  model.matrix.reStruct.U = model.matrix.reStruct.U,
  simulateY.lme  = simulateY.lme,
#  print.Pwr  = print.Pwr,
  Pwr.lme  = Pwr.lme
)
}

Xverbose <- function(xv, object, xverbose= numeric()){}