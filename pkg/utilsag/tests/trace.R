TracedFunction <- function(x){
   funNm <- "TracedFunction" 

   hl <-  options()$traceR            # List
   htrace <- hl[["TracedFunction"]]   # Function. Change Function name, after cutting and pasting!!!               
   if (is.null(htrace)) htrace <- attr(hl, "default")
   .traceR <- if (is.null(htrace))  function(id, xp , funNm, msg, lbl){ } else  htrace 
   
   .traceR(1, , funNm, "TracedFunction STARTS here")
   .traceR(2, x, funNm)
   .traceR(3, c(1,x), funNm)

    mtx <- diag(x,2)
   .traceR(4, mtx, funNm)
   .traceR(5, list(x, mtx), funNm)
   .traceR(6, str(mtx), funNm, lbl="6a")
   .traceR(6, capture.output(str(mtx)), funNm, lbl="6b")

    envx <- new.env()
    assign("Mtx", 2*mtx, env = envx)
   .traceR(70, as.list(envx), funNm, "Environment envx as list")
    z <- NULL
   .traceR(80, z, funNm, lbl= "80a")

   .traceR(80, capture.output(z), funNm, lbl = "80b")

   .traceR(1, ,funNm, "TracedFunction ENDS here")
   x
}


## Ex1: Default settings
htrace <- list("TracedFunction" = utilsag:::traceDefault)

options (traceR = htrace) 
options()$traceR[["TracedFunction"]]
TracedFunction(3)



## Ex2: Default tracePrint1 for all functions
htrace <- list()
attr(htrace, "default") <- utilsag:::tracePrint1
attr(htrace, "default")
options (traceR=htrace) 
TracedFunction(4)

## Ex3: traceList 

htrace <- list("TracedFunction" = utilsag:::traceList)

options (traceR=htrace) 
.traceList <- list()
TracedFunction(5)
names(.traceList)



