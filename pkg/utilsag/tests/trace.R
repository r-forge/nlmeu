library(utilsag)

TestedFunction <- function(x){
   funNm <- "TestedFunction"  
   .traceFunction(1, "TestedFunction STARTS here", funNm, tags = c("1","msg"))
   .traceFunction(2, x, funNm)
   .traceFunction(3, c(1,x), funNm)
    mtx <- diag(x,2)
   .traceFunction(4, mtx, funNm, tags = c("mtx"), alt = capture.output(str(mtx)))
   .traceFunction(5, list(x, mtx) , funNm, tags = c("list"))
   .traceFunction(6, capture.output(str(mtx)), funNm, tags = c("mtx"))
    envx <- new.env()
    assign("Mtx", 2*mtx, env = envx)
   .traceFunction(7, as.list(envx), funNm, tags ="env")
    z <- NULL
   .traceFunction(8, z, funNm)

   .traceFunction(9, capture.output(z), funNm)

   .traceFunction(1, "TestedFunction ENDS here", funNm, tags = c("1","msg"))
   x
}


## Ex1: Default settings
trOps1 <- traceOptions()  # Default trace options
options(trace = trOps1) 
str(options()$trace)

.traceList <- list()
TestedFunction(3)
.traceList


## Ex2. Default settings modified. 

trOps2 <- traceOptions(    # Default trace options modified
 xtags= c("1","mtx"),
 xuid = 1:6
)  
options(trace = trOps2)    #  
str(options()$trace)

.traceList <- list()
TestedFunction(5)
.traceList

## Ex3. Specific settings for TestedFunction(). tracePrint() function used.

funL <- list(TestedFunction = list(xtrace = "tracePrint", xtags="msg"))
trOps3 <- traceOptions(    # Default trace options modified
 funL = funL,
 xtags= c("1","mtx"),
 xuid = 1:6
)  
options(trace = trOps3)    #  


str(options()$trace)

TestedFunction(7)

