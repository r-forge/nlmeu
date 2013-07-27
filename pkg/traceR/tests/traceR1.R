

### ====================================
TracedFunction1 <- function(.x){
   .functionLabel <- "TF1"                                # Function label (recommended)
   .traceR <- attr(options()$traceR, "fun")
   .traceR <- if (is.null(.traceR)) function(...){} else .traceR      
   # functionLabel <- "TF1"
   .traceR(1)
   y <- 1

   .traceR(2)
   .x + 10*y
}

TracedFunction2 <- function(x){ 
   .functionLabel <- "TF2"                                # Function label (recommended)
   .traceR <- attr(options()$traceR, "fun")
   .traceR <- if (is.null(.traceR)) function(...){} else .traceR      
   # functionLabel <- "TF1"
   .traceR(1)
   x^2
}


TracedFunction <- function(x){
   .functionLabel <- "TF"                               # Function label (recommended)
   .traceR <-  attr(options()$traceR, "fun")
   .traceR <-  if (is.null(.traceR)) function(...){} else .traceR      

   .traceR(1)
    zz1 <- TracedFunction1(x)

    mtx <- diag(x,2)
    zz1 <- TracedFunction1(x+33)
   .traceR(2, lbl = "mtx")
   
    envx <- new.env()
    assign("Mtx", 2*mtx, env = envx)
   .traceR(70, lbl = "envx", store = FALSE)
   
    TracedFunction2(x-3)
    z <- NULL
   .traceR(80, lbl= "80a")
   x
}

library(traceR)

## Ex1: By default: No tracing
options(traceR = NULL)                           #  Just in case reset traceR options to NULL
TracedFunction1(1)

## Ex2: Uniform trace for all functions

# library(SOAR)

traceR.on()
TracedFunction(1)
TracedFunction1(1)
.traceRmap
getwd()
Objects()
traceR.off()



## Ex3: Modify default options
## Selected id, prefix for object names in .R_Cache, function to select 

fenv1 <- function(env){
    e <- new.env()
    nms <- ls(env)
    if ("zz1" %in% nms) rm("zz1", envir = env) # Object zz1 removed
    e <- env
    return(e)
}


traceR.on(defaultOptions = list(id = c(1, 2, 70), prefix = "z", modifyEnv = fenv1))
TracedFunction(1)
.traceRmap
traceR.off()


## Ex4: Modify function specific options
traceR.on(
  TF  = list(id = c(1, 70) ),
  TF1 = list(id = c(1, 2) ),
  TF2 = list(),                # TF2 will not be traced
  defaultOptions = list(id = c(1, 2, 80), prefix = "z" , asList = FALSE)
)
TracedFunction(1)
traceR.report()
traceR.off()
Remove(list = Objects())
detach(package:traceR)
detach(package:SOAR)

