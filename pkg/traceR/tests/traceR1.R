

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

library(SOAR)

traceRsetup()
TracedFunction(1)
TracedFunction1(1)
.traceRmap
getwd()
Objects()
traceRcleanup()



## Ex3: Modify default options
## Selected id, prefix for object names in .R_Cache, filter for objects to be saved
traceRsetup(defaultOptions = list(id = c(1, 2, 70), prefix = "z", xin = c("x","y","mtx")))
TracedFunction(1)
.traceRmap
traceRcleanup()


## Ex4: Modify function specific options
traceRsetup(
  TF  = list(id = c(1, 70), xin = c("x","y"), xout ="zz1"),
  TF1 = list(id = c(1, 2), xin = "x"),
  # TF2 = list(),    # TF2 will not be traced
  defaultOptions = list(id = c(1, 2, 80), prefix = "z", xin = c("x","y","mtx"), asList = FALSE)
)
TracedFunction(1)
.traceRmap
traceRead()
traceRcleanup()

















