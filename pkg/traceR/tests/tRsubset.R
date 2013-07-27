
subset.data.frameU <-
function (x, subset, select, drop = FALSE, ...)
 {


   .functionLabel <- "sdf"                                # Function label (recommended)
   .traceR <- attr(options()$traceR, "fun")
   .traceR <-  if (is.null(.traceR)) function(...){} else .traceR      
   .traceR(1, lbl = "->START")
 
    if (missing(subset)) {
        .traceR(11, store= FALSE)
        r <- TRUE
    } else {
        e <- substitute(subset)
        r <- eval(e, x, parent.frame())
        
        if (!is.logical(r)) 
            stop("'subset' must be logical")
        r <- r & !is.na(r)
        .traceR(12, store= FALSE)
    }
    .traceR(2, lbl = "middle")    
    if (missing(select)){ 
        .traceR(21, store = FALSE)  
        vars <- TRUE
    } else {
        nl <- as.list(seq_along(x))
        names(nl) <- names(x)
        vars <- eval(substitute(select), nl, parent.frame())
        .traceR(22, store = FALSE)  
    }
    .traceR(3, lbl = "END<-") 
    x[r, vars, drop = drop]
}

library(traceR)
library(testthat)
traceR.on()
res <- subset.data.frameU(airquality, Temp > 80, select = c(Ozone, Temp))
.traceRmap
ls(e_1)
dim(e_1$x)
err1 <- "object 'Ozone' not found"
expect_that(e_1$select, throws_error(err1))
delayedAssign("val", substitute(select), eval.env = e_1)
val 

ls(e_5)
summary(e_5$r)
e_5$vars

q()

traceR.optim()
.traceRmap
names(l_1)
l_1$select
names(d_5)
str(d_5)

traceR.off()
Remove(list = Objects())
detach(package:traceR)
detach(package:SOAR)

