 .traceRenv <- new.env()
 
 
.onLoad <- function(libname, pkgname){
assign(".traceR", .traceRdump, envir =  .traceRenv)
}
