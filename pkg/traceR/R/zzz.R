.onAttach <- function(libname ="traceR", pkgname =  "traceR"){
   .traceRmap <- data.frame(character(0))
   assign(".traceRmap", .traceRmap, envir = .GlobalEnv)
}
