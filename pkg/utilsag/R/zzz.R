
.utilsagEnv <- new.env()
assign("XverboseControl", list(), envir = .utilsagEnv)

.onLoad <- function(libname, pkgname) 
{
    XverboseControl(.XverboseControl())
}

