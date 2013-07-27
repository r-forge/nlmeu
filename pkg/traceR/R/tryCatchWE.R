#  Copyright (C) 2010 The R Core Team
# demo(error.catching)
tryCatch.W.E <- function(expr)
{
     W <- NULL
     w.handler <- function(w){ # warning handler
       W <<- w
       invokeRestart("muffleWarning")
     }
     list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                    warning = w.handler),
        warning = W)
}

