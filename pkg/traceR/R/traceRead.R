traceRead.default <- function(el) names(el)

traceRead <- function(traceRmap = .traceRmap, hook = traceRead.default){
res  <- .traceRmap$res
res <- res[res != ""]
# print(res)

resx <- lapply(as.list(res), function(el) names(eval(parse(text=el))))
names(resx) <- res
resx
}

