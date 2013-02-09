
univariateStats <- function(data, fun){
dscLs <- lapply(data, FUN = fun)
nms <- names(dscLs)        # Variable names
rsDt <- NULL 
for (i in seq_along(dscLs)){
 nmsi <- nms[i]
 dsci <- dscLs[[i]]
 tmp <- if(is.null(dsci)) NULL else data.frame(dsci)
 if (!is.null(tmp)) rownames(tmp) <- nmsi
 rsDt <- rbind(rsDt, tmp)
}
rsDt
}
