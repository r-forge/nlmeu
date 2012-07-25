TextSectionsProcess <- function(TSobject){
## Process TSobject representing text file 
srch <- "stringr" %in% search()
if (!srch)  require(stringr)
Dt <- attr(TSobject,"LogData")  
funs <- attr(TSobject,"FUN")
fnmL <- as.list(names(funs))
assignFun <- sapply(fnmL, 
     FUN = function(el)  assign(x= el, value =funs[[el]], envir = .GlobalEnv))
## print(str(Dt))
tt <- sapply(as.list(rownames(Dt)), 
             FUN = function(el){
             #print("here")
             #print(el)
             Dtx <- Dt[el,]
             txt <- TSobject[[el]]
             fn <- Dt[el,"FUN"]
             #print(fn)
             aL <- list(txt = txt, dt = Dtx)
             #print(aL)
             if (fn == "") txt else do.call(fn, args = aL)}) 
names(tt) <- rownames(Dt)
tt
} # TextSectionsProcess ENDS

