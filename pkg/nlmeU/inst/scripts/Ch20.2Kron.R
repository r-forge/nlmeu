
###################################################
### code chunk: Ch20.2Kron  Init
###################################################

options(width = 65, digits = 5, show.signif.stars = FALSE)
date()
sessionInfo()



###################################################
### code chunk: R20.1
###################################################
D1 <- c(3, 9, 
        9, 30)
dim(D1) <- c(2, 2)       # D1 for factor  f1
D2 <- c(2, 4, 
        4, 10)     
dim(D2) <- c(2, 2)       # D2 for factor f2
D1 %x% D2                # D1 @ D2


###################################################
### code chunk number 4: R20.2
###################################################
library(nlme)
(pdId <- pdIdent(as.matrix(1), form = ~1)) # Mandatory
(pd1 <- pdLogChol(D1, form = ~f1-1))       # \zi{\bm{D}}{1}
(pd2 <- pdLogChol(D2, form = ~f2-1))       # \zi{\bm{D}}{2}
pdL1 <-                                    # The main argument
   list(X = pdId,
        pD1 = pd1,
        pD2 = pd2)


###################################################
### code chunk number 7: R20.3a
###################################################
f1 <- gl(2, 1, labels = c("A","B")) 
f2 <- gl(2, 1, labels = c("a","b"))
(dt <- data.frame(f1, f2))


###################################################
### code chunk number 8: R20.3b
###################################################
library(nlmeU)
(pdK <- pdKronecker(pdL1, data = dt))      # \zi{\bm{D}}{1} \zx{\otimes} \zi{\bm{D}}{2}
(nms <- Names(pdK))


###################################################
### code chunk number 9: R20.4
###################################################
(c0x <- as.matrix(pdK[[1]]))
(D1x <- as.matrix(pdK[[2]]))               #  Proportional to \zi{\bm{D}}{1}
(D2x <- as.matrix(pdK[[3]]))               #  Proportional to \zi{\bm{D}}{2}
Dx <-c0x %x% D1x %x% D2x                   #  \zi{\bm{D}=\bm{D}}{1} \zi{\otimes \bm{D}}{2}
dimnames(Dx) <- list(nms, nms)
Dx
                                     


###################################################
### code chunk number 13: R20.5a
###################################################
formula(pdK, asList = TRUE)               # List of component formulae
formula(pdK)                              # One-sided formula (default)


###################################################
### code chunk number 14: R20.5b
###################################################
(pdKform <- formula(~(f2-1):(f1-1)-1))
pdKterms <- terms(pdKform)                # Terms object  
labels(pdKterms)                          # Expanded formula
attr(pdKterms, "intercept")               # Intercept omitted

sessionInfo()

