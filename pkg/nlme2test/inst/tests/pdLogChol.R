print("-> Testing pdLogChol.R")

library(testthat)
library(nlme)
Dx <- c(1, 1,  1,
        1, 5 , 5,
        1, 5 , 14)
dim(Dx) <- c(3,3)
chol(Dx)

nms <- c("a1","a2","a3")
fnms <- c("fxa1","fxa2", "fxa3")
frmx <- ~fx -1


dt <- data.frame(fx = factor(nms))
mtx <- 1:3
mtx <- rep(mtx,3)
dim(mtx) <- c(3,3)

Z <- mtx
colnames(Z) <- fnms
dt <- cbind(dt, Z)
dt


p1f <- pdLogChol(frmx)
expect_that(formula(p1f), equals(frmx))    #  ~ fx -1

p1fdt <- pdLogChol(p1f, data=dt)
expect_that(Names(p1fdt), equals(c("fxa1", "fxa2", "fxa3")))


p1n <- pdLogChol(nms) 
expect_that(Names(p1n), equals(nms))        # see nms
p1ndt <- pdLogChol(p1n, data=dt)
expect_that(formula(p1ndt), equals(NULL))
expect_that(Names(p1ndt), equals(nms))        # see nms


p1nf <- pdConstruct(p1n, frmx)
expect_that(formula(p1nf), equals(frmx))    #  ~ fx -1
expect_that(Names(p1nf), equals(nms))        # see nms
expect_that(pdLogChol(p1nf, data=dt), throws_error())  # Intentional error: Formula does not match nms


### p1fdt -> p1fdtx

p1fdtx <- pdConstruct(p1fdt, Dx)
expect_that(formula(p1fdtx), equals(frmx))
expect_that(Names(p1fdtx), equals(fnms))

pdFac1 <- pdFactor(p1fdtx)
expect_that(pdFac1, equals(as.vector(chol(Dx))))

pdM1 <- pdMatrix(p1fdtx)    
expect_that(rownames(pdM1), equals(fnms))
expect_that(as.vector(pdM1), equals(as.vector(Dx)))

### p1fdtx -> p1fdtxr
p1fdtxr <- p1fdtx
Names(p1fdtxr) <- rev(fnms)                                 # Order changed
pdM1a <- pdMatrix(p1fdtxr)                                
expect_that(rownames(pdM1a), equals(rev(fnms)))

pdFac1a <- pdFactor(p1fdtxr)
expect_that(pdFac1a, equals(as.vector(chol(pdM1a))))
expect_that(formula(pdLogChol(p1fdtxr)), equals(frmx))
expect_that(formula(pdLogChol(p1fdtxr, ~fx)), equals(~fx))



### p1ndt -> p1ndtx   
p1ndtx <- pdConstruct(p1ndt, Dx)
expect_that(formula(p1ndtx), equals(NULL))
expect_that(Names(p1ndtx), equals(nms))

pdFac1 <- pdFactor(p1ndtx)
expect_that(pdFac1, equals(as.vector(chol(Dx))))

pdM1 <- pdMatrix(p1ndtx)    
expect_that(rownames(pdM1), equals(nms))
expect_that(as.vector(pdM1), equals(as.vector(Dx)))


### p1ndtx -> p1ndtxr
p1ndtxr <- p1ndtx
Names(p1ndtxr) <- rev(nms)                                 # Order changed
pdM1a <- pdMatrix(p1ndtxr)                                
expect_that(rownames(pdM1a), equals(rev(nms)))
pdFac1a <- pdFactor(p1ndtxr)
expect_that(pdFac1a, equals(as.vector(chol(pdM1a))))

### p1ndtx -> p1ndts
p1ndts <- solve(p1ndtx)
pdM1 <- pdMatrix(p1ndtx)  
pdM1s <- pdMatrix(p1ndts)
expect_that(as.vector(pdM1 %*% pdM1s), equals(as.vector(diag(length(rownames(pdM1)))))) 
  
## reStruct1
reSt <- reStruct(p1f, REML=TRUE, data=NULL)
groups <- getGroupsFormula(reSt)
groups <- ~1
names(reSt) <- "1"
str(reSt)
lmeSt <- lmeStruct(reStruct = reSt)
Names(lmeSt$reStruct)
mfArgs <- list(asOneFormula(formula(lmeSt)), data=dt)      # ~fx
mfArgs$drop.unused.levels <- TRUE

dataMix <- do.call("model.frame", mfArgs)
Z <- model.matrix(reSt, dataMix)
ncols <- attr(Z, "ncols")
Names(lmeSt$reStruct) <- attr(Z, "nams")
Names(lmeSt$reStruct)                         #  "fxa1" "fxa2" "fxa3"

### reStruct2
p1fx <- pdLogChol(p1f, form = ~fxa1 + fxa2 +fxa3 -1)

reSt <- reStruct(p1fx, REML=TRUE, data=NULL)
groups <- getGroupsFormula(reSt)
groups <- ~1
names(reSt) <- "1"
str(reSt)
lmeSt <- lmeStruct(reStruct = reSt)
Names(lmeSt$reStruct)
mfArgs <- list(asOneFormula(formula(lmeSt)), data=dt)      # ~fx
mfArgs$drop.unused.levels <- TRUE

dataMix <- do.call("model.frame", mfArgs)
Z <- model.matrix(reSt, dataMix)
ncols <- attr(Z, "ncols")
Names(lmeSt$reStruct) <- attr(Z, "nams")
Names(lmeSt$reStruct)                        # "Zfxa1" "Zfxa2" "Zfxa3"










