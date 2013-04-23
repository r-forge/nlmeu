###--------------------
D0 <- as.matrix(1)

D1 <- c(3, 9,
        9,30)
dim(D1) <- c(2,2)

D2 <- c(2, 4,  0,
        4, 10, 0,
        0, 0 , 1)

dim(D2) <- c(3,3)

D3 <- diag(2)


library(nlmeForTesting)
pd0f <- pdIdent(~1)
pd1f <- pdLogChol(~f1 -1)
pd2f <- pdLogChol(~f2 -1)
pd3f <- pdIdent(~ f3 -1)

pdBf <- pdBlocked(list(pd0f, pd1f, pd2f, pd3f))


pd0ni <- pdIdent(2*D0, nam="x")
pd1ni <- pdLogChol(D1, nam= c("A","B"))
pd2ni <- pdLogChol(D2, nam= c("a1", "a2", "a3"))
pd3ni <- pdIdent(D3,   nam= c("X1", "X2"))

f1 <- factor(c("A","B","A"))
f2 <- factor(c("a1","a2","a3"))
f3 <- factor(c("X1","X2","X1"))

dt <- data.frame(f1, f2, f3)
dt


pdBni <- pdBlocked(list(pd0ni, pd1ni, pd2ni, pd3ni))

frmL  <- list(~1, ~f1-1, ~f2-1)
namL  <- list(c("X"), c("f1A","f1B"), c("f2a", "f2b","f3b"), c("X1","X2"))
mtxL  <- list(D0, D1,D2,D3)



pdL1f <- list(pd0f = pd0f, pd1f = pd1f, pd2f= pd2f, pd3f= pd3f)      # Uninitialized pdMat list with formulas 
pdL1ni <- list(pd0ni, pd1ni, pd2ni, pd3ni)  # Initialized pdMat list with names 

### pdKronecker calls ###

# pf0 <- pdKronecker(frmL)  # List of formulas. DO NOT USE IT as argument

print("===========>>>>>>> pL1f")
str(pdL1f)
pL1f <- pdKroneckeR(pdL1f)          # value = list of pdMat

pL1fdt <- pdKroneckeR(pL1f, data=dt)

print("===========>>>>>> pL2f")
pL2f <- pdKroneckeR(pL1f, form = ~newform)

pL2fdt <- pdConstruct(pL2f, data=dt)

reStruct(pL2fdt)

print("pdL1ni")
pL1ni<- pdKroneckeR(pdL1ni)        # value = named list of initialized pdMat

print("pdBf")
pBf <-  pdKroneckeR(pdBf)          # value = pdBlocked uninitialized (OK)

print("pdBni")
pBni <- pdKroneckeR(pdBni)         # pdKenv has names, but it is not initialized

print("pdBni, pBf")
pBnif <- pdConstruct(pBf, pBni)

cfx <- coef(pBnif)
cfx[1] <- 0.1
pBnif2 <- pdConstruct(pBnif, cfx)
str(pBnif2)
