options(width = 120)
library(traceR)   ## Depends on SOAR and reshape2

library(testthat)

library(reshape)
## Modify functions in namespace. Implicitly fixInNamespace function is used 
oNms <- ls(getNamespace("reshape"))       # 
traceRedit(oNms, "reshape")
detach(package:reshape)

library(reshape)
str(melt.data.frame)
head(melt.data.frame, n = 36)   # Shows that changes were implemented

traceR.on()
head(melt(tips))
names(airquality) <- tolower(names(airquality))
head(melt(airquality, id = c("month", "day")))
dim(.traceRmap)
traceR.report("traceR_reshape.html")

# Environments
ls(e_1)
ls(e_4)
ls(e_18)
identical(e_18$df, e_19$df)
(val <- e_1$id.vars)

q()

err0 <- 'argument "val" is missing, with no default'
expect_that(val, throws_error(err0))

missing(val)

identical(e_1$id.vars, e_2$id.vars)   # TRUE

err <- 'argument "id.vars" is missing, with no default'
expect_that(val <- e_3$id.vars, throws_error(err))

delayedAssign("val", id.vars, eval.env = e_3)  # Work around 
missing(val)                                   # Good news. Same as above 

expect_that(identical(e_3$id.vars, e_4$id.vars), throws_error(err))

## From env to list using as.list().

x_1 <- as.list(e_1) 
x_2 <- as.list(e_2) 
x_3 <- as.list(e_3)
x_18 <- as.list(e_18)
dim(x_18$data)

##--- optimize
# traceR.optim()
q()

.traceRmap$idLbl <- .traceRmap$removed <- NULL
.traceRmap
names(l_1)
identical(l_1, l_2)

## Function melt_check is called => Next stack begins: l_3
names(l_3)
l_3$id.vars  # This is incorrect :-(. See e_3$id.vars above

## Difference btwn l_4 and l_3: varnames added
identical(l_3$id.vars, l_4$id.vars)
setdiff(names(l_4), names(l_3))
tmp <- l_4
tmp$varnames <- NULL
identical(tmp, l_3)

## Lists l_4 to l_8 are identical
identical(l_4, l_5)
identical(l_5, l_6)
identical(l_6, l_7)
identical(l_7, l_8)

## d_9 represents (cumulative) difference btwn l_9 and l_3
## varnames added in l_4  
## categorical added in l_9
## measure.vars and id.vars modified at l_9
 
nms <- c("measure.vars", "id.vars")
Map(identical, l_3[nms], l_9[nms])

nms8 <- names(l_8)
nms9 <- names(l_9)
setdiff(nms9, nms8)
names(d_9)
## l_9 - l_11 are identical
identical(l_9, l_10)
identical(l_10, l_11)


## New stack begins: l_12 

names(l_12)
identical(l_13, l_12)

traceR.off()
Remove(list = Objects())
detach(package:traceR)
detach(package:SOAR)

unloadNamespace("reshape")
sessionInfo()

