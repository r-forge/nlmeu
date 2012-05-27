### R code from vignette source 'D:/RbookX/RnwNlmeU/Ch2.Rnw'

###################################################
### code chunk number 1: Init
###################################################
options(width=65, digits=5)
sessionInfo()


###################################################
### code chunk number 2: R2.1
###################################################
## dataDir <- file.path("C:", "temp")                # Data directory
dataDir <- file.path(.Library, "nlmeU", "csvData")   # Directory in package
fp   <- file.path(dataDir, "armd240.data.csv")       # .csv file path
armd240.data <- read.csv(fp, header = TRUE)          # Loading data
dim(armd240.data)                       # No. of rows and cols
(nms <- names(armd240.data))            # Variables' names
unique(sapply(armd240.data, class))     # Variables' classes
str(armd240.data)                       # Data structure
names(armd240.data) <- abbreviate(nms)  # Variables' names shortened
head(armd240.data, 3)                   # First 3 records
names(armd240.data) <- nms              # Variables' names reinstated


###################################################
### code chunk number 3: R2.2
###################################################
data(armd240, package = "nlmeU")            # armd240 loaded
str(armd240)                                # Structure of data
(facs <- sapply(armd240, is.factor))        # Factors indicated 
names(facs[facs == TRUE])                   # Factor names displayed                  


###################################################
### code chunk number 4: R2.3a
###################################################
attach(armd240.data)                  # Attach armd240.data
treat.f <- factor(treat,              # Factor created 
  labels = c("Placebo", "Active")     # 1 -> Placebo, 2 -> Active
) 
levels(treat.f)
str(treat.f)


###################################################
### code chunk number 5: R2.3b
###################################################
library(nlmeU)                        # Package nlmeU loaded
miss.pat <-                           # missPat() function from nlmeU
    missPat(visual4, visual12, visual24, visual52) # missing patterms
detach(package:nlmeU)                 # Package nlmeU detached
length(miss.pat)                      # Vector length
mode(miss.pat)                        # Vector mode
miss.pat                              # Vector contents 
detach(armd240.data)                  # Detach armd240.data


###################################################
### code chunk number 6: R2.4
###################################################
data(armd240.v5, package = "nlmeU")        # From nlmeU package
head(armd240.v5)                           # First six records
names(armd240.v5)                          # Variables' names
dim(armd240.v5)                            # No. of rows and cols
str(armd240.v5)                            # Data structure    


###################################################
### code chunk number 7: R2.5
###################################################
auxDt <- subset(armd240.v5, time > 0)     # Post-baseline measures
dim(auxDt)                                # No. of rows & cols
levels(auxDt$time.f)                      # Levels of treat.f
armd240.v4 <- droplevels(auxDt)           # Drop unused levels   
levels(armd240.v4$time.f)                 # Baseline level dropped
armd240.v4 <- within(armd240.v4, {        # Contrasts assigned
   contrasts(time.f) <- contr.poly(4, scores = c(4, 12, 24, 52))
})
head(armd240.v4)                          # First six records


###################################################
### code chunk number 8: R2.6a
###################################################
fp <- file.path(dataDir, "prt.subjects.data.csv")
prt.subjects.data <- read.csv(fp, header = TRUE, as.is = TRUE) 
dim(prt.subjects.data)
names(prt.subjects.data)
str(prt.subjects.data)
head(prt.subjects.data, 4)


###################################################
### code chunk number 9: R2.6b
###################################################
fp <- file.path(dataDir, "prt.fiber.data.csv")
prt.fiber.data <- read.csv(fp, header = TRUE) 
str(prt.fiber.data)
head(prt.fiber.data, 4)


###################################################
### code chunk number 10: R2.7a
###################################################
prt.subjects <- within(prt.subjects.data,{
   id    <- factor(id)
   bmi   <- weight/(height^2)
   sex.f <- factor(gender, labels = c("Female", "Male"))
   age.f <- factor(ageGrp, labels = c("Young", "Old"))
   prt.f <- factor(trainGrp, levels = c("1", "0"), 
             labels = c("High", "Low"))
   gender<- ageGrp <- trainGrp <- height <- weight <- NULL
})
str(prt.subjects)


###################################################
### code chunk number 11: R2.7b
###################################################
prt.fiber  <- within(prt.fiber.data, {
   id      <- factor(id)
   fiber.f <- factor(fiber.type, 
               labels = c("Type 1", "Type 2"))
   occ.f   <- factor(train.pre.pos, 
               labels = c("Pre", "Pos"))     
   fiber.type <- train.pre.pos <- NULL
})
str(prt.fiber)


###################################################
### code chunk number 12: R2.7c
###################################################
prt <- merge(prt.subjects, prt.fiber, sort=FALSE)
dim(prt)
names(prt)
head(prt)


###################################################
### code chunk number 13: R2.8
###################################################
data(classroom, package = "WWGbook")
dim(classroom)                       # Number of rows & variables
names(classroom)                     # Variable names
# classroom                          # Raw data (long output)
str(classroom)


###################################################
### code chunk number 14: R2.9
###################################################
SIIdata <- within(classroom, {
   sex <- factor(sex, 
        levels = c(0, 1),
        labels = c("M", "F"))             # 0 -> 1(M),  1 -> 2(F) 
   minority <- factor(minority, 
        labels = c("Mnrt:No", "Mnrt:Yes"))# 0 -> 1(No), 1 -> 2(Yes)
   schoolid <- factor(schoolid)         
   classid  <- factor(classid)          
   childid  <- factor(childid)
})
str(SIIdata)


###################################################
### code chunk number 15: R2.10
###################################################
rdaDir <- file.path("C:", "temp")         # Dir path
fp <- file.path(rdaDir, "SIIdata.Rdata")  # External file path
save(SIIdata, file = fp)                  # Save data
file.exists(fp)                           
(load(fp))                                # Load data


###################################################
### code chunk number 16: R2.11
###################################################
data(SIIdata, package = "nlmeU")         # Load data
dtId <- subset(SIIdata, select = c(schoolid, classid, childid))
names(dtId)                              # id names
any(duplicated(dtId))                    # Any duplicate ids?
require(nlme)
names(gsummary(dtId, form = ~childid, inv = TRUE))
names(gsummary(dtId, form = ~classid, inv = TRUE))
names(gsummary(dtId, form = ~schoolid, inv = TRUE))


###################################################
### code chunk number 17: R2.12a
###################################################
(nms1 <- names(gsummary(SIIdata,
    form = ~schoolid,              # schoolid-specific
    inv = TRUE))) 


###################################################
### code chunk number 18: R2.12b
###################################################
nms2a <- names(gsummary(SIIdata, 
    form = ~classid,               # classid- and schoolid-specific 
    inv = TRUE)) 
idx1  <- match(nms1, nms2a)
(nms2 <- nms2a[-idx1])             # classid-specific


###################################################
### code chunk number 19: R2.12c
###################################################
nms3a <- names(gsummary(SIIdata, 
                 form = ~childid,  # All
                 inv = TRUE)
)
idx12 <- match(c(nms1, nms2), nms3a)
nms3a[-idx12]                    # childid-specific


###################################################
### code chunk number 20: R2.13
###################################################
fp <- file.path(dataDir, "crossreg.data.csv")
crossreg.data <- read.csv(fp, header = TRUE) 
dim(crossreg.data)                      # No. of rows and columns
names(crossreg.data)                    # Variable names
head(crossreg.data)                     # First six records
str(crossreg.data)                      # Data structure


###################################################
### code chunk number 21: R2.14
###################################################
unique(crossreg.data$target)          # Unique values for target
(unique(crossreg.data$id))            # Unique values for id 
unique(crossreg.data$scorec)          # Unique values for scorec
summary(crossreg.data$scorec)         # Summary statistics for scorec


###################################################
### code chunk number 22: R2.15a
###################################################
nItms  <- c(4, 6, 8, 5, 9, 6, 8, 6, 5)        # See Table \ref!tab:atot:targets?
(lbls <- paste("T", 1:9, "(", nItms, ")", sep = ""))
fcat <- within(crossreg.data, {
        id <- factor(id)
        target <- factor(target, labels = lbls)
})
str(fcat)


###################################################
### code chunk number 23: R2.15b
###################################################
(tab1 <- xtabs(~ id + target, data = fcat))   # id by target table
all(tab1 > 0)                                 # All counts > 0? 
range(tab1)                                   # Range of counts


###################################################
### code chunk number 24: Ch2Cleanup
###################################################
rm(armd240.data, armd240, armd240.v5, armd240.v4)      # Data not needed
rm(prt.fiber.data, prt.subjects.data, prt.fiber, prt.subjects, prt)
rm(classroom, SIIdata) 
rm(crossreg.data, fcat) 
rm(dataDir, rdaDir, auxDt)
rm(treat.f, miss.pat, facs, lbls)
rm(nms,nms1, nms2, nms2a, nms3a)
rm(idx1, idx12, nItms, tab1, dtId)


