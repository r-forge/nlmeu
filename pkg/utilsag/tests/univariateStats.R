library(utilsag)

## 1. User defined function processing numeric variables
univNum <- function(x){
# x is a vector
if (!(class(x)[1] %in% c("integer", "numeric"))) return()
n <- length(x)
nmiss <- sum(is.na(x))
mean <- mean(x, na.rm=TRUE)
med <- median(x, na.rm=TRUE)
varx <- var(x, na.rm=TRUE)
std  <- sqrt(varx)
sem <- if (n-nmiss-1) sqrt(varx/(n- nmiss- 1)) else NA
min. <- if (n-nmiss)  min(x, na.rm = TRUE) else NA 
max. <- if (n-nmiss) max(x, na.rm = TRUE) else NA
data.frame(n, nmiss, mean, median = med, std, sem, min = min., max = max.)
}

univariateStats(BOD, univNum)



## 2. User defined function processing character variables

univChar <- function(x){  # For character factor vars
# x is  a vector
class1 <- class(x)[1]
if (class1 %in% c("character", "factor","ordered")) 
  summary(x) else NULL
}

res <- lapply(Orange, univChar)  
res[!sapply(res,is.null)]   # Remove null
 
