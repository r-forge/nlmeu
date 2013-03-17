% File nlmeU/man/Pwr.lme.Rd
% Part of the nlmeU package for R
\name{Pwr.lme}
\alias{Pwr.lme}
\title{Performs power calculations}
\usage{
\method{Pwr}{lme}(object, ..., type, Terms, L, verbose, sigma, ddf, alpha, altB, tol)
}
\arguments{
 \item{object}{an object containing \code{lme} object, which provides information needed for power calculations} 
 \item{\dots}{some additional arguments may be required.}
 \item{type}{an optional character string specifying the type of sum of squares to be used in F-tests
  needed for power calculations. Syntax is the same as for \code{anova.lme()} in \code{nlme} package.}
 \item{Terms}{an optional integer or character vector specifying which terms in the model should be jointly tested to be zero using a Wald F-test. See \code{anova.lme} in \code{nlme} package for details.}
 \item{L}{an optional numeric vector or array specifying linear combinations of the coefficients in the model that should be tested to be zero. See \code{anova.lme} in \code{nlme} package for details.}
 \item{verbose}{an optional logical value. See anova.lme in nlme package for details.}
 \item{sigma}{numeric scalar value.}
 \item{ddf}{numeric scalar value. Argument can be used to redefine default number of denominator degrees of freedom}
 \item{alpha}{numeric scalar value.}
 \item{altB}{matrix/vector containing alternative values for beta parameters}
 \item{tol}{numeric scalar value.}
}
\description{
  This is method  for \code{Pwr()} generic function. It works fine for an example given in the book. It may require additional testing, especially for post-hoc power analysis 
}
\value{
  a data frame inheriting from class Pwr.lme
}
\author{Andrzej Galecki and Tomasz Burzykowski}
\seealso{\code{\link{anova.lme}}}
%  \code{\link{sigma.pdMat}}}
\examples{
### Panel: R20.15a
npg <- 20                        # No of subjects per group
subject <- 1:(2*npg)                       # Subjects' ids
treat.f <- gl(2, npg, labels = c("Placebo", "Active"))
dts <- data.frame(subject, treat.f)        # Subject-level data

dtL <- 
   list(time = c(4, 12, 24, 52),
        subject = subject)
dtLong <- expand.grid(dtL)                 # "Long" format
mrgDt  <- merge(dtLong, dts, sort = FALSE) # Merged
exmpDt <- 
   within(mrgDt, 
          {    
           m0 <- 65 - 0.1 * time    # Under H0  
           mA <- 75 - 0.1 * time    # Under Ha. 85 changed to 75
           mA <- ifelse(treat.f == "Active", mA, m0) 
          })

###################################################
### Panel: R20.16a
###################################################
library(nlme)
D0 <- diag(c(100, 0.09))                 # calligraphic D     
sgma  <- 5                               # sigma
(D  <- D0/(sgma*sgma))                   # D
(pd1 <- pdDiag(D, form = ~time, data = armd))
(vF <- varPower(form = ~time, fixed = 0.15))


###################################################
### Panel: R20.16b
###################################################
cntrl <- 
   lmeControl(maxIter = 0, msMaxIter = 0, niterEM = 0, 
              returnObject = TRUE, opt = "optim")

fmA <- 
   lme(mA ~ time + treat.f,
       random = list(subject = pd1),
       weights = vF,
       data = exmpDt,
       control = cntrl)
fixef(fmA)                               # beta verified
sigma(fmA)                               # sigma approx. 0

###################################################
### Panel: R20.17a
###################################################
Pwr(fmA, sigma = sgma, L = c("treat.fActive" = 1))
}
%\keyword{models}
