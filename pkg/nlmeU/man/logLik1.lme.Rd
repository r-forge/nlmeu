
% File nlmeU/man/logLik1.lme.Rd
% Part of the nlmeU package for R

\name{logLik1.lme}
\title{Calculates contribution of one subject to the log-likelihood of \code{lme} object}
\usage{
\method{logLik1}{lme}(modfit, dt1, dtInit)
}
\alias{logLik1.lme}
\arguments{
 \item{modfit}{an \code{lme} object representing model fitted using maximum likelihood.} 
 \item{dt1}{a data frame with data for one subject, for whom the log-likelihood function is to be evaluated}
 \item{dtInit}{an optional auxiliary data frame.}
}
\description{
  This is method  for \code{logLik1()} generic function. 
}
\value{
  numeric scalar value representing contribution of a given subject to the overall log-likelihood returned by \code{logLik()} function applied to \code{lme} object defined by \code{modfit} argument.
}
\author{Andrzej Galecki and Tomasz Burzykowski}
%\seealso{\code{\link{sigma.default}},
%  \code{\link{sigma.pdMat}}}
\examples{ 
require(nlme)
lm3.form <- visual ~ visual0 + time + treat.f 
(fm16.5ml <-                                # M16.5
    lme(lm3.form,                         
    random = list(subject = pdDiag(~time)), 
    weights = varPower(form = ~time),
    data = armd, method = "ML"))
df1 <- subset(armd, subject == "1")       # Panel R20.7
logLik1(fm16.5ml, df1)
}
%\keyword{models}




