% File nlmeU/man/logLik1.Rd
% Part of the nlmeU package for R

\name{logLik1}
\title{Calculates contribution of one subject to the log-likelihood}
\usage{
logLik1(modfit, dt1, dtInit)
}
\alias{logLik1}
\arguments{
 \item{modfit}{an object representing model fitted to data using ML estimation.} 
 \item{dt1}{a data frame with data for one subject, for whom the log-likelihood function is to be evaluated}
 \item{dtInit}{an optional auxiliary data frame.}
}
\description{
  This function is generic; method functions can be written to handle specific classes of objects. 
}
\value{
  numeric scalar value representing contribution of a given subject to the overall log-likelihood returned by \code{logLik()} function.
}
\author{Andrzej Galecki and Tomasz Burzykowski}
%\seealso{\code{\link{sigma.default}},
%  \code{\link{sigma.pdMat}}}
%\keyword{models}
