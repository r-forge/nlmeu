% File nlmeU/man/simulateY.Rd
% Part of the nlmeU package for R

\name{simulateY}
\title{Simulates values of the dependent variable based on a model fit}
\usage{
simulateY(object, nsim, seed, ..., verbose, sigma)
}
\alias{simulateY}
\arguments{
 \item{object}{an object with a model fit for which dependent variable is to be simulated.}
 \item{nsim}{number of simulations. nsim = 1 by default.} 
 \item{seed}{integer scalar used to initiate random numbers generator.}
 \item{\dots}{some methods for this generic function may require additional arguments.}
 \item{verbose}{logical. If TRUE basic information about arguments is provided. By default set to FALSE.}
 \item{sigma}{numeric scalar. Allows to perform simulations based on alternative value of the scale parameter.}
}
\description{
  This function is generic; method functions can be written to handle
  specific classes of objects. 
}
\value{
  numeric matrix. Number of columns determined by nsim argument. 
}
\author{Andrzej Galecki and Tomasz Burzykowski}
%\seealso{\code{\link{sigma.default}},
%  \code{\link{sigma.pdMat}}}
\examples{
## simulateY (fm1)
}
%\keyword{models}
