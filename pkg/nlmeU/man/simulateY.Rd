\name{simulateY}
\alias{simulateY}
\title{Simulates values of the dependent variable based on a model fit}
\usage{
  simulateY(object, nsim = 1, seed = NULL, ...,
    verbose = FALSE, sigma)
}
\arguments{
  \item{object}{an object with a model fit for which
  dependent variable is to be simulated.}

  \item{nsim}{number of simulations. nsim = 1 by default.}

  \item{seed}{integer scalar used to initiate random
  numbers generator.}

  \item{\dots}{some methods for this generic function may
  require additional arguments.}

  \item{verbose}{logical. If TRUE basic information about
  arguments is provided. By default set to FALSE.}

  \item{sigma}{numeric scalar. Allows to perform
  simulations employing alternative value of the scale
  parameter.}
}
\value{
  numeric matrix. Number of columns determined by nsim
  argument.
}
\description{
  This function is generic; method functions can be written
  to handle specific classes of objects.
}
\examples{
## simulateY (fm1)
}
\author{
  Andrzej Galecki and Tomasz Burzykowski
}

