\name{logLik1}
\alias{logLik1}
\title{Calculates contribution of one subject to the log-likelihood}
\usage{
  logLik1(modfit, dt1, dtInit)
}
\arguments{
  \item{modfit}{an object representing model fitted to data
  using ML estimation.}

  \item{dt1}{a data frame with data for one subject, for
  whom the log-likelihood function is to be evaluated}

  \item{dtInit}{an optional auxiliary data frame.}
}
\value{
  numeric scalar value representing contribution of a given
  subject to the overall log-likelihood returned by
  \code{logLik()} function.
}
\description{
  This function is generic; method functions can be written
  to handle specific classes of objects.
}
\examples{
require(nlme)
  logLik(fm1 <- lme(distance ~ age, data = Orthodont)) # random is ~ age
  dt1 <- subset(Orthodont, Subject == "M01")
  logLik1(fm1, dt1)
}
\author{
  Andrzej Galecki and Tomasz Burzykowski
}
\references{
  ???
}

