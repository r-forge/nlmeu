\name{logLik1.lme}
\alias{logLik1.lme}
\title{Calculates contribution of one subject to the log-likelihood for \code{lme} object}
\usage{
  \method{logLik1}{lme} (modfit, dt1, dtInit)
}
\arguments{
  \item{modfit}{an \code{lme} object representing model
  fitted using maximum likelihood.}

  \item{dt1}{a data frame with data for one subject, for
  whom the log-likelihood function is to be evaluated}

  \item{dtInit}{an optional auxiliary data frame.}
}
\value{
  numeric scalar value representing contribution of a given
  subject to the overall log-likelihood returned by
  \code{logLik()} function applied to \code{lme} object
  defined by \code{modfit} argument.
}
\description{
  This is method for \code{logLik1()} generic function.
}
\details{
  Calculates profile likelihood (with beta profiled out)
  for *one* subject. Data with *one* level of grouping
  only. correlation component in modelStruct not
  implemented.
}
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
\author{
  Andrzej Galecki and Tomasz Burzykowski
}

