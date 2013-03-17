% nlmeU/man/prt.subjects.Rd
% Part of the nlmeU package for R 
% Distributed under GPL 2 or later: see nlmeU/LICENCE
\name{prt.subjects}
\alias{prt.subjects}
\docType{data}
\title{prt.subjects Data (63 x 5)}% Data Title
\description{Data prt.subjects
  ...
}
\usage{data(prt.subjects)}
\format{
The \code{prt.subjects} data frame has 63 rows and 5 columns
\describe{
   \item{id}{
     a  factor with 63 levels  \code{5}, \code{10}, \code{15}, \code{20}, \code{25}, ...
   }
   \item{prt.f}{
     a  factor with 2 levels  \code{High}, \code{Low}
   }
   \item{age.f}{
     a  factor with 2 levels  \code{Young}, \code{Old}
   }
   \item{sex.f}{
     a  factor with 2 levels  \code{Female}, \code{Male}
   }
   \item{bmi}{
     a numeric vector with values from 18.3661027282426 to 32.2863403944485
   }
}}
\details{
The working hypothesis was that a 12-week program of PRT would increase: (a) the power output
of the overall musculature associated with movements of the ankles, knees, and hips; (b) the cross-sectional area and the force and power of permeabilized single fibers obtained from the vastus lateralis muscle; and (c) the ability of young and elderly men and women to safely arrest standardized falls. The training consisted of repeated leg extensions by shortening contractions of the leg extensor muscles
against a resistance that was increased as the subject trained using a specially
designed apparatus}
\source{
  ???? Doe, A. B. and Doe, J. M. (xxxx), \emph{Title},
  Springer, New York.  (Appendix A.17)
}
\examples{
summary(prt.subjects)
}
\keyword{datasets}
