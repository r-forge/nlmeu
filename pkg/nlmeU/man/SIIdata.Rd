% nlmeU/man/SIIdata.Rd
% Part of the nlmeU package for R 
% Distributed under GPL 2 or later: see nlmeU/LICENCE
\name{SIIdata}
\alias{SIIdata}
\docType{data}
\title{SIIdata Data (1190 x 12)}% Data Title
\description{Data SIIdata
  ...
}
\usage{data(SIIdata)}
\format{
The \code{SIIdata} data frame has 1190 rows and 12 columns
\describe{
   \item{sex}{
     a  factor with 2 levels  \code{M}, \code{F}
   }
   \item{minority}{
     a  factor with 2 levels  \code{Mnrt=No}, \code{Mnrt=Yes}
   }
   \item{mathkind}{
     an integer vector with values from 290 to 629
   }
   \item{mathgain}{
     an integer vector with values from -110 to 253
   }
   \item{ses}{
     a numeric vector with values from -1.61 to 3.21
   }
   \item{yearstea}{
     a numeric vector with values from 0 to 40
   }
   \item{mathknow}{
     a numeric vector with values from -2.5 to 2.61
   }
   \item{housepov}{
     a numeric vector with values from 0.012 to 0.564
   }
   \item{mathprep}{
     a numeric vector with values from 1 to 6
   }
   \item{classid}{
     a  factor with 312 levels  \code{1}, \code{2}, \code{3}, \code{4}, \code{5}, ...
   }
   \item{schoolid}{
     a  factor with 107 levels  \code{1}, \code{2}, \code{3}, \code{4}, \code{5}, ...
   }
   \item{childid}{
     a  factor with 1190 levels  \code{1}, \code{2}, \code{3}, \code{4}, \code{5}, ...
   }
}}
\details{
 ...
}
\source{
  ???? Doe, A. B. and Doe, J. M. (xxxx), \emph{Title},
  Springer, New York.  (Appendix A.17)
}
\examples{
summary(SIIdata)
}
\keyword{datasets}
