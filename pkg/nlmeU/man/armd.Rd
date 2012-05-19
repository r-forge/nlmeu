% nlmeU/man/armd.Rd
% Part of the nlmeU package for R 
% Distributed under GPL 2 or later: see nlmeU/LICENCE
\name{armd}
\alias{armd}
\docType{data}
\title{armd Data (867 x 8)}% Data Title
\description{Data armd
  ...
}
\usage{data(armd)}
\format{
The \code{armd} data frame has 867 rows and 8 columns
\describe{
   \item{subject}{
     a  factor with 234 levels  \code{1}, \code{2}, \code{3}, \code{4}, \code{6}, ...
   }
   \item{treat.f}{
     a  factor with 2 levels  \code{Placebo}, \code{Active}
   }
   \item{visual0}{
     an integer vector with values from 20 to 85
   }
   \item{miss.pat}{
     a  factor with 8 levels  \code{----}, \code{---X}, \code{--X-}, \code{--XX}, \code{-XX-}, ...
   }
   \item{time.f}{
     a  factor with 4 levels  \code{4wks}, \code{12wks}, \code{24wks}, \code{52wks}
   }
   \item{time}{
     a numeric vector with values from 4 to 52
   }
   \item{visual}{
     an integer vector with values from 3 to 85
   }
   \item{tp}{
     a numeric vector with values from 1 to 4
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
summary(armd)
}
\keyword{datasets}
