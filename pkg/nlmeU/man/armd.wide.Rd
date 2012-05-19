% nlmeU/man/armd.wide.Rd
% Part of the nlmeU package for R 
% Distributed under GPL 2 or later: see nlmeU/LICENCE
\name{armd.wide}
\alias{armd.wide}
\docType{data}
\title{armd.wide Data (240 x 10)}% Data Title
\description{Data armd.wide
  ...
}
\usage{data(armd.wide)}
\format{
The \code{armd.wide} data frame has 240 rows and 10 columns
\describe{
   \item{subject}{
     a  factor with 240 levels  \code{1}, \code{2}, \code{3}, \code{4}, \code{5}, ...
   }
   \item{lesion}{
     an integer vector with values from 1 to 4
   }
   \item{line0}{
     an integer vector with values from 5 to 17
   }
   \item{visual0}{
     an integer vector with values from 20 to 85
   }
   \item{visual4}{
     an integer vector with values from 12 to 84
   }
   \item{visual12}{
     an integer vector with values from 3 to 85
   }
   \item{visual24}{
     an integer vector with values from 5 to 85
   }
   \item{visual52}{
     an integer vector with values from 4 to 85
   }
   \item{treat.f}{
     a  factor with 2 levels  \code{Placebo}, \code{Active}
   }
   \item{miss.pat}{
     a  factor with 9 levels  \code{----}, \code{---X}, \code{--X-}, \code{--XX}, \code{-XX-}, ...
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
summary(armd.wide)
}
\keyword{datasets}
