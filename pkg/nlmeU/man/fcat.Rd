\docType{data}
\name{fcat}
\alias{fcat}
\title{fcat Data (4851 x 3)}
\format{The \code{fcat} data frame has 4851 rows and 3 columns
\describe{
  \item{target}{ a factor with 9 levels \code{T1(4)}, \code{T2(6)}, \code{T3(8)}, \code{T4(5)}, \code{T5(9)}, ..., \code{T9(5)}}
  \item{id}{ a factor with 539 levels \code{1}, \code{2}, \code{3}, \code{4}, \code{5}, ..., \code{539}}
  \item{scorec}{ an integer vector with values from 0 to 9 }}}
\source{
  Janssen, R., Tuerlinckx, F., Meulders, M., & De Boeck, P.
  (2000). A hierarchical IRT model for criterion-referenced
  measurement. Journal of Educational and Behavioral
  Statistics. 25(3), 285.
}
\description{
  Data from Flemish Community Attainment-Targets (FCAT)
  Study
}
\details{
  An educational study, in which elementary school
  graduates were evaluated with respect to reading
  comprehension in Dutch. Pupils from randomly selected
  schools were assessed for a set of nine attainment
  targets. The dataset is an example of grouped data, for
  which the grouping factors are crossed.
}
\examples{
summary(fcat)
}
\keyword{datasets}

