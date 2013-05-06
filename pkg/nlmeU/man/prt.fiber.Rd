\docType{data}
\name{prt.fiber}
\alias{prt.fiber}
\title{prt.fiber Data (2471 x 5)}
\format{The \code{prt.fiber} data frame has 2471 rows and 5 columns. Each row
 in the data corresponds to one muscle fiber collected during muscle biopsy.
 See \code{prt} data frame for the description of the study design.
 \describe{
  \item{id}{a factor with 63 levels \code{5}, \code{10}, \code{15}, \code{20}, \code{25}, ..., \code{520}}
  \item{iso.fo}{a numeric vector with values of isometric force ranging from 0.16 to 2.565}
  \item{spec.fo}{a numeric vector with values of specific force ranging from 80.5 to 290}
  \item{occ.f}{a factor with 2 levels \code{Pre}, \code{Pos}, i.e. pre- and post- intervention}
  \item{fiber.f}{ a factor with 2 levels \code{Type 1}, \code{Type 2}, i.e. Type 1 and Type 2 muscle fiber.}}}
\source{
  Claflin, D.R., Larkin, L.M., Cederna, P.S., Horowitz,
  J.F., Alexander, N.B., Cole, N.M., Galecki, A.T., Chen,
  S., Nyquist, L.V., Carlson, B.M., Faulkner, J.A., &
  Ashton-Miller, J.A. (2011) Effects of high- and
  low-velocity resistance training on the contractile
  properties of skeletal muscle fibers from young and older
  humans. Journal of Applied Physiology, 111, 1021-1030.
}
\description{
  Data from a Progressive Resistance Randomized Trial.
}
\details{
  PRT trial was aimed for devising evidence-based methods
  for improving and measuring the mobility and muscle power
  of elderly men and women
}
\examples{
summary(prt.fiber)
}
\seealso{
  \code{\link{prt}}, \code{\link{prt.subjects}}
}
\keyword{datasets}

