\docType{data}
\name{prt}
\alias{prt}
\title{prt Data (2471 x 9)}
\format{The \code{prt} data frame has 2471 rows and 9 columns. It contains
  data for n = 63 subjects. Each subject underwent muscle biopsy before and
  after intervention. Data are stored in a long format with each record
  corresponding to one muscle fiber. There are two types of muscle fibers: Type
  1 and Type 2. Dependent variables: specific force and isometric force are
  measured pre- and post intervention.
  \describe{
   \item{id}{ a factor with 63 levels \code{5}, \code{10}, \code{15}, \code{20}, \code{25}, ..., \code{520} (subject id)}
   \item{prt.f}{ a factor with 2 levels \code{High}, \code{Low}, i.e. training (intervention) intensity}
   \item{age.f}{ a factor with 2 levels \code{Young}, \code{Old} (stratifying variable) }
   \item{sex.f}{ a factor with 2 levels \code{Female}, \code{Male} (stratifying variable)}
   \item{bmi}{ a numeric vector with values of BMI at baseline ranging from 18.36 to 32.29 }
   \item{iso.fo}{ a numeric vector with values of isometric force ranging from 0.16 to 2.565}
   \item{spec.fo}{ a numeric vector with values of specific force ranging from 80.5 to 290 }
   \item{occ.f}{ a factor with 2 levels \code{Pre}, \code{Pos}, i.e. pre- and post-intervention.}
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
  Data frame \code{prt} was obtained by merging
  \code{prt.subjects} and \code{prt.fiber}.
}
\examples{
summary(prt)
}
\seealso{
  \code{\link{prt.fiber}}, \code{\link{prt.subjects}}
}
\keyword{datasets}

