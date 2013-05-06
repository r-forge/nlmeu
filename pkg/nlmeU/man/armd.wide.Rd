\docType{data}
\name{armd.wide}
\alias{armd.wide}
\title{armd.wide Data (240 x 10)}
\format{The \code{armd.wide} data frame has 240 rows and 10 columns. Data are
stored in wide format with each row corresponding to one subject.
\describe{
  \item{subject}{ a factor with 240 levels \code{1}, \code{2}, \code{3}, \code{4}, \code{5}, ..., \code{240}}
  \item{lesion}{ an integer vector with values 1, 2, 3, 4 }
  \item{line0}{ an integer vector with values ranging from 5 to 17}
  \item{visual0}{ an integer vector with values of visual acuity measured at baseline ranging from 20 to 85}
  \item{visual4}{ an integer vector with values of visual acuity measured at 4 weeks ranging from 12 to 84}
  \item{visual12}{ an integer vector with values of visual acuity measured at 12 weeks ranging from 3 to 85}
  \item{visual24}{ an integer vector with values of visual acuity measured at 24 weeks ranging from 5 to 85}
  \item{visual52}{ an integer vector with values of visual acuity measured at 52 weeks from 4 to 85 }
  \item{treat.f}{ a factor with 2 levels \code{Placebo}, \code{Active}}
  \item{miss.pat}{ a factor with 9 levels \code{----}, \code{---X}, \code{--X-}, \code{--XX}, \code{-XX-}, ...,\code{XXXX}}}}
\source{
  Pharmacological Therapy for Macular Degeneration Study
  Group (1997).  Interferon alpha-IIA is ineffective for
  patients with choroidal neovascularization secondary to
  age-related macular degeneration. Results of a
  prospective randomized placebo-controlled clinical trial.
  Archives of Ophthalmology, 115, 865-872.
}
\description{
  Data from Age-Related Macular Degeneration (ARMD)
  clinical trial
}
\details{
  The ARMD data arise from a randomized multi-center
  clinical trial comparing an experimental treatment
  (interferon-alpha) versus placebo for patients diagnosed
  with ARMD.
}
\examples{
summary(armd.wide)
}
\seealso{
  \code{\link{armd}}, \code{\link{armd0}}
}
\keyword{datasets}

