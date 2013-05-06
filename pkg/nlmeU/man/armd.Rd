\docType{data}
\name{armd}
\alias{armd}
\title{armd Data (867 x 8)}
\format{The \code{armd} data frame has 867 rows and 8 columns. It contains
data for n=234 subjects stored in a long format with up to four rows for one
subject.

\describe{
  \item{subject}{ a factor with 234 levels \code{1}, \code{2}, \code{3}, \code{4}, \code{6}, ..., \code{240}}
  \item{treat.f}{ a factor with 2 levels \code{Placebo}, \code{Active}}
  \item{visual0}{ an integer vector with values ranging from 20 to 85 }
  \item{miss.pat}{ a factor with 8 levels \code{----}, \code{---X}, \code{--X-}, \code{--XX}, \code{-XX-}, ..., \code{X-XX}}
  \item{time.f}{ a factor with 4 levels \code{4wks}, \code{12wks}, \code{24wks}, \code{52wks}}
  \item{time}{ a numeric vector with values 4, 12, 24, 52 }
  \item{visual}{ an integer vector with values ranging from 3 to 85}
  \item{tp}{ a numeric vector with values 1, 2, 3, 4 corresponding to time points 4, 12, 24, 52, respectively}}}
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
summary(armd)
}
\seealso{
  \code{\link{armd0}}, \code{\link{armd.wide}}
}
\keyword{datasets}

