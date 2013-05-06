\docType{data}
\name{armd0}
\alias{armd0}
\title{armd0 Data (1107 x 8)}
\format{The \code{armd0} data frame has 1107 rows and 8 columns. It contains data for n=240 subjects
stored in a long format with up to five rows for one subject.
\describe{
  \item{subject}{
    a  factor with 240 levels  \code{1}, \code{2}, \code{3}, \code{4}, \code{5}, ...
  }
  \item{treat.f}{
    a  factor with 2 levels  \code{Placebo}, \code{Active}
  }
  \item{visual0}{
    an integer vector with values from 20 to 85
  }
  \item{miss.pat}{
    a  factor with 9 levels  \code{----}, \code{---X}, \code{--X-}, \code{--XX}, \code{-XX-}, ...
  }
  \item{time.f}{
    a  factor with 5 levels  \code{Baseline}, \code{4wks}, \code{12wks}, \code{24wks}, \code{52wks}
  }
  \item{time}{
    a numeric vector with values from 0 to 52
  }
  \item{visual}{
    an integer vector with values from 3 to 85
  }
  \item{tp}{
    a numeric vector with values from 0 to 4
  }
}}
\source{
  Pharmacological Therapy for Macular Degeneration Study
  Group (1997). Interferon alpha-IIA is ineffective for
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
\seealso{
  \code{\link{armd}}, \code{\link{armd.wide}}
}
\keyword{datasets}

