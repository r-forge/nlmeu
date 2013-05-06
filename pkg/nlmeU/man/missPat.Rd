\name{missPat}
\alias{missPat}
\title{Extract pattern of missing data}
\usage{
  missPat(..., symbols = c("X", "-"), collapse = "",
    missData = FALSE)
}
\arguments{
  \item{\dots}{one or more vectors/matrices/data frames.
  They need to be compatible for columnwise binding.}

  \item{symbols}{vector containing two single characters
  used to indicate NA and remaining values. By defualt it
  has values: \code{X} and \code{-}.}

  \item{collapse}{an optional character string. It is used
  in the internal call \code{paste()} function to separate
  the results. Rarely used. By default set to NULL}

  \item{missData}{logical. If \code{TRUE} data frame with
  pattern of missing values is saved in \code{missData}
  attribute of the vector returned by this function.}
}
\value{
  character vector with as many elements as length of
  vectors(s)/number of rows in matrices and/or data frames
  in \code{\dots{}} argument(s).  Attribute \code{cnames}
  contains names of vectors/columns/variables.  Optional
  attribute \code{missData} contains data frame with
  missing pattern.
}
\description{
  This function allows to compactly present pattern of
  missing data in a given vector/matrix/data frame or
  combination of thereof.
}
\examples{
dtf <- subset(armd.wide,
             select = c(visual12, visual24, visual52))
missPat(dtf, symbols = c("?","+"))
}
\author{
  Andrzej Galecki and Tomasz Burzykowski
}

