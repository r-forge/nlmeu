\name{runScript}
\alias{runScript}
\title{Executes scripts from GB book}
\usage{
  runScript(script = NA, package = "nlmeU",
    subdir = "scriptsR15.0", echo = TRUE)
}
\arguments{
  \item{script}{character string containing name of the
  script to be executed. By default is set to NA.}

  \item{package}{character string containing package name.
  By default nlmeU.}

  \item{subdir}{subdirectory containing scripts. By
  default: scriptsR15.0.}

  \item{echo}{logical. Used by source function. By default
  set to TRUE.}
}
\value{
  Script is executed and results are printed.
}
\description{
  Default call of the function without arguments, prints a
  list of available scripts.
}
\examples{
runScript()
}
\author{
  Andrzej Galecki and Tomasz Burzykowski
}

