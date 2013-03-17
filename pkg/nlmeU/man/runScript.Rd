% File nlmeU/man/sigma.Rd
% Part of the nlmeU package for R

\name{runScript}
\title{Executes scripts from GB book}
\usage{
runScript(script, package, subdir, echo)
}
\alias{runScript}
\arguments{
 \item{script}{character string containing name of the script to be executed. By default is set to NA.} 
 \item{package}{character string containing package name. By default nlmeU.}
 \item{subdir}{subdirectory containing scripts. By default: scriptsR15.0.}
 \item{echo}{logical. Used by source function. By default set to TRUE}
}
\description{
Default call of the function without arguments, prints a list of avialable scripts, which can be executed.
}
\value{
 Script is executed and results are printed.
}
\author{Andrzej Galecki and Tomasz Burzykowski}
%\seealso{\code{\link{sigma.default}},
%  \code{\link{sigma.pdMat}}}
\examples{
runScript()
}
%\keyword{models}
