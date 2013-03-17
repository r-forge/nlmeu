% nlmeU/man/SIIdata.Rd
% Part of the nlmeU package for R 
% Distributed under GPL 2 or later: see nlmeU/LICENCE
\name{SIIdata}
\alias{SIIdata}
\docType{data}
\title{SIIdata Data (1190 x 12)}% Data Title
\description{Data from Study of Instructional Improvement Project}
\usage{data(SIIdata)}
\format{
The \code{SIIdata} data frame has 1190 rows and 12 columns.
The dataset includes results for 1190 first grade pupils sampled from 312 classrooms in 107 schools.
\describe{
   \item{sex}{
     a factor with 2 levels  \code{M}, \code{F},i.e. males and females, resepectively
   }
   \item{minority}{
     a factor with 2 levels  \code{Mnrt=No}, \code{Mnrt=Yes}. An indicator variable for the minority status
   }
   \item{mathkind}{
     an integer vector with values from 290 to 629. This is pupil's math score in the spring of the kindergarten year
   }
   \item{mathgain}{
     an integer vector with values from -110 to 253. Number represents pupil's gain in the math achievement score from the spring of kindergarten to the spring of first grade
   }
   \item{ses}{
     a numeric vector with values from -1.61 to 3.21. Value represents socioeconomical status
   }
   \item{yearstea}{
     a numeric vector with values from 0 to 40. It is number of years of teacher's experience in teaching in the first grade
   }
   \item{mathknow}{
     a numeric vector with values from -2.5 to 2.61. Number represents teacher's knowledge of the first-grade math contents (higher
values indicate a higher knowledge of the contents)
   }
   \item{housepov}{
     a numeric vector containing proportion of households in the nneighborhood of the school below the poverty level with values ranging from 0.012 to 0.564
   }
   \item{mathprep}{
     a numeric vector with values from 1 to 6. Contains the number of preparatory courses on the first-grade math contents and methods followed by the teacher.
}
   \item{classid}{
     a  factor with 312 levels  \code{1}, \code{2}, \code{3}, \code{4}, \code{5}, ..., \code{312}. Classroom's id
   }
   \item{schoolid}{
     a  factor with 107 levels  \code{1}, \code{2}, \code{3}, \code{4}, \code{5}, ..., \code{107}. School's id
   }
   \item{childid}{
     a  factor with 1190 levels  \code{1}, \code{2}, \code{3}, \code{4}, \code{5}, ..., \code{1190}. Pupil's id
   }
}}
\details{
The SII Project was carried out to assess the math achievement scores of first- and third-grade pupils in randomly selected classrooms from a national US sample of elementary schools (Hill et al, 2005). Data were also analyzed in West et al, 2007. The outcome of interest is \code{mathgain} variable. Data were created based on \code{classroom} data from \code{WWGbook} package 
}
\source{
Hill, H., Rowan, B., and Ball, D. (2005). Effect of teachers mathematical knowledge for teaching on student achievement. American Educational Research Journal, 42, 371-406.

West, B. T.,Welch, K. B., and Galecki, A. T. (2007). Linear Mixed Models: A Practical Guide Using Statistical Software. Chapman and Hall/CRC.
}

\examples{
summary(SIIdata)
}
\keyword{datasets}
