\name{derivatives}
\alias{fprime}
\alias{fsecond}
\title{ First and Second Derivatives}

\description{
Three points first and second derivatives numerical approximation used with the \code{m4plEstimateMore} function.
 }

\usage{
 fprime( x, FUN = "FP", h = 0.001, names = paste("x", c(1:length(x)), sep = ""))
 fsecond(x, FUN = "FP", h = 0.001, names = paste("x", c(1:length(x)), sep = ""))
 }

\arguments{
  \item{x}{ numeric; vector of values at which the derivation is to be done. }
  \item{FUN}{ function; function to derive. }
  \item{h}{ numeric; neighbouring value. }
  \item{names}{ character; names given to each results. DEFAULT to the existing names of the vector x. }
 }

\details{
This function could be used for numerical derivation in general, but is dedicated to be used internally by \code{m4plEstimateMore}.
For other general purposes \code{D}, \code{deriv} and \code{deriv3} are preferred.
 }

\value{
  \item{fprime}{numeric; vector of first derivatives.}
  \item{fsecond}{numeric; matrix of second derivatives.}
 }

\references{
 Press, W. H., Vetterling, W. T., Teukolsky, S. A. and Flannery, B. P. (2002).
 \emph{Numerical recipees in C++. The art of scientific computing, 2nd edition}.
 Cambridge, United Kingdom: Cambridge University press.

 Yakowitz, S. and Szidarovszky, F. (1986). \emph{An introduction to numerical computations}.
 New York, New Jersey: MacMilla.
 }

\author{Gilles Raiche, Universite du Quebec a Montreal (UQAM),

 Departement d'education et pedagogie

 \email{Raiche.Gilles@uqam.ca}, \url{http://www.er.uqam.ca/nobel/r17165/}
 }

\seealso{ \code{\link{D}}, \code{\link{deriv}} and \code{\link{deriv3}} }

\examples{
## .....................................
 test  <- function(x) 2*x+5
  test(   x=0)
  fprime( x=0, FUN=test)
  fsecond(x=0, FUN=test)
## .....................................
 test2 <- function(x) 2*(x[1]*x[2])+5
  test2(  x=c(0,0))
  fprime( x=c(0,0), FUN=test2)
  fsecond(x=c(0,0), FUN=test2)
 }
 
\keyword{ math }

