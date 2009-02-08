\name{checkAdequation}
\alias{checkAdequation}

\title{ Function to Check the Adequation of the Second Derivatives }
\description{
  Check the adequation of the second derivatives (hessian) for computation
  of variance and covariance. Use inside the function
  \command{m4plEstimateMore} to assure that computations can be applied to
  this second derivative. Problems encountered can be of not positive definitetess,
  singular matrix, diagonal not complitely positive. In these cases inversion
  of the matrix is not posssible or variances cannot be computed from the
  inverse of the diagonal.
}

\usage{
checkAdequation(x)
}

\arguments{
  \item{x}{matrix: second derivative (hessian).}
}

\value{
  \item{isNumbers}{logical: check if the elements of the x matrix are all numerics.}
  \item{correctClass }{logical: check if the the x matrix is of class \code{matrix}.}
  \item{squareMatrix }{logical: check if the the x matrix is square.}
  \item{diagPositive }{logical: check if the elements of the x matrix are all positive and > 0.}
  \item{positiveDefinite }{logical: check if the the x matrix is positiveDefinite.}
  \item{nonSingular }{logical: check if the the x matrix is nonSingular.}
}

\references{
Seber, G. E. A. F. (2008). \emph{A matrix handbook for statisticians}. New York, New Jersey: Wiley.
}

\author{
Gilles Raiche, Universite du Quebec a Montreal (UQAM),

Departement d'education et pedagogie

\email{Raiche.Gilles@uqam.ca}, \url{http://www.er.uqam.ca/nobel/r17165/}
}


\seealso{
\code{\link{eigen}}, \code{\link{det}}, \code{\link{diag}}
}

\examples{
## Complete adequation of the matrix
## ..........................................................
 x <- matrix(c(4.867054, 16.66902, 16.669023, 107.36390), ncol=2)
 checkAdequation(x)
 all(checkAdequation(x))
 eigen(x)$values
 det(x)
 diag(x)
## ..........................................................

## Not positiveDefinite matrix
## ..........................................................
 x <- matrix(1:4, ncol=2)
 checkAdequation(x)
 all(checkAdequation(x))
 eigen(x)$values
 det(x)
 diag(x)
## ..........................................................

## More problems
## ..........................................................
 x <- matrix(c("Inf",2,5,10), ncol=2)
 checkAdequation(x)
 all(checkAdequation(x))
 det(x)
 diag(x)
# eigen(x)$values
 x <- matrix(c("NaN",2,5,10), ncol=2)
 checkAdequation(x)
 all(checkAdequation(x))
 det(x)
 diag(x)
# eigen(x)$values
## ..........................................................
}

\keyword{ distribution }

