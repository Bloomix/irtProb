\name{responses4pl}
\alias{gr4pl}
\alias{ggr4pl}
\alias{pggr4pl}

\title{ Simulation of Response Patterns and  Computation of the Probability of the Patterns}

\description{
  Simulation of response patterns and computation of the probability of the patterns according
  to the one, two, three and four parameters logistic item response models.~
 }

\usage{
 gr4pl(N = 10, theta = 0, a = 1, b = 0, c = 0, d = 1)

 ggr4pl(n = 5, rep = 1, theta = 0, a = rep(1, n), b = rep(0, n),
        c = rep(0, n), d = rep(1, n))
       
 pggr4pl(x = ggr4pl(rep = 1), rep = 1, n = dim(x)[2], N = dim(x)[1],
         theta = rep(0, N), a = rep(1, n), b = rep(0, n), c = rep(0, n),
         d = rep(1, n), log.p=FALSE, TCC = FALSE)
 }

\arguments{
  \item{theta}{ numeric; vector of proficiency levels (z sscores). }
  \item{x}{ numeric matrix; response patterns (0 or 1). }
  \item{rep}{ numeric; number of replications of the simulation of the response patterns. }
  \item{n}{ numeric; number of items. }
  \item{N}{ numeric; number of response patterns }
  \item{a}{ numeric; item discrimination parameters. }
  \item{b}{ numeric; item difficulty parameters. }
  \item{c}{ numeric; item pseudo-guessing parameters. }
  \item{d}{ numeric; item inattention parameters. }
  \item{log.p}{ logical; if TRUE, probabilities p are given as log(p). }
  \item{TCC}{ logical; if TRUE generate the TCC figures for each response patterns. Default FALSE. }
 }

\details{
  The function \code{gr4pl} generates \code{N} responses to an item according to the theta parameter and the items parameters.
  The funcfion \code{ggr4pl} will be used to generate \code{rep} respose patterns at \code{n} items. To compute
  the probability of the response patterns, according to known person and item parameters, the function \code{pggr4pl} will be applied.
 }

\value{
 \item{gr4pl}{ numeric; vector of item responses (0 or 1).}
 \item{ggr4pl}{ numeric; data.frame of responses at n items.}
 \item{pggr4pl}{  logical; if (TCC ==TRUE) return(list(prob=prob, tcc=tcc));  if (TCC==FALSE) return(prob)}
 }

\references{
 Hambleton, R. K. and Swaminathan, H. (1985). \emph{Item response theory - Principles and applications}.
 Boston, Massachuset: Kluwer.
 }

\author{
 Gilles Raiche, Universite du Quebec a Montreal (UQAM),

 Departement d'education et pedagogie

 \email{Raiche.Gilles@uqam.ca}, \url{http://www.er.uqam.ca/nobel/r17165/}
 }

\seealso{
 \code{\link{grm4pl}}, \code{\link{ggrm4pl}}, \code{\link{pggrm4pl}},
 \code{\link{ctt2irt}}, \code{\link{irt2ctt}}
 }

\examples{
## ....................................................................
# Generation of reponses (0,1) from r4pl() for N subjects (default value of N= 10)
 gr4pl(c = 1)
 gr4pl(N = 5, theta = c(-4, 4), c = 0)

# Generation of a 7 responses pattern (0,1) for [rep * length(theta)] subjects
#  The subjects number is equal to [rep * length(theta)]]
#  a,b,c et d are item parameters vectors
 nitems <- 7
 N      <- 10
 a      <- rep(1, nitems)
 b      <- rnorm(nitems)
 c      <- rep(0, nitems)
 d      <- rep(1, nitems)
 theta  <- seq(-4,4,length=5)
 x      <- ggr4pl(n = nitems, rep = N, theta = theta, a = a, b = b, c = c, d = d)
 x

## Probability of a 10 responses pattern and test caracteristic curve (TCC)
 nitems <- 10
 a      <- rep(1,nitems)
 b      <- seq(-4,4,length=nitems)
 c      <- rep(0,nitems)
 d      <- rep(1,nitems)
 N      <- 3
 theta  <- seq(-1,1,length=12)
 # Generation of the response patterns
 x      <- ggr4pl(n = nitems, rep = N, theta = theta, a = a, b = b, c = c, d = d)
 x
 # Without TCC
 res    <- pggr4pl(x=x, rep=N, theta=theta,a=a,c=c,d=d,TCC=FALSE); res
 # With TCC for each response pattern
 res    <- pggr4pl(x=x, rep=N, theta=theta,a=a,c=c,d=d,TCC=TRUE); res
## ....................................................................
 }

\keyword{ distribution }

